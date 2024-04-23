const std = @import("std");
const GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator;
const argsAlloc = std.process.argsAlloc;
const argsFree = std.process.argsFree;
const fixedBufferStream = std.io.fixedBufferStream;
const Allocator = std.mem.Allocator;
const chunk_package = @import("chunk.zig");
const OpCode = chunk_package.OpCode;
const Chunk = chunk_package.Chunk;
const debug_package = @import("debug.zig");
const disassembleChunk = debug_package.disassembleChunk;
const vm_package = @import("vm.zig");
const VM = vm_package.VM;
const memory_package = @import("memory.zig");
const free = memory_package.free;
const compiler_package = @import("compiler.zig");
const Compiler = compiler_package.Compiler;
const io_handler_package = @import("io_handler.zig");
const IoHandler = io_handler_package.IoHandler;

pub fn main() !void {
    var gpa = GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const args = try argsAlloc(allocator);
    defer argsFree(allocator, args);

    var io = try IoHandler.init(allocator);
    defer io.deinit();

    var vm = try VM.init(allocator, &io);
    defer vm.deinit();

    var compiler = try Compiler.init(allocator, &vm, &io);
    defer compiler.deinit();

    switch (args.len) {
        1 => try repl(&compiler, &vm, &io),
        2 => try runFile(allocator, args[1], &compiler, &vm, &io),
        else => {
            io.err("Usage: zig-lox [path]\n", .{});
            std.posix.exit(64);
        },
    }
}

fn runFile(allocator: Allocator, file_path: []u8, compiler: *Compiler, vm: *VM, io: *IoHandler) !void {
    const source = try readFileAlloc(allocator, file_path, io);
    defer free(allocator, source);

    vm.interpret(source, compiler) catch |err| switch (err) {
        error.CompileError => std.posix.exit(65),
        error.RuntimeError => std.posix.exit(70),
        else => return err,
    };
}

fn readFileAlloc(allocator: Allocator, file_path: []u8, io: *IoHandler) ![]u8 {
    const file = std.fs.cwd().openFile(file_path, .{ .mode = .read_only }) catch {
        io.print("Could not open file '{s}'.\n", .{file_path});
        std.posix.exit(74);
    };
    defer file.close();

    try file.seekFromEnd(0);
    const end = try file.getPos();
    try file.seekTo(0);

    return file.reader().readAllAlloc(allocator, end) catch {
        io.print("Not enough memory to read '{s}'.\n", .{file_path});
        std.posix.exit(74);
    };
}

fn repl(compiler: *Compiler, vm: *VM, io: *IoHandler) !void {
    var buf: [1024]u8 = undefined;
    var fbs = fixedBufferStream(&buf);

    while (true) {
        fbs.reset();

        io.print("> ", .{});
        const line = io.readLine() catch {
            io.print("Invalid input.\n", .{});
            break;
        };

        if (line.len == 0) {
            io.print("\n", .{});
            break;
        }

        _ = try vm.interpret(line, compiler);
    }
}

test {
    _ = @import("vm.zig");
}
