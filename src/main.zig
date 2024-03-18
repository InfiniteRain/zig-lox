const std = @import("std");
const GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator;
const process = std.process;
const log = std.log;
const os = std.os;
const io = std.io;
const mem = std.mem;
const Allocator = mem.Allocator;
const chunk_package = @import("chunk.zig");
const OpCode = chunk_package.OpCode;
const Chunk = chunk_package.Chunk;
const debug_package = @import("debug.zig");
const disassembleChunk = debug_package.disassembleChunk;
const vm_package = @import("vm.zig");
const VM = vm_package.VM;
const InterpretResult = vm_package.InterpretResult;
const memory_package = @import("memory.zig");
const free = memory_package.free;
const compiler_package = @import("compiler.zig");
const Compiler = compiler_package.Compiler;

pub fn main() !void {
    var gpa = GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const args = try process.argsAlloc(allocator);
    defer process.argsFree(allocator, args);

    var vm = try VM.init(allocator);
    defer vm.deinit();

    const stdin = io.getStdIn().reader();
    const stdout = io.getStdOut().writer();

    if (args.len == 1) {
        try repl(&vm, stdin, stdout);
    } else if (args.len == 2) {
        try runFile(allocator, args[1], &vm, stdout);
    } else {
        log.err("Usage: zig-lox [path]\n", .{});
        os.exit(64);
    }
}

fn runFile(allocator: Allocator, file_path: []u8, vm: *VM, stdout: anytype) !void {
    const source = try readFileAlloc(allocator, file_path, stdout);
    defer free(allocator, source);

    const result: InterpretResult = vm.interpret(source);

    if (result == .compile_error) {
        os.exit(65);
    }

    if (result == .runtime_error) {
        os.exit(70);
    }
}

fn readFileAlloc(allocator: Allocator, file_path: []u8, stdout: anytype) ![]u8 {
    const file = std.fs.cwd().openFile(file_path, .{ .mode = .read_only }) catch {
        try stdout.print("Could not open file '{s}'.\n", .{file_path});
        os.exit(74);
    };
    defer file.close();

    try file.seekFromEnd(0);
    const end = try file.getPos();
    try file.seekTo(0);

    return file.reader().readAllAlloc(allocator, end) catch {
        try stdout.print("Not enough memory to read '{s}'.\n", .{file_path});
        os.exit(74);
    };
}

fn repl(vm: *VM, stdin: anytype, stdout: anytype) !void {
    var buf: [1024]u8 = undefined;
    var fbs = io.fixedBufferStream(&buf);

    while (true) {
        fbs.reset();

        try stdout.print("> ", .{});
        stdin.streamUntilDelimiter(fbs.writer(), '\n', 1024) catch {
            try stdout.print("\n", .{});
            break;
        };

        const line = fbs.getWritten();

        if (line.len == 0) {
            try stdout.print("\n", .{});
            break;
        }

        _ = vm.interpret(line);
    }
}
