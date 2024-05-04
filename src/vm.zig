const std = @import("std");
const Writer = std.fs.File.Writer;
const Allocator = std.mem.Allocator;
const AllocatorError = Allocator.Error;
const assert = std.debug.assert;
const chunk_package = @import("chunk.zig");
const Chunk = chunk_package.Chunk;
const OpCode = chunk_package.OpCode;
const value_package = @import("value.zig");
const Value = value_package.Value;
const debug_package = @import("debug.zig");
const disassembleInstruction = debug_package.disassembleInstruction;
const compiler_package = @import("compiler.zig");
const Compiler = compiler_package.Compiler;
const CompilerError = compiler_package.CompilerError;
const io_handler_package = @import("io_handler.zig");
const IoHandler = io_handler_package.IoHandler;
const exe_options = @import("exe_options");
const trace_execution = exe_options.trace_execution;
const activeTag = std.meta.activeTag;
const memory_package = @import("memory.zig");
const alloc = memory_package.alloc;
const freeObjects = memory_package.freeObjects;
const object_package = @import("object.zig");
const Obj = object_package.Obj;
const table_package = @import("table.zig");
const Table = table_package.Table;

pub const InterpretError = error{
    CompileError,
    RuntimeError,
};

pub const BinaryOperation = enum {
    subtract,
    multiply,
    divide,
    greater,
    less,
};

pub const CallFrame = struct {
    function: *Obj.Function,
    ip: [*]u8,
    slots: [*]Value,
};

const Stack = struct {
    const Self = @This();
    const max_stack = VM.max_frames * 256;

    allocator: Allocator,
    stack: []Value,
    top: [*]Value,

    pub fn init(allocator: Allocator) !Self {
        const stack = try allocator.alloc(Value, max_stack);

        return .{
            .allocator = allocator,
            .stack = stack,
            .top = @ptrCast(&stack[0]),
        };
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.stack);
    }

    pub fn push(self: *Self, value: Value) void {
        self.top[0] = value;
        self.top += 1;
    }

    pub fn pop(self: *Self) Value {
        self.top -= 1;
        return self.top[0];
    }

    pub fn peek(self: *Self, distance: usize) Value {
        return (self.top - 1 - distance)[0];
    }

    pub fn reset(self: *Self) void {
        self.top = @ptrCast(&self.stack[0]);
    }
};

pub const VM = struct {
    const Self = @This();
    const max_frames = 64;

    allocator: Allocator,
    frames: [max_frames]CallFrame,
    frame_count: usize,
    stack: Stack,
    io: *IoHandler,
    strings: Table,
    globals: Table,
    objects: ?*Obj,

    pub fn init(allocator: Allocator, io: *IoHandler) !Self {
        return .{
            .allocator = allocator,
            .frames = undefined,
            .frame_count = 0,
            .stack = try Stack.init(allocator),
            .io = io,
            .strings = try Table.init(allocator),
            .globals = try Table.init(allocator),
            .objects = null,
        };
    }

    pub fn deinit(self: *Self) void {
        self.stack.deinit();
        self.strings.deinit();
        self.globals.deinit();
        Obj.freeList(self.allocator, self.objects);
    }

    pub fn interpret(self: *Self, source: []const u8, compiler: *Compiler) !void {
        const function = try compiler.compile(source);

        self.stack.push(.{ .obj = &function.obj });

        const frame = &self.frames[self.frame_count];
        frame.function = function;
        frame.ip = @ptrCast(function.chunk.code.data);
        frame.slots = @ptrCast(self.stack.stack);

        self.frame_count += 1;

        try self.run();
    }

    fn run(self: *Self) !void {
        const frame = self.getStackFrame();

        while (true) {
            if (comptime trace_execution) {
                self.io.print("           ", .{});

                var slot: [*]Value = @ptrCast(&self.stack.stack[0]);

                while (@intFromPtr(slot) < @intFromPtr(self.stack.top)) {
                    self.io.print("[ ", .{});
                    slot[0].print(self.io);
                    self.io.print(" ]", .{});
                    slot += 1;
                }

                self.io.print("\n", .{});

                _ = disassembleInstruction(&frame.function.chunk, self.getOffset(), self.io) catch unreachable;
            }

            const instruction = self.readOpCode();

            switch (instruction) {
                .constant, .constant_long => {
                    const value = if (instruction == .constant) self.readConstant() else self.readConstantLong();
                    self.stack.push(value);
                },
                .negate => {
                    if (self.stack.peek(0) != .number) {
                        self.runtimeError("Operand must be a number.", .{});
                        return error.RuntimeError;
                    }

                    self.stack.push(Value{ .number = -self.stack.pop().number });
                },
                .print => {
                    const value = self.stack.pop();
                    value.print(self.io);
                    self.io.print("\n", .{});
                },
                .add => {
                    const r = self.stack.peek(0);
                    const l = self.stack.peek(1);

                    if (l.isObjType(.string) and r.isObjType(.string)) {
                        try self.concatenate();
                    } else if (l == .number and r == .number) {
                        const b = self.stack.pop().number;
                        const a = self.stack.pop().number;

                        self.stack.push(.{ .number = a + b });
                    } else {
                        self.runtimeError("Operands must be two numbers or two strings.", .{});
                        return InterpretError.RuntimeError;
                    }
                },
                .nil => self.stack.push(.nil),
                .true => self.stack.push(Value{ .bool = true }),
                .false => self.stack.push(Value{ .bool = false }),
                .pop => {
                    self.assertStackNotEmpty();
                    _ = self.stack.pop();
                },
                .duplicate => {
                    self.stack.push(self.stack.peek(0));
                },
                .get_local, .get_local_long => {
                    const slot = if (instruction == .get_local) self.readU8() else self.readU24();
                    self.stack.push(frame.slots[slot]);
                },
                .set_local, .set_local_long => {
                    const slot = if (instruction == .set_local) self.readU8() else self.readU24();
                    frame.slots[slot] = self.stack.peek(0);
                },
                .get_global, .get_global_long => {
                    const constant_value = if (instruction == .get_global) self.readConstant() else self.readConstantLong();
                    const name = constant_value.obj.as(.string);
                    var value: Value = undefined;

                    if (!self.globals.get(name, &value)) {
                        self.runtimeError("Undefined variable '{s}'.", .{name.chars});
                        return error.RuntimeError;
                    }

                    self.stack.push(value);
                },
                .set_global, .set_global_long => {
                    const constant_value = if (instruction == .set_global) self.readConstant() else self.readConstantLong();
                    const name = constant_value.obj.as(.string);

                    if (try self.globals.set(name, self.stack.peek(0))) {
                        _ = self.globals.delete(name);
                        self.runtimeError("Undefined variable '{s}'.", .{name.chars});
                        return error.RuntimeError;
                    }
                },
                .define_global, .define_global_long => {
                    const constant_value = if (instruction == .define_global) self.readConstant() else self.readConstantLong();
                    const name = constant_value.obj.as(.string);
                    _ = try self.globals.set(name, self.stack.peek(0));
                    _ = self.stack.pop();
                },
                .subtract => try self.binaryOperation(.subtract),
                .multiply => try self.binaryOperation(.multiply),
                .divide => try self.binaryOperation(.divide),
                .not => self.stack.push(Value{ .bool = self.stack.pop().isFalsey() }),
                .equal => {
                    self.assertStackGreaterEqual(2);

                    const b = self.stack.pop();
                    const a = self.stack.pop();

                    self.stack.push(Value{ .bool = a.equals(b) });
                },
                .less => try self.binaryOperation(.less),
                .greater => try self.binaryOperation(.greater),
                .jump => {
                    const offset = self.readU16();
                    frame.ip += offset;
                },
                .jump_if_false => {
                    const offset = self.readU16();

                    if (self.stack.peek(0).isFalsey()) {
                        frame.ip += offset;
                    }
                },
                .loop => {
                    const offset = self.readU16();
                    frame.ip -= offset;
                },
                .ret => {
                    self.assertStackEmpty();
                    return;
                },
                _ => return error.InterpretError,
            }
        }
    }

    fn assertStackEmpty(self: *Self) void {
        const frame = self.getStackFrame();
        assert(@intFromPtr(self.stack.top) == @intFromPtr(frame.slots) + @sizeOf(Value));
    }

    fn assertStackNotEmpty(self: *Self) void {
        const frame = self.getStackFrame();
        assert(@intFromPtr(self.stack.top) > @intFromPtr(frame.slots) + @sizeOf(Value));
    }

    fn assertStackGreaterEqual(self: *Self, comptime length: usize) void {
        const frame = self.getStackFrame();
        assert(@intFromPtr(frame.slots) + @sizeOf(Value) + length * @sizeOf(Value) <= @intFromPtr(self.stack.top));
    }

    fn concatenate(self: *Self) !void {
        const b = self.stack.pop().obj.as(.string);
        const a = self.stack.pop().obj.as(.string);

        const new_length = a.chars.len + b.chars.len;
        const new_chars = try alloc(u8, self.allocator, new_length);
        @memcpy(new_chars[0..a.chars.len], a.chars);
        @memcpy(new_chars[a.chars.len..(a.chars.len + b.chars.len)], b.chars);

        const result = try Obj.String.fromHeapBufAlloc(self.allocator, new_chars, self);
        self.stack.push(.{ .obj = &result.obj });
    }

    fn binaryOperation(self: *Self, operation: BinaryOperation) !void {
        if (self.stack.peek(0) != .number or self.stack.peek(1) != .number) {
            self.runtimeError("Operands must be numbers.", .{});
            return error.RuntimeError;
        }

        const b = self.stack.pop().number;
        const a = self.stack.pop().number;

        self.stack.push(
            switch (operation) {
                .subtract => Value{ .number = a - b },
                .multiply => Value{ .number = a * b },
                .divide => Value{ .number = a / b },
                .greater => Value{ .bool = a > b },
                .less => Value{ .bool = a < b },
            },
        );
    }

    pub fn readConstant(self: *Self) Value {
        const frame = self.getStackFrame();
        return frame.function.chunk.getConstant(self.readU8());
    }

    pub fn readConstantLong(self: *Self) Value {
        const frame = self.getStackFrame();
        return frame.function.chunk.getConstant(@intCast(self.readU24()));
    }

    fn readOpCode(self: *Self) OpCode {
        return @as(OpCode, @enumFromInt(self.readU8()));
    }

    fn readU24(self: *Self) u24 {
        const left: u24 = self.readU8();
        const middle: u24 = self.readU8();
        const right = self.readU8();
        return (left << 16) | (middle << 8) | right;
    }

    fn readU16(self: *Self) u16 {
        const left: u16 = self.readU8();
        const right = self.readU8();
        return (left << 8) | right;
    }

    fn readU8(self: *Self) u8 {
        const frame = self.getStackFrame();

        assert(self.getOffset() < frame.function.chunk.code.count);

        const byte = frame.ip[0];
        frame.ip += 1;
        return byte;
    }

    fn getOffset(self: *Self) usize {
        const frame = self.getStackFrame();
        return @intFromPtr(frame.ip) - @intFromPtr(&frame.function.chunk.code.data[0]);
    }

    fn getStackFrame(self: *Self) *CallFrame {
        return &self.frames[self.frame_count - 1];
    }

    fn runtimeError(self: *Self, comptime format: []const u8, args: anytype) void {
        self.io.err(format, args);
        self.io.err("\n", .{});

        const frame = self.getStackFrame();
        const instruction = frame.ip - @intFromPtr(&frame.function.chunk.code.data[0]) - 1;
        const line = frame.function.chunk.getLine(@intFromPtr(instruction)) catch unreachable;

        self.io.err("[line {}] in script\n", .{line});
        self.stack.reset();
    }
};

test "all intermediary strings should be dealloced at the end of the program" {
    const allocator = std.testing.allocator;

    var io = try IoHandler.init(allocator);
    defer io.deinit();

    var vm = try VM.init(allocator, &io);
    defer vm.deinit();

    var compiler = try Compiler.init(allocator, .script, &vm, &io);
    defer compiler.deinit();

    vm.interpret("\"st\" + \"ri\" + \"ng\";", &compiler) catch unreachable;
}
