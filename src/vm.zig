const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const chunk_package = @import("chunk.zig");
const Chunk = chunk_package.Chunk;
const OpCode = chunk_package.OpCode;
const value_package = @import("value.zig");
const Value = value_package.Value;
const debug_package = @import("debug.zig");
const debug_trace_execution = debug_package.debug_trace_execution;
const printPlainValue = debug_package.printPlainValue;
const disassembleInstruction = debug_package.disassembleInstruction;
const is_debug_mode = debug_package.is_debug_mode;
const assertInBounds = debug_package.assertInBounds;
const compiler_package = @import("compiler.zig");
const Compiler = compiler_package.Compiler;

pub const InterpretResult = enum {
    ok,
    compile_error,
    runtime_error,
};

pub const BinaryOperation = enum {
    add,
    subtract,
    multiply,
    divide,
};

const Stack = struct {
    const Self = @This();
    const max_stack = 256;

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

    pub fn reset(self: *Self) void {
        self.top = @ptrCast(&self.stack[0]);
    }
};

pub const VM = struct {
    const Self = @This();

    allocator: Allocator,
    chunk: *const Chunk,
    ip: [*]u8,
    stack: Stack,
    compiler: Compiler,

    pub fn init(allocator: Allocator) !Self {
        var vm: Self = .{
            .allocator = allocator,
            .chunk = undefined,
            .ip = undefined,
            .stack = try Stack.init(allocator),
            .compiler = try Compiler.init(allocator),
        };

        return vm;
    }

    pub fn deinit(self: *Self) void {
        self.stack.deinit();
        self.compiler.deinit();
    }

    pub fn interpret(self: *Self, source: []const u8) InterpretResult {
        self.compiler.compile(source);
        return .ok;
    }

    fn run(self: *Self) InterpretResult {
        while (true) {
            if (comptime debug_trace_execution) {
                std.debug.print("           ", .{});

                var slot: [*]Value = @ptrCast(&self.stack.stack[0]);

                while (@intFromPtr(slot) < @intFromPtr(self.stack.top)) {
                    std.debug.print("[ ", .{});
                    printPlainValue(slot[0]);
                    std.debug.print(" ]", .{});
                    slot += 1;
                }

                std.debug.print("\n", .{});

                _ = disassembleInstruction(self.chunk, self.getOffset()) catch {};
            }

            const instruction = self.readOpCode();

            switch (instruction) {
                .constant => {
                    const value = self.readConstant();
                    self.stack.push(value);
                },
                .constant_long => {
                    const value = self.readConstantLong();
                    self.stack.push(value);
                },
                .negate => self.stack.push(-self.stack.pop()),
                .add => self.binaryOperation(.add),
                .subtract => self.binaryOperation(.subtract),
                .multiply => self.binaryOperation(.multiply),
                .divide => self.binaryOperation(.divide),
                .ret => {
                    printPlainValue(self.stack.pop());
                    std.debug.print("\n", .{});
                    return .ok;
                },
                _ => return .compile_error,
            }
        }

        return .ok;
    }

    fn binaryOperation(self: *Self, operation: BinaryOperation) void {
        const b = self.stack.pop();
        const a = self.stack.pop();

        self.stack.push(switch (operation) {
            .add => a + b,
            .subtract => a - b,
            .multiply => a * b,
            .divide => a / b,
        });
    }

    fn readConstant(self: *Self) Value {
        return self.chunk.getConstant(self.readByte());
    }

    fn readConstantLong(self: *Self) Value {
        const left: u24 = self.readByte();
        const middle: u24 = self.readByte();
        const right = self.readByte();
        const index: usize = (left << 16) | (middle << 8) | right;

        return self.chunk.getConstant(index);
    }

    fn readOpCode(self: *Self) OpCode {
        return @as(OpCode, @enumFromInt(self.readByte()));
    }

    fn readByte(self: *Self) u8 {
        if (comptime is_debug_mode) {
            assertInBounds("byte", self.getOffset(), self.chunk.code.count);
        }

        const byte = self.ip[0];
        self.ip += 1;
        return byte;
    }

    fn getOffset(self: *Self) usize {
        // const offset = @intFromPtr(self.ip) - @intFromPtr(@as([*]u8, @ptrCast(self.chunk.code.data)));
        return @intFromPtr(self.ip) - @intFromPtr(&self.chunk.code.data[0]);
    }
};
