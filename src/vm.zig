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
const Memory = memory_package.Memory;
const object_package = @import("object.zig");
const Obj = object_package.Obj;
const NativeFn = object_package.NativeFn;
const NativeResult = object_package.NativeResult;
const table_package = @import("table.zig");
const Table = table_package.Table;
const scanner_package = @import("scanner.zig");
const Scanner = scanner_package.Scanner;
const time = @cImport({
    @cInclude("time.h");
});

pub const BinaryOperation = enum {
    subtract,
    multiply,
    divide,
    greater,
    less,
};

pub const CallFrame = struct {
    closure: *Obj.Closure,
    ip: [*]u8,
    slots: [*]Value,
};

fn clockNative(
    arg_count: u8,
    args: [*]Value,
) NativeResult {
    _ = arg_count;
    _ = args;

    return .{ .ok = .{ .number = @as(f64, @floatFromInt(time.clock())) / time.CLOCKS_PER_SEC } };
}

const Stack = struct {
    const Self = @This();
    const max_stack = VM.max_frames * 256;

    memory: *Memory,
    stack: []Value,
    top: [*]Value,

    pub fn init(memory: *Memory) !Self {
        const stack = try memory.alloc(Value, max_stack);

        return .{
            .memory = memory,
            .stack = stack,
            .top = @ptrCast(&stack[0]),
        };
    }

    pub fn deinit(self: *Self) void {
        self.memory.free(self.stack);
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

    memory: *Memory,
    frames: [max_frames]CallFrame,
    frame_count: usize,
    stack: Stack,
    io: *IoHandler,
    strings: Table,
    open_upvalues: ?*Obj.Upvalue,
    globals: Table,
    objects: ?*Obj,
    root_compiler: ?*Compiler,

    pub fn init(self: *Self, memory: *Memory, io: *IoHandler) !void {
        self.memory = memory;
        self.frames = undefined;
        self.frame_count = 0;
        self.stack = try Stack.init(memory);
        self.io = io;
        self.strings = try Table.init(memory);
        self.open_upvalues = null;
        self.globals = try Table.init(memory);
        self.objects = null;
        self.memory.vm = self;

        try self.defineNative("clock", clockNative, 0);
    }

    pub fn deinit(self: *Self) void {
        self.stack.deinit();
        self.strings.deinit();
        self.globals.deinit();
        Obj.freeList(self.memory, self.objects);
    }

    pub fn interpret(self: *Self, source: []const u8, compiler: *Compiler) !void {
        const scanner = Scanner.init(source, self.io);
        const function = try compiler.compile(scanner);

        self.stack.push(.{ .obj = &function.obj });
        const closure = try Obj.Closure.allocNew(self.memory, function, self);
        _ = self.stack.pop();
        self.stack.push(.{ .obj = &closure.obj });
        try self.call(closure, 0);
        try self.run();
    }

    fn run(self: *Self) !void {
        var frame = self.getStackFrame();

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

                _ = disassembleInstruction(&frame.closure.function.chunk, self.getOffset(), self.io) catch unreachable;
            }

            const instruction = self.readOpCode();

            switch (instruction) {
                .closure, .closure_long => {
                    const value = if (instruction == .closure) self.readConstant() else self.readConstantLong();
                    const function = value.obj.as(.function);
                    const closure = try Obj.Closure.allocNew(self.memory, function, self);
                    self.stack.push(.{ .obj = &closure.obj });

                    var i: usize = 0;

                    while (i < closure.upvalue_count) : (i += 1) {
                        const is_local = self.readU8();
                        const index = self.readU8();

                        closure.upvalues[i] = if (is_local == 1)
                            try self.captureUpvalue(&(frame.slots + index)[0])
                        else
                            frame.closure.upvalues[index];
                    }
                },
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
                        return error.RuntimeError;
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
                .get_upvalue => {
                    const slot = self.readU8();
                    self.stack.push(frame.closure.upvalues[slot].location.*);
                },
                .set_upvalue => {
                    const slot = self.readU8();
                    frame.closure.upvalues[slot].location.* = self.stack.peek(0);
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
                .call => {
                    const arg_count = self.readU8();
                    try self.callValue(self.stack.peek(arg_count), arg_count);
                    frame = &self.frames[self.frame_count - 1];
                },
                .close_upvalue => {
                    self.closeUpvalues(&(self.stack.top - 1)[0]);
                    _ = self.stack.pop();
                },
                .ret => {
                    const result = self.stack.pop();
                    self.closeUpvalues(&frame.slots[0]);
                    self.frame_count -= 1;

                    if (self.frame_count == 0) {
                        _ = self.stack.pop();
                        return;
                    }

                    self.stack.top = frame.slots;
                    self.stack.push(result);
                    frame = &self.frames[self.frame_count - 1];
                },
                _ => return error.RuntimeError,
            }
        }
    }

    pub fn call(self: *Self, closure: *Obj.Closure, arg_count: u8) !void {
        if (arg_count != closure.function.arity) {
            self.runtimeError("Expected {} arguments but got {}.", .{
                closure.function.arity,
                arg_count,
            });
            return error.RuntimeError;
        }

        if (self.frame_count == max_frames) {
            self.runtimeError("Stack overflow.", .{});
            return error.RuntimeError;
        }

        const frame = &self.frames[self.frame_count];
        frame.closure = closure;
        frame.ip = @ptrCast(closure.function.chunk.code.data);
        frame.slots = self.stack.top - arg_count - 1;

        self.frame_count += 1;
    }

    pub fn callValue(self: *Self, callee: Value, arg_count: u8) !void {
        if (callee == .obj) {
            switch (callee.obj.type) {
                .closure => return self.call(callee.obj.as(.closure), arg_count),
                .native => {
                    const native = callee.obj.as(.native);

                    if (arg_count != native.arity) {
                        self.runtimeError("Expected {} arguments but got {}.", .{
                            native.arity,
                            arg_count,
                        });
                        return error.RuntimeError;
                    }

                    const result = native.function(arg_count, self.stack.top - arg_count);

                    if (result == .err) {
                        self.runtimeError("{s}", .{result.err});
                        return error.RuntimeError;
                    }

                    self.stack.top -= arg_count + 1;
                    self.stack.push(result.ok);
                    return;
                },
                else => {},
            }
        }

        self.runtimeError("Can only call functions and classes.", .{});
        return error.RuntimeError;
    }

    fn captureUpvalue(self: *Self, local: *Value) !*Obj.Upvalue {
        var prev_upvalue_opt: ?*Obj.Upvalue = null;
        var upvalue_opt = self.open_upvalues;

        while (upvalue_opt != null and @intFromPtr(upvalue_opt.?.location) > @intFromPtr(local)) {
            prev_upvalue_opt = upvalue_opt;
            upvalue_opt = upvalue_opt.?.next;
        }

        if (upvalue_opt != null and upvalue_opt.?.location == local) {
            return upvalue_opt.?;
        }

        const created_upvalue = try Obj.Upvalue.allocNew(self.memory, local, self);
        created_upvalue.next = upvalue_opt;

        if (prev_upvalue_opt == null) {
            self.open_upvalues = created_upvalue;
        } else {
            prev_upvalue_opt.?.next = created_upvalue;
        }

        return created_upvalue.obj.as(.upvalue);
    }

    fn closeUpvalues(self: *Self, last: *Value) void {
        while (self.open_upvalues != null and @intFromPtr(self.open_upvalues.?.location) >= @intFromPtr(last)) {
            var upvalue = self.open_upvalues.?;
            upvalue.closed = upvalue.location.*;
            upvalue.location = &upvalue.closed;
            self.open_upvalues = upvalue.next;
        }
    }

    fn concatenate(self: *Self) !void {
        const b = self.stack.pop().obj.as(.string);
        const a = self.stack.pop().obj.as(.string);

        const new_length = a.chars.len + b.chars.len;
        const new_chars = try self.memory.alloc(u8, new_length);
        @memcpy(new_chars[0..a.chars.len], a.chars);
        @memcpy(new_chars[a.chars.len..(a.chars.len + b.chars.len)], b.chars);

        const result = try Obj.String.fromHeapBufAlloc(self.memory, new_chars, self);
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
        return frame.closure.function.chunk.getConstant(self.readU8());
    }

    pub fn readConstantLong(self: *Self) Value {
        const frame = self.getStackFrame();
        return frame.closure.function.chunk.getConstant(@intCast(self.readU24()));
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

        assert(self.getOffset() < frame.closure.function.chunk.code.count);

        const byte = frame.ip[0];
        frame.ip += 1;
        return byte;
    }

    fn getOffset(self: *Self) usize {
        const frame = self.getStackFrame();
        return @intFromPtr(frame.ip) - @intFromPtr(&frame.closure.function.chunk.code.data[0]);
    }

    fn getStackFrame(self: *Self) *CallFrame {
        return &self.frames[self.frame_count - 1];
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

    fn runtimeError(self: *Self, comptime format: []const u8, args: anytype) void {
        self.io.err("Error: ", .{});
        self.io.err(format, args);
        self.io.err("\n", .{});

        var i = self.frame_count;

        while (i > 0) {
            i -= 1;

            const frame = &self.frames[i];
            const function = frame.closure.function;
            const instruction = @intFromPtr(frame.ip) - @intFromPtr(&function.chunk.code.data[0]) - 1;

            self.io.err("[line {}] in ", .{function.chunk.lines.get(instruction) catch unreachable});

            if (function.name) |name| {
                self.io.err("{s}()\n", .{name.chars});
            } else {
                self.io.err("script\n", .{});
            }
        }

        self.stack.reset();
    }

    fn defineNative(
        self: *Self,
        name: []const u8,
        function: NativeFn,
        arity: u8,
    ) !void {
        self.stack.push(.{ .obj = &(try Obj.String.fromBufAlloc(self.memory, name, self)).obj });
        self.stack.push(.{ .obj = &(try Obj.Native.allocNew(self.memory, function, arity, self)).obj });
        _ = try self.globals.set(self.stack.stack[0].obj.as(.string), self.stack.stack[1]);
        _ = self.stack.pop();
        _ = self.stack.pop();
    }
};

test "all intermediary strings should be dealloced at the end of the program" {
    const allocator = std.testing.allocator;

    var memory = Memory.init(allocator);

    var io = try IoHandler.init(allocator);
    defer io.deinit();

    var vm: VM = undefined;
    try vm.init(&memory, &io);
    // var vm = try VM.init(&memory, &io);
    defer vm.deinit();

    var compiler: Compiler = undefined;
    try compiler.init(&memory, .script, &vm, &io);
    defer compiler.deinit();

    vm.interpret("\"st\" + \"ri\" + \"ng\";", &compiler) catch unreachable;
}
