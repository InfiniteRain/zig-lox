const std = @import("std");
const testing = std.testing;
const assert = std.debug.assert;
const expect = testing.expect;
const expectError = testing.expectError;
const Allocator = std.mem.Allocator;
const memory_package = @import("memory.zig");
const Memory = memory_package.Memory;
const dynamic_array_package = @import("dynamic_array.zig");
const DynamicArray = dynamic_array_package.DynamicArray;
const value_package = @import("value.zig");
const Value = value_package.Value;
const rle_array_package = @import("rle_array.zig");
const RleArray = rle_array_package.RleArray;
const table_package = @import("table.zig");
const Table = table_package.Table;
const object_packge = @import("object.zig");
const Obj = object_packge.Obj;
const vm_package = @import("vm.zig");
const VM = vm_package.VM;
const test_suite_package = @import("test_util.zig");
const GeneralSuite = test_suite_package.GeneralSuite;

const OpCodeError = error{NotOperand};

pub const OpCode = enum(u8) {
    call,
    ret,
    class,
    closure,
    closure_long,
    close_upvalue,
    constant,
    constant_long,
    negate,
    print,
    jump,
    jump_if_false,
    loop,
    nil,
    true,
    false,
    pop,
    duplicate,
    get_local,
    get_local_long,
    set_local,
    set_local_long,
    get_global,
    get_global_long,
    set_global,
    set_global_long,
    set_upvalue,
    get_upvalue,
    set_property,
    get_property,
    define_global,
    define_global_long,
    equal,
    greater,
    less,
    add,
    subtract,
    multiply,
    divide,
    not,
    _,
};

pub const Chunk = struct {
    const Self = @This();

    vm: *VM,
    code: DynamicArray(u8),
    constants: DynamicArray(Value),
    const_vars: Table,
    lines: RleArray(u64),

    pub fn init(memory: *Memory, vm: *VM) !Self {
        const code = try DynamicArray(u8).init(memory);
        errdefer code.deinit();

        const constants = try DynamicArray(Value).init(memory);
        errdefer constants.deinit();

        const lines = try RleArray(u64).init(memory);
        errdefer lines.deinit();

        const const_vars = try Table.init(memory);

        return .{
            .vm = vm,
            .code = code,
            .constants = constants,
            .lines = lines,
            .const_vars = const_vars,
        };
    }

    pub fn deinit(self: *Self) void {
        self.code.deinit();
        self.constants.deinit();
        self.lines.deinit();
        self.const_vars.deinit();
    }

    pub fn writeOpCode(self: *Self, op_code: OpCode, line: u64) !void {
        try self.writeByte(@intFromEnum(op_code), line);
    }

    pub fn writeByte(self: *Self, byte: u8, line: u64) !void {
        try self.code.push(byte);
        try self.lines.push(line);
    }

    pub fn writeConstant(self: *Self, value: Value, line: u64) !void {
        const index = try self.addConstant(value);

        if (index <= 0xFF) {
            try self.writeOpCode(.constant, line);
        } else {
            // bytecode could handle more, but giving a limit of 255 to comply to tests
            return error.TooManyConstants;
        }

        try self.writeConstantIndex(index, line);
    }

    pub fn writeClosure(self: *Self, value: Value, line: u64) !void {
        const index = try self.addConstant(value);

        if (index <= 0xFF) {
            try self.writeOpCode(.closure, line);
        } else {
            try self.writeOpCode(.closure_long, line);
        }

        try self.writeConstantIndex(index, line);
    }

    pub fn writeConstantIndex(self: *Self, index: usize, line: u64) !void {
        if (index < 0xFF) {
            try self.writeByte(@intCast(index), line);
        } else {
            try self.writeByte(@intCast(0xFF & (@as(u24, @intCast(index & 0xFFFFFF)) >> 16)), line);
            try self.writeByte(@intCast(0xFF & (@as(u24, @intCast(index & 0xFFFFFF)) >> 8)), line);
            try self.writeByte(@intCast(0xFF & (@as(u24, @intCast(index & 0xFFFFFF)))), line);
        }
    }

    pub fn readByte(self: *const Self, index: usize) u8 {
        assert(index < self.code.count);

        return self.code.data[index];
    }

    pub fn getLine(self: *const Self, index: usize) !u64 {
        assert(index < self.lines.count);

        return try self.lines.get(index);
    }

    pub fn addConstant(self: *Self, value: Value) !usize {
        self.vm.stack.push(value);
        try self.constants.push(value);
        _ = self.vm.stack.pop();

        return self.constants.count - 1;
    }

    pub fn getConstant(self: *const Self, index: usize) Value {
        assert(index < self.constants.count);

        return self.constants.data[index];
    }

    pub fn setVarConstness(self: *Self, name: *Obj.String, is_const: bool) !void {
        self.vm.stack.push(.{ .obj = &name.obj });
        _ = try self.const_vars.set(name, .{ .bool = is_const });
        _ = self.vm.stack.pop();
    }

    pub fn getVarConstness(self: *Self, name: *Obj.String) bool {
        var value: Value = undefined;
        const is_found = self.const_vars.get(name, &value);
        return if (is_found) value.bool else false;
    }
};

test "writeOpcode adds opcode and line as expected" {
    const allocator = testing.allocator;

    var s = try GeneralSuite.init(allocator);
    defer s.deinit();

    var chunk = try Chunk.init(s.memory, s.vm);
    defer chunk.deinit();

    try expect(chunk.code.count == 0);
    try expect(chunk.lines.count == 0);

    try chunk.writeOpCode(OpCode.ret, 10);

    try expect(chunk.code.count == 1);
    try expect(chunk.code.data[0] == @intFromEnum(OpCode.ret));
    try expect(chunk.lines.count == 1);
    try expect(try chunk.lines.get(0) == 10);

    try chunk.writeOpCode(OpCode.constant, 20);

    try expect(chunk.code.count == 2);
    try expect(chunk.code.data[1] == @intFromEnum(OpCode.constant));
    try expect(chunk.lines.count == 2);
    try expect(try chunk.lines.get(1) == 20);
}

test "readByte works as expected" {
    const allocator = testing.allocator;

    var s = try GeneralSuite.init(allocator);
    defer s.deinit();

    var chunk = try Chunk.init(s.memory, s.vm);
    defer chunk.deinit();

    try chunk.writeOpCode(OpCode.ret, 0);
    try chunk.writeOpCode(OpCode.constant, 0);

    try expect(chunk.readByte(0) == @intFromEnum(OpCode.ret));
    try expect(chunk.readByte(1) == @intFromEnum(OpCode.constant));
}

test "readLine works as expected" {
    const allocator = testing.allocator;
    var s = try GeneralSuite.init(allocator);
    defer s.deinit();

    var chunk = try Chunk.init(s.memory, s.vm);
    defer chunk.deinit();

    try chunk.writeOpCode(OpCode.ret, 10);
    try chunk.writeOpCode(OpCode.constant, 20);

    try expect(try chunk.getLine(0) == 10);
    try expect(try chunk.getLine(1) == 20);
}

test "writeConstant and read_constant work as expected" {
    const allocator = testing.allocator;
    var s = try GeneralSuite.init(allocator);
    defer s.deinit();

    var chunk = try Chunk.init(s.memory, s.vm);
    defer chunk.deinit();

    for (0..300) |i| {
        const value: f64 = @floatFromInt(i);
        const index = try chunk.addConstant(.{ .number = value });

        try expect(@abs(chunk.getConstant(index).number - value) < 1e-9);
    }
}
