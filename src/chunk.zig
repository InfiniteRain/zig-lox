const std = @import("std");
const testing = std.testing;
const expect = testing.expect;
const expectError = testing.expectError;
const mem = std.mem;
const Allocator = mem.Allocator;
const memory = @import("memory.zig");
const grow_capacity = memory.grow_capacity;
const alloc = memory.alloc;
const realloc = memory.realloc;
const free = memory.free;
const dynamic_array = @import("dynamic_array.zig");
const DynamicArray = dynamic_array.DynamicArray;
const value = @import("value.zig");
const Value = value.Value;
const rle_array = @import("rle_array.zig");
const RleArray = rle_array.RleArray;

pub const OpCode = union(enum(u8)) {
    operand: u8,
    ret,
    constant,
    constant_long,
};

pub const Chunk = struct {
    const Self = @This();

    code: DynamicArray(OpCode),
    constants: DynamicArray(Value),
    lines: RleArray(u64),

    pub fn init(allocator: Allocator) !Self {
        const code = try DynamicArray(OpCode).init(allocator);
        errdefer code.deinit();

        const constants = try DynamicArray(Value).init(allocator);
        errdefer constants.deinit();

        const lines = try RleArray(u64).init(allocator);

        return .{
            .code = code,
            .constants = constants,
            .lines = lines,
        };
    }

    pub fn deinit(self: *Self) void {
        self.code.deinit();
        self.constants.deinit();
        self.lines.deinit();
    }

    pub fn write_opcode(self: *Self, opcode: OpCode, line: u64) !void {
        try self.code.push(opcode);
        try self.lines.push(line);
    }

    pub fn write_constant(self: *Self, val: Value, line: u64) !void {
        const index = try self.add_constant(val);

        if (index < 256) {
            try self.write_opcode(OpCode.constant, line);
            try self.write_opcode(OpCode{ .operand = @intCast(index) }, line);
        } else {
            try self.write_opcode(OpCode.constant_long, line);
            try self.write_opcode(OpCode{ .operand = @intCast(0xFF & (@as(u24, @intCast(index & 0xFFFFFF)) >> 16)) }, line);
            try self.write_opcode(OpCode{ .operand = @intCast(0xFF & (@as(u24, @intCast(index & 0xFFFFFF)) >> 8)) }, line);
            try self.write_opcode(OpCode{ .operand = @intCast(0xFF & (@as(u24, @intCast(index & 0xFFFFFF)))) }, line);
        }
    }

    pub fn read_opcode(self: *const Self, index: usize) OpCode {
        return self.code.data[index];
    }

    pub fn read_line(self: *const Self, index: usize) !u64 {
        return try self.lines.get(index);
    }

    pub fn add_constant(self: *Self, val: Value) !usize {
        try self.constants.push(val);
        return self.constants.count - 1;
    }

    pub fn get_constant(self: *Self, index: usize) Value {
        return self.constants.data[index];
    }
};

test "first allocation in init fails" {
    var failing_allocator = testing.FailingAllocator.init(testing.allocator, 0);
    const allocator = failing_allocator.allocator();

    const result = Chunk.init(allocator);

    try expectError(error.OutOfMemory, result);
}

test "second allocation in init fails" {
    var failing_allocator = testing.FailingAllocator.init(testing.allocator, 1);
    const allocator = failing_allocator.allocator();

    const result = Chunk.init(allocator);

    try expectError(error.OutOfMemory, result);
}

test "third allocation in init fails" {
    var failing_allocator = testing.FailingAllocator.init(testing.allocator, 2);
    const allocator = failing_allocator.allocator();

    const result = Chunk.init(allocator);

    try expectError(error.OutOfMemory, result);
}

test "write_opcode adds opcode and line as expected" {
    const allocator = testing.allocator;

    var chunk = try Chunk.init(allocator);
    defer chunk.deinit();

    try expect(chunk.code.count == 0);
    try expect(chunk.lines.count == 0);

    try chunk.write_opcode(OpCode.ret, 10);

    try expect(chunk.code.count == 1);
    try expect(chunk.code.data[0] == OpCode.ret);
    try expect(chunk.lines.count == 1);
    try expect(try chunk.lines.get(0) == 10);

    try chunk.write_opcode(OpCode.constant, 20);

    try expect(chunk.code.count == 2);
    try expect(chunk.code.data[1] == OpCode.constant);
    try expect(chunk.lines.count == 2);
    try expect(try chunk.lines.get(1) == 20);
}

test "read_opcode works as expected" {
    const allocator = testing.allocator;

    var chunk = try Chunk.init(allocator);
    defer chunk.deinit();

    try chunk.write_opcode(OpCode.ret, 0);
    try chunk.write_opcode(OpCode.constant, 0);

    try expect(chunk.read_opcode(0) == OpCode.ret);
    try expect(chunk.read_opcode(1) == OpCode.constant);
}

test "read_line works as expected" {
    const allocator = testing.allocator;

    var chunk = try Chunk.init(allocator);
    defer chunk.deinit();

    try chunk.write_opcode(OpCode.ret, 10);
    try chunk.write_opcode(OpCode.constant, 20);

    try expect(try chunk.read_line(0) == 10);
    try expect(try chunk.read_line(1) == 20);
}

test "write_constant and read_constant work as expected" {
    const allocator = testing.allocator;

    var chunk = try Chunk.init(allocator);
    defer chunk.deinit();

    for (0..300) |i| {
        const val: f64 = @floatFromInt(i);
        const index = try chunk.add_constant(val);

        try expect(@fabs(chunk.get_constant(index) - val) < 1e-9);
    }
}
