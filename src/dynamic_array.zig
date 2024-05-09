const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const testing = std.testing;
const expect = testing.expect;
const memory_package = @import("memory.zig");
const Memory = memory_package.Memory;
const test_suite_package = @import("test_util.zig");
const GeneralSuite = test_suite_package.GeneralSuite;

pub fn DynamicArray(comptime T: type) type {
    return struct {
        const Self = @This();

        memory: *Memory,
        count: usize,
        data: []T,

        pub fn init(memory: *Memory) !Self {
            return .{
                .memory = memory,
                .count = 0,
                .data = try memory.alloc(T, 8),
            };
        }

        pub fn deinit(self: Self) void {
            self.memory.free(self.data);
        }

        pub fn push(self: *Self, value: T) !void {
            const capacity = self.data.len;

            if (capacity < self.count + 1) {
                const new_capacity = Memory.growCapacity(capacity);
                self.data = try self.memory.realloc(self.data, new_capacity);
            }

            self.data[self.count] = value;
            self.count += 1;
        }

        pub fn set(self: *Self, index: usize, value: T) !void {
            assert(index >= 0 and index <= self.data.len);

            if (index == self.data.len) {
                const new_capacity = Memory.growCapacity(self.data.len);
                self.data = try self.memory.realloc(self.data, new_capacity);
            }

            self.data[index] = value;
        }
    };
}

test "array size gets properly reallocated" {
    const allocator = testing.allocator;

    var s = try GeneralSuite.init(allocator);
    defer s.deinit();

    var array = try DynamicArray(u8).init(s.memory);
    defer array.deinit();

    try expect(array.data.len == 8);

    for (0..8) |i| {
        try array.push(@intCast(i));

        try expect(array.data.len == 8);
        try expect(array.data[i] == i);
    }

    try array.push(8);

    try expect(array.data.len == 16);
    try expect(array.data[8] == 8);

    for (9..16) |i| {
        try array.push(@intCast(i));

        try expect(array.data.len == 16);
        try expect(array.data[i] == i);
    }

    try array.push(16);

    try expect(array.data.len == 32);
    try expect(array.data[16] == 16);
}

test "array size gets reallocated when set is used" {
    const allocator = testing.allocator;

    var s = try GeneralSuite.init(allocator);
    defer s.deinit();

    var array = try DynamicArray(u8).init(s.memory);
    defer array.deinit();

    for (0..8) |i| {
        try array.push(@intCast(i));
    }

    try expect(array.data.len == 8);
}
