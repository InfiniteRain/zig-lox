const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const testing = std.testing;
const expect = testing.expect;
const memory_package = @import("memory.zig");
const alloc = memory_package.alloc;
const free = memory_package.free;
const realloc = memory_package.realloc;
const growCapacity = memory_package.growCapacity;

pub fn DynamicArray(comptime T: type) type {
    return struct {
        const Self = @This();

        count: usize,
        data: []T,
        allocator: Allocator,

        pub fn init(allocator: Allocator) !Self {
            return .{
                .count = 0,
                .data = try alloc(T, allocator, 8),
                .allocator = allocator,
            };
        }

        pub fn deinit(self: Self) void {
            free(self.allocator, self.data);
        }

        pub fn push(self: *Self, value: T) !void {
            const capacity = self.data.len;

            if (capacity < self.count + 1) {
                const new_capacity = growCapacity(capacity);
                self.data = try realloc(self.allocator, self.data, new_capacity);
            }

            self.data[self.count] = value;
            self.count += 1;
        }
    };
}

test "array size gets properly reallocated" {
    const allocator = testing.allocator;

    var array = try DynamicArray(u8).init(allocator);
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
