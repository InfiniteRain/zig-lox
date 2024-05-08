const std = @import("std");
const testing = std.testing;
const expect = testing.expect;
const expectError = testing.expectError;
const Allocator = std.mem.Allocator;
const memory_package = @import("memory.zig");
const Memory = memory_package.Memory;

const RleError = error{IndexOutOfBounds};

pub fn RleArray(comptime T: type) type {
    return struct {
        const Self = @This();
        const Entry = struct { T, usize };

        memory: *Memory,
        entries_count: usize,
        entries: []Entry,
        count: usize,

        pub fn init(memory: *Memory) !Self {
            return .{
                .memory = memory,
                .entries_count = 0,
                .entries = try memory.alloc(Entry, 8),
                .count = 0,
            };
        }

        pub fn deinit(self: Self) void {
            self.memory.free(self.entries);
        }

        pub fn push(self: *Self, value: T) !void {
            const top_index = if (self.entries_count == 0) 0 else self.entries_count - 1;

            if (self.entries.len <= 0 or self.entries[top_index][0] != value) {
                try self.push_entry(.{ value, 1 });
            } else {
                self.entries[top_index][1] += 1;
            }

            self.count += 1;
        }

        pub fn get(self: *const Self, index: usize) !T {
            if (index < 0 or index >= self.count) {
                return error.IndexOutOfBounds;
            }

            var offset: i128 = index;

            for (0..self.entries_count) |i| {
                const entry = self.entries[i];

                offset -= entry[1];

                if (offset < 0) {
                    return entry[0];
                }
            }

            unreachable;
        }

        fn push_entry(self: *Self, entry: Entry) !void {
            const capacity = self.entries.len;

            if (capacity < self.entries_count + 1) {
                const new_capacity = Memory.growCapacity(capacity);
                self.entries = try self.memory.realloc(self.entries, new_capacity);
            }

            self.entries[self.entries_count] = entry;
            self.entries_count += 1;
        }
    };
}

test "array size gets properly reallocated" {
    const allocator = testing.allocator;
    var memory = Memory.init(allocator);

    var array = try RleArray(u8).init(&memory);
    defer array.deinit();

    try expect(array.entries.len == 8);

    for (0..8) |i| {
        try array.push(@intCast(i));

        try expect(array.entries.len == 8);
        try expect(array.entries[i][0] == i);
    }

    try array.push(8);

    try expect(array.entries.len == 16);
    try expect(array.entries[8][0] == 8);

    for (9..16) |i| {
        try array.push(@intCast(i));

        try expect(array.entries.len == 16);
        try expect(array.entries[i][0] == i);
    }

    try array.push(16);

    try expect(array.entries.len == 32);
    try expect(array.entries[16][0] == 16);
}

test "run-length encoding should work as expected" {
    const allocator = testing.allocator;
    var memory = Memory.init(allocator);

    var array = try RleArray(u8).init(&memory);
    defer array.deinit();

    try expect(array.entries_count == 0);

    try array.push(10);

    try expect(array.entries_count == 1);
    try expect(array.entries[0][0] == 10);
    try expect(array.entries[0][1] == 1);

    for (2..21) |i| {
        try array.push(10);

        try expect(array.entries_count == 1);
        try expect(array.entries[0][0] == 10);
        try expect(array.entries[0][1] == @as(u8, @intCast(i)));
    }

    try array.push(20);

    try expect(array.entries_count == 2);
    try expect(array.entries[1][0] == 20);
    try expect(array.entries[1][1] == 1);

    for (2..21) |i| {
        try array.push(20);

        try expect(array.entries_count == 2);
        try expect(array.entries[1][0] == 20);
        try expect(array.entries[1][1] == @as(u8, @intCast(i)));
    }

    try array.push(10);

    try expect(array.entries_count == 3);
    try expect(array.entries[2][0] == 10);
    try expect(array.entries[2][1] == 1);
}

test "sequencing works properly" {
    const allocator = testing.allocator;
    var memory = Memory.init(allocator);

    var array = try RleArray(u8).init(&memory);
    defer array.deinit();

    try expectError(error.IndexOutOfBounds, array.get(0));

    try array.push(10);

    try expect(try array.get(0) == 10);
    try expectError(error.IndexOutOfBounds, array.get(1));

    try array.push(10);

    try expect(try array.get(1) == 10);
    try expectError(error.IndexOutOfBounds, array.get(2));

    for (2..20) |i| {
        try array.push(10);

        try expect(try array.get(i) == 10);
        try expectError(error.IndexOutOfBounds, array.get(i + 1));
    }

    try array.push(20);

    try expect(try array.get(19) == 10);
    try expect(try array.get(20) == 20);
    try expectError(error.IndexOutOfBounds, array.get(21));

    try array.push(10);

    try expect(try array.get(20) == 20);
    try expect(try array.get(21) == 10);

    try expectError(error.IndexOutOfBounds, array.get(22));
}
