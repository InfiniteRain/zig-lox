const std = @import("std");
const bufPrint = std.fmt.bufPrint;
const mem = std.mem;
const testing = std.testing;
const expect = std.testing.expect;
const Allocator = std.mem.Allocator;
const object_package = @import("object.zig");
const Obj = object_package.Obj;
const value_package = @import("value.zig");
const Value = value_package.Value;
const memory = @import("memory.zig");
const alloc = memory.alloc;
const free = memory.free;
const growCapacity = memory.growCapacity;
const io_handler_package = @import("io_handler.zig");
const IoHandler = io_handler_package.IoHandler;
const vm_package = @import("vm.zig");
const VM = vm_package.VM;
const test_util_package = @import("test_util.zig");
const TableSuite = test_util_package.TableSuite;

pub const Entry = struct {
    key: ?*Obj.String,
    value: Value,
};

pub const Table = struct {
    const Self = @This();
    const max_load = 0.75;

    allocator: Allocator,
    count: usize,
    entries: []Entry,

    pub fn init(allocator: Allocator) !Self {
        return .{
            .allocator = allocator,
            .count = 0,
            .entries = try alloc(Entry, allocator, 0),
        };
    }

    pub fn deinit(self: *Self) void {
        free(self.allocator, self.entries);
    }

    pub fn set(self: *Self, key: *Obj.String, value: Value) !bool {
        if (self.count + 1 > @as(usize, @intFromFloat(@as(f64, @floatFromInt(self.entries.len)) * max_load))) {
            const new_capacity = growCapacity(self.entries.len);
            try self.adjustCapacity(new_capacity);
        }

        const entry = Self.findEntry(self.entries, key);
        const is_new_key = entry.key == null;

        if (is_new_key and entry.value == .nil) {
            self.count += 1;
        }

        entry.key = key;
        entry.value = value;

        return is_new_key;
    }

    pub fn get(self: *Self, key: *Obj.String, value: *Value) bool {
        if (self.entries.len == 0) {
            return false;
        }

        const entry = Self.findEntry(self.entries, key);

        if (entry.key == null) {
            return false;
        }

        value.* = entry.value;

        return true;
    }

    pub fn delete(self: *Self, key: *Obj.String) bool {
        if (self.entries.len == 0) {
            return false;
        }

        var entry = Self.findEntry(self.entries, key);

        if (entry.key == null) {
            return false;
        }

        entry.key = null;
        entry.value = .{ .bool = true };

        return true;
    }

    pub fn findString(self: *Self, buf: []const u8, hash: u32) ?*Obj.String {
        if (self.count == 0) {
            return null;
        }

        var index = hash % self.entries.len;

        while (true) : (index = (index + 1) % self.entries.len) {
            const entry = &self.entries[index];

            if (entry.key) |key| {
                if (key.chars.len == buf.len and key.hash == hash and mem.eql(u8, key.chars, buf)) {
                    return key;
                }
            } else if (entry.value == .nil) {
                return null;
            }
        }
    }

    fn addAllFrom(self: *Self, from: *Self) void {
        for (from.entries) |*entry| {
            if (entry.key != null) {
                self.set(entry.key, entry.value);
            }
        }
    }

    fn adjustCapacity(self: *Self, capacity: usize) !void {
        const new_entries = try alloc(Entry, self.allocator, capacity);

        for (new_entries) |*new_entry| {
            new_entry.key = null;
            new_entry.value = Value.nil;
        }

        self.count = 0;

        for (self.entries) |*entry| {
            if (entry.key) |key| {
                var dest = Self.findEntry(new_entries, key);

                dest.key = entry.key;
                dest.value = entry.value;

                self.count += 1;
            }
        }

        free(self.allocator, self.entries);
        self.entries = new_entries;
    }

    fn findEntry(entries: []Entry, key: *Obj.String) *Entry {
        var index = key.hash % entries.len;
        var tombstone: ?*Entry = null;

        while (true) : (index = (index + 1) % entries.len) {
            const entry = &entries[index];

            if (entry.key == key) {
                return entry;
            }

            if (entry.key != null) {
                continue;
            }

            if (entry.value == .nil) {
                return if (tombstone) |unwrapped| unwrapped else entry;
            }

            if (tombstone == null) {
                tombstone = entry;
            }
        }
    }
};

test "hashing function overflows properly" {
    // GIVEN
    const allocator = testing.allocator;

    var s = try TableSuite.init(allocator);
    defer s.deinit();

    // WHEN
    const result = s.createString("some very long string that will overflow hash");

    // THEN
    _ = try result;
}

test "table starts at 0 capacity" {
    // GIVEN
    const allocator = testing.allocator;

    // WHEN
    var s = try TableSuite.init(allocator);
    defer s.deinit();

    // THEN
    try expect(s.table.entries.len == 0);
}

test "adding entries resizes the capacity" {
    // GIVEN
    const allocator = testing.allocator;

    var s = try TableSuite.init(allocator);
    defer s.deinit();

    // WHEN
    for (0..6) |i| {
        var buf: [16]u8 = undefined;
        const str = try bufPrint(&buf, "str {}", .{i});
        const str_obj = try s.createString(str);

        _ = try s.table.set(str_obj, .nil);
    }

    // THEN
    try expect(s.table.entries.len == 8);
}

test "adding entries beyond 75% of capacity two times resizes the capacity" {
    // GIVEN
    const allocator = testing.allocator;

    var s = try TableSuite.init(allocator);
    defer s.deinit();

    // WHEN
    for (0..7) |i| {
        var buf: [16]u8 = undefined;
        const str = try bufPrint(&buf, "str {}", .{i});
        const str_obj = try s.createString(str);

        _ = try s.table.set(str_obj, .nil);
    }

    // THEN
    try expect(s.table.entries.len == 16);
}

test "adding entries beyond 75% of capacity three times resizes the capacity" {
    // GIVEN
    const allocator = testing.allocator;

    var s = try TableSuite.init(allocator);
    defer s.deinit();

    // WHEN
    for (0..13) |i| {
        var buf: [16]u8 = undefined;
        const str = try bufPrint(&buf, "str {}", .{i});
        const str_obj = try s.createString(str);

        _ = try s.table.set(str_obj, .nil);
    }

    // THEN
    try expect(s.table.entries.len == 32);
}

test "set should return true when new key inserted" {
    // GIVEN
    const allocator = testing.allocator;

    var s = try TableSuite.init(allocator);
    defer s.deinit();

    const str = try s.createString("key");

    // WHEN
    const result = try s.table.set(str, .nil);

    // THEN
    try expect(result);
}

test "set should return false when key value overriden" {
    // GIVEN
    const allocator = testing.allocator;

    var s = try TableSuite.init(allocator);
    defer s.deinit();

    const str = try s.createString("key");
    _ = try s.table.set(str, .nil);

    // WHEN
    const result = try s.table.set(str, .{ .number = 10.0 });

    // THEN
    try expect(!result);
}

test "set should properly handle collisions" {
    // GIVEN
    const allocator = testing.allocator;

    var s = try TableSuite.init(allocator);
    defer s.deinit();

    var s2 = try TableSuite.init(allocator);
    defer s2.deinit();

    // hashes of both AAAA and AAAI % 8 are 1
    const s_key1 = try s.createString("AAAA");
    const s_key2 = try s.createString("AAAI");

    const s2_key1 = try s2.createString("AAAA");
    const s2_key2 = try s2.createString("AAAI");

    // WHEN
    _ = try s.table.set(s_key1, .{ .bool = true });
    _ = try s.table.set(s_key2, .{ .bool = false });

    _ = try s2.table.set(s2_key2, .{ .bool = false });
    _ = try s2.table.set(s2_key1, .{ .bool = true });

    // THEN
    try expect(s.table.entries[1].value.bool);
    try expect(&s.table.entries[1].key.?.obj == &s_key1.obj);

    try expect(!s.table.entries[2].value.bool);
    try expect(&s.table.entries[2].key.?.obj == &s_key2.obj);

    try expect(!s2.table.entries[1].value.bool);
    try expect(&s2.table.entries[1].key.?.obj == &s2_key2.obj);

    try expect(s2.table.entries[2].value.bool);
    try expect(&s2.table.entries[2].key.?.obj == &s2_key1.obj);
}

test "get should return true on existent items" {
    // GIVEN
    const allocator = testing.allocator;

    var s = try TableSuite.init(allocator);
    defer s.deinit();

    const str = try s.createString("key");
    _ = try s.table.set(str, .nil);

    // WHEN
    var value: Value = undefined;
    const result = s.table.get(str, &value);

    // THEN
    try expect(result);
}

test "get should return false on non-existent item" {
    // GIVEN
    const allocator = testing.allocator;

    var s = try TableSuite.init(allocator);
    defer s.deinit();

    const str = try s.createString("non-existent key");

    // WHEN
    var value: Value = undefined;
    const result = s.table.get(str, &value);

    // THEN
    try expect(!result);
}

test "get should properly handle collisions" {
    // GIVEN
    const allocator = testing.allocator;

    var s = try TableSuite.init(allocator);
    defer s.deinit();

    var s2 = try TableSuite.init(allocator);
    defer s2.deinit();

    // hashes of both AAAA and AAAI % 8 are 1
    const s_key1 = try s.createString("AAAA");
    const s_key2 = try s.createString("AAAI");

    const s2_key1 = try s2.createString("AAAA");
    const s2_key2 = try s2.createString("AAAI");

    _ = try s.table.set(s_key1, .{ .bool = true });
    _ = try s.table.set(s_key2, .{ .bool = false });

    _ = try s2.table.set(s2_key2, .{ .bool = false });
    _ = try s2.table.set(s2_key1, .{ .bool = true });

    // WHEN
    var s_val1: Value = undefined;
    var s_val2: Value = undefined;

    var s2_val1: Value = undefined;
    var s2_val2: Value = undefined;

    _ = s.table.get(s_key1, &s_val1);
    _ = s.table.get(s_key2, &s_val2);

    _ = s2.table.get(s2_key1, &s2_val1);
    _ = s2.table.get(s2_key2, &s2_val2);

    // THEN
    try expect(s_val1.bool);
    try expect(!s_val2.bool);

    try expect(s2_val1.bool);
    try expect(!s2_val2.bool);
}

test "get should resolve with correct values" {
    // GIVEN
    const allocator = testing.allocator;

    var s = try TableSuite.init(allocator);
    defer s.deinit();

    const str1 = try s.createString("key1");
    const str2 = try s.createString("key2");
    const str3 = try s.createString("key3");
    const str4 = try s.createString("key4");

    _ = try s.table.set(str1, .nil);
    _ = try s.table.set(str2, .{ .number = 10.0 });
    _ = try s.table.set(str3, .{ .bool = false });
    _ = try s.table.set(str4, .{ .obj = &(try s.createString("some value")).obj });
    _ = try s.table.set(str2, .{ .number = 20.0 });

    // WHEN
    var val1: Value = undefined;
    var val2: Value = undefined;
    var val3: Value = undefined;
    var val4: Value = undefined;

    _ = s.table.get(str1, &val1);
    _ = s.table.get(str2, &val2);
    _ = s.table.get(str3, &val3);
    _ = s.table.get(str4, &val4);

    // THEN
    try expect(val1 == .nil);
    try expect(@abs(val2.number - 20.0) < 1e-9);
    try expect(!val3.bool);
    try expect(val4.obj == &(try s.createString("some value")).obj);
}

test "delete returns true on successful deletion" {
    // GIVEN
    const allocator = testing.allocator;

    var s = try TableSuite.init(allocator);
    defer s.deinit();

    const str = try s.createString("key1");

    _ = try s.table.set(str, .nil);

    // WHEN
    const result = s.table.delete(str);

    // THEN
    try expect(result);
}

test "delete returns false on unsuccessful deletion" {
    // GIVEN
    const allocator = testing.allocator;

    var s = try TableSuite.init(allocator);
    defer s.deinit();

    const str = try s.createString("key1");

    // WHEN
    const result = s.table.delete(str);

    // THEN
    try expect(!result);
}

test "delete should properly delete values" {
    // GIVEN
    const allocator = testing.allocator;

    var s = try TableSuite.init(allocator);
    defer s.deinit();

    const str1 = try s.createString("key1");
    const str2 = try s.createString("key2");

    _ = try s.table.set(str1, .nil);
    _ = try s.table.set(str2, .{ .bool = true });

    const delete_result = s.table.delete(str1);
    _ = delete_result;

    // WHEN
    var val1: Value = undefined;
    var val2: Value = undefined;

    const result1 = s.table.get(str1, &val1);
    const result2 = s.table.get(str2, &val2);

    // THEN
    try expect(!result1);
    try expect(result2);
    try expect(val2.bool);
}

test "delete should place a tombstone" {
    // GIVEN
    const allocator = testing.allocator;

    var s = try TableSuite.init(allocator);
    defer s.deinit();

    // key AAAA will put the entry into index 1
    const str1 = try s.createString("AAAA");

    _ = try s.table.set(str1, .nil);

    // WHEN
    _ = s.table.delete(str1);

    // THEN
    try expect(s.table.entries[1].key == null);
    try expect(s.table.entries[1].value.bool == true);
}

test "tombstone should prevent collided items from becoming inaccessible" {
    // GIVEN
    const allocator = testing.allocator;

    var s = try TableSuite.init(allocator);
    defer s.deinit();

    // hashes of both AAAA and AAAI % 8 are 1
    const key1 = try s.createString("AAAA");
    const key2 = try s.createString("AAAI");

    _ = try s.table.set(key1, .nil);
    _ = try s.table.set(key2, .{ .bool = true });

    // WHEN
    _ = s.table.delete(key1);

    var val: Value = undefined;
    const result = s.table.get(key2, &val);

    // THEN
    try expect(result);
    try expect(val.bool);
}

test "tombstones should not be copied over to the resized buffer" {
    // GIVEN
    const allocator = testing.allocator;

    var s = try TableSuite.init(allocator);
    defer s.deinit();

    const str1 = try s.createString("key1");

    _ = try s.table.set(str1, .nil);
    _ = s.table.delete(str1);

    // WHEN
    for (0..7) |i| {
        var buf: [16]u8 = undefined;
        const str = try bufPrint(&buf, "str {}", .{i});
        const str_obj = try s.createString(str);

        _ = try s.table.set(str_obj, .nil);
    }

    // THEN
    for (s.table.entries) |entry| {
        try expect(entry.key != null or entry.value != .bool or !entry.value.bool);
    }
}
