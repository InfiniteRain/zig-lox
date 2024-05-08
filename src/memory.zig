const std = @import("std");
const Allocator = std.mem.Allocator;
const vm_package = @import("vm.zig");
const VM = vm_package.VM;

pub const Memory = struct {
    const Self = @This();

    allocator: Allocator,
    vm: ?*VM,

    pub fn init(allocator: Allocator) Self {
        return .{
            .allocator = allocator,
            .vm = null,
        };
    }

    fn reallocate(self: *Self, comptime T: type, pointer_opt: ?[]T, new_size: usize) ![]T {
        return if (pointer_opt) |pointer|
            try self.allocator.realloc(pointer, new_size)
        else
            return try self.allocator.alloc(T, new_size);
    }

    pub fn alloc(self: *Self, comptime T: type, size: usize) ![]T {
        return try self.reallocate(T, null, size);
    }

    pub fn create(self: *Self, comptime T: type) !*T {
        return &(try self.alloc(T, 1))[0];
    }

    pub fn free(self: *Self, pointer: anytype) void {
        const ChildType = @typeInfo(@TypeOf(pointer)).Pointer.child;
        _ = self.reallocate(ChildType, pointer, 0) catch unreachable;
    }

    pub fn destroy(self: *Self, pointer: anytype) void {
        const ChildType = @typeInfo(@TypeOf(pointer)).Pointer.child;
        const slice: []ChildType = @as(*[1]ChildType, pointer);
        self.free(slice);
    }

    pub fn realloc(self: *Self, pointer: anytype, new_size: usize) ![]@typeInfo(@TypeOf(pointer)).Pointer.child {
        const PointerType = @typeInfo(@TypeOf(pointer)).Pointer.child;
        return try self.reallocate(PointerType, pointer, new_size);
    }

    pub fn growCapacity(capacity: usize) usize {
        return if (capacity <= 0) 8 else capacity * 2;
    }
};
