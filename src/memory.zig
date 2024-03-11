const std = @import("std");
const Allocator = std.mem.Allocator;

fn reallocate(comptime T: type, allocator: Allocator, pointer_opt: ?[]T, new_size: usize) ![]T {
    return if (pointer_opt) |pointer|
        try allocator.realloc(pointer, new_size)
    else
        return try allocator.alloc(T, new_size);
}

pub fn alloc(comptime T: type, allocator: Allocator, size: usize) ![]T {
    return try reallocate(T, allocator, null, size);
}

pub fn free(allocator: Allocator, pointer: anytype) void {
    const ChildType = @typeInfo(@TypeOf(pointer)).Pointer.child;
    _ = reallocate(ChildType, allocator, pointer, 0) catch unreachable;
}

pub fn realloc(allocator: Allocator, pointer: anytype, new_size: usize) ![]@typeInfo(@TypeOf(pointer)).Pointer.child {
    const PointerType = @typeInfo(@TypeOf(pointer)).Pointer.child;
    return try reallocate(PointerType, allocator, pointer, new_size);
}
