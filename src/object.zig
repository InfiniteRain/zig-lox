const std = @import("std");
const Allocator = std.mem.Allocator;
const value_package = @import("value.zig");
const Value = value_package.Value;
const memory_package = @import("memory.zig");
const alloc = memory_package.alloc;
const _free = memory_package.free;
const create = memory_package.create;
const destroy = memory_package.destroy;
const vm_package = @import("vm.zig");
const VM = vm_package.VM;

pub const Obj = struct {
    const Self = @This();

    pub const Type = enum {
        string,

        pub fn TypeStruct(comptime _type: Type) type {
            return switch (_type) {
                .string => String,
            };
        }
    };

    pub const String = struct {
        obj: Self,
        chars: []u8,

        pub fn fromHeapBufAlloc(allocator: Allocator, heap_buf: []u8, vm: *VM) !*String {
            const string_obj = (try Self.fromTypeAlloc(allocator, .string, vm)).as(.string);
            string_obj.chars = heap_buf;

            return string_obj;
        }

        pub fn fromBufAlloc(allocator: Allocator, buf: []const u8, vm: *VM) !*String {
            const heap_buf = try alloc(u8, allocator, buf.len);
            @memcpy(heap_buf, buf);

            return String.fromHeapBufAlloc(allocator, heap_buf, vm);
        }
    };

    type: Type,
    next: ?*Self,

    pub fn free(self: *Self, allocator: Allocator) void {
        switch (self.type) {
            .string => {
                var string = self.as(.string);
                _free(allocator, string.chars);
                destroy(allocator, self.as(.string));
            },
        }
    }

    pub fn freeList(allocator: Allocator, objects: ?*Self) void {
        var current = objects;

        while (current) |object| {
            var next = object.next;
            object.free(allocator);
            current = next;
        }
    }

    fn fromTypeAlloc(allocator: Allocator, _type: Type, vm: *VM) !*Self {
        const TypedStruct = Type.TypeStruct(_type);
        const typed_obj = try create(TypedStruct, allocator);

        typed_obj.obj = .{
            .type = _type,
            .next = vm.objects,
        };

        vm.objects = &typed_obj.obj;

        return &typed_obj.obj;
    }

    pub fn is(self: *Self, _type: Type) bool {
        return self.type == _type;
    }

    pub fn as(self: *Self, comptime _type: Type) *Type.TypeStruct(_type) {
        return @fieldParentPtr(Type.TypeStruct(_type), "obj", self);
    }
};
