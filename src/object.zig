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
        hash: u32,

        fn allocNew(allocator: Allocator, heap_buf: []u8, string_hash: u32, vm: *VM) !*String {
            const string_obj = (try Self.fromTypeAlloc(allocator, .string, vm)).as(.string);
            string_obj.chars = heap_buf;
            string_obj.hash = string_hash;

            _ = try vm.strings.set(string_obj, .nil);

            return string_obj;
        }

        pub fn fromHeapBufAlloc(allocator: Allocator, heap_buf: []u8, vm: *VM) !*String {
            const string_hash = String.hash(heap_buf);
            const interned = vm.strings.findString(heap_buf, string_hash);

            if (interned) |unwrapped| {
                _free(allocator, heap_buf);
                return unwrapped;
            }

            return String.allocNew(allocator, heap_buf, string_hash, vm);
        }

        pub fn fromBufAlloc(allocator: Allocator, buf: []const u8, vm: *VM) !*String {
            const string_hash = String.hash(buf);
            const interned = vm.strings.findString(buf, string_hash);

            if (interned) |unwrapped| {
                return unwrapped;
            }

            const heap_buf = try alloc(u8, allocator, buf.len);
            @memcpy(heap_buf, buf);

            return String.allocNew(allocator, heap_buf, string_hash, vm);
        }

        pub fn hash(heap_buf: []const u8) u32 {
            var current_hash: u32 = 2_166_136_261;

            for (heap_buf) |char| {
                current_hash ^= char;
                current_hash *%= 16_777_619;
            }

            return current_hash;
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
