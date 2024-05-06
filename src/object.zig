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
const chunk_package = @import("chunk.zig");
const Chunk = chunk_package.Chunk;

pub const NativeFn = *const fn (u8, [*]Value) Value;

pub const Obj = struct {
    const Self = @This();

    pub const Type = enum {
        string,
        function,
        native,

        pub fn TypeStruct(comptime _type: Type) type {
            return switch (_type) {
                .string => String,
                .function => Function,
                .native => Native,
            };
        }
    };

    pub const String = struct {
        obj: Self,
        chars: []u8,
        hash: u32,

        fn allocNew(allocator: Allocator, heap_buf: []u8, string_hash: u32, vm: *VM) !*String {
            const string_obj = (try Self.fromTypeAlloc(.string, allocator, vm)).as(.string);
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

    pub const Function = struct {
        obj: Self,
        arity: u16,
        chunk: Chunk,
        name: ?*String,

        pub const Type = enum {
            function,
            script,
        };

        pub fn allocNew(allocator: Allocator, vm: *VM) !*Function {
            const function_obj = (try Self.fromTypeAlloc(.function, allocator, vm)).as(.function);
            function_obj.arity = 0;
            function_obj.name = null;
            function_obj.chunk = try Chunk.init(allocator);

            return function_obj;
        }
    };

    pub const Native = struct {
        obj: Self,
        function: NativeFn,
        arity: u8,

        pub fn allocNew(allocator: Allocator, nativeFn: NativeFn, arity: u8, vm: *VM) !*Native {
            const native_obj = (try Self.fromTypeAlloc(.native, allocator, vm)).as(.native);
            native_obj.function = nativeFn;
            native_obj.arity = arity;
            return native_obj;
        }
    };

    type: Type,
    next: ?*Self,

    pub fn free(self: *Self, allocator: Allocator) void {
        switch (self.type) {
            .string => {
                const string = self.as(.string);
                _free(allocator, string.chars);
                destroy(allocator, self.as(.string));
            },
            .function => {
                const function = self.as(.function);
                function.chunk.deinit();
                destroy(allocator, self.as(.function));
            },
            .native => {
                destroy(allocator, self.as(.native));
            },
        }
    }

    pub fn freeList(allocator: Allocator, objects: ?*Self) void {
        var current = objects;

        while (current) |object| {
            const next = object.next;
            object.free(allocator);
            current = next;
        }
    }

    fn fromTypeAlloc(comptime _type: Type, allocator: Allocator, vm: *VM) !*Self {
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
        return @fieldParentPtr("obj", self);
    }
};
