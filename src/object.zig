const std = @import("std");
const Allocator = std.mem.Allocator;
const value_package = @import("value.zig");
const Value = value_package.Value;
const memory_package = @import("memory.zig");
const Memory = memory_package.Memory;
const vm_package = @import("vm.zig");
const VM = vm_package.VM;
const chunk_package = @import("chunk.zig");
const Chunk = chunk_package.Chunk;

pub const NativeResult = union(enum) { ok: Value, err: []const u8 };

pub const NativeFn = *const fn (u8, [*]Value) NativeResult;

pub const Obj = struct {
    const Self = @This();

    pub const Type = enum {
        string,
        function,
        native,
        closure,
        upvalue,

        pub fn TypeStruct(comptime _type: Type) type {
            return switch (_type) {
                .string => String,
                .function => Function,
                .native => Native,
                .closure => Closure,
                .upvalue => Upvalue,
            };
        }
    };

    pub const String = struct {
        obj: Self,
        chars: []u8,
        hash: u32,

        fn allocNew(memory: *Memory, heap_buf: []u8, string_hash: u32, vm: *VM) !*String {
            const string_obj = (try Self.fromTypeAlloc(.string, memory, vm)).as(.string);
            string_obj.chars = heap_buf;
            string_obj.hash = string_hash;

            _ = try vm.strings.set(string_obj, .nil);

            return string_obj;
        }

        pub fn fromHeapBufAlloc(memory: *Memory, heap_buf: []u8, vm: *VM) !*String {
            const string_hash = String.hash(heap_buf);
            const interned = vm.strings.findString(heap_buf, string_hash);

            if (interned) |unwrapped| {
                memory.free(heap_buf);
                return unwrapped;
            }

            return String.allocNew(memory, heap_buf, string_hash, vm);
        }

        pub fn fromBufAlloc(memory: *Memory, buf: []const u8, vm: *VM) !*String {
            const string_hash = String.hash(buf);
            const interned = vm.strings.findString(buf, string_hash);

            if (interned) |unwrapped| {
                return unwrapped;
            }

            const heap_buf = try memory.alloc(u8, buf.len);
            @memcpy(heap_buf, buf);

            return String.allocNew(memory, heap_buf, string_hash, vm);
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
        upvalue_count: usize,
        chunk: Chunk,
        name: ?*String,

        pub const Type = enum {
            function,
            script,
        };

        pub fn allocNew(memory: *Memory, vm: *VM) !*Function {
            const function_obj = (try Self.fromTypeAlloc(.function, memory, vm)).as(.function);
            function_obj.arity = 0;
            function_obj.upvalue_count = 0;
            function_obj.name = null;
            function_obj.chunk = try Chunk.init(memory);

            return function_obj;
        }
    };

    pub const Native = struct {
        obj: Self,
        function: NativeFn,
        arity: u8,

        pub fn allocNew(memory: *Memory, nativeFn: NativeFn, arity: u8, vm: *VM) !*Native {
            const native_obj = (try Self.fromTypeAlloc(.native, memory, vm)).as(.native);
            native_obj.function = nativeFn;
            native_obj.arity = arity;
            return native_obj;
        }
    };

    pub const Closure = struct {
        obj: Self,
        function: *Function,
        upvalues: []*Upvalue,
        upvalue_count: usize,

        pub fn allocNew(memory: *Memory, function: *Function, vm: *VM) !*Closure {
            const upvalues = try memory.alloc(*Upvalue, function.upvalue_count);
            var i: usize = 0;

            while (i < function.upvalue_count) : (i += 1) {
                upvalues[i] = undefined;
            }

            const closure = (try Self.fromTypeAlloc(.closure, memory, vm)).as(.closure);
            closure.function = function;
            closure.upvalues = upvalues;
            closure.upvalue_count = function.upvalue_count;

            return closure;
        }
    };

    pub const Upvalue = struct {
        obj: Self,
        location: *Value,
        closed: Value,
        next: ?*Upvalue,

        pub fn allocNew(memory: *Memory, slot: *Value, vm: *VM) !*Upvalue {
            const upvalue = (try Self.fromTypeAlloc(.upvalue, memory, vm)).as(.upvalue);
            upvalue.location = slot;
            upvalue.closed = .nil;
            upvalue.next = null;
            return upvalue;
        }
    };

    type: Type,
    next: ?*Self,

    pub fn free(self: *Self, memory: *Memory) void {
        switch (self.type) {
            .string => {
                const string = self.as(.string);
                memory.free(string.chars);
                memory.destroy(self.as(.string));
            },
            .function => {
                const function = self.as(.function);
                function.chunk.deinit();
                memory.destroy(self.as(.function));
            },
            .native => {
                memory.destroy(self.as(.native));
            },
            .closure => {
                const closure = self.as(.closure);
                memory.free(closure.upvalues);
                memory.destroy(self.as(.closure));
            },
            .upvalue => {
                memory.destroy(self.as(.upvalue));
            },
        }
    }

    pub fn freeList(memory: *Memory, objects: ?*Self) void {
        var current = objects;

        while (current) |object| {
            const next = object.next;
            object.free(memory);
            current = next;
        }
    }

    fn fromTypeAlloc(comptime _type: Type, memory: *Memory, vm: *VM) !*Self {
        const TypedStruct = Type.TypeStruct(_type);
        const typed_obj = try memory.create(TypedStruct);

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
