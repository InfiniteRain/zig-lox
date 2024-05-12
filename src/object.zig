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
const table_package = @import("table.zig");
const Table = table_package.Table;
const exe_options = @import("exe_options");

pub const NativeResult = union(enum) { ok: Value, err: []const u8 };

pub const NativeFn = *const fn (*VM, u8, [*]Value) NativeResult;

pub const Obj = struct {
    const Self = @This();

    pub const Type = enum {
        string,
        function,
        native,
        closure,
        upvalue,
        class,
        instance,
        bound_method,

        pub fn TypeStruct(comptime _type: Type) type {
            return switch (_type) {
                .string => String,
                .function => Function,
                .native => Native,
                .closure => Closure,
                .upvalue => Upvalue,
                .class => Class,
                .instance => Instance,
                .bound_method => BoundMethod,
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

            vm.stack.push(.{ .obj = &string_obj.obj });
            _ = try vm.strings.set(string_obj, .nil);
            _ = vm.stack.pop();

            return string_obj;
        }

        pub fn fromHeapBufAlloc(memory: *Memory, heap_buf: []u8, vm: *VM) !*String {
            const string_hash = String.hash(heap_buf);
            const interned_opt = vm.strings.findString(heap_buf, string_hash);

            if (interned_opt) |interned| {
                memory.free(heap_buf);
                return interned;
            }

            return String.allocNew(memory, heap_buf, string_hash, vm);
        }

        pub fn fromBufAlloc(memory: *Memory, buf: []const u8, vm: *VM) !*String {
            const string_hash = String.hash(buf);
            const interned_opt = vm.strings.findString(buf, string_hash);

            if (interned_opt) |interned| {
                return interned;
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
            initializer,
            method,
            script,
        };

        pub fn allocNew(memory: *Memory, vm: *VM) !*Function {
            const chunk = try Chunk.init(memory, vm);
            const function_obj = (try Self.fromTypeAlloc(.function, memory, vm)).as(.function);
            function_obj.arity = 0;
            function_obj.upvalue_count = 0;
            function_obj.name = null;
            function_obj.chunk = chunk;

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
        upvalues: []?*Upvalue,
        upvalue_count: usize,

        pub fn allocNew(memory: *Memory, function: *Function, vm: *VM) !*Closure {
            const upvalues = try memory.alloc(?*Upvalue, function.upvalue_count);
            var i: usize = 0;

            while (i < function.upvalue_count) : (i += 1) {
                upvalues[i] = null;
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

    pub const Class = struct {
        obj: Self,
        name: *String,
        methods: Table,

        pub fn allocNew(memory: *Memory, name: *String, vm: *VM) !*Class {
            const class = (try Self.fromTypeAlloc(.class, memory, vm)).as(.class);
            class.name = name;
            class.methods = try Table.init(memory);
            return class;
        }
    };

    pub const Instance = struct {
        obj: Self,
        class: *Class,
        fields: Table,

        pub fn allocNew(memory: *Memory, class: *Class, vm: *VM) !*Instance {
            const instance = (try Self.fromTypeAlloc(.instance, memory, vm)).as(.instance);
            instance.class = class;
            instance.fields = try Table.init(memory);
            return instance;
        }
    };

    pub const BoundMethod = struct {
        obj: Self,
        receiver: Value,
        method: *Closure,

        pub fn allocNew(
            memory: *Memory,
            receiver: Value,
            method: *Closure,
            vm: *VM,
        ) !*BoundMethod {
            const bound_method = (try Self.fromTypeAlloc(.bound_method, memory, vm)).as(.bound_method);
            bound_method.receiver = receiver;
            bound_method.method = method;
            return bound_method;
        }
    };

    type: Type,
    is_marked: bool,
    next: ?*Self,

    pub fn free(self: *Self, memory: *Memory) void {
        switch (self.type) {
            .string => {
                const string = self.as(.string);
                memory.free(string.chars);
                memory.destroy(string);
            },
            .function => {
                const function = self.as(.function);
                function.chunk.deinit();
                memory.destroy(function);
            },
            .native => {
                memory.destroy(self.as(.native));
            },
            .closure => {
                const closure = self.as(.closure);
                memory.free(closure.upvalues);
                memory.destroy(closure);
            },
            .upvalue => {
                memory.destroy(self.as(.upvalue));
            },
            .class => {
                const class = self.as(.class);
                class.methods.deinit();
                memory.destroy(class);
            },
            .instance => {
                const instance = self.as(.instance);
                instance.fields.deinit();
                memory.destroy(instance);
            },
            .bound_method => {
                memory.destroy(self.as(.bound_method));
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
            .is_marked = false,
            .next = vm.objects,
        };

        vm.objects = &typed_obj.obj;

        if (comptime exe_options.log_gc) {
            vm.io.print("{x} allocate {} for {s}\n", .{
                @intFromPtr(&typed_obj.obj),
                @sizeOf(TypedStruct),
                @tagName(_type),
            });
        }

        return &typed_obj.obj;
    }

    pub fn is(self: *Self, _type: Type) bool {
        return self.type == _type;
    }

    pub fn as(self: *Self, comptime _type: Type) *Type.TypeStruct(_type) {
        return @fieldParentPtr("obj", self);
    }
};
