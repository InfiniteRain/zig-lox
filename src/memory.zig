const std = @import("std");
const Allocator = std.mem.Allocator;
const vm_package = @import("vm.zig");
const VM = vm_package.VM;
const exe_options = @import("exe_options");
const io_handler_package = @import("io_handler.zig");
const IoHandler = io_handler_package.IoHandler;
const value_package = @import("value.zig");
const Value = value_package.Value;
const obj_package = @import("object.zig");
const Obj = obj_package.Obj;
const table_package = @import("table.zig");
const Table = table_package.Table;
const dynamic_array_package = @import("dynamic_array.zig");
const DynamicArray = dynamic_array_package.DynamicArray;

pub const Memory = struct {
    const Self = @This();
    const heap_grow_factor = 2;

    allocator: Allocator,
    vm: ?*VM,
    io: *IoHandler,

    bytes_allocated: usize,
    next_gc: usize,

    pub fn init(allocator: Allocator, io: *IoHandler) Self {
        return .{
            .allocator = allocator,
            .vm = null,
            .io = io,

            .bytes_allocated = 0,
            .next_gc = 1024 * 1024,
        };
    }

    fn reallocate(self: *Self, comptime T: type, pointer_opt: ?[]T, old_size: usize, new_size: usize) ![]T {
        const old: isize = @as(isize, @intCast(old_size)) * @as(isize, @intCast(@sizeOf(T)));
        const new: isize = @as(isize, @intCast(new_size)) * @as(isize, @intCast(@sizeOf(T)));
        const new_bytes = @as(isize, @intCast(self.bytes_allocated)) + new - old;

        self.bytes_allocated = @intCast(new_bytes);

        if (new_size > old_size) {
            if (comptime exe_options.stress_gc) {
                self.collectGarbage();
            }
        }

        if (self.bytes_allocated > self.next_gc) {
            self.collectGarbage();
        }

        return if (pointer_opt) |pointer|
            try self.allocator.realloc(pointer, new_size)
        else
            try self.allocator.alloc(T, new_size);
    }

    pub fn alloc(self: *Self, comptime T: type, size: usize) ![]T {
        return try self.reallocate(T, null, 0, size);
    }

    pub fn create(self: *Self, comptime T: type) !*T {
        return &(try self.alloc(T, 1))[0];
    }

    pub fn free(self: *Self, pointer: anytype) void {
        const ChildType = @typeInfo(@TypeOf(pointer)).Pointer.child;
        _ = self.reallocate(ChildType, pointer, pointer.len, 0) catch unreachable;
    }

    pub fn destroy(self: *Self, pointer: anytype) void {
        const ChildType = @typeInfo(@TypeOf(pointer)).Pointer.child;
        const slice: []ChildType = @as(*[1]ChildType, pointer);
        self.free(slice);
    }

    pub fn realloc(self: *Self, pointer: anytype, new_size: usize) ![]@typeInfo(@TypeOf(pointer)).Pointer.child {
        const ChildType = @typeInfo(@TypeOf(pointer)).Pointer.child;
        return try self.reallocate(ChildType, pointer, pointer.len, new_size);
    }

    pub fn growCapacity(capacity: usize) usize {
        return if (capacity <= 0) 8 else capacity * 2;
    }

    pub fn collectGarbage(self: *Self) void {
        if (self.vm == null) {
            return;
        }

        var before: usize = undefined;

        if (comptime exe_options.log_gc) {
            self.io.print("-- gc begin\n", .{});
            before = self.bytes_allocated;
        }

        self.markRoots();
        self.traceReferences();
        self.removeWhiteStrings();
        self.sweep();

        self.next_gc = self.bytes_allocated * heap_grow_factor;

        if (comptime exe_options.log_gc) {
            self.io.print("-- gc end\n", .{});
            self.io.print("   collected {} bytes (from {} to {}) next at {}\n", .{
                before - self.bytes_allocated,
                before,
                self.bytes_allocated,
                self.next_gc,
            });
        }
    }

    fn removeWhiteStrings(self: *Self) void {
        const vm = self.vm orelse unreachable;

        for (vm.strings.entries) |*entry| {
            if (entry.key != null and !entry.key.?.obj.is_marked) {
                _ = vm.strings.delete(entry.key.?);
            }
        }
    }

    fn sweep(self: *Self) void {
        const vm = self.vm orelse unreachable;
        var previous_opt: ?*Obj = null;
        var object_opt = vm.objects;

        while (object_opt) |object| {
            if (object.is_marked) {
                object.is_marked = false;
                previous_opt = object;
                object_opt = object.next;
                continue;
            }

            const unreached = object;
            object_opt = object.next;

            if (previous_opt) |previous| {
                previous.next = object_opt;
            } else {
                vm.objects = object_opt;
            }

            if (comptime exe_options.log_gc) {
                const value: Value = .{ .obj = unreached };
                self.io.print("{x} sweep ", .{@intFromPtr(unreached)});
                value.print(self.io);
                self.io.print("\n", .{});
            }

            unreached.free(self);
        }
    }

    fn traceReferences(self: *Self) void {
        const vm = self.vm orelse unreachable;

        while (vm.gray_count > 0) {
            vm.gray_count -= 1;
            const object = vm.gray_stack[vm.gray_count];
            self.blackenObject(object);
        }
    }

    fn blackenObject(self: *Self, object: *Obj) void {
        if (comptime exe_options.log_gc) {
            const value = Value{ .obj = object };
            self.io.print("{x} blacken ", .{@intFromPtr(object)});
            value.print(self.io);
            self.io.print("\n", .{});
        }

        switch (object.type) {
            .class => {
                const class = object.as(.class);

                self.markObject(&class.name.obj);
            },
            .instance => {
                const instance = object.as(.instance);

                self.markObject(&instance.class.obj);
                self.markTable(&instance.fields);
            },
            .upvalue => self.markValue(object.as(.upvalue).closed),
            .function => {
                const function = object.as(.function);

                if (function.name) |name| {
                    self.markObject(&name.obj);
                }

                self.markArray(function.chunk.constants);
            },
            .closure => {
                const closure = object.as(.closure);
                var i: usize = 0;

                self.markObject(&closure.function.obj);

                while (i < closure.upvalue_count) : (i += 1) {
                    if (closure.upvalues[i]) |upvalue| {
                        self.markObject(&upvalue.obj);
                    }
                }
            },
            .native, .string => {},
        }
    }

    fn markArray(self: *Self, array: DynamicArray(Value)) void {
        var i: usize = 0;

        while (i < array.count) : (i += 1) {
            self.markValue(array.data[i]);
        }
    }

    fn markRoots(self: *Self) void {
        const vm = self.vm orelse unreachable;
        var slot: [*]Value = @ptrCast(vm.stack.stack);
        var upvalue_opt = vm.open_upvalues;

        while (@intFromPtr(slot) < @intFromPtr(vm.stack.top)) : (slot += 1) {
            self.markValue(slot[0]);
        }

        while (upvalue_opt) |upvalue| : (upvalue_opt = upvalue.next) {
            self.markObject(&upvalue.obj);
        }

        self.markTable(&vm.globals);
        self.markCompilerRoots();
    }

    fn markCompilerRoots(self: *Self) void {
        const vm = self.vm orelse unreachable;
        var compiler_opt = vm.current_compiler;

        while (compiler_opt) |compiler| : (compiler_opt = compiler.enclosing) {
            if (compiler.current_function) |function| {
                self.markObject(&function.obj);
            }
        }
    }

    fn markTable(self: *Self, table: *Table) void {
        for (table.entries) |*entry| {
            if (entry.key) |key| {
                self.markObject(&key.obj);
            }
            self.markValue(entry.value);
        }
    }

    fn markValue(self: *Self, value: Value) void {
        if (value == .obj) {
            self.markObject(value.obj);
        }
    }

    fn markObject(self: *Self, object: *Obj) void {
        const vm = self.vm orelse unreachable;

        if (object.is_marked) {
            return;
        }

        if (comptime exe_options.log_gc) {
            const value = Value{ .obj = object };
            self.io.print("{x} mark ", .{@intFromPtr(object)});
            value.print(self.io);
            self.io.print("\n", .{});
        }

        object.is_marked = true;

        if (vm.gray_stack.len < vm.gray_count + 1) {
            vm.gray_stack = self.allocator.realloc(vm.gray_stack, Self.growCapacity(vm.gray_stack.len)) catch {
                std.posix.exit(1);
            };
        }

        vm.gray_stack[vm.gray_count] = object;
        vm.gray_count += 1;
    }
};
