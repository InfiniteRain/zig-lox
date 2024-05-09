const std = @import("std");
const bufPrint = std.fmt.bufPrint;
const Allocator = std.mem.Allocator;
const table_package = @import("table.zig");
const Table = table_package.Table;
const io_handler_package = @import("io_handler.zig");
const IoHandler = io_handler_package.IoHandler;
const vm_package = @import("vm.zig");
const VM = vm_package.VM;
const object_package = @import("object.zig");
const Obj = object_package.Obj;
const memory_package = @import("memory.zig");
const Memory = memory_package.Memory;

pub const GeneralSuite = struct {
    const Self = @This();

    allocator: Allocator,
    memory: *Memory,
    io: *IoHandler,
    vm: *VM,
    table: *Table,

    pub fn init(allocator: Allocator) !Self {
        const io = try allocator.create(IoHandler);
        io.* = try IoHandler.init(allocator);

        const memory = try allocator.create(Memory);
        memory.* = Memory.init(allocator, io);

        var vm = try allocator.create(VM);
        try vm.init(memory, io);

        const table = try allocator.create(Table);
        table.* = try Table.init(memory);

        return .{
            .allocator = allocator,
            .memory = memory,
            .io = io,
            .vm = vm,
            .table = table,
        };
    }

    pub fn deinit(self: *Self) void {
        self.table.deinit();
        self.allocator.destroy(self.table);
        self.vm.deinit();
        self.allocator.destroy(self.vm);
        self.allocator.destroy(self.memory);
        self.io.deinit();
        self.allocator.destroy(self.io);
    }

    pub fn createString(self: *Self, buf: []const u8) !*Obj.String {
        return try Obj.String.fromBufAlloc(self.memory, buf, self.vm);
    }
};
