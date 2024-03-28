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

pub const TableSuite = struct {
    const Self = @This();

    allocator: Allocator,
    io: IoHandler,
    vm: VM,
    table: Table,

    pub fn init(allocator: Allocator) !Self {
        var io = try IoHandler.init(allocator);
        var vm = try VM.init(allocator, &io);
        var table = try Table.init(allocator);

        return .{
            .allocator = allocator,
            .io = io,
            .vm = vm,
            .table = table,
        };
    }

    pub fn deinit(self: *Self) void {
        self.io.deinit();
        self.vm.deinit();
        self.table.deinit();
    }

    pub fn createString(self: *Self, buf: []const u8) !*Obj.String {
        return try Obj.String.fromBufAlloc(self.allocator, buf, &self.vm);
    }
};
