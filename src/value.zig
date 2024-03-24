const std = @import("std");
const activeTag = std.meta.activeTag;
const dynamic_array_package = @import("dynamic_array.zig");
const io_handler_package = @import("io_handler.zig");
const IoHandler = io_handler_package.IoHandler;

pub const Value = union(enum) {
    const Self = @This();

    bool: bool,
    nil,
    number: f64,

    pub fn print(self: Self, io: *IoHandler) void {
        switch (self) {
            .bool => io.print("bool({s})", .{if (self.bool) "true" else "false"}),
            .nil => io.print("nil", .{}),
            .number => io.print("number({d})", .{self.number}),
        }
    }

    pub fn isFalsey(self: Self) bool {
        return (self == .bool and !self.bool) or self == .nil;
    }

    pub fn equals(a: Self, b: Self) bool {
        if (activeTag(a) != activeTag(b)) {
            return false;
        }

        return switch (a) {
            .bool => a.bool == b.bool,
            .nil => true,
            .number => a.number == b.number,
        };
    }
};
