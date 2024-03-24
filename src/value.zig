const std = @import("std");
const mem = std.mem;
const activeTag = std.meta.activeTag;
const dynamic_array_package = @import("dynamic_array.zig");
const io_handler_package = @import("io_handler.zig");
const IoHandler = io_handler_package.IoHandler;
const object_package = @import("object.zig");
const Obj = object_package.Obj;

pub const Value = union(enum) {
    const Self = @This();

    bool: bool,
    nil,
    number: f64,
    obj: *Obj,

    pub fn print(self: Self, io: *IoHandler) void {
        switch (self) {
            .bool => io.print("{s}", .{if (self.bool) "true" else "false"}),
            .nil => io.print("nil", .{}),
            .number => io.print("{d}", .{self.number}),
            .obj => switch (self.obj.type) {
                .string => io.print("\"{s}\"", .{self.obj.as(.string).chars}),
            },
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
            .obj => {
                if (a.obj.type != b.obj.type) {
                    return false;
                }

                const a_string = a.obj.as(.string).chars;
                const b_string = b.obj.as(.string).chars;

                return switch (a.obj.type) {
                    .string => mem.eql(u8, a_string, b_string),
                };
            },
        };
    }

    pub fn isObjType(value: Value, _type: Obj.Type) bool {
        return value == .obj and value.obj.type == _type;
    }
};
