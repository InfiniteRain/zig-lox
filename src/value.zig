const std = @import("std");
const dynamic_array_package = @import("dynamic_array.zig");

pub const Value = union(enum) {
    bool: bool,
    nil,
    number: f64,
};
