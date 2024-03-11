const std = @import("std");
const debug = std.debug;
const adt = @import("dynamic_array.zig");
const DynamicArray = adt.DynamicArray;

pub const Value = f64;

pub const ValueArray = DynamicArray(Value);
