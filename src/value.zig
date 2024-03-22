const std = @import("std");
const dynamic_array_package = @import("dynamic_array.zig");
const DynamicArray = dynamic_array_package.DynamicArray;

pub const Value = f64;

pub const ValueArray = DynamicArray(Value);
