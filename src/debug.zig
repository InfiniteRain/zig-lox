const std = @import("std");
const debug = std.debug;
const chunk_package = @import("chunk.zig");
const OpCode = chunk_package.OpCode;
const Chunk = chunk_package.Chunk;
const value_package = @import("value.zig");
const Value = value_package.Value;
const builtin = @import("builtin");

pub const is_debug_mode = builtin.mode == .Debug;
pub const debug_trace_execution = true;

pub fn assertInBounds(name: []const u8, index: usize, count: usize) void {
    if (index >= count) {
        std.debug.panic("attempt to read {s} at invalid index {}, last index is {}", .{ name, index, count - 1 });
    }
}

pub fn disassembleChunk(chunk: *const Chunk, name: []const u8) !void {
    debug.print("== {s} ==\n", .{name});

    var offset: usize = 0;

    while (offset < chunk.code.count) {
        offset = try disassembleInstruction(chunk, offset);
    }
}

pub fn disassembleInstruction(chunk: *const Chunk, offset: usize) !usize {
    debug.print("{:0>4} ", .{offset});

    const line = try chunk.getLine(offset);

    if (offset > 0 and line == try chunk.getLine(offset - 1)) {
        debug.print("    | ", .{});
    } else {
        debug.print("{:0>4}| ", .{line});
    }

    const instruction = chunk.code.data[offset];

    return switch (@as(OpCode, @enumFromInt(instruction))) {
        OpCode.ret => simpleInstruction("RET", offset),
        OpCode.constant => constantInstruction(chunk, "CONSTANT", offset),
        OpCode.constant_long => constantLongInstruction(chunk, "CONSTANT_LONG", offset),
        OpCode.negate => simpleInstruction("NEGATE", offset),
        OpCode.add => simpleInstruction("ADD", offset),
        OpCode.subtract => simpleInstruction("SUBTRACT", offset),
        OpCode.multiply => simpleInstruction("MULTIPLY", offset),
        OpCode.divide => simpleInstruction("DIVIDE", offset),
        _ => blk: {
            debug.print("{s: <16} Error: invalid opcode, got {}\n", .{ "?", instruction });
            break :blk offset + 1;
        },
    };
}

fn constantInstruction(chunk: *const Chunk, name: []const u8, offset: usize) usize {
    const index = chunk.readByte(offset + 1);
    debug.print("{s: <16} {:0>4} '", .{ name, index });
    printPlainValue(extractConstant(chunk, index));
    debug.print("'\n", .{});

    return offset + 2;
}

fn constantLongInstruction(chunk: *const Chunk, name: []const u8, offset: usize) usize {
    const left: u24 = chunk.readByte(offset + 1);
    const middle: u24 = chunk.readByte(offset + 2);
    const right = chunk.readByte(offset + 3);
    const index: usize = (left << 16) | (middle << 8) | right;

    debug.print("{s: <16} {:0>4} '", .{ name, index });
    printPlainValue(extractConstant(chunk, index));
    debug.print("'\n", .{});

    return offset + 4;
}

fn simpleInstruction(name: []const u8, offset: usize) usize {
    debug.print("{s}\n", .{name});
    return offset + 1;
}

fn extractConstant(chunk: *const Chunk, index: usize) Value {
    return chunk.constants.data[index];
}

pub fn printPlainValue(value: Value) void {
    debug.print("{d}", .{value});
}
