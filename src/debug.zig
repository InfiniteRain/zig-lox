const std = @import("std");
const Allocator = std.mem.Allocator;
const allocPrint = std.fmt.allocPrint;
const assert = std.debug.assert;
const chunk_package = @import("chunk.zig");
const OpCode = chunk_package.OpCode;
const Chunk = chunk_package.Chunk;
const value_package = @import("value.zig");
const Value = value_package.Value;
const builtin = @import("builtin");
const io_handler_package = @import("io_handler.zig");
const IoHandler = io_handler_package.IoHandler;

pub fn disassembleChunk(chunk: *const Chunk, name: []const u8, io: *IoHandler) !void {
    io.print("== {s} ==\n", .{name});

    var offset: usize = 0;

    while (offset < chunk.code.count) {
        offset = try disassembleInstruction(chunk, offset, io);
    }
}

pub fn disassembleInstruction(chunk: *const Chunk, offset: usize, io: *IoHandler) !usize {
    io.print("{:0>4} ", .{offset});

    const line = try chunk.getLine(offset);

    if (offset > 0 and line == try chunk.getLine(offset - 1)) {
        io.print("    | ", .{});
    } else {
        io.print("{:0>4}| ", .{line});
    }

    const instruction = chunk.code.data[offset];

    return switch (@as(OpCode, @enumFromInt(instruction))) {
        .ret => simpleInstruction("RET", offset, io),
        .constant => constantInstruction(chunk, "CONSTANT", offset, io),
        .constant_long => constantLongInstruction(chunk, "CONSTANT_LONG", offset, io),
        .negate => simpleInstruction("NEGATE", offset, io),
        .print => simpleInstruction("PRINT", offset, io),
        .add => simpleInstruction("ADD", offset, io),
        .nil => simpleInstruction("NIL", offset, io),
        .true => simpleInstruction("TRUE", offset, io),
        .false => simpleInstruction("FALSE", offset, io),
        .pop => simpleInstruction("POP", offset, io),
        .subtract => simpleInstruction("SUBTRACT", offset, io),
        .multiply => simpleInstruction("MULTIPLY", offset, io),
        .divide => simpleInstruction("DIVIDE", offset, io),
        .not => simpleInstruction("NOT", offset, io),
        .equal => simpleInstruction("EQUAL", offset, io),
        .get_local => byteInstruction("GET_LOCAL", chunk, offset, io),
        .get_local_long => threeByteInstruction("GET_LOCAL_LONG", chunk, offset, io),
        .set_local => byteInstruction("SET_LOCAL", chunk, offset, io),
        .set_local_long => threeByteInstruction("SET_LOCAL_LONG", chunk, offset, io),
        .get_global => constantInstruction(chunk, "GET_GLOBAL", offset, io),
        .get_global_long => constantLongInstruction(chunk, "GET_GLOBAL_LONG", offset, io),
        .set_global => constantInstruction(chunk, "SET_GLOBAL", offset, io),
        .set_global_long => constantLongInstruction(chunk, "SET_GLOBAL_LONG", offset, io),
        .define_global => constantInstruction(chunk, "DEFINE_GLOBAL", offset, io),
        .define_global_long => constantLongInstruction(chunk, "DEFINE_GLOBAL_LONG", offset, io),
        .greater => simpleInstruction("GREATER", offset, io),
        .less => simpleInstruction("LESS", offset, io),
        _ => blk: {
            io.print("{s: <18} Error: invalid opcode, got {}\n", .{ "?", instruction });
            break :blk offset + 1;
        },
    };
}

fn constantInstruction(chunk: *const Chunk, name: []const u8, offset: usize, io: *IoHandler) usize {
    const index = chunk.readByte(offset + 1);
    io.print("{s: <18} {:0>4} '", .{ name, index });
    extractConstant(chunk, index).print(io);
    io.print("'\n", .{});

    return offset + 2;
}

fn constantLongInstruction(chunk: *const Chunk, name: []const u8, offset: usize, io: *IoHandler) usize {
    const left: u24 = chunk.readByte(offset + 1);
    const middle: u24 = chunk.readByte(offset + 2);
    const right = chunk.readByte(offset + 3);
    const index: usize = (left << 16) | (middle << 8) | right;

    io.print("{s: <18} {:0>4} '", .{ name, index });
    extractConstant(chunk, index).print(io);
    io.print("'\n", .{});

    return offset + 4;
}

fn simpleInstruction(name: []const u8, offset: usize, io: *IoHandler) usize {
    io.print("{s}\n", .{name});
    return offset + 1;
}

fn byteInstruction(name: []const u8, chunk: *const Chunk, offset: usize, io: *IoHandler) usize {
    const slot = chunk.code.data[offset + 1];
    io.print("{s: <18} {:0>4}\n", .{ name, slot });
    return offset + 2;
}

fn threeByteInstruction(name: []const u8, chunk: *const Chunk, offset: usize, io: *IoHandler) usize {
    const left: u24 = @intCast(chunk.code.data[offset + 1]);
    const middle: u24 = @intCast(chunk.code.data[offset + 2]);
    const right: u24 = @intCast(chunk.code.data[offset + 3]);

    io.print("{s: <18} {:0>4}\n", .{ name, (left << 16) | (middle << 8) | right });
    return offset + 4;
}

fn extractConstant(chunk: *const Chunk, index: usize) Value {
    return chunk.constants.data[index];
}
