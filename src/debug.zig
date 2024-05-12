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
        io.print("   | ", .{});
    } else {
        io.print("{: >4} ", .{line});
    }

    const instruction = chunk.code.data[offset];
    const op_code = @as(OpCode, @enumFromInt(instruction));

    return switch (op_code) {
        .call => byteInstruction("CALL", chunk, offset, io),
        .ret => simpleInstruction("RET", offset, io),
        .class => constantInstruction(chunk, "CLASS", offset, io),
        .method => constantInstruction(chunk, "METHOD", offset, io),
        .closure, .closure_long => {
            var new_offset = offset + 1;
            var constant: u24 = undefined;

            if (op_code == .closure) {
                constant = chunk.code.data[new_offset];
                new_offset += 1;
            } else {
                constant = readU24(chunk, new_offset);
                new_offset += 3;
            }

            io.print("{s: <18} {: <4} ", .{ "CLOSURE", constant });
            chunk.constants.data[constant].print(io);
            io.print("\n", .{});

            const function = chunk.constants.data[constant].obj.as(.function);
            var i: usize = 0;

            while (i < function.upvalue_count) : (i += 1) {
                const is_local = chunk.code.data[new_offset];
                new_offset += 1;
                const index = chunk.code.data[new_offset];
                new_offset += 1;

                io.print("{:0>4}    |                         {s} {}\n", .{
                    new_offset - 2,
                    if (is_local == 1) "local" else "upvalue",
                    index,
                });
            }

            return new_offset;
        },
        .close_upvalue => simpleInstruction("CLOSE_UPVALUE", offset, io),
        .loop => jumpInstruction("LOOP", -1, chunk, offset, io),
        .constant => constantInstruction(chunk, "CONSTANT", offset, io),
        .constant_long => constantLongInstruction(chunk, "CONSTANT_LONG", offset, io),
        .negate => simpleInstruction("NEGATE", offset, io),
        .print => simpleInstruction("PRINT", offset, io),
        .jump => jumpInstruction("JUMP", 1, chunk, offset, io),
        .jump_if_false => jumpInstruction("JUMP_IF_FALSE", 1, chunk, offset, io),
        .add => simpleInstruction("ADD", offset, io),
        .in => simpleInstruction("IN", offset, io),
        .nil => simpleInstruction("NIL", offset, io),
        .true => simpleInstruction("TRUE", offset, io),
        .false => simpleInstruction("FALSE", offset, io),
        .pop => simpleInstruction("POP", offset, io),
        .duplicate => simpleInstruction("DUPLICATE", offset, io),
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
        .get_upvalue => byteInstruction("GET_UPVALUE", chunk, offset, io),
        .set_upvalue => byteInstruction("SET_UPVALUE", chunk, offset, io),
        .get_property => constantInstruction(chunk, "GET_PROPERTY", offset, io),
        .set_property => constantInstruction(chunk, "SET_PROPERTY", offset, io),
        .define_global => constantInstruction(chunk, "DEFINE_GLOBAL", offset, io),
        .define_global_long => constantLongInstruction(chunk, "DEFINE_GLOBAL_LONG", offset, io),
        .greater => simpleInstruction("GREATER", offset, io),
        .less => simpleInstruction("LESS", offset, io),
        .invoke => invokeInstruction("INVOKE", chunk, offset, io),
        _ => blk: {
            io.print("{s: <18} Error: invalid opcode, got {}\n", .{ "?", instruction });
            break :blk offset + 1;
        },
    };
}

fn constantInstruction(chunk: *const Chunk, name: []const u8, offset: usize, io: *IoHandler) usize {
    const index = chunk.readByte(offset + 1);
    io.print("{s: <18} {: <4} '", .{ name, index });
    extractConstant(chunk, index).print(io);
    io.print("'\n", .{});

    return offset + 2;
}

fn constantLongInstruction(chunk: *const Chunk, name: []const u8, offset: usize, io: *IoHandler) usize {
    const left: u24 = chunk.readByte(offset + 1);
    const middle: u24 = chunk.readByte(offset + 2);
    const right = chunk.readByte(offset + 3);
    const index: usize = (left << 16) | (middle << 8) | right;

    io.print("{s: <18} {: <4} '", .{ name, index });
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
    io.print("{s: <18} {: <4}\n", .{ name, slot });
    return offset + 2;
}

fn readU24(chunk: *const Chunk, offset: usize) u24 {
    const left: u24 = @intCast(chunk.code.data[offset]);
    const middle: u24 = @intCast(chunk.code.data[offset + 1]);
    const right: u24 = @intCast(chunk.code.data[offset + 2]);

    return (left << 16) | (middle << 8) | right;
}

fn threeByteInstruction(name: []const u8, chunk: *const Chunk, offset: usize, io: *IoHandler) usize {
    const left: u24 = @intCast(chunk.code.data[offset + 1]);
    const middle: u24 = @intCast(chunk.code.data[offset + 2]);
    const right: u24 = @intCast(chunk.code.data[offset + 3]);

    io.print("{s: <18} {: <4}\n", .{ name, (left << 16) | (middle << 8) | right });
    return offset + 4;
}

fn jumpInstruction(name: []const u8, sign: i32, chunk: *const Chunk, offset: usize, io: *IoHandler) usize {
    const left: u16 = @intCast(chunk.code.data[offset + 1]);
    const right: u16 = @intCast(chunk.code.data[offset + 2]);
    const jump = (left << 8) | right;

    io.print("{s: <18} {: <4} -> {}\n", .{
        name,
        offset,
        @as(i32, @intCast(offset)) + 3 + sign * @as(i32, @intCast(jump)),
    });

    return offset + 3;
}

fn invokeInstruction(name: []const u8, chunk: *const Chunk, offset: usize, io: *IoHandler) usize {
    const constant = chunk.code.data[offset + 1];
    const arg_count = chunk.code.data[offset + 2];

    io.print("{s: <18} ({} args) {: >4} '", .{
        name,
        arg_count,
        constant,
    });
    chunk.constants.data[constant].print(io);
    io.print("'\n", .{});

    return offset + 3;
}

fn extractConstant(chunk: *const Chunk, index: usize) Value {
    return chunk.constants.data[index];
}
