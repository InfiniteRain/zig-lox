const std = @import("std");
const GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator;
const chunk = @import("chunk.zig");
const OpCode = chunk.OpCode;
const Chunk = chunk.Chunk;
const debug = @import("debug.zig");
const Disassmbler = debug.Disassembler;

pub fn main() !void {
    var gpa = GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var my_chunk = try Chunk.init(allocator);
    defer my_chunk.deinit();

    const constant = try my_chunk.write_constant(1.12);

    try my_chunk.write_opcode(OpCode.constant, 123);
    try my_chunk.write_opcode(OpCode{ .operand = @intCast(constant) }, 123);
    try my_chunk.write_opcode(OpCode.ret, 123);

    var disassmbler = Disassmbler{ .chunk = &my_chunk };
    disassmbler.disassemble_chunk("test chunk");
}
