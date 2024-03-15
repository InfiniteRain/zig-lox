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

    for (0..257) |i| {
        try my_chunk.write_constant(10, @intCast(i));
    }
    try my_chunk.write_opcode(OpCode.ret, 257);

    var disassmbler = Disassmbler{ .chunk = &my_chunk };
    try disassmbler.disassemble_chunk("test chunk");
}
