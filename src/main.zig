const std = @import("std");
const GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator;
const chunk_package = @import("chunk.zig");
const OpCode = chunk_package.OpCode;
const Chunk = chunk_package.Chunk;
const debug_package = @import("debug.zig");
const disassembleChunk = debug_package.disassembleChunk;
const vm_package = @import("vm.zig");
const VM = vm_package.VM;

pub fn main() !void {
    var gpa = GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var vm = try VM.init(allocator);
    defer vm.deinit();

    var chunk = try Chunk.init(allocator);
    defer chunk.deinit();

    try chunk.writeConstant(1.2, 123);
    try chunk.writeConstant(3.4, 123);
    try chunk.writeOpCode(OpCode.add, 123);

    try chunk.writeConstant(5.6, 123);
    try chunk.writeOpCode(OpCode.divide, 123);

    try chunk.writeOpCode(OpCode.negate, 1);
    try chunk.writeOpCode(OpCode.ret, 1);

    _ = vm.interpret(&chunk);
}
