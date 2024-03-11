const std = @import("std");
const debug = std.debug;
const print = debug.print;
const chunk = @import("chunk.zig");
const OpCode = chunk.OpCode;
const Chunk = chunk.Chunk;
const value = @import("value.zig");
const Value = value.Value;

pub const Disassembler = struct {
    const Self = @This();

    chunk: *Chunk,

    pub fn disassemble_chunk(self: *Self, name: []const u8) void {
        print("== {s} ==\n", .{name});

        var offset: usize = 0;

        while (offset < self.chunk.code.count) {
            offset = self.disassemble_instruction(offset);
        }
    }

    fn disassemble_instruction(self: *Self, offset: usize) usize {
        print("{:0>4} ", .{offset});

        if (offset > 0 and self.chunk.read_line(offset) == self.chunk.read_line(offset - 1)) {
            print("    | ", .{});
        } else {
            print("{:0>4}| ", .{self.chunk.read_line(offset)});
        }

        const instruction = self.chunk.code.data[offset];

        return switch (instruction) {
            OpCode.ret => Self.simple_instruction("RET", offset),
            OpCode.constant => self.constant_instruction("CONSTANT", offset),
            else => blk: {
                print("{s: <16} Error: invalid opcode, got {}\n", .{ "?", instruction });
                break :blk offset + 1;
            },
        };
    }

    fn constant_instruction(self: *Self, name: []const u8, offset: usize) usize {
        if (self.extract_operand(name, offset + 1)) |constant| {
            print("{s: <16} {:0>4} '", .{ name, constant });
            Self.plain_value(self.extract_constant(constant));
            print("'\n", .{});
        }

        return offset + 2;
    }

    fn simple_instruction(name: []const u8, offset: usize) usize {
        print("{s}\n", .{name});
        return offset + 1;
    }

    fn plain_value(val: Value) void {
        print("{d}", .{val});
    }

    fn extract_operand(self: *Self, name: []const u8, offset: usize) ?u8 {
        const instruction = self.chunk.code.data[offset];
        return switch (instruction) {
            OpCode.operand => |val| val,
            else => {
                print("{s: <16} Error: no operand at offset {:0>4}, got {}\n", .{ name, offset, instruction });
                return null;
            },
        };
    }

    fn extract_constant(self: *Self, index: u8) Value {
        return self.chunk.constants.data[index];
    }
};
