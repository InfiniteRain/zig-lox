const std = @import("std");
const mem = std.mem;
const debug = std.debug;
const Allocator = mem.Allocator;
const scanner_package = @import("scanner.zig");
const Scanner = scanner_package.Scanner;

pub const Compiler = struct {
    const Self = @This();

    scanner: Scanner,

    pub fn init(allocator: Allocator) !Self {
        _ = allocator;
        return .{
            .scanner = undefined,
        };
    }

    pub fn deinit(self: *Self) void {
        _ = self;
    }

    pub fn compile(self: *Self, source: []const u8) void {
        self.scanner = Scanner.init(source);

        var line: u64 = 0;

        while (true) {
            const token = self.scanner.scanToken();

            if (token.line != line) {
                line = token.line;
                debug.print("{:0>4}| ", .{line});
            } else {
                debug.print("    | ", .{});
            }

            debug.print("{:0>2} '{s}'\n", .{ token.type, token.lexeme });

            if (token.type == .eof) {
                break;
            }
        }
    }
};
