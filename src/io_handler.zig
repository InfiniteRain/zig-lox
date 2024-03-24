const std = @import("std");
const Allocator = std.mem.Allocator;
const io = std.io;
const Writer = std.fs.File.Writer;
const Reader = std.fs.File.Reader;
const memory_package = @import("memory.zig");
const alloc = memory_package.alloc;
const free = memory_package.free;

pub const IoHandler = struct {
    const Self = @This();
    const buffer_length = 1024;

    allocator: Allocator,
    write_buffer: []u8,
    write_buffer_stream: io.FixedBufferStream([]u8),
    stdin: Reader,
    stdout: Writer,
    stderr: Writer,

    pub fn init(allocator: Allocator) !IoHandler {
        var write_buffer = try alloc(u8, allocator, buffer_length);
        var write_buffer_stream = io.fixedBufferStream(write_buffer);

        const stdin = io.getStdIn().reader();
        const stdout = io.getStdOut().writer();
        const stderr = io.getStdErr().writer();

        return .{
            .allocator = allocator,
            .write_buffer = write_buffer,
            .write_buffer_stream = write_buffer_stream,
            .stdin = stdin,
            .stdout = stdout,
            .stderr = stderr,
        };
    }

    pub fn deinit(self: *const Self) void {
        free(self.allocator, self.write_buffer);
    }

    pub fn print(self: *const Self, comptime format: []const u8, args: anytype) void {
        self.stdout.print(format, args) catch {};
    }

    pub fn err(self: *const Self, comptime format: []const u8, args: anytype) void {
        self.stderr.print(format, args) catch {};
    }

    pub fn readLine(self: *Self) ![]const u8 {
        self.write_buffer_stream.reset();
        try self.stdin.streamUntilDelimiter(self.write_buffer_stream.writer(), '\n', buffer_length);
        return self.write_buffer_stream.getWritten();
    }
};
