const std = @import("std");
const parseFloat = std.fmt.parseFloat;
const Allocator = std.mem.Allocator;
const EnumArray = std.EnumArray;
const scanner_package = @import("scanner.zig");
const Scanner = scanner_package.Scanner;
const TokenType = scanner_package.TokenType;
const Token = scanner_package.Token;
const chunk_package = @import("chunk.zig");
const Chunk = chunk_package.Chunk;
const OpCode = chunk_package.OpCode;
const io_handler_package = @import("io_handler.zig");
const IoHandler = io_handler_package.IoHandler;
const value_package = @import("value.zig");
const Value = value_package.Value;
const exe_options = @import("exe_options");
const debug_package = @import("debug.zig");
const disassembleChunk = debug_package.disassembleChunk;
const object_package = @import("object.zig");
const Obj = object_package.Obj;
const vm_package = @import("vm.zig");
const VM = vm_package.VM;

const Precedence = enum(u8) {
    none,
    assignment,
    _or,
    _and,
    equality,
    comparison,
    term,
    factor,
    unary,
    call,
    primary,
};

const ParseRule = struct {
    prefix: ?ParseFn = null,
    infix: ?ParseFn = null,
    precedence: Precedence = .none,
};

const CompilerError = error{OutOfMemory};

const ParseFn = *const fn (*Compiler) CompilerError!void;

pub const Compiler = struct {
    const Self = @This();
    const rules = blk: {
        var array = EnumArray(TokenType, ParseRule).initUndefined();
        array.set(.left_paren, .{ .prefix = Self.grouping });
        array.set(.right_paren, .{});
        array.set(.left_brace, .{});
        array.set(.right_brace, .{});
        array.set(.comma, .{});
        array.set(.dot, .{});
        array.set(.minus, .{ .prefix = Self.unary, .infix = Self.binary, .precedence = .term });
        array.set(.plus, .{ .infix = Self.binary, .precedence = .term });
        array.set(.semicolon, .{});
        array.set(.slash, .{ .infix = Self.binary, .precedence = .factor });
        array.set(.star, .{ .infix = Self.binary, .precedence = .factor });
        array.set(.bang, .{ .prefix = Self.unary });
        array.set(.bang_equal, .{ .prefix = Self.binary, .precedence = .equality });
        array.set(.equal, .{});
        array.set(.equal_equal, .{ .infix = Self.binary, .precedence = .equality });
        array.set(.greater, .{ .infix = Self.binary, .precedence = .comparison });
        array.set(.greater_equal, .{ .infix = Self.binary, .precedence = .comparison });
        array.set(.less, .{ .infix = Self.binary, .precedence = .comparison });
        array.set(.less_equal, .{ .infix = Self.binary, .precedence = .comparison });
        array.set(.identifier, .{});
        array.set(.string, .{ .prefix = Self.string });
        array.set(.number, .{ .prefix = Self.number });
        array.set(._and, .{});
        array.set(.class, .{});
        array.set(._else, .{});
        array.set(.false, .{ .prefix = Self.literal });
        array.set(._for, .{});
        array.set(.fun, .{});
        array.set(._if, .{});
        array.set(.nil, .{ .prefix = Self.literal });
        array.set(._or, .{});
        array.set(.print, .{});
        array.set(._return, .{});
        array.set(.super, .{});
        array.set(.this, .{});
        array.set(.true, .{ .prefix = Self.literal });
        array.set(._var, .{});
        array.set(._while, .{});
        array.set(._error, .{});
        array.set(.eof, .{});
        break :blk array;
    };

    allocator: Allocator,
    scanner: Scanner,
    vm: *VM,
    current: Token,
    previous: Token,
    compiling_chunk: *Chunk,
    had_error: bool,
    had_panic: bool,
    io: *IoHandler,

    pub fn init(allocator: Allocator, vm: *VM, io: *IoHandler) !Self {
        return .{
            .allocator = allocator,
            .scanner = undefined,
            .vm = vm,
            .current = undefined,
            .previous = undefined,
            .compiling_chunk = undefined,
            .had_error = false,
            .had_panic = false,
            .io = io,
        };
    }

    pub fn deinit(self: *Self) void {
        _ = self;
    }

    pub fn compile(self: *Self, source: []const u8, chunk: *Chunk) !bool {
        self.scanner = Scanner.init(source, self.io);
        self.compiling_chunk = chunk;
        self.advance();
        try self.expression();
        self.consume(.eof, "Expect end of expression.");
        try self.endCompiler();

        return !self.had_error;
    }

    fn currentChunk(self: *Self) *Chunk {
        return self.compiling_chunk;
    }

    fn advance(self: *Self) void {
        self.previous = self.current;

        while (true) {
            self.current = self.scanner.scanToken();

            if (self.current.type != ._error) {
                break;
            }

            self.errAtCurrent(self.current.lexeme);
        }
    }

    fn consume(self: *Self, token_type: TokenType, message: []const u8) void {
        if (self.current.type == token_type) {
            self.advance();
            return;
        }

        self.errAtCurrent(message);
    }

    fn emitByte(self: *Self, byte: u8) void {
        self.compiling_chunk.writeByte(byte, self.previous.line);
    }

    fn emitOpCodes(self: *Self, op_code1: OpCode, op_code2: OpCode) CompilerError!void {
        try self.emitOpCode(op_code1);
        try self.emitOpCode(op_code2);
    }

    fn emitOpCode(self: *Self, op_code: OpCode) CompilerError!void {
        try self.compiling_chunk.writeOpCode(op_code, self.previous.line);
    }

    fn emitReturn(self: *Self) CompilerError!void {
        try self.emitOpCode(.ret);
    }

    fn emitConstant(self: *Self, value: Value) CompilerError!void {
        try self.compiling_chunk.writeConstant(value, self.previous.line);
    }

    fn endCompiler(self: *Self) CompilerError!void {
        try self.emitReturn();

        if (comptime exe_options.print_code) {
            disassembleChunk(self.currentChunk(), "code", self.io) catch unreachable;
        }
    }

    fn binary(self: *Self) CompilerError!void {
        const operator_type = self.previous.type;
        const rule = rules.get(operator_type);
        try self.parsePrecedence(@enumFromInt(@intFromEnum(rule.precedence) + 1));

        switch (operator_type) {
            .plus => try self.emitOpCode(.add),
            .minus => try self.emitOpCode(.subtract),
            .star => try self.emitOpCode(.multiply),
            .slash => try self.emitOpCode(.divide),
            .bang_equal => try self.emitOpCodes(.equal, .not),
            .equal_equal => try self.emitOpCode(.equal),
            .greater => try self.emitOpCode(.greater),
            .greater_equal => try self.emitOpCodes(.less, .not),
            .less => try self.emitOpCode(.less),
            .less_equal => try self.emitOpCodes(.greater, .not),
            else => unreachable,
        }
    }

    fn literal(self: *Self) CompilerError!void {
        switch (self.previous.type) {
            .false => try self.emitOpCode(.false),
            .nil => try self.emitOpCode(.nil),
            .true => try self.emitOpCode(.true),
            else => unreachable,
        }
    }

    fn expression(self: *Self) CompilerError!void {
        try self.parsePrecedence(.assignment);
    }

    fn grouping(self: *Self) CompilerError!void {
        try self.expression();
        self.consume(.right_paren, "Expect ')' after expression.");
    }

    fn number(self: *Self) CompilerError!void {
        const value = parseFloat(f64, self.previous.lexeme) catch unreachable;
        try self.emitConstant(.{ .number = value });
    }

    fn string(self: *Self) CompilerError!void {
        const string_obj = try Obj.String.fromBufAlloc(
            self.allocator,
            self.previous.lexeme[1 .. self.previous.lexeme.len - 1],
            self.vm,
        );

        try self.emitConstant(.{
            .obj = &string_obj.obj,
        });
    }

    fn unary(self: *Self) CompilerError!void {
        const operator_type = self.previous.type;

        try self.parsePrecedence(.unary);

        switch (operator_type) {
            .bang => try self.emitOpCode(.not),
            .minus => try self.emitOpCode(.negate),
            else => return,
        }
    }

    fn parsePrecedence(self: *Self, precedence: Precedence) CompilerError!void {
        self.advance();

        const prefixRule = rules.get(self.previous.type).prefix;

        if (prefixRule == null) {
            self.err("Expect expression.");
            return;
        }

        try prefixRule.?(self);

        while (@intFromEnum(precedence) <= @intFromEnum(rules.get(self.current.type).precedence)) {
            self.advance();
            const infixRule = rules.get(self.previous.type).infix;
            try infixRule.?(self);
        }
    }

    fn errAtCurrent(self: *Self, message: []const u8) void {
        self.errAt(&self.current, message);
    }

    fn err(self: *Self, message: []const u8) void {
        self.errAt(&self.previous, message);
    }

    fn errAt(self: *Self, token: *Token, message: []const u8) void {
        if (self.had_panic) {
            return;
        }

        self.had_panic = true;
        self.io.print("[line {}] Error", .{token.line});

        switch (token.type) {
            .eof => self.io.err(" at end", .{}),
            ._error => {},
            else => self.io.err(" at '{s}'", .{token.lexeme}),
        }

        self.io.err(": {s}\n", .{message});
        self.had_error = true;
    }
};
