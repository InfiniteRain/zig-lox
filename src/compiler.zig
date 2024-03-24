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

const ParseFn = *const fn (*Compiler) void;

pub const CompilerError = error{
    CompilationFailed,
};

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
        array.set(.string, .{});
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

    scanner: Scanner,
    current: Token,
    previous: Token,
    compiling_chunk: *Chunk,
    had_error: bool,
    had_panic: bool,
    io: *IoHandler,

    pub fn init(allocator: Allocator, io: *IoHandler) !Self {
        _ = allocator;
        return .{
            .scanner = undefined,
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

    pub fn currentChunk(self: *Self) *Chunk {
        return self.compiling_chunk;
    }

    pub fn compile(self: *Self, source: []const u8, chunk: *Chunk) !bool {
        self.scanner = Scanner.init(source, self.io);
        self.compiling_chunk = chunk;
        self.advance();
        self.expression();
        self.consume(.eof, "Expect end of expression.");
        self.endCompiler();

        return !self.had_error;
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

    fn emitOpCodes(self: *Self, op_code1: OpCode, op_code2: OpCode) void {
        self.emitOpCode(op_code1);
        self.emitOpCode(op_code2);
    }

    fn emitOpCode(self: *Self, op_code: OpCode) void {
        self.compiling_chunk.writeOpCode(op_code, self.previous.line) catch {
            // todo: handle error
        };
    }

    fn emitReturn(self: *Self) void {
        self.emitOpCode(.ret);
    }

    fn emitConstant(self: *Self, value: Value) void {
        self.compiling_chunk.writeConstant(value, self.previous.line) catch {
            // todo: handle error
        };
    }

    fn endCompiler(self: *Self) void {
        self.emitReturn();

        if (comptime exe_options.print_code) {
            disassembleChunk(self.currentChunk(), "code", self.io) catch unreachable;
        }
    }

    fn binary(self: *Self) void {
        const operator_type = self.previous.type;
        const rule = rules.get(operator_type);
        self.parsePrecedence(rule.precedence);

        switch (operator_type) {
            .plus => self.emitOpCode(.add),
            .minus => self.emitOpCode(.subtract),
            .star => self.emitOpCode(.multiply),
            .slash => self.emitOpCode(.divide),
            .bang_equal => self.emitOpCodes(.equal, .not),
            .equal_equal => self.emitOpCode(.equal),
            .greater => self.emitOpCode(.greater),
            .greater_equal => self.emitOpCodes(.less, .not),
            .less => self.emitOpCode(.less),
            .less_equal => self.emitOpCodes(.greater, .not),
            else => unreachable,
        }
    }

    fn literal(self: *Self) void {
        switch (self.previous.type) {
            .false => self.emitOpCode(.false),
            .nil => self.emitOpCode(.nil),
            .true => self.emitOpCode(.true),
            else => unreachable,
        }
    }

    fn expression(self: *Self) void {
        self.parsePrecedence(.assignment);
    }

    fn grouping(self: *Self) void {
        self.expression();
        self.consume(.right_paren, "Expect ')' after expression.");
    }

    fn number(self: *Self) void {
        const value = parseFloat(f64, self.previous.lexeme) catch {
            // todo: properly handle the error
            self.emitConstant(.{ .number = 0.0 });
            return;
        };
        self.emitConstant(.{ .number = value });
    }

    fn unary(self: *Self) void {
        const operator_type = self.previous.type;

        self.parsePrecedence(.unary);

        switch (operator_type) {
            .bang => self.emitOpCode(.not),
            .minus => self.emitOpCode(.negate),
            else => return,
        }
    }

    fn parsePrecedence(self: *Self, precedence: Precedence) void {
        self.advance();

        const prefixRule = rules.get(self.previous.type).prefix;

        if (prefixRule == null) {
            self.err("Expect expression.");
            return;
        }

        prefixRule.?(self);

        while (@intFromEnum(precedence) <= @intFromEnum(rules.get(self.current.type).precedence)) {
            self.advance();
            const infixRule = rules.get(self.previous.type).infix;
            infixRule.?(self);
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
