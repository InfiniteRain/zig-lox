const std = @import("std");
const mem = std.mem;
const io_handler_package = @import("io_handler.zig");
const IoHandler = io_handler_package.IoHandler;

pub const TokenType = enum {
    left_paren,
    right_paren,
    left_brace,
    right_brace,
    colon,
    comma,
    dot,
    minus,
    plus,
    semicolon,
    slash,
    star,

    bang,
    bang_equal,
    equal,
    equal_equal,
    greater,
    greater_equal,
    less,
    less_equal,

    identifier,
    string,
    number,

    _and,
    case,
    class,
    _const,
    default,
    _else,
    false,
    _for,
    fun,
    _if,
    nil,
    _or,
    print,
    _return,
    super,
    this,
    true,
    _var,
    _while,
    _switch,

    _error,
    eof,
};

pub const Token = struct {
    const Self = @This();

    type: TokenType,
    lexeme: []const u8,
    line: u64,

    pub fn lexemeEquals(self: *const Self, other: *const Self) bool {
        return mem.eql(u8, self.lexeme, other.lexeme);
    }
};

pub const Scanner = struct {
    const Self = @This();

    source: []const u8,
    start: usize,
    current: usize,
    line: u64,
    io: *IoHandler,

    pub fn init(source: []const u8, io: *IoHandler) Self {
        return .{
            .source = source,
            .start = 0,
            .current = 0,
            .line = 1,
            .io = io,
        };
    }

    pub fn scanToken(self: *Self) Token {
        self.skipWhitespace();
        self.start = self.current;

        if (self.isAtEnd()) {
            return self.makeEofToken();
        }

        const char = self.advance();

        if (Self.isAlpha(char)) {
            return self.identifier();
        }

        if (Self.isDigit(char)) {
            return self.number();
        }

        return switch (char) {
            '(' => self.makeToken(.left_paren),
            ')' => self.makeToken(.right_paren),
            '{' => self.makeToken(.left_brace),
            '}' => self.makeToken(.right_brace),
            ';' => self.makeToken(.semicolon),
            ',' => self.makeToken(.comma),
            '.' => self.makeToken(.dot),
            '-' => self.makeToken(.minus),
            '+' => self.makeToken(.plus),
            '/' => self.makeToken(.slash),
            '*' => self.makeToken(.star),
            ':' => self.makeToken(.colon),
            '!' => self.makeToken(if (self.match('=')) .bang_equal else .bang),
            '=' => self.makeToken(if (self.match('=')) .equal_equal else .equal),
            '<' => self.makeToken(if (self.match('=')) .less_equal else .less),
            '>' => self.makeToken(if (self.match('=')) .greater_equal else .greater),
            '"' => self.string(),
            else => self.makeErrorToken("Unexpected character"),
        };
    }

    fn isAlpha(char: u8) bool {
        return (char >= 'a' and char <= 'z') or
            (char >= 'A' and char <= 'Z') or
            char == '_';
    }

    fn isDigit(char: u8) bool {
        return char >= '0' and char <= '9';
    }

    fn isAtEnd(self: *Self) bool {
        return self.current >= self.source.len;
    }

    fn advance(self: *Self) u8 {
        const char = self.source[self.current];
        self.current += 1;
        return char;
    }

    fn peek(self: *Self) u8 {
        if (self.isAtEnd()) {
            return 0;
        }

        return self.source[self.current];
    }

    fn peekNext(self: *Self) u8 {
        if (self.current + 1 >= self.source.len) {
            return 0;
        }

        return self.source[self.current + 1];
    }

    fn match(self: *Self, expected: u8) bool {
        if (self.isAtEnd()) {
            return false;
        }

        if (self.source[self.current] != expected) {
            return false;
        }

        self.current += 1;
        return true;
    }

    fn makeToken(self: *Self, _type: TokenType) Token {
        return .{
            .type = _type,
            .lexeme = self.source[self.start..self.current],
            .line = self.line,
        };
    }

    fn makeEofToken(self: *Self) Token {
        return .{
            .type = .eof,
            .lexeme = "",
            .line = self.line,
        };
    }

    fn makeErrorToken(self: *Self, message: []const u8) Token {
        return .{
            .type = ._error,
            .lexeme = message,
            .line = self.line,
        };
    }

    fn identifier(self: *Self) Token {
        while (Self.isAlpha(self.peek()) or Self.isDigit(self.peek())) {
            _ = self.advance();
        }

        return self.makeToken(self.identifierType());
    }

    fn skipWhitespace(self: *Self) void {
        while (true) {
            switch (self.peek()) {
                ' ', '\r', '\t' => {
                    _ = self.advance();
                },
                '\n' => {
                    self.line += 1;
                    _ = self.advance();
                },
                '/' => {
                    if (self.peekNext() != '/') {
                        return;
                    }

                    while (self.peek() != '\n' and !self.isAtEnd()) {
                        _ = self.advance();
                    }
                },
                else => {
                    break;
                },
            }
        }
    }

    fn checkKeyword(self: *Self, start: usize, rest: []const u8, _type: TokenType) TokenType {
        if (self.current - self.start == start + rest.len and
            mem.eql(u8, rest, self.source[self.start + start .. self.start + rest.len + start]))
        {
            return _type;
        }

        return .identifier;
    }

    fn identifierType(self: *Self) TokenType {
        return switch (self.source[self.start]) {
            'a' => self.checkKeyword(1, "nd", ._and),
            'c' => {
                return if (self.current - self.start > 1)
                    switch (self.source[self.start + 1]) {
                        'a' => self.checkKeyword(2, "se", .case),
                        'l' => self.checkKeyword(2, "ass", .class),
                        'o' => self.checkKeyword(2, "nst", ._const),
                        else => .identifier,
                    }
                else
                    .identifier;
            },
            'd' => self.checkKeyword(1, "efault", .default),
            'e' => self.checkKeyword(1, "lse", ._else),
            'f' => {
                return if (self.current - self.start > 1)
                    switch (self.source[self.start + 1]) {
                        'a' => self.checkKeyword(2, "lse", .false),
                        'o' => self.checkKeyword(2, "r", ._for),
                        'u' => self.checkKeyword(2, "n", .fun),
                        else => .identifier,
                    }
                else
                    .identifier;
            },
            'i' => self.checkKeyword(1, "f", ._if),
            'n' => self.checkKeyword(1, "il", .nil),
            'o' => self.checkKeyword(1, "r", ._or),
            'p' => self.checkKeyword(1, "rint", .print),
            'r' => self.checkKeyword(1, "eturn", ._return),
            's' => {
                return if (self.current - self.start > 1)
                    switch (self.source[self.start + 1]) {
                        'u' => self.checkKeyword(2, "per", .super),
                        'w' => self.checkKeyword(2, "itch", ._switch),
                        else => .identifier,
                    }
                else
                    .identifier;
            },
            't' => {
                return if (self.current - self.start > 1)
                    switch (self.source[self.start + 1]) {
                        'h' => self.checkKeyword(2, "is", .this),
                        'r' => self.checkKeyword(2, "ue", .true),
                        else => .identifier,
                    }
                else
                    .identifier;
            },
            'v' => self.checkKeyword(1, "ar", ._var),
            'w' => self.checkKeyword(1, "hile", ._while),
            else => .identifier,
        };
    }

    fn number(self: *Self) Token {
        while (Self.isDigit(self.peek())) {
            _ = self.advance();
        }

        if (self.peek() == '.' and Self.isDigit(self.peekNext())) {
            _ = self.advance();

            while (Self.isDigit(self.peek())) {
                _ = self.advance();
            }
        }

        return self.makeToken(.number);
    }

    fn string(self: *Self) Token {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') {
                self.line += 1;
            }

            _ = self.advance();
        }

        if (self.isAtEnd()) {
            return self.makeErrorToken("Unterminated string.");
        }

        _ = self.advance();
        return self.makeToken(.string);
    }
};
