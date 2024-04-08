const std = @import("std");
const Allocator = std.mem.Allocator;
const parseFloat = std.fmt.parseFloat;
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

const ParseFn = *const fn (*Compiler, bool) CompilerError!void;

pub const Local = struct {
    name: Token,
    depth: usize,
    initialized: bool,
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
        array.set(.identifier, .{ .prefix = Self.variable });
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
    const u8_count = std.math.maxInt(u8) + 1;

    allocator: Allocator,
    scanner: Scanner,
    vm: *VM,
    current: Token,
    previous: Token,
    compiling_chunk: *Chunk,
    had_error: bool,
    panic_mode: bool,
    io: *IoHandler,

    locals: [u8_count]Local,
    local_count: usize,
    scope_depth: usize,

    pub fn init(allocator: Allocator, vm: *VM, io: *IoHandler) !Self {
        return .{
            .allocator = allocator,
            .scanner = undefined,
            .vm = vm,
            .current = undefined,
            .previous = undefined,
            .compiling_chunk = undefined,
            .had_error = false,
            .panic_mode = false,
            .io = io,

            .locals = undefined,
            .local_count = 0,
            .scope_depth = 0,
        };
    }

    pub fn deinit(self: *Self) void {
        _ = self;
    }

    pub fn compile(self: *Self, source: []const u8, chunk: *Chunk) CompilerError!bool {
        self.scanner = Scanner.init(source, self.io);
        self.compiling_chunk = chunk;
        self.advance();

        while (!self.check(.eof)) {
            try self.declaration();
        }

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

    fn check(self: *Self, token_type: TokenType) bool {
        return self.current.type == token_type;
    }

    fn match(self: *Self, token_type: TokenType) bool {
        if (!self.check(token_type)) {
            return false;
        }

        self.advance();
        return true;
    }

    fn emitByte(self: *Self, byte: anytype) CompilerError!void {
        const ByteType = @TypeOf(byte);

        switch (ByteType) {
            @TypeOf(.enum_literal), OpCode => {
                if (ByteType == @TypeOf(.enum_literal) and !@hasField(OpCode, @tagName(byte))) {
                    @compileError("expected valid OpCode");
                }

                try self.currentChunk().writeOpCode(byte, self.previous.line);
            },
            comptime_int, u8 => {
                try self.currentChunk().writeByte(byte, self.previous.line);
            },
            else => {
                @compileError("expected byte to be of type OpCode or u8, found" ++ @typeName(ByteType));
            },
        }
    }

    fn emitBytes(self: *Self, bytes: anytype) CompilerError!void {
        const BytesType = @TypeOf(bytes);
        const bytes_type_info = @typeInfo(BytesType);

        if (bytes_type_info != .Struct or !bytes_type_info.Struct.is_tuple) {
            @compileError("expected tuple, found " ++ @typeName(BytesType));
        }

        inline for (bytes_type_info.Struct.fields) |field| {
            const union_index = comptime std.fmt.parseInt(usize, field.name, 10) catch unreachable;
            try self.emitByte(bytes[union_index]);
        }
    }

    fn emitReturn(self: *Self) CompilerError!void {
        try self.emitByte(.ret);
    }

    fn emitConstant(self: *Self, value: Value) CompilerError!void {
        try self.currentChunk().writeConstant(value, self.previous.line);
    }

    fn emitConstantIndex(self: *Self, index: usize) CompilerError!void {
        try self.currentChunk().writeConstantIndex(index, self.previous.line);
    }

    fn endCompiler(self: *Self) CompilerError!void {
        try self.emitReturn();

        if (comptime exe_options.print_code) {
            disassembleChunk(self.currentChunk(), "code", self.io) catch unreachable;
        }
    }

    fn beginScope(self: *Self) void {
        self.scope_depth += 1;
    }

    fn endScope(self: *Self) CompilerError!void {
        self.scope_depth -= 1;

        while (self.local_count > 0 and self.locals[self.local_count - 1].depth > self.scope_depth) {
            try self.emitByte(.pop);
            self.local_count -= 1;
        }
    }

    fn binary(self: *Self, canAssign: bool) CompilerError!void {
        _ = canAssign;

        const operator_type = self.previous.type;
        const rule = rules.get(operator_type);
        try self.parsePrecedence(@enumFromInt(@intFromEnum(rule.precedence) + 1));

        switch (operator_type) {
            .plus => try self.emitByte(.add),
            .minus => try self.emitByte(.subtract),
            .star => try self.emitByte(.multiply),
            .slash => try self.emitByte(.divide),
            .bang_equal => try self.emitBytes(.{ .equal, .not }),
            .equal_equal => try self.emitByte(.equal),
            .greater => try self.emitByte(.greater),
            .greater_equal => try self.emitBytes(.{ .less, .not }),
            .less => try self.emitByte(.less),
            .less_equal => try self.emitBytes(.{ .greater, .not }),
            else => unreachable,
        }
    }

    fn literal(self: *Self, canAssign: bool) CompilerError!void {
        _ = canAssign;

        switch (self.previous.type) {
            .false => try self.emitByte(.false),
            .nil => try self.emitByte(.nil),
            .true => try self.emitByte(.true),
            else => unreachable,
        }
    }

    fn expression(self: *Self) CompilerError!void {
        try self.parsePrecedence(.assignment);
    }

    fn block(self: *Self) CompilerError!void {
        while (!self.check(.right_brace) and !self.check(.eof)) {
            try self.declaration();
        }

        self.consume(.right_brace, "Expect '}' after block.");
    }

    fn varDeclaration(self: *Self) CompilerError!void {
        const global = try self.parseVariable("Expect variable name");

        if (self.match(.equal)) {
            try self.expression();
        } else {
            try self.emitByte(.nil);
        }

        self.consume(.semicolon, "Expect ';' after variable declaration.");
        try self.defineVariable(global);
    }

    fn expressionStatement(self: *Self) CompilerError!void {
        try self.expression();
        self.consume(.semicolon, "Expect ';' after expression.");
        try self.emitByte(.pop);
    }

    fn printStatement(self: *Self) CompilerError!void {
        try self.expression();
        self.consume(.semicolon, "Expect ';' after value.");
        try self.emitByte(.print);
    }

    fn synchronize(self: *Self) void {
        self.panic_mode = false;

        while (self.current.type != .eof) {
            if (self.previous.type == .semicolon) {
                return;
            }

            switch (self.current.type) {
                .class, .fun, ._var, ._for, ._if, ._while, .print, ._return => return,
                else => {
                    self.advance();
                },
            }
        }
    }

    fn declaration(self: *Self) CompilerError!void {
        if (self.match(._var)) {
            try self.varDeclaration();
        } else {
            try self.statement();
        }

        if (self.panic_mode) {
            self.synchronize();
        }
    }

    fn statement(self: *Self) CompilerError!void {
        if (self.match(.print)) {
            try self.printStatement();
        } else if (self.match(.left_brace)) {
            self.beginScope();
            try self.block();
            try self.endScope();
        } else {
            try self.expressionStatement();
        }
    }

    fn grouping(self: *Self, canAssign: bool) CompilerError!void {
        _ = canAssign;

        try self.expression();
        self.consume(.right_paren, "Expect ')' after expression.");
    }

    fn number(self: *Self, canAssign: bool) CompilerError!void {
        _ = canAssign;

        const value = parseFloat(f64, self.previous.lexeme) catch unreachable;
        try self.emitConstant(.{ .number = value });
    }

    fn string(self: *Self, canAssign: bool) CompilerError!void {
        _ = canAssign;

        const string_obj = try Obj.String.fromBufAlloc(
            self.allocator,
            self.previous.lexeme[1 .. self.previous.lexeme.len - 1],
            self.vm,
        );

        try self.emitConstant(.{
            .obj = &string_obj.obj,
        });
    }

    fn namedVariable(self: *Self, name: Token, canAssign: bool) CompilerError!void {
        const arg = self.resolveLocal(&name);
        var constant_index: usize = undefined;
        var get_op: OpCode = undefined;
        var set_op: OpCode = undefined;

        if (arg) |i| {
            constant_index = i;
            get_op = .get_local;
            set_op = .set_local;
        } else {
            constant_index = try self.identifierConstant(&name);
            get_op = if (constant_index < 0xFF) .get_global else .get_global_long;
            set_op = if (constant_index < 0xFF) .set_global else .set_global_long;
        }

        if (canAssign and self.match(.equal)) {
            try self.expression();
            try self.emitByte(set_op);
        } else {
            try self.emitByte(get_op);
        }

        try self.emitConstantIndex(constant_index);
    }

    fn variable(self: *Self, canAssign: bool) CompilerError!void {
        try self.namedVariable(self.previous, canAssign);
    }

    fn unary(self: *Self, canAssign: bool) CompilerError!void {
        _ = canAssign;

        const operator_type = self.previous.type;

        try self.parsePrecedence(.unary);

        switch (operator_type) {
            .bang => try self.emitByte(.not),
            .minus => try self.emitByte(.negate),
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

        const canAssign = @intFromEnum(precedence) <= @intFromEnum(Precedence.assignment);
        try prefixRule.?(self, canAssign);

        while (@intFromEnum(precedence) <= @intFromEnum(rules.get(self.current.type).precedence)) {
            self.advance();
            const infixRule = rules.get(self.previous.type).infix;
            try infixRule.?(self, canAssign);
        }

        if (canAssign and self.match(.equal)) {
            self.err("Invalid assignment target.");
        }
    }

    fn identifierConstant(self: *Self, token: *const Token) CompilerError!usize {
        return try self.currentChunk().addConstant(.{
            .obj = &(try Obj.String.fromBufAlloc(self.allocator, token.lexeme, self.vm)).obj,
        });
    }

    fn resolveLocal(self: *Self, name: *const Token) ?usize {
        if (self.local_count > 0) {
            var i: usize = self.local_count - 1;

            while (i >= 0) : (i -= 1) {
                const local = &self.locals[i];

                if (name.lexemeEquals(&local.name)) {
                    if (!local.initialized) {
                        self.err("Can't read local variable in its own initializer.");
                    }

                    return i;
                }

                if (i <= 0) {
                    break;
                }
            }
        }

        return null;
    }

    fn addLocal(self: *Self, name: Token) void {
        if (self.local_count == u8_count) {
            self.err("Too many local variables in a function.");
            return;
        }

        var local = &self.locals[self.local_count];
        local.name = name;
        local.depth = self.scope_depth;
        local.initialized = false;
        self.local_count += 1;
    }

    fn declareVariable(self: *Self) void {
        if (self.scope_depth == 0) {
            return;
        }

        const name = &self.previous;

        if (self.local_count > 0) {
            var i: usize = self.local_count - 1;

            while (i >= 0) : (i -= 1) {
                const local = &self.locals[i];

                if (local.initialized and self.scope_depth > local.depth) {
                    break;
                }

                if (name.lexemeEquals(&local.name)) {
                    self.err("Already a variable with this name in this scope.");
                }

                if (i <= 0) {
                    break;
                }
            }
        }

        self.addLocal(name.*);
    }

    fn parseVariable(self: *Self, errorMessage: []const u8) CompilerError!usize {
        self.consume(.identifier, errorMessage);
        self.declareVariable();

        if (self.scope_depth > 0) {
            return 0;
        }

        return try self.identifierConstant(&self.previous);
    }

    fn markInitialized(self: *Self) void {
        self.locals[self.local_count - 1].initialized = true;
    }

    fn defineVariable(self: *Self, global: usize) CompilerError!void {
        if (self.scope_depth > 0) {
            self.markInitialized();
            return;
        }

        if (global < 0xFF) {
            try self.emitByte(.define_global);
        } else {
            try self.emitByte(.define_global_long);
        }

        try self.emitConstantIndex(global);
    }

    fn errAtCurrent(self: *Self, message: []const u8) void {
        self.errAt(&self.current, message);
    }

    fn err(self: *Self, message: []const u8) void {
        self.errAt(&self.previous, message);
    }

    fn errAt(self: *Self, token: *Token, message: []const u8) void {
        if (self.panic_mode) {
            return;
        }

        self.panic_mode = true;
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
