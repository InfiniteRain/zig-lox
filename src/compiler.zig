const std = @import("std");
const mem = std.mem;
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
const dynamic_array_package = @import("dynamic_array.zig");
const DynamicArray = dynamic_array_package.DynamicArray;
const memory_package = @import("memory.zig");
const Memory = memory_package.Memory;

const MarkConstOption = union(enum) {
    no_change,
    change: bool,
};

const Precedence = enum(u8) {
    none,
    assignment,
    _or,
    _and,
    in,
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

const CompilerError = error{
    OutOfMemory,
    CompileError,
};

const ParseFn = *const fn (*Compiler, bool) CompilerError!void;

pub const Local = struct {
    name: Token,
    depth: usize,
    initialized: bool,
    is_const: bool,
    is_captured: bool,
};

pub const Upvalue = struct {
    index: usize,
    is_local: bool,
};

pub const ClassCompiler = struct {
    enclosing: ?*ClassCompiler,
    has_super_class: bool,
};

pub const Compiler = struct {
    const Self = @This();
    const rules = blk: {
        var array = EnumArray(TokenType, ParseRule).initUndefined();
        array.set(.left_paren, .{ .prefix = Self.grouping, .infix = Self.call, .precedence = .call });
        array.set(.right_paren, .{});
        array.set(.left_brace, .{});
        array.set(.right_brace, .{});
        array.set(.comma, .{});
        array.set(._const, .{});
        array.set(.dot, .{ .infix = Self.dot, .precedence = .call });
        array.set(.minus, .{ .prefix = Self.unary, .infix = Self.binary, .precedence = .term });
        array.set(.plus, .{ .infix = Self.binary, .precedence = .term });
        array.set(.semicolon, .{});
        array.set(.slash, .{ .infix = Self.binary, .precedence = .factor });
        array.set(.star, .{ .infix = Self.binary, .precedence = .factor });
        array.set(.bang, .{ .prefix = Self.unary });
        array.set(.bang_equal, .{ .infix = Self.binary, .precedence = .equality });
        array.set(.equal, .{});
        array.set(.equal_equal, .{ .infix = Self.binary, .precedence = .equality });
        array.set(.greater, .{ .infix = Self.binary, .precedence = .comparison });
        array.set(.greater_equal, .{ .infix = Self.binary, .precedence = .comparison });
        array.set(.less, .{ .infix = Self.binary, .precedence = .comparison });
        array.set(.less_equal, .{ .infix = Self.binary, .precedence = .comparison });
        array.set(.identifier, .{ .prefix = Self.variable });
        array.set(.string, .{ .prefix = Self.string });
        array.set(.number, .{ .prefix = Self.number });
        array.set(._and, .{ .infix = Self._and, .precedence = ._and });
        array.set(.class, .{});
        array.set(._else, .{});
        array.set(.false, .{ .prefix = Self.literal });
        array.set(._for, .{});
        array.set(.fun, .{});
        array.set(._if, .{});
        array.set(.nil, .{ .prefix = Self.literal });
        array.set(._or, .{ .infix = Self._or, .precedence = ._or });
        array.set(.print, .{});
        array.set(._return, .{});
        array.set(.super, .{ .prefix = Self.super });
        array.set(.this, .{ .prefix = Self.this });
        array.set(.true, .{ .prefix = Self.literal });
        array.set(._var, .{});
        array.set(._while, .{});
        array.set(._error, .{});
        array.set(.in, .{ .infix = Self.binary, .precedence = .in });
        array.set(.eof, .{});
        break :blk array;
    };
    const u8_count = std.math.maxInt(u8) + 1;
    const u16_max = std.math.maxInt(u16);

    memory: *Memory,

    current_function: ?*Obj.Function,
    current_class: ?*ClassCompiler,
    function_type: Obj.Function.Type,

    enclosing: ?*Compiler,
    scanner: Scanner,
    vm: *VM,
    current: Token,
    previous: Token,
    had_error: bool,
    panic_mode: bool,
    io: *IoHandler,

    locals: DynamicArray(Local),
    local_count: usize,
    upvalues: [256]Upvalue,
    scope_depth: usize,

    loop_depth: ?usize,
    loop_start: usize,

    pub fn create(
        memory: *Memory,
        function_type: Obj.Function.Type,
        vm: *VM,
        io: *IoHandler,
    ) Self {
        const blank_token = Token{ .type = .eof, .lexeme = "", .line = 1 };

        return .{
            .memory = memory,

            .current_function = null,
            .current_class = null,
            .function_type = function_type,

            .enclosing = null,
            .scanner = undefined,
            .vm = vm,
            .current = blank_token,
            .previous = blank_token,
            .had_error = false,
            .panic_mode = false,
            .io = io,

            .locals = undefined,
            .local_count = 0,
            .upvalues = undefined,
            .scope_depth = 0,

            .loop_depth = null,
            .loop_start = 0,
        };
    }

    pub fn init(self: *Self, enclosing_opt: ?*Self) !void {
        const locals = try DynamicArray(Local).init(self.memory);
        const current_function = try Obj.Function.allocNew(self.memory, self.vm);
        const local = &locals.data[0];

        local.depth = 0;
        local.name = .{
            .type = .identifier,
            .lexeme = if (self.function_type != .function) "this" else "",
            .line = 0,
        };
        local.initialized = true;
        local.is_const = true;
        local.is_captured = false;

        self.current_function = current_function;
        self.locals = locals;
        self.local_count = 1;

        if (enclosing_opt) |enclosing| {
            self.enclosing = enclosing;
            self.scanner = enclosing.scanner;
            self.current = enclosing.current;
            self.previous = enclosing.previous;
            self.current_class = enclosing.current_class;
            self.had_error = enclosing.had_error;
        }

        self.vm.current_compiler = self;

        if (enclosing_opt) |enclosing| {
            if (self.function_type != .script) {
                current_function.name = try Obj.String.fromBufAlloc(
                    self.memory,
                    enclosing.previous.lexeme,
                    self.vm,
                );
            }
        }
    }

    pub fn deinit(self: *Self) void {
        self.locals.deinit();
    }

    pub fn compile(self: *Self, scanner: Scanner) CompilerError!*Obj.Function {
        self.scanner = scanner;

        self.advance();

        while (!self.check(.eof)) {
            try self.declaration();
        }

        const function = try self.endCompiler();
        return if (self.had_error) error.CompileError else function;
    }

    fn currentChunk(self: *Self) *Chunk {
        return &self.current_function.?.chunk;
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
                @compileError("expected byte to be of type OpCode or u8, found " ++ @typeName(ByteType));
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

    fn emitLoop(self: *Self, loop_start: usize) CompilerError!void {
        try self.emitByte(.loop);

        const offset = self.currentChunk().code.count - loop_start + 2;
        if (offset > u16_max) {
            self.err("Loop body too large.");
        }

        try self.emitByte(@as(u8, @intCast((offset >> 8) & 0xFF)));
        try self.emitByte(@as(u8, @intCast(offset & 0xFF)));
    }

    fn emitJump(self: *Self, byte: anytype) CompilerError!u16 {
        try self.emitByte(byte);
        try self.emitByte(0xFF);
        try self.emitByte(0xFF);
        return @intCast(self.currentChunk().code.count - 2);
    }

    fn emitReturn(self: *Self) CompilerError!void {
        if (self.function_type == .initializer) {
            try self.emitBytes(.{ .get_local, 0 });
        } else {
            try self.emitByte(.nil);
        }

        try self.emitByte(.ret);
    }

    fn emitConstant(self: *Self, value: Value) CompilerError!void {
        self.currentChunk().writeConstant(value, self.previous.line) catch |e| {
            switch (e) {
                error.TooManyConstants => {
                    self.err("Too many constants in one chunk.");
                },
                error.OutOfMemory => {
                    return error.OutOfMemory;
                },
            }
        };
    }

    fn emitConstantIndex(self: *Self, index: usize) CompilerError!void {
        try self.currentChunk().writeConstantIndex(index, self.previous.line);
    }

    fn patchJump(self: *Self, offset: u16) void {
        const jump = self.currentChunk().code.count - offset - 2;

        if (jump > u16_max) {
            self.err("Too much code to jump over.");
        }

        const jump_converted = @as(u24, @intCast(jump));

        self.currentChunk().code.set(offset, @intCast((jump_converted >> 8) & 0xFF)) catch unreachable;
        self.currentChunk().code.set(offset + 1, @intCast(jump_converted & 0xFF)) catch unreachable;
    }

    fn endCompiler(self: *Self) CompilerError!*Obj.Function {
        try self.emitReturn();
        const function = self.current_function;

        if (comptime exe_options.print_code) {
            disassembleChunk(
                self.currentChunk(),
                if (function.?.name) |name| name.chars else "<script>",
                self.io,
            ) catch unreachable;
        }

        if (self.vm.current_compiler) |compiler| {
            self.vm.current_compiler = compiler.enclosing;
        }

        return function.?;
    }

    fn beginScope(self: *Self) void {
        self.scope_depth += 1;
    }

    fn endScope(self: *Self) CompilerError!void {
        self.scope_depth -= 1;

        while (self.local_count > 0 and self.locals.data[self.local_count - 1].depth > self.scope_depth) {
            if (self.locals.data[self.local_count - 1].is_captured) {
                try self.emitByte(.close_upvalue);
            } else {
                try self.emitByte(.pop);
            }
            self.local_count -= 1;
        }
    }

    fn popLoopScope(self: *Self) CompilerError!void {
        if (self.local_count > 0) {
            var i: usize = self.local_count - 1;

            while (i >= 0) : (i -= 1) {
                const local = &self.locals.data[i];

                if (local.depth > self.loop_depth.?) {
                    try self.emitByte(.pop);
                }

                if (i <= 0) {
                    break;
                }
            }
        }
    }

    fn binary(self: *Self, can_assign: bool) CompilerError!void {
        _ = can_assign;

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
            .in => try self.emitByte(.in),
            else => unreachable,
        }
    }

    fn call(self: *Self, can_assign: bool) CompilerError!void {
        _ = can_assign;

        const arg_count = try self.argumentList();
        try self.emitBytes(.{ .call, arg_count });
    }

    fn dot(self: *Self, can_assign: bool) CompilerError!void {
        self.consume(.identifier, "Expect property name after '.'.");
        const name_result = try self.identifierConstant(&self.previous, .no_change);
        const name: u8 = @intCast(name_result[0]);

        if (can_assign and self.match(.equal)) {
            try self.expression();
            try self.emitBytes(.{ .set_property, name });
        } else if (self.match(.left_paren)) {
            const arg_count = try self.argumentList();
            try self.emitBytes(.{ .invoke, name, arg_count });
        } else {
            try self.emitBytes(.{ .get_property, name });
        }
    }

    fn literal(self: *Self, can_assign: bool) CompilerError!void {
        _ = can_assign;

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

    fn fun(self: *Self, function_type: Obj.Function.Type) !void {
        var compiler = Compiler.create(self.memory, function_type, self.vm, self.io);
        try compiler.init(self);
        defer compiler.deinit();

        compiler.beginScope();
        compiler.consume(.left_paren, "Expect '(' after function name.");

        var current_function = compiler.current_function orelse unreachable;

        if (!compiler.check(.right_paren)) {
            while (true) {
                current_function.arity += 1;

                if (current_function.arity > 255) {
                    compiler.errAtCurrent("Can't have more than 255 parameters.");
                }

                const constant = try compiler.parseVariable(false, "Expect parameter name.");
                try compiler.defineVariable(constant);

                if (!compiler.match(.comma)) {
                    break;
                }
            }
        }

        compiler.consume(.right_paren, "Expect ')' after parameters.");
        compiler.consume(.left_brace, "Expect '{' before function body.");
        try compiler.block();

        var final_function = try compiler.endCompiler();
        self.scanner = compiler.scanner;
        self.previous = compiler.previous;
        self.current = compiler.current;
        self.had_error = compiler.had_error;

        try self.currentChunk().writeClosure(.{ .obj = &final_function.obj }, self.previous.line);

        var i: usize = 0;

        while (i < final_function.upvalue_count) : (i += 1) {
            try self.emitBytes(.{ @as(u8, if (compiler.upvalues[i].is_local) 1 else 0), @as(u8, @intCast(compiler.upvalues[i].index)) });
        }
    }

    fn method(self: *Self) CompilerError!void {
        self.consume(.identifier, "Expect method name.");

        const constant = try self.identifierConstant(&self.previous, .no_change);
        const function_type = if (mem.eql(u8, self.previous.lexeme, "init"))
            Obj.Function.Type.initializer
        else
            Obj.Function.Type.method;

        try self.fun(function_type);
        try self.emitBytes(.{ .method, @as(u8, @intCast(constant[0])) });
    }

    fn classDeclaration(self: *Self) CompilerError!void {
        self.consume(.identifier, "Expect class name.");
        const class_name = self.previous;
        const name_constant = try self.identifierConstant(&self.previous, .no_change);
        try self.declareVariable(name_constant[1]);

        try self.emitBytes(.{ .class, @as(u8, @intCast(name_constant[0])) });
        try self.defineVariable(name_constant[0]);

        var class_compiler = ClassCompiler{
            .enclosing = self.current_class,
            .has_super_class = false,
        };
        self.current_class = &class_compiler;

        if (self.match(.less)) {
            self.consume(.identifier, "Expect superclass name.");
            try self.variable(false);

            if (class_name.lexemeEquals(&self.previous)) {
                self.err("A class can't inherit from itself.");
            }

            self.beginScope();
            try self.addLocal(true, Self.syntheticToken("super"));
            try self.defineVariable(0);

            try self.namedVariable(class_name, false);
            try self.emitByte(.inherit);

            class_compiler.has_super_class = true;
        }

        try self.namedVariable(class_name, false);
        self.consume(.left_brace, "Expect '{' before class body.");

        while (!self.check(.right_brace) and !self.check(.eof)) {
            try self.method();
        }

        self.consume(.right_brace, "Expect '}' after class body.");
        try self.emitByte(.pop);

        if (class_compiler.has_super_class) {
            _ = try self.endScope();
        }

        self.current_class = class_compiler.enclosing;
    }

    fn funDeclaration(self: *Self) CompilerError!void {
        const global = try self.parseVariable(false, "Expect function name.");
        self.markInitialized();
        try self.fun(.function);
        try self.defineVariable(global);
    }

    fn varDeclaration(self: *Self) CompilerError!void {
        const global = try self.parseVariable(false, "Expect variable name.");

        if (self.match(.equal)) {
            try self.expression();
        } else {
            try self.emitByte(.nil);
        }

        self.consume(.semicolon, "Expect ';' after variable declaration.");
        try self.defineVariable(global);
    }

    fn constDeclaration(self: *Self) CompilerError!void {
        const global = try self.parseVariable(true, "Expect constant name.");
        self.consume(.equal, "Expect constant to be assigned a value.");
        try self.expression();
        self.consume(.semicolon, "Expect ';' after constant declaration.");
        try self.defineVariable(global);
    }

    fn expressionStatement(self: *Self) CompilerError!void {
        try self.expression();
        self.consume(.semicolon, "Expect ';' after expression.");
        try self.emitByte(.pop);
    }

    fn switchClauses(self: *Self) CompilerError!void {
        if (self.match(.case)) {
            try self.emitByte(.duplicate);
            try self.expression();
            try self.emitByte(.equal);
            self.consume(.colon, "Expect ':' after case expression.");

            const case_jump = try self.emitJump(.jump_if_false);

            try self.emitByte(.pop);

            while (!self.check(.case) and !self.check(.default) and
                !self.check(.right_brace) and !self.check(.eof))
            {
                try self.statement();
            }

            const exit_jump = try self.emitJump(.jump);

            self.patchJump(case_jump);
            try self.emitByte(.pop);
            try self.switchClauses();
            self.patchJump(exit_jump);
        } else if (self.match(.default)) {
            self.consume(.colon, "Expect ':' after default expression.");

            if (!self.check(.right_brace) and !self.check(.eof)) {
                try self.statement();
            }
        }
    }

    fn switchStatement(self: *Self) CompilerError!void {
        self.beginScope();
        self.consume(.left_paren, "Expect '(' after 'switch'.");
        try self.expression();
        self.consume(.right_paren, "Expect ')' after switch expression.");
        self.consume(.left_brace, "Expect '{' to open switch body.");
        try self.switchClauses();
        self.consume(.right_brace, "Expect '}' to close switch body.");
        try self.emitByte(.pop);
        try self.endScope();
    }

    fn forStatement(self: *Self) CompilerError!void {
        self.beginScope();

        const old_loop_start = self.loop_start;
        const old_loop_depth = self.loop_depth;

        self.loop_depth = self.scope_depth;

        self.consume(.left_paren, "Expect '(' after 'for'.");

        if (self.match(.semicolon)) {
            // no initializer
        } else if (self.match(._var)) {
            try self.varDeclaration();
        } else {
            try self.expressionStatement();
        }

        self.loop_start = self.currentChunk().code.count;
        var exit_jump: ?u16 = null;

        if (!self.match(.semicolon)) {
            try self.expression();
            self.consume(.semicolon, "Expect ';' after loop condition.");

            exit_jump = try self.emitJump(.jump_if_false);
            try self.emitByte(.pop);
        }

        if (!self.match(.right_paren)) {
            const body_jump = try self.emitJump(.jump);
            const increment_start = self.currentChunk().code.count;

            try self.expression();
            try self.emitByte(.pop);
            self.consume(.right_paren, "Expect ')' after for clauses.");

            try self.emitLoop(self.loop_start);
            self.loop_start = increment_start;
            self.patchJump(body_jump);
        }

        try self.statement();
        try self.emitLoop(self.loop_start);

        if (exit_jump) |jump_index| {
            self.patchJump(jump_index);
            try self.emitByte(.pop);
        }

        self.loop_start = old_loop_start;
        self.loop_depth = old_loop_depth;

        try self.endScope();
    }

    fn ifStatement(self: *Self) CompilerError!void {
        self.consume(.left_paren, "Expect '(' after 'if'.");
        try self.expression();
        self.consume(.right_paren, "Expect ')' after confition.");

        const then_jump = try self.emitJump(.jump_if_false);
        try self.emitByte(.pop);

        try self.statement();
        const else_jump = try self.emitJump(.jump);

        self.patchJump(then_jump);
        try self.emitByte(.pop);

        if (self.match(._else)) {
            try self.statement();
        }

        self.patchJump(else_jump);
    }

    fn printStatement(self: *Self) CompilerError!void {
        try self.expression();
        self.consume(.semicolon, "Expect ';' after value.");
        try self.emitByte(.print);
    }

    fn returnStatement(self: *Self) CompilerError!void {
        if (self.function_type == .script) {
            self.err("Can't return from top-level code.");
        }

        if (self.match(.semicolon)) {
            try self.emitReturn();
        } else {
            if (self.function_type == .initializer) {
                self.err("Can't return a value from an initializer.");
            }

            try self.expression();
            self.consume(.semicolon, "Expect ';' after return value.");
            try self.emitByte(.ret);
        }
    }

    fn whileStatement(self: *Self) CompilerError!void {
        const old_loop_depth = self.loop_depth;
        const old_loop_start = self.loop_start;

        self.loop_depth = self.scope_depth;
        self.loop_start = self.currentChunk().code.count;

        self.consume(.left_paren, "Expect '(' after 'while'.");
        try self.expression();
        self.consume(.right_paren, "Expect ')' after condition.");

        const exit_jump = try self.emitJump(.jump_if_false);
        try self.emitByte(.pop);
        try self.statement();

        try self.emitLoop(self.loop_start);

        self.patchJump(exit_jump);
        try self.emitByte(.pop);

        self.loop_depth = old_loop_depth;
        self.loop_start = old_loop_start;
    }

    fn continueStatement(self: *Self) CompilerError!void {
        if (self.loop_depth == null) {
            self.err("The 'continue' keyword can only be used inside of a loop.");
            self.synchronize();
            return;
        }

        self.consume(.semicolon, "Expect ';' after continue.");

        try self.popLoopScope();
        try self.emitLoop(self.loop_start);
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
        if (self.match(.class)) {
            try self.classDeclaration();
        } else if (self.match(.fun)) {
            try self.funDeclaration();
        } else if (self.match(._var)) {
            try self.varDeclaration();
        } else if (self.match(._const)) {
            try self.constDeclaration();
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
        } else if (self.match(._for)) {
            try self.forStatement();
        } else if (self.match(._if)) {
            try self.ifStatement();
        } else if (self.match(._return)) {
            try self.returnStatement();
        } else if (self.match(._switch)) {
            try self.switchStatement();
        } else if (self.match(._while)) {
            try self.whileStatement();
        } else if (self.match(.left_brace)) {
            self.beginScope();
            try self.block();
            try self.endScope();
        } else if (self.match(._continue)) {
            try self.continueStatement();
        } else {
            try self.expressionStatement();
        }
    }

    fn grouping(self: *Self, can_assign: bool) CompilerError!void {
        _ = can_assign;

        try self.expression();
        self.consume(.right_paren, "Expect ')' after expression.");
    }

    fn number(self: *Self, can_assign: bool) CompilerError!void {
        _ = can_assign;

        const value = parseFloat(f64, self.previous.lexeme) catch unreachable;
        try self.emitConstant(.{ .number = value });
    }

    fn string(self: *Self, can_assign: bool) CompilerError!void {
        _ = can_assign;

        const string_obj = try Obj.String.fromBufAlloc(
            self.memory,
            self.previous.lexeme[1 .. self.previous.lexeme.len - 1],
            self.vm,
        );

        try self.emitConstant(.{
            .obj = &string_obj.obj,
        });
    }

    fn namedVariable(self: *Self, name: Token, can_assign: bool) CompilerError!void {
        var arg = self.resolveLocal(&name);
        var constant_index: usize = undefined;
        var get_op: OpCode = undefined;
        var set_op: OpCode = undefined;
        var is_const = false;

        if (arg) |result| {
            constant_index = result[0];
            get_op = if (constant_index < 0xFF) .get_local else .get_local_long;
            set_op = if (constant_index < 0xFF) .set_local else .set_local_long;
            is_const = result[1];
        } else {
            arg = try self.resolveUpvalue(&name);

            if (arg) |result| {
                constant_index = result[0];
                get_op = .get_upvalue;
                set_op = .set_upvalue;
                is_const = result[1];
            } else {
                const result = try self.identifierConstant(&name, .no_change);

                constant_index = result[0];
                get_op = if (constant_index < 0xFF) .get_global else .get_global_long;
                set_op = if (constant_index < 0xFF) .set_global else .set_global_long;
                is_const = result[1];
            }
        }

        if (can_assign and self.match(.equal)) {
            if (is_const) {
                self.err("Can't reassign a constant.");
            }

            try self.expression();
            try self.emitByte(set_op);
        } else {
            try self.emitByte(get_op);
        }

        try self.emitConstantIndex(constant_index);
    }

    fn variable(self: *Self, can_assign: bool) CompilerError!void {
        try self.namedVariable(self.previous, can_assign);
    }

    fn syntheticToken(text: []const u8) Token {
        return .{ .type = .eof, .lexeme = text, .line = 0 };
    }

    fn super(self: *Self, can_assign: bool) CompilerError!void {
        _ = can_assign;

        if (self.current_class == null) {
            self.err("Can't use 'super' outside of a class.");
        } else if (!self.current_class.?.has_super_class) {
            self.err("Can't use 'super' in a class with no superclass.");
        }

        self.consume(.dot, "Expect '.' after 'super'.");
        self.consume(.identifier, "Expect superclass method name.");
        const name = try self.identifierConstant(&self.previous, .no_change);

        try self.namedVariable(Self.syntheticToken("this"), false);

        if (self.match(.left_paren)) {
            const arg_count = try self.argumentList();
            try self.namedVariable(syntheticToken("super"), false);
            try self.emitBytes(.{ .super_invoke, @as(u8, @intCast(name[0])) });
            try self.emitByte(arg_count);
        } else {
            try self.namedVariable(Self.syntheticToken("super"), false);
            try self.emitBytes(.{ .get_super, @as(u8, @intCast(name[0])) });
        }
    }

    fn this(self: *Self, can_assign: bool) CompilerError!void {
        _ = can_assign;

        if (self.current_class == null) {
            self.err("Can't use 'this' outside of a class.");
            return;
        }

        try self.variable(false);
    }

    fn unary(self: *Self, can_assign: bool) CompilerError!void {
        _ = can_assign;

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

        const can_assign = @intFromEnum(precedence) <= @intFromEnum(Precedence.assignment);
        try prefixRule.?(self, can_assign);

        while (@intFromEnum(precedence) <= @intFromEnum(rules.get(self.current.type).precedence)) {
            self.advance();
            const infixRule = rules.get(self.previous.type).infix;
            try infixRule.?(self, can_assign);
        }

        if (can_assign and self.match(.equal)) {
            self.err("Invalid assignment target.");
        }
    }

    fn identifierConstant(self: *Self, token: *const Token, const_option: MarkConstOption) CompilerError!struct { usize, bool } {
        const string_obj = try Obj.String.fromBufAlloc(self.memory, token.lexeme, self.vm);

        switch (const_option) {
            .change => |is_const| try self.currentChunk().setVarConstness(string_obj, is_const),
            .no_change => {},
        }

        return .{
            try self.currentChunk().addConstant(.{
                .obj = &string_obj.obj,
            }),
            self.currentChunk().getVarConstness(string_obj),
        };
    }

    fn resolveLocal(self: *Self, name: *const Token) ?struct { usize, bool } {
        if (self.local_count > 0) {
            var i: usize = self.local_count;

            while (true) {
                i -= 1;

                const local = &self.locals.data[i];

                if (name.lexemeEquals(&local.name)) {
                    if (!local.initialized) {
                        self.err("Can't read local variable in its own initializer.");
                    }

                    return .{ i, local.is_const };
                }

                if (i <= 0) {
                    break;
                }
            }
        }

        return null;
    }

    fn addUpvalue(self: *Self, index: usize, is_local: bool) !usize {
        const upvalue_count = self.current_function.?.upvalue_count;
        var i: usize = 0;

        while (i < upvalue_count) : (i += 1) {
            const upvalue = &self.upvalues[i];

            if (upvalue.index == index and upvalue.is_local == is_local) {
                return i;
            }
        }

        if (upvalue_count == 256) {
            self.err("Too many closure variables in function.");
            return 0;
        }

        self.upvalues[upvalue_count].is_local = is_local;
        self.upvalues[upvalue_count].index = index;
        self.current_function.?.upvalue_count += 1;
        return upvalue_count;
    }

    fn resolveUpvalue(self: *Self, name: *const Token) !?struct { usize, bool } {
        const enclosing = self.enclosing orelse return null;
        const local_opt = enclosing.resolveLocal(name);

        if (local_opt) |local| {
            enclosing.locals.data[local[0]].is_captured = true;
            return .{ try self.addUpvalue(local[0], true), local[1] };
        }

        const upvalue_opt = try enclosing.resolveUpvalue(name);

        if (upvalue_opt) |upvalue| {
            return .{ try self.addUpvalue(upvalue[0], false), upvalue[1] };
        }

        return null;
    }

    fn addLocal(self: *Self, is_const: bool, name: Token) CompilerError!void {
        // bytecode could handle more, but giving a limit of 255 to comply to tests
        if (self.local_count == u8_count) {
            self.err("Too many local variables in function.");
        }

        const local = Local{
            .name = name,
            .depth = self.scope_depth,
            .initialized = false,
            .is_const = is_const,
            .is_captured = false,
        };

        try self.locals.set(self.local_count, local);
        self.local_count += 1;
    }

    fn declareVariable(self: *Self, is_const: bool) CompilerError!void {
        if (self.scope_depth == 0) {
            return;
        }

        const name = &self.previous;

        if (self.local_count > 0) {
            var i: usize = self.local_count - 1;

            while (i >= 0) : (i -= 1) {
                const local = &self.locals.data[i];

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

        try self.addLocal(is_const, name.*);
    }

    fn parseVariable(self: *Self, is_const: bool, errorMessage: []const u8) CompilerError!usize {
        self.consume(.identifier, errorMessage);
        try self.declareVariable(is_const);

        if (self.scope_depth > 0) {
            return 0;
        }

        return (try self.identifierConstant(&self.previous, .{ .change = is_const }))[0];
    }

    fn markInitialized(self: *Self) void {
        if (self.scope_depth == 0) {
            return;
        }

        self.locals.data[self.local_count - 1].initialized = true;
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

    fn argumentList(self: *Self) CompilerError!u8 {
        var arg_count: u16 = 0;

        if (!self.check(.right_paren)) {
            while (true) {
                try self.expression();
                arg_count += 1;

                if (arg_count > 255) {
                    self.err("Can't have more than 255 arguments.");
                }

                if (!self.match(.comma)) {
                    break;
                }
            }
        }

        self.consume(.right_paren, "Expect ')' after arguments.");
        return @intCast(if (arg_count > 255) 0 else arg_count);
    }

    fn _and(self: *Self, can_assign: bool) CompilerError!void {
        _ = can_assign;

        const end_jump = try self.emitJump(.jump_if_false);

        try self.emitByte(.pop);
        try self.parsePrecedence(._and);

        self.patchJump(end_jump);
    }

    fn _or(self: *Self, can_assign: bool) CompilerError!void {
        _ = can_assign;

        const else_jump = try self.emitJump(.jump_if_false);
        const end_jump = try self.emitJump(.jump);

        self.patchJump(else_jump);
        try self.emitByte(.pop);

        try self.parsePrecedence(._or);
        self.patchJump(end_jump);
    }

    fn errAtCurrent(self: *Self, message: []const u8) void {
        self.errAt(&self.current, message);
    }

    pub fn err(self: *Self, message: []const u8) void {
        self.errAt(&self.previous, message);
    }

    fn errAt(self: *Self, token: *Token, message: []const u8) void {
        if (self.panic_mode) {
            return;
        }

        self.panic_mode = true;
        self.io.err("[line {}] Error", .{token.line});

        switch (token.type) {
            .eof => self.io.err(" at end", .{}),
            ._error => {},
            else => self.io.err(" at '{s}'", .{token.lexeme}),
        }

        self.io.err(": {s}\n", .{message});
        self.had_error = true;
    }
};
