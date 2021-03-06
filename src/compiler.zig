const std = @import("std");
const root = @import("root");

const GarbageCollector = @import("memory.zig").GarbageCollector;

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;

const Value = @import("value.zig").Value;

const scanner = @import("scanner.zig");
const Scanner = scanner.Scanner;
const Token = scanner.Token;
const TokenType = scanner.TokenType;

const object = @import("object.zig");
const ObjString = object.ObjString;
const ObjFunction = object.ObjFunction;

const DEBUG_PRINT_CODE = @hasDecl(root, "DEBUG_PRINT_CODE") and root.DEBUG_PRINT_CODE;

const ERROR_LIMIT = 255;

pub const CompileError = error{ Compile, OutOfMemory, ICE, InvalidCharacter };

const Parser = struct {
    current: Token = Token{},
    previous: Token = Token{},
    hadError: ?CompileError = null,
    panicMode: bool = false,
    abortMode: bool = false,
    error_count: u32 = 0,
    scanner: Scanner,
};

const Precedence = enum {
    None,
    Assignment, // =
    Or, // or
    And, // and
    Equality, // == !=
    Comparison, // < > <= >=
    Term, // + -
    Factor, // * /
    Unary, // ! -
    Call, // . ()
    Primary,
};

const ParseFn = ?fn (*CompileContext, bool) void;

const ParseRule = struct {
    prefix: ParseFn,
    infix: ParseFn,
    precedence: Precedence,

    fn get(ty: TokenType) ParseRule {
        const grouping = CompileContext.grouping;
        const unary = CompileContext.unary;
        const binary = CompileContext.binary;
        const literal = CompileContext.literal;
        const number = CompileContext.number;
        const string = CompileContext.string;
        const variable = CompileContext.variable;
        const and_ = CompileContext.and_;
        const or_ = CompileContext.or_;
        const call = CompileContext.call;
        const dot = CompileContext.dot;
        const this = CompileContext.this;
        const super = CompileContext.super;
        return switch (ty) {
            .LeftParen => .{ .prefix = grouping, .infix = call, .precedence = .Call },
            .RightParen => .{ .prefix = null, .infix = null, .precedence = .None },
            .LeftBrace => .{ .prefix = null, .infix = null, .precedence = .None },
            .RightBrace => .{ .prefix = null, .infix = null, .precedence = .None },
            .Comma => .{ .prefix = null, .infix = null, .precedence = .None },
            .Dot => .{ .prefix = null, .infix = dot, .precedence = .Call },
            .Minus => .{ .prefix = unary, .infix = binary, .precedence = .Term },
            .Plus => .{ .prefix = null, .infix = binary, .precedence = .Term },
            .Semicolon => .{ .prefix = null, .infix = null, .precedence = .None },
            .Slash => .{ .prefix = null, .infix = binary, .precedence = .Factor },
            .Star => .{ .prefix = null, .infix = binary, .precedence = .Factor },
            .Bang => .{ .prefix = unary, .infix = null, .precedence = .None },
            .BangEqual => .{ .prefix = null, .infix = binary, .precedence = .Equality },
            .Equal => .{ .prefix = null, .infix = null, .precedence = .None },
            .EqualEqual => .{ .prefix = null, .infix = binary, .precedence = .Equality },
            .Greater => .{ .prefix = null, .infix = binary, .precedence = .Comparison },
            .GreaterEqual => .{ .prefix = null, .infix = binary, .precedence = .Comparison },
            .Less => .{ .prefix = null, .infix = binary, .precedence = .Comparison },
            .LessEqual => .{ .prefix = null, .infix = binary, .precedence = .Comparison },
            .Identifier => .{ .prefix = variable, .infix = null, .precedence = .None },
            .String => .{ .prefix = string, .infix = null, .precedence = .None },
            .Number => .{ .prefix = number, .infix = null, .precedence = .None },
            .And => .{ .prefix = null, .infix = and_, .precedence = .And },
            .Class => .{ .prefix = null, .infix = null, .precedence = .None },
            .Else => .{ .prefix = null, .infix = null, .precedence = .None },
            .False => .{ .prefix = literal, .infix = null, .precedence = .None },
            .For => .{ .prefix = null, .infix = null, .precedence = .None },
            .Fun => .{ .prefix = null, .infix = null, .precedence = .None },
            .If => .{ .prefix = null, .infix = null, .precedence = .None },
            .Nil => .{ .prefix = literal, .infix = null, .precedence = .None },
            .Or => .{ .prefix = null, .infix = or_, .precedence = .Or },
            .Print => .{ .prefix = null, .infix = null, .precedence = .None },
            .Return => .{ .prefix = null, .infix = null, .precedence = .None },
            .Super => .{ .prefix = super, .infix = null, .precedence = .None },
            .This => .{ .prefix = this, .infix = null, .precedence = .None },
            .True => .{ .prefix = literal, .infix = null, .precedence = .None },
            .Var => .{ .prefix = null, .infix = null, .precedence = .None },
            .While => .{ .prefix = null, .infix = null, .precedence = .None },
            .Error => .{ .prefix = null, .infix = null, .precedence = .None },
            .EOF => .{ .prefix = null, .infix = null, .precedence = .None },
        };
    }
};

pub const MAX_LOCALS: usize = 0x100;
pub const MAX_UPVALUES: usize = 0x100;

pub const Local = struct {
    name: Token = .{},
    depth: ?u8 = 0,
    is_captured: bool = false,
};

pub const Upvalue = struct {
    index: u8 = 0,
    is_local: bool = true,
};

pub const FunctionType = enum { Function, Script, Method, Initializer };

pub const Compiler = struct {
    const Self = Compiler;

    gc: *GarbageCollector,
    current: ?*CompileContext,
    current_class: ?*ClassCompiler,

    pub fn init(gc: *GarbageCollector) Self {
        return Self{ .gc = gc, .current = null, .current_class = null };
    }

    pub fn compile(self: *Self, source: []const u8) CompileError!*ObjFunction {
        var parser = Parser{ .scanner = Scanner.init(source) };
        var context = try CompileContext.init(self.gc, self, &parser, .Script);
        self.current = &context;
        context.advance();
        while (!context.match(TokenType.EOF)) {
            context.declaration();
        }
        const function = context.endCompiler();
        if (parser.hadError) |err| {
            return err;
        } else {
            return function;
        }
    }
};

const ClassCompiler = struct {
    enclosing: ?*ClassCompiler,
    has_superclass: bool = false,
};

const CompileContext = struct {
    const Self = CompileContext;
    enclosing: ?*Self = null,
    gc: *GarbageCollector,
    compiler: *Compiler,
    parser: *Parser,

    obj_function: *ObjFunction,
    type: FunctionType,

    locals: [MAX_LOCALS]Local = [_]Local{.{}} ** MAX_LOCALS,
    upvalues: [MAX_UPVALUES]Upvalue = [_]Upvalue{.{}} ** MAX_UPVALUES,
    local_count: usize = 1,
    scope_depth: u8 = 0,

    fn init(gc: *GarbageCollector, compiler: *Compiler, parser: *Parser, ty: FunctionType) !Self {
        var f = try gc.newFunction();
        var r = Self{ .gc = gc, .compiler = compiler, .parser = parser, .obj_function = f, .type = ty };
        if (ty == .Method or ty == .Initializer) {
            r.locals[0].name.str = "this";
        }
        return r;
    }

    fn currentChunk(self: *Self) *Chunk {
        return &self.obj_function.chunk;
    }

    fn advance(self: *Self) void {
        self.parser.previous = self.parser.current;
        if (self.parser.abortMode) {
            self.parser.current = Token{ .type = .EOF };
            return;
        }
        while (true) {
            self.parser.current = self.parser.scanner.scanToken();
            // std.debug.print("Scanning: {} {s} {}\n", self.parser.current);
            if (self.parser.current.type != .Error) break;
            self.errorAtCurrent(self.parser.current.str, CompileError.Compile);
        }
    }

    fn consume(self: *Self, ty: TokenType, message: []const u8) void {
        if (self.check(ty)) {
            self.advance();
            return;
        }

        self.errorAtCurrent(message, CompileError.Compile);
    }

    fn check(self: *Self, ty: TokenType) bool {
        return self.parser.current.type == ty;
    }

    fn match(self: *Self, ty: TokenType) bool {
        if (!self.check(ty)) {
            return false;
        }
        self.advance();
        return true;
    }

    fn emitByte(self: *Self, byte: u8) void {
        self.currentChunk().write(byte, self.parser.previous.line) catch |err| {
            return self.errorAtPrevious("Too much bytecode", err);
        };
    }

    fn emitOpCode(self: *Self, opCode: OpCode) void {
        self.emitByte(@enumToInt(opCode));
    }

    fn emitOpCodes(self: *Self, opCode1: OpCode, opCode2: OpCode) void {
        self.emitOpCode(opCode1);
        self.emitOpCode(opCode2);
    }

    fn emitBytes(self: *Self, byte1: u8, byte2: u8) void {
        self.emitByte(byte1);
        self.emitByte(byte2);
    }

    fn emitLoop(self: *Self, loopStart: usize) void {
        self.emitOpCode(.Loop);

        const jump = self.currentChunk().code.len - loopStart + 2;
        if (jump > 0xffff) {
            self.errorAtPrevious("Loop body too large.", CompileError.Compile);
        }
        self.emitByte(@intCast(u8, (jump >> 8) & 0xff));
        self.emitByte(@intCast(u8, jump & 0xff));
    }

    fn emitJump(self: *Self, instruction: OpCode) usize {
        self.emitOpCode(instruction);
        self.emitByte(0xff);
        self.emitByte(0xff);
        return self.currentChunk().code.len - 2;
    }

    fn emitReturn(self: *Self) void {
        if (self.type == .Initializer) {
            self.emitOpCode(.GetLocal);
            self.emitByte(0);
        } else {
            self.emitOpCode(.Nil);
        }
        self.emitOpCode(.Return);
    }

    fn emitConstant(self: *Self, value: Value) void {
        self.currentChunk().writeConstant(value, self.parser.previous.line) catch |err| {
            return self.errorAtPrevious("Too many constants", err);
        };
    }

    fn patchJump(self: *Self, offset: usize) void {
        const jump = self.currentChunk().code.len - offset - 2;
        if (jump > 0xffff) {
            self.errorAtPrevious("Too much code to jump over.", CompileError.Compile);
        }
        self.currentChunk().code[offset] = @intCast(u8, (jump >> 8) & 0xff);
        self.currentChunk().code[offset + 1] = @intCast(u8, jump & 0xff);
    }

    fn beginScope(self: *Self) void {
        if (self.scope_depth == 255) {
            self.errorAtPrevious("Too many nested scopes", CompileError.Compile);
            self.parser.abortMode = true;
            return;
        }
        self.scope_depth += 1;
    }

    fn endScope(self: *Self) void {
        if (self.scope_depth == 0) {
            return;
        }
        self.scope_depth -= 1;
        while (self.local_count > 0 and self.locals[self.local_count - 1].depth.? > self.scope_depth) {
            if (self.locals[self.local_count - 1].is_captured) {
                self.emitOpCode(.CloseUpvalue);
            } else {
                self.emitOpCode(.Pop);
            }
            self.local_count -= 1;
        }
    }

    fn endCompiler(self: *Self) *ObjFunction {
        self.emitReturn();
        const f = self.obj_function;
        if (DEBUG_PRINT_CODE) {
            if (self.parser.hadError == null) {
                const name = if (f.name) |n| n.str else "script";
                @import("debug.zig").disassembleChunk(self.currentChunk(), name);
            }
        }
        return f;
    }

    fn errorAtCurrent(self: *Self, message: []const u8, err: CompileError) void {
        self.errorAt(&self.parser.current, message, err);
    }

    fn errorAtPrevious(self: *Self, message: []const u8, err: CompileError) void {
        self.errorAt(&self.parser.previous, message, err);
    }

    fn errorAt(self: *Self, token: *Token, message: []const u8, err: CompileError) void {
        if (self.parser.abortMode or self.parser.panicMode) return;
        self.parser.panicMode = true;
        self.parser.hadError = err;
        self.parser.error_count += 1;
        const print = std.debug.print;
        print("[line {d}] Error", .{token.line});
        if (token.type == .EOF) {
            print(" at end", .{});
        } else if (token.type != .Error) {
            print(" at '{s}'", .{token.str});
        }
        print(": {s}\n", .{message});
        if (self.parser.error_count >= ERROR_LIMIT) {
            print("Too many errors {d}. Rest skipped", .{self.parser.error_count});
            self.parser.abortMode = true;
        }
    }

    fn expression(self: *Self) void {
        self.parsePrecedence(.Assignment);
    }

    fn block(self: *Self) void {
        while (!self.check(.RightBrace) and !self.check(.EOF)) {
            self.declaration();
        }

        self.consume(.RightBrace, "Expect '}' after block.");
    }

    fn function(self: *Self, ty: FunctionType) void {
        var context = CompileContext.init(self.gc, self.compiler, self.parser, ty) catch |err| {
            return self.errorAtPrevious("Could not allocate for function", err);
        };
        self.compiler.current = &context;
        context.enclosing = self;
        context.obj_function.name = self.gc.copyString(self.parser.previous.str) catch |err| {
            return self.errorAtPrevious("Could not allocate for function name", err);
        };
        context.beginScope();

        context.consume(.LeftParen, "Expect '(' after function name.");
        if (!context.check(.RightParen)) {
            while (true) {
                if (context.obj_function.arity == 255) {
                    self.errorAtCurrent("Can't have more than 255 parameters.", CompileError.Compile);
                } else {
                    context.obj_function.arity += 1;
                }
                const constant = context.parseVariable("Expect parameter name");
                context.defineVariable(constant);
                if (!context.match(.Comma)) {
                    break;
                }
            }
        }
        context.consume(.RightParen, "Expect ')' after parameters.");
        context.consume(.LeftBrace, "Expect '{' before function body.");
        context.block();

        const f = context.endCompiler();
        self.emitOpCode(.Closure);
        self.emitByte(self.makeConstant(Value.obj(f.asObj())));

        for (context.upvalues[0..f.upvalue_count]) |upvalue| {
            self.emitByte(if (upvalue.is_local) 1 else 0);
            self.emitByte(upvalue.index);
        }
        self.compiler.current = self;
    }

    fn method(self: *Self) void {
        self.consume(.Identifier, "Expect method name.");
        const constant = self.identifierConstant(&self.parser.previous);

        if (std.mem.eql(u8, self.parser.previous.str, "init")) {
            self.function(.Initializer);
        } else {
            self.function(.Method);
        }

        self.emitOpCode(.Method);
        self.emitByte(constant);
    }

    fn declaration(self: *Self) void {
        if (self.match(.Class)) {
            self.classDeclaration();
        } else if (self.match(.Fun)) {
            self.funDeclaration();
        } else if (self.match(.Var)) {
            self.varDeclaration();
        } else {
            self.statement();
        }
        if (self.parser.panicMode) {
            self.synchronize();
        }
    }

    fn classDeclaration(self: *Self) void {
        self.consume(.Identifier, "Expect class name.");
        const class_name = self.parser.previous;
        const name_constant = self.identifierConstant(&class_name);

        self.declareVariable();
        self.emitOpCode(.Class);
        self.emitByte(name_constant);
        self.defineVariable(name_constant);

        var class_compiler = ClassCompiler{ .enclosing = self.compiler.current_class };
        self.compiler.current_class = &class_compiler;

        if (self.match(.Less)) {
            self.consume(.Identifier, "Expect superclass name.");
            self.variable(false);
            if (std.mem.eql(u8, class_name.str, self.parser.previous.str)) {
                self.errorAtPrevious("A class can't inherit from itself.", CompileError.Compile);
            }
            self.beginScope();
            self.addLocal(Token{ .str = "super" });
            self.defineVariable(0);

            self.namedVariable(class_name, false);
            self.emitOpCode(.Inherit);
            class_compiler.has_superclass = true;
        }

        self.namedVariable(class_name, false);

        self.consume(.LeftBrace, "Expect '{' before class body.");
        while (!self.check(.RightBrace) and !self.check(.EOF)) {
            self.method();
        }
        self.consume(.RightBrace, "Expect '}' after class body.");
        self.emitOpCode(.Pop);

        if (class_compiler.has_superclass) {
            self.endScope();
        }

        self.compiler.current_class = self.compiler.current_class.?.enclosing;
    }

    fn funDeclaration(self: *Self) void {
        const global = self.parseVariable("Expect variable name.");
        self.markInitialized();
        self.function(.Function);
        self.defineVariable(global);
    }

    fn varDeclaration(self: *Self) void {
        const global = self.parseVariable("Expect variable name.");

        if (self.match(.Equal)) {
            self.expression();
        } else {
            self.emitOpCode(.Nil);
        }
        self.consume(.Semicolon, "Expect ';' after variable declaration.");
        self.defineVariable(global);
    }

    fn statement(self: *Self) void {
        if (self.match(.Print)) {
            self.printStatement();
        } else if (self.match(.For)) {
            self.forStatement();
        } else if (self.match(.If)) {
            self.ifStatement();
        } else if (self.match(.Return)) {
            self.returnStatement();
        } else if (self.match(.While)) {
            self.whileStatement();
        } else if (self.match(.LeftBrace)) {
            self.beginScope();
            self.block();
            self.endScope();
        } else {
            self.expressionStatement();
        }
    }

    fn synchronize(self: *Self) void {
        self.parser.panicMode = false;
        while (self.parser.current.type != .EOF) {
            if (self.parser.previous.type == .Semicolon) return;
            switch (self.parser.current.type) {
                .Class, .Fun, .Var, .For, .If, .While, .Print, .Return => return,
                else => {},
            }
            _ = self.advance();
        }
    }

    fn printStatement(self: *Self) void {
        self.expression();
        self.consume(.Semicolon, "Expect ';' after value.");
        self.emitOpCode(.Print);
    }

    fn returnStatement(self: *Self) void {
        if (self.type == .Script) {
            self.errorAtPrevious("Can't return from top-level code.", CompileError.Compile);
        }
        if (self.match(.Semicolon)) {
            self.emitReturn();
        } else {
            if (self.type == .Initializer) {
                self.errorAtPrevious("Can't return a value from an initializer.", CompileError.Compile);
            }
            self.expression();
            self.consume(.Semicolon, "Expect ';' after value.");
            self.emitOpCode(.Return);
        }
    }

    fn expressionStatement(self: *Self) void {
        self.expression();
        self.consume(.Semicolon, "Expect ';' after expression.");
        self.emitOpCode(.Pop);
    }

    fn forStatement(self: *Self) void {
        self.beginScope();
        self.consume(.LeftParen, "Expect '(' after 'for'.");
        if (self.match(.Semicolon)) {} else if (self.match(.Var)) {
            self.varDeclaration();
        } else {
            self.expressionStatement();
        }

        var loopStart = self.currentChunk().code.len;
        var exitJump: ?usize = null;
        if (!self.match(.Semicolon)) {
            self.expression();
            self.consume(.Semicolon, "Expect ';' after loop condition.");

            // Jump out of the loop if the condition is false.
            exitJump = self.emitJump(.JumpIfFalse);
            self.emitOpCode(.Pop);
        }

        if (!self.match(.RightParen)) {
            const bodyJump = self.emitJump(.Jump);
            const incStart = self.currentChunk().code.len;
            self.expression();
            self.emitOpCode(.Pop);
            self.consume(.RightParen, "Expect ')' after for clauses.");

            self.emitLoop(loopStart);
            loopStart = incStart;
            self.patchJump(bodyJump);
        }

        self.statement();
        self.emitLoop(loopStart);

        if (exitJump) |jump| {
            self.patchJump(jump);
            self.emitOpCode(.Pop);
        }

        self.endScope();
    }

    fn whileStatement(self: *Self) void {
        const loopStart = self.currentChunk().code.len;
        self.consume(.LeftParen, "Expect '(' after 'while'.");
        self.expression();
        self.consume(.RightParen, "Expect ')' after condition.");

        const exitJump = self.emitJump(.JumpIfFalse);
        self.emitOpCode(.Pop);
        self.statement();
        self.emitLoop(loopStart);

        self.patchJump(exitJump);
        self.emitOpCode(.Pop);
    }

    fn ifStatement(self: *Self) void {
        self.consume(.LeftParen, "Expect '(' after if.");
        self.expression();
        self.consume(.RightParen, "Expect ')' after condition.");

        const thenJump = self.emitJump(.JumpIfFalse);
        self.emitOpCode(.Pop);
        self.statement();

        const elseJump = self.emitJump(.Jump);

        self.patchJump(thenJump);
        self.emitOpCode(.Pop);

        if (self.match(.Else)) {
            self.statement();
        }

        self.patchJump(elseJump);
    }

    fn grouping(self: *Self, can_assign: bool) void {
        _ = can_assign;
        self.expression();
        self.consume(.RightParen, "Expect ')' after expression.");
    }

    fn number(self: *Self, can_assign: bool) void {
        _ = can_assign;
        const value = std.fmt.parseFloat(f64, self.parser.previous.str) catch |err| {
            return self.errorAtPrevious("Could not parse number", err);
        };
        self.emitConstant(Value.number(value));
    }

    fn string(self: *Self, can_assign: bool) void {
        _ = can_assign;
        const n = self.parser.previous.str.len - 1;
        // remove quotes
        const value = self.gc.copyString(self.parser.previous.str[1..n]) catch |err| {
            return self.errorAtPrevious("Could not allocate memory for string", err);
        };
        self.emitConstant(Value.obj(value.asObj()));
    }

    fn namedVariable(self: *Self, name: Token, can_assign: bool) void {
        var getOp = OpCode.GetLocal;
        var setOp = OpCode.SetLocal;
        var arg = self.resolveLocal(&name);
        if (arg == null) {
            getOp = .GetUpvalue;
            setOp = .SetUpvalue;
            arg = self.resolveUpvalue(&name);
        }
        if (arg == null) {
            getOp = .GetGlobal;
            setOp = .SetGlobal;
            arg = self.identifierConstant(&name);
        }
        if (can_assign and self.match(.Equal)) {
            self.expression();
            self.emitOpCode(setOp);
            self.emitByte(arg.?);
        } else {
            self.emitOpCode(getOp);
            self.emitByte(arg.?);
        }
    }

    fn variable(self: *Self, can_assign: bool) void {
        self.namedVariable(self.parser.previous, can_assign);
    }

    fn super(self: *Self, can_assign: bool) void {
        _ = can_assign;
        if (self.compiler.current_class == null) {
            self.errorAtPrevious("Can't use 'super' outside of a class.", CompileError.Compile);
        } else if (!self.compiler.current_class.?.has_superclass) {
            self.errorAtPrevious("Can't use 'super' in a class with no superclass.", CompileError.Compile);
        }
        self.consume(.Dot, "Expect '.' after 'super'.");
        self.consume(.Identifier, "Expect superclass method name.");
        const name = self.identifierConstant(&self.parser.previous);

        self.namedVariable(Token{ .str = "this" }, false);
        self.namedVariable(Token{ .str = "super" }, false);
        self.emitOpCode(.GetSuper);
        self.emitByte(name);
    }

    fn this(self: *Self, can_assign: bool) void {
        _ = can_assign;
        if (self.compiler.current_class == null) {
            self.errorAtPrevious("Can't use 'this' outside of a class.", CompileError.Compile);
        }
        self.variable(false);
    }

    fn unary(self: *Self, can_assign: bool) void {
        _ = can_assign;
        const operator = &self.parser.previous;
        const operatorType = operator.type;
        self.parsePrecedence(.Unary);
        switch (operatorType) {
            .Bang => self.emitOpCode(.Not),
            .Minus => self.emitOpCode(.Negate),
            else => {
                self.errorAt(operator, "ICE: Invalid unary operator", CompileError.ICE);
                return;
            },
        }
    }

    fn binary(self: *Self, can_assign: bool) void {
        _ = can_assign;
        const operator = &self.parser.previous;
        const operatorType = operator.type;
        const rule = ParseRule.get(operatorType);
        const higher_precedence = @intToEnum(Precedence, @enumToInt(rule.precedence) + 1);
        self.parsePrecedence(higher_precedence);
        switch (operatorType) {
            .Plus => self.emitOpCode(.Add),
            .Minus => self.emitOpCode(.Subtract),
            .Star => self.emitOpCode(.Multiply),
            .Slash => self.emitOpCode(.Divide),
            .BangEqual => self.emitOpCodes(.Equal, .Not),
            .EqualEqual => self.emitOpCode(.Equal),
            .Greater => self.emitOpCode(.Greater),
            .GreaterEqual => self.emitOpCodes(.Less, .Not),
            .Less => self.emitOpCode(.Less),
            .LessEqual => self.emitOpCodes(.Greater, .Not),
            else => {
                self.errorAt(operator, "ICE: Invalid binary operator", CompileError.ICE);
                return;
            },
        }
    }

    fn call(self: *Self, can_assign: bool) void {
        _ = can_assign;
        const arg_count = self.argumentList();
        self.emitOpCode(.Call);
        self.emitByte(arg_count);
    }

    fn dot(self: *Self, can_assign: bool) void {
        self.consume(.Identifier, "Expect property name after '.'.");
        const name = self.identifierConstant(&self.parser.previous);
        if (can_assign and self.match(.Equal)) {
            self.expression();
            self.emitOpCode(.SetProperty);
            self.emitByte(name);
        } else if (self.match(.LeftParen)) {
            const arg_count = self.argumentList();
            self.emitOpCode(.Invoke);
            self.emitByte(name);
            self.emitByte(arg_count);
        } else {
            self.emitOpCode(.GetProperty);
            self.emitByte(name);
        }
    }

    fn literal(self: *Self, can_assign: bool) void {
        _ = can_assign;
        const lit = self.parser.previous.type;
        switch (lit) {
            .False => self.emitOpCode(.False),
            .Nil => self.emitOpCode(.Nil),
            .True => self.emitOpCode(.True),
            else => {
                self.errorAtPrevious("ICE: Invalid literal", CompileError.ICE);
                return;
            },
        }
    }

    fn parsePrecedence(self: *Self, precedence: Precedence) void {
        self.advance();
        const rule = ParseRule.get(self.parser.previous.type);
        const prefix_rule = rule.prefix orelse {
            self.errorAtPrevious("Expect expression.", CompileError.Compile);
            return;
        };
        const can_assign = @enumToInt(precedence) <= @enumToInt(Precedence.Assignment);
        prefix_rule(self, can_assign);
        while (@enumToInt(precedence) <= @enumToInt(ParseRule.get(self.parser.current.type).precedence)) {
            self.advance();
            const rule1 = ParseRule.get(self.parser.previous.type);
            const infix_rule = rule1.infix orelse {
                self.errorAtPrevious("ICE: No infix rule found", CompileError.ICE);
                return;
            };
            infix_rule(self, can_assign);
        }

        if (can_assign and self.match(.Equal)) {
            self.errorAtPrevious("Invalid assignment target.", CompileError.Compile);
        }
    }

    fn identifierConstant(self: *Self, name: *const Token) u8 {
        const value = self.gc.copyString(name.str) catch |err| {
            self.errorAtPrevious("Could not allocate memory for identifier", err);
            return 0;
        };
        return self.makeConstant(Value.obj(value.asObj()));
    }

    fn makeConstant(self: *Self, value: Value) u8 {
        const c = self.currentChunk().addConstant(value) catch |err| {
            self.errorAtPrevious("Could not allocate memory for identifier", err);
            return 0;
        };
        if (c > 0xff) {
            self.errorAtPrevious("Too many constants in one chunk", CompileError.Compile);
            return 0;
        }
        return @intCast(u8, c);
    }

    fn resolveLocal(self: *Self, name: *const Token) ?u8 {
        if (self.local_count >= 255) {
            self.parser.panicMode = true;
            return null;
        }
        var i = @intCast(u8, self.local_count);
        while (i > 0) {
            i -= 1;
            const local = &self.locals[i];
            if (std.mem.eql(u8, name.str, local.name.str)) {
                if (local.depth == null) {
                    self.errorAtPrevious("Can't read local variable in its own initializer.", CompileError.Compile);
                }
                return i;
            }
        }
        return null;
    }

    fn addUpvalue(self: *Self, index: u8, is_local: bool) u8 {
        var upvalue_count = self.obj_function.upvalue_count;
        for (self.upvalues[0..upvalue_count]) |*upvalue, i| {
            if (upvalue.index == index and upvalue.is_local == is_local) {
                return @intCast(u8, i);
            }
        }
        if (upvalue_count == 0xff) {
            self.errorAtPrevious("Too many closure variables in function.", CompileError.Compile);
            return 0;
        }
        self.upvalues[upvalue_count].is_local = is_local;
        self.upvalues[upvalue_count].index = index;
        self.obj_function.upvalue_count += 1;
        return upvalue_count;
    }

    fn resolveUpvalue(self: *Self, name: *const Token) ?u8 {
        var enclosing = self.enclosing orelse return null;
        if (enclosing.resolveLocal(name)) |local| {
            enclosing.locals[local].is_captured = true;
            return self.addUpvalue(local, true);
        }
        if (enclosing.resolveUpvalue(name)) |upvalue| {
            return self.addUpvalue(upvalue, false);
        }
        return null;
    }

    fn addLocal(self: *Self, name: Token) void {
        if (self.local_count == MAX_LOCALS) {
            self.errorAtPrevious("Too many local variables in function.", CompileError.Compile);
            return;
        }
        const local = &self.locals[self.local_count];
        self.local_count += 1;
        local.name = name;
        local.is_captured = false;
        local.depth = null;
    }

    fn declareVariable(self: *Self) void {
        if (self.scope_depth == 0) return;

        const name = self.parser.previous;
        var i = self.local_count;
        while (i > 0) {
            i -= 1;
            const local = &self.locals[i];
            if (local.depth != null and local.depth.? < self.scope_depth) {
                break;
            }
            if (std.mem.eql(u8, name.str, local.name.str)) {
                self.errorAtPrevious("Already a variable with this name in this scope.", CompileError.Compile);
            }
        }
        self.addLocal(name);
    }

    fn parseVariable(self: *Self, error_message: []const u8) u8 {
        self.consume(.Identifier, error_message);

        self.declareVariable();
        if (self.scope_depth > 0) return 0;

        return self.identifierConstant(&self.parser.previous);
    }

    fn defineVariable(self: *Self, global: u8) void {
        if (self.scope_depth > 0) {
            self.markInitialized();
            return;
        }
        self.emitOpCode(.DefineGlobal);
        self.emitByte(global);
    }

    fn argumentList(self: *Self) u8 {
        var arg_count: u8 = 0;
        if (!self.check(.RightParen)) {
            while (true) {
                self.expression();
                if (arg_count == 255) {
                    self.errorAtPrevious("Can't have more than 255 arguments.", CompileError.Compile);
                    break;
                }
                arg_count += 1;
                if (!self.match(.Comma)) {
                    break;
                }
            }
        }
        self.consume(.RightParen, "expect ')' after arguments.");
        return arg_count;
    }

    fn and_(self: *Self, can_assign: bool) void {
        _ = can_assign;
        const endJump = self.emitJump(.JumpIfFalse);

        self.emitOpCode(.Pop);
        self.parsePrecedence(.And);

        self.patchJump(endJump);
    }

    fn or_(self: *Self, can_assign: bool) void {
        _ = can_assign;
        const elseJump = self.emitJump(.JumpIfFalse);
        const endJump = self.emitJump(.Jump);

        self.patchJump(elseJump);
        self.emitOpCode(.Pop);

        self.parsePrecedence(.Or);
        self.patchJump(endJump);
    }

    fn markInitialized(self: *Self) void {
        if (self.scope_depth == 0) return;
        self.locals[self.local_count - 1].depth = self.scope_depth;
    }
};
