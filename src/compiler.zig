const std = @import("std");

const Allocator = std.mem.Allocator;

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;

const Value = @import("value.zig").Value;

const scanner = @import("scanner.zig");
const Scanner = scanner.Scanner;
const Token = scanner.Token;
const TokenType = scanner.TokenType;

const Parser = struct {
    current: Token,
    previous: Token,
    hadError: bool,
    panicMode: bool,
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

const ParseFn = ?fn (*CompileContext) void;

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
        return switch (ty) {
            .LeftParen => .{ .prefix = grouping, .infix = null, .precedence = .None },
            .RightParen => .{ .prefix = null, .infix = null, .precedence = .None },
            .LeftBrace => .{ .prefix = null, .infix = null, .precedence = .None },
            .RightBrace => .{ .prefix = null, .infix = null, .precedence = .None },
            .Comma => .{ .prefix = null, .infix = null, .precedence = .None },
            .Dot => .{ .prefix = null, .infix = null, .precedence = .None },
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
            .Identifier => .{ .prefix = null, .infix = null, .precedence = .None },
            .String => .{ .prefix = null, .infix = null, .precedence = .None },
            .Number => .{ .prefix = number, .infix = null, .precedence = .None },
            .And => .{ .prefix = null, .infix = null, .precedence = .None },
            .Class => .{ .prefix = null, .infix = null, .precedence = .None },
            .Else => .{ .prefix = null, .infix = null, .precedence = .None },
            .False => .{ .prefix = literal, .infix = null, .precedence = .None },
            .For => .{ .prefix = null, .infix = null, .precedence = .None },
            .Fun => .{ .prefix = null, .infix = null, .precedence = .None },
            .If => .{ .prefix = null, .infix = null, .precedence = .None },
            .Nil => .{ .prefix = literal, .infix = null, .precedence = .None },
            .Or => .{ .prefix = null, .infix = null, .precedence = .None },
            .Print => .{ .prefix = null, .infix = null, .precedence = .None },
            .Return => .{ .prefix = null, .infix = null, .precedence = .None },
            .Super => .{ .prefix = null, .infix = null, .precedence = .None },
            .This => .{ .prefix = null, .infix = null, .precedence = .None },
            .True => .{ .prefix = literal, .infix = null, .precedence = .None },
            .Var => .{ .prefix = null, .infix = null, .precedence = .None },
            .While => .{ .prefix = null, .infix = null, .precedence = .None },
            .Error => .{ .prefix = null, .infix = null, .precedence = .None },
            .EOF => .{ .prefix = null, .infix = null, .precedence = .None },
        };
    }
};
pub const CompileError = error{Compile};

pub const Compiler = struct {
    const Self = Compiler;

    allocator: Allocator,

    pub fn init(allocator: Allocator) Self {
        return Self{ .allocator = allocator };
    }

    pub fn compile(self: *Self, source: []const u8, chunk: *Chunk) CompileError!bool {
        _ = self;
        var context = CompileContext.init(source, chunk);
        context.advance();
        context.expression();
        context.consume(TokenType.EOF, "Expect end of expression");
        context.endCompiler();
        return !context.parser.hadError;
    }
};

const CompileContext = struct {
    const Self = CompileContext;
    scanner: Scanner,
    parser: Parser,
    chunk: *Chunk,

    fn init(source: []const u8, chunk: *Chunk) Self {
        return Self{ .scanner = Scanner.init(source), .parser = Parser{ .current = Token.empty(), .previous = Token.empty(), .hadError = false, .panicMode = false }, .chunk = chunk };
    }

    fn currentChunk(self: *Self) *Chunk {
        return self.chunk;
    }

    fn advance(self: *Self) void {
        self.parser.previous = self.parser.current;
        while (true) {
            self.parser.current = self.scanner.scanToken();
            // std.debug.print("Scanning: {} {s} {}\n", self.parser.current);
            if (self.parser.current.type != .Error) break;
            self.errorAtCurrent(self.parser.current.str);
        }
    }

    fn consume(self: *Self, ty: TokenType, message: []const u8) void {
        if (self.parser.current.type == ty) {
            self.advance();
            return;
        }

        self.errorAtCurrent(message);
    }

    fn emitByte(self: *Self, byte: u8) void {
        self.currentChunk().write(byte, self.parser.previous.line) catch {
            return self.errorAtPrevious("Too much bytecode");
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

    fn emitReturn(self: *Self) void {
        self.emitOpCode(.Return);
    }

    fn emitConstant(self: *Self, value: Value) void {
        self.currentChunk().writeConstant(value, self.parser.previous.line) catch {
            return self.errorAtPrevious("Too many constants");
        };
    }

    fn endCompiler(self: *Self) void {
        self.emitReturn();
        if (std.log.level == .debug) {
            if (!self.parser.hadError) {
                @import("debug.zig").disassembleChunk(self.currentChunk(), "code");
            }
        }
    }

    fn errorAtCurrent(self: *Self, message: []const u8) void {
        self.errorAt(&self.parser.current, message);
    }

    fn errorAtPrevious(self: *Self, message: []const u8) void {
        self.errorAt(&self.parser.previous, message);
    }

    fn errorAt(self: *Self, token: *Token, message: []const u8) void {
        if (self.parser.panicMode) return;
        self.parser.panicMode = true;
        self.parser.hadError = true;
        const print = std.debug.print;
        print("[line {d}] Error ", .{token.line});
        if (token.type == .EOF) {
            print("at end", .{});
        } else if (token.type != .Error) {
            print("at '{s}'", .{token.str});
        }
        print(": {s}'\n", .{message});
    }

    fn expression(self: *Self) void {
        self.parsePrecedence(.Assignment);
    }

    fn grouping(self: *Self) void {
        self.expression();
        self.consume(.RightParen, "Expect ')' after expression.");
    }

    fn number(self: *Self) void {
        const value = std.fmt.parseFloat(f64, self.parser.previous.str) catch {
            return self.errorAtPrevious("Could not parse number");
        };
        self.emitConstant(.{ .number = value });
    }

    fn unary(self: *Self) void {
        const operatorType = self.parser.previous.type;
        self.parsePrecedence(.Unary);
        switch (operatorType) {
            .Bang => self.emitOpCode(.Not),
            .Minus => self.emitOpCode(.Negate),
            else => {
                std.log.err("ICE: Invalid unary operator {s}", .{@tagName(operatorType)});
                self.parser.panicMode = true;
                self.parser.hadError = true;
                return;
            },
        }
    }

    fn binary(self: *Self) void {
        const operatorType = self.parser.previous.type;
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
                std.log.err("ICE: Invalid binary operator {s}", .{@tagName(operatorType)});
                self.parser.panicMode = true;
                self.parser.hadError = true;
                return;
            },
        }
    }

    fn literal(self: *Self) void {
        const lit = self.parser.previous.type;
        switch (lit) {
            .False => self.emitOpCode(.False),
            .Nil => self.emitOpCode(.Nil),
            .True => self.emitOpCode(.True),
            else => {
                std.log.err("ICE: Invalid literal {s}", .{@tagName(lit)});
                self.parser.panicMode = true;
                self.parser.hadError = true;
                return;
            },
        }
    }

    fn parsePrecedence(self: *Self, precedence: Precedence) void {
        self.advance();
        const rule = ParseRule.get(self.parser.previous.type);
        const prefix_rule = rule.prefix orelse {
            self.errorAtPrevious("Expect expression.");
            return;
        };
        prefix_rule(self);
        while (@enumToInt(precedence) <= @enumToInt(ParseRule.get(self.parser.current.type).precedence)) {
            self.advance();
            const rule1 = ParseRule.get(self.parser.previous.type);
            const infix_rule = rule1.infix orelse {
                std.log.err("ICE: No infix rule for {s}", .{@tagName(self.parser.previous.type)});
                self.parser.panicMode = true;
                self.parser.hadError = true;
                return;
            };
            infix_rule(self);
        }
    }
};
