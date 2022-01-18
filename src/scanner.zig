pub const Token = struct {
    const Self = Token;
    type: TokenType = .Error,
    str: []const u8 = "",
    line: u32 = 0,
};

pub const TokenType = enum {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // Literals.
    Identifier,
    String,
    Number,
    // Keywords.
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Error,
    EOF,
};

pub const Scanner = struct {
    const Self = Scanner;

    start: [*]const u8,
    end: [*]const u8,
    current: [*]const u8,
    line: u32,

    pub fn init(source: []const u8) Self {
        const start: [*]const u8 = source.ptr;
        return Self{ .start = start, .end = start + source.len, .current = start, .line = 1 };
    }

    pub fn scanToken(self: *Self) Token {
        self.skipWhitespace();
        self.start = self.current;
        if (self.isAtEnd()) {
            return self.makeToken(TokenType.EOF);
        }
        const c = self.advance();
        if (isAlpha(c)) {
            return self.identifier();
        }
        if (isDigit(c)) {
            return self.number();
        }
        switch (c) {
            '(' => return self.makeToken(.LeftParen),
            ')' => return self.makeToken(.RightParen),
            '{' => return self.makeToken(.LeftBrace),
            '}' => return self.makeToken(.RightBrace),
            ';' => return self.makeToken(.Semicolon),
            ',' => return self.makeToken(.Comma),
            '.' => return self.makeToken(.Dot),
            '-' => return self.makeToken(.Minus),
            '+' => return self.makeToken(.Plus),
            '/' => return self.makeToken(.Slash),
            '*' => return self.makeToken(.Star),
            '!' => return self.makeToken(if (self.match('=')) .BangEqual else .Bang),
            '=' => return self.makeToken(if (self.match('=')) .EqualEqual else .Equal),
            '<' => return self.makeToken(if (self.match('=')) .LessEqual else .Less),
            '>' => return self.makeToken(if (self.match('=')) .GreaterEqual else .Greater),
            '"' => return self.string(),
            else => return self.errorToken("Unexpected character."),
        }
    }

    fn isAtEnd(self: *Self) bool {
        return self.current == self.end;
    }

    fn advance(self: *Self) u8 {
        const r = self.current[0];
        self.current += 1;
        return r;
    }

    fn peek(self: *Self) u8 {
        if (self.isAtEnd()) {
            return 0;
        }
        return self.current[0];
    }

    fn peekNext(self: *Self) u8 {
        if (self.isAtEnd()) {
            return 0;
        }
        return self.current[1];
    }

    fn match(self: *Self, expected: u8) bool {
        if (self.isAtEnd()) {
            return false;
        }
        if (self.current[0] != expected) {
            return false;
        }
        self.current += 1;
        return true;
    }

    fn makeToken(self: *Self, ty: TokenType) Token {
        const n = @ptrToInt(self.current) - @ptrToInt(self.start);
        return Token{
            .type = ty,
            .str = self.start[0..n],
            .line = self.line,
        };
    }

    fn errorToken(self: *Self, message: []const u8) Token {
        return Token{
            .type = TokenType.Error,
            .str = message,
            .line = self.line,
        };
    }

    pub fn skipWhitespace(self: *Self) void {
        while (true) {
            const c = self.peek();
            switch (c) {
                ' ', '\r', '\t' => {
                    _ = self.advance();
                },
                '\n' => {
                    self.line += 1;
                    _ = self.advance();
                    break;
                },
                '/' => {
                    if (self.peekNext() == '/') {
                        // A comment goes until the end of the line.
                        while (self.peek() != '\n' and !self.isAtEnd()) {
                            _ = self.advance();
                        }
                    } else {
                        return;
                    }
                    break;
                },
                else => {
                    return;
                },
            }
        }
    }

    fn string(self: *Self) Token {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') {
                self.line += 1;
            }
            _ = self.advance();
        }
        if (self.isAtEnd()) {
            return self.errorToken("Unterminated string");
        }
        // The closing quote
        _ = self.advance();
        return self.makeToken(TokenType.String);
    }

    fn number(self: *Self) Token {
        while (isDigit(self.peek())) {
            _ = self.advance();
        }
        if (self.peek() == '.' and isDigit(self.peekNext())) {
            // The "."
            _ = self.advance();
            while (isDigit(self.peek())) {
                _ = self.advance();
            }
        }
        return self.makeToken(TokenType.Number);
    }

    fn identifier(self: *Self) Token {
        while (isAlpha(self.peek()) or isDigit(self.peek())) {
            _ = self.advance();
        }
        return self.makeToken(self.identifierType());
    }

    fn identifierType(self: *Self) TokenType {
        switch (self.start[0]) {
            'a' => return self.checkKeyword(1, "nd", TokenType.And),
            'c' => return self.checkKeyword(1, "lass", TokenType.Class),
            'e' => return self.checkKeyword(1, "lse", TokenType.Else),
            'f' => {
                if (self.currentLength() > 1) {
                    switch (self.start[1]) {
                        'a' => return self.checkKeyword(2, "lse", TokenType.False),
                        'o' => return self.checkKeyword(2, "r", TokenType.For),
                        'u' => return self.checkKeyword(2, "n", TokenType.Fun),
                        else => return TokenType.Identifier,
                    }
                }
                return TokenType.Identifier;
            },
            'i' => return self.checkKeyword(1, "f", TokenType.If),
            'n' => return self.checkKeyword(1, "il", TokenType.Nil),
            'o' => return self.checkKeyword(1, "r", TokenType.Or),
            'p' => return self.checkKeyword(1, "rint", TokenType.Print),
            'r' => return self.checkKeyword(1, "eturn", TokenType.Return),
            's' => return self.checkKeyword(1, "uper", TokenType.Super),
            't' => {
                if (self.currentLength() > 1) {
                    switch (self.start[1]) {
                        'h' => return self.checkKeyword(2, "is", TokenType.This),
                        'r' => return self.checkKeyword(2, "ue", TokenType.True),
                        else => return TokenType.Identifier,
                    }
                }
                return TokenType.Identifier;
            },
            'v' => return self.checkKeyword(1, "ar", TokenType.Var),
            'w' => return self.checkKeyword(1, "hile", TokenType.While),
            else => return TokenType.Identifier,
        }
    }

    fn checkKeyword(self: *Self, start: usize, rest: []const u8, ty: TokenType) TokenType {
        if (self.currentLength() != start + rest.len) {
            return TokenType.Identifier;
        }
        for (rest) |v, i| {
            if (self.start[start + i] != v) {
                return TokenType.Identifier;
            }
        }
        return ty;
    }

    fn currentLength(self: *Self) usize {
        return @ptrToInt(self.current) - @ptrToInt(self.start);
    }
};

fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn isAlpha(c: u8) bool {
    return (c >= 'a' and c <= 'z') or
        (c >= 'A' and c <= 'Z') or c == '_';
}
