const std = @import("std");
var hadError = false;
var keywords: std.StringHashMapUnmanaged(TokenType) = .{};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    try initKeywords(allocator);
    defer keywords.deinit(allocator);
    // std.debug.print("Args {s}", .{args.len});
    if (args.len > 2) {
        std.debug.print("Usage milo [script]", .{});
        std.process.exit(0);
    } else if (args.len == 2) {
        try runFile(args[0], allocator);
    } else {
        try runPrompt(allocator);
        if (hadError) {
            std.os.exit(64);
        }
    }
}

fn initKeywords(alloc: std.mem.Allocator) !void {
    // _ = keywords.init(alloc);
    try keywords.put(alloc, "and", TokenType.AND);
    try keywords.put(alloc, "class", TokenType.CLASS);
    try keywords.put(alloc, "else", TokenType.ELSE);
    try keywords.put(alloc, "false", TokenType.FALSE);
    try keywords.put(alloc, "for", TokenType.FOR);
    try keywords.put(alloc, "fun", TokenType.FUN);
    try keywords.put(alloc, "if", TokenType.IF);
    try keywords.put(alloc, "nil", TokenType.NIL);
    try keywords.put(alloc, "or", TokenType.OR);
    try keywords.put(alloc, "print", TokenType.PRINT);
    try keywords.put(alloc, "return", TokenType.RETURN);
    try keywords.put(alloc, "super", TokenType.SUPER);
    try keywords.put(alloc, "this", TokenType.THIS);
    try keywords.put(alloc, "true", TokenType.TRUE);
    try keywords.put(alloc, "var", TokenType.VAR);
    try keywords.put(alloc, "while", TokenType.WHILE);
}

fn runPrompt(alloc: std.mem.Allocator) !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    var buffer: [1024]u8 = undefined;
    var line: []u8 = undefined;
    while (true) {
        _ = try stdout.write(">> ");
        line = try stdin.readUntilDelimiterOrEof(&buffer, '\n') orelse "";
        if (std.mem.eql(u8, line, "exit")) std.process.exit(0);
        var scanner = Scanner{ .source = line, .alloc = alloc };
        var rs = try scanner.scanTokens();

        if (!hadError) {
            for (rs[0..]) |*r| {
                // if (std.mem.eql(u8, r.*.lexer, "exit")) std.process.exit(0);
                if (r.*.tokenType == TokenType.EOF) break;
                const final_url = try std.fmt.allocPrint(alloc, "Token: `{s}` Type: {}\n", .{ r.*.lexer, r.*.tokenType });
                defer alloc.free(final_url);
                _ = try stdout.write(final_url);
            }
        }

        hadError = false;
    }
}

fn runFile(path: []const u8, allocator: std.mem.Allocator) !void {
    _ = allocator;
    var file = try std.fs.cwd().openFile(path, .{});
    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [1024]u8 = undefined;

    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        std.debug.print("Line: {s}", .{line});
    }
}

fn err(line: u16, message: []const u8, alloc: std.mem.Allocator) !void {
    try report(line, "", message, alloc);
}

fn report(line: u16, where: []const u8, message: []const u8, alloc: std.mem.Allocator) !void {
    const stdout = std.io.getStdErr().writer();
    const final_url = try std.fmt.allocPrint(alloc, "[line {}] Error {s}: {s} \n", .{ line, where, message });
    defer alloc.free(final_url);
    _ = try stdout.write(final_url);
    hadError = true;
}

const Scanner = struct {
    source: []const u8,
    tokens: [1024]*Token = undefined,
    tokensSize: usize = 0,
    start: u16 = 0,
    current: u16 = 0,
    line: u16 = 1,
    alloc: std.mem.Allocator = undefined,

    fn scanTokens(self: *Scanner) ![1024]*Token {
        while (!self.isAtEnd()) {
            self.*.start = self.*.current;
            try self.*.scanToken();
        }

        self.*.tokens[self.*.tokensSize] = self.createToken(TokenType.EOF, "", self.*.line).?;
        self.*.tokensSize += 1;
        return self.tokens;
    }

    fn isAtEnd(self: *Scanner) bool {
        return self.*.current >= self.*.source.len;
    }

    fn scanToken(self: *Scanner) !void {
        var c: u8 = self.advance();
        try switch (c) {
            '(' => self.addToken(TokenType.LEFT_PAREN),
            ')' => self.addToken(TokenType.RIGHT_PAREN),
            '{' => self.addToken(TokenType.LEFT_BRACE),
            '}' => self.addToken(TokenType.RIGHT_BRACE),
            ',' => self.addToken(TokenType.COMMA),
            '.' => self.addToken(TokenType.DOT),
            '-' => self.addToken(TokenType.MINUS),
            '+' => self.addToken(TokenType.PLUS),
            ';' => self.addToken(TokenType.SEMICOLON),
            '*' => self.addToken(TokenType.STAR),
            '!' => self.addToken(if (self.match('=')) TokenType.BANG_EQUAL else TokenType.BANG),
            '=' => self.addToken(if (self.match('=')) TokenType.EQUAL_EQUAL else TokenType.EQUAL),
            '<' => self.addToken(if (self.match('=')) TokenType.LESS_EQUAL else TokenType.LESS),
            '>' => self.addToken(if (self.match('=')) TokenType.GREATER_EQUAL else TokenType.GREATER),
            '/' => if (self.match('/')) {
                while (self.peek() != '\n' and !self.isAtEnd()) {
                    _ = self.advance();
                }
            } else self.addToken(TokenType.SLASH),
            ' ', '\r', '\t' => {},
            '\n' => self.line += 1,
            '"' => self.string(),
            '\'' => self.char(),
            else => if (self.isDigit(c)) {
                self.number();
            } else if (self.isAlpha(c)) {
                self.identifier();
            } else err(self.*.line, "Unexpected character.", self.*.alloc),
        };
    }

    fn char(self: *Scanner) !void {
        std.debug.print("PEek char: {c} is alpha {}", .{ self.peek(), self.isAlphaNumeric(self.peek()) });
        // _ = self.advance();
        if (self.isAlphaNumeric(self.peek()) and !self.isAlphaNumeric(self.peekNext())) {
            _ = self.advance();
            _ = self.advance();
            self.addToken(TokenType.CHAR);
        } else {
            try err(self.line, "Invalid char", self.alloc);
        }
    }

    fn identifier(self: *Scanner) void {
        while (self.isAlphaNumeric(self.peek())) _ = self.advance();
        var text = self.source[self.start..self.current];
        var tokenType: ?TokenType = keywords.get(text);
        if (tokenType == null) tokenType = TokenType.IDENTIFIER;
        self.addToken(tokenType.?);
    }

    fn isAlphaNumeric(self: *Scanner, c: u8) bool {
        return self.isAlpha(c) or self.isDigit(c);
    }

    fn isAlpha(self: *Scanner, c: u8) bool {
        _ = self;
        return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
    }

    fn isDigit(self: *Scanner, c: u8) bool {
        _ = self;
        return c >= '0' and c <= '9';
    }

    fn number(self: *Scanner) void {
        while (self.isDigit(self.peek())) _ = self.advance();

        if (self.peek() == '.' and self.isDigit(self.peekNext())) {
            _ = self.advance();
            while (self.isDigit(self.peek())) _ = self.advance();
        }
        self.addToken(TokenType.NUMBER);
    }

    fn peekNext(self: *Scanner) u8 {
        if (self.current + 1 >= self.source.len) return '\\';
        return self.source[self.current + 1];
    }

    fn string(self: *Scanner) !void {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') self.line += 1;
            _ = self.advance();
        }

        if (self.isAtEnd()) {
            try err(self.line, "Unterminated string.", self.alloc);
        } else {
            // The closing ".
            _ = self.advance();

            // Trim the surrounding quotes.
            // var value = self.source[self.start + 1 .. self.current - 1];
            self.addToken(TokenType.STRING);
        }
    }

    fn peek(self: *Scanner) u8 {
        if (self.isAtEnd()) return '\\';
        return self.source[self.current];
    }

    fn advance(self: *Scanner) u8 {
        var current = self.*.source[self.*.current];
        self.*.current += 1;
        return current;
    }

    fn addToken(self: *Scanner, tokenType: TokenType) void {
        var text = self.*.source[self.start..self.current];

        self.*.tokens[self.*.tokensSize] = self.createToken(tokenType, text, self.*.line).?;
        self.*.tokensSize += 1;
    }

    fn match(self: *Scanner, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.*.source[self.current] != expected) return false;
        self.*.current += 1;
        return true;
    }

    fn createToken(self: *Scanner, tokenType: TokenType, text: []const u8, line: u16) ?*Token {
        var token: ?*Token = self.*.alloc.create(Token) catch |e| {
            std.debug.print("Error creating Token: {}", .{e});
            return null;
        };

        if (token) |tk| {
            tk.*.tokenType = tokenType;
            tk.*.lexer = text;
            tk.*.line = line;
        }
        return token;
    }
};

const TokenType = enum {
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,

    // One or two character tokens.
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,

    // Literals.
    IDENTIFIER,
    STRING,
    NUMBER,
    CHAR,

    // Keywords.
    AND,
    CLASS,
    ELSE,
    FALSE,
    FUN,
    FOR,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,

    EOF,
};

const Token = struct {
    tokenType: TokenType,
    lexer: []const u8 = undefined,
    line: u16 = undefined,
};
