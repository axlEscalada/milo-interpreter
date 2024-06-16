const std = @import("std");
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const Logger = @import("logger.zig");
const Object = @import("expression.zig").Object;

fn initKeywords() std.StaticStringMap(TokenType) {
    const KV = struct { []const u8, TokenType };
    const kvs: [16]KV = [_]KV{ .{ "and", TokenType.AND }, .{ "class", TokenType.CLASS }, .{ "else", TokenType.ELSE }, .{ "false", TokenType.FALSE }, .{ "for", TokenType.FOR }, .{ "fun", TokenType.FUN }, .{ "if", TokenType.IF }, .{ "nil", TokenType.NIL }, .{ "or", TokenType.OR }, .{ "print", TokenType.PRINT }, .{ "return", TokenType.RETURN }, .{ "super", TokenType.SUPER }, .{ "this", TokenType.THIS }, .{ "true", TokenType.TRUE }, .{ "var", TokenType.VAR }, .{ "while", TokenType.WHILE } };

    return std.StaticStringMap(TokenType).initComptime(kvs);
}

const keywords = initKeywords();

const Scanner = @This();

source: []const u8,
tokens: std.ArrayList(Token),
start: u16 = 0,
current: u16 = 0,
line: u16 = 1,
alloc: std.mem.Allocator = undefined,

pub fn init(source: []const u8, alloc: std.mem.Allocator) Scanner {
    return .{ .source = source, .alloc = alloc, .tokens = std.ArrayList(Token).init(alloc) };
}

pub fn deinit(self: *Scanner) void {
    self.tokens.deinit();
}

pub fn scanTokens(self: *Scanner) !std.ArrayList(Token) {
    while (!self.isAtEnd()) {
        self.start = self.*.current;
        try self.scanToken();
    }

    const token = self.createToken(TokenType.EOF, "", self.line);
    try self.tokens.append(token);
    // for (self.tokens.items) |t| {
    //     std.debug.print("ITEMS: {s}\n", .{t.lexer});
    // }
    return self.tokens;
}

fn isAtEnd(self: *Scanner) bool {
    return self.*.current >= self.*.source.len;
}

fn scanToken(self: *Scanner) !void {
    const c: u8 = self.advance();
    try switch (c) {
        '(' => try self.addToken(TokenType.LEFT_PAREN),
        ')' => try self.addToken(TokenType.RIGHT_PAREN),
        '{' => try self.addToken(TokenType.LEFT_BRACE),
        '}' => try self.addToken(TokenType.RIGHT_BRACE),
        ',' => try self.addToken(TokenType.COMMA),
        '.' => try self.addToken(TokenType.DOT),
        '-' => if (self.isDigit(self.peekNext())) try self.number() else try self.addToken(TokenType.MINUS),
        '+' => try self.addToken(TokenType.PLUS),
        ';' => try self.addToken(TokenType.SEMICOLON),
        '*' => try self.addToken(TokenType.STAR),
        '!' => try self.addToken(if (self.match('=')) TokenType.BANG_EQUAL else TokenType.BANG),
        '=' => try self.addToken(if (self.match('=')) TokenType.EQUAL_EQUAL else TokenType.EQUAL),
        '<' => try self.addToken(if (self.match('=')) TokenType.LESS_EQUAL else TokenType.LESS),
        '>' => try self.addToken(if (self.match('=')) TokenType.GREATER_EQUAL else TokenType.GREATER),
        '/' => if (self.match('/')) {
            while (self.peek() != '\n' and !self.isAtEnd()) {
                _ = self.advance();
            }
        } else try self.addToken(TokenType.SLASH),
        ' ', '\r', '\t' => {},
        '\n' => self.line += 1,
        '"' => self.string(),
        '\'' => self.char(),
        else => if (self.isDigit(c)) {
            try self.number();
        } else if (self.isAlpha(c)) {
            try self.identifier();
        } else Logger.report(self.*.alloc, self.*.line, "", "Unexpected character."),
    };
}

fn char(self: *Scanner) !void {
    std.debug.print("PEek char: {c} is alpha {}", .{ self.peek(), self.isAlphaNumeric(self.peek()) });
    // _ = self.advance();
    if (self.isAlphaNumeric(self.peek()) and !self.isAlphaNumeric(self.peekNext())) {
        _ = self.advance();
        _ = self.advance();
        try self.addToken(TokenType.CHAR);
    } else Logger.report(self.alloc, self.line, "", "Invalid char");
}

fn identifier(self: *Scanner) !void {
    while (self.isAlphaNumeric(self.peek())) _ = self.advance();
    const text = self.source[self.start..self.current];
    var tokenType: ?TokenType = keywords.get(text);
    if (tokenType == null) tokenType = TokenType.IDENTIFIER;
    try self.addToken(tokenType.?);
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

fn number(self: *Scanner) !void {
    if (self.peek() == '-' and self.isDigit(self.peekNext())) {
        _ = self.advance();
    }
    while (self.isDigit(self.peek())) _ = self.advance();

    if (self.peek() == '.' and self.isDigit(self.peekNext())) {
        _ = self.advance();
        while (self.isDigit(self.peek())) _ = self.advance();
    }
    try self.addToken(TokenType.NUMBER);
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
        Logger.report(self.alloc, self.line, "", "Unterminated string.");
    } else {
        // The closing ".
        _ = self.advance();

        // Trim the surrounding quotes.
        // var value = self.source[self.start + 1 .. self.current - 1];
        try self.addToken(TokenType.STRING);
    }
}

fn peek(self: *Scanner) u8 {
    if (self.isAtEnd()) return '\\';
    return self.source[self.current];
}

fn advance(self: *Scanner) u8 {
    const current = self.*.source[self.*.current];
    self.*.current += 1;
    return current;
}

fn addToken(self: *Scanner, tokenType: TokenType) !void {
    const text = self.*.source[self.*.start..self.*.current];

    const token = self.createToken(tokenType, text, self.*.line);
    try self.tokens.append(token);
}

fn match(self: *Scanner, expected: u8) bool {
    if (self.isAtEnd()) return false;
    if (self.*.source[self.current] != expected) return false;
    self.*.current += 1;
    return true;
}

fn createToken(self: *Scanner, tokenType: TokenType, text: []const u8, line: u16) Token {
    return self.createLiteralToken(tokenType, text, line, null);
}

fn createLiteralToken(self: *Scanner, tokenType: TokenType, text: []const u8, line: u16, literal: ?Object) Token {
    _ = self;
    // const token: *Token = self.alloc.create(Token) catch |e| {
    //     std.debug.print("Error creating Token: {!}", .{e});
    //     @panic("Error allocating token");
    // };
    //
    // token.*.tokenType = tokenType;
    // token.*.lexer = text;
    // token.*.line = line;
    // token.*.literal = literal;
    // return token;
    return .{
        .tokenType = tokenType,
        .lexer = text,
        .line = line,
        .literal = literal,
    };
}
