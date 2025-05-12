const std = @import("std");
const Allocator = std.mem.Allocator;
const Object = @import("expression.zig").Object;

pub const Token = struct {
    tokenType: TokenType,
    lexer: []const u8,
    line: u16,
    literal: ?Object = null,

    pub fn init(allocator: Allocator, tokenType: TokenType, lexer: []const u8, line: u16, literal: ?Object) !*Token {
        const token = try allocator.create(Token);
        token.* = .{ .tokenType = tokenType, .lexer = lexer, .line = line, .literal = literal };
        return token;
    }

};

pub fn createToken(alloc: Allocator, tokenType: TokenType, text: []const u8, line: u16) *Token {
    const token: *Token = alloc.create(Token) catch |e| {
        std.debug.print("Error creating Token: {!}", .{e});
        @panic("Error allocating token");
    };

    token.*.tokenType = tokenType;
    token.*.lexer = text;
    token.*.line = line;
    return token;
}

pub const TokenType = enum {
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
