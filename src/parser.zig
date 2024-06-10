const std = @import("std");
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const Allocator = std.mem.Allocator;
const Expr = @import("expression.zig").Expr;
const Object = @import("expression.zig").Object;
const Logger = @import("logger.zig");

pub const Parser = @This();

current: usize = 0,
tokens: std.ArrayList(*Token),
allocator: Allocator,

pub fn init(tokens: std.ArrayList(*Token), allocator: Allocator) Parser {
    std.debug.print("Tokens parser {any}\n", .{tokens});
    return .{
        .tokens = tokens,
        .allocator = allocator,
    };
}

pub fn expression(self: *Parser) ParserError!*Expr {
    return self.equality() catch |e| {
        std.log.err("Error parsing expression {!}\n", .{e});
        return ParserError.ParsingExpression;
    };
}

fn equality(self: *Parser) ParserError!*Expr {
    var expr = self.comparison() catch |e| {
        return e;
    };

    var tokenTypes = [2]TokenType{ TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL };
    while (self.match(&tokenTypes)) {
        const operator = self.previous();
        const right = self.comparison() catch |e| {
            std.log.err("Error parsing equality {!}\n", .{e});
            return e;
        };
        expr = Expr.initBinary(self.allocator, operator, expr, right) catch |e| {
            std.log.err("Error parsing binary {!}\n", .{e});
            return ParserError.ParsingBinary;
        };
    }
    return expr;
}

fn comparison(self: *Parser) ParserError!*Expr {
    var expr = self.term() catch |e| {
        std.log.err("Error parsing comparison {!}\n", .{e});
        return e;
    };
    var tokenTypes = [4]TokenType{ TokenType.GREATER, TokenType.GREATER_EQUAL, TokenType.LESS, TokenType.LESS_EQUAL };
    while (self.match(&tokenTypes)) {
        const operator = self.previous();
        const right = self.term() catch |e| return e;
        expr = Expr.initBinary(self.allocator, operator, expr, right) catch |e| {
            std.log.err("Error parsing binary {!}\n", .{e});
            return ParserError.ParsingBinary;
        };
    }
    return expr;
}

fn term(self: *Parser) ParserError!*Expr {
    var expr = self.factor() catch |e| return e;
    var tokenTypes = [2]TokenType{ TokenType.MINUS, TokenType.PLUS };
    while (self.match(&tokenTypes)) {
        const operator = self.previous();
        const right = self.factor() catch |e| return e;
        expr = Expr.initBinary(self.allocator, operator, expr, right) catch |e| {
            std.log.err("Error parsing binary {!}\n", .{e});
            return ParserError.ParsingBinary;
        };
    }
    return expr;
}

fn factor(self: *Parser) ParserError!*Expr {
    var expr = self.unary() catch |e| return e;
    var tokenTypes = [2]TokenType{ TokenType.STAR, TokenType.SLASH };
    while (self.match(&tokenTypes)) {
        const operator = self.previous();
        const right = self.unary() catch |e| return e;
        expr = Expr.initBinary(self.allocator, operator, expr, right) catch |e| {
            std.log.err("Error parsing binary {!}\n", .{e});
            return ParserError.ParsingBinary;
        };
    }
    return expr;
}

fn unary(self: *Parser) ParserError!*Expr {
    var tokenTypes = [2]TokenType{ TokenType.BANG, TokenType.LESS };
    if (self.match(&tokenTypes)) {
        const operator = self.previous();
        const right = self.unary() catch |e| return e;
        return Expr.initUnary(self.allocator, right, operator) catch |e| {
            std.log.err("Error parsing unary {!}\n", .{e});
            return ParserError.ParsingUnary;
        };
    }
    return self.primary();
}

fn primary(self: *Parser) ParserError!*Expr {
    var tokenTypes = [_]TokenType{ TokenType.STRING, TokenType.NUMBER, TokenType.NIL, TokenType.TRUE, TokenType.FALSE };
    if (self.matchType(&tokenTypes)) |tokenType| {
        const literal = self.createLiteral(tokenType, self.peek().lexer) catch |e| {
            std.log.err("Error parsing primary {!}\n", .{e});
            return ParserError.ParsingLiteral;
        };
        return Expr.initLiteral(self.allocator, self.peek().lexer, literal) catch |e| {
            std.log.err("Error parsing literal {!}\n", .{e});
            return ParserError.ParsingLiteral;
        };
    }
    var parType = [1]TokenType{TokenType.LEFT_PAREN};
    if (self.match(&parType)) {
        const expr = self.expression() catch |e| return e;
        _ = self.consume(TokenType.RIGHT_PAREN, "Expr ')' after expression");
        return Expr.initGrouping(self.allocator, expr) catch |e| {
            std.log.err("Error parsing primary {!}\n", .{e});
            return ParserError.ParsingPrimary;
        };
    }
    @panic("Can't close parenthesis");
}

pub fn createLiteral(self: *Parser, tokenType: TokenType, lexer: []const u8) !*Object {
    std.debug.print("LEXER IS {s}\n", .{lexer});
    return switch (tokenType) {
        .STRING => return try Object.initString(self.allocator, lexer),
        .FALSE => return try Object.initBool(self.allocator, false),
        .TRUE => return try Object.initBool(self.allocator, true),
        .NUMBER => return try Object.initFloat(self.allocator, std.fmt.parseFloat(f64, lexer) catch @panic("Error parsing float")),
        else => unreachable,
    };
}

fn consume(self: *Parser, tokenType: TokenType, msg: []const u8) Token {
    if (self.check(tokenType)) return self.advance();

    Logger.report(self.allocator, self.peek().line, "Error", msg);
    return self.peek();
}

fn match(self: *Parser, types: []TokenType) bool {
    for (types) |tp| {
        if (self.check(tp)) {
            _ = self.advance();
            return true;
        }
    }
    return false;
}

fn matchType(self: *Parser, types: []TokenType) ?TokenType {
    for (types) |tp| {
        if (self.check(tp)) {
            _ = self.advance();
            return tp;
        }
    }
    return null;
}

fn check(self: *Parser, tokenType: TokenType) bool {
    if (self.isAtEnd()) return false;
    return self.peek().tokenType == tokenType;
}

fn advance(self: *Parser) Token {
    if (!self.isAtEnd()) self.current += 1;
    return self.previous();
}

fn isAtEnd(self: *Parser) bool {
    return self.peek().tokenType == TokenType.EOF;
}

fn peek(self: *Parser) Token {
    std.debug.print("PEEK IS {any}\n", .{self.tokens.items[self.current].tokenType});
    return self.tokens.items[self.current].*;
}

fn previous(self: *Parser) Token {
    return self.tokens.items[self.current - 1].*;
}

const ParserError = error{
    NotClosingParenthesisError,
    ParsingExpression,
    ParsingLiteral,
    ParsingUnary,
    ParsingBinary,
    ParsingPrimary,
};
