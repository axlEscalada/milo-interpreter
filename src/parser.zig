const std = @import("std");
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const Allocator = std.mem.Allocator;
const Expr = @import("expression.zig").Expr;
const Object = @import("expression.zig").Object;
const Logger = @import("logger.zig");

pub const Parser = @This();

current: usize = 0,
tokens: []*Token,
allocator: Allocator,

pub fn expression(self: *Parser) !*Expr {
    return self.equality();
}

fn equality(self: *Parser) !*Expr {
    var expr = self.comparison();

    var tokenTypes = [2]TokenType{ TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL };
    while (self.match(&tokenTypes)) {
        const operator = self.previous();
        var right = self.comparison();
        expr = Expr.initBinary(self.allocator, operator, expr, right);
    }
    return expr;
}

fn comparison(self: *Parser) !*Expr {
    var expr = self.term();
    var tokenTypes = [4]TokenType{ TokenType.GREATER, TokenType.GREATER_EQUAL, TokenType.LESS, TokenType.LESS_EQUAL };
    while (self.match(&tokenTypes)) {
        const operator = self.previous();
        var right = self.term();
        expr = Expr.initBinary(self.allocator, operator, expr, right);
    }
    return expr;
}

fn term(self: *Parser) !*Expr {
    var expr = self.factor();
    var tokenTypes = [2]TokenType{ TokenType.MINUS, TokenType.PLUS };
    while (self.match(&tokenTypes)) {
        const operator = self.previous();
        var right = self.factor();
        expr = Expr.initBinary(self.allocator, operator, expr, right);
    }
    return expr;
}

fn factor(self: *Parser) !*Expr {
    var expr = self.unary();
    var tokenTypes = [2]TokenType{ TokenType.STAR, TokenType.SLASH };
    while (self.match(&tokenTypes)) {
        const operator = self.previous();
        var right = self.unary();
        expr = Expr.initBinary(self.allocator, operator, expr, right);
    }
    return expr;
}

fn unary(self: *Parser) !*Expr {
    var tokenTypes = [2]TokenType{ TokenType.BANG, TokenType.LESS };
    if (self.match(&tokenTypes)) {
        var operator = self.previous();
        var right = self.unary();
        return Expr.initUnary(self.allocator, right, operator);
    }
    return self.primary();
}

fn primary(self: *Parser) !*Expr {
    var tokenTypes = [_]TokenType{ TokenType.STRING, TokenType.NUMBER, TokenType.NIL, TokenType.TRUE, TokenType.FALSE };
    if (self.matchType(&tokenTypes)) |tokenType| {
        return Expr.initLiteral(self.allocator, self.peek().lexer, self.createLiteral(tokenType, self.peek().lexer));
    }
    var parType = [1]TokenType{TokenType.LEFT_PAREN};
    if (self.match(&parType)) {
        var expr = self.expression();
        _ = self.consume(TokenType.RIGHT_PAREN, "Expr ')' after expression");
        return Expr.initGrouping(self.allocator, expr);
    }
}

fn createLiteral(self: *Parser, tokenType: TokenType, lexer: []const u8) !*Object {
    return switch (tokenType) {
        .STRING => return Object.initString(self.allocator, lexer),
        .FALSE => return Object.initBool(self.allocator, false),
        .TRUE => return Object.initBool(self.allocator, true),
        .NUMBER => return Object.initFloat(self.allocator, std.fmt.parseFloat(f64, lexer)),
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
    return self.tokens[self.current].*;
}

fn previous(self: *Parser) Token {
    return self.tokens[self.current - 1].*;
}

const ParserError = error{
    NotClosingParenthesisError,
};
