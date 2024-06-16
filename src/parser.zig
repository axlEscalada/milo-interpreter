const std = @import("std");
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const Expr = @import("expression.zig").Expr;
const Object = @import("expression.zig").Object;
const Logger = @import("logger.zig");
const Stmt = @import("statement.zig").Stmt;

pub const Parser = @This();

current: usize = 0,
tokens: std.ArrayList(Token),
allocator: Allocator,

pub fn parse(self: *Parser) ![]*Stmt {
    var statements = std.ArrayList(*Stmt).init(self.allocator);
    while (!self.isAtEnd()) {
        if (try self.declaration()) |st| {
            try statements.append(st);
        }
    }
    return statements.items;
}

fn declaration(self: *Parser) !?*Stmt {
    var types = [_]TokenType{TokenType.VAR};
    if (self.match(&types)) return self.varDeclaration();
    return self.statement() catch |e| {
        std.log.err("error {any}\n", .{e});
        self.syncrhonize();
        return null;
    };
}

fn varDeclaration(self: *Parser) !*Stmt {
    const name = self.consume(TokenType.IDENTIFIER, "Expect variable name.");
    var initializer: ?*Expr = null;
    var token_types = [_]TokenType{TokenType.EQUAL};
    if (self.match(&token_types)) {
        initializer = try self.expression();
    }
    _ = self.consume(TokenType.SEMICOLON, "Expect ';' after variable declaration.");
    return Stmt.init(self.allocator, .{ .variable = .{ .name = name, .initializer = initializer } });
}

fn statement(self: *Parser) !*Stmt {
    var tokenTypes = [_]TokenType{TokenType.PRINT};
    if (self.match(&tokenTypes)) return self.printStatement();
    return self.expressionStatement();
}

fn printStatement(self: *Parser) !*Stmt {
    const value = try self.expression();
    _ = self.consume(TokenType.SEMICOLON, "Expect ';' after value.");
    const stmt = try self.allocator.create(Stmt);
    stmt.* = .{ .print = .{ .expression = value } };
    return stmt;
}

fn expressionStatement(self: *Parser) !*Stmt {
    const expr = try self.expression();
    _ = self.consume(TokenType.SEMICOLON, "Expect ';' after expression.");
    const stmt = try self.allocator.create(Stmt);
    stmt.* = .{ .expression = .{ .expression = expr } };
    return stmt;
}

pub fn init(tokens: std.ArrayList(Token), allocator: Allocator) Parser {
    return .{
        .tokens = tokens,
        .allocator = allocator,
    };
}

pub fn expression(self: *Parser) ParserError!*Expr {
    return self.assignment();
}

fn assignment(self: *Parser) ParserError!*Expr {
    const expr = try self.equality();

    var token_type = [_]TokenType{TokenType.EQUAL};
    if (self.match(&token_type)) {
        const equals = self.previous();
        const value = try self.assignment();

        if (expr.* == Expr.variable) {
            const name = expr.variable.name;
            return Expr.init(self.allocator, .{ .assign = .{ .name = name, .value = value } }) catch |e| {
                std.log.err("Error parsing assignment expression {any}\n", .{e});
                return ParserError.ParsingAssign;
            };
        }
        self.err(equals, "Invalid assignment target.");
    }
    return expr;
}

fn err(self: *Parser, token: Token, message: []const u8) void {
    if (token.tokenType == TokenType.EOF) {
        Logger.report(self.allocator, token.line, " at end", message);
    } else {
        const m = std.fmt.allocPrint(self.allocator, " at '{s}'", .{token.lexer}) catch |e| {
            std.debug.print("Error {!}", .{e});
            @panic("Error logging");
        };
        Logger.report(self.allocator, token.line, m, message);
    }
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
        const right = try self.unary();
        return Expr.initUnary(self.allocator, right, operator) catch |e| {
            std.log.err("Error parsing unary {!}\n", .{e});
            return ParserError.ParsingUnary;
        };
    }
    return self.primary();
}

fn primary(self: *Parser) ParserError!*Expr {
    var tokenTypes = [_]TokenType{ TokenType.STRING, TokenType.NUMBER, TokenType.NIL, TokenType.TRUE, TokenType.FALSE };
    if (self.matchType(&tokenTypes)) |token| {
        const literal = self.createLiteral(token.tokenType, token.lexer) catch |e| {
            std.log.err("Error parsing primary {!}\n", .{e});
            return ParserError.ParsingLiteral;
        };
        const expr = Expr.initLiteral(self.allocator, token.lexer, literal) catch |e| {
            std.log.err("Error parsing literal {!}\n", .{e});
            return ParserError.ParsingLiteral;
        };
        return expr;
    }
    var identifier = [_]TokenType{TokenType.IDENTIFIER};
    if (self.match(&identifier)) {
        return Expr.initVariable(self.allocator, self.previous()) catch |e| {
            std.log.err("Error parsing variable init {!}\n", .{e});
            return ParserError.ParsingVariable;
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

fn matchType(self: *Parser, types: []TokenType) ?Token {
    for (types) |tp| {
        if (self.check(tp)) {
            const token = self.peek();
            _ = self.advance();
            return token;
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
    return self.tokens.items[self.current];
}

fn previous(self: *Parser) Token {
    return self.tokens.items[self.current - 1];
}

fn syncrhonize(self: *Parser) void {
    _ = self.advance();

    while (!self.isAtEnd()) {
        if (self.previous().tokenType == TokenType.SEMICOLON) return;

        switch (self.peek().tokenType) {
            .CLASS, .FUN, .VAR, .FOR, .IF, .WHILE, .PRINT, .RETURN => break,
            else => continue,
        }
        _ = self.advance();
    }
}

const ParserError = error{
    NotClosingParenthesisError,
    ParsingExpression,
    ParsingLiteral,
    ParsingUnary,
    ParsingBinary,
    ParsingPrimary,
    ParsingVariable,
    ParsingAssign,
};
