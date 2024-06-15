const std = @import("std");
const Token = @import("token.zig").Token;
const Expr = @import("expression.zig").Expr;
const Interpreter = @import("interpreter.zig").Interpreter;
const TokenType = @import("token.zig").TokenType;

pub const StatementType = enum { block, class, expression, function, if_statement, print, return_statement, variable, while_statement };

pub const Stmt = union(StatementType) {
    block: Block,
    class: Class,
    expression: Expression,
    function: Function,
    if_statement: If,
    print: Print,
    return_statement: Return,
    variable: Variable,
    while_statement: While,

    pub fn init(allocator: std.mem.Allocator, args: anytype) !*Stmt {
        const stmt = try allocator.create(Stmt);
        stmt.* = args;
        return stmt;
    }

    pub fn accept(self: *Stmt, comptime T: type, visitor: *anyopaque) T {
        var it: *Interpreter = @ptrCast(@alignCast(visitor));
        std.debug.print("interpreter pointer addr {any}\n", .{&it});
        const a = Token{ .line = 1, .lexer = "a", .literal = null, .tokenType = TokenType.IDENTIFIER };
        std.debug.print("IS STORED A IN MAP {any}\n", .{it.environment.get(a)});
        return switch (self.*) {
            .block => it.visitBlock(self),
            .class => it.visitClass(self),
            .expression => it.visitExpression(self),
            .function => it.visitFunction(self),
            .if_statement => it.visitIf(self),
            .print => it.visitPrint(self),
            .return_statement => it.visitReturn(self),
            .variable => it.visitVariableStmt(self),
            .while_statement => it.visitWhile(self),
        };
    }
};

pub const Block = struct {
    statements: []Stmt,
};

pub const Class = struct {
    name: Token,
    superclass: Expr,
    methods: []Stmt,
};

pub const Expression = struct {
    expression: Expr,
};

pub const Function = struct {
    name: Token,
    params: []Token,
    body: []Stmt,
};

pub const If = struct {
    condition: Expr,
    then_branch: *Stmt,
    else_branch: *Stmt,
};

pub const Print = struct {
    expression: Expr,
};

pub const Return = struct {
    keyword: Token,
    value: Expr,
};

pub const Variable = struct {
    name: *Token,
    initializer: ?Expr,
};

pub const While = struct {
    condition: Expr,
    body: *Stmt,
};
