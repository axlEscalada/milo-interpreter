const std = @import("std");
const Token = @import("token.zig").Token;
const Expr = @import("expression.zig").Expr;
const Interpreter = @import("interpreter.zig").Interpreter;
const TokenType = @import("token.zig").TokenType;

pub const StatementType = enum { block, class, expression, function, if_statement, print, return_statement, variable, while_statement, @"for" };

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
    @"for": For,

    pub fn init(allocator: std.mem.Allocator, args: Stmt) !*Stmt {
        const stmt = try allocator.create(Stmt);
        stmt.* = args;
        return stmt;
    }

    pub fn accept(self: *Stmt, comptime T: type, visitor: anytype) T {
        return switch (self.*) {
            .block => visitor.visitBlockStmt(self),
            .class => visitor.visitClassStmt(self),
            .expression => visitor.visitExpressionStmt(self),
            .function => visitor.visitFunctionStmt(self),
            .if_statement => visitor.visitIfStmt(self),
            .print => visitor.visitPrintStmt(self),
            .return_statement => visitor.visitReturnStmt(self),
            .variable => visitor.visitVariableStmt(self),
            .while_statement => visitor.visitWhileStmt(self),
            .@"for" => visitor.visitForStmt(self),
        };
    }
};

pub const Block = struct {
    statements: []*Stmt,
};

pub const Class = struct {
    name: Token,
    superclass: *Expr,
    methods: []Stmt,
};

pub const Expression = struct {
    expression: *Expr,
};

pub const Function = struct {
    name: Token,
    params: []Token,
    body: []*Stmt,
};

pub const If = struct {
    condition: *Expr,
    then_branch: *Stmt,
    else_branch: ?*Stmt,
};

pub const Print = struct {
    expression: *Expr,
};

pub const Return = struct {
    keyword: Token,
    value: ?*Expr,
};

pub const Variable = struct {
    name: Token,
    initializer: ?*Expr,
};

pub const While = struct {
    condition: *Expr,
    body: *Stmt,
};

pub const For = struct {
    condition: *Expr,
    body: *Stmt,
};
