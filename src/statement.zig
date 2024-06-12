const std = @import("std");
const Token = @import("token.zig").Token;
const Expr = @import("expression.zig").Expr;

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

    pub fn accept(self: *Stmt, comptime T: type, visitor: anytype) T {
        return switch (self.*) {
            .block => visitor.visitBlock(self),
            .class => visitor.visitClass(self),
            .expression => visitor.visitExpression(self),
            .function => visitor.visitFunction(self),
            .if_statement => visitor.visitIf(self),
            .print => visitor.visitPrint(self),
            .return_statement => visitor.visitReturn(self),
            .variable => visitor.visitVariable(self),
            .while_statement => visitor.visitWhile(self),
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
    expression: *Expr,
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
    expression: *Expr,
};

pub const Return = struct {
    keyword: Token,
    value: Expr,
};

pub const Variable = struct {
    name: *Token,
    initializer: ?*Expr,
};

pub const While = struct {
    condition: Expr,
    body: *Stmt,
};
