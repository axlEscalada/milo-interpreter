const std = @import("std");
const Allocator = std.mem.Allocator;
const Interpreter = @import("interpreter.zig").Interpreter;
const Stmt = @import("statement.zig").Stmt;
const Expr = @import("expression.zig").Expr;
const Object = @import("expression.zig").Object;
const Token = @import("token.zig").Token;

const FunctionType = enum {
    NONE,
    FUNCTION,
};

pub const Resolver = struct {
    allocator: Allocator,
    interpreter: *Interpreter,
    scopes: std.ArrayList(std.StringHashMap(?*Object)),
    current_function: FunctionType,

    pub fn init(allocator: Allocator, interpreter: *Interpreter) !*Resolver {
        const resolver = try allocator.create(Resolver);

        resolver.* = .{
            .allocator = allocator,
            .interpreter = interpreter,
            .scopes = std.ArrayList(std.StringHashMap(?*Object)).init(allocator),
            .current_function = FunctionType.NONE,
        };

        return resolver;
    }

    pub fn resolve(self: *Resolver, statements: []*Stmt) !void {
        for (statements) |st| {
            try st.accept(anyerror!void, self);
        }
    }

    pub fn resolveStmt(self: *Resolver, statement: *Stmt) !void {
        try statement.accept(anyerror!void, self);
    }

    pub fn resolveExpr(self: *Resolver, expr: *Expr) !void {
        try expr.accept(self, void);
    }

    fn beginScope(self: *Resolver) !void {
        const values = std.StringHashMap(?*Object).init(self.allocator);
        try self.scopes.append(values);
    }

    fn endScope(self: *Resolver) !void {
        _ = self.scopes.pop();
    }

    fn declare(self: *Resolver, name: Token) !void {
        if (self.scopes.items.len == 0) return;

        var scope = self.scopes.getLast();
        if (scope.contains(name.lexer)) {
            std.log.err("Already a variable with this name in this scope.\n", .{});
            return error.ShadowingVariable;
        }
        try scope.put(name.lexer, try Object.initBool(self.allocator, false));
    }

    fn define(self: *Resolver, name: Token) !void {
        if (self.scopes.items.len == 0) return;
        var scope = &self.scopes.items[self.scopes.items.len - 1];
        try scope.put(name.lexer, try Object.initBool(self.allocator, true));
    }

    fn resolveLocal(self: *Resolver, expr: Expr, name: Token) !void {
        const size = self.scopes.items.len;
        for (0..size) |i| {
            if (self.scopes.items[size - 1 - i].contains(name.lexer)) {
                try self.interpreter.resolve(expr, self.scopes.items.len - 1 - i);
                return;
            }
        }
    }

    fn resolveFunction(self: *Resolver, stmt: *Stmt, function_type: FunctionType) !void {
        _ = self.current_function;
        self.current_function = function_type;

        try self.beginScope();
        for (stmt.function.params) |param| {
            try self.declare(param);
            try self.define(param);
        }
        try self.resolve(stmt.function.body);
        try self.endScope();
    }

    pub fn visitBlockStmt(self: *Resolver, stmt: *Stmt) !void {
        try self.beginScope();
        try self.resolve(stmt.block.statements);
        try self.endScope();
    }

    pub fn visitVariableStmt(self: *Resolver, stmt: *Stmt) !void {
        try self.declare(stmt.variable.name);

        if (stmt.variable.initializer != null) {
            try self.resolveExpr(stmt.variable.initializer.?);
        }
        try self.define(stmt.variable.name);
    }

    pub fn visitVariableExpr(self: *Resolver, expr: Expr) anyerror!void {
        if (self.scopes.items.len != 0) {
            const peek = self.scopes.getLast().get(expr.variable.name.lexer);
            if (peek != null and peek.? != null and peek.?.?.boolean == false) {
                std.log.err("Can't read local variable in its own initializer.\n", .{});
            }
        }
        try self.resolveLocal(expr, expr.variable.name);
    }

    pub fn visitAssignExpr(self: *Resolver, expr: Expr) anyerror!void {
        try self.resolveExpr(expr.assign.value);
        try self.resolveLocal(expr, expr.assign.name);
    }

    pub fn visitFunctionStmt(self: *Resolver, stmt: *Stmt) !void {
        try self.declare(stmt.function.name);
        try self.define(stmt.function.name);

        try self.resolveFunction(stmt, FunctionType.FUNCTION);
    }

    pub fn visitExpressionStmt(self: *Resolver, stmt: *Stmt) !void {
        try self.resolveExpr(stmt.expression.expression);
    }

    pub fn visitIfStmt(self: *Resolver, stmt: *Stmt) !void {
        try self.resolveExpr(stmt.if_statement.condition);
        try self.resolveStmt(stmt.if_statement.then_branch);

        if (stmt.if_statement.else_branch != null) try self.resolveStmt(stmt.if_statement.else_branch.?);
    }

    pub fn visitPrintStmt(self: *Resolver, stmt: *Stmt) !void {
        try self.resolveExpr(stmt.print.expression);
    }

    pub fn visitReturnStmt(self: *Resolver, stmt: *Stmt) !void {
        if (self.current_function == FunctionType.NONE) {
            std.log.err("Can't return from top-level code.\n", .{});
            return error.NoReturnTopLevel;
        }
        if (stmt.return_statement.value != null) {
            try self.resolveExpr(stmt.return_statement.value.?);
        }
    }

    pub fn visitWhileStmt(self: *Resolver, stmt: *Stmt) !void {
        try self.resolveExpr(stmt.while_statement.condition);
        try self.resolveStmt(stmt.while_statement.body);
    }

    pub fn visitBinaryExpr(self: *Resolver, expr: Expr) anyerror!void {
        try self.resolveExpr(expr.binary.left);
        try self.resolveExpr(expr.binary.right);
    }

    pub fn visitCallExpr(self: *Resolver, expr: Expr) anyerror!void {
        try self.resolveExpr(expr.call.callee);

        for (expr.call.arguments) |arg| {
            try self.resolveExpr(arg);
        }
    }

    pub fn visitLiteralExpr(self: *Resolver, expr: Expr) anyerror!void {
        _ = self;
        _ = expr;
    }

    pub fn visitLogicalExpr(self: *Resolver, expr: Expr) !void {
        try self.resolveExpr(expr.logical.left);
        try self.resolveExpr(expr.logical.right);
    }

    pub fn visitUnaryExpr(self: *Resolver, expr: Expr) !void {
        try self.resolve(expr.unary.right);
    }

    pub fn visitClassStmt(self: *Resolver, stmt: *Stmt) !void {
        _ = self;
        _ = stmt;
    }

    pub fn visitForStmt(self: *Resolver, stmt: *Stmt) !void {
        _ = self;
        _ = stmt;
    }

    pub fn visitUnary(self: *Resolver, expr: Expr) !void {
        _ = self;
        _ = expr;
    }

    pub fn visitGroupingExpr(self: *Resolver, expr: Expr) !void {
        _ = self;
        _ = expr;
    }

    pub fn visitFunctionExpr(self: *Resolver, expr: Expr) !void {
        _ = self;
        _ = expr;
    }
};
