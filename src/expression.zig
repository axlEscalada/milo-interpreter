const std = @import("std");
const Allocator = std.mem.Allocator;
const token = @import("token.zig");
const statement = @import("statement.zig");
const Callable = @import("interpreter.zig").Callable;

pub const Binary = struct {
    operator: token.Token,
    left: *Expr,
    right: *Expr,
};

pub const Literal = struct {
    value_string: []const u8,
    value: *Object,
};

pub const Grouping = struct {
    expression: *Expr,
};

pub const Unary = struct {
    operator: token.Token,
    right: *Expr,
};

pub const Variable = struct {
    name: token.Token,
};

pub const Assign = struct {
    name: token.Token,
    value: *Expr,
};

pub const Logical = struct {
    left: *Expr,
    operator: token.Token,
    right: *Expr,
};

pub const Call = struct {
    callee: *Expr,
    paren: token.Token,
    arguments: []*Expr,
};

pub const Function = struct {
    name: token.Token,
    params: []token.Token,
    body: []*statement.Stmt,
};

pub const Expr = union(ExprType) {
    binary: Binary,
    unary: Unary,
    literal: Literal,
    grouping: Grouping,
    variable: Variable,
    assign: Assign,
    logical: Logical,
    call: Call,
    function: Function,

    pub const Self = @This();

    pub fn init(allocator: Allocator, expr_type: Expr) !*Expr {
        const expr = allocator.create(Expr) catch |e| {
            std.log.err("Error {!}", .{e});
            return error.InitializingExpression;
        };
        expr.* = expr_type;

        return expr;
    }

    pub fn accept(this: Expr, visitor: anytype, comptime T: type) !T {
        return switch (this) {
            .binary => visitor.visitBinary(this),
            .unary => visitor.visitUnary(this),
            .literal => visitor.visitLiteral(this),
            .grouping => visitor.visitGrouping(this),
            .variable => visitor.visitVariableExpr(this),
            .assign => visitor.visitAssignExpr(this),
            .logical => visitor.visitLogicalExpr(this),
            .call => visitor.visitCallExpr(this),
            .function => visitor.visitFunctionExpr(this),
        };
    }
};

pub const ExprType = enum { binary, unary, literal, grouping, variable, assign, logical, call, function };
pub const ObjectType = enum { string, float, boolean, nil, callable };
pub const Object = union(ObjectType) {
    string: []const u8,
    float: f64,
    boolean: bool,
    nil: ?u1,
    callable: Callable,

    pub fn init(allocator: Allocator) !*Object {
        return allocator.create(Object) catch |e| {
            std.log.err("Error {!}", .{e});
            return error.InitializingObject;
        };
    }

    fn Type(comptime field: anytype) type {
        return @typeInfo(std.meta.fieldInfo(Object, field).type).Pointer.child;
    }

    pub fn initCallable(allocator: Allocator, callable: Callable) !*Object {
        const obj = try allocator.create(Object);
        obj.* = .{ .callable = callable };
        return obj;
    }

    pub fn initBool(allocator: Allocator, boolean: bool) !*Object {
        const obj = try allocator.create(Object);
        obj.* = .{ .boolean = boolean };
        return obj;
    }

    pub fn initFloat(allocator: Allocator, float: f64) !*Object {
        const obj = try allocator.create(Object);
        obj.* = .{ .float = float };
        return obj;
    }

    pub fn initString(allocator: Allocator, value: []const u8) !*Object {
        const obj = try allocator.create(Object);
        const str_copy = try allocator.dupe(u8, value);
        obj.* = .{ .string = str_copy };
        return obj;
    }

    pub fn initNil(allocator: Allocator) !*Object {
        const obj = try allocator.create(Object);
        obj.* = .{ .nil = null };
        return obj;
    }
};
