const std = @import("std");
const Allocator = std.mem.Allocator;
const token = @import("token.zig");

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

pub const Expr = union(ExprType) {
    binary: Binary,
    unary: Unary,
    literal: Literal,
    grouping: Grouping,
    variable: Variable,
    assign: Assign,

    pub const Self = @This();

    pub fn init(allocator: Allocator, expr_type: anytype) !*Expr {
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
        };
    }
};

pub const ExprType = enum { binary, unary, literal, grouping, variable, assign };
pub const ObjectType = enum { string, float, boolean };
pub const Object = union(ObjectType) {
    string: []const u8,
    float: f64,
    boolean: bool,

    pub fn init(allocator: Allocator) !*Object {
        return allocator.create(Object) catch |e| {
            std.log.err("Error {!}", .{e});
            return error.InitializingObject;
        };
    }

    fn Type(comptime field: anytype) type {
        return @typeInfo(std.meta.fieldInfo(Object, field).type).Pointer.child;
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

    pub fn initString(allocator: Allocator, string: []const u8) !*Object {
        const obj = try allocator.create(Object);
        obj.* = .{ .string = string };
        return obj;
    }
};
