const std = @import("std");
const Allocator = std.mem.Allocator;
const token = @import("token.zig");

pub const Binary = struct {
    operator: *token.Token,
    left: Expr,
    right: Expr,
};

pub const Literal = struct {
    value_string: []const u8,
    value: Object,
};

pub const Grouping = struct {
    expression: Expr,
};

pub const Unary = struct {
    operator: *token.Token,
    right: Expr,
};

pub const Variable = struct {
    name: *token.Token,
};

pub const Expr = union(ExprType) {
    binary: *Binary,
    unary: *Unary,
    literal: *Literal,
    grouping: *Grouping,
    variable: *Variable,

    pub const Self = @This();

    pub fn initBinary(allocator: Allocator, op: *token.Token, left: Expr, right: Expr) !Expr {
        const binary = allocator.create(Binary) catch |e| {
            std.log.err("Error {!}", .{e});
            return error.InitializingExpression;
        };

        binary.* = .{
            .operator = op,
            .left = left,
            .right = right,
        };
        return Expr{ .binary = binary };
    }

    pub fn initLiteral(allocator: Allocator, value_string: []const u8, value: Object) !Expr {
        const literal = allocator.create(Literal) catch |e| {
            std.log.err("Error {!}", .{e});
            return error.InitializingLiteral;
        };
        literal.* = .{ .value_string = value_string, .value = value };
        return Expr{ .literal = literal };
    }

    pub fn initGrouping(allocator: Allocator, group: Expr) !Expr {
        const grouping = allocator.create(Grouping) catch |e| {
            std.log.err("Error {!}", .{e});
            return error.InitializingGrouping;
        };
        grouping.* = .{ .expression = group };
        return Expr{ .grouping = grouping };
    }

    pub fn initUnary(allocator: Allocator, right: Expr, op: *token.Token) !Expr {
        const unary = allocator.create(Unary) catch |e| {
            std.log.err("Error {!}", .{e});
            return error.InitializingUnary;
        };
        unary.* = .{ .operator = op, .right = right };
        return Expr{ .unary = unary };
    }

    pub fn initVariable(allocator: Allocator, name: *token.Token) !Expr {
        const variable = allocator.create(Variable) catch |e| {
            std.log.err("Error {!}", .{e});
            return error.InitializingUnary;
        };
        variable.* = .{ .name = name };
        return Expr{ .variable = variable };
    }

    pub fn accept(this: Expr, visitor: anytype, comptime T: type) !T {
        return switch (this) {
            .binary => visitor.visitBinary(this),
            .unary => visitor.visitUnary(this),
            .literal => visitor.visitLiteral(this),
            .grouping => visitor.visitGrouping(this),
            .variable => visitor.visitVariableExpr(this),
            // else => @panic("error accept token"),
        };
    }
};

pub const ExprType = enum { binary, unary, literal, grouping, variable };
pub const ObjectType = enum { string, float, boolean };
pub const Object = union(ObjectType) {
    string: *[]const u8,
    float: *f64,
    boolean: *bool,

    pub fn init(allocator: Allocator) !*Object {
        return allocator.create(Object) catch |e| {
            std.log.err("Error {!}", .{e});
            return error.InitializingObject;
        };
    }

    fn Type(comptime field: anytype) type {
        return @typeInfo(std.meta.fieldInfo(Object, field).type).Pointer.child;
    }

    pub fn initBool(allocator: Allocator, boolean: bool) !Object {
        const obj = try allocator.create(bool);
        obj.* = boolean;
        // var obj = try Object.init(allocator);
        // obj.string = string;
        return Object{ .boolean = obj };
    }

    pub fn initFloat(allocator: Allocator, float: f64) !Object {
        const obj = try allocator.create(f64);
        obj.* = float;
        // var obj = try Object.init(allocator);
        // obj.string = string;
        return Object{ .float = obj };
    }

    pub fn initString(allocator: Allocator, string: []const u8) !Object {
        const obj = try allocator.create([]const u8);
        obj.* = string;
        return Object{ .string = obj };
    }
};
