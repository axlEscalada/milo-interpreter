const std = @import("std");
const Allocator = std.mem.Allocator;
const token = @import("token.zig");

pub const Expr = struct {
    operator: ?token.Token = null,
    expression: ?*Expr = null,
    left: ?*Expr = null,
    right: ?*Expr = null,
    valueString: ?[]const u8 = null,
    value: ?*Object = null,
    tag: ExprType,

    pub const Self = @This();

    pub fn initBinary(allocator: Allocator, op: token.Token, left: *Expr, right: *Expr) *Expr {
        var expr = allocator.create(Expr) catch |e| {
            std.debug.print("Error {}", .{e});
            std.os.exit(64);
        };

        expr.tag = ExprType.binary;
        expr.operator = op;
        expr.left = left;
        expr.right = right;
        return expr;
    }

    pub fn initLiteral(allocator: Allocator, valueString: []const u8, value: *Object) *Expr {
        var expr = allocator.create(Expr) catch |e| {
            std.debug.print("Error {}", .{e});
            std.os.exit(64);
        };
        expr.tag = ExprType.literal;
        expr.valueString = valueString;
        expr.value = value;
        return expr;
    }

    pub fn initGrouping(allocator: Allocator, group: *Expr) *@This() {
        var expr = allocator.create(Expr) catch |e| {
            std.debug.print("Error {}", .{e});
            std.os.exit(64);
        };
        expr.tag = ExprType.grouping;
        expr.expression = group;
        return expr;
    }

    pub fn initUnary(allocator: Allocator, right: *Expr, op: token.Token) *@This() {
        var expr = allocator.create(Expr) catch |e| {
            std.debug.print("Error {}", .{e});
            std.os.exit(64);
        };
        expr.tag = ExprType.unary;
        expr.operator = op;
        expr.right = right;
        return expr;
    }

    pub fn accept(this: *Expr, visitor: anytype, comptime T: type) T {
        return switch (this.tag) {
            .binary => visitor.visitBinary(this),
            .unary => visitor.visitUnary(this),
            .literal => visitor.visitLiteral(this),
            .grouping => visitor.visitGrouping(this),
            else => @panic("error accept token"),
        };
    }
};

pub const ExprType = enum { binary, unary, literal, grouping, err };
pub const ObjectType = enum { string, float, boolean };
pub const Object = union(ObjectType) {
    string: []const u8,
    float: f64,
    boolean: bool,

    fn init(allocator: Allocator) *Object {
        return allocator.create(Object) catch |e| {
            std.debug.print("Error {}", .{e});
            std.os.exit(64);
        };
    }

    fn initBool(allocator: Allocator, boolean: bool) *Object {
        var obj = Object.init(allocator);
        obj.boolean = boolean;
        return obj;
    }

    fn initFloat(allocator: Allocator, float: f64) *Object {
        var obj = Object.init(allocator);
        obj.float = float;
        return obj;
    }

    fn initString(allocator: Allocator, string: []const u8) *Object {
        var obj = Object.init(allocator);
        obj.string = string;
        return obj;
    }
};
