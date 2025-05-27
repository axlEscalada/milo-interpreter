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
            .binary => visitor.visitBinaryExpr(this),
            .unary => visitor.visitUnary(this),
            .literal => visitor.visitLiteralExpr(this),
            .grouping => visitor.visitGroupingExpr(this),
            .variable => visitor.visitVariableExpr(this),
            .assign => visitor.visitAssignExpr(this),
            .logical => visitor.visitLogicalExpr(this),
            .call => visitor.visitCallExpr(this),
            .function => visitor.visitFunctionExpr(this),
        };
    }
};

pub const ExprContext = struct {
    pub fn hash(ctx: ExprContext, key: Expr) u32 {
        _ = ctx;
        var h = std.hash.Fnv1a_32.init();

        h.update(std.mem.asBytes(&@intFromEnum(key)));

        switch (key) {
            .binary => |b| {
                h.update(std.mem.asBytes(&@intFromPtr(b.left)));
                h.update(std.mem.asBytes(&@intFromPtr(b.right)));
                h.update(b.operator.lexer);
            },
            .unary => |u| {
                h.update(std.mem.asBytes(&@intFromPtr(u.right)));
                h.update(u.operator.lexer);
            },
            .literal => |l| {
                h.update(l.value_string);
                h.update(std.mem.asBytes(&@intFromPtr(l.value)));
            },
            .grouping => |g| {
                h.update(std.mem.asBytes(&@intFromPtr(g.expression)));
            },
            .variable => |v| {
                h.update(v.name.lexer);
            },
            .assign => |a| {
                h.update(a.name.lexer);
                h.update(std.mem.asBytes(&@intFromPtr(a.value)));
            },
            .logical => |l| {
                h.update(std.mem.asBytes(&@intFromPtr(l.left)));
                h.update(std.mem.asBytes(&@intFromPtr(l.right)));
                h.update(l.operator.lexer);
            },
            .call => |c| {
                h.update(std.mem.asBytes(&@intFromPtr(c.callee)));
                h.update(c.paren.lexer);
                for (c.arguments) |arg| {
                    h.update(std.mem.asBytes(&@intFromPtr(arg)));
                }
            },
            .function => |f| {
                h.update(f.name.lexer);
                for (f.params) |param| {
                    h.update(param.lexer);
                }
                for (f.body) |stmt| {
                    h.update(std.mem.asBytes(&@intFromPtr(stmt)));
                }
            },
        }

        return h.final();
    }

    pub fn eql(ctx: ExprContext, a: Expr, b: Expr) bool {
        _ = ctx;

        if (@intFromEnum(a) != @intFromEnum(b)) return false;

        return switch (a) {
            .binary => |a_bin| std.mem.eql(u8, a_bin.operator.lexer, b.binary.operator.lexer) and
                a_bin.left == b.binary.left and a_bin.right == b.binary.right,
            .unary => |a_un| std.mem.eql(u8, a_un.operator.lexer, b.unary.operator.lexer) and
                a_un.right == b.unary.right,
            .literal => |a_lit| std.mem.eql(u8, a_lit.value_string, b.literal.value_string),
            .grouping => |a_grp| a_grp.expression == b.grouping.expression,
            .variable => |a_var| std.mem.eql(u8, a_var.name.lexer, b.variable.name.lexer),
            .assign => |a_asgn| std.mem.eql(u8, a_asgn.name.lexer, b.assign.name.lexer) and
                a_asgn.value == b.assign.value,
            .logical => |a_log| std.mem.eql(u8, a_log.operator.lexer, b.logical.operator.lexer) and
                a_log.left == b.logical.left and a_log.right == b.logical.right,
            .call => |a_call| a_call.callee == b.call.callee and
                std.mem.eql(u8, a_call.paren.lexer, b.call.paren.lexer) and
                a_call.arguments.len == b.call.arguments.len and
                std.mem.eql(*Expr, a_call.arguments, b.call.arguments),
            .function => |a_fn| std.mem.eql(u8, a_fn.name.lexer, b.function.name.lexer) and
                a_fn.params.len == b.function.params.len and
                a_fn.body.len == b.function.body.len,
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
