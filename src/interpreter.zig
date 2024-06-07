const std = @import("std");
const Allocator = std.mem.Allocator;
const Expr = @import("expression.zig").Expr;
const Object = @import("expression.zig").Object;

pub const Interpreter = struct {
    allocator: Allocator,

    pub fn visitLiteralExpr(self: *Interpreter, expr: *Expr) *Object {
        _ = self;
        return expr.value.?;
    }

    pub fn visitUnaryExpr(self: *Interpreter, expr: *Expr) *Object {
        const right = self.evaluate(expr.right);

        return switch (expr.operator.?.tokenType) {
            .BANG => !self.isTruthy(right),
            .MINUS => return -right.double,
            else => unreachable,
        };
    }

    fn evaluate(self: *Interpreter, expr: *Expr) *Object {
        return expr.accept(self, Object);
    }

    fn isTruthy(self: *Interpreter, object: *Object) *Object {
        if (object == null) return Object.initBool(self.allocator, false);
        return switch (object) {
            .boolean => return object,
            else => return true,
        };
    }
};

pub const AstPrinter = struct {
    allocator: Allocator,

    pub fn print(this: *AstPrinter, expr: *Expr) ![]const u8 {
        return expr.accept(this, []const u8);
    }

    pub fn visitBinary(this: *AstPrinter, expr: *Expr) ![]const u8 {
        var expressions = [_]*Expr{ expr.left.?, expr.right.? };
        return try this.parenthesize(expr.operator.?.lexer, &expressions);
    }

    pub fn visitLiteral(this: *AstPrinter, expr: *Expr) []const u8 {
        _ = this;
        if (expr.valueString) |v| {
            return v;
        } else return "nil";
    }

    pub fn visitUnary(this: *AstPrinter, expr: *Expr) ![]const u8 {
        var expressions = [_]*Expr{expr.right.?};
        return try this.parenthesize(expr.operator.?.lexer, &expressions);
    }

    pub fn visitGrouping(this: *AstPrinter, expr: *Expr) ![]const u8 {
        var expressions = [_]*Expr{expr.expression.?};
        return try this.parenthesize("group", &expressions);
    }

    fn parenthesize(this: *AstPrinter, name: []const u8, expres: []*Expr) ![]const u8 {
        var builder: []const u8 = std.fmt.allocPrint(this.allocator, "({s}", .{name}) catch |e| {
            std.log.err("Error while allocating parenthesis {!}\n", .{e});
            return error.AllocationError;
        };
        for (expres) |expr| {
            builder = std.fmt.allocPrint(this.allocator, "{s} {any}", .{ builder, expr.accept(this, []const u8) }) catch |e| {
                std.log.err("Error while parenthesize {!}\n", .{e});
                return error.AllocationError;
            };
        }
        builder = std.fmt.allocPrint(this.allocator, "{s})", .{builder}) catch |e| {
            std.log.err("Error while parenthesize {!}\n", .{e});
            return error.AllocationError;
        };
        return builder;
    }
};
