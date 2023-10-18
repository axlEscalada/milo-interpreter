const std = @import("std");
const Allocator = std.mem.Allocator;
const Expr = @import("expression.zig").Expr;
const Object = @import("expression.zig").Object;

const Interpreter = struct {
    allocator: Allocator,

    fn visitLiteralExpr(self: *Interpreter, expr: *Expr) *Object {
        _ = self;
        return expr.value.?;
    }

    fn visitUnaryExpr(self: *Interpreter, expr: *Expr) *Object {
        var right = self.evaluate(expr.right);

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

const AstPrinter = struct {
    allocator: Allocator,

    fn print(this: *AstPrinter, expr: *Expr) []const u8 {
        return expr.accept(this, []const u8);
    }

    fn visitBinary(this: *AstPrinter, expr: *Expr) []const u8 {
        var expressions = [_]*Expr{ expr.left.?, expr.right.? };
        return this.parenthesize(expr.operator.?.lexer, &expressions);
    }

    fn visitLiteral(this: *AstPrinter, expr: *Expr) []const u8 {
        _ = this;
        if (expr.valueString) |v| {
            return v;
        } else return "nil";
    }

    fn visitUnary(this: *AstPrinter, expr: *Expr) []const u8 {
        var expressions = [_]*Expr{expr.right.?};
        return this.parenthesize(expr.operator.?.lexer, &expressions);
    }

    fn visitGrouping(this: *AstPrinter, expr: *Expr) []const u8 {
        var expressions = [_]*Expr{expr.expression.?};
        return this.parenthesize("group", &expressions);
    }

    fn parenthesize(this: *AstPrinter, name: []const u8, expres: []*Expr) []const u8 {
        // std.debug.print("{} AND POINTEr {}", .{ this.allocator, &this.allocator });
        var builder: []const u8 = std.fmt.allocPrint(this.allocator, "({s}", .{name}) catch |e| {
            std.debug.print("Error {}", .{e});
            std.os.exit(64);
        };
        for (expres) |expr| {
            builder = std.fmt.allocPrint(this.allocator, "{s} {s}", .{ builder, expr.accept(this, []const u8) }) catch |e| {
                std.debug.print("Error {}", .{e});
                std.os.exit(64);
            };
        }
        builder = std.fmt.allocPrint(this.allocator, "{s})", .{builder}) catch |e| {
            std.debug.print("Error {}", .{e});
            std.os.exit(64);
        };
        return builder;
    }
};
