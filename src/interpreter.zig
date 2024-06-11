const std = @import("std");
const Allocator = std.mem.Allocator;
const Expr = @import("expression.zig").Expr;
const Object = @import("expression.zig").Object;

pub const Interpreter = struct {
    allocator: Allocator,

    pub fn interpret(self: *Interpreter, expr: *Expr) !void {
        const value = try self.evaluate(expr);
        const str = try self.stringify(value);
        std.debug.print("VALUE {s}\n", .{str});
    }

    fn stringify(self: *Interpreter, object: *Object) ![]const u8 {
        // if (object == null) return "nil";

        if (object.* == Object.float) {
            const text = try std.fmt.allocPrint(self.allocator, "{d}", .{object.float()});
            if (std.mem.endsWith(u8, text, ".0")) {
                text = text[0 .. text.len - 2];
            }
            return text;
        }

        return "nonimopl";
    }

    pub fn visitLiteral(self: *Interpreter, expr: *Expr) !*Object {
        _ = self;
        return expr.value.?;
    }

    pub fn visitUnaryExpr(self: *Interpreter, expr: *Expr) !?*Object {
        const right = self.evaluate(expr.right.?);

        return switch (expr.operator.?.tokenType) {
            .BANG => !self.isTruthy(right),
            .MINUS => return -right.double,
            else => unreachable,
        };
    }

    pub fn visitBinary(self: *Interpreter, expr: *Expr) !*Object {
        const left = self.evaluate(expr.left.?);
        const right = self.evaluate(expr.right.?);

        const value = switch (expr.operator.?.tokenType) {
            .MINUS => left.float - right.float,
            .SLASH => left.float / right.float,
            .STAR => if (left == .float and right == .float) {
                left.float * right.float;
            } else if (left == .string and right == .string) {
                //TODO: fmt alloc two strings
            },
            .PLUS => left.float + right.float,
            else => 0,
        };
        return Object.initFloat(self.allocator, value);
    }

    pub fn visitUnary(self: *Interpreter, expr: *Expr) !?*Object {
        const right = try self.evaluate(expr.right);
        return switch (expr.operator.tokenType) {
            .MINUS => right,
            else => null,
        };
    }

    fn evaluate(self: *Interpreter, expr: *Expr) !?*Object {
        return try expr.accept(self, ?*Object);
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

    fn parenthesize(this: *AstPrinter, name: []const u8, expres: []*Expr) anyerror![]const u8 {
        var builder: []const u8 = std.fmt.allocPrint(this.allocator, "({s}", .{name}) catch |e| {
            std.log.err("Error while allocating parenthesis {!}\n", .{e});
            return error.AllocationError;
        };
        for (expres) |expr| {
            const str = expr.accept(this, []const u8) catch |e| {
                std.log.err("Error {any}\n", .{e});
                return error.HandlingExpression;
            };
            builder = std.fmt.allocPrint(this.allocator, "{s} {s}", .{ builder, str }) catch |e| {
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
