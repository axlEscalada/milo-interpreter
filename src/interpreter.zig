const std = @import("std");
const Allocator = std.mem.Allocator;
const Expr = @import("expression.zig").Expr;
const Object = @import("expression.zig").Object;
const Stmt = @import("statement.zig").Stmt;

pub const Interpreter = struct {
    allocator: Allocator,

    // pub fn interpret(self: *Interpreter, expr: *Expr) !void {
    //     const value = try self.evaluate(expr);
    //     const str = try self.stringify(value);
    //     std.debug.print("VALUE {s}\n", .{str});
    // }

    pub fn interpret(self: *Interpreter, statements: []*Stmt) void {
        for (statements) |st| {
            // self.execute(st);
            st.accept(void, self);
        }
    }

    fn stringify(self: *Interpreter, object: Object) ![]const u8 {
        if (object == Object.float) {
            var text = try std.fmt.allocPrint(self.allocator, "{d}", .{object.float.*});
            if (std.mem.endsWith(u8, text, ".0")) {
                text = text[0 .. text.len - 2];
            }
            return text;
        } else if (object == Object.string) {
            return object.string.*;
        } else if (object == Object.boolean) {
            if (object.boolean.*) {
                return "true";
            } else {
                return "false";
            }
        }

        return "nonimopl";
    }

    pub fn visitLiteral(self: *Interpreter, expr: *Expr) !Object {
        _ = self;
        return expr.value.?;
    }

    pub fn visitGrouping(self: *Interpreter, expr: *Expr) !Object {
        return self.evaluate(expr.expression.?);
    }

    pub fn visitUnaryExpr(self: *Interpreter, expr: *Expr) !Object {
        const right = self.evaluate(expr.right.?);

        return switch (expr.operator.?.tokenType) {
            .BANG => !self.isTruthy(right),
            .MINUS => return -right.double,
            else => unreachable,
        };
    }

    pub fn visitBinary(self: *Interpreter, expr: *Expr) anyerror!Object {
        const left = try self.evaluate(expr.left.?);
        const right = try self.evaluate(expr.right.?);

        return switch (expr.operator.?.tokenType) {
            .MINUS => try Object.initFloat(self.allocator, left.float.* - right.float.*),
            .SLASH => try Object.initFloat(self.allocator, left.float.* / right.float.*),
            .STAR => try Object.initFloat(self.allocator, left.float.* * right.float.*),
            .PLUS => if (left == Object.float and right == Object.float) {
                return try Object.initFloat(self.allocator, left.float.* + right.float.*);
            } else if (left == Object.string and right == Object.string) {
                const concat_str = try std.fmt.allocPrint(self.allocator, "{s}{s}", .{ left.string.*[1 .. left.string.*.len - 1], right.string.*[1 .. right.string.*.len - 1] });
                return try Object.initString(self.allocator, concat_str);
            } else @panic("Unsopported types for addition"),
            .GREATER => try Object.initBool(self.allocator, left.float.* > right.float.*),
            .GREATER_EQUAL => try Object.initBool(self.allocator, left.float.* >= right.float.*),
            .LESS => try Object.initBool(self.allocator, left.float.* < right.float.*),
            .LESS_EQUAL => try Object.initBool(self.allocator, left.float.* <= right.float.*),
            .EQUAL_EQUAL => try Object.initBool(self.allocator, self.isEqual(left, right)),
            else => Object.initString(self.allocator, "NOT EVALUATED"),
        };
    }

    pub fn visitExpressions(self: *Interpreter, stmt: Stmt) !void {
        self.evaluate(stmt.expression);
    }

    pub fn visitPrint(self: *Interpreter, stmt: Stmt) !void {
        const value = self.evaluate(stmt.expression);
        std.io.getStdOut().write(self.stringify(value));
    }

    fn execute(self: *Interpreter, stmt: *Stmt) void {
        stmt.accept(void, self);
    }

    pub fn visitUnary(self: *Interpreter, expr: *Expr) !Object {
        const right = try self.evaluate(expr.right.?);
        return switch (expr.operator.?.tokenType) {
            .MINUS => right,
            else => @panic("PANIC UNARY"),
        };
    }

    fn evaluate(self: *Interpreter, expr: *Expr) !Object {
        return try expr.accept(self, Object);
    }

    fn isEqual(self: *Interpreter, a: Object, b: Object) bool {
        _ = self;
        // if (a == null and b == null) return true;
        // if (a == null) return false;

        if (a == Object.float and b == Object.float) {
            return a.float.* == b.float.*;
        } else if (a == Object.string and b == Object.string) {
            return std.mem.eql(u8, a.string.*, b.string.*);
        } else if (a == Object.boolean and b == Object.boolean) {
            return a.boolean.* == b.boolean.*;
        }
        return false;
    }

    fn isTruthy(self: *Interpreter, object: *Object) bool {
        _ = self;
        if (object == null) return false;
        return switch (object) {
            .boolean => object.bool.*,
            else => true,
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
