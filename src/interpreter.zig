const std = @import("std");
const Allocator = std.mem.Allocator;
const Expr = @import("expression.zig").Expr;
const Object = @import("expression.zig").Object;
const Stmt = @import("statement.zig").Stmt;
const Token = @import("token.zig").Token;

pub const Interpreter = struct {
    allocator: Allocator,
    environment: Environment,

    // pub fn interpret(self: *Interpreter, expr: *Expr) !void {
    //     const value = try self.evaluate(expr);
    //     const str = try self.stringify(value);
    //     std.debug.print("VALUE {s}\n", .{str});
    // }

    pub fn interpret(self: *Interpreter, statements: []*Stmt) !void {
        for (statements) |st| {
            // self.execute(st);
            try st.accept(anyerror!void, self);
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

    pub fn visitLiteral(self: *Interpreter, expr: Expr) !Object {
        _ = self;
        return expr.literal.value;
    }

    pub fn visitGrouping(self: *Interpreter, expr: Expr) !Object {
        return self.evaluate(expr.grouping.expression);
    }

    pub fn visitUnaryExpr(self: *Interpreter, expr: Expr) !Object {
        const right = self.evaluate(expr.right.?);

        return switch (expr.operator.?.tokenType) {
            .BANG => !self.isTruthy(right),
            .MINUS => return -right.double,
            else => unreachable,
        };
    }

    pub fn visitBinary(self: *Interpreter, expr: Expr) anyerror!Object {
        const left = try self.evaluate(expr.binary.left);
        const right = try self.evaluate(expr.binary.right);

        return switch (expr.binary.operator.tokenType) {
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

    pub fn visitExpression(self: *Interpreter, stmt: *Stmt) !void {
        _ = try self.evaluate(stmt.expression.expression);
    }

    pub fn visitPrint(self: *Interpreter, stmt: *Stmt) !void {
        const value = try self.evaluate(stmt.print.expression);
        const str = try self.stringify(value);
        const file = std.io.getStdOut();
        _ = try file.write(str);
        _ = try file.write("\n");
    }

    pub fn visitBlock(self: *Interpreter, stmt: *Stmt) !void {
        _ = self;
        _ = stmt;
    }

    pub fn visitClass(self: *Interpreter, stmt: *Stmt) !void {
        _ = self;
        _ = stmt;
    }

    pub fn visitFunction(self: *Interpreter, stmt: *Stmt) !void {
        _ = self;
        _ = stmt;
    }

    pub fn visitIf(self: *Interpreter, stmt: *Stmt) !void {
        _ = self;
        _ = stmt;
    }

    pub fn visitReturn(self: *Interpreter, stmt: *Stmt) !void {
        _ = self;
        _ = stmt;
    }

    pub fn visitVariable(self: *Interpreter, stmt: *Stmt) !void {
        var value: ?Object = null;
        if (stmt.variable.initializer) |it| {
            value = try self.evaluate(it);
        }
        try self.environment.define(stmt.variable.name.lexer, value);
    }

    pub fn visitWhile(self: *Interpreter, stmt: *Stmt) !void {
        _ = self;
        _ = stmt;
    }

    fn execute(self: *Interpreter, stmt: *Stmt) void {
        stmt.accept(void, self);
    }

    pub fn visitUnary(self: *Interpreter, expr: Expr) !Object {
        const right = try self.evaluate(expr.unary.right);
        return switch (expr.unary.operator.tokenType) {
            .MINUS => right,
            else => @panic("PANIC UNARY"),
        };
    }

    fn evaluate(self: *Interpreter, expr: Expr) !Object {
        return expr.accept(self, Object);
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

pub const Environment = struct {
    values: std.StringHashMap(?Object),

    pub fn init(allocator: Allocator) Environment {
        return .{
            .values = std.StringHashMap(?Object).init(allocator),
        };
    }

    pub fn define(self: Environment, name: []const u8, value: ?Object) !void {
        try self.values.put(name, value);
    }

    pub fn get(self: Environment, name: Token) !Object {
        if (self.values.contains(name.lexer)) {
            return self.values.get(name.lexer);
        }
        std.log.err("Use of undefined variable {s}\n", .{name.lexer});
        return error.UndefinedVariable;
    }
};
