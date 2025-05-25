const std = @import("std");
const Allocator = std.mem.Allocator;
const Expr = @import("expression.zig").Expr;
const Object = @import("expression.zig").Object;
const Stmt = @import("statement.zig").Stmt;
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const Environment = @import("environment.zig").Environment;

pub const Interpreter = struct {
    allocator: Allocator,
    environment: *Environment,

    pub fn init(allocator: Allocator) *Interpreter {
        const interpreter = allocator.create(Interpreter) catch |e| {
            std.log.err("Error while creating interpreter {any}\n", .{e});
            @panic("error while creating");
        };

        const global_env = Environment.init(allocator, null) catch |e| {
            std.log.err("Error while creating global environment {any}\n", .{e});
            @panic("error while creating");
        };

        interpreter.* = .{ .allocator = allocator, .environment = global_env };
        return interpreter;
    }

    pub fn deinit(self: *Interpreter) void {
        self.environment.deinit();
        self.allocator.destroy(self);
    }

    pub fn interpret(self: *Interpreter, statements: []*Stmt) !void {
        for (statements) |st| {
            try st.accept(anyerror!void, self);
        }
    }

    fn stringify(self: *Interpreter, object: ?*Object) ![]const u8 {
        if (object) |ob| {
            return switch (ob.*) {
                Object.float => {
                    var text = try std.fmt.allocPrint(self.allocator, "{d}", .{ob.*.float});
                    if (std.mem.endsWith(u8, text, ".0")) {
                        text = text[0 .. text.len - 2];
                    }
                    return text;
                },
                Object.string => {
                    return ob.*.string;
                },
                Object.boolean => {
                    if (ob.*.boolean) {
                        return "true";
                    } else {
                        return "false";
                    }
                },
            };
        }

        return "nonimopl";
    }

    pub fn visitLiteral(self: *Interpreter, expr: Expr) !*Object {
        _ = self;
        return expr.literal.value;
    }

    pub fn visitGrouping(self: *Interpreter, expr: Expr) !*Object {
        return self.evaluate(expr.grouping.expression);
    }

    pub fn visitUnaryExpr(self: *Interpreter, expr: Expr) !*Object {
        const right = self.evaluate(expr.right.?);

        return switch (expr.operator.?.tokenType) {
            .BANG => !self.isTruthy(right),
            .MINUS => return -right.double,
            else => unreachable,
        };
    }

    pub fn visitBinary(self: *Interpreter, expr: Expr) anyerror!*Object {
        const left = try self.evaluate(expr.binary.left);
        const right = try self.evaluate(expr.binary.right);

        return switch (expr.binary.operator.tokenType) {
            .MINUS => try Object.initFloat(self.allocator, left.float - right.float),
            .SLASH => try Object.initFloat(self.allocator, left.float / right.float),
            .STAR => try Object.initFloat(self.allocator, left.float * right.float),
            .PLUS => if (left.* == Object.float and right.* == Object.float) {
                return try Object.initFloat(self.allocator, left.float + right.float);
            } else if (left.* == Object.string and right.* == Object.string) {
                const concat_str = try std.fmt.allocPrint(self.allocator, "{s}{s}", .{ left.string, right.string });
                return try Object.initString(self.allocator, concat_str);
            } else @panic("Unsopported types for addition"),
            .GREATER => try Object.initBool(self.allocator, left.float > right.float),
            .GREATER_EQUAL => try Object.initBool(self.allocator, left.float >= right.float),
            .LESS => try Object.initBool(self.allocator, left.float < right.float),
            .LESS_EQUAL => try Object.initBool(self.allocator, left.float <= right.float),
            .EQUAL_EQUAL => try Object.initBool(self.allocator, self.isEqual(left.*, right.*)),
            else => Object.initString(self.allocator, "NOT EVALUATED"),
        };
    }

    pub fn visitExpression(self: *Interpreter, stmt: *Stmt) !void {
        _ = try self.evaluate(stmt.expression.expression);
    }

    pub fn visitPrint(self: *Interpreter, stmt: *Stmt) !void {
        const value = self.evaluate(stmt.print.expression) catch |err| {
            if (err == error.UndefinedVariable) {
                return;
            }
            return err;
        };
        const str = try self.stringify(value);
        const file = std.io.getStdOut();
        _ = try file.write(str);
        _ = try file.write("\n");
    }

    pub fn visitBlock(self: *Interpreter, stmt: *Stmt) !void {
        var environment = try Environment.init(self.allocator, self.environment);
        defer environment.deinit();

        try self.executeBlock(stmt.block.statements, environment);
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

    pub fn visitVariableStmt(self: *Interpreter, stmt: *Stmt) !void {
        var value: ?*Object = null;
        if (stmt.variable.initializer) |it| {
            value = try self.evaluate(it);
        }
        try self.environment.define(self.allocator, stmt.variable.name.lexer, value);
    }

    pub fn visitVariableExpr(self: *Interpreter, expr: Expr) !*Object {
        const variable = try self.environment.get(expr.variable.name);
        if (variable) |v| {
            return v;
        } else return error.UndefinedVariable;
    }

    pub fn visitAssignExpr(self: *Interpreter, expr: Expr) !*Object {
        const value = try self.evaluate(expr.assign.value);
        try self.environment.assign(expr.assign.name, value);
        return value;
    }

    pub fn visitWhile(self: *Interpreter, stmt: *Stmt) !void {
        _ = self;
        _ = stmt;
    }

    pub fn visitUnary(self: *Interpreter, expr: Expr) !*Object {
        const right = try self.evaluate(expr.unary.right);
        return switch (expr.unary.operator.tokenType) {
            .MINUS => right,
            else => @panic("PANIC UNARY"),
        };
    }

    fn executeBlock(self: *Interpreter, statements: []*Stmt, environment: *Environment) !void {
        const previous = self.environment;
        self.environment = environment;
        errdefer self.environment = previous;

        for (statements) |st| {
            try self.execute(st);
        }
        self.environment = previous;
    }

    fn execute(self: *Interpreter, stmt: *Stmt) !void {
        try stmt.accept(anyerror!void, self);
    }

    fn evaluate(self: *Interpreter, expr: *Expr) !*Object {
        return expr.accept(self, *Object);
    }

    fn isEqual(self: *Interpreter, a: Object, b: Object) bool {
        _ = self;
        // if (a == null and b == null) return true;
        // if (a == null) return false;

        if (a == Object.float and b == Object.float) {
            return a.float == b.float;
        } else if (a == Object.string and b == Object.string) {
            return std.mem.eql(u8, a.string, b.string);
        } else if (a == Object.boolean and b == Object.boolean) {
            return a.boolean == b.boolean;
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

test "expect save environment correctly" {
    const allocator = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    const interpreter = Interpreter.init(arena.allocator());
    defer interpreter.deinit();

    const obj = try Object.initBool(arena.allocator(), true);

    try interpreter.environment.define(arena.allocator(), "a", obj);
    const env_obj = try interpreter.environment.get(Token{ .lexer = "a", .line = 1, .tokenType = TokenType.IDENTIFIER });

    try std.testing.expectEqual(obj, env_obj);
    try std.testing.expectEqual(obj.boolean, env_obj.?.*.boolean);
}
