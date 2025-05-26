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
    globals: *Environment,
    environment: *Environment,

    pub fn init(allocator: Allocator) !*Interpreter {
        const interpreter = allocator.create(Interpreter) catch |e| {
            std.log.err("Error while creating interpreter {any}\n", .{e});
            @panic("error while creating");
        };

        const global_env = Environment.init(allocator, null) catch |e| {
            std.log.err("Error while creating global environment {any}\n", .{e});
            @panic("error while creating");
        };

        const callable_clock = ClockFunction.init(allocator).callable();
        const clock_object = Object.initCallable(allocator, callable_clock) catch |e| {
            std.log.err("Error while creating clock object {any}\n", .{e});
            @panic("error while creating");
        };

        try global_env.define(allocator, "clock", clock_object);

        interpreter.* = .{ .allocator = allocator, .environment = global_env, .globals = global_env };
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
                Object.nil => {
                    return "nil";
                },
                Object.callable => {
                    return try std.fmt.allocPrint(self.allocator, "<native fn@{*}>", .{ob.callable.ptr});
                    // return "<native fn>";
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
                return err;
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

    pub fn visitFunctionStmt(self: *Interpreter, stmt: *Stmt) !void {
        const function = MiloFunction.init(self.allocator, stmt);
        try self.environment.define(self.allocator, stmt.function.name.lexer, try Object.initCallable(self.allocator, function.callable()));
    }

    pub fn visitIfStmt(self: *Interpreter, stmt: *Stmt) !void {
        std.debug.print("Visiting if statement\n", .{});
        if (self.isTruthy(try self.evaluate(stmt.if_statement.condition))) {
            try self.execute(stmt.if_statement.then_branch);
        } else if (stmt.if_statement.else_branch != null) {
            try self.execute(stmt.if_statement.else_branch.?);
        }
    }

    pub fn visitWhileStmt(self: *Interpreter, stmt: *Stmt) !void {
        std.debug.print("Visiting while statement\n", .{});
        while (self.isTruthy(try self.evaluate(stmt.while_statement.condition))) {
            try self.execute(stmt.while_statement.body);
        }
    }

    pub fn visitForStatement(self: *Interpreter, stmt: *Stmt) !void {
        std.debug.print("Visiting for statement\n", .{});
        while (self.isTruthy(try self.evaluate(stmt.@"for".condition))) {
            try self.execute(stmt.@"for".body);
        }
    }

    pub fn visitReturn(self: *Interpreter, stmt: *Stmt) !void {
        _ = self;
        _ = stmt;
    }

    pub fn visitVariableStmt(self: *Interpreter, stmt: *Stmt) !void {
        if (self.environment.contains(stmt.variable.name.lexer)) {
            return error.VariableShadowing;
        }

        var value: ?*Object = null;
        if (stmt.variable.initializer) |it| {
            value = try self.evaluate(it);
        }
        try self.environment.define(self.allocator, stmt.variable.name.lexer, value);
    }

    pub fn visitCallExpr(self: *Interpreter, expr: Expr) !*Object {
        const callee = try self.evaluate(expr.call.callee);

        var arguments = std.ArrayList(*Object).init(self.allocator);

        for (expr.call.arguments) |arg| {
            try arguments.append(try self.evaluate(arg));
        }

        return switch (callee.*) {
            .callable => {
                if (arguments.items.len != callee.callable.arity()) {
                    std.log.err("Expected {d} arguments but got {d}.\n", .{ callee.callable.arity(), arguments.items.len });
                    return error.UnexpectedArgsSize;
                }
                return callee.callable.call(self, try arguments.toOwnedSlice());
            },
            else => return error.NotCallable,
        };
    }

    pub fn visitVariableExpr(self: *Interpreter, expr: Expr) !*Object {
        const variable = try self.environment.get(expr.variable.name);
        if (variable) |v| {
            return v;
        } else return error.NotInitializedVariable;
    }

    pub fn visitAssignExpr(self: *Interpreter, expr: Expr) !*Object {
        const value = try self.evaluate(expr.assign.value);
        try self.environment.assign(expr.assign.name, value);
        return value;
    }

    pub fn visitLogicalExpr(self: *Interpreter, expr: Expr) !*Object {
        const left = try self.evaluate(expr.logical.left);

        if (expr.logical.operator.tokenType == TokenType.OR) {
            if (self.isTruthy(left)) return left;
        } else {
            if (!self.isTruthy(left)) return left;
        }

        return try self.evaluate(expr.logical.right);
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
        return switch (object.*) {
            .boolean => object.*.boolean,
            .nil => false,
            else => true,
        };
    }
};

const CallErr = error{
    CallError,
    OutOfMemory,
};

pub const Callable = struct {
    ptr: *anyopaque,
    callFn: *const fn (ptr: *anyopaque, interpreter: *Interpreter, arguments: []*Object) CallErr!*Object,
    arityFn: *const fn (ptr: *anyopaque) usize,

    fn call(self: Callable, interpreter: *Interpreter, arguments: []*Object) !*Object {
        return self.callFn(self.ptr, interpreter, arguments);
    }

    fn arity(self: Callable) usize {
        return self.arityFn(self.ptr);
    }
};

const ClockFunction = struct {
    allocator: Allocator,

    fn init(allocator: Allocator) *ClockFunction {
        const clock = allocator.create(ClockFunction) catch unreachable;
        clock.* = ClockFunction{
            .allocator = allocator,
        };
        return clock;
    }

    fn arity(ptr: *anyopaque) usize {
        _ = ptr;
        return 0;
    }

    fn call(ptr: *anyopaque, interpreter: *Interpreter, arguments: []*Object) CallErr!*Object {
        _ = interpreter;
        _ = arguments;

        const self: *ClockFunction = @ptrCast(@alignCast(ptr));

        const millis = std.time.milliTimestamp();
        const time = @as(f64, @floatFromInt(millis)) / 1000.0;

        return Object.initFloat(self.allocator, time) catch |e| {
            std.debug.print("Error {!}", .{e});
            return CallErr.CallError;
        };
    }

    fn callable(self: *ClockFunction) Callable {
        return .{
            .ptr = self,
            .callFn = call,
            .arityFn = arity,
        };
    }
};

const MiloFunction = struct {
    allocator: Allocator,
    declaration: *Stmt,

    fn init(allocator: Allocator, declaration: *Stmt) *MiloFunction {
        const function = allocator.create(MiloFunction) catch unreachable;
        function.* = MiloFunction{
            .allocator = allocator,
            .declaration = declaration,
        };
        return function;
    }

    fn arity(ptr: *anyopaque) usize {
        const self: *MiloFunction = @ptrCast(@alignCast(ptr));
        return self.declaration.function.params.len;
    }

    fn call(ptr: *anyopaque, interpreter: *Interpreter, arguments: []*Object) CallErr!*Object {
        const self: *MiloFunction = @ptrCast(@alignCast(ptr));
        const environment = Environment.init(self.allocator, interpreter.globals) catch |e| {
            std.log.err("Error {!}\n", .{e});
            return CallErr.CallError;
        };

        for (self.declaration.function.params, 0..) |param, i| {
            try environment.define(self.allocator, param.lexer, arguments[i]);
        }

        interpreter.executeBlock(self.declaration.function.body, environment) catch return CallErr.CallError;

        return try Object.initNil(self.allocator);
    }

    fn callable(self: *MiloFunction) Callable {
        return .{
            .ptr = self,
            .callFn = call,
            .arityFn = arity,
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
