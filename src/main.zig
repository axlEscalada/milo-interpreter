const std = @import("std");
const Parser = @import("parser.zig");
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const Scanner = @import("scanner.zig");
const Allocator = std.mem.Allocator;
const Interpreter = @import("interpreter.zig").Interpreter;
const AstPrinter = @import("interpreter.zig").AstPrinter;
const GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator;
const Expr = @import("expression.zig").Expr;
const Expression = @import("expression.zig").Expression;
const Binary = @import("expression.zig").Binary;
const Literal = @import("expression.zig").Literal;
const Object = @import("expression.zig").Object;
var hadError = false;

// pub fn main() !void {
//     var allocator: Allocator = undefined;
//     var general_purpose_allocator = GeneralPurposeAllocator(.{}){};
//     const gpa = general_purpose_allocator.allocator();
//     var arena_instance = std.heap.ArenaAllocator.init(gpa);
//     defer arena_instance.deinit();
//     allocator = arena_instance.allocator();
//
//     const nm: []const u8 = "123";
//     var obj = try Object.initString(allocator, nm);
//     printObj(&obj);
//     const lit = try Expr.initLiteral(allocator, nm, obj);
//     printExpr(lit);
//     const tokenUnary = try Token.init(allocator, TokenType.MINUS, "-", 1, null);
//     const unary = try Expr.initUnary(allocator, lit, tokenUnary);
//     printExpr(unary.right.?);
//     const token = try Token.init(allocator, TokenType.STAR, "*", 1, null);
//     const otherNm: []const u8 = "123";
//     const otherObj = try Object.initString(allocator, otherNm);
//     const otherLit = try Expr.initLiteral(allocator, "45.67", otherObj);
//     const grouping = try Expr.initGrouping(allocator, otherLit);
//     const expression = try Expr.initBinary(allocator, token, unary, grouping);
//     var astPrinter: AstPrinter = .{ .allocator = allocator };
//     const result = try astPrinter.print(expression);
//     std.debug.print("{s}", .{result});
// }

fn printObj(obj: *Object) void {
    std.debug.print("OBJECT {any}\n", .{obj});
    std.debug.print("OBJECT String {s}\n", .{obj.string.*});
}

fn printExpr(expr: *Expr) void {
    std.debug.print("EXPRESSION PRINT {any}\n", .{expr});
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len > 2) {
        std.debug.print("Usage milo [script]", .{});
        std.process.exit(0);
    } else if (args.len == 2) {
        try runFile(args[0], allocator);
    } else {
        try runPrompt(allocator);
        if (hadError) {
            return;
        }
    }
}
// pub fn main() !void {
//     var allocator: Allocator = undefined;
//     var general_purpose_allocator = GeneralPurposeAllocator(.{}){};
//     const gpa = general_purpose_allocator.allocator();
//     var arena_instance = std.heap.ArenaAllocator.init(gpa);
//     defer arena_instance.deinit();
//     allocator = arena_instance.allocator();
//
//     const astPrinter: AstPrinter = .{ .allocator = allocator };
//
//     var o: f64 = 1.0;
//     var e: f64 = 2.0;
//     const one = Object{ .float = &o };
//     const two = Object{ .float = &e };
//     var literal = Literal{ .value = one, .value_string = "1" };
//     var right = Literal{ .value = two, .value_string = "2" };
//     var exprLeft = Expression.init(&literal);
//     var exprRight = Expression.init(&right);
//     const binary: Binary = Binary{ .operator = Token{ .tokenType = TokenType.MINUS, .lexer = "-", .line = 1 }, .left = &exprLeft, .right = &exprRight };
//
//     const rs = binary.accept(astPrinter, []const u8);
//     std.debug.print("RESULT {s}\n", .{rs});
// }

fn runPrompt(alloc: std.mem.Allocator) !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    var buffer: [1024]u8 = undefined;
    var line: []u8 = undefined;
    while (true) {
        _ = try stdout.write(">> ");
        line = try stdin.readUntilDelimiterOrEof(&buffer, '\n') orelse "";
        defer line = undefined;
        if (std.mem.eql(u8, line, "exit")) std.process.exit(0);

        var scanner = Scanner.init(line, alloc);
        defer scanner.deinit();

        const tokens = try scanner.scanTokens();
        var parser = Parser.init(tokens, alloc);
        var astPrinter = AstPrinter{ .allocator = alloc };
        var interpreter = Interpreter{ .allocator = alloc };
        const expr = parser.expression() catch |e| {
            std.log.err("Error while parsing prompt {any}\n", .{e});
            return error.Prompt;
        };
        if (!hadError) {
            const result = astPrinter.print(expr) catch |e| return e;
            std.debug.print("{s}\n", .{result});
            try interpreter.interpret(expr);
            // for (tokens[0..]) |*r| {
            //     if (r.*.tokenType == TokenType.EOF) break;
            //     const final_url = try std.fmt.allocPrint(alloc, "Token: `{s}` Type: {}\n", .{ r.*.lexer, r.*.tokenType });
            //     defer alloc.free(final_url);
            //     _ = try stdout.write(final_url);
            // }
        }

        hadError = false;
    }
}

fn runFile(path: []const u8, allocator: std.mem.Allocator) !void {
    _ = allocator;
    var file = try std.fs.cwd().openFile(path, .{});
    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [1024]u8 = undefined;

    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        std.debug.print("Line: {s}", .{line});
    }
}

const RuntimeError = error{
    RuntimeErrorInterpreter,
};
