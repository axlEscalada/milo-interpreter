const std = @import("std");
const Parser = @import("parser.zig");
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const Scanner = @import("scanner.zig");
const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const Interpreter = @import("interpreter.zig").Interpreter;
const Environment = @import("interpreter.zig").Environment;
const AstPrinter = @import("interpreter.zig").AstPrinter;
const GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator;
const Expr = @import("expression.zig").Expr;
const Binary = @import("expression.zig").Binary;
const Literal = @import("expression.zig").Literal;
const Object = @import("expression.zig").Object;
var hadError = false;

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
    var arena = std.heap.ArenaAllocator.init(allocator);
    errdefer {
        std.debug.print("FREEING ARENA MEMORY\n", .{});
        arena.deinit();
    }

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len > 2) {
        std.debug.print("Usage milo [script]", .{});
        std.process.exit(0);
    } else if (args.len == 2) {
        try runFile(args[0], allocator);
    } else {
        try runPrompt(arena.allocator());
        if (hadError) {
            return;
        }
    }
}

fn runPrompt(alloc: std.mem.Allocator) !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    var buffer: [1024]u8 = undefined;
    var line: []u8 = undefined;
    _ = try alloc.alloc(u8, 1024);
    var interpreter = Interpreter.init(alloc);
    defer interpreter.deinit();
    while (true) {
        _ = try stdout.write(">> ");
        line = try stdin.readUntilDelimiterOrEof(&buffer, '\n') orelse "";
        defer line = undefined;
        if (std.mem.eql(u8, line, "exit")) std.process.exit(0);

        var scanner = Scanner.init(line, alloc);
        defer scanner.deinit();

        const tokens = try scanner.scanTokens();
        var parser = Parser.init(tokens, alloc);
        // var astPrinter = AstPrinter{ .allocator = alloc };
        // const expr = parser.expression() catch |e| {
        //     std.log.err("Error while parsing prompt {any}\n", .{e});
        //     return error.Prompt;
        // };
        const statements = try parser.parse();
        if (!hadError) {
            // const result = astPrinter.print(expr) catch |e| return e;
            // std.debug.print("AST print: {s}\n", .{result});
            // try interpreter.interpret(expr);
            try interpreter.interpret(statements);
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
