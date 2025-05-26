const std = @import("std");
const Parser = @import("parser.zig");
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const Scanner = @import("scanner.zig");
const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const Interpreter = @import("interpreter.zig").Interpreter;
const Environment = @import("interpreter.zig").Environment;
const AstPrinter = @import("ast_printer.zig").AstPrinter;
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
    defer arena.deinit();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len > 2) {
        std.debug.print("Usage milo [script]", .{});
        std.process.exit(0);
    } else if (args.len == 2) {
        try runFile(args[1], arena.allocator());
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
    var interpreter = try Interpreter.init(alloc);
    defer interpreter.deinit();
    while (true) {
        _ = try stdout.write(">> ");
        line = try stdin.readUntilDelimiterOrEof(&buffer, '\n') orelse "";
        defer line = undefined;
        if (std.mem.eql(u8, line, "exit")) std.process.exit(0);

        var scanner = Scanner.init(line, alloc);
        defer scanner.deinit();

        const tokens = try scanner.scanTokens();
        // printTokens(tokens);

        var parser = Parser.init(tokens, alloc);
        const statements = try parser.parse();
        if (!hadError) {
            try interpreter.interpret(statements);
        }

        hadError = false;
    }
}

fn runFile(path: []const u8, allocator: std.mem.Allocator) !void {
    std.debug.print("\x1b[36mInterpreted file: {s}\x1b[0m \n\n", .{path});

    const file_contents = try std.fs.cwd().readFileAlloc(allocator, path, 1024 * 1024);
    defer allocator.free(file_contents);

    var interpreter = try Interpreter.init(allocator);
    defer interpreter.deinit();

    var scanner = Scanner.init(file_contents, allocator);
    defer scanner.deinit();

    const tokens = try scanner.scanTokens();
    // printTokens(tokens);

    var parser = Parser.init(tokens, allocator);
    const statements = try parser.parse();
    try interpreter.interpret(statements);

    var line_count: usize = 1;
    for (file_contents) |c| {
        if (c == '\n') line_count += 1;
    }

    std.debug.print("\n\x1b[36m----------------------- \x1b[33m{d}\x1b[36m lines, \x1b[33m{d}\x1b[36m bytes \x1b[36m-----------------------\x1b[0m", .{ line_count, file_contents.len });
}

fn printTokens(tokens: std.ArrayList(Token)) void {
    for (tokens.items) |t| {
        std.debug.print("T {s} -> {s}\n", .{ @tagName(t.tokenType), t.lexer });
    }
}

const RuntimeError = error{
    RuntimeErrorInterpreter,
};
