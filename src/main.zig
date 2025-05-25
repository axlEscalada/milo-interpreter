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
        for (tokens.items) |t| {
            std.debug.print("T -> {s}\n", .{t.lexer});

        }
        var parser = Parser.init(tokens, alloc);
        const statements = try parser.parse();
        if (!hadError) {
            try interpreter.interpret(statements);
        }

        hadError = false;
    }
}

fn runFile(path: []const u8, allocator: std.mem.Allocator) !void {
    std.debug.print("FILE IS {s}\n", .{path});
    var file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    var buffered = std.io.bufferedReader(file.reader());
    var reader = buffered.reader();

    var arr = std.ArrayList(u8).init(allocator);
    defer arr.deinit();

    var line_count: usize = 0;
    var byte_count: usize = 0;
    var interpreter = Interpreter.init(allocator);
    defer interpreter.deinit();
    while (true) {
        reader.streamUntilDelimiter(arr.writer(), '\n', null) catch |err| switch (err) {
            error.EndOfStream => break,
            else => return err,
        };
        line_count += 1;
        byte_count += arr.items.len;
    }
    var scanner = Scanner.init(arr.items, allocator);
    defer scanner.deinit();

    const tokens = try scanner.scanTokens();
    var parser = Parser.init(tokens, allocator);
    const statements = try parser.parse();
    try interpreter.interpret(statements);

    std.debug.print("{d} lines, {d} bytes", .{ line_count, byte_count });
}

const RuntimeError = error{
    RuntimeErrorInterpreter,
};
