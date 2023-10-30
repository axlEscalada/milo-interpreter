const std = @import("std");
const Parser = @import("parser.zig");
const Token = @import("token.zig");
const TokenType = Token.TokenType;
const Scanner = @import("scanner.zig");
const Allocator = std.mem.Allocator;
const Interpreter = @import("interpreter.zig").Intrepreter;
const AstPrinter = @import("interpreter.zig").AstPrinter;
const GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator;
var hadError = false;

// pub fn main() !void {
//     var allocator: Allocator = undefined;
//     var general_purpose_allocator = GeneralPurposeAllocator(.{}){};
//     const gpa = general_purpose_allocator.allocator();
//     var arena_instance = std.heap.ArenaAllocator.init(gpa);
//     defer arena_instance.deinit();
//     allocator = arena_instance.allocator();
//
//     var nm: []const u8 = "123";
//     var lit = Expr.initLiteral(nm);
//     var unary = Expr.initUnary(&lit, Token{ .tokenType = TokenType.MINUS, .lexer = "-", .line = 1 });
//     var token = .{ .tokenType = TokenType.STAR, .lexer = "*", .line = 1 };
//     var otherLit = Expr.initLiteral("45.67");
//     var grouping = Expr.initGrouping(&otherLit);
//     var expression = Expr.initBinary(token, &unary, &grouping);
//     var astPrinter: AstPrinter = .{ .allocator = allocator };
//     var result = astPrinter.print(&expression);
//     std.debug.print("{s}", .{result});
// }

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
            std.os.exit(64);
        }
    }
}

fn runPrompt(alloc: std.mem.Allocator) !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    var buffer: [1024]u8 = undefined;
    var line: []u8 = undefined;
    while (true) {
        _ = try stdout.write(">> ");
        line = try stdin.readUntilDelimiterOrEof(&buffer, '\n') orelse "";
        if (std.mem.eql(u8, line, "exit")) std.process.exit(0);

        var scanner = Scanner{ .source = line, .alloc = alloc, .tokens = undefined };
        var rs = try scanner.scanTokens();
        _ = rs;
        // var parser = Parser{ .tokens = rs, .allocator = alloc };
        // var astPrinter = AstPrinter{ .allocator = alloc };
        // var expr = parser.expression();
        // if (!hadError) {
        //     var resultPrint = astPrinter.print(expr);
        //     std.debug.print("{s}\n", .{resultPrint});
        //     for (rs[0..]) |*r| {
        //         if (r.*.tokenType == TokenType.EOF) break;
        //         const final_url = try std.fmt.allocPrint(alloc, "Token: `{s}` Type: {}\n", .{ r.*.lexer, r.*.tokenType });
        //         defer alloc.free(final_url);
        //         _ = try stdout.write(final_url);
        //     }
        // }

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
