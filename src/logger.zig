const std = @import("std");

pub fn report(alloc: std.mem.Allocator, line: u16, where: []const u8, message: []const u8) void {
    const stdout = std.io.getStdErr().writer();
    const final_url = std.fmt.allocPrint(alloc, "[line {}] Error {s}: {s} \n", .{ line, where, message }) catch |e| {
        std.debug.print("Error {!}", .{e});
        @panic("Error logging");
    };
    defer alloc.free(final_url);
    _ = stdout.write(final_url) catch |e| {
        std.debug.print("Error {!}", .{e});
        @panic("Error loggin");
    };
}
