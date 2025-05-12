const std = @import("std");
const Allocator = std.mem.Allocator;
const Object = @import("expression.zig").Object;
const Token = @import("token.zig").Token;

pub const Environment = struct {
    enclosing: ?*Environment,
    values: std.StringHashMap(?*Object),

    pub fn init(allocator: Allocator, enclosing: ?*Environment) Environment {
        return .{
            .values = std.StringHashMap(?*Object).init(allocator),
            .enclosing = enclosing,
        };
    }

    pub fn deinit(self: *Environment) void {
        self.values.deinit();
    }

    pub fn size(self: *Environment) usize {
        std.debug.print("CAPACITY {d}\n", .{self.values.capacity()});
        return self.values.count();
    }

    pub fn define(self: *Environment, allocator: Allocator, name: []const u8, value: ?*Object) !void {
        _ = allocator;
        try self.values.put(name, value);
    }

    pub fn iterator(self: *Environment) void {
        var it = self.values.iterator();
        while (it.next()) |v| {
            std.debug.print("KEY {s} VALUE {any}\n", .{ v.key_ptr.*, v.value_ptr.* });
        }
    }

    pub fn assign(self: *Environment, name: Token, value: *Object) !void {
        if (self.values.contains(name.lexer)) {
            try self.values.put(name.lexer, value);
            return;
        }
        if (self.enclosing) |enc| {
            try enc.assign(name, value);
            return;
        }
        return error.UndefinedVariable;
    }

    pub fn get(self: *Environment, name: Token) !?*Object {
        if (self.values.contains(name.lexer)) {
            return self.values.get(name.lexer).?;
        }

        if (self.enclosing) |enc| return enc.get(name);

        std.log.err("Use of undefined variable {s}\n", .{name.lexer});
        return error.UndefinedVariable;
    }
};
