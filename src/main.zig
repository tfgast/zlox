const std = @import("std");
const chunk = @import("chunk.zig");
const debug = @import("debug.zig");
const vm = @import("vm.zig");

const Allocator = std.mem.Allocator;

const GPA = std.heap.GeneralPurposeAllocator(.{});

pub fn main() anyerror!void {
    // const allocator = std.heap.page_allocator;
    var gpa = GPA{};
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    if (args.len == 1) {
        try repl(allocator);
    } else if (args.len == 1) {
        try runFile(allocator, args[1]);
    } else {
        std.log.err("Usage: zlox [path]\n", .{});
        std.process.exit(64);
    }
}

pub fn repl(allocator: Allocator) !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut();
    var v = try vm.VM.init(allocator);
    defer v.free();
    var buf: [1024]u8 = undefined;
    while (true) {
        try stdout.writeAll("> ");
        if (try stdin.readUntilDelimiterOrEof(buf[0..], '\n')) |line| {
            v.interpret(line) catch {
                continue;
            };
        } else {
            try stdout.writeAll("\n");
            break;
        }
    }
}

pub fn runFile(allocator: Allocator, path: []u8) !void {
    var v = try vm.VM.init(allocator);
    defer v.free();
    const file = try std.fs.cwd().openFile(path, .{ .read = true });
    defer file.close();

    const contents = try file.reader().readAllAlloc(
        allocator,
        1 << 20,
    );
    defer allocator.free(contents);

    try v.interpret(contents);
}
