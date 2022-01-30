const std = @import("std");
const chunk = @import("chunk.zig");
const debug = @import("debug.zig");
const vm = @import("vm.zig");

pub const log_level = std.log.Level.debug;
pub var runtime_log_level = std.log.default_level;

pub const DEBUG_STRESS_GC = false;
pub const DEBUG_LIMIT_EXECUTION = 1000000;

const InterpretError = vm.InterpretError;

const Allocator = std.mem.Allocator;

const GPA = std.heap.GeneralPurposeAllocator(.{});

pub fn log(
    comptime level: std.log.Level,
    comptime scope: @TypeOf(.EnumLiteral),
    comptime format: []const u8,
    args: anytype,
) void {
    _ = scope;
    if (@enumToInt(level) > @enumToInt(runtime_log_level)) {
        return;
    }

    const prefix = "[" ++ level.asText() ++ "] ";

    // Print the message to stderr, silently ignoring any errors
    std.debug.getStderrMutex().lock();
    defer std.debug.getStderrMutex().unlock();
    const stderr = std.io.getStdErr().writer();
    nosuspend stderr.print(prefix ++ format ++ "\n", args) catch return;
}

pub fn main() anyerror!void {
    // const allocator = std.heap.page_allocator;
    var gpa = GPA{};
    defer if (gpa.deinit()) std.process.abort();
    const allocator = gpa.allocator();
    // const allocator = std.heap.c_allocator;

    if (std.os.getenv("ZLOX_DEBUG")) |value| {
        _ = value;
        runtime_log_level = .debug;
    }

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    if (args.len == 1) {
        try repl(allocator);
    } else if (args.len == 2) {
        try runFile(allocator, args[1]);
    } else {
        std.log.err("Usage: zlox [path]\n", .{});
        std.process.exit(64);
    }
}

pub fn repl(allocator: Allocator) !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut();
    var v = try vm.VM.init(allocator, stdout);
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
    const stdout = std.io.getStdOut();
    var v = try vm.VM.init(allocator, stdout);
    defer v.free();
    const file = try std.fs.cwd().openFile(path, .{ .read = true });
    defer file.close();

    const contents = try file.reader().readAllAlloc(
        allocator,
        1 << 20,
    );
    defer allocator.free(contents);

    // v.interpret(contents) catch |err| {
    //     if (err == InterpretError.Compile) {
    //         std.process.exit(65);
    //     } else if (err == InterpretError.Runtime) {
    //         std.process.exit(70);
    //     }
    // };
    v.interpret(contents) catch return;
}
