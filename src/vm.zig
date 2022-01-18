const std = @import("std");
const builtin = @import("builtin");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const value = @import("value.zig");
const Value = value.Value;
const debug = @import("debug.zig");
const Table = @import("table.zig").Table;
const object = @import("object.zig");
const ObjString = object.ObjString;

const Compiler = @import("compiler.zig").Compiler;
const GarbageCollector = @import("memory.zig").GarbageCollector;
const Allocator = std.mem.Allocator;

const InterpretError = error{ Compile, Runtime } || std.os.WriteError;
const STACK_MAX = 256;

pub const VM = struct {
    const Self = VM;

    gc: *GarbageCollector,
    compiler: Compiler,
    globals: Table,
    stack: [STACK_MAX]Value = [_]Value{.nil} ** STACK_MAX,

    pub fn init(allocator: Allocator) !Self {
        const gc = try GarbageCollector.init(allocator);
        return Self{
            .gc = gc,
            .compiler = Compiler.init(gc),
            .globals = Table.init(gc),
        };
    }

    pub fn free(self: *Self) void {
        self.globals.free();
        self.gc.free();
    }

    pub fn interpret(self: *Self, source: []u8) InterpretError!void {
        var chunk = Chunk.init(self.gc.allocator);
        defer chunk.free();
        if (try self.compiler.compile(source, &chunk)) {
            var c = ExecutionContext{ .vm = self, .chunk = &chunk, .ip = chunk.code.ptr, .stack_top = &self.stack };
            try c.run();
        }
    }
};

fn greater(x: f64, y: f64) Value {
    return .{ .boolean = (x > y) };
}

fn less(x: f64, y: f64) Value {
    return .{ .boolean = (x < y) };
}

fn add(x: f64, y: f64) Value {
    return .{ .number = x + y };
}

fn sub(x: f64, y: f64) Value {
    return .{ .number = x - y };
}

fn mul(x: f64, y: f64) Value {
    return .{ .number = x * y };
}

fn div(x: f64, y: f64) Value {
    return .{ .number = x / y };
}

const ExecutionContext = struct {
    const Self = ExecutionContext;

    vm: *VM,
    chunk: *Chunk,
    ip: [*]u8,
    stack_top: [*]Value,

    fn resetStack(self: *Self) void {
        self.stack_top = &self.vm.stack;
    }

    fn runtimeError(self: *Self, comptime format: []const u8, args: anytype) void {
        std.debug.print(format, args);
        std.debug.print("\n", .{});
        const offset = @ptrToInt(self.ip) - @ptrToInt(self.chunk.code.ptr) - 1;
        const line = self.chunk.getLine(offset);
        std.debug.print("[line {d}] in script\n", .{line});
        self.resetStack();
    }

    fn push(self: *Self, v: Value) void {
        self.stack_top[0] = v;
        self.stack_top += 1;
    }

    fn pop(self: *Self) Value {
        self.stack_top -= 1;
        return self.stack_top[0];
    }

    fn peek(self: *Self, distance: usize) Value {
        const v = self.stack_top - (distance + 1);
        return v[0];
    }

    fn read_byte(self: *Self) u8 {
        const b = self.ip[0];
        self.ip += 1;
        return b;
    }

    fn read_short(self: *Self) u16 {
        const b1 = self.ip[0];
        const b0 = self.ip[1];
        self.ip += 2;
        return (@intCast(u16, b1) << 8) | @intCast(u16, b0);
    }

    fn read_constant(self: *Self) Value {
        return self.chunk.constants.values[self.read_byte()];
    }

    fn read_string(self: *Self) *ObjString {
        return self.read_constant().asString();
    }

    fn read_constant_long(self: *Self) Value {
        const b0 = @intCast(u32, self.read_byte());
        const b1 = @intCast(u32, self.read_byte());
        const b2 = @intCast(u32, self.read_byte());
        return self.chunk.constants.values[(b2 << 16) | (b1 << 8) | b0];
    }

    fn run(self: *Self) InterpretError!void {
        while (true) {
            if (builtin.mode == std.builtin.Mode.Debug) {
                std.debug.print("          ", .{});
                const n = (@ptrToInt(self.stack_top) - @ptrToInt(&self.vm.stack)) / @sizeOf(Value);
                var slice = self.vm.stack[0..n];
                for (slice) |v| {
                    std.debug.print("[ ", .{});
                    value.print(v);
                    std.debug.print(" ]", .{});
                }
                std.debug.print("\n", .{});
                _ = debug.disassembleInstruction(self.chunk, @ptrToInt(self.ip) - @ptrToInt(self.chunk.code.ptr));
            }
            const instruction = self.read_byte();
            switch (@intToEnum(OpCode, instruction)) {
                .Print => {
                    value.print(self.pop());
                    std.debug.print("\n", .{});
                    return;
                },
                .Jump => {
                    const offset = self.read_short();
                    self.ip += offset;
                },
                .JumpIfFalse => {
                    const offset = self.read_short();
                    if (self.peek(0).isFalsey()) {
                        self.ip += offset;
                    }
                },
                .Return => {
                    return;
                },
                .Constant => {
                    const constant = self.read_constant();
                    self.push(constant);
                },
                .ConstantLong => {
                    const constant = self.read_constant_long();
                    self.push(constant);
                },
                .Nil => {
                    self.push(.nil);
                },
                .True => {
                    self.push(.{ .boolean = true });
                },
                .False => {
                    self.push(.{ .boolean = false });
                },
                .Pop => {
                    _ = self.pop();
                },
                .GetLocal => {
                    const slot = self.read_byte();
                    self.push(self.vm.stack[slot]);
                },
                .SetLocal => {
                    const slot = self.read_byte();
                    self.vm.stack[slot] = self.peek(0);
                },
                .GetGlobal => {
                    const name = self.read_string();
                    if (self.vm.globals.get(name)) |v| {
                        self.push(v.*);
                    } else {
                        self.runtimeError("Undefined variable '{s}'", .{name.str});
                        return InterpretError.Runtime;
                    }
                },
                .DefineGlobal => {
                    const name = self.read_string();
                    _ = self.vm.globals.set(name, self.peek(0)) catch {
                        self.runtimeError("Out of Memory", .{});
                        return InterpretError.Runtime;
                    };
                    _ = self.pop();
                },
                .SetGlobal => {
                    const name = self.read_string();
                    const was_new = self.vm.globals.set(name, self.peek(0)) catch {
                        self.runtimeError("Out of Memory", .{});
                        return InterpretError.Runtime;
                    };
                    if (was_new) {
                        _ = self.vm.globals.delete(name);
                        self.runtimeError("Undefined variable '{s}'", .{name});
                        return InterpretError.Runtime;
                    }
                },
                .Equal => {
                    const b = self.pop();
                    const a = self.pop();
                    self.push(.{ .boolean = a.equal(b) });
                },
                .Greater => {
                    try self.binaryOp(greater);
                },
                .Less => {
                    try self.binaryOp(less);
                },
                .Add => {
                    if (self.peek(0).isString() and self.peek(1).isString()) {
                        try self.concatenate();
                    } else if ((self.peek(0) == .number) and (self.peek(1) == .number)) {
                        const rhs = self.pop();
                        const lhs = self.pop();
                        self.push(add(lhs.number, rhs.number));
                    } else {
                        self.runtimeError("Operands must be numbers.", .{});
                        return InterpretError.Runtime;
                    }
                },
                .Subtract => {
                    try self.binaryOp(sub);
                },
                .Multiply => {
                    try self.binaryOp(mul);
                },
                .Divide => {
                    try self.binaryOp(div);
                },
                .Not => {
                    self.push(.{ .boolean = self.pop().isFalsey() });
                },
                .Negate => {
                    if (self.peek(0) != .number) {
                        self.runtimeError("Operand must be a number.", .{});
                        return InterpretError.Runtime;
                    }
                    self.push(.{ .number = -self.pop().number });
                },
                _ => {
                    std.debug.print("Unknown opcode {d}", .{instruction});
                },
            }
        }
    }

    fn binaryOp(self: *Self, comptime op: anytype) InterpretError!void {
        if ((self.peek(0) != .number) or (self.peek(1) != .number)) {
            self.runtimeError("Operands must be numbers.", .{});
            return InterpretError.Runtime;
        }
        const rhs = self.pop();
        const lhs = self.pop();
        self.push(op(lhs.number, rhs.number));
    }

    fn concatenate(self: *Self) InterpretError!void {
        const b = self.pop().asString();
        const a = self.pop().asString();
        const chars = std.mem.concat(self.vm.gc.allocator, u8, &[_][]const u8{ a.str, b.str }) catch {
            self.runtimeError("Memory allocation failed.", .{});
            return InterpretError.Runtime;
        };
        errdefer {
            self.vm.gc.allocator.free(chars);
        }
        const result = self.vm.gc.takeString(chars) catch {
            self.runtimeError("Memory allocation failed.", .{});
            return InterpretError.Runtime;
        };
        self.push(.{ .obj = result.toObj() });
    }
};
