const std = @import("std");
const builtin = @import("builtin");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const value = @import("value.zig");
const debug = @import("debug.zig");

const Compiler = @import("compiler.zig").Compiler;
const Allocator = std.mem.Allocator;

const InterpretError = error{ Compile, Runtime } || std.os.WriteError;
const STACK_MAX = 256;

pub const VM = struct {
    const Self = VM;

    allocator: Allocator,
    compiler: Compiler,
    stack: [STACK_MAX]value.Value,

    pub fn init(allocator: Allocator) Self {
        return Self{ .allocator = allocator, .compiler = Compiler.init(allocator), .stack = [_]value.Value{0.0} ** STACK_MAX };
    }

    pub fn free(self: *Self) void {
        _ = self;
    }

    pub fn interpret(self: *Self, source: []u8) InterpretError!void {
        var chunk = Chunk.init(self.allocator);
        defer chunk.free();
        if (try self.compiler.compile(source, &chunk)) {
            var c = ExecutionContext{ .vm = self, .chunk = &chunk, .ip = chunk.code.ptr, .stack_top = &self.stack };
            try c.run();
        }
    }
};

const ExecutionContext = struct {
    const Self = ExecutionContext;

    vm: *VM,
    chunk: *Chunk,
    ip: [*]u8,
    stack_top: [*]value.Value,

    fn resetStack(self: *Self) void {
        self.stack_top = self.vm.stack.ptr;
    }

    fn push(self: *Self, v: value.Value) void {
        self.stack_top[0] = v;
        self.stack_top += 1;
    }

    fn pop(self: *Self) value.Value {
        self.stack_top -= 1;
        return self.stack_top[0];
    }

    fn read_byte(self: *Self) u8 {
        const b = self.ip[0];
        self.ip += 1;
        return b;
    }

    fn read_constant(self: *Self) value.Value {
        return self.chunk.constants.values[self.read_byte()];
    }

    fn read_constant_long(self: *Self) value.Value {
        const b0 = @intCast(u32, self.read_byte());
        const b1 = @intCast(u32, self.read_byte());
        const b2 = @intCast(u32, self.read_byte());
        return self.chunk.constants.values[(b2 << 16) | (b1 << 8) | b0];
    }

    fn run(self: *Self) InterpretError!void {
        while (true) {
            if (builtin.mode == std.builtin.Mode.Debug) {
                std.debug.print("          ", .{});
                const n = (@ptrToInt(self.stack_top) - @ptrToInt(&self.vm.stack)) / @sizeOf(value.Value);
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
                .Return => {
                    value.print(self.pop());
                    std.debug.print("\n", .{});
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
                .Add => {
                    const b = self.pop();
                    const a = self.pop();
                    self.push(a + b);
                },
                .Subtract => {
                    const b = self.pop();
                    const a = self.pop();
                    self.push(a - b);
                },
                .Multiply => {
                    const b = self.pop();
                    const a = self.pop();
                    self.push(a * b);
                },
                .Divide => {
                    const b = self.pop();
                    const a = self.pop();
                    self.push(a / b);
                },
                .Negate => {
                    self.push(-self.pop());
                },
                _ => {
                    std.debug.print("Unknown opcode {d}", .{instruction});
                },
            }
        }
    }
};
