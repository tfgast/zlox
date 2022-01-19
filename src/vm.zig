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
const ObjFunction = object.ObjFunction;
const ObjClosure = object.ObjClosure;
const ObjUpvalue = object.ObjUpvalue;
const NativeFn = object.NativeFn;

const Compiler = @import("compiler.zig").Compiler;
const CompileError = @import("compiler.zig").CompileError;
const GarbageCollector = @import("memory.zig").GarbageCollector;
const Allocator = std.mem.Allocator;

pub const RuntimeError = error{Runtime};
pub const InterpretError = RuntimeError || CompileError || std.os.WriteError;
const FRAMES_MAX = 64;
const STACK_MAX = FRAMES_MAX * 256;

const CallFrame = struct {
    closure: *ObjClosure,
    ip: [*]u8,
    slots: [*]Value,
};

pub const VM = struct {
    const Self = VM;

    gc: *GarbageCollector,
    compiler: Compiler,
    globals: Table,
    open_upvalues: ?*ObjUpvalue,
    stack: [STACK_MAX]Value = [_]Value{.nil} ** STACK_MAX,
    frames: [FRAMES_MAX]CallFrame,

    pub fn init(allocator: Allocator) !Self {
        const gc = try GarbageCollector.init(allocator);
        var vm = Self{
            .gc = gc,
            .compiler = Compiler.init(gc),
            .globals = Table.init(gc),
            .open_upvalues = null,
            .frames = undefined,
        };
        var c = ExecutionContext.init(&vm);
        _ = try clockNative(&[_]Value{});
        try c.defineNative("clock", clockNative);
        return vm;
    }

    pub fn free(self: *Self) void {
        self.globals.free();
        self.gc.free();
    }

    pub fn interpret(self: *Self, source: []u8) InterpretError!void {
        var c = ExecutionContext.init(self);
        if (builtin.mode == std.builtin.Mode.Debug) {
            std.debug.print("== Compiling ==\n", .{});
        }
        const function = try self.compiler.compile(source);
        c.push(.{ .obj = function.asObj() });
        const closure = try self.gc.newClosure(function);
        _ = c.pop();
        c.push(.{ .obj = closure.asObj() });
        _ = c.call(closure, 0);
        if (builtin.mode == std.builtin.Mode.Debug) {
            std.debug.print("== Running ==\n", .{});
        }
        try c.run();
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
    stack_top: [*]Value,
    frame: [*]CallFrame,
    frame_count: usize = 0,

    fn init(vm: *VM) Self {
        return Self{ .vm = vm, .stack_top = &vm.stack, .frame = &vm.frames };
    }

    fn resetStack(self: *Self) void {
        self.stack_top = &self.vm.stack;
        self.frame_count = 0;
    }

    fn runtimeError(self: *Self, comptime format: []const u8, args: anytype) void {
        std.debug.print(format, args);
        std.debug.print("\n", .{});
        var i = self.frame_count;
        while (i > 0) {
            i -= 1;
            const frame = &self.vm.frames[i];
            const function = frame.closure.function;
            const offset = @ptrToInt(frame.ip) - @ptrToInt(function.chunk.code.ptr) - 1;
            const line = frame.closure.function.chunk.getLine(offset);
            std.debug.print("[line {d}] in ", .{line});
            function.print();
            std.debug.print("\n", .{});
        }
        self.resetStack();
    }

    fn defineNative(self: *Self, name: []const u8, function: NativeFn) InterpretError!void {
        const string = try self.vm.gc.copyString(name);
        self.push(.{ .obj = string.asObj() });
        const native = try self.vm.gc.newNative(function);
        self.push(.{ .obj = native.asObj() });
        _ = try self.vm.globals.set(self.peek(1).asString(), self.peek(0));
        _ = self.pop();
        _ = self.pop();
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
        const b = self.frame[0].ip[0];
        self.frame[0].ip += 1;
        return b;
    }

    fn read_short(self: *Self) u16 {
        const b1 = self.frame[0].ip[0];
        const b0 = self.frame[0].ip[1];
        self.frame[0].ip += 2;
        return (@intCast(u16, b1) << 8) | @intCast(u16, b0);
    }

    fn read_constant(self: *Self) Value {
        return self.frame[0].closure.function.chunk.constants.values[self.read_byte()];
    }

    fn read_string(self: *Self) *ObjString {
        return self.read_constant().asString();
    }

    fn read_constant_long(self: *Self) Value {
        const b0 = @intCast(u32, self.read_byte());
        const b1 = @intCast(u32, self.read_byte());
        const b2 = @intCast(u32, self.read_byte());
        return self.frame[0].closure.function.chunk.constants.values[(b2 << 16) | (b1 << 8) | b0];
    }

    fn run(self: *Self) InterpretError!void {
        var base_frame: [*]CallFrame = &self.vm.frames;
        self.frame = base_frame + (self.frame_count - 1);
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
                _ = debug.disassembleInstruction(&self.frame[0].closure.function.chunk, @ptrToInt(self.frame[0].ip) - @ptrToInt(self.frame[0].closure.function.chunk.code.ptr));
            }
            const instruction = self.read_byte();
            switch (@intToEnum(OpCode, instruction)) {
                .Print => {
                    value.print(self.pop());
                    std.debug.print("\n", .{});
                },
                .Jump => {
                    const offset = self.read_short();
                    self.frame[0].ip += offset;
                },
                .JumpIfFalse => {
                    const offset = self.read_short();
                    if (self.peek(0).isFalsey()) {
                        self.frame[0].ip += offset;
                    }
                },
                .Loop => {
                    const offset = self.read_short();
                    self.frame[0].ip -= offset;
                },
                .Call => {
                    const arg_count = self.read_byte();
                    if (!self.callValue(self.peek(arg_count), arg_count)) {
                        return InterpretError.Runtime;
                    }
                    self.frame = base_frame + self.frame_count - 1;
                },
                .CloseUpvalue => {
                    self.closeUpvalues(self.stack_top - 1);
                    _ = self.pop();
                },
                .Return => {
                    const result = self.pop();
                    self.closeUpvalues(self.frame[0].slots);
                    self.frame_count -= 1;
                    if (self.frame_count == 0) {
                        _ = self.pop();
                        return;
                    }

                    self.stack_top = self.frame[0].slots;
                    self.push(result);
                    self.frame = base_frame + self.frame_count - 1;
                },
                .Closure => {
                    const function = self.read_constant().asFunction();
                    const closure = self.vm.gc.newClosure(function) catch {
                        self.runtimeError("Out of Memory", .{});
                        return InterpretError.Runtime;
                    };
                    self.push(.{ .obj = closure.asObj() });
                    for (closure.upvalues) |*upvalue| {
                        const is_local = self.read_byte();
                        const index = self.read_byte();
                        if (is_local == 1) {
                            upvalue.* = self.captureUpvalue(&self.frame[0].slots[index]) catch {
                                self.runtimeError("Memory allocation failed.", .{});
                                return InterpretError.Runtime;
                            };
                        } else {
                            std.debug.assert(is_local == 0);
                            upvalue.* = self.frame[0].closure.upvalues[index];
                        }
                    }
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
                    self.push(self.frame[0].slots[slot]);
                },
                .SetLocal => {
                    const slot = self.read_byte();
                    self.frame[0].slots[slot] = self.peek(0);
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
                .GetUpvalue => {
                    const slot = self.read_byte();
                    self.push(self.frame[0].closure.upvalues[slot].?.location.*);
                },
                .SetUpvalue => {
                    const slot = self.read_byte();
                    self.frame[0].closure.upvalues[slot].?.location.* = self.peek(0);
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
        self.push(.{ .obj = result.asObj() });
    }

    fn callValue(self: *Self, callee: Value, arg_count: u8) bool {
        switch (callee) {
            .obj => |obj| {
                switch (obj.type) {
                    .Closure => return self.call(obj.asClosure(), arg_count),
                    .Native => {
                        const native = obj.asNative();
                        const result = native.function((self.stack_top - arg_count)[0..arg_count]) catch {
                            self.runtimeError("Native function returned an error.", .{});
                            return false;
                        };
                        self.stack_top -= arg_count + 1;
                        self.push(result);
                        return true;
                    },
                    else => {},
                }
            },
            else => {},
        }
        self.runtimeError("Can only call functions and classes.", .{});
        return false;
    }

    fn call(self: *Self, closure: *ObjClosure, arg_count: u8) bool {
        if (arg_count != closure.function.arity) {
            self.runtimeError("Expected {d} arguments but got {d}.", .{ closure.function.arity, arg_count });
            return false;
        }
        if (self.frame_count == FRAMES_MAX) {
            self.runtimeError("Stack overflow.", .{});
            return false;
        }
        var frame = &self.vm.frames[self.frame_count];
        self.frame_count += 1;
        frame.closure = closure;
        frame.ip = closure.function.chunk.code.ptr;
        frame.slots = self.stack_top - arg_count - 1;
        return true;
    }

    fn captureUpvalue(self: *Self, local: *Value) InterpretError!*ObjUpvalue {
        var prev_upvalue: ?*ObjUpvalue = null;
        var upvalue = self.vm.open_upvalues;
        while (upvalue) |u| {
            if (u.location == local) {
                return u;
            }
            if (@ptrToInt(u.location) < @ptrToInt(local)) {
                break;
            }
            prev_upvalue = u;
            upvalue = u.next;
        }
        const created_upvalue = try self.vm.gc.newUpvalue(local);
        created_upvalue.next = upvalue;
        if (prev_upvalue) |p| {
            p.next = created_upvalue;
        } else {
            self.vm.open_upvalues = created_upvalue;
        }
        return created_upvalue;
    }

    fn closeUpvalues(self: *Self, last: [*]Value) void {
        while (self.vm.open_upvalues) |upvalue| {
            if (@ptrToInt(upvalue.location) < @ptrToInt(last)) {
                break;
            }
            upvalue.closed = upvalue.location.*;
            upvalue.location = &upvalue.closed;
            self.vm.open_upvalues = upvalue.next;
        }
    }
};

fn clockNative(values: []Value) RuntimeError!Value {
    _ = values;
    const T = struct {
        var timer: ?std.time.Timer = null;
    };
    if (T.timer) |timer| {
        return Value{ .number = @intToFloat(f64, timer.read()) * 1.0e-9 };
    } else {
        T.timer = std.time.Timer.start() catch {
            return RuntimeError.Runtime;
        };
        return Value{ .number = 0.0 };
    }
}
