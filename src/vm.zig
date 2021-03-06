const std = @import("std");
const root = @import("root");
const File = std.fs.File;
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
const ObjClass = object.ObjClass;
const ObjBoundMethod = object.ObjBoundMethod;
const ObjUpvalue = object.ObjUpvalue;
const NativeFn = object.NativeFn;

const Compiler = @import("compiler.zig").Compiler;
const CompileError = @import("compiler.zig").CompileError;
const GarbageCollector = @import("memory.zig").GarbageCollector;
const Allocator = std.mem.Allocator;

pub const RuntimeError = error{Runtime};
pub const InterpretError = RuntimeError || CompileError || std.os.WriteError;

const DEBUG_TRACE_EXECUTION = @hasDecl(root, "DEBUG_TRACE_EXECUTION") and root.DEBUG_TRACE_EXECUTION;
const DEBUG_LIMIT_EXECUTION = if (@hasDecl(root, "DEBUG_LIMIT_EXECUTION")) root.DEBUG_LIMIT_EXECUTION else -1;
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
    stack: [STACK_MAX]Value,
    frames: [FRAMES_MAX]CallFrame,
    stack_top: [*]Value,
    frame: [*]CallFrame,
    frame_count: usize,
    init_string: ?*ObjString,
    output: File,

    pub fn init(allocator: Allocator, output: File) !*Self {
        var vm = try allocator.create(Self);
        const gc = try GarbageCollector.init(allocator, vm);
        vm.output = output;
        vm.gc = gc;
        vm.compiler = Compiler.init(gc);
        vm.globals = Table.init(gc);
        vm.open_upvalues = null;
        vm.stack_top = &vm.stack;
        vm.frame = &vm.frames;
        vm.frame_count = 0;
        vm.init_string = null;

        vm.init_string = try gc.copyString("init");

        _ = try clockNative(&[_]Value{});
        try vm.defineNative("clock", clockNative);
        return vm;
    }

    pub fn free(self: *Self) void {
        self.init_string = null;
        self.globals.free();
        self.gc.free();
    }

    pub fn interpret(self: *Self, source: []u8) InterpretError!void {
        if (DEBUG_TRACE_EXECUTION) {
            std.debug.print("== Compiling ==\n", .{});
        }
        const function = try self.compiler.compile(source);
        self.push(function.val());
        const closure = try self.gc.newClosure(function);
        _ = self.pop();
        self.push(closure.val());
        _ = self.call(closure, 0);
        if (DEBUG_TRACE_EXECUTION) {
            std.debug.print("== Running ==\n", .{});
        }
        try self.run();
    }

    fn resetStack(self: *Self) void {
        self.stack_top = &self.stack;
        self.frame_count = 0;
    }

    fn runtimeError(self: *Self, comptime format: []const u8, args: anytype) void {
        std.debug.print(format, args);
        std.debug.print("\n", .{});
        var i = self.frame_count;
        while (i > 0) {
            i -= 1;
            const frame = &self.frames[i];
            const function = frame.closure.function;
            const offset = @ptrToInt(frame.ip) - @ptrToInt(function.chunk.code.ptr) - 1;
            const line = frame.closure.function.chunk.getLine(offset);
            std.debug.print("[line {d}] in {} \n", .{ line, function });
        }
        self.resetStack();
    }

    fn defineNative(self: *Self, name: []const u8, function: NativeFn) InterpretError!void {
        const string = try self.gc.copyString(name);
        self.push(string.val());
        const native = try self.gc.newNative(function);
        self.push(native.val());
        _ = try self.globals.set(self.peek(1).as(.String).?, self.peek(0));
        _ = self.pop();
        _ = self.pop();
    }

    pub fn push(self: *Self, v: Value) void {
        self.stack_top[0] = v;
        self.stack_top += 1;
    }

    pub fn pop(self: *Self) Value {
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
        return self.read_constant().as(.String).?;
    }

    fn read_constant_long(self: *Self) Value {
        const b0 = @intCast(u32, self.read_byte());
        const b1 = @intCast(u32, self.read_byte());
        const b2 = @intCast(u32, self.read_byte());
        return self.frame[0].closure.function.chunk.constants.values[(b2 << 16) | (b1 << 8) | b0];
    }

    fn run(self: *Self) InterpretError!void {
        var base_frame: [*]CallFrame = &self.frames;
        self.frame = base_frame + (self.frame_count - 1);
        while (true) {
            if (DEBUG_TRACE_EXECUTION) {
                std.debug.print("          ", .{});
                const n = (@ptrToInt(self.stack_top) - @ptrToInt(&self.stack)) / @sizeOf(Value);
                var slice = self.stack[0..n];
                for (slice) |v| {
                    std.debug.print("[ {s} ]", .{v});
                }
                std.debug.print("\n", .{});
                _ = debug.disassembleInstruction(&self.frame[0].closure.function.chunk, @ptrToInt(self.frame[0].ip) - @ptrToInt(self.frame[0].closure.function.chunk.code.ptr));
            }
            if (DEBUG_LIMIT_EXECUTION >= 0) {
                const Execution = struct {
                    var limit: i64 = 0;
                };
                Execution.limit += 1;
                if (Execution.limit > DEBUG_LIMIT_EXECUTION) {
                    self.runtimeError("Execeeded bytecode execution limit {d}.", .{DEBUG_LIMIT_EXECUTION});
                    return InterpretError.Runtime;
                }
            }
            const instruction = self.read_byte();
            switch (@intToEnum(OpCode, instruction)) {
                .Print => {
                    self.output.writer().print("{s}\n", .{self.pop()}) catch {
                        self.runtimeError("Could not write to output.", .{});
                        return RuntimeError.Runtime;
                    };
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
                .Invoke => {
                    const method = self.read_string();
                    const arg_count = self.read_byte();
                    if (!self.invoke(method, arg_count)) {
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
                    const function = self.read_constant().as(.Function).?;
                    const closure = self.gc.newClosure(function) catch {
                        self.runtimeError("Out of Memory.", .{});
                        return InterpretError.Runtime;
                    };
                    self.push(closure.val());
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
                    self.push(Value.nil());
                },
                .True => {
                    self.push(Value.boolean(true));
                },
                .False => {
                    self.push(Value.boolean(false));
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
                    if (self.globals.get(name)) |v| {
                        self.push(v.*);
                    } else {
                        self.runtimeError("Undefined variable '{s}'.", .{name.str});
                        return InterpretError.Runtime;
                    }
                },
                .DefineGlobal => {
                    const name = self.read_string();
                    _ = self.globals.set(name, self.peek(0)) catch {
                        self.runtimeError("Out of Memory.", .{});
                        return InterpretError.Runtime;
                    };
                    _ = self.pop();
                },
                .SetGlobal => {
                    const name = self.read_string();
                    const was_new = self.globals.set(name, self.peek(0)) catch {
                        self.runtimeError("Out of Memory.", .{});
                        return InterpretError.Runtime;
                    };
                    if (was_new) {
                        _ = self.globals.delete(name);
                        self.runtimeError("Undefined variable '{s}'.", .{name});
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
                .GetProperty => {
                    const instance = self.peek(0).as(.Instance) orelse {
                        self.runtimeError("Only instances have properties.", .{});
                        return InterpretError.Runtime;
                    };
                    const name = self.read_string();
                    if (instance.fields.get(name)) |v| {
                        _ = self.pop(); //Instance
                        self.push(v.*);
                    } else if (!self.bindMethod(instance.class, name)) {
                        return InterpretError.Runtime;
                    }
                },
                .SetProperty => {
                    const instance = self.peek(1).as(.Instance) orelse {
                        self.runtimeError("Only instances have fields.", .{});
                        return InterpretError.Runtime;
                    };
                    const name = self.read_string();
                    _ = instance.fields.set(name, self.peek(0)) catch {
                        self.runtimeError("Out of Memory.", .{});
                        return InterpretError.Runtime;
                    };
                    var v = self.pop();
                    _ = self.pop(); //Instance
                    self.push(v);
                },
                .GetSuper => {
                    const superclass = self.pop().as(.Class).?;
                    const name = self.read_string();
                    if (!self.bindMethod(superclass, name)) {
                        return InterpretError.Runtime;
                    }
                },
                .Equal => {
                    const b = self.pop();
                    const a = self.pop();
                    self.push(Value.boolean(a.equal(b)));
                },
                .Greater => {
                    try self.binaryOp(greater);
                },
                .Less => {
                    try self.binaryOp(less);
                },
                .Add => {
                    if (self.peek(0).as(.String) != null and self.peek(1).as(.String) != null) {
                        try self.concatenate();
                    } else if ((self.peek(0).isNumber()) and (self.peek(1).isNumber())) {
                        const rhs = self.pop();
                        const lhs = self.pop();
                        self.push(add(lhs.asNumber().?, rhs.asNumber().?));
                    } else {
                        self.runtimeError("Operands must be two numbers or two strings.", .{});
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
                    self.push(Value.boolean(self.pop().isFalsey()));
                },
                .Negate => {
                    if (!self.peek(0).isNumber()) {
                        self.runtimeError("Operand must be a number.", .{});
                        return InterpretError.Runtime;
                    }
                    self.push(Value.number(-self.pop().asNumber().?));
                },
                .Class => {
                    const class = self.gc.newClass(self.read_string()) catch {
                        self.runtimeError("Out of Memory.", .{});
                        return InterpretError.Runtime;
                    };
                    self.push(class.val());
                },
                .Inherit => {
                    const superclass = self.peek(1).as(.Class) orelse {
                        self.runtimeError("Superclass must be a class.", .{});
                        return InterpretError.Runtime;
                    };
                    const subclass = self.peek(0).as(.Class).?;

                    superclass.methods.addAll(&subclass.methods) catch {
                        self.runtimeError("Out of Memory.", .{});
                        return InterpretError.Runtime;
                    };
                    _ = self.pop();
                },
                .Method => {
                    self.defineMethod(self.read_string()) catch {
                        self.runtimeError("Out of Memory.", .{});
                        return InterpretError.Runtime;
                    };
                },
                _ => {
                    std.debug.print("Unknown opcode {d}", .{instruction});
                },
            }
        }
    }

    fn binaryOp(self: *Self, comptime op: anytype) InterpretError!void {
        if (!self.peek(0).isNumber() or !self.peek(1).isNumber()) {
            self.runtimeError("Operands must be numbers.", .{});
            return InterpretError.Runtime;
        }
        const rhs = self.pop();
        const lhs = self.pop();
        self.push(op(lhs.asNumber().?, rhs.asNumber().?));
    }

    fn concatenate(self: *Self) InterpretError!void {
        const b = self.peek(0).as(.String).?;
        const a = self.peek(1).as(.String).?;
        const chars = std.mem.concat(self.gc.allocator, u8, &[_][]const u8{ a.str, b.str }) catch {
            self.runtimeError("Memory allocation failed.", .{});
            return InterpretError.Runtime;
        };
        errdefer {
            self.gc.allocator.free(chars);
        }
        const result = self.gc.takeString(chars) catch {
            self.runtimeError("Memory allocation failed.", .{});
            return InterpretError.Runtime;
        };
        _ = self.pop();
        _ = self.pop();
        self.push(result.val());
    }

    fn callValue(self: *Self, callee: Value, arg_count: u8) bool {
        if (callee.asObj()) |obj| {
            switch (obj.type) {
                .Closure => return self.call(obj.as(.Closure).?, arg_count),
                .BoundMethod => {
                    const bound = obj.as(.BoundMethod).?;
                    (self.stack_top - arg_count - 1)[0] = bound.receiver;
                    return self.call(bound.method, arg_count);
                },
                .Class => {
                    const class = obj.as(.Class).?;
                    const instance = self.gc.newInstance(class) catch {
                        self.runtimeError("Memory allocation failed.", .{});
                        return false;
                    };
                    (self.stack_top - arg_count - 1)[0] = instance.val();
                    if (class.methods.get(self.init_string.?)) |initializer| {
                        return self.call(initializer.as(.Closure).?, arg_count);
                    } else if (arg_count != 0) {
                        self.runtimeError("Expected 0 arguments but got {d}.", .{arg_count});
                        return false;
                    }
                    return true;
                },
                .Native => {
                    const native = obj.as(.Native).?;
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
        }
        self.runtimeError("Can only call functions and classes.", .{});
        return false;
    }

    fn invoke(self: *Self, name: *ObjString, arg_count: u8) bool {
        const receiver = self.peek(arg_count);
        const instance = receiver.as(.Instance) orelse {
            self.runtimeError("Only instances have methods.", .{});
            return false;
        };

        if (instance.fields.get(name)) |v| {
            (self.stack_top - arg_count - 1)[0] = v.*;
            return self.callValue(v.*, arg_count);
        }

        return self.invokeFromClass(instance.class, name, arg_count);
    }

    fn invokeFromClass(self: *Self, class: *ObjClass, name: *ObjString, arg_count: u8) bool {
        const method = class.methods.get(name) orelse {
            self.runtimeError("Undefined property '{s}'.", .{name});
            return false;
        };
        return self.call(method.as(.Closure).?, arg_count);
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
        var frame = &self.frames[self.frame_count];
        self.frame_count += 1;
        frame.closure = closure;
        frame.ip = closure.function.chunk.code.ptr;
        frame.slots = self.stack_top - arg_count - 1;
        return true;
    }

    fn captureUpvalue(self: *Self, local: *Value) InterpretError!*ObjUpvalue {
        var prev_upvalue: ?*ObjUpvalue = null;
        var upvalue = self.open_upvalues;
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
        const created_upvalue = try self.gc.newUpvalue(local);
        created_upvalue.next = upvalue;
        if (prev_upvalue) |p| {
            p.next = created_upvalue;
        } else {
            self.open_upvalues = created_upvalue;
        }
        return created_upvalue;
    }

    fn closeUpvalues(self: *Self, last: [*]Value) void {
        while (self.open_upvalues) |upvalue| {
            if (@ptrToInt(upvalue.location) < @ptrToInt(last)) {
                break;
            }
            upvalue.closed = upvalue.location.*;
            upvalue.location = &upvalue.closed;
            self.open_upvalues = upvalue.next;
        }
    }

    fn defineMethod(self: *Self, name: *ObjString) !void {
        const method = self.peek(0);
        var class = self.peek(1).as(.Class).?;
        _ = try class.methods.set(name, method);
        _ = self.pop();
    }

    fn bindMethod(self: *Self, class: *ObjClass, name: *ObjString) bool {
        const method = class.methods.get(name) orelse {
            self.runtimeError("Undefined property '{s}'.", .{name});
            return false;
        };
        const bound = self.gc.newBoundMethod(self.peek(0), method.as(.Closure).?) catch {
            self.runtimeError("Out of Memory.", .{});
            return false;
        };
        _ = self.pop();
        self.push(bound.val());
        return true;
    }
};

fn greater(x: f64, y: f64) Value {
    return Value.boolean((x > y));
}

fn less(x: f64, y: f64) Value {
    return Value.boolean((x < y));
}

fn add(x: f64, y: f64) Value {
    return Value.number(x + y);
}

fn sub(x: f64, y: f64) Value {
    return Value.number(x - y);
}

fn mul(x: f64, y: f64) Value {
    return Value.number(x * y);
}

fn div(x: f64, y: f64) Value {
    return Value.number(x / y);
}

const ExecutionContext = struct {
    const Self = ExecutionContext;

    vm: *VM,
};

fn clockNative(values: []Value) RuntimeError!Value {
    _ = values;
    const T = struct {
        var timer: ?std.time.Timer = null;
    };
    if (T.timer) |timer| {
        return Value.number(@intToFloat(f64, timer.read()) * 1.0e-9);
    } else {
        T.timer = std.time.Timer.start() catch {
            return RuntimeError.Runtime;
        };
        return Value.number(0.0);
    }
}
