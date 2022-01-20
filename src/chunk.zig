const std = @import("std");
const memory = @import("memory.zig");
const value = @import("value.zig");

const GarbageCollector = memory.GarbageCollector;

const Allocator = std.mem.Allocator;

// Declare an enum.
pub const OpCode =
    enum(u8) { Constant, ConstantLong, Nil, True, False, Pop, GetLocal, SetLocal, GetGlobal, DefineGlobal, SetGlobal, GetUpvalue, SetUpvalue, Equal, Greater, Less, Add, Subtract, Multiply, Divide, Not, Negate, Print, Jump, JumpIfFalse, Loop, Call, Closure, CloseUpvalue, Return, Class, _ };

pub const Chunk = struct {
    constants: value.Array,
    code: []u8,
    lines: LineTracker,
    capacity: usize,
    gc: *GarbageCollector,

    pub fn init(gc: *GarbageCollector) Chunk {
        return Chunk{
            .constants = value.Array.init(gc),
            .code = &[_]u8{},
            .lines = LineTracker.init(gc.allocator),
            .capacity = 0,
            .gc = gc,
        };
    }

    pub fn write(self: *Chunk, byte: u8, line: u32) !void {
        if (self.capacity < self.code.len + 1) {
            const new_capacity = memory.grow_capacity(self.capacity);
            const new_memory = try self.gc.allocator.reallocAtLeast(self.code.ptr[0..self.capacity], new_capacity);
            self.code.ptr = new_memory.ptr;
            self.capacity = new_memory.len;
        }
        const n = self.code.len;
        self.code.len += 1;
        self.code[n] = byte;

        try self.lines.write(line);
    }

    pub fn writeConstant(self: *Chunk, v: value.Value, line: u32) !void {
        const c = try self.addConstant(v);
        if (c < 256) {
            try self.write(@enumToInt(OpCode.Constant), line);
            try self.write(@intCast(u8, c), line);
        } else {
            try self.write(@enumToInt(OpCode.ConstantLong), line);
            try self.write(@intCast(u8, c & 0xff), line);
            try self.write(@intCast(u8, (c >> 8) & 0xff), line);
            try self.write(@intCast(u8, (c >> 16) & 0xff), line);
        }
    }

    pub fn getLine(self: *Chunk, offset: usize) u32 {
        return self.lines.getLine(offset);
    }

    pub fn addConstant(self: *Chunk, v: value.Value) !usize {
        self.gc.vm.push(v);
        defer _ = self.gc.vm.pop();
        try self.constants.write(v);
        return self.constants.values.len - 1;
    }

    pub fn free(self: *Chunk) void {
        self.constants.free();
        self.gc.allocator.free(self.code.ptr[0..self.capacity]);
        self.lines.free();
        self.code.len = 0;
        self.capacity = 0;
    }
};

const Line = struct {
    number: u32,
    offset: u32,
};

const LineTracker = struct {
    values: []Line,
    capacity: usize,
    allocator: Allocator,

    pub fn init(allocator: Allocator) LineTracker {
        return LineTracker{
            .values = &[_]Line{},
            .capacity = 0,
            .allocator = allocator,
        };
    }

    pub fn write(self: *LineTracker, line: u32) !void {
        var last_offset: u32 = 0;
        if (self.values.len > 0) {
            const last_line = self.values[self.values.len - 1];
            last_offset = last_line.offset;
            if (last_line.number == line) {
                self.values[self.values.len - 1].offset += 1;
                return;
            }
        }
        if (self.capacity < self.values.len + 1) {
            const new_capacity = memory.grow_capacity(self.capacity);
            const new_memory = try self.allocator.reallocAtLeast(self.values.ptr[0..self.capacity], new_capacity);
            self.values.ptr = new_memory.ptr;
            self.capacity = new_memory.len;
        }
        const n = self.values.len;
        self.values.len += 1;
        self.values[n] = Line{ .number = line, .offset = last_offset + 1 };
    }

    pub fn getLine(self: *LineTracker, offset: usize) u32 {
        // could also do binary search
        for (self.values) |line| {
            if (offset < line.offset) {
                return line.number;
            }
        }
        return 0;
    }

    pub fn free(self: *LineTracker) void {
        self.allocator.free(self.values.ptr[0..self.capacity]);
        self.values.len = 0;
        self.capacity = 0;
    }
};
