const std = @import("std");
const memory = @import("memory.zig");
const Allocator = std.mem.Allocator;

pub const Value = f64;

pub fn print(value: Value) void {
    std.debug.print("{d}", .{value});
}

pub const Array = struct {
    values: []Value,
    capacity: usize,
    allocator: Allocator,

    pub fn init(allocator: Allocator) Array {
        return Array{
            .values = &[_]Value{},
            .capacity = 0,
            .allocator = allocator,
        };
    }

    pub fn write(self: *Array, byte: Value) !void {
        if (self.capacity < self.values.len + 1) {
            const new_capacity = memory.grow_capacity(self.capacity);
            const new_memory = try self.allocator.reallocAtLeast(self.values.ptr[0..self.capacity], new_capacity);
            self.values.ptr = new_memory.ptr;
            self.capacity = new_memory.len;
        }
        const n = self.values.len;
        self.values.len += 1;
        self.values[n] = byte;
    }

    pub fn free(self: *Array) void {
        self.allocator.free(self.values.ptr[0..self.capacity]);
        self.values.len = 0;
        self.capacity = 0;
    }
};
