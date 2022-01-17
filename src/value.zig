const std = @import("std");
const memory = @import("memory.zig");
const Allocator = std.mem.Allocator;

pub const ValueType = enum {
    boolean, nil, number
};

pub const Value = union(ValueType) {
    boolean: bool,
    nil: void,
    number: f64,

    pub fn isFalsey(self: Value) bool {
        return switch (self) {
            .boolean => |boolean| boolean,
            .nil => false,
            .number => true,
        };
    }

    pub fn equal(self: Value, other: Value) bool {
        const ty: ValueType = self;
        if (ty != other) {
            return false;
        }
        return switch (self) {
            .boolean => |boolean| boolean == other.boolean,
            .nil => true,
            .number => |number| number == other.number,
        };
    }
};

pub fn print(value: Value) void {
    switch (value) {
        .boolean => |boolean| std.debug.print("{}", .{boolean}),
        .nil => std.debug.print("nil", .{}),
        .number => |number| std.debug.print("{d}", .{number}),
    }
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
