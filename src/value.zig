const std = @import("std");
const memory = @import("memory.zig");
const Allocator = std.mem.Allocator;

const object = @import("object.zig");
const Obj = object.Obj;
const ObjType = object.ObjType;
const ObjString = object.ObjString;
const ObjClosure = object.ObjClosure;
const ObjFunction = object.ObjFunction;
const ObjNative = object.ObjNative;
const ObjUpvalue = object.ObjUpvalue;

pub const ValueType = enum { boolean, nil, number, obj };

pub const Value = union(ValueType) {
    boolean: bool,
    nil: void,
    number: f64,
    obj: *Obj,

    pub fn isFalsey(self: Value) bool {
        return switch (self) {
            .boolean => |boolean| !boolean,
            .nil => true,
            else => false,
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
            .obj => |obj| obj == other.obj,
        };
    }

    pub fn isObjType(self: Value, ty: ObjType) bool {
        return switch (self) {
            .obj => |obj| obj.type == ty,
            else => false,
        };
    }

    pub fn isString(self: Value) bool {
        return self.isObjType(.String);
    }

    pub fn isClosure(self: Value) bool {
        return self.isObjType(.Closure);
    }

    pub fn isFunction(self: Value) bool {
        return self.isObjType(.Function);
    }

    pub fn isNative(self: Value) bool {
        return self.isObjType(.Native);
    }

    pub fn isUpvalue(self: Value) bool {
        return self.isObjType(.Upvalue);
    }

    pub fn asString(self: Value) *ObjString {
        std.debug.assert(self.isString());
        return self.obj.asString();
    }

    pub fn asClosure(self: Value) *ObjClosure {
        std.debug.assert(self.isClosure());
        return self.obj.asClosure();
    }

    pub fn asFunction(self: Value) *ObjFunction {
        std.debug.assert(self.isFunction());
        return self.obj.asFunction();
    }

    pub fn asNative(self: Value) *ObjNative {
        std.debug.assert(self.isNative());
        return self.obj.asNative();
    }

    pub fn asUpvalue(self: Value) *ObjUpvalue {
        std.debug.assert(self.isUpvalue());
        return self.obj.asUpvalue();
    }

    pub fn asStringBytes(self: Value) []u8 {
        return self.asString().str;
    }
};

pub fn print(value: Value) void {
    switch (value) {
        .boolean => |boolean| std.debug.print("{}", .{boolean}),
        .nil => std.debug.print("nil", .{}),
        .number => |number| std.debug.print("{d}", .{number}),
        .obj => |obj| obj.print(),
    }
}

pub const Entry = struct {
    key: ?*ObjString,
    value: Value,
};

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
