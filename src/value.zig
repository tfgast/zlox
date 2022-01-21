const std = @import("std");
const memory = @import("memory.zig");
const GarbageCollector = memory.GarbageCollector;

const object = @import("object.zig");
const Obj = object.Obj;
const ToType = object.ToType;
const ObjType = object.ObjType;
const ObjString = object.ObjString;
const ObjClosure = object.ObjClosure;
const ObjFunction = object.ObjFunction;
const ObjNative = object.ObjNative;
const ObjUpvalue = object.ObjUpvalue;
const ObjInstance = object.ObjInstance;
const ObjClass = object.ObjClass;
const ObjBoundMethod = object.ObjBoundMethod;

pub const ValueType = enum { boolean, nil, number, obj };

pub const Value = union(ValueType) {
    boolean: bool,
    nil: void,
    number: f64,
    obj: *Obj,

    pub fn boolean(b: bool) Value {
        return .{ .boolean = b };
    }

    pub fn number(n: f64) Value {
        return .{ .number = n };
    }

    pub fn obj(o: *Obj) Value {
        return .{ .obj = o };
    }

    pub fn nil() Value {
        return .nil;
    }

    pub fn isNil(v: Value) bool {
        return v == .nil;
    }

    pub fn asObj(v: Value) ?*Obj {
        return switch (v) {
            .obj => |obj| return obj,
            else => return null,
        };
    }

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

    pub fn as(self: Value, comptime ty: ObjType) ?*ToType(ty) {
        return switch (self) {
            .obj => |obj| obj.as(ty),
            else => null,
        };
    }

    pub fn isObjType(self: Value, ty: ObjType) bool {
        return switch (self) {
            .obj => |obj| obj.type == ty,
            else => false,
        };
    }

    pub fn asStringBytes(self: Value) []u8 {
        return self.as(.String).?.str;
    }

    pub fn format(
        self: Value,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .boolean => |boolean| try writer.print("{}", .{boolean}),
            .nil => try writer.print("nil", .{}),
            .number => |number| try writer.print("{d}", .{number}),
            .obj => |obj| {
                switch (obj.type) {
                    .String => {
                        try writer.print("{s}", .{obj.as(.String).?});
                    },
                    .Class => {
                        try writer.print("{s}", .{obj.as(.Class).?});
                    },
                    .Instance => {
                        try writer.print("{s}", .{obj.as(.Instance).?});
                    },
                    .Function => {
                        try writer.print("{s}", .{obj.as(.Function).?});
                    },
                    .Closure => {
                        try writer.print("{s}", .{obj.as(.Closure).?});
                    },
                    .Native => {
                        try writer.print("{s}", .{obj.as(.Native).?});
                    },
                    .Upvalue => {
                        try writer.print("{s}", .{obj.as(.Upvalue).?});
                    },
                    .BoundMethod => {
                        try writer.print("{s}", .{obj.as(.BoundMethod).?});
                    },
                }
            },
        }
    }
};

pub const Entry = struct {
    key: ?*ObjString,
    value: Value,
};

pub const Array = struct {
    values: []Value,
    capacity: usize,
    gc: *GarbageCollector,

    pub fn init(gc: *GarbageCollector) Array {
        return Array{
            .values = &[_]Value{},
            .capacity = 0,
            .gc = gc,
        };
    }

    pub fn write(self: *Array, byte: Value) !void {
        if (self.capacity < self.values.len + 1) {
            const new_capacity = memory.grow_capacity(self.capacity);
            const new_memory = try self.gc.allocator.reallocAtLeast(self.values.ptr[0..self.capacity], new_capacity);
            self.values.ptr = new_memory.ptr;
            self.capacity = new_memory.len;
        }
        const n = self.values.len;
        self.values.len += 1;
        self.values[n] = byte;
    }

    pub fn free(self: *Array) void {
        self.gc.allocator.free(self.values.ptr[0..self.capacity]);
        self.values.len = 0;
        self.capacity = 0;
    }
};
