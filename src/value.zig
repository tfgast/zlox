const std = @import("std");
const memory = @import("memory.zig");
const GarbageCollector = memory.GarbageCollector;

const object = @import("object.zig");
const Obj = object.Obj;
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

    pub fn isClass(self: Value) bool {
        return self.isObjType(.Class);
    }

    pub fn isInstance(self: Value) bool {
        return self.isObjType(.Instance);
    }

    pub fn isBoundMethod(self: Value) bool {
        return self.isObjType(.BoundMethod);
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

    pub fn asClass(self: Value) *ObjClass {
        std.debug.assert(self.isClass());
        return self.obj.asClass();
    }

    pub fn asInstance(self: Value) *ObjInstance {
        std.debug.assert(self.isInstance());
        return self.obj.asInstance();
    }

    pub fn asBoundMethod(self: Value) *ObjBoundMethod {
        std.debug.assert(self.isBoundMethod());
        return self.obj.asBoundMethod();
    }

    pub fn asStringBytes(self: Value) []u8 {
        return self.asString().str;
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
                        try writer.print("{s}", .{obj.asString()});
                    },
                    .Class => {
                        try writer.print("{s}", .{obj.asClass()});
                    },
                    .Instance => {
                        try writer.print("{s}", .{obj.asInstance()});
                    },
                    .Function => {
                        try writer.print("{s}", .{obj.asFunction()});
                    },
                    .Closure => {
                        try writer.print("{s}", .{obj.asClosure()});
                    },
                    .Native => {
                        try writer.print("{s}", .{obj.asNative()});
                    },
                    .Upvalue => {
                        try writer.print("{s}", .{obj.asUpvalue()});
                    },
                    .BoundMethod => {
                        try writer.print("{s}", .{obj.asBoundMethod()});
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
