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

const NAN_BOXING: bool = false;
// const NAN_BOXING: bool = true;

const QNAN: u64 = 0x7ffc000000000000;
const TAG_NIL: u64 = 1;
const TAG_FALSE: u64 = 2;
const TAG_TRUE: u64 = 3;
const SIGN_BIT: u64 = 0x8000000000000000;

pub const Value = Val(NAN_BOXING);

fn Val(comptime NanBoxing: bool) type {
    if (NanBoxing) {
        return struct {
            const Self = @This();
            bits: u64,
            pub fn number(n: f64) Self {
                return .{ .bits = @bitCast(u64, n) };
            }

            pub fn asNumber(v: Self) ?f64 {
                return if (v.isNumber()) @bitCast(f64, v.bits) else null;
            }

            pub fn isNumber(v: Self) bool {
                return (v.bits & QNAN) != QNAN;
            }

            pub fn nil() Self {
                return .{ .bits = QNAN | TAG_NIL };
            }

            pub fn isNil(v: Self) bool {
                return v.bits == QNAN | TAG_NIL;
            }

            pub fn boolean(b: bool) Self {
                return .{ .bits = QNAN | if (b) TAG_TRUE else TAG_FALSE };
            }

            pub fn asBoolean(v: Self) ?bool {
                return if (v.isBoolean()) v.bits == QNAN | TAG_TRUE else null;
            }

            pub fn isBoolean(v: Self) bool {
                return (v.bits | 1) == QNAN | TAG_TRUE;
            }

            pub fn obj(o: *Obj) Self {
                return .{ .bits = SIGN_BIT | QNAN | @ptrToInt(o) };
            }

            pub fn asObj(v: Self) ?*Obj {
                return if (v.isObj()) @intToPtr(*Obj, v.bits & ~(SIGN_BIT | QNAN)) else null;
            }

            pub fn isObj(v: Self) bool {
                return (v.bits & (SIGN_BIT | QNAN)) == (SIGN_BIT | QNAN);
            }

            pub fn isFalsey(self: Self) bool {
                return self.isNil() or (!(self.asBoolean() orelse true));
            }

            pub fn equal(self: Self, other: Self) bool {
                if (self.asNumber()) |a| {
                    if (other.asNumber()) |b| {
                        return a == b;
                    }
                }
                return self.bits == other.bits;
            }

            pub fn as(self: Self, comptime ty: ObjType) ?*ToType(ty) {
                const o = self.asObj() orelse return null;
                return o.as(ty);
            }

            pub fn isObjType(self: Self, ty: ObjType) bool {
                const o = self.asObj() orelse return null;
                return o.type == ty;
            }

            pub fn asStringBytes(self: Self) []u8 {
                return self.as(.String).?.str;
            }

            pub fn format(
                self: Self,
                comptime fmt: []const u8,
                options: std.fmt.FormatOptions,
                writer: anytype,
            ) !void {
                _ = fmt;
                _ = options;
                if (self.isNil()) {
                    try writer.print("nil", .{});
                } else if (self.asBoolean()) |b| {
                    try writer.print("{}", .{b});
                } else if (self.asNumber()) |n| {
                    try writer.print("{d}", .{n});
                } else if (self.asObj()) |o| {
                    switch (o.type) {
                        .String => {
                            try writer.print("{s}", .{o.as(.String).?});
                        },
                        .Class => {
                            try writer.print("{s}", .{o.as(.Class).?});
                        },
                        .Instance => {
                            try writer.print("{s}", .{o.as(.Instance).?});
                        },
                        .Function => {
                            try writer.print("{s}", .{o.as(.Function).?});
                        },
                        .Closure => {
                            try writer.print("{s}", .{o.as(.Closure).?});
                        },
                        .Native => {
                            try writer.print("{s}", .{o.as(.Native).?});
                        },
                        .Upvalue => {
                            try writer.print("{s}", .{o.as(.Upvalue).?});
                        },
                        .BoundMethod => {
                            try writer.print("{s}", .{o.as(.BoundMethod).?});
                        },
                    }
                }
            }
        };
    } else {
        return union(ValueType) {
            const Self = @This();
            boolean: bool,
            nil: void,
            number: f64,
            obj: *Obj,

            pub fn boolean(b: bool) Self {
                return .{ .boolean = b };
            }

            pub fn number(n: f64) Self {
                return .{ .number = n };
            }

            pub fn asNumber(v: Self) ?f64 {
                return switch (v) {
                    .number => |number| return number,
                    else => return null,
                };
            }

            pub fn isNumber(self: Self) bool {
                return self == .number;
            }

            pub fn obj(o: *Obj) Self {
                return .{ .obj = o };
            }

            pub fn nil() Self {
                return .nil;
            }

            pub fn isNil(v: Self) bool {
                return v == .nil;
            }

            pub fn asObj(v: Self) ?*Obj {
                return switch (v) {
                    .obj => |obj| return obj,
                    else => return null,
                };
            }

            pub fn isFalsey(self: Self) bool {
                return switch (self) {
                    .boolean => |boolean| !boolean,
                    .nil => true,
                    else => false,
                };
            }

            pub fn equal(self: Self, other: Self) bool {
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

            pub fn as(self: Self, comptime ty: ObjType) ?*ToType(ty) {
                return switch (self) {
                    .obj => |obj| obj.as(ty),
                    else => null,
                };
            }

            pub fn isObjType(self: Self, ty: ObjType) bool {
                return switch (self) {
                    .obj => |obj| obj.type == ty,
                    else => false,
                };
            }

            pub fn asStringBytes(self: Self) []u8 {
                return self.as(.String).?.str;
            }

            pub fn format(
                self: Self,
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
    }
}

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
