const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const Table = @import("table.zig").Table;
const Value = @import("value.zig").Value;
const RuntimeError = @import("vm.zig").RuntimeError;

pub const ObjType = enum { Class, Instance, String, Closure, Function, Native, Upvalue, BoundMethod };

pub fn ToType(comptime ty: ObjType) type {
    return switch (ty) {
        .Class => ObjClass,
        .Instance => ObjInstance,
        .String => ObjString,
        .Closure => ObjClosure,
        .Function => ObjFunction,
        .Native => ObjNative,
        .Upvalue => ObjUpvalue,
        .BoundMethod => ObjBoundMethod,
    };
}

pub const Obj = struct {
    const Self = Obj;
    type: ObjType,
    next: ?*Self,
    is_marked: bool,

    pub fn as(self: *Self, comptime ty: ObjType) ?*ToType(ty) {
        return if (self.type == ty) @ptrCast(*ToType(ty), self) else null;
    }

    pub fn asStringBytes(self: *Self) []u8 {
        return self.as(.String).?.str;
    }
};

pub const ObjString = struct {
    const Self = ObjString;

    obj: Obj,
    str: []u8,
    hash: u32,

    pub fn asObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn val(self: *Self) Value {
        return Value.obj(self.asObj());
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("{s}", .{self.str});
    }
};

pub const ObjFunction = struct {
    const Self = ObjFunction;

    obj: Obj,
    arity: u8,
    upvalue_count: u8,
    chunk: Chunk,
    name: ?*ObjString,

    pub fn asObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn val(self: *Self) Value {
        return Value.obj(self.asObj());
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        if (self.name) |name| {
            try writer.print("<fn {s}>", .{name.str});
        } else {
            try writer.writeAll("<script>");
        }
    }
};

pub const NativeFn = fn ([]Value) RuntimeError!Value;

pub const ObjNative = struct {
    const Self = ObjNative;

    obj: Obj,
    function: NativeFn,

    pub fn asObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn val(self: *Self) Value {
        return Value.obj(self.asObj());
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = self;
        _ = fmt;
        _ = options;
        try writer.writeAll("<native fn>");
    }
};

pub const ObjClosure = struct {
    const Self = ObjClosure;

    obj: Obj,
    function: *ObjFunction,
    upvalues: []?*ObjUpvalue,

    pub fn asObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn val(self: *Self) Value {
        return Value.obj(self.asObj());
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("{s}", .{self.function});
    }
};

pub const ObjUpvalue = struct {
    const Self = ObjUpvalue;

    obj: Obj,
    location: *Value,
    closed: Value,
    next: ?*ObjUpvalue,

    pub fn asObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn val(self: *Self) Value {
        return Value.obj(self.asObj());
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = self;
        _ = fmt;
        _ = options;
        try writer.writeAll("upvalue");
    }
};

pub const ObjClass = struct {
    const Self = ObjClass;

    obj: Obj,
    name: *ObjString,
    methods: Table,

    pub fn asObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn val(self: *Self) Value {
        return Value.obj(self.asObj());
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("{s}", .{self.name});
    }
};

pub const ObjInstance = struct {
    const Self = ObjInstance;

    obj: Obj,
    class: *ObjClass,
    fields: Table,

    pub fn asObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn val(self: *Self) Value {
        return Value.obj(self.asObj());
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("{s} instance", .{self.class.name});
    }
};

pub const ObjBoundMethod = struct {
    const Self = ObjBoundMethod;

    obj: Obj,
    receiver: Value,
    method: *ObjClosure,

    pub fn asObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn val(self: *Self) Value {
        return Value.obj(self.asObj());
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("{s}", .{self.method.function});
    }
};
