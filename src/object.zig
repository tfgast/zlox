const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const Value = @import("value.zig").Value;
const RuntimeError = @import("vm.zig").RuntimeError;

pub const ObjType = enum { String, Function, Native };

pub const Obj = struct {
    const Self = Obj;
    type: ObjType,
    next: ?*Self,

    pub fn asString(self: *Self) *ObjString {
        std.debug.assert(self.type == .String);
        return @ptrCast(*ObjString, self);
    }

    pub fn asFunction(self: *Self) *ObjFunction {
        std.debug.assert(self.type == .Function);
        return @ptrCast(*ObjFunction, self);
    }

    pub fn asNative(self: *Self) *ObjNative {
        std.debug.assert(self.type == .Native);
        return @ptrCast(*ObjNative, self);
    }

    pub fn asStringBytes(self: *Self) []u8 {
        return self.asString().str;
    }

    pub fn print(self: *Self) void {
        switch (self.type) {
            .String => {
                std.debug.print("{s}", .{self.asStringBytes()});
            },
            .Function => {
                self.asFunction().print();
            },
            .Native => {
                std.debug.print("<native fn>", .{});
            },
        }
    }
};

pub const ObjString = struct {
    const Self = ObjString;

    obj: Obj,
    str: []u8,
    hash: u32,

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
    }
};

pub const ObjFunction = struct {
    const Self = ObjFunction;

    obj: Obj,
    arity: u8,
    chunk: Chunk,
    name: ?*ObjString,

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn print(self: *Self) void {
        if (self.name) |name| {
            std.debug.print("<fn {s}>", .{name.str});
        } else {
            std.debug.print("<script>", .{});
        }
    }
};

pub const NativeFn = fn ([]Value) RuntimeError!Value;

pub const ObjNative = struct {
    const Self = ObjNative;

    obj: Obj,
    function: NativeFn,

    pub fn toObj(self: *Self) *Obj {
        return &self.obj;
    }
};
