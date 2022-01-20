const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const Value = @import("value.zig").Value;
const RuntimeError = @import("vm.zig").RuntimeError;

pub const ObjType = enum { String, Closure, Function, Native, Upvalue };

pub const Obj = struct {
    const Self = Obj;
    type: ObjType,
    next: ?*Self,
    is_marked: bool,

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

    pub fn asClosure(self: *Self) *ObjClosure {
        std.debug.assert(self.type == .Closure);
        return @ptrCast(*ObjClosure, self);
    }

    pub fn asUpvalue(self: *Self) *ObjUpvalue {
        std.debug.assert(self.type == .Upvalue);
        return @ptrCast(*ObjUpvalue, self);
    }

    pub fn asStringBytes(self: *Self) []u8 {
        return self.asString().str;
    }

    pub fn toStr(self: *Self) []const u8 {
        switch (self.type) {
            .String => {
                return self.asStringBytes();
            },
            .Function => {
                return self.asFunction().toStr();
            },
            .Closure => {
                return self.asClosure().function.toStr();
            },
            .Native => {
                return "native fn";
            },
            .Upvalue => {
                return "upvalue";
            },
        }
    }

    pub fn print(self: *Self) void {
        switch (self.type) {
            .String => {
                std.debug.print("{s}", .{self.asStringBytes()});
            },
            .Function => {
                self.asFunction().print();
            },
            .Closure => {
                self.asClosure().function.print();
            },
            .Native => {
                std.debug.print("<native fn>", .{});
            },
            .Upvalue => {
                std.debug.print("upvalue", .{});
            },
        }
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

    pub fn print(self: *Self) void {
        if (self.name) |name| {
            std.debug.print("<fn {s}>", .{name.str});
        } else {
            std.debug.print("<script>", .{});
        }
    }
    pub fn toStr(self: *Self) []const u8 {
        if (self.name) |name| {
            return name.str;
        } else {
            return "<script>";
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
};

pub const ObjClosure = struct {
    const Self = ObjClosure;

    obj: Obj,
    function: *ObjFunction,
    upvalues: []?*ObjUpvalue,

    pub fn asObj(self: *Self) *Obj {
        return &self.obj;
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
};
