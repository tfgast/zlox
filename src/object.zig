const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;

pub const ObjType = enum { String, Function };

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
