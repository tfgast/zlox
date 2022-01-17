const std = @import("std");

pub const ObjType = enum { String };

pub const Obj = struct {
    const Self = Obj;
    type: ObjType,
    next: ?*Self,

    pub fn asString(self: *Self) *ObjString {
        std.debug.assert(self.type == .String);
        return @ptrCast(*ObjString, self);
    }

    pub fn asStringBytes(self: *Self) []u8 {
        return self.asString().str;
    }

    pub fn print(self: *Self) void {
        switch (self.type) {
            .String => {
                std.debug.print("{s}", .{self.asStringBytes()});
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
