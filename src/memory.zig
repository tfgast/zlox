const std = @import("std");
const assert = std.debug.assert;

const Allocator = std.mem.Allocator;

const object = @import("object.zig");
const Obj = object.Obj;
const ObjType = object.ObjType;
const ObjString = object.ObjString;

pub fn grow_capacity(capacity: usize) usize {
    if (capacity < 8) {
        return 8;
    } else {
        return capacity * 2;
    }
}

pub const GarbageCollector = struct {
    const Self = GarbageCollector;
    allocator: Allocator,
    inner_allocator: Allocator,
    objects: ?*Obj,

    pub fn init(allocator : Allocator) !*Self {
        var self = try allocator.create(Self);
        self.allocator = Allocator.init(self, Self.allocFn, Self.resizeFn, Self.freeFn);
        self.inner_allocator = allocator;
        self.objects = null;
        return self;
    }

    fn allocateObject(self: *Self, comptime T: type, comptime ty: ObjType) !*T {
       switch (ty) {
            .String => {
                comptime {
                    assert(T == ObjString);
                }
                var string = try self.allocator.create(ObjString);
                string.obj.type = .String;
                string.obj.next = self.objects;
                self.objects = string.toObj();
                return string;
            }
       }
    }

    pub fn copyString(self: *Self, chars: []const u8) !*ObjString {
        const owned_chars = try self.allocator.dupe(u8, chars); 
        errdefer {
            self.allocator.free(owned_chars);
        }
        const ret = try self.allocateString(owned_chars);
        return ret;
    }
    pub fn takeString(self: *Self, owned_chars: []u8) !*ObjString {
        return self.allocateString(owned_chars);
    }
    fn allocateString(self: *Self, owned_chars: []u8) !*ObjString {
        const string = try self.allocateObject(ObjString, .String);
        string.str = owned_chars;
        return string;
    }

    pub fn free(self: *Self) void {
        self.freeObjects();
        self.inner_allocator.destroy(self);
    }

    pub fn freeObjects(self: *Self) void {
        var optional_obj = self.objects;
        while (true) {
            const obj = optional_obj orelse break;
            optional_obj = obj.next;
            self.freeObject(obj);
        }
    }

    pub fn freeObject(self: *Self, obj: *Obj) void {
       switch (obj.type) {
        .String => {
            const string = obj.asString();
            self.allocator.free(string.str);
            self.allocator.destroy(string);
        }
       }
    }

    fn allocFn(self: *Self, len: usize, ptr_align: u29, len_align: u29, ret_addr: usize) Allocator.Error![]u8 {
        return self.inner_allocator.rawAlloc(len, ptr_align, len_align, ret_addr);
    }
    fn resizeFn(self: *Self, buf: []u8, buf_align: u29, new_len: usize, len_align: u29, ret_addr: usize) ?usize {
        return self.inner_allocator.rawResize(buf, buf_align, new_len, len_align, ret_addr);
    }
    fn freeFn(self: *Self, buf: []u8, buf_align: u29, ret_addr: usize) void {
        return self.inner_allocator.rawFree(buf, buf_align, ret_addr);
    }
};
