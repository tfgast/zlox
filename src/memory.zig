const std = @import("std");
const assert = std.debug.assert;

const Allocator = std.mem.Allocator;
const Table = @import("table.zig").Table;
const Chunk = @import("chunk.zig").Chunk;
const Value = @import("value.zig").Value;

const object = @import("object.zig");
const Obj = object.Obj;
const ObjType = object.ObjType;
const ObjString = object.ObjString;
const ObjFunction = object.ObjFunction;
const ObjClosure = object.ObjClosure;
const ObjUpvalue = object.ObjUpvalue;
const ObjNative = object.ObjNative;
const NativeFn = object.NativeFn;

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
    strings: Table,
    objects: ?*Obj,

    pub fn init(allocator: Allocator) !*Self {
        var self = try allocator.create(Self);
        self.allocator = Allocator.init(self, Self.allocFn, Self.resizeFn, Self.freeFn);
        self.inner_allocator = allocator;
        self.objects = null;
        self.strings = Table.init(self);
        return self;
    }

    pub fn free(self: *Self) void {
        self.strings.free();
        self.freeObjects();
        self.inner_allocator.destroy(self);
    }

    fn allocateObject(self: *Self, comptime T: type, comptime ty: ObjType) !*T {
        switch (ty) {
            .String => {
                comptime {
                    assert(T == ObjString);
                }
            },
            .Function => {
                comptime {
                    assert(T == ObjFunction);
                }
            },
            .Closure => {
                comptime {
                    assert(T == ObjClosure);
                }
            },
            .Native => {
                comptime {
                    assert(T == ObjNative);
                }
            },
            .Upvalue => {
                comptime {
                    assert(T == ObjUpvalue);
                }
            },
        }
        var o = try self.allocator.create(T);
        o.obj.type = ty;
        o.obj.next = self.objects;
        self.objects = o.asObj();
        return o;
    }

    pub fn newFunction(self: *Self) !*ObjFunction {
        const function = try self.allocateObject(ObjFunction, .Function);
        function.arity = 0;
        function.upvalue_count = 0;
        function.chunk = Chunk.init(self.allocator);
        function.name = null;
        return function;
    }

    pub fn newNative(self: *Self, function: NativeFn) !*ObjNative {
        const native = try self.allocateObject(ObjNative, .Native);
        native.function = function;
        return native;
    }

    pub fn newClosure(self: *Self, function: *ObjFunction) !*ObjClosure {
        const upvalues = try self.allocator.alloc(?*ObjUpvalue, function.upvalue_count);
        errdefer self.allocator.free(upvalues);
        std.mem.set(?*ObjUpvalue, upvalues, null);

        const closure = try self.allocateObject(ObjClosure, .Closure);
        closure.function = function;
        closure.upvalues = upvalues;
        return closure;
    }

    pub fn newUpvalue(self: *Self, slot: *Value) !*ObjUpvalue {
        const upvalue = try self.allocateObject(ObjUpvalue, .Upvalue);
        upvalue.location = slot;
        return upvalue;
    }

    pub fn copyString(self: *Self, chars: []const u8) !*ObjString {
        const hash = hashString(chars);
        if (self.strings.findString(chars, hash)) |interned| {
            return interned;
        }
        const owned_chars = try self.allocator.dupe(u8, chars);
        errdefer {
            self.allocator.free(owned_chars);
        }
        const ret = try self.allocateString(owned_chars, hash);
        return ret;
    }
    pub fn takeString(self: *Self, owned_chars: []u8) !*ObjString {
        const hash = hashString(owned_chars);
        if (self.strings.findString(owned_chars, hash)) |interned| {
            self.allocator.free(owned_chars);
            return interned;
        }
        return self.allocateString(owned_chars, hash);
    }
    fn allocateString(self: *Self, owned_chars: []u8, hash: u32) !*ObjString {
        const string = try self.allocateObject(ObjString, .String);
        errdefer {
            self.freeString(string);
        }
        string.str = owned_chars;
        string.hash = hash;
        _ = try self.strings.set(string, .nil);
        return string;
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
                self.freeString(obj.asString());
            },
            .Function => {
                self.freeFunction(obj.asFunction());
            },
            .Native => {
                self.freeNative(obj.asNative());
            },
            .Closure => {
                self.freeClosure(obj.asClosure());
            },
            .Upvalue => {
                self.freeUpvalue(obj.asUpvalue());
            },
        }
    }

    pub fn freeString(self: *Self, string: *ObjString) void {
        self.allocator.free(string.str);
        self.allocator.destroy(string);
    }

    pub fn freeFunction(self: *Self, function: *ObjFunction) void {
        function.chunk.free();
        self.allocator.destroy(function);
    }

    pub fn freeClosure(self: *Self, closure: *ObjClosure) void {
        self.allocator.free(closure.upvalues);
        self.allocator.destroy(closure);
    }

    pub fn freeNative(self: *Self, native: *ObjNative) void {
        self.allocator.destroy(native);
    }

    pub fn freeUpvalue(self: *Self, upvalue: *ObjUpvalue) void {
        self.allocator.destroy(upvalue);
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

pub fn hashString(key: []const u8) u32 {
    var hash: u32 = 2166136261;
    for (key) |k| {
        hash ^= k;
        hash = hash *% 16777619;
    }
    return hash;
}
