const std = @import("std");
const builtin = @import("builtin");
const assert = std.debug.assert;

const Allocator = std.mem.Allocator;
const Table = @import("table.zig").Table;
const Chunk = @import("chunk.zig").Chunk;
const Value = @import("value.zig").Value;
const Array = @import("value.zig").Array;
const VM = @import("vm.zig").VM;
const Compiler = @import("compiler.zig").Compiler;

const object = @import("object.zig");
const Obj = object.Obj;
const ObjType = object.ObjType;
const ObjString = object.ObjString;
const ObjFunction = object.ObjFunction;
const ObjClosure = object.ObjClosure;
const ObjUpvalue = object.ObjUpvalue;
const ObjNative = object.ObjNative;
const ObjClass = object.ObjClass;
const ObjInstance = object.ObjInstance;
const NativeFn = object.NativeFn;

pub const GrayStack = std.ArrayList(*Obj);

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
    vm: *VM,
    strings: Table,
    objects: ?*Obj,
    gray_stack: GrayStack,
    bytes_allocated: usize,
    next_gc: usize,

    pub fn init(allocator: Allocator, vm: *VM) !*Self {
        var self = try allocator.create(Self);
        self.gray_stack = GrayStack.init(allocator);
        self.allocator = Allocator.init(self, Self.allocFn, Self.resizeFn, Self.freeFn);
        self.vm = vm;
        self.inner_allocator = allocator;
        self.objects = null;
        self.strings = Table.init(self);
        self.bytes_allocated = 0;
        self.next_gc = 1024 * 1024;
        return self;
    }

    pub fn free(self: *Self) void {
        self.strings.free();
        self.freeObjects();
        self.gray_stack.deinit();
        self.allocator.destroy(self.vm);
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
            .Class => {
                comptime {
                    assert(T == ObjClass);
                }
            },
            .Instance => {
                comptime {
                    assert(T == ObjInstance);
                }
            },
        }
        var o = try self.allocator.create(T);
        o.obj.type = ty;
        o.obj.next = self.objects;
        o.obj.is_marked = false;
        self.objects = o.asObj();
        std.log.debug("{*} allocate {d} for {s}", .{ o, @sizeOf(T), @tagName(ty) });
        return o;
    }

    pub fn newFunction(self: *Self) !*ObjFunction {
        const function = try self.allocateObject(ObjFunction, .Function);
        function.arity = 0;
        function.upvalue_count = 0;
        function.chunk = Chunk.init(self);
        function.name = null;
        return function;
    }

    pub fn newClass(self: *Self, name: *ObjString) !*ObjClass {
        const class = try self.allocateObject(ObjClass, .Class);
        class.name = name;
        return class;
    }

    pub fn newInstance(self: *Self, class: *ObjClass) !*ObjInstance {
        const instance = try self.allocateObject(ObjInstance, .Instance);
        instance.class = class;
        instance.fields = Table.init(self);
        return instance;
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
        upvalue.closed = .nil;
        upvalue.next = null;
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
        self.vm.push(.{ .obj = string.asObj() });
        defer _ = self.vm.pop();
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
        std.log.debug("{*} free {s}", .{ obj, @tagName(obj.type) });
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
            .Class => {
                self.freeClass(obj.asClass());
            },
            .Instance => {
                self.freeInstance(obj.asInstance());
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

    pub fn freeClass(self: *Self, class: *ObjClass) void {
        self.allocator.destroy(class);
    }

    pub fn freeInstance(self: *Self, instance: *ObjInstance) void {
        instance.fields.free();
        self.allocator.destroy(instance);
    }

    pub fn collectGarbage(self: *Self) void {
        const before = self.bytes_allocated;
        std.log.debug("-- gc begin", .{});
        self.markRoots();
        self.traceReferences();
        self.strings.removeWhite();
        self.sweep();
        std.log.debug("-- gc end", .{});
        self.next_gc = self.bytes_allocated * 2;
        std.log.debug("   collected {d} bytes (from {d} to {d}) next at {d}", .{ before - self.bytes_allocated, before, self.bytes_allocated, self.next_gc });
    }

    fn markRoots(self: *Self) void {
        var slot: [*]Value = &self.vm.stack;
        while (@ptrToInt(slot) < @ptrToInt(self.vm.stack_top)) : (slot += 1) {
            self.markValue(slot[0]);
        }
        for (self.vm.frames[0..self.vm.frame_count]) |frame| {
            self.markObj(frame.closure.asObj());
        }
        var upvalue = self.vm.open_upvalues;
        while (upvalue) |u| {
            self.markObj(u.asObj());
            upvalue = u.next;
        }
        self.markTable(&self.vm.globals);
        self.markCompilerRoots(&self.vm.compiler);
    }

    fn markCompilerRoots(self: *Self, compiler: *Compiler) void {
        var context = compiler.current;
        while (context) |c| {
            self.markObj(c.obj_function.asObj());
            context = c.enclosing;
        }
    }

    fn markValue(self: *Self, value: Value) void {
        return switch (value) {
            .obj => |obj| self.markObj(obj),
            else => {},
        };
    }

    fn markObj(self: *Self, obj: *Obj) void {
        if (obj.is_marked) return;
        const v = Value{ .obj = obj };
        std.log.debug("{*} mark {s}", .{ obj, v });
        obj.is_marked = true;
        self.gray_stack.append(obj) catch {
            std.debug.panic("Failed to allocate for gray stack", .{});
        };
    }

    fn markTable(self: *Self, table: *Table) void {
        for (table.entries) |entry| {
            const key = entry.key orelse continue;
            self.markObj(key.asObj());
            self.markValue(entry.value);
        }
    }

    fn markArray(self: *Self, array: *Array) void {
        for (array.values) |v| {
            self.markValue(v);
        }
    }

    fn traceReferences(self: *Self) void {
        while (self.gray_stack.popOrNull()) |o| {
            self.blackenObject(o);
        }
    }

    fn blackenObject(self: *Self, obj: *Obj) void {
        const v = Value{ .obj = obj };
        std.log.debug("{*} blacken {s}", .{ obj, v });
        switch (obj.type) {
            .Upvalue => self.markValue(obj.asUpvalue().closed),
            .Function => {
                const function = obj.asFunction();
                if (function.name) |name| {
                    self.markObj(name.asObj());
                }
                self.markArray(&function.chunk.constants);
            },
            .Class => {
                const class = obj.asClass();
                self.markObj(class.name.asObj());
            },
            .Instance => {
                const instance = obj.asInstance();
                self.markObj(instance.class.asObj());
                self.markTable(&instance.fields);
            },
            .Closure => {
                const closure = obj.asClosure();
                self.markObj(closure.function.asObj());
                for (closure.upvalues) |maybe_upvalue| {
                    const upvalue = maybe_upvalue orelse continue;
                    self.markObj(upvalue.asObj());
                }
            },
            else => {},
        }
    }

    fn sweep(self: *Self) void {
        var previous: ?*Obj = null;
        var next = self.objects;
        while (next) |obj| {
            if (obj.is_marked) {
                obj.is_marked = false;
                previous = obj;
                next = obj.next;
            } else {
                next = obj.next;
                if (previous) |p| {
                    p.next = next;
                } else {
                    self.objects = next;
                }
                self.freeObject(obj);
            }
        }
    }

    fn allocFn(self: *Self, len: usize, ptr_align: u29, len_align: u29, ret_addr: usize) Allocator.Error![]u8 {
        self.bytes_allocated += len;
        if (builtin.mode == std.builtin.Mode.Debug) {
            self.collectGarbage();
        } else if (self.bytes_allocated > self.next_gc) {
            self.collectGarbage();
        }
        return self.inner_allocator.rawAlloc(len, ptr_align, len_align, ret_addr);
    }
    fn resizeFn(self: *Self, buf: []u8, buf_align: u29, new_len: usize, len_align: u29, ret_addr: usize) ?usize {
        self.bytes_allocated += new_len - buf.len;
        if (builtin.mode == std.builtin.Mode.Debug) {
            if (new_len > buf.len) {
                self.collectGarbage();
            }
        } else if (self.bytes_allocated > self.next_gc) {
            self.collectGarbage();
        }
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
