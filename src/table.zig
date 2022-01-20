const std = @import("std");
const value = @import("value.zig");
const object = @import("object.zig");
const memory = @import("memory.zig");

const Value = value.Value;
const Entry = value.Entry;
const ObjString = object.ObjString;
const GarbageCollector = memory.GarbageCollector;

const TABLE_MAX_LOAD: f64 = 0.75;

pub const Table = struct {
    const Self = Table;

    count: usize,
    entries: []Entry,
    gc: *GarbageCollector,

    pub fn init(gc: *GarbageCollector) Self {
        return Self{
            .count = 0,
            .entries = &[_]Entry{},
            .gc = gc,
        };
    }

    pub fn free(self: *Self) void {
        self.gc.allocator.free(self.entries);
        self.entries.len = 0;
        self.count = 0;
    }

    pub fn get(self: *const Self, key: *ObjString) ?*Value {
        if (self.count == 0) {
            return null;
        }
        const entry = Self.findEntry(self.entries, key);
        if (entry.key == null) {
            return null;
        }
        return &entry.value;
    }

    pub fn set(self: *Self, key: *ObjString, v: Value) !bool {
        if (self.count + 1 > @floatToInt(usize, @intToFloat(f64, self.entries.len) *
            TABLE_MAX_LOAD))
        {
            const new_capacity = memory.grow_capacity(self.entries.len);
            try self.adjustCapacity(new_capacity);
        }
        var entry = Self.findEntry(self.entries, key);
        const isNewKey = entry.key == null;
        if (isNewKey and entry.value == .nil) {
            self.count += 1;
        }
        entry.key = key;
        entry.value = v;
        return isNewKey;
    }

    pub fn delete(self: *Self, key: *ObjString) bool {
        if (self.count == 0) {
            return false;
        }
        var entry = Self.findEntry(self.entries, key);
        if (entry.key == null) {
            return false;
        }
        // Place tombstone in the entry
        entry.key = null;
        entry.value = .{ .boolean = true };
        return true;
    }

    pub fn addAll(self: *Self, to: *Self) !void {
        for (self.entries) |entry| {
            const key = entry.key orelse continue;
            _ = try to.set(key, entry.value);
        }
    }

    fn findEntry(entries: []Entry, key: *ObjString) *Entry {
        const capacity = entries.len;
        var index = key.hash % capacity;
        var tombstone: ?*Entry = null;
        while (true) {
            const entry = &entries[index];
            if (entry.key == null) {
                if (entry.value == .nil) {
                    //Empty entry
                    if (tombstone) |t| {
                        return t;
                    } else {
                        return entry;
                    }
                } else {
                    // We found a tombstone
                    if (tombstone == null) {
                        tombstone = entry;
                    }
                }
            } else if (entry.key == key) {
                // We found the key
                return entry;
            }

            index += 1;
            if (index == capacity) {
                index = 0;
            }
        }
    }

    pub fn findString(self: *Self, chars: []const u8, hash: u32) ?*ObjString {
        if (self.count == 0) {
            return null;
        }
        const capacity = self.entries.len;
        var index = hash % capacity;
        while (true) {
            const entry = &self.entries[index];
            if (entry.key) |key| {
                if (key.hash == hash and std.mem.eql(u8, key.str, chars)) {
                    return entry.key;
                }
            } else if (entry.value == .nil) {
                // tombstone
                return null;
            }
            index += 1;
            if (index == capacity) {
                index = 0;
            }
        }
    }

    fn adjustCapacity(self: *Self, new_capacity: usize) !void {
        var entries = try self.gc.allocator.alloc(Entry, new_capacity);
        std.mem.set(Entry, entries, Entry{ .key = null, .value = .nil });
        self.count = 0;
        for (self.entries) |entry| {
            const key = entry.key orelse continue;
            var dest = Self.findEntry(entries, key);
            dest.key = key;
            dest.value = entry.value;
            self.count += 1;
        }
        self.gc.allocator.free(self.entries);
        self.entries = entries;
    }

    pub fn removeWhite(self: *Self) void {
        for (self.entries) |entry| {
            const key = entry.key orelse continue;
            if (!key.obj.is_marked) {
                _ = self.delete(key);
            }
        }
    }
};
