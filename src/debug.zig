const chunk = @import("chunk.zig");
const value = @import("value.zig");
const print = @import("std").debug.print;

pub fn disassembleChunk(c: *chunk.Chunk, name: []const u8) void {
    print("== {s} ==\n", .{name});

    var offset: usize = 0;
    while (offset < c.code.len) {
        offset = disassembleInstruction(c, offset);
    }
}

pub fn disassembleInstruction(c: *chunk.Chunk, offset: usize) usize {
    print("{d:0>4} ", .{offset});
    if (offset > 0 and c.getLine(offset) == c.getLine(offset - 1)) {
        print("   | ", .{});
    } else {
        print("{d: >4} ", .{c.getLine(offset)});
    }
    const instruction = c.code[offset];
    switch (@intToEnum(chunk.OpCode, instruction)) {
        .Return => return simpleInstruction("OP_RETURN", offset),
        .Constant => {
            return constantInstruction("OP_CONSTANT", c, offset);
        },
        .ConstantLong => {
            return constantLongInstruction("OP_CONSTANT_LONG", c, offset);
        },
        .Add => {
            return simpleInstruction("OP_ADD", offset);
        },
        .Subtract => {
            return simpleInstruction("OP_SUBTRACT", offset);
        },
        .Multiply => {
            return simpleInstruction("OP_MULTIPLY", offset);
        },
        .Divide => {
            return simpleInstruction("OP_DIVIDE", offset);
        },
        .Negate => {
            return simpleInstruction("OP_NEGATE", offset);
        },
        _ => {
            print("Unknown opcode {d}", .{instruction});
            return offset + 1;
        },
    }
}

fn simpleInstruction(name: []const u8, offset: usize) usize {
    print("{s}\n", .{name});
    return offset + 1;
}

fn constantInstruction(name: []const u8, c: *chunk.Chunk, offset: usize) usize {
    const constant = c.code[offset + 1];
    print("{s:<16} {d: >4} '", .{ name, constant });
    value.print(c.constants.values[constant]);
    print("'\n", .{});
    return offset + 2;
}

fn constantLongInstruction(name: []const u8, c: *chunk.Chunk, offset: usize) usize {
    const constant = (@intCast(u32, c.code[offset + 3]) << 16) | (@intCast(u32, c.code[offset + 2]) << 8) | @intCast(u32, c.code[offset + 1]);
    print("{s:<16} {d: >4} '", .{ name, constant });
    value.print(c.constants.values[constant]);
    print("'\n", .{});
    return offset + 4;
}
