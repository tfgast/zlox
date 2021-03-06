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
        .Print => return simpleInstruction("OP_PRINT", offset),
        .Jump => {
            return jumpInstruction("OP_JUMP", 1, c, offset);
        },
        .JumpIfFalse => {
            return jumpInstruction("OP_JUMP_IF_FALSE", 1, c, offset);
        },
        .Loop => {
            return jumpInstruction("OP_LOOP", -1, c, offset);
        },
        .Call => {
            return byteInstruction("OP_CALL", c, offset);
        },
        .Invoke => {
            return invokeInstruction("OP_CALL", c, offset);
        },
        .Closure => {
            var o = offset;
            o += 1;
            const constant = c.code[o];
            o += 1;
            const v = c.constants.values[constant];
            print("{s:<16} {d: >4} {s}\n", .{ "OP_CLOSURE", constant, v });

            const function = v.as(.Function).?;
            var j: u8 = 0;
            while (j < function.upvalue_count) : (j += 1) {
                const is_local = if (c.code[o] == 1) "local" else "upvalue";
                o += 1;
                const index = c.code[o];
                o += 1;
                print("{d:0>4}    | |                     {s} {d}\n", .{ o - 2, is_local, index });
            }
            return o;
        },
        .CloseUpvalue => return simpleInstruction("OP_CLOSE_UPVALUE", offset),
        .Return => return simpleInstruction("OP_RETURN", offset),
        .Class => {
            return constantInstruction("OP_CLASS", c, offset);
        },
        .Inherit => {
            return simpleInstruction("OP_INHERIT", offset);
        },
        .Method => {
            return constantInstruction("OP_METHOD", c, offset);
        },
        .Constant => {
            return constantInstruction("OP_CONSTANT", c, offset);
        },
        .ConstantLong => {
            return constantLongInstruction("OP_CONSTANT_LONG", c, offset);
        },
        .Nil => {
            return simpleInstruction("OP_NIL", offset);
        },
        .True => {
            return simpleInstruction("OP_TRUE", offset);
        },
        .False => {
            return simpleInstruction("OP_FALSE", offset);
        },
        .Pop => {
            return simpleInstruction("OP_POP", offset);
        },
        .GetLocal => {
            return byteInstruction("OP_GET_LOCAL", c, offset);
        },
        .SetLocal => {
            return byteInstruction("OP_SET_LOCAL", c, offset);
        },
        .GetGlobal => {
            return constantInstruction("OP_GET_GLOBAL", c, offset);
        },
        .DefineGlobal => {
            return constantInstruction("OP_DEFINE_GLOBAL", c, offset);
        },
        .SetGlobal => {
            return constantInstruction("OP_SET_GLOBAL", c, offset);
        },
        .GetUpvalue => {
            return byteInstruction("OP_GET_UPVALUE", c, offset);
        },
        .SetUpvalue => {
            return byteInstruction("OP_SET_UPVALUE", c, offset);
        },
        .GetProperty => {
            return constantInstruction("OP_GET_PROPERTY", c, offset);
        },
        .SetProperty => {
            return constantInstruction("OP_SET_PROPERTY", c, offset);
        },
        .GetSuper => {
            return constantInstruction("OP_GET_SUPER", c, offset);
        },
        .Equal => {
            return simpleInstruction("OP_EQUAL", offset);
        },
        .Greater => {
            return simpleInstruction("OP_GREATER", offset);
        },
        .Less => {
            return simpleInstruction("OP_LESS", offset);
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
        .Not => {
            return simpleInstruction("OP_NOT", offset);
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

fn byteInstruction(name: []const u8, c: *chunk.Chunk, offset: usize) usize {
    const slot = c.code[offset + 1];
    print("{s:<16} {d: >4}\n", .{ name, slot });
    return offset + 2;
}

fn constantInstruction(name: []const u8, c: *chunk.Chunk, offset: usize) usize {
    const constant = c.code[offset + 1];
    const v = c.constants.values[constant];
    print("{s:<16} {d: >4} '{s}'\n", .{ name, constant, v });
    return offset + 2;
}

fn invokeInstruction(name: []const u8, c: *chunk.Chunk, offset: usize) usize {
    const constant = c.code[offset + 1];
    const arg_count = c.code[offset + 2];
    const v = c.constants.values[constant];
    print("{s:<16} ({d} args) {d: >4} '{s}'\n", .{ name, arg_count, constant, v });
    return offset + 3;
}

fn constantLongInstruction(name: []const u8, c: *chunk.Chunk, offset: usize) usize {
    const constant = (@intCast(u32, c.code[offset + 3]) << 16) | (@intCast(u32, c.code[offset + 2]) << 8) | @intCast(u32, c.code[offset + 1]);
    const v = c.constants.values[constant];
    print("{s:<16} {d: >4} '{s}'\n", .{ name, constant, v });
    return offset + 4;
}

fn jumpInstruction(name: []const u8, sign: i8, c: *chunk.Chunk, offset: usize) usize {
    const jump = (@intCast(u16, c.code[offset + 1]) << 8) | @intCast(u16, c.code[offset + 2]);
    const new_offset = if (sign > 0) offset + 3 + jump else offset + 3 - jump;
    print("{s:<16} {d: >4} -> {d}\n", .{ name, offset, new_offset });
    return offset + 3;
}
