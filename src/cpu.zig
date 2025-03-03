const std = @import("std");
const RAM = @import("mem.zig").RAM;
const BUS = @import("bus.zig").BUS;

pub const CPU = struct {
    alloc: std.mem.Allocator,
    a: u8,      // Accumulator
    x: u8,      // X register
    y: u8,      // Y register
    sp: u8,     // Stack pointer
    pc: u16,    // Program counter
    status: u8, // Status flags

    ram: *RAM,
    bus: *BUS,

    opcode: u16,
    stack: []u16,

    cycles: u32,
    empty_cycles: u32,

    const Self = @This();

    pub fn init(alloc: std.mem.Allocator, ram: *RAM, bus: *BUS) !Self {
        const stack = try alloc.alloc(u16, 16);
        // const pc = @as(u16, &bus[0xFC]) | (@as(u16, &bus[0xFD]) << 8);

        const c = CPU{
            .a = 0,
            .x = 0,
            .y = 0,
            .ram = ram,
            .bus = bus,
            .sp = 0xFF,
            .cycles = 0,
            .opcode = 0,
            .pc = 0xFFFE,
            .status = 0x34,
            .alloc = alloc,
            .stack = stack,
            .empty_cycles = 0
        };
        return c;
    }

    pub fn deinit(self: *CPU) void {
        self.alloc.free(self.stack);
    }

    pub fn reset(self: *Self, entrypoint: u16) void {
        self.sp = 0xFF;
        self.pc = entrypoint;
        self.status = 0x34;

        self.opcode = 0;
        self.cycles = 0;
        self.empty_cycles = 0;

        for (self.stack) |*p|
            p.* = 0;
    }

    pub fn readInstruction(self: *Self) void {
        self.opcode = self.bus.read(self.pc);

        std.debug.print("C:{d:0>4} PC:0x{X:0>4} OP1:0x{X}\n", .{self.cycles, self.pc, self.opcode});
        // self.opcode = (self.ram[self.pc] << 8 | self.ram[self.pc + 1]) >> 8;
    }

    pub fn cycle(self: *Self) void {
        self.cycleIncrement(1);

        if (self.empty_cycles > 0) {
            self.empty_cycles = self.empty_cycles - 1;
            return;
        }

        self.readInstruction();

        // TODO: make this with enums
        switch(self.opcode) {
            0xEA => {
                self.pcIncrement(1);
                self.empty_cycles = 2;
            },
            0x18 => { // Clear Carry
                self.pcIncrement(1);
                self.status = self.status & 0b11111110;
            },
            0x38 => { // Set Carry
                self.pcIncrement(1);
                self.status = self.status ^ 0b00000001;
            },
            0x58 => { // Clear Interrupt Disable
                self.pcIncrement(1);
                self.status = self.status ^ 0b11011111;
            },
            0x84 => { // STY (Store Index Register Y In Memory)
                // self.bus.ram.write(self.opcode,  = @intCast(self.opcode);
            },
            0x85 => { // STA (Store Accumulator)
                self.pcIncrement(1);
                self.cycleIncrement(1);
                self.readInstruction();
                self.a = @intCast(self.opcode);
                self.empty_cycles = 2;
                //  TODO: Status registers
            },
            0xB8 => { // Clear Overflow
                self.pcIncrement(1);
                self.status = self.status & 0b01000000;
            },
            0xD8 => { // Clear Decimal
                self.pcIncrement(1);
                self.status = self.status & 0b00001000;
            },
            0xF8 => { // Set Decimal
                self.pcIncrement(1);
                self.status = self.status ^ 0b00001000;
            },
            0xA6 => { // LDX (Load Index Register X from Memory)
                self.pcIncrement(1);
                self.cycleIncrement(1);
                self.readInstruction();
                self.x = @intCast(self.opcode);
                self.empty_cycles = 2;
            },
            0xCA => { // Decremetn X
                self.empty_cycles = 2;
                self.pcIncrement(1);
                if (self.x > 0)
                    self.x = self.x - 1
                else
                    std.debug.print("[error] Cannot decrement X register  [current:{}]\n", .{self.x});

                if (self.x > 0)
                    self.status = self.status & 0b01000000; // Clear Zero flag

                // TODO: FIX
                // if (self.x & 128)
                //     self.status = self.status & 0b00000001; // Set Negative flag
            },
            0xE8 => { // Increment X
                self.pcIncrement(1);
                self.empty_cycles = 2;
                if (self.x < 0xFF)
                    self.x = self.x + 1
                else
                    std.debug.print("[error] Cannot increment X register  [current:{}]\n", .{self.x});
            },
            0x4C => { // Absolute Jump (3 cycles)
                self.pcIncrement(1);
                self.cycleIncrement(1);
                const op1: u16 = self.bus.read(self.pc);

                self.pcIncrement(1);
                self.cycleIncrement(1);
                const op2: u16 = self.bus.read(self.pc);

                self.pc = op1 << 8 | op2;
                std.debug.print("Jumping to address 0x{X}\n", .{self.pc});
            },
            0x6C => { // Indirect Jump
                // 5 cycles
                // Indirect Mode (0x6C): Jumps to the address stored at the specified memory location.
                // Useful for dynamic jumps (e.g., jump tables), but it has a known bug on the original 6502: if the low byte is at $xxFF (e.g., $12FF),
                // the high byte is incorrectly fetched from $1200 instead of $1300. This quirk is present in the 6507 too.
            },
            0x0 => {
                self.pcIncrement(1);
            },
            else => {
                std.debug.print("Unimplemented instruction 0x{X}\n", .{self.opcode});
                self.pcIncrement(1);
            }
        }

    }

    pub fn pcIncrement(self: *Self, p: u16) void {
        var pc: u32 = @intCast(self.pc);

        const pp: u32 = @intCast(p);
        const max_pc: u32 = 0xFFFF;

        if ((pc + p) > 0xFFFF) {
            pc = (pc + pp) % max_pc;
            self.pc = @intCast(pc);
        } else {
            self.pc = self.pc + p;
        }
    }

    pub fn cycleIncrement(self: *Self, c: u16) void {
        var cycles: u32 = @intCast(self.cycles);

        const cc: u32 = @intCast(c);
        const max_cycles: u32 = 0xFFFFFFFF;

        if ((cycles + c) > 0xFFFFFFFF) {
            cycles = (cycles + cc) % max_cycles;
            self.cycles = @intCast(cycles);
        } else {
            self.cycles = self.cycles + c;
        }
    }

    pub fn cycleCounter(self: *Self, c: u16, p: u16) void {
        var pc: u32 = @intCast(self.pc);
        var cycles: u32 = @intCast(self.cycles);

        const pp: u32 = @intCast(p);
        const cc: u32 = @intCast(c);

        const max_pc: u32 = 0xFFFF;
        const max_cycles: u32 = 0xFFFFFFFF;

        if ((pc + p) > 0xFFFF) {
            pc = (pc + pp) % max_pc;
            self.pc = @intCast(pc);
        } else {
            self.pc = self.pc + p;
        }

        if ((cycles + c) > 0xFFFFFFFF) {
            cycles = (cycles + cc) % max_cycles;
            self.cycles = @intCast(cycles);
        } else {
            self.cycles = self.cycles + c;
        }
    }
};
