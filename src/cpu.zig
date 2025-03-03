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

    cycles: u32,
    opcode: u16,
    stack: []u16,
    delay_timer: u8,

    const Self = @This();

    pub fn init(alloc: std.mem.Allocator, ram: *RAM, bus: *BUS) !Self {
        const stack = try alloc.alloc(u16, 16);
        // const pc = @as(u16, &bus[0xFC]) | (@as(u16, &bus[0xFD]) << 8);
            // .pc = 0xF000,

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
            .delay_timer = 0
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
        self.delay_timer = 0;

        for (self.stack) |*p|
            p.* = 0;
    }

    pub fn readInstruction(self: *Self) void {
        // const op1 = self.bus.readCart(self.pc);
        self.opcode = self.bus.read(self.pc);

        std.debug.print("C:{d:0>4} PC:0x{X:0>4} OP1:0x{X}\n", .{self.cycles, self.pc, self.opcode});
        // self.opcode = (self.ram[self.pc] << 8 | self.ram[self.pc + 1]) >> 8;
    }

    pub fn cycle(self: *Self) void {
        self.readInstruction();

        // TODO: make this with enums
        switch(self.opcode) {
            0xEA => {
                self.cycleCounter(2, 1);
            },
            0x18 => { // Clear Carry
                self.cycleCounter(1, 1);
                self.status = self.status & 0b11111110;
            },
            0x38 => { // Set Carry
                self.cycleCounter(1, 1);
                self.status = self.status ^ 0b00000001;
            },
            0xB8 => { // Clear Overflow
                self.cycleCounter(1, 1);
                self.status = self.status & 0b01000000;
            },
            0xD8 => { // Clear Decimal
                self.cycleCounter(1, 1);
                self.status = self.status & 0b00001000;
            },
            0xF8 => { // Set Decimal
                self.cycleCounter(1, 1);
                self.status = self.status ^ 0b00001000;
            },
            0x4C => { // Absolute Jump (3 cycles)
                self.cycleCounter(1, 1);
                const op1: u16 = self.bus.read(self.pc);

                self.cycleCounter(1, 1);
                const op2: u16 = self.bus.read(self.pc);

                self.pc = op1 << 8 | op2;
                std.debug.print("Jumping to address 0x{X}\n", .{self.pc});
            },
            0x6C => { // Indirect Jump
                // 5 cycles
                // Indirect Mode (0x6C): Jumps to the address stored at the specified memory location. Useful for dynamic jumps (e.g., jump tables), but it has a known bug on the original 6502: if the low byte is at $xxFF (e.g., $12FF), the high byte is incorrectly fetched from $1200 instead of $1300. This quirk is present in the 6507 too.
            },
            0x0 => {
                self.cycleCounter(1, 1);
            },
            else => {
                self.cycleCounter(1, 1);
            }
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
