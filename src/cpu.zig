const std = @import("std");
const BUS = @import("bus.zig").BUS;

pub const CPU = struct {
    alloc: std.mem.Allocator,
    a: u8,      // Accumulator
    x: u8,      // X register
    y: u8,      // Y register
    sp: u8,     // Stack pointer
    pc: u16,    // Program counter
    status: u8, // Status flags

    bus: *BUS,

    opcode: u16,  // Current OPCode
    stack: []u16, // Stack array
    cycles: u32,  // Cycles counter
    empty_cycles: u32, // Cycles to sleep as if we're busy

    const Self = @This();

    pub fn init(alloc: std.mem.Allocator, bus: *BUS) !Self {
        const stack = try alloc.alloc(u16, 16);

        return CPU{
            .a = 0,
            .x = 0,
            .y = 0,
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
        self.pcIncrement(1);    // Increment PC
        std.debug.print("C:{d:0>4} PC:0x{X:0>4} OP1:0x{X}\n", .{self.cycles, self.pc, self.opcode});
    }

    pub fn interruptBit(self: *Self, b: bool) void {
        if (!b)
            self.status = self.status & 0b00000100;
    }

    pub fn overflowBit(self: *Self, b: bool) void {
        if (!b)
            self.status = self.status & 0b01000000;
    }

    pub fn decimalBit(self: *Self, b: bool) void {
        if (b)
            self.status = self.status ^ 0b00001000
        else
            self.status = self.status & 0b00001000;
    }

    pub fn carryBit(self: *Self, b: bool) void {
        if (b)
            self.status = self.status ^ 0b00000001
        else
            self.status = self.status & 0b11111110;
    }

    pub fn negativeBit(self: *Self, b: bool) void {
        if (b)
            self.status = self.status ^ 0b10000000
        else
            self.status = self.status & 0b01111111;
    }

    pub fn zeroBit(self: *Self, b: bool) void {
        if (b)
            self.status = self.status ^ 0b00000010
        else
            self.status = self.status & 0b11111101;
    }

    pub fn cycle(self: *Self) void {
        self.cycleIncrement(1); // Increment cycle counter
        // Sleep few cycles if we need to act busy
        // simulating multi cycle instruction execution
        if (self.empty_cycles > 0) {
            self.empty_cycles = self.empty_cycles - 1;
            return;
        }
        // Read next instruction
        self.readInstruction();

        // TODO: make this with enums
        switch(self.opcode) {
            0xEA => {
                self.empty_cycles = 1;
            },
            0x0A => { // ASL (Arithmetic Shift Left)
                if ((self.a & 0b10000000) > 0) {
                    self.negativeBit(true);
                    self.a = self.a ^ 0b10000000; // Drop highest bit TODO: double check this
                }
                self.a = self.a << 1;
                self.empty_cycles = 1;
            },
            0x18 => { // CLC (Clear Carry)
                self.carryBit(false);
            },
            0x38 => { // SEC (Set Carry)
                self.carryBit(true);
            },
            0x44 => { // DEC (Decrement memory)
                self.readInstruction();

                const data = self.bus.readRam(self.opcode);
                self.bus.write(self.opcode, data-1);

                if (data == 1) {
                    self.zeroBit(true);
                } else if (data == 0) {
                    self.negativeBit(true);
                }
                self.empty_cycles = 4;
            },
            0x58 => { // CLI (Clear Interrupt Disable)
                self.interruptBit(false);
            },
            0x84 => { // STY (Store Index Register Y In Memory)
                // self.bus.ram.write(self.opcode,  = @intCast(self.opcode);
            },
            0x85 => { // STA (Store Accumulator)
                self.readInstruction();
                self.a = @intCast(self.opcode);
                self.empty_cycles = 3;
                //  TODO: Status registers
            },
            0xB8 => { // CLV (Clear Overflow)
                self.overflowBit(false);
            },
            0xD8 => { // CLD (Clear Decimal)
                self.decimalBit(false);
            },
            0xF8 => { // SED (Set Decimal)
                self.decimalBit(true);
            },
            0xA2 => { // LDX (Load X register Immidate)
                self.readInstruction();
                self.x = @intCast(self.opcode);

                // Set Zero Flag
                if (self.x == 0)
                    self.zeroBit(true);
                // Set Negative Flag
                if ((self.x & 0b10000000) > 0)
                    self.negativeBit(true);

                self.empty_cycles = 2;
            },
            0xA5 => { // LDA (Load Accumulator ZeroPage)
                self.readInstruction();
                const zero_page_addr: u16 = self.opcode;

                self.readInstruction();
                self.a = @intCast(self.bus.read(zero_page_addr));

                // Set Zero Flag
                if (self.x == 0)
                    self.zeroBit(true);
                // Set Negative Flag
                if ((self.x & 0b10000000) > 0)
                    self.negativeBit(true);

                self.empty_cycles = 2;
            },
            0xA6 => { // LDX (Load Index Register X from Memory)
                self.readInstruction();
                self.x = @intCast(self.opcode);

                // Set Zero Flag
                if (self.x == 0)
                    self.zeroBit(true);
                // Set Negative Flag
                if ((self.x & 0b10000000) > 0)
                    self.negativeBit(true);

                self.empty_cycles = 2;
            },
            0xCA => { // DEX (Decremetn X)
                self.empty_cycles = 2;
                if (self.x > 0)
                    self.x = self.x - 1
                else
                    std.debug.print("[error] Cannot decrement X register  [current:{}]\n", .{self.x});
                // Set Zero Flag
                if (self.x > 0)
                    self.zeroBit(false);
                // Set Negative Flag
                if ((self.x & 0b10000000) > 0)
                    self.negativeBit(true);
            },
            0xE8 => { // INX (Increment X)
                if (self.x < 0xFF) {
                    self.x = self.x + 1;
                } else {
                    self.x = 0;
                    self.overflowBit(true);
                    self.zeroBit(true);
                }
                // Set Negative Flag
                if ((self.x & 0b10000000) > 0)
                    self.negativeBit(true);

                self.empty_cycles = 2;
            },
            0x4C => { // JMP (Absolute Jump)
                const op1: u16 = self.bus.read(self.pc);

                self.pcIncrement(1);
                const op2: u16 = self.bus.read(self.pc);

                self.pc = op1 << 8 | op2;

                self.empty_cycles = 2;
                std.debug.print("Jumping to address 0x{X}\n", .{self.pc});
            },
            0x6C => { // Indirect Jump
                // 5 cycles
                // Indirect Mode (0x6C): Jumps to the address stored at the specified memory location.
                // Useful for dynamic jumps (e.g., jump tables), but it has a known bug on the original 6502: if the low byte is at $xxFF (e.g., $12FF),
                // the high byte is incorrectly fetched from $1200 instead of $1300. This quirk is present in the 6507 too.
            },
            0x0 => {
                //self.pcIncrement(1);
            },
            else => {
                std.debug.print("[warn] Unimplemented instruction 0x{X}\n", .{self.opcode});
            }
        }

    }

    pub fn pcIncrement(self: *Self, p: u16) void {
        var pc: u32 = @intCast(self.pc);

        const pp: u32 = @intCast(p);
        const max_pc: u32 = 0xFFFF;

        if ((pc + p) <= 0xFFFF) {
            self.pc = self.pc + p;
        } else {
            pc = (pc + pp) % max_pc;
            self.pc = @intCast(pc);
        }
    }

    pub fn cycleIncrement(self: *Self, c: u16) void {
        var cycles: u32 = @intCast(self.cycles);

        const cc: u32 = @intCast(c);
        const max_cycles: u32 = 0xFFFFFFFF;

        if ((cycles + c) <= 0xFFFFFFFF) {
            self.cycles = self.cycles + c;
        } else {
            cycles = (cycles + cc) % max_cycles;
            self.cycles = @intCast(cycles);
        }
    }

};
