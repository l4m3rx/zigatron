const std = @import("std");
const BUS = @import("bus.zig").BUS;

const CarryFlag: u8            = 0b00000001; // Bit 0
const ZeroFlag: u8             = 0b00000010;
const InterruptDisable: u8     = 0b00000100;
const DecimalMode: u8          = 0b00001000;
const BreakCommand: u8         = 0b00010000;
const UnusedFlag: u8           = 0b00100000;
const OverflowFlag: u8         = 0b01000000;
const NegativeFlag: u8         = 0b10000000; // Bit 7

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
    cycles: u32,  // Cycles counter
    empty_cycles: u32, // Cycles to sleep as if we're busy

    const Self = @This();

    pub fn init(alloc: std.mem.Allocator, bus: *BUS) !Self {
        return CPU{
            .a = 0,
            .x = 0,
            .y = 0,
            .bus = bus,
            .sp = 0xFF,
            .cycles = 0,
            .opcode = 0,
            .pc = 0xFFFC,
            .status = 0x34,
            .alloc = alloc,
            .empty_cycles = 0
        };
    }

    // pub fn deinit(self: *CPU) void {
    //     self.alloc.free(self.stack);
    // }

    pub fn reset(self: *Self, entrypoint: u16) void {
        self.sp = 0xFF;
        self.pc = entrypoint;
        self.status = 0x34;

        self.opcode = 0;
        self.cycles = 0;
        self.empty_cycles = 0;
    }

    pub fn readInstruction(self: *Self) void {
        self.opcode = self.bus.read(self.pc);
        self.pcIncrement(1);
    }

    pub fn getWord(self: *Self) u16 {
        const low = self.bus.read(self.pc);
        self.pcIncrement(1);
        const high = self.bus.read(self.pc);
        self.pcIncrement(1);

        return (@as(u16, high) << 8) | low;
    }

    pub fn getByte(self: *Self) u8 {
        const byte = self.bus.read(self.pc);
        self.pcIncrement(1);
        return byte;
    }

    pub fn samePage(fpage: i32, spage: i32) u8 {
        if ((fpage & 0xFF00) != (spage & 0xFF00))
            return 1;
        return 0;
    }

    pub fn pushStack(self: *Self, value: u8) void {
        self.bus.writeStack(0x0100 + @as(u16, self.sp), value);
        self.sp -%= 1;
    }

    pub fn pullStack(self: *Self) u8 {
        self.sp +%= 1;
        return self.bus.readStack(0x0100 + @as(u16, self.sp));
    }

    pub fn setZeroNegative(self: *Self, value: u8) void {
        self.zeroBit(value == 0);
        self.negativeBit((value & 0x80) != 0);
    }

    pub fn setOverflow(self: *Self, value: u8) void {
        if (value & 0x40 != 0)
            self.overflowBit(true)
        else
            self.overflowBit(true);
    }

    pub fn interruptBit(self: *Self, b: bool) void {
        if (b)
            self.status |= InterruptDisable
        else
            self.status &= ~InterruptDisable;
    }

    pub fn overflowBit(self: *Self, b: bool) void {
        if (b)
            self.status |= OverflowFlag
        else
            self.status &= ~OverflowFlag;
    }

    pub fn decimalBit(self: *Self, b: bool) void {
        if (b)
            self.status |= DecimalMode
        else
            self.status &= ~DecimalMode;
    }

    pub fn carryBit(self: *Self, b: bool) void {
        if (b)
            self.status |= CarryFlag
        else
            self.status &= ~CarryFlag;
    }

    pub fn negativeBit(self: *Self, b: bool) void {
        if (b)
            self.status |= NegativeFlag
        else
            self.status &= ~NegativeFlag;
    }

    pub fn zeroBit(self: *Self, b: bool) void {
        if (b)
            self.status |= ZeroFlag
        else
            self.status &= ~ZeroFlag;
    }

    pub fn cycle(self: *Self) void {
        self.cycleIncrement(1);
        if (self.empty_cycles > 0) { // Sleep acting busy
            self.empty_cycles = self.empty_cycles - 1;
            return;
        }
        // Read next instruction
        self.readInstruction();
        std.debug.print("[D] Cycle: {d} OPCode:0x{X} PC:0x{X}\n", .{self.cycles, self.opcode, self.pc });

        // TODO: speedup
        switch(self.opcode) {
            0x0 => {
                self.pcIncrement(1);
                const pc_high: u8 = @intCast((self.pc >> 8) & 0xFF);
                self.pushStack(pc_high);
                const pc_low: u8 = @intCast(self.pc & 0xFF);
                self.pushStack(pc_low);

                const status_with_b = self.status | 0b00010000;
                self.pushStack(status_with_b);
                self.interruptBit(true);

                const irq_low = self.bus.read(0xFFFE);
                const irq_high = self.bus.read(0xFFFF);
                self.pc = (@as(u16, irq_high) << 8) | irq_low;

                self.empty_cycles = 6;
            },
            0x01 => { // ORA (Indirect,X)
                const zp_addr = self.getByte();
                const effective_zp = (zp_addr +% self.x) & 0xFF;

                const low = self.bus.read(effective_zp);
                const high = self.bus.read((effective_zp +% 1) & 0xFF);
                const effective_addr = (@as(u16, high) << 8) | low;

                const value = self.bus.read(effective_addr);
                self.a |= value;

                self.setZeroNegative(self.a);
                self.empty_cycles = 5;
            },
            0x05 => { // ORA Zero Page
                const zp_addr = self.getByte();
                const value = self.bus.read(zp_addr);
                self.a |= value;

                self.setZeroNegative(self.a);
                self.empty_cycles = 2;
            },
            0x06 => { // ASL Zero Page
                const zp_addr_u8 = self.getByte();
                const zp_addr: u16 = @intCast(zp_addr_u8);

                const value = self.bus.read(zp_addr);
                const carry_out = (value & 0x80) != 0;

                const result = value << 1;
                self.bus.write(zp_addr, result);

                self.carryBit(carry_out);
                self.setZeroNegative(result);
                self.empty_cycles = 4;
            },
            0x08 => { // PHP - Push Processor Status
                const status_with_b = self.status | 0x10; // Set Break flag (bit 4)
                self.pushStack(status_with_b);
                self.empty_cycles = 2;
            },
            0x09 => { // ORA Immediate
                const value = self.bus.read(self.pc);
                self.pcIncrement(1);

                self.a |= value;
                self.setZeroNegative(self.a);
                self.empty_cycles = 1;
            },
            0x0A => { // ASL (Arithmetic Shift Left)
                const carry = (self.a & 0x80) != 0;
                self.a = self.a << 1;

                self.carryBit(carry);
                self.setZeroNegative(self.a);
                self.empty_cycles = 1;
            },
            0x0D => { // ORA Absolute
                const addr = self.getWord();
                const value = self.bus.read(addr);

                self.a |= value;
                self.setZeroNegative(self.a);
                self.empty_cycles = 3;
            },
            0x0E => { // ASL Absolute
                const addr = self.getWord();
                const value = self.bus.read(addr);
                const carry_out = (value & 0x80) != 0;

                const result = value << 1;
                self.bus.write(addr, result);

                self.carryBit(carry_out);
                self.setZeroNegative(self.a);

                self.empty_cycles = 5;
            },
            0x10 => { // BPL
                const offset: i8 = @bitCast(self.bus.read(self.pc));
                self.pcIncrement(1);

                if ((self.status & 0x80) == 0) {
                    const wide: i32 = self.pc;
                    self.pc = @intCast(wide + offset);
                    self.empty_cycles = 2 + samePage(wide, self.pc);
                } else {
                    self.empty_cycles = 1;
                }
            },
            0x11 => { // ORA (Indirect,Y)
                const zp_addr = self.getByte();

                const low = self.bus.read(zp_addr);
                const high = self.bus.read((zp_addr +% 1) & 0xFF);

                const base_addr = (@as(u16, high) << 8) | low;
                const effective_addr = base_addr +% @as(u16, self.y);

                const value = self.bus.read(effective_addr);
                self.a |= value;

                self.setZeroNegative(self.a);
                self.empty_cycles = 4 + samePage(base_addr, effective_addr);
            },
            0x15 => { // ORA Zero Page,X
                const zp_addr = self.getByte();
                const effective_addr = (zp_addr +% self.x) & 0xFF;

                const value = self.bus.read(effective_addr);
                self.a |= value;

                self.setZeroNegative(self.a);
                self.empty_cycles = 3;
            },
            0x16 => { // ASL Zero Page,X
                const zp_addr = self.getByte();
                const effective_addr = (zp_addr +% self.x) & 0xFF;

                const value = self.bus.read(effective_addr);
                const carry_out = (value & 0x80) != 0;

                const result = value << 1;
                self.bus.write(effective_addr, result);

                self.carryBit(carry_out);
                self.setZeroNegative(self.a);
                self.empty_cycles = 5;
            },
            0x18 => { // CLC (Clear Carry)
                self.carryBit(false);
            },
            0x19 => { // ORA Absolute,Y
                const base_addr = self.getWord();
                const effective_addr = base_addr +% @as(u16, self.y);

                const value = self.bus.read(effective_addr);
                self.a |= value;

                self.setZeroNegative(self.a);
                self.empty_cycles = 3 + samePage(base_addr, effective_addr);
            },
            0x1D => { // ORA Absolute,X
                const base_addr = self.getWord();
                const effective_addr = base_addr +% @as(u16, self.x);

                const value = self.bus.read(effective_addr);
                self.a |= value;

                self.setZeroNegative(self.a);
                self.empty_cycles = 3 + samePage(base_addr, effective_addr);
            },
            0x1E => { // ASL Absolute,X
                const base_addr = self.getWord();
                const effective_addr = base_addr +% @as(u16, self.x);

                const value = self.bus.read(effective_addr);
                const carry_out = (value & 0x80) != 0;

                const result = value << 1;
                self.bus.write(effective_addr, result);

                self.carryBit(carry_out);
                self.setZeroNegative(result);

                self.empty_cycles = 6;
            },
            0x20 => { // JSR Absolute
                const addr = self.getWord();
                const return_addr = self.pc;

                const temp_sp: u16 = self.sp;
                const sp_addr: u16 = 0x100 + temp_sp;

                self.bus.write(sp_addr, @intCast((return_addr >> 8) & 0xFF));
                self.sp -%= 1;
                self.bus.write(sp_addr, @intCast(return_addr & 0xFF));
                self.sp -%= 1;

                self.pc = addr;
                self.empty_cycles = 5;
            },
            0x21 => { // AND (Indirect,Y)
                const zp_addr = self.getByte();
                const low = self.bus.read(zp_addr);
                const high = self.bus.read((zp_addr +% 1) & 0xFF);
                const base_addr = (@as(u16, high) << 8) | low;
                const effective_addr = base_addr +% @as(u16, self.y);

                const value = self.bus.read(effective_addr);
                self.a &= value;

                self.setZeroNegative(self.a);
                self.empty_cycles = 3 + samePage(base_addr, effective_addr);
            },
            0x24 => { // BIT Zero Page
                const zp_addr = self.getByte();
                const value = self.bus.read(zp_addr);

                const result = self.a & value;

                self.setZeroNegative(result);
                self.setOverflow(value);

                self.empty_cycles = 2;
            },
            0x25 => { // AND Zero Page
                const zp_addr = self.getByte();
                const value = self.bus.read(zp_addr);
                self.a &= value;

                self.setZeroNegative(self.a);
                self.empty_cycles = 2;
            },
            0x26 => { // ROL Zero Page
                const zp_addr = self.getByte();
                const value = self.bus.read(zp_addr);

                const carry_in = (self.status & 0x01) != 0;
                const carry_out = (value & 0x80) != 0;
                const carry_bit: u8 = if (carry_in) 1 else 0;

                const result = (value << 1) | carry_bit;

                self.bus.write(zp_addr, result);

                self.carryBit(carry_out);
                self.setZeroNegative(result);
                self.empty_cycles = 4;
            },
            0x28 => { // PLP - Pull Processor Status
                self.sp +%= 1;
                self.status = self.bus.read(0x0100 + @as(u16, self.sp));
                self.empty_cycles = 3;
            },
            0x29 => { // AND Immediate
                const value = self.getByte();
                self.a &= value;
                self.setZeroNegative(self.a);
                self.empty_cycles = 1;
            },
            0x2A => { // ROL Accumulator
                const carry_in = (self.status & 0x01) != 0;
                const carry_out = (self.a & 0x80) != 0;
                const carry_bit: u8 = if (carry_in) 1 else 0;
                self.a = (self.a << 1) | carry_bit;

                self.carryBit(carry_out);
                self.setZeroNegative(self.a);
                self.empty_cycles = 1;
            },
            0x2C => { // BIT Absolute
                const addr = self.getWord();
                const value = self.bus.read(addr);

                const result = self.a & value;

                self.setZeroNegative(result);
                self.setOverflow(value);
                self.empty_cycles = 3;
            },
            0x2D => { // AND Absolute
                const addr = self.getWord();
                const value = self.bus.read(addr);
                self.a &= value;

                self.setZeroNegative(self.a);
                self.empty_cycles = 3;
            },
            0x2E => { // ROL Absolute
                const addr = self.getWord();
                const value = self.bus.read(addr);

                const carry_in = (self.status & 0x01) != 0;
                const carry_out = (value & 0x80) != 0;
                const carry_bit: u8 = if (carry_in) 1 else 0;

                const result = (value << 1) | carry_bit;

                self.bus.write(addr, result);
                self.carryBit(carry_out);
                self.setZeroNegative(result);

                self.empty_cycles = 5;
            },
            0x30 => { // BMI - Branch if Minus
                const offset: i8 = @bitCast(self.bus.read(self.pc));
                self.pcIncrement(1);

                if ((self.status & 0x80) != 0) { // Negative flag set
                    const wide: i32 = self.pc;
                    self.pc = @intCast(wide + offset);
                    self.empty_cycles = 2 + samePage(wide, self.pc);
                } else {
                    self.empty_cycles = 1;
                }
            },
            0x31 => { // EOR (Indirect,Y)
                const zp_addr = self.getByte();

                const low = self.bus.read(zp_addr);
                const high = self.bus.read((zp_addr +% 1) & 0xFF);

                const base_addr = (@as(u16, high) << 8) | low;
                const effective_addr = base_addr +% @as(u16, self.y);

                const value = self.bus.read(effective_addr);
                self.a ^= value;

                self.setZeroNegative(self.a);
                self.empty_cycles = 4 + samePage(base_addr, effective_addr);
            },
            0x35 => { // AND Zero Page,X
                const zp_addr = self.getByte();
                const effective_addr = (zp_addr +% self.x) & 0xFF;

                const value = self.bus.read(effective_addr);
                self.a &= value;

                self.setZeroNegative(self.a);
                self.empty_cycles = 3;
            },
            0x38 => { // SEC (Set Carry)
                self.carryBit(true);
            },
            0x39 => { // AND Absolute,Y
                const base_addr = self.getWord();
                const effective_addr = base_addr +% @as(u16, self.y);

                const value = self.bus.read(effective_addr);
                self.a &= value;

                self.setZeroNegative(self.a);
                self.empty_cycles = 3 + samePage(base_addr, effective_addr);
            },
            0x3D => { // AND Absolute,X
                const base_addr = self.getWord();
                const effective_addr = base_addr +% @as(u16, self.x);

                const value = self.bus.read(effective_addr);
                self.a &= value;

                self.setZeroNegative(self.a);
                self.empty_cycles = 3 + samePage(base_addr, effective_addr);
            },
            0x3E => { // ROL Absolute,X
                const base_addr = self.getWord();
                const effective_addr = base_addr +% @as(u16, self.x);

                const value = self.bus.read(effective_addr);

                const carry_in = (self.status & 0x01) != 0;
                const carry_out = (value & 0x80) != 0;
                const carry_bit: u8 = if (carry_in) 1 else 0;

                const result = (value << 1) | carry_bit;
                self.bus.write(effective_addr, result);

                self.carryBit(carry_out);
                self.setZeroNegative(self.a);
                self.empty_cycles = 6;
            },
            0x40 => { // RTI - Return from Interrupt
                self.sp +%= 1;
                self.status = self.bus.read(0x0100 + @as(u16, self.sp));

                self.sp +%= 1;
                const pc_low = self.bus.read(0x0100 + @as(u16, self.sp));
                self.sp +%= 1;
                const pc_high = self.bus.read(0x0100 + @as(u16, self.sp));

                self.pc = (@as(u16, pc_high) << 8) | pc_low;
                self.empty_cycles = 5;
            },
            0x41 => { // EOR (Indirect,X)
                const zp_addr = self.getByte();
                const effective_zp = (zp_addr +% self.x) & 0xFF;

                const low = self.bus.read(effective_zp);
                const high = self.bus.read((effective_zp +% 1) & 0xFF);
                const effective_addr = (@as(u16, high) << 8) | low;

                const value = self.bus.read(effective_addr);
                self.a ^= value;

                self.setZeroNegative(self.a);
                self.empty_cycles = 5;
            },
            0x44 => { // DEC (Decrement memory)
                self.readInstruction();

                const data = self.bus.read(self.opcode);
                self.bus.write(self.opcode, data-1);
                self.setZeroNegative(data-1); // TODO: Verify this

                self.empty_cycles = 4;
            },
            0x45 => { // EOR Zero Page
                const zp_addr = self.getByte();
                const value = self.bus.read(zp_addr);
                self.a ^= value;

                self.setZeroNegative(self.a);
                self.empty_cycles = 2;
            },
            0x46 => { // LSR Zero Page
                const zp_addr = self.getByte();
                const value = self.bus.read(zp_addr);
                const carry_out = (value & 0x01) != 0;

                const result = value >> 1;
                self.bus.write(zp_addr, result);

                self.carryBit(carry_out);
                self.zeroBit(result == 0);
                self.negativeBit(false);
                self.empty_cycles = 4;
            },
            0x48 => { // PHA - Push Accumulator
                self.pushStack(self.a);
                self.empty_cycles = 2;
            },
            0x49 => { // EOR Immediate
                const value = self.bus.read(self.pc);
                self.pcIncrement(1);

                self.a ^= value;

                self.setZeroNegative(self.a);
                self.empty_cycles = 1;
            },
            0x4A => { // LSR Accumulator
                const carry_out = (self.a & 0x01) != 0;
                self.a >>= 1;

                self.carryBit(carry_out);
                self.zeroBit(self.a == 0);
                self.negativeBit(false);
                self.empty_cycles = 1;
            },
            0x4C => { // JMP (Absolute Jump)
                self.pc = self.getWord();
                self.empty_cycles = 1;
            },
            0x4D => { // EOR Absolute
                const addr = self.getWord();
                const value = self.bus.read(addr);
                self.a ^= value;

                self.setZeroNegative(self.a);
                self.empty_cycles = 3;
            },
            0x4E => { // LSR Absolute
                const addr = self.getWord();
                const value = self.bus.read(addr);
                const carry_out = (value & 0x01) != 0;

                const result = value >> 1;
                self.bus.write(addr, result);

                self.carryBit(carry_out);
                self.zeroBit(result == 0);
                self.negativeBit(false);
                self.empty_cycles = 5;
            },
            0x50 => { // BVC - Branch if Overflow Clear
                const offset: i8 = @bitCast(self.bus.read(self.pc)); // Signed offset
                self.pcIncrement(1);

                if ((self.status & 0x40) == 0) { // Overflow flag (bit 6) clear
                    const wide: i32 = self.pc;
                    self.pc +%= @intCast(wide + offset);
                    self.empty_cycles = 3 + samePage(wide, self.pc);
                } else {
                    self.empty_cycles = 1;
                }
            },
            0x51 => { // EOR (Indirect,Y)
                const zp_addr = self.getByte();
                const low = self.bus.read(zp_addr);
                const high = self.bus.read((zp_addr +% 1) & 0xFF);
                const base_addr = (@as(u16, high) << 8) | low;
                const effective_addr = base_addr +% @as(u16, self.y);

                const value = self.bus.read(effective_addr);
                self.a ^= value;

                self.setZeroNegative(self.a);
                self.empty_cycles = 4 + samePage(base_addr, effective_addr);
            },
            0x55 => { // EOR Zero Page,X
                const zp_addr = self.getByte();
                const effective_addr = (zp_addr +% self.x) & 0xFF;
                const value = self.bus.read(effective_addr);
                self.a ^= value;

                self.setZeroNegative(self.a);
                self.empty_cycles = 3;
            },
            0x56 => { // LSR Zero Page,X
                const zp_addr = self.getByte();
                const effective_addr = (zp_addr +% self.x) & 0xFF;
                const value = self.bus.read(effective_addr);
                const carry_out = (value & 0x01) != 0;

                const result = value >> 1;
                self.bus.write(effective_addr, result);

                self.carryBit(carry_out);
                self.zeroBit(result == 0);
                self.negativeBit(false);
                self.empty_cycles = 5;
            },
            0x58 => { // CLI (Clear Interrupt Disable)
                self.interruptBit(false);
            },
            0x59 => { // EOR Absolute,Y
                const base_addr = self.getWord();
                const effective_addr = base_addr +% @as(u16, self.y);

                const value = self.bus.read(effective_addr);
                self.a ^= value;

                self.setZeroNegative(self.a);
                self.empty_cycles = 3 + samePage(base_addr, effective_addr);
            },
            0x5D => { // EOR Absolute,X
                const base_addr = self.getWord();
                const effective_addr = base_addr +% @as(u16, self.x);

                const value = self.bus.read(effective_addr);
                self.a ^= value;

                self.setZeroNegative(self.a);
                self.empty_cycles = 3 + samePage(base_addr, effective_addr);
            },
            0x5E => { // LSR Absolute,X
                const base_addr = self.getWord();
                const effective_addr = base_addr +% @as(u16, self.x);

                const value = self.bus.read(effective_addr);
                const carry_out = (value & 0x01) != 0;

                const result = value >> 1;
                self.bus.write(effective_addr, result);

                self.carryBit(carry_out);
                self.zeroBit(result == 0);
                self.negativeBit(false);
                self.empty_cycles = 6;
            },
            0x60 => { // RTS - Return from Subroutine
                self.sp +%= 1;
                const pc_low = self.bus.read(0x0100 + @as(u16, self.sp));
                self.sp +%= 1;
                const pc_high = self.bus.read(0x0100 + @as(u16, self.sp));

                self.pc = ((@as(u16, pc_high) << 8) | pc_low) +% 1;

                self.empty_cycles = 5;
            },
            0x61 => { // ADC (Indirect,X)
                const zp_addr = self.getByte();
                const effective_zp = (zp_addr +% self.x) & 0xFF;
                const low = self.bus.read(effective_zp);
                const high = self.bus.read((effective_zp +% 1) & 0xFF);
                const effective_addr = (@as(u16, high) << 8) | low;

                const operand = self.bus.read(effective_addr);
                const carry = (self.status & 0x01) != 0;
                const a = self.a;
                const carry_val: u8 = if (carry) 1 else 0;
                const result = a +% operand +% carry_val;
                self.a = result;

                self.carryBit(@as(u16, a) + @as(u16, operand) + @as(u16, carry_val) > 0xFF);
                self.setZeroNegative(result);

                const overflow = ((a ^ result) & (operand ^ result) & 0x80) != 0;
                self.overflowBit(overflow);

                self.empty_cycles = 5;
            },
            0x65 => { // ADC Zero Page
                const zp_addr = self.getByte();
                const operand = self.bus.read(zp_addr);
                const carry = (self.status & 0x01) != 0;
                const a = self.a;
                const carry_val: u8 = if (carry) 1 else 0;
                const result = a +% operand +% carry_val;
                self.a = result;

                self.carryBit(@as(u16, a) + @as(u16, operand) + @as(u16, carry_val) > 0xFF);
                self.setZeroNegative(result);

                const overflow = ((a ^ result) & (operand ^ result) & 0x80) != 0;
                self.overflowBit(overflow);
                self.empty_cycles = 2;
            },
            0x66 => { // ROR Zero Page
                const zp_addr = self.getByte();
                const value = self.bus.read(zp_addr);

                const carry_in = (self.status & 0x01) != 0;
                const carry_out = (value & 0x01) != 0;
                const carry_bit: u8 = if (carry_in) 0x80 else 0;

                const result = (value >> 1) | carry_bit;
                self.bus.write(zp_addr, result);

                self.carryBit(carry_out);
                self.setZeroNegative(result);

                self.empty_cycles = 4;
            },
            0x68 => { // PLA - Pull Accumulator
                self.sp +%= 1;
                self.a = self.bus.read(0x0100 + @as(u16, self.sp));
                self.setZeroNegative(self.a);
                self.empty_cycles = 3;
            },
            0x69 => { // ADC Immediate
                const operand = self.bus.read(self.pc);
                self.pcIncrement(1);

                const carry = (self.status & 0x01) != 0;
                const a = self.a;
                const carry_val: u8 = if (carry) 1 else 0; // Explicitly u8
                const result = a +% operand +% carry_val;
                self.a = result;

                self.carryBit(@as(u16, a) + @as(u16, operand) + @as(u16, carry_val) > 0xFF);
                self.setZeroNegative(result);

                const overflow = ((a ^ result) & (operand ^ result) & 0x80) != 0;
                self.overflowBit(overflow);
                self.empty_cycles = 1;
            },
            0x6A => { // ROR Accumulator
                const carry_in = (self.status & 0x01) != 0;
                const carry_out = (self.a & 0x01) != 0;
                const carry_bit: u8 = if (carry_in) 0x80 else 0;
                self.a = (self.a >> 1) | carry_bit;

                self.carryBit(carry_out);
                self.setZeroNegative(self.a);

                self.empty_cycles = 1;
            },
            0x6C => { // Indirect Jump
                // Indirect Mode (0x6C): Jumps to the address stored at the specified memory location.
                // Useful for dynamic jumps (e.g., jump tables), but it has a known bug on the original 6502: if the low byte is at $xxFF (e.g., $12FF),
                // the high byte is incorrectly fetched from $1200 instead of $1300. This quirk is present in the 6507 too.
                //
                const indirect_addr = self.getWord();
                const page = indirect_addr & 0xFF00;
                const offset = indirect_addr & 0x00FF;

                // Read the target address with bug emulation
                const target_low = self.bus.read(indirect_addr);
                const target_high = self.bus.read(page | ((offset + 1) & 0xFF));
                // TODO: Wrap ?
                // If offset = 0xFF, (offset + 1) & 0xFF = 0x00, so it reads from page start
                self.pc = (@as(u16, target_high) << 8) | target_low;
                self.empty_cycles = 4;
            },
            0x6D => { // ADC Absolute
                const addr = self.getWord();
                const operand = self.bus.read(addr);
                const carry = (self.status & 0x01) != 0;
                const a = self.a;
                const carry_val: u8 = if (carry) 1 else 0;
                const result = a +% operand +% carry_val;
                self.a = result;

                self.carryBit(@as(u16, a) + @as(u16, operand) + @as(u16, carry_val) > 0xFF);
                self.setZeroNegative(result);

                const overflow = ((a ^ result) & (operand ^ result) & 0x80) != 0;
                self.overflowBit(overflow);
                self.empty_cycles = 3;
            },
            0x6E => { // ROR Absolute
                const addr = self.getWord();
                const value = self.bus.read(addr);
                const carry_in = (self.status & 0x01) != 0;
                const carry_out = (value & 0x01) != 0;
                const carry_bit: u8 = if (carry_in) 0x80 else 0;

                const result = (value >> 1) | carry_bit;
                self.bus.write(addr, result);

                self.carryBit(carry_out);
                self.setZeroNegative(result);
                self.empty_cycles = 5;
            },
            0x70 => { // BVS - Branch if Overflow Set
                const offset: i8 = @bitCast(self.bus.read(self.pc));
                self.pcIncrement(1);

                if ((self.status & 0x40) != 0) { // Overflow flag (bit 6) set
                    const wide: i32 = self.pc;
                    self.pc +%= @intCast(wide + offset);
                    self.empty_cycles = 2 + samePage(wide, self.pc);
                } else {
                    self.empty_cycles = 1;
                }
            },
            0x71 => { // ADC (Indirect,Y)
                const zp_addr = self.getByte();
                const low = self.bus.read(zp_addr);
                const high = self.bus.read((zp_addr +% 1) & 0xFF);
                const base_addr = (@as(u16, high) << 8) | low;
                const effective_addr = base_addr +% @as(u16, self.y);

                const operand = self.bus.read(effective_addr);
                const carry = (self.status & 0x01) != 0;
                const a = self.a;
                const carry_val: u8 = if (carry) 1 else 0;
                const result = a +% operand +% carry_val;
                self.a = result;

                self.carryBit(@as(u16, a) + @as(u16, operand) + @as(u16, carry_val) > 0xFF);
                self.setZeroNegative(result);

                const overflow = ((a ^ result) & (operand ^ result) & 0x80) != 0;
                self.overflowBit(overflow);
                self.empty_cycles = 4 + samePage(base_addr, effective_addr);
            },
            0x75 => { // ADC Zero Page,X
                const zp_addr = self.getByte();
                const effective_addr = (zp_addr +% self.x) & 0xFF;
                const operand = self.bus.read(effective_addr);
                const carry = (self.status & 0x01) != 0;
                const a = self.a;
                const carry_val: u8 = if (carry) 1 else 0;
                const result = a +% operand +% carry_val;
                self.a = result;

                self.carryBit(@as(u16, a) + @as(u16, operand) + @as(u16, carry_val) > 0xFF);
                self.setZeroNegative(result);

                const overflow = ((a ^ result) & (operand ^ result) & 0x80) != 0;
                self.overflowBit(overflow);
                self.empty_cycles = 3;
            },
            0x76 => { // ROR Zero Page,X
                const zp_addr = self.getByte();
                const effective_addr = (zp_addr +% self.x) & 0xFF;
                const value = self.bus.read(effective_addr);
                const carry_in = (self.status & 0x01) != 0;
                const carry_out = (value & 0x01) != 0;
                const carry_bit: u8 = if (carry_in) 0x80 else 0;

                const result = (value >> 1) | carry_bit;
                self.bus.write(effective_addr, result);

                self.carryBit(carry_out);
                self.setZeroNegative(result);
                self.empty_cycles = 5;
            },
            0x78 => { // SEI
                self.status |= 0x04; // Set interrupt disable flag (bit 2)
                self.empty_cycles = 1;
            },
            0x79 => { // ADC Absolute,Y
                const base_addr = self.getWord();
                const effective_addr = base_addr +% @as(u16, self.y);

                const operand = self.bus.read(effective_addr);
                const carry = (self.status & 0x01) != 0;
                const a = self.a;
                const carry_val: u8 = if (carry) 1 else 0;
                const result = a +% operand +% carry_val;
                self.a = result;

                self.carryBit(@as(u16, a) + @as(u16, operand) + @as(u16, carry_val) > 0xFF);
                self.setZeroNegative(result);

                const overflow = ((a ^ result) & (operand ^ result) & 0x80) != 0;
                self.overflowBit(overflow);
                self.empty_cycles = 3 + samePage(base_addr, effective_addr);
            },
            0x7D => { // ADC Absolute,X
                const base_addr = self.getWord();
                const effective_addr = base_addr +% @as(u16, self.x);

                const operand = self.bus.read(effective_addr);
                const carry = (self.status & 0x01) != 0;
                const a = self.a;
                const carry_val: u8 = if (carry) 1 else 0;
                const result = a +% operand +% carry_val;
                self.a = result;

                self.carryBit(@as(u16, a) + @as(u16, operand) + @as(u16, carry_val) > 0xFF);
                self.setZeroNegative(result);

                const overflow = ((a ^ result) & (operand ^ result) & 0x80) != 0;
                self.overflowBit(overflow);
                self.empty_cycles = 3 + samePage(base_addr, effective_addr);
            },
            0x7E => { // ROR Absolute,X
                const base_addr = self.getWord();
                const effective_addr = base_addr +% @as(u16, self.x);

                const value = self.bus.read(effective_addr);
                const carry_in = (self.status & 0x01) != 0;
                const carry_out = (value & 0x01) != 0;
                const carry_bit: u8 = if (carry_in) 0x80 else 0;

                const result = (value >> 1) | carry_bit;
                self.bus.write(effective_addr, result);

                self.carryBit(carry_out);
                self.setZeroNegative(result);
                self.empty_cycles = 6;
            },
            0x81 => { // STA (Indirect,X)
                const zp_addr = self.getByte();
                const effective_zp = (zp_addr +% self.x) & 0xFF;
                const low = self.bus.read(effective_zp);
                const high = self.bus.read((effective_zp +% 1) & 0xFF);
                const effective_addr = (@as(u16, high) << 8) | low;

                self.bus.write(effective_addr, self.a);
                self.empty_cycles = 5;
            },
            0x84 => { // STY (Store Index Register Y In Memory)
                const addr = self.bus.read(self.pc);
                self.pcIncrement(1);
                self.bus.write(addr, self.y);
                self.empty_cycles = 2;
            },
            0x85 => { // STA (Store Accumulator)
                const addr = self.bus.read(self.pc);
                self.pcIncrement(1);
                self.bus.write(addr, self.a);
                self.empty_cycles = 2;
            },
            0x86 => { // STX Zero Page
                const zp_addr = self.getByte();
                self.bus.write(zp_addr, self.x);
                self.empty_cycles = 2;
            },
            0x88 => { // DEY - Decrement Y
                self.y -%= 1;
                self.setZeroNegative(self.y);
                self.empty_cycles = 1;
            },
            0x8A => { // TXA - Transfer X to Accumulator
                self.a = self.x;
                self.setZeroNegative(self.a);
                self.empty_cycles = 1;
            },
            0x8C => { // STY Absolute
                const addr = self.getWord();
                self.bus.write(addr, self.y);
                self.empty_cycles = 3;
            },
            0x8D => { // STA Absolute
                const addr = self.getWord();
                self.bus.write(addr, self.a);
                self.empty_cycles = 3;
            },
            0x8E => { // STX Absolute
                const addr = self.getWord();
                self.bus.write(addr, self.x);
                self.empty_cycles = 3;
            },
            0x90 => { // BCC - Branch if Carry Clear
                const offset: i8 = @bitCast(self.bus.read(self.pc));
                self.pcIncrement(1);

                if ((self.status & 0x01) == 0) { // Carry flag clear
                    const wide: i32 = self.pc;
                    self.pc +%= @intCast(wide + offset);
                    self.empty_cycles = 2 + samePage(wide, self.pc);
                } else {
                    self.empty_cycles = 1;
                }
            },
            0x91 => { // STA (Indirect,Y)
                const zp_addr = self.getByte();
                const low = self.bus.read(zp_addr);
                const high = self.bus.read((zp_addr +% 1) & 0xFF);
                const base_addr = (@as(u16, high) << 8) | low;
                const effective_addr = base_addr +% @as(u16, self.y);
                self.bus.write(effective_addr, self.a);

                self.empty_cycles = 5;
            },
            0x94 => { // STY Zero Page, X
                const zp_addr = self.getByte();
                const effective_addr = zp_addr +% self.x;
                self.bus.write(effective_addr, self.y);
                self.empty_cycles = 3;
            },
            0x95 => { // STA (Store Accumulator, X)
                const base_addr = self.getByte();
                const addr = @addWithOverflow(base_addr, self.x);
                self.bus.write(addr[0], self.a);
                self.empty_cycles = 3;
            },
            0x96 => { // STX Zero Page,Y
                const zp_addr = self.getByte();
                const effective_addr = (zp_addr +% self.y) & 0xFF;
                self.bus.write(effective_addr, self.x);
                self.empty_cycles = 3;
            },
            0x98 => { // TYA - Transfer Y to Accumulator
                self.a = self.y;
                self.setZeroNegative(self.a);

                self.empty_cycles = 1;
            },
            0x99 => { // STA Absolute,Y
                const base_addr = self.getWord();
                const effective_addr = base_addr +% @as(u16, self.y);
                self.bus.write(effective_addr, self.a);
                self.empty_cycles = 4;
            },
            0x9A => { // TXS - Transfer X to Stack Pointer
                self.sp = self.x;
                self.empty_cycles = 1;
            },
            0x9D => { // STA Absolute,X
                const base_addr = self.getWord();
                const effective_addr = base_addr +% @as(u16, self.x);
                self.bus.write(effective_addr, self.a);
                self.empty_cycles = 4;
            },
            0xA0 => { // LDY Immediate
                const value = self.bus.read(self.pc);
                self.pcIncrement(1);
                self.y = value;
                self.setZeroNegative(value);
                self.empty_cycles = 1;
            },
            0xA1 => { // LDA (Indirect,X)
                const zp_addr = self.getByte();
                const effective_zp = (zp_addr +% self.x) & 0xFF;
                const low = self.bus.read(effective_zp);
                const high = self.bus.read((effective_zp +% 1) & 0xFF);
                const effective_addr = (@as(u16, high) << 8) | low;

                self.a = self.bus.read(effective_addr);
                self.setZeroNegative(self.a);
                self.empty_cycles = 5;
            },
            0xA2 => { // LDX (Load X register Immidate)
                self.readInstruction();
                self.x = @intCast(self.opcode);
                self.setZeroNegative(self.x);
                self.empty_cycles = 1;
            },
            0xA4 => { // LDY Zero Page
                const zp_addr = self.getByte();
                self.y = self.bus.read(zp_addr);
                self.setZeroNegative(self.y);
                self.empty_cycles = 2;
            },
            0xA5 => { // LDA (Load Accumulator ZeroPage)
                const addr = self.bus.read(self.pc);
                self.pcIncrement(1);

                self.a = self.bus.read(addr);
                self.setZeroNegative(self.a);
                self.empty_cycles = 2;
            },
            0xA6 => { // LDX (Load Index Register X from Memory)
                const addr = self.bus.read(self.pc);
                self.pcIncrement(1);
                self.x = self.bus.read(addr);
                self.setZeroNegative(self.x);
                self.empty_cycles = 2;
            },
            0xA8 => { // TAY - Transfer Accumulator to Y
                self.y = self.a;
                self.setZeroNegative(self.y);
                self.empty_cycles = 1;
            },
            0xA9 => { // LDA Immediate
                self.a = self.bus.read(self.pc);
                self.pcIncrement(1);
                self.setZeroNegative(self.a);
                self.empty_cycles = 1;
            },
            0xAA => { // TAX - Transfer Accumulator to X
                self.x = self.a;
                self.setZeroNegative(self.x);
                self.empty_cycles = 1;
            },
            0xAC => { // LDY Absolute
                const addr = self.getWord();
                const value = self.bus.read(addr);
                self.y = value;
                self.setZeroNegative(value);
                self.empty_cycles = 3;
            },
            0xAD => { // LDA Absolute
                const addr = self.getWord();
                const value = self.bus.read(addr);
                self.a = value;
                self.setZeroNegative(value);
                self.empty_cycles = 3;
            },
            0xAE => { // LDX Absolute
                const addr = self.getWord();
                self.x = self.bus.read(addr);
                self.setZeroNegative(self.x);
                self.empty_cycles = 3;
            },
            0xB0 => { // BCS - Branch if Carry Set
                const offset: i8 = @bitCast(self.bus.read(self.pc));
                self.pcIncrement(1);

                if ((self.status & 0x01) != 0) { // Carry flag set
                    const wide: i32 = self.pc;
                    self.pc = @intCast(wide + offset);
                    self.empty_cycles = 2 + samePage(wide, self.pc);
                } else {
                    self.empty_cycles = 1;
                }
            },
            0xB1 => { // LDA (Indirect,Y)
                const zp_addr = self.getByte();
                const low = self.bus.read(zp_addr);
                const high = self.bus.read((zp_addr +% 1) & 0xFF);
                const base_addr = (@as(u16, high) << 8) | low;
                const effective_addr = base_addr +% @as(u16, self.y);

                self.a = self.bus.read(effective_addr);
                self.setZeroNegative(self.a);
                self.empty_cycles = 4 + samePage(base_addr, effective_addr);
            },
            0xB4 => { // LDY Zero Page,X
                const zp_addr = self.getByte();
                const effective_addr = (zp_addr +% self.x) & 0xFF;
                self.y = self.bus.read(effective_addr);
                self.setZeroNegative(self.y);
                self.empty_cycles = 3;
            },
            0xB5 => { // LDA Zero Page,X
                const zp_addr = self.getByte();
                const effective_addr = (zp_addr +% self.x) & 0xFF;
                self.a = self.bus.read(effective_addr);
                self.setZeroNegative(self.a);
                self.empty_cycles = 3;
            },
            0xB6 => { // LDX Zero Page,Y
                const zp_addr = self.getByte();
                const effective_addr = (zp_addr +% self.y) & 0xFF;
                self.x = self.bus.read(effective_addr);
                self.setZeroNegative(self.x);
                self.empty_cycles = 3;
            },
            0xB8 => { // CLV (Clear Overflow)
                self.overflowBit(false);
            },
            0xB9 => { // LDA Absolute,Y
                const base_addr = self.getWord();
                const effective_addr = base_addr +% @as(u16, self.y);

                self.a = self.bus.read(effective_addr);
                self.setZeroNegative(self.a);
                self.empty_cycles = 3 + samePage(base_addr, effective_addr);
            },
            0xBA => { // TSX - Transfer Stack Pointer to X
                self.x = self.sp;
                self.setZeroNegative(self.x);
                self.empty_cycles = 1;
            },
            0xBC => { // LDY Absolute,X
                const base_addr = self.getWord();
                const effective_addr = base_addr +% @as(u16, self.x);

                self.y = self.bus.read(effective_addr);
                self.setZeroNegative(self.y);
                self.empty_cycles = 3 + samePage(base_addr, effective_addr);
            },
            0xBD => { // LDA Absolute,X
                const low = self.bus.read(self.pc);
                self.pcIncrement(1);
                const high = self.bus.read(self.pc);
                self.pcIncrement(1);
                // Form the 16-bit base address (little-endian)
                const base_addr = (@as(u16, high) << 8) | low;
                const effective_addr = base_addr + @as(u16, self.x);

                const value = self.bus.read(effective_addr);
                self.a = value;
                self.setZeroNegative(value);

                const temp = @as(u16, low) + @as(u16, self.x);
                if (temp > 0xFF)
                    self.empty_cycles = 4
                 else
                    self.empty_cycles = 3;
            },
            0xBE => { // LDX Absolute,Y
                const low = self.bus.read(self.pc);
                self.pcIncrement(1);
                const high = self.bus.read(self.pc);
                self.pcIncrement(1);

                const base_addr = (@as(u16, high) << 8) | low;
                const effective_addr = base_addr +% @as(u16, self.y);

                self.x = self.bus.read(effective_addr);
                self.setZeroNegative(self.x);

                const temp = @as(u16, low) + @as(u16, self.x);
                if (temp > 0xFF)
                    self.empty_cycles = 4
                 else
                    self.empty_cycles = 3;
            },
            0xC0 => { // CPY - Compare Y Immediate
                const operand = self.bus.read(self.pc);
                self.pcIncrement(1);

                const result = self.y -% operand;

                self.carryBit(self.y >= operand);
                self.setZeroNegative(result);
                self.empty_cycles = 1;
            },
            0xC1 => { // CMP (Indirect,X)
                const zp_addr = self.getByte();
                const effective_zp = (zp_addr +% self.x) & 0xFF;
                const low = self.bus.read(effective_zp);
                const high = self.bus.read((effective_zp +% 1) & 0xFF);
                const effective_addr = (@as(u16, high) << 8) | low;

                const value = self.bus.read(effective_addr);
                const result = self.a -% value;

                self.carryBit(self.a >= value);
                self.setZeroNegative(result);
                self.empty_cycles = 5;
            },
            0xC4 => { // CPY Zero Page
                const zp_addr = self.bus.read(self.pc);
                const value = self.bus.read(zp_addr);
                const result = self.y -% value;

                self.carryBit(self.y >= value);
                self.setZeroNegative(result);
                self.empty_cycles = 2;
            },
            0xC5 => { // CMP Zero Page
                const zp_addr = self.bus.read(self.pc);
                const value = self.bus.read(zp_addr);
                const result = self.a -% value;

                self.carryBit(self.a >= value);
                self.setZeroNegative(result);
                self.empty_cycles = 2;
            },
            0xC6 => { // DEC Zero Page (replacing 0x44)
                const addr = self.bus.read(self.pc);
                const value = self.bus.read(addr);
                const result = value - 1;
                self.bus.write(addr, result);

                self.zeroBit(result == 0);
                self.negativeBit((result & 0x80) != 0);
                self.empty_cycles = 4;
            },
            0xC8 => { // INY (Increment Y)
                self.y +%= 1;
                self.zeroBit(self.y == 0);
                self.negativeBit((self.y & 0x80) != 0);
                self.empty_cycles = 1;
            },
            0xC9 => { // CMP Immediate
                const value = self.bus.read(self.pc);
                const result = self.a -% value;

                self.carryBit(self.a >= value);
                self.setZeroNegative(result);
                self.empty_cycles = 1;
            },
            0xCA => { // DEX (Decremetn X)
                self.x -%= 1;
                self.zeroBit(self.x == 0);
                self.negativeBit((self.x & 0x80) != 0);
                self.empty_cycles = 1;
            },
            0xCC => { // CPY Absolute
                const addr = self.getWord();
                const value = self.bus.read(addr);
                const result = self.y -% value;

                self.carryBit(self.y >= value);
                self.setZeroNegative(result);
                self.empty_cycles = 3;
            },
            0xCD => { // CMP Absolute
                const addr = self.getWord();
                const value = self.bus.read(addr);
                const result = self.a -% value;

                self.carryBit(self.a >= value);
                self.setZeroNegative(result);
                self.empty_cycles = 3;
            },
            0xCE => { // DEC Absolute
                const addr = self.getWord();
                const value = self.bus.read(addr);
                const result = value -% 1;

                self.bus.write(addr, result);
                self.setZeroNegative(result);
                self.empty_cycles = 5;
            },
            0xD0 => { // BNE - Branch if Not Equal
                const offset: u8 = @bitCast(self.bus.read(self.pc));
                self.pcIncrement(1);

                // Check if the Zero Flag is clear
                if ((self.status & ZeroFlag) == 0) {
                    const old_pc: u16 = self.pc; // Store current PC before branching
                    const new_pc: u16 = self.pc + offset;

                    self.pc = new_pc; // Update PC to the new address
                    std.debug.print("[D] OPC: 0x{X} NPC: 0x{X} OF:0x{X}\n", .{old_pc, self.pc, offset});
                    self.empty_cycles = 2 + samePage(old_pc, self.pc);
                } else {
                    self.empty_cycles = 1;
                }
            },
            0xD1 => { // CMP (Indirect,Y)
                const zp_addr = self.bus.read(self.pc);
                const low = self.bus.read(zp_addr);
                const high = self.bus.read((zp_addr +% 1) & 0xFF);
                const base_addr = (@as(u16, high) << 8) | low;
                const effective_addr = base_addr +% @as(u16, self.y);

                const value = self.bus.read(effective_addr);
                const result = self.a -% value;

                self.carryBit(self.a >= value);
                self.setZeroNegative(result);
                self.empty_cycles = 4 + samePage(base_addr, effective_addr);
            },
            0xD5 => { // CMP Zero Page,X
                const zp_addr = self.bus.read(self.pc);
                const effective_addr = (zp_addr +% self.x) & 0xFF;
                const value = self.bus.read(effective_addr);
                const result = self.a -% value;

                self.carryBit(self.a >= value);
                self.setZeroNegative(result);
                self.empty_cycles = 3;
            },
            0xD6 => { // DEC Zero Page,X
                const zp_addr = self.bus.read(self.pc);
                const effective_addr = (zp_addr +% self.x) & 0xFF;
                const value = self.bus.read(effective_addr);
                const result = value -% 1;

                self.bus.write(effective_addr, result);
                self.setZeroNegative(result);
                self.empty_cycles = 5;
            },
            0xD8 => { // CLD (Clear Decimal)
                self.decimalBit(false);
            },
            0xD9 => { // CMP Absolute,Y
                const base_addr = self.getWord();
                const effective_addr = base_addr +% @as(u16, self.y);

                const value = self.bus.read(effective_addr);
                const result = self.a -% value;

                self.carryBit(self.a >= value);
                self.setZeroNegative(result);
                self.empty_cycles = 3 + samePage(base_addr, effective_addr);
            },
            0xDD => { // CMP Absolute,X
                const base_addr = self.getWord();
                const effective_addr = base_addr +% @as(u16, self.x);

                const value = self.bus.read(effective_addr);
                const result = self.a -% value;

                self.carryBit(self.a >= value);
                self.setZeroNegative(result);
                self.empty_cycles = 3 + samePage(base_addr, effective_addr);
            },
            0xDE => { // DEC Absolute,X
                const base_addr = self.getWord();
                const effective_addr = base_addr +% @as(u16, self.x);

                const value = self.bus.read(effective_addr);
                const result = value -% 1;

                self.bus.write(effective_addr, result);
                self.setZeroNegative(result);
                self.empty_cycles = 6;
            },
            0xE0 => { // CPX Immediate
                const value = self.bus.read(self.pc);
                self.pcIncrement(1);

                const result = self.x -% value;

                self.carryBit(self.x >= value);
                self.setZeroNegative(result);
                self.empty_cycles = 1;
            },
            0xE1 => { // SBC (Indirect,X)
                const zp_addr = self.bus.read(self.pc);
                const low = self.bus.read(zp_addr);
                const high = self.bus.read((zp_addr +% 1) & 0xFF);
                const base_addr = (@as(u16, high) << 8) | low;
                const effective_addr = base_addr +% @as(u16, self.x);

                const operand = self.bus.read(effective_addr);
                const carry = (self.status & 0x01) != 0;
                const a = self.a;
                const borrow: u8 = if (carry) 0 else 1;
                const result = a -% operand -% borrow;
                self.a = result;

                self.carryBit(a >= (operand +% borrow));
                self.setZeroNegative(result);

                const overflow = ((a ^ result) & (a ^ operand) & 0x80) != 0;
                self.overflowBit(overflow);
                self.empty_cycles = 4 + samePage(base_addr, effective_addr);
            },
            0xE4 => { // CPX Zero Page
                const zp_addr = self.bus.read(self.pc);
                const value = self.bus.read(zp_addr);
                const result = self.x -% value;

                self.carryBit(self.x >= value);
                self.setZeroNegative(result);
                self.empty_cycles = 2;
            },
            0xE5 => { // SBC Zero Page
                const zp_addr = self.bus.read(self.pc);
                const operand = self.bus.read(zp_addr);
                const carry = (self.status & 0x01) != 0;
                const a = self.a;
                const borrow: u8 = if (carry) 0 else 1;
                const result = a -% operand -% borrow;

                self.a = result;
                self.carryBit(a >= (operand +% borrow));
                self.setZeroNegative(result);

                const overflow = ((a ^ result) & (a ^ operand) & 0x80) != 0;
                self.overflowBit(overflow);
                self.empty_cycles = 2;
            },
            0xE6 => { // INC Zero Page
                const zp_addr = self.bus.read(self.pc);
                const value = self.bus.read(zp_addr);
                const result = value +% 1;

                self.bus.write(zp_addr, result);
                self.setZeroNegative(result);
                self.empty_cycles = 4;
            },
            0xE8 => { // INX (Increment X)
                self.x +%= 1;

                self.zeroBit(self.x == 0);
                self.negativeBit((self.x & 0x80) != 0);
                self.empty_cycles = 1;
            },
            0xE9 => { // SBC Immediate
                const operand = self.bus.read(self.pc);
                self.pcIncrement(1);

                const carry = (self.status & 0x01) != 0;
                const a = self.a;
                const borrow: u8 = if (carry) 0 else 1;
                const result = a -% operand -% borrow;

                self.a = result;
                self.setZeroNegative(result);
                self.carryBit(a >= (operand +% borrow));

                // Overflow: (A positive, operand negative) or (A negative, operand positive) crossing zero
                const overflow = ((a ^ result) & (a ^ operand) & 0x80) != 0;
                self.overflowBit(overflow);
                self.empty_cycles = 1;
            },
            0xEA => { // NOP
                self.empty_cycles = 1;
            },
            0xEC => { // CPX Absolute
                const addr = self.getWord();
                const value = self.bus.read(addr);
                const result = self.x -% value;

                self.carryBit(self.x >= value);
                self.setZeroNegative(result);
                self.empty_cycles = 3;
            },
            0xED => { // SBC Absolute
                const addr = self.getWord();
                const operand = self.bus.read(addr);
                const carry = (self.status & 0x01) != 0;
                const a = self.a;
                const borrow: u8 = if (carry) 0 else 1;
                const result = a -% operand -% borrow;

                self.a = result;
                self.carryBit(a >= (operand +% borrow));
                self.setZeroNegative(result);

                const overflow = ((a ^ result) & (a ^ operand) & 0x80) != 0;
                self.overflowBit(overflow);
                self.empty_cycles = 3;
            },
            0xEE => { // INC Absolute
                const addr = self.getWord();
                const value = self.bus.read(addr);
                const new_value = value +% 1;

                self.bus.write(addr , new_value);
                self.setZeroNegative(new_value);
                self.empty_cycles = 6;
            },
            0xF0 => { // BEQ - Branch if Equal
                const offset: i8 = @bitCast(self.bus.read(self.pc));
                self.pcIncrement(1);

                if ((self.status & 0x02) != 0) { // Zero flag set
                    const wide: i32 = self.pc;
                    self.pc +%= @intCast(wide + offset);
                    self.empty_cycles = 2 + samePage(wide, self.pc);
                } else {
                    self.empty_cycles = 1;
                }
            },
            0xF1 => { // SBC (Indirect,Y)
                const zp_addr = self.bus.read(self.pc);
                self.pcIncrement(1);

                const low = self.bus.read(zp_addr);
                const high = self.bus.read((zp_addr +% 1) & 0xFF);
                const base_addr = (@as(u16, high) << 8) | low;
                const effective_addr = base_addr +% @as(u16, self.y);

                const operand = self.bus.read(effective_addr);
                const carry = (self.status & 0x01) != 0;
                const a = self.a;
                const borrow: u8 = if (carry) 0 else 1;
                const result = a -% operand -% borrow;
                self.a = result;

                self.carryBit(a >= (operand +% borrow));
                self.setZeroNegative(result);

                const overflow = ((a ^ result) & (a ^ operand) & 0x80) != 0;
                self.overflowBit(overflow);
                self.empty_cycles = 4 + samePage(base_addr, effective_addr);
            },
            0xF5 => { // SBC Zero Page,X
                const zp_addr = self.bus.read(self.pc);
                self.pcIncrement(1);

                const effective_addr = (zp_addr +% self.x) & 0xFF;
                const operand = self.bus.read(effective_addr);
                const carry = (self.status & 0x01) != 0;
                const a = self.a;
                const borrow: u8 = if (carry) 0 else 1;
                const result = a -% operand -% borrow;

                self.a = result;
                self.carryBit(a >= (operand +% borrow));
                self.setZeroNegative(result);

                const overflow = ((a ^ result) & (a ^ operand) & 0x80) != 0;
                self.overflowBit(overflow);
                self.empty_cycles = 3;
            },
            0xF6 => { // INC Zero Page, X
                const zp_addr = self.bus.read(self.pc);
                self.pcIncrement(1);

                const effective_addr: u8 = @intCast(zp_addr +% self.x);
                const value = self.bus.read(effective_addr);
                const new_value = value +% 1;

                self.bus.write(effective_addr, new_value);
                self.setZeroNegative(new_value);
                self.empty_cycles = 5;
            },
            0xF8 => { // SED (Set Decimal)
                self.decimalBit(true);
            },
            0xF9 => { // SBC Absolute,Y
                const base_addr = self.getWord();
                const effective_addr = base_addr +% @as(u16, self.y);
                const operand = self.bus.read(effective_addr);
                const carry = (self.status & 0x01) != 0; // Carry flag (bit 0)
                const a = self.a;
                const borrow: u8 = if (carry) 0 else 1;
                const result = a -% operand -% borrow;

                self.a = result;
                self.carryBit(a >= (operand +% borrow));
                self.setZeroNegative(result);
                const overflow = ((a ^ result) & (a ^ operand) & 0x80) != 0;
                self.overflowBit(overflow);
                self.empty_cycles = 3 + samePage(base_addr, effective_addr);
            },
            0xFE => { // INC Absolute,X
                const base_addr = self.getWord();
                const effective_addr = base_addr +% @as(u16, self.x);
                const value = self.bus.read(effective_addr);
                const new_value = value +% 1;

                self.bus.write(effective_addr, new_value);
                self.setZeroNegative(new_value);
                self.empty_cycles = 6;
            },
            0xFD => { // SBC Absolute,X
                const base_addr = self.getWord();
                const effective_addr = base_addr +% @as(u16, self.x);
                const operand = self.bus.read(effective_addr);
                const carry = (self.status & 0x01) != 0;
                const a = self.a;
                const borrow: u8 = if (carry) 0 else 1;
                const result = a -% operand -% borrow;

                self.a = result;
                self.carryBit(a >= (operand +% borrow));
                self.setZeroNegative(result);

                const overflow = ((a ^ result) & (a ^ operand) & 0x80) != 0;
                self.overflowBit(overflow);
                self.empty_cycles = 4 + samePage(base_addr, effective_addr);
            },
            else => {
                std.debug.print("[warn] Unimplemented instruction 0x{X}\n", .{self.opcode});
            }
        }

}

    pub fn pcIncrement(self: *Self, p: u16) void {
        self.pc +%= p;
    }

    pub fn cycleIncrement(self: *Self, c: u16) void {
        self.cycles +%= c;
    }

};
