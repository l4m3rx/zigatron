const std = @import("std");
const BUS = @import("bus.zig").BUS;

const CarryFlag: u8        = 0b00000001; // Bit 0
const ZeroFlag: u8         = 0b00000010;
const InterruptDisable: u8 = 0b00000100;
const DecimalMode: u8      = 0b00001000;
const BreakCommand: u8     = 0b00010000;
const UnusedFlag: u8       = 0b00100000;
const OverflowFlag: u8     = 0b01000000;
const NegativeFlag: u8     = 0b10000000; // Bit 7

pub const CPU = struct {
    alloc: std.mem.Allocator,
    a: u8,       // Accumulator
    x: u8,       // X register
    y: u8,       // Y register
    sp: u8,      // Stack pointer
    pc: u16,     // Program counter
    status: u8,  // Status flags
    bus: *BUS,   // Bus object
    opcode: u16, // Current OPCode
    cycles: u32, // Cycles counter
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
            .empty_cycles = 0,
        };
    }

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
        std.debug.print("[D] Cycle: {d} OPCode:0x{X} PC:0x{X}\n", .{self.cycles, self.opcode, self.pc});
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

    pub fn interruptBit(self: *Self, b: bool) void {
        if (b) self.status |= InterruptDisable else self.status &= ~InterruptDisable;
    }

    pub fn overflowBit(self: *Self, b: bool) void {
        if (b) self.status |= OverflowFlag else self.status &= ~OverflowFlag;
    }

    pub fn decimalBit(self: *Self, b: bool) void {
        if (b) self.status |= DecimalMode else self.status &= ~DecimalMode;
    }

    pub fn carryBit(self: *Self, b: bool) void {
        if (b) self.status |= CarryFlag else self.status &= ~CarryFlag;
    }

    pub fn negativeBit(self: *Self, b: bool) void {
        if (b) self.status |= NegativeFlag else self.status &= ~NegativeFlag;
    }

    pub fn zeroBit(self: *Self, b: bool) void {
        if (b) self.status |= ZeroFlag else self.status &= ~ZeroFlag;
    }

    pub fn pcIncrement(self: *Self, p: u16) void {
        self.pc +%= p;
    }

    pub fn cycleIncrement(self: *Self, c: u16) void {
        self.cycles +%= c;
    }

    pub fn getImmediate(self: *Self) u8 {
        const value = self.bus.read(self.pc);
        self.pcIncrement(1);
        return value;
    }

    pub fn getZeroPageAddr(self: *Self) u8 {
        return self.getByte();
    }

    pub fn getZeroPageXAddr(self: *Self) u8 {
        const zp_addr = self.getByte();
        return (zp_addr +% self.x) & 0xFF;
    }

    pub fn getZeroPageYAddr(self: *Self) u8 {
        const zp_addr = self.getByte();
        return (zp_addr +% self.y) & 0xFF;
    }

    pub fn getAbsoluteAddr(self: *Self) u16 {
        return self.getWord();
    }

    pub fn getAbsoluteXAddr(self: *Self) struct { addr: u16, crossed: bool } {
        const base_addr = self.getWord();
        const effective_addr = base_addr +% @as(u16, self.x);
        const crossed = (base_addr & 0xFF00) != (effective_addr & 0xFF00);
        return .{ .addr = effective_addr, .crossed = crossed };
    }

    pub fn getAbsoluteYAddr(self: *Self) struct { addr: u16, crossed: bool } {
        const base_addr = self.getWord();
        const effective_addr = base_addr +% @as(u16, self.y);
        const crossed = (base_addr & 0xFF00) != (effective_addr & 0xFF00);
        return .{ .addr = effective_addr, .crossed = crossed };
    }

    pub fn getIndirectXAddr(self: *Self) u16 {
        const zp_addr = self.getByte();
        const effective_zp = (zp_addr +% self.x) & 0xFF;
        const low = self.bus.read(effective_zp);
        const high = self.bus.read((effective_zp +% 1) & 0xFF);
        return (@as(u16, high) << 8) | low;
    }

    pub fn getIndirectYAddr(self: *Self) struct { addr: u16, crossed: bool } {
        const zp_addr = self.getByte();
        const low = self.bus.read(zp_addr);
        const high = self.bus.read((zp_addr +% 1) & 0xFF);
        const base_addr = (@as(u16, high) << 8) | low;
        const effective_addr = base_addr +% @as(u16, self.y);
        const crossed = (base_addr & 0xFF00) != (effective_addr & 0xFF00);
        return .{ .addr = effective_addr, .crossed = crossed };
    }

    pub fn pageCrossed(addr1: u16, addr2: u16) u8 {
        return if ((addr1 & 0xFF00) != (addr2 & 0xFF00)) 1 else 0;
    }

    pub fn ora(self: *Self, value: u8) void {
        self.a |= value;
        self.setZeroNegative(self.a);
    }

    pub fn cand(self: *Self, value: u8) void {
        self.a &= value;
        self.setZeroNegative(self.a);
    }

    pub fn eor(self: *Self, value: u8) void {
        self.a ^= value;
        self.setZeroNegative(self.a);
    }

    pub fn adc(self: *Self, operand: u8) void {
        const carry = (self.status & CarryFlag) != 0;
        const a = self.a;
        const carry_val: u8 = if (carry) 1 else 0;
        const result = a +% operand +% carry_val;
        self.a = result;
        self.carryBit(@as(u16, a) + @as(u16, operand) + @as(u16, carry_val) > 0xFF);
        self.setZeroNegative(result);
        self.overflowBit(((a ^ result) & (operand ^ result) & 0x80) != 0);
    }

    pub fn sbc(self: *Self, operand: u8) void {
        const carry = (self.status & CarryFlag) != 0;
        const a = self.a;
        const borrow: u8 = if (carry) 0 else 1;
        const result = a -% operand -% borrow;
        self.a = result;
        self.carryBit(a >= (operand +% borrow));
        self.setZeroNegative(result);
        self.overflowBit(((a ^ result) & (a ^ operand) & 0x80) != 0);
    }

    pub fn cmp(self: *Self, value: u8) void {
        const result = self.a -% value;
        self.carryBit(self.a >= value);
        self.setZeroNegative(result);
    }

    pub fn cpx(self: *Self, value: u8) void {
        const result = self.x -% value;
        self.carryBit(self.x >= value);
        self.setZeroNegative(result);
    }

    pub fn cpy(self: *Self, value: u8) void {
        const result = self.y -% value;
        self.carryBit(self.y >= value);
        self.setZeroNegative(result);
    }

    pub fn aslMem(self: *Self, addr: u16) void {
        const value = self.bus.read(addr);
        const carry_out = (value & 0x80) != 0;
        const result = value << 1;
        self.bus.write(addr, result);
        self.carryBit(carry_out);
        self.setZeroNegative(result);
    }

    pub fn aslA(self: *Self) void {
        const carry = (self.a & 0x80) != 0;
        self.a <<= 1;
        self.carryBit(carry);
        self.setZeroNegative(self.a);
    }

    pub fn lsrMem(self: *Self, addr: u16) void {
        const value = self.bus.read(addr);
        const carry_out = (value & 0x01) != 0;
        const result = value >> 1;
        self.bus.write(addr, result);
        self.carryBit(carry_out);
        self.zeroBit(result == 0);
        self.negativeBit(false);
    }

    pub fn lsrA(self: *Self) void {
        const carry_out = (self.a & 0x01) != 0;
        self.a >>= 1;
        self.carryBit(carry_out);
        self.zeroBit(self.a == 0);
        self.negativeBit(false);
    }

    pub fn rolMem(self: *Self, addr: u16) void {
        const value = self.bus.read(addr);
        const carry_in = (self.status & CarryFlag) != 0;
        const carry_out = (value & 0x80) != 0;
        const carry_bit: u8 = if (carry_in) 1 else 0;
        const result = (value << 1) | carry_bit;
        self.bus.write(addr, result);
        self.carryBit(carry_out);
        self.setZeroNegative(result);
    }

    pub fn rolA(self: *Self) void {
        const carry_in = (self.status & CarryFlag) != 0;
        const carry_out = (self.a & 0x80) != 0;
        const carry_bit: u8 = if (carry_in) 1 else 0;
        self.a = (self.a << 1) | carry_bit;
        self.carryBit(carry_out);
        self.setZeroNegative(self.a);
    }

    pub fn rorMem(self: *Self, addr: u16) void {
        const value = self.bus.read(addr);
        const carry_in = (self.status & CarryFlag) != 0;
        const carry_out = (value & 0x01) != 0;
        const carry_bit: u8 = if (carry_in) 0x80 else 0;
        const result = (value >> 1) | carry_bit;
        self.bus.write(addr, result);
        self.carryBit(carry_out);
        self.setZeroNegative(result);
    }

    pub fn rorA(self: *Self) void {
        const carry_in = (self.status & CarryFlag) != 0;
        const carry_out = (self.a & 0x01) != 0;
        const carry_bit: u8 = if (carry_in) 0x80 else 0;
        self.a = (self.a >> 1) | carry_bit;
        self.carryBit(carry_out);
        self.setZeroNegative(self.a);
    }

    pub fn lda(self: *Self, value: u8) void {
        self.a = value;
        self.setZeroNegative(self.a);
    }

    pub fn ldx(self: *Self, value: u8) void {
        self.x = value;
        self.setZeroNegative(self.x);
    }

    pub fn ldy(self: *Self, value: u8) void {
        self.y = value;
        self.setZeroNegative(self.y);
    }

    pub fn sta(self: *Self, addr: u16) void {
        self.bus.write(addr, self.a);
    }

    pub fn stx(self: *Self, addr: u16) void {
        self.bus.write(addr, self.x);
    }

    pub fn sty(self: *Self, addr: u16) void {
        self.bus.write(addr, self.y);
    }

    pub fn decMem(self: *Self, addr: u16) void {
        const value = self.bus.read(addr);
        const result = value -% 1;
        self.bus.write(addr, result);
        self.setZeroNegative(result);
    }

    pub fn incMem(self: *Self, addr: u16) void {
        const value = self.bus.read(addr);
        const result = value +% 1;
        self.bus.write(addr, result);
        self.setZeroNegative(result);
    }

    pub fn branchIf(self: *Self, condition: bool) void {
        const offset: i8 = @bitCast(self.getByte());
        if (condition) {
            const old_pc = self.pc;
            self.pc = @intCast(@as(i32, self.pc) + offset);
            self.empty_cycles = 2 + pageCrossed(old_pc, self.pc);
        } else {
            self.empty_cycles = 1;
        }
    }

    pub fn tick(self: *Self) void {
        self.cycleIncrement(1);
        if (self.empty_cycles > 0) {
            self.empty_cycles -= 1;
            return;
        }
        self.readInstruction();

        switch (self.opcode) {
            0x00 => { // BRK
                self.pcIncrement(1);
                const return_addr = self.pc;
                self.pushStack(@intCast((return_addr >> 8) & 0xFF));
                self.pushStack(@intCast(return_addr & 0xFF));
                self.pushStack(self.status | BreakCommand);
                self.interruptBit(true);
                const low = self.bus.read(0xFFFE);
                const high = self.bus.read(0xFFFF);
                self.pc = (@as(u16, high) << 8) | low;
                self.empty_cycles = 6;
            },
            0x01 => { // ORA (Indirect,X)
                const addr = self.getIndirectXAddr();
                const value = self.bus.read(addr);
                self.ora(value);
                self.empty_cycles = 5;
            },
            0x05 => { // ORA Zero Page
                const addr = self.getZeroPageAddr();
                const value = self.bus.read(addr);
                self.ora(value);
                self.empty_cycles = 2;
            },
            0x06 => { // ASL Zero Page
                const addr = self.getZeroPageAddr();
                self.aslMem(addr);
                self.empty_cycles = 4;
            },
            0x08 => { // PHP
                self.pushStack(self.status | BreakCommand);
                self.empty_cycles = 2;
            },
            0x09 => { // ORA Immediate
                const value = self.getImmediate();
                self.ora(value);
                self.empty_cycles = 1;
            },
            0x0A => { // ASL Accumulator
                self.aslA();
                self.empty_cycles = 1;
            },
            0x0D => { // ORA Absolute
                const addr = self.getAbsoluteAddr();
                const value = self.bus.read(addr);
                self.ora(value);
                self.empty_cycles = 3;
            },
            0x0E => { // ASL Absolute
                const addr = self.getAbsoluteAddr();
                self.aslMem(addr);
                self.empty_cycles = 5;
            },
            0x10 => { // BPL
                self.branchIf((self.status & NegativeFlag) == 0);
            },
            0x11 => { // ORA (Indirect,Y)
                const result = self.getIndirectYAddr();
                const value = self.bus.read(result.addr);
                self.ora(value);
                self.empty_cycles = 4 + @as(u32, if (result.crossed) 1 else 0);
            },
            0x15 => { // ORA Zero Page,X
                const addr = self.getZeroPageXAddr();
                const value = self.bus.read(addr);
                self.ora(value);
                self.empty_cycles = 3;
            },
            0x16 => { // ASL Zero Page,X
                const addr = self.getZeroPageXAddr();
                self.aslMem(addr);
                self.empty_cycles = 5;
            },
            0x18 => { // CLC
                self.carryBit(false);
                self.empty_cycles = 1;
            },
            0x19 => { // ORA Absolute,Y
                const result = self.getAbsoluteYAddr();
                const value = self.bus.read(result.addr);
                self.ora(value);
                self.empty_cycles = 3 + @as(u32, if (result.crossed) 1 else 0);
            },
            0x1D => { // ORA Absolute,X
                const result = self.getAbsoluteXAddr();
                const value = self.bus.read(result.addr);
                self.ora(value);
                self.empty_cycles = 3 + @as(u32, if (result.crossed) 1 else 0);
            },
            0x1E => { // ASL Absolute,X
                const result = self.getAbsoluteXAddr();
                self.aslMem(result.addr);
                self.empty_cycles = 6;
            },
            0x20 => { // JSR Absolute
                const addr = self.getWord();
                const return_addr = self.pc - 1; // PC0 + 2
                self.pushStack(@intCast((return_addr >> 8) & 0xFF));
                self.pushStack(@intCast(return_addr & 0xFF));
                self.pc = addr;
                self.empty_cycles = 5;
            },
            0x21 => { // AND (Indirect,X)
                const addr = self.getIndirectXAddr();
                const value = self.bus.read(addr);
                self.cand(value);
                self.empty_cycles = 5;
            },
            0x24 => { // BIT Zero Page
                const addr = self.getZeroPageAddr();
                const value = self.bus.read(addr);
                const result = self.a & value;
                self.zeroBit(result == 0);
                self.overflowBit((value & 0x40) != 0);
                self.negativeBit((value & 0x80) != 0);
                self.empty_cycles = 2;
            },
            0x25 => { // AND Zero Page
                const addr = self.getZeroPageAddr();
                const value = self.bus.read(addr);
                self.cand(value);
                self.empty_cycles = 2;
            },
            0x26 => { // ROL Zero Page
                const addr = self.getZeroPageAddr();
                self.rolMem(addr);
                self.empty_cycles = 4;
            },
            0x28 => { // PLP
                self.status = self.pullStack();
                self.empty_cycles = 3;
            },
            0x29 => { // AND Immediate
                const value = self.getImmediate();
                self.cand(value);
                self.empty_cycles = 1;
            },
            0x2A => { // ROL Accumulator
                self.rolA();
                self.empty_cycles = 1;
            },
            0x2C => { // BIT Absolute
                const addr = self.getAbsoluteAddr();
                const value = self.bus.read(addr);
                const result = self.a & value;
                self.zeroBit(result == 0);
                self.overflowBit((value & 0x40) != 0);
                self.negativeBit((value & 0x80) != 0);
                self.empty_cycles = 3;
            },
            0x2D => { // AND Absolute
                const addr = self.getAbsoluteAddr();
                const value = self.bus.read(addr);
                self.cand(value);
                self.empty_cycles = 3;
            },
            0x2E => { // ROL Absolute
                const addr = self.getAbsoluteAddr();
                self.rolMem(addr);
                self.empty_cycles = 5;
            },
            0x30 => { // BMI
                self.branchIf((self.status & NegativeFlag) != 0);
            },
            0x31 => { // AND (Indirect,Y)
                const result = self.getIndirectYAddr();
                const value = self.bus.read(result.addr);
                self.cand(value);
                self.empty_cycles = 4 + @as(u32, if (result.crossed) 1 else 0);
            },
            0x35 => { // AND Zero Page,X
                const addr = self.getZeroPageXAddr();
                const value = self.bus.read(addr);
                self.cand(value);
                self.empty_cycles = 3;
            },
            0x38 => { // SEC
                self.carryBit(true);
                self.empty_cycles = 1;
            },
            0x39 => { // AND Absolute,Y
                const result = self.getAbsoluteYAddr();
                const value = self.bus.read(result.addr);
                self.cand(value);
                self.empty_cycles = 3 + @as(u32, if (result.crossed) 1 else 0);
            },
            0x3D => { // AND Absolute,X
                const result = self.getAbsoluteXAddr();
                const value = self.bus.read(result.addr);
                self.cand(value);
                self.empty_cycles = 3 + @as(u32, if (result.crossed) 1 else 0);
            },
            0x3E => { // ROL Absolute,X
                const result = self.getAbsoluteXAddr();
                self.rolMem(result.addr);
                self.empty_cycles = 6;
            },
            0x40 => { // RTI
                self.status = self.pullStack();
                const low = self.pullStack();
                const high = self.pullStack();
                self.pc = (@as(u16, high) << 8) | low;
                self.empty_cycles = 5;
            },
            0x41 => { // EOR (Indirect,X)
                const addr = self.getIndirectXAddr();
                const value = self.bus.read(addr);
                self.eor(value);
                self.empty_cycles = 5;
            },
            0x45 => { // EOR Zero Page
                const addr = self.getZeroPageAddr();
                const value = self.bus.read(addr);
                self.eor(value);
                self.empty_cycles = 2;
            },
            0x46 => { // LSR Zero Page
                const addr = self.getZeroPageAddr();
                self.lsrMem(addr);
                self.empty_cycles = 4;
            },
            0x48 => { // PHA
                self.pushStack(self.a);
                self.empty_cycles = 2;
            },
            0x49 => { // EOR Immediate
                const value = self.getImmediate();
                self.eor(value);
                self.empty_cycles = 1;
            },
            0x4A => { // LSR Accumulator
                self.lsrA();
                self.empty_cycles = 1;
            },
            0x4C => { // JMP Absolute
                self.pc = self.getAbsoluteAddr();
                self.empty_cycles = 2;
            },
            0x4D => { // EOR Absolute
                const addr = self.getAbsoluteAddr();
                const value = self.bus.read(addr);
                self.eor(value);
                self.empty_cycles = 3;
            },
            0x4E => { // LSR Absolute
                const addr = self.getAbsoluteAddr();
                self.lsrMem(addr);
                self.empty_cycles = 5;
            },
            0x50 => { // BVC
                self.branchIf((self.status & OverflowFlag) == 0);
            },
            0x51 => { // EOR (Indirect,Y)
                const result = self.getIndirectYAddr();
                const value = self.bus.read(result.addr);
                self.eor(value);
                self.empty_cycles = 4 + @as(u32, if (result.crossed) 1 else 0);
            },
            0x55 => { // EOR Zero Page,X
                const addr = self.getZeroPageXAddr();
                const value = self.bus.read(addr);
                self.eor(value);
                self.empty_cycles = 3;
            },
            0x56 => { // LSR Zero Page,X
                const addr = self.getZeroPageXAddr();
                self.lsrMem(addr);
                self.empty_cycles = 5;
            },
            0x58 => { // CLI
                self.interruptBit(false);
                self.empty_cycles = 1;
            },
            0x59 => { // EOR Absolute,Y
                const result = self.getAbsoluteYAddr();
                const value = self.bus.read(result.addr);
                self.eor(value);
                self.empty_cycles = 3 + @as(u32, if (result.crossed) 1 else 0);
            },
            0x5D => { // EOR Absolute,X
                const result = self.getAbsoluteXAddr();
                const value = self.bus.read(result.addr);
                self.eor(value);
                self.empty_cycles = 3 + @as(u32, if (result.crossed) 1 else 0);
            },
            0x5E => { // LSR Absolute,X
                const result = self.getAbsoluteXAddr();
                self.lsrMem(result.addr);
                self.empty_cycles = 6;
            },
            0x60 => { // RTS
                const low = self.pullStack();
                const high = self.pullStack();
                self.pc = ((@as(u16, high) << 8) | low) + 1;
                self.empty_cycles = 5;
            },
            0x61 => { // ADC (Indirect,X)
                const addr = self.getIndirectXAddr();
                const value = self.bus.read(addr);
                self.adc(value);
                self.empty_cycles = 5;
            },
            0x65 => { // ADC Zero Page
                const addr = self.getZeroPageAddr();
                const value = self.bus.read(addr);
                self.adc(value);
                self.empty_cycles = 2;
            },
            0x66 => { // ROR Zero Page
                const addr = self.getZeroPageAddr();
                self.rorMem(addr);
                self.empty_cycles = 4;
            },
            0x68 => { // PLA
                self.a = self.pullStack();
                self.setZeroNegative(self.a);
                self.empty_cycles = 3;
            },
            0x69 => { // ADC Immediate
                const value = self.getImmediate();
                self.adc(value);
                self.empty_cycles = 1;
            },
            0x6A => { // ROR Accumulator
                self.rorA();
                self.empty_cycles = 1;
            },
            0x6C => { // JMP Indirect
                const indirect_addr = self.getWord();
                const low = self.bus.read(indirect_addr);
                const high = self.bus.read((indirect_addr & 0xFF00) | ((indirect_addr + 1) & 0x00FF));
                self.pc = (@as(u16, high) << 8) | low;
                self.empty_cycles = 4;
            },
            0x6D => { // ADC Absolute
                const addr = self.getAbsoluteAddr();
                const value = self.bus.read(addr);
                self.adc(value);
                self.empty_cycles = 3;
            },
            0x6E => { // ROR Absolute
                const addr = self.getAbsoluteAddr();
                self.rorMem(addr);
                self.empty_cycles = 5;
            },
            0x70 => { // BVS
                self.branchIf((self.status & OverflowFlag) != 0);
            },
            0x71 => { // ADC (Indirect,Y)
                const result = self.getIndirectYAddr();
                const value = self.bus.read(result.addr);
                self.adc(value);
                self.empty_cycles = 4 + @as(u32, if (result.crossed) 1 else 0);
            },
            0x75 => { // ADC Zero Page,X
                const addr = self.getZeroPageXAddr();
                const value = self.bus.read(addr);
                self.adc(value);
                self.empty_cycles = 3;
            },
            0x76 => { // ROR Zero Page,X
                const addr = self.getZeroPageXAddr();
                self.rorMem(addr);
                self.empty_cycles = 5;
            },
            0x78 => { // SEI
                self.interruptBit(true);
                self.empty_cycles = 1;
            },
            0x79 => { // ADC Absolute,Y
                const result = self.getAbsoluteYAddr();
                const value = self.bus.read(result.addr);
                self.adc(value);
                self.empty_cycles = 3 + @as(u32, if (result.crossed) 1 else 0);
            },
            0x7D => { // ADC Absolute,X
                const result = self.getAbsoluteXAddr();
                const value = self.bus.read(result.addr);
                self.adc(value);
                self.empty_cycles = 3 + @as(u32, if (result.crossed) 1 else 0);
            },
            0x7E => { // ROR Absolute,X
                const result = self.getAbsoluteXAddr();
                self.rorMem(result.addr);
                self.empty_cycles = 6;
            },
            0x81 => { // STA (Indirect,X)
                const addr = self.getIndirectXAddr();
                self.sta(addr);
                self.empty_cycles = 5;
            },
            0x84 => { // STY Zero Page
                const addr = self.getZeroPageAddr();
                self.sty(addr);
                self.empty_cycles = 2;
            },
            0x85 => { // STA Zero Page
                const addr = self.getZeroPageAddr();
                self.sta(addr);
                self.empty_cycles = 2;
            },
            0x86 => { // STX Zero Page
                const addr = self.getZeroPageAddr();
                self.stx(addr);
                self.empty_cycles = 2;
            },
            0x88 => { // DEY
                self.y -%= 1;
                self.setZeroNegative(self.y);
                self.empty_cycles = 1;
            },
            0x8A => { // TXA
                self.a = self.x;
                self.setZeroNegative(self.a);
                self.empty_cycles = 1;
            },
            0x8C => { // STY Absolute
                const addr = self.getAbsoluteAddr();
                self.sty(addr);
                self.empty_cycles = 3;
            },
            0x8D => { // STA Absolute
                const addr = self.getAbsoluteAddr();
                self.sta(addr);
                self.empty_cycles = 3;
            },
            0x8E => { // STX Absolute
                const addr = self.getAbsoluteAddr();
                self.stx(addr);
                self.empty_cycles = 3;
            },
            0x90 => { // BCC
                self.branchIf((self.status & CarryFlag) == 0);
            },
            0x91 => { // STA (Indirect,Y)
                const result = self.getIndirectYAddr();
                self.sta(result.addr);
                self.empty_cycles = 5;
            },
            0x94 => { // STY Zero Page,X
                const addr = self.getZeroPageXAddr();
                self.sty(addr);
                self.empty_cycles = 3;
            },
            0x95 => { // STA Zero Page,X
                const addr = self.getZeroPageXAddr();
                self.sta(addr);
                self.empty_cycles = 3;
            },
            0x96 => { // STX Zero Page,Y
                const addr = self.getZeroPageYAddr();
                self.stx(addr);
                self.empty_cycles = 3;
            },
            0x98 => { // TYA
                self.a = self.y;
                self.setZeroNegative(self.a);
                self.empty_cycles = 1;
            },
            0x99 => { // STA Absolute,Y
                const result = self.getAbsoluteYAddr();
                self.sta(result.addr);
                self.empty_cycles = 4;
            },
            0x9A => { // TXS
                self.sp = self.x;
                self.empty_cycles = 1;
            },
            0x9D => { // STA Absolute,X
                const result = self.getAbsoluteXAddr();
                self.sta(result.addr);
                self.empty_cycles = 4;
            },
            0xA0 => { // LDY Immediate
                const value = self.getImmediate();
                self.ldy(value);
                self.empty_cycles = 1;
            },
            0xA1 => { // LDA (Indirect,X)
                const addr = self.getIndirectXAddr();
                const value = self.bus.read(addr);
                self.lda(value);
                self.empty_cycles = 5;
            },
            0xA2 => { // LDX Immediate
                const value = self.getImmediate();
                self.ldx(value);
                self.empty_cycles = 1;
            },
            0xA4 => { // LDY Zero Page
                const addr = self.getZeroPageAddr();
                const value = self.bus.read(addr);
                self.ldy(value);
                self.empty_cycles = 2;
            },
            0xA5 => { // LDA Zero Page
                const addr = self.getZeroPageAddr();
                const value = self.bus.read(addr);
                self.lda(value);
                self.empty_cycles = 2;
            },
            0xA6 => { // LDX Zero Page
                const addr = self.getZeroPageAddr();
                const value = self.bus.read(addr);
                self.ldx(value);
                self.empty_cycles = 2;
            },
            0xA8 => { // TAY
                self.y = self.a;
                self.setZeroNegative(self.y);
                self.empty_cycles = 1;
            },
            0xA9 => { // LDA Immediate
                const value = self.getImmediate();
                self.lda(value);
                self.empty_cycles = 1;
            },
            0xAA => { // TAX
                self.x = self.a;
                self.setZeroNegative(self.x);
                self.empty_cycles = 1;
            },
            0xAC => { // LDY Absolute
                const addr = self.getAbsoluteAddr();
                const value = self.bus.read(addr);
                self.ldy(value);
                self.empty_cycles = 3;
            },
            0xAD => { // LDA Absolute
                const addr = self.getAbsoluteAddr();
                const value = self.bus.read(addr);
                self.lda(value);
                self.empty_cycles = 3;
            },
            0xAE => { // LDX Absolute
                const addr = self.getAbsoluteAddr();
                const value = self.bus.read(addr);
                self.ldx(value);
                self.empty_cycles = 3;
            },
            0xB0 => { // BCS
                self.branchIf((self.status & CarryFlag) != 0);
            },
            0xB1 => { // LDA (Indirect,Y)
                const result = self.getIndirectYAddr();
                const value = self.bus.read(result.addr);
                self.lda(value);
                self.empty_cycles = 4 + @as(u32, if (result.crossed) 1 else 0);
                // self.empty_cycles = 4 + if (result.crossed) 1 else 0;
            },
            0xB4 => { // LDY Zero Page,X
                const addr = self.getZeroPageXAddr();
                const value = self.bus.read(addr);
                self.ldy(value);
                self.empty_cycles = 3;
            },
            0xB5 => { // LDA Zero Page,X
                const addr = self.getZeroPageXAddr();
                const value = self.bus.read(addr);
                self.lda(value);
                self.empty_cycles = 3;
            },
            0xB6 => { // LDX Zero Page,Y
                const addr = self.getZeroPageYAddr();
                const value = self.bus.read(addr);
                self.ldx(value);
                self.empty_cycles = 3;
            },
            0xB8 => { // CLV
                self.overflowBit(false);
                self.empty_cycles = 1;
            },
            0xB9 => { // LDA Absolute,Y
                const result = self.getAbsoluteYAddr();
                const value = self.bus.read(result.addr);
                self.lda(value);
                self.empty_cycles = 3 + @as(u32, if (result.crossed) 1 else 0);
            },
            0xBA => { // TSX
                self.x = self.sp;
                self.setZeroNegative(self.x);
                self.empty_cycles = 1;
            },
            0xBC => { // LDY Absolute,X
                const result = self.getAbsoluteXAddr();
                const value = self.bus.read(result.addr);
                self.ldy(value);
                self.empty_cycles = 3 + @as(u32, if (result.crossed) 1 else 0);
            },
            0xBD => { // LDA Absolute,X
                const result = self.getAbsoluteXAddr();
                const value = self.bus.read(result.addr);
                self.lda(value);
                self.empty_cycles = 3 + @as(u32, if (result.crossed) 1 else 0);
            },
            0xBE => { // LDX Absolute,Y
                const result = self.getAbsoluteYAddr();
                const value = self.bus.read(result.addr);
                self.ldx(value);
                self.empty_cycles = 3 + @as(u32, if (result.crossed) 1 else 0);
            },
            0xC0 => { // CPY Immediate
                const value = self.getImmediate();
                self.cpy(value);
                self.empty_cycles = 1;
            },
            0xC1 => { // CMP (Indirect,X)
                const addr = self.getIndirectXAddr();
                const value = self.bus.read(addr);
                self.cmp(value);
                self.empty_cycles = 5;
            },
            0xC4 => { // CPY Zero Page
                const addr = self.getZeroPageAddr();
                const value = self.bus.read(addr);
                self.cpy(value);
                self.empty_cycles = 2;
            },
            0xC5 => { // CMP Zero Page
                const addr = self.getZeroPageAddr();
                const value = self.bus.read(addr);
                self.cmp(value);
                self.empty_cycles = 2;
            },
            0xC6 => { // DEC Zero Page
                const addr = self.getZeroPageAddr();
                self.decMem(addr);
                self.empty_cycles = 4;
            },
            0xC8 => { // INY
                self.y +%= 1;
                self.setZeroNegative(self.y);
                self.empty_cycles = 1;
            },
            0xC9 => { // CMP Immediate
                const value = self.getImmediate();
                self.cmp(value);
                self.empty_cycles = 1;
            },
            0xCA => { // DEX
                self.x -%= 1;
                self.setZeroNegative(self.x);
                self.empty_cycles = 1;
            },
            0xCC => { // CPY Absolute
                const addr = self.getAbsoluteAddr();
                const value = self.bus.read(addr);
                self.cpy(value);
                self.empty_cycles = 3;
            },
            0xCD => { // CMP Absolute
                const addr = self.getAbsoluteAddr();
                const value = self.bus.read(addr);
                self.cmp(value);
                self.empty_cycles = 3;
            },
            0xCE => { // DEC Absolute
                const addr = self.getAbsoluteAddr();
                self.decMem(addr);
                self.empty_cycles = 5;
            },
            0xD0 => { // BNE
                self.branchIf((self.status & ZeroFlag) == 0);
            },
            0xD1 => { // CMP (Indirect,Y)
                const result = self.getIndirectYAddr();
                const value = self.bus.read(result.addr);
                self.cmp(value);
                self.empty_cycles = 4 + @as(u32, if (result.crossed) 1 else 0);
            },
            0xD5 => { // CMP Zero Page,X
                const addr = self.getZeroPageXAddr();
                const value = self.bus.read(addr);
                self.cmp(value);
                self.empty_cycles = 3;
            },
            0xD6 => { // DEC Zero Page,X
                const addr = self.getZeroPageXAddr();
                self.decMem(addr);
                self.empty_cycles = 5;
            },
            0xD8 => { // CLD
                self.decimalBit(false);
                self.empty_cycles = 1;
            },
            0xD9 => { // CMP Absolute,Y
                const result = self.getAbsoluteYAddr();
                const value = self.bus.read(result.addr);
                self.cmp(value);
                self.empty_cycles = 3 + @as(u32, if (result.crossed) 1 else 0);
            },
            0xDD => { // CMP Absolute,X
                const result = self.getAbsoluteXAddr();
                const value = self.bus.read(result.addr);
                self.cmp(value);
                self.empty_cycles = 3 + @as(u32, if (result.crossed) 1 else 0);
            },
            0xDE => { // DEC Absolute,X
                const result = self.getAbsoluteXAddr();
                self.decMem(result.addr);
                self.empty_cycles = 6;
            },
            0xE0 => { // CPX Immediate
                const value = self.getImmediate();
                self.cpx(value);
                self.empty_cycles = 1;
            },
            0xE1 => { // SBC (Indirect,X)
                const addr = self.getIndirectXAddr();
                const value = self.bus.read(addr);
                self.sbc(value);
                self.empty_cycles = 5;
            },
            0xE4 => { // CPX Zero Page
                const addr = self.getZeroPageAddr();
                const value = self.bus.read(addr);
                self.cpx(value);
                self.empty_cycles = 2;
            },
            0xE5 => { // SBC Zero Page
                const addr = self.getZeroPageAddr();
                const value = self.bus.read(addr);
                self.sbc(value);
                self.empty_cycles = 2;
            },
            0xE6 => { // INC Zero Page
                const addr = self.getZeroPageAddr();
                self.incMem(addr);
                self.empty_cycles = 4;
            },
            0xE8 => { // INX
                self.x +%= 1;
                self.setZeroNegative(self.x);
                self.empty_cycles = 1;
            },
            0xE9 => { // SBC Immediate
                const value = self.getImmediate();
                self.sbc(value);
                self.empty_cycles = 1;
            },
            0xEA => { // NOP
                self.empty_cycles = 1;
            },
            0xEC => { // CPX Absolute
                const addr = self.getAbsoluteAddr();
                const value = self.bus.read(addr);
                self.cpx(value);
                self.empty_cycles = 3;
            },
            0xED => { // SBC Absolute
                const addr = self.getAbsoluteAddr();
                const value = self.bus.read(addr);
                self.sbc(value);
                self.empty_cycles = 3;
            },
            0xEE => { // INC Absolute
                const addr = self.getAbsoluteAddr();
                self.incMem(addr);
                self.empty_cycles = 5;
            },
            0xF0 => { // BEQ
                self.branchIf((self.status & ZeroFlag) != 0);
            },
            0xF1 => { // SBC (Indirect,Y)
                const result = self.getIndirectYAddr();
                const value = self.bus.read(result.addr);
                self.sbc(value);
                self.empty_cycles = 4 + @as(u32, if (result.crossed) 1 else 0);
            },
            0xF5 => { // SBC Zero Page,X
                const addr = self.getZeroPageXAddr();
                const value = self.bus.read(addr);
                self.sbc(value);
                self.empty_cycles = 3;
            },
            0xF6 => { // INC Zero Page,X
                const addr = self.getZeroPageXAddr();
                self.incMem(addr);
                self.empty_cycles = 5;
            },
            0xF8 => { // SED
                self.decimalBit(true);
                self.empty_cycles = 1;
            },
            0xF9 => { // SBC Absolute,Y
                const result = self.getAbsoluteYAddr();
                const value = self.bus.read(result.addr);
                self.sbc(value);
                self.empty_cycles = 3 + @as(u32, if (result.crossed) 1 else 0);
            },
            0xFD => { // SBC Absolute,X
                const result = self.getAbsoluteXAddr();
                const value = self.bus.read(result.addr);
                self.sbc(value);
                self.empty_cycles = 3 + @as(u32, if (result.crossed) 1 else 0);
            },
            0xFE => { // INC Absolute,X
                const result = self.getAbsoluteXAddr();
                self.incMem(result.addr);
                self.empty_cycles = 6;
            },
            else => {
                std.debug.print("[warn] Unimplemented instruction 0x{X}\n", .{self.opcode});
            },
        }
    }
};

