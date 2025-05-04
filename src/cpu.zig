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

const Operand = struct {
    setAcc: bool,   // 1 byte
    value: u8,     // 1 bytes
    address: u16,   // 2 bytes
    // value: u16,     // 2 bytes
};



pub const CPU = struct {
    alloc: std.mem.Allocator,
    a: u8,       // Accumulator
    x: u8,       // X register
    y: u8,       // Y register
    sp: u8,      // Stack pointer
    pc: u16,     // Program counter
    opcode: u8,  // Opcode
    status: u8,  // Status flags
    bus: *BUS,   // Bus object
    cycles: u32, // Cycles counter
    busy: u8,    // Busy cycles
    op: Operand,

    const Self = @This();

    // // Define function types
    // const AddressingMode = fn (cpu: *CPU) void;
    // const Instruction = fn (cpu: *CPU) void;
    // const PCCycles6502 = fn (cpu: *CPU) void;
    // const Cycles6502 = fn (cpu: *CPU) void;

    // const cycles6502: [256]Cycles6502 = [_]Cycles6502 {
    //     0, 6, 0, 0, 0, 3, 5, 0, 3, 2, 2, 0, 0, 4, 6, 0,
    //     2, 5, 0, 0, 0, 4, 6, 0, 2, 4, 0, 0, 0, 4, 7, 0,
    //     6, 6, 0, 0, 3, 3, 5, 0, 4, 2, 2, 0, 4, 4, 6, 0,
    //     2, 5, 0, 0, 0, 4, 6, 0, 2, 4, 0, 0, 0, 4, 7, 0,
    //     6, 6, 0, 0, 0, 3, 5, 0, 3, 2, 2, 0, 3, 4, 6, 0,
    //     2, 5, 0, 0, 0, 4, 6, 0, 2, 4, 0, 0, 0, 4, 7, 0,
    //     6, 6, 0, 0, 0, 3, 5, 0, 4, 2, 2, 0, 5, 4, 6, 0,
    //     2, 5, 0, 0, 0, 4, 6, 0, 2, 4, 0, 0, 0, 4, 7, 0,
    //     0, 6, 0, 0, 3, 3, 3, 0, 2, 0, 2, 0, 4, 4, 4, 0,
    //     2, 6, 0, 0, 4, 4, 4, 0, 2, 5, 2, 0, 0, 5, 0, 0,
    //     2, 6, 2, 0, 3, 3, 3, 0, 2, 2, 2, 0, 4, 4, 4, 0,
    //     2, 5, 0, 0, 4, 4, 4, 0, 2, 4, 2, 0, 4, 4, 4, 0,
    //     2, 6, 0, 0, 3, 3, 5, 0, 2, 2, 2, 0, 4, 4, 6, 0,
    //     2, 5, 0, 0, 0, 4, 6, 0, 2, 4, 0, 0, 0, 4, 7, 0,
    //     2, 6, 0, 0, 3, 3, 5, 0, 2, 2, 2, 0, 4, 4, 6, 0,
    //     2, 5, 0, 0, 0, 4, 6, 0, 2, 4, 0, 0, 0, 4, 7, 0
    // };

    // const pc_cycles: [256]PCCycles6502 = [_]PCCycles6502 {
    //     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
    //     1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1,
    //     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
    //     1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1,
    //     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
    //     1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1,
    //     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
    //     1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1,
    //     1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
    //     1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
    //     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
    //     1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1,
    //     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
    //     1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1,
    //     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
    //     1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1
    // };

    pub fn init(alloc: std.mem.Allocator, bus: *BUS) !Self {
        return CPU{
            .a = 0,
            .x = 0,
            .y = 0,
            .bus = bus,
            .sp = 0xFF,
            .busy = 0,
            .opcode = 0,
            .cycles = 0,
            .pc = 0xFFFC,
            .status = 0x34,
            .alloc = alloc,
            .op = Operand{ .setAcc = false, .value = 0, .address = 0 }
        };
    }

    // Stack operations
    pub fn stackPush(self: *Self, value: u8) void {
        self.bus.write(0x100 + @as(u16, (self.sp - 1)), value);
    }

    pub fn stackPull(self: *Self) u8 {
        return self.bus.read(0x100 + @as(u16, (self.sp + 1)));
    }

    pub fn reset(self: *Self, entrypoint: u16) void {
        self.busy = 0;
        self.sp = 0xFF;
        self.pc = entrypoint;
        self.status = 0x34;
        self.cycles = 0;
        self.op.setAcc = false;
        self.op.value = 0;
        self.op.address = 0;
    }

    fn setSZ(self: *Self, value: u16) void {
        if ((value & 0x00FF) != 0) self.status &= ~ZeroFlag
        else self.status |= ~ZeroFlag;

        if ((value & 0x80) != 0) self.status |= ~NegativeFlag
        else self.status &= ~NegativeFlag;
    }

    // Addressing modes
    // fn IMP(self: *Self) void { // Implicit
    // }

    fn ACC(self: *Self) void { // Accumulator
        self.op.value = self.a;
        self.op.setAcc = true;
    }

    fn IMM(self: *Self) void { // IMMediate
        self.op.address = self.pc;
        self.pc +%= 1;
        self.op.value = self.bus.read(self.op.address);
    }

    fn ZPG(self: *Self) void { // Zero page
        self.op.address = self.bus.read(self.pc);
        self.pc +%= 1;
        self.op.value = self.bus.read(self.op.address);
    }

    fn ZPX(self: *Self) void { // Zero page,X
        self.op.address = (self.bus.read(self.pc) + self.x) & 0xFF;
        self.pc +%= 1;
        self.op.value = self.bus.read(self.op.address);
    }

    fn ZPY(self: *Self) void { // Zero page,Y
        self.op.address = (self.bus.read(self.pc) + self.y) & 0xFF;
        self.pc +%= 1;
        self.op.value = self.bus.read(self.op.address);
    }

    fn REL(self: *Self) void { // Relative
        self.op.address = self.bus.read(self.pc);
        self.pc +%= 1;
        if ((self.op.address & 0x80) != 0)
            self.op.address |= 0xFF00;
    }

    fn ABS(self: *Self) void { // Absolute
        const addressLow: u8 = self.bus.read(self.pc);
        const addressHigh: u8 = self.bus.read(self.pc + 1);
        self.op.address = @as(u16, addressLow) | (@as(u16, addressHigh) << 8);
        self.op.value = self.bus.read(self.op.address);
        self.pc +%= 2;
    }

    fn ABX(self: *Self) void { // Asbolute, X
        // self.op.address = (self.bus.read(self.pc) | (self.bus.read(self.pc + 1) << 8)) + self.x;
        const addressLow: u8 = self.bus.read(self.pc);
        const addressHigh: u8 = self.bus.read(self.pc + 1);
        self.op.address = (@as(u16, addressLow) | (@as(u16, addressHigh) << 8)) + self.x;
        self.op.value = self.bus.read(self.op.address);
        self.pc +%= 2;
    }

    fn ABY(self: *Self) void { // Asbolute, Y
        // self.op.address = (self.bus.read(self.pc) | (self.bus.read(self.pc + 1) << 8)) + self.y;
        const addressLow: u8 = self.bus.read(self.pc);
        const addressHigh: u8 = self.bus.read(self.pc + 1);
        self.op.address = (@as(u16, addressLow) | (@as(u16, addressHigh) << 8)) + self.y;
        self.op.value = self.bus.read(self.op.address);
        self.pc += 2;
    }

    fn IND(self: *Self) void { // Indirect jump with page bountry wraparound bug
        const byte1: u8 = self.bus.read(self.pc + 1);
        const addressLow: u8 = self.bus.read(byte1);
        const addressHigh: u8 = self.bus.read((byte1 + 1) & 0x00FF);
        self.op.address = @as(u16, addressLow) | (@as(u16, addressHigh) << 8);
        self.op.value = self.bus.read(self.op.address);
        self.pc += 2;
    }

    fn IDX(self: *Self) void { // Indexed indirect X
        const v1: u8 = (self.bus.read(self.pc) + self.x) & 0xFF;
        const lowerByte: u8 = self.bus.read(v1 & 0x00FF);
        const upperByte: u8 = self.bus.read((v1 + 1) & 0x00FF);

        self.pc +%= 1;
        self.op.address = @as(u16, lowerByte) | (@as(u16, upperByte) << 8);
        self.op.value = self.bus.read(self.op.address);
    }

    fn IDY(self: *Self) void {
        const byte1: u8 = self.bus.read(self.pc);
        const addressLow: u8 = self.bus.read(byte1);
        const addressHigh: u8 = self.bus.read((byte1 + 1) & 0x00FF);

        self.pc +%= 1;
        self.op.address = @as(u16, addressLow) | (@as(u16, addressHigh) << 8);
        self.op.address += self.y;
        self.op.value = self.bus.read(self.op.address);
    }

    // Instructions
    fn NOP(self: *Self) void { // no op
        _ = self.a;
    }

    fn BRK(self: *Self) void { // Break
        self.pc +%= 1;
        const highByte: u8 = @intCast(self.pc >> 8);
        const lowByte: u8 = @intCast(self.pc & 0xFF);
        self.stackPush(highByte);
        self.stackPush(lowByte);
        self.stackPush(self.status | BreakCommand);
        self.status |= InterruptDisable;
        // self.pc = self.bus.read(0xFFFE) | (self.bus.read(0xFFFF) << 8);
        const lowerByte: u8 = self.bus.read(0xFFFE);
        const higherByte: u8 = self.bus.read(0xFFFF);

        const lowAddress: u16 = @intCast(lowerByte);
        const highAddress: u16 = (@as(u16, higherByte) << 8);
        // const highAddress: u16 = @intCast(self.bus.read(0xFFFF) << 8);
        self.pc = lowAddress | highAddress;
    }

    fn CLD(self: *Self) void { // Clear decimal
        self.status &= ~DecimalMode;
    }

    fn SED(self: *Self) void { // Set Decimal
        self.status |= DecimalMode;
    }

    fn CLC(self: *Self) void { // Clear Carry
        self.status &= ~CarryFlag;
    }

    fn SEC(self: *Self) void { // Set Carry
        self.status |= CarryFlag;
    }

    fn CLI(self: *Self) void { // Clear Interrupt
        self.status &= ~InterruptDisable;
    }

    fn SEI(self: *Self) void { // Set Interrupt
        self.status |= InterruptDisable;
    }

    fn CLV(self: *Self) void { // Clear overflow
        self.status &= ~OverflowFlag;
    }

    fn LDA(self: *Self) void {
        self.a = @intCast(self.op.value);
        self.setSZ(self.a);
    }

    fn LDX(self: *Self) void {
        self.x = @intCast(self.op.value);
        self.setSZ(self.x);
    }

    fn LDY(self: *Self) void {
        self.y = @intCast(self.op.value);
        self.setSZ(self.y);
    }

    fn STA(self: *Self) void { // Store Accumulator
        self.bus.write(self.op.address, self.a);
    }

    fn STX(self: *Self) void { // Store X
        self.bus.write(self.op.address, self.x);
    }

    fn STY(self: *Self) void { // Store Y
        self.bus.write(self.op.address, self.y);
    }

    fn DEC(self: *Self) void { // Decrement
        self.op.value -%= 1;
        self.bus.write(self.op.address, self.op.value);
        self.setSZ(self.op.value);
    }

    fn DEX(self: *Self) void { // Decrement X
        self.setSZ(self.x);
    }

    fn DEY(self: *Self) void { // Decrement Y
        self.setSZ(self.y);
    }

    fn INC(self: *Self) void { // Increment
        self.op.value +%= 1;
        self.bus.write(self.op.address, self.op.value);
        const sz_value: u16 = @intCast(self.op.value);
        self.setSZ(sz_value);
    }

    fn INX(self: *Self) void { // Increment X
        self.setSZ(self.x);
    }

    fn INY(self: *Self) void { // Increment Y
        self.setSZ(self.y);
    }

    fn TAX(self: *Self) void { // Trasnfer accumulator to X
        self.x = self.a;
        self.setSZ(self.x);
    }

    fn TAY(self: *Self) void { // Trasnfer accumulator to Y
        self.y = self.a;
        self.setSZ(self.y);
    }

    fn TXA(self: *Self) void { // Trasnfer X to Accumulator
        self.a = self.x;
        self.setSZ(self.a);
    }

    fn TYA(self: *Self) void { // Trasnfer Y to Accumulator
        self.a = self.y;
        self.setSZ(self.a);
    }

    fn TSX(self: *Self) void { // Trasnfer SP to X
        self.x = self.sp;
        self.setSZ(self.x);
    }

    fn TXS(self: *Self) void { // Trasnfer X to SP
        self.sp = self.x;
    }

    fn BEQ(self: *Self) void { // Branch not Equal
        if ((self.status & ZeroFlag) != 0)
            self.pc +%= self.op.address;
    }

    fn BNE(self: *Self) void { // Branch on not equal
        if ((self.status & ZeroFlag) == 0)
            self.pc += self.op.address;
    }

    fn BMI(self: *Self) void { // Branch if minus
        if ((self.status & NegativeFlag) != 0)
            self.pc += self.op.address;
    }

    fn BPL(self: *Self) void { // Branch if plus
        if ((self.status & NegativeFlag) == 0)
            self.pc +%= self.op.address;
    }

    fn BVS(self: *Self) void { // Branch on overflow set
        if ((self.status & OverflowFlag) != 0)
            self.pc += self.op.address;
    }

    fn BVC(self: *Self) void { // Branch on overflow clear
        if ((self.status & OverflowFlag) == 0)
            self.pc += self.op.address;
    }

    fn BCS(self: *Self) void { // Branch on carry set
        if ((self.status & CarryFlag) != 0)
            self.pc += self.op.address;
    }

    fn BCC(self: *Self) void { // Branch on carry clear
        if ((self.status & CarryFlag) == 0)
            self.pc += self.op.address;
    }

    fn PHA(self: *Self) void { // Push A to stack
        self.stackPush(self.a);
    }

    fn PLA(self: *Self) void { // Pull stack into A
        self.a = self.stackPull();
        self.setSZ(self.a);
    }

    fn PHP(self: *Self) void { // Push status register into stack
        self.stackPush(self.status | BreakCommand);
    }

    fn PLP(self: *Self) void { // Pull stack into SR
        self.status = self.stackPull() | 0x34;
    }

    fn JMP(self: *Self) void { // Jump
        self.pc = self.op.address;
    }

    fn JSR(self: *Self) void { // Jump sub-routine
        self.pc -%= 1;
        const highByte: u8 = @intCast(self.pc >> 8);
        const lowByte: u8 = @intCast(self.pc & 0xFF);
        self.stackPush(highByte);
        self.stackPush(lowByte);
        self.pc = self.op.address;
    }

    fn RTS(self: *Self) void { // Return from subroutine
        // self.pc = (self.stackPull() | (self.stackPull() << 8)) + 1;
        self.pc = (@as(u16, self.stackPull()) | (@as(u16, self.stackPull()) << 8)) + 1;
    }

    fn RTI(self: *Self) void {
        self.status = self.stackPull();
        self.pc = (@as(u16, self.stackPull()) | (@as(u16, self.stackPull()) << 8)) + 1;
        // self.pc = (self.stackPull() | (self.stackPull() << 8)) + 1;
    }

    fn CMP(self: *Self) void { // Compare with A
        self.setSZ(self.a - self.op.value);
        if (self.a >= self.op.value)
            self.status |= CarryFlag
        else
            self.status &= ~CarryFlag;
    }

    fn CPX(self: *Self) void { // Compare with X
        self.setSZ(self.x - self.op.value);
        if (self.x >= self.op.value)
            self.status |= CarryFlag
        else
            self.status &= ~CarryFlag;
    }

    fn CPY(self: *Self) void { // Compare with Y
        self.setSZ(self.y - self.op.value);
        if (self.y >= self.op.value)
            self.status |= CarryFlag
        else
            self.status &= ~CarryFlag;
    }

    fn AND(self: *Self) void { // AND with A
        self.a &= @intCast(self.op.value);
        self.setSZ(self.a);
    }

    fn ORA(self: *Self) void { // OR with A
        self.a |= @intCast(self.op.value);
        self.setSZ(self.a);
    }

    fn EOR(self: *Self) void { // XOR with A
        self.a ^= @intCast(self.op.value);
        self.setSZ(self.a);
    }

    fn BIT(self: *Self) void { // BIT with A
        if ((self.a & self.op.value) != 0)
            self.status &= ~ZeroFlag
        else
            self.status |= ZeroFlag;
    }

    fn update(self: *Self, value: u8) void { // Helper function
        if (self.op.setAcc) self.a = value
        else self.bus.write(self.op.address, @intCast(self.op.value));

        self.op.setAcc = false;
        self.setSZ(value);
    }

    fn ASL(self: *Self) void { // Arithmetic Shift left
        const result: u16 = @intCast(self.op.value << 1);
        if ((result & 0xFF00) != 0) self.status |= CarryFlag
        else self.status &= ~CarryFlag;
        self.update(@intCast(result & 0xFF));
    }

    fn LSR(self: *Self) void { // Logical Shift Right
        if ((self.op.value & 1) != 0) self.status |= CarryFlag
        else self.status &= ~CarryFlag;
        self.update(@intCast((self.op.value >> 1 ) & 0xFF));
    }

    fn ROL(self: *Self) void { // Rotate left
        const result: u16 = @intCast((self.op.value << 1) | (self.status & CarryFlag));
        if ((result & 0x100) != 0) self.status |= CarryFlag
        else self.status &= ~CarryFlag;
        self.update(@intCast(result & 0xFF));
    }

    fn ROR(self: *Self) void { // Rotate right
        const result = (self.op.value >> 1) | ((self.status & CarryFlag) << 7);
        if ((self.op.value & 0x1) != 0) self.status |= CarryFlag
        else self.status &= ~CarryFlag;
        self.update(@intCast(result & 0xFF));
    }

    fn ADC(self: *Self) void { // Add with carry
        // var result = self.a + self.op.value + (self.status & CarryFlag);
        var result: u16 = @intCast(self.a + self.op.value + (self.status & CarryFlag));
        self.setSZ(result);

        // if (((result)^(self.a)) & ((result)^(self.op.value)) & 0x0080)
        if ((((result)^(self.a)) & ((result)^(self.op.value)) & 0x0080) != 0)
            self.status |= OverflowFlag
        else
            self.status &= ~OverflowFlag;

        // if (self.status & NegativeFlag)
        if ((self.status & NegativeFlag) != 0)
            result += ((((result+0x66) ^ self.a ^ self.op.value) >> 3) & 0x22) * 3;

        if ((result & 0xFF00) != 0) self.status |= CarryFlag
        else self.status &= ~CarryFlag;

        self.a = @intCast(result & 0xFF);
    }

    fn SBC(self: *Self) void {
        self.op.value ^= 0xFF;
        if ((self.status & NegativeFlag) != 0)
            self.op.value -= 0x0066;

        var result: u16 = @intCast(self.a + self.op.value + (self.status & CarryFlag));
        self.setSZ(result);

        // if (((result) ^ (self.a)) & ((result) ^ (self.op.value)) & 0x0080)
        if (((result ^ self.a) & (result ^ self.op.value) & 0x0080) != 0)
            self.status |= OverflowFlag
        else
            self.status &= ~OverflowFlag;

        if ((self.status & NegativeFlag) != 0)
            result += ((((result + 0x66) ^ self.a ^ self.op.value) >> 3) & 0x22) * 3;

        if ((result & 0xFF00) != 0) self.status |= CarryFlag
        else self.status &= ~CarryFlag;

        self.a = @intCast(result & 0xFF);
    }

    fn UND() void {
        BRK();
    }

    pub fn tick(self: *Self) void {
        if (self.busy > 0) {
            self.busy -= 1;
        } else {
            self.pc +%= 1;
            self.opcode = self.bus.read(self.pc);

            switch (self.opcode) {
                0x00 => {
                    // self.IMP();
                    self.BRK();
                },
                0x01 => {
                    self.IDX();
                    self.ORA();
                },
                0x02 => {},
                0x03 => {},
                0x04 => {},
                0x05 => {
                    self.ZPG();
                    self.ORA();
                },
                0x06 => {
                    self.ZPG();
                    self.ASL();
                },
                0x07 => {},
                0x08 => {
                    // self.IMP();
                    self.PHP();
                },
                0x09 => {
                    self.IMM();
                    self.ORA();
                },
                0x0A => {
                    self.ACC();
                    self.ASL();
                },
                0x0B => {},
                0x0C => {},
                0x0D => {
                    self.ABS();
                    self.ORA();
                },
                0x0E => {
                    self.ABS();
                    self.ASL();
                },
                0x0F => {},
                0x10 => {
                    self.REL();
                    self.BPL();
                },
                0x11 => {
                    self.IDY();
                    self.ORA();
                },
                0x12 => {},
                0x13 => {},
                0x14 => {},
                0x15 => {
                    self.ZPX();
                    self.ORA();
                },
                0x16 => {
                    self.ZPX();
                    self.ASL();
                },
                0x17 => {},
                0x18 => {
                    // self.IMP();
                    self.CLC();
                },
                0x19 => {
                    self.ABY();
                    self.ORA();
                },
                0x1A => {},
                0x1B => {},
                0x1C => {},
                0x1D => {
                    self.ABX();
                    self.ORA();
                },
                0x1E => {
                    self.ABX();
                    self.ASL();
                },
                0x1F => {},
                0x20 => {
                    self.ABS();
                    self.JSR();
                },
                0x21 => {
                    self.IDX();
                    self.AND();
                },
                0x22 => {},
                0x23 => {},
                0x24 => {
                    self.ZPG();
                    self.BIT();
                },
                0x25 => {
                    self.ZPG();
                    self.AND();
                },
                0x26 => {
                    self.ZPG();
                    self.ROL();
                },
                0x27 => {},
                0x28 => {
                    // self.IMP();
                    self.PLP();
                },
                0x29 => {
                    self.IMM();
                    self.AND();
                },
                0x2A => {
                    self.ACC();
                    self.ROL();
                },
                0x2B => {},
                0x2C => {
                    self.ABS();
                    self.BIT();
                },
                0x2D => {
                    self.ABS();
                    self.AND();
                },
                0x2E => {
                    self.ABS();
                    self.ROL();
                },
                0x2F => {},
                0x30 => {
                    self.REL();
                    self.BMI();
                },
                0x31 => {
                    self.IDY();
                    self.AND();
                },
                0x32 => {},
                0x33 => {},
                0x34 => {},
                0x35 => {
                    self.ZPX();
                    self.AND();
                },
                0x36 => {},
                0x37 => {},
                0x38 => {
                    // self.IMP();
                    self.SEC();
                },
                0x39 => {
                    self.ABY();
                    self.AND();
                },
                0x3A => {},
                0x3B => {},
                0x3C => {},
                0x3D => {
                    self.ABX();
                    self.AND();
                },
                0x3E => {
                    self.ABX();
                    self.ROL();
                },
                0x3F => {},
                0x40 => {
                    // self.IMP();
                    self.RTI();
                },
                0x41 => {
                    self.IDX();
                    self.EOR();
                },
                0x42 => {},
                0x43 => {},
                0x44 => {},
                0x45 => {
                    self.ZPG();
                    self.EOR();
                },
                0x46 => {
                    self.ZPG();
                    self.LSR();
                },
                0x47 => {},
                0x48 => {
                    // self.IMP();
                    self.PHA();
                },
                0x49 => {
                    self.IMM();
                    self.EOR();
                },
                0x4A => {
                    self.ACC();
                    self.LSR();
                },
                0x4B => {},
                0x4C => {
                    self.ABS();
                    self.JMP();
                },
                0x4D => {
                    self.ABS();
                    self.EOR();
                },
                0x4E => {
                    self.ABS();
                    self.LSR();
                },
                0x4F => {},
                0x50 => {
                    self.REL();
                    self.BVC();
                },
                0x51 => {
                    self.IDY();
                    self.EOR();
                },
                0x52 => {},
                0x53 => {},
                0x54 => {},
                0x55 => {
                    self.ZPX();
                    self.EOR();
                },
                0x56 => {
                    self.ZPX();
                    self.LSR();
                },
                0x57 => {},
                0x58 => {
                    // self.IMP();
                    self.CLI();
                },
                0x59 => {
                    self.ABY();
                    self.EOR();
                },
                0x5A => {},
                0x5B => {},
                0x5C => {},
                0x5D => {
                    self.ABX();
                    self.EOR();
                },
                0x5E => {
                    self.ABX();
                    self.LSR();
                },
                0x5F => {},
                0x60 => {
                    // self.IMP();
                    self.RTS();
                },
                0x61 => {
                    self.IDX();
                    self.ADC();
                },
                0x62 => {},
                0x63 => {},
                0x64 => {},
                0x65 => {
                    self.ZPG();
                    self.ADC();
                },
                0x66 => {
                    self.ZPG();
                    self.ROR();
                },
                0x67 => {},
                0x68 => {
                    // self.IMP();
                    self.PLA();
                },
                0x69 => {
                    self.IMM();
                    self.ADC();
                },
                0x6A => {
                    self.ACC();
                    self.ROR();
                },
                0x6B => {},
                0x6C => {
                    self.IND();
                    self.JMP();
                },
                0x6D => {
                    self.ABS();
                    self.ADC();
                },
                0x6E => {
                    self.ABS();
                    self.ROR();
                },
                0x6F => {},
                0x70 => {
                    self.REL();
                    self.BVS();
                },
                0x71 => {
                    self.IDY();
                    self.ADC();
                },
                0x72 => {},
                0x73 => {},
                0x74 => {},
                0x75 => {
                    self.ZPX();
                    self.ADC();
                },
                0x76 => {
                    self.ZPX();
                    self.ROR();
                },
                0x77 => {},
                0x78 => {
                    // self.IMP();
                    self.SEI();
                },
                0x79 => {
                    self.ABY();
                    self.ADC();
                },
                0x7A => {},
                0x7B => {},
                0x7C => {},
                0x7D => {
                    self.ABX();
                    self.ADC();
                },
                0x7E => {
                    self.ABX();
                    self.ROR();
                },
                0x7F => {},
                0x80 => {},
                0x81 => {
                    self.IDX();
                    self.STA();
                },
                0x82 => {},
                0x83 => {},
                0x84 => {
                    self.ZPG();
                    self.STY();
                },
                0x85 => {
                    self.ZPG();
                    self.STA();
                },
                0x86 => {
                    self.ZPG();
                    self.STX();
                },
                0x87 => {},
                0x88 => {
                    // self.IMP();
                    self.DEY();
                },
                0x89 => {},
                0x8A => {
                    // self.IMP();
                    self.TXA();
                },
                0x8B => {},
                0x8C => {
                    self.ABS();
                    self.STY();
                },
                0x8D => {
                    self.ABS();
                    self.STA();
                },
                0x8E => {
                    self.ABS();
                    self.STX();
                },
                0x8F => {},
                0x90 => {
                    self.REL();
                    self.BCC();
                },
                0x91 => {
                    self.IDY();
                    self.STA();
                },
                0x92 => {},
                0x93 => {},
                0x94 => {
                    self.ZPX();
                    self.STY();
                },
                0x95 => {
                    self.ZPX();
                    self.STA();
                },
                0x96 => {
                    self.ZPY();
                    self.STX();
                },
                0x97 => {},
                0x98 => {
                    // self.IMP();
                    self.TYA();
                },
                0x99 => {
                    self.ABY();
                    self.STA();
                },
                0x9A => {
                    // self.IMP();
                    self.TXS();
                },
                0x9B => {},
                0x9C => {},
                0x9D => {
                    self.ABX();
                    self.STA();
                },
                0x9E => {},
                0x9F => {},
                0xA0 => {
                    self.IMM();
                    self.LDY();
                },
                0xA1 => {
                    self.IDX();
                    self.LDA();
                },
                0xA2 => {
                    self.IMM();
                    self.LDX();
                },
                0xA3 => {},
                0xA4 => {
                    self.ZPG();
                    self.LDY();
                },
                0xA5 => {
                    self.ZPG();
                    self.LDA();
                },
                0xA6 => {
                    self.ZPG();
                    self.LDX();
                },
                0xA7 => {},
                0xA8 => {
                    // self.IMP();
                    self.TAY();
                },
                0xA9 => {
                    // self.IMP();
                    self.LDA();
                },
                0xAA => {
                    // self.IMP();
                    self.TAX();
                },
                0xAB => {},
                0xAC => {
                    self.ABS();
                    self.LDY();
                },
                0xAD => {
                    self.ABS();
                    self.LDA();
                },
                0xAE => {
                    self.ABS();
                    self.LDX();
                },
                0xAF => {},
                0xB0 => {
                    self.REL();
                    self.BCS();
                },
                0xB1 => {
                    self.IDY();
                    self.LDA();
                },
                0xB2 => {},
                0xB3 => {},
                0xB4 => {
                    self.ZPX();
                    self.LDY();
                },
                0xB5 => {
                    self.ZPX();
                    self.LDA();
                },
                0xB6 => {
                    self.ZPY();
                    self.LDX();
                },
                0xB7 => {},
                0xB8 => {
                    // self.IMP();
                    self.CLV();
                },
                0xB9 => {
                    self.ABY();
                    self.LDA();
                },
                0xBA => {
                    // self.IMP();
                    self.TSX();
                },
                0xBB => {},
                0xBC => {
                    self.ABX();
                    self.LDY();
                },
                0xBD => {
                    self.ABX();
                    self.LDA();
                },
                0xBE => {
                    self.ABY();
                    self.LDX();
                },
                0xBF => {},
                0xC0 => {
                    self.IMM();
                    self.CPY();
                },
                0xC1 => {
                    self.IDX();
                    self.CMP();
                },
                0xC2 => {},
                0xC3 => {},
                0xC4 => {
                    self.ZPG();
                    self.CPY();
                },
                0xC5 => {
                    self.ZPG();
                    self.CMP();
                },
                0xC6 => {
                    self.ZPG();
                    self.DEC();
                },
                0xC7 => {},
                0xC8 => {
                    // self.IMP();
                    self.INY();
                },
                0xC9 => {},
                0xCA => {
                    // self.IMP();
                    self.DEX();
                },
                0xCB => {},
                0xCC => {
                    self.ABS();
                    self.CPY();
                },
                0xCD => {
                    self.ABS();
                    self.CMP();
                },
                0xCE => {
                    self.ABS();
                    self.DEC();
                },
                0xCF => {},
                0xD0 => {
                    self.REL();
                    self.BNE();
                },
                0xD1 => {
                    self.IDY();
                    self.CMP();
                },
                0xD2 => {},
                0xD3 => {},
                0xD4 => {},
                0xD5 => {
                    self.ZPX();
                    self.CMP();
                },
                0xD6 => {
                    self.ZPX();
                    self.DEC();
                },
                0xD7 => {},
                0xD8 => {
                    // self.IMP();
                    self.CLD();
                },
                0xD9 => {
                    self.ABY();
                    self.CMP();
                },
                0xDA => {},
                0xDB => {},
                0xDC => {},
                0xDD => {
                    self.ABX();
                    self.CMP();
                },
                0xDE => {
                    self.ABX();
                    self.DEC();
                },
                0xDF => {},
                0xE0 => {
                    self.IMM();
                    self.CPX();
                },
                0xE1 => {
                    self.IDX();
                    self.SBC();
                },
                0xE2 => {},
                0xE3 => {},
                0xE4 => {
                    self.ZPG();
                    self.CPX();
                },
                0xE5 => {
                    self.ZPG();
                    self.SBC();
                },
                0xE6 => {
                    self.ZPG();
                    self.INC();
                },
                0xE7 => {},
                0xE8 => {
                    // self.IMP();
                    self.INX();
                },
                0xE9 => {
                    self.IMM();
                    self.SBC();
                },
                0xEA => {
                    // self.IMP();
                    self.NOP();
                },
                0xEB => {},
                0xEC => {
                    self.ABS();
                    self.CPX();
                },
                0xED => {
                    self.ABS();
                    self.SBC();
                },
                0xEE => {
                    self.ABS();
                    self.INC();
                },
                0xEF => {},
                0xF0 => {
                    self.REL();
                    self.BEQ();
                },
                0xF1 => {
                    self.IDY();
                    self.SBC();
                },
                0xF2 => {},
                0xF3 => {},
                0xF4 => {},
                0xF5 => {
                    self.ZPX();
                    self.SBC();
                },
                0xF6 => {
                    self.ZPX();
                    self.INC();
                },
                0xF7 => {},
                0xF8 => {
                    // self.IMP();
                    self.SED();
                },
                0xF9 => {
                    self.ABY();
                    self.SBC();
                },
                0xFA => {},
                0xFB => {},
                0xFC => {},
                0xFD => {
                    self.ABX();
                    self.SBC();
                },
                0xFE => {
                    self.ABX();
                    self.INC();
                },
                0xFF => {},
            }
            // cycles6502[self.opcode]();
        }
        if ((self.cycles % 100) == 0) {
            // read from kbd
        }
    }

};

