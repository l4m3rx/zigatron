const std = @import("std");

pub const RIOT = struct {
    allocator: std.mem.Allocator,
    cycles: u16,
    timer: u8,          // Current timer value
    interval: u16,      // Countdown interval (1, 8, 64, or 1024)
    timer_counter: u16, // Counts cycles until the interval is reached
    timer_flag: bool,   // Timer underflow flag
    porta_out: u8,      // Output latch for Port A (SWCHA)
    portb_out: u8,      // Output latch for Port B (SWCHB)
    ddra: u8,           // Data Direction Register A
    ddrb: u8,           // Data Direction Register B
    pram: []u8,
    swcha: u8,
    swchb: u8,

    pub fn init(allocator: std.mem.Allocator) !RIOT {
        const pram = try allocator.alloc(u8, 128);
        return RIOT{
            .allocator = allocator,
            .timer = 0,
            .cycles = 0,
            .pram = pram,
            .ddra = 0x00,
            .ddrb = 0x00,
            .swcha = 0xFF,
            .swchb = 0xFF,
            .interval = 1,
            .porta_out = 0x00,
            .portb_out = 0x00,
            .timer_counter = 0,
            .timer_flag = false,
        };
    }

    pub fn deinit(self: *RIOT) void {
        self.allocator.free(self.pram);
    }

    pub fn readRam(self: *RIOT, address: u16) u8 {
        if (address >= 0x80) {
            std.debug.print("[E] RAM Read Address out of range 0x{X}\n", .{address});
            return 0;
        }
        return self.pram[address];
    }

    pub fn writeRam(self: *RIOT, address: u16, data: u8) void {
        if (address < 0x80) {
            self.pram[address] = data;
        }
    }

    pub fn read(self: *RIOT, address: u16) u8 {
        std.debug.print("[D] RIOT Reading: 0x{X}\n", .{address});
        if (address >= 0x80 and address <= 0xFF) {
            std.debug.print("[D] RAM Read: 0x{X}\n", .{address});
            return self.pram[address - 0x80];
        } else if (address == 0x280) {
            return (self.swcha & ~self.ddra) | (self.porta_out & self.ddra);
        } else if (address == 0x281) { // SWACNT: DDRA
            return self.ddra;
        } else if (address == 0x282) {
            return (self.swchb & ~self.ddrb) | (self.portb_out & self.ddrb);
        } else if (address == 0x283) { // SWBCNT: DDRB
            return self.ddrb;
        } else if (address == 0x284) { // INTIM: Timer read register
            std.debug.print("[D] Timer Value: {}\n", .{self.timer});
            return self.timer;
        } else if (address == 0x285) { // TIMINT: Timer interrupt flag
            self.timer_flag = false; // Reading clears the flag
            if (self.timer_flag)
                return 0x80
            else
                return 0x00;
        } else {
            std.debug.print("[W] Unhandled RIOT read: 0x{X}\n", .{address});
            return 0xFF;
        }
    }

    pub fn write(self: *RIOT, address: u16, value: u8) void {
        std.debug.print("[D] RIOT Writing: 0x{X}={d}\n", .{address, value});
        if (address >= 0x80 and address <= 0xFF) {
            self.pram[address - 0x80] = value;
        } else if (address == 0x280) { // SWCHA
            self.porta_out = value;
        } else if (address == 0x282) { // SWCHB
            self.portb_out = value;
        } else if (address == 0x281) { // SWACNT: DDRA
            self.ddra = value;
        } else if (address == 0x283) { // SWBCNT: DDRB
            self.ddrb = value;
        } else if (address == 0x294) { // TIM1T: Set timer, count every cycle
            self.timer = value;
            self.interval = 1;
            self.timer_counter = 0;
            self.timer_flag = false;
        } else if (address == 0x295) { // TIM8T: Set timer, count every 8 cycles
            self.timer = value;
            self.interval = 8;
            self.timer_counter = 0;
            self.timer_flag = false;
        } else if (address == 0x296) { // TIM64T: Set timer, count every 64 cycles
            self.timer = value;
            self.interval = 64;
            self.timer_counter = 0;
            self.timer_flag = false;
        } else if (address == 0x297) { // TIM1024T: Set timer, count every 1024 cycles
            self.timer = value;
            self.interval = 1024;
            self.timer_counter = 0;
            self.timer_flag = false;
        } else {
            std.debug.print("[W] Unhandled RIOT write: 0x{X}=0x{X}\n", .{address, value});
        }
    }

    pub fn tick(self: *RIOT) void {
        self.cycles +%= 1;
        self.timer_counter +%= 1;
        if (self.timer_counter >= self.interval) {
            const old_timer = self.timer;
            self.timer -%= 1;
            if (old_timer == 0) {
                self.timer_flag = true;
            }
            self.timer_counter = 0;
        }
    }

};
