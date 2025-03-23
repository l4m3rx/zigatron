const std = @import("std");

pub const RIOT = struct {
    allocator: std.mem.Allocator,
    cycles: u16,
    timer: u8,          // Current timer value
    interval: u16,      // Countdown interval (1, 8, 64, or 1024)
    timer_counter: u16, // Counts cycles until the interval is reached
    timer_flag: bool,   // Timer underflow flag
    pram: []u8,
    swcha: u8,
    swchb: u8,

    pub fn init(allocator: std.mem.Allocator) !RIOT {
        const pram = try allocator.alloc(u8, 128);
        return RIOT{
            .allocator = allocator,
            .pram = pram,
            .swcha = 0xFF,
            .swchb = 0xFF,
            .cycles = 0,
            .timer = 0,
            .interval = 1,
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
            return self.swcha;
        } else if (address == 0x282) {
            return self.swchb;
        } else if (address == 0x284) { // INTIM: Timer read register
            std.debug.print("[D] Timer Value: {}\n", .{self.timer});
            return self.timer;
        } else if (address == 0x285) { // TIMINT: Timer interrupt flag
            const flag = if (self.timer_flag) 0x80 else 0x00;
            self.timer_flag = false; // Reading clears the flag
            return flag;
        } else {
            std.debug.print("[W] Unhandled RIOT read: 0x{X}\n", .{address});
            return 0xFF;
        }
    }

    pub fn write(self: *RIOT, address: u16, value: u8) void {
        std.debug.print("[D] RIOT Writing: 0x{X}={d}\n", .{address, value});
        if (address >= 0x80 and address <= 0xFF) {
            self.pram[address - 0x80] = value;
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

    pub fn cycle(self: *RIOT) void {
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
