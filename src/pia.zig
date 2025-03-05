const std = @import("std");

pub const PIA = struct {
    allocator: std.mem.Allocator,
    cycles: u16,
    tim1t: u8,
    tim8t: u8,
    tim64t: u8,
    tim1024t: u8,
    pram: []u8,

    pub fn init(allocator: std.mem.Allocator) !PIA {
        const pram = try allocator.alloc(u8, 128);

        return PIA{
            .allocator = allocator,
            .pram = pram,
            .cycles = 0,
            .tim1t = 0,
            .tim8t = 0,
            .tim64t = 0,
            .tim1024t = 0
        };
    }

    pub fn deinit(self: *PIA) void {
        self.allocator.free(self.pram);
    }

    pub fn read(self: *PIA, address: u16) u8 {
        if ((address >= 0x0) and (address <= 0xFF)) {
            std.debug.print("[info] PIA RAM Reading address: 0x{X}\n", .{ address });
            return self.pram[address];
        } else {
            std.debug.print("[error] PIA RAM Address out of range 0x{X}\n", .{ address });
            return 0;
        }
    }

    pub fn write(self: *PIA, address: u16, value: u8) void {
        std.debug.print("Writing to PIA RAM address: 0x{X}={d}\n", .{ address, value });
        self.pram[address] = value;
    }

    pub fn cycle(self: *PIA) void {
        self.cycles +%= 1;

        // Timers
        self.tim1t -%= 1;
        if ((self.cycles % 8) == 0) self.tim8t -%= 1;
        if ((self.cycles % 64) == 0) self.tim64t -%= 1;
        if ((self.cycles % 1024) == 0) self.tim1024t -%= 1;
        // std.debug.print("[info] TIA Timers: t1: {}, t8: {}, t64: {}, t1024: {}\n",
        //                      .{self.tim1t, self.tim8t, self.tim64t, self.tim1024t});
    }
};
