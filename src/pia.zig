const std = @import("std");

pub const PIA = struct {
    allocator: std.mem.Allocator,
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
};
