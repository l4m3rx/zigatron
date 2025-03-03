const std = @import("std");

pub const RAM = struct {
    allocator: std.mem.Allocator,
    ram: []u8,
    size: u16,

    pub fn init(allocator: std.mem.Allocator, size: u16) !RAM {
        const ram = try allocator.alloc(u8, 1024);
        const m = RAM{
            .allocator = allocator,
            .ram = ram,
            .size = size
        };
        return m;
    }

    pub fn deinit(self: *RAM) void {
        self.allocator.free(self.ram);
    }

    pub fn read(self: *RAM, address: u16) u8 {
        if ((address >= 0x0) and (address <= self.size)) {
            std.debug.print("[info] Reading address: 0x{x}\n", .{ address });
            return self.ram[address];
        } else {
            std.debug.print("[error] Address out of range 0x{X}\n", .{ address });
            return 0;
        }
    }

    pub fn write(self: *RAM, address: u16, value: u8) void {
        std.debug.print("Writing to address: 0x{x}={d}\n", .{ address, value });
        self.ram[address] = value;
    }

    pub fn nuller(self: *RAM) void {
        for (self.ram) |*p|
            p.* = 0;
    }

    pub fn dump(self: *RAM) void {
        var n: u16 = 0;
        for (self.ram) |*p| {
            std.debug.print("0x{X}: {}\n", .{n, p.*});
            n += 1;
        }
    }
};
