const std = @import("std");
const RAM = @import("mem.zig").RAM;
const CAR = @import("cartridge.zig").Cartridge;


pub const BUS = struct {
    allocator: std.mem.Allocator,
    ram: *RAM = undefined,
    car: *CAR = undefined,
    bus: []u8,

    pub fn init(allocator: std.mem.Allocator, ram: *RAM, car: *CAR) !BUS {
        const bus = try allocator.alloc(u8, 1024);
        const b = BUS{
            .allocator = allocator,
            .bus = bus,
            .car = car,
            .ram = ram
        };
        return b;
    }

    pub fn reset(self: *BUS) void {
        for (self.bus) |*p|
            p.* = 0;
    }

    pub fn deinit(self: *BUS) void {
        self.allocator.free(self.bus);
    }

    pub fn readRam(self: *BUS, addr: u16) u8 {
        std.debug.print("Read RAM address: 0x{X:0>4}\n", .{addr});
        return self.car.read(addr);
    }

    pub fn readCart(self: *BUS, addr: u16) u8 {
        // std.debug.print("Read bus address: 0x{X:0>4}\n", .{addr});
        return self.car.read(addr);
    }

    pub fn read(self: *BUS, addr: u16) u8 {
        // TODO: Use enums?
        if ((addr >= 0) and (addr <= 0x007F))
            return self.readRam(addr)
        else if ((addr >= 0x80) and (addr <= 0xFF))
            return 0 // RIOT
        else if ((addr >= 0x0200) and (addr <= 0x02FF))
            return 0 // TIA (Television interface adapter)
        else if ((addr >= 0x1000) and (addr <= 0xFFFF))
            return self.readCart(addr & 0x1FFF)
        else
            return 0; // bad place!
    }

    pub fn write(self: *BUS, addr: u16, data: u8) void {
        if ((addr >= 0) and (addr <= 0x00FF))
            self.ram.write(addr, data)
        else if ((addr >= 0xE000) and (addr <= 0x1FFF))
            self.bus[addr] = data
        else
            std.debug.print("[warn]: Unhandled write request [Address: 0x{X}]\n", .{addr});
            return;
    }
};

