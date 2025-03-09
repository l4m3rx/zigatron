const std = @import("std");
const CAR = @import("cartridge.zig").Cartridge;
const RIOT = @import("riot.zig").RIOT;


pub const BUS = struct {
    allocator: std.mem.Allocator,
    car: *CAR = undefined,
    riot: *RIOT = undefined,
    bus: []u8,

    pub fn init(allocator: std.mem.Allocator, car: *CAR, riot: *RIOT) !BUS {
        const bus = try allocator.alloc(u8, 1024);
        return BUS{
            .allocator = allocator,
            .bus = bus,
            .car = car,
            .riot = riot
        };
    }

    pub fn reset(self: *BUS) void {
        for (self.bus) |*p|
            p.* = 0;
    }

    pub fn deinit(self: *BUS) void {
        self.allocator.free(self.bus);
    }

    pub fn readStack(self: *BUS, addr: u16) u8 {
        // std.debug.print("[I] Stack pull \n", .{});
        return self.bus[addr];
    }

    pub fn writeStack(self: *BUS, addr: u16, data: u8) void {
        // std.debug.print("[I] Stack push {}\n", .{value});
        self.bus[addr] = data;
    }

    pub fn read(self: *BUS, addr: u16) u8 {
        // Extract Relevent Address bits
        const a7  = (addr & 0b0000_0000_1000_0000) != 0;
        const a9  = (addr & 0b0000_0010_0000_0000) != 0;
        const a12 = (addr & 0b0001_0000_0000_0000) != 0;

        if (a12) { // Cartrage memory is selected by A12=1
            return self.car.read(addr & 0x0FFF);
        } else if (a7 and a9) { // RIOT I/O is selected by A12=0, A9=1, A7=1
            std.debug.print("[D] Timer Read 0x{X}/0x{X}\n", .{addr, addr & 0x02FF});
            return self.riot.read(addr & 0x02FF);
        } else if ((!a9) and a7) { // RAM is selected by A12=0, A9=0, A7=1
            std.debug.print("[D] RAM Read 0x{X}/0x{X}\n", .{addr, addr & 0x7F});
            return self.riot.readRam(addr & 0x7F);
        } else { // The TIA chip is addressed by A12=0, A7=0
            return 0;
            // return self.tia.read((addr & 0x0F) | 0x30);
        }
    }

    pub fn write(self: *BUS, addr: u16, data: u8) void {
        // Extract Relevent Address bits
        const a7  = (addr & 0b0000_0000_1000_0000) != 0;
        const a9  = (addr & 0b0000_0010_0000_0000) != 0;
        const a12 = (addr & 0b0001_0000_0000_0000) != 0;

        if (a12) {
            // Cartridge space (0x1000–0x1FFF): A12=1
            // Standard cartridges are read-only (ROM), so writes are ignored
            // Note: If the cartridge has RAM (e.g., Super Chip), you could add logic like:
            // if (addr >= 0x1000 and addr <= 0x107F) {
            //     self.car[addr & 0x7F] = data;
            // }
        } else if (!a7) { // TIA registers (0x0000–0x007F): A12=0, A7=0
            // self.tia.write(addr & 0x3F, data);
        } else if (!a9) { // System RAM (RIOT RAM, 0x0080–0x00FF): A12=0, A7=1, A9=0
            self.riot.writeRam(addr & 0x7F, data);
        } else { // RIOT I/O registers (0x0280–0x029F): A12=0, A9=1
            self.riot.write(addr & 0x1F, data);
        }
    }

};

