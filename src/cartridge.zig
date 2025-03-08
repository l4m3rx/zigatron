const std = @import("std");

pub const Cartridge = struct {
    allocator: std.mem.Allocator,
    rom: []u8 = undefined,
    size: usize = 0,
    nmi: u16 = 0,
    reset: u16 = 0,
    entry: u16 = 0,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) !Cartridge {
        return Cartridge{
            .allocator = allocator,
        };
    }

    // Deinitialize the cartridge and free memory
    pub fn deinit(self: *Cartridge) void {
        self.allocator.free(self.rom);
    }

    // Load cartrage
    pub fn load(self: *Cartridge, filePath: []const u8) !void {
        const file = try std.fs.cwd().openFile(filePath, .{});
        defer file.close();

        // Validate size
        self.size = try file.getEndPos();
        if (self.size < 2048 or self.size > 65536)
            std.debug.print("[warn] Unusual cartridge size: {} bytes. (Expected 2KB-64KB)\n", .{self.size});

        // Read the ROM data
        self.rom = try file.readToEndAlloc(self.allocator, self.size);

        // Set NMI Vector, Reset Vector, Entry Point
        if (self.getNmiVector()) |nmi|
            self.nmi = nmi;
        if (self.getResetVector()) |reset|
            self.reset = reset;
        if (self.getEntryPoint()) |entry|
            self.entry = entry;
    }

    // Read from ROM
    pub fn read(self: *Cartridge, addr: u16) u8 {
        // std.debug.print("ROM Read 0x{X}\n", .{addr});
        if (addr <= self.size) {
            return self.rom[addr];
        } else {
            return self.rom[addr - self.size];
        }
        return 0;
    }

    // Write to ROM (ram?)
    // TODO: Add address limit
    pub fn write(self: *Cartridge, addr: u16, data: u8) void {
        self.rom[addr] = data;
    }

    // Read a little-endian u16 from a byte slice
    fn readLittleU16(bytes: []const u8) u16 {
        return @as(u16, bytes[1]) << 8 | @as(u16, bytes[0]);
    }

    // Get NMI Vector
    pub fn getNmiVector(self: Self) ?u16 {
        if (self.size >= 4096) {
            return readLittleU16(self.rom[self.size - 4 .. self.size - 2]);
        }
        return null;
    }

    // Get reset Vector
    pub fn getResetVector(self: Self) ?u16 {
        if (self.size == 2048) {
            return readLittleU16(self.rom[2046..2048]); // 2KB ROM
        } else if (self.size >= 4096) {
            return readLittleU16(self.rom[self.size - 2 .. self.size]); // 4KB or larger
        }
        return null;
    }

    // Get the entry point
    // Reset vector adjusted for cartridge mapping
    pub fn getEntryPoint(self: Self) ?u16 {
        if (self.getResetVector()) |vector| {
            if (vector >= 0x1000 and vector <= 0x1FFF) {
                return vector;
            } else {
                std.debug.print("[I] Reset vector (0x{X:0>4}) outside expected range (0x1000-0x1FFF)\n", .{vector});
                std.debug.print("[I] Using Vector 0x{X}\n", .{vector + 0x1000});
                return vector + 0x1000;
            }
        }
        return null;
    }

    // Dump the first `limit` bytes of the ROM as a hex dump
    pub fn dumpRom(self: Self, limit: usize) void {
        const max = @min(limit, self.size);
        for (self.rom[0..max], 0..) |byte, i| {
            if (i % 16 == 0) std.debug.print("\n{X:0>4}: ", .{i});
            std.debug.print("{X:0>2} ", .{byte});
        }
        std.debug.print("\n", .{});
    }

    // Basic cartridge info
    pub fn printInfo(self: Self) void {
        std.debug.print("[info] Cartridge size: {} bytes\n", .{self.size});
        std.debug.print("[info] NMI Vector: 0x{X:0>4}\n", .{self.nmi});
        std.debug.print("[info] Reset Vector: 0x{X:0>4}\n", .{self.reset});
        std.debug.print("[info] Entry Point: 0x{X:0>4}\n", .{self.entry});
    }
};
