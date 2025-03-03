const std = @import("std");

pub const Cartridge = struct {
    allocator: std.mem.Allocator,
    rom: []const u8,
    size: usize,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, file_path: []const u8) !Cartridge {
        // Open file
        const file = try std.fs.cwd().openFile(file_path, .{});
        defer file.close();

        // Validate size
        const file_size = try file.getEndPos();
        if (file_size < 2048 or file_size > 65536)
            std.debug.print("[warn] Unusual cartridge size: {} bytes. (Expected 2KB-64KB)\n", .{file_size});

        // Read the ROM data
        const rom = try file.readToEndAlloc(allocator, file_size);

        return Cartridge{
            .rom = rom,
            .size = file_size,
            .allocator = allocator,
        };
    }

    // Deinitialize the cartridge and free memory
    pub fn deinit(self: *Cartridge) void {
        self.allocator.free(self.rom);
    }

    // Read from ROM
    pub fn read(self: *Cartridge, addr: u16) u8 {
        if ((addr >= 0) and (addr <= 0xFFFF)) {
            if (addr <= self.size) {
                return self.rom[addr];
            }
        }
        return 0;
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
                std.debug.print("[warn] Reset vector (0x{x:0>4}) outside expected range (0x1000-0x1FFF)\n", .{vector});
                return vector;
            }
        }
        return null;
    }

    // Dump the first `limit` bytes of the ROM as a hex dump
    pub fn dumpRom(self: Self, limit: usize) void {
        const max = @min(limit, self.size);
        for (self.rom[0..max], 0..) |byte, i| {
            if (i % 16 == 0) std.debug.print("\n{x:0>4}: ", .{i});
            std.debug.print("{x:0>2} ", .{byte});
        }
        std.debug.print("\n", .{});
    }

    // Basic cartridge info
    pub fn printInfo(self: Self) void {
        std.debug.print("[info] Cartridge size: {} bytes\n", .{self.size});

        if (self.getNmiVector()) |nmi|
            std.debug.print("[info] NMI Vector: 0x{x:0>4}\n", .{nmi});
        if (self.getResetVector()) |reset|
            std.debug.print("[info] Reset Vector: 0x{x:0>4}\n", .{reset});
        if (self.getEntryPoint()) |entry|
            std.debug.print("[info] Entry Point: 0x{x:0>4}\n", .{entry});
    }
};
