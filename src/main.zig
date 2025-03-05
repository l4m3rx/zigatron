const std = @import("std");
const RAM = @import("mem.zig").RAM;
const PIA = @import("pia.zig").PIA;
const CPU = @import("cpu.zig").CPU;
const BUS = @import("bus.zig").BUS;
const CAR = @import("cartridge.zig");


pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var ram = try RAM.init(allocator, 1024);
    defer ram.deinit();

    var pia = try PIA.init(allocator);
    defer pia.deinit();

    var cart = try CAR.Cartridge.init(allocator, "game.bin");
    defer cart.deinit();

    var bus = try BUS.init(allocator, &ram, &cart, &pia);
    defer bus.deinit();

    var cpu = try CPU.init(allocator, &bus);
    defer cpu.deinit();

    ram.nuller();
    bus.reset();

    cart.printInfo();
    // cart.dumpRom(cart.size);

    if (cart.getEntryPoint()) |entrypoint|
        cpu.reset(entrypoint);

    // const a = bus.readCart(0x8040);
    // std.debug.print("|{}|\n", .{a});
    while (true) {
        cpu.cycle();
        pia.cycle();
        std.time.sleep(100*100*100);
        // std.time.sleep(100*1000*1000);
    }
}

