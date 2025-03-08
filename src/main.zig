const std = @import("std");
const CPU = @import("cpu.zig").CPU;
const BUS = @import("bus.zig").BUS;
const CAR = @import("cartridge.zig");
const RIOT = @import("riot.zig").RIOT;


pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var riot = try RIOT.init(allocator);
    defer riot.deinit();

    var cart = try CAR.Cartridge.init(allocator, "game.bin");
    defer cart.deinit();

    var bus = try BUS.init(allocator, &cart, &riot);
    defer bus.deinit();

    var cpu = try CPU.init(allocator, &bus);
    // defer cpu.deinit();

    cart.printInfo();
    cart.dumpRom(cart.size);

    bus.reset();

    if (cart.getEntryPoint()) |entrypoint|
        cpu.reset(entrypoint);

    // const a = bus.readCart(0x8040);
    // std.debug.print("|{}|\n", .{a});

    while (true) {
        cpu.cycle();
        riot.cycle();
        // std.time.sleep(100*100*100);
        std.time.sleep(100*1000*100_0);
    }
}

