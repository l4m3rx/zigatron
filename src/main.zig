const std = @import("std");
const CPU = @import("cpu.zig").CPU;
const BUS = @import("bus.zig").BUS;
const TIA = @import("tia.zig").TIA;
const RIOT = @import("riot.zig").RIOT;
const CAR = @import("cartridge.zig").Cartridge;


pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    // Get command line arguments
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    // Check if filename argument is provided
    if (args.len < 2) {
        std.debug.print("Usage: {s} <game_file.a26>\n", .{args[0]});
        return error.MissingArgument;
    }

    const game_file = args[1];

    var riot = try RIOT.init(allocator);
    defer riot.deinit();

    var tia = try TIA.init(allocator);
    defer tia.deinit();

    var cart = try CAR.init(allocator);
    defer cart.deinit();
    try cart.load(game_file);

    var bus = try BUS.init(allocator, &cart, &riot, &tia);
    defer bus.deinit();

    var cpu = try CPU.init(allocator, &bus);
    // defer cpu.deinit();

    cart.printInfo();
    // cart.dumpRom(cart.size);

    bus.reset();
    cpu.reset(cart.entry);

    // const a = bus.readCart(0x8040);
    // std.debug.print("|{}|\n", .{a});

    while (true) {
        cpu.cycle();
        riot.cycle();
        // std.time.sleep(100*100*100);
        std.time.sleep(100*1000*100_0);
    }
}

