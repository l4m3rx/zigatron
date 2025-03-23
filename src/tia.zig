const std = @import("std");

//   160 x 192 pixels (NTSC)
//
//   | Clock Counts
//   |<--------------------- 228 --------------------->|
//   |<-- 68 --|--------------- 160 ------------------>|
//
//   |-------------------------------------------------|   -3  -  (vsync)
//   |-------------------------------------------------|   -   |
//   |                                                 |   |   |
//   |       VBlank                                    |   37  |
//   |                                                 |   |   |
//   |-------------------------------------------------|   -   |
//   |         |                                       |   |   |
//   |         |                                       |   |   |
//   |         |                                       |   |  262  (212? PAL)
//   |         |                                       |   |   |
//   | HBLank  |                                       |  192  |
//   |         |                                       |   |   |
//   |         |                                       |   |   |
//   |         |                                       |   |   |
//   |         |                                       |   |   |
//   |-------------------------------------------------|   -   |
//   |                                                 |   |   |
//   |      Overscan                                   |   30  |
//   |                                                 |   |   |
//   |-------------------------------------------------|   -   -
//
//   |<------------- 78 machine cycles --------------->|
//            3 clock counts per machine cycle
//

pub const TIA = struct {
    allocator: std.mem.Allocator,
    // Framebuffer
    framebuffer: []u8,
    // Color Registers (7-bit values, but stored as u8 for simplicity)
    color_p0: u8,  // COLUP0: Color for Player 0
    color_p1: u8,  // COLUP1: Color for Player 1
    color_pf: u8,  // COLUPF: Color for Playfield
    color_bg: u8,  // COLUBK: Color for Background
    // Playfield Registers (PF0 uses bits 4-7, PF1 and PF2 use all 8 bits)
    pf0: u8,  // PF0: Playfield pattern (left 4 bits)
    pf1: u8,  // PF1: Playfield pattern (middle 8 bits)
    pf2: u8,  // PF2: Playfield pattern (right 8 bits)
    // Player Graphics (8-bit patterns for sprites)
    grp0: u8,  // GRP0: Graphics for Player 0
    grp1: u8,  // GRP1: Graphics for Player 1
    // Enable Flags for Missiles and Ball
    enam0: bool,  // ENAM0: Enable Missile 0
    enam1: bool,  // ENAM1: Enable Missile 1
    enabl: bool,  // ENABL: Enable Ball
    // Control Registers
    nusiz0: u8,   // NUSIZ0: Number and size of Player 0 and Missile 0
    nusiz1: u8,   // NUSIZ1: Number and size of Player 1 and Missile 1
    ctrlpf: u8,   // CTRLPF: Playfield control (reflection, ball size, etc.)
    refp0: bool,  // REFP0: Reflect Player 0
    refp1: bool,  // REFP1: Reflect Player 1
    // Horizontal Motion Registers (signed values, but stored as u8)
    hmp0: u8,  // HMP0: Horizontal motion for Player 0
    hmp1: u8,  // HMP1: Horizontal motion for Player 1
    hmm0: u8,  // HMM0: Horizontal motion for Missile 0
    hmm1: u8,  // HMM1: Horizontal motion for Missile 1
    hmbl: u8,  // HMBL: Horizontal motion for Ball
    // Vertical Delay Flags
    vdelp0: bool,  // VDELP0: Vertical delay for Player 0
    vdelp1: bool,  // VDELP1: Vertical delay for Player 1
    vdelbl: bool,  // VDELBL: Vertical delay for Ball
    // Audio Registers
    audc0: u8,  // AUDC0: Audio control for Channel 0
    audc1: u8,  // AUDC1: Audio control for Channel 1
    audf0: u8,  // AUDF0: Audio frequency for Channel 0
    audf1: u8,  // AUDF1: Audio frequency for Channel 1
    audv0: u8,  // AUDV0: Audio volume for Channel 0
    audv1: u8,  // AUDV1: Audio volume for Channel 1
    // Object Positions (horizontal positions, 0-159)
    pos_p0: u16,  // Player 0 position
    pos_p1: u16,  // Player 1 position
    pos_m0: u16,  // Missile 0 position
    pos_m1: u16,  // Missile 1 position
    pos_bl: u16,  // Ball position
    // Collision Registers (latches, bits set on collision)
    cxm0p: u8,   // CXM0P: Missile 0 collisions (bit 7: M0-P1, bit 6: M0-P0)
    cxm1p: u8,   // CXM1P: Missile 1 collisions
    cxp0fb: u8,  // CXP0FB: Player 0 collisions with PF and Ball
    cxp1fb: u8,  // CXP1FB: Player 1 collisions
    cxm0fb: u8,  // CXM0FB: Missile 0 collisions
    cxm1fb: u8,  // CXM1FB: Missile 1 collisions
    cxblpf: u8,  // CXBLPF: Ball-Playfield collision
    cxppmm: u8,  // CXPPMM: Player and Missile collisions
    // Screen
    x: u8,        // horizontal position
    y: u16,       // aka scanline (0-261 NTSC)
    vsync: bool,  // VSYNC: Vertical sync enabled
    vblank: bool, // VBLANK: Vertical blank enabled

    cycles: u32,  // TIA cycles

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) !TIA {
        const fb = try allocator.alloc(u8, 192*160); // 192x160px
        return TIA{
            .allocator = allocator,
            .framebuffer = fb,
            .color_p0 = 0,
            .color_p1 = 0,
            .color_pf = 0,
            .color_bg = 0,
            .pf0 = 0,
            .pf1 = 0,
            .pf2 = 0,
            .grp0 = 0,
            .grp1 = 0,
            .enam0 = false,
            .enam1 = false,
            .enabl = false,
            .nusiz0 = 0,
            .nusiz1 = 0,
            .ctrlpf = 0,
            .refp0 = false,
            .refp1 = false,
            .hmp0 = 0,
            .hmp1 = 0,
            .hmm0 = 0,
            .hmm1 = 0,
            .hmbl = 0,
            .vdelp0 = false,
            .vdelp1 = false,
            .vdelbl = false,
            .audc0 = 0,
            .audc1 = 0,
            .audf0 = 0,
            .audf1 = 0,
            .audv0 = 0,
            .audv1 = 0,
            .pos_p0 = 0,
            .pos_p1 = 0,
            .pos_m0 = 0,
            .pos_m1 = 0,
            .pos_bl = 0,
            .cxm0p = 0,
            .cxm1p = 0,
            .cxp0fb = 0,
            .cxp1fb = 0,
            .cxm0fb = 0,
            .cxm1fb = 0,
            .cxblpf = 0,
            .cxppmm = 0,
            .x = 0,
            .y = 0,
            .vsync = false,
            .vblank = false,
            .cycles = 0,
        };
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.framebuffer);
    }

    pub fn write(self: *Self, address: u16, data: u8) void {
        switch (address) {
            0x00 => self.vsync =  ((data & 0x02) != 0), // VSYNC
            0x01 => self.vblank = ((data & 0x02) != 0), // VBLANK
            0x02 => {},                                 // WSYNC (TODO: Sync CPU to HBLANK)
            0x05 => self.nusiz1 = data,                 // NUSIZ1
            0x06 => self.color_p0 = data,               // COLUP0
            0x07 => self.color_p1 = data,               // COLUP1
            0x08 => self.color_pf = data,               // COLUPF
            0x09 => self.color_bg = data,               // COLUBK
            0x0A => self.ctrlpf = data,                 // CTRLPF
            0x0B => self.refp0 = ((data & 0x08) != 0),  // REFP0
            0x0C => self.refp1 = ((data & 0x08) != 0),  // REFP1
            0x0D => self.pf0 = data,                    // PF0
            0x0E => self.pf1 = data,                    // PF1
            0x0F => self.pf2 = data,                    // PF2
            0x10 => self.pos_p0 = @intCast(self.x),     // RESP0: Set to current horizontal position
            0x11 => self.pos_p1 = @intCast(self.x),     // RESP1
            0x12 => self.pos_m0 = @intCast(self.x),     // RESM0
            0x13 => self.pos_m1 = @intCast(self.x),     // RESM1
            0x14 => self.pos_bl = @intCast(self.x),     // RESBL
            0x15 => self.audc0 = data,                  // AUDC0
            0x16 => self.audc1 = data,                  // AUDC1
            0x17 => self.audf0 = data,                  // AUDF0
            0x18 => self.audf1 = data,                  // AUDF1
            0x19 => self.audv0 = data,                  // AUDV0
            0x1A => self.audv1 = data,                  // AUDV1
            0x1B => self.grp0 = data,                   // GRP0
            0x1C => self.grp1 = data,                   // GRP1
            0x1D => self.enam0 = ((data & 0x02) != 0),  // ENAM0
            0x1E => self.enam1 = ((data & 0x02) != 0),  // ENAM1
            0x1F => self.enabl = ((data & 0x02) != 0),  // ENABL
            0x20 => self.hmp0 = data,                   // HMP0
            0x21 => self.hmp1 = data,                   // HMP1
            0x22 => self.hmm0 = data,                   // HMM0
            0x23 => self.hmm1 = data,                   // HMM1
            0x24 => self.hmbl = data,                   // HMBL
            0x25 => self.vdelp0 = ((data & 0x01) != 0), // VDELP0
            0x26 => self.vdelp1 = ((data & 0x01) != 0), // VDELP1
            0x27 => self.vdelbl = ((data & 0x01) != 0), // VDELBL
            0x28 => if ((data & 0x02) != 0) { self.pos_m0 = self.pos_p0; }, // RESMP0
            0x29 => if ((data & 0x02) != 0) { self.pos_m1 = self.pos_p1; }, // RESMP1
            0x2A => {   // HMOVE (TODO: verify, colision check?)
                self.pos_p0 += self.hmp0;
                self.pos_p1 += self.hmp1;
                self.pos_m0 += self.hmm0;
                self.pos_m1 += self.hmm1;
                self.pos_bl += self.hmbl;
                // while (self.pos_p0 < 68)  self.pos_p0 += 160;
                // while (self.pos_p0 > 228) self.pos_p0 -= 160;
                // while (self.pos_p1 < 68)  self.pos_p1 += 160;
                // while (self.pos_p1 > 228) self.pos_p1 -= 160;
                // while (self.pos_m0 < 68)  self.pos_m0 += 160;
                // while (self.pos_m0 > 228) self.pos_m0 -= 160;
                // while (self.pos_m1 < 68)  self.pos_m1 += 160;
                // while (self.pos_m1 > 228) self.pos_m1 -= 160;
                // while (self.pos_bl < 68)  self.pos_bl += 160;
                // while (self.pos_bl > 228) self.pos_bl -= 160;
            },
            0x2B => {   // HMCLR
                self.hmp0 = 0;
                self.hmp1 = 0;
                self.hmm0 = 0;
                self.hmm1 = 0;
                self.hmbl = 0;
            },
            0x2C => {   // CXCLR
                self.cxm0p = 0;
                self.cxm1p = 0;
                self.cxp0fb = 0;
                self.cxp1fb = 0;
                self.cxm0fb = 0;
                self.cxm1fb = 0;
                self.cxblpf = 0;
                self.cxppmm = 0;
            },
            else => { std.debug.print("[E] TIA: Bad Write address 0x{X}\n", .{address}); },
        }
    }

    pub fn read(self: *Self, address: u16) u8 {
        switch (address) {
            0x00 => return self.cxm0p,  // CXM0P
            0x01 => return self.cxm1p,  // CXM1P
            0x02 => return self.cxp0fb, // CXP0FB
            0x03 => return self.cxp1fb, // CXP1FB
            0x04 => return self.cxm0fb, // CXM0FB
            0x05 => return self.cxm1fb, // CXM1FB
            0x06 => return self.cxblpf, // CXBLPF
            0x07 => return self.cxppmm, // CXPPMM
            0x08 => return 0,           // INPT0
            0x09 => return 0,           // INPT1
            0x0A => return 0,           // INPT2
            0x0B => return 0,           // INPT3
            0x0C => return 0x80,        // INPT4
            0x0D => return 0x80,        // INPT5
            else => return 0,           // Default for unmapped reads
        }
    }

    pub fn increment(self: *Self) void {
        self.cycles +%= 1;
    }

    fn getPlayfieldBit(self: *Self, pixel_x: u8) bool {
        const pf_index = pixel_x / 4; // Calc playfield index
        // Determine if the pixel is in the right half of the screen
        const is_right_half = pf_index >= 20;
        // Compute the playfield bit position (0 to 19)
        const bit_pos = if (!is_right_half)
                            pf_index // Left half: direct mapping
                        else if (self.ctrlpf & 0x01 == 0)
                            pf_index - 20 // Right half, repeat mode
                        else
                            39 - pf_index; // Right half, reflect mode
        // Select the appropriate playfield register based on bit position
        const pf_reg = if (bit_pos < 4)
                           self.pf0 // Bits 0-3 from PF0 (bits 4-7)
                       else if (bit_pos < 12)
                           self.pf1 // Bits 4-11 from PF1 (bits 7-0, reversed)
                       else
                           self.pf2; // Bits 12-19 from PF2 (bits 0-7)
        // Calculate the shift amount for the selected register, ensuring it's a u3 type
        const shift: u3 = if (bit_pos < 4)
                              @intCast(4 + bit_pos) // PF0: bit 4 to 7
                          else if (bit_pos < 12)
                              @intCast(11 - bit_pos) // PF1: bit 7 to 0 (reversed)
                          else
                              @intCast(bit_pos - 12); // PF2: bit 0 to 7
        // Extract the bit and return whether it's set
        const bit = (pf_reg >> shift) & 1;
        return bit != 0;
    }

    pub fn tick(self: *Self) void {
        self.cycles +%= 1;
        self.x +%= 1;
        if (self.x == 228) {
            self.x = 0;
            self.y +%= 1;
            if (self.y == 262) {
                self.y = 0;
                // TODO: Signal end of frame
            }
        }
        // Render during visible area: x = 68–227, when not in vblank
        if (!self.vblank and self.x >= 68 and self.x < 228) {
            const pixel_x = self.x - 68;
            if (self.y < 192) {
                const index = self.y * 160 + pixel_x;
                var color = self.color_bg;
                if (self.getPlayfieldBit(pixel_x)) {
                    color = self.color_pf;
                }
                // Player 0: 8 pixels wide starting at pos_p0
                if (self.x >= self.pos_p0 and self.x < self.pos_p0 + 8) {
                    const bit = 7 - (self.x - self.pos_p0); // Bit 7 is leftmost
                    if ((self.grp0 >> @intCast(bit)) & 1 != 0) {
                    // if ((self.grp0 >> bit) & 1 != 0) {
                        color = self.color_p0;
                    }
                }
                self.framebuffer[index] = color;
            }
        }

    }

    // pub fn inScreen(self: *TIA) !u8 {
    //     // Check if beam is in the viewable part of the screen
    // }
};
