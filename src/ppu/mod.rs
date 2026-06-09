use minifb::Window;

use crate::{
    constants::{
        BGP_ADDR, DOTS_PER_SCAN_LINE, IF_ADDR, LCDC_ADDR, LCDC_BG_MAP_MASK,
        LCDC_BG_WIN_ADDR_MODE_MASK, LCDC_BG_WIN_PRIORITY_MASK, LCDC_OBJ_ENABLE_MASK,
        LCDC_OBJ_SIZE_MASK, LCDC_WIN_ENABLE_MASK, LCDC_WIN_MAP_MASK, LCD_INTERRUPT_MASK, LYC_ADDR,
        LY_ADDR, N_SCAN_LINES, OAM_ENTRY_SIZE, OAM_OBJ_DMG_PALETTE_MASK, OAM_OBJ_FLIP_X_MASK,
        OAM_OBJ_FLIP_Y_MASK, OAM_OBJ_PRIORITY_MASK, OBP_0_ADDR, OBP_1_ADDR, PALETTE_RGB,
        PPU_MODE_HBLANK, PPU_MODE_VBLANK, SCREEN_PIXEL_HEIGHT, SCREEN_PIXEL_WIDTH, SCX_ADDR,
        SCY_ADDR, STAT_ADDR, STAT_HBLANK_INT_SELECT_MASK, STAT_LYC_INT_SELECT_MASK, STAT_LYC_MASK,
        STAT_MODE_1_INT_SELECT_MASK, STAT_MODE_2_INT_SELECT_MASK, TILE_MAP_START_ADDR,
        TILE_MAP_WIDTH, TILE_SIZE, TILE_WIDTH, VBLANK_INTERRUPT_MASK, VRAM_START_ADDR, WX_ADDR,
        WX_OFFSET, WY_ADDR,
    },
    mem::MemoryHandle,
};

struct Tile<'a>(&'a [u8]);

impl<'a> From<&'a [u8]> for Tile<'a> {
    fn from(value: &'a [u8]) -> Self {
        assert!(value.len() == TILE_SIZE as usize);
        Tile(value)
    }
}

impl Tile<'_> {
    fn palette(&self, x: u8, y: u8, flip_x: bool, flip_y: bool) -> u8 {
        // Get the two bytes
        let tile = self.0;
        let idx_y = (match flip_y {
            true => TILE_WIDTH - 1 - y,
            false => y,
        } * 2) as usize;
        let b_lo = tile[idx_y];
        let b_hi = tile[idx_y + 1];
        let rs = match flip_x {
            true => x,
            false => TILE_WIDTH - 1 - x,
        };

        // Find the palette to use
        (((b_hi >> rs) & 0x1) << 1) | ((b_lo >> rs) & 0x1)
    }
}

struct ObjectAttribute<'a>(&'a [u8]);

impl<'a> From<&'a [u8]> for ObjectAttribute<'a> {
    fn from(value: &'a [u8]) -> Self {
        assert!(value.len() == OAM_ENTRY_SIZE as usize);
        ObjectAttribute(value)
    }
}

impl ObjectAttribute<'_> {
    fn y(&self) -> u8 {
        self.0[0]
    }

    fn x(&self) -> u8 {
        self.0[1]
    }

    fn tile_idx(&self) -> usize {
        self.0[2] as usize
    }

    fn attr(&self) -> u8 {
        self.0[3]
    }

    fn priority(&self) -> bool {
        self.attr() & OAM_OBJ_PRIORITY_MASK > 0
    }

    fn flip_y(&self) -> bool {
        self.attr() & OAM_OBJ_FLIP_Y_MASK > 0
    }

    fn flip_x(&self) -> bool {
        self.attr() & OAM_OBJ_FLIP_X_MASK > 0
    }

    fn dmg_palette(&self) -> bool {
        self.attr() & OAM_OBJ_DMG_PALETTE_MASK > 0
    }
}

pub fn make(memory: MemoryHandle, window: Window) -> Ppu {
    Ppu {
        memory,
        framebuf: [0; SCREEN_PIXEL_WIDTH * SCREEN_PIXEL_HEIGHT],
        window,
        acc_dot_size: 0,
    }
}

fn get_map(vram: &[u8], map_idx: u8) -> &[u8] {
    assert!(map_idx < 2);
    const BASE_ADDR: usize = (TILE_MAP_START_ADDR - VRAM_START_ADDR) as usize;
    vram[BASE_ADDR..]
        .chunks(TILE_MAP_WIDTH * TILE_MAP_WIDTH)
        .nth(map_idx as usize)
        .unwrap()
}

#[derive(Debug)]
pub struct Ppu {
    memory: MemoryHandle,
    window: Window,
    framebuf: [u32; SCREEN_PIXEL_WIDTH * SCREEN_PIXEL_HEIGHT],
    acc_dot_size: u16,
}

fn get_palette(
    tiles_arr: &[u8],
    tiles_map: &[u8],
    x: usize, // The x coordinate of the full 256 x 256 BG / WIN picture
    y: usize, // The y coordinate of the full 256 x 256 BG / WIN picture
    unsigned_addr_mod: bool,
) -> u8 {
    // Calculate tile index
    let tile_map_x = x / TILE_WIDTH as usize;
    let tile_map_y = y / TILE_WIDTH as usize;
    let tile_map_idx = tile_map_y * TILE_MAP_WIDTH + tile_map_x;
    let tile_idx = tiles_map[tile_map_idx];

    // Calculate the coordinate within the tile
    let tile_x = x as u8 % TILE_WIDTH;
    let tile_y = y as u8 % TILE_WIDTH;

    // Adjust tile index if signed address mode is set
    let tile_idx = match unsigned_addr_mod {
        true => tile_idx as usize,
        false => {
            let signed_idx = unsafe { *((&tile_idx as *const u8) as *const i8) } as i32;
            const BASE_IDX: i32 = 256;
            (BASE_IDX + signed_idx) as usize
        }
    };

    // Get the two bytes
    let tile = tiles_arr.chunks(TILE_SIZE as usize).nth(tile_idx).unwrap();
    Tile::from(tile).palette(tile_x, tile_y, false, false)
}

impl Ppu {
    fn render_screen(&mut self, lcdc: u8) {
        // LCDC flags
        let win_enable = (lcdc & LCDC_WIN_ENABLE_MASK) > 0;
        let bg_win_addr_mode = (lcdc & LCDC_BG_WIN_ADDR_MODE_MASK) > 0;

        // BG & Window offsets
        let scroll_x = self.memory.read(SCX_ADDR) as usize;
        let scroll_y = self.memory.read(SCY_ADDR) as usize;
        let win_x = self.memory.read(WX_ADDR) as usize;
        let win_y = self.memory.read(WY_ADDR) as usize;

        // Tiles & Maps
        let vram = self.memory.vram();
        let tiles_arr = &vram[0..(TILE_MAP_START_ADDR - VRAM_START_ADDR) as usize];

        // Get map
        let bg_map = get_map(&vram, (lcdc & LCDC_BG_MAP_MASK) / LCDC_BG_MAP_MASK);
        let win_map = get_map(&vram, (lcdc & LCDC_WIN_MAP_MASK) / LCDC_WIN_MAP_MASK);

        // For each pixel
        const BG_WIDTH: usize = TILE_WIDTH as usize * TILE_MAP_WIDTH;
        for screen_y in 0..SCREEN_PIXEL_HEIGHT {
            for screen_x in 0..SCREEN_PIXEL_WIDTH {
                // Get palette for BG
                let bg_palette = get_palette(
                    tiles_arr,
                    bg_map,
                    (screen_x + scroll_x) % BG_WIDTH,
                    (screen_y + scroll_y) % BG_WIDTH,
                    bg_win_addr_mode,
                );

                // Get palette for Window
                let win_palette = match win_enable {
                    false => 0,
                    true => get_palette(
                        tiles_arr,
                        win_map,
                        (screen_x + WX_OFFSET).wrapping_sub(win_x) % BG_WIDTH,
                        screen_y.wrapping_sub(win_y) % BG_WIDTH,
                        bg_win_addr_mode,
                    ),
                };

                // Check priority
                let bgp_idx = match win_palette > 0 {
                    true => win_palette,
                    false => bg_palette,
                } as usize;

                // Get palette
                assert!(bgp_idx < 4);
                let bgp = self.memory.read(BGP_ADDR);
                let palette_idx = (bgp >> (bgp_idx * 2)) & 0x3;

                // Set the framebuf
                let framebuf_idx = screen_x + screen_y * SCREEN_PIXEL_WIDTH;
                self.framebuf[framebuf_idx] = PALETTE_RGB[palette_idx as usize];
            }
        }
    }

    fn render_object(&mut self, lcdc: u8) {
        let oam = self.memory.oam();
        let vram = self.memory.vram();
        let obj_double_size = lcdc & LCDC_OBJ_SIZE_MASK > 0;

        for obj in oam
            .chunks(OAM_ENTRY_SIZE as usize)
            .map(|o| ObjectAttribute::from(o))
        {
            let x = obj.x();
            let y = obj.y();
            let obj_size = match obj_double_size {
                true => 2,
                false => 1,
            };

            // // Skip this obj if it cannot be viewed
            // if x == 0
            //     || x >= SCREEN_PIXEL_WIDTH as u8 + TILE_WIDTH
            //     || y <= (TILE_WIDTH * 2 - obj_size * TILE_WIDTH)
            //     || y >= SCREEN_PIXEL_HEIGHT as u8 + TILE_WIDTH * 2
            // {
            //     continue;
            // }

            let tile_idx = match obj_double_size {
                false => obj.tile_idx(),
                true => obj.tile_idx() & 0xFE,
            } as usize;

            let tiles = vram
                .chunks(TILE_SIZE as usize)
                .skip(tile_idx)
                .take(obj_size as usize)
                .map(|t| Tile::from(t));

            // Tile iteration
            for (i, tile) in tiles.enumerate() {
                // Prevent iteration of off-screen pixels
                let tile_offset_y =
                    TILE_WIDTH * 2 - std::cmp::min(TILE_WIDTH * 2, y + i as u8 * TILE_WIDTH);
                let tile_limit_y = std::cmp::min(
                    TILE_WIDTH,
                    SCREEN_PIXEL_HEIGHT as u8 + TILE_WIDTH * 2
                        - std::cmp::min(
                            SCREEN_PIXEL_HEIGHT as u8 + TILE_WIDTH * 2,
                            y + i as u8 * TILE_WIDTH,
                        ),
                );
                for tile_y in tile_offset_y..tile_limit_y {
                    // Calculate screen y position
                    let screen_y = obj
                        .y()
                        .wrapping_add(tile_y)
                        .wrapping_add(i as u8 * TILE_WIDTH)
                        .wrapping_sub(TILE_WIDTH * 2);

                    // Prevent iteration of off-screen pixels
                    let tile_offset_x = TILE_WIDTH - std::cmp::min(TILE_WIDTH, x);
                    let tile_limit_x = std::cmp::min(
                        TILE_WIDTH,
                        SCREEN_PIXEL_WIDTH as u8 + TILE_WIDTH
                            - std::cmp::min(SCREEN_PIXEL_WIDTH as u8 + TILE_WIDTH, x),
                    );
                    for tile_x in tile_offset_x..tile_limit_x {
                        // Calculate screen x position
                        let screen_x = obj.x().wrapping_add(tile_x).wrapping_sub(TILE_WIDTH);

                        // Find frame buffer pixel
                        let framebuf_idx =
                            screen_x as usize + screen_y as usize * SCREEN_PIXEL_WIDTH;
                        let pixel = &mut self.framebuf[framebuf_idx];

                        // Don't draw if priority flag is set and pixel already has color
                        if obj.priority() && *pixel != PALETTE_RGB[0] {
                            continue;
                        }

                        // Get palette
                        let obp_idx = tile.palette(tile_x, tile_y, obj.flip_x(), obj.flip_y());
                        assert!(obp_idx < 4);
                        let obp = self.memory.read(match obj.dmg_palette() {
                            false => OBP_0_ADDR,
                            true => OBP_1_ADDR,
                        });
                        let palette_idx = (obp >> (obp_idx * 2)) & 0x3;

                        // Set pixel color
                        *pixel = PALETTE_RGB[palette_idx as usize];
                    }
                }
            }
        }
    }

    pub fn is_window_open(&self) -> bool {
        self.window.is_open()
    }

    pub fn render(&mut self) {
        // Make screen white
        for b in self.framebuf.iter_mut() {
            *b = 0x00FFFFFF;
        }

        // Get lcdc
        let lcdc = self.memory.read(LCDC_ADDR);

        // Render screen
        if (lcdc & LCDC_BG_WIN_PRIORITY_MASK) > 0 {
            self.render_screen(lcdc);
        }

        // Render objects
        if (lcdc & LCDC_OBJ_ENABLE_MASK) > 0 {
            self.render_object(lcdc);
        }

        // Update window
        self.window
            .update_with_buffer(&self.framebuf, SCREEN_PIXEL_WIDTH, SCREEN_PIXEL_HEIGHT)
            .unwrap();
    }

    pub fn step(&mut self, dots_taken: u16) {
        // Increment accumulated dots
        self.acc_dot_size += dots_taken;

        //  Advance scan lines
        while self.acc_dot_size >= DOTS_PER_SCAN_LINE {
            self.advance_scanline();
            self.acc_dot_size -= DOTS_PER_SCAN_LINE;
        }
    }

    fn advance_scanline(&mut self) {
        // Update LY register
        let ly = (self.memory.read(LY_ADDR) + 1) % N_SCAN_LINES;
        self.memory.write(LY_ADDR, ly);

        // Read STAT & LYC
        let lyc = self.memory.read(LYC_ADDR);
        let lyc_enable = ly == lyc;
        let enter_vblank = ly == SCREEN_PIXEL_HEIGHT as u8;
        let in_vblank = ly >= SCREEN_PIXEL_HEIGHT as u8;
        let stat = self.memory.read(STAT_ADDR);
        let lyc_select = (stat & STAT_LYC_INT_SELECT_MASK) > 0;
        let mode_2_select = (stat & STAT_MODE_2_INT_SELECT_MASK) > 0;
        let mode_1_select = (stat & STAT_MODE_1_INT_SELECT_MASK) > 0;
        let h_blank_select = (stat & STAT_HBLANK_INT_SELECT_MASK) > 0;

        // LCD interupt
        // The whole scan line is processed atomically in the emulator, therefore int select for mode 0-2 always triggers interrupt
        let int_mode_2_hblank = !in_vblank && (mode_2_select || h_blank_select);
        let int_mode_1 = in_vblank && mode_1_select;
        let int_lyc = lyc_select && lyc_enable;
        if int_mode_2_hblank || int_mode_1 || int_lyc {
            self.memory
                .write(IF_ADDR, self.memory.read(IF_ADDR) | LCD_INTERRUPT_MASK);
        }

        // VBlank interrupt when first enter vblank
        if enter_vblank {
            self.memory
                .write(IF_ADDR, self.memory.read(IF_ADDR) | VBLANK_INTERRUPT_MASK);
        }

        // Update STAT LYC flag
        let stat = match lyc_enable {
            true => stat | STAT_LYC_MASK,
            false => stat & !STAT_LYC_MASK,
        };

        // Update STAT PPU MODE flag
        // The whole scan line is processed atomically in the emulator, therefore ppu mode is always in either H-Blank or V-Blank
        let stat = stat & 0xFC // Clear lower 2 bits and set them base on current mode
            | match in_vblank {
                true => PPU_MODE_VBLANK,
                false => PPU_MODE_HBLANK,
            };

        // Write stat
        self.memory.write(STAT_ADDR, stat);
    }
}
