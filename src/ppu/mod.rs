use minifb::Window;

use crate::{
    constants::{
        LCDC_ADDR, LCDC_BG_MAP_MASK, LCDC_BG_WIN_ADDR_MODE_MASK, LCDC_BG_WIN_PRIORITY_MASK,
        LCDC_WIN_ENABLE_MASK, LCDC_WIN_MAP_MASK, PALETTE_RGB, SCREEN_PIXEL_HEIGHT,
        SCREEN_PIXEL_WIDTH, SCX_ADDR, SCY_ADDR, TILE_MAP_START_ADDR, TILE_MAP_WIDTH, TILE_SIZE,
        TILE_WIDTH, VRAM_START_ADDR, WX_ADDR, WX_OFFSET, WY_ADDR,
    },
    mem::MemoryHandle,
};

pub fn make(memory: MemoryHandle, window: Window) -> Ppu {
    Ppu {
        memory,
        framebuf: [0; SCREEN_PIXEL_WIDTH * SCREEN_PIXEL_HEIGHT],
        window,
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
    framebuf: [u32; SCREEN_PIXEL_WIDTH * SCREEN_PIXEL_HEIGHT],
    window: Window,
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
    let tile_x = x % TILE_WIDTH as usize;
    let tile_y = y % TILE_WIDTH as usize;

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
    let b_lo = tile[tile_y * 2];
    let b_hi = tile[tile_y * 2 + 1];
    let rs = 7 - tile_x;

    // Find the palette to use
    (((b_hi >> rs) & 0x1) << 1) | ((b_lo >> rs) & 0x1)
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
                let palette_idx = match win_palette > 0 {
                    true => win_palette,
                    false => bg_palette,
                } as usize;

                // Set the framebuf
                let framebuf_idx = screen_x + screen_y * SCREEN_PIXEL_WIDTH;
                let rgb = &PALETTE_RGB[palette_idx];
                self.framebuf[framebuf_idx] =
                    (rgb[0] as u32) << 16 | (rgb[1] as u32) << 8 | rgb[2] as u32;
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

        // Update window
        self.window
            .update_with_buffer(&self.framebuf, SCREEN_PIXEL_WIDTH, SCREEN_PIXEL_HEIGHT)
            .unwrap();
    }
}
