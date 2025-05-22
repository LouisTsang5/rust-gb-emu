use minifb::Window;

use crate::{
    constants::{
        PALETTE_RGB, SCREEN_PIXEL_HEIGHT, SCREEN_PIXEL_WIDTH, SCX_ADDR, SCY_ADDR,
        TILES_ARR_START_ADDR, TILE_MAP_START_ADDR, TILE_MAP_WIDTH, TILE_SIZE, TILE_WIDTH,
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

pub fn rgb_to_u32(rgb: &[u8; 3]) -> u32 {
    (rgb[0] as u32) << 16 | (rgb[1] as u32) << 8 | rgb[2] as u32
}

fn get_palette(tiles_arr: &[u8], tile_idx: usize, tile_x: usize, tile_y: usize) -> u8 {
    // Get the two bytes
    let tile = tiles_arr.chunks(TILE_SIZE as usize).nth(tile_idx).unwrap();
    let b_lo = tile[tile_y * 2];
    let b_hi = tile[tile_y * 2 + 1];
    let rs = 7 - tile_x;

    // Find the palette to use
    (((b_hi >> rs) & 0x1) << 1) | ((b_lo >> rs) & 0x1)
}

#[derive(Debug)]
pub struct Ppu {
    memory: MemoryHandle,
    framebuf: [u32; SCREEN_PIXEL_WIDTH * SCREEN_PIXEL_HEIGHT],
    window: Window,
}

fn get_bg_palette(
    tiles_arr: &[u8],
    bg_map: &[u8],
    screen_x: usize,
    screen_y: usize,
    scroll_x: u8,
    scroll_y: u8,
) -> u8 {
    let x = (screen_x + scroll_x as usize) % 256;
    let y = (screen_y + scroll_y as usize) % 256;

    // Calculate tile index
    let tile_map_x = x / TILE_WIDTH as usize;
    let tile_map_y = y / TILE_WIDTH as usize;
    let tile_map_idx = tile_map_y * TILE_MAP_WIDTH + tile_map_x;

    // Calculate the coordinate within the tile
    let tile_x = x % TILE_WIDTH as usize;
    let tile_y = y % TILE_WIDTH as usize;

    // Get palette for BG & Window
    let palette = get_palette(tiles_arr, bg_map[tile_map_idx] as usize, tile_x, tile_y);
    palette
}

fn get_win_palette(tiles_arr: &[u8], win_map: &[u8], screen_x: usize, screen_y: usize) -> u8 {
    let x = screen_x;
    let y = screen_y;

    // Calculate tile index
    let tile_map_x = x / TILE_WIDTH as usize;
    let tile_map_y = y / TILE_WIDTH as usize;
    let tile_map_idx = tile_map_y * TILE_MAP_WIDTH + tile_map_x;

    // Calculate the coordinate within the tile
    let tile_x = x % TILE_WIDTH as usize;
    let tile_y = y % TILE_WIDTH as usize;

    // Get palette for BG & Window
    let palette = get_palette(tiles_arr, win_map[tile_map_idx] as usize, tile_x, tile_y);
    palette
}

impl Ppu {
    fn render_screen(&mut self, scroll_x: u8, scroll_y: u8) {
        let vram = self.memory.vram();
        const ARR_END: usize = (TILE_MAP_START_ADDR - TILES_ARR_START_ADDR) as usize;
        let tiles_arr = &vram[0..ARR_END];
        let bg_map = vram[ARR_END..vram.len()]
            .chunks(TILE_MAP_WIDTH * TILE_MAP_WIDTH)
            .nth(0)
            .unwrap();
        let win_map = vram[ARR_END..vram.len()]
            .chunks(TILE_MAP_WIDTH * TILE_MAP_WIDTH)
            .nth(1)
            .unwrap();

        // For each pixel
        for screen_y in 0..SCREEN_PIXEL_HEIGHT {
            for screen_x in 0..SCREEN_PIXEL_WIDTH {
                // Get palette for BG & Window
                let bg_palette =
                    get_bg_palette(tiles_arr, bg_map, screen_x, screen_y, scroll_x, scroll_y);
                let win_palette = get_win_palette(tiles_arr, win_map, screen_x, screen_y);
                let palette_idx = match win_palette > 0 {
                    true => win_palette,
                    false => bg_palette,
                };

                // Set the framebuf
                let framebuf_idx = screen_x + screen_y * SCREEN_PIXEL_WIDTH;
                self.framebuf[framebuf_idx] = rgb_to_u32(&PALETTE_RGB[palette_idx as usize]);
            }
        }
    }

    pub fn is_window_open(&self) -> bool {
        self.window.is_open()
    }

    pub fn render(&mut self) {
        // Render BG
        {
            let scroll_x = self.memory.read(SCX_ADDR);
            let scroll_y = self.memory.read(SCY_ADDR);
            self.render_screen(scroll_x, scroll_y);
        }

        // Update window
        self.window
            .update_with_buffer(&self.framebuf, SCREEN_PIXEL_WIDTH, SCREEN_PIXEL_HEIGHT)
            .unwrap();
    }
}
