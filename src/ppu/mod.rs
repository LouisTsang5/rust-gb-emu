use std::io::Write;

use crate::{
    constants::{
        PALETTE_RGB, SCREEN_PIXEL_HEIGHT, SCREEN_PIXEL_WIDTH, SCX_ADDR, SCY_ADDR,
        TILES_ARR_START_ADDR, TILE_MAP_START_ADDR, TILE_MAP_WIDTH, TILE_SIZE, TILE_WIDTH,
    },
    mem::MemoryHandle,
};

pub fn make(memory: MemoryHandle) -> Ppu {
    Ppu {
        memory,
        framebuf: Box::new([[0u8; 3]; SCREEN_PIXEL_WIDTH * SCREEN_PIXEL_HEIGHT]),
    }
}

#[derive(Debug)]
pub struct Ppu {
    memory: MemoryHandle,
    framebuf: Box<[[u8; 3]; SCREEN_PIXEL_WIDTH * SCREEN_PIXEL_HEIGHT]>,
}

impl Ppu {
    pub fn render(&mut self) {
        let vram = self.memory.vram();
        const ARR_END: usize = (TILE_MAP_START_ADDR - TILES_ARR_START_ADDR) as usize;
        let tiles_arr = &vram[0..ARR_END];
        let tiles_map = &vram[ARR_END..vram.len()];

        // Calculate x & y offset
        let scroll_x = self.memory.read(SCX_ADDR);
        let scroll_y = self.memory.read(SCY_ADDR);

        // For each pixel
        for screen_y in 0..SCREEN_PIXEL_HEIGHT {
            for screen_x in 0..SCREEN_PIXEL_WIDTH {
                // Calculate x & y with SCX & SCY offset
                let x = (screen_x + scroll_x as usize) % 256;
                let y = (screen_y + scroll_y as usize) % 256;

                // Calculate tile index
                let tile_map_x = x / TILE_WIDTH as usize;
                let tile_map_y = y / TILE_WIDTH as usize;
                let tile_map_idx = tile_map_y * TILE_MAP_WIDTH + tile_map_x;
                let tile_idx = tiles_map[tile_map_idx] as usize;

                // Calculate the coordinate within the tile
                let tile_x = x % TILE_WIDTH as usize;
                let tile_y = y % TILE_WIDTH as usize;

                // Get the two bytes
                let tile = tiles_arr.chunks(TILE_SIZE as usize).nth(tile_idx).unwrap();
                let b_lo = tile[tile_y * 2];
                let b_hi = tile[tile_y * 2 + 1];
                let rs = 7 - tile_x;

                // Find the palette to use
                let palette_idx = ((((b_hi >> rs) & 0x1) << 1) | ((b_lo >> rs) & 0x1)) as usize;

                // Set the framebuf
                let framebuf_idx = screen_x + screen_y * SCREEN_PIXEL_WIDTH;
                self.framebuf[framebuf_idx] = rgb_to_u32(&PALETTE_RGB[palette_idx]);
            }
        }

        // TODO: TEMP Render logic
        let file = std::fs::File::create("test.ppm").unwrap();
        let mut file = std::io::BufWriter::new(file);
        let header = format!("P6\n{} {}\n255\n", SCREEN_PIXEL_WIDTH, SCREEN_PIXEL_HEIGHT);
        file.write(header.as_bytes()).unwrap();
        for p in self.framebuf.iter() {
            file.write(p).unwrap();
        }
    }
}
