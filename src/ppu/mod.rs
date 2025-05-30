use minifb::Window;

use crate::{
    constants::{
        LCDC_ADDR, LCDC_BG_MAP_MASK, LCDC_BG_WIN_ADDR_MODE_MASK, LCDC_BG_WIN_PRIORITY_MASK,
        LCDC_OBJ_ENABLE_MASK, LCDC_OBJ_SIZE_MASK, LCDC_WIN_ENABLE_MASK, LCDC_WIN_MAP_MASK,
        OAM_END_ADDR, OAM_ENTRY_SIZE, OAM_START_ADDR, PALETTE_RGB, SCREEN_PIXEL_HEIGHT,
        SCREEN_PIXEL_WIDTH, SCX_ADDR, SCY_ADDR, TILE_MAP_START_ADDR, TILE_MAP_WIDTH, TILE_SIZE,
        TILE_WIDTH, VRAM_START_ADDR, WX_ADDR, WX_OFFSET, WY_ADDR,
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
    fn palette(&self, x: u8, y: u8) -> u8 {
        // Get the two bytes
        let tile = self.0;
        let b_lo = tile[y as usize * 2];
        let b_hi = tile[y as usize * 2 + 1];
        let rs = 7 - x;

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
}

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
    Tile::from(tile).palette(tile_x, tile_y)
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
                self.framebuf[framebuf_idx] = PALETTE_RGB[palette_idx];
            }
        }
    }

    fn render_object(&mut self, lcdc: u8) {
        let oam = self.memory.oam();
        let vram = self.memory.vram();
        let obj_double_size = lcdc & LCDC_OBJ_SIZE_MASK > 0;

        const N_OBJECTS: usize = ((OAM_END_ADDR - OAM_START_ADDR) / OAM_ENTRY_SIZE as u16) as usize;
        for obj in oam
            .chunks(OAM_ENTRY_SIZE as usize)
            .take(N_OBJECTS)
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

                        // Update frame buffer
                        let framebuf_idx =
                            screen_x as usize + screen_y as usize * SCREEN_PIXEL_WIDTH;
                        self.framebuf[framebuf_idx] =
                            PALETTE_RGB[tile.palette(tile_x, tile_y) as usize];
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
}
