// Memory
pub const MEM_SIZE: usize = u16::MAX as usize + 1;

// Interrupt
pub const IE_ADDR: u16 = 0xFFFF;
pub const IF_ADDR: u16 = 0xFF0F;
pub const T_N_INTERRUPT: u16 = 5;
pub const INTERRUPT_HANDLER_BASE_ADDR: u16 = 0x40;

// Timer
pub const DIV_ADDR: u16 = 0xFF04;
pub const TIMA_ADDR: u16 = 0xFF05;
pub const TMA_ADDR: u16 = 0xFF06;
pub const TAC_ADDR: u16 = 0xFF07;
pub const TAC_ENABLE_MASK: u8 = 0x04; // Bit 2
pub const TAC_CLOCK_SELECT_MASK: u8 = 0x03; // Bit 0 & 1
pub const DIV_INC_PER_M_CYCLE: u16 = 0x0004;
pub const TIMER_INTERRUPT_MASK: u8 = 0x04;

// Graphics
pub const VRAM_START_ADDR: u16 = 0x8000;
pub const VRAM_SIZE: u16 = 0x2000;
pub const TILES_ARR_START_ADDR: u16 = VRAM_START_ADDR;
pub const TILE_SIZE: u8 = 16;
pub const TILE_WIDTH: u8 = 8;
pub const TILE_MAP_START_ADDR: u16 = 0x9800;
pub const TILE_MAP_WIDTH: usize = 32;
pub const BGP_ADDR: u16 = 0xFF47;
pub const OBP_ADDRS: [u16; 2] = [BGP_ADDR + 1, BGP_ADDR + 2];
pub const SCREEN_PIXEL_WIDTH: usize = 160;
pub const SCREEN_PIXEL_HEIGHT: usize = 144;
pub const PALETTE_RGB: [u32; 4] = [0x00FFFFFF, 0x00C0C0C0, 0x00606060, 0];
pub const SCY_ADDR: u16 = 0xFF42;
pub const SCX_ADDR: u16 = 0xFF43;
pub const WY_ADDR: u16 = 0xFF4A;
pub const WX_ADDR: u16 = 0xFF4B;
pub const WX_OFFSET: usize = 7;
pub const LCDC_ADDR: u16 = 0xFF4;
pub const LCDC_WIN_MAP_MASK: u8 = 1 << 6;
pub const LCDC_WIN_ENABLE_MASK: u8 = 1 << 5;
pub const LCDC_BG_WIN_ADDR_MODE_MASK: u8 = 1 << 4;
pub const LCDC_BG_MAP_MASK: u8 = 1 << 3;
pub const LCDC_OBJ_SIZE_MASK: u8 = 1 << 2;
pub const LCDC_OBJ_ENABLE_MASK: u8 = 1 << 1;
pub const LCDC_BG_WIN_PRIORITY_MASK: u8 = 1;
pub const OAM_START_ADDR: u16 = 0xFE00;
pub const OAM_END_ADDR: u16 = 0xFE9F;
pub const OAM_ENTRY_SIZE: u8 = 4;

// For Tests
pub const RESULT_VRAM_START: usize = 0x9800;
pub const RESULT_VRAM_END: usize = 0x9900;
pub const MEM_DUMP_FILE: &str = "memdump";
