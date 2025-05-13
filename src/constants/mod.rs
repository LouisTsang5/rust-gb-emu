// Memory
pub const MEM_SIZE: usize = u16::MAX as usize + 1;

// Interrupt
pub const IE_ADDR: u16 = 0xFFFF;
pub const IF_ADDR: u16 = 0xFF0F;
pub const T_N_INTERRUPT: u16 = 5;
pub const INTERRUPT_HANDLER_BASE_ADDR: u16 = 0x40;

// For Tests
pub const RESULT_VRAM_START: usize = 0x9800;
pub const RESULT_VRAM_END: usize = 0x9900;
pub const MEM_DUMP_FILE: &str = "memdump";
