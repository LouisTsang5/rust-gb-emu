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

// For Tests
pub const RESULT_VRAM_START: usize = 0x9800;
pub const RESULT_VRAM_END: usize = 0x9900;
pub const MEM_DUMP_FILE: &str = "memdump";
