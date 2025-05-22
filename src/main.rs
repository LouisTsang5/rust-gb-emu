use constants::{
    MEM_DUMP_FILE, RESULT_VRAM_END, RESULT_VRAM_START, SCREEN_PIXEL_HEIGHT, SCREEN_PIXEL_WIDTH,
    SCX_ADDR, SCY_ADDR, TILES_ARR_START_ADDR, TILE_MAP_START_ADDR, TILE_MAP_WIDTH, TILE_SIZE,
    VRAM_SIZE, VRAM_START_ADDR,
};

mod constants;
mod cpu;
mod mem;
mod ppu;
mod timer;
mod util;

fn read_rom(memory: &mem::MemoryHandle, file_name: &str) {
    let f = std::io::BufReader::new(std::fs::File::open(file_name).unwrap());
    for (i, byte) in std::io::Read::bytes(f).enumerate() {
        let byte = byte.unwrap();
        memory.write(i as u16, byte);
    }
}

fn make_test_vram(memory: &mem::MemoryHandle) {
    const TILE: [u8; TILE_SIZE as usize] = [
        0x3C, 0x7E, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x7E, 0x5E, 0x7E, 0x0A, 0x7C, 0x56, 0x38,
        0x7C,
    ];
    const TILE_ID: u8 = 1;

    // Write tile to first slot
    for (i, &b) in TILE.iter().enumerate() {
        memory.write(
            TILES_ARR_START_ADDR + (TILE_ID * TILE_SIZE) as u16 + i as u16,
            b,
        );
    }

    const TILE_MAP_MID_ADDR: u16 = (TILE_MAP_WIDTH * TILE_MAP_WIDTH) as u16 + TILE_MAP_START_ADDR;
    const TILE_MAP_END_ADDR: u16 = VRAM_START_ADDR + VRAM_SIZE;

    // Set BG
    for i in TILE_MAP_START_ADDR..TILE_MAP_MID_ADDR {
        memory.write(i, TILE_ID);
    }

    // Set Window
    for i in TILE_MAP_MID_ADDR..TILE_MAP_END_ADDR {
        let base = i - TILE_MAP_MID_ADDR;
        let x = base % TILE_MAP_WIDTH as u16;
        let y = base / TILE_MAP_WIDTH as u16;

        if x < 2 && y < 2 {
            memory.write(i, TILE_ID);
        }
    }
}

fn main() {
    // Make timer
    let timer = timer::make();

    // Make memory
    let memory = mem::make(timer.clone());
    timer.attach_mem(memory.clone());

    // Write ROM to mem
    {
        let file_name = std::env::args().nth(1).expect("Missing ROM File");
        read_rom(&memory, &file_name);
        make_test_vram(&memory);
    }

    // Make CPU
    let mut cpu = cpu::make(memory.clone());

    // Make PPU
    let mut window = minifb::Window::new(
        "GB EMU",
        SCREEN_PIXEL_WIDTH,
        SCREEN_PIXEL_HEIGHT,
        minifb::WindowOptions {
            resize: true,
            ..Default::default()
        },
    )
    .unwrap();
    window.set_target_fps(30);
    let mut ppu = ppu::make(memory.clone(), window);

    // Step
    let mut x_offset = 0;
    loop {
        // TODO: TEMP RENDER LOGIC
        time!(ppu.render());
        memory.write(SCX_ADDR, x_offset);
        memory.write(SCY_ADDR, x_offset);
        x_offset = x_offset.wrapping_add(1);
        std::thread::sleep(std::time::Duration::from_millis(300));

        // break;

        // let cycles_taken = cpu.step();
        // let cycles_taken = std::cmp::max(cycles_taken, 1); // Al least 1 cycle is taken (inc timer even in cpu halted mode)
        // for _ in 0..cycles_taken {
        //     timer.step();
        // }

        // Check if SIGINT
        if !ppu.is_window_open() {
            break;
        }
    }

    // Print result
    timer.print_timer();
    cpu.print_reg();

    // Dump Memory
    let mut dmem = [0; 0xFFFF + 1];
    for i in 0..dmem.len() {
        dmem[i] = memory.read(i as u16);
    }
    std::fs::write(MEM_DUMP_FILE, &dmem).unwrap();

    println!("VRAM ASCII:");
    for chunk in dmem[RESULT_VRAM_START..RESULT_VRAM_END].chunks(16) {
        let s: String = chunk
            .iter()
            .map(|&b| if b >= 32 && b <= 126 { b as char } else { '.' })
            .collect();
        println!("{}", s);
    }
}
