use constants::{
    MEM_DUMP_FILE, RESULT_VRAM_END, RESULT_VRAM_START, TILES_ARR_START_ADDR, TILE_MAP_START_ADDR,
    TILE_SIZE, VRAM_SIZE, VRAM_START_ADDR,
};

mod constants;
mod cpu;
mod mem;
mod ppu;
mod timer;

fn read_rom(memory: &mem::MemoryHandle, file_name: &str) {
    let f = std::io::BufReader::new(std::fs::File::open(file_name).unwrap());
    for (i, byte) in std::io::Read::bytes(f).enumerate() {
        let byte = byte.unwrap();
        memory.write(i as u16, byte);
    }
}

fn make_test_vram(memory: &mem::MemoryHandle) {
    const TILE: [u8; TILE_SIZE as usize] = [
        0xAA, 0xAA, 0x55, 0x55, 0xAA, 0xAA, 0x55, 0x55, 0xAA, 0xAA, 0x55, 0x55, 0xAA, 0xAA, 0x55,
        0x55,
    ];
    const TILE_ID: u8 = 1;

    // Write tile to first slot
    for (i, &b) in TILE.iter().enumerate() {
        memory.write(
            TILES_ARR_START_ADDR + (TILE_ID * TILE_SIZE) as u16 + i as u16,
            b,
        );
    }

    // Point to TILE_ID for all tile map elements
    for i in TILE_MAP_START_ADDR..VRAM_START_ADDR + VRAM_SIZE {
        memory.write(i, TILE_ID);
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

    // Listen Event
    let (tx, rx) = std::sync::mpsc::channel();
    ctrlc::set_handler(move || tx.send(()).expect("Channel Failed")).unwrap();

    // Make CPU
    let mut cpu = cpu::make(memory.clone());

    // Make PPU
    let mut ppu = ppu::make(memory.clone());

    // Step
    loop {
        // TODO: TEMP RENDER LOGIC
        ppu.render();
        break;

        let cycles_taken = cpu.step();
        let cycles_taken = std::cmp::max(cycles_taken, 1); // Al least 1 cycle is taken (inc timer even in cpu halted mode)
        for _ in 0..cycles_taken {
            timer.step();
        }

        // Check if SIGINT
        if rx.try_recv().is_ok() {
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
