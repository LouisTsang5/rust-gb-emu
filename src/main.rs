use std::{io::Read, time::Duration};

use constants::{
    LCDC_ADDR, LCDC_BG_WIN_ADDR_MODE_MASK, LCDC_BG_WIN_PRIORITY_MASK, LCDC_OBJ_ENABLE_MASK,
    LCDC_OBJ_SIZE_MASK, LCDC_WIN_MAP_MASK, MEM_DUMP_FILE, RESULT_VRAM_END, RESULT_VRAM_START,
    SCREEN_PIXEL_HEIGHT, SCREEN_PIXEL_WIDTH, SCX_ADDR, SCY_ADDR, TARGET_FPS,
};

mod constants;
mod cpu;
mod mem;
mod ppu;
mod timer;
mod util;

fn read_rom(memory: &mem::MemoryHandle, file_name: &str) {
    let f = std::io::BufReader::new(std::fs::File::open(file_name).unwrap());
    for (i, byte) in f.bytes().enumerate() {
        let byte = byte.unwrap();
        memory.write(i as u16, byte);
    }
}

fn main() {
    // Listen Event
    let (tx, rx) = std::sync::mpsc::channel();
    ctrlc::set_handler(move || tx.send(()).expect("Channel Failed")).unwrap();

    // Make timer
    let timer = timer::make();

    // Make memory
    let memory = mem::make(timer.clone());
    timer.attach_mem(memory.clone());

    // Write ROM to mem
    {
        let file_name = std::env::args().nth(1).expect("Missing ROM File");
        read_rom(&memory, &file_name);
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
            scale: minifb::Scale::X4,
            ..Default::default()
        },
    )
    .unwrap();
    window.set_target_fps(TARGET_FPS as usize);
    let mut ppu = ppu::make(memory.clone(), window);

    // Initialize PPU display
    memory.write(SCX_ADDR, 0);
    memory.write(SCY_ADDR, 0);
    memory.write(
        LCDC_ADDR,
        0x0000
            // | LCDC_WIN_ENABLE_MASK
            | LCDC_WIN_MAP_MASK
            | LCDC_BG_WIN_ADDR_MODE_MASK
            | LCDC_BG_WIN_PRIORITY_MASK
            | LCDC_OBJ_SIZE_MASK
            | LCDC_OBJ_ENABLE_MASK,
    );

    // Main loop
    let mut last_render = std::time::Instant::now();
    loop {
        // Render
        let should_render = std::time::Instant::now().duration_since(last_render)
            >= Duration::from_millis((1000.0 / TARGET_FPS as f64) as u64);
        if should_render {
            ppu.render();
            last_render = std::time::Instant::now();
        }

        // Step the CPU
        let (cycles_taken, op_info, prefix_op_info) = cpu.step();

        // Increment timer
        let cycles_taken = std::cmp::max(cycles_taken, 1); // Al least 1 cycle is taken (inc timer even in cpu halted mode)
        for _ in 0..cycles_taken {
            timer.step();
        }

        // Print info
        if let Some(info) = op_info {
            println!(
                "0x{0:04x}: {1} (0b{2:08b}) (0x{2:02x})",
                info.mem_addr,
                info.op,
                u8::from(info.op)
            );
        }
        if let Some(info) = prefix_op_info {
            println!(
                "0x{0:04x}: {1} (0b{2:08b}) (0x{2:02x})",
                info.mem_addr,
                info.op,
                u8::from(info.op)
            );
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
