use constants::{MEM_DUMP_FILE, RESULT_VRAM_END, RESULT_VRAM_START};

mod constants;
mod cpu;
mod mem;
mod timer;

fn main() {
    // Make timer
    let timer = timer::make();

    // Make memory
    let memory = mem::make_mem(timer.clone());
    timer.attach_mem(memory.clone());

    // Write ROM to mem
    let file_name = std::env::args().nth(1).expect("Missing ROM File");
    {
        let f = std::io::BufReader::new(std::fs::File::open(file_name).unwrap());
        for (i, byte) in std::io::Read::bytes(f).enumerate() {
            let byte = byte.unwrap();
            memory.write(i as u16, byte);
        }
    }

    // Listen Event
    let (tx, rx) = std::sync::mpsc::channel();
    ctrlc::set_handler(move || tx.send(()).expect("Channel Failed")).unwrap();

    // Make CPU
    let mut cpu = cpu::make(memory.clone());

    // Step
    loop {
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
