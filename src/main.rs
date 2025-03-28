#[derive(Debug, Default)]
struct Reg16([u8; 2]);
impl Reg16 {
    // // little endian to native u16
    fn get(&self) -> u16 {
        u16::from_le_bytes(self.0)
    }

    // native u16 to little endian
    fn set(&mut self, val: u16) {
        self.0[0] = (val & 0xff) as u8;
        self.0[1] = (val >> 8) as u8;
    }

    fn inner(&self) -> &[u8; 2] {
        &self.0
    }

    fn post_inc(&mut self) -> u16 {
        let retval = self.get();
        self.set(retval.wrapping_add(1));
        retval
    }
}

impl std::fmt::Display for Reg16 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "0x{:04x}", self.get())
    }
}

#[derive(Debug, Default)]
struct Reg8(u8);

impl Reg8 {
    // // little endian to native u16
    fn get(&self) -> u8 {
        self.0
    }

    // native u16 to little endian
    fn set(&mut self, val: u8) {
        self.0 = val;
    }
}

impl std::fmt::Display for Reg8 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "0x{:02x}", self.0)
    }
}

#[derive(Debug, Default)]
struct Cpu<'a> {
    halted: bool,
    mem: &'a mut [u8],

    a: Reg8,
    f: Reg8,
    b: Reg8,
    c: Reg8,
    d: Reg8,
    e: Reg8,
    h: Reg8,
    l: Reg8,
    sp: Reg16,
    pc: Reg16,
}

impl<'a> Cpu<'a> {
    fn halted(&self) -> bool {
        self.halted
    }

    fn reset(&mut self) {
        self.a.set(0);
        self.b.set(0);
        self.c.set(0);
        self.d.set(0);
        self.e.set(0);
        self.f.set(0);
        self.h.set(0);
        self.l.set(0);
        self.sp.set(0);
        self.pc.set(0);
        self.halted = false;
    }

    fn print_reg(&self) {
        let Cpu {
            a,
            b,
            c,
            d,
            e,
            f,
            h,
            l,
            sp,
            pc,
            ..
        } = self;
        println!(
            "a: {a}, f: {f}, b: {b}, c: {c}, d: {d}, e: {e}, h: {h}, l: {l}, sp: {sp}, pc: {pc}",
        );
    }

    fn new(mem: &'a mut [u8]) -> Self {
        let mut cpu = Cpu {
            mem,
            ..Default::default()
        };
        cpu.reset();
        cpu
    }

    fn step(&mut self) {
        let Cpu {
            pc,
            sp,
            mem,
            halted,
            ..
        } = self;

        // Fetch
        let byte = mem[pc.post_inc() as usize];

        // Decode
        let op = match Op::try_from(byte) {
            Some(o) => o,
            None => panic!("Invalid opcode 0x{:02x}", byte),
        };

        // Print opcode
        println!(
            "0x{:04x}: {} (0x{:02x})",
            pc.get().wrapping_sub(1),
            op,
            byte
        );

        // Execute
        match op {
            Op::Nop => {
                // TODO: TMP - halt CPU on nop
                *halted = true;
            }
            Op::Ld_Imm16_Sp => {
                // Read next 2 bytes
                let mut arr = [0u8; 2];
                arr[0] = mem[pc.post_inc() as usize];
                arr[1] = mem[pc.post_inc() as usize];

                // Use it as index to write val of sp
                let idx = u16::from_le_bytes(arr);
                mem[idx as usize] = sp.inner()[0];
                mem[idx as usize + 1] = sp.inner()[1];
            }
        }
    }
}

// _ is []
#[derive(Debug)]
enum Op {
    Nop,
    Ld_Imm16_Sp,
}

impl Op {
    fn try_from(b: u8) -> Option<Self> {
        match b {
            0x0 => Some(Self::Nop),
            0b00_00_1000 => Some(Self::Ld_Imm16_Sp),
            _ => None,
        }
    }

    fn opcode(&self) -> u8 {
        match self {
            Self::Nop => 0x0,
            Self::Ld_Imm16_Sp => 0b00_00_1000,
        }
    }
}

impl std::fmt::Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nop => write!(f, "Nop"),
            Self::Ld_Imm16_Sp => write!(f, "Ld Imm16 Sp"),
        }
    }
}

fn get_instrcs() -> Vec<u8> {
    let mut instrcs = vec![0u8; u16::MAX as usize];
    instrcs[0] = Op::Ld_Imm16_Sp.opcode();
    instrcs[1] = 0x1;
    instrcs
}

fn main() {
    let mut mem = get_instrcs();
    let mut cpu = Cpu::new(&mut mem);
    while !cpu.halted() {
        cpu.step();
    }

    // Print result
    cpu.print_reg();
    for b in &mem[0..10] {
        print!("0x{:02x} ", b);
    }
    println!("");
}
