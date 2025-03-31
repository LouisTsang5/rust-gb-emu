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

    fn inner_mut(&mut self) -> &mut [u8; 2] {
        &mut self.0
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
            a,
            b,
            c,
            d,
            e,
            f,
            h,
            l,
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
            "0x{:04x}: {} (0b{:08b})",
            pc.get().wrapping_sub(1),
            op,
            byte
        );

        // Execute
        match op {
            Op::Nop => {}
            Op::LdR16Imm16(param) => {
                // Read 2 bytes
                let lo = mem[pc.post_inc() as usize];
                let hi = mem[pc.post_inc() as usize];

                // Write to register
                match param {
                    ParamR16::BC => {
                        b.set(lo);
                        c.set(hi);
                    }
                    ParamR16::DE => {
                        d.set(lo);
                        e.set(hi);
                    }
                    ParamR16::HL => {
                        h.set(lo);
                        l.set(hi);
                    }
                    ParamR16::SP => {
                        sp.inner_mut()[0] = lo;
                        sp.inner_mut()[1] = hi;
                    }
                }
            }
            Op::LdZImm16ZSp => {
                // Read next 2 bytes
                let mut arr = [0u8; 2];
                arr[0] = mem[pc.post_inc() as usize];
                arr[1] = mem[pc.post_inc() as usize];

                // Use it as index to write val of sp
                let idx = u16::from_le_bytes(arr);
                mem[idx as usize] = sp.inner()[0];
                mem[idx as usize + 1] = sp.inner()[1];
            }
            Op::Stop => {
                *halted = true;
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum ParamR16 {
    BC = 0b00,
    DE = 0b01,
    HL = 0b10,
    SP = 0b11,
}

impl From<u8> for ParamR16 {
    fn from(value: u8) -> Self {
        match value {
            0b00 => Self::BC,
            0b01 => Self::DE,
            0b10 => Self::HL,
            0b11 => Self::SP,
            _ => panic!("Invalid ParamR16 {:#x}", value),
        }
    }
}

impl std::fmt::Display for ParamR16 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::BC => "bc",
                Self::DE => "de",
                Self::HL => "hl",
                Self::SP => "sp",
            }
        )
    }
}

// Z = []
#[derive(Debug)]
enum Op {
    Nop,                  // nop
    LdR16Imm16(ParamR16), // ld r16, imm16
    LdZImm16ZSp,          // ld [imm16], sp
    Stop,                 // stop
}

impl Op {
    fn try_from(b: u8) -> Option<Self> {
        // match first top 2 bits to find instruction block
        match b >> 6 {
            // Block 0: match bottom 4 bits
            0b00 => match b & 0b00001111 {
                // Nop or stop
                0b0000 => match (b & 0b00110000) >> 4 {
                    0b00 => Some(Self::Nop),
                    0b01 => Some(Self::Stop),
                    _ => None,
                },
                0b0001 => Some(Self::LdR16Imm16(ParamR16::from((b & 0b00110000) >> 4))),
                0b1000 => Some(Self::LdZImm16ZSp),
                _ => None,
            },
            _ => None,
        }
    }
}

impl Into<u8> for Op {
    fn into(self) -> u8 {
        match self {
            Self::Nop => 0x0,
            Self::LdR16Imm16(param) => 0b0000_0001 | ((param as u8) << 4),
            Self::LdZImm16ZSp => 0b0000_1000,
            Self::Stop => 0b0001_0000,
        }
    }
}

impl std::fmt::Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nop => write!(f, "nop"),
            Self::LdR16Imm16(param) => write!(f, "ld {}, imm16", param),
            Self::LdZImm16ZSp => write!(f, "ld [imm16] sp"),
            Self::Stop => write!(f, "stop"),
        }
    }
}

fn get_instrcs() -> Vec<u8> {
    let i: u8 = Op::LdR16Imm16(ParamR16::BC).into();
    dbg!(i);
    vec![
        Op::Nop.into(),
        Op::LdR16Imm16(ParamR16::BC).into(),
        0x11,
        0x22,
        Op::Stop.into(),
    ]
}

fn main() {
    let mut mem = get_instrcs();
    let mut cpu = Cpu::new(&mut mem);
    while !cpu.halted() {
        cpu.step();
    }

    // Print result
    cpu.print_reg();
    for b in &mem {
        print!("0x{:02x} ", b);
    }
    println!("");
}
