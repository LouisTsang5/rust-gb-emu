#[derive(Debug, Default)]
struct Reg16([u8; 2]);
impl Reg16 {
    // // little endian to native u16
    fn get(&self) -> u16 {
        u16::from_le_bytes(self.0)
    }

    fn get_hi(&self) -> u8 {
        self.0[0]
    }

    fn get_lo(&self) -> u8 {
        self.0[1]
    }

    // native u16 to little endian
    fn set(&mut self, val: u16) {
        self.0[0] = (val & 0xff) as u8;
        self.0[1] = (val >> 8) as u8;
    }

    fn set_hi(&mut self, val: u8) {
        self.0[0] = val;
    }

    fn set_lo(&mut self, val: u8) {
        self.0[1] = val;
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
struct Cpu<'a> {
    halted: bool,
    mem: &'a mut [u8],

    af: Reg16,
    bc: Reg16,
    de: Reg16,
    hl: Reg16,
    sp: Reg16,
    pc: Reg16,
}

impl<'a> Cpu<'a> {
    fn halted(&self) -> bool {
        self.halted
    }

    fn reset(&mut self) {
        self.af.set(0);
        self.bc.set(0);
        self.de.set(0);
        self.hl.set(0);
        self.sp.set(0);
        self.pc.set(0);
        self.halted = false;
    }

    fn print_reg(&self) {
        let Cpu {
            af,
            bc,
            de,
            hl,
            sp,
            pc,
            ..
        } = self;
        println!(
            "a: 0x{:02x}, f: 0x{:02x}, b: 0x{:02x}, c: 0x{:02x}, d: 0x{:02x}, e: 0x{:02x}, h: 0x{:02x}, l: 0x{:02x}, sp: {}, pc: {}",
            af.get_hi(),
            af.get_lo(),
            bc.get_hi(),
            bc.get_lo(),
            de.get_hi(),
            de.get_lo(),
            hl.get_hi(),
            hl.get_lo(),
            sp,
            pc,
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
            af,
            bc,
            de,
            hl,
            sp,
            pc,
            mem,
            halted,
            ..
        } = self;

        // Fetch
        let byte = mem[pc.post_inc() as usize];

        // Decode
        let op = match Op::try_from(byte) {
            Some(o) => o,
            None => panic!("Invalid opcode 0x{:02x} (0b{:08b})", byte, byte),
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
                let first = mem[pc.post_inc() as usize];
                let second = mem[pc.post_inc() as usize];

                // Write to register
                match param {
                    ParamR16::BC => bc,
                    ParamR16::DE => de,
                    ParamR16::HL => hl,
                    ParamR16::SP => sp,
                }
                .set(u16::from_le_bytes([first, second]));
            }
            Op::LdZR16MemZA(param) => {
                // Load index
                let idx = match param {
                    ParamR16Mem::BC => bc.get(),
                    ParamR16Mem::DE => de.get(),
                    ParamR16Mem::HLD | ParamR16Mem::HLI => hl.get(),
                };

                // Write a to addr
                mem[idx as usize] = af.get_hi();

                // Increment or decrement hl
                match param {
                    ParamR16Mem::HLI => hl.set(hl.get().wrapping_add(1)),
                    ParamR16Mem::HLD => hl.set(hl.get().wrapping_sub(1)),
                    _ => {}
                };
            }
            Op::LdAZR16MemZ(param) => {
                // Load index
                let idx = match param {
                    ParamR16Mem::BC => bc.get(),
                    ParamR16Mem::DE => de.get(),
                    ParamR16Mem::HLD | ParamR16Mem::HLI => hl.get(),
                };

                // Write byte pointed to a
                af.set_hi(mem[idx as usize]);

                // Increment or decrement hl
                match param {
                    ParamR16Mem::HLI => hl.set(hl.get().wrapping_add(1)),
                    ParamR16Mem::HLD => hl.set(hl.get().wrapping_sub(1)),
                    _ => {}
                };
            }
            Op::LdZImm16ZSp => {
                // Read next 2 bytes
                let first = mem[pc.post_inc() as usize];
                let second = mem[pc.post_inc() as usize];
                let idx = u16::from_le_bytes([first, second]);

                // Use it as index to write val of sp
                mem[idx as usize] = sp.get_hi();
                mem[idx as usize + 1] = sp.get_lo();
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
            _ => panic!("Invalid r16 param {:#x}", value),
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

#[derive(Debug, Clone, Copy)]
enum ParamR16Mem {
    BC = 0b00,
    DE = 0b01,
    HLI = 0b10, // Post increment HL
    HLD = 0b11, // Post decrement HL
}

impl From<u8> for ParamR16Mem {
    fn from(value: u8) -> Self {
        match value {
            0b00 => Self::BC,
            0b01 => Self::DE,
            0b10 => Self::HLI,
            0b11 => Self::HLD,
            _ => panic!("Invalid r16mem param {:#x}", value),
        }
    }
}

impl std::fmt::Display for ParamR16Mem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::BC => "bc",
                Self::DE => "de",
                Self::HLI => "hl+",
                Self::HLD => "hl-",
            }
        )
    }
}

// Z = []
#[derive(Debug)]
enum Op {
    Nop,                      // nop
    LdR16Imm16(ParamR16),     // ld r16, imm16
    LdZR16MemZA(ParamR16Mem), // ld [r16mem], a
    LdAZR16MemZ(ParamR16Mem), // ld a, [r16mem]
    LdZImm16ZSp,              // ld [imm16], sp
    Stop,                     // stop
}

impl Op {
    fn try_from(b: u8) -> Option<Self> {
        // match first top 2 bits to find instruction block
        match b >> 6 {
            // Block 0: match bottom 4 bits
            0b00 => match b & 0b00001111 {
                // Nop or stop
                0b0000 => match (b & 0b00110000) >> 4 {
                    0b00 => Some(Self::Nop),  // nop
                    0b01 => Some(Self::Stop), // stop
                    _ => None,
                },
                0b0001 => Some(Self::LdR16Imm16(ParamR16::from((b & 0b00110000) >> 4))), // ld r16, imm16
                0b0010 => Some(Self::LdZR16MemZA(ParamR16Mem::from((b & 0b00110000) >> 4))), // ld [r16mem], a
                0b1010 => Some(Self::LdAZR16MemZ(ParamR16Mem::from((b & 0b00110000) >> 4))), // ld a, [r16mem]
                0b1000 => Some(Self::LdZImm16ZSp), // ld [imm16], sp
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
            Self::LdZR16MemZA(param) => 0b0000_0010 | ((param as u8) << 4),
            Self::LdAZR16MemZ(param) => 0b0000_1010 | ((param as u8) << 4),
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
            Self::LdZR16MemZA(param) => write!(f, "ld [{}], a", param),
            Self::LdAZR16MemZ(param) => write!(f, "ld a, [{}]", param),
            Self::LdZImm16ZSp => write!(f, "ld [imm16] sp"),
            Self::Stop => write!(f, "stop"),
        }
    }
}

fn make_mem() -> Vec<u8> {
    const MEM_SIZE: usize = 16;
    const INSTC_END: usize = MEM_SIZE / 2; // Free memory after this point
    let mut mem = vec![0u8; MEM_SIZE];

    // ld hl INSTC_END
    mem[0] = Op::LdR16Imm16(ParamR16::HL).into();
    mem[1] = INSTC_END.to_le_bytes()[0];
    mem[2] = INSTC_END.to_le_bytes()[1];

    // ld a [hl+]
    mem[3] = Op::LdAZR16MemZ(ParamR16Mem::HLI).into();

    // ld [hl+] a
    mem[4] = Op::LdZR16MemZA(ParamR16Mem::HLI).into();

    // nop
    mem[5] = Op::Nop.into();

    // stop
    mem[6] = Op::Stop.into();

    // Set free mem values
    mem[INSTC_END] = 0xFF;
    mem[INSTC_END + 1] = 0xEE;

    // Return
    mem
}

fn main() {
    let mut mem = make_mem();
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
