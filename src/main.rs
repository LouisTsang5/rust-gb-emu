#[derive(Debug, Default)]
struct Reg16([u8; 2]);
impl Reg16 {
    // // little endian to native u16
    fn get(&self) -> u16 {
        u16::from_be_bytes(self.0)
    }

    fn get_hi(&self) -> u8 {
        self.0[0]
    }

    fn get_lo(&self) -> u8 {
        self.0[1]
    }

    // native u16 to little endian
    fn set(&mut self, val: u16) {
        self.0[0] = (val >> 8) as u8;
        self.0[1] = (val & 0xff) as u8;
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
    // Memory
    mem: &'a mut [u8],

    // Registers
    af: Reg16,
    bc: Reg16,
    de: Reg16,
    hl: Reg16,
    sp: Reg16,
    pc: Reg16,

    // Halted
    halted: bool,
}

// Generate flag functions
macro_rules! flag_fns {
    ($($get_fn:ident => $set_fn:ident => $offset:expr),+ $(,)*) => {
        $(
            fn $get_fn(&self) -> bool {
                ((self.af.get_lo() & (0b1 << $offset)) >> $offset) > 0
            }

            fn $set_fn(&mut self, val: bool) {
                let f = self.af.get_lo();
                let mask = 0b1 << $offset;
                self.af.set_lo(match val {
                    true => f | mask,
                    false => f & (0xff ^ mask),
                });
            }
        )*
    };
}

impl<'a> Cpu<'a> {
    flag_fns! {
        get_zf => set_zf => 7, // Zero Flag 0b1000_0000
        get_nf => set_nf => 6, // Subtraction Flag 0b0100_0000
        get_hf => set_hf => 5, // Half Carry Flag 0b0010_0000
        get_cf => set_cf => 4, // Carry Flag 0b0001_0000
    }

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
            "a: 0x{0:02X} (0b{0:08b}), f: 0x{1:002X} (0b{1:08b}), b: 0x{2:002X}, c: 0x{3:002X}, d: 0x{4:002X}, e: 0x{5:002X}, h: 0x{6:002X}, l: 0x{7:002X}, sp: {8}, pc: {9}",
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
        // Fetch
        let byte = self.mem[self.pc.post_inc() as usize];

        // Decode
        let op = match Op::try_from(byte) {
            Some(o) => o,
            None => panic!("Invalid opcode 0x{:02X} (0b{:08b})", byte, byte),
        };

        // Print opcode
        println!(
            "0x{:04x}: {} (0b{:08b})",
            self.pc.get().wrapping_sub(1),
            op,
            byte
        );

        // Execute
        match op {
            Op::Nop => {}
            Op::LdR16Imm16(param) => {
                // Read 2 bytes
                let first = self.mem[self.pc.post_inc() as usize];
                let second = self.mem[self.pc.post_inc() as usize];

                // Write to register
                match param {
                    ParamR16::BC => &mut self.bc,
                    ParamR16::DE => &mut self.de,
                    ParamR16::HL => &mut self.hl,
                    ParamR16::SP => &mut self.sp,
                }
                .set(u16::from_le_bytes([first, second]));
            }
            Op::LdZR16MemZA(param) => {
                // Load index
                let idx = match param {
                    ParamR16Mem::BC => self.bc.get(),
                    ParamR16Mem::DE => self.de.get(),
                    ParamR16Mem::HLD | ParamR16Mem::HLI => self.hl.get(),
                };

                // Write a to addr
                self.mem[idx as usize] = self.af.get_hi();

                // Increment or decrement hl
                match param {
                    ParamR16Mem::HLI => self.hl.set(self.hl.get().wrapping_add(1)),
                    ParamR16Mem::HLD => self.hl.set(self.hl.get().wrapping_sub(1)),
                    _ => {}
                };
            }
            Op::LdAZR16MemZ(param) => {
                // Load index
                let idx = match param {
                    ParamR16Mem::BC => self.bc.get(),
                    ParamR16Mem::DE => self.de.get(),
                    ParamR16Mem::HLD | ParamR16Mem::HLI => self.hl.get(),
                };

                // Write byte pointed to a
                self.af.set_hi(self.mem[idx as usize]);

                // Increment or decrement hl
                match param {
                    ParamR16Mem::HLI => self.hl.set(self.hl.get().wrapping_add(1)),
                    ParamR16Mem::HLD => self.hl.set(self.hl.get().wrapping_sub(1)),
                    _ => {}
                };
            }
            Op::LdZImm16ZSp => {
                // Read next 2 bytes
                let first = self.mem[self.pc.post_inc() as usize];
                let second = self.mem[self.pc.post_inc() as usize];
                let idx = u16::from_le_bytes([first, second]);

                // Use it as index to write val of sp
                self.mem[idx as usize] = self.sp.get_lo();
                self.mem[idx as usize + 1] = self.sp.get_hi();
            }
            Op::IncR16(param) => {
                let r = match param {
                    ParamR16::BC => &mut self.bc,
                    ParamR16::DE => &mut self.de,
                    ParamR16::HL => &mut self.hl,
                    ParamR16::SP => &mut self.sp,
                };
                r.set(r.get().wrapping_add(1));
            }
            Op::DecR16(param) => {
                let r = match param {
                    ParamR16::BC => &mut self.bc,
                    ParamR16::DE => &mut self.de,
                    ParamR16::HL => &mut self.hl,
                    ParamR16::SP => &mut self.sp,
                };
                r.set(r.get().wrapping_sub(1));
            }
            Op::AddHlR16(param) => {
                let hl_val = self.hl.get();
                let add_num: u16 = match param {
                    ParamR16::BC => self.bc.get(),
                    ParamR16::DE => self.de.get(),
                    ParamR16::HL => self.hl.get(),
                    ParamR16::SP => self.sp.get(),
                };

                // Addition
                self.hl.set(hl_val.wrapping_add(add_num));

                // Set flags
                self.set_nf(false); // sub flag
                self.set_hf(((hl_val & 0x0FFF) + (add_num & 0x0FFF)) > 0x0FFF); // 12th bit overflow
                self.set_cf((0xFFFFu16 - add_num) < hl_val); // 16th bit overflow
            }
            Op::IncR8(param) => {
                // Read the byte
                let old_val = match param {
                    ParamR8::A => self.af.get_hi(),
                    ParamR8::B => self.bc.get_hi(),
                    ParamR8::C => self.bc.get_lo(),
                    ParamR8::D => self.de.get_hi(),
                    ParamR8::E => self.de.get_lo(),
                    ParamR8::H => self.hl.get_hi(),
                    ParamR8::L => self.hl.get_lo(),
                    ParamR8::ZHLZ => self.mem[self.hl.get() as usize],
                };

                // Write the byte
                let new_val = old_val.wrapping_add(1);
                match param {
                    ParamR8::A => self.af.set_hi(new_val),
                    ParamR8::B => self.bc.set_hi(new_val),
                    ParamR8::C => self.bc.set_lo(new_val),
                    ParamR8::D => self.de.set_hi(new_val),
                    ParamR8::E => self.de.set_lo(new_val),
                    ParamR8::H => self.hl.set_hi(new_val),
                    ParamR8::L => self.hl.set_lo(new_val),
                    ParamR8::ZHLZ => self.mem[self.hl.get() as usize] = new_val,
                };

                // Set Flags
                self.set_zf(new_val == 0);
                self.set_nf(false);
                self.set_hf((old_val & 0x0F) == 0x0F); // 4th bit overflow
            }
            Op::DecR8(param) => {
                // Read the byte
                let old_val = match param {
                    ParamR8::A => self.af.get_hi(),
                    ParamR8::B => self.bc.get_hi(),
                    ParamR8::C => self.bc.get_lo(),
                    ParamR8::D => self.de.get_hi(),
                    ParamR8::E => self.de.get_lo(),
                    ParamR8::H => self.hl.get_hi(),
                    ParamR8::L => self.hl.get_lo(),
                    ParamR8::ZHLZ => self.mem[self.hl.get() as usize],
                };

                // Write the byte
                let new_val = old_val.wrapping_sub(1);
                match param {
                    ParamR8::A => self.af.set_hi(new_val),
                    ParamR8::B => self.bc.set_hi(new_val),
                    ParamR8::C => self.bc.set_lo(new_val),
                    ParamR8::D => self.de.set_hi(new_val),
                    ParamR8::E => self.de.set_lo(new_val),
                    ParamR8::H => self.hl.set_hi(new_val),
                    ParamR8::L => self.hl.set_lo(new_val),
                    ParamR8::ZHLZ => self.mem[self.hl.get() as usize] = new_val,
                };

                // Set Flags
                self.set_zf(new_val == 0);
                self.set_nf(true);
                self.set_hf((old_val & 0x0F) == 0x00); // 5th bit borrow
            }
            Op::LdR8Imm8(param) => {
                let val = self.mem[self.pc.post_inc() as usize];
                match param {
                    ParamR8::A => self.af.set_hi(val),
                    ParamR8::B => self.bc.set_hi(val),
                    ParamR8::C => self.bc.set_lo(val),
                    ParamR8::D => self.de.set_hi(val),
                    ParamR8::E => self.de.set_lo(val),
                    ParamR8::H => self.hl.set_hi(val),
                    ParamR8::L => self.hl.set_lo(val),
                    ParamR8::ZHLZ => self.mem[self.hl.get() as usize] = val,
                };
            }
            Op::Rlca => {
                // Rotate A
                let a_val = self.af.get_hi();
                let top_bit = (a_val & 0b1000_0000) != 0;
                self.af.set_hi(
                    (a_val << 1)
                        + match top_bit {
                            true => 0b0000_0001,
                            false => 0,
                        },
                );

                // Set flags
                self.set_zf(false);
                self.set_nf(false);
                self.set_hf(false);
                self.set_cf(top_bit);
            }
            Op::Rrca => {
                // Rotate A
                let a_val = self.af.get_hi();
                let bottom_bit = (a_val & 0b0000_0001) != 0;
                self.af.set_hi(
                    (a_val >> 1)
                        + match bottom_bit {
                            true => 0b1000_0000,
                            false => 0,
                        },
                );

                // Set flags
                self.set_zf(false);
                self.set_nf(false);
                self.set_hf(false);
                self.set_cf(bottom_bit);
            }
            Op::Rla => {
                // Rotate A
                let a_val = self.af.get_hi();
                let add_val = match self.get_cf() {
                    true => 0b0000_0001,
                    false => 0,
                };
                self.af.set_hi((a_val << 1) + add_val);

                // Set flags
                self.set_zf(false);
                self.set_nf(false);
                self.set_hf(false);
                self.set_cf((a_val & 0b1000_0000) != 0);
            }
            Op::Rra => {
                // Rotate A
                let a_val = self.af.get_hi();
                let add_val = match self.get_cf() {
                    true => 0b1000_0000,
                    false => 0,
                };
                self.af.set_hi((a_val >> 1) + add_val);

                // Set flags
                self.set_zf(false);
                self.set_nf(false);
                self.set_hf(false);
                self.set_cf((a_val & 0b0000_0001) != 0);
            }
            Op::Daa => {
                let nf = self.get_nf();
                let a = self.af.get_hi();
                let mut adjust = 0u8;

                // check h flag
                if self.get_hf() || (!nf && (a & 0xF) > 0x9) {
                    adjust += 0x6;
                }

                // check c flag
                if self.get_cf() || (!nf && a > 0x99) {
                    adjust += 0x60;
                }

                // adjust a
                let a = match nf {
                    true => a.wrapping_sub(adjust),
                    false => a.wrapping_add(adjust),
                };
                self.af.set_hi(a);

                // set flags
                self.set_zf(a == 0);
                self.set_hf(false);
                if !nf && adjust >= 0x60 {
                    self.set_cf(true);
                }
            }
            Op::Cpl => {
                self.af.set_hi(!self.af.get_hi());
                self.set_nf(true);
                self.set_hf(true);
            }
            Op::Scf => {
                self.set_nf(false);
                self.set_hf(false);
                self.set_cf(true);
            }
            Op::Ccf => {
                self.set_nf(false);
                self.set_hf(false);
                self.set_cf(!self.get_cf());
            }
            Op::JrImm8 => {
                // Read next byte as 2's complement relative position
                let n = self.mem[self.pc.post_inc() as usize];
                let rlt = unsafe { *(&n as *const u8 as *const i8) };

                // Update pc
                let pc = self.pc.get() as i32 + rlt as i32;
                self.pc.set(pc as u16);
            }
            Op::Stop => {
                self.halted = true;
            }
            Op::AddAImm8 => {
                let lhs = self.af.get_hi();
                let rhs = self.mem[self.pc.post_inc() as usize];
                let a = lhs.wrapping_add(rhs);
                self.af.set_hi(a);

                self.set_zf(a == 0);
                self.set_nf(false);
                self.set_hf(((lhs & 0xF) + (rhs & 0xF)) > 0xF); // bit 3 overflow
                self.set_cf((0xFF - lhs) < rhs); // bit 7 overflow
            }
            Op::SubAImm8 => {
                let lhs = self.af.get_hi();
                let rhs = self.mem[self.pc.post_inc() as usize];
                let a = lhs.wrapping_sub(rhs);
                self.af.set_hi(a);

                self.set_zf(a == 0);
                self.set_nf(true);
                self.set_hf((lhs & 0xF) < (rhs & 0xF)); // bit 4 borrow
                self.set_cf(lhs < rhs); // results in negative
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum ParamR8 {
    B = 0b000,
    C = 0b001,
    D = 0b010,
    E = 0b011,
    H = 0b100,
    L = 0b101,
    ZHLZ = 0b110,
    A = 0b111,
}

impl From<u8> for ParamR8 {
    fn from(value: u8) -> Self {
        match value {
            0b000 => Self::B,
            0b001 => Self::C,
            0b010 => Self::D,
            0b011 => Self::E,
            0b100 => Self::H,
            0b101 => Self::L,
            0b110 => Self::ZHLZ,
            0b111 => Self::A,
            _ => panic!("Invalid r8 param {:#X}", value),
        }
    }
}

impl std::fmt::Display for ParamR8 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::B => "a",
                Self::C => "c",
                Self::D => "d",
                Self::E => "e",
                Self::H => "h",
                Self::L => "l",
                Self::ZHLZ => "[hl]",
                Self::A => "a",
            }
        )
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
            _ => panic!("Invalid r16 param {:#X}", value),
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
            _ => panic!("Invalid r16mem param {:#X}", value),
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
    IncR16(ParamR16),         // inc r16
    DecR16(ParamR16),         // dec r16
    AddHlR16(ParamR16),       // add hl, r16
    IncR8(ParamR8),           // inc r8
    DecR8(ParamR8),           // dec r8
    LdR8Imm8(ParamR8),        // ld r8, imm8
    Rlca,                     // rlca
    Rrca,                     // rrca
    Rla,                      // rla
    Rra,                      // rra
    Daa,                      // daa
    Cpl,                      // cpl
    Scf,                      // scf
    Ccf,                      // ccf
    JrImm8,                   // jr imm8
    AddAImm8,                 // add a, imm8
    SubAImm8,                 // sub a, imm8
    Stop,                     // stop
}

impl Op {
    fn try_from(b: u8) -> Option<Self> {
        // match first top 2 bits to find instruction block
        match b >> 6 {
            // Block 0:
            // 1. First, match the bottom 3 bits for instructions
            // 2. Then, if there is no match in 1., match the bottom 4 bits for instructions
            0b00 => match b & 0b0000_0111 {
                0b100 => Some(Self::IncR8(ParamR8::from((b & 0b00111000) >> 3))), // inc r8
                0b101 => Some(Self::DecR8(ParamR8::from((b & 0b00111000) >> 3))), // dec r8
                0b110 => Some(Self::LdR8Imm8(ParamR8::from((b & 0b00111000) >> 3))), // dec r8
                0b111 => match b {
                    0b0000_0111 => Some(Self::Rlca), // rlca
                    0b0000_1111 => Some(Self::Rrca), // rrca
                    0b0001_0111 => Some(Self::Rla),  // rla
                    0b0001_1111 => Some(Self::Rra),  // rra
                    0b0010_0111 => Some(Self::Daa),  // daa
                    0b0010_1111 => Some(Self::Cpl),  // cpl
                    0b0011_0111 => Some(Self::Scf),  // scf
                    0b0011_1111 => Some(Self::Ccf),  // ccf
                    _ => None,
                },
                0b000 => match b {
                    0b0000_0000 => Some(Self::Nop),         // nop
                    0b0000_1000 => Some(Self::LdZImm16ZSp), // ld [imm16], sp
                    0b0001_0000 => Some(Self::Stop),        // stop
                    0b0001_1000 => Some(Self::JrImm8),      // jr imm8,
                    _ => None,
                },
                _ => match b & 0b0000_1111 {
                    0b0001 => Some(Self::LdR16Imm16(ParamR16::from((b & 0b00110000) >> 4))), // ld r16, imm16
                    0b0010 => Some(Self::LdZR16MemZA(ParamR16Mem::from((b & 0b00110000) >> 4))), // ld [r16mem], a
                    0b1010 => Some(Self::LdAZR16MemZ(ParamR16Mem::from((b & 0b00110000) >> 4))), // ld a, [r16mem]
                    0b0011 => Some(Self::IncR16(ParamR16::from((b & 0b00110000) >> 4))), // inc r16
                    0b1011 => Some(Self::DecR16(ParamR16::from((b & 0b00110000) >> 4))), // dec r16
                    0b1001 => Some(Self::AddHlR16(ParamR16::from((b & 0b00110000) >> 4))), // add hl, r16
                    _ => None,
                },
            },
            // Block 3:
            0b11 => match b {
                0b1100_0110 => Some(Self::AddAImm8),
                0b1101_0110 => Some(Self::SubAImm8),
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
            Self::IncR16(param) => 0b0000_0011 | ((param as u8) << 4),
            Self::DecR16(param) => 0b0000_1011 | ((param as u8) << 4),
            Self::AddHlR16(param) => 0b0000_1001 | ((param as u8) << 4),
            Self::IncR8(param) => 0b0000_0100 | ((param as u8) << 3),
            Self::DecR8(param) => 0b0000_0101 | ((param as u8) << 3),
            Self::LdR8Imm8(param) => 0b0000_0110 | ((param as u8) << 3),
            Self::Rlca => 0b0000_0111,
            Self::Rrca => 0b0000_1111,
            Self::Rla => 0b0001_0111,
            Self::Rra => 0b0001_1111,
            Self::Daa => 0b0010_0111,
            Self::Cpl => 0b0010_1111,
            Self::Scf => 0b0011_0111,
            Self::Ccf => 0b0011_1111,
            Self::JrImm8 => 0b0001_1000,
            Self::Stop => 0b0001_0000,
            Self::AddAImm8 => 0b1100_0110,
            Self::SubAImm8 => 0b1101_0110,
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
            Self::IncR16(param) => write!(f, "inc {}", param),
            Self::DecR16(param) => write!(f, "dec {}", param),
            Self::AddHlR16(param) => write!(f, "add hl, {}", param),
            Self::IncR8(param) => write!(f, "inc {}", param),
            Self::DecR8(param) => write!(f, "dec {}", param),
            Self::LdR8Imm8(param) => write!(f, "ld {}, imm8", param),
            Self::Rlca => write!(f, "rlca"),
            Self::Rrca => write!(f, "rrca"),
            Self::Rla => write!(f, "rla"),
            Self::Rra => write!(f, "rra"),
            Self::Daa => write!(f, "daa"),
            Self::Cpl => write!(f, "cpl"),
            Self::Scf => write!(f, "scf"),
            Self::Ccf => write!(f, "ccf"),
            Self::JrImm8 => write!(f, "jr imm8"),
            Self::Stop => write!(f, "stop"),
            Self::AddAImm8 => write!(f, "add a, imm8"),
            Self::SubAImm8 => write!(f, "sub a, imm8"),
        }
    }
}

fn make_mem() -> Vec<u8> {
    const MEM_SIZE: usize = 64;
    const INSTC_END: usize = MEM_SIZE - 8; // Free memory after this point
    let mut mem = vec![0u8; MEM_SIZE];
    let mut i_instr = 0;
    let mut i_data = INSTC_END;

    macro_rules! add_instrc {
        ($x: expr) => {
            mem[i_instr] = $x.into();
            i_instr += 1;
        };
    }

    macro_rules! add_data {
        ($x: expr) => {
            mem[i_data] = $x;
            i_data += 1;
        };
    }

    // ld hl INSTC_END
    add_instrc!(Op::LdR16Imm16(ParamR16::HL));
    add_instrc!(INSTC_END.to_le_bytes()[0]);
    add_instrc!(INSTC_END.to_le_bytes()[1]);

    // inc hl
    add_instrc!(Op::IncR16(ParamR16::HL));
    add_instrc!(Op::IncR16(ParamR16::HL));

    // ld a [hl+]
    add_instrc!(Op::LdAZR16MemZ(ParamR16Mem::HLI));

    // ld [hl+] a
    add_instrc!(Op::LdZR16MemZA(ParamR16Mem::HLI));

    // inc hl
    add_instrc!(Op::IncR16(ParamR16::HL));
    add_instrc!(Op::IncR16(ParamR16::HL));

    // dec bc
    add_instrc!(Op::DecR16(ParamR16::BC));
    add_instrc!(Op::DecR16(ParamR16::BC));

    // add hl, bc
    add_instrc!(Op::AddHlR16(ParamR16::BC));

    // ld e, $0xEE
    add_instrc!(Op::LdR8Imm8(ParamR8::E));
    add_instrc!(0xEE);

    // inc e
    add_instrc!(Op::IncR8(ParamR8::E));
    add_instrc!(Op::IncR8(ParamR8::E));

    // dec e
    add_instrc!(Op::DecR8(ParamR8::E));

    // rlca
    add_instrc!(Op::Rlca);
    add_instrc!(Op::Rlca);

    // rrca
    add_instrc!(Op::Rrca);
    add_instrc!(Op::Rrca);

    // rla
    add_instrc!(Op::Rla);

    // rra
    add_instrc!(Op::Rra);

    // add a, $0x23
    add_instrc!(Op::AddAImm8);
    add_instrc!(0x23);

    // daa
    add_instrc!(Op::Daa);

    // sub a, $0x09
    add_instrc!(Op::SubAImm8);
    add_instrc!(0x09);

    // daa
    add_instrc!(Op::Daa);

    // sub a, $0x09
    add_instrc!(Op::SubAImm8);
    add_instrc!(0x09);

    // daa
    add_instrc!(Op::Daa);

    // cpl
    add_instrc!(Op::Cpl);

    // scf
    add_instrc!(Op::Scf);

    // ccf
    add_instrc!(Op::Ccf);

    // jr $0x02
    add_instrc!(Op::JrImm8);
    add_instrc!(0x2);

    // nop
    add_instrc!(Op::Nop);
    add_instrc!(Op::Nop);
    add_instrc!(Op::Nop);

    // stop
    add_instrc!(Op::Stop);

    // Set free mem values
    add_data!(0xFF);
    add_data!(0xEE);
    add_data!(0x12);

    dbg!(i_instr);
    dbg!(i_data);

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
        print!("0x{:02X} ", b);
    }
    println!("");
}
