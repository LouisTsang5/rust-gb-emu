#[derive(Debug, Clone, Copy)]
pub enum ParamR8 {
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
                Self::B => "b",
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
pub struct RstAddr {
    idx: u8,
}

impl RstAddr {
    pub fn addr(&self) -> u16 {
        self.idx as u16 * 8
    }

    pub fn idx(&self) -> u8 {
        self.idx
    }

    pub fn from_idx(idx: u8) -> Self {
        if idx > 7 {
            panic!("Invalid rst index {:#X}", idx);
        }
        RstAddr { idx }
    }
}

impl std::fmt::Display for RstAddr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "0x{:02X}", self.addr())
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ParamR16 {
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
pub enum ParamR16Stk {
    BC = 0b00,
    DE = 0b01,
    HL = 0b10,
    AF = 0b11,
}

impl From<u8> for ParamR16Stk {
    fn from(value: u8) -> Self {
        match value {
            0b00 => Self::BC,
            0b01 => Self::DE,
            0b10 => Self::HL,
            0b11 => Self::AF,
            _ => panic!("Invalid r16stk param {:#X}", value),
        }
    }
}

impl std::fmt::Display for ParamR16Stk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::BC => "bc",
                Self::DE => "de",
                Self::HL => "hl",
                Self::AF => "af",
            }
        )
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ParamR16Mem {
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

#[derive(Debug, Clone, Copy)]
pub enum ParamCond {
    Nz = 0b00,
    Z = 0b01,
    Nc = 0b10,
    C = 0b11,
}

impl From<u8> for ParamCond {
    fn from(value: u8) -> Self {
        match value {
            0b00 => Self::Nz,
            0b01 => Self::Z,
            0b10 => Self::Nc,
            0b11 => Self::C,
            _ => panic!("Invalid cond param {:#X}", value),
        }
    }
}

impl std::fmt::Display for ParamCond {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Nz => "nz",
                Self::Z => "z",
                Self::Nc => "nc",
                Self::C => "c",
            }
        )
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ParamB3(u8);

impl ParamB3 {
    pub fn val(&self) -> u8 {
        self.0
    }
}

impl From<u8> for ParamB3 {
    fn from(value: u8) -> Self {
        assert!(value < 8);
        Self(value)
    }
}

impl From<ParamB3> for u8 {
    fn from(value: ParamB3) -> Self {
        value.0
    }
}

impl std::fmt::Display for ParamB3 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// 0xCB prefixed opertations
#[derive(Debug, Clone, Copy)]
pub enum CbPrefixOp {
    RlcR8(ParamR8),            // rlc r8
    RrcR8(ParamR8),            // rrc r8
    RlR8(ParamR8),             // rl r8
    RrR8(ParamR8),             // rr r8
    SlaR8(ParamR8),            // sla r8
    SraR8(ParamR8),            // sra r8
    SwapR8(ParamR8),           // swap r8
    SrlR8(ParamR8),            // srl r8
    BitB3R8(ParamB3, ParamR8), // bit b3, r8
    ResB3R8(ParamB3, ParamR8), // res b3, r8
    SetB3R8(ParamB3, ParamR8), // set b3, r8
}

impl TryFrom<u8> for CbPrefixOp {
    /// only one reason for error, invalid byte
    type Error = ();

    fn try_from(b: u8) -> Result<Self, Self::Error> {
        match (b & 0b1100_0000) >> 6 {
            0b00 => match (b & 0b0011_1000) >> 3 {
                0b000 => Ok(Self::RlcR8(ParamR8::from(b & 0b0000_0111))),
                0b001 => Ok(Self::RrcR8(ParamR8::from(b & 0b0000_0111))),
                0b010 => Ok(Self::RlR8(ParamR8::from(b & 0b0000_0111))),
                0b011 => Ok(Self::RrR8(ParamR8::from(b & 0b0000_0111))),
                0b100 => Ok(Self::SlaR8(ParamR8::from(b & 0b0000_0111))),
                0b101 => Ok(Self::SraR8(ParamR8::from(b & 0b0000_0111))),
                0b110 => Ok(Self::SwapR8(ParamR8::from(b & 0b0000_0111))),
                0b111 => Ok(Self::SrlR8(ParamR8::from(b & 0b0000_0111))),
                _ => unreachable!(),
            },
            0b01 => Ok(Self::BitB3R8(
                ParamB3::from((b & 0b0011_1000) >> 3),
                ParamR8::from(b & 0b0000_0111),
            )),
            0b10 => Ok(Self::ResB3R8(
                ParamB3::from((b & 0b0011_1000) >> 3),
                ParamR8::from(b & 0b0000_0111),
            )),
            0b11 => Ok(Self::SetB3R8(
                ParamB3::from((b & 0b0011_1000) >> 3),
                ParamR8::from(b & 0b0000_0111),
            )),
            _ => unreachable!(),
        }
    }
}

impl From<CbPrefixOp> for u8 {
    fn from(b: CbPrefixOp) -> Self {
        match b {
            CbPrefixOp::RlcR8(p) => 0b0000_0000 | (p as u8),
            CbPrefixOp::RrcR8(p) => 0b0000_1000 | (p as u8),
            CbPrefixOp::RlR8(p) => 0b0001_0000 | (p as u8),
            CbPrefixOp::RrR8(p) => 0b0001_1000 | (p as u8),
            CbPrefixOp::SlaR8(p) => 0b0010_0000 | (p as u8),
            CbPrefixOp::SraR8(p) => 0b0010_1000 | (p as u8),
            CbPrefixOp::SwapR8(p) => 0b0011_0000 | (p as u8),
            CbPrefixOp::SrlR8(p) => 0b0011_1000 | (p as u8),
            CbPrefixOp::BitB3R8(b, p) => 0b0100_0000 | (b.val() << 3) | (p as u8),
            CbPrefixOp::ResB3R8(b, p) => 0b1000_0000 | (b.val() << 3) | (p as u8),
            CbPrefixOp::SetB3R8(b, p) => 0b1100_0000 | (b.val() << 3) | (p as u8),
        }
    }
}

impl std::fmt::Display for CbPrefixOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::RlcR8(p) => write!(f, "rlc {}", p),
            Self::RrcR8(p) => write!(f, "rrc {}", p),
            Self::RlR8(p) => write!(f, "rl {}", p),
            Self::RrR8(p) => write!(f, "rr {}", p),
            Self::SlaR8(p) => write!(f, "sla {}", p),
            Self::SraR8(p) => write!(f, "sra {}", p),
            Self::SwapR8(p) => write!(f, "swap {}", p),
            Self::SrlR8(p) => write!(f, "srl {}", p),
            Self::BitB3R8(b, p) => write!(f, "bit {}, {}", b, p),
            Self::ResB3R8(b, p) => write!(f, "res {}, {}", b, p),
            Self::SetB3R8(b, p) => write!(f, "set {}, {}", b, p),
        }
    }
}

// Z = [], X = +
#[derive(Debug)]
pub enum Op {
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
    JrCcImm8(ParamCond),      // jr cc, imm8
    Stop,                     // stop
    LdR8R8(ParamR8, ParamR8), // ld r8, r8
    Halt,                     // halt
    AddAR8(ParamR8),          // add a, r8
    AdcAR8(ParamR8),          // adc a, r8
    SubAR8(ParamR8),          // sub a, r8
    SbcAR8(ParamR8),          // sbc a, r8
    AndAR8(ParamR8),          // and a, r8
    XorAR8(ParamR8),          // xor a, r8
    OrAR8(ParamR8),           // or a, r8
    CpAR8(ParamR8),           // cp a, r8
    AddAImm8,                 // add a, imm8
    AdcAImm8,                 // adc a, imm8
    SubAImm8,                 // sub a, imm8
    SbcAImm8,                 // sbc a, imm8
    AndAImm8,                 // and a, imm8
    XorAImm8,                 // xor a, imm8
    OrAImm8,                  // or a, imm8
    CpAImm8,                  // cp a, imm8
    RetCond(ParamCond),       // ret cond
    Ret,                      // ret
    Reti,                     // reti
    JpCondImm16(ParamCond),   // jp cond, imm16
    JpImm16,                  // jp imm16
    JpHl,                     // jp hl
    CallCondImm16(ParamCond), // call cond, imm16
    CallImm16,                // call imm16
    Rst(RstAddr),             // rst addrvec
    Pop(ParamR16Stk),         // pop r16stk
    Push(ParamR16Stk),        // push r16stk
    Prefix,                   // prefix
    LdhZCZA,                  // ldh [c], a
    LdhZImm8ZA,               // ldh [imm8], a
    LdZImm16ZA,               // ld [imm16], a
    LdhAZCZ,                  // ldh a, [c]
    LdhAZImm8Z,               // ldh a, [imm8]
    LdAZImm16Z,               // ld a, [imm16]
    AddSpImm8,                // add sp, imm8
    LdHlSpXImm8,              // ld hl, sp + imm8
    LdSpHl,                   // ld sp, hl
    Di,                       // di
    Ei,                       // ei
}

impl Op {
    pub fn try_from(b: u8) -> Option<Self> {
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
                    _ => match (b & 0b0010_0000) == 0b0010_0000 {
                        true => Some(Self::JrCcImm8(ParamCond::from((b & 0b0001_1000) >> 3))), // jr cc, imm8
                        false => None,
                    },
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
            // Block 1:
            0b01 => match b {
                0b0111_0110 => Some(Self::Halt),
                _ => Some(Self::LdR8R8(
                    ParamR8::from((b & 0b0011_1000) >> 3),
                    ParamR8::from(b & 0b0000_0111),
                )),
            },
            // Block 2:
            0b10 => match (b & 0b0011_1000) >> 3 {
                0b000 => Some(Self::AddAR8(ParamR8::from(b & 0b0000_0111))),
                0b001 => Some(Self::AdcAR8(ParamR8::from(b & 0b0000_0111))),
                0b010 => Some(Self::SubAR8(ParamR8::from(b & 0b0000_0111))),
                0b011 => Some(Self::SbcAR8(ParamR8::from(b & 0b0000_0111))),
                0b100 => Some(Self::AndAR8(ParamR8::from(b & 0b0000_0111))),
                0b101 => Some(Self::XorAR8(ParamR8::from(b & 0b0000_0111))),
                0b110 => Some(Self::OrAR8(ParamR8::from(b & 0b0000_0111))),
                0b111 => Some(Self::CpAR8(ParamR8::from(b & 0b0000_0111))),
                _ => None,
            },
            // Block 3:
            0b11 => match b {
                0b1100_0110 => Some(Self::AddAImm8),
                0b1100_1110 => Some(Self::AdcAImm8),
                0b1101_0110 => Some(Self::SubAImm8),
                0b1101_1110 => Some(Self::SbcAImm8),
                0b1110_0110 => Some(Self::AndAImm8),
                0b1110_1110 => Some(Self::XorAImm8),
                0b1111_0110 => Some(Self::OrAImm8),
                0b1111_1110 => Some(Self::CpAImm8),
                0b1100_1001 => Some(Self::Ret),
                0b1101_1001 => Some(Self::Reti),
                0b1100_0011 => Some(Self::JpImm16),
                0b1110_1001 => Some(Self::JpHl),
                0b1100_1101 => Some(Self::CallImm16),
                0b1100_1011 => Some(Self::Prefix),
                0b1110_0010 => Some(Self::LdhZCZA),
                0b1110_0000 => Some(Self::LdhZImm8ZA),
                0b1110_1010 => Some(Self::LdZImm16ZA),
                0b1111_0010 => Some(Self::LdhAZCZ),
                0b1111_0000 => Some(Self::LdhAZImm8Z),
                0b1111_1010 => Some(Self::LdAZImm16Z),
                0b1110_1000 => Some(Self::AddSpImm8),
                0b1111_1000 => Some(Self::LdHlSpXImm8),
                0b1111_1001 => Some(Self::LdSpHl),
                0b1111_0011 => Some(Self::Di),
                0b1111_1011 => Some(Self::Ei),
                _ => match b & 0b0000_1111 {
                    0b0001 => Some(Self::Pop(ParamR16Stk::from((b & 0b0011_0000) >> 4))),
                    0b0101 => Some(Self::Push(ParamR16Stk::from((b & 0b0011_0000) >> 4))),
                    _ => match b & 0b0000_0111 {
                        0b000 => Some(Self::RetCond(ParamCond::from((b & 0b0001_1000) >> 3))),
                        0b010 => Some(Self::JpCondImm16(ParamCond::from((b & 0b0001_1000) >> 3))),
                        0b100 => Some(Self::CallCondImm16(ParamCond::from((b & 0b0001_1000) >> 3))),
                        0b111 => Some(Self::Rst(RstAddr::from_idx((b & 0b0011_1000) >> 3))),
                        _ => None,
                    },
                },
            },
            _ => None,
        }
    }
}

impl From<Op> for u8 {
    fn from(val: Op) -> Self {
        match val {
            Op::Nop => 0x0,
            Op::LdR16Imm16(param) => 0b0000_0001 | ((param as u8) << 4),
            Op::LdZR16MemZA(param) => 0b0000_0010 | ((param as u8) << 4),
            Op::LdAZR16MemZ(param) => 0b0000_1010 | ((param as u8) << 4),
            Op::LdZImm16ZSp => 0b0000_1000,
            Op::IncR16(param) => 0b0000_0011 | ((param as u8) << 4),
            Op::DecR16(param) => 0b0000_1011 | ((param as u8) << 4),
            Op::AddHlR16(param) => 0b0000_1001 | ((param as u8) << 4),
            Op::IncR8(param) => 0b0000_0100 | ((param as u8) << 3),
            Op::DecR8(param) => 0b0000_0101 | ((param as u8) << 3),
            Op::LdR8Imm8(param) => 0b0000_0110 | ((param as u8) << 3),
            Op::Rlca => 0b0000_0111,
            Op::Rrca => 0b0000_1111,
            Op::Rla => 0b0001_0111,
            Op::Rra => 0b0001_1111,
            Op::Daa => 0b0010_0111,
            Op::Cpl => 0b0010_1111,
            Op::Scf => 0b0011_0111,
            Op::Ccf => 0b0011_1111,
            Op::JrImm8 => 0b0001_1000,
            Op::JrCcImm8(param) => 0b0010_0000 | ((param as u8) << 3),
            Op::Stop => 0b0001_0000,
            Op::LdR8R8(dest, src) => 0b0100_0000 | ((dest as u8) << 3) | src as u8,
            Op::Halt => 0b0111_0110,
            Op::AddAR8(param) => 0b1000_0000 | param as u8,
            Op::AdcAR8(param) => 0b1000_1000 | param as u8,
            Op::SubAR8(param) => 0b1001_0000 | param as u8,
            Op::SbcAR8(param) => 0b1001_1000 | param as u8,
            Op::AndAR8(param) => 0b1010_0000 | param as u8,
            Op::XorAR8(param) => 0b1010_1000 | param as u8,
            Op::OrAR8(param) => 0b1011_0000 | param as u8,
            Op::CpAR8(param) => 0b1011_1000 | param as u8,
            Op::AddAImm8 => 0b1100_0110,
            Op::AdcAImm8 => 0b1100_1110,
            Op::SubAImm8 => 0b1101_0110,
            Op::SbcAImm8 => 0b1101_1110,
            Op::AndAImm8 => 0b1110_0110,
            Op::XorAImm8 => 0b1110_1110,
            Op::OrAImm8 => 0b1111_0110,
            Op::CpAImm8 => 0b1111_1110,
            Op::RetCond(param) => 0b1100_0000 | ((param as u8) << 3),
            Op::Ret => 0b1100_1001,
            Op::Reti => 0b1101_1001,
            Op::JpCondImm16(param) => 0b1100_0010 | ((param as u8) << 3),
            Op::JpImm16 => 0b1100_0011,
            Op::JpHl => 0b1110_1001,
            Op::CallCondImm16(param) => 0b1100_0100 | ((param as u8) << 3),
            Op::CallImm16 => 0b1100_1101,
            Op::Rst(addr) => 0b1100_0111 | (addr.idx() << 3),
            Op::Pop(param) => 0b1100_0001 | ((param as u8) << 4),
            Op::Push(param) => 0b1100_0101 | ((param as u8) << 4),
            Op::Prefix => 0b1100_1011,
            Op::LdhZCZA => 0b1110_0010,
            Op::LdhZImm8ZA => 0b1110_0000,
            Op::LdZImm16ZA => 0b1110_1010,
            Op::LdhAZCZ => 0b1111_0010,
            Op::LdhAZImm8Z => 0b1111_0000,
            Op::LdAZImm16Z => 0b1111_1010,
            Op::AddSpImm8 => 0b1110_1000,
            Op::LdHlSpXImm8 => 0b1111_1000,
            Op::LdSpHl => 0b1111_1001,
            Op::Di => 0b1111_0011,
            Op::Ei => 0b1111_1011,
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
            Self::JrCcImm8(param) => write!(f, "jr {}, imm8", param),
            Self::Stop => write!(f, "stop"),
            Self::LdR8R8(dest, src) => write!(f, "ld {}, {}", dest, src),
            Self::Halt => write!(f, "halt"),
            Self::AddAR8(param) => write!(f, "add a, {}", param),
            Self::AdcAR8(param) => write!(f, "adc a, {}", param),
            Self::SubAR8(param) => write!(f, "sub a, {}", param),
            Self::SbcAR8(param) => write!(f, "sbc a, {}", param),
            Self::AndAR8(param) => write!(f, "and a, {}", param),
            Self::XorAR8(param) => write!(f, "xor a, {}", param),
            Self::OrAR8(param) => write!(f, "or a, {}", param),
            Self::CpAR8(param) => write!(f, "cp a, {}", param),
            Self::AddAImm8 => write!(f, "add a, imm8"),
            Self::AdcAImm8 => write!(f, "adc a, imm8"),
            Self::SubAImm8 => write!(f, "sub a, imm8"),
            Self::SbcAImm8 => write!(f, "sbc a, imm8"),
            Self::AndAImm8 => write!(f, "and a, imm8"),
            Self::XorAImm8 => write!(f, "xor a, imm8"),
            Self::OrAImm8 => write!(f, "or a, imm8"),
            Self::CpAImm8 => write!(f, "cp a, imm8"),
            Self::RetCond(param) => write!(f, "ret {}", param),
            Self::Ret => write!(f, "ret"),
            Self::Reti => write!(f, "reti"),
            Self::JpCondImm16(param) => write!(f, "jp {}, imm16", param),
            Self::JpImm16 => write!(f, "jp imm16"),
            Self::JpHl => write!(f, "jp hl"),
            Self::CallCondImm16(param) => write!(f, "call {}, imm16", param),
            Self::CallImm16 => write!(f, "call imm16"),
            Self::Rst(addr) => write!(f, "rst {}", addr),
            Self::Pop(param) => write!(f, "pop {}", param),
            Self::Push(param) => write!(f, "push {}", param),
            Self::Prefix => write!(f, "prefix"),
            Self::LdhZCZA => write!(f, "ldh [c], a"),
            Self::LdhZImm8ZA => write!(f, "ldh [imm8], a"),
            Self::LdZImm16ZA => write!(f, "ld [imm16], a"),
            Self::LdhAZCZ => write!(f, "ldh a, [c]"),
            Self::LdhAZImm8Z => write!(f, "ldh a, [imm8]"),
            Self::LdAZImm16Z => write!(f, "ld a, [imm16]"),
            Self::AddSpImm8 => write!(f, "add sp, imm8"),
            Self::LdHlSpXImm8 => write!(f, "ld hl, sp + imm8"),
            Self::LdSpHl => write!(f, "ld sp, hl"),
            Self::Di => write!(f, "di"),
            Self::Ei => write!(f, "ei"),
        }
    }
}

impl super::Cpu {
    fn get_r8_val(&self, param: ParamR8) -> u8 {
        match param {
            ParamR8::A => self.af.get_hi(),
            ParamR8::B => self.bc.get_hi(),
            ParamR8::C => self.bc.get_lo(),
            ParamR8::D => self.de.get_hi(),
            ParamR8::E => self.de.get_lo(),
            ParamR8::H => self.hl.get_hi(),
            ParamR8::L => self.hl.get_lo(),
            ParamR8::ZHLZ => self.mem.read(self.hl.get()),
        }
    }

    fn set_r8_val(&mut self, param: ParamR8, val: u8) {
        match param {
            ParamR8::A => self.af.set_hi(val),
            ParamR8::B => self.bc.set_hi(val),
            ParamR8::C => self.bc.set_lo(val),
            ParamR8::D => self.de.set_hi(val),
            ParamR8::E => self.de.set_lo(val),
            ParamR8::H => self.hl.set_hi(val),
            ParamR8::L => self.hl.set_lo(val),
            ParamR8::ZHLZ => self.mem.write(self.hl.get(), val),
        };
    }

    fn get_r16_val(&self, param: ParamR16) -> u16 {
        match param {
            ParamR16::BC => self.bc.get(),
            ParamR16::DE => self.de.get(),
            ParamR16::HL => self.hl.get(),
            ParamR16::SP => self.sp.get(),
        }
    }

    fn set_r16_val(&mut self, param: ParamR16, val: u16) {
        match param {
            ParamR16::BC => self.bc.set(val),
            ParamR16::DE => self.de.set(val),
            ParamR16::HL => self.hl.set(val),
            ParamR16::SP => self.sp.set(val),
        };
    }

    fn get_r16_mem_val(&self, param: ParamR16Mem) -> u16 {
        match param {
            ParamR16Mem::BC => self.bc.get(),
            ParamR16Mem::DE => self.de.get(),
            ParamR16Mem::HLD | ParamR16Mem::HLI => self.hl.get(),
        }
    }

    fn get_r16_stk_val(&self, param: ParamR16Stk) -> u16 {
        match param {
            ParamR16Stk::AF => self.af.get(),
            ParamR16Stk::BC => self.bc.get(),
            ParamR16Stk::DE => self.de.get(),
            ParamR16Stk::HL => self.hl.get(),
        }
    }

    fn set_r16_stk_val(&mut self, param: ParamR16Stk, val: u16) {
        match param {
            ParamR16Stk::AF => self.af.set(val),
            ParamR16Stk::BC => self.bc.set(val),
            ParamR16Stk::DE => self.de.set(val),
            ParamR16Stk::HL => self.hl.set(val),
        };
    }

    fn check_cond(&self, cond: ParamCond) -> bool {
        match cond {
            ParamCond::C => self.get_cf(),
            ParamCond::Nc => !self.get_cf(),
            ParamCond::Z => self.get_zf(),
            ParamCond::Nz => !self.get_zf(),
        }
    }

    fn step_cb_op(&mut self, op: CbPrefixOp) -> u8 {
        match op {
            CbPrefixOp::RlcR8(param) => {
                let val = self.get_r8_val(param);
                let cf = (val & 0x80) > 0;
                let val = val.rotate_left(1);

                self.set_r8_val(param, val);
                self.set_cf(cf);
                self.set_zf(val == 0);
                self.set_hf(false);
                self.set_nf(false);

                match param {
                    ParamR8::ZHLZ => 4,
                    _ => 2,
                }
            }
            CbPrefixOp::RrcR8(param) => {
                let val = self.get_r8_val(param);
                let cf = (val & 0x01) > 0;
                let val = val.rotate_right(1);

                self.set_r8_val(param, val);
                self.set_cf(cf);
                self.set_zf(val == 0);
                self.set_hf(false);
                self.set_nf(false);

                match param {
                    ParamR8::ZHLZ => 4,
                    _ => 2,
                }
            }
            CbPrefixOp::RlR8(param) => {
                let o_cf = self.get_cf();
                let o_val = self.get_r8_val(param);

                // after rotation
                let n_val = o_val << 1
                    | match o_cf {
                        true => 1,
                        false => 0,
                    };
                let n_cf = (o_val & 0x80) > 0;

                // set val
                self.set_r8_val(param, n_val);
                self.set_cf(n_cf);
                self.set_zf(n_val == 0);
                self.set_hf(false);
                self.set_nf(false);

                match param {
                    ParamR8::ZHLZ => 4,
                    _ => 2,
                }
            }
            CbPrefixOp::RrR8(param) => {
                let o_cf = self.get_cf();
                let o_val = self.get_r8_val(param);

                // after rotation
                let n_val = o_val >> 1
                    | match o_cf {
                        true => 0x80,
                        false => 0,
                    };
                let n_cf = (o_val & 0x01) > 0;

                // set val
                self.set_r8_val(param, n_val);
                self.set_cf(n_cf);
                self.set_zf(n_val == 0);
                self.set_hf(false);
                self.set_nf(false);

                match param {
                    ParamR8::ZHLZ => 4,
                    _ => 2,
                }
            }
            CbPrefixOp::SlaR8(param) => {
                let o_val = self.get_r8_val(param);
                let n_val = o_val << 1;
                self.set_r8_val(param, n_val);
                self.set_cf(o_val & 0x80 > 0);
                self.set_zf(n_val == 0);
                self.set_hf(false);
                self.set_nf(false);

                match param {
                    ParamR8::ZHLZ => 4,
                    _ => 2,
                }
            }
            CbPrefixOp::SraR8(param) => {
                let o_val = self.get_r8_val(param);
                let n_val = (o_val >> 1) | (o_val & 0x80); // preserve bit 7
                self.set_r8_val(param, n_val);
                self.set_cf(o_val & 0x01 > 0);
                self.set_zf(n_val == 0);
                self.set_hf(false);
                self.set_nf(false);

                match param {
                    ParamR8::ZHLZ => 4,
                    _ => 2,
                }
            }
            CbPrefixOp::SwapR8(param) => {
                let o_val = self.get_r8_val(param);
                let n_val = ((o_val & 0xF0) >> 4) | ((o_val & 0x0F) << 4);
                self.set_r8_val(param, n_val);
                self.set_zf(n_val == 0);
                self.set_cf(false);
                self.set_hf(false);
                self.set_nf(false);

                match param {
                    ParamR8::ZHLZ => 4,
                    _ => 2,
                }
            }
            CbPrefixOp::SrlR8(param) => {
                let o_val = self.get_r8_val(param);
                let n_val = o_val >> 1;
                self.set_r8_val(param, n_val);
                self.set_cf((o_val & 0x01) > 0);
                self.set_zf(n_val == 0);
                self.set_hf(false);
                self.set_nf(false);

                match param {
                    ParamR8::ZHLZ => 4,
                    _ => 2,
                }
            }
            CbPrefixOp::BitB3R8(bit, param) => {
                let val = self.get_r8_val(param);
                self.set_zf(((val >> bit.val()) & 0x01) == 0);
                self.set_hf(true);
                self.set_nf(false);

                match param {
                    ParamR8::ZHLZ => 3,
                    _ => 2,
                }
            }
            CbPrefixOp::ResB3R8(bit, param) => {
                let val = self.get_r8_val(param);
                let mask = !(0x01u8 << bit.val());
                self.set_r8_val(param, val & mask);

                match param {
                    ParamR8::ZHLZ => 4,
                    _ => 2,
                }
            }
            CbPrefixOp::SetB3R8(bit, param) => {
                let val = self.get_r8_val(param);
                let mask = 0x01u8 << bit.val();
                self.set_r8_val(param, val | mask);

                match param {
                    ParamR8::ZHLZ => 4,
                    _ => 2,
                }
            }
        }
    }

    pub fn execute_op(&mut self) -> u8 {
        // Fetch
        let byte = self.mem.read(self.pc.post_inc());

        // Decode
        let op = match Op::try_from(byte) {
            Some(o) => o,
            None => panic!("Invalid opcode 0x{:02X} (0b{:08b})", byte, byte),
        };

        // Print opcode
        println!(
            "0x{0:04x}: {1} (0b{2:08b}) (0x{2:02x})",
            self.pc.get().wrapping_sub(1),
            op,
            byte
        );

        // Execute
        match op {
            Op::Nop => 1,
            Op::LdR16Imm16(param) => {
                // Read 2 bytes
                let first = self.mem.read(self.pc.post_inc());
                let second = self.mem.read(self.pc.post_inc());

                // Write to register
                self.set_r16_val(param, u16::from_le_bytes([first, second]));
                3
            }
            Op::LdZR16MemZA(param) => {
                // Load index
                let idx = self.get_r16_mem_val(param);

                // Write a to addr
                self.mem.write(idx, self.af.get_hi());

                // Increment or decrement hl
                match param {
                    ParamR16Mem::HLI => self.hl.set(self.hl.get().wrapping_add(1)),
                    ParamR16Mem::HLD => self.hl.set(self.hl.get().wrapping_sub(1)),
                    _ => {}
                };
                2
            }
            Op::LdAZR16MemZ(param) => {
                // Load index
                let idx = self.get_r16_mem_val(param);

                // Write byte pointed to a
                self.af.set_hi(self.mem.read(idx));

                // Increment or decrement hl
                match param {
                    ParamR16Mem::HLI => self.hl.set(self.hl.get().wrapping_add(1)),
                    ParamR16Mem::HLD => self.hl.set(self.hl.get().wrapping_sub(1)),
                    _ => {}
                };
                2
            }
            Op::LdZImm16ZSp => {
                // Read next 2 bytes
                let first = self.mem.read(self.pc.post_inc());
                let second = self.mem.read(self.pc.post_inc());
                let idx = u16::from_le_bytes([first, second]);

                // Use it as index to write val of sp
                self.mem.write(idx, self.sp.get_lo());
                self.mem.write(idx + 1, self.sp.get_hi());
                5
            }
            Op::IncR16(param) => {
                let val = self.get_r16_val(param);
                self.set_r16_val(param, val.wrapping_add(1));
                2
            }
            Op::DecR16(param) => {
                let val = self.get_r16_val(param);
                self.set_r16_val(param, val.wrapping_sub(1));
                2
            }
            Op::AddHlR16(param) => {
                let hl_val = self.hl.get();
                let add_num: u16 = self.get_r16_val(param);

                // Addition
                let (val, cf) = hl_val.overflowing_add(add_num);
                self.hl.set(val);

                // Set flags
                self.set_nf(false); // sub flag
                self.set_hf(((hl_val & 0x0FFF) + (add_num & 0x0FFF)) > 0x0FFF); // 12th bit overflow
                self.set_cf(cf); // 16th bit overflow
                2
            }
            Op::IncR8(param) => {
                // Read the byte
                let old_val = self.get_r8_val(param);

                // Write the byte
                let new_val = old_val.wrapping_add(1);
                self.set_r8_val(param, new_val);

                // Set Flags
                self.set_zf(new_val == 0);
                self.set_nf(false);
                self.set_hf(hf_add(old_val, 1)); // 4th bit overflow

                match param {
                    ParamR8::ZHLZ => 3,
                    _ => 1,
                }
            }
            Op::DecR8(param) => {
                // Read the byte
                let old_val = self.get_r8_val(param);

                // Write the byte
                let new_val = old_val.wrapping_sub(1);
                self.set_r8_val(param, new_val);

                // Set Flags
                self.set_zf(new_val == 0);
                self.set_nf(true);
                self.set_hf(hf_sub(old_val, 1)); // 5th bit borrow

                match param {
                    ParamR8::ZHLZ => 3,
                    _ => 1,
                }
            }
            Op::LdR8Imm8(param) => {
                let val = self.mem.read(self.pc.post_inc());
                self.set_r8_val(param, val);
                match param {
                    ParamR8::ZHLZ => 3,
                    _ => 2,
                }
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
                1
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
                1
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
                1
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
                1
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
                1
            }
            Op::Cpl => {
                self.af.set_hi(!self.af.get_hi());
                self.set_nf(true);
                self.set_hf(true);
                1
            }
            Op::Scf => {
                self.set_nf(false);
                self.set_hf(false);
                self.set_cf(true);
                1
            }
            Op::Ccf => {
                self.set_nf(false);
                self.set_hf(false);
                self.set_cf(!self.get_cf());
                1
            }
            Op::JrImm8 => {
                // Read next byte as 2's complement relative position
                let n = self.mem.read(self.pc.post_inc());
                let rlt = unsafe { *(&n as *const u8 as *const i8) };

                // Update pc
                let pc = self.pc.get() as i32 + rlt as i32;
                self.pc.set(pc as u16);
                3
            }
            Op::JrCcImm8(cond) => {
                // Read next byte
                let n = self.mem.read(self.pc.post_inc());

                // Check condition
                let is_jp = match cond {
                    ParamCond::C => self.get_cf(),
                    ParamCond::Nc => !self.get_cf(),
                    ParamCond::Z => self.get_zf(),
                    ParamCond::Nz => !self.get_zf(),
                };

                // Jmp if cond is met
                if is_jp {
                    // relative position in 2's complement
                    let rlt = unsafe { *(&n as *const u8 as *const i8) };

                    // Update pc
                    let pc = self.pc.get() as i32 + rlt as i32;
                    self.pc.set(pc as u16);
                    3
                } else {
                    2
                }
            }
            Op::Stop => {
                // TODO: place holder
                1
            }
            Op::LdR8R8(dest, src) => {
                let val = self.get_r8_val(src);
                self.set_r8_val(dest, val);
                match dest {
                    ParamR8::ZHLZ => 2,
                    _ => match src {
                        ParamR8::ZHLZ => 2,
                        _ => 1,
                    },
                }
            }
            Op::Halt => {
                self.halted = true;
                1
            }
            Op::AddAR8(param) => {
                let lhs = self.af.get_hi();
                let rhs = self.get_r8_val(param);
                let (a, cf) = lhs.overflowing_add(rhs);

                self.af.set_hi(a);
                self.set_zf(a == 0);
                self.set_nf(false);
                self.set_hf(hf_add(lhs, rhs));
                self.set_cf(cf);

                match param {
                    ParamR8::ZHLZ => 2,
                    _ => 1,
                }
            }
            Op::AdcAR8(param) => {
                let lhs = self.af.get_hi();
                let rhs = self.get_r8_val(param);
                let carry = match self.get_cf() {
                    true => 1,
                    false => 0,
                };

                let (a1, c1) = lhs.overflowing_add(rhs);
                let (a2, c2) = a1.overflowing_add(carry);
                self.af.set_hi(a2);
                self.set_zf(a2 == 0);
                self.set_nf(false);
                self.set_cf(c1 || c2);
                self.set_hf(hf_add(lhs, rhs) || hf_add(a1, carry));

                match param {
                    ParamR8::ZHLZ => 2,
                    _ => 1,
                }
            }
            Op::SubAR8(param) => {
                let lhs = self.af.get_hi();
                let rhs = self.get_r8_val(param);
                let (a, cf) = lhs.overflowing_sub(rhs);

                self.af.set_hi(a);
                self.set_zf(a == 0);
                self.set_nf(true);
                self.set_hf(hf_sub(lhs, rhs));
                self.set_cf(cf);

                match param {
                    ParamR8::ZHLZ => 2,
                    _ => 1,
                }
            }
            Op::SbcAR8(param) => {
                let lhs = self.af.get_hi();
                let rhs = self.get_r8_val(param);
                let carry = match self.get_cf() {
                    true => 1,
                    false => 0,
                };

                let (a1, c1) = lhs.overflowing_sub(rhs);
                let (a2, c2) = a1.overflowing_sub(carry);
                self.af.set_hi(a2);
                self.set_zf(a2 == 0);
                self.set_nf(true);
                self.set_hf(hf_sub(lhs, rhs) || hf_sub(a1, carry));
                self.set_cf(c1 || c2);

                match param {
                    ParamR8::ZHLZ => 2,
                    _ => 1,
                }
            }
            Op::AndAR8(param) => {
                let lhs = self.af.get_hi();
                let rhs = self.get_r8_val(param);
                let a = lhs & rhs;
                self.af.set_hi(a);
                self.set_zf(a == 0);
                self.set_nf(false);
                self.set_hf(true);
                self.set_cf(false);

                match param {
                    ParamR8::ZHLZ => 2,
                    _ => 1,
                }
            }
            Op::XorAR8(param) => {
                let lhs = self.af.get_hi();
                let rhs = self.get_r8_val(param);
                let a = lhs ^ rhs;
                self.af.set_hi(a);
                self.set_zf(a == 0);
                self.set_nf(false);
                self.set_hf(false);
                self.set_cf(false);

                match param {
                    ParamR8::ZHLZ => 2,
                    _ => 1,
                }
            }
            Op::OrAR8(param) => {
                let lhs = self.af.get_hi();
                let rhs = self.get_r8_val(param);
                let a = lhs | rhs;
                self.af.set_hi(a);
                self.set_zf(a == 0);
                self.set_nf(false);
                self.set_hf(false);
                self.set_cf(false);

                match param {
                    ParamR8::ZHLZ => 2,
                    _ => 1,
                }
            }
            Op::CpAR8(param) => {
                let lhs = self.af.get_hi();
                let rhs = self.get_r8_val(param);
                let (val, cf) = lhs.overflowing_sub(rhs);
                self.set_zf(val == 0);
                self.set_nf(true);
                self.set_hf(hf_sub(lhs, rhs));
                self.set_cf(cf);

                match param {
                    ParamR8::ZHLZ => 2,
                    _ => 1,
                }
            }
            Op::AddAImm8 => {
                let lhs = self.af.get_hi();
                let rhs = self.mem.read(self.pc.post_inc());
                let (a, cf) = lhs.overflowing_add(rhs);

                self.af.set_hi(a);
                self.set_zf(a == 0);
                self.set_nf(false);
                self.set_hf(hf_add(lhs, rhs));
                self.set_cf(cf);
                2
            }
            Op::AdcAImm8 => {
                let lhs = self.af.get_hi();
                let rhs = self.mem.read(self.pc.post_inc());
                let carry = match self.get_cf() {
                    true => 1,
                    false => 0,
                };

                let (a1, c1) = lhs.overflowing_add(rhs);
                let (a2, c2) = a1.overflowing_add(carry);
                self.af.set_hi(a2);
                self.set_zf(a2 == 0);
                self.set_nf(false);
                self.set_hf(hf_add(lhs, rhs) || hf_add(a1, carry));
                self.set_cf(c1 || c2);
                2
            }
            Op::SubAImm8 => {
                let lhs = self.af.get_hi();
                let rhs = self.mem.read(self.pc.post_inc());
                let (a, cf) = lhs.overflowing_sub(rhs);

                self.af.set_hi(a);
                self.set_zf(a == 0);
                self.set_nf(true);
                self.set_hf(hf_sub(lhs, rhs));
                self.set_cf(cf);
                2
            }
            Op::SbcAImm8 => {
                let lhs = self.af.get_hi();
                let rhs = self.mem.read(self.pc.post_inc());
                let carry = match self.get_cf() {
                    true => 1,
                    false => 0,
                };

                let (a1, c1) = lhs.overflowing_sub(rhs);
                let (a2, c2) = a1.overflowing_sub(carry);
                self.af.set_hi(a2);
                self.set_zf(a2 == 0);
                self.set_nf(true);
                self.set_hf(hf_sub(lhs, rhs) || hf_sub(a1, carry));
                self.set_cf(c1 || c2);
                2
            }
            Op::AndAImm8 => {
                let lhs = self.af.get_hi();
                let rhs = self.mem.read(self.pc.post_inc());
                let a = lhs & rhs;
                self.af.set_hi(a);
                self.set_zf(a == 0);
                self.set_nf(false);
                self.set_hf(true);
                self.set_cf(false);
                2
            }
            Op::XorAImm8 => {
                let lhs = self.af.get_hi();
                let rhs = self.mem.read(self.pc.post_inc());
                let a = lhs ^ rhs;
                self.af.set_hi(a);
                self.set_zf(a == 0);
                self.set_nf(false);
                self.set_hf(false);
                self.set_cf(false);
                2
            }
            Op::OrAImm8 => {
                let lhs = self.af.get_hi();
                let rhs = self.mem.read(self.pc.post_inc());
                let a = lhs | rhs;
                self.af.set_hi(a);
                self.set_zf(a == 0);
                self.set_nf(false);
                self.set_hf(false);
                self.set_cf(false);
                2
            }
            Op::CpAImm8 => {
                let lhs = self.af.get_hi();
                let rhs = self.mem.read(self.pc.post_inc());
                let (val, cf) = lhs.overflowing_sub(rhs);
                self.set_zf(val == 0);
                self.set_nf(true);
                self.set_hf(hf_sub(lhs, rhs));
                self.set_cf(cf);
                2
            }
            Op::RetCond(cond) => {
                let is_jp = self.check_cond(cond);
                if is_jp {
                    self.pc.set_lo(self.mem.read(self.sp.post_inc()));
                    self.pc.set_hi(self.mem.read(self.sp.post_inc()));
                    5
                } else {
                    2
                }
            }
            Op::Ret => {
                self.pc.set_lo(self.mem.read(self.sp.post_inc()));
                self.pc.set_hi(self.mem.read(self.sp.post_inc()));
                4
            }
            Op::Reti => {
                self.ime_pending = super::ImePendingStatus::ThisInstr;
                self.pc.set_lo(self.mem.read(self.sp.post_inc()));
                self.pc.set_hi(self.mem.read(self.sp.post_inc()));
                4
            }
            Op::JpCondImm16(cond) => {
                let is_jp = self.check_cond(cond);

                let jp_addr = u16::from_le_bytes([
                    self.mem.read(self.pc.post_inc()),
                    self.mem.read(self.pc.post_inc()),
                ]);

                if is_jp {
                    self.pc.set(jp_addr);
                    4
                } else {
                    3
                }
            }
            Op::JpImm16 => {
                let jp_addr = u16::from_le_bytes([
                    self.mem.read(self.pc.post_inc()),
                    self.mem.read(self.pc.post_inc()),
                ]);
                self.pc.set(jp_addr);
                4
            }
            Op::JpHl => {
                self.pc.set(self.hl.get());
                1
            }
            Op::CallCondImm16(cond) => {
                let is_jp = self.check_cond(cond);

                // Get the jump address
                let jp_addr = u16::from_le_bytes([
                    self.mem.read(self.pc.post_inc()),
                    self.mem.read(self.pc.post_inc()),
                ]);

                if is_jp {
                    // Push the current PC to stack
                    let cur_pc = self.pc.get().to_le_bytes();
                    self.mem.write(self.sp.pre_dec(), cur_pc[1]);
                    self.mem.write(self.sp.pre_dec(), cur_pc[0]);

                    // Update PC
                    self.pc.set(jp_addr);
                    6
                } else {
                    3
                }
            }
            Op::CallImm16 => {
                // Get the jump address
                let jp_addr = u16::from_le_bytes([
                    self.mem.read(self.pc.post_inc()),
                    self.mem.read(self.pc.post_inc()),
                ]);

                // Push the current PC to stack
                let cur_pc = self.pc.get().to_le_bytes();
                self.mem.write(self.sp.pre_dec(), cur_pc[1]);
                self.mem.write(self.sp.pre_dec(), cur_pc[0]);

                // Update PC
                self.pc.set(jp_addr);
                6
            }
            Op::Rst(addr) => {
                // Push the current PC to stack
                let cur_pc = self.pc.get().to_le_bytes();
                self.mem.write(self.sp.pre_dec(), cur_pc[1]);
                self.mem.write(self.sp.pre_dec(), cur_pc[0]);

                // Update PC
                self.pc.set(addr.addr());
                4
            }
            Op::Pop(param) => {
                let lo = match param {
                    ParamR16Stk::AF => self.mem.read(self.sp.post_inc()) & 0xF0, // The lower nibble of F is always zero
                    _ => self.mem.read(self.sp.post_inc()),
                };
                let hi = self.mem.read(self.sp.post_inc());
                let val = u16::from_le_bytes([lo, hi]);
                self.set_r16_stk_val(param, val);
                3
            }
            Op::Push(param) => {
                let val = self.get_r16_stk_val(param).to_le_bytes();
                self.mem.write(self.sp.pre_dec(), val[1]);
                self.mem.write(self.sp.pre_dec(), val[0]);
                4
            }
            Op::Prefix => {
                let b = self.mem.read(self.pc.post_inc());
                let op = CbPrefixOp::try_from(b)
                    .unwrap_or_else(|_| panic!("Invalid 0xCB prefix op code 0x{:02X}", b));
                println!(
                    "0x{0:04x}: {1} (0b{2:08b}) (0x{2:02x})",
                    self.pc.get().wrapping_sub(1),
                    op,
                    u8::from(op),
                );
                self.step_cb_op(op)
            }
            Op::LdhZCZA => {
                let addr = 0xFF00 + self.bc.get_lo() as u16;
                self.mem.write(addr, self.af.get_hi());
                2
            }
            Op::LdhZImm8ZA => {
                let addr = 0xFF00 + self.mem.read(self.pc.post_inc()) as u16;
                self.mem.write(addr, self.af.get_hi());
                3
            }
            Op::LdZImm16ZA => {
                let addr = u16::from_le_bytes([
                    self.mem.read(self.pc.post_inc()),
                    self.mem.read(self.pc.post_inc()),
                ]);
                self.mem.write(addr, self.af.get_hi());
                4
            }
            Op::LdhAZCZ => {
                let addr = 0xFF00 + self.bc.get_lo() as u16;
                self.af.set_hi(self.mem.read(addr));
                2
            }
            Op::LdhAZImm8Z => {
                let addr = 0xFF00 + self.mem.read(self.pc.post_inc()) as u16;
                self.af.set_hi(self.mem.read(addr));
                3
            }
            Op::LdAZImm16Z => {
                let addr = u16::from_le_bytes([
                    self.mem.read(self.pc.post_inc()),
                    self.mem.read(self.pc.post_inc()),
                ]);
                self.af.set_hi(self.mem.read(addr));
                4
            }
            Op::AddSpImm8 => {
                let imm8 = self.mem.read(self.pc.post_inc());
                let splo = self.sp.get_lo();
                let sphi = self.sp.get_hi();

                // Add imm8 with sp-lo
                let hf = hf_add(splo, imm8);
                let (splo, cf) = splo.overflowing_add(imm8);

                // Inc / Dec sp-hi according to carry flag and sign bit of imm8
                let signed = (imm8 & 0x80) > 0;
                let sphi = if cf && !signed {
                    // if carry & not signed, inc sp hi
                    sphi.wrapping_add(1)
                } else if !cf && signed {
                    // if not carry & signed, dec sp hi
                    sphi.wrapping_sub(1)
                } else {
                    // keep sp hi as-is
                    sphi
                };

                // Set values
                self.sp.set_lo(splo);
                self.sp.set_hi(sphi);
                self.set_cf(cf);
                self.set_hf(hf);
                self.set_zf(false);
                self.set_nf(false);
                4
            }
            Op::LdHlSpXImm8 => {
                let imm8 = self.mem.read(self.pc.post_inc());
                let splo = self.sp.get_lo();
                let sphi = self.sp.get_hi();

                // Add imm8 with sp-lo
                let hf = hf_add(splo, imm8);
                let (splo, cf) = splo.overflowing_add(imm8);

                // Inc / Dec sp-hi according to carry flag and sign bit of imm8
                let signed = (imm8 & 0x80) > 0;
                let sphi = if cf && !signed {
                    // if carry & not signed, inc sp hi
                    sphi.wrapping_add(1)
                } else if !cf && signed {
                    // if not carry & signed, dec sp hi
                    sphi.wrapping_sub(1)
                } else {
                    // keep sp hi as-is
                    sphi
                };

                // Set values
                self.hl.set_lo(splo);
                self.hl.set_hi(sphi);
                self.set_cf(cf);
                self.set_hf(hf);
                self.set_zf(false);
                self.set_nf(false);
                3
            }
            Op::LdSpHl => {
                self.sp.set(self.hl.get());
                2
            }
            Op::Di => {
                self.ime = false;
                1
            }
            Op::Ei => {
                self.ime_pending = super::ImePendingStatus::NextInstr;
                1
            }
        }
    }
}

#[doc = "if lhs add rhs will overflow from bit 3 to bit 4"]
fn hf_add(lhs: u8, rhs: u8) -> bool {
    ((lhs & 0x0Fu8) + (rhs & 0x0Fu8)) > 0x0Fu8
}

#[doc = "if lhs sub rhs will borrow from bit 4 to bit 3"]
fn hf_sub(lhs: u8, rhs: u8) -> bool {
    (lhs & 0x0Fu8) < (rhs & 0x0Fu8)
}
