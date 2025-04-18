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

    fn post_inc(&mut self) -> usize {
        let retval = self.get();
        self.set(retval.wrapping_add(1));
        retval as usize
    }

    fn pre_dec(&mut self) -> usize {
        let retval = self.get().wrapping_sub(1);
        self.set(retval);
        retval as usize
    }

    fn as_idx(&self) -> usize {
        self.get() as usize
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

#[doc = "if lhs add rhs will overflow from bit 3 to bit 4"]
fn hf_add(lhs: u8, rhs: u8) -> bool {
    ((lhs & 0x0Fu8) + (rhs & 0x0Fu8)) > 0x0Fu8
}

#[doc = "if lhs sub rhs will borrow from bit 4 to bit 3"]
fn hf_sub(lhs: u8, rhs: u8) -> bool {
    (lhs & 0x0Fu8) < (rhs & 0x0Fu8)
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
        let byte = self.mem[self.pc.post_inc()];

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
                let first = self.mem[self.pc.post_inc()];
                let second = self.mem[self.pc.post_inc()];

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
                    ParamR16Mem::BC => self.bc.as_idx(),
                    ParamR16Mem::DE => self.de.as_idx(),
                    ParamR16Mem::HLD | ParamR16Mem::HLI => self.hl.as_idx(),
                };

                // Write a to addr
                self.mem[idx] = self.af.get_hi();

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
                    ParamR16Mem::BC => self.bc.as_idx(),
                    ParamR16Mem::DE => self.de.as_idx(),
                    ParamR16Mem::HLD | ParamR16Mem::HLI => self.hl.as_idx(),
                };

                // Write byte pointed to a
                self.af.set_hi(self.mem[idx]);

                // Increment or decrement hl
                match param {
                    ParamR16Mem::HLI => self.hl.set(self.hl.get().wrapping_add(1)),
                    ParamR16Mem::HLD => self.hl.set(self.hl.get().wrapping_sub(1)),
                    _ => {}
                };
            }
            Op::LdZImm16ZSp => {
                // Read next 2 bytes
                let first = self.mem[self.pc.post_inc()];
                let second = self.mem[self.pc.post_inc()];
                let idx = u16::from_le_bytes([first, second]) as usize;

                // Use it as index to write val of sp
                self.mem[idx] = self.sp.get_lo();
                self.mem[idx + 1] = self.sp.get_hi();
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
                let (val, cf) = hl_val.overflowing_add(add_num);
                self.hl.set(val);

                // Set flags
                self.set_nf(false); // sub flag
                self.set_hf(((hl_val & 0x0FFF) + (add_num & 0x0FFF)) > 0x0FFF); // 12th bit overflow
                self.set_cf(cf); // 16th bit overflow
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
                    ParamR8::ZHLZ => self.mem[self.hl.as_idx()],
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
                    ParamR8::ZHLZ => self.mem[self.hl.as_idx()] = new_val,
                };

                // Set Flags
                self.set_zf(new_val == 0);
                self.set_nf(false);
                self.set_hf(hf_add(old_val, 1)); // 4th bit overflow
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
                    ParamR8::ZHLZ => self.mem[self.hl.as_idx()],
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
                    ParamR8::ZHLZ => self.mem[self.hl.as_idx()] = new_val,
                };

                // Set Flags
                self.set_zf(new_val == 0);
                self.set_nf(true);
                self.set_hf(hf_sub(old_val, 1)); // 5th bit borrow
            }
            Op::LdR8Imm8(param) => {
                let val = self.mem[self.pc.post_inc()];
                match param {
                    ParamR8::A => self.af.set_hi(val),
                    ParamR8::B => self.bc.set_hi(val),
                    ParamR8::C => self.bc.set_lo(val),
                    ParamR8::D => self.de.set_hi(val),
                    ParamR8::E => self.de.set_lo(val),
                    ParamR8::H => self.hl.set_hi(val),
                    ParamR8::L => self.hl.set_lo(val),
                    ParamR8::ZHLZ => self.mem[self.hl.as_idx()] = val,
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
                let n = self.mem[self.pc.post_inc()];
                let rlt = unsafe { *(&n as *const u8 as *const i8) };

                // Update pc
                let pc = self.pc.get() as i32 + rlt as i32;
                self.pc.set(pc as u16);
            }
            Op::JrCcImm8(cond) => {
                // Read next byte
                let n = self.mem[self.pc.post_inc()];

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
                }
            }
            Op::Stop => {
                // TODO: place holder
                self.halted = true;
            }
            Op::LdR8R8(dest, src) => {
                // Read val
                let val = match src {
                    ParamR8::A => self.af.get_hi(),
                    ParamR8::B => self.bc.get_hi(),
                    ParamR8::C => self.bc.get_lo(),
                    ParamR8::D => self.de.get_hi(),
                    ParamR8::E => self.de.get_lo(),
                    ParamR8::H => self.hl.get_hi(),
                    ParamR8::L => self.hl.get_lo(),
                    ParamR8::ZHLZ => self.mem[self.hl.as_idx()],
                };

                // Set val
                match dest {
                    ParamR8::A => self.af.set_hi(val),
                    ParamR8::B => self.bc.set_hi(val),
                    ParamR8::C => self.bc.set_lo(val),
                    ParamR8::D => self.de.set_hi(val),
                    ParamR8::E => self.de.set_lo(val),
                    ParamR8::H => self.hl.set_hi(val),
                    ParamR8::L => self.hl.set_lo(val),
                    ParamR8::ZHLZ => self.mem[self.hl.as_idx()] = val,
                };
            }
            Op::Halt => {
                // TODO: place holder
                self.halted = true;
            }
            Op::AddAR8(param) => {
                let lhs = self.af.get_hi();
                let rhs = match param {
                    ParamR8::A => self.af.get_hi(),
                    ParamR8::B => self.bc.get_hi(),
                    ParamR8::C => self.bc.get_lo(),
                    ParamR8::D => self.de.get_hi(),
                    ParamR8::E => self.de.get_lo(),
                    ParamR8::H => self.hl.get_hi(),
                    ParamR8::L => self.hl.get_lo(),
                    ParamR8::ZHLZ => self.mem[self.hl.as_idx()],
                };
                let (a, cf) = lhs.overflowing_add(rhs);

                self.af.set_hi(a);
                self.set_zf(a == 0);
                self.set_nf(false);
                self.set_hf(hf_add(lhs, rhs));
                self.set_cf(cf);
            }
            Op::AdcAR8(param) => {
                let lhs = self.af.get_hi();
                let rhs = match param {
                    ParamR8::A => self.af.get_hi(),
                    ParamR8::B => self.bc.get_hi(),
                    ParamR8::C => self.bc.get_lo(),
                    ParamR8::D => self.de.get_hi(),
                    ParamR8::E => self.de.get_lo(),
                    ParamR8::H => self.hl.get_hi(),
                    ParamR8::L => self.hl.get_lo(),
                    ParamR8::ZHLZ => self.mem[self.hl.as_idx()],
                };
                let carry = match self.get_cf() {
                    true => 1,
                    false => 0,
                };

                let (a1, c1) = lhs.overflowing_add(rhs);
                let (a2, c2) = a1.overflowing_add(carry);
                self.af.set_hi(a2);
                self.set_nf(false);
                self.set_cf(c1 || c2);
                self.set_hf(hf_add(lhs, rhs) || hf_add(a1, carry));
            }
            Op::SubAR8(param) => {
                let lhs = self.af.get_hi();
                let rhs = match param {
                    ParamR8::A => self.af.get_hi(),
                    ParamR8::B => self.bc.get_hi(),
                    ParamR8::C => self.bc.get_lo(),
                    ParamR8::D => self.de.get_hi(),
                    ParamR8::E => self.de.get_lo(),
                    ParamR8::H => self.hl.get_hi(),
                    ParamR8::L => self.hl.get_lo(),
                    ParamR8::ZHLZ => self.mem[self.hl.as_idx()],
                };
                let (a, cf) = lhs.overflowing_sub(rhs);

                self.af.set_hi(a);
                self.set_zf(a == 0);
                self.set_nf(true);
                self.set_hf(hf_sub(lhs, rhs));
                self.set_cf(cf);
            }
            Op::SbcAR8(param) => {
                let lhs = self.af.get_hi();
                let rhs = match param {
                    ParamR8::A => self.af.get_hi(),
                    ParamR8::B => self.bc.get_hi(),
                    ParamR8::C => self.bc.get_lo(),
                    ParamR8::D => self.de.get_hi(),
                    ParamR8::E => self.de.get_lo(),
                    ParamR8::H => self.hl.get_hi(),
                    ParamR8::L => self.hl.get_lo(),
                    ParamR8::ZHLZ => self.mem[self.hl.as_idx()],
                };
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
            }
            Op::AndAR8(param) => {
                let lhs = self.af.get_hi();
                let rhs = match param {
                    ParamR8::A => self.af.get_hi(),
                    ParamR8::B => self.bc.get_hi(),
                    ParamR8::C => self.bc.get_lo(),
                    ParamR8::D => self.de.get_hi(),
                    ParamR8::E => self.de.get_lo(),
                    ParamR8::H => self.hl.get_hi(),
                    ParamR8::L => self.hl.get_lo(),
                    ParamR8::ZHLZ => self.mem[self.hl.as_idx()],
                };
                let a = lhs & rhs;
                self.af.set_hi(a);
                self.set_zf(a == 0);
                self.set_nf(false);
                self.set_hf(true);
                self.set_cf(false);
            }
            Op::XorAR8(param) => {
                let lhs = self.af.get_hi();
                let rhs = match param {
                    ParamR8::A => self.af.get_hi(),
                    ParamR8::B => self.bc.get_hi(),
                    ParamR8::C => self.bc.get_lo(),
                    ParamR8::D => self.de.get_hi(),
                    ParamR8::E => self.de.get_lo(),
                    ParamR8::H => self.hl.get_hi(),
                    ParamR8::L => self.hl.get_lo(),
                    ParamR8::ZHLZ => self.mem[self.hl.as_idx()],
                };
                let a = lhs ^ rhs;
                self.af.set_hi(a);
                self.set_zf(a == 0);
                self.set_nf(false);
                self.set_hf(false);
                self.set_cf(false);
            }
            Op::OrAR8(param) => {
                let lhs = self.af.get_hi();
                let rhs = match param {
                    ParamR8::A => self.af.get_hi(),
                    ParamR8::B => self.bc.get_hi(),
                    ParamR8::C => self.bc.get_lo(),
                    ParamR8::D => self.de.get_hi(),
                    ParamR8::E => self.de.get_lo(),
                    ParamR8::H => self.hl.get_hi(),
                    ParamR8::L => self.hl.get_lo(),
                    ParamR8::ZHLZ => self.mem[self.hl.as_idx()],
                };
                let a = lhs | rhs;
                self.af.set_hi(a);
                self.set_zf(a == 0);
                self.set_nf(false);
                self.set_hf(false);
                self.set_cf(false);
            }
            Op::CpAR8(param) => {
                let lhs = self.af.get_hi();
                let rhs = match param {
                    ParamR8::A => self.af.get_hi(),
                    ParamR8::B => self.bc.get_hi(),
                    ParamR8::C => self.bc.get_lo(),
                    ParamR8::D => self.de.get_hi(),
                    ParamR8::E => self.de.get_lo(),
                    ParamR8::H => self.hl.get_hi(),
                    ParamR8::L => self.hl.get_lo(),
                    ParamR8::ZHLZ => self.mem[self.hl.as_idx()],
                };
                let (val, cf) = lhs.overflowing_sub(rhs);
                self.set_zf(val == 0);
                self.set_nf(true);
                self.set_hf(hf_sub(lhs, rhs));
                self.set_cf(cf);
            }
            Op::AddAImm8 => {
                let lhs = self.af.get_hi();
                let rhs = self.mem[self.pc.post_inc()];
                let (a, cf) = lhs.overflowing_add(rhs);

                self.af.set_hi(a);
                self.set_zf(a == 0);
                self.set_nf(false);
                self.set_hf(hf_add(lhs, rhs));
                self.set_cf(cf);
            }
            Op::AdcAImm8 => {
                let lhs = self.af.get_hi();
                let rhs = self.mem[self.pc.post_inc()];
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
            }
            Op::SubAImm8 => {
                let lhs = self.af.get_hi();
                let rhs = self.mem[self.pc.post_inc()];
                let (a, cf) = lhs.overflowing_sub(rhs);

                self.af.set_hi(a);
                self.set_zf(a == 0);
                self.set_nf(true);
                self.set_hf(hf_sub(lhs, rhs));
                self.set_cf(cf);
            }
            Op::SbcAImm8 => {
                let lhs = self.af.get_hi();
                let rhs = self.mem[self.pc.post_inc()];
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
            }
            Op::AndAImm8 => {
                let lhs = self.af.get_hi();
                let rhs = self.mem[self.pc.post_inc()];
                let a = lhs & rhs;
                self.af.set_hi(a);
                self.set_zf(a == 0);
                self.set_nf(false);
                self.set_hf(true);
                self.set_cf(false);
            }
            Op::XorAImm8 => {
                let lhs = self.af.get_hi();
                let rhs = self.mem[self.pc.post_inc()];
                let a = lhs ^ rhs;
                self.af.set_hi(a);
                self.set_zf(a == 0);
                self.set_nf(false);
                self.set_hf(false);
                self.set_cf(false);
            }
            Op::OrAImm8 => {
                let lhs = self.af.get_hi();
                let rhs = self.mem[self.pc.post_inc()];
                let a = lhs | rhs;
                self.af.set_hi(a);
                self.set_zf(a == 0);
                self.set_nf(false);
                self.set_hf(false);
                self.set_cf(false);
            }
            Op::CpAImm8 => {
                let lhs = self.af.get_hi();
                let rhs = self.mem[self.pc.post_inc()];
                let (val, cf) = lhs.overflowing_sub(rhs);
                self.set_zf(val == 0);
                self.set_nf(true);
                self.set_hf(hf_sub(lhs, rhs));
                self.set_cf(cf);
            }
            Op::Ret => {
                self.pc.set_lo(self.mem[self.sp.post_inc()]);
                self.pc.set_hi(self.mem[self.sp.post_inc()]);
            }
            Op::CallImm16 => {
                // Get the jump address
                let jp_addr = u16::from_le_bytes([
                    self.mem[self.pc.post_inc()],
                    self.mem[self.pc.post_inc()],
                ]);

                // Push the current PC to stack
                let cur_pc = self.pc.get().to_le_bytes();
                self.mem[self.sp.pre_dec()] = cur_pc[1];
                self.mem[self.sp.pre_dec()] = cur_pc[0];

                // Update PC
                self.pc.set(jp_addr);
            }
            Op::Pop(param) => {
                let r = match param {
                    ParamR16Stk::AF => &mut self.af,
                    ParamR16Stk::BC => &mut self.bc,
                    ParamR16Stk::DE => &mut self.de,
                    ParamR16Stk::HL => &mut self.hl,
                };
                r.set_lo(self.mem[self.sp.post_inc()]);
                r.set_hi(self.mem[self.sp.post_inc()]);
            }
            Op::Push(param) => {
                let r = match param {
                    ParamR16Stk::AF => &self.af,
                    ParamR16Stk::BC => &self.bc,
                    ParamR16Stk::DE => &self.de,
                    ParamR16Stk::HL => &self.hl,
                };
                self.mem[self.sp.pre_dec()] = r.get_hi();
                self.mem[self.sp.pre_dec()] = r.get_lo();
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
enum ParamR16Stk {
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

#[derive(Debug, Clone, Copy)]
enum ParamCond {
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
    Ret,                      // ret
    CallImm16,                // call imm16
    Pop(ParamR16Stk),         // pop r16stk
    Push(ParamR16Stk),        // push r16stk
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
                0b1100_1101 => Some(Self::CallImm16),
                _ => match b & 0b0000_1111 {
                    0b0001 => Some(Self::Pop(ParamR16Stk::from((b & 0b0011_0000) >> 4))),
                    0b0101 => Some(Self::Push(ParamR16Stk::from((b & 0b0011_0000) >> 4))),
                    _ => None,
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
            Op::Ret => 0b1100_1001,
            Op::CallImm16 => 0b1100_1101,
            Op::Pop(param) => 0b1100_0001 | ((param as u8) << 4),
            Op::Push(param) => 0b1100_0101 | ((param as u8) << 4),
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
            Self::Ret => write!(f, "ret"),
            Self::CallImm16 => write!(f, "call imm16"),
            Self::Pop(param) => write!(f, "pop {}", param),
            Self::Push(param) => write!(f, "push {}", param),
        }
    }
}

fn make_mem() -> Vec<u8> {
    const MEM_SIZE: usize = 256;
    const INIT_OFFSET: u16 = 0;
    const FUNC_1_OFFSET: u16 = 8;
    const FUNC_2_OFFSET: u16 = 16;
    const MAIN_OFFSET: u16 = 48;
    const DATA_OFFSET: u16 = 128;
    const STACK_OFFSET: u16 = MEM_SIZE as u16;

    let mut mem = vec![0u8; MEM_SIZE];

    macro_rules! add_instrc {
        ($offset: expr, $($x:expr),+ $(,)?) => {
            let mut i = $offset;
            $(
                mem[i as usize] = $x.into();
                i += 1;
            )*
            // dbg!(i);
            println!("{}: start = {}, end = {}", stringify!($offset), $offset, i)
        }
    }

    // Init
    // 1. Setup stack pointer
    // 2. Call Main
    // 3. Halt
    add_instrc!(
        INIT_OFFSET,
        // ld sp, STACK_OFFSET
        Op::LdR16Imm16(ParamR16::SP),
        STACK_OFFSET.to_le_bytes()[0],
        STACK_OFFSET.to_le_bytes()[1],
        // call main
        Op::CallImm16,
        MAIN_OFFSET.to_le_bytes()[0],
        MAIN_OFFSET.to_le_bytes()[1],
        //halt
        Op::Nop,
        Op::Halt,
    );

    // Func_1 - clear registers
    add_instrc!(
        FUNC_1_OFFSET,
        Op::XorAR8(ParamR8::A),
        Op::LdR8R8(ParamR8::B, ParamR8::A),
        Op::LdR8R8(ParamR8::C, ParamR8::A),
        Op::LdR8R8(ParamR8::D, ParamR8::A),
        Op::LdR8R8(ParamR8::E, ParamR8::A),
        Op::LdR8R8(ParamR8::H, ParamR8::A),
        Op::LdR8R8(ParamR8::L, ParamR8::A),
        Op::Ret,
    );

    // Func_2 - imm8 arithmetics
    add_instrc!(
        FUNC_2_OFFSET,
        Op::CallImm16,
        FUNC_1_OFFSET.to_le_bytes()[0],
        FUNC_1_OFFSET.to_le_bytes()[1],
        Op::LdR8Imm8(ParamR8::A),
        0xDD,
        Op::AddAImm8,
        0xAA,
        Op::AdcAImm8,
        0x89,
        Op::SbcAImm8,
        0x21,
        Op::AndAImm8,
        0xF0,
        Op::XorAImm8,
        0x07,
        Op::OrAImm8,
        0xF0,
        Op::CpAImm8,
        0x0F,
        Op::Ret,
    );

    // Main
    add_instrc!(
        MAIN_OFFSET,
        // call FUNC_1
        Op::CallImm16,
        FUNC_1_OFFSET.to_le_bytes()[0],
        FUNC_1_OFFSET.to_le_bytes()[1],
        // ld hl INSTRC_END
        Op::LdR16Imm16(ParamR16::HL),
        DATA_OFFSET.to_le_bytes()[0],
        DATA_OFFSET.to_le_bytes()[1],
        // inc hl
        Op::IncR16(ParamR16::HL),
        Op::IncR16(ParamR16::HL),
        // ld a [hl+]
        Op::LdAZR16MemZ(ParamR16Mem::HLI),
        // ld [hl+] a
        Op::LdZR16MemZA(ParamR16Mem::HLI),
        // inc hl
        Op::IncR16(ParamR16::HL),
        Op::IncR16(ParamR16::HL),
        // dec bc
        Op::DecR16(ParamR16::BC),
        Op::DecR16(ParamR16::BC),
        // add hl, bc
        Op::AddHlR16(ParamR16::BC),
        // ld e, $0xEE
        Op::LdR8Imm8(ParamR8::E),
        0xEE,
        // inc e
        Op::IncR8(ParamR8::E),
        Op::IncR8(ParamR8::E),
        // dec e
        Op::DecR8(ParamR8::E),
        // rlca
        Op::Rlca,
        Op::Rlca,
        // rrca
        Op::Rrca,
        Op::Rrca,
        // rla
        Op::Rla,
        // rra
        Op::Rra,
        // add a, $0x23
        Op::AddAImm8,
        0x23,
        // daa
        Op::Daa,
        // sub a, $0x09
        Op::SubAImm8,
        0x09,
        // daa
        Op::Daa,
        // sub a, $0x09
        Op::SubAImm8,
        0x09,
        // daa
        Op::Daa,
        // cpl
        Op::Cpl,
        // scf
        Op::Scf,
        // ccf
        Op::Ccf,
        // jr $0x02
        Op::JrImm8,
        0x2,
        // jr nz $0x2
        Op::JrCcImm8(ParamCond::Nz),
        0x2,
        // jr nc $-0x2
        Op::JrCcImm8(ParamCond::Nc),
        0u8.wrapping_sub(0x4),
        // ld [hl], l
        Op::LdR8R8(ParamR8::ZHLZ, ParamR8::L),
        // add a, l
        Op::AddAR8(ParamR8::L),
        // ld d, $0xF0
        Op::LdR8Imm8(ParamR8::D),
        0xF0,
        // add a, d
        Op::AdcAR8(ParamR8::D),
        // ld d, $0x20
        Op::LdR8Imm8(ParamR8::D),
        0x20,
        // sub a, d
        Op::SubAR8(ParamR8::D),
        // ld d, $0xF5
        Op::LdR8Imm8(ParamR8::D),
        0xF5,
        // sbc a, d
        Op::SbcAR8(ParamR8::D),
        // and a, c
        Op::AndAR8(ParamR8::C),
        // xor a, e
        Op::XorAR8(ParamR8::E),
        // or a, a
        Op::OrAR8(ParamR8::A),
        // ld a, c
        Op::LdR8R8(ParamR8::A, ParamR8::C),
        // cp a, c
        Op::CpAR8(ParamR8::C),
        // push af
        Op::Push(ParamR16Stk::AF),
        // pop bc
        Op::Pop(ParamR16Stk::BC),
        // call func 2
        Op::CallImm16,
        FUNC_2_OFFSET.to_le_bytes()[0],
        FUNC_2_OFFSET.to_le_bytes()[1],
        // ret
        Op::Ret,
    );

    // Set data section values
    add_instrc!(DATA_OFFSET, 0xFF, 0xEE, 0x12);

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
    for (i, &b) in mem.iter().enumerate() {
        print!(
            "0x{:02X}{}",
            b,
            match (i + 1) % 16 == 0 {
                true => '\n',
                false => ' ',
            }
        );
    }
    println!();
}
