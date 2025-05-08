#[derive(Debug, Default)]
struct Reg16([u8; 2]);
impl Reg16 {
    // // little endian to native u16
    fn get(&self) -> u16 {
        u16::from_be_bytes(self.0)
    }

    fn mut_hi(&mut self) -> &mut u8 {
        &mut self.0[0]
    }

    fn mut_lo(&mut self) -> &mut u8 {
        &mut self.0[1]
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

/// Indicates when should the IME flag be set
#[derive(Debug, Default)]
enum ImePendingStatus {
    /// IME flag is set after THE NEXT instruction
    NextInstr,
    /// IME flag is set after THIS instruction
    ThisInstr,
    #[default]
    /// IME flag remains unchanged
    None,
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

    // Interrupt Master Enable
    ime_pending: ImePendingStatus,
    ime: bool,
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
        self.af.set(0x0108);
        self.bc.set(0x0013);
        self.de.set(0x00D8);
        self.hl.set(0x014D);
        self.sp.set(0xFFFE);
        self.pc.set(0x0100);
        self.halted = false;
        self.ime = false;
    }

    fn print_reg(&self) {
        let Cpu {
            af,
            bc,
            de,
            hl,
            sp,
            pc,
            ime,
            ..
        } = self;
        println!(
            "a: 0x{0:02X} (0b{0:08b}), f: 0x{1:002X} (0b{1:08b}), b: 0x{2:002X}, c: 0x{3:002X}, d: 0x{4:002X}, e: 0x{5:002X}, h: 0x{6:002X}, l: 0x{7:002X}, sp: {8}, pc: {9}, ime: {10}",
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
            ime,
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

    fn step_cb_op(&mut self, op: CbPrefixOp) {
        macro_rules! mut_r8 {
            ($r8: expr) => {
                match $r8 {
                    ParamR8::A => self.af.mut_hi(),
                    ParamR8::B => self.bc.mut_hi(),
                    ParamR8::C => self.bc.mut_lo(),
                    ParamR8::D => self.de.mut_hi(),
                    ParamR8::E => self.de.mut_lo(),
                    ParamR8::H => self.hl.mut_hi(),
                    ParamR8::L => self.hl.mut_lo(),
                    ParamR8::ZHLZ => &mut self.mem[self.hl.as_idx()],
                }
            };
        }

        macro_rules! val_r8 {
            ($r8: expr) => {
                match $r8 {
                    ParamR8::A => self.af.get_hi(),
                    ParamR8::B => self.bc.get_hi(),
                    ParamR8::C => self.bc.get_lo(),
                    ParamR8::D => self.de.get_hi(),
                    ParamR8::E => self.de.get_lo(),
                    ParamR8::H => self.hl.get_hi(),
                    ParamR8::L => self.hl.get_lo(),
                    ParamR8::ZHLZ => self.mem[self.hl.as_idx()],
                }
            };
        }

        match op {
            CbPrefixOp::RlcR8(param) => {
                let r = mut_r8!(param);
                let val = *r;
                let cf = (val & 0x80) > 0;
                let val = val.rotate_left(1);

                *r = val;
                self.set_cf(cf);
                self.set_zf(val == 0);
                self.set_hf(false);
                self.set_nf(false);
            }
            CbPrefixOp::RrcR8(param) => {
                let r = mut_r8!(param);
                let val = *r;
                let cf = (val & 0x01) > 0;
                let val = val.rotate_right(1);

                *r = val;
                self.set_cf(cf);
                self.set_zf(val == 0);
                self.set_hf(false);
                self.set_nf(false);
            }
            CbPrefixOp::RlR8(param) => {
                let o_cf = self.get_cf();
                let r = mut_r8!(param);
                let o_val = *r;

                // after rotation
                let n_val = o_val << 1
                    | match o_cf {
                        true => 1,
                        false => 0,
                    };
                let n_cf = (o_val & 0x80) > 0;

                // set val
                *r = n_val;
                self.set_cf(n_cf);
                self.set_zf(n_val == 0);
                self.set_hf(false);
                self.set_nf(false);
            }
            CbPrefixOp::RrR8(param) => {
                let o_cf = self.get_cf();
                let r = mut_r8!(param);
                let o_val = *r;

                // after rotation
                let n_val = o_val >> 1
                    | match o_cf {
                        true => 0x80,
                        false => 0,
                    };
                let n_cf = (o_val & 0x01) > 0;

                // set val
                *r = n_val;
                self.set_cf(n_cf);
                self.set_zf(n_val == 0);
                self.set_hf(false);
                self.set_nf(false);
            }
            CbPrefixOp::SlaR8(param) => {
                let r = mut_r8!(param);
                let o_val = *r;
                let n_val = o_val << 1;
                *r = n_val;
                self.set_cf(o_val & 0x80 > 0);
                self.set_zf(n_val == 0);
                self.set_hf(false);
                self.set_nf(false);
            }
            CbPrefixOp::SraR8(param) => {
                let r = mut_r8!(param);
                let o_val = *r;
                let n_val = (o_val >> 1) | (o_val & 0x80); // preserve bit 7
                *r = n_val;
                self.set_cf(o_val & 0x01 > 0);
                self.set_zf(n_val == 0);
                self.set_hf(false);
                self.set_nf(false);
            }
            CbPrefixOp::SwapR8(param) => {
                let r = mut_r8!(param);
                let o_val = *r;
                let n_val = ((o_val & 0xF0) >> 4) | ((o_val & 0x0F) << 4);
                *r = n_val;
                self.set_zf(n_val == 0);
                self.set_cf(false);
                self.set_hf(false);
                self.set_nf(false);
            }
            CbPrefixOp::SrlR8(param) => {
                let r = mut_r8!(param);
                let o_val = *r;
                let n_val = o_val >> 1;
                *r = n_val;
                self.set_cf((o_val & 0x01) > 0);
                self.set_zf(n_val == 0);
                self.set_hf(false);
                self.set_nf(false);
            }
            CbPrefixOp::BitB3R8(bit, param) => {
                let val = val_r8!(param);
                self.set_zf(((val >> bit.val()) & 0x01) == 0);
                self.set_hf(true);
                self.set_nf(false);
            }
            CbPrefixOp::ResB3R8(bit, param) => {
                let r = mut_r8!(param);
                let mask = !(0x01u8 << bit.val());
                *r = *r & mask;
            }
            CbPrefixOp::SetB3R8(bit, param) => {
                let r = mut_r8!(param);
                let mask = 0x01u8 << bit.val();
                *r = *r | mask;
            }
        };
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
                self.set_zf(a2 == 0);
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
            Op::RetCond(cond) => {
                let is_jp = match cond {
                    ParamCond::C => self.get_cf(),
                    ParamCond::Nc => !self.get_cf(),
                    ParamCond::Z => self.get_zf(),
                    ParamCond::Nz => !self.get_zf(),
                };
                if is_jp {
                    self.pc.set_lo(self.mem[self.sp.post_inc()]);
                    self.pc.set_hi(self.mem[self.sp.post_inc()]);
                }
            }
            Op::Ret => {
                self.pc.set_lo(self.mem[self.sp.post_inc()]);
                self.pc.set_hi(self.mem[self.sp.post_inc()]);
            }
            Op::Reti => {
                self.ime_pending = ImePendingStatus::ThisInstr;
                self.pc.set_lo(self.mem[self.sp.post_inc()]);
                self.pc.set_hi(self.mem[self.sp.post_inc()]);
            }
            Op::JpCondImm16(cond) => {
                let is_jp = match cond {
                    ParamCond::C => self.get_cf(),
                    ParamCond::Nc => !self.get_cf(),
                    ParamCond::Z => self.get_zf(),
                    ParamCond::Nz => !self.get_zf(),
                };

                let jp_addr = u16::from_le_bytes([
                    self.mem[self.pc.post_inc()],
                    self.mem[self.pc.post_inc()],
                ]);

                if is_jp {
                    self.pc.set(jp_addr);
                }
            }
            Op::JpImm16 => {
                let jp_addr = u16::from_le_bytes([
                    self.mem[self.pc.post_inc()],
                    self.mem[self.pc.post_inc()],
                ]);
                self.pc.set(jp_addr);
            }
            Op::JpHl => {
                self.pc.set(self.hl.get());
            }
            Op::CallCondImm16(cond) => {
                let is_jp = match cond {
                    ParamCond::C => self.get_cf(),
                    ParamCond::Nc => !self.get_cf(),
                    ParamCond::Z => self.get_zf(),
                    ParamCond::Nz => !self.get_zf(),
                };

                // Get the jump address
                let jp_addr = u16::from_le_bytes([
                    self.mem[self.pc.post_inc()],
                    self.mem[self.pc.post_inc()],
                ]);

                if is_jp {
                    // Push the current PC to stack
                    let cur_pc = self.pc.get().to_le_bytes();
                    self.mem[self.sp.pre_dec()] = cur_pc[1];
                    self.mem[self.sp.pre_dec()] = cur_pc[0];

                    // Update PC
                    self.pc.set(jp_addr);
                }
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
            Op::Rst(addr) => {
                // Push the current PC to stack
                let cur_pc = self.pc.get().to_le_bytes();
                self.mem[self.sp.pre_dec()] = cur_pc[1];
                self.mem[self.sp.pre_dec()] = cur_pc[0];

                // Update PC
                self.pc.set(addr.addr());
            }
            Op::Pop(param) => {
                let r = match param {
                    ParamR16Stk::AF => &mut self.af,
                    ParamR16Stk::BC => &mut self.bc,
                    ParamR16Stk::DE => &mut self.de,
                    ParamR16Stk::HL => &mut self.hl,
                };
                if let ParamR16Stk::AF = param {
                    r.set_lo(self.mem[self.sp.post_inc()] & 0xF0); // The lower nibble of F is always zero
                } else {
                    r.set_lo(self.mem[self.sp.post_inc()]);
                }
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
            Op::Prefix => {
                let b = self.mem[self.pc.post_inc()];
                let op = CbPrefixOp::try_from(b)
                    .unwrap_or_else(|_| panic!("Invalid 0xCB prefix op code 0x{:02X}", b));
                println!(
                    "0x{:04x}: {} (0b{:08b})",
                    self.pc.get().wrapping_sub(1),
                    op,
                    u8::from(op),
                );
                self.step_cb_op(op);
            }
            Op::LdhZCZA => {
                let addr = 0xFF00 + self.bc.get_lo() as usize;
                self.mem[addr] = self.af.get_hi();
            }
            Op::LdhZImm8ZA => {
                let addr = 0xFF00 + self.mem[self.pc.post_inc()] as usize;
                self.mem[addr] = self.af.get_hi();
            }
            Op::LdZImm16ZA => {
                let addr = u16::from_le_bytes([
                    self.mem[self.pc.post_inc()],
                    self.mem[self.pc.post_inc()],
                ]) as usize;
                self.mem[addr] = self.af.get_hi();
            }
            Op::LdhAZCZ => {
                let addr = 0xFF00 + self.bc.get_lo() as usize;
                self.af.set_hi(self.mem[addr]);
            }
            Op::LdhAZImm8Z => {
                let addr = 0xFF00 + self.mem[self.pc.post_inc()] as usize;
                self.af.set_hi(self.mem[addr]);
            }
            Op::LdAZImm16Z => {
                let addr = u16::from_le_bytes([
                    self.mem[self.pc.post_inc()],
                    self.mem[self.pc.post_inc()],
                ]) as usize;
                self.af.set_hi(self.mem[addr]);
            }
            Op::AddSpImm8 => {
                let imm8 = self.mem[self.pc.post_inc()];
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
            }
            Op::LdHlSpXImm8 => {
                let imm8 = self.mem[self.pc.post_inc()];
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
            }
            Op::LdSpHl => {
                self.sp.set(self.hl.get());
            }
            Op::Di => {
                self.ime = false;
            }
            Op::Ei => {
                self.ime_pending = ImePendingStatus::NextInstr;
            }
        };

        // IME flag handling
        match self.ime_pending {
            ImePendingStatus::None => {}
            ImePendingStatus::NextInstr => self.ime_pending = ImePendingStatus::ThisInstr,
            ImePendingStatus::ThisInstr => {
                self.ime = true;
                self.ime_pending = ImePendingStatus::None;
            }
        };
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
struct RstAddr {
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

    pub fn from_addr(addr: u16) -> Self {
        if (addr % 8 != 0) || (addr / 8) > 7 {
            panic!("Invalid rst address {:#X}", addr);
        }
        RstAddr {
            idx: (addr / 8) as u8,
        }
    }
}

impl std::fmt::Display for RstAddr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "0x{:02X}", self.addr())
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

#[derive(Debug, Clone, Copy)]
struct ParamB3(u8);

impl ParamB3 {
    fn val(&self) -> u8 {
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
enum CbPrefixOp {
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

const RESULT_VRAM_START: usize = 0x9800;
const RESULT_VRAM_END: usize = 0x9900;

fn main() {
    // Read ROM
    let mut mem = vec![0; u16::MAX as usize + 1];
    let file_name = std::env::args().nth(1).expect("Missing ROM File");
    let mut f = std::fs::File::open(file_name).unwrap();
    std::io::Read::read(&mut f, &mut mem).unwrap();

    // Listen Event
    let (tx, rx) = std::sync::mpsc::channel();
    ctrlc::set_handler(move || tx.send(()).expect("Channel Failed")).unwrap();

    let mut cpu = Cpu::new(&mut mem);
    while !cpu.halted() {
        cpu.step();

        // Check if SIGINT
        if rx.try_recv().is_ok() {
            break;
        }
    }

    // Print result
    cpu.print_reg();
    for (start, end) in [(0, 0xFF + 1), (0xFF00, 0xFF0F + 1)] {
        println!("0x{0:04X}({0}) ... 0x{1:04X}({1})", start, end - 1);
        for (i, &b) in mem[start..end].iter().enumerate() {
            match (i + 1) % 16 == 0 {
                false => print!("0x{:02X} ", b),
                true => println!("0x{:02X} - 0x{:04X}", b, i + start),
            };
        }
        println!();
    }

    println!("VRAM ASCII:");
    for chunk in mem[RESULT_VRAM_START..RESULT_VRAM_END].chunks(16) {
        let s: String = chunk
            .iter()
            .map(|&b| if b >= 32 && b <= 126 { b as char } else { '.' })
            .collect();
        println!("{}", s);
    }
}
