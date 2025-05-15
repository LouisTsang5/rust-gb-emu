use crate::{
    constants::{IE_ADDR, IF_ADDR, INTERRUPT_HANDLER_BASE_ADDR, T_N_INTERRUPT},
    mem::MemoryHandle,
};

mod instr;

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

    fn pre_dec(&mut self) -> u16 {
        let retval = self.get().wrapping_sub(1);
        self.set(retval);
        retval
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

#[derive(Debug)]
pub struct Cpu {
    // Memory
    mem: MemoryHandle,

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

impl Cpu {
    flag_fns! {
        get_zf => set_zf => 7, // Zero Flag 0b1000_0000
        get_nf => set_nf => 6, // Subtraction Flag 0b0100_0000
        get_hf => set_hf => 5, // Half Carry Flag 0b0010_0000
        get_cf => set_cf => 4, // Carry Flag 0b0001_0000
    }

    pub fn reset(&mut self) {
        self.af.set(0x0108);
        self.bc.set(0x0013);
        self.de.set(0x00D8);
        self.hl.set(0x014D);
        self.sp.set(0xFFFE);
        self.pc.set(0x0100);
        self.halted = false;
        self.ime = false;
    }

    pub fn print_reg(&self) {
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

    fn handle_interupt(&mut self) -> (u8, bool) {
        // Check if there is any interrupt need to be handled
        let b_ie = self.mem.read(IE_ADDR);
        let b_if = self.mem.read(IF_ADDR);
        let serviceable_interrupt = b_ie & b_if; // both the corresponding bit in the two bytes are set

        // Define retval
        let mut n_cycle = 0;
        let should_exit_halt = serviceable_interrupt > 0; // Exit halt mode if there is any servicable interrupt

        // Dont handle interrupt if IME is reset or no interrupt to be serviced
        if !self.ime || serviceable_interrupt == 0 {
            return (n_cycle, should_exit_halt);
        }

        for bit_offset in 0..T_N_INTERRUPT {
            // no interrupt to handle
            if serviceable_interrupt & (1 << bit_offset) == 0 {
                continue;
            }

            // Reset IF flag
            self.mem.write(IF_ADDR, b_if & !(1 << bit_offset));

            // Reset IME
            self.ime = false;

            // Push PC
            self.mem.write(self.sp.pre_dec(), self.pc.get_hi());
            self.mem.write(self.sp.pre_dec(), self.pc.get_lo());

            // Jump to handler
            let jp_addr = INTERRUPT_HANDLER_BASE_ADDR + (bit_offset * 8);
            self.pc.set(jp_addr);

            // Set m-cycle
            n_cycle = 5;
            break;
        }

        // Return
        (n_cycle, should_exit_halt)
    }

    pub fn step(&mut self) -> u8 {
        let mut cycles_taken = 0;

        // No opcode execution in halt mode
        if !self.halted {
            // Execute operation
            cycles_taken += self.execute_op();

            // IME flag handling
            match self.ime_pending {
                ImePendingStatus::None => {}
                ImePendingStatus::NextInstr => self.ime_pending = ImePendingStatus::ThisInstr,
                ImePendingStatus::ThisInstr => {
                    self.ime = true;
                    self.ime_pending = ImePendingStatus::None;
                }
            };
        };

        // Handle Interrupt
        let (ct, exit_halt) = self.handle_interupt();
        cycles_taken += ct;

        // Exit halt mode
        if exit_halt {
            self.halted = false;
        }

        // Return total number of cycles taken
        cycles_taken
    }
}

pub fn make(mem: MemoryHandle) -> Cpu {
    let mut cpu = Cpu {
        mem,
        af: Reg16::default(),
        bc: Reg16::default(),
        de: Reg16::default(),
        hl: Reg16::default(),
        sp: Reg16::default(),
        pc: Reg16::default(),
        halted: false,
        ime_pending: ImePendingStatus::default(),
        ime: false,
    };
    cpu.reset();
    cpu
}
