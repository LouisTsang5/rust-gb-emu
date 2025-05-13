use std::{cell::RefCell, rc::Rc};

use crate::{
    constants::{
        DIV_INC_PER_M_CYCLE, IF_ADDR, TAC_CLOCK_SELECT_MASK, TAC_ENABLE_MASK, TIMER_INTERRUPT_MASK,
    },
    mem::MemoryHandle,
};

#[derive(Debug, Default)]
pub struct Timer {
    div: u16,
    tima: u8,
    tma: u8,
    tac: u8,
    mem: Option<MemoryHandle>, // Use option to prevent cyclic dependency
}

impl Timer {
    fn enabled(&self) -> bool {
        self.tac & TAC_ENABLE_MASK > 0
    }

    fn clock_select(&self) -> u8 {
        self.tac & TAC_CLOCK_SELECT_MASK
    }

    // Returns old div counter value
    fn inc_div(&mut self) -> u16 {
        let old_div = self.div;
        self.div = self.div.wrapping_add(DIV_INC_PER_M_CYCLE);
        old_div
    }

    fn update_tima(&mut self, old_div: u16) {
        if !self.enabled() {
            return;
        }

        let new_div = self.div;
        let mask = match self.clock_select() {
            0 => 0x0200,
            1 => 0x0008,
            2 => 0x0020,
            3 => 0x0080,
            _ => unreachable!(),
        };

        // Check for falling edge of masked bit (value 1 -> 0)
        if !(old_div & mask > 0 && new_div & mask == 0) {
            return; // Return if no such event
        }

        // Inc tima
        let (new_tima, of) = self.tima.overflowing_add(1);

        match of {
            false => self.tima = new_tima,
            true => {
                // Reset TIMA to TMA
                self.tima = self.tma;

                // Send Interrupt
                let mem = self.mem.as_ref().unwrap();
                mem.write(IF_ADDR, mem.read(IF_ADDR) | TIMER_INTERRUPT_MASK);
            }
        }
    }

    fn step(&mut self) {
        let old_div = self.inc_div();
        self.update_tima(old_div);
    }

    fn reset_div(&mut self) {
        let old_div = self.div;
        self.div = 0;
        self.update_tima(old_div);
    }
}

#[derive(Debug, Clone)]
pub struct TimerHandle {
    timer: Rc<RefCell<Timer>>,
}

pub fn make() -> TimerHandle {
    TimerHandle {
        timer: Rc::new(RefCell::new(Timer::default())),
    }
}

impl TimerHandle {
    pub fn step(&self) {
        self.timer.borrow_mut().step();
    }

    pub fn attach_mem(&self, mem: MemoryHandle) {
        self.timer.borrow_mut().mem.replace(mem);
    }

    pub fn get_div(&self) -> u8 {
        ((self.timer.borrow().div & 0xFF00) >> 8) as u8 // Return upper byte
    }

    pub fn reset_div(&self) {
        self.timer.borrow_mut().reset_div();
    }

    pub fn get_tima(&self) -> u8 {
        self.timer.borrow().tima
    }

    pub fn set_tima(&self, tima: u8) {
        self.timer.borrow_mut().tima = tima
    }

    pub fn get_tma(&self) -> u8 {
        self.timer.borrow().tma
    }

    pub fn set_tma(&self, tma: u8) {
        self.timer.borrow_mut().tma = tma
    }

    pub fn get_tac(&self) -> u8 {
        self.timer.borrow().tac
    }

    pub fn set_tac(&self, tac: u8) {
        self.timer.borrow_mut().tac = tac
    }

    pub fn print_timer(&self) {
        let timer = self.timer.borrow();
        let Timer {
            div,
            tima,
            tma,
            tac,
            mem: _mem,
        } = &*timer;
        println!(
            "div: 0x{:04x}, tima: 0x{:02x}, tma: 0x{:02x}, tac: 0x{:02x}",
            div, tima, tma, tac
        );
    }
}
