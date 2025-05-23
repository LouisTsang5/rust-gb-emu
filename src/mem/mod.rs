use std::{
    cell::{Ref, RefCell},
    rc::Rc,
};

use crate::{
    constants::{DIV_ADDR, MEM_SIZE, TAC_ADDR, TIMA_ADDR, TMA_ADDR, VRAM_SIZE, VRAM_START_ADDR},
    timer::TimerHandle,
};

pub fn make(timer: TimerHandle) -> MemoryHandle {
    let mem = Rc::new(RefCell::new(Memory {
        inner: [0; MEM_SIZE],
        timer,
    }));
    MemoryHandle { mem }
}

#[derive(Debug)]
pub struct Memory {
    inner: [u8; MEM_SIZE],
    timer: TimerHandle,
}

#[derive(Debug, Clone)]
pub struct MemoryHandle {
    mem: Rc<RefCell<Memory>>,
}

impl MemoryHandle {
    pub fn read(&self, addr: u16) -> u8 {
        match addr {
            DIV_ADDR => self.mem.borrow().timer.get_div(),
            TIMA_ADDR => self.mem.borrow().timer.get_tima(),
            TMA_ADDR => self.mem.borrow().timer.get_tma(),
            TAC_ADDR => self.mem.borrow().timer.get_tac(),
            _ => self.mem.borrow().inner[addr as usize],
        }
    }

    pub fn write(&self, addr: u16, val: u8) {
        match addr {
            DIV_ADDR => self.mem.borrow_mut().timer.reset_div(), // Writing to DIV resets it
            TIMA_ADDR => self.mem.borrow_mut().timer.set_tima(val),
            TMA_ADDR => self.mem.borrow_mut().timer.set_tma(val),
            TAC_ADDR => self.mem.borrow_mut().timer.set_tac(val),
            _ => self.mem.borrow_mut().inner[addr as usize] = val,
        };
    }

    pub fn vram(&self) -> Ref<[u8]> {
        Ref::map(self.mem.borrow(), |m| {
            &m.inner[VRAM_START_ADDR as usize..(VRAM_START_ADDR + VRAM_SIZE) as usize]
        })
    }
}
