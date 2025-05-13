use crate::constants::MEM_SIZE;

pub fn make_mem() -> MemoryHandle {
    MemoryHandle {
        mem: std::rc::Rc::new(std::cell::RefCell::new([0; MEM_SIZE])),
    }
}

#[derive(Debug, Clone)]
pub struct MemoryHandle {
    mem: std::rc::Rc<std::cell::RefCell<[u8; MEM_SIZE]>>,
}

impl MemoryHandle {
    pub fn read(&self, addr: u16) -> u8 {
        self.mem.borrow()[addr as usize]
    }

    pub fn write(&self, addr: u16, val: u8) {
        self.mem.borrow_mut()[addr as usize] = val;
    }

    pub fn inner(&self) -> std::cell::Ref<'_, [u8]> {
        self.mem.borrow()
    }
}
