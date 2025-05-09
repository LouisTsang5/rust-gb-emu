const MEM_SIZE: usize = u16::MAX as usize + 1;

enum ReqType {
    Read { addr: u16 },
    Write { addr: u16, val: u8 },
}

struct Req {
    id: u8,
    tp: ReqType,
}

enum Res {
    Value(u8),
    Written,
}

pub struct Memory {
    req_rx: std::sync::mpsc::Receiver<Req>,
    res_txs: Vec<std::sync::mpsc::Sender<Res>>,
    mem: [u8; MEM_SIZE],
}

impl Memory {
    fn run(mut self) {
        for Req { id, tp } in self.req_rx {
            // Get channel by id
            let res_tx = &self.res_txs[id as usize];

            // Handle r/w
            match tp {
                ReqType::Read { addr } => {
                    let val = self.mem[addr as usize];
                    res_tx.send(Res::Value(val)).unwrap();
                }
                ReqType::Write { addr, val } => {
                    self.mem[addr as usize] = val;
                    res_tx.send(Res::Written).unwrap();
                }
            }
        }
    }

    pub fn make(n_handles: usize) -> (std::thread::JoinHandle<()>, Vec<MemoryHandle>) {
        // Make handles
        let mut handles = Vec::with_capacity(n_handles);
        let mut res_txs = Vec::with_capacity(n_handles);
        let (req_tx, req_rx) = std::sync::mpsc::channel::<Req>();
        for id in 0..n_handles {
            // Make res channels
            let (res_tx, res_rx) = std::sync::mpsc::channel::<Res>();

            // Push res receiver to vec
            res_txs.push(res_tx);

            // Make new handle
            let id = id as u8;
            let req_tx = req_tx.clone();
            handles.push(MemoryHandle { id, req_tx, res_rx });
        }

        // Make memory
        let mem = Memory {
            req_rx,
            res_txs,
            mem: [0; MEM_SIZE],
        };

        // Make new thread that runs the memory loop
        let t = std::thread::spawn(move || mem.run());

        // Return handles
        (t, handles)
    }
}

#[derive(Debug)]
pub struct MemoryHandle {
    id: u8,
    req_tx: std::sync::mpsc::Sender<Req>,
    res_rx: std::sync::mpsc::Receiver<Res>,
}

impl MemoryHandle {
    pub fn read(&self, addr: u16) -> u8 {
        // Send request
        self.req_tx
            .send(Req {
                id: self.id,
                tp: ReqType::Read { addr },
            })
            .unwrap();

        // Get Response
        match self.res_rx.recv().unwrap() {
            Res::Written => {
                panic!("Wrong response received from memory (expected Value, received Written).")
            }
            Res::Value(val) => val,
        }
    }

    pub fn write(&self, addr: u16, val: u8) {
        // Send request
        self.req_tx
            .send(Req {
                id: self.id,
                tp: ReqType::Write { addr, val },
            })
            .unwrap();

        // Get Response
        match self.res_rx.recv().unwrap() {
            Res::Value(val) => panic!(
                "Wrong response received from memory (expected Written, received Value({})).",
                val
            ),
            Res::Written => (),
        };
    }
}
