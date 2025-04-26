use crate::storage::register::MipsRegisterManager;
use crate::storage::stack::MipsStackFrame;

pub mod location;
pub mod register;
pub mod stack;

pub struct MipsStorage {
    pub frames: Vec<MipsStackFrame>,
}

impl MipsStorage {
    pub fn new() -> Self {
        Self { frames: Vec::new() }
    }
    /// save the registers to the current frame
    /// push a new frame to the stack
    pub fn push_frame(&mut self) -> &mut MipsStackFrame {
        let frame = MipsStackFrame::new();
        self.frames.push(frame);
        self.frames.last_mut().unwrap()
    }
    pub fn pop_frame(&mut self) {
        self.frames.pop();
    }
    pub fn register_manager(&self) -> &MipsRegisterManager {
        &*self.frames.last().unwrap().register
    }
}
