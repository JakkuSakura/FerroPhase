use std::rc::Rc;

use crate::storage::location::MipsLocation;
use crate::storage::register::{MipsRegister, MipsRegisterManager};

pub enum MipsStackSlotState {
    Uninitialized,
    Initialized,
    Droped,
}
pub struct MipsStackSlot {
    pub offset: i16,
    pub size: u32,
    pub state: MipsStackSlotState,
    pub register: Option<MipsRegister>,
}
impl MipsStackSlot {
    pub fn new(offset: i16, size: u32) -> Self {
        Self {
            offset,
            size,
            state: MipsStackSlotState::Uninitialized,
            register: None,
        }
    }

    pub fn set_state(&mut self, state: MipsStackSlotState) {
        self.state = state;
    }
    pub fn get_location(&self) -> MipsLocation {
        MipsLocation::Stack(self.offset)
    }
}
pub struct MipsStackFrame {
    pub stack: Vec<MipsStackSlot>,
    pub register: Rc<MipsRegisterManager>,
}
impl MipsStackFrame {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            register: Rc::new(MipsRegisterManager::new()),
        }
    }
    pub fn offset(&self) -> i16 {
        if let Some(slot) = self.stack.last() {
            slot.offset - slot.size as i16
        } else {
            0
        }
    }
    pub fn push(&mut self, size: u32) -> MipsLocation {
        // stack grows from high address to low address
        let offset = self.offset() - size as i16;
        let slot = MipsStackSlot::new(offset, size);
        self.stack.push(slot);
        MipsLocation::Stack(offset)
    }
    pub fn push_with_register(&mut self, size: u32, register: MipsRegister) -> MipsLocation {
        let offset = self.offset() - size as i16;
        let slot = MipsStackSlot {
            offset,
            size,
            state: MipsStackSlotState::Initialized,
            register: Some(register),
        };
        self.stack.push(slot);
        MipsLocation::Stack(offset)
    }
    pub fn pop(&mut self) -> MipsLocation {
        if let Some(slot) = self.stack.pop() {
            slot.get_location()
        } else {
            panic!("pop from empty stack");
        }
    }
}
