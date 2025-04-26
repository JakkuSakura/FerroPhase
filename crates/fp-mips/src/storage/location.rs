use crate::storage::register::{MipsRegister, MipsRegisterOwned};

#[derive(Debug)]
pub enum MipsLocation {
    Register(MipsRegister),
    RegisterOwned(MipsRegisterOwned),
    Stack(i16),
    Global(String),
}
