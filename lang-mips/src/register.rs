use std::cell::RefCell;
use std::fmt::{Debug, Display};
use std::rc::Rc;

use strum_macros::{FromRepr, IntoStaticStr};

pub type RegisterId = u8;
#[rustfmt::skip]
#[derive(Debug, Clone, Copy, FromRepr, IntoStaticStr)]
#[strum(serialize_all = "lowercase")]
pub enum MipsRegister {
    Zero,
    At,
    V0, V1,
    A0, A1, A2, A3,
    T0, T1, T2, T3, T4, T5, T6, T7,
    S0, S1, S2, S3, S4, S5, S6, S7,
    T8, T9,
    K0, K1,
    Gp,
    Sp,
    Fp,
    Ra,
}
impl MipsRegister {
    pub fn from_id(id: RegisterId) -> Option<Self> {
        Self::from_repr(id as usize)
    }
    pub fn id(&self) -> RegisterId {
        *self as RegisterId
    }
    pub fn as_str(&self) -> &'static str {
        self.into()
    }
    pub fn is_s(&self) -> bool {
        matches!(
            self,
            Self::S0 | Self::S1 | Self::S2 | Self::S3 | Self::S4 | Self::S5 | Self::S6 | Self::S7
        )
    }
    pub fn is_t(&self) -> bool {
        matches!(
            self,
            Self::T0
                | Self::T1
                | Self::T2
                | Self::T3
                | Self::T4
                | Self::T5
                | Self::T6
                | Self::T7
                | Self::T8
                | Self::T9
        )
    }
}
impl Display for MipsRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "${}", self.as_str())
    }
}
#[derive(Debug)]
pub struct RegisterManager {
    registers_s: RefCell<Vec<MipsRegister>>,
    registers_t: RefCell<Vec<MipsRegister>>,
}
impl RegisterManager {
    pub fn new() -> Self {
        let this = Self {
            registers_s: RefCell::new(vec![
                MipsRegister::S0,
                MipsRegister::S1,
                MipsRegister::S2,
                MipsRegister::S3,
                MipsRegister::S4,
                MipsRegister::S5,
                MipsRegister::S6,
                MipsRegister::S7,
            ]),
            registers_t: RefCell::new(vec![
                MipsRegister::T0,
                MipsRegister::T1,
                MipsRegister::T2,
                MipsRegister::T3,
                MipsRegister::T4,
                MipsRegister::T5,
                MipsRegister::T6,
                MipsRegister::T7,
                MipsRegister::T8,
                MipsRegister::T9,
            ]),
        };
        this.registers_t.borrow_mut().reverse();
        this.registers_s.borrow_mut().reverse();
        this
    }
    fn new_owned(self: &Rc<Self>, register: MipsRegister) -> MipsRegisterOwned {
        MipsRegisterOwned {
            register,
            manager: Some(Rc::clone(self)),
        }
    }
    pub fn acquire_s(self: &Rc<Self>) -> Option<MipsRegisterOwned> {
        let register = self.registers_s.borrow_mut().pop();
        register.map(|register| self.new_owned(register))
    }
    pub fn acquire_t(self: &Rc<Self>) -> Option<MipsRegisterOwned> {
        let register = self.registers_t.borrow_mut().pop();
        register.map(|register| self.new_owned(register))
    }
    pub fn release(&self, register: MipsRegister) {
        if register.is_s() {
            self.registers_s.borrow_mut().push(register);
        } else if register.is_t() {
            self.registers_t.borrow_mut().push(register);
        }
    }
}
#[derive(Debug)]
pub struct MipsRegisterOwned {
    pub register: MipsRegister,
    pub manager: Option<Rc<RegisterManager>>,
}
impl MipsRegisterOwned {
    pub fn zero() -> Self {
        Self {
            register: MipsRegister::Zero,
            manager: None,
        }
    }
}
impl Drop for MipsRegisterOwned {
    fn drop(&mut self) {
        if let Some(manager) = &self.manager {
            manager.release(self.register);
        }
    }
}
impl Display for MipsRegisterOwned {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.register)
    }
}
