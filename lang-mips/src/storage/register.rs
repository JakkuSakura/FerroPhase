use std::cell::Cell;
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
pub struct MipsRegisterManaged {
    pub register: MipsRegister,
    pub borrowed: Cell<bool>,
}
impl MipsRegisterManaged {
    pub fn new(register: MipsRegister) -> Self {
        Self {
            register,
            borrowed: Cell::new(false),
        }
    }
    pub fn borrow(&self) {
        assert!(
            !self.borrowed.get(),
            "register {} is already borrowed",
            self.register
        );
        self.borrowed.set(true);
    }
    pub fn release(&self) {
        self.borrowed.set(false);
    }
    pub fn is_borrowed(&self) -> bool {
        self.borrowed.get()
    }
}
// TODO: abstract to lang-asm
#[derive(Debug)]
pub struct MipsRegisterManager {
    registers_s: Vec<Rc<MipsRegisterManaged>>,
    registers_t: Vec<Rc<MipsRegisterManaged>>,
}
impl MipsRegisterManager {
    pub fn new() -> Self {
        let this = Self {
            registers_s: [
                MipsRegister::S0,
                MipsRegister::S1,
                MipsRegister::S2,
                MipsRegister::S3,
                MipsRegister::S4,
                MipsRegister::S5,
                MipsRegister::S6,
                MipsRegister::S7,
            ]
            .into_iter()
            .map(MipsRegisterManaged::new)
            .map(Rc::new)
            .collect(),
            registers_t: [
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
            ]
            .into_iter()
            .map(MipsRegisterManaged::new)
            .map(Rc::new)
            .collect(),
        };
        this
    }

    pub fn acquire_s(&self) -> Option<MipsRegisterOwned> {
        let register = self.registers_s.iter().find(|r| !r.is_borrowed()).cloned();
        register.map(MipsRegisterOwned::new_managed)
    }
    pub fn acquire_t(&self) -> Option<MipsRegisterOwned> {
        let register = self.registers_t.iter().find(|r| !r.is_borrowed()).cloned();
        register.map(MipsRegisterOwned::new_managed)
    }
    pub fn list_borrowed(&self) -> Vec<MipsRegister> {
        self.registers_s
            .iter()
            .chain(self.registers_t.iter())
            .filter(|r| r.is_borrowed())
            .map(|r| r.register)
            .collect()
    }
}
#[derive(Debug)]
pub enum MipsRegisterOwned {
    Managed(Rc<MipsRegisterManaged>),
    Free(MipsRegister),
}
impl MipsRegisterOwned {
    pub fn zero() -> Self {
        Self::Free(MipsRegister::Zero)
    }
    pub fn new_managed(register: Rc<MipsRegisterManaged>) -> Self {
        register.borrow();
        Self::Managed(register)
    }
    pub fn new_free(register: MipsRegister) -> Self {
        Self::Free(register)
    }
    pub fn get(&self) -> MipsRegister {
        match self {
            Self::Managed(r) => r.register,
            Self::Free(r) => *r,
        }
    }
}

impl Drop for MipsRegisterOwned {
    fn drop(&mut self) {
        if let Self::Managed(r) = self {
            r.release();
        }
    }
}
impl Display for MipsRegisterOwned {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.get())
    }
}
