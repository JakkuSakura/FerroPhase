use std::fmt::Display;

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
}
impl Display for MipsRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "${}", self.as_str())
    }
}
