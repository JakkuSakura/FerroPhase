use crate::common_struct;
use std::fmt::{Display, Formatter};
common_struct! {
    pub struct ExprSelfType {
        pub reference: bool,
        pub mutability: bool,
    }
}
impl Display for ExprSelfType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.reference {
            write!(f, "&")?;
        }
        if self.mutability {
            write!(f, "mut ")?;
        }
        write!(f, "Self")
    }
}
