use crate::ast::{Ast, Expr};
use common::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize, Ord, PartialOrd, Eq, PartialEq)]
pub struct LiteralInt {
    pub value: i64,
}

impl LiteralInt {
    pub fn new(i: i64) -> Self {
        Self { value: i }
    }
}

impl Ast for LiteralInt {
    fn is_literal(&self) -> bool {
        true
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Ord, PartialOrd, Eq, PartialEq)]
pub struct LiteralBool {
    pub value: bool,
}

impl LiteralBool {
    pub fn new(i: bool) -> Self {
        Self { value: i }
    }
}

impl Ast for LiteralBool {
    fn is_literal(&self) -> bool {
        true
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialOrd, PartialEq)]
pub struct LiteralDecimal {
    pub value: f64,
}

impl LiteralDecimal {
    pub fn new(v: f64) -> Self {
        Self { value: v }
    }
}

impl Ast for LiteralDecimal {
    fn is_literal(&self) -> bool {
        true
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LiteralChar {
    pub value: char,
}

impl Ast for LiteralChar {
    fn is_literal(&self) -> bool {
        true
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LiteralString {
    pub value: String,
    pub owned: bool,
}

impl LiteralString {
    pub fn new_owned(s: impl Into<String>) -> Self {
        Self {
            value: s.into(),
            owned: true,
        }
    }
    pub fn new_ref(s: impl Into<String>) -> Self {
        Self {
            value: s.into(),
            owned: false,
        }
    }
}

impl Ast for LiteralString {
    fn is_literal(&self) -> bool {
        true
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LiteralList {
    pub value: Vec<Expr>,
}

impl Ast for LiteralList {
    fn is_literal(&self) -> bool {
        true
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LiteralUnknown {}

impl Ast for LiteralUnknown {
    fn is_literal(&self) -> bool {
        true
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LiteralTest {
    pub value: String,
    pub owned: bool,
}

impl LiteralTest {
    pub fn new_owned(s: impl Into<String>) -> Self {
        Self {
            value: s.into(),
            owned: true,
        }
    }
    pub fn new_ref(s: impl Into<String>) -> Self {
        Self {
            value: s.into(),
            owned: false,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LiteralUnit;

impl Ast for LiteralUnit {
    fn is_literal(&self) -> bool {
        true
    }
}
