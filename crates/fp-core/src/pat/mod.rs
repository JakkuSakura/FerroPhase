use crate::ast::AstExpr;
use crate::ast::AstType;
use crate::id::{Ident, Locator};
use crate::{common_enum, common_struct};
pub type BPattern = Box<Pattern>;
common_enum! {
    pub enum Pattern {
        Ident(PatternIdent),
        Tuple(PatternTuple),
        TupleStruct(PatternTupleStruct),
        Struct(PatternStruct),
        Structural(PatternStructural),
        Box(PatternBox),
        Variant(PatternVariant),
        Type(PatternType),
        Wildcard(PatternWildcard),
    }
}
impl Pattern {
    pub fn as_ident(&self) -> Option<&Ident> {
        match self {
            Pattern::Ident(ident) => Some(&ident.ident),
            Pattern::Type(pattern_type) => pattern_type.pat.as_ident(),
            _ => None,
        }
    }
    pub fn make_mut(&mut self) {
        match self {
            Pattern::Ident(ident) => {
                ident.mutability = Some(true);
            }
            Pattern::Type(PatternType { pat, .. }) => {
                pat.make_mut();
            }
            _ => {}
        }
    }
}
// TODO: add patterns for let, if, match, etc.
common_struct! {
    pub struct PatternTuple {
        pub patterns: Vec<Pattern>,
    }
}
common_struct! {
    pub struct PatternTupleStruct {
        pub name: Locator,
        pub patterns: Vec<Pattern>,
    }
}
common_struct! {
    pub struct PatternStruct {
        pub name: Ident,
        pub fields: Vec<PatternStructField>,
    }
}
common_struct! {
    pub struct PatternStructural {
        pub fields: Vec<PatternStructField>,
    }
}
common_struct! {
    pub struct PatternStructField {
        pub name: Ident,
        pub rename: Option<Box<Pattern>>,
    }
}
common_struct! {
    pub struct PatternBox {
        pub pattern: Box<Pattern>,
    }
}
common_struct! {
    pub struct PatternVariant {
        pub name: AstExpr, // TypeExpr
        pub pattern: Option<Box<Pattern>>,
    }

}
common_struct! {
    /// let x: T = expr;
    /// where x: T is PatternType
    pub struct PatternType {
        pub pat: BPattern,
        pub ty: AstType,
    }
}
impl PatternType {
    pub fn new(pat: Pattern, ty: AstType) -> Self {
        Self {
            pat: pat.into(),
            ty,
        }
    }
}

common_struct! {
    /// pattern like `mut x`
    pub struct PatternIdent {
        pub ident: Ident,
        pub mutability: Option<bool>,
    }
}
impl PatternIdent {
    pub fn new(ident: Ident) -> Self {
        Self {
            ident,
            mutability: None,
        }
    }
}
common_struct! {
    pub struct PatternWildcard {}
}
