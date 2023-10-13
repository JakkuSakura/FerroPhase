use crate::ast::{Ident, Locator, TypeExpr};
use crate::value::TypeValue;
use crate::{common_derives, common_enum};
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
            _ => None,
        }
    }
}
// TODO: add patterns for let, if, match, etc.
common_derives! {
    pub struct PatternTuple {
        pub patterns: Vec<Pattern>,
    }
}
common_derives! {
    pub struct PatternTupleStruct {
        pub name: Locator,
        pub patterns: Vec<Pattern>,
    }
}
common_derives! {
    pub struct PatternStruct {
        pub name: Ident,
        pub fields: Vec<PatternStructField>,
    }
}
common_derives! {
    pub struct PatternStructural {
        pub fields: Vec<PatternStructField>,
    }
}
common_derives! {
    pub struct PatternStructField {
        pub name: Ident,
        pub rename: Option<Box<Pattern>>,
    }
}
common_derives! {
    pub struct PatternBox {
        pub pattern: Box<Pattern>,
    }
}
common_derives! {
    pub struct PatternVariant {
        pub name: TypeExpr,
        pub pattern: Option<Box<Pattern>>,
    }

}
common_derives! {
    /// let x: T = expr;
    /// where x: T is PatternType
    pub struct PatternType {
        pub pat: Box<Pattern>,
        pub ty: TypeValue,
    }
}

common_derives! {
    /// pattern like `mut x`
    pub struct PatternIdent {
        pub ident: Ident,
        pub mutability: Option<bool>,
    }
}
common_derives! {
    pub struct PatternWildcard {}
}
