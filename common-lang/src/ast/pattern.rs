use crate::ast::{Ident, Locator, TypeExpr};
use crate::{common_derives, common_enum};
common_enum! {
    pub enum Pattern {
        Ident(Ident),
        Tuple(PatternTuple),
        TupleStruct(PatternTupleStruct),
        Struct(PatternStruct),
        Structural(PatternStructural),
        Box(PatternBox),
        Variant(PatternVariant),
    }
}
impl Pattern {
    pub fn as_ident(&self) -> Option<&Ident> {
        match self {
            Pattern::Ident(ident) => Some(ident),
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
