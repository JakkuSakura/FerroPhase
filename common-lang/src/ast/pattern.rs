use crate::ast::Ident;
use crate::common_derives;
common_derives! {
    pub enum Pattern {
        Ident(Ident),
        Tuple(Vec<Pattern>),
        Struct(Vec<(Ident, Pattern)>),
    }
}
// TODO: add patterns for let, if, match, etc.
