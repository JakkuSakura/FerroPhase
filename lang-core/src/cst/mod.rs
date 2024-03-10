//! At this point we don't need CST yet, can just use span in AST
//! https://matklad.github.io/2023/05/21/resilient-ll-parsing-tutorial.html is worth looking at
//! basic idea:
//! - define TokenKind and TreeKind
//! - Kinds include Error and EOF
//! - use literal name for tokens like Star for '*', instead of Mult
//! - a Tree is a Kind with a list of Children
//! - a Parser that does error recovery at opening of the Tree
//! also refer to https://github.com/rust-lang/rust-analyzer/blob/master/docs/dev/syntax.md
//! Another thing: CST and AST are tree like, so Box<T> should be fine
