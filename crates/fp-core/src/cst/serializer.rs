use super::CstNode;

pub trait CstSerializer {
    fn serialize_cst(&self, node: &CstNode) -> String;
}
