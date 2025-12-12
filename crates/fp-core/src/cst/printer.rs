use super::{CstElement, CstNode, CstSerializer, CstTokenKind};

/// Printer for `CstNode` that reconstructs source-like text.
///
/// This is intended for debugging, snapshotting, and tooling.
#[derive(Default)]
pub struct CstPrinter {
    buf: String,
}

impl CstPrinter {
    pub fn print(node: &CstNode) -> String {
        let mut printer = Self::default();
        printer.write(node);
        printer.buf
    }

    pub fn write(&mut self, node: &CstNode) {
        for child in &node.children {
            match child {
                CstElement::Node(inner) => self.write(inner),
                CstElement::Token(tok) => {
                    // Preserve trivia and raw token text; the CST is responsible for spacing.
                    match tok.kind {
                        CstTokenKind::Token
                        | CstTokenKind::TriviaWhitespace
                        | CstTokenKind::TriviaLineComment
                        | CstTokenKind::TriviaBlockComment => {
                            self.buf.push_str(&tok.text);
                        }
                    }
                }
            }
        }
    }
}

impl CstSerializer for CstPrinter {
    fn serialize_cst(&self, node: &CstNode) -> String {
        Self::print(node)
    }
}
