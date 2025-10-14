use fp_core::ast::{AstSerializer, Node};

use super::ZigEmitter;

/// Public entry point used by the CLI transpiler.
pub struct ZigSerializer;

impl AstSerializer for ZigSerializer {
    fn serialize_node(&self, node: &Node) -> fp_core::error::Result<String> {
        let mut emitter = ZigEmitter::new();
        emitter.emit_node(node)?;
        Ok(emitter.finish())
    }
}
