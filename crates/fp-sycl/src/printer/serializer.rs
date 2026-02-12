use fp_core::ast::{AstSerializer, Node};

use super::SyclEmitter;

/// Public entry point used by the CLI target emitter.
pub struct SyclSerializer;

impl AstSerializer for SyclSerializer {
    fn serialize_node(&self, node: &Node) -> fp_core::error::Result<String> {
        let mut emitter = SyclEmitter::new();
        emitter.emit_node(node)?;
        Ok(emitter.finish())
    }
}
