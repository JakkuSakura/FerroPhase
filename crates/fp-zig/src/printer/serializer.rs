use fp_core::ast::{AstSerializer, File};

use super::ZigEmitter;

/// Public entry point used by the CLI target emitter.
pub struct ZigSerializer;

impl AstSerializer for ZigSerializer {
    fn serialize_file(&self, file: &File) -> fp_core::error::Result<String> {
        let mut emitter = ZigEmitter::new();
        emitter.emit_file(file)?;
        Ok(emitter.finish())
    }
}
