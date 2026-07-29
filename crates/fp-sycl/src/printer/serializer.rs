use fp_core::ast::{AstSerializer, File};

use super::SyclEmitter;

/// Public entry point used by the CLI target emitter.
pub struct SyclSerializer;

impl AstSerializer for SyclSerializer {
    fn serialize_file(&self, file: &File) -> fp_core::error::Result<String> {
        let mut emitter = SyclEmitter::new();
        emitter.emit_file(file)?;
        Ok(emitter.finish())
    }
}
