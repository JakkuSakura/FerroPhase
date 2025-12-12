use anyhow::Result;
use fp_core::span::Span;
use llvm_ir::Module;
use std::collections::HashMap;
use std::path::Path;

/// Debug information builder for LLVM modules (simplified version)
pub struct DebugInfoBuilder {
    source_file: String,
    producer: String,
    debug_metadata: HashMap<String, DebugMetadata>,
}

/// Debug metadata for tracking source locations
#[derive(Debug, Clone)]
pub struct DebugMetadata {
    pub file: String,
    pub line: u32,
    pub column: u32,
    pub function: Option<String>,
}

impl DebugInfoBuilder {
    /// Create a new debug info builder
    pub fn new(_module: &Module, source_file: &Path, producer: &str) -> Result<Self> {
        let filename = source_file
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("unknown")
            .to_string();

        Ok(Self {
            source_file: filename,
            producer: producer.to_string(),
            debug_metadata: HashMap::new(),
        })
    }

    /// Finalize debug info generation (placeholder)
    pub fn finalize(&self) {
        // In a full implementation, this would finalize debug metadata
        tracing::debug!(
            "Debug info finalized for {} (produced by {})",
            self.source_file,
            self.producer
        );
    }

    /// Get the source file name
    pub fn source_file(&self) -> &str {
        &self.source_file
    }

    /// Get the producer name
    pub fn producer(&self) -> &str {
        &self.producer
    }

    /// Create debug metadata for a function
    pub fn create_function_debug_info(&mut self, name: &str, span: Span) -> String {
        let metadata = DebugMetadata {
            file: self.source_file.clone(),
            line: span.lo,
            column: span.hi,
            function: Some(name.to_string()),
        };

        let metadata_id = format!("func_{}", name);
        self.debug_metadata.insert(metadata_id.clone(), metadata);
        metadata_id
    }

    /// Create debug metadata for a basic block
    pub fn create_basic_block_debug_info(
        &mut self,
        function: &str,
        block_name: &str,
        span: Span,
    ) -> String {
        let metadata = DebugMetadata {
            file: self.source_file.clone(),
            line: span.lo,
            column: span.hi,
            function: Some(function.to_string()),
        };

        let metadata_id = format!("block_{}_{}", function, block_name);
        self.debug_metadata.insert(metadata_id.clone(), metadata);
        metadata_id
    }

    /// Create debug location for an instruction
    pub fn create_instruction_debug_info(&mut self, instruction_id: &str, span: Span) -> String {
        let metadata = DebugMetadata {
            file: self.source_file.clone(),
            line: span.lo,
            column: span.hi,
            function: None,
        };

        let metadata_id = format!("instr_{}", instruction_id);
        self.debug_metadata.insert(metadata_id.clone(), metadata);
        metadata_id
    }

    /// Get debug metadata by ID
    pub fn get_metadata(&self, id: &str) -> Option<&DebugMetadata> {
        self.debug_metadata.get(id)
    }

    /// List all debug metadata entries
    pub fn list_metadata(&self) -> Vec<(&String, &DebugMetadata)> {
        self.debug_metadata.iter().collect()
    }
}

/// Helper trait to convert spans to debug locations
pub trait SpanToDebugInfo {
    fn to_debug_info(&self, debug_builder: &mut DebugInfoBuilder, context: &str) -> String;
}

impl SpanToDebugInfo for Span {
    fn to_debug_info(&self, debug_builder: &mut DebugInfoBuilder, context: &str) -> String {
        debug_builder.create_instruction_debug_info(context, *self)
    }
}

/// Debug location information
#[derive(Debug, Clone)]
pub struct DebugLocation {
    pub file: String,
    pub line: u32,
    pub column: u32,
}

impl DebugLocation {
    /// Create a new debug location
    pub fn new(file: String, line: u32, column: u32) -> Self {
        Self { file, line, column }
    }

    /// Create debug location from span
    pub fn from_span(span: Span, file: String) -> Self {
        Self {
            file,
            line: span.lo,
            column: span.hi,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    fn empty_module(name: &str) -> Module {
        Module {
            name: name.to_string(),
            source_file_name: String::new(),
            data_layout: llvm_ir::module::DataLayout::default(),
            target_triple: Some("x86_64-unknown-linux-gnu".to_string()),
            functions: Vec::new(),
            global_vars: Vec::new(),
            global_aliases: Vec::new(),
            func_declarations: Vec::new(),
            global_ifuncs: Vec::new(),
            types: llvm_ir::types::Types::blank_for_testing(),
            inline_assembly: String::new(),
        }
    }

    #[test]
    fn test_debug_info_builder_creation() {
        let module = empty_module("test");
        let source_file = PathBuf::from("test.fp");

        let result = DebugInfoBuilder::new(&module, &source_file, "fp-compiler");
        assert!(result.is_ok());

        let debug_builder = result.unwrap();
        assert_eq!(debug_builder.source_file(), "test.fp");
        assert_eq!(debug_builder.producer(), "fp-compiler");
    }

    #[test]
    fn test_function_debug_info_creation() {
        let module = empty_module("test");
        let source_file = PathBuf::from("test.fp");

        let mut debug_builder =
            DebugInfoBuilder::new(&module, &source_file, "fp-compiler").unwrap();

        let span = Span::new(0, 10, 5);
        let metadata_id = debug_builder.create_function_debug_info("test_func", span);

        assert_eq!(metadata_id, "func_test_func");

        let metadata = debug_builder.get_metadata(&metadata_id);
        assert!(metadata.is_some());

        let metadata = metadata.unwrap();
        assert_eq!(metadata.line, 10);
        assert_eq!(metadata.column, 5);
        assert_eq!(metadata.function, Some("test_func".to_string()));
    }

    #[test]
    fn test_span_to_debug_info() {
        let module = empty_module("test");
        let source_file = PathBuf::from("test.fp");

        let mut debug_builder =
            DebugInfoBuilder::new(&module, &source_file, "fp-compiler").unwrap();

        let span = Span::new(0, 15, 8);
        let debug_id = span.to_debug_info(&mut debug_builder, "test_instruction");

        assert_eq!(debug_id, "instr_test_instruction");

        let metadata = debug_builder.get_metadata(&debug_id);
        assert!(metadata.is_some());

        let metadata = metadata.unwrap();
        assert_eq!(metadata.line, 15);
        assert_eq!(metadata.column, 8);
    }

    #[test]
    fn test_debug_location() {
        let span = Span::new(0, 20, 10);
        let location = DebugLocation::from_span(span, "test.fp".to_string());

        assert_eq!(location.file, "test.fp");
        assert_eq!(location.line, 20);
        assert_eq!(location.column, 10);
    }
}
