//! Code generation utilities for integrating C/C++ with FerroPhase

use crate::{ClangError, ClangParser, CompileOptions, Result};
use fp_core::lir;
use std::path::Path;
use tracing::{debug, info};

/// Integrates C/C++ compilation with the FerroPhase LLVM backend
pub struct ClangCodegen {
    parser: ClangParser,
}

impl ClangCodegen {
    /// Create a new ClangCodegen instance
    pub fn new() -> Result<Self> {
        Ok(Self {
            parser: ClangParser::new()?,
        })
    }

    /// Compile a C/C++ file and return LLVM IR module
    pub fn compile_file(
        &self,
        source_file: &Path,
        options: &CompileOptions,
    ) -> Result<llvm_ir::Module> {
        info!("Compiling C/C++ file: {}", source_file.display());
        self.parser.parse_to_llvm_ir(source_file, options)
    }

    /// Link multiple C/C++ object files with FerroPhase LIR
    pub fn link_with_lir(
        &self,
        c_files: &[&Path],
        _lir_program: &lir::LirProgram,
        options: &CompileOptions,
    ) -> Result<llvm_ir::Module> {
        debug!("Linking {} C/C++ files with LIR program", c_files.len());

        // Compile all C files to LLVM IR
        let mut modules = Vec::new();
        for file in c_files {
            let module = self.compile_file(file, options)?;
            modules.push(module);
        }

        // TODO: Merge LLVM modules and integrate with LIR
        // For now, return the first module if available
        modules
            .into_iter()
            .next()
            .ok_or_else(|| ClangError::Other("No C files to compile".to_string()))
    }

    /// Extract function declarations from C header files
    pub fn extract_declarations(
        &self,
        header_file: &Path,
        options: &CompileOptions,
    ) -> Result<Vec<FunctionSignature>> {
        debug!("Extracting declarations from: {}", header_file.display());

        // Create a temporary C file that includes the header
        use std::fs;
        let temp_dir = tempfile::tempdir().map_err(|e| ClangError::IoError(e))?;
        let temp_c_file = temp_dir.path().join("wrapper.c");

        let wrapper_content = format!("#include \"{}\"\n", header_file.to_str().unwrap_or(""));
        fs::write(&temp_c_file, wrapper_content)?;

        // Compile wrapper to LLVM IR with header's directory in include path
        let mut wrapper_options = options.clone();
        if let Some(parent) = header_file.parent() {
            wrapper_options
                .include_dirs
                .push(parent.to_str().unwrap_or("").to_string());
        }

        let module = self.compile_file(&temp_c_file, &wrapper_options)?;

        // Extract function signatures (skip compiler-generated functions)
        let mut signatures = Vec::new();
        for func in &module.functions {
            // Skip LLVM intrinsics
            if func.name.starts_with("llvm.") {
                continue;
            }

            signatures.push(FunctionSignature {
                name: func.name.clone(),
                return_type: format_llvm_type(&func.return_type),
                parameters: func
                    .parameters
                    .iter()
                    .map(|p| (p.name.to_string(), format_llvm_type(&p.ty)))
                    .collect(),
                is_variadic: func.is_var_arg,
            });
        }

        Ok(signatures)
    }
}

/// Represents a C/C++ function signature
#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub name: String,
    pub return_type: String,
    pub parameters: Vec<(String, String)>,
    pub is_variadic: bool,
}

impl FunctionSignature {
    /// Convert to a C declaration string
    pub fn to_declaration(&self) -> String {
        let params: Vec<String> = self
            .parameters
            .iter()
            .map(|(name, ty)| format!("{} {}", ty, name))
            .collect();

        let param_str = if self.is_variadic {
            format!("{}, ...", params.join(", "))
        } else {
            params.join(", ")
        };

        format!("{} {}({})", self.return_type, self.name, param_str)
    }
}

/// Format an LLVM type as a C type string (simplified)
fn format_llvm_type(ty: &llvm_ir::Type) -> String {
    match ty {
        llvm_ir::Type::VoidType => "void".to_string(),
        llvm_ir::Type::IntegerType { bits: 1 } => "bool".to_string(),
        llvm_ir::Type::IntegerType { bits: 8 } => "char".to_string(),
        llvm_ir::Type::IntegerType { bits: 16 } => "short".to_string(),
        llvm_ir::Type::IntegerType { bits: 32 } => "int".to_string(),
        llvm_ir::Type::IntegerType { bits: 64 } => "long long".to_string(),
        llvm_ir::Type::IntegerType { bits } => format!("int{}_t", bits),
        llvm_ir::Type::FPType(fp) => match fp {
            llvm_ir::types::FPType::Half => "half".to_string(),
            llvm_ir::types::FPType::Single => "float".to_string(),
            llvm_ir::types::FPType::Double => "double".to_string(),
            llvm_ir::types::FPType::FP128 => "long double".to_string(),
            _ => "float".to_string(),
        },
        llvm_ir::Type::PointerType { .. } => "void*".to_string(),
        llvm_ir::Type::ArrayType { element_type, .. } => {
            format!("{}[]", format_llvm_type(element_type))
        }
        llvm_ir::Type::StructType { .. } => "struct".to_string(),
        _ => "unknown".to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_function_signature_display() {
        let sig = FunctionSignature {
            name: "printf".to_string(),
            return_type: "int".to_string(),
            parameters: vec![("format".to_string(), "char*".to_string())],
            is_variadic: true,
        };

        let decl = sig.to_declaration();
        assert!(decl.contains("printf"));
        assert!(decl.contains("..."));
    }
}
