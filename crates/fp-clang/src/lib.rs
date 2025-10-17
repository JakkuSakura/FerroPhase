//! fp-clang: C/C++ frontend using clang for the FerroPhase compiler
//!
//! This crate provides integration with clang to parse C and C++ source files
//! and convert them to LLVM IR that can be used with the fp-llvm backend.

pub mod ast;
mod clang_ast_lowering;
pub mod codegen;
pub mod error;
pub mod intrinsics;
pub mod parser;

pub use codegen::{ClangCodegen, FunctionSignature};
pub use error::{ClangError, Result};
pub use intrinsics::CRuntimeIntrinsics;
pub use parser::ClangParser;

/// Supported C/C++ standards
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Standard {
    C89,
    C99,
    C11,
    C17,
    C23,
    Cxx98,
    Cxx03,
    Cxx11,
    Cxx14,
    Cxx17,
    Cxx20,
    Cxx23,
}

impl Standard {
    pub fn as_flag(&self) -> &'static str {
        match self {
            Standard::C89 => "-std=c89",
            Standard::C99 => "-std=c99",
            Standard::C11 => "-std=c11",
            Standard::C17 => "-std=c17",
            Standard::C23 => "-std=c23",
            Standard::Cxx98 => "-std=c++98",
            Standard::Cxx03 => "-std=c++03",
            Standard::Cxx11 => "-std=c++11",
            Standard::Cxx14 => "-std=c++14",
            Standard::Cxx17 => "-std=c++17",
            Standard::Cxx20 => "-std=c++20",
            Standard::Cxx23 => "-std=c++23",
        }
    }
}

/// Options for C/C++ compilation
#[derive(Debug, Clone)]
pub struct CompileOptions {
    /// C/C++ standard to use
    pub standard: Option<Standard>,
    /// Additional include directories
    pub include_dirs: Vec<String>,
    /// Additional compiler flags
    pub flags: Vec<String>,
    /// Optimization level (0-3, s, z)
    pub optimization: Option<String>,
    /// Enable debug information
    pub debug: bool,
}

impl Default for CompileOptions {
    fn default() -> Self {
        Self {
            standard: None,
            include_dirs: Vec::new(),
            flags: Vec::new(),
            optimization: None,
            debug: false,
        }
    }
}
