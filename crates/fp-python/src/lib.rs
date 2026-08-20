//! Python code generation utilities for FerroPhase AST artifacts.

pub mod codegen;
pub mod frontend;
pub mod manifest;
pub mod package;

pub use codegen::{PythonBackend, PythonSerializer};
pub use frontend::PythonFrontend;
pub use manifest::{
    PyProjectManifest, default_module_roots, estimate_module_path, estimate_module_path_with_roots,
    read_pyproject,
};
