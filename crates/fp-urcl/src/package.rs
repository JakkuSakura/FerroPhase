use std::path::Path;
use std::sync::Arc;

use fp_core::package::provider::PackageProvider;

/// URCL has no project/directory shape at all — always a standalone text
/// file, lifted once at construction into a target-independent
/// `LirProgram` via `fp_core::package::provider::lir_from_text`, the same
/// treatment `fp_goasm::package::file_provider` gives goasm text.
pub fn file_provider(root: &Path) -> Option<Arc<dyn PackageProvider>> {
    fp_core::package::provider::lir_from_text(root, crate::parse_program)
}
