use crate::ast::path::QualifiedPath;
use crate::lir::PackageId;

/// Addresses one LIR lowering unit — a package plus the module path within
/// it — the same two components the old ad hoc `"lir:{package}:{path}"`
/// string key (`fp_compiler::LirId`) encoded, now a real struct instead of
/// a formatted string. Mirrors `hir::path::HirPath`/`mir::path::MirPath`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LirPath {
    pub package_id: PackageId,
    pub module_path: QualifiedPath,
}

impl LirPath {
    pub fn new(package_id: PackageId, module_path: QualifiedPath) -> Self {
        Self {
            package_id,
            module_path,
        }
    }
}
