use crate::ast::package::PackageId;
use crate::ast::path::InPackagePath;

/// Addresses one HIR lowering unit — a package plus the module path within
/// it — the same two components the old ad hoc `"hir:{package}:{path}"`
/// string key (`fp_compiler::HirId`) encoded, now a real struct instead of
/// a formatted string. Mirrors `mir::path::MirPath`/`lir::path::LirPath`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HirPath {
    pub package_id: PackageId,
    pub module_path: InPackagePath,
}

impl HirPath {
    pub fn new(package_id: PackageId, module_path: InPackagePath) -> Self {
        Self {
            package_id,
            module_path,
        }
    }
}
