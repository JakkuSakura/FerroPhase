use crate::ast::path::InPackagePath;
use crate::lir::PackageId;

/// Addresses one LIR lowering unit — a package plus the module path within
/// it — the same two components the old ad hoc `"lir:{package}:{path}"`
/// string key (`fp_compiler::LirId`) encoded, now a real struct instead of
/// a formatted string. Mirrors `mir::path::MirPath`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LirPath {
    pub package_id: PackageId,
    pub module_path: InPackagePath,
}

impl LirPath {
    pub fn new(package_id: PackageId, module_path: InPackagePath) -> Self {
        Self {
            package_id,
            module_path,
        }
    }
}
