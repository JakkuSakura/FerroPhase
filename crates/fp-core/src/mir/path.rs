use crate::ast::package::PackageId;
use crate::ast::path::InPackagePath;

/// Addresses one MIR lowering unit — a package plus the module path within
/// it — the same two components the old ad hoc `"mir:{package}:{path}"`
/// string key (`fp_compiler::MirId`) encoded, now a real struct instead of
/// a formatted string. Mirrors `lir::path::LirPath`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MirPath {
    pub package_id: PackageId,
    pub module_path: InPackagePath,
}

impl MirPath {
    pub fn new(package_id: PackageId, module_path: InPackagePath) -> Self {
        Self {
            package_id,
            module_path,
        }
    }
}
