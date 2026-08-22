use std::path::Path;
use std::sync::Arc;

use fp_core::package::provider::PackageProvider;

use crate::NativeObjectPackageProvider;

fn package_name_for(root: &Path) -> String {
    root.file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("main")
        .to_string()
}

/// A native object file is never a manifest-based multi-file project — the
/// package name is just derived from the file itself, the same way every
/// other single-file provider does.
pub fn object_provider(root: &Path) -> Option<Arc<dyn PackageProvider>> {
    let bytes = std::fs::read(root).ok()?;
    let name = package_name_for(root);
    NativeObjectPackageProvider::new(fp_core::package::PackageId::new(name), &bytes)
        .ok()
        .map(|p| Arc::new(p) as Arc<dyn PackageProvider>)
}

/// A native archive (`.a`/`.lib`) — one package, one item per member
/// (`NativeObjectPackageProvider::from_archive`).
pub fn archive_provider(root: &Path) -> Option<Arc<dyn PackageProvider>> {
    let bytes = std::fs::read(root).ok()?;
    let name = package_name_for(root);
    NativeObjectPackageProvider::from_archive(fp_core::package::PackageId::new(name), &bytes)
        .ok()
        .map(|p| Arc::new(p) as Arc<dyn PackageProvider>)
}

/// Which native asm dialect to parse as — `Auto` tries x86_64 first,
/// falling back to aarch64, matching every other extension-detected
/// language's "just figure it out" default.
pub enum AsmDialect {
    Auto,
    X86_64,
    Aarch64,
}

/// Reads `root` as asm text, parses+lifts it to a target-independent
/// `AsmProgram` (the same `fp_native::asmir` machinery `fp_native::binary::
/// lift_object_to_asmir` uses for binary object files), and wraps it as a
/// one-package provider the same way `NativeObjectPackageProvider::new`
/// does for objects — `NativeEmitter::emit_package_artifact`/`emit_precompiled`
/// then retargets and emits it (as text, an object, or an executable,
/// depending on `BackendConfig`) without knowing or caring that it came
/// from text rather than a binary.
pub fn asm_text_provider(root: &Path, dialect: AsmDialect) -> Option<Arc<dyn PackageProvider>> {
    use crate::asm::{aarch64::AsmAarch64Program, x86_64::AsmX86_64Program};
    use crate::asmir::{lift_from_aarch64, lift_from_x86_64};

    let text = std::fs::read_to_string(root).ok()?;
    let asm = match dialect {
        AsmDialect::X86_64 => lift_from_x86_64(&AsmX86_64Program::parse_text(&text).ok()?).ok()?,
        AsmDialect::Aarch64 => lift_from_aarch64(&AsmAarch64Program::parse_text(&text).ok()?).ok()?,
        AsmDialect::Auto => match AsmX86_64Program::parse_text(&text) {
            Ok(program) => lift_from_x86_64(&program).ok()?,
            Err(_) => lift_from_aarch64(&AsmAarch64Program::parse_text(&text).ok()?).ok()?,
        },
    };
    let name = package_name_for(root);
    Some(Arc::new(NativeObjectPackageProvider::from_asm(
        fp_core::package::PackageId::new(name),
        asm,
    )) as Arc<dyn PackageProvider>)
}
