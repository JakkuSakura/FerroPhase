use std::path::Path;
use std::sync::Arc;

use fp_core::ast::package::provider::PackageProvider;
use fp_core::ast::package::{AstPackage, PackageId};

fn package_name_for(root: &Path) -> String {
    root.file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("main")
        .to_string()
}

/// A `.class`/`.jar` file carries both a `PrecompiledArtifact` (raw bytes,
/// for byte-identical passthrough back to `--target jvm-bytecode` —
/// `fp_jvm::JvmBackend` checks for it before its normal MIR-based path)
/// and, best-effort, a `PrecompiledLir` (so retargeting to
/// native/goasm/urcl/cil works the same generic way goasm/URCL input
/// already does).
pub fn bytecode_provider(root: &Path) -> Option<Arc<dyn PackageProvider>> {
    let bytes = std::fs::read(root).ok()?;
    let is_jar = bytes.starts_with(b"PK\x03\x04");
    let package_id = PackageId::new(package_name_for(root));
    let mut source = AstPackage::single_item(
        package_id.clone(),
        fp_core::ast::Item::precompiled_artifact(bytes.clone()),
    );
    let lir = if is_jar {
        crate::extract_class_files_from_jar(&bytes)
            .ok()
            .and_then(|classes| {
                let mut merged: Option<fp_core::lir::LirBlob> = None;
                for class in classes {
                    let mut program = crate::parse_class_to_lir(&class.bytes).ok()?;
                    match merged.as_mut() {
                        Some(merged_program)
                            if merged_program.data_layout == program.data_layout =>
                        {
                            merged_program.functions.append(&mut program.functions);
                            merged_program.globals.append(&mut program.globals);
                            merged_program
                                .type_definitions
                                .append(&mut program.type_definitions);
                            merged_program.queries.append(&mut program.queries);
                        }
                        Some(_) => return None,
                        None => merged = Some(program),
                    }
                }
                merged
            })
    } else {
        crate::parse_class_to_lir(&bytes).ok()
    };
    if let Some(lir) = lir {
        source.items.push(fp_core::ast::package::PackageItem {
            module_path: fp_core::ast::path::QualifiedPath::new(Vec::new()),
            item: fp_core::ast::Item::precompiled_lir(lir),
        });
    }
    Some(Arc::new(
        fp_core::ast::package::provider::FixedPackageProvider::for_source(package_id, source),
    ) as Arc<dyn PackageProvider>)
}
