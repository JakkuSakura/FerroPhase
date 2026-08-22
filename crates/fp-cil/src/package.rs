use std::path::Path;
use std::sync::Arc;

use fp_core::ast::package::provider::PackageProvider;
use fp_core::ast::package::{PackageId, AstPackage};

fn package_name_for(root: &Path) -> String {
    root.file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("main")
        .to_string()
}

/// CIL text or an assembled `.dll`/`.exe`: always carries the raw bytes as
/// a `PrecompiledArtifact` (`CilBackend`'s passthrough path, both
/// `assemble: false`/`true`, needs the original text/PE bytes verbatim);
/// text input also best-effort lifts to a `PrecompiledLir` for retargeting
/// to any other LIR-consuming backend. Binary PE input has no lift path —
/// matches the previous bespoke pipeline's own "binary -> native
/// transpilation is not implemented yet" limitation, just without a
/// bespoke error message for it (`merged_lir_program` errors naturally
/// instead when nothing retargets it).
pub fn provider_for_path(root: &Path) -> Option<Arc<dyn PackageProvider>> {
    let bytes = std::fs::read(root).ok()?;
    let is_pe = bytes.starts_with(b"MZ");
    let package_id = PackageId::new(package_name_for(root));
    let mut source =
        AstPackage::single_item(package_id.clone(), fp_core::ast::Item::precompiled_artifact(bytes.clone()));
    if !is_pe {
        if let Ok(text) = String::from_utf8(bytes) {
            if let Ok(lir) = crate::parse_cil_program(&text) {
                source.items.push(fp_core::ast::package::PackageItem {
                    module_path: fp_core::ast::path::QualifiedPath::new(Vec::new()),
                    item: fp_core::ast::Item::precompiled_lir(lir),
                });
            }
        }
    }
    Some(Arc::new(fp_core::ast::package::provider::FixedPackageProvider::for_source(
        package_id, source,
    )) as Arc<dyn PackageProvider>)
}
