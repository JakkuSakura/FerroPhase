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
    let mut items = vec![fp_core::ast::Item::precompiled_artifact(bytes.clone())];
    if !is_pe {
        if let Ok(text) = String::from_utf8(bytes) {
            if let Ok(lir) = crate::parse_cil_program(&text) {
                items.push(fp_core::ast::Item::precompiled_lir(lir));
            }
        }
    }
    let source = AstPackage::new(
        package_id.clone(),
        package_id.as_str(),
        fp_core::ast::package::PackageDescriptor::empty(package_id.clone(), package_id.as_str()),
        vec![fp_core::ast::Module {
            attrs: Vec::new(),
            name: fp_core::ast::Ident::new(""),
            items,
            visibility: fp_core::ast::Visibility::Public,
            is_external: false,
        }],
    );
    Some(Arc::new(
        fp_core::ast::package::provider::FixedPackageProvider::for_source(package_id, source),
    ) as Arc<dyn PackageProvider>)
}
