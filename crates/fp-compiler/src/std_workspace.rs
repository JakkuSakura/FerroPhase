use std::rc::Rc;

use fp_core::package::provider::{PackageProvider, ProviderError};
use fp_core::package::PackageId;
use fp_core::workspace::WorkspaceContext;
use fp_typing::{default_extern_prelude, AstTypeInferencer, TypingContext};

/// Load the embedded Ferro standard library (via `provider`) and compile
/// its items into typing tables, producing a fresh `WorkspaceContext`. This
/// is purely mechanical — discovery/parsing is the provider's job (see
/// `fp_core::package::provider::PackageProvider::load_package_items`), kept
/// out of `fp-compiler` so it doesn't need to know about any specific
/// frontend language.
pub fn build_workspace_with_std(
    provider: &dyn PackageProvider,
) -> Result<WorkspaceContext, ProviderError> {
    let mut krate = provider.load_package_items(&PackageId::new("std"))?;

    let env_ctx = Rc::new(WorkspaceContext::new());
    let typing_ctx = Rc::new(TypingContext::new(env_ctx));
    let mut inferencer =
        AstTypeInferencer::new(typing_ctx).with_extern_prelude(default_extern_prelude());

    for (path, items) in &krate.items {
        inferencer.inject_module(path, items);
    }

    let typed = inferencer.into_package_crate("std", krate.graph.clone());
    krate.struct_defs = typed.struct_defs;
    krate.enum_defs = typed.enum_defs;
    krate.function_sigs = typed.function_sigs;
    krate.trait_defs = typed.trait_defs;

    let mut workspace = WorkspaceContext::new();
    workspace.push_crate(krate);
    Ok(workspace)
}
