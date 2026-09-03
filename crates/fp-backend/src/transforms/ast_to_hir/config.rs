#[derive(Clone, Debug, Default)]
pub struct HirLoweringConfig {
    pub capabilities: fp_core::capabilities::LanguageCapabilities,
    /// Operation declarations supplied by the active source/provider
    /// packages. Resolution must consult these declarations, never a core
    /// hardcoded operation catalog.
    pub operations: fp_core::lang::LangItemRegistry,
}
