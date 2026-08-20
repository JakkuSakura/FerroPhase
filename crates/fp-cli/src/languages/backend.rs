/// What a given output target can express directly — see
/// `fp_core::capabilities::LanguageCapabilities`. Each target-emitting
/// crate that wants anything other than the conservative default declares
/// its own `CAPABILITIES` const (e.g. `fp_kotlin::CAPABILITIES`); this is
/// the one place that maps a requested target name to the right one.
/// Anything not listed here (including any target whose crate is a
/// disabled optional feature, or any externally-registered target) gets
/// `LanguageCapabilities::NATIVE`.
pub fn capabilities_for_target(name: &str) -> fp_core::capabilities::LanguageCapabilities {
    match name.to_lowercase().as_str() {
        #[cfg(feature = "lang-kotlin")]
        "kotlin" | "kt" => fp_kotlin::CAPABILITIES,
        _ => fp_core::capabilities::LanguageCapabilities::NATIVE,
    }
}

/// Generic fallback output extension for a `--target <name>` compile when
/// the user gives no explicit `-o` — every target (built-in or externally
/// registered) is opaque to fp-cli now (see `fp_core::backend::TargetBackend`),
/// so there's no per-target extension to guess; every `--target` compile
/// gets the same generic default.
pub const DEFAULT_TARGET_OUTPUT_EXTENSION: &str = "out";
