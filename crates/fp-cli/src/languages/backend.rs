/// Generic fallback output extension for a `--target <name>` compile when
/// the user gives no explicit `-o` — every target (built-in or externally
/// registered) is opaque to fp-cli now (see `fp_core::backend::TargetBackend`),
/// so there's no per-target extension to guess; every `--target` compile
/// gets the same generic default.
pub const DEFAULT_TARGET_OUTPUT_EXTENSION: &str = "out";
