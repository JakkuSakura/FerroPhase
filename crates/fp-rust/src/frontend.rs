use std::path::Path;

use fp_core::error::Result;
use fp_core::frontend::{FrontendParseMode, FrontendResult, LanguageFrontend};
use fp_lang::FerroFrontend;

pub const RUST: &str = "rust";

/// Frontend for real `.rs` source.
///
/// Currently delegates all actual parsing to `FerroFrontend` (`fp-lang`'s
/// Rust-superset parser — the only Rust-capable parser that exists today).
/// This type exists to give Rust-specific parsing its own home to grow into
/// (handling real std's attribute/unsafe/cfg surface, etc. — see
/// `docs/RustStd.md`) without entangling that work with `FerroFrontend`'s
/// own `.fp`-dialect behavior and identity (`language() == "ferrophase"`).
pub struct RustFrontend {
    inner: FerroFrontend,
}

impl RustFrontend {
    pub fn new() -> Self {
        Self {
            inner: FerroFrontend::new(),
        }
    }
}

impl Default for RustFrontend {
    fn default() -> Self {
        Self::new()
    }
}

impl LanguageFrontend for RustFrontend {
    fn language(&self) -> &'static str {
        RUST
    }

    fn extensions(&self) -> &'static [&'static str] {
        &["rs"]
    }

    fn parse_expr(&self, source: &str) -> Result<FrontendResult> {
        self.inner.parse_expr(source)
    }

    fn parse_file(&self, source: &str, path: &Path) -> Result<FrontendResult> {
        self.inner.parse_file(source, path)
    }

    fn parse(&self, source: &str, path: Option<&Path>) -> Result<FrontendResult> {
        self.inner.parse(source, path)
    }

    fn parse_mode(&self) -> FrontendParseMode {
        self.inner.parse_mode()
    }

    fn set_parse_mode(&self, mode: FrontendParseMode) {
        self.inner.set_parse_mode(mode)
    }
}
