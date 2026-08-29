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

    /// See `FerroFrontend::register_file_only`.
    pub fn register_file_only(&self, source: &str, path: &Path) -> fp_core::span::FileId {
        self.inner.register_file_only(source, path)
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

#[cfg(test)]
mod tests {
    use super::*;
    use fp_core::ast::{AttrMeta, ItemKind};
    use fp_core::frontend::LanguageFrontend;

    #[test]
    fn preserves_error_derive_metadata_on_rust_enums() {
        let parsed = RustFrontend::new()
            .parse_file(
                "#[derive(Debug, Error)] pub enum Problem { Broken(String) }",
                Path::new("problem.rs"),
            )
            .expect("parse Rust enum");
        let ItemKind::DefEnum(def) = parsed.ast.items[0].kind() else {
            panic!("expected enum");
        };
        assert!(def.attrs.iter().any(|attr| {
            matches!(
                &attr.meta,
                AttrMeta::List(list)
                    if list.name.last().as_str() == "derive"
                        && list.items.iter().any(|item| {
                            matches!(item, AttrMeta::Path(path) if path.last().as_str() == "Error")
                        })
            )
        }));
    }
}
