use std::path::{Path, PathBuf};
use std::sync::Arc;

use crate::ast::FerroPhaseParser;
mod macro_parser;
pub mod module_path;
mod normalization;
mod serializer;
use crate::macro_parser::FerroMacroExpansionParser;
use crate::normalization::FerroIntrinsicNormalizer;
use fp_core::ast::{AstSerializer, Node};
use fp_core::diagnostics::Diagnostic;
use fp_core::frontend::{FrontendResult, FrontendSnapshot, LanguageFrontend};
use fp_core::intrinsics::IntrinsicNormalizer;
use fp_core::span::FileId;
use fp_core::Result as CoreResult;
pub use serializer::PrettyAstSerializer;

/// Canonical identifier for the FerroPhase source language.
pub const FERROPHASE: &str = "ferrophase";

/// Frontend that parses FerroPhase sources using the existing Rust infrastructure.
pub struct FerroFrontend {
    ferro: FerroPhaseParser,
}

fn register_source(path: PathBuf, source: &str) -> FileId {
    fp_core::source_map::source_map().register_or_update(path, source)
}

impl FerroFrontend {
    pub fn new() -> Self {
        Self {
            ferro: FerroPhaseParser::new(),
        }
    }

    fn clean_source(&self, source: &str) -> String {
        if source.starts_with("#!") {
            source.lines().skip(1).collect::<Vec<_>>().join("\n")
        } else {
            source.to_string()
        }
    }

    fn wrap_statement_like_expr_input<'a>(&self, source: &'a str) -> std::borrow::Cow<'a, str> {
        let trimmed = source.trim_start();
        if trimmed.starts_with("let ") || trimmed.starts_with("defer ") {
            return std::borrow::Cow::Owned(format!("{{ {source} }}"));
        }
        std::borrow::Cow::Borrowed(source)
    }

    fn looks_like_file_input(&self, source: &str) -> bool {
        let trimmed = source.trim_start();
        if trimmed.starts_with("struct {") || trimmed == "struct {}" {
            return false;
        }
        [
            "#[",
            "#![",
            "pub ",
            "fn ",
            "mod ",
            "struct ",
            "enum ",
            "trait ",
            "impl ",
            "impl<",
            "type ",
            "use ",
            "extern ",
            "const ",
            "static ",
            "async fn ",
            "quote fn ",
            "opaque type ",
        ]
        .iter()
        .any(|prefix| trimmed.starts_with(prefix))
    }

    fn setup(
        &self,
    ) -> (
        Arc<dyn AstSerializer>,
        Arc<dyn IntrinsicNormalizer>,
        Arc<FerroMacroExpansionParser>,
    ) {
        (
            Arc::new(PrettyAstSerializer::new()),
            Arc::new(FerroIntrinsicNormalizer::default()),
            Arc::new(FerroMacroExpansionParser::new()),
        )
    }

    fn diagnostic_err(&self, message: String) -> fp_core::error::Error {
        let mut diagnostic = Diagnostic::error(message);
        if let Some(span) = self
            .ferro
            .diagnostics()
            .get_diagnostics()
            .iter()
            .find_map(|diag| diag.span)
        {
            diagnostic = diagnostic.with_span(span);
        }
        fp_core::error::Error::diagnostic(diagnostic)
    }

    fn parse_file_unbacked(&self, source: &str) -> CoreResult<FrontendResult> {
        let cleaned = self.clean_source(source);
        let source_path = PathBuf::from("<file>");
        let file_id = register_source(source_path.clone(), &cleaned);
        let (serializer, intrinsic_normalizer, macro_parser) = self.setup();

        self.ferro.clear_diagnostics();
        let file = self
            .ferro
            .parse_file_ast_with_file(&cleaned, file_id, None, source_path.clone())
            .map_err(|err| self.diagnostic_err(format!("failed to parse file: {err}")))?;
        let diagnostics = self.ferro.diagnostics();
        let last = Node::file(file);
        let mut ast = last.clone();
        fp_core::intrinsics::normalize_intrinsics_with(&mut ast, intrinsic_normalizer.as_ref())
            .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
        Ok(FrontendResult {
            last,
            ast,
            serializer,
            intrinsic_normalizer: Some(intrinsic_normalizer),
            macro_parser: Some(macro_parser),
            snapshot: None,
            diagnostics,
        })
    }
}

impl LanguageFrontend for FerroFrontend {
    fn language(&self) -> &'static str {
        FERROPHASE
    }

    fn extensions(&self) -> &'static [&'static str] {
        &["fp", "ferro", "rs", "rust", "ferrophase"]
    }

    fn parse_expr(&self, source: &str) -> CoreResult<FrontendResult> {
        let cleaned = self.clean_source(source);
        let file_id = register_source(PathBuf::from("<expr>"), &cleaned);
        let (serializer, intrinsic_normalizer, macro_parser) = self.setup();

        self.ferro.clear_diagnostics();
        let expr_source = self.wrap_statement_like_expr_input(&cleaned);
        let expr = self
            .ferro
            .parse_expr_ast_with_file(expr_source.as_ref(), file_id)
            .map_err(|err| self.diagnostic_err(format!("failed to parse expression: {err}")))?;
        let diagnostics = self.ferro.diagnostics();
        let expr = match expr.kind() {
            fp_core::ast::ExprKind::Async(async_expr)
                if matches!(async_expr.expr.kind(), fp_core::ast::ExprKind::Block(_)) =>
            {
                (*async_expr.expr).clone()
            }
            _ => expr,
        };
        let last = Node::expr(expr.clone());
        let mut ast = last.clone();
        fp_core::intrinsics::normalize_intrinsics_with(&mut ast, intrinsic_normalizer.as_ref())
            .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
        Ok(FrontendResult {
            last,
            ast,
            serializer,
            intrinsic_normalizer: Some(intrinsic_normalizer),
            macro_parser: Some(macro_parser),
            snapshot: None,
            diagnostics,
        })
    }

    fn parse_file(&self, source: &str, path: &Path) -> CoreResult<FrontendResult> {
        let cleaned = self.clean_source(source);
        let source_path = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
        let file_id = register_source(source_path.clone(), &cleaned);
        let (serializer, intrinsic_normalizer, macro_parser) = self.setup();

        self.ferro.clear_diagnostics();
        let file = self
            .ferro
            .parse_file_ast_with_file(&cleaned, file_id, Some(&source_path), source_path.clone())
            .map_err(|err| self.diagnostic_err(format!("failed to parse file: {err}")))?;
        let diagnostics = self.ferro.diagnostics();
        let last = Node::file(file);
        let mut ast = last.clone();
        fp_core::intrinsics::normalize_intrinsics_with(&mut ast, intrinsic_normalizer.as_ref())
            .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
        let snapshot = FrontendSnapshot {
            language: self.language().to_string(),
            description: format!("FerroPhase LAST for {}", source_path.display()),
            serialized: None,
        };
        Ok(FrontendResult {
            last,
            ast,
            serializer,
            intrinsic_normalizer: Some(intrinsic_normalizer),
            macro_parser: Some(macro_parser),
            snapshot: Some(snapshot),
            diagnostics,
        })
    }

    fn parse(&self, source: &str, path: Option<&Path>) -> CoreResult<FrontendResult> {
        match path {
            Some(path) => self.parse_file(source, path),
            None if self.looks_like_file_input(source) => self.parse_file_unbacked(source),
            None => self.parse_expr(source),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fp_core::ast::NodeKind;
    use std::fs;
    use std::path::Path;

    fn collect_fp_files(root: &Path, out: &mut Vec<std::path::PathBuf>) {
        let entries = fs::read_dir(root).expect("read_dir");
        for entry in entries {
            let entry = entry.expect("dir entry");
            let path = entry.path();
            if path.is_dir() {
                collect_fp_files(&path, out);
                continue;
            }
            if path.extension().and_then(|ext| ext.to_str()) == Some("fp") {
                out.push(path);
            }
        }
    }

    #[test]
    fn language_identifier_is_ferrophase() {
        let frontend = FerroFrontend::new();
        assert_eq!(frontend.language(), FERROPHASE);
    }

    #[test]
    fn parse_file_mode_handles_extern_c_declarations() {
        let frontend = FerroFrontend::new();
        let dir = std::env::temp_dir().join(format!("fp-lang-test-{}", std::process::id()));
        fs::create_dir_all(&dir).expect("create temp dir");
        let path = dir.join("ffi_parse.fp");
        let source = "extern \"C\" fn strlen(s: &std::ffi::CStr) -> i64;\n\nfn main() {\n    strlen(\"hello\")\n}\n";
        fs::write(&path, source).expect("write temp source");

        let result = frontend.parse(source, Some(&path));
        assert!(result.is_ok(), "unexpected parse error: {:?}", result.err());

        let _ = fs::remove_file(&path);
        let _ = fs::remove_dir(&dir);
    }

    #[test]
    fn parse_embedded_std_sources() {
        let frontend = FerroFrontend::new();
        let mut files = Vec::new();
        collect_fp_files(Path::new("src/std"), &mut files);
        for path in files {
            let source = fs::read_to_string(&path).expect("read std source");
            let result = frontend.parse(&source, Some(&path));
            assert!(
                result.is_ok(),
                "failed to parse {}: {:?}",
                path.display(),
                result.err()
            );
        }
    }

    #[test]
    fn parse_single_std_source_from_env() {
        let Ok(path) = std::env::var("FP_STD_FILE") else {
            return;
        };
        let frontend = FerroFrontend::new();
        let path = Path::new(&path);
        let source = fs::read_to_string(path).expect("read std source");
        let result = frontend.parse(&source, Some(path));
        assert!(
            result.is_ok(),
            "failed to parse {}: {:?}",
            path.display(),
            result.err()
        );
    }

    #[test]
    fn parse_single_std_ast_only_from_env() {
        let Ok(path) = std::env::var("FP_STD_FILE") else {
            return;
        };
        let parser = FerroPhaseParser::new();
        let path = Path::new(&path);
        let source = fs::read_to_string(path).expect("read std source");
        let source_path = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
        let file_id = register_source(source_path.clone(), &source);
        let result = parser.parse_file_ast_with_file(
            &source,
            file_id,
            Some(&source_path),
            source_path.clone(),
        );
        assert!(
            result.is_ok(),
            "failed to parse AST-only {}: {:?}",
            path.display(),
            result.err()
        );
    }

    #[test]
    fn parse_expression_mode_supports_turbofish_method_call() {
        let frontend = FerroFrontend::new();
        let result = frontend.parse("ap.arg::<u64>()", None);
        assert!(result.is_ok(), "unexpected parse error: {:?}", result.err());
    }

    #[test]
    fn query_expression_stays_as_host_ast_until_feature_pass() {
        let frontend = FerroFrontend::new();
        let result = frontend
            .parse(
                "from(ticks).filter(symbol == \"AAPL\").select(value).take(5)",
                None,
            )
            .expect("parse");
        assert!(matches!(result.ast.kind(), NodeKind::Expr(_)));
    }

    #[test]
    fn query_file_without_items_fails_as_file_input() {
        let frontend = FerroFrontend::new();
        let dir = std::env::temp_dir().join(format!("fp-lang-query-pass-{}", std::process::id()));
        fs::create_dir_all(&dir).expect("create temp dir");
        let path = dir.join("query_feature.fp");
        fs::write(
            &path,
            "from(ticks).where(ts >= 10 && ts < 20).select(symbol, value)",
        )
        .expect("write temp source");

        let result = frontend.parse_file(&fs::read_to_string(&path).expect("read"), &path);
        assert!(result.is_err(), "query syntax is not valid file input");

        let _ = fs::remove_file(&path);
        let _ = fs::remove_dir(&dir);
    }

    #[test]
    fn file_backed_expression_without_items_fails() {
        let frontend = FerroFrontend::new();
        let dir = std::env::temp_dir().join(format!("fp-lang-if-expr-{}", std::process::id()));
        fs::create_dir_all(&dir).expect("create temp dir");
        let path = dir.join("if_expr.fp");
        fs::write(&path, "if a > b { a } else { b }").expect("write temp source");

        let result = frontend.parse(&fs::read_to_string(&path).expect("read"), Some(&path));
        assert!(
            result.is_err(),
            "expression-only file is not valid file input"
        );

        let _ = fs::remove_file(&path);
        let _ = fs::remove_dir(&dir);
    }
}

pub mod ast;
pub mod embedded_std;
pub mod lexer;
