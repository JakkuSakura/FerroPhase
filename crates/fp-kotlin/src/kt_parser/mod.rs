//! Declaration-only Kotlin parser (see `docs/KotlinStd.md`). Parses just
//! enough of Kotlin's grammar to extract top-level and class/interface/
//! object-member *signatures* — function/property bodies are skipped, not
//! parsed. Self-contained: no dependency on fp-lang (a different, unrelated
//! language crate); tokenizing is built directly on `winnow` (the same
//! external parser-combinator crate fp-lang uses, depended on independently).
//! Skipped/unparseable declarations are reported through
//! `fp_core::diagnostics` (context `"kt_parser"`), not a bespoke warning type.

mod decl;
mod lexer;

#[derive(rust_embed::RustEmbed)]
#[folder = "std/kotlin/"]
struct KotlinStd;

impl fp_core::embedded_std::SourceBundle for KotlinStd {
    fn paths() -> &'static [&'static str] {
        Box::leak(
            <Self as rust_embed::RustEmbed>::iter()
                .map(|p| Box::leak(p.into_owned().into_boxed_str()) as &'static str)
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        )
    }
    fn get(path: &str) -> Option<std::borrow::Cow<'static, [u8]>> {
        <Self as rust_embed::RustEmbed>::get(path).map(|f| f.data)
    }
}

pub use decl::{KtDecl, KtDeclKind, KtParam, KtParseError, KtType, parse_declarations};

/// Load and parse the vendored Kotlin stdlib declarations.
///
/// This is the canonical stdlib loading path used by Kotlin tooling.  Paths
/// are returned relative to the `kotlin` package root so consumers can build
/// declaration paths without reimplementing filesystem traversal.
pub fn load_std_declarations(
    diagnostics: &fp_core::diagnostics::DiagnosticManager,
) -> Option<Vec<(std::path::PathBuf, Vec<KtDecl>)>> {
    fp_core::embedded_std::load_sources::<KotlinStd>("kt")?
        .into_iter()
        .filter_map(|(relative, source)| {
            parse_declarations(&source, diagnostics)
                .ok()
                .map(|declarations| (relative, declarations))
        })
        .collect::<Vec<_>>()
        .into()
}

#[cfg(test)]
mod tests {
    use super::*;
    use fp_core::diagnostics::DiagnosticManager;

    fn parse_and_count_warnings(src: &str) -> (Vec<KtDecl>, usize) {
        let mgr = DiagnosticManager::new();
        let decls = parse_declarations(src, &mgr).unwrap();
        let warnings = mgr.get_diagnostics().len();
        (decls, warnings)
    }

    #[test]
    fn parses_simple_top_level_function() {
        let (decls, warnings) =
            parse_and_count_warnings("fun add(a: Int, b: Int): Int { return a + b }");
        assert_eq!(warnings, 0);
        assert_eq!(decls.len(), 1);
        let f = &decls[0];
        assert_eq!(f.kind, KtDeclKind::Function);
        assert_eq!(f.name, "add");
        assert_eq!(f.params.len(), 2);
        assert_eq!(f.params[0].name, "a");
        assert_eq!(f.params[0].ty.name, "Int");
        assert_eq!(f.return_type.as_ref().unwrap().name, "Int");
    }

    #[test]
    fn parses_extension_function_with_receiver() {
        let (decls, warnings) =
            parse_and_count_warnings("public inline fun <T> Array<T>.first(): T = this[0]");
        assert_eq!(warnings, 0);
        assert_eq!(decls.len(), 1);
        let f = &decls[0];
        assert_eq!(f.name, "first");
        assert_eq!(f.type_params, vec!["T".to_string()]);
        let recv = f.receiver.as_ref().expect("receiver");
        assert_eq!(recv.name, "Array");
        assert_eq!(recv.args[0].name, "T");
    }

    #[test]
    fn parses_nullable_and_generic_types() {
        let (decls, warnings) =
            parse_and_count_warnings("fun find(): Map<String, List<Int>>? = null");
        assert_eq!(warnings, 0);
        let ret = decls[0].return_type.as_ref().unwrap();
        assert_eq!(ret.name, "Map");
        assert!(ret.nullable);
        assert_eq!(ret.args[0].name, "String");
        assert_eq!(ret.args[1].name, "List");
        assert_eq!(ret.args[1].args[0].name, "Int");
    }

    #[test]
    fn parses_interface_with_members() {
        let src = r#"
            public interface Collection<out E> : Iterable<E> {
                public val size: Int
                public fun isEmpty(): Boolean
                public fun contains(element: @UnsafeVariance E): Boolean
            }
        "#;
        let (decls, warnings) = parse_and_count_warnings(src);
        assert_eq!(warnings, 0);
        assert_eq!(decls.len(), 1);
        let iface = &decls[0];
        assert_eq!(iface.kind, KtDeclKind::Interface);
        assert_eq!(iface.name, "Collection");
        assert_eq!(iface.supertypes[0].name, "Iterable");
        assert_eq!(iface.members.len(), 3);
        assert_eq!(iface.members[0].kind, KtDeclKind::Property);
        assert_eq!(iface.members[1].name, "isEmpty");
    }

    #[test]
    fn parses_data_class_primary_constructor() {
        let (decls, warnings) = parse_and_count_warnings(
            "public data class Pair<out A, out B>(val first: A, val second: B)",
        );
        assert_eq!(warnings, 0);
        let c = &decls[0];
        assert_eq!(c.kind, KtDeclKind::Class);
        assert_eq!(c.name, "Pair");
        assert_eq!(c.params.len(), 2);
        assert_eq!(c.params[0].name, "first");
    }

    #[test]
    fn parses_function_type_param() {
        let (decls, warnings) = parse_and_count_warnings(
            "public inline fun <T, R> T.let(block: (T) -> R): R = block(this)",
        );
        assert_eq!(warnings, 0);
        let f = &decls[0];
        assert_eq!(f.params[0].name, "block");
        assert_eq!(f.params[0].ty.name, "Function");
    }

    #[test]
    fn skips_unparseable_declaration_and_recovers() {
        let src = r#"
            fun good1(): Int = 1
            fun broken(x Int)
            fun good2(): Int = 2
        "#;
        let (decls, warnings) = parse_and_count_warnings(src);
        assert!(warnings > 0);
        let names: Vec<_> = decls.iter().map(|d| d.name.as_str()).collect();
        assert!(names.contains(&"good1"));
        assert!(names.contains(&"good2"));
    }

    #[test]
    fn handles_string_template_with_braces() {
        let src = r#"fun greet(name: String = "hello ${name.uppercase()}!"): String = name"#;
        let (decls, warnings) = parse_and_count_warnings(src);
        assert_eq!(warnings, 0);
        assert_eq!(decls[0].params[0].name, "name");
    }

    #[test]
    fn parses_companion_object_and_enum_class() {
        let src = r#"
            enum class Color {
                RED, GREEN, BLUE;
                companion object {
                    fun default(): Color = RED
                }
            }
        "#;
        let (decls, warnings) = parse_and_count_warnings(src);
        assert_eq!(warnings, 0);
        assert_eq!(decls[0].kind, KtDeclKind::Class);
        assert_eq!(decls[0].name, "Color");
    }

    #[test]
    fn captures_op_class_and_op_method_annotations() {
        let src = r#"
            @Op(class = "Option")
            public class OptionBox<T> {
                @Op(method = "unwrap_or")
                public fun unwrapOr(default: T): T = default
            }
        "#;
        let (decls, warnings) = parse_and_count_warnings(src);
        assert_eq!(warnings, 0);
        assert_eq!(decls[0].op_class.as_deref(), Some("Option"));
        assert_eq!(decls[0].members[0].op_method.as_deref(), Some("unwrap_or"));
    }

    /// Not a pass/fail gate — walks the vendored Kotlin stdlib
    /// (`crates/fp-kotlin/std`, see `docs/KotlinStd.md`) and reports how
    /// many files parse with zero warnings vs. partial/zero declarations
    /// recovered. Run with `-- --nocapture` to see the summary.
    #[test]
    fn measures_vendored_stdlib_parse_coverage() {
        let mgr = DiagnosticManager::new();
        let mut clean = 0usize;
        let mut with_warnings = 0usize;
        let mut hard_errors = 0usize;
        let mut total_decls = 0usize;

        let files = fp_core::embedded_std::load_sources::<KotlinStd>("kt").unwrap_or_default();
        for (_path, src) in &files {
            let before = mgr.snapshot();
            match parse_declarations(src, &mgr) {
                Ok(decls) => {
                    total_decls += decls.len();
                    if mgr.diagnostics_since(before).is_empty() {
                        clean += 1;
                    } else {
                        with_warnings += 1;
                    }
                }
                Err(_) => hard_errors += 1,
            }
        }
        let total_warnings = mgr.get_diagnostics().len();

        eprintln!(
            "kt_parser coverage: {} files ({} clean, {} with warnings, {} hard errors); {} decls parsed, {} warnings",
            files.len(),
            clean,
            with_warnings,
            hard_errors,
            total_decls,
            total_warnings
        );
    }
}
