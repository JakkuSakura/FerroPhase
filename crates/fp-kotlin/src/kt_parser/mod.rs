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

pub use decl::{KtDecl, KtDeclKind, KtParam, KtParseError, KtType, parse_declarations};

#[cfg(test)]
mod tests {
    use super::*;
    use fp_core::diagnostics::diagnostic_manager;

    fn parse_and_count_warnings(src: &str) -> (Vec<KtDecl>, usize) {
        let mgr = diagnostic_manager();
        let start = mgr.snapshot();
        let decls = parse_declarations(src).unwrap();
        let warnings = mgr.diagnostics_since(start).len();
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

    /// Not a pass/fail gate — walks the vendored Kotlin stdlib
    /// (`crates/fp-kotlin/std`, see `docs/KotlinStd.md`) and reports how
    /// many files parse with zero warnings vs. partial/zero declarations
    /// recovered. Run with `-- --nocapture` to see the summary.
    #[test]
    fn measures_vendored_stdlib_parse_coverage() {
        let std_root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("std/kotlin");
        if !std_root.exists() {
            eprintln!("skipping: {std_root:?} not present");
            return;
        }
        let mut files = Vec::new();
        collect_kt_files(&std_root, &mut files);
        files.sort();

        let mgr = diagnostic_manager();
        let mut clean = 0usize;
        let mut with_warnings = 0usize;
        let mut hard_errors = 0usize;
        let mut total_decls = 0usize;

        let start_total = mgr.snapshot();
        for path in &files {
            let src = match std::fs::read_to_string(path) {
                Ok(s) => s,
                Err(_) => {
                    hard_errors += 1;
                    continue;
                }
            };
            let before = mgr.snapshot();
            match parse_declarations(&src) {
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
        let total_warnings = mgr.diagnostics_since(start_total).len();

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

    fn collect_kt_files(dir: &std::path::Path, out: &mut Vec<std::path::PathBuf>) {
        let Ok(entries) = std::fs::read_dir(dir) else { return };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                collect_kt_files(&path, out);
            } else if path.extension().and_then(|e| e.to_str()) == Some("kt") {
                out.push(path);
            }
        }
    }
}
