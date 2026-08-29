use super::*;

pub(super) fn expr_to_name(expr: &Expr) -> String {
    match expr.kind() {
        ExprKind::Name(name) => name_to_string(name),
        ExprKind::Select(sel) => {
            format!("{}.{}", expr_to_name(&sel.obj), sel.field.name.as_str())
        }
        _ => format!("Any"),
    }
}

pub(super) fn name_to_string(name: &fp_core::ast::Name) -> String {
    use fp_core::ast::Name::*;
    match name {
        Ident(id) => id.name.clone(),
        Path(p) => p
            .segments
            .iter()
            .map(|s| s.name.as_str())
            .collect::<Vec<_>>()
            .join("."),
        ParameterPath(pp) => {
            let base = pp
                .segments
                .iter()
                .map(|s| {
                    let name = s.ident.name.as_str();
                    if s.args.is_empty() {
                        name.to_string()
                    } else {
                        let args = s
                            .args
                            .iter()
                            .map(|ty| KotlinEmitter::new().kotlin_type_from_ty(ty))
                            .collect::<Vec<_>>()
                            .join(", ");
                        format!("{}<{}>", name, args)
                    }
                })
                .collect::<Vec<_>>()
                .join(".");
            base
        }
    }
}

/// If `dot_name` is `wrapper<Inner>` or `some.qualifier.wrapper<Inner>` (a generic
/// wrapper at a dot-segment boundary, possibly qualified by a module path), return
/// `Inner`. Handles qualified paths like `std.io.Result<Unit>`, not just bare
/// `Result<...>`.
pub(super) fn strip_generic_wrapper<'a>(dot_name: &'a str, wrapper: &str) -> Option<&'a str> {
    let pat = format!("{}<", wrapper);
    let idx = dot_name.rfind(&pat)?;
    if idx != 0 && dot_name.as_bytes()[idx - 1] != b'.' {
        return None;
    }
    dot_name[idx + pat.len()..].strip_suffix('>')
}

/// Splits `s` on `sep`, ignoring any `sep` nested inside `<...>`/`(...)` —
/// e.g. `split_top_level("String, Vec<Int>", ',')` → `["String", " Vec<Int>"]`,
/// not a bogus 3-way split on the inner comma.
pub(super) fn split_top_level(s: &str, sep: char) -> Vec<&str> {
    let mut parts = Vec::new();
    let mut depth = 0i32;
    let mut start = 0;
    for (i, c) in s.char_indices() {
        match c {
            '<' | '(' => depth += 1,
            '>' | ')' => depth -= 1,
            c if c == sep && depth == 0 => {
                parts.push(&s[start..i]);
                start = i + c.len_utf8();
            }
            _ => {}
        }
    }
    parts.push(&s[start..]);
    parts
}

pub(super) fn map_name_to_kt(name: &str) -> String {
    // `dyn Trait` — a trait object's type-position name still carries the
    // `dyn` keyword this far (there's no dedicated `Ty` shape for it; it's
    // parsed as a plain type-expression string). A Rust trait is emitted as
    // a Kotlin `interface` of the same name (see `emit_trait`), so `dyn`
    // just needs dropping — the trait name alone is already the right
    // Kotlin type.
    if let Some(inner) = name.strip_prefix("dyn ") {
        return map_name_to_kt(inner);
    }
    // A bare tuple type spelled out as text (`(String, bool)`) — reachable
    // for a trait method's declared return/param type, which (unlike a
    // struct field or a `let`'s inferred type) goes through this
    // string-based path rather than the structured `Ty::Tuple` one. Only
    // top-level commas count as separators — a nested generic's own comma
    // (`(String, Vec<Int>)`) must not split there.
    if let Some(inner) = name.strip_prefix('(').and_then(|s| s.strip_suffix(')')) {
        let parts = split_top_level(inner, ',');
        let mapped: Vec<String> = parts.iter().map(|p| map_name_to_kt(p.trim())).collect();
        return match mapped.len() {
            2 => format!("Pair<{}, {}>", mapped[0], mapped[1]),
            3 => format!("Triple<{}, {}, {}>", mapped[0], mapped[1], mapped[2]),
            _ => "Any".into(),
        };
    }
    // Normalize :: separators to dots for path resolution
    let dot_name = name.replace("::", ".");
    let last_seg = dot_name.rsplit('.').next().unwrap_or(&dot_name);

    if dot_name.starts_with("std.env") {
        return "System".into();
    }

    // Generic wrapper simplifications — match on the last dotted segment before `<`
    // so qualified paths like `std::io::Result<Unit>` also unwrap correctly, not
    // just bare `Result<...>`. Must run before KnownPackage resolution below, or a
    // qualified path like `std::io::Result<()>` gets misclassified as plain `std::io`.
    if let Some(inner) = strip_generic_wrapper(&dot_name, "Vec") {
        return format!("MutableList<{}>", map_name_to_kt(inner));
    }
    // `HashSet<T>`/`HashMap<K, V>` as a type annotation need to agree with
    // `HashSet::new()`/`HashMap::new()`'s constructor mapping (`map_kt_path`,
    // `mutableSetOf`/`mutableMapOf` — which return `MutableSet`/`MutableMap`,
    // not `HashSet`/`HashMap`) or a `let x: HashSet<T> = HashSet::new();`
    // binding is a declared-vs-actual type mismatch.
    if let Some(inner) = strip_generic_wrapper(&dot_name, "HashSet") {
        return format!("MutableSet<{}>", map_name_to_kt(inner));
    }
    if let Some(inner) = strip_generic_wrapper(&dot_name, "HashMap") {
        return format!("MutableMap<{}>", map_name_to_kt(inner));
    }
    if let Some(inner) = strip_generic_wrapper(&dot_name, "Option") {
        return format!("{}?", map_name_to_kt(inner));
    }
    if let Some(inner) = strip_generic_wrapper(&dot_name, "Arc") {
        return map_name_to_kt(inner);
    }
    if let Some(inner) = strip_generic_wrapper(&dot_name, "Box") {
        return map_name_to_kt(inner);
    }
    // `std::fmt::Formatter` — the one parameter type `Display`/`Debug`'s
    // `fmt` method takes, always by `&mut` reference. Modeled directly as
    // Kotlin's `StringBuilder`: `write!(f, ..)` normalizes to `f.append(..)`
    // (see `fp-lang`'s `write`/`writeln` macro handling), which is a real,
    // valid `StringBuilder` method call — no synthetic Formatter type or
    // fmt-specific codegen needed anywhere else.
    if last_seg == "Formatter" {
        return "StringBuilder".into();
    }
    // winnow's `ModalResult<T>` (≈ `Result<T, ContextError>`) — single type
    // argument, unlike `Result`, so just unwrap to T directly.
    if let Some(inner) = strip_generic_wrapper(&dot_name, "ModalResult") {
        return map_name_to_kt(inner);
    }

    // KnownPackage-based resolution (skips language-internal crates)
    match known_package(&dot_name) {
        KnownPackage::StdPath => return kt_type_for_class(KnownClass::Path),
        KnownPackage::StdProcess => return "ProcessBuilder".into(),
        KnownPackage::StdFs => return "Path".into(),
        // "java.io.*" is a glob import, not a valid type — use a concrete class here.
        KnownPackage::StdIo => return "java.io.IOException".into(),
        KnownPackage::StdCollections
        | KnownPackage::StdStr
        | KnownPackage::StdOption
        | KnownPackage::StdSync
        | KnownPackage::Serde
        | KnownPackage::Winnow
        | KnownPackage::ThisError
        | KnownPackage::Tracing
        | KnownPackage::AsyncTrait
        | KnownPackage::Anyhow
        | KnownPackage::Unsupported => return "Any".into(),
        _ => {}
    }

    // KnownClass resolution (portable type descriptors from fp-core)
    if let Some(kc) = KnownClass::from_source_type(last_seg) {
        return kt_type_for_class(kc);
    }

    // Primitive type resolution
    match last_seg {
        "str" | "String" => return "String".into(),
        "char" => return "Char".into(),
        "bool" => return "Boolean".into(),
        "i8" => return "Byte".into(),
        "i16" => return "Short".into(),
        "i32" => return "Int".into(),
        "i64" => return "Long".into(),
        "u8" => return "Int".into(),
        "u16" => return "Int".into(),
        "u32" => return "Long".into(),
        "u64" => return "Long".into(),
        "f32" => return "Float".into(),
        "f64" => return "Double".into(),
        "usize" => return "Long".into(),
        "isize" => return "Long".into(),
        _ => {}
    }

    // A module-qualified name with no other match (a workspace-local
    // struct/enum reference, e.g. `crate::config::GlobalConfig`) — Kotlin
    // has no nested package hierarchy mirroring Rust's module tree (every
    // struct/enum is emitted as a flat top-level class/companion object
    // per generated file), so only the type's own last segment is ever a
    // real Kotlin identifier; earlier module segments would render as
    // literal `::`/`.`-joined garbage (`crate.config.GlobalConfig`, not a
    // resolvable reference) rather than the intended type name. Falls
    // back to the last segment alone for exactly the same reason
    // `is_local_type` in `map_kt_path` already special-cases a
    // *single*-segment PascalCase prefix — this is that same rule
    // extended to a prefix that still carries its module qualification.
    last_seg.to_string()
}

/// Map a KnownClass descriptor to its Kotlin type representation.
pub(super) fn kt_type_for_class(kc: KnownClass) -> String {
    use KnownClass::*;
    match kc {
        Path => "Path".into(),
        Instant => "java.time.Instant".into(),
        Duration => "java.time.Duration".into(),
        LocalDateTime => "java.time.LocalDateTime".into(),
        UtcDateTime => "java.time.ZonedDateTime".into(),
        Date => "java.time.LocalDate".into(),
        IpAddr => "java.net.InetAddress".into(),
        TcpStream => "java.net.Socket".into(),
        TcpListener => "java.net.ServerSocket".into(),
        UdpSocket => "java.net.DatagramSocket".into(),
        FileHandle => "Path".into(),
        IoStream => "java.io.InputStream".into(),
        ChildProcess => "java.lang.Process".into(),
        ExitCode => "Int".into(),
    }
}
