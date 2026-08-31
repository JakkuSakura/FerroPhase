//! Declaration-only Kotlin parser. Parses `fun`/`class`/`interface`/
//! `object`/top-level `val`/`var` signatures; function and property
//! *bodies* are skipped (balanced-brace scan), never parsed. A single
//! declaration that fails to parse is skipped (with its source resynced
//! at the next plausible top-level boundary) rather than failing the
//! whole file — mirrors `fp-rust`'s tolerance for real vendored std source.

use super::lexer::{LexError, Token, TokenKind, tokenize};

#[derive(Debug, Clone, thiserror::Error)]
pub enum KtParseError {
    #[error(transparent)]
    Lex(#[from] LexError),
}

#[derive(Debug, Clone, PartialEq)]
pub struct KtType {
    pub name: String,
    pub args: Vec<KtType>,
    pub nullable: bool,
}

impl KtType {
    fn simple(name: impl Into<String>) -> Self {
        KtType {
            name: name.into(),
            args: Vec::new(),
            nullable: false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct KtParam {
    pub name: String,
    pub ty: KtType,
    pub has_default: bool,
    pub is_vararg: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum KtDeclKind {
    Function,
    Class,
    Interface,
    Object,
    Property,
    TypeAlias,
}

#[derive(Debug, Clone, PartialEq)]
pub struct KtDecl {
    pub kind: KtDeclKind,
    pub name: String,
    pub type_params: Vec<String>,
    pub receiver: Option<KtType>,
    pub params: Vec<KtParam>,
    pub return_type: Option<KtType>,
    pub supertypes: Vec<KtType>,
    pub is_mutable: bool,
    pub members: Vec<KtDecl>,
    /// `@Op(class = "Foo")` on a `class`/`interface`/`object` — the
    /// portable-op lookup key prefix for its tagged members (see
    /// `op_method`). Mirrors the Rust frontend's `#[op(class = "Foo")]` on
    /// an `impl` block (`fp-core/src/lang/mod.rs`) — same framework,
    /// Kotlin's own attribute call syntax (`@Name(args)`, not
    /// `#[name(args)]`).
    pub op_class: Option<String>,
    /// `@Op(method = "bar")` on a `fun` — mirrors `#[op(method = "bar")]`.
    pub op_method: Option<String>,
    /// `@Op(func = "bar")` on a top-level `fun` — mirrors `#[op(func = "bar")]`.
    pub op_func: Option<String>,
}

impl KtDecl {
    fn new(kind: KtDeclKind, name: impl Into<String>) -> Self {
        KtDecl {
            kind,
            name: name.into(),
            type_params: Vec::new(),
            receiver: None,
            params: Vec::new(),
            return_type: None,
            supertypes: Vec::new(),
            is_mutable: false,
            members: Vec::new(),
            op_class: None,
            op_method: None,
            op_func: None,
        }
    }
}

/// Parses declarations, reporting any skipped/unparseable declaration as a
/// warning on `diagnostics` (context `"kt_parser"`) rather than a bespoke
/// return-value list — callers that want a per-file count (e.g. a coverage
/// measurement) can snapshot `diagnostics` before/after the call.
pub fn parse_declarations(
    source: &str,
    diagnostics: &fp_core::diagnostics::DiagnosticManager,
) -> Result<Vec<KtDecl>, KtParseError> {
    let tokens = tokenize(source)?;
    let mut cur = Cursor {
        tokens: &tokens,
        pos: 0,
    };
    Ok(parse_body(&mut cur, /* top_level */ true, diagnostics))
}

const MODIFIER_KEYWORDS: &[&str] = &[
    "public",
    "private",
    "protected",
    "internal",
    "open",
    "final",
    "abstract",
    "sealed",
    "data",
    "inline",
    "noinline",
    "crossinline",
    "tailrec",
    "operator",
    "infix",
    "external",
    "const",
    "lateinit",
    "actual",
    "expect",
    "annotation",
    "inner",
    "override",
    "suspend",
];

struct Cursor<'a> {
    tokens: &'a [Token],
    pos: usize,
}

impl<'a> Cursor<'a> {
    fn peek(&self) -> Option<&str> {
        self.tokens.get(self.pos).map(|t| t.text.as_str())
    }

    fn peek_at(&self, offset: usize) -> Option<&str> {
        self.tokens.get(self.pos + offset).map(|t| t.text.as_str())
    }

    fn peek_kind(&self) -> Option<&TokenKind> {
        self.tokens.get(self.pos).map(|t| &t.kind)
    }

    fn bump(&mut self) -> Option<&Token> {
        let t = self.tokens.get(self.pos);
        if t.is_some() {
            self.pos += 1;
        }
        t
    }

    fn eat(&mut self, text: &str) -> bool {
        if self.peek() == Some(text) {
            self.pos += 1;
            true
        } else {
            false
        }
    }

    fn expect(&mut self, text: &str) -> Result<(), String> {
        if self.eat(text) {
            Ok(())
        } else {
            Err(format!("expected `{text}`, found {:?}", self.peek()))
        }
    }

    fn expect_ident(&mut self) -> Result<String, String> {
        match self.peek_kind() {
            Some(TokenKind::Ident) => {
                let text = self.peek().unwrap().to_string();
                self.pos += 1;
                Ok(text)
            }
            other => Err(format!("expected identifier, found {other:?}")),
        }
    }
}

/// Parses declarations until end of input (top level) or a matching `}`
/// (nested member list) — the closing brace itself is consumed by the caller.
fn parse_body(
    cur: &mut Cursor,
    top_level: bool,
    diagnostics: &fp_core::diagnostics::DiagnosticManager,
) -> Vec<KtDecl> {
    let mut decls = Vec::new();
    loop {
        match cur.peek() {
            None => break,
            Some("}") if !top_level => break,
            Some(";") => {
                cur.bump();
            }
            Some("package") | Some("import") => {
                skip_to_next_boundary(cur);
            }
            _ => {
                let start = cur.pos;
                match parse_one_declaration(cur, diagnostics) {
                    Ok(Some(decl)) => decls.push(decl),
                    Ok(None) => {}
                    Err(err) => {
                        diagnostics.add_diagnostic(
                            fp_core::diagnostics::Diagnostic::warning(err)
                                .with_source_context("kt_parser"),
                        );
                        if cur.pos == start {
                            cur.bump();
                        }
                        resync(cur, top_level);
                    }
                }
            }
        }
    }
    decls
}

/// Best-effort recovery: skip forward to the next token that plausibly
/// starts a new top-level declaration, or to a `}` that closes the current
/// nesting level.
fn resync(cur: &mut Cursor, top_level: bool) {
    let mut depth = 0i32;
    loop {
        match cur.peek() {
            None => return,
            Some("}") if !top_level && depth == 0 => return,
            Some("{") => {
                depth += 1;
                cur.bump();
            }
            Some("}") => {
                depth -= 1;
                cur.bump();
            }
            Some(kw) if depth == 0 && is_decl_start_keyword(kw) => return,
            _ => {
                cur.bump();
            }
        }
    }
}

fn skip_to_next_boundary(cur: &mut Cursor) {
    while let Some(t) = cur.peek() {
        if t == ";" {
            cur.bump();
            return;
        }
        if is_decl_start_keyword(t) {
            return;
        }
        if t == "@" {
            return;
        }
        cur.bump();
    }
}

fn is_decl_start_keyword(kw: &str) -> bool {
    matches!(
        kw,
        "fun" | "class" | "interface" | "object" | "val" | "var" | "typealias"
    ) || MODIFIER_KEYWORDS.contains(&kw)
        || matches!(kw, "enum" | "companion" | "value" | "@")
}

/// Captured state from `skip_annotations_and_modifiers`.
#[derive(Default)]
struct Modifiers {
    is_enum: bool,
    /// `@Op(class = "Foo")` — mirrors the Rust frontend's
    /// `#[op(class = "Foo")]` on an `impl` block (`fp-core/src/lang/
    /// mod.rs`); same portable-op framework, Kotlin's own `@Name(args)`
    /// annotation call syntax instead of `#[name(args)]`.
    op_class: Option<String>,
    /// `@Op(method = "bar")` — mirrors `#[op(method = "bar")]`.
    op_method: Option<String>,
    /// `@Op(func = "bar")` — mirrors `#[op(func = "bar")]`.
    op_func: Option<String>,
}

/// Skips annotations/modifiers, capturing `enum class` and the single
/// `@Op(class = "...", method = "...", func = "...")` portable-op marker
/// along the way (needed by `parse_class_like`/`parse_fun_decl` — see
/// `Modifiers`).
fn skip_annotations_and_modifiers(cur: &mut Cursor) -> Modifiers {
    let mut mods = Modifiers::default();
    loop {
        if cur.peek() == Some("@") {
            cur.bump();
            // Optional use-site target: `get:`/`field:`/`file:`/...
            if matches!(cur.peek_kind(), Some(TokenKind::Ident)) && cur.peek_at(1) == Some(":") {
                cur.bump();
                cur.bump();
            }
            // Qualified annotation name.
            let name = cur.peek().map(|s| s.to_string());
            let _ = cur.expect_ident();
            while cur.eat(".") {
                let _ = cur.expect_ident();
            }
            if cur.peek() == Some("(") {
                let start = cur.pos;
                let args = if name.as_deref() == Some("Op") {
                    parse_op_annotation_args(cur)
                } else {
                    None
                };
                cur.pos = start;
                skip_balanced(cur, "(", ")");
                if let Some((class, method, func)) = args {
                    mods.op_class = mods.op_class.or(class);
                    mods.op_method = mods.op_method.or(method);
                    mods.op_func = mods.op_func.or(func);
                }
            }
            continue;
        }
        match cur.peek() {
            Some("fun") if cur.peek_at(1) == Some("interface") => {
                cur.bump();
            }
            Some("enum") if cur.peek_at(1) == Some("class") => {
                cur.bump();
                mods.is_enum = true;
            }
            Some("companion") if cur.peek_at(1) == Some("object") => {
                cur.bump();
            }
            Some("value") if cur.peek_at(1) == Some("class") => {
                cur.bump();
            }
            Some(kw) if MODIFIER_KEYWORDS.contains(&kw) => {
                cur.bump();
            }
            _ => break,
        }
    }
    mods
}

/// Parses `@Op(class = "Foo", method = "bar", func = "baz")`'s parenthesized
/// argument list (cursor positioned at the opening `(`) into its three
/// possible named values — mirrors the Rust frontend's `#[op(class = "Foo",
/// method = "bar", func = "baz")]` (`fp-core/src/lang/mod.rs`).
fn parse_op_annotation_args(
    cur: &mut Cursor,
) -> Option<(Option<String>, Option<String>, Option<String>)> {
    if !cur.eat("(") {
        return None;
    }
    let (mut class, mut method, mut func) = (None, None, None);
    if cur.peek() != Some(")") {
        loop {
            let key = cur.peek().map(|s| s.to_string())?;
            if !matches!(cur.peek_kind(), Some(TokenKind::Ident)) {
                return None;
            }
            cur.bump();
            if !cur.eat("=") {
                return None;
            }
            let value = if matches!(cur.peek_kind(), Some(TokenKind::StringLiteral)) {
                let v = cur.peek().and_then(string_literal_content);
                cur.bump();
                v
            } else {
                None
            };
            match key.as_str() {
                "class" => class = value,
                "method" => method = value,
                "func" => func = value,
                _ => {}
            }
            if cur.eat(",") {
                continue;
            }
            break;
        }
    }
    let _ = cur.eat(")");
    Some((class, method, func))
}

/// Strips a lexed string-literal token's surrounding quotes (single- or
/// triple-quoted) — good enough for a bare op-tag name, which never
/// contains escapes in practice.
fn string_literal_content(text: &str) -> Option<String> {
    let inner = text
        .strip_prefix("\"\"\"")
        .and_then(|s| s.strip_suffix("\"\"\""))
        .or_else(|| text.strip_prefix('"').and_then(|s| s.strip_suffix('"')));
    inner.map(|s| s.to_string())
}

fn skip_balanced(cur: &mut Cursor, open: &str, close: &str) {
    if !cur.eat(open) {
        return;
    }
    let mut depth = 1i32;
    while depth > 0 {
        match cur.peek() {
            None => return,
            Some(t) if t == open => {
                depth += 1;
                cur.bump();
            }
            Some(t) if t == close => {
                depth -= 1;
                cur.bump();
            }
            _ => {
                cur.bump();
            }
        }
    }
}

fn parse_one_declaration(
    cur: &mut Cursor,
    diagnostics: &fp_core::diagnostics::DiagnosticManager,
) -> Result<Option<KtDecl>, String> {
    let mods = skip_annotations_and_modifiers(cur);
    let decl = match cur.peek() {
        Some("fun") => Some(parse_fun_decl(cur)?),
        Some("class") => Some(parse_class_like(
            cur,
            KtDeclKind::Class,
            mods.is_enum,
            diagnostics,
        )?),
        Some("interface") => Some(parse_class_like(
            cur,
            KtDeclKind::Interface,
            false,
            diagnostics,
        )?),
        Some("object") => Some(parse_class_like(
            cur,
            KtDeclKind::Object,
            false,
            diagnostics,
        )?),
        Some("val") | Some("var") => Some(parse_property_decl(cur)?),
        Some("typealias") => Some(parse_typealias_decl(cur)?),
        _ => None,
    };
    if let Some(mut decl) = decl {
        decl.op_class = mods.op_class;
        decl.op_method = mods.op_method;
        decl.op_func = mods.op_func;
        return Ok(Some(decl));
    }
    match cur.peek() {
        Some("{") => {
            // Anonymous init block or similar — skip its body wholesale.
            skip_balanced(cur, "{", "}");
            Ok(None)
        }
        Some(other) => Err(format!("unrecognized declaration start `{other}`")),
        None => Ok(None),
    }
}

fn parse_type_params(cur: &mut Cursor) -> Result<Vec<String>, String> {
    let mut names = Vec::new();
    if cur.eat("<") {
        loop {
            // Variance/reified modifiers.
            while matches!(cur.peek(), Some("in") | Some("out") | Some("reified")) {
                cur.bump();
            }
            names.push(cur.expect_ident()?);
            if cur.eat(":") {
                let _ = parse_type(cur)?;
            }
            if cur.eat(",") {
                continue;
            }
            break;
        }
        cur.expect(">")?;
    }
    Ok(names)
}

fn parse_type(cur: &mut Cursor) -> Result<KtType, String> {
    // Type-position annotations, e.g. `@UnsafeVariance E`.
    while cur.peek() == Some("@") {
        cur.bump();
        let _ = cur.expect_ident();
        while cur.eat(".") {
            let _ = cur.expect_ident();
        }
        if cur.peek() == Some("(") {
            skip_balanced(cur, "(", ")");
        }
    }
    if cur.peek() == Some("suspend") {
        cur.bump();
    }
    if cur.peek() == Some("(") {
        let start = cur.pos;
        if let Ok(param_types) = try_parse_function_type_params(cur) {
            if cur.eat("->") {
                let ret = parse_type(cur)?;
                let mut ty = KtType::simple("Function");
                ty.args = param_types
                    .into_iter()
                    .chain(std::iter::once(ret))
                    .collect();
                return Ok(ty);
            }
        }
        cur.pos = start;
        cur.expect("(")?;
        let inner = parse_type(cur)?;
        cur.expect(")")?;
        return Ok(inner);
    }

    let mut name = cur.expect_ident()?;
    // Only fold a `.Segment` into a qualified type name (e.g. `Map.Entry`)
    // when the segment looks like a type (PascalCase, by convention) — a
    // lowercase segment (e.g. `T.let`) is an extension-receiver dot, which
    // the caller (parse_fun_decl/parse_property_decl) must see intact.
    while cur.peek() == Some(".")
        && cur
            .peek_at(1)
            .and_then(|s| s.chars().next())
            .is_some_and(|c| c.is_uppercase())
    {
        cur.bump();
        let next = cur.expect_ident()?;
        name.push('.');
        name.push_str(&next);
    }
    let mut args = Vec::new();
    if cur.eat("<") {
        loop {
            if cur.peek() == Some("*") {
                cur.bump();
                args.push(KtType::simple("*"));
            } else {
                while matches!(cur.peek(), Some("in") | Some("out")) {
                    cur.bump();
                }
                args.push(parse_type(cur)?);
            }
            if cur.eat(",") {
                continue;
            }
            break;
        }
        cur.expect(">")?;
    }
    let nullable = cur.eat("?");

    // Extension-function-type receiver shape: `Receiver.(Args) -> Ret`.
    if cur.peek() == Some(".") && cur.peek_at(1) == Some("(") {
        cur.bump();
        if let Ok(param_types) = try_parse_function_type_params(cur) {
            if cur.eat("->") {
                let ret = parse_type(cur)?;
                let mut ty = KtType::simple("Function");
                ty.args = param_types
                    .into_iter()
                    .chain(std::iter::once(ret))
                    .collect();
                return Ok(ty);
            }
        }
        return Err("malformed extension function type".to_string());
    }

    Ok(KtType {
        name,
        args,
        nullable,
    })
}

fn try_parse_function_type_params(cur: &mut Cursor) -> Result<Vec<KtType>, String> {
    cur.expect("(")?;
    let mut types = Vec::new();
    if cur.peek() != Some(")") {
        loop {
            // Optional parameter name label: `name: Type`.
            if matches!(cur.peek_kind(), Some(TokenKind::Ident)) && cur.peek_at(1) == Some(":") {
                cur.bump();
                cur.bump();
            }
            types.push(parse_type(cur)?);
            if cur.eat(",") {
                continue;
            }
            break;
        }
    }
    cur.expect(")")?;
    Ok(types)
}

fn parse_params(cur: &mut Cursor) -> Result<Vec<KtParam>, String> {
    cur.expect("(")?;
    let mut params = Vec::new();
    if cur.peek() != Some(")") {
        loop {
            skip_annotations_and_modifiers(cur);
            // Primary-constructor property markers.
            let _ = cur.eat("val") || cur.eat("var");
            let is_vararg = cur.eat("vararg");
            let name = cur.expect_ident()?;
            cur.expect(":")?;
            let ty = parse_type(cur)?;
            let has_default = if cur.eat("=") {
                skip_expression_until_comma_or_close(cur);
                true
            } else {
                false
            };
            params.push(KtParam {
                name,
                ty,
                has_default,
                is_vararg,
            });
            if cur.eat(",") {
                continue;
            }
            break;
        }
    }
    cur.expect(")")?;
    Ok(params)
}

/// Skips a default-value expression up to (but not including) the next
/// top-level `,` or the closing `)` of the parameter list — tracking
/// bracket/paren/brace depth so a nested call's own commas aren't mistaken
/// for parameter separators.
fn skip_expression_until_comma_or_close(cur: &mut Cursor) {
    let mut depth = 0i32;
    loop {
        match cur.peek() {
            None => return,
            Some(",") if depth == 0 => return,
            Some(")") if depth == 0 => return,
            Some("(") | Some("[") | Some("{") => {
                depth += 1;
                cur.bump();
            }
            Some(")") | Some("]") | Some("}") => {
                depth -= 1;
                cur.bump();
            }
            _ => {
                cur.bump();
            }
        }
    }
}

fn parse_fun_decl(cur: &mut Cursor) -> Result<KtDecl, String> {
    cur.expect("fun")?;
    let type_params = parse_type_params(cur)?;

    // Optional extension receiver: `ReceiverType.name(...)`. Parse a type,
    // then check for a following `.` + name; if there's no `.`, the parsed
    // type *is* the function name (reinterpret as identifier).
    let checkpoint = cur.pos;
    let mut receiver = None;
    let mut name;
    if let Ok(ty) = parse_type(cur) {
        if cur.eat(".") {
            receiver = Some(ty);
            name = cur.expect_ident()?;
        } else {
            cur.pos = checkpoint;
            name = cur.expect_ident()?;
        }
    } else {
        cur.pos = checkpoint;
        name = cur.expect_ident()?;
    }
    // Operator function names can be symbolic-ish keywords (`get`, `set`,
    // `plus`, ...) — already plain idents, nothing extra needed.
    let _ = &mut name;

    let params = parse_params(cur)?;
    let return_type = if cur.eat(":") {
        Some(parse_type(cur)?)
    } else {
        None
    };
    if cur.eat("where") {
        // Type-parameter bounds clause — skip to the body/semicolon.
        while !matches!(cur.peek(), Some("{") | Some("=") | Some(";") | None) {
            cur.bump();
        }
    }
    if cur.eat("=") {
        skip_expression_statement(cur);
    } else if cur.peek() == Some("{") {
        skip_balanced(cur, "{", "}");
    } else {
        let _ = cur.eat(";");
    }

    let mut decl = KtDecl::new(KtDeclKind::Function, name);
    decl.type_params = type_params;
    decl.receiver = receiver;
    decl.params = params;
    decl.return_type = return_type;
    Ok(decl)
}

/// Skips a `= expr` function/property body until the next statement
/// boundary at depth 0 (a `;` or a newline-adjacent declaration start —
/// approximated here as "next top-level-looking keyword or `}`").
fn skip_expression_statement(cur: &mut Cursor) {
    let mut depth = 0i32;
    loop {
        match cur.peek() {
            None => return,
            Some(";") if depth == 0 => {
                cur.bump();
                return;
            }
            Some("}") if depth == 0 => return,
            Some(kw) if depth == 0 && is_decl_start_keyword(kw) => return,
            Some("(") | Some("[") | Some("{") => {
                depth += 1;
                cur.bump();
            }
            Some(")") | Some("]") | Some("}") => {
                depth -= 1;
                cur.bump();
            }
            _ => {
                cur.bump();
            }
        }
    }
}

fn parse_class_like(
    cur: &mut Cursor,
    kind: KtDeclKind,
    is_enum: bool,
    diagnostics: &fp_core::diagnostics::DiagnosticManager,
) -> Result<KtDecl, String> {
    cur.bump(); // class | interface | object
    let name = if kind == KtDeclKind::Object && !matches!(cur.peek_kind(), Some(TokenKind::Ident)) {
        // Anonymous companion object.
        "companion".to_string()
    } else {
        cur.expect_ident()?
    };
    let type_params = parse_type_params(cur)?;
    let params = if cur.peek() == Some("(") {
        parse_params(cur)?
    } else {
        Vec::new()
    };

    let mut supertypes = Vec::new();
    if cur.eat(":") {
        loop {
            supertypes.push(parse_type(cur)?);
            if cur.peek() == Some("(") {
                skip_balanced(cur, "(", ")");
            }
            if cur.eat(",") {
                continue;
            }
            break;
        }
    }
    if cur.eat("where") {
        while !matches!(cur.peek(), Some("{") | None) {
            cur.bump();
        }
    }

    let mut members = Vec::new();
    if cur.eat("{") {
        if is_enum {
            members.extend(parse_enum_constants(cur));
        }
        members.extend(parse_body(cur, false, diagnostics));
        let _ = cur.expect("}");
    }

    let mut decl = KtDecl::new(kind, name);
    decl.type_params = type_params;
    decl.params = params;
    decl.supertypes = supertypes;
    decl.members = members;
    Ok(decl)
}

/// Parses an `enum class` body's leading constant list (`RED, GREEN(1),
/// BLUE { ... };`) — each constant becomes a plain `Property`-kind member
/// (its own constructor args/body are not modeled, just its name).
fn parse_enum_constants(cur: &mut Cursor) -> Vec<KtDecl> {
    let mut constants = Vec::new();
    loop {
        skip_annotations_and_modifiers(cur);
        match cur.peek_kind() {
            Some(TokenKind::Ident) if !is_decl_start_keyword(cur.peek().unwrap()) => {
                let name = cur.expect_ident().unwrap();
                if cur.peek() == Some("(") {
                    skip_balanced(cur, "(", ")");
                }
                if cur.peek() == Some("{") {
                    skip_balanced(cur, "{", "}");
                }
                constants.push(KtDecl::new(KtDeclKind::Property, name));
                if cur.eat(",") {
                    continue;
                }
                break;
            }
            _ => break,
        }
    }
    let _ = cur.eat(";");
    constants
}

fn parse_property_decl(cur: &mut Cursor) -> Result<KtDecl, String> {
    let is_mutable = cur.peek() == Some("var");
    cur.bump(); // val | var
    let type_params = parse_type_params(cur)?;

    let checkpoint = cur.pos;
    let mut receiver = None;
    let name;
    if let Ok(ty) = parse_type(cur) {
        if cur.eat(".") {
            receiver = Some(ty);
            name = cur.expect_ident()?;
        } else {
            cur.pos = checkpoint;
            name = cur.expect_ident()?;
        }
    } else {
        cur.pos = checkpoint;
        name = cur.expect_ident()?;
    }

    let return_type = if cur.eat(":") {
        Some(parse_type(cur)?)
    } else {
        None
    };
    if cur.eat("=") {
        skip_expression_statement(cur);
    } else {
        let _ = cur.eat(";");
    }
    // Optional custom getter/setter block(s).
    loop {
        skip_annotations_and_modifiers(cur);
        match cur.peek() {
            Some("get") | Some("set") => {
                cur.bump();
                if cur.peek() == Some("(") {
                    skip_balanced(cur, "(", ")");
                    if cur.eat(":") {
                        let _ = parse_type(cur);
                    }
                    if cur.eat("=") {
                        skip_expression_statement(cur);
                    } else if cur.peek() == Some("{") {
                        skip_balanced(cur, "{", "}");
                    }
                }
            }
            _ => break,
        }
    }

    let mut decl = KtDecl::new(KtDeclKind::Property, name);
    decl.type_params = type_params;
    decl.receiver = receiver;
    decl.return_type = return_type;
    decl.is_mutable = is_mutable;
    Ok(decl)
}

fn parse_typealias_decl(cur: &mut Cursor) -> Result<KtDecl, String> {
    cur.expect("typealias")?;
    let name = cur.expect_ident()?;
    let type_params = parse_type_params(cur)?;
    cur.expect("=")?;
    let target = parse_type(cur)?;
    let _ = cur.eat(";");

    let mut decl = KtDecl::new(KtDeclKind::TypeAlias, name);
    decl.type_params = type_params;
    decl.return_type = Some(target);
    Ok(decl)
}
