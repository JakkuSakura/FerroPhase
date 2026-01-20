use std::fmt::{Display, Formatter};
use std::hash::Hash;

use crate::ast::{
    get_threadlocal_serializer, BExpr, BPattern, BType, Expr, ExprBlock, ExprKind, Ident, Locator,
    Pattern, Ty, Value, ValueFunction,
};
use crate::intrinsics::IntrinsicCallKind;
use crate::ops::{BinOpKind, UnOpKind};
use crate::{common_enum, common_struct};
use crate::span::Span;

common_enum! {
    pub enum ExprInvokeTarget {
        Function(Locator),
        Type(Ty),
        Method(ExprSelect),
        Closure(ValueFunction),
        BinOp(BinOpKind),
        Expr(BExpr),
    }
}
impl ExprInvokeTarget {
    pub fn expr(expr: Expr) -> Self {
        let (ty, kind) = expr.into_parts();
        match kind {
            ExprKind::Locator(locator) => Self::Function(locator),
            ExprKind::Select(select) => Self::Method(select),
            ExprKind::Value(value) => Self::value(*value),
            other => Self::Expr(Expr::from_parts(ty, other).into()),
        }
    }
    pub fn value(value: Value) -> Self {
        match value {
            Value::Function(func) => Self::Closure(func.clone()),
            Value::BinOpKind(kind) => Self::BinOp(kind.clone()),
            Value::Type(ty) => Self::Type(ty.clone()),
            Value::Expr(expr) => Self::expr(*expr),
            other => {
                // Gracefully handle unexpected values by treating them as dynamic expressions.
                // This avoids panicking in library code paths and keeps the pipeline resilient.
                tracing::warn!(
                    "ExprInvokeTarget::value received unsupported value kind: {}",
                    other
                );
                Self::Expr(Expr::value(other).into())
            }
        }
    }
}

common_struct! {
    pub struct ExprInvoke {
        #[serde(default)]
        pub span: Span,
        pub target: ExprInvokeTarget,
        pub args: Vec<Expr>,
    }
}

common_struct! {
    pub struct ExprAwait {
        #[serde(default)]
        pub span: Span,
        pub base: BExpr,
    }
}
impl Display for ExprInvoke {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = get_threadlocal_serializer().serialize_invoke(self).unwrap();
        f.write_str(&s)
    }
}

common_struct! {
    pub struct ExprStringTemplate {
        /// Template parts - alternating literals and placeholders
        pub parts: Vec<FormatTemplatePart>,
    }
}

common_enum! {
    pub enum FormatTemplatePart {
        /// A literal string part
        Literal(String),
        /// A placeholder that references an argument
        Placeholder(FormatPlaceholder),
    }
}

common_struct! {
    pub struct FormatPlaceholder {
        /// Argument reference - can be positional index, name, or implicit
        pub arg_ref: FormatArgRef,
        /// Optional format specification (Rust-like, raw form preserved)
        pub format_spec: Option<FormatSpec>,
    }
}

common_struct! {
    pub struct FormatSpec {
        pub raw: String,
        pub parsed: Option<RustFormatSpec>,
    }
}

common_struct! {
    pub struct RustFormatSpec {
        pub fill: Option<char>,
        pub align: Option<FormatAlign>,
        pub sign: Option<FormatSign>,
        pub alternate: bool,
        pub zero: bool,
        pub width: Option<usize>,
        pub precision: Option<usize>,
        pub ty: Option<char>,
    }
}

common_enum! {
    pub enum FormatAlign {
        Left,
        Right,
        Center,
        SignAware,
    }
}

common_enum! {
    pub enum FormatSign {
        Plus,
        Minus,
        Space,
    }
}

common_enum! {
    pub enum FormatArgRef {
        /// Implicit positional argument (next in sequence)
        Implicit,
        /// Explicit positional argument by index (e.g., {0}, {1})
        Positional(usize),
        /// Named argument (e.g., {name}, {value})
        Named(String),
    }
}

common_struct! {
    pub struct ExprKwArg {
        /// The keyword name
        pub name: String,
        /// The expression value
        pub value: Expr,
    }
}

common_struct! {
    pub struct ExprIntrinsicCall {
        #[serde(default)]
        pub span: Span,
        pub kind: IntrinsicCallKind,
        pub args: Vec<Expr>,
        pub kwargs: Vec<ExprKwArg>,
    }
}

impl ExprIntrinsicCall {
    pub fn new(kind: IntrinsicCallKind, args: Vec<Expr>, kwargs: Vec<ExprKwArg>) -> Self {
        Self {
            span: Span::null(),
            kind,
            args,
            kwargs,
        }
    }
}

// === Quoting & Splicing (AST-level keywords) ===

common_enum! {
    #[derive(Copy)]
    pub enum QuoteFragmentKind {
        Expr,
        Stmt,
        Item,
        Type,
    }
}

common_enum! {
    #[derive(Copy)]
    pub enum QuoteItemKind {
        Function,
        Struct,
        Enum,
        Trait,
        Impl,
        Type,
        Const,
        Static,
        Module,
        Use,
        Macro,
    }
}

common_struct! {
    /// Quote expression: captures a block of code as a compile-time token.
    ///
    /// - `block` holds the surface fragment. Kind may be inferred later.
    /// - `kind` is optional and, when present, indicates explicit fragment kind.
    pub struct ExprQuote {
        #[serde(default)]
        pub span: Span,
        pub block: ExprBlock,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub kind: Option<QuoteFragmentKind>,
    }
}

common_struct! {
    /// Splice expression: inserts a previously quoted token into the AST.
    /// The `token` expression should evaluate (in const) to a QuoteToken.
    pub struct ExprSplice {
        #[serde(default)]
        pub span: Span,
        pub token: BExpr,
    }
}

impl Display for ExprStringTemplate {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "template!(\"")?;
        // Reconstruct the template string from parts.
        for part in &self.parts {
            write!(f, "{}", part)?;
        }
        write!(f, "\")")
    }
}

impl Display for FormatTemplatePart {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            FormatTemplatePart::Literal(s) => write!(f, "{}", s),
            FormatTemplatePart::Placeholder(placeholder) => write!(f, "{{{}}}", placeholder),
        }
    }
}

impl Display for FormatPlaceholder {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.arg_ref)?;
        if let Some(spec) = &self.format_spec {
            write!(f, ":{}", spec.raw)?;
        }
        Ok(())
    }
}

impl Display for FormatArgRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            FormatArgRef::Implicit => Ok(()), // Empty for implicit {}
            FormatArgRef::Positional(idx) => write!(f, "{}", idx),
            FormatArgRef::Named(name) => write!(f, "{}", name),
        }
    }
}

impl Display for ExprKwArg {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}={}", self.name, self.value)
    }
}

fn union_spans<I>(spans: I) -> Span
where
    I: IntoIterator<Item = Span>,
{
    Span::union(spans)
}

fn span_or(span: Span, fallback: Span) -> Span {
    if span.is_null() {
        fallback
    } else {
        span
    }
}

impl ExprInvokeTarget {
    pub fn span(&self) -> Span {
        match self {
            ExprInvokeTarget::Function(locator) => locator.span(),
            ExprInvokeTarget::Type(ty) => ty.span(),
            ExprInvokeTarget::Method(select) => select.span(),
            ExprInvokeTarget::Closure(func) => func.span(),
            ExprInvokeTarget::Expr(expr) => expr.span(),
            ExprInvokeTarget::BinOp(_) => Span::null(),
        }
    }
}

impl ExprInvoke {
    pub fn span(&self) -> Span {
        span_or(
            self.span,
            union_spans(
                Some(self.target.span())
                    .into_iter()
                    .chain(self.args.iter().map(Expr::span)),
            ),
        )
    }
}

impl ExprAwait {
    pub fn span(&self) -> Span {
        span_or(self.span, self.base.span())
    }
}

impl ExprStringTemplate {
    pub fn span(&self) -> Span {
        union_spans(self.parts.iter().map(FormatTemplatePart::span))
    }
}

impl FormatTemplatePart {
    pub fn span(&self) -> Span {
        match self {
            FormatTemplatePart::Literal(_) => Span::null(),
            FormatTemplatePart::Placeholder(placeholder) => placeholder.span(),
        }
    }
}

impl FormatPlaceholder {
    pub fn span(&self) -> Span {
        match self.arg_ref {
            FormatArgRef::Implicit => Span::null(),
            FormatArgRef::Positional(_) => Span::null(),
            FormatArgRef::Named(_) => Span::null(),
        }
    }
}

impl ExprKwArg {
    pub fn span(&self) -> Span {
        self.value.span()
    }
}

impl ExprIntrinsicCall {
    pub fn span(&self) -> Span {
        span_or(
            self.span,
            union_spans(
                self.args
                    .iter()
                    .map(Expr::span)
                    .chain(self.kwargs.iter().map(ExprKwArg::span)),
            ),
        )
    }
}

impl ExprQuote {
    pub fn span(&self) -> Span {
        span_or(self.span, self.block.span())
    }
}

impl ExprSplice {
    pub fn span(&self) -> Span {
        span_or(self.span, self.token.span())
    }
}

impl ExprSelect {
    pub fn span(&self) -> Span {
        span_or(self.span, self.obj.span())
    }
}

impl ExprIndex {
    pub fn span(&self) -> Span {
        span_or(self.span, union_spans([self.obj.span(), self.index.span()]))
    }
}

impl ExprReference {
    pub fn span(&self) -> Span {
        span_or(self.span, self.referee.span())
    }
}

impl ExprDereference {
    pub fn span(&self) -> Span {
        span_or(self.span, self.referee.span())
    }
}

impl ExprMatch {
    pub fn span(&self) -> Span {
        span_or(
            self.span,
            union_spans(
                self.scrutinee
                    .as_ref()
                    .map(|expr| expr.span())
                    .into_iter()
                    .chain(self.cases.iter().map(ExprMatchCase::span)),
            ),
        )
    }
}

impl ExprIf {
    pub fn span(&self) -> Span {
        span_or(
            self.span,
            union_spans(
                [
                    Some(self.cond.span()),
                    Some(self.then.span()),
                    self.elze.as_ref().map(|expr| expr.span()),
                ]
                .into_iter()
                .flatten(),
            ),
        )
    }
}

impl ExprLoop {
    pub fn span(&self) -> Span {
        span_or(self.span, self.body.span())
    }
}

impl ExprWhile {
    pub fn span(&self) -> Span {
        span_or(self.span, union_spans([self.cond.span(), self.body.span()]))
    }
}

impl ExprReturn {
    pub fn span(&self) -> Span {
        span_or(
            self.span,
            self.value
                .as_ref()
                .map(|value| value.span())
                .unwrap_or_else(Span::null),
        )
    }
}

impl ExprBreak {
    pub fn span(&self) -> Span {
        span_or(
            self.span,
            self.value
                .as_ref()
                .map(|value| value.span())
                .unwrap_or_else(Span::null),
        )
    }
}

impl ExprContinue {
    pub fn span(&self) -> Span {
        span_or(self.span, Span::null())
    }
}

impl ExprConstBlock {
    pub fn span(&self) -> Span {
        span_or(self.span, self.expr.span())
    }
}

impl ExprMatchCase {
    pub fn span(&self) -> Span {
        span_or(
            self.span,
            union_spans(
                [
                    self.pat.as_ref().map(|pat| pat.span()),
                    Some(self.cond.span()),
                    self.guard.as_ref().map(|expr| expr.span()),
                    Some(self.body.span()),
                ]
                .into_iter()
                .flatten(),
            ),
        )
    }
}

impl ExprAsync {
    pub fn span(&self) -> Span {
        span_or(self.span, self.expr.span())
    }
}

impl ExprFor {
    pub fn span(&self) -> Span {
        span_or(
            self.span,
            union_spans([self.pat.span(), self.iter.span(), self.body.span()]),
        )
    }
}

impl ExprStruct {
    pub fn span(&self) -> Span {
        span_or(
            self.span,
            union_spans(
                [
                    Some(self.name.span()),
                    Some(Span::union(self.fields.iter().map(ExprField::span))),
                    self.update.as_ref().map(|expr| expr.span()),
                ]
                .into_iter()
                .flatten(),
            ),
        )
    }
}

impl ExprStructural {
    pub fn span(&self) -> Span {
        span_or(
            self.span,
            Span::union(self.fields.iter().map(ExprField::span)),
        )
    }
}

impl ExprField {
    pub fn span(&self) -> Span {
        span_or(
            self.span,
            self.value
                .as_ref()
                .map(|value| value.span())
                .unwrap_or_else(Span::null),
        )
    }
}

impl ExprCast {
    pub fn span(&self) -> Span {
        span_or(self.span, union_spans([self.expr.span(), self.ty.span()]))
    }
}

impl ExprBinOp {
    pub fn span(&self) -> Span {
        span_or(self.span, union_spans([self.lhs.span(), self.rhs.span()]))
    }
}

impl ExprUnOp {
    pub fn span(&self) -> Span {
        span_or(self.span, self.val.span())
    }
}

impl ExprAssign {
    pub fn span(&self) -> Span {
        span_or(
            self.span,
            union_spans([self.target.span(), self.value.span()]),
        )
    }
}

impl ExprParen {
    pub fn span(&self) -> Span {
        span_or(self.span, self.expr.span())
    }
}

impl ExprRange {
    pub fn span(&self) -> Span {
        span_or(
            self.span,
            union_spans(
                [
                    self.start.as_ref().map(|expr| expr.span()),
                    self.end.as_ref().map(|expr| expr.span()),
                    self.step.as_ref().map(|expr| expr.span()),
                ]
                .into_iter()
                .flatten(),
            ),
        )
    }
}

impl ExprTuple {
    pub fn span(&self) -> Span {
        self.span
    }
}

impl ExprTry {
    pub fn span(&self) -> Span {
        span_or(self.span, self.expr.span())
    }
}

impl ExprLet {
    pub fn span(&self) -> Span {
        span_or(self.span, union_spans([self.pat.span(), self.expr.span()]))
    }
}

impl ExprClosure {
    pub fn span(&self) -> Span {
        span_or(
            self.span,
            union_spans(
                self.params
                    .iter()
                    .map(Pattern::span)
                    .chain(self.ret_ty.as_ref().map(|ty| ty.span()))
                    .chain([self.body.span()]),
            ),
        )
    }
}

impl ExprArray {
    pub fn span(&self) -> Span {
        span_or(
            self.span,
            Span::union(self.values.iter().map(Expr::span)),
        )
    }
}

impl ExprArrayRepeat {
    pub fn span(&self) -> Span {
        span_or(self.span, union_spans([self.elem.span(), self.len.span()]))
    }
}

impl ExprSplat {
    pub fn span(&self) -> Span {
        span_or(self.span, self.iter.span())
    }
}

impl ExprSplatDict {
    pub fn span(&self) -> Span {
        span_or(self.span, self.dict.span())
    }
}

impl FormatSpec {
    pub fn parse(raw: &str) -> Result<Self, String> {
        if raw.starts_with('%') {
            return Ok(Self {
                raw: raw.to_string(),
                parsed: None,
            });
        }

        let parsed = parse_rust_format_spec(raw)?;
        Ok(Self {
            raw: raw.to_string(),
            parsed: Some(parsed),
        })
    }
}

fn parse_rust_format_spec(raw: &str) -> Result<RustFormatSpec, String> {
    if raw.is_empty() {
        return Ok(RustFormatSpec {
            fill: None,
            align: None,
            sign: None,
            alternate: false,
            zero: false,
            width: None,
            precision: None,
            ty: None,
        });
    }

    let bytes = raw.as_bytes();
    let mut idx = 0usize;

    let mut fill = None;
    let mut align = None;

    if idx + 1 < bytes.len() && is_align(bytes[idx + 1]) {
        fill = Some(bytes[idx] as char);
        align = Some(parse_align(bytes[idx + 1])?);
        idx += 2;
    } else if idx < bytes.len() && is_align(bytes[idx]) {
        align = Some(parse_align(bytes[idx])?);
        idx += 1;
    }

    let mut sign = None;
    if idx < bytes.len() {
        sign = match bytes[idx] {
            b'+' => {
                idx += 1;
                Some(FormatSign::Plus)
            }
            b'-' => {
                idx += 1;
                Some(FormatSign::Minus)
            }
            b' ' => {
                idx += 1;
                Some(FormatSign::Space)
            }
            _ => None,
        };
    }

    let mut alternate = false;
    if idx < bytes.len() && bytes[idx] == b'#' {
        alternate = true;
        idx += 1;
    }

    let mut zero = false;
    if idx < bytes.len() && bytes[idx] == b'0' {
        zero = true;
        idx += 1;
    }

    let (width, next) = parse_decimal(bytes, idx)?;
    idx = next;

    let mut precision = None;
    if idx < bytes.len() && bytes[idx] == b'.' {
        idx += 1;
        let (parsed, next_idx) = parse_decimal(bytes, idx)?;
        if parsed.is_none() {
            return Err("format precision requires digits".to_string());
        }
        precision = parsed;
        idx = next_idx;
    }

    let mut ty = None;
    if idx < bytes.len() {
        if idx + 1 != bytes.len() {
            return Err(format!(
                "format spec has trailing characters: {}",
                &raw[idx..]
            ));
        }
        ty = Some(bytes[idx] as char);
        idx += 1;
    }

    if idx != bytes.len() {
        return Err("format spec parsing did not consume input".to_string());
    }

    Ok(RustFormatSpec {
        fill,
        align,
        sign,
        alternate,
        zero,
        width,
        precision,
        ty,
    })
}

fn is_align(byte: u8) -> bool {
    matches!(byte, b'<' | b'>' | b'^' | b'=')
}

fn parse_align(byte: u8) -> Result<FormatAlign, String> {
    match byte {
        b'<' => Ok(FormatAlign::Left),
        b'>' => Ok(FormatAlign::Right),
        b'^' => Ok(FormatAlign::Center),
        b'=' => Ok(FormatAlign::SignAware),
        _ => Err("invalid alignment specifier".to_string()),
    }
}

fn parse_decimal(bytes: &[u8], mut idx: usize) -> Result<(Option<usize>, usize), String> {
    let start = idx;
    let mut value: usize = 0;
    while idx < bytes.len() && bytes[idx].is_ascii_digit() {
        let digit = (bytes[idx] - b'0') as usize;
        value = value
            .checked_mul(10)
            .and_then(|v| v.checked_add(digit))
            .ok_or_else(|| "format width/precision overflow".to_string())?;
        idx += 1;
    }
    if idx == start {
        return Ok((None, idx));
    }
    Ok((Some(value), idx))
}

/// Attempt to recognise canonical intrinsic calls inside a generic invoke expression.
pub fn intrinsic_call_from_invoke(invoke: &ExprInvoke) -> Option<ExprIntrinsicCall> {
    let (kind, _locator) = match &invoke.target {
        ExprInvokeTarget::Function(locator) => (detect_intrinsic_call(locator)?, locator),
        _ => return None,
    };

    match kind {
        IntrinsicCallKind::Print | IntrinsicCallKind::Println => {
            let newline = matches!(kind, IntrinsicCallKind::Println);
            let (template, skip) = build_string_template_from_args(&invoke.args, newline)?;
            let mut args = Vec::with_capacity(1 + invoke.args.len().saturating_sub(skip));
            args.push(Expr::new(ExprKind::FormatString(template)));
            args.extend(invoke.args.iter().skip(skip).cloned());
            Some(ExprIntrinsicCall::new(kind, args, Vec::new()))
        }
        IntrinsicCallKind::Len => {
            if invoke.args.len() != 1 {
                return None;
            }
            Some(ExprIntrinsicCall::new(
                kind,
                vec![invoke.args[0].clone()],
                Vec::new(),
            ))
        }
        IntrinsicCallKind::TimeNow => {
            if !invoke.args.is_empty() {
                return None;
            }
            Some(ExprIntrinsicCall::new(kind, Vec::new(), Vec::new()))
        }
        IntrinsicCallKind::CatchUnwind => Some(ExprIntrinsicCall::new(
            kind,
            invoke.args.clone(),
            Vec::new(),
        )),
        IntrinsicCallKind::ProcMacroTokenStreamFromStr => {
            if invoke.args.len() != 1 {
                return None;
            }
            Some(ExprIntrinsicCall::new(
                kind,
                vec![invoke.args[0].clone()],
                Vec::new(),
            ))
        }
        IntrinsicCallKind::ProcMacroTokenStreamToString => {
            if invoke.args.len() != 1 {
                return None;
            }
            Some(ExprIntrinsicCall::new(
                kind,
                vec![invoke.args[0].clone()],
                Vec::new(),
            ))
        }
        IntrinsicCallKind::Format => None,
        IntrinsicCallKind::DebugAssertions
        | IntrinsicCallKind::Input
        | IntrinsicCallKind::Panic
        | IntrinsicCallKind::Slice
        | IntrinsicCallKind::SizeOf
        | IntrinsicCallKind::ReflectFields
        | IntrinsicCallKind::HasMethod
        | IntrinsicCallKind::TypeName
        | IntrinsicCallKind::TypeOf
        | IntrinsicCallKind::CreateStruct
        | IntrinsicCallKind::CloneStruct
        | IntrinsicCallKind::AddField
        | IntrinsicCallKind::HasField
        | IntrinsicCallKind::FieldCount
        | IntrinsicCallKind::MethodCount
        | IntrinsicCallKind::FieldType
        | IntrinsicCallKind::VecType
        | IntrinsicCallKind::FieldNameAt
        | IntrinsicCallKind::StructSize
        | IntrinsicCallKind::GenerateMethod
        | IntrinsicCallKind::CompileError
        | IntrinsicCallKind::CompileWarning => None,
    }
}

fn detect_intrinsic_call(locator: &Locator) -> Option<IntrinsicCallKind> {
    if let Some(kind) = crate::lang::lookup_lang_item_intrinsic(locator) {
        return Some(kind);
    }

    match locator {
        Locator::Ident(ident) => match ident.name.as_str() {
            "print" => Some(IntrinsicCallKind::Print),
            "println" => Some(IntrinsicCallKind::Println),
            "len" => Some(IntrinsicCallKind::Len),
            "catch_unwind" => Some(IntrinsicCallKind::CatchUnwind),
            _ => None,
        },
        Locator::Path(path) => {
            let names: Vec<_> = path.segments.iter().map(|seg| seg.name.as_str()).collect();
            match names.as_slice() {
                ["std", "print"] | ["std", "io", "print"] => Some(IntrinsicCallKind::Print),
                ["std", "println"] | ["std", "io", "println"] => Some(IntrinsicCallKind::Println),
                ["std", "len"] | ["std", "builtins", "len"] | ["len"] => {
                    Some(IntrinsicCallKind::Len)
                }
                ["std", "time", "now"] => Some(IntrinsicCallKind::TimeNow),
                ["proc_macro", "token_stream_from_str"]
                | ["std", "proc_macro", "token_stream_from_str"]
                | ["proc_macro", "TokenStream", "from_str"]
                | ["std", "proc_macro", "TokenStream", "from_str"] => {
                    Some(IntrinsicCallKind::ProcMacroTokenStreamFromStr)
                }
                ["proc_macro", "token_stream_to_string"]
                | ["std", "proc_macro", "token_stream_to_string"]
                | ["proc_macro", "TokenStream", "to_string"]
                | ["std", "proc_macro", "TokenStream", "to_string"] => {
                    Some(IntrinsicCallKind::ProcMacroTokenStreamToString)
                }
                _ => None,
            }
        }
        _ => None,
    }
}

fn build_string_template_from_args(
    args: &[Expr],
    _newline: bool,
) -> Option<(ExprStringTemplate, usize)> {
    if args.is_empty() {
        return Some((
            ExprStringTemplate {
                parts: vec![FormatTemplatePart::Literal(String::new())],
            },
            0,
        ));
    }

    match args[0].kind() {
        ExprKind::FormatString(fmt) => Some((fmt.clone(), 1)),
        ExprKind::Value(value) => {
            if let Value::String(str_val) = &**value {
                if args.len() == 1 {
                    return Some((
                        ExprStringTemplate {
                            parts: vec![FormatTemplatePart::Literal(str_val.value.clone())],
                        },
                        1,
                    ));
                }

                // When extra args are provided, decide whether the first string literal
                // is intended as a Rust-style format template.
                let template = str_val.value.clone();
                let looks_like_format_template = template.contains('{');
                if looks_like_format_template {
                    let format = ExprStringTemplate {
                        parts: vec![FormatTemplatePart::Literal(template)],
                    };
                    return Some((format, 1));
                }

                // Otherwise treat it like a multi-arg print: prefix + placeholders.
                let mut parts = vec![FormatTemplatePart::Literal(template)];
                if !matches!(
                    parts.last(),
                    Some(FormatTemplatePart::Literal(lit)) if lit.is_empty()
                ) {
                    parts.push(FormatTemplatePart::Literal(" ".to_string()));
                }

                for (idx, _arg) in args[1..].iter().enumerate() {
                    parts.push(FormatTemplatePart::Placeholder(FormatPlaceholder {
                        arg_ref: FormatArgRef::Implicit,
                        format_spec: None,
                    }));
                    if idx + 1 < args.len() - 1 {
                        parts.push(FormatTemplatePart::Literal(" ".to_string()));
                    }
                }

                Some((ExprStringTemplate { parts }, 1))
            } else {
                None
            }
        }
        _ => {
            let mut parts = Vec::new();
            for idx in 0..args.len() {
                parts.push(FormatTemplatePart::Placeholder(FormatPlaceholder {
                    arg_ref: FormatArgRef::Implicit,
                    format_spec: None,
                }));
                if idx + 1 < args.len() {
                    parts.push(FormatTemplatePart::Literal(" ".to_string()));
                }
            }
            Some((ExprStringTemplate { parts }, 0))
        }
    }
}

common_enum! {
    pub enum ExprSelectType {
        Unknown,
        Field,
        Method,
        Function,
        Const,
    }

}

common_struct! {
    pub struct ExprSelect {
        #[serde(default)]
        pub span: Span,
        pub obj: BExpr,
        pub field: Ident,
        pub select: ExprSelectType,
    }
}

common_struct! {
    pub struct ExprIndex {
        #[serde(default)]
        pub span: Span,
        pub obj: BExpr,
        pub index: BExpr,
    }
}

common_struct! {
    pub struct ExprReference {
        #[serde(default)]
        pub span: Span,
        pub referee: BExpr,
        pub mutable: Option<bool>,
    }
}
common_struct! {
    pub struct ExprDereference {
        #[serde(default)]
        pub span: Span,
        pub referee: BExpr,
    }
}

common_struct! {
    pub struct ExprMatch {
        #[serde(default)]
        pub span: Span,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub scrutinee: Option<BExpr>,
        pub cases: Vec<ExprMatchCase>,
    }
}

common_struct! {
    pub struct ExprIf {
        #[serde(default)]
        pub span: Span,
        pub cond: BExpr,
        pub then: BExpr,
        pub elze: Option<BExpr>,
    }
}
common_struct! {
    pub struct ExprLoop {
        #[serde(default)]
        pub span: Span,
        pub label: Option<Ident>,
        pub body: BExpr,
    }
}
common_struct! {
    pub struct ExprWhile {
        #[serde(default)]
        pub span: Span,
        pub cond: BExpr,
        pub body: BExpr,
    }
}
common_struct! {
    pub struct ExprReturn {
        #[serde(default)]
        pub span: Span,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub value: Option<BExpr>,
    }
}
common_struct! {
    pub struct ExprBreak {
        #[serde(default)]
        pub span: Span,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub value: Option<BExpr>,
    }
}
common_struct! {
    pub struct ExprContinue {
        #[serde(default)]
        pub span: Span,
    }
}
common_struct! {
    pub struct ExprConstBlock {
        #[serde(default)]
        pub span: Span,
        pub expr: BExpr,
    }
}
common_struct! {
    pub struct ExprMatchCase {
        #[serde(default)]
        pub span: Span,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub pat: Option<BPattern>,
        pub cond: BExpr,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub guard: Option<BExpr>,
        pub body: BExpr,
    }
}

common_struct! {
    /// Async expression wrapper. Semantics are provided by later
    /// lowering/normalization passes; at the AST level this acts
    /// as a marker around an inner expression.
    pub struct ExprAsync {
        #[serde(default)]
        pub span: Span,
        pub expr: BExpr,
    }
}

common_struct! {
    /// High-level `for` loop expression: `for pat in iter { body }`.
    ///
    /// Lowering into concrete control-flow constructs is handled in
    /// later passes; the AST captures the pattern, iterator and body.
    pub struct ExprFor {
        #[serde(default)]
        pub span: Span,
        pub pat: BPattern,
        pub iter: BExpr,
        pub body: BExpr,
    }
}

common_enum! {
    pub enum ControlFlow {
        Continue,
        #[from(ignore)]
        Break(Option<Expr>),
        #[from(ignore)]
        Return(Option<Expr>),
        Into,
        #[from(ignore)]
        IntoAndBreak(Option<Expr>),
    }
}
common_struct! {
    pub struct ExprStruct {
        #[serde(default)]
        pub span: Span,
        pub name: BExpr,
        pub fields: Vec<ExprField>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub update: Option<BExpr>,
    }
}
impl ExprStruct {
    pub fn new_ident(name: Ident, fields: Vec<ExprField>) -> Self {
        Self {
            span: Span::null(),
            name: Expr::ident(name).into(),
            fields,
            update: None,
        }
    }
    pub fn new(name: BExpr, fields: Vec<ExprField>) -> Self {
        Self {
            span: Span::null(),
            name,
            fields,
            update: None,
        }
    }
}
common_struct! {
    pub struct ExprStructural {
        #[serde(default)]
        pub span: Span,
        pub fields: Vec<ExprField>,
    }
}
common_struct! {
    pub struct ExprField {
        #[serde(default)]
        pub span: Span,
        pub name: Ident,
        pub value: Option<Expr>,
    }
}
impl ExprField {
    pub fn new(name: Ident, value: Expr) -> Self {
        Self {
            span: Span::null(),
            name,
            value: Some(value),
        }
    }
    pub fn new_no_value(name: Ident) -> Self {
        Self {
            span: Span::null(),
            name,
            value: None,
        }
    }
}
common_struct! {
    pub struct ExprCast {
        #[serde(default)]
        pub span: Span,
        pub expr: BExpr,
        pub ty: Ty,
    }
}
common_struct! {
    pub struct ExprBinOp {
        #[serde(default)]
        pub span: Span,
        pub kind: BinOpKind,
        pub lhs: BExpr,
        pub rhs: BExpr,
    }
}
common_struct! {
    pub struct ExprUnOp {
        #[serde(default)]
        pub span: Span,
        pub op: UnOpKind,
        pub val: BExpr,

    }
}

common_struct! {
    pub struct ExprAssign {
        #[serde(default)]
        pub span: Span,
        pub target: BExpr,
        pub value: BExpr,
    }
}
common_struct! {
    pub struct ExprParen {
        #[serde(default)]
        pub span: Span,
        pub expr: BExpr,
    }
}
common_enum! {
    pub enum ExprRangeLimit {
        Inclusive,
        Exclusive,
    }
}
common_struct! {
    pub struct ExprRange {
        #[serde(default)]
        pub span: Span,
        pub start: Option<BExpr>,
        pub limit: ExprRangeLimit,
        pub end: Option<BExpr>,
        pub step: Option<BExpr>,
    }
}

common_struct! {
    pub struct ExprTuple {
        #[serde(default)]
        pub span: Span,
        pub values: Vec<Expr>,
    }
}

common_struct! {
    pub struct ExprTry {
        #[serde(default)]
        pub span: Span,
        pub expr: BExpr,
    }
}

common_struct! {
    pub struct ExprLet {
        #[serde(default)]
        pub span: Span,
        pub pat: BPattern,
        pub expr: BExpr,
    }
}
common_struct! {
    pub struct ExprClosure {
        #[serde(default)]
        pub span: Span,
        pub params: Vec<Pattern>,
        pub ret_ty: Option<BType>,
        pub movability: Option<bool>,
        pub body: BExpr,
    }
}
common_struct! {
    pub struct ExprArray {
        #[serde(default)]
        pub span: Span,
        pub values: Vec<Expr>,
    }
}

common_struct! {
    pub struct ExprArrayRepeat {
        #[serde(default)]
        pub span: Span,
        pub elem: BExpr,
        pub len: BExpr,
    }
}
common_struct! {
    /// To "splat" or expand an iterable.
    /// For example, in Python, `*a` will expand `a` into the arguments of a function
    pub struct ExprSplat {
        #[serde(default)]
        pub span: Span,
        pub iter: Box<Expr>,
    }
}
common_struct! {
    /// To "splat" or expand a dict.
    /// For example, in Python, `**d` will expand `d` into the keyword arguments of a function
    pub struct ExprSplatDict {
        #[serde(default)]
        pub span: Span,
        pub dict: Box<Expr>,
    }
}
