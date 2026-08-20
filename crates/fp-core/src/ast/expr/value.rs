use std::fmt::{Display, Formatter};
use std::hash::Hash;

use crate::ast::{
    BExpr, BPattern, BType, Expr, ExprBlock, ExprKind, Ident, ItemChunk, Name, Pattern, Ty, Value,
    ValueFunction, get_threadlocal_serializer,
};
use crate::intrinsics::CallKind;
use crate::ops::{BinOpKind, UnOpKind};
use crate::span::Span;
use crate::{common_enum, common_struct};

common_enum! {
    pub enum ExprInvokeTarget {
        Function(Name),
        Type(Ty),
        Method(ExprSelect),
        Closure(ValueFunction),
        BinOp(BinOpKind),
        Expr(BExpr),
    }
}
impl ExprInvokeTarget {
    pub fn expr(expr: Expr) -> Self {
        let (id, span, kind) = expr.into_parts();
        match kind {
            ExprKind::Name(name) => Self::Function(name),
            ExprKind::Select(select) => Self::Method(select),
            ExprKind::Value(value) => Self::value(*value),
            other => Self::Expr(Expr::from_parts(id, span, other).into()),
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
        #[serde(default)]
        pub kwargs: Vec<ExprKwArg>,
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
        pub dynamic_width: bool,
        pub precision: Option<usize>,
        pub dynamic_precision: bool,
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
        pub kind: CallKind,
        pub args: Vec<Expr>,
        pub kwargs: Vec<ExprKwArg>,
    }
}

impl ExprIntrinsicCall {
    pub fn new(kind: impl Into<CallKind>, args: Vec<Expr>, kwargs: Vec<ExprKwArg>) -> Self {
        Self {
            span: Span::null(),
            kind: kind.into(),
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
        #[serde(default)]
        pub collected_items: ItemChunk,
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

common_struct! {
    /// Placeholder for a splice whose evaluation has been delegated to the scheduler.
    /// request_id maps to an entry in CompilerState::splice_results.
    pub struct ExprSplicePending {
        #[serde(default)]
        pub span: Span,
        pub request_id: u64,
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
    if span.is_null() { fallback } else { span }
}

impl ExprInvokeTarget {
    pub fn span(&self) -> Span {
        match self {
            ExprInvokeTarget::Function(name) => name.span(),
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
                    .chain(self.args.iter().map(Expr::span))
                    .chain(self.kwargs.iter().map(ExprKwArg::span)),
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

impl ExprSplicePending {
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
        span_or(
            self.span,
            union_spans(
                Some(self.expr.span())
                    .into_iter()
                    .chain(self.catches.iter().map(ExprTryCatch::span))
                    .chain(self.elze.as_ref().map(|expr| expr.span()))
                    .chain(self.finally.as_ref().map(|expr| expr.span())),
            ),
        )
    }
}

impl ExprTryCatch {
    pub fn span(&self) -> Span {
        span_or(
            self.span,
            union_spans(
                self.pat
                    .as_ref()
                    .map(|pat| pat.span())
                    .into_iter()
                    .chain([self.body.span()]),
            ),
        )
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

impl ExprWith {
    pub fn span(&self) -> Span {
        span_or(
            self.span,
            union_spans([self.context.span(), self.body.span()]),
        )
    }
}

impl ExprArray {
    pub fn span(&self) -> Span {
        span_or(self.span, Span::union(self.values.iter().map(Expr::span)))
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
            dynamic_width: false,
            precision: None,
            dynamic_precision: false,
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

    let (mut width, mut next) = parse_decimal(bytes, idx)?;
    let mut dynamic_width = false;
    if width.is_some() && next < bytes.len() && bytes[next] == b'$' {
        // `{:5$}` — width taken from the argument at index 5, not a literal width.
        dynamic_width = true;
        width = None;
        next += 1;
    } else if width.is_none() {
        // `{:name$}` — width taken from a named argument/const.
        let ident_end = scan_ident(bytes, idx);
        if ident_end > idx && ident_end < bytes.len() && bytes[ident_end] == b'$' {
            dynamic_width = true;
            next = ident_end + 1;
        }
    }
    idx = next;

    let mut precision = None;
    let mut dynamic_precision = false;
    if idx < bytes.len() && bytes[idx] == b'.' {
        idx += 1;
        if idx < bytes.len() && bytes[idx] == b'*' {
            dynamic_precision = true;
            idx += 1;
        } else {
            let (parsed, next_idx) = parse_decimal(bytes, idx)?;
            if parsed.is_some() && next_idx < bytes.len() && bytes[next_idx] == b'$' {
                // `{:.5$}` — precision taken from the argument at index 5.
                dynamic_precision = true;
                idx = next_idx + 1;
            } else if parsed.is_none() && {
                let ident_end = scan_ident(bytes, idx);
                ident_end > idx && ident_end < bytes.len() && bytes[ident_end] == b'$'
            } {
                // `{:.name$}` — precision taken from a named argument/const.
                dynamic_precision = true;
                idx = scan_ident(bytes, idx) + 1;
            } else if parsed.is_none() {
                return Err("format precision requires digits".to_string());
            } else {
                precision = parsed;
                idx = next_idx;
            }
        }
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
        dynamic_width,
        precision,
        dynamic_precision,
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

/// Consume a `[A-Za-z_][A-Za-z0-9_]*` identifier starting at `idx`, returning
/// the end index (== `idx` if no identifier starts there). Used to recognize
/// named dynamic width/precision references (`{:name$}`, `{:.name$}`).
fn scan_ident(bytes: &[u8], mut idx: usize) -> usize {
    if idx < bytes.len() && (bytes[idx].is_ascii_alphabetic() || bytes[idx] == b'_') {
        idx += 1;
        while idx < bytes.len() && (bytes[idx].is_ascii_alphanumeric() || bytes[idx] == b'_') {
            idx += 1;
        }
    }
    idx
}

/// Attempt to recognise canonical intrinsic calls inside a generic invoke expression,
/// resolving which intrinsic (if any) by the callee's *name* — for callers (pre-HIR
/// AST normalization) that have no resolved `DefId` to consult yet. Anything that
/// resolves a real declaration first and already knows the `CallKind` from that
/// declaration's own `#[op]`/`#[intrinsic]` attribute should call
/// `build_intrinsic_call` directly instead, skipping this name lookup entirely.
pub fn intrinsic_call_from_invoke(invoke: &ExprInvoke) -> Option<ExprIntrinsicCall> {
    let kind = match &invoke.target {
        ExprInvokeTarget::Function(name) => crate::lang::lookup_op_intrinsic(name)
            .or_else(|| crate::lang::lookup_intrinsic(name))?,
        _ => return None,
    };
    build_intrinsic_call(kind, invoke)
}

/// Shapes `invoke`'s arguments the way each `CallKind` expects (e.g. `Print`/
/// `Println` need their first argument rebuilt into a `FormatString` template),
/// given an already-known `CallKind` — shared by both `intrinsic_call_from_invoke`
/// (name-resolved) and any `DefId`-resolved caller (see `hir::Program::intrinsic_defs`/
/// `op_defs`), so the two never disagree on how a given intrinsic's call is built.
pub fn build_intrinsic_call(kind: CallKind, invoke: &ExprInvoke) -> Option<ExprIntrinsicCall> {
    let call = match kind {
        CallKind::Print | CallKind::Println => {
            let (template, skip) =
                build_string_template_from_args(&invoke.args, invoke.kwargs.len())?;
            let mut args = Vec::with_capacity(1 + invoke.args.len().saturating_sub(skip));
            args.push(Expr::new(ExprKind::FormatString(template)));
            args.extend(invoke.args.iter().skip(skip).cloned());
            Some(ExprIntrinsicCall::new(kind, args, invoke.kwargs.clone()))
        }
        CallKind::Len => {
            if invoke.args.len() != 1 {
                return None;
            }
            Some(ExprIntrinsicCall::new(
                kind,
                vec![invoke.args[0].clone()],
                invoke.kwargs.clone(),
            ))
        }
        CallKind::TimeNow => {
            if !invoke.args.is_empty() {
                return None;
            }
            Some(ExprIntrinsicCall::new(
                kind,
                Vec::new(),
                invoke.kwargs.clone(),
            ))
        }
        CallKind::FsReadToString => {
            if invoke.args.len() != 1 {
                return None;
            }
            Some(ExprIntrinsicCall::new(
                kind,
                vec![invoke.args[0].clone()],
                invoke.kwargs.clone(),
            ))
        }
        CallKind::FsWriteString | CallKind::FsAppendString => {
            if invoke.args.len() != 2 {
                return None;
            }
            Some(ExprIntrinsicCall::new(
                kind,
                invoke.args.clone(),
                invoke.kwargs.clone(),
            ))
        }
        CallKind::FsExists | CallKind::FsIsDir | CallKind::FsIsFile => {
            if invoke.args.len() != 1 {
                return None;
            }
            Some(ExprIntrinsicCall::new(
                kind,
                vec![invoke.args[0].clone()],
                invoke.kwargs.clone(),
            ))
        }
        CallKind::Sleep => {
            if invoke.args.len() != 1 {
                return None;
            }
            Some(ExprIntrinsicCall::new(
                kind,
                vec![invoke.args[0].clone()],
                invoke.kwargs.clone(),
            ))
        }
        CallKind::Spawn => {
            if invoke.args.len() != 1 {
                return None;
            }
            Some(ExprIntrinsicCall::new(
                kind,
                vec![invoke.args[0].clone()],
                invoke.kwargs.clone(),
            ))
        }
        CallKind::Join => {
            if invoke.args.is_empty() {
                return None;
            }
            Some(ExprIntrinsicCall::new(
                kind,
                invoke.args.clone(),
                invoke.kwargs.clone(),
            ))
        }
        CallKind::Select => {
            if invoke.args.len() < 2 {
                return None;
            }
            Some(ExprIntrinsicCall::new(
                kind,
                invoke.args.clone(),
                invoke.kwargs.clone(),
            ))
        }
        CallKind::CatchUnwind => Some(ExprIntrinsicCall::new(
            kind,
            invoke.args.clone(),
            invoke.kwargs.clone(),
        )),
        CallKind::CatchUnwindResult => Some(ExprIntrinsicCall::new(
            kind,
            invoke.args.clone(),
            invoke.kwargs.clone(),
        )),
        CallKind::ProcMacroTokenStreamFromStr => {
            if invoke.args.len() != 1 {
                return None;
            }
            Some(ExprIntrinsicCall::new(
                kind,
                vec![invoke.args[0].clone()],
                invoke.kwargs.clone(),
            ))
        }
        CallKind::ProcMacroTokenStreamToString => {
            if invoke.args.len() != 1 {
                return None;
            }
            Some(ExprIntrinsicCall::new(
                kind,
                vec![invoke.args[0].clone()],
                invoke.kwargs.clone(),
            ))
        }
        CallKind::TypeOf => {
            if invoke.args.len() != 1 || !invoke.kwargs.is_empty() {
                return None;
            }
            Some(ExprIntrinsicCall::new(
                kind,
                vec![invoke.args[0].clone()],
                Vec::new(),
            ))
        }
        CallKind::Format => None,
        CallKind::CreateStruct => {
            if invoke.args.len() != 1 {
                return None;
            }
            Some(ExprIntrinsicCall::new(
                kind,
                vec![invoke.args[0].clone()],
                invoke.kwargs.clone(),
            ))
        }
        CallKind::AddField => {
            if invoke.args.len() != 3 {
                return None;
            }
            Some(ExprIntrinsicCall::new(
                kind,
                invoke.args.clone(),
                invoke.kwargs.clone(),
            ))
        }
        CallKind::DebugAssertions
        | CallKind::Input
        | CallKind::Panic
        | CallKind::Slice
        | CallKind::Yield
        | CallKind::SizeOf
        | CallKind::ReflectFields
        | CallKind::HasMethod
        | CallKind::TypeName
        | CallKind::BuildType
        | CallKind::CloneStruct
        | CallKind::HasField
        | CallKind::FieldCount
        | CallKind::MethodCount
        | CallKind::FieldType
        | CallKind::VecType
        | CallKind::FieldNameAt
        | CallKind::StructSize
        | CallKind::GenerateMethod
        | CallKind::CompileError
        | CallKind::CompileWarning
        | CallKind::FsReadDir
        | CallKind::FsWalkDir
        | CallKind::FsCreateDirAll
        | CallKind::FsRemoveFile
        | CallKind::FsRemoveDirAll
        | CallKind::FsGlob
        | CallKind::EnvCurrentDir
        | CallKind::EnvTempDir
        | CallKind::EnvHomeDir
        | CallKind::EnvVar
        | CallKind::EnvVarExists
        | CallKind::PathJoin
        | CallKind::PathParent
        | CallKind::PathFileName
        | CallKind::PathExtension
        | CallKind::PathStem
        | CallKind::PathIsAbsolute
        | CallKind::PathNormalize
        | CallKind::IoReadStdinToString
        | CallKind::IoWriteStdout
        | CallKind::IoWriteStderr
        | CallKind::YamlToJson
        | CallKind::JsonParse
        | CallKind::TestCommandMockReset
        | CallKind::TestCommandMockPush
        | CallKind::TestCommandMockTakeCalls
        | CallKind::TestCommandMockApply => None,
        CallKind::ShellExec
        | CallKind::ShellFileCopy
        | CallKind::ShellFileTemplate
        | CallKind::ShellFileRsync => None,
        // Portable ops: only the constructor-shaped ones (recognized here by
        // canonical name, since `PortableOp` is no longer a matchable closed
        // enum) rebuild with their args cloned; every other portable op
        // needs a real receiver/typed context this pre-typecheck,
        // name-resolved path doesn't have, so it defers (`None`) to the
        // typed `DefId`-resolved path instead (see `build_intrinsic_call`'s
        // doc comment).
        CallKind::Op(ref op)
            if matches!(
                op.name(),
                "option_some" | "option_none" | "option_unwrap" | "result_ok" | "result_err"
                    | "vec_new" | "clone"
            ) =>
        {
            Some(ExprIntrinsicCall::new(kind, invoke.args.clone(), invoke.kwargs.clone()))
        }
        CallKind::Op(_) => None,
        CallKind::Intrinsic(_) => None,
    }?;
    Some(call)
}

fn build_string_template_from_args(
    args: &[Expr],
    kwargs_len: usize,
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
                if args.len() == 1 && kwargs_len == 0 {
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
                let looks_like_format_template = template.contains('{') || template.contains('%');
                if looks_like_format_template {
                    if let Ok(parts) = parse_format_template(&template) {
                        return Some((ExprStringTemplate { parts }, 1));
                    }
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

pub fn parse_format_template(template: &str) -> Result<Vec<FormatTemplatePart>, String> {
    let mut parts = Vec::new();
    let mut current_literal = String::new();
    let mut chars = template.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch == '{' {
            if matches!(chars.peek(), Some('{')) {
                chars.next();
                current_literal.push('{');
                continue;
            }
            if !current_literal.is_empty() {
                parts.push(FormatTemplatePart::Literal(current_literal.clone()));
                current_literal.clear();
            }
            if matches!(chars.peek(), Some('}')) {
                chars.next();
                parts.push(FormatTemplatePart::Placeholder(FormatPlaceholder {
                    arg_ref: FormatArgRef::Implicit,
                    format_spec: None,
                }));
                continue;
            }
            let mut placeholder_content = String::new();
            while let Some(inner_ch) = chars.next() {
                if inner_ch == '}' {
                    break;
                }
                placeholder_content.push(inner_ch);
            }
            let placeholder = parse_placeholder_content(&placeholder_content)?;
            parts.push(FormatTemplatePart::Placeholder(placeholder));
            continue;
        }
        if ch == '}' {
            if matches!(chars.peek(), Some('}')) {
                chars.next();
                current_literal.push('}');
                continue;
            }
            current_literal.push('}');
            continue;
        }
        if ch == '%' {
            if matches!(chars.peek(), Some('%')) {
                chars.next();
                current_literal.push('%');
                continue;
            }

            if !current_literal.is_empty() {
                parts.push(FormatTemplatePart::Literal(current_literal.clone()));
                current_literal.clear();
            }

            let mut spec = String::new();
            while let Some(&next) = chars.peek() {
                spec.push(next);
                chars.next();
                if next.is_ascii_alphabetic() {
                    break;
                }
            }
            if spec.is_empty() {
                spec.push('s');
            }
            parts.push(FormatTemplatePart::Placeholder(FormatPlaceholder {
                arg_ref: FormatArgRef::Implicit,
                format_spec: Some(FormatSpec::parse(&format!("%{}", spec))?),
            }));
            continue;
        }

        current_literal.push(ch);
    }

    if !current_literal.is_empty() {
        parts.push(FormatTemplatePart::Literal(current_literal));
    }

    Ok(parts)
}

fn parse_placeholder_content(content: &str) -> Result<FormatPlaceholder, String> {
    if content.is_empty() {
        return Ok(FormatPlaceholder {
            arg_ref: FormatArgRef::Implicit,
            format_spec: None,
        });
    }

    if let Some(colon_pos) = content.find(':') {
        let arg_part = &content[..colon_pos];
        let format_spec = &content[colon_pos + 1..];

        let arg_ref = if arg_part.is_empty() {
            FormatArgRef::Implicit
        } else if let Ok(index) = arg_part.parse::<usize>() {
            FormatArgRef::Positional(index)
        } else {
            FormatArgRef::Named(arg_part.to_string())
        };

        Ok(FormatPlaceholder {
            arg_ref,
            format_spec: Some(FormatSpec::parse(format_spec)?),
        })
    } else if let Ok(index) = content.parse::<usize>() {
        Ok(FormatPlaceholder {
            arg_ref: FormatArgRef::Positional(index),
            format_spec: None,
        })
    } else {
        Ok(FormatPlaceholder {
            arg_ref: FormatArgRef::Named(content.to_string()),
            format_spec: None,
        })
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
    pub struct ExprWith {
        #[serde(default)]
        pub span: Span,
        pub context: BExpr,
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
        #[serde(default)]
        pub collected_items: ItemChunk,
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
        #[serde(default)]
        pub catches: Vec<ExprTryCatch>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub elze: Option<BExpr>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub finally: Option<BExpr>,
    }
}

common_struct! {
    pub struct ExprTryCatch {
        #[serde(default)]
        pub span: Span,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub pat: Option<BPattern>,
        pub body: BExpr,
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

#[cfg(test)]
mod format_spec_tests {
    use super::*;

    #[test]
    fn parses_literal_width_and_precision() {
        let spec = parse_rust_format_spec("5.2?").unwrap();
        assert_eq!(spec.width, Some(5));
        assert!(!spec.dynamic_width);
        assert_eq!(spec.precision, Some(2));
        assert!(!spec.dynamic_precision);
        assert_eq!(spec.ty, Some('?'));
    }

    #[test]
    fn parses_numeric_dynamic_width() {
        let spec = parse_rust_format_spec("5$").unwrap();
        assert_eq!(spec.width, None);
        assert!(spec.dynamic_width);
    }

    #[test]
    fn parses_named_dynamic_width() {
        let spec = parse_rust_format_spec("name$").unwrap();
        assert_eq!(spec.width, None);
        assert!(spec.dynamic_width);
    }

    #[test]
    fn parses_numeric_dynamic_precision() {
        let spec = parse_rust_format_spec(".5$").unwrap();
        assert_eq!(spec.precision, None);
        assert!(spec.dynamic_precision);
    }

    #[test]
    fn parses_named_dynamic_precision() {
        let spec = parse_rust_format_spec(".name$").unwrap();
        assert_eq!(spec.precision, None);
        assert!(spec.dynamic_precision);
    }

    #[test]
    fn parses_reported_hex_width_case() {
        let spec = parse_rust_format_spec("HEX_WIDTH$?").unwrap();
        assert!(spec.dynamic_width);
        assert_eq!(spec.ty, Some('?'));
    }
}
