use fp_core::diagnostics::{Diagnostic, DiagnosticLevel, DiagnosticManager};
use std::path::{Path as FsPath, PathBuf};

const FERRO_CONTEXT: &str = "ferrophase.parser";

fn resolve_file_id(file: FileId, source: &str, source_path: Option<&FsPath>) -> FileId {
    if file != 0 {
        return file;
    }
    let path = source_path
        .map(FsPath::to_path_buf)
        .unwrap_or_else(|| PathBuf::from("<expr>"));
    fp_core::source_map::source_map().register_or_update(path, source)
}

/// Parser for the FerroPhase language.
pub struct FerroPhaseParser {
    diagnostics: std::sync::Arc<DiagnosticManager>,
}

impl Default for FerroPhaseParser {
    fn default() -> Self {
        Self {
            diagnostics: std::sync::Arc::new(DiagnosticManager::new()),
        }
    }
}

impl FerroPhaseParser {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn diagnostics(&self) -> std::sync::Arc<DiagnosticManager> {
        self.diagnostics.clone()
    }

    pub fn clear_diagnostics(&self) {
        self.diagnostics.clear();
    }

    fn record_diagnostic(&self, level: DiagnosticLevel, message: impl Into<String>) {
        let message = message.into();
        let diagnostic = match level {
            DiagnosticLevel::Error => Diagnostic::error(message),
            DiagnosticLevel::Warning => Diagnostic::warning(message),
            DiagnosticLevel::Info => Diagnostic::info(message),
        }
        .with_source_context(FERRO_CONTEXT.to_string());

        self.diagnostics.add_diagnostic(diagnostic);
    }

    fn record_error(&self, message: impl Into<String>) {
        self.record_diagnostic(DiagnosticLevel::Error, message);
    }

    fn record_error_with_span(&self, message: impl Into<String>, span: fp_core::span::Span) {
        let message = message.into();
        let diagnostic = Diagnostic::error(message)
            .with_span(span)
            .with_source_context(FERRO_CONTEXT.to_string());
        self.diagnostics.add_diagnostic(diagnostic);
    }

    fn lex_expr_tokens(
        &self,
        source: &str,
        file: FileId,
    ) -> Result<Vec<crate::lexer::tokenizer::Token>> {
        crate::lexer::tokenizer::lex(source).map_err(|err| {
            if let Some(span) = err.span() {
                let span = fp_core::span::Span::new(file, span.start as u32, span.end as u32);
                self.record_error_with_span(format!("failed to lex expression: {err}"), span);
            } else {
                self.record_error(format!("failed to lex expression: {err}"));
            }
            eyre::eyre!(err)
        })
    }

    pub fn parse_expr_ast(&self, source: &str) -> Result<Expr> {
        self.parse_expr_ast_with_file(source, 0)
    }

    pub fn parse_expr_ast_with_file(&self, source: &str, file: FileId) -> Result<Expr> {
        let file = resolve_file_id(file, source, None);
        fp_core::span::set_current_parse_file(file);
        let tokens = self.lex_expr_tokens(source, file)?;
        crate::ast::parse_expr_tokens(&tokens, file).map_err(|err| {
            if let Some(span) = err.span() {
                self.record_error_with_span(format!("failed to parse expression: {err}"), span);
            } else {
                self.record_error(format!("failed to parse expression: {err}"));
            }
            eyre::eyre!(err)
        })
    }

    pub fn parse_items_ast(&self, source: &str) -> Result<Vec<fp_core::ast::Item>> {
        self.parse_items_ast_with_file(source, 0, None)
    }

    pub fn parse_items_ast_with_file(
        &self,
        source: &str,
        file: FileId,
        source_path: Option<&FsPath>,
    ) -> Result<Vec<fp_core::ast::Item>> {
        let file = resolve_file_id(file, source, source_path);
        let tokens = crate::lexer::tokenizer::lex(source).map_err(|err| {
            if let Some(span) = err.span() {
                let span = fp_core::span::Span::new(file, span.start as u32, span.end as u32);
                self.record_error_with_span(format!("failed to lex items: {err}"), span);
            } else {
                self.record_error(format!("failed to lex items: {err}"));
            }
            eyre::eyre!(err)
        })?;
        crate::ast::parse_items_tokens(&tokens, file).map_err(|err| {
            if let Some(span) = err.span() {
                self.record_error_with_span(format!("failed to parse items: {err}"), span);
            } else {
                self.record_error(format!("failed to parse items: {err}"));
            }
            eyre::eyre!(err)
        })
    }

    pub fn parse_file_ast_with_file(
        &self,
        source: &str,
        file: FileId,
        source_path: Option<&FsPath>,
        path: PathBuf,
    ) -> Result<fp_core::ast::File> {
        let file_id = resolve_file_id(file, source, source_path);
        // So `token_span_to_span` (used throughout tokenizing/parsing below)
        // stamps every span it builds with this file instead of a
        // placeholder — see `fp_core::span::set_current_parse_file`'s doc
        // comment.
        fp_core::span::set_current_parse_file(file_id);
        let tokens = crate::lexer::tokenizer::lex(source).map_err(|err| {
            if let Some(span) = err.span() {
                let span = fp_core::span::Span::new(file_id, span.start as u32, span.end as u32);
                self.record_error_with_span(format!("failed to lex items: {err}"), span);
            } else {
                self.record_error(format!("failed to lex items: {err}"));
            }
            eyre::eyre!(err)
        })?;
        let (attrs, items) = crate::ast::parse_file_tokens(&tokens, file_id).map_err(|err| {
            if let Some(span) = err.span() {
                self.record_error_with_span(format!("failed to parse file: {err}"), span);
            } else {
                self.record_error(format!("failed to parse file: {err}"));
            }
            eyre::eyre!(err)
        })?;
        Ok(fp_core::ast::File { path, attrs, items })
    }

    /// Parse top-level content into a `ScriptBlock` directly — no `File`,
    /// no item wrapping. Mirrors `parse_file_ast_with_file` but calls
    /// `parse_script_tokens` (ordered item/let/defer/expr dispatch) instead
    /// of `parse_file_tokens` (item-or-bare-expr-only).
    pub fn parse_script_ast_with_file(&self, source: &str, file: FileId) -> Result<ScriptBlock> {
        let file_id = resolve_file_id(file, source, None);
        fp_core::span::set_current_parse_file(file_id);
        let tokens = crate::lexer::tokenizer::lex(source).map_err(|err| {
            if let Some(span) = err.span() {
                let span = fp_core::span::Span::new(file_id, span.start as u32, span.end as u32);
                self.record_error_with_span(format!("failed to lex script: {err}"), span);
            } else {
                self.record_error(format!("failed to lex script: {err}"));
            }
            eyre::eyre!(err)
        })?;
        let (_attrs, script) =
            crate::ast::parse_script_tokens(&tokens, file_id).map_err(|err| {
                if let Some(span) = err.span() {
                    self.record_error_with_span(format!("failed to parse script: {err}"), span);
                } else {
                    self.record_error(format!("failed to parse script: {err}"));
                }
                eyre::eyre!(err)
            })?;
        Ok(script)
    }
}

use eyre::Result;
use fp_core::ast::path::PathPrefix;
use fp_core::ast::{
    AngleBracketedArg, AngleBracketedArgs, AssocItemConstraint, AssocItemConstraintKind, AttrMeta,
    AttrMetaList, AttrMetaNameValue, AttrStyle, Attribute, BlockStmt, BlockStmtExpr, DecimalType,
    EnumTypeVariant, Expr, ExprArray, ExprArrayRepeat, ExprAssign, ExprAwait, ExprBinOp, ExprBlock,
    ExprBreak, ExprCast, ExprClosure, ExprConstBlock, ExprContinue, ExprField, ExprFieldAccess,
    ExprFor, ExprIf, ExprIndex, ExprIntrinsicCall, ExprInvoke, ExprInvokeTarget, ExprKind,
    ExprKwArg, ExprLoop, ExprMacro, ExprParen, ExprQuote, ExprRange, ExprRangeLimit, ExprReference,
    ExprReturn, ExprSplice, ExprStringTemplate, ExprStruct, ExprStructural, ExprTry, ExprTryCatch,
    ExprTuple, ExprUnOp, ExprWhile, ExprWith, FormatArgRef, FormatPlaceholder, FormatSpec,
    FormatTemplatePart, FunctionParam, FunctionParamReceiver, FunctionSignature, GenericArg, Ident,
    Item, ItemDeclConst, ItemDeclFunction, ItemDeclStatic, ItemDeclType, ItemDefConst, ItemDefEnum,
    ItemDefFunction, ItemDefStatic, ItemDefStruct, ItemDefTrait, ItemDefType, ItemImpl, ItemKind,
    ItemMacro, ItemOpaqueType, MacroDelimiter, MacroGroup, MacroInvocation, MacroToken,
    MacroTokenTree, Module, Name, Path, PathArguments, PathSegment, Pattern, PatternBox,
    PatternIdent, PatternKind, PatternOr, PatternQuote, PatternStruct, PatternStructural,
    PatternTuple, PatternTupleStruct, PatternType, PatternVariant, PatternWildcard,
    QuoteFragmentKind, QuoteItemKind, ReprOptions, ScriptBlock, StmtDefer, StmtLet,
    StructuralField, Term, Ty, TypeArray, TypeBinaryOp, TypeBinaryOpKind, TypeBounds, TypeEnum,
    TypeFunction, TypeInt, TypePrimitive, TypeQuote, TypeReference, TypeSlice, TypeStruct, Value,
    ValueBytes, ValueChar, ValueNone, ValueUInt, Visibility,
};
use fp_core::intrinsics::CallKind;
use fp_core::ops::{BinOpKind, UnOpKind};
use fp_core::span::{FileId, Span};
use num_bigint::BigInt;
use winnow::combinator::{alt, opt, repeat};
use winnow::error::{ContextError, ErrMode};
use winnow::{ModalResult, Parser};

use crate::ast::lower_common::{
    decode_bytes_literal, decode_single_char_literal, decode_string_literal, split_path_prefix,
    split_path_prefix_segments,
};
use crate::lexer::tokenizer::{Keyword, Token, TokenKind, strip_number_suffix};

mod expr;
mod items;
mod pattern_expansion;
mod types;

pub(crate) use expr::patterns::parse_general_pattern;
pub(crate) use expr::*;
pub(crate) use items::*;
pub(crate) use types::*;

#[derive(Debug, thiserror::Error)]
pub enum DirectParseError {
    #[error("{message}")]
    Message { message: String, span: Option<Span> },
}

impl DirectParseError {
    pub fn span(&self) -> Option<Span> {
        match self {
            Self::Message { span, .. } => *span,
        }
    }
}

pub fn parse_expr_tokens(tokens: &[Token], file: FileId) -> Result<Expr, DirectParseError> {
    let mut input = tokens;
    let expr = parse_expr_winnow(&mut input, file).map_err(|err| map_err(err, input))?;
    if !input.is_empty() {
        return Err(error_at_current(input, "trailing tokens after expression"));
    }
    Ok(expr)
}

/// Like `parse_expr_tokens`, but tolerates trailing tokens and reports how
/// many were consumed — used by `macro_rules!` fragment matching (an
/// `$x:expr` metavariable consumes exactly one expression's worth of
/// tokens out of a longer invocation stream, not the whole thing).
pub(crate) fn parse_expr_prefix_tokens(
    tokens: &[Token],
    file: FileId,
) -> Result<(Expr, usize), DirectParseError> {
    let mut input = tokens;
    let expr = parse_expr_winnow(&mut input, file).map_err(|err| map_err(err, input))?;
    let consumed = tokens.len() - input.len();
    Ok((expr, consumed))
}

pub fn parse_type_tokens(tokens: &[Token], _file: FileId) -> Result<Ty, DirectParseError> {
    let mut input = tokens;
    let ty = parse_type_expr(&mut input).map_err(|err| map_err(err, input))?;
    if !input.is_empty() {
        return Err(error_at_current(input, "trailing tokens after type"));
    }
    Ok(ty)
}

pub fn parse_type_prefix_tokens(
    tokens: &[Token],
    _file: FileId,
) -> Result<(Ty, usize), DirectParseError> {
    let mut input = tokens;
    let ty = parse_type_expr(&mut input).map_err(|err| map_err(err, input))?;
    let consumed = tokens.len() - input.len();
    Ok((ty, consumed))
}

pub fn parse_item_prefix_tokens(
    tokens: &[Token],
    file: FileId,
) -> Result<(Item, usize), DirectParseError> {
    let mut input = tokens;
    let item = parse_item_winnow(&mut input, file).map_err(|err| map_err(err, input))?;
    let consumed = tokens.len() - input.len();
    Ok((item, consumed))
}

pub fn parse_items_tokens(tokens: &[Token], file: FileId) -> Result<Vec<Item>, DirectParseError> {
    crate::ast::items::parse_items_tokens(tokens, file)
}

pub fn parse_item_tokens(tokens: &[Token], file: FileId) -> Result<Vec<Item>, DirectParseError> {
    crate::ast::items::parse_item_tokens(tokens, file)
}

pub fn parse_file_tokens(
    tokens: &[Token],
    file: FileId,
) -> Result<(Vec<Attribute>, Vec<Item>), DirectParseError> {
    crate::ast::items::parse_file_tokens(tokens, file)
}

/// True if `input` (already known to start with a `const`/`async`/`unsafe`
/// modifier keyword) leads to `fn` once every modifier keyword in the run
/// is skipped — real Rust allows these in any relative order (`const
/// unsafe fn`, `unsafe fn`, ...), not just the single fixed pair this used
/// to check (`first == X && second == Fn`), which missed e.g. `const
/// unsafe fn` (`second` is `unsafe`, not `fn`).
pub(super) fn skips_modifiers_to_fn(input: &[Token]) -> bool {
    let mut rest = input;
    loop {
        match rest.first().map(|t| &t.kind) {
            Some(TokenKind::Keyword(Keyword::Unsafe | Keyword::Async | Keyword::Const)) => {
                rest = &rest[1..];
            }
            // `unsafe extern "C" fn ...` — an ABI clause can appear in the
            // modifier run too, and must itself be skipped (string literal
            // included) before continuing to look for `fn`.
            Some(TokenKind::Keyword(Keyword::Extern)) => {
                rest = &rest[1..];
                if matches!(
                    rest.first().map(|t| &t.kind),
                    Some(TokenKind::StringLiteral)
                ) {
                    rest = &rest[1..];
                }
            }
            Some(TokenKind::Keyword(Keyword::Fn)) => return true,
            _ => return false,
        }
    }
}

/// Same idea as `skips_modifiers_to_fn`, for `trait` — real Rust allows
/// `unsafe trait`/`const unsafe trait` (the latter currently unstable,
/// but present in real `core::alloc::Allocator`) before the keyword.
pub(super) fn skips_modifiers_to_trait(input: &[Token]) -> bool {
    let mut rest = input;
    loop {
        match rest.first() {
            Some(token) if token.kind == TokenKind::Keyword(Keyword::Unsafe) => rest = &rest[1..],
            Some(token) if token.kind == TokenKind::Keyword(Keyword::Const) => rest = &rest[1..],
            // `auto trait Foo {}` (marker traits) — `auto` isn't a lexer
            // keyword (tokenizes as a plain `Ident`), unlike `unsafe`/
            // `const`.
            Some(token) if token.kind == TokenKind::Ident && token.lexeme == "auto" => {
                rest = &rest[1..]
            }
            Some(token) if token.kind == TokenKind::Keyword(Keyword::Trait) => return true,
            _ => return false,
        }
    }
}

fn starts_const_fn(input: &[Token]) -> bool {
    matches!(
        input,
        [first, ..] if first.kind == TokenKind::Keyword(Keyword::Const)
    ) && skips_modifiers_to_fn(input)
}

fn starts_async_fn(input: &[Token]) -> bool {
    matches!(
        input,
        [first, ..] if first.kind == TokenKind::Keyword(Keyword::Async)
    ) && skips_modifiers_to_fn(input)
}

fn starts_const_struct(input: &[Token]) -> bool {
    matches!(
        input,
        [first, second, ..]
            if first.kind == TokenKind::Keyword(Keyword::Const)
                && second.kind == TokenKind::Keyword(Keyword::Struct)
    )
}

fn looks_like_extern_block(input: &[Token]) -> bool {
    matches!(
        input,
        [first, second, third, ..]
            if first.kind == TokenKind::Keyword(Keyword::Extern)
                && second.kind == TokenKind::StringLiteral
                && third.kind == TokenKind::Symbol
                && third.lexeme == "{"
    )
}

fn looks_like_unsafe_extern_block(input: &[Token]) -> bool {
    matches!(
        input,
        [first, second, third, fourth, ..]
            if first.kind == TokenKind::Keyword(Keyword::Unsafe)
                && second.kind == TokenKind::Keyword(Keyword::Extern)
                && third.kind == TokenKind::StringLiteral
                && fourth.kind == TokenKind::Symbol
                && fourth.lexeme == "{"
    )
}

fn starts_unsafe_extern_block(input: &[Token]) -> bool {
    match input {
        [first, second, third, fourth, ..]
            if first.kind == TokenKind::Keyword(Keyword::Unsafe)
                && second.kind == TokenKind::Keyword(Keyword::Extern)
                && third.kind == TokenKind::StringLiteral
                && fourth.kind == TokenKind::Symbol
                && fourth.lexeme == "{" =>
        {
            true
        }
        [first, ..] if first.kind == TokenKind::Symbol && first.lexeme == "#" => {
            let mut probe = input;
            while matches!(probe.first(), Some(token) if token.kind == TokenKind::Symbol && token.lexeme == "#")
            {
                let mut attr_probe = probe;
                if crate::ast::items::parse_outer_attrs(&mut attr_probe, 0).is_err() {
                    return false;
                }
                probe = attr_probe;
            }
            looks_like_unsafe_extern_block(probe)
        }
        _ => false,
    }
}

fn parse_name(input: &mut &[Token]) -> ModalResult<Name> {
    let original = *input;
    let saw_root = opt(|input: &mut &[Token]| expect_symbol(input, "::"))
        .parse_next(input)?
        .is_some();
    let first = ident_like(input)?;
    let first_args = parse_optional_path_arguments(input)?;
    let mut segments = vec![PathSegment::with_arguments(first, first_args)];
    loop {
        let mut probe = *input;
        if skip_symbol(&mut probe, "::").is_err() {
            break;
        }
        if peek_symbol(probe) == Some("<") {
            let args = parse_optional_path_arguments(&mut probe)?;
            if let Some(segment) = segments.last_mut() {
                segment.arguments = args.map(Box::new);
            }
            *input = probe;
            continue;
        }
        let Ok(next) = ident_like(&mut probe) else {
            break;
        };
        let args = parse_optional_path_arguments(&mut probe)?;
        *input = probe;
        segments.push(PathSegment::with_arguments(next, args));
    }
    let (prefix, segments) = split_path_prefix_segments(segments, saw_root);
    let consumed = original.len().saturating_sub(input.len());
    let span = original
        .get(..consumed)
        .and_then(|tokens| tokens.first().zip(tokens.last()))
        .map(|(first, last)| Span::union([token_span_to_span(first), token_span_to_span(last)]))
        .unwrap_or_else(Span::null);
    Ok(Name::path(Path::with_span(span, prefix, segments)))
}

pub(crate) fn parse_optional_path_arguments(
    input: &mut &[Token],
) -> ModalResult<Option<PathArguments>> {
    let mut probe = *input;
    match parse_path_arguments_inner(&mut probe) {
        Ok(arguments) => {
            *input = probe;
            Ok(Some(arguments))
        }
        Err(_) => Ok(None),
    }
}

fn parse_path_arguments_inner(input: &mut &[Token]) -> ModalResult<PathArguments> {
    let original = *input;
    let mut probe = *input;
    if !try_eat_symbol(&mut probe, "<") {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }
    let mut args = Vec::new();
    if peek_symbol(probe) != Some(">") {
        loop {
            let mut item_probe = probe;
            if let Some(name) = peek_ident_like(item_probe) {
                let ident = ident_like(&mut item_probe)?;
                if name.starts_with('\'') {
                    probe = item_probe;
                    args.push(AngleBracketedArg::Arg(GenericArg::Lifetime(
                        name.to_owned(),
                    )));
                } else {
                    // Rustc's `AssocItemConstraint` retains generic arguments
                    // on the constrained item (`Item<'a> = T` and
                    // `Item<'a>: Bound`). Probe the arguments together with
                    // the constraint marker; if neither marker follows, this
                    // is an ordinary positional type argument and the outer
                    // type parser handles the complete path expression.
                    let mut constraint_probe = item_probe;
                    let gen_args = parse_optional_path_arguments(&mut constraint_probe)?;
                    if skip_symbol(&mut constraint_probe, "=").is_ok() {
                        let term = parse_assoc_item_term(&mut constraint_probe)?;
                        probe = constraint_probe;
                        args.push(AngleBracketedArg::Constraint(AssocItemConstraint {
                            ident,
                            gen_args,
                            kind: AssocItemConstraintKind::Equality { term },
                        }));
                    } else if skip_symbol(&mut constraint_probe, ":").is_ok() {
                        let bounds = parse_type_bounds(&mut constraint_probe)?;
                        probe = constraint_probe;
                        args.push(AngleBracketedArg::Constraint(AssocItemConstraint {
                            ident,
                            gen_args,
                            kind: AssocItemConstraintKind::Bound {
                                bounds: bounds
                                    .bounds
                                    .into_iter()
                                    .map(|bound| Ty::Expr(Box::new(bound)))
                                    .collect(),
                            },
                        }));
                    } else {
                        let ty = parse_type_expr(&mut probe)?;
                        args.push(AngleBracketedArg::Arg(GenericArg::from_ty(ty)));
                    }
                }
            } else if input_is_const_argument(probe) {
                let expr = parse_cast_no_struct(&mut probe, 0)?;
                args.push(AngleBracketedArg::Arg(GenericArg::Const(Box::new(expr))));
            } else {
                let ty = parse_type_expr(&mut probe)?;
                args.push(AngleBracketedArg::Arg(GenericArg::from_ty(ty)));
            }
            let mut comma_probe = probe;
            if skip_symbol(&mut comma_probe, ",").is_err() {
                break;
            }
            if peek_symbol(comma_probe) == Some(">") {
                probe = comma_probe;
                break;
            }
            probe = comma_probe;
        }
    }
    skip_symbol(&mut probe, ">")?;
    *input = probe;
    let consumed = original.len().saturating_sub(input.len());
    let span = original
        .get(..consumed)
        .and_then(|tokens| tokens.first().zip(tokens.last()))
        .map(|(first, last)| Span::union([token_span_to_span(first), token_span_to_span(last)]))
        .unwrap_or_else(Span::null);
    Ok(PathArguments::AngleBracketed(AngleBracketedArgs {
        span,
        args,
    }))
}

/// Parse the equality side of an associated-item constraint while retaining
/// whether it is a type or a const term. Bare paths remain type terms because
/// their namespace is resolved later; literal and block expressions are
/// unambiguously const terms at this stage.
fn parse_assoc_item_term(input: &mut &[Token]) -> ModalResult<Term> {
    if peek_symbol(*input) == Some("{") {
        skip_symbol(input, "{")?;
        let expr = parse_expr_winnow_no_struct(input, 0)?;
        skip_symbol(input, "}")?;
        return Ok(Term::Const(Box::new(expr)));
    }
    if matches!(input.first(), Some(token) if token.kind == TokenKind::Number) {
        return parse_number(input).map(|expr| Term::Const(Box::new(expr)));
    }
    if matches!(input.first(), Some(token) if token.kind == TokenKind::StringLiteral) {
        return parse_string(input, 0).map(|expr| Term::Const(Box::new(expr)));
    }
    if matches!(peek_ident_like(*input), Some("true" | "false" | "null")) {
        return parse_name_expr(input).map(|expr| Term::Const(Box::new(expr)));
    }
    let ty = parse_type_expr(input)?;
    let is_const = matches!(
        &ty,
        Ty::Expr(expr)
            if matches!(
                expr.kind(),
                ExprKind::Value(value)
                    if matches!(
                        value.as_ref(),
                        Value::Int(_)
                            | Value::UInt(_)
                            | Value::BigInt(_)
                            | Value::Bool(_)
                            | Value::Decimal(_)
                            | Value::BigDecimal(_)
                            | Value::Char(_)
                            | Value::String(_)
                            | Value::Bytes(_)
                            | Value::Unit(_)
                            | Value::Null(_)
                            | Value::None(_)
                    )
            )
    );
    if is_const {
        let Ty::Expr(expr) = ty else {
            unreachable!("const term classification requires an expression type")
        };
        Ok(Term::Const(expr))
    } else {
        Ok(Term::Ty(Box::new(ty)))
    }
}

fn input_is_const_argument(input: &[Token]) -> bool {
    input.first().is_some_and(|token| {
        token.kind == TokenKind::Number || token.kind == TokenKind::StringLiteral
    }) || matches!(peek_ident_like(input), Some("true" | "false" | "null"))
        || peek_symbol(input) == Some("{")
}

pub(crate) fn parse_module_path(input: &mut &[Token]) -> ModalResult<Path> {
    let original = *input;
    let saw_root = opt(|input: &mut &[Token]| expect_symbol(input, "::"))
        .parse_next(input)?
        .is_some();
    let mut segments = vec![ident_like(input)?];
    loop {
        let mut probe = *input;
        if skip_symbol(&mut probe, "::").is_err() {
            break;
        }
        let Ok(next) = ident_like(&mut probe) else {
            break;
        };
        *input = probe;
        segments.push(next);
    }
    let (prefix, segments) = split_path_prefix(segments, saw_root);
    let consumed = original.len().saturating_sub(input.len());
    let span = original
        .get(..consumed)
        .and_then(|tokens| tokens.first().zip(tokens.last()))
        .map(|(first, last)| Span::union([token_span_to_span(first), token_span_to_span(last)]))
        .unwrap_or_else(Span::null);
    Ok(Path::with_span(
        span,
        prefix,
        segments.into_iter().map(Into::into).collect(),
    ))
}

fn token_kind(input: &mut &[Token], kind: TokenKind) -> ModalResult<Token> {
    match input.split_first() {
        Some((token, rest)) if token.kind == kind => {
            *input = rest;
            Ok(token.clone())
        }
        _ => Err(ErrMode::Backtrack(ContextError::new())),
    }
}

fn expect_keyword(input: &mut &[Token], expected: Keyword) -> ModalResult<Token> {
    match input.split_first() {
        Some((token, rest)) if token.kind == TokenKind::Keyword(expected) => {
            *input = rest;
            Ok(token.clone())
        }
        _ => Err(ErrMode::Backtrack(ContextError::new())),
    }
}

fn expect_symbol(input: &mut &[Token], expected: &str) -> ModalResult<Token> {
    match input.split_first() {
        Some((token, rest))
            if token.kind == TokenKind::Symbol && token.lexeme.as_str() == expected =>
        {
            *input = rest;
            Ok(token.clone())
        }
        _ => Err(ErrMode::Backtrack(ContextError::new())),
    }
}

/// Non-cloning counterpart to `expect_keyword`, for the large majority of
/// call sites that only consume the token as a bare statement or check
/// `.is_ok()`/`.is_err()` and never need the matched `Token` (whose
/// `lexeme: String` field `expect_keyword` would otherwise heap-clone for
/// nothing, once per token consumed across every file compiled, including
/// embedded std/libc).
fn skip_keyword(input: &mut &[Token], expected: Keyword) -> ModalResult<()> {
    match input.split_first() {
        Some((token, rest)) if token.kind == TokenKind::Keyword(expected) => {
            *input = rest;
            Ok(())
        }
        _ => Err(ErrMode::Backtrack(ContextError::new())),
    }
}

/// Non-cloning counterpart to `expect_symbol` — see `skip_keyword`'s doc
/// comment.
fn skip_symbol(input: &mut &[Token], expected: &str) -> ModalResult<()> {
    match input.split_first() {
        Some((token, rest))
            if token.kind == TokenKind::Symbol && token.lexeme.as_str() == expected =>
        {
            *input = rest;
            Ok(())
        }
        _ => Err(ErrMode::Backtrack(ContextError::new())),
    }
}

/// `skip_symbol`, but as a plain bool instead of a `ModalResult` — for
/// call sites that branch on presence/absence rather than backtrack on
/// mismatch (the common `skip_symbol(input, "x").is_ok()` idiom, spelled
/// out as its own named primitive instead of repeated inline). Mirrors
/// `rustc_parse`'s own `Parser::eat` (bump-if-present, no error path).
///
/// For `expected == "<"` specifically, this also glues apart a `<<`
/// token when the single `<` alone doesn't match — real generic-argument
/// position routinely nests two openers back to back (e.g. `Option<<I
/// as Iterator>::Item>`), which the tokenizer has no way to tell apart
/// from a real `<<` shift-left operator at lex time (unlike `>>`, which
/// it already splits based on bracket-nesting depth, `<` nesting isn't
/// known until the *second* `<` is reached). Splitting only on demand,
/// only for this one caller-selected symbol, means shift-left itself
/// (looked up as a single `<<` token) is never affected.
pub(super) fn try_eat_symbol(input: &mut &[Token], expected: &str) -> bool {
    if skip_symbol(input, expected).is_ok() {
        return true;
    }
    if expected == "<" {
        return try_split_leading_double_angle(input);
    }
    false
}

/// See `try_eat_symbol`'s doc comment. Consumes a leading `<<` token and
/// replaces `*input` with a synthetic single `<` (covering the token's
/// second character) followed by the rest of the original stream —
/// leaked as `'static` since this compiler processes one input per
/// process and never accumulates these across a long-running session.
fn try_split_leading_double_angle(input: &mut &[Token]) -> bool {
    let Some((first, rest)) = input.split_first() else {
        return false;
    };
    if first.kind != TokenKind::Symbol || first.lexeme != "<<" {
        return false;
    }
    let synthetic = Token {
        kind: TokenKind::Symbol,
        lexeme: "<".to_string(),
        raw_identifier: false,
        span: crate::lexer::tokenizer::Span {
            start: first.span.start + 1,
            end: first.span.end,
        },
    };
    let combined: Vec<Token> = std::iter::once(synthetic)
        .chain(rest.iter().cloned())
        .collect();
    *input = Box::leak(combined.into_boxed_slice());
    true
}

fn ident_like(input: &mut &[Token]) -> ModalResult<Ident> {
    match input.split_first() {
        Some((token, rest)) if matches!(token.kind, TokenKind::Ident | TokenKind::Keyword(_)) => {
            *input = rest;
            Ok(Ident::new(token.lexeme.clone()))
        }
        _ => Err(ErrMode::Backtrack(ContextError::new())),
    }
}

fn peek_symbol(input: &[Token]) -> Option<&str> {
    match input.first() {
        Some(token) if token.kind == TokenKind::Symbol => Some(token.lexeme.as_str()),
        _ => None,
    }
}

fn type_to_expr(ty: &Ty) -> Expr {
    match ty {
        Ty::Expr(expr) => (**expr).clone(),
        other => Expr::value(Value::Type(other.clone())),
    }
}

fn peek_ident_like(input: &[Token]) -> Option<&str> {
    match input.first() {
        Some(token) if matches!(token.kind, TokenKind::Ident | TokenKind::Keyword(_)) => {
            Some(token.lexeme.as_str())
        }
        _ => None,
    }
}

fn peek_binary_op(input: &[Token]) -> Option<(&str, u8, BinOpKind)> {
    if matches!(
        input,
        [first, second, ..]
            if first.kind == TokenKind::Symbol
                && first.lexeme == ">"
                && second.kind == TokenKind::Symbol
                && second.lexeme == ">"
    ) {
        let (prec, kind) = binary_op(">>")?;
        return Some((">>", prec, kind));
    }
    let op = peek_symbol(input)?;
    let (prec, kind) = binary_op(op)?;
    Some((op, prec, kind))
}

fn consume_binary_op(input: &mut &[Token], op: &str) -> ModalResult<()> {
    if op == ">>"
        && matches!(
            *input,
            [first, second, ..]
                if first.kind == TokenKind::Symbol
                    && first.lexeme == ">"
                    && second.kind == TokenKind::Symbol
                    && second.lexeme == ">"
        )
    {
        *input = &input[2..];
        return Ok(());
    }
    skip_symbol(input, op)?;
    Ok(())
}

fn binary_op(symbol: &str) -> Option<(u8, BinOpKind)> {
    Some(match symbol {
        "||" => (1, BinOpKind::Or),
        "&&" => (2, BinOpKind::And),
        "==" => (3, BinOpKind::Eq),
        "!=" => (3, BinOpKind::Ne),
        "<" => (4, BinOpKind::Lt),
        "<=" => (4, BinOpKind::Le),
        ">" => (4, BinOpKind::Gt),
        ">=" => (4, BinOpKind::Ge),
        "|" => (5, BinOpKind::BitOr),
        "^" => (6, BinOpKind::BitXor),
        "&" => (7, BinOpKind::BitAnd),
        "<<" => (8, BinOpKind::Shl),
        ">>" => (8, BinOpKind::Shr),
        "+" => (9, BinOpKind::Add),
        "-" => (9, BinOpKind::Sub),
        "*" => (10, BinOpKind::Mul),
        "/" => (10, BinOpKind::Div),
        "%" => (10, BinOpKind::Mod),
        _ => return None,
    })
}

fn map_err(err: ErrMode<ContextError>, input: &[Token]) -> DirectParseError {
    let message = match err {
        ErrMode::Backtrack(_) | ErrMode::Cut(_) => "failed to parse expression directly",
        ErrMode::Incomplete(_) => "incomplete expression input",
    };
    error_at_current(input, message)
}

fn error_at_current(input: &[Token], message: impl Into<String>) -> DirectParseError {
    DirectParseError::Message {
        message: message.into(),
        span: input.first().map(token_span_to_span),
    }
}

fn token_span_to_span(token: &Token) -> Span {
    Span::new(
        fp_core::span::current_parse_file(),
        token.span.start as u32,
        token.span.end as u32,
    )
}

fn span_from_expr(expr: &Expr) -> Span {
    expr.span()
}

fn union_spans(a: Span, b: Span) -> Span {
    Span::union([a, b])
}

fn union_exprs(a: &Expr, b: &Expr) -> Span {
    union_spans(span_from_expr(a), span_from_expr(b))
}

fn parse_numeric_literal_local(raw: &str) -> std::result::Result<(Value, Option<Ty>), ()> {
    let stripped = strip_number_suffix(raw);
    let normalized = stripped.replace('_', "");
    let suffix = raw[stripped.len()..]
        .strip_prefix('_')
        .unwrap_or(&raw[stripped.len()..]);
    match suffix {
        "ib" => {
            if normalized.contains('.') {
                return Err(());
            }
            let value = parse_big_int_literal(&normalized).ok_or(())?;
            Ok((
                Value::big_int(value),
                Some(Ty::Primitive(TypePrimitive::Int(TypeInt::BigInt))),
            ))
        }
        "fb" => {
            let value = normalized.parse::<f64>().map_err(|_| ())?;
            Ok((
                Value::decimal(value),
                Some(Ty::Primitive(TypePrimitive::Decimal(
                    DecimalType::BigDecimal,
                ))),
            ))
        }
        "i8" | "i16" | "i32" | "i64" | "isize" => {
            if normalized.contains('.') {
                return Err(());
            }
            let value = parse_i64_literal(&normalized).ok_or(())?;
            let ty = match suffix {
                "isize" => Ty::path(Path::plain(vec![Ident::new("isize")])),
                "i8" => Ty::Primitive(TypePrimitive::Int(TypeInt::I8)),
                "i16" => Ty::Primitive(TypePrimitive::Int(TypeInt::I16)),
                "i32" => Ty::Primitive(TypePrimitive::Int(TypeInt::I32)),
                _ => Ty::Primitive(TypePrimitive::Int(TypeInt::I64)),
            };
            Ok((Value::int(value), Some(ty)))
        }
        "i128" => {
            if normalized.contains('.') {
                return Err(());
            }
            let value = parse_big_int_literal(&normalized).ok_or(())?;
            Ok((
                Value::big_int(value),
                Some(Ty::Primitive(TypePrimitive::Int(TypeInt::I128))),
            ))
        }
        "u8" | "u16" | "u32" | "u64" | "usize" => {
            if normalized.contains('.') {
                return Err(());
            }
            let value = parse_u64_literal(&normalized).ok_or(())?;
            let ty = match suffix {
                "usize" => Ty::path(Path::plain(vec![Ident::new("usize")])),
                "u8" => Ty::Primitive(TypePrimitive::Int(TypeInt::U8)),
                "u16" => Ty::Primitive(TypePrimitive::Int(TypeInt::U16)),
                "u32" => Ty::Primitive(TypePrimitive::Int(TypeInt::U32)),
                _ => Ty::Primitive(TypePrimitive::Int(TypeInt::U64)),
            };
            Ok((Value::uint(value), Some(ty)))
        }
        "u128" => {
            if normalized.contains('.') {
                return Err(());
            }
            let value = parse_big_int_literal(&normalized).ok_or(())?;
            Ok((
                Value::big_int(value),
                Some(Ty::Primitive(TypePrimitive::Int(TypeInt::U128))),
            ))
        }
        "f32" | "f64" => {
            let value = normalized.parse::<f64>().map_err(|_| ())?;
            let ty = if suffix == "f32" {
                DecimalType::F32
            } else {
                DecimalType::F64
            };
            Ok((
                Value::decimal(value),
                Some(Ty::Primitive(TypePrimitive::Decimal(ty))),
            ))
        }
        _ => {
            // A decimal exponent (`1e0`, `6.022e23`, real
            // `core::num::imp::dec2flt`'s own lookup tables) makes this a
            // float even with no `.` in sight — `contains('.')` alone
            // missed the mantissa-less case. Only decimal literals can
            // have one: `0xE0`'s `E` is an ordinary hex digit, not an
            // exponent marker, so radix-prefixed literals are excluded.
            let is_radix_prefixed = normalized.starts_with("0x")
                || normalized.starts_with("0X")
                || normalized.starts_with("0o")
                || normalized.starts_with("0O")
                || normalized.starts_with("0b")
                || normalized.starts_with("0B");
            let has_exponent =
                !is_radix_prefixed && normalized.chars().any(|c| c == 'e' || c == 'E');
            if normalized.contains('.') || has_exponent {
                let d = normalized.parse::<f64>().map_err(|_| ())?;
                Ok((Value::decimal(d), None))
            } else if let Some(i) = parse_i64_literal(&normalized) {
                Ok((Value::int(i), None))
            } else {
                let big = parse_big_int_literal(&normalized).ok_or(())?;
                Ok((
                    Value::big_int(big),
                    Some(Ty::Primitive(TypePrimitive::Int(TypeInt::BigInt))),
                ))
            }
        }
    }
}

fn parse_i64_literal(raw: &str) -> Option<i64> {
    let (digits, radix) = integer_digits_and_radix(raw)?;
    i64::from_str_radix(digits, radix).ok()
}

fn parse_u64_literal(raw: &str) -> Option<u64> {
    let (digits, radix) = integer_digits_and_radix(raw)?;
    u64::from_str_radix(digits, radix).ok()
}

fn parse_big_int_literal(raw: &str) -> Option<BigInt> {
    let (digits, radix) = integer_digits_and_radix(raw)?;
    BigInt::parse_bytes(digits.as_bytes(), radix)
}

fn integer_digits_and_radix(raw: &str) -> Option<(&str, u32)> {
    if raw.is_empty() {
        return None;
    }
    if let Some(digits) = raw.strip_prefix("0x").or_else(|| raw.strip_prefix("0X")) {
        return Some((digits, 16));
    }
    if let Some(digits) = raw.strip_prefix("0o").or_else(|| raw.strip_prefix("0O")) {
        return Some((digits, 8));
    }
    if let Some(digits) = raw.strip_prefix("0b").or_else(|| raw.strip_prefix("0B")) {
        return Some((digits, 2));
    }
    Some((raw, 10))
}

fn parse_f_string_literal_local(
    raw: &str,
    file: FileId,
) -> std::result::Result<Expr, DirectParseError> {
    let Some(decoded) = strip_string_prefix(raw, "f") else {
        return Err(DirectParseError::Message {
            message: "invalid f-string literal".to_string(),
            span: None,
        });
    };
    let (template, args) = parse_f_string_template_local(&decoded, file)?;
    let mut call_args = Vec::with_capacity(1 + args.len());
    call_args.push(Expr::new(ExprKind::FormatString(template)));
    call_args.extend(args);
    Ok(ExprKind::IntrinsicCall(ExprIntrinsicCall::new(
        CallKind::Format,
        call_args,
        Vec::new(),
    ))
    .into())
}

fn strip_string_prefix(raw: &str, prefix: &str) -> Option<String> {
    if !raw.starts_with(prefix) {
        return None;
    }
    let rest = &raw[prefix.len()..];
    decode_string_literal(rest)
}

fn parse_f_string_template_local(
    input: &str,
    file: FileId,
) -> std::result::Result<(ExprStringTemplate, Vec<Expr>), DirectParseError> {
    let mut parts = Vec::new();
    let mut args = Vec::new();
    let mut current_literal = String::new();
    let mut chars = input.chars().peekable();
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
            let mut placeholder = String::new();
            let mut found_end = false;
            for inner in chars.by_ref() {
                if inner == '}' {
                    found_end = true;
                    break;
                }
                placeholder.push(inner);
            }
            if !found_end {
                return Err(DirectParseError::Message {
                    message: "unterminated f-string placeholder".to_string(),
                    span: None,
                });
            }
            let trimmed = placeholder.trim();
            if trimmed.is_empty() {
                return Err(DirectParseError::Message {
                    message: "empty f-string placeholder".to_string(),
                    span: None,
                });
            }
            let (expr_src, format_spec) = match trimmed.split_once(':') {
                Some((expr_part, spec_part)) => (expr_part.trim(), Some(spec_part.trim())),
                None => (trimmed, None),
            };
            let expr = parse_expr_tokens(
                &crate::lexer::tokenizer::lex(expr_src).map_err(|_| DirectParseError::Message {
                    message: "failed to lex f-string placeholder".to_string(),
                    span: None,
                })?,
                file,
            )?;
            args.push(expr);
            parts.push(FormatTemplatePart::Placeholder(FormatPlaceholder {
                arg_ref: FormatArgRef::Implicit,
                format_spec: format_spec
                    .filter(|s| !s.is_empty())
                    .map(|s| FormatSpec::parse(s))
                    .transpose()
                    .map_err(|err| DirectParseError::Message {
                        message: format!("invalid format spec: {err}"),
                        span: None,
                    })?,
            }));
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
        current_literal.push(ch);
    }
    if !current_literal.is_empty() {
        parts.push(FormatTemplatePart::Literal(current_literal));
    }
    Ok((ExprStringTemplate { parts }, args))
}

pub(crate) mod lower_common;

#[cfg(test)]
mod tests;
