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
}

use eyre::Result;
use fp_core::ast::{
    AttrMeta, AttrMetaList, AttrMetaNameValue, AttrStyle, Attribute, BlockStmt, BlockStmtExpr,
    DecimalType, EnumTypeVariant, Expr, ExprArray, ExprArrayRepeat, ExprAssign, ExprAwait,
    ExprBinOp, ExprBlock, ExprBreak, ExprCast, ExprClosure, ExprConstBlock, ExprContinue,
    ExprField, ExprFor, ExprIf, ExprIndex, ExprIntrinsicCall, ExprInvoke, ExprInvokeTarget,
    ExprKind, ExprKwArg, ExprLoop, ExprMacro, ExprParen, ExprQuote, ExprRange, ExprRangeLimit,
    ExprReference, ExprReturn, ExprSelect, ExprSelectType, ExprSplice, ExprStringTemplate,
    ExprStruct, ExprStructural, ExprTry, ExprTryCatch, ExprTuple, ExprUnOp, ExprWhile, ExprWith,
    FormatArgRef, FormatPlaceholder, FormatSpec, FormatTemplatePart, FunctionParam,
    FunctionParamReceiver, FunctionSignature, Ident, Item, ItemDeclConst, ItemDeclFunction,
    ItemDeclType, ItemDefConst, ItemDefEnum, ItemDefFunction, ItemDefStatic, ItemDefStruct,
    ItemDefTrait, ItemDefType, ItemImpl, ItemKind, ItemMacro, ItemOpaqueType, MacroDelimiter,
    MacroGroup, MacroInvocation, MacroToken, MacroTokenTree, Module, Name, ParameterPath,
    ParameterPathSegment, Path, Pattern, PatternIdent, PatternKind, PatternQuote, PatternTuple,
    PatternTupleStruct, PatternType, PatternVariant, PatternWildcard, QuoteFragmentKind,
    QuoteItemKind, ReprOptions, StmtDefer, StmtLet, StructuralField, Ty, TypeBinaryOp,
    TypeBinaryOpKind, TypeBounds, TypeEnum, TypeFunction, TypeInt, TypePrimitive, TypeQuote,
    TypeReference, TypeSlice, TypeStruct, Value, ValueNone, Visibility,
};
use fp_core::intrinsics::IntrinsicCallKind;
use fp_core::module::path::PathPrefix;
use fp_core::ops::{BinOpKind, UnOpKind};
use fp_core::span::{FileId, Span};
use num_bigint::BigInt;
use winnow::combinator::{alt, opt, repeat};
use winnow::error::{ContextError, ErrMode};
use winnow::{ModalResult, Parser};

use crate::ast::lower_common::{decode_string_literal, split_path_prefix};
use crate::lexer::tokenizer::{strip_number_suffix, Keyword, Token, TokenKind};

mod expr;
mod items;
mod types;

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

pub fn parse_file_tokens(
    tokens: &[Token],
    file: FileId,
) -> Result<(Vec<Attribute>, Vec<Item>), DirectParseError> {
    crate::ast::items::parse_file_tokens(tokens, file)
}

fn starts_const_fn(input: &[Token]) -> bool {
    matches!(
        input,
        [first, second, ..]
            if first.kind == TokenKind::Keyword(Keyword::Const)
                && second.kind == TokenKind::Keyword(Keyword::Fn)
    )
}

fn starts_async_fn(input: &[Token]) -> bool {
    matches!(
        input,
        [first, second, ..]
            if first.kind == TokenKind::Keyword(Keyword::Async)
                && second.kind == TokenKind::Keyword(Keyword::Fn)
    )
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

fn parse_name(input: &mut &[Token]) -> ModalResult<Name> {
    let saw_root = opt(|input: &mut &[Token]| expect_symbol(input, "::"))
        .parse_next(input)?
        .is_some();
    let first = ident_like(input)?;
    let first_args = parse_optional_type_args(input)?;
    let mut segments = vec![ParameterPathSegment::new(first, first_args)];
    loop {
        let mut probe = *input;
        if expect_symbol(&mut probe, "::").is_err() {
            break;
        }
        let Ok(next) = ident_like(&mut probe) else {
            break;
        };
        let args = parse_optional_type_args(&mut probe)?;
        *input = probe;
        segments.push(ParameterPathSegment::new(next, args));
    }
    let has_args = segments.iter().any(|segment| !segment.args.is_empty());
    let idents: Vec<Ident> = segments
        .iter()
        .map(|segment| segment.ident.clone())
        .collect();
    let (prefix, plain_segments) = split_path_prefix(idents, saw_root);
    if has_args {
        let stripped_len = plain_segments.len();
        let segments = segments
            .into_iter()
            .rev()
            .take(stripped_len)
            .collect::<Vec<_>>()
            .into_iter()
            .rev()
            .collect();
        Ok(Name::parameter_path(ParameterPath::new(prefix, segments)))
    } else {
        Ok(Name::path(Path::new(prefix, plain_segments)))
    }
}

pub(crate) fn parse_module_path(input: &mut &[Token]) -> ModalResult<Path> {
    let saw_root = opt(|input: &mut &[Token]| expect_symbol(input, "::"))
        .parse_next(input)?
        .is_some();
    let mut segments = vec![ident_like(input)?];
    loop {
        let mut probe = *input;
        if expect_symbol(&mut probe, "::").is_err() {
            break;
        }
        let Ok(next) = ident_like(&mut probe) else {
            break;
        };
        *input = probe;
        segments.push(next);
    }
    let (prefix, segments) = split_path_prefix(segments, saw_root);
    Ok(Path::new(prefix, segments))
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
        Some(Token {
            kind: TokenKind::Ident | TokenKind::Keyword(_),
            lexeme,
            ..
        }) => Some(lexeme.as_str()),
        _ => None,
    }
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
    Span::new(0, token.span.start as u32, token.span.end as u32)
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
    let suffix = &raw[stripped.len()..];
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
                "i8" => TypeInt::I8,
                "i16" => TypeInt::I16,
                "i32" => TypeInt::I32,
                _ => TypeInt::I64,
            };
            Ok((
                Value::int(value),
                Some(Ty::Primitive(TypePrimitive::Int(ty))),
            ))
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
                "u8" => TypeInt::U8,
                "u16" => TypeInt::U16,
                "u32" => TypeInt::U32,
                _ => TypeInt::U64,
            };
            Ok((
                Value::uint(value),
                Some(Ty::Primitive(TypePrimitive::Int(ty))),
            ))
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
            if normalized.contains('.') {
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
        IntrinsicCallKind::Format,
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
