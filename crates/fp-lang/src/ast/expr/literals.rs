use crate::ast::lower_common::decode_string_literal;
use crate::ast::FerroPhaseParser;
use crate::lexer::lexeme::LexemeKind;
use crate::lexer::tokenizer::strip_number_suffix;
use bigdecimal::BigDecimal;
use fp_core::ast::{
    DecimalType, Expr, ExprIntrinsicCall, ExprKind, ExprStringTemplate, FormatArgRef,
    FormatPlaceholder, FormatSpec, FormatTemplatePart, Ty, TypeInt, TypePrimitive, Value,
};
use fp_core::intrinsics::IntrinsicCallKind;
use num_bigint::BigInt;

use super::{lower_expr_from_cst, LowerError};

pub(super) fn parse_numeric_literal(raw: &str) -> Result<(Value, Option<Ty>), LowerError> {
    let stripped = strip_number_suffix(raw);
    let normalized = stripped.replace('_', "");
    let suffix = &raw[stripped.len()..];

    match suffix {
        "ib" => {
            if normalized.contains('.') {
                return Err(LowerError::InvalidNumber(raw.to_string()));
            }
            let value = parse_big_int_literal(&normalized)
                .ok_or_else(|| LowerError::InvalidNumber(raw.to_string()))?;
            Ok((
                Value::big_int(value),
                Some(Ty::Primitive(TypePrimitive::Int(TypeInt::BigInt))),
            ))
        }
        "fb" => {
            let value = normalized
                .parse::<BigDecimal>()
                .map_err(|_| LowerError::InvalidNumber(raw.to_string()))?;
            Ok((
                Value::big_decimal(value),
                Some(Ty::Primitive(TypePrimitive::Decimal(
                    DecimalType::BigDecimal,
                ))),
            ))
        }
        "i8" | "i16" | "i32" | "i64" | "isize" => {
            if normalized.contains('.') {
                return Err(LowerError::InvalidNumber(raw.to_string()));
            }
            let value = parse_i64_literal(&normalized)
                .ok_or_else(|| LowerError::InvalidNumber(raw.to_string()))?;
            let ty = match suffix {
                "i8" => TypeInt::I8,
                "i16" => TypeInt::I16,
                "i32" => TypeInt::I32,
                "i64" => TypeInt::I64,
                "isize" => TypeInt::I64,
                _ => TypeInt::I64,
            };
            Ok((
                Value::int(value),
                Some(Ty::Primitive(TypePrimitive::Int(ty))),
            ))
        }
        "i128" => {
            if normalized.contains('.') {
                return Err(LowerError::InvalidNumber(raw.to_string()));
            }
            let value = parse_big_int_literal(&normalized)
                .ok_or_else(|| LowerError::InvalidNumber(raw.to_string()))?;
            Ok((
                Value::big_int(value),
                Some(Ty::Primitive(TypePrimitive::Int(TypeInt::I128))),
            ))
        }
        "u8" | "u16" | "u32" | "u64" | "usize" => {
            if normalized.contains('.') {
                return Err(LowerError::InvalidNumber(raw.to_string()));
            }
            let value = parse_u64_literal(&normalized)
                .ok_or_else(|| LowerError::InvalidNumber(raw.to_string()))?;
            let ty = match suffix {
                "u8" => TypeInt::U8,
                "u16" => TypeInt::U16,
                "u32" => TypeInt::U32,
                "u64" => TypeInt::U64,
                "usize" => TypeInt::U64,
                _ => TypeInt::U64,
            };
            Ok((
                Value::uint(value),
                Some(Ty::Primitive(TypePrimitive::Int(ty))),
            ))
        }
        "u128" => {
            if normalized.contains('.') {
                return Err(LowerError::InvalidNumber(raw.to_string()));
            }
            let value = parse_big_int_literal(&normalized)
                .ok_or_else(|| LowerError::InvalidNumber(raw.to_string()))?;
            Ok((
                Value::big_int(value),
                Some(Ty::Primitive(TypePrimitive::Int(TypeInt::U128))),
            ))
        }
        "f32" | "f64" => {
            let value = normalized
                .parse::<f64>()
                .map_err(|_| LowerError::InvalidNumber(raw.to_string()))?;
            let ty = match suffix {
                "f32" => DecimalType::F32,
                _ => DecimalType::F64,
            };
            Ok((
                Value::decimal(value),
                Some(Ty::Primitive(TypePrimitive::Decimal(ty))),
            ))
        }
        _ => {
            if normalized.contains('.') {
                let d = normalized
                    .parse::<f64>()
                    .map_err(|_| LowerError::InvalidNumber(raw.to_string()))?;
                Ok((Value::decimal(d), None))
            } else {
                if let Some(i) = parse_i64_literal(&normalized) {
                    return Ok((Value::int(i), None));
                }
                let big = parse_big_int_literal(&normalized)
                    .ok_or_else(|| LowerError::InvalidNumber(raw.to_string()))?;
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

pub(super) fn parse_f_string_literal(
    raw: &str,
    file: fp_core::span::FileId,
) -> Result<Expr, LowerError> {
    let Some(decoded) = strip_string_prefix(raw, "f") else {
        return Err(LowerError::Unsupported(
            "invalid f-string literal".to_string(),
        ));
    };
    let (template, args) = parse_f_string_template(&decoded, file)?;
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

fn parse_f_string_template(
    input: &str,
    file: fp_core::span::FileId,
) -> Result<(ExprStringTemplate, Vec<Expr>), LowerError> {
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
            while let Some(inner) = chars.next() {
                if inner == '}' {
                    found_end = true;
                    break;
                }
                placeholder.push(inner);
            }
            if !found_end {
                return Err(LowerError::Unsupported(
                    "unterminated f-string placeholder".to_string(),
                ));
            }
            let trimmed = placeholder.trim();
            if trimmed.is_empty() {
                return Err(LowerError::Unsupported(
                    "empty f-string placeholder".to_string(),
                ));
            }
            let (expr_src, format_spec) = match trimmed.split_once(':') {
                Some((expr_part, spec_part)) => (expr_part.trim(), Some(spec_part.trim())),
                None => (trimmed, None),
            };
            let expr = parse_f_string_expr(expr_src, file)?;
            args.push(expr);
            parts.push(FormatTemplatePart::Placeholder(FormatPlaceholder {
                arg_ref: FormatArgRef::Implicit,
                format_spec: format_spec
                    .filter(|s| !s.is_empty())
                    .map(|s| FormatSpec::parse(s))
                    .transpose()
                    .map_err(|err| {
                        LowerError::Unsupported(format!("invalid format spec: {err}"))
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

fn parse_f_string_expr(src: &str, file: fp_core::span::FileId) -> Result<Expr, LowerError> {
    let parser = FerroPhaseParser::new();
    let lexemes = parser.lex_expr_lexemes(src, file).map_err(|err| {
        LowerError::Unsupported(format!("failed to tokenize f-string expression: {err}"))
    })?;
    let (cst, consumed) = parser
        .parse_expr_prefix_cst(&lexemes, file)
        .map_err(|err| {
            LowerError::Unsupported(format!("failed to parse f-string expression: {}", err))
        })?;
    if lexemes[consumed..]
        .iter()
        .any(|lex| lex.kind == LexemeKind::Token)
    {
        return Err(LowerError::Unsupported(
            "f-string expression contains trailing tokens".to_string(),
        ));
    }
    lower_expr_from_cst(&cst)
}
