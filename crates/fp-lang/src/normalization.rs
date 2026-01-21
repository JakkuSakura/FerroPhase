use fp_core::ast::{
    BlockStmt, BlockStmtExpr, Expr, ExprBinOp, ExprBlock, ExprIf, ExprIntrinsicCall, ExprKind,
    ExprStringTemplate, ExprUnOp, FormatArgRef, FormatPlaceholder, FormatSpec, FormatTemplatePart,
    Ident, StmtLet, Ty, Value,
};
use fp_core::error::Result;
use fp_core::intrinsics::{IntrinsicCallKind, IntrinsicNormalizer, NormalizeOutcome};
use fp_core::ops::{BinOpKind, UnOpKind};

use crate::ast::expr::{lower_expr_from_cst, lower_type_from_cst};
use crate::cst::{parse_expr_lexemes_prefix_to_cst, parse_type_lexemes_prefix_to_cst};
use crate::lexer::lexeme::LexemeKind;
use crate::lexer::tokenizer::lex_lexemes;

/// FerroPhase intrinsic normalizer that adds `t!` macro lowering for type expressions,
/// delegating all other macros to the Rust normalizer.
#[derive(Debug, Default, Clone, Copy)]
pub struct FerroIntrinsicNormalizer;

impl IntrinsicNormalizer for FerroIntrinsicNormalizer {
    fn normalize_macro(&self, expr: Expr) -> Result<NormalizeOutcome<Expr>> {
        let (ty_slot, kind) = expr.into_parts();
        let ExprKind::Macro(macro_expr) = kind else {
            return Ok(NormalizeOutcome::Ignored(Expr::from_parts(ty_slot, kind)));
        };

        if let Some(name) = macro_expr.invocation.path.segments.last() {
            let macro_name = name.as_str().trim_end_matches('!');
            if macro_name == "t" {
                let ty = parse_type_macro_tokens(&macro_expr.invocation.tokens)?;
                let replacement = Expr::value(Value::Type(ty)).with_ty_slot(ty_slot);
                return Ok(NormalizeOutcome::Normalized(replacement));
            }
            if macro_name == "vec" {
                let expr = parse_vec_macro_tokens(&macro_expr.invocation.tokens)?;
                let replacement = expr.with_ty_slot(ty_slot);
                return Ok(NormalizeOutcome::Normalized(replacement));
            }
            if macro_name == "assert" {
                let args = parse_expr_macro_tokens(&macro_expr.invocation.tokens)?;
                if args.len() != 1 {
                    return Err(fp_core::error::Error::from(
                        "assert! requires exactly one argument",
                    ));
                }
                let replacement =
                    assert_macro(args.into_iter().next().unwrap(), "assertion failed")
                        .with_ty_slot(ty_slot);
                return Ok(NormalizeOutcome::Normalized(replacement));
            }
            if macro_name == "assert_eq" {
                let args = parse_expr_macro_tokens(&macro_expr.invocation.tokens)?;
                if args.len() != 2 {
                    return Err(fp_core::error::Error::from(
                        "assert_eq! requires exactly two arguments",
                    ));
                }
                let replacement = assert_compare_macro(
                    args[0].clone(),
                    args[1].clone(),
                    BinOpKind::Eq,
                    "assertion failed: left != right",
                )
                .with_ty_slot(ty_slot);
                return Ok(NormalizeOutcome::Normalized(replacement));
            }
            if macro_name == "assert_ne" {
                let args = parse_expr_macro_tokens(&macro_expr.invocation.tokens)?;
                if args.len() != 2 {
                    return Err(fp_core::error::Error::from(
                        "assert_ne! requires exactly two arguments",
                    ));
                }
                let replacement = assert_compare_macro(
                    args[0].clone(),
                    args[1].clone(),
                    BinOpKind::Ne,
                    "assertion failed: left == right",
                )
                .with_ty_slot(ty_slot);
                return Ok(NormalizeOutcome::Normalized(replacement));
            }
            if macro_name == "panic" {
                let args = parse_expr_macro_tokens(&macro_expr.invocation.tokens)?;
                if args.len() > 1 {
                    return Err(fp_core::error::Error::from(
                        "panic! accepts at most one argument",
                    ));
                }
                let replacement = panic_macro(args).with_ty_slot(ty_slot);
                return Ok(NormalizeOutcome::Normalized(replacement));
            }
            if macro_name == "format" {
                let args = parse_expr_macro_tokens(&macro_expr.invocation.tokens)?;
                if args.is_empty() {
                    return Err(fp_core::error::Error::from(
                        "format! requires at least one argument",
                    ));
                }
                let template = match args[0].kind() {
                    ExprKind::Value(value) => match value.as_ref() {
                        Value::String(string) => {
                            let parts = parse_format_template(&string.value)?;
                            ExprStringTemplate { parts }
                        }
                        _ => {
                            return Err(fp_core::error::Error::from(
                                "format! expects a string literal as the first argument",
                            ));
                        }
                    },
                    ExprKind::FormatString(format) => ExprStringTemplate {
                        parts: format.parts.clone(),
                    },
                    _ => {
                        return Err(fp_core::error::Error::from(
                            "format! expects a string literal as the first argument",
                        ));
                    }
                };

                let mut call_args = Vec::with_capacity(args.len());
                call_args.push(Expr::new(ExprKind::FormatString(template)));
                call_args.extend(args[1..].iter().cloned());
                let replacement = Expr::from_parts(
                    ty_slot.clone(),
                    ExprKind::IntrinsicCall(ExprIntrinsicCall::new(
                        IntrinsicCallKind::Format,
                        call_args,
                        Vec::new(),
                    )),
                );
                return Ok(NormalizeOutcome::Normalized(replacement));
            }
            if macro_name == "type_of" || macro_name == "typeof" {
                let args = parse_expr_macro_tokens(&macro_expr.invocation.tokens)?;
                if args.len() != 1 {
                    return Err(fp_core::error::Error::from(
                        "type_of! requires exactly one argument",
                    ));
                }
                let replacement = Expr::from_parts(
                    ty_slot.clone(),
                    ExprKind::IntrinsicCall(ExprIntrinsicCall::new(
                        IntrinsicCallKind::TypeOf,
                        args,
                        Vec::new(),
                    )),
                );
                return Ok(NormalizeOutcome::Normalized(replacement));
            }
            if macro_name == "print" || macro_name == "println" {
                let args = parse_expr_macro_tokens(&macro_expr.invocation.tokens)?;
                let kind = if macro_name == "println" {
                    IntrinsicCallKind::Println
                } else {
                    IntrinsicCallKind::Print
                };
                let replacement = Expr::from_parts(
                    ty_slot.clone(),
                    ExprKind::IntrinsicCall(ExprIntrinsicCall::new(kind, args, Vec::new())),
                );
                return Ok(NormalizeOutcome::Normalized(replacement));
            }
            if let Some(kind) = intrinsic_macro_kind(macro_name) {
                let args = parse_expr_macro_tokens(&macro_expr.invocation.tokens)?;
                let replacement = Expr::from_parts(
                    ty_slot.clone(),
                    ExprKind::IntrinsicCall(ExprIntrinsicCall::new(kind, args, Vec::new())),
                );
                return Ok(NormalizeOutcome::Normalized(replacement));
            }
        }

    Ok(NormalizeOutcome::Ignored(Expr::from_parts(
        ty_slot,
        ExprKind::Macro(macro_expr),
    )))
}
}

fn intrinsic_macro_kind(name: &str) -> Option<IntrinsicCallKind> {
    match name {
        "sizeof" => Some(IntrinsicCallKind::SizeOf),
        "reflect_fields" => Some(IntrinsicCallKind::ReflectFields),
        "hasmethod" => Some(IntrinsicCallKind::HasMethod),
        "type_name" => Some(IntrinsicCallKind::TypeName),
        "type_info" => Some(IntrinsicCallKind::TypeOf),
        "type_of" => Some(IntrinsicCallKind::TypeOf),
        "clone_struct" => Some(IntrinsicCallKind::CloneStruct),
        "hasfield" => Some(IntrinsicCallKind::HasField),
        "count_fields" => Some(IntrinsicCallKind::FieldCount),
        "field_count" => Some(IntrinsicCallKind::FieldCount),
        "method_count" => Some(IntrinsicCallKind::MethodCount),
        "field_type" => Some(IntrinsicCallKind::FieldType),
        "vec_type" => Some(IntrinsicCallKind::VecType),
        "field_name_at" => Some(IntrinsicCallKind::FieldNameAt),
        "struct_size" => Some(IntrinsicCallKind::StructSize),
        "generate_method" => Some(IntrinsicCallKind::GenerateMethod),
        "compile_error" => Some(IntrinsicCallKind::CompileError),
        "compile_warning" => Some(IntrinsicCallKind::CompileWarning),
        _ => None,
    }
}

fn parse_type_macro_tokens(tokens: &str) -> Result<fp_core::ast::Ty> {
    let lexemes =
        lex_lexemes(tokens).map_err(|err| fp_core::error::Error::from(err.to_string()))?;
    let (ty_cst, consumed) = parse_type_lexemes_prefix_to_cst(&lexemes, 0, &[])
        .map_err(|err| fp_core::error::Error::from(err.to_string()))?;

    if lexemes[consumed..]
        .iter()
        .any(|lex| lex.kind == LexemeKind::Token)
    {
        return Err(fp_core::error::Error::from(
            "t! macro tokens contain trailing input",
        ));
    }

    lower_type_from_cst(&ty_cst).map_err(|err| fp_core::error::Error::from(err.to_string()))
}

fn parse_expr_macro_tokens(tokens: &str) -> Result<Vec<Expr>> {
    let lexemes =
        lex_lexemes(tokens).map_err(|err| fp_core::error::Error::from(err.to_string()))?;
    let mut idx = 0;
    let mut args = Vec::new();
    while idx < lexemes.len() {
        while idx < lexemes.len() && lexemes[idx].kind != LexemeKind::Token {
            idx += 1;
        }
        if idx >= lexemes.len() {
            break;
        }
        if lexemes[idx].text == "," {
            idx += 1;
            continue;
        }
        let (expr_cst, consumed) =
            parse_expr_lexemes_prefix_to_cst(&lexemes[idx..], 0).map_err(|err| {
                fp_core::error::Error::from(format!("assert macro parse error: {}", err))
            })?;
        let expr = lower_expr_from_cst(&expr_cst)
            .map_err(|err| fp_core::error::Error::from(err.to_string()))?;
        args.push(expr);
        idx += consumed;
    }
    Ok(args)
}

fn parse_vec_macro_tokens(tokens: &str) -> Result<Expr> {
    let text = format!("[{}]", tokens);
    let lexemes =
        lex_lexemes(&text).map_err(|err| fp_core::error::Error::from(err.to_string()))?;
    let (expr_cst, consumed) = parse_expr_lexemes_prefix_to_cst(&lexemes, 0)
        .map_err(|err| fp_core::error::Error::from(err.to_string()))?;
    if lexemes[consumed..]
        .iter()
        .any(|lex| lex.kind == LexemeKind::Token)
    {
        return Err(fp_core::error::Error::from(
            "vec! macro tokens contain trailing input",
        ));
    }
    lower_expr_from_cst(&expr_cst).map_err(|err| fp_core::error::Error::from(err.to_string()))
}

fn parse_macro_tokens_with_type_args(tokens: &str, type_positions: &[usize]) -> Result<Vec<Expr>> {
    let lexemes =
        lex_lexemes(tokens).map_err(|err| fp_core::error::Error::from(err.to_string()))?;
    let mut idx = 0;
    let mut args = Vec::new();
    let mut arg_index = 0;
    while idx < lexemes.len() {
        while idx < lexemes.len() && lexemes[idx].kind != LexemeKind::Token {
            idx += 1;
        }
        if idx >= lexemes.len() {
            break;
        }
        if lexemes[idx].text == "," {
            idx += 1;
            continue;
        }
        let is_type = type_positions.iter().any(|pos| *pos == arg_index);
        if is_type {
            match parse_type_lexemes_prefix_to_cst(&lexemes[idx..], 0, &[","]) {
                Ok((ty_cst, consumed)) => {
                    let ty = lower_type_from_cst(&ty_cst)
                        .map_err(|err| fp_core::error::Error::from(err.to_string()))?;
                    args.push(Expr::value(Value::Type(ty)));
                    idx += consumed;
                }
                Err(_) => {
                    let (expr_cst, consumed) =
                        parse_expr_lexemes_prefix_to_cst(&lexemes[idx..], 0).map_err(|err| {
                            fp_core::error::Error::from(format!(
                                "assert macro parse error: {}",
                                err
                            ))
                        })?;
                    let expr = lower_expr_from_cst(&expr_cst)
                        .map_err(|err| fp_core::error::Error::from(err.to_string()))?;
                    args.push(Expr::value(Value::Type(Ty::Expr(expr.into()))));
                    idx += consumed;
                }
            }
        } else {
            let (expr_cst, consumed) =
                parse_expr_lexemes_prefix_to_cst(&lexemes[idx..], 0).map_err(|err| {
                    fp_core::error::Error::from(format!("assert macro parse error: {}", err))
                })?;
            let expr = lower_expr_from_cst(&expr_cst)
                .map_err(|err| fp_core::error::Error::from(err.to_string()))?;
            args.push(expr);
            idx += consumed;
        }
        arg_index += 1;
    }
    Ok(args)
}

fn parse_format_template(template: &str) -> Result<Vec<FormatTemplatePart>> {
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
                format_spec: Some(
                    FormatSpec::parse(&format!("%{}", spec))
                        .map_err(fp_core::error::Error::from)?,
                ),
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

fn parse_placeholder_content(content: &str) -> Result<FormatPlaceholder> {
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
            format_spec: Some(FormatSpec::parse(format_spec).map_err(fp_core::error::Error::from)?),
        })
    } else {
        let arg_ref = if let Ok(index) = content.parse::<usize>() {
            FormatArgRef::Positional(index)
        } else {
            FormatArgRef::Named(content.to_string())
        };

        Ok(FormatPlaceholder {
            arg_ref,
            format_spec: None,
        })
    }
}

fn assert_macro(cond: Expr, message: &str) -> Expr {
    let panic_expr = panic_call_with_message(message);
    let negated = Expr::new(ExprKind::UnOp(ExprUnOp {
        span: fp_core::span::Span::null(),
        op: UnOpKind::Not,
        val: cond.into(),
    }));
    let if_expr = Expr::new(ExprKind::If(ExprIf {
        span: fp_core::span::Span::null(),
        cond: negated.into(),
        then: Expr::block(ExprBlock::new_stmts(vec![BlockStmt::Expr(
            BlockStmtExpr::new(panic_expr).with_semicolon(true),
        )]))
        .into(),
        elze: None,
    }));

    Expr::block(ExprBlock::new_stmts_expr(
        vec![BlockStmt::Expr(
            BlockStmtExpr::new(if_expr).with_semicolon(true),
        )],
        Expr::unit(),
    ))
}

fn assert_compare_macro(left: Expr, right: Expr, op: BinOpKind, message: &str) -> Expr {
    let left_ident = Ident::new("__fp_assert_left");
    let right_ident = Ident::new("__fp_assert_right");
    let left_binding = BlockStmt::Let(StmtLet::new_simple(left_ident.clone(), left));
    let right_binding = BlockStmt::Let(StmtLet::new_simple(right_ident.clone(), right));

    let comparison = Expr::new(ExprKind::BinOp(ExprBinOp {
        span: fp_core::span::Span::null(),
        kind: op,
        lhs: Expr::ident(left_ident).into(),
        rhs: Expr::ident(right_ident).into(),
    }));
    let negated = Expr::new(ExprKind::UnOp(ExprUnOp {
        span: fp_core::span::Span::null(),
        op: UnOpKind::Not,
        val: comparison.into(),
    }));
    let panic_expr = panic_call_with_message(message);
    let if_expr = Expr::new(ExprKind::If(ExprIf {
        span: fp_core::span::Span::null(),
        cond: negated.into(),
        then: Expr::block(ExprBlock::new_stmts(vec![BlockStmt::Expr(
            BlockStmtExpr::new(panic_expr).with_semicolon(true),
        )]))
        .into(),
        elze: None,
    }));

    Expr::block(ExprBlock::new_stmts_expr(
        vec![
            left_binding,
            right_binding,
            BlockStmt::Expr(BlockStmtExpr::new(if_expr).with_semicolon(true)),
        ],
        Expr::unit(),
    ))
}

fn panic_macro(args: Vec<Expr>) -> Expr {
    let message = if args.is_empty() {
        panic_call_with_message("panic! macro triggered")
    } else {
        Expr::new(ExprKind::IntrinsicCall(ExprIntrinsicCall::new(
            IntrinsicCallKind::Panic,
            args,
            Vec::new(),
        )))
    };
    Expr::block(ExprBlock::new_stmts_expr(
        vec![BlockStmt::Expr(
            BlockStmtExpr::new(message).with_semicolon(true),
        )],
        Expr::unit(),
    ))
}

fn panic_call_with_message(message: &str) -> Expr {
    Expr::new(ExprKind::IntrinsicCall(ExprIntrinsicCall::new(
        IntrinsicCallKind::Panic,
        vec![Expr::value(Value::string(message.to_string()))],
        Vec::new(),
    )))
}
