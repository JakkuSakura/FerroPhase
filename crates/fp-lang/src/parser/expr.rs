use fp_core::ast::{
    BlockStmt, BlockStmtExpr, Expr, ExprArray, ExprArrayRepeat, ExprAssign, ExprAsync, ExprAwait,
    ExprBinOp, ExprBlock, ExprClosure, ExprField, ExprFor, ExprIf, ExprIndex, ExprInvoke,
    ExprInvokeTarget, ExprKind, ExprLoop, ExprMacro, ExprMatch, ExprMatchCase, ExprQuote,
    ExprRange, ExprRangeLimit, ExprSelect, ExprSelectType, ExprSplice, ExprStruct, ExprStructural,
    ExprTry, ExprTuple, ExprUnOp, ExprWhile, Ident, Locator, MacroDelimiter, MacroInvocation, Path,
    Pattern, PatternIdent, PatternKind, PatternTuple, PatternType, StmtLet, Value,
};
use fp_core::intrinsics::{IntrinsicCall, IntrinsicCallKind, IntrinsicCallPayload};
use fp_core::ops::{BinOpKind, UnOpKind};
use std::str::FromStr;
use thiserror::Error;
use winnow::combinator::{alt, delimited, peek};
use winnow::error::{ContextError, ErrMode};
use winnow::ModalResult;
use winnow::Parser;

use crate::lexer::winnow::backtrack_err;
use crate::lexer::{self, Keyword, LexerError, Token, TokenKind};
use crate::parser::items;
use crate::parser::items::parse_path_as_ty;

#[derive(Debug, Error)]
pub enum ExprParseError {
    #[error("lex error: {0}")]
    Lex(#[from] LexerError),
    #[error("parse error: {0}")]
    Parse(String),
    #[error("unexpected trailing token '{0}'")]
    UnexpectedTrailingToken(String),
}

impl From<ErrMode<ContextError>> for ExprParseError {
    fn from(err: ErrMode<ContextError>) -> Self {
        match err {
            ErrMode::Backtrack(ctx) | ErrMode::Cut(ctx) => ExprParseError::Parse(ctx.to_string()),
            ErrMode::Incomplete(_) => ExprParseError::Parse("incomplete expression".to_string()),
        }
    }
}

pub fn parse_expression(source: &str) -> Result<Expr, ExprParseError> {
    let tokens = lexer::lex(source)?;
    let mut slice: &[Token] = tokens.as_slice();
    let expr = match parse_expr_prec(&mut slice, 0) {
        Ok(e) => e,
        Err(_) => {
            let msg = match slice.first() {
                Some(tok) => format!(
                    "failed to parse expression starting at token '{}'",
                    tok.lexeme
                ),
                None => "failed to parse expression at end of input".to_string(),
            };
            return Err(ExprParseError::Parse(msg));
        }
    };
    if let Some(token) = slice.first() {
        return Err(ExprParseError::UnexpectedTrailingToken(
            token.lexeme.clone(),
        ));
    }
    Ok(expr)
}

pub(crate) fn parse_expr_prec(input: &mut &[Token], min_prec: u8) -> ModalResult<Expr> {
    let mut left = parse_prefix(input)?;
    left = parse_postfix(input, left)?;
    loop {
        let Some((prec, op, right_assoc)) = peek_binop(input) else {
            break;
        };
        if prec < min_prec {
            break;
        }
        // consume operator
        advance(input);
        let next_min = if right_assoc { prec } else { prec + 1 };
        let right = if matches!(op, BinaryOp::Cast) {
            Expr::unit() // placeholder
        } else {
            parse_expr_prec(input, next_min)?
        };
        left = match op {
            BinaryOp::Assign => ExprKind::Assign(ExprAssign {
                target: Box::new(left),
                value: Box::new(right),
            })
            .into(),
            BinaryOp::AssignBin(kind) => {
                let combined = ExprKind::BinOp(ExprBinOp {
                    kind,
                    lhs: Box::new(left.clone()),
                    rhs: Box::new(right),
                })
                .into();
                ExprKind::Assign(ExprAssign {
                    target: Box::new(left),
                    value: Box::new(combined),
                })
                .into()
            }
            BinaryOp::Range(limit) => ExprKind::Range(ExprRange {
                start: Some(Box::new(left)),
                limit,
                end: Some(Box::new(right)),
                step: None,
            })
            .into(),
            BinaryOp::Cast => {
                let ty = items::parse_type(input)?;
                ExprKind::Cast(fp_core::ast::ExprCast {
                    expr: Box::new(left),
                    ty,
                })
                .into()
            }
            BinaryOp::Bin(kind) => ExprKind::BinOp(ExprBinOp {
                kind,
                lhs: Box::new(left),
                rhs: Box::new(right),
            })
            .into(),
        };
        left = parse_postfix(input, left)?;
    }
    Ok(left)
}

fn parse_prefix(input: &mut &[Token]) -> ModalResult<Expr> {
    alt((parse_control_prefix, parse_unary_or_atom)).parse_next(input)
}

fn parse_control_prefix(input: &mut &[Token]) -> ModalResult<Expr> {
    if match_keyword(input, Keyword::Return) {
        return parse_return(input);
    }
    if match_keyword(input, Keyword::Break) {
        return parse_break(input);
    }
    if match_keyword(input, Keyword::Continue) {
        return parse_continue(input);
    }
    if match_keyword(input, Keyword::If) {
        return parse_if(input);
    }
    if match_keyword(input, Keyword::Loop) {
        return parse_loop(input);
    }
    if match_keyword(input, Keyword::While) {
        return parse_while(input);
    }
    if match_keyword(input, Keyword::For) {
        return parse_for(input);
    }
    Err(backtrack_err())
}

fn parse_unary_or_atom(input: &mut &[Token]) -> ModalResult<Expr> {
    if matches_symbol(input.first(), "|") {
        match_symbol(input, "|");
        return parse_closure(input);
    }
    if match_keyword(input, Keyword::Await) {
        return parse_await(input);
    }
    if match_keyword(input, Keyword::Async) {
        return parse_async(input);
    }
    if match_keyword(input, Keyword::Const) {
        // const { ... } block expression
        let block = parse_block(input)?;
        let block_expr = Expr::block(block);
        let call = IntrinsicCall::new(
            IntrinsicCallKind::ConstBlock,
            IntrinsicCallPayload::Args {
                args: vec![block_expr],
            },
        );
        return Ok(ExprKind::IntrinsicCall(call).into());
    }
    if let Some(Token {
        kind: TokenKind::StringLiteral,
        lexeme: _,
        ..
    }) = input.first()
    {
        let tok = advance(input).unwrap();
        match parse_string(tok) {
            Ok(expr) => return Ok(expr),
            Err(e) => return Err(e),
        }
    }
    if match_symbol(input, "!") {
        let expr = parse_expr_prec(input, 30)?;
        return Ok(ExprKind::UnOp(ExprUnOp {
            op: UnOpKind::Not,
            val: Box::new(expr),
        })
        .into());
    }
    if match_symbol(input, "-") {
        let expr = parse_expr_prec(input, 30)?;
        return Ok(ExprKind::UnOp(ExprUnOp {
            op: UnOpKind::Neg,
            val: Box::new(expr),
        })
        .into());
    }
    if match_symbol(input, "*") {
        let expr = parse_expr_prec(input, 30)?;
        return Ok(ExprKind::Dereference(fp_core::ast::ExprDereference {
            referee: Box::new(expr),
        })
        .into());
    }
    if match_symbol(input, "&") {
        let mutable = match_keyword(input, Keyword::Mut).then_some(true);
        let expr = parse_expr_prec(input, 30)?;
        return Ok(ExprKind::Reference(fp_core::ast::ExprReference {
            referee: Box::new(expr),
            mutable,
        })
        .into());
    }
    if match_keyword(input, Keyword::Quote) {
        return parse_quote_body(input);
    }
    if match_keyword(input, Keyword::Splice) {
        return parse_splice_body(input);
    }
    if match_keyword(input, Keyword::Match) {
        return parse_match(input);
    }
    if match_keyword(input, Keyword::Struct) {
        return parse_structural_literal(input);
    }
    if match_keyword(input, Keyword::Emit) {
        // Lower emit! { ... } to splice(quote { ... })
        if !match_symbol(input, "!") {
            return Err(ErrMode::Cut(ContextError::new()));
        }
        let quoted = if matches_symbol(input.first(), "{") {
            let block = parse_block(input)?;
            ExprKind::Quote(ExprQuote { block, kind: None }).into()
        } else {
            // Fallback: treat following expression as the quote body
            let expr = parse_expr_prec(input, 0)?;
            let mut block = ExprBlock::new();
            block.push_expr(expr);
            ExprKind::Quote(ExprQuote { block, kind: None }).into()
        };
        let splice = ExprKind::Splice(ExprSplice {
            token: Box::new(quoted),
        });
        return Ok(splice.into());
    }
    if match_keyword(input, Keyword::Type) {
        // 解析类型表达式并将其作为 Value::Type 包装成表达式值。
        let ty = items::parse_type(input)?;
        return Ok(Expr::value(Value::Type(ty)));
    }
    // Bare block expression `{ ... }`.
    if matches_symbol(input.first(), "{") {
        let block = parse_block(input)?;
        return Ok(ExprKind::Block(block).into());
    }
    if match_symbol(input, "[") {
        return parse_array_literal(input);
    }
    if match_symbol(input, "(") {
        return parse_paren_or_tuple(input);
    }
    if let Some(Token {
        kind: TokenKind::Ident,
        lexeme,
        ..
    }) = input.first()
    {
        if lexeme == "true" || lexeme == "false" {
            let tok = advance(input).unwrap();
            let val = tok.lexeme == "true";
            return Ok(Expr::value(Value::bool(val)));
        }
    }
    let token = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
    match token.kind {
        TokenKind::Ident => parse_locator_expr(input, token),
        TokenKind::Number => parse_number(token),
        TokenKind::StringLiteral => parse_string(token),
        TokenKind::Keyword(_) | TokenKind::Symbol => Err(ErrMode::Cut(ContextError::new())),
    }
}

fn parse_postfix(input: &mut &[Token], mut expr: Expr) -> ModalResult<Expr> {
    loop {
        // macro invocation: path! { ... } / path!(...) / path![...]
        if matches_symbol(input.first(), "!") {
            if let Some(path) = locator_path_from_expr(&expr) {
                expr = parse_macro_invocation(path, input)?;
                continue;
            }
        }
        if matches_symbol(input.first(), "(") {
            let args = parse_argument_list(input)?;
            expr = ExprKind::Invoke(ExprInvoke {
                target: ExprInvokeTarget::expr(expr),
                args,
            })
            .into();
            continue;
        }
        if match_symbol(input, ".") {
            let ident = expect_ident(input)?;
            // look ahead for method call
            if matches_symbol(input.first(), "(") {
                let args = parse_argument_list(input)?;
                let select = ExprSelect {
                    obj: Box::new(expr),
                    field: Ident::new(ident),
                    select: ExprSelectType::Method,
                };
                let select_expr: Expr = ExprKind::Select(select).into();
                expr = ExprKind::Invoke(ExprInvoke {
                    target: ExprInvokeTarget::expr(select_expr),
                    args,
                })
                .into();
                continue;
            }
            expr = ExprKind::Select(ExprSelect {
                obj: Box::new(expr),
                field: Ident::new(ident),
                select: ExprSelectType::Field,
            })
            .into();
            continue;
        }
        // Struct literal: Path { field: value, ... }
        if matches_symbol(input.first(), "{") {
            // Only treat as struct literal when the base is a path/locator expression.
            if matches!(expr.kind(), ExprKind::Locator(_)) {
                // Heuristic: if the block immediately closes and is followed by `else`,
                // we're likely in a control-flow construct (e.g., `if a > b { a } else { b }`)
                // rather than a struct literal. In that case, leave the '{' for the caller.
                let mut depth = 0i32;
                let mut idx_after_close = None;
                for (i, tok) in input.iter().enumerate() {
                    if tok.lexeme == "{" {
                        depth += 1;
                    } else if tok.lexeme == "}" {
                        depth -= 1;
                        if depth == 0 {
                            idx_after_close = Some(i + 1);
                            break;
                        }
                    }
                }
                if let Some(next_idx) = idx_after_close {
                    if let Some(Token {
                        kind: TokenKind::Keyword(Keyword::Else),
                        ..
                    }) = input.get(next_idx)
                    {
                        break;
                    }
                }
                // Look ahead: struct literal must start with Ident and that Ident must be followed by ':'/','/'}.
                if let (Some(first), Some(second)) = (input.get(1), input.get(2)) {
                    let ident_start = matches!(
                        first,
                        Token {
                            kind: TokenKind::Ident,
                            ..
                        }
                    ) && (matches_symbol(Some(second), ":")
                        || matches_symbol(Some(second), ",")
                        || matches_symbol(Some(second), "}"));
                    let rest_wildcard = matches!(
                        first,
                        Token {
                            kind: TokenKind::Symbol,
                            lexeme,
                            ..
                        } if lexeme == ".."
                    );
                    if !(ident_start || rest_wildcard) {
                        break;
                    }
                }
                match_symbol(input, "{");
                let fields = parse_struct_fields(input)?;
                let struct_expr = ExprStruct {
                    name: expr.into(),
                    fields,
                };
                expr = ExprKind::Struct(struct_expr).into();
                continue;
            } else {
                // Leave the '{' to be consumed by outer constructs (e.g., control-flow blocks).
                break;
            }
        }
        if match_symbol(input, "[") {
            let index = parse_expr_prec(input, 0)?;
            expect_symbol(input, "]")?;
            expr = ExprKind::Index(ExprIndex {
                obj: Box::new(expr),
                index: Box::new(index),
            })
            .into();
            continue;
        }
        // Try operator: `expr?`
        if match_symbol(input, "?") {
            let try_expr = ExprTry {
                expr: Box::new(expr),
            };
            expr = ExprKind::Try(try_expr).into();
            continue;
        }
        break;
    }
    Ok(expr)
}

fn parse_if(input: &mut &[Token]) -> ModalResult<Expr> {
    let cond = parse_expr_prec(input, 0)?;
    let then_block = parse_block(input)?;
    let then_expr: Expr = ExprKind::Block(then_block).into();
    let elze = if match_keyword(input, Keyword::Else) {
        if match_keyword(input, Keyword::If) {
            Some(parse_if(input)?.into())
        } else {
            let block = parse_block(input)?;
            Some(ExprKind::Block(block).into())
        }
    } else {
        None
    };
    Ok(ExprKind::If(ExprIf {
        cond: cond.into(),
        then: then_expr.into(),
        elze: elze.map(Box::new),
    })
    .into())
}

fn parse_argument_list(input: &mut &[Token]) -> ModalResult<Vec<Expr>> {
    symbol_parser("(").parse_next(input)?;
    let mut args = Vec::new();
    if !matches_symbol(input.first(), ")") {
        loop {
            let e = parse_expr_prec(input, 0)?;
            args.push(e);
            if match_symbol(input, ")") {
                break;
            }
            expect_symbol(input, ",")?;
        }
    } else {
        expect_symbol(input, ")")?;
    }
    Ok(args)
}

fn parse_loop(input: &mut &[Token]) -> ModalResult<Expr> {
    let body = parse_block(input)?;
    Ok(ExprKind::Loop(ExprLoop {
        label: None,
        body: Box::new(ExprKind::Block(body).into()),
    })
    .into())
}

fn parse_while(input: &mut &[Token]) -> ModalResult<Expr> {
    let cond = parse_expr_prec(input, 0)?;
    let body = parse_block(input)?;
    Ok(ExprKind::While(ExprWhile {
        cond: cond.into(),
        body: Box::new(ExprKind::Block(body).into()),
    })
    .into())
}

fn parse_for(input: &mut &[Token]) -> ModalResult<Expr> {
    // Parse loop variable pattern: ident or tuple of idents `(a, b, ..)`.
    let pat = if match_symbol(input, "(") {
        let mut pats = Vec::new();
        if matches_symbol(input.first(), ")") {
            expect_symbol(input, ")")?;
        } else {
            loop {
                let name = Ident::new(expect_ident(input)?);
                pats.push(Pattern::from(PatternKind::Ident(PatternIdent::new(name))));
                if match_symbol(input, ")") {
                    break;
                }
                expect_symbol(input, ",")?;
            }
        }
        Pattern::from(PatternKind::Tuple(PatternTuple { patterns: pats }))
    } else {
        let pat_ident = Ident::new(expect_ident(input)?);
        Pattern::from(PatternKind::Ident(PatternIdent::new(pat_ident)))
    };

    // Expect `in` keyword
    keyword_parser(Keyword::In)
        .parse_next(input)
        .map_err(|_| ErrMode::Cut(ContextError::new()))?;

    // Parse iterable expression
    let iter_expr = parse_expr_prec(input, 0)?;

    // Parse loop body block
    let body_block = parse_block(input)?;
    let body_expr: Expr = ExprKind::Block(body_block).into();

    Ok(ExprKind::For(ExprFor {
        pat: Box::new(pat),
        iter: Box::new(iter_expr),
        body: Box::new(body_expr),
    })
    .into())
}

fn parse_return(input: &mut &[Token]) -> ModalResult<Expr> {
    let expr = if matches_symbol(input.first(), ";") || matches_symbol(input.first(), "}") {
        None
    } else {
        Some(parse_expr_prec(input, 0)?)
    };
    Ok(control_flow_call(IntrinsicCallKind::Return, expr))
}

fn parse_break(input: &mut &[Token]) -> ModalResult<Expr> {
    let expr = if matches_symbol(input.first(), ";") || matches_symbol(input.first(), "}") {
        None
    } else {
        Some(parse_expr_prec(input, 0)?)
    };
    Ok(control_flow_call(IntrinsicCallKind::Break, expr))
}

fn parse_continue(_input: &mut &[Token]) -> ModalResult<Expr> {
    Ok(control_flow_call(IntrinsicCallKind::Continue, None))
}

fn control_flow_call(kind: IntrinsicCallKind, expr: Option<Expr>) -> Expr {
    let payload = IntrinsicCallPayload::Args {
        args: expr.into_iter().collect(),
    };
    ExprKind::IntrinsicCall(IntrinsicCall::new(kind, payload)).into()
}

fn parse_async(input: &mut &[Token]) -> ModalResult<Expr> {
    let inner = parse_expr_prec(input, 0)?;
    Ok(ExprKind::Async(ExprAsync {
        expr: Box::new(inner),
    })
    .into())
}

fn parse_quote_body(input: &mut &[Token]) -> ModalResult<Expr> {
    let block = parse_block(input)?;
    Ok(ExprKind::Quote(ExprQuote { block, kind: None }).into())
}

fn parse_closure(input: &mut &[Token]) -> ModalResult<Expr> {
    let mut params = Vec::new();
    if match_symbol(input, "|") {
        // no parameters
    } else {
        loop {
            let pat = parse_closure_param(input)?;
            params.push(pat);
            if match_symbol(input, "|") {
                break;
            }
            expect_symbol(input, ",")?;
        }
    }
    let body_expr = if peek(symbol_parser("{")).parse_next(input).is_ok() {
        let block = parse_block(input)?;
        ExprKind::Block(block).into()
    } else {
        parse_expr_prec(input, 0)?
    };
    Ok(ExprKind::Closure(ExprClosure {
        params,
        ret_ty: None,
        movability: None,
        body: Box::new(body_expr),
    })
    .into())
}

fn parse_await(input: &mut &[Token]) -> ModalResult<Expr> {
    let base = parse_expr_prec(input, 30)?;
    Ok(ExprKind::Await(ExprAwait {
        base: Box::new(base),
    })
    .into())
}

fn parse_splice_body(input: &mut &[Token]) -> ModalResult<Expr> {
    // TODO: 当前未对 splice 进行上下文/片段种类的禁用诊断，仅做语法包装；后续可在不匹配位置记录诊断。
    let target = if match_symbol(input, "(") {
        let expr = parse_expr_prec(input, 0)?;
        expect_symbol(input, ")")?;
        expr
    } else if match_keyword(input, Keyword::Quote) {
        parse_quote_body(input)?
    } else if peek(symbol_parser("{")).parse_next(input).is_ok() {
        let block = parse_block(input)?;
        Expr::from(ExprKind::Block(block))
    } else {
        parse_expr_prec(input, 0)?
    };
    Ok(ExprKind::Splice(ExprSplice {
        token: Box::new(target),
    })
    .into())
}

fn parse_closure_param(input: &mut &[Token]) -> ModalResult<Pattern> {
    // Optional `mut` before the binding name.
    let is_mut = match_keyword(input, Keyword::Mut);

    let name = expect_ident(input)?;

    // Treat `_` specially as a wildcard pattern.
    let mut pat = if name == "_" {
        Pattern::from(PatternKind::Wildcard(fp_core::ast::PatternWildcard {}))
    } else {
        let ident = PatternIdent::new(Ident::new(name));
        Pattern::from(PatternKind::Ident(ident))
    };

    if is_mut {
        pat.make_mut();
    }

    // Optional type annotation: `: Type`.
    if match_symbol(input, ":") {
        let (_path, ty) = parse_path_as_ty(input)?;
        // Attach the type both to the wrapper node and to the outer slot so
        // callers can read it without unwrapping PatternType.
        pat = Pattern::from(PatternKind::Type(PatternType::new(pat, ty.clone())));
        pat.set_ty(ty);
    }

    Ok(pat)
}

fn parse_array_literal(input: &mut &[Token]) -> ModalResult<Expr> {
    // We are called after the leading '[' has already been consumed.
    // Handle `[]`, `[a, b, c]` and `[elem; len]`.

    // Empty array: `[]`.
    if matches_symbol(input.first(), "]") {
        advance(input);
        let array = ExprArray { values: Vec::new() };
        return Ok(ExprKind::Array(array).into());
    }

    // Parse the first element expression.
    let first = parse_expr_prec(input, 0)?;

    // Repeat form: `[elem; len]`.
    if match_symbol(input, ";") {
        let len = parse_expr_prec(input, 0)?;
        expect_symbol(input, "]")?;
        let repeat = ExprArrayRepeat {
            elem: Box::new(first),
            len: Box::new(len),
        };
        return Ok(ExprKind::ArrayRepeat(repeat).into());
    }

    // Element list form: `[a, b, c]` (with optional trailing comma).
    let mut values = Vec::new();
    values.push(first);

    while match_symbol(input, ",") {
        if matches_symbol(input.first(), "]") {
            // Trailing comma before closing bracket.
            break;
        }
        let next = parse_expr_prec(input, 0)?;
        values.push(next);
    }

    expect_symbol(input, "]")?;
    let array = ExprArray { values };
    Ok(ExprKind::Array(array).into())
}

fn parse_struct_fields(input: &mut &[Token]) -> ModalResult<Vec<ExprField>> {
    let mut fields = Vec::new();
    if matches_symbol(input.first(), "}") {
        // Empty `{}`
        advance(input);
        return Ok(fields);
    }

    loop {
        // Support struct patterns with `..` (ignore remaining fields).
        if match_symbol(input, "..") {
            // consume optional trailing comma before closing brace
            match_symbol(input, ",");
            expect_symbol(input, "}")?;
            break;
        }
        let field_name = expect_ident(input)?;
        let name_ident = Ident::new(field_name.clone());
        let value_expr = if match_symbol(input, ":") {
            // Explicit value: `field: expr`
            match parse_expr_prec(input, 0) {
                Ok(v) => v,
                Err(e) => return Err(e),
            }
        } else {
            // Shorthand: `field` => locator with same name.
            let locator = Locator::Ident(Ident::new(field_name));
            Expr::locator(locator)
        };
        let field = ExprField::new(name_ident, value_expr);
        fields.push(field);
        if match_symbol(input, "}") {
            break;
        }
        if match_symbol(input, ",") {
            // allow trailing comma before closing brace
            if match_symbol(input, "}") {
                break;
            }
            continue;
        }
        expect_symbol(input, ",")?;
    }

    Ok(fields)
}

fn parse_structural_literal(input: &mut &[Token]) -> ModalResult<Expr> {
    // We have consumed the `struct` keyword. Expect a field list in braces.
    expect_symbol(input, "{")?;
    let fields = parse_struct_fields(input)?;
    let structural = ExprStructural { fields };
    Ok(ExprKind::Structural(structural).into())
}

fn parse_paren_or_tuple(input: &mut &[Token]) -> ModalResult<Expr> {
    // We are called after the leading '(' has already been consumed.
    // Distinguish between:
    // - `()`           -> unit expression
    // - `(expr)`       -> grouped expression
    // - `(a, b, ..)`   -> tuple expression

    // Empty parens: unit.
    if matches_symbol(input.first(), ")") {
        advance(input);
        return Ok(Expr::unit());
    }

    // Parse first element/expression.
    let first = parse_expr_prec(input, 0)?;

    // Tuple if there is a comma following the first expression.
    if match_symbol(input, ",") {
        let mut values = Vec::new();
        values.push(first);

        while !matches_symbol(input.first(), ")") {
            let next = parse_expr_prec(input, 0)?;
            values.push(next);
            if !match_symbol(input, ",") {
                break;
            }
        }

        expect_symbol(input, ")")?;
        let tuple = ExprTuple { values };
        return Ok(ExprKind::Tuple(tuple).into());
    }

    // No comma: plain grouping.
    expect_symbol(input, ")")?;
    Ok(first)
}
fn parse_match(input: &mut &[Token]) -> ModalResult<Expr> {
    let scrutinee = parse_expr_prec(input, 0)?;
    symbol_parser("{").parse_next(input)?;
    let mut cases = Vec::new();
    loop {
        if match_symbol(input, "}") {
            break;
        }
        let arm_pattern = parse_expr_prec(input, 0)?;
        let mut cond_base = if is_wildcard_pattern(&arm_pattern) || is_binding_pattern(&arm_pattern)
        {
            Expr::value(Value::bool(true))
        } else {
            ExprKind::BinOp(ExprBinOp {
                kind: BinOpKind::Eq,
                lhs: Box::new(scrutinee.clone()),
                rhs: Box::new(arm_pattern),
            })
            .into()
        };

        if match_keyword(input, Keyword::If) {
            let guard = parse_expr_prec(input, 0)?;
            cond_base = ExprKind::BinOp(ExprBinOp {
                kind: BinOpKind::And,
                lhs: Box::new(cond_base),
                rhs: Box::new(guard),
            })
            .into();
        }

        let arm_cond = cond_base;
        expect_symbol(input, "=>")?;
        let arm_body = if peek(symbol_parser("{")).parse_next(input).is_ok() {
            let block = parse_block(input)?;
            ExprKind::Block(block).into()
        } else {
            parse_expr_prec(input, 0)?
        };
        cases.push(ExprMatchCase {
            cond: Box::new(arm_cond),
            body: Box::new(arm_body),
        });
        match_symbol(input, ",");
    }
    Ok(ExprKind::Match(ExprMatch { cases }).into())
}

fn locator_path_from_expr(expr: &Expr) -> Option<Path> {
    match expr.kind() {
        ExprKind::Locator(loc) => Some(loc.to_path()),
        _ => None,
    }
}

pub(crate) fn parse_macro_invocation(path: Path, input: &mut &[Token]) -> ModalResult<Expr> {
    // consume '!'
    match_symbol(input, "!");
    let next = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
    let (delimiter, open, close) = match next.kind {
        TokenKind::Symbol => match next.lexeme.as_str() {
            "(" => (MacroDelimiter::Parenthesis, "(", ")"),
            "[" => (MacroDelimiter::Bracket, "[", "]"),
            "{" => (MacroDelimiter::Brace, "{", "}"),
            _ => return Err(ErrMode::Cut(ContextError::new())),
        },
        _ => return Err(ErrMode::Cut(ContextError::new())),
    };

    let mut depth = 1i32;
    let mut pieces = Vec::new();
    while depth > 0 {
        let tok = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
        if let TokenKind::Symbol = tok.kind {
            if tok.lexeme == open {
                depth += 1;
                pieces.push(tok.lexeme);
                continue;
            }
            if tok.lexeme == close {
                depth -= 1;
                if depth == 0 {
                    break;
                }
                pieces.push(tok.lexeme);
                continue;
            }
        }
        pieces.push(tok.lexeme);
    }
    let tokens = pieces.join(" ");
    let invocation = MacroInvocation::new(path, delimiter, tokens);
    Ok(ExprKind::Macro(ExprMacro::new(invocation)).into())
}

fn is_wildcard_pattern(expr: &Expr) -> bool {
    match expr.kind() {
        ExprKind::Locator(loc) => loc.as_ident().map(|id| id.as_str() == "_").unwrap_or(false),
        _ => false,
    }
}

fn is_binding_pattern(expr: &Expr) -> bool {
    match expr.kind() {
        ExprKind::Locator(loc) => loc.as_ident().map(|id| id.as_str() != "_").unwrap_or(false),
        _ => false,
    }
}

pub fn parse_block(input: &mut &[Token]) -> ModalResult<ExprBlock> {
    delimited(symbol_parser("{"), parse_block_body, symbol_parser("}")).parse_next(input)
}

fn parse_block_body(input: &mut &[Token]) -> ModalResult<ExprBlock> {
    let mut stmts = Vec::new();
    loop {
        if peek(symbol_parser("}")).parse_next(input).is_ok() {
            break;
        }
        if input.is_empty() {
            return Err(ErrMode::Cut(ContextError::new()));
        }
        // Support block-level items (const/static/struct/enum/type/mod/trait/impl/use/fn).
        if let Some(Token {
            kind: TokenKind::Keyword(k),
            ..
        }) = input.first()
        {
            let mut snapshot = *input;
            let parsed_item = match k {
                Keyword::Pub => Some(items::parse_item(&mut snapshot)),
                Keyword::Const => {
                    // const { ... } is an expression, not an item.
                    if matches_symbol(snapshot.get(1), "{") {
                        None
                    } else if matches!(
                        snapshot.get(1),
                        Some(Token {
                            kind: TokenKind::Keyword(Keyword::Fn),
                            ..
                        })
                    ) {
                        // consume `const` and let the fn parser handle the rest
                        snapshot = &snapshot[1..];
                        Some(items::parse_fn_item(&mut snapshot))
                    } else {
                        Some(items::parse_const_item(&mut snapshot))
                    }
                }
                Keyword::Static => Some(items::parse_static_item(&mut snapshot)),
                Keyword::Struct => Some(items::parse_struct_item(&mut snapshot)),
                Keyword::Enum => Some(items::parse_enum_item(&mut snapshot)),
                Keyword::Type => Some(items::parse_type_item(&mut snapshot)),
                Keyword::Mod => Some(items::parse_mod_item(&mut snapshot)),
                Keyword::Trait => Some(items::parse_trait_item(&mut snapshot)),
                Keyword::Impl => Some(items::parse_impl_item(&mut snapshot)),
                Keyword::Use => Some(items::parse_use_item(&mut snapshot)),
                Keyword::Fn => Some(items::parse_fn_item(&mut snapshot)),
                _ => None,
            };
            if let Some(parsed_item) = parsed_item {
                match parsed_item {
                    Ok(item) => {
                        *input = snapshot;
                        stmts.push(BlockStmt::Item(Box::new(item)));
                        continue;
                    }
                    Err(err) => return Err(err),
                }
            }
        }
        // Treat stray semicolons as no-op statements to keep
        // block parsing resilient in the presence of extra `;`.
        if matches_symbol(input.first(), ";") {
            advance(input);
            stmts.push(BlockStmt::Noop);
            continue;
        }
        if peek_keyword(input, Keyword::Let) {
            keyword_parser(Keyword::Let).parse_next(input)?;
            let stmt = parse_let_stmt(input)?;
            expect_symbol(input, ";")?;
            stmts.push(BlockStmt::Let(stmt));
            continue;
        }
        let expr = parse_expr_prec(input, 0)?;
        let has_semicolon = match_symbol(input, ";");
        let stmt = BlockStmt::Expr(BlockStmtExpr::new(expr).with_semicolon(has_semicolon));
        stmts.push(stmt);
        if !has_semicolon {
            // If this is a control-like expression (if/loop/while/match) and we're
            // not at the end of the block yet, treat it as a statement even
            // without a semicolon. Otherwise, treat it as the trailing expr.
            let last_expr = match &stmts.last() {
                Some(BlockStmt::Expr(e)) => &e.expr,
                _ => unreachable!(),
            };
            let control_like = matches!(
                last_expr.kind(),
                ExprKind::If(_)
                    | ExprKind::Loop(_)
                    | ExprKind::While(_)
                    | ExprKind::For(_)
                    | ExprKind::Match(_)
            ) || matches!(
                last_expr.kind(),
                ExprKind::IntrinsicCall(call)
                    if matches!(call.kind(), IntrinsicCallKind::ConstBlock)
            );
            if !(control_like && !matches_symbol(input.first(), "}")) {
                break;
            }
        }
    }
    Ok(ExprBlock::new_stmts(stmts))
}

fn parse_let_stmt(input: &mut &[Token]) -> ModalResult<StmtLet> {
    let is_mut = match_keyword(input, Keyword::Mut);
    let ident = expect_ident(input)?;
    let mut pat = Pattern::from(PatternKind::Ident(PatternIdent::new(Ident::new(ident))));
    // Optional type annotation
    if match_symbol(input, ":") {
        let (_path, ty) = items::parse_path_as_ty(input)?;
        pat = Pattern::from(PatternKind::Type(PatternType::new(pat, ty)));
    }
    expect_symbol(input, "=")?;
    let init = parse_expr_prec(input, 0)?;
    if is_mut {
        pat.make_mut();
    }
    Ok(StmtLet::new(pat, Some(init), None))
}

fn parse_locator_expr(input: &mut &[Token], token: Token) -> ModalResult<Expr> {
    let mut segments = vec![Ident::new(token.lexeme)];
    while match_symbol(input, "::") {
        let ident = expect_ident(input)?;
        segments.push(Ident::new(ident));
    }
    let locator = if segments.len() == 1 {
        Locator::Ident(segments.remove(0))
    } else {
        Locator::path(Path::new(segments))
    };
    Ok(Expr::locator(locator))
}

fn parse_number(token: Token) -> ModalResult<Expr> {
    let cleaned = token.lexeme.replace('_', "");
    // TODO: 当前只按 i64/f64 解析并忽略数值后缀的类型/溢出校验；后续应依据后缀决定具体位宽并给出诊断。
    if cleaned.is_empty() {
        return Err(ErrMode::Cut(ContextError::new()));
    }
    if cleaned.contains('.') || cleaned.contains('e') || cleaned.contains('E') {
        let value = f64::from_str(&cleaned).map_err(|_| ErrMode::Cut(ContextError::new()))?;
        Ok(Expr::value(Value::decimal(value)))
    } else {
        let value = i64::from_str(&cleaned).map_err(|_| ErrMode::Cut(ContextError::new()))?;
        Ok(Expr::value(Value::int(value)))
    }
}

fn parse_string(token: Token) -> ModalResult<Expr> {
    if token.lexeme.len() < 2 {
        return Err(ErrMode::Cut(ContextError::new()));
    }
    let inner = token.lexeme.trim_matches('"').to_string();
    Ok(Expr::value(Value::string(inner)))
}

#[derive(Debug, Clone)]
enum BinaryOp {
    Assign,
    AssignBin(BinOpKind),
    Range(ExprRangeLimit),
    Cast,
    Bin(BinOpKind),
}

fn peek_binop(input: &[Token]) -> Option<(u8, BinaryOp, bool)> {
    let token = input.first()?;
    match token.kind {
        TokenKind::Symbol => match token.lexeme.as_str() {
            "=" => Some((1, BinaryOp::Assign, true)),
            "+=" => Some((1, BinaryOp::AssignBin(BinOpKind::Add), true)),
            "-=" => Some((1, BinaryOp::AssignBin(BinOpKind::Sub), true)),
            "*=" => Some((1, BinaryOp::AssignBin(BinOpKind::Mul), true)),
            "/=" => Some((1, BinaryOp::AssignBin(BinOpKind::Div), true)),
            "%=" => Some((1, BinaryOp::AssignBin(BinOpKind::Mod), true)),
            "<<=" => Some((1, BinaryOp::AssignBin(BinOpKind::Shl), true)),
            ">>=" => Some((1, BinaryOp::AssignBin(BinOpKind::Shr), true)),
            "&=" => Some((1, BinaryOp::AssignBin(BinOpKind::BitAnd), true)),
            "|=" => Some((1, BinaryOp::AssignBin(BinOpKind::BitOr), true)),
            "^=" => Some((1, BinaryOp::AssignBin(BinOpKind::BitXor), true)),
            ".." => Some((3, BinaryOp::Range(ExprRangeLimit::Exclusive), false)),
            "..=" | "..." => Some((3, BinaryOp::Range(ExprRangeLimit::Inclusive), false)),
            "||" => Some((4, BinaryOp::Bin(BinOpKind::Or), false)),
            "&&" => Some((5, BinaryOp::Bin(BinOpKind::And), false)),
            "|" => Some((6, BinaryOp::Bin(BinOpKind::BitOr), false)),
            "^" => Some((7, BinaryOp::Bin(BinOpKind::BitXor), false)),
            "&" => Some((8, BinaryOp::Bin(BinOpKind::BitAnd), false)),
            "==" => Some((9, BinaryOp::Bin(BinOpKind::Eq), false)),
            "!=" => Some((9, BinaryOp::Bin(BinOpKind::Ne), false)),
            "<" => Some((10, BinaryOp::Bin(BinOpKind::Lt), false)),
            ">" => Some((10, BinaryOp::Bin(BinOpKind::Gt), false)),
            "<=" => Some((10, BinaryOp::Bin(BinOpKind::Le), false)),
            ">=" => Some((10, BinaryOp::Bin(BinOpKind::Ge), false)),
            "<<" => Some((11, BinaryOp::Bin(BinOpKind::Shl), false)),
            ">>" => Some((11, BinaryOp::Bin(BinOpKind::Shr), false)),
            "+" => Some((12, BinaryOp::Bin(BinOpKind::Add), false)),
            "-" => Some((12, BinaryOp::Bin(BinOpKind::Sub), false)),
            "*" => Some((13, BinaryOp::Bin(BinOpKind::Mul), false)),
            "/" => Some((13, BinaryOp::Bin(BinOpKind::Div), false)),
            "%" => Some((13, BinaryOp::Bin(BinOpKind::Mod), false)),
            _ => None,
        },
        TokenKind::Keyword(Keyword::As) => Some((14, BinaryOp::Cast, false)),
        _ => None,
    }
}

pub(crate) fn match_keyword(input: &mut &[Token], keyword: Keyword) -> bool {
    keyword_parser(keyword).parse_next(input).is_ok()
}

fn peek_keyword(input: &[Token], keyword: Keyword) -> bool {
    matches!(
        input.first(),
        Some(Token {
            kind: TokenKind::Keyword(k),
            ..
        }) if *k == keyword
    )
}

fn match_symbol<'a>(input: &mut &'a [Token], symbol: &'a str) -> bool {
    symbol_parser(symbol).parse_next(input).is_ok()
}

fn matches_symbol(token: Option<&Token>, symbol: &str) -> bool {
    matches!(
        token,
        Some(Token {
            kind: TokenKind::Symbol,
            lexeme,
            ..
        }) if lexeme == symbol
    )
}

fn expect_symbol<'a>(input: &mut &'a [Token], symbol: &'a str) -> ModalResult<()> {
    symbol_parser(symbol)
        .parse_next(input)
        .map_err(|_| ErrMode::Cut(ContextError::new()))
}

fn symbol_parser<'a>(symbol: &'a str) -> impl Parser<&'a [Token], (), ContextError> {
    let sym = symbol.to_string();
    move |input: &mut &[Token]| {
        if matches_symbol(input.first(), &sym) {
            *input = &input[1..];
            Ok(())
        } else {
            Err(backtrack_err())
        }
    }
}

fn expect_ident(input: &mut &[Token]) -> ModalResult<String> {
    ident_parser()
        .parse_next(input)
        .map_err(|_| ErrMode::Cut(ContextError::new()))
}

fn advance(input: &mut &[Token]) -> Option<Token> {
    let token = input.first().cloned()?;
    *input = &input[1..];
    Some(token)
}

fn keyword_parser<'a>(keyword: Keyword) -> impl Parser<&'a [Token], (), ContextError> {
    move |input: &mut &[Token]| match input.first() {
        Some(Token {
            kind: TokenKind::Keyword(k),
            ..
        }) if *k == keyword => {
            *input = &input[1..];
            Ok(())
        }
        _ => Err(backtrack_err()),
    }
}

fn ident_parser<'a>() -> impl Parser<&'a [Token], String, ContextError> {
    move |input: &mut &[Token]| match input.first() {
        Some(Token {
            kind: TokenKind::Ident,
            lexeme,
            ..
        }) => {
            let ident = lexeme.clone();
            *input = &input[1..];
            Ok(ident)
        }
        _ => Err(backtrack_err()),
    }
}
