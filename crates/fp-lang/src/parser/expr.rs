use fp_core::ast::{
    BlockStmt, BlockStmtExpr, Expr, ExprAssign, ExprBinOp, ExprBlock, ExprClosure, ExprIf, ExprIndex,
    ExprInvoke, ExprInvokeTarget, ExprKind, ExprLoop, ExprMacro, ExprMatch, ExprMatchCase, ExprQuote,
    ExprRange, ExprRangeLimit, ExprSelect, ExprSelectType, ExprSplice, ExprUnOp, ExprWhile, Ident,
    Locator, MacroDelimiter, MacroInvocation, Path, Pattern, PatternIdent, PatternKind, StmtLet,
    Value,
};
use fp_core::intrinsics::{IntrinsicCall, IntrinsicCallKind, IntrinsicCallPayload};
use fp_core::ops::{BinOpKind, UnOpKind};
use std::str::FromStr;
use thiserror::Error;
use winnow::combinator::{alt, delimited, peek, separated};
use winnow::error::{ContextError, ErrMode};
use winnow::ModalResult;
use winnow::Parser;

use crate::lexer::{self, Keyword, LexerError, Token, TokenKind};
use crate::lexer::winnow::backtrack_err;

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
    let expr = parse_expr_prec(&mut slice, 0).map_err(ExprParseError::from)?;
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
        let right = parse_expr_prec(input, next_min)?;
        left = match op {
            BinaryOp::Assign => ExprKind::Assign(ExprAssign {
                target: Box::new(left),
                value: Box::new(right),
            })
            .into(),
            BinaryOp::Range(limit) => ExprKind::Range(ExprRange {
                start: Some(Box::new(left)),
                limit,
                end: Some(Box::new(right)),
                step: None,
            })
            .into(),
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
    Err(backtrack_err())
}

fn parse_unary_or_atom(input: &mut &[Token]) -> ModalResult<Expr> {
    if matches_symbol(input.first(), "|") {
        match_symbol(input, "|");
        return parse_closure(input);
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
    if match_symbol(input, "(") {
        return delimited(
            symbol_parser("("),
            |i: &mut &[Token]| parse_expr_prec(i, 0),
            symbol_parser(")"),
        )
        .parse_next(input);
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
    delimited(
        symbol_parser("("),
        separated(0.., |i: &mut &[Token]| parse_expr_prec(i, 0), symbol_parser(",")),
        symbol_parser(")"),
    )
    .parse_next(input)
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
            let name = expect_ident(input)?;
            let pat = Pattern::from(PatternKind::Ident(PatternIdent::new(Ident::new(name))));
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

fn parse_splice_body(input: &mut &[Token]) -> ModalResult<Expr> {
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

fn parse_match(input: &mut &[Token]) -> ModalResult<Expr> {
    let scrutinee = parse_expr_prec(input, 0)?;
    symbol_parser("{").parse_next(input)?;
    let mut cases = Vec::new();
    loop {
        if match_symbol(input, "}") {
            break;
        }
        let arm_pattern = parse_expr_prec(input, 0)?;
        let mut cond_base = if is_wildcard_pattern(&arm_pattern) {
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

fn parse_macro_invocation(path: Path, input: &mut &[Token]) -> ModalResult<Expr> {
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

pub fn parse_block(input: &mut &[Token]) -> ModalResult<ExprBlock> {
    delimited(symbol_parser("{"), parse_block_body, symbol_parser("}"))
        .parse_next(input)
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
            // trailing expression; stop before closing brace
            break;
        }
    }
    Ok(ExprBlock::new_stmts(stmts))
}

fn parse_let_stmt(input: &mut &[Token]) -> ModalResult<StmtLet> {
    let ident = expect_ident(input)?;
    expect_symbol(input, "=")?;
    let init = parse_expr_prec(input, 0)?;
    Ok(StmtLet::new_simple(Ident::new(ident), init))
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
    let value = i64::from_str(&cleaned).map_err(|_| ErrMode::Cut(ContextError::new()))?;
    Ok(Expr::value(Value::int(value)))
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
    Range(ExprRangeLimit),
    Bin(BinOpKind),
}

fn peek_binop(input: &[Token]) -> Option<(u8, BinaryOp, bool)> {
    let token = input.first()?;
    match token.kind {
        TokenKind::Symbol => match token.lexeme.as_str() {
            "=" => Some((1, BinaryOp::Assign, true)),
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
            "+" => Some((11, BinaryOp::Bin(BinOpKind::Add), false)),
            "-" => Some((11, BinaryOp::Bin(BinOpKind::Sub), false)),
            "*" => Some((12, BinaryOp::Bin(BinOpKind::Mul), false)),
            "/" => Some((12, BinaryOp::Bin(BinOpKind::Div), false)),
            "%" => Some((12, BinaryOp::Bin(BinOpKind::Mod), false)),
            _ => None,
        },
        _ => None,
    }
}

fn match_keyword(input: &mut &[Token], keyword: Keyword) -> bool {
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
    move |input: &mut &[Token]| {
        match input.first() {
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
