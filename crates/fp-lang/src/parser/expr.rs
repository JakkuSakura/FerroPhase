use fp_core::ast::{
    BlockStmt, BlockStmtExpr, Expr, ExprBinOp, ExprBlock, ExprIf, ExprKind, ExprLoop, ExprQuote,
    ExprSplice, ExprWhile, Ident, Locator, Path, StmtLet, Value,
};
use fp_core::intrinsics::{IntrinsicCall, IntrinsicCallKind, IntrinsicCallPayload};
use fp_core::ops::BinOpKind;
use std::str::FromStr;
use thiserror::Error;
use winnow::error::{ContextError, ErrMode};
use winnow::ModalResult;

use super::lexer::{self, Keyword, LexerError, Token, TokenKind};

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

pub(crate) fn parse_expr_tokens(input: &mut &[Token]) -> ModalResult<Expr> {
    parse_expr_prec(input, 0)
}

pub(crate) fn parse_block_tokens(input: &mut &[Token]) -> ModalResult<ExprBlock> {
    parse_block(input)
}

pub(crate) fn parse_expr_prec(input: &mut &[Token], min_prec: u8) -> ModalResult<Expr> {
    let mut left = parse_prefix(input)?;
    loop {
        let Some((prec, op)) = peek_binop(input) else {
            break;
        };
        if prec < min_prec {
            break;
        }
        // consume operator
        advance(input);
        let right = parse_expr_prec(input, prec + 1)?;
        left = ExprKind::BinOp(ExprBinOp {
            kind: op,
            lhs: Box::new(left),
            rhs: Box::new(right),
        })
        .into();
    }
    Ok(left)
}

fn parse_prefix(input: &mut &[Token]) -> ModalResult<Expr> {
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
    if match_keyword(input, Keyword::Quote) {
        return parse_quote_body(input);
    }
    if match_keyword(input, Keyword::Splice) {
        return parse_splice_body(input);
    }
    if match_symbol(input, "(") {
        let expr = parse_expr_prec(input, 0)?;
        expect_symbol(input, ")")?;
        return Ok(expr);
    }
    let token = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
    match token.kind {
        TokenKind::Ident => parse_locator_expr(input, token),
        TokenKind::Number => parse_number(token),
        TokenKind::StringLiteral => parse_string(token),
        TokenKind::Keyword(_) | TokenKind::Symbol => Err(ErrMode::Cut(ContextError::new())),
    }
}

fn parse_if(input: &mut &[Token]) -> ModalResult<Expr> {
    let cond = parse_expr_prec(input, 0)?;
    let then_block = parse_block_tokens(input)?;
    let then_expr: Expr = ExprKind::Block(then_block).into();
    let elze = if match_keyword(input, Keyword::Else) {
        if match_keyword(input, Keyword::If) {
            Some(parse_if(input)?.into())
        } else {
            let block = parse_block_tokens(input)?;
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

fn parse_loop(input: &mut &[Token]) -> ModalResult<Expr> {
    let body = parse_block_tokens(input)?;
    Ok(ExprKind::Loop(ExprLoop {
        label: None,
        body: Box::new(ExprKind::Block(body).into()),
    })
    .into())
}

fn parse_while(input: &mut &[Token]) -> ModalResult<Expr> {
    let cond = parse_expr_prec(input, 0)?;
    let body = parse_block_tokens(input)?;
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

fn parse_splice_body(input: &mut &[Token]) -> ModalResult<Expr> {
    let target = if match_symbol(input, "(") {
        let expr = parse_expr_prec(input, 0)?;
        expect_symbol(input, ")")?;
        expr
    } else if match_keyword(input, Keyword::Quote) {
        parse_quote_body(input)?
    } else if match_symbol(input, "{") {
        let block = parse_block_body(input)?;
        Expr::from(ExprKind::Block(block))
    } else {
        parse_expr_prec(input, 0)?
    };
    Ok(ExprKind::Splice(ExprSplice {
        token: Box::new(target),
    })
    .into())
}

fn parse_block(input: &mut &[Token]) -> ModalResult<ExprBlock> {
    expect_symbol(input, "{")?;
    parse_block_body(input)
}

fn parse_block_body(input: &mut &[Token]) -> ModalResult<ExprBlock> {
    let mut stmts = Vec::new();
    loop {
        if match_symbol(input, "}") {
            break;
        }
        if input.is_empty() {
            return Err(ErrMode::Cut(ContextError::new()));
        }
        if peek_keyword(input, Keyword::Let) {
            match_keyword(input, Keyword::Let);
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
            expect_symbol(input, "}")?;
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

fn peek_binop(input: &[Token]) -> Option<(u8, BinOpKind)> {
    let token = input.first()?;
    match token.kind {
        TokenKind::Symbol => match token.lexeme.as_str() {
            "+" => Some((10, BinOpKind::Add)),
            "-" => Some((10, BinOpKind::Sub)),
            "*" => Some((20, BinOpKind::Mul)),
            "/" => Some((20, BinOpKind::Div)),
            _ => None,
        },
        _ => None,
    }
}

fn match_keyword(input: &mut &[Token], keyword: Keyword) -> bool {
    if peek_keyword(input, keyword) {
        *input = &input[1..];
        true
    } else {
        false
    }
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

fn match_symbol(input: &mut &[Token], symbol: &str) -> bool {
    if matches_symbol(input.first(), symbol) {
        *input = &input[1..];
        true
    } else {
        false
    }
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

fn expect_symbol(input: &mut &[Token], symbol: &str) -> ModalResult<()> {
    if match_symbol(input, symbol) {
        Ok(())
    } else {
        Err(ErrMode::Cut(ContextError::new()))
    }
}

fn expect_ident(input: &mut &[Token]) -> ModalResult<String> {
    match input.first() {
        Some(Token {
            kind: TokenKind::Ident,
            lexeme,
            ..
        }) => {
            let ident = lexeme.clone();
            *input = &input[1..];
            Ok(ident)
        }
        _ => Err(ErrMode::Cut(ContextError::new())),
    }
}

fn advance(input: &mut &[Token]) -> Option<Token> {
    let token = input.first().cloned()?;
    *input = &input[1..];
    Some(token)
}
