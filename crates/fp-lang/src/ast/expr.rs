use crate::ast::items::LowerItemsError;
use crate::ast::lower_common::{
    collect_path_tokens_with_generics, decode_string_literal, split_path_prefix,
};
use crate::syntax::{SyntaxKind, SyntaxNode};
use fp_core::ast::{
    BlockStmt, BlockStmtExpr, DecimalType, Expr, ExprArray, ExprArrayRepeat, ExprAsync, ExprAwait,
    ExprBinOp, ExprBlock, ExprBreak, ExprClosure, ExprConstBlock, ExprContinue, ExprField, ExprFor,
    ExprIf, ExprIndex, ExprIntrinsicCall, ExprInvoke, ExprInvokeTarget, ExprKind, ExprKwArg,
    ExprLet, ExprLoop, ExprMatch, ExprMatchCase, ExprQuote, ExprRange, ExprRangeLimit, ExprReturn,
    ExprSelect, ExprSelectType, ExprSplice, ExprStringTemplate, ExprStruct, ExprStructural,
    ExprTry, ExprTryCatch, ExprTuple, ExprWhile, ExprWith, FormatArgRef, FormatPlaceholder,
    FormatSpec, FormatTemplatePart, Ident, MacroInvocation, Name, ParameterPath,
    ParameterPathSegment, Path, Pattern, PatternBind,
    PatternBox, PatternIdent, PatternKind, PatternQuote, PatternQuotePlural, PatternRef,
    PatternStruct, PatternStructField, PatternStructural, PatternTuple, PatternTupleStruct,
    PatternType, PatternVariant,
    PatternWildcard, QuoteFragmentKind, QuoteItemKind, StmtDefer, StmtLet, StructuralField, Ty,
    TypeArray, TypeBinaryOp, TypeBinaryOpKind, TypeBounds, TypeFunction, TypeInt, TypePrimitive,
    TypeQuote, TypeReference, TypeSlice, TypeStructural, TypeTuple, TypeType, TypeVec, Value,
    ValueNone, ValueString,
};
use fp_core::cst::CstCategory;
use fp_core::intrinsics::IntrinsicCallKind;
use fp_core::module::path::PathPrefix;
use fp_core::ops::{BinOpKind, UnOpKind};

mod literals;
mod quote;
mod types;

use self::literals::{parse_f_string_literal, parse_numeric_literal};
use self::quote::{
    quote_block_pattern_from_cst, quote_kind_from_cst, quote_pattern_from_cst,
    quote_pattern_kind_from_cst,
};
pub(crate) use self::types::lower_type_from_cst;
use self::types::node_children_types;

#[derive(Debug, thiserror::Error)]
pub enum LowerError {
    #[error("unexpected CST node kind: {0:?}")]
    UnexpectedNode(SyntaxKind),
    #[error("missing operator token")]
    MissingOperator,
    #[error("invalid number literal: {0}")]
    InvalidNumber(String),
    #[error("unsupported feature: {0}")]
    Unsupported(String),
    #[error("failed to lower item: {0}")]
    Item(#[from] LowerItemsError),
}

pub fn lower_expr_from_cst(node: &SyntaxNode) -> Result<Expr, LowerError> {
    if node.kind.category() == CstCategory::Item {
        if node.kind == SyntaxKind::ItemExternBlock {
            return Err(LowerError::UnexpectedNode(node.kind));
        }
        let item = crate::ast::items::lower_item_from_cst(node)?;
        return Ok(ExprKind::Item(Box::new(item)).into());
    }
    let mut expr = match node.kind {
        SyntaxKind::Root => {
            let expr = node
                .children
                .iter()
                .find_map(|c| match c {
                    crate::syntax::SyntaxElement::Node(n)
                        if n.kind.category() == CstCategory::Expr =>
                    {
                        Some(n.as_ref())
                    }
                    _ => None,
                })
                .ok_or(LowerError::UnexpectedNode(SyntaxKind::Root))?;
            lower_expr_from_cst(expr)
        }
        SyntaxKind::ExprParen => {
            // Treat as grouping only.
            // Find the first nested expression node.
            let inner = node
                .children
                .iter()
                .find_map(|c| match c {
                    crate::syntax::SyntaxElement::Node(n)
                        if n.kind.category() == CstCategory::Expr =>
                    {
                        Some(n.as_ref())
                    }
                    _ => None,
                })
                .ok_or(LowerError::UnexpectedNode(SyntaxKind::ExprParen))?;
            lower_expr_from_cst(inner)
        }
        SyntaxKind::ExprUnit => Ok(Expr::unit()),
        SyntaxKind::ExprTuple => {
            let values = node_children_exprs(node)
                .map(lower_expr_from_cst)
                .collect::<Result<Vec<_>, _>>()?;
            Ok(ExprKind::Tuple(ExprTuple {
                span: node.span,
                values,
            })
            .into())
        }
        SyntaxKind::ExprArray => {
            let values = node_children_exprs(node)
                .map(lower_expr_from_cst)
                .collect::<Result<Vec<_>, _>>()?;
            Ok(ExprKind::Array(ExprArray {
                span: node.span,
                values,
            })
            .into())
        }
        SyntaxKind::ExprArrayRepeat => {
            let mut exprs = node_children_exprs(node);
            let elem = exprs
                .next()
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::ExprArrayRepeat))?;
            let len = exprs
                .next()
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::ExprArrayRepeat))?;
            let elem = lower_expr_from_cst(elem)?;
            let len = lower_expr_from_cst(len)?;
            Ok(ExprKind::ArrayRepeat(ExprArrayRepeat {
                span: node.span,
                elem: Box::new(elem),
                len: Box::new(len),
            })
            .into())
        }
        SyntaxKind::ExprStruct => {
            let mut exprs = node_children_exprs(node);
            let name = exprs
                .next()
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::ExprStruct))?;
            let name_expr = lower_expr_from_cst(name)?;
            let (fields, update) = lower_struct_fields(node)?;
            Ok(ExprKind::Struct(ExprStruct {
                span: node.span,
                name: Box::new(name_expr),
                fields,
                update: update.map(Box::new),
            })
            .into())
        }
        SyntaxKind::ExprStructural => {
            let (fields, update) = lower_struct_fields(node)?;
            if update.is_some() {
                return Err(LowerError::UnexpectedNode(SyntaxKind::ExprStructural));
            }
            Ok(ExprKind::Structural(ExprStructural {
                span: node.span,
                fields,
            })
            .into())
        }
        SyntaxKind::ExprBlock => {
            let expr = lower_block_expr_from_cst(node)?;
            Ok(expr)
        }
        SyntaxKind::ExprQuote => {
            let block = lower_block_from_expr_parent(node, SyntaxKind::ExprQuote)?;
            let kind = quote_kind_from_cst(node)?;
            Ok(ExprKind::Quote(ExprQuote {
                span: node.span,
                block,
                kind,
            })
            .into())
        }
        SyntaxKind::ExprQuoteToken => Err(LowerError::UnexpectedNode(SyntaxKind::ExprQuoteToken)),
        SyntaxKind::ExprSplice => {
            let token_expr = last_child_expr(node)?;
            let expr = lower_expr_from_cst(token_expr)?;
            Ok(ExprKind::Splice(ExprSplice {
                span: node.span,
                token: Box::new(expr),
            })
            .into())
        }
        SyntaxKind::ExprAsync => {
            let block = lower_block_from_expr_parent(node, SyntaxKind::ExprAsync)?;
            Ok(ExprKind::Async(ExprAsync {
                span: node.span,
                expr: Box::new(ExprKind::Block(block).into()),
            })
            .into())
        }
        SyntaxKind::ExprConstBlock => {
            let block = lower_block_from_expr_parent(node, SyntaxKind::ExprConstBlock)?;
            Ok(ExprKind::ConstBlock(ExprConstBlock {
                span: node.span,
                expr: Box::new(ExprKind::Block(block).into()),
            })
            .into())
        }
        SyntaxKind::ExprIf => {
            let mut expr_nodes = node_children_exprs(node);
            let cond = expr_nodes
                .next()
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::ExprIf))?;
            let then_block = expr_nodes
                .next()
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::ExprIf))?;
            let else_block = expr_nodes.next();

            let cond = lower_expr_from_cst(cond)?;
            let then_expr = lower_block_expr_from_cst(then_block)?;
            let else_expr = if let Some(else_node) = else_block {
                if else_node.kind == SyntaxKind::ExprBlock {
                    Some(Box::new(lower_block_expr_from_cst(else_node)?))
                } else {
                    Some(Box::new(lower_expr_from_cst(else_node)?))
                }
            } else {
                None
            };

            Ok(ExprKind::If(ExprIf {
                span: node.span,
                cond: Box::new(cond),
                then: Box::new(then_expr),
                elze: else_expr,
            })
            .into())
        }
        SyntaxKind::ExprLoop => {
            let block = lower_block_from_expr_parent(node, SyntaxKind::ExprLoop)?;
            Ok(ExprKind::Loop(ExprLoop {
                span: node.span,
                label: None,
                body: Box::new(ExprKind::Block(block).into()),
            })
            .into())
        }
        SyntaxKind::ExprWhile => {
            let mut expr_nodes = node_children_exprs(node);
            let cond = expr_nodes
                .next()
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::ExprWhile))?;
            let body = expr_nodes
                .next()
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::ExprWhile))?;
            let cond = lower_expr_from_cst(cond)?;
            let body_expr = lower_block_expr_from_cst(body)?;
            Ok(ExprKind::While(ExprWhile {
                span: node.span,
                cond: Box::new(cond),
                body: Box::new(body_expr),
            })
            .into())
        }
        SyntaxKind::ExprWith => {
            let mut expr_nodes = node_children_exprs(node);
            let context = expr_nodes
                .next()
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::ExprWith))?;
            let body = expr_nodes
                .next()
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::ExprWith))?;
            let context = lower_expr_from_cst(context)?;
            let body = lower_block_expr_from_cst(body)?;
            Ok(ExprKind::With(ExprWith {
                span: node.span,
                context: Box::new(context),
                body: Box::new(body),
            })
            .into())
        }

        SyntaxKind::ExprFor => {
            let pat_node = first_child_in_category(node, CstCategory::Pattern, SyntaxKind::ExprFor)?;
            let pat = lower_pattern_from_cst(pat_node)?;

            let mut expr_nodes = node_children_exprs(node);
            let iter = expr_nodes
                .next()
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::ExprFor))?;
            let body = expr_nodes
                .next()
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::ExprFor))?;
            let iter = lower_expr_from_cst(iter)?;
            let body_block = lower_block_expr_from_cst(body)?;
            Ok(ExprKind::For(ExprFor {
                span: node.span,
                pat: Box::new(pat),
                iter: Box::new(iter),
                body: Box::new(body_block),
            })
            .into())
        }
        SyntaxKind::ExprMatch => {
            // Children: first expr is scrutinee, then MatchArm nodes.
            let scrutinee = node_children_exprs(node)
                .next()
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::ExprMatch))?;
            let scrutinee_expr = lower_expr_from_cst(scrutinee)?;

            let mut cases = Vec::new();
            for child in &node.children {
                let crate::syntax::SyntaxElement::Node(arm) = child else {
                    continue;
                };
                if arm.kind != SyntaxKind::MatchArm {
                    continue;
                }
                let (pat, guard, body) = split_match_arm(arm)?;
                let pat = lower_match_pattern_from_cst(pat)?;
                let guard_expr = guard.map(lower_expr_from_cst).transpose()?;
                let body_expr = lower_expr_from_cst(body)?;

                // New-style match: store scrutinee + pattern and let typing/backends
                // decide how to lower. Keep the legacy `cond` field populated with
                // `true` for compatibility.
                cases.push(ExprMatchCase {
                    span: arm.span,
                    pat: Some(Box::new(pat)),
                    cond: Box::new(Expr::value(Value::bool(true))),
                    guard: guard_expr.map(Box::new),
                    body: Box::new(body_expr),
                });
            }

            Ok(ExprKind::Match(ExprMatch {
                span: node.span,
                scrutinee: Some(Box::new(scrutinee_expr)),
                cases,
            })
            .into())
        }
        SyntaxKind::ExprLet => {
            let mut pattern: Option<Pattern> = None;
            let mut ty: Option<Ty> = None;
            let mut init_expr: Option<Expr> = None;

            for child in &node.children {
                match child {
                    crate::syntax::SyntaxElement::Node(n)
                        if n.kind.category() == CstCategory::Pattern =>
                    {
                        if pattern.is_none() {
                            pattern = Some(lower_pattern_from_cst(n)?);
                        }
                    }
                    crate::syntax::SyntaxElement::Node(n)
                        if n.kind.category() == CstCategory::Type =>
                    {
                        ty = Some(lower_type_from_cst(n)?);
                    }
                    crate::syntax::SyntaxElement::Node(n)
                        if n.kind.category() == CstCategory::Expr =>
                    {
                        init_expr = Some(lower_expr_from_cst(n)?);
                    }
                    _ => {}
                }
            }

            let Some(pat) = pattern else {
                return Err(LowerError::UnexpectedNode(SyntaxKind::ExprLet));
            };
            let Some(init) = init_expr else {
                return Err(LowerError::UnexpectedNode(SyntaxKind::ExprLet));
            };

            let pat = if let Some(ty) = ty {
                Pattern::from(PatternKind::Type(PatternType::new(pat, ty)))
            } else {
                pat
            };

            Ok(ExprKind::Let(ExprLet {
                span: node.span,
                pat: Box::new(pat),
                expr: Box::new(init),
            })
            .into())
        }
        SyntaxKind::ExprClosure => {
            let (params, body) = lower_closure_from_cst(node)?;
            Ok(ExprKind::Closure(ExprClosure {
                span: node.span,
                params,
                ret_ty: None,
                movability: None,
                body: Box::new(body),
            })
            .into())
        }
        SyntaxKind::ExprYield => {
            let value = node_children_exprs(node)
                .next()
                .map(lower_expr_from_cst)
                .transpose()?
                .unwrap_or_else(Expr::unit);
            Ok(ExprKind::IntrinsicCall(ExprIntrinsicCall::new(
                IntrinsicCallKind::Yield,
                vec![value],
                Vec::new(),
            ))
            .into())
        }
        SyntaxKind::ExprReturn => {
            let value = node_children_exprs(node)
                .next()
                .map(lower_expr_from_cst)
                .transpose()?
                .map(Box::new);
            Ok(ExprKind::Return(ExprReturn {
                span: node.span,
                value,
            })
            .into())
        }
        SyntaxKind::ExprBreak => {
            let value = node_children_exprs(node)
                .next()
                .map(lower_expr_from_cst)
                .transpose()?
                .map(Box::new);
            Ok(ExprKind::Break(ExprBreak {
                span: node.span,
                value,
            })
            .into())
        }
        SyntaxKind::ExprContinue => Ok(ExprKind::Continue(ExprContinue { span: node.span }).into()),
        SyntaxKind::ExprName => {
            let name = direct_first_non_trivia_token_text(node)
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::ExprName))?;
            match name.as_str() {
                "true" => Ok(Expr::value(Value::bool(true))),
                "false" => Ok(Expr::value(Value::bool(false))),
                "null" => Ok(Expr::value(Value::null())),
                _ => Ok(ExprKind::Name(Name::from_ident(Ident::new(name))).into()),
            }
        }
        SyntaxKind::ExprPath => {
            let path_tokens = collect_path_tokens_with_generics(node);
            let saw_generic_start = path_tokens.saw_generic_start;
            let generic_segment_index = path_tokens.generic_segment_index;
            let (prefix, segments) =
                split_path_prefix(path_tokens.segments, path_tokens.saw_root);
            if segments.is_empty() && matches!(prefix, PathPrefix::Plain | PathPrefix::Root) {
                return Err(LowerError::UnexpectedNode(SyntaxKind::ExprPath));
            }
            let args = node_children_types(node)
                .map(lower_type_from_cst)
                .collect::<Result<Vec<_>, _>>()?;
            if saw_generic_start && !args.is_empty() {
                let mut param_segments: Vec<ParameterPathSegment> = segments
                    .into_iter()
                    .map(ParameterPathSegment::from_ident)
                    .collect();
                if let Some(idx) = generic_segment_index {
                    if let Some(seg) = param_segments.get_mut(idx) {
                        seg.args = args;
                    }
                } else if let Some(last) = param_segments.last_mut() {
                    last.args = args;
                }
                let ppath = ParameterPath::new(prefix, param_segments);
                Ok(ExprKind::Name(Name::parameter_path(ppath)).into())
            } else {
                Ok(ExprKind::Name(Name::path(Path::new(prefix, segments))).into())
            }
        }
        SyntaxKind::ExprNumber => {
            let raw = direct_first_non_trivia_token_text(node)
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::ExprNumber))?;
            let (value, ty) = parse_numeric_literal(&raw)?;
            let mut expr = Expr::value(value);
            if let Some(ty) = ty {
                expr.ty = Some(ty);
            }
            Ok(expr)
        }
        SyntaxKind::ExprString => {
            let raw = direct_first_non_trivia_token_text(node)
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::ExprString))?;
            if raw.starts_with("f\"") {
                parse_f_string_literal(&raw, node.span.file)
            } else if raw.starts_with("t\"") {
                let decoded = decode_string_literal(&raw).unwrap_or(raw);
                Ok(Expr::value(Value::String(ValueString::new_ref(decoded))))
            } else {
                let decoded = decode_string_literal(&raw).unwrap_or(raw);
                // String literals should lower to borrowed `&'static str` equivalents by default.
                Ok(Expr::value(Value::String(ValueString::new_ref(decoded))))
            }
        }
        SyntaxKind::ExprType => {
            let ty_node = node
                .children
                .iter()
                .find_map(|c| match c {
                    crate::syntax::SyntaxElement::Node(n)
                        if n.kind.category() == CstCategory::Type =>
                    {
                        Some(n.as_ref())
                    }
                    _ => None,
                })
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::ExprType))?;
            let ty = lower_type_from_cst(ty_node)?;
            Ok(Expr::value(Value::Type(ty)))
        }
        SyntaxKind::ExprUnary => {
            let op = direct_operator_token_text(node).ok_or(LowerError::MissingOperator)?;
            let expr = last_child_expr(node)?;
            let inner = lower_expr_from_cst(expr)?;
            match op.as_str() {
                "-" => Ok(ExprKind::UnOp(fp_core::ast::ExprUnOp {
                    span: node.span,
                    op: UnOpKind::Neg,
                    val: Box::new(inner),
                })
                .into()),
                "!" => Ok(ExprKind::UnOp(fp_core::ast::ExprUnOp {
                    span: node.span,
                    op: UnOpKind::Not,
                    val: Box::new(inner),
                })
                .into()),
                "+" => {
                    // Unary plus is a no-op.
                    Ok(inner)
                }
                "*" => Ok(ExprKind::Dereference(fp_core::ast::ExprDereference {
                    span: node.span,
                    referee: Box::new(inner),
                })
                .into()),
                "&" => {
                    let is_mut = node.children.iter().any(|c| {
                        matches!(c, crate::syntax::SyntaxElement::Token(t) if !t.is_trivia() && t.text == "mut")
                    });
                    Ok(ExprKind::Reference(fp_core::ast::ExprReference {
                        span: node.span,
                        referee: Box::new(inner),
                        mutable: if is_mut { Some(true) } else { None },
                    })
                    .into())
                }
                _ => Err(LowerError::MissingOperator),
            }
        }
        SyntaxKind::ExprTry => {
            let mut expr = None;
            let mut catches = Vec::new();
            let mut elze = None;
            let mut finally = None;
            let mut pending_clause = None::<&str>;

            for child in &node.children {
                match child {
                    crate::syntax::SyntaxElement::Node(n) if n.kind == SyntaxKind::ExprTryCatch => {
                        catches.push(lower_try_catch_from_cst(n)?);
                    }
                    crate::syntax::SyntaxElement::Node(n)
                        if n.kind.category() == CstCategory::Expr =>
                    {
                        let lowered = Box::new(lower_expr_from_cst(n)?);
                        match (expr.is_none(), pending_clause) {
                            (true, _) => expr = Some(lowered),
                            (false, Some("else")) => elze = Some(lowered),
                            (false, Some("finally")) => finally = Some(lowered),
                            _ => return Err(LowerError::UnexpectedNode(SyntaxKind::ExprTry)),
                        }
                    }
                    crate::syntax::SyntaxElement::Token(tok) if !tok.is_trivia() => {
                        pending_clause = match tok.text.as_str() {
                            "else" => Some("else"),
                            "finally" => Some("finally"),
                            _ => None,
                        };
                    }
                    _ => {}
                }
            }

            let expr = expr.ok_or(LowerError::UnexpectedNode(SyntaxKind::ExprTry))?;
            Ok(ExprKind::Try(ExprTry {
                span: node.span,
                expr,
                catches,
                elze,
                finally,
            })
            .into())
        }
        SyntaxKind::ExprAttr => {
            let inner = node
                .children
                .iter()
                .rev()
                .find_map(|c| match c {
                    crate::syntax::SyntaxElement::Node(n)
                        if n.kind.category() == CstCategory::Expr =>
                    {
                        Some(n.as_ref())
                    }
                    _ => None,
                })
                .ok_or(LowerError::UnexpectedNode(SyntaxKind::ExprAttr))?;
            lower_expr_from_cst(inner)
        }
        SyntaxKind::ExprAwait => {
            let base = first_child_expr(node)?;
            let expr = lower_expr_from_cst(base)?;
            Ok(ExprKind::Await(ExprAwait {
                span: node.span,
                base: Box::new(expr),
            })
            .into())
        }
        SyntaxKind::ExprBinary => {
            let mut expr_nodes = node.children.iter().filter_map(|c| match c {
                crate::syntax::SyntaxElement::Node(n) => Some(n.as_ref()),
                _ => None,
            });
            let lhs_node = expr_nodes
                .next()
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::ExprBinary))?;
            let rhs_node = expr_nodes
                .next()
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::ExprBinary))?;
            let op = direct_operator_token_text(node).ok_or(LowerError::MissingOperator)?;

            if lhs_node.kind.category() == CstCategory::Pattern {
                let pattern = lower_pattern_from_cst(lhs_node)?;
                let rhs = lower_expr_from_cst(rhs_node)?;
                return lower_assign_pattern_expr(pattern, rhs, &op, node.span);
            }

            let lhs = lower_expr_from_cst(lhs_node)?;
            let rhs = lower_expr_from_cst(rhs_node)?;

            if op == "=" {
                return Ok(ExprKind::Assign(fp_core::ast::ExprAssign {
                    span: node.span,
                    target: Box::new(lhs),
                    value: Box::new(rhs),
                })
                .into());
            }

            if let Some(binop) = compound_assign_binop(&op) {
                let combined = ExprKind::BinOp(ExprBinOp {
                    span: node.span,
                    kind: binop,
                    lhs: Box::new(lhs.clone()),
                    rhs: Box::new(rhs),
                })
                .into();
                return Ok(ExprKind::Assign(fp_core::ast::ExprAssign {
                    span: node.span,
                    target: Box::new(lhs),
                    value: Box::new(combined),
                })
                .into());
            }

            let kind = binop_from_text(&op).ok_or(LowerError::MissingOperator)?;
            Ok(ExprKind::BinOp(ExprBinOp {
                span: node.span,
                kind,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            })
            .into())
        }
        SyntaxKind::ExprRange => {
            let op = direct_operator_token_text(node).ok_or(LowerError::MissingOperator)?;
            let limit = match op.as_str() {
                ".." => ExprRangeLimit::Exclusive,
                "..=" => ExprRangeLimit::Inclusive,
                _ => return Err(LowerError::MissingOperator),
            };
            let (start_node, end_node) = range_child_exprs(node)?;
            let start = start_node
                .map(lower_expr_from_cst)
                .transpose()?
                .map(Box::new);
            let end = end_node.map(lower_expr_from_cst).transpose()?.map(Box::new);
            Ok(ExprKind::Range(ExprRange {
                span: node.span,
                start,
                limit,
                end,
                step: None,
            })
            .into())
        }
        SyntaxKind::ExprSelect => {
            let obj = first_child_expr(node)?;
            let obj = lower_expr_from_cst(obj)?;
            let field = direct_last_ident_token_text(node)
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::ExprSelect))?;
            Ok(ExprKind::Select(ExprSelect {
                span: node.span,
                obj: Box::new(obj),
                field: Ident::new(field),
                select: ExprSelectType::Unknown,
            })
            .into())
        }
        SyntaxKind::ExprIndex => {
            let mut nodes = node_children_exprs(node);
            let obj = nodes
                .next()
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::ExprIndex))?;
            let index = nodes
                .next()
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::ExprIndex))?;
            let obj = lower_expr_from_cst(obj)?;
            let index = lower_expr_from_cst(index)?;
            Ok(ExprKind::Index(ExprIndex {
                span: node.span,
                obj: Box::new(obj),
                index: Box::new(index),
            })
            .into())
        }
        SyntaxKind::ExprSplat => {
            let inner = last_child_expr(node)?;
            let inner = lower_expr_from_cst(inner)?;
            Ok(ExprKind::Splat(fp_core::ast::ExprSplat {
                span: node.span,
                iter: Box::new(inner),
            })
            .into())
        }
        SyntaxKind::ExprSplatDict => {
            let inner = last_child_expr(node)?;
            let inner = lower_expr_from_cst(inner)?;
            Ok(ExprKind::SplatDict(fp_core::ast::ExprSplatDict {
                span: node.span,
                dict: Box::new(inner),
            })
            .into())
        }
        SyntaxKind::ExprCall => {
            let mut nodes = node_children_exprs(node);
            let callee = nodes
                .next()
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::ExprCall))?;
            let callee = lower_expr_from_cst(callee)?;
            let (args, kwargs) = lower_call_args_from_cst(nodes)?;

            let target = match callee.kind() {
                ExprKind::Name(locator) => ExprInvokeTarget::Function(locator.clone()),
                ExprKind::Select(select) => ExprInvokeTarget::Method(select.clone()),
                _ => ExprInvokeTarget::expr(callee),
            };

            Ok(ExprKind::Invoke(ExprInvoke {
                span: node.span,
                target,
                args,
                kwargs,
            })
            .into())
        }
        SyntaxKind::ExprMacroCall => {
            // Shape: <name> ! <group>
            let name = first_child_expr(node)?;
            if !matches!(
                name.kind,
                SyntaxKind::ExprName | SyntaxKind::ExprPath | SyntaxKind::ExprSelect
            ) {
                return Err(LowerError::UnexpectedNode(SyntaxKind::ExprMacroCall));
            }
            let path = macro_callee_path(name)?;

            let macro_tokens = crate::ast::macros::macro_group_tokens(node)
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::ExprMacroCall))?;

            Ok(ExprKind::Macro(fp_core::ast::ExprMacro::new(
                MacroInvocation::new(path, macro_tokens.delimiter, macro_tokens.text)
                    .with_token_trees(macro_tokens.token_trees)
                    .with_span(node.span),
            ))
            .into())
        }
        SyntaxKind::ExprCast => {
            let expr = node
                .children
                .iter()
                .find_map(|c| match c {
                    crate::syntax::SyntaxElement::Node(n)
                        if n.kind.category() == CstCategory::Expr =>
                    {
                        Some(n.as_ref())
                    }
                    _ => None,
                })
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::ExprCast))?;
            let ty_node = node
                .children
                .iter()
                .find_map(|c| match c {
                    crate::syntax::SyntaxElement::Node(n)
                        if n.kind.category() == CstCategory::Type =>
                    {
                        Some(n.as_ref())
                    }
                    _ => None,
                })
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::ExprCast))?;

            let expr = lower_expr_from_cst(expr)?;
            let ty = lower_type_from_cst(ty_node)?;
            Ok(ExprKind::Cast(fp_core::ast::ExprCast {
                span: node.span,
                expr: Box::new(expr),
                ty,
            })
            .into())
        }
        other => Err(LowerError::UnexpectedNode(other)),
    }?;

    if !matches!(node.kind, SyntaxKind::Root) {
        expr = expr.with_span(node.span);
    }

    Ok(expr)
}

fn lower_struct_fields(node: &SyntaxNode) -> Result<(Vec<ExprField>, Option<Expr>), LowerError> {
    let mut out = Vec::new();
    for child in &node.children {
        let crate::syntax::SyntaxElement::Node(field) = child else {
            continue;
        };
        if field.kind != SyntaxKind::StructField {
            continue;
        }
        let name = direct_first_non_trivia_token_text(field)
            .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::StructField))?;
        let name_ident = Ident::new(name.clone());
        let value = if let Some(expr_node) = node_children_exprs(field).next() {
            Some(lower_expr_from_cst(expr_node)?)
        } else {
            // Shorthand field: `x` => `x: x`.
            Some(Expr::ident(name_ident.clone()))
        };
        out.push(ExprField {
            span: field.span,
            name: name_ident,
            value,
        });
    }

    let mut update: Option<Expr> = None;
    let mut saw_update = false;
    for (idx, child) in node.children.iter().enumerate() {
        let crate::syntax::SyntaxElement::Token(tok) = child else {
            continue;
        };
        if tok.is_trivia() || tok.text != ".." {
            continue;
        }
        if update.is_some() {
            return Err(LowerError::UnexpectedNode(SyntaxKind::ExprStruct));
        }
        saw_update = true;
        for next in node.children.iter().skip(idx + 1) {
            match next {
                crate::syntax::SyntaxElement::Node(n) if n.kind.category() == CstCategory::Expr => {
                    update = Some(lower_expr_from_cst(n.as_ref())?);
                    break;
                }
                crate::syntax::SyntaxElement::Token(t) if !t.is_trivia() && t.text == "}" => {
                    break;
                }
                _ => {}
            }
        }
        // Allow `..` without an update expression. This is needed for destructuring assignments
        // and keeps FP permissive in parse-only mode.
    }

    Ok((out, update))
}

fn lower_pattern_from_cst(node: &SyntaxNode) -> Result<Pattern, LowerError> {
    match node.kind {
        SyntaxKind::PatternIdent => {
            let mut name: Option<String> = None;
            let mut is_mut = false;
            for child in &node.children {
                let crate::syntax::SyntaxElement::Token(t) = child else {
                    continue;
                };
                if t.is_trivia() {
                    continue;
                }
                if t.text == "mut" {
                    is_mut = true;
                    continue;
                }
                if name.is_none() {
                    name = Some(t.text.clone());
                }
            }
            let name = name.ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::PatternIdent))?;
            let mut ident = PatternIdent::new(Ident::new(name));
            if is_mut {
                ident.mutability = Some(true);
            }
            Ok(Pattern::new(PatternKind::Ident(ident)))
        }
        SyntaxKind::PatternWildcard => Ok(Pattern::new(PatternKind::Wildcard(PatternWildcard {}))),
        SyntaxKind::PatternTuple => {
            let patterns = node
                .children
                .iter()
                .filter_map(|c| match c {
                    crate::syntax::SyntaxElement::Node(n)
                        if matches!(
                            n.kind,
                            SyntaxKind::PatternIdent
                                | SyntaxKind::PatternWildcard
                                | SyntaxKind::PatternTuple
                                | SyntaxKind::PatternType
                                | SyntaxKind::PatternStruct
                                | SyntaxKind::PatternTupleStruct
                                | SyntaxKind::PatternBox
                                | SyntaxKind::PatternRef
                                | SyntaxKind::PatternSlice
                                | SyntaxKind::PatternRest
                                | SyntaxKind::PatternBind
                                | SyntaxKind::PatternPath
                                | SyntaxKind::PatternParen
                        ) =>
                    {
                        Some(n.as_ref())
                    }
                    _ => None,
                })
                .map(|n| {
                    if n.kind == SyntaxKind::PatternRest {
                        Ok(Pattern::new(PatternKind::Wildcard(PatternWildcard {})))
                    } else {
                        lower_pattern_from_cst(n)
                    }
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Pattern::new(PatternKind::Tuple(PatternTuple { patterns })))
        }
        SyntaxKind::PatternParen => {
            let inner =
                first_child_in_category(node, CstCategory::Pattern, SyntaxKind::PatternParen)?;
            lower_pattern_from_cst(inner)
        }
        SyntaxKind::PatternSlice => {
            let patterns = node
                .children
                .iter()
                .filter_map(|c| match c {
                    crate::syntax::SyntaxElement::Node(n)
                        if n.kind.category() == CstCategory::Pattern =>
                    {
                        Some(n.as_ref())
                    }
                    _ => None,
                })
                .map(|n| {
                    if n.kind == SyntaxKind::PatternRest {
                        Ok(Pattern::new(PatternKind::Wildcard(PatternWildcard {})))
                    } else {
                        lower_pattern_from_cst(n)
                    }
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Pattern::new(PatternKind::Tuple(PatternTuple { patterns })))
        }
        SyntaxKind::PatternRest => Ok(Pattern::new(PatternKind::Wildcard(PatternWildcard {}))),
        SyntaxKind::PatternPath => {
            let name = lower_pattern_path_name(node)?;
            Ok(Pattern::new(PatternKind::Variant(PatternVariant {
                name: Expr::name(name),
                pattern: None,
            })))
        }
        SyntaxKind::PatternBind => {
            let mut pattern_nodes = node
                .children
                .iter()
                .filter_map(|c| match c {
                    crate::syntax::SyntaxElement::Node(n)
                        if n.kind.category() == CstCategory::Pattern =>
                    {
                        Some(n.as_ref())
                    }
                    _ => None,
                });
            let lhs = pattern_nodes
                .next()
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::PatternBind))?;
            let rhs = pattern_nodes
                .next()
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::PatternBind))?;
            let ident = match lhs.kind {
                SyntaxKind::PatternIdent => match lower_pattern_from_cst(lhs)?.kind() {
                    PatternKind::Ident(ident) => ident.clone(),
                    _ => return Err(LowerError::UnexpectedNode(SyntaxKind::PatternBind)),
                },
                SyntaxKind::PatternPath => {
                    let name = lower_pattern_path_name(lhs)?;
                    match name {
                        Name::Ident(ident) => PatternIdent::new(ident),
                        Name::Path(path)
                            if path.prefix == PathPrefix::Plain && path.segments.len() == 1 =>
                        {
                            PatternIdent::new(path.segments[0].clone())
                        }
                        _ => return Err(LowerError::UnexpectedNode(SyntaxKind::PatternBind)),
                    }
                }
                _ => return Err(LowerError::UnexpectedNode(SyntaxKind::PatternBind)),
            };
            let pattern = lower_pattern_from_cst(rhs)?;
            Ok(Pattern::new(PatternKind::Bind(PatternBind {
                ident,
                pattern: Box::new(pattern),
            })))
        }
        SyntaxKind::PatternStruct => {
            let name_node = node
                .children
                .iter()
                .find_map(|c| match c {
                    crate::syntax::SyntaxElement::Node(n)
                        if n.kind == SyntaxKind::PatternPath =>
                    {
                        Some(n.as_ref())
                    }
                    _ => None,
                })
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::PatternStruct))?;
            let name = lower_pattern_path_name(name_node)?;

            let mut fields = Vec::new();
            for child in &node.children {
                let crate::syntax::SyntaxElement::Node(field) = child else {
                    continue;
                };
                if field.kind != SyntaxKind::StructField {
                    continue;
                }
                let field_name = direct_first_non_trivia_token_text(field)
                    .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::StructField))?;
                let mut rename = None;
                if let Some(value_pat) = field.children.iter().find_map(|c| match c {
                    crate::syntax::SyntaxElement::Node(n)
                        if n.kind.category() == CstCategory::Pattern =>
                    {
                        Some(n.as_ref())
                    }
                    _ => None,
                }) {
                    rename = Some(Box::new(lower_pattern_from_cst(value_pat)?));
                }
                fields.push(PatternStructField {
                    name: Ident::new(field_name),
                    rename,
                });
            }

            let has_rest = node.children.iter().any(|c| {
                matches!(
                    c,
                    crate::syntax::SyntaxElement::Node(n) if n.kind == SyntaxKind::PatternRest
                )
            });

            match name {
                Name::Ident(ident) => Ok(Pattern::new(PatternKind::Struct(PatternStruct {
                    name: ident,
                    fields,
                    has_rest,
                }))),
                Name::Path(path)
                    if path.prefix == PathPrefix::Plain && path.segments.len() == 1 =>
                {
                    Ok(Pattern::new(PatternKind::Struct(PatternStruct {
                        name: path.segments[0].clone(),
                        fields,
                        has_rest,
                    })))
                }
                _ => Ok(Pattern::new(PatternKind::Variant(PatternVariant {
                    name: Expr::name(name),
                    pattern: Some(Box::new(Pattern::new(PatternKind::Structural(
                        PatternStructural { fields, has_rest },
                    )))),
                }))),
            }
        }
        SyntaxKind::PatternTupleStruct => {
            let name_node = node
                .children
                .iter()
                .find_map(|c| match c {
                    crate::syntax::SyntaxElement::Node(n)
                        if n.kind == SyntaxKind::PatternPath =>
                    {
                        Some(n.as_ref())
                    }
                    _ => None,
                })
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::PatternTupleStruct))?;
            let name = lower_pattern_path_name(name_node)?;
            let patterns = node
                .children
                .iter()
                .filter_map(|c| match c {
                    crate::syntax::SyntaxElement::Node(n)
                        if n.kind.category() == CstCategory::Pattern
                            && n.kind != SyntaxKind::PatternPath =>
                    {
                        Some(n.as_ref())
                    }
                    _ => None,
                })
                .map(|n| {
                    if n.kind == SyntaxKind::PatternRest {
                        Ok(Pattern::new(PatternKind::Wildcard(PatternWildcard {})))
                    } else {
                        lower_pattern_from_cst(n)
                    }
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Pattern::new(PatternKind::TupleStruct(PatternTupleStruct {
                name,
                patterns,
            })))
        }
        SyntaxKind::PatternBox => {
            let pat_node =
                first_child_in_category(node, CstCategory::Pattern, SyntaxKind::PatternBox)?;
            let pattern = lower_pattern_from_cst(pat_node)?;
            Ok(Pattern::new(PatternKind::Box(PatternBox {
                pattern: Box::new(pattern),
            })))
        }
        SyntaxKind::PatternRef => {
            let mut is_mut = false;
            let pat_node = node
                .children
                .iter()
                .find_map(|c| match c {
                    crate::syntax::SyntaxElement::Node(n)
                        if n.kind.category() == CstCategory::Pattern =>
                    {
                        Some(n.as_ref())
                    }
                    crate::syntax::SyntaxElement::Token(t)
                        if !t.is_trivia() && t.text == "mut" =>
                    {
                        is_mut = true;
                        None
                    }
                    _ => None,
                })
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::PatternRef))?;
            let pattern = lower_pattern_from_cst(pat_node)?;
            Ok(Pattern::new(PatternKind::Ref(PatternRef {
                mutability: if is_mut { Some(true) } else { None },
                pattern: Box::new(pattern),
            })))
        }
        SyntaxKind::PatternType => {
            let pat_node =
                first_child_in_category(node, CstCategory::Pattern, SyntaxKind::PatternType)?;
            let ty_node = first_child_in_category(node, CstCategory::Type, SyntaxKind::PatternType)?;
            let pat = lower_pattern_from_cst(pat_node)?;
            let ty = lower_type_from_cst(ty_node)?;
            Ok(Pattern::new(PatternKind::Type(PatternType::new(pat, ty))))
        }
        SyntaxKind::ExprName => {
            let name = direct_first_non_trivia_token_text(node)
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::ExprName))?;
            if name == "_" {
                return Ok(Pattern::new(PatternKind::Wildcard(PatternWildcard {})));
            }
            if name == "true" || name == "false" {
                return Ok(Pattern::new(PatternKind::Variant(PatternVariant {
                    name: Expr::value(Value::bool(name == "true")),
                    pattern: None,
                })));
            }
            Ok(Pattern::new(PatternKind::Ident(PatternIdent::new(
                Ident::new(name),
            ))))
        }
        SyntaxKind::ExprNumber | SyntaxKind::ExprString | SyntaxKind::ExprRange => {
            let expr = lower_expr_from_cst(node)?;
            Ok(Pattern::new(PatternKind::Variant(PatternVariant {
                name: expr,
                pattern: None,
            })))
        }
        other => Err(LowerError::UnexpectedNode(other)),
    }
}

fn lower_assign_pattern_expr(
    pattern: Pattern,
    rhs: Expr,
    op: &str,
    span: fp_core::span::Span,
) -> Result<Expr, LowerError> {
    if op != "=" {
        if let Some(binop) = compound_assign_binop(op) {
            if let Some(target_ident) = assign_pattern_target_ident(&pattern) {
                let target_expr = Expr::ident(target_ident);
                let combined = ExprKind::BinOp(ExprBinOp {
                    span,
                    kind: binop,
                    lhs: Box::new(target_expr.clone()),
                    rhs: Box::new(rhs),
                })
                .into();
                return Ok(ExprKind::Assign(fp_core::ast::ExprAssign {
                    span,
                    target: Box::new(target_expr),
                    value: Box::new(combined),
                })
                .into());
            }
        }
        return Err(LowerError::Unsupported(format!(
            "pattern assignment operator '{op}'"
        )));
    }

    let mut counter = 0usize;
    let mut assignments = Vec::new();
    let rewritten_pattern = rewrite_assign_pattern(pattern, &mut counter, &mut assignments);
    let rhs_ident = Ident::new(format!("__fp_assign_rhs{counter}"));

    let mut block = ExprBlock::new();
    block.push_stmt(BlockStmt::Let(StmtLet::new_simple(rhs_ident.clone(), rhs)));
    let match_expr = build_assign_pattern_match(rhs_ident, rewritten_pattern, assignments, span);
    block.push_stmt(BlockStmt::Expr(
        BlockStmtExpr::new(match_expr).with_semicolon(true),
    ));
    block.push_expr(Expr::unit());
    Ok(ExprKind::Block(block).into())
}

fn assign_pattern_target_ident(pattern: &Pattern) -> Option<Ident> {
    match pattern.kind() {
        PatternKind::Ident(ident) => Some(ident.ident.clone()),
        PatternKind::Bind(bind) => Some(bind.ident.ident.clone()),
        PatternKind::Type(pattern_type) => assign_pattern_target_ident(&pattern_type.pat),
        PatternKind::Ref(pattern_ref) => assign_pattern_target_ident(&pattern_ref.pattern),
        PatternKind::Box(pattern_box) => assign_pattern_target_ident(&pattern_box.pattern),
        _ => None,
    }
}

fn rewrite_assign_pattern(
    pattern: Pattern,
    counter: &mut usize,
    assignments: &mut Vec<(Ident, Ident)>,
) -> Pattern {
    let (ty, kind) = pattern.into_parts();
    let kind = match kind {
        PatternKind::Ident(ident) => {
            let temp = next_assign_temp_ident(counter);
            assignments.push((ident.ident.clone(), temp.clone()));
            let mut rewritten = PatternIdent::new(temp);
            rewritten.mutability = ident.mutability;
            PatternKind::Ident(rewritten)
        }
        PatternKind::Bind(bind) => {
            let temp = next_assign_temp_ident(counter);
            assignments.push((bind.ident.ident.clone(), temp.clone()));
            let mut rewritten_ident = PatternIdent::new(temp);
            rewritten_ident.mutability = bind.ident.mutability;
            let inner = rewrite_assign_pattern(*bind.pattern, counter, assignments);
            PatternKind::Bind(PatternBind {
                ident: rewritten_ident,
                pattern: Box::new(inner),
            })
        }
        PatternKind::Type(pattern_type) => {
            let inner = rewrite_assign_pattern(*pattern_type.pat, counter, assignments);
            PatternKind::Type(PatternType::new(inner, pattern_type.ty))
        }
        PatternKind::Tuple(tuple) => {
            let patterns = tuple
                .patterns
                .into_iter()
                .map(|pat| rewrite_assign_pattern(pat, counter, assignments))
                .collect();
            PatternKind::Tuple(PatternTuple { patterns })
        }
        PatternKind::TupleStruct(tuple_struct) => {
            let patterns = tuple_struct
                .patterns
                .into_iter()
                .map(|pat| rewrite_assign_pattern(pat, counter, assignments))
                .collect();
            PatternKind::TupleStruct(PatternTupleStruct {
                name: tuple_struct.name,
                patterns,
            })
        }
        PatternKind::Struct(pattern_struct) => {
            let fields = rewrite_assign_struct_fields(pattern_struct.fields, counter, assignments);
            PatternKind::Struct(PatternStruct {
                name: pattern_struct.name,
                fields,
                has_rest: pattern_struct.has_rest,
            })
        }
        PatternKind::Structural(pattern_struct) => {
            let fields = rewrite_assign_struct_fields(pattern_struct.fields, counter, assignments);
            PatternKind::Structural(PatternStructural {
                fields,
                has_rest: pattern_struct.has_rest,
            })
        }
        PatternKind::Box(pattern_box) => {
            let inner = rewrite_assign_pattern(*pattern_box.pattern, counter, assignments);
            PatternKind::Box(PatternBox {
                pattern: Box::new(inner),
            })
        }
        PatternKind::Ref(pattern_ref) => {
            let inner = rewrite_assign_pattern(*pattern_ref.pattern, counter, assignments);
            PatternKind::Ref(PatternRef {
                mutability: pattern_ref.mutability,
                pattern: Box::new(inner),
            })
        }
        PatternKind::Variant(variant) => {
            let pattern = variant
                .pattern
                .map(|pat| Box::new(rewrite_assign_pattern(*pat, counter, assignments)));
            PatternKind::Variant(PatternVariant {
                name: variant.name,
                pattern,
            })
        }
        PatternKind::Quote(quote) => PatternKind::Quote(quote),
        PatternKind::QuotePlural(quote) => PatternKind::QuotePlural(quote),
        PatternKind::Wildcard(wildcard) => PatternKind::Wildcard(wildcard),
    };
    Pattern::from_parts(ty, kind)
}

fn rewrite_assign_struct_fields(
    fields: Vec<PatternStructField>,
    counter: &mut usize,
    assignments: &mut Vec<(Ident, Ident)>,
) -> Vec<PatternStructField> {
    fields
        .into_iter()
        .map(|field| {
            let rename = match field.rename {
                Some(rename) => Some(Box::new(rewrite_assign_pattern(
                    *rename,
                    counter,
                    assignments,
                ))),
                None => {
                    let temp = next_assign_temp_ident(counter);
                    assignments.push((field.name.clone(), temp.clone()));
                    Some(Box::new(Pattern::new(PatternKind::Ident(
                        PatternIdent::new(temp),
                    ))))
                }
            };
            PatternStructField {
                name: field.name,
                rename,
            }
        })
        .collect()
}

fn build_assign_pattern_match(
    rhs_ident: Ident,
    pattern: Pattern,
    assignments: Vec<(Ident, Ident)>,
    span: fp_core::span::Span,
) -> Expr {
    let mut arm_block = ExprBlock::new();
    for (target, source) in assignments {
        let assign_expr = Expr::new(ExprKind::Assign(fp_core::ast::ExprAssign {
            span,
            target: Box::new(Expr::ident(target)),
            value: Box::new(Expr::ident(source)),
        }));
        arm_block.push_stmt(BlockStmt::Expr(
            BlockStmtExpr::new(assign_expr).with_semicolon(true),
        ));
    }
    arm_block.push_expr(Expr::unit());
    let arm_expr = ExprKind::Block(arm_block).into();

    let match_case = ExprMatchCase {
        span,
        pat: Some(Box::new(pattern)),
        cond: Box::new(Expr::value(Value::bool(true))),
        guard: None,
        body: Box::new(arm_expr),
    };
    let wildcard_case = ExprMatchCase {
        span,
        pat: Some(Box::new(Pattern::new(PatternKind::Wildcard(
            PatternWildcard {},
        )))),
        cond: Box::new(Expr::value(Value::bool(true))),
        guard: None,
        body: Box::new(Expr::unit()),
    };
    ExprKind::Match(ExprMatch {
        span,
        scrutinee: Some(Box::new(Expr::ident(rhs_ident))),
        cases: vec![match_case, wildcard_case],
    })
    .into()
}

fn next_assign_temp_ident(counter: &mut usize) -> Ident {
    let name = format!("__fp_assign_pat{counter}");
    *counter += 1;
    Ident::new(name)
}

fn lower_block_from_expr_parent(
    node: &SyntaxNode,
    expected_kind: SyntaxKind,
) -> Result<ExprBlock, LowerError> {
    let block_node = node_children_exprs(node)
        .find(|n| n.kind == SyntaxKind::ExprBlock)
        .ok_or_else(|| LowerError::UnexpectedNode(expected_kind))?;
    lower_block_from_cst(block_node)
}

fn lower_block_expr_from_cst(node: &SyntaxNode) -> Result<Expr, LowerError> {
    let block = lower_block_from_cst(node)?;
    Ok(ExprKind::Block(block).into())
}

fn first_child_in_category<'a>(
    node: &'a SyntaxNode,
    category: CstCategory,
    expected_kind: SyntaxKind,
) -> Result<&'a SyntaxNode, LowerError> {
    node.children
        .iter()
        .find_map(|c| match c {
            crate::syntax::SyntaxElement::Node(n) if n.kind.category() == category => {
                Some(n.as_ref())
            }
            _ => None,
        })
        .ok_or_else(|| LowerError::UnexpectedNode(expected_kind))
}

fn lower_pattern_path_name(node: &SyntaxNode) -> Result<Name, LowerError> {
    let path_tokens = collect_path_tokens_with_generics(node);
    let (prefix, segments) = split_path_prefix(path_tokens.segments, path_tokens.saw_root);
    if segments.is_empty() && matches!(prefix, PathPrefix::Plain | PathPrefix::Root) {
        return Err(LowerError::UnexpectedNode(node.kind));
    }
    Ok(Name::path(Path::new(prefix, segments)))
}

fn lower_block_from_cst(node: &SyntaxNode) -> Result<ExprBlock, LowerError> {
    if node.kind != SyntaxKind::ExprBlock {
        return Err(LowerError::UnexpectedNode(node.kind));
    }
    let mut stmts: Vec<BlockStmt> = Vec::new();
    for child in &node.children {
        let crate::syntax::SyntaxElement::Node(stmt) = child else {
            continue;
        };
        match stmt.kind {
            SyntaxKind::BlockStmtItem => {
                let item_node = stmt
                    .children
                    .iter()
                    .find_map(|c| match c {
                        crate::syntax::SyntaxElement::Node(n)
                            if n.kind.category() == CstCategory::Item =>
                        {
                            Some(n.as_ref())
                        }
                        _ => None,
                    })
                    .ok_or(LowerError::UnexpectedNode(SyntaxKind::BlockStmtItem))?;
                if item_node.kind == SyntaxKind::ItemExternBlock {
                    let items = crate::ast::items::lower_extern_block(item_node)?;
                    for item in items {
                        stmts.push(BlockStmt::item(item));
                    }
                } else {
                    let item = crate::ast::items::lower_item_from_cst(item_node)?;
                    stmts.push(BlockStmt::item(item));
                }
            }
            SyntaxKind::BlockStmtLet => stmts.push(lower_let_stmt(stmt)?),
            SyntaxKind::BlockStmtDefer => stmts.push(lower_defer_stmt(stmt)?),
            SyntaxKind::BlockStmtExpr => {
                let expr_node = stmt
                    .children
                    .iter()
                    .find_map(|c| match c {
                        crate::syntax::SyntaxElement::Node(n) => Some(n.as_ref()),
                        _ => None,
                    })
                    .ok_or(LowerError::UnexpectedNode(SyntaxKind::BlockStmtExpr))?;
                let expr = lower_expr_from_cst(expr_node)?;
                let has_semicolon = stmt
                    .children
                    .iter()
                    .any(|c| matches!(c, crate::syntax::SyntaxElement::Token(t) if !t.is_trivia() && t.text == ";"));
                let stmt_expr = BlockStmtExpr::new(expr).with_semicolon(has_semicolon);
                stmts.push(BlockStmt::Expr(stmt_expr));
            }
            _ => {}
        }
    }
    let mut block = ExprBlock::new_stmts(stmts);
    block.span = node.span;
    Ok(block)
}

fn lower_let_stmt(node: &SyntaxNode) -> Result<BlockStmt, LowerError> {
    let mut pattern: Option<Pattern> = None;
    let mut ty: Option<Ty> = None;
    let mut expr_nodes: Vec<&SyntaxNode> = Vec::new();

    for child in &node.children {
        match child {
            crate::syntax::SyntaxElement::Node(n) if n.kind.category() == CstCategory::Pattern => {
                if pattern.is_none() {
                    pattern = Some(lower_pattern_from_cst(n)?);
                }
            }
            crate::syntax::SyntaxElement::Node(n) if n.kind.category() == CstCategory::Type => {
                ty = Some(lower_type_from_cst(n)?);
            }
            crate::syntax::SyntaxElement::Node(n) if n.kind.category() == CstCategory::Expr => {
                expr_nodes.push(n);
            }
            crate::syntax::SyntaxElement::Node(n) if matches!(
                n.kind,
                SyntaxKind::ExprName
                    | SyntaxKind::ExprNumber
                    | SyntaxKind::ExprBinary
                    | SyntaxKind::ExprCall
                    | SyntaxKind::ExprIndex
                    | SyntaxKind::ExprSelect
                    | SyntaxKind::ExprCast
                    | SyntaxKind::ExprRange
                    | SyntaxKind::ExprTry
                    | SyntaxKind::ExprAwait
                    | SyntaxKind::ExprUnary
                    | SyntaxKind::ExprMacroCall
                    | SyntaxKind::ExprParen
                    | SyntaxKind::ExprBlock
            ) => {
                expr_nodes.push(n);
            }
            _ => {}
        }
    }

    let Some(pat) = pattern else {
        return Err(LowerError::UnexpectedNode(SyntaxKind::BlockStmtLet));
    };

    let mut init_expr = None;
    let mut diverge_expr = None;
    if let Some(first) = expr_nodes.get(0) {
        init_expr = Some(lower_expr_from_cst(first)?);
    }
    if let Some(second) = expr_nodes.get(1) {
        diverge_expr = Some(lower_expr_from_cst(second)?);
    }
    if init_expr.is_none() {
        diverge_expr = None;
    }

    let stmt = match (ty, init_expr) {
        (Some(ty), Some(init)) => {
            let typed_pat = Pattern::from(PatternKind::Type(PatternType::new(pat, ty)));
            StmtLet::new(typed_pat, Some(init), diverge_expr)
        }
        (Some(_ty), None) => StmtLet::new(pat, None, None),
        (None, init) => StmtLet::new(pat, init, diverge_expr),
    };
    Ok(BlockStmt::Let(stmt))
}

fn lower_defer_stmt(node: &SyntaxNode) -> Result<BlockStmt, LowerError> {
    let expr = first_child_expr(node)?;
    Ok(BlockStmt::Defer(StmtDefer {
        span: node.span,
        expr: Box::new(lower_expr_from_cst(expr)?),
    }))
}

fn lower_try_catch_from_cst(node: &SyntaxNode) -> Result<ExprTryCatch, LowerError> {
    let mut pat = None;
    let mut body = None;

    for child in &node.children {
        match child {
            crate::syntax::SyntaxElement::Node(n) if n.kind.category() == CstCategory::Pattern => {
                pat = Some(Box::new(lower_pattern_from_cst(n)?));
            }
            crate::syntax::SyntaxElement::Node(n) if n.kind.category() == CstCategory::Expr => {
                body = Some(Box::new(lower_expr_from_cst(n)?));
            }
            _ => {}
        }
    }

    let body = body.ok_or(LowerError::UnexpectedNode(SyntaxKind::ExprTryCatch))?;
    Ok(ExprTryCatch {
        span: node.span,
        pat,
        body,
    })
}

fn split_match_arm<'a>(
    arm: &'a SyntaxNode,
) -> Result<(&'a SyntaxNode, Option<&'a SyntaxNode>, &'a SyntaxNode), LowerError> {
    let mut pat: Option<&SyntaxNode> = None;
    let mut exprs: Vec<&SyntaxNode> = Vec::new();

    for child in &arm.children {
        let crate::syntax::SyntaxElement::Node(n) = child else {
            continue;
        };
        if n.kind.category() == CstCategory::Pattern && pat.is_none() {
            pat = Some(n.as_ref());
            continue;
        }
        if n.kind.category() == CstCategory::Expr {
            exprs.push(n.as_ref());
        }
    }

    let pat = match pat {
        Some(pat) => pat,
        None => {
            let first = exprs
                .first()
                .copied()
                .ok_or(LowerError::UnexpectedNode(SyntaxKind::MatchArm))?;
            exprs.remove(0);
            first
        }
    };

    match exprs.as_slice() {
        [body] => Ok((pat, None, *body)),
        [guard, body] => Ok((pat, Some(*guard), *body)),
        _ => Err(LowerError::UnexpectedNode(SyntaxKind::MatchArm)),
    }
}

fn lower_match_pattern_from_cst(node: &SyntaxNode) -> Result<Pattern, LowerError> {
    if node.kind.category() == CstCategory::Pattern {
        return lower_pattern_from_cst(node);
    }
    // Patterns are currently parsed as expression CST nodes in match arms.
    // We lower a small subset of expressions into `Pattern` so that the typer
    // can bind names for match bodies.
    match node.kind {
        SyntaxKind::ExprName => {
            let name = direct_first_non_trivia_token_text(node)
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::ExprName))?;
            if name == "_" {
                return Ok(Pattern::new(PatternKind::Wildcard(PatternWildcard {})));
            }
            if name == "true" || name == "false" {
                return Ok(Pattern::new(PatternKind::Variant(PatternVariant {
                    name: Expr::value(Value::bool(name == "true")),
                    pattern: None,
                })));
            }
            Ok(Pattern::new(PatternKind::Ident(PatternIdent::new(
                Ident::new(name),
            ))))
        }
        SyntaxKind::ExprNumber | SyntaxKind::ExprString => {
            let expr = lower_expr_from_cst(node)?;
            Ok(Pattern::new(PatternKind::Variant(PatternVariant {
                name: expr,
                pattern: None,
            })))
        }
        SyntaxKind::ExprRange => Ok(Pattern::new(PatternKind::Wildcard(PatternWildcard {}))),
        SyntaxKind::ExprPath => {
            let expr = lower_expr_from_cst(node)?;
            if let ExprKind::Name(name) = expr.kind() {
                if let Some(ident) = name.as_ident() {
                    if ident.as_str() == "true" || ident.as_str() == "false" {
                        return Ok(Pattern::new(PatternKind::Variant(PatternVariant {
                            name: Expr::value(Value::bool(ident.as_str() == "true")),
                            pattern: None,
                        })));
                    }
                }
            }
            Ok(Pattern::new(PatternKind::Variant(PatternVariant {
                name: expr,
                pattern: None,
            })))
        }
        SyntaxKind::ExprCall => {
            // Tuple-struct/enum-variant pattern: `Path(p0, p1, ...)`.
            let mut exprs = node_children_exprs(node);
            let callee = exprs
                .next()
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::ExprCall))?;
            if callee.kind == SyntaxKind::ExprQuoteToken {
                let (fragment, _item, plural) = quote_pattern_kind_from_cst(callee)?;
                if !plural {
                    return Err(LowerError::UnexpectedNode(SyntaxKind::ExprCall));
                }
                let patterns = exprs
                    .map(lower_match_pattern_from_cst)
                    .collect::<Result<Vec<_>, _>>()?;
                return Ok(Pattern::new(PatternKind::QuotePlural(PatternQuotePlural {
                    fragment,
                    patterns,
                })));
            }
            let callee_expr = lower_expr_from_cst(callee)?;
            let locator = match callee_expr.kind() {
                ExprKind::Name(locator) => locator.clone(),
                _ => return Err(LowerError::UnexpectedNode(SyntaxKind::ExprCall)),
            };
            let mut pattern_nodes = Vec::new();
            for node in exprs {
                if node.kind == SyntaxKind::ExprKwArg {
                    return Err(LowerError::Unsupported(
                        "keyword arguments are not allowed in patterns".to_string(),
                    ));
                }
                pattern_nodes.push(node);
            }
            let patterns = pattern_nodes
                .into_iter()
                .map(lower_match_pattern_from_cst)
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Pattern::new(PatternKind::TupleStruct(PatternTupleStruct {
                name: locator,
                patterns,
            })))
        }
        SyntaxKind::ExprStruct => {
            // Struct-like enum variant pattern: `Enum::Variant { .. }`.
            // We preserve the qualified variant name and (optionally) record
            // whether `..` was present so that Rust codegen can print a valid
            // struct-variant pattern.
            let has_rest = node.children.iter().any(|child| {
                matches!(
                    child,
                    crate::syntax::SyntaxElement::Token(tok) if !tok.is_trivia() && tok.text == ".."
                )
            });
            let mut exprs = node_children_exprs(node);
            let name_node = exprs
                .next()
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::ExprStruct))?;

            let mut fields = Vec::new();
            for child in &node.children {
                let crate::syntax::SyntaxElement::Node(field) = child else {
                    continue;
                };
                if field.kind != SyntaxKind::StructField {
                    continue;
                }
                let name = direct_first_non_trivia_token_text(field)
                    .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::StructField))?;
                let mut rename = None;
                if let Some(value_expr) = node_children_exprs(field).next() {
                    let pattern = lower_match_pattern_from_cst(value_expr)?;
                    rename = Some(Box::new(pattern));
                }
                fields.push(PatternStructField {
                    name: Ident::new(name),
                    rename,
                });
            }

            if name_node.kind == SyntaxKind::ExprQuoteToken {
                let (fragment, item, plural) = quote_pattern_kind_from_cst(name_node)?;
                if plural {
                    return Err(LowerError::UnexpectedNode(SyntaxKind::ExprStruct));
                }
                let mut quote = PatternQuote {
                    fragment,
                    item,
                    fields: Vec::new(),
                    has_rest: false,
                };
                quote.fields = fields;
                quote.has_rest = has_rest;
                return Ok(Pattern::new(PatternKind::Quote(quote)));
            }

            let name_expr = lower_expr_from_cst(name_node)?;

            if let ExprKind::Name(locator) = name_expr.kind() {
                if let Some(ident) = locator.as_ident() {
                    let item_kind = match ident.as_str() {
                        "fn" => Some(QuoteItemKind::Function),
                        "struct" => Some(QuoteItemKind::Struct),
                        "enum" => Some(QuoteItemKind::Enum),
                        "trait" => Some(QuoteItemKind::Trait),
                        "impl" => Some(QuoteItemKind::Impl),
                        "const" => Some(QuoteItemKind::Const),
                        "static" => Some(QuoteItemKind::Static),
                        "mod" => Some(QuoteItemKind::Module),
                        "use" => Some(QuoteItemKind::Use),
                        "macro" => Some(QuoteItemKind::Macro),
                        "item" => None,
                        _ => {
                            // Not a quote item pattern; continue with regular struct/variant logic.
                            None
                        }
                    };
                    if item_kind.is_some() || ident.as_str() == "item" {
                        return Ok(Pattern::new(PatternKind::Quote(PatternQuote {
                            fragment: QuoteFragmentKind::Item,
                            item: item_kind,
                            fields,
                            has_rest,
                        })));
                    }
                }
            }

            if let ExprKind::Name(locator) = name_expr.kind() {
                let ident = match locator {
                    Name::Ident(ident) => Some(ident.clone()),
                    Name::Path(path)
                        if path.prefix == PathPrefix::Plain && path.segments.len() == 1 =>
                    {
                        Some(path.segments[0].clone())
                    }
                    _ => None,
                };
                if let Some(ident) = ident {
                    return Ok(Pattern::new(PatternKind::Struct(PatternStruct {
                        name: ident,
                        fields,
                        has_rest,
                    })));
                }
            }

            Ok(Pattern::new(PatternKind::Variant(PatternVariant {
                name: name_expr,
                pattern: Some(Box::new(Pattern::new(PatternKind::Structural(
                    PatternStructural { fields, has_rest },
                )))),
            })))
        }
        SyntaxKind::ExprTuple => {
            let patterns = node_children_exprs(node)
                .map(lower_match_pattern_from_cst)
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Pattern::new(PatternKind::Tuple(PatternTuple { patterns })))
        }
        SyntaxKind::ExprBinary => {
            let op = direct_operator_token_text(node).ok_or(LowerError::MissingOperator)?;
            if op == "@" {
                let lhs = first_child_expr(node)?;
                let rhs = last_child_expr(node)?;
                let name = match lhs.kind {
                    SyntaxKind::ExprName => direct_first_non_trivia_token_text(lhs)
                        .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::ExprName))?,
                    _ => return Err(LowerError::UnexpectedNode(SyntaxKind::ExprBinary)),
                };
                let pattern = lower_match_pattern_from_cst(rhs)?;
                return Ok(Pattern::new(PatternKind::Bind(PatternBind {
                    ident: PatternIdent::new(Ident::new(name)),
                    pattern: Box::new(pattern),
                })));
            }
            Err(LowerError::UnexpectedNode(SyntaxKind::ExprBinary))
        }
        SyntaxKind::ExprUnary => {
            let op = direct_operator_token_text(node).ok_or(LowerError::MissingOperator)?;
            if op == "box" {
                let rhs = last_child_expr(node)?;
                let pattern = lower_match_pattern_from_cst(rhs)?;
                return Ok(Pattern::new(PatternKind::Box(PatternBox {
                    pattern: Box::new(pattern),
                })));
            }
            Err(LowerError::UnexpectedNode(SyntaxKind::ExprUnary))
        }
        SyntaxKind::ExprQuote => {
            let quote = quote_block_pattern_from_cst(node)?;
            Ok(Pattern::new(PatternKind::Quote(quote)))
        }
        SyntaxKind::ExprQuoteToken => {
            let quote = quote_pattern_from_cst(node)?;
            Ok(Pattern::new(PatternKind::Quote(quote)))
        }
        other => Err(LowerError::UnexpectedNode(other)),
    }
}

fn lower_closure_from_cst(node: &SyntaxNode) -> Result<(Vec<Pattern>, Expr), LowerError> {
    let params = node
        .children
        .iter()
        .filter_map(|c| match c {
            crate::syntax::SyntaxElement::Node(n) if n.kind.category() == CstCategory::Pattern => {
                Some(n.as_ref())
            }
            _ => None,
        })
        .map(lower_pattern_from_cst)
        .collect::<Result<Vec<_>, _>>()?;

    // Body is the last child expr node.
    let body_node = last_child_expr(node)?;
    let body = lower_expr_from_cst(body_node)?;
    Ok((params, body))
}

fn first_child_expr(node: &SyntaxNode) -> Result<&SyntaxNode, LowerError> {
    node_children_exprs(node)
        .next()
        .ok_or(LowerError::UnexpectedNode(node.kind))
}

fn last_child_expr(node: &SyntaxNode) -> Result<&SyntaxNode, LowerError> {
    node_children_exprs(node)
        .last()
        .ok_or(LowerError::UnexpectedNode(node.kind))
}

fn lower_call_args_from_cst<'a>(
    nodes: impl Iterator<Item = &'a SyntaxNode>,
) -> Result<(Vec<Expr>, Vec<ExprKwArg>), LowerError> {
    let mut args = Vec::new();
    let mut kwargs: Vec<ExprKwArg> = Vec::new();
    let mut saw_kwarg = false;

    for node in nodes {
        if node.kind == SyntaxKind::ExprKwArg {
            let kwarg = lower_kwarg_from_cst(node)?;
            if kwargs.iter().any(|existing| existing.name == kwarg.name) {
                return Err(LowerError::Unsupported(format!(
                    "duplicate keyword argument '{}'",
                    kwarg.name
                )));
            }
            kwargs.push(kwarg);
            saw_kwarg = true;
            continue;
        }

        if saw_kwarg {
            return Err(LowerError::Unsupported(
                "positional argument after keyword argument".to_string(),
            ));
        }
        args.push(lower_expr_from_cst(node)?);
    }

    Ok((args, kwargs))
}

fn lower_kwarg_from_cst(node: &SyntaxNode) -> Result<ExprKwArg, LowerError> {
    let name = direct_first_non_trivia_token_text(node)
        .ok_or(LowerError::UnexpectedNode(SyntaxKind::ExprKwArg))?;
    let value_node = node_children_exprs(node)
        .next()
        .ok_or(LowerError::UnexpectedNode(SyntaxKind::ExprKwArg))?;
    let value = lower_expr_from_cst(value_node)?;
    Ok(ExprKwArg { name, value })
}

fn node_children_exprs<'a>(node: &'a SyntaxNode) -> impl Iterator<Item = &'a SyntaxNode> {
    node.children.iter().filter_map(|c| match c {
        crate::syntax::SyntaxElement::Node(n) if n.kind.category() == CstCategory::Expr => {
            Some(n.as_ref())
        }
        _ => None,
    })
}

fn range_child_exprs(
    node: &SyntaxNode,
) -> Result<(Option<&SyntaxNode>, Option<&SyntaxNode>), LowerError> {
    let mut exprs = node_children_exprs(node);
    let first = exprs.next();
    let second = exprs.next();
    if exprs.next().is_some() {
        return Err(LowerError::UnexpectedNode(node.kind));
    }

    if let Some(second) = second {
        return Ok((first, Some(second)));
    }

    if range_operator_is_leading(node) {
        Ok((None, first))
    } else {
        Ok((first, None))
    }
}

fn range_operator_is_leading(node: &SyntaxNode) -> bool {
    for child in &node.children {
        match child {
            crate::syntax::SyntaxElement::Token(token)
                if !token.is_trivia() && (token.text == ".." || token.text == "..=") =>
            {
                return true;
            }
            crate::syntax::SyntaxElement::Node(n) if n.kind.category() == CstCategory::Expr => {
                return false;
            }
            _ => {}
        }
    }
    false
}

fn direct_operator_token_text(node: &SyntaxNode) -> Option<String> {
    node.children.iter().find_map(|child| match child {
        crate::syntax::SyntaxElement::Token(t)
            if !t.is_trivia()
                && !t
                    .text
                    .chars()
                    .next()
                    .is_some_and(|c| c.is_ascii_alphanumeric() || c == '_') =>
        {
            Some(t.text.clone())
        }
        _ => None,
    })
}

fn direct_first_non_trivia_token_text(node: &SyntaxNode) -> Option<String> {
    node.children.iter().find_map(|child| match child {
        crate::syntax::SyntaxElement::Token(t) if !t.is_trivia() => Some(t.text.clone()),
        _ => None,
    })
}

fn macro_callee_path(node: &SyntaxNode) -> Result<Path, LowerError> {
    let mut segments = Vec::new();
    let mut saw_root = false;
    let mut saw_first_token = false;
    for child in &node.children {
        let crate::syntax::SyntaxElement::Token(tok) = child else {
            continue;
        };
        if tok.is_trivia() {
            continue;
        }
        if !saw_first_token && tok.text == "::" {
            saw_root = true;
            saw_first_token = true;
            continue;
        }
        if tok.text == "::" || tok.text == "." {
            saw_first_token = true;
            continue;
        }
        let text = tok.text.strip_prefix("r#").unwrap_or(&tok.text);
        if text
            .chars()
            .next()
            .is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
        {
            segments.push(Ident::new(text.to_string()));
        }
        saw_first_token = true;
    }
    if segments.is_empty() {
        return Err(LowerError::UnexpectedNode(SyntaxKind::ExprMacroCall));
    }
    let (prefix, segments) = split_path_prefix(segments, saw_root);
    if segments.is_empty() && matches!(prefix, PathPrefix::Plain | PathPrefix::Root) {
        return Err(LowerError::UnexpectedNode(SyntaxKind::ExprMacroCall));
    }
    Ok(Path::new(prefix, segments))
}

fn direct_last_ident_token_text(node: &SyntaxNode) -> Option<String> {
    node.children.iter().rev().find_map(|child| match child {
        crate::syntax::SyntaxElement::Token(t) if !t.is_trivia() => {
            let text = t.text.as_str();
            if text
                .chars()
                .next()
                .is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
            {
                return Some(t.text.clone());
            }
            if text.chars().all(|c| c.is_ascii_digit()) {
                return Some(t.text.clone());
            }
            None
        }
        _ => None,
    })
}

fn binop_from_text(op: &str) -> Option<BinOpKind> {
    Some(match op {
        "+" => BinOpKind::Add,
        "-" => BinOpKind::Sub,
        "*" => BinOpKind::Mul,
        "/" => BinOpKind::Div,
        "%" => BinOpKind::Mod,
        "==" => BinOpKind::Eq,
        "!=" => BinOpKind::Ne,
        "<" => BinOpKind::Lt,
        "<=" => BinOpKind::Le,
        ">" => BinOpKind::Gt,
        ">=" => BinOpKind::Ge,
        "&&" => BinOpKind::And,
        "||" => BinOpKind::Or,
        "&" => BinOpKind::BitAnd,
        "|" => BinOpKind::BitOr,
        "^" => BinOpKind::BitXor,
        "<<" => BinOpKind::Shl,
        ">>" => BinOpKind::Shr,
        _ => return None,
    })
}

fn compound_assign_binop(op: &str) -> Option<BinOpKind> {
    Some(match op {
        "+=" => BinOpKind::Add,
        "-=" => BinOpKind::Sub,
        "*=" => BinOpKind::Mul,
        "/=" => BinOpKind::Div,
        "%=" => BinOpKind::Mod,
        "<<=" => BinOpKind::Shl,
        ">>=" => BinOpKind::Shr,
        "&=" => BinOpKind::BitAnd,
        "|=" => BinOpKind::BitOr,
        "^=" => BinOpKind::BitXor,
        _ => return None,
    })
}
