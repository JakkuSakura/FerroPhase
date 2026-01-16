use crate::ast::items::LowerItemsError;
use crate::cst::parse_expr_lexemes_prefix_to_cst;
use crate::lexer::lexeme::LexemeKind;
use crate::lexer::tokenizer::lex_lexemes;
use crate::lexer::tokenizer::strip_number_suffix;
use crate::syntax::{SyntaxKind, SyntaxNode};
use fp_core::ast::{
    BlockStmt, BlockStmtExpr, Expr, ExprArray, ExprArrayRepeat, ExprAsync, ExprAwait, ExprBinOp,
    ExprBlock, ExprBreak, ExprClosure, ExprConstBlock, ExprContinue, ExprField, ExprFor, ExprIf,
    ExprIndex, ExprIntrinsicCall, ExprInvoke, ExprInvokeTarget, ExprKind, ExprLoop, ExprMatch,
    ExprMatchCase, ExprQuote, ExprRange, ExprRangeLimit, ExprReturn, ExprSelect, ExprSelectType,
    ExprSplice, ExprStringTemplate, ExprStruct, ExprStructural, ExprTry, ExprTuple, ExprWhile,
    FormatArgRef, FormatPlaceholder, FormatSpec, FormatTemplatePart, Ident, ImplTraits, Locator,
    MacroDelimiter, MacroInvocation, ParameterPath, ParameterPathSegment, Path, Pattern,
    PatternBind, PatternIdent, PatternKind, PatternQuote, PatternQuotePlural, PatternStruct,
    PatternStructField, PatternStructural, PatternTuple, PatternTupleStruct, PatternType,
    PatternVariant, PatternWildcard, QuoteFragmentKind, QuoteItemKind, StmtLet, StructuralField,
    Ty, TypeArray, TypeBinaryOp, TypeBinaryOpKind, TypeBounds, TypeFunction, TypeQuote,
    TypeReference, TypeSlice, TypeStructural, TypeTuple, TypeVec, Value, ValueNone, ValueString,
};
use fp_core::cst::CstCategory;
use fp_core::intrinsics::IntrinsicCallKind;
use fp_core::ops::{BinOpKind, UnOpKind};

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

fn quote_kind_from_cst(node: &SyntaxNode) -> Result<Option<QuoteFragmentKind>, LowerError> {
    let mut tokens = Vec::new();
    crate::syntax::collect_tokens(node, &mut tokens);
    let mut iter = tokens.iter().filter(|t| !t.is_trivia());

    while let Some(tok) = iter.next() {
        if tok.text != "quote" {
            continue;
        }
        let Some(next) = iter.next() else {
            return Ok(None);
        };
        if next.text != "<" {
            return Ok(None);
        }
        let Some(kind_tok) = iter.next() else {
            return Err(LowerError::UnexpectedNode(node.kind));
        };
        if kind_tok.text == "[" {
            let Some(inner_tok) = iter.next() else {
                return Err(LowerError::UnexpectedNode(node.kind));
            };
            let Some(close_bracket) = iter.next() else {
                return Err(LowerError::UnexpectedNode(node.kind));
            };
            if close_bracket.text != "]" {
                return Err(LowerError::UnexpectedNode(node.kind));
            }
            let Some(close_tok) = iter.next() else {
                return Err(LowerError::UnexpectedNode(node.kind));
            };
            if close_tok.text != ">" {
                return Err(LowerError::UnexpectedNode(node.kind));
            }
            let kind = match inner_tok.text.as_str() {
                "item" | "fn" | "struct" | "enum" | "trait" | "impl" | "const" | "static"
                | "mod" | "use" | "macro" => QuoteFragmentKind::Item,
                _ => return Err(LowerError::UnexpectedNode(node.kind)),
            };
            return Ok(Some(kind));
        }
        let Some(close_tok) = iter.next() else {
            return Err(LowerError::UnexpectedNode(node.kind));
        };
        if close_tok.text != ">" {
            return Err(LowerError::UnexpectedNode(node.kind));
        }
        let kind = match kind_tok.text.as_str() {
            "expr" => QuoteFragmentKind::Expr,
            "stmt" => QuoteFragmentKind::Stmt,
            "item" => QuoteFragmentKind::Item,
            "type" => QuoteFragmentKind::Type,
            "items" | "fns" | "structs" | "enums" | "traits" | "impls" | "consts" | "statics"
            | "mods" | "uses" | "macros" => {
                tracing::warn!("deprecated plural quote fragment kind: {}", kind_tok.text);
                QuoteFragmentKind::Item
            }
            "exprs" | "stmts" | "types" => {
                return Err(LowerError::UnexpectedNode(node.kind));
            }
            "fn" | "struct" | "enum" | "trait" | "impl" | "const" | "static" | "mod" | "use"
            | "macro" => QuoteFragmentKind::Item,
            _ => return Err(LowerError::UnexpectedNode(node.kind)),
        };
        return Ok(Some(kind));
    }

    Ok(None)
}

fn quote_pattern_from_cst(node: &SyntaxNode) -> Result<PatternQuote, LowerError> {
    let mut tokens = Vec::new();
    crate::syntax::collect_tokens(node, &mut tokens);
    let mut iter = tokens.iter().filter(|t| !t.is_trivia());

    while let Some(tok) = iter.next() {
        if tok.text != "quote" {
            continue;
        }
        let Some(next) = iter.next() else {
            return Ok(PatternQuote {
                fragment: QuoteFragmentKind::Item,
                item: None,
                fields: Vec::new(),
                has_rest: false,
            });
        };
        if next.text != "<" {
            return Ok(PatternQuote {
                fragment: QuoteFragmentKind::Item,
                item: None,
                fields: Vec::new(),
                has_rest: false,
            });
        }
        let Some(kind_tok) = iter.next() else {
            return Err(LowerError::UnexpectedNode(node.kind));
        };
        if kind_tok.text == "[" {
            let Some(inner_tok) = iter.next() else {
                return Err(LowerError::UnexpectedNode(node.kind));
            };
            let Some(close_bracket) = iter.next() else {
                return Err(LowerError::UnexpectedNode(node.kind));
            };
            if close_bracket.text != "]" {
                return Err(LowerError::UnexpectedNode(node.kind));
            }
            let Some(close_tok) = iter.next() else {
                return Err(LowerError::UnexpectedNode(node.kind));
            };
            if close_tok.text != ">" {
                return Err(LowerError::UnexpectedNode(node.kind));
            }
            let (fragment, item) = match inner_tok.text.as_str() {
                "item" => (QuoteFragmentKind::Item, None),
                "fn" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Function)),
                "struct" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Struct)),
                "enum" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Enum)),
                "trait" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Trait)),
                "impl" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Impl)),
                "const" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Const)),
                "static" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Static)),
                "mod" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Module)),
                "use" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Use)),
                "macro" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Macro)),
                _ => return Err(LowerError::UnexpectedNode(node.kind)),
            };
            return Ok(PatternQuote {
                fragment,
                item,
                fields: Vec::new(),
                has_rest: false,
            });
        }
        let Some(close_tok) = iter.next() else {
            return Err(LowerError::UnexpectedNode(node.kind));
        };
        if close_tok.text != ">" {
            return Err(LowerError::UnexpectedNode(node.kind));
        }
        let (fragment, item) = match kind_tok.text.as_str() {
            "expr" => (QuoteFragmentKind::Expr, None),
            "stmt" => (QuoteFragmentKind::Stmt, None),
            "item" => (QuoteFragmentKind::Item, None),
            "type" => (QuoteFragmentKind::Type, None),
            "fn" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Function)),
            "struct" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Struct)),
            "enum" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Enum)),
            "trait" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Trait)),
            "impl" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Impl)),
            "const" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Const)),
            "static" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Static)),
            "mod" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Module)),
            "use" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Use)),
            "macro" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Macro)),
            "items" | "fns" | "structs" | "enums" | "traits" | "impls" | "consts" | "statics"
            | "mods" | "uses" | "macros" => {
                tracing::warn!("deprecated plural quote fragment kind: {}", kind_tok.text);
                (QuoteFragmentKind::Item, None)
            }
            "exprs" | "stmts" | "types" => {
                return Err(LowerError::UnexpectedNode(node.kind));
            }
            _ => return Err(LowerError::UnexpectedNode(node.kind)),
        };
        return Ok(PatternQuote {
            fragment,
            item,
            fields: Vec::new(),
            has_rest: false,
        });
    }

    Err(LowerError::UnexpectedNode(node.kind))
}

fn quote_block_pattern_from_cst(node: &SyntaxNode) -> Result<PatternQuote, LowerError> {
    let mut tokens = Vec::new();
    crate::syntax::collect_tokens(node, &mut tokens);
    let tokens: Vec<&crate::syntax::SyntaxToken> =
        tokens.into_iter().filter(|t| !t.is_trivia()).collect();

    let mut idx = 0;
    while idx < tokens.len() && tokens[idx].text != "quote" {
        idx += 1;
    }
    if idx >= tokens.len() {
        return Err(LowerError::UnexpectedNode(node.kind));
    }
    idx += 1;

    if tokens.get(idx).is_some_and(|t| t.text == "<") {
        idx += 1;
        let (kind_tok, has_brackets) = if tokens.get(idx).is_some_and(|t| t.text == "[") {
            idx += 1;
            let kind = tokens
                .get(idx)
                .ok_or(LowerError::UnexpectedNode(node.kind))?;
            idx += 1;
            if !tokens.get(idx).is_some_and(|t| t.text == "]") {
                return Err(LowerError::UnexpectedNode(node.kind));
            }
            idx += 1;
            (kind, true)
        } else {
            let kind = tokens
                .get(idx)
                .ok_or(LowerError::UnexpectedNode(node.kind))?;
            idx += 1;
            (kind, false)
        };
        if !tokens.get(idx).is_some_and(|t| t.text == ">") {
            return Err(LowerError::UnexpectedNode(node.kind));
        }
        idx += 1;

        let kind_text = kind_tok.text.as_str();
        let _ = kind_text;
        let _ = has_brackets;
    }

    while idx < tokens.len() && tokens[idx].text != "{" {
        idx += 1;
    }
    if idx >= tokens.len() {
        return Err(LowerError::UnexpectedNode(node.kind));
    }
    idx += 1;

    let mut depth = 1;
    let mut fn_idx = None;
    let mut cursor = idx;
    while cursor < tokens.len() {
        let text = tokens[cursor].text.as_str();
        if text == "{" {
            depth += 1;
        } else if text == "}" {
            depth -= 1;
            if depth == 0 {
                break;
            }
        } else if depth == 1 && text == "fn" {
            fn_idx = Some(cursor);
            break;
        }
        cursor += 1;
    }
    let fn_idx = fn_idx.ok_or(LowerError::UnexpectedNode(node.kind))?;

    let mut fields = Vec::new();
    let mut name_idx = fn_idx + 1;
    if tokens.get(name_idx).is_some_and(|t| t.text == "splice") {
        if !tokens.get(name_idx + 1).is_some_and(|t| t.text == "(") {
            return Err(LowerError::UnexpectedNode(node.kind));
        }
        let bind_tok = tokens
            .get(name_idx + 2)
            .ok_or(LowerError::UnexpectedNode(node.kind))?;
        if !tokens.get(name_idx + 3).is_some_and(|t| t.text == ")") {
            return Err(LowerError::UnexpectedNode(node.kind));
        }
        let binder = Ident::new(bind_tok.text.clone());
        fields.push(PatternStructField {
            name: Ident::new("name"),
            rename: Some(Box::new(Pattern::new(PatternKind::Ident(
                PatternIdent::new(binder),
            )))),
        });
        name_idx += 4;
    } else {
        name_idx += 1;
    }

    if !tokens.get(name_idx).is_some_and(|t| t.text == "(") {
        return Err(LowerError::UnexpectedNode(node.kind));
    }
    let mut paren_depth = 0;
    while name_idx < tokens.len() {
        let text = tokens[name_idx].text.as_str();
        if text == "(" {
            paren_depth += 1;
        } else if text == ")" {
            paren_depth -= 1;
            if paren_depth == 0 {
                break;
            }
        }
        name_idx += 1;
    }
    if paren_depth != 0 {
        return Err(LowerError::UnexpectedNode(node.kind));
    }

    Ok(PatternQuote {
        fragment: QuoteFragmentKind::Item,
        item: Some(QuoteItemKind::Function),
        fields,
        has_rest: false,
    })
}

fn quote_pattern_kind_from_cst(
    node: &SyntaxNode,
) -> Result<(QuoteFragmentKind, Option<QuoteItemKind>, bool), LowerError> {
    let mut tokens = Vec::new();
    crate::syntax::collect_tokens(node, &mut tokens);
    let mut iter = tokens.iter().filter(|t| !t.is_trivia());

    while let Some(tok) = iter.next() {
        if tok.text != "quote" {
            continue;
        }
        let Some(next) = iter.next() else {
            return Ok((QuoteFragmentKind::Item, None, false));
        };
        if next.text != "<" {
            return Ok((QuoteFragmentKind::Item, None, false));
        }
        let Some(kind_tok) = iter.next() else {
            return Err(LowerError::UnexpectedNode(node.kind));
        };
        if kind_tok.text == "[" {
            let Some(inner_tok) = iter.next() else {
                return Err(LowerError::UnexpectedNode(node.kind));
            };
            let Some(close_bracket) = iter.next() else {
                return Err(LowerError::UnexpectedNode(node.kind));
            };
            if close_bracket.text != "]" {
                return Err(LowerError::UnexpectedNode(node.kind));
            }
            let Some(close_tok) = iter.next() else {
                return Err(LowerError::UnexpectedNode(node.kind));
            };
            if close_tok.text != ">" {
                return Err(LowerError::UnexpectedNode(node.kind));
            }
            let (fragment, item) = match inner_tok.text.as_str() {
                "item" => (QuoteFragmentKind::Item, None),
                "fn" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Function)),
                "struct" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Struct)),
                "enum" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Enum)),
                "trait" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Trait)),
                "impl" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Impl)),
                "const" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Const)),
                "static" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Static)),
                "mod" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Module)),
                "use" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Use)),
                "macro" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Macro)),
                _ => return Err(LowerError::UnexpectedNode(node.kind)),
            };
            return Ok((fragment, item, true));
        }
        let Some(close_tok) = iter.next() else {
            return Err(LowerError::UnexpectedNode(node.kind));
        };
        if close_tok.text != ">" {
            return Err(LowerError::UnexpectedNode(node.kind));
        }
        let (fragment, item, plural) = match kind_tok.text.as_str() {
            "expr" => (QuoteFragmentKind::Expr, None, false),
            "stmt" => (QuoteFragmentKind::Stmt, None, false),
            "item" => (QuoteFragmentKind::Item, None, false),
            "type" => (QuoteFragmentKind::Type, None, false),
            "fn" => (
                QuoteFragmentKind::Item,
                Some(QuoteItemKind::Function),
                false,
            ),
            "struct" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Struct), false),
            "enum" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Enum), false),
            "trait" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Trait), false),
            "impl" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Impl), false),
            "const" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Const), false),
            "static" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Static), false),
            "mod" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Module), false),
            "use" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Use), false),
            "macro" => (QuoteFragmentKind::Item, Some(QuoteItemKind::Macro), false),
            "items" => {
                tracing::warn!("deprecated plural quote fragment kind: {}", kind_tok.text);
                (QuoteFragmentKind::Item, None, true)
            }
            "fns" => {
                tracing::warn!("deprecated plural quote fragment kind: {}", kind_tok.text);
                (QuoteFragmentKind::Item, Some(QuoteItemKind::Function), true)
            }
            "structs" => {
                tracing::warn!("deprecated plural quote fragment kind: {}", kind_tok.text);
                (QuoteFragmentKind::Item, Some(QuoteItemKind::Struct), true)
            }
            "enums" => {
                tracing::warn!("deprecated plural quote fragment kind: {}", kind_tok.text);
                (QuoteFragmentKind::Item, Some(QuoteItemKind::Enum), true)
            }
            "traits" => {
                tracing::warn!("deprecated plural quote fragment kind: {}", kind_tok.text);
                (QuoteFragmentKind::Item, Some(QuoteItemKind::Trait), true)
            }
            "impls" => {
                tracing::warn!("deprecated plural quote fragment kind: {}", kind_tok.text);
                (QuoteFragmentKind::Item, Some(QuoteItemKind::Impl), true)
            }
            "consts" => {
                tracing::warn!("deprecated plural quote fragment kind: {}", kind_tok.text);
                (QuoteFragmentKind::Item, Some(QuoteItemKind::Const), true)
            }
            "statics" => {
                tracing::warn!("deprecated plural quote fragment kind: {}", kind_tok.text);
                (QuoteFragmentKind::Item, Some(QuoteItemKind::Static), true)
            }
            "mods" => {
                tracing::warn!("deprecated plural quote fragment kind: {}", kind_tok.text);
                (QuoteFragmentKind::Item, Some(QuoteItemKind::Module), true)
            }
            "uses" => {
                tracing::warn!("deprecated plural quote fragment kind: {}", kind_tok.text);
                (QuoteFragmentKind::Item, Some(QuoteItemKind::Use), true)
            }
            "macros" => {
                tracing::warn!("deprecated plural quote fragment kind: {}", kind_tok.text);
                (QuoteFragmentKind::Item, Some(QuoteItemKind::Macro), true)
            }
            "exprs" | "stmts" | "types" => {
                return Err(LowerError::UnexpectedNode(node.kind));
            }
            _ => return Err(LowerError::UnexpectedNode(node.kind)),
        };
        return Ok((fragment, item, plural));
    }

    Err(LowerError::UnexpectedNode(node.kind))
}

pub fn lower_expr_from_cst(node: &SyntaxNode) -> Result<Expr, LowerError> {
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
            Ok(ExprKind::Tuple(ExprTuple { values }).into())
        }
        SyntaxKind::ExprArray => {
            let values = node_children_exprs(node)
                .map(lower_expr_from_cst)
                .collect::<Result<Vec<_>, _>>()?;
            Ok(ExprKind::Array(ExprArray { values }).into())
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
            Ok(ExprKind::Structural(ExprStructural { fields }).into())
        }
        SyntaxKind::ExprBlock => {
            let block = lower_block_from_cst(node)?;
            Ok(ExprKind::Block(block).into())
        }
        SyntaxKind::ExprQuote => {
            let block_node = node_children_exprs(node)
                .find(|n| n.kind == SyntaxKind::ExprBlock)
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::ExprQuote))?;
            let block = lower_block_from_cst(block_node)?;
            let kind = quote_kind_from_cst(node)?;
            Ok(ExprKind::Quote(ExprQuote { block, kind }).into())
        }
        SyntaxKind::ExprQuoteToken => Err(LowerError::UnexpectedNode(SyntaxKind::ExprQuoteToken)),
        SyntaxKind::ExprSplice => {
            let token_expr = last_child_expr(node)?;
            let expr = lower_expr_from_cst(token_expr)?;
            Ok(ExprKind::Splice(ExprSplice {
                token: Box::new(expr),
            })
            .into())
        }
        SyntaxKind::ExprAsync => {
            let block_node = node_children_exprs(node)
                .find(|n| n.kind == SyntaxKind::ExprBlock)
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::ExprAsync))?;
            let block = lower_block_from_cst(block_node)?;
            Ok(ExprKind::Async(ExprAsync {
                expr: Box::new(ExprKind::Block(block).into()),
            })
            .into())
        }
        SyntaxKind::ExprConstBlock => {
            let block_node = node_children_exprs(node)
                .find(|n| n.kind == SyntaxKind::ExprBlock)
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::ExprConstBlock))?;
            let block = lower_block_from_cst(block_node)?;
            Ok(ExprKind::ConstBlock(ExprConstBlock {
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
            let then_block = lower_block_from_cst(then_block)?;
            let then_expr: Expr = ExprKind::Block(then_block).into();
            let else_expr = if let Some(else_node) = else_block {
                if else_node.kind == SyntaxKind::ExprBlock {
                    let b = lower_block_from_cst(else_node)?;
                    Some(Box::new(ExprKind::Block(b).into()))
                } else {
                    Some(Box::new(lower_expr_from_cst(else_node)?))
                }
            } else {
                None
            };

            Ok(ExprKind::If(ExprIf {
                cond: Box::new(cond),
                then: Box::new(then_expr),
                elze: else_expr,
            })
            .into())
        }
        SyntaxKind::ExprLoop => {
            let block_node = node_children_exprs(node)
                .find(|n| n.kind == SyntaxKind::ExprBlock)
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::ExprLoop))?;
            let block = lower_block_from_cst(block_node)?;
            Ok(ExprKind::Loop(ExprLoop {
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
            let body_block = lower_block_from_cst(body)?;
            Ok(ExprKind::While(ExprWhile {
                cond: Box::new(cond),
                body: Box::new(ExprKind::Block(body_block).into()),
            })
            .into())
        }

        SyntaxKind::ExprFor => {
            let pat_node = node
                .children
                .iter()
                .find_map(|c| match c {
                    crate::syntax::SyntaxElement::Node(n)
                        if matches!(
                            n.kind,
                            SyntaxKind::PatternIdent
                                | SyntaxKind::PatternWildcard
                                | SyntaxKind::PatternTuple
                        ) =>
                    {
                        Some(n.as_ref())
                    }
                    _ => None,
                })
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::ExprFor))?;
            let pat = lower_pattern_from_cst(pat_node)?;

            let mut expr_nodes = node_children_exprs(node);
            let iter = expr_nodes
                .next()
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::ExprFor))?;
            let body = expr_nodes
                .next()
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::ExprFor))?;
            let iter = lower_expr_from_cst(iter)?;
            let body_block = lower_block_from_cst(body)?;
            Ok(ExprKind::For(ExprFor {
                pat: Box::new(pat),
                iter: Box::new(iter),
                body: Box::new(ExprKind::Block(body_block).into()),
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
                    pat: Some(Box::new(pat)),
                    cond: Box::new(Expr::value(Value::bool(true))),
                    guard: guard_expr.map(Box::new),
                    body: Box::new(body_expr),
                });
            }

            Ok(ExprKind::Match(ExprMatch {
                scrutinee: Some(Box::new(scrutinee_expr)),
                cases,
            })
            .into())
        }
        SyntaxKind::ExprClosure => {
            let (params, body) = lower_closure_from_cst(node)?;
            Ok(ExprKind::Closure(ExprClosure {
                params,
                ret_ty: None,
                movability: None,
                body: Box::new(body),
            })
            .into())
        }
        SyntaxKind::ExprReturn => {
            let value = node_children_exprs(node)
                .next()
                .map(lower_expr_from_cst)
                .transpose()?
                .map(Box::new);
            Ok(ExprKind::Return(ExprReturn { value }).into())
        }
        SyntaxKind::ExprBreak => {
            let value = node_children_exprs(node)
                .next()
                .map(lower_expr_from_cst)
                .transpose()?
                .map(Box::new);
            Ok(ExprKind::Break(ExprBreak { value }).into())
        }
        SyntaxKind::ExprContinue => Ok(ExprKind::Continue(ExprContinue {}).into()),
        SyntaxKind::ExprName => {
            let name = direct_first_non_trivia_token_text(node)
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::ExprName))?;
            match name.as_str() {
                "true" => Ok(Expr::value(Value::bool(true))),
                "false" => Ok(Expr::value(Value::bool(false))),
                "null" => Ok(Expr::value(Value::null())),
                _ => Ok(ExprKind::Locator(Locator::from_ident(Ident::new(name))).into()),
            }
        }
        SyntaxKind::ExprPath => {
            let mut segments = Vec::new();
            for child in &node.children {
                let crate::syntax::SyntaxElement::Token(tok) = child else {
                    continue;
                };
                if tok.is_trivia() {
                    continue;
                }
                if tok.text == "::" {
                    continue;
                }
                if tok
                    .text
                    .chars()
                    .next()
                    .is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
                {
                    segments.push(Ident::new(tok.text.clone()));
                }
            }
            if segments.is_empty() {
                return Err(LowerError::UnexpectedNode(SyntaxKind::ExprPath));
            }
            Ok(ExprKind::Locator(Locator::path(Path::new(segments))).into())
        }
        SyntaxKind::ExprNumber => {
            let raw = direct_first_non_trivia_token_text(node)
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::ExprNumber))?;
            let stripped = strip_number_suffix(&raw);
            let normalized = stripped.replace('_', "");
            if normalized.contains('.') {
                let d = normalized
                    .parse::<f64>()
                    .map_err(|_| LowerError::InvalidNumber(raw.clone()))?;
                Ok(Expr::value(Value::decimal(d)))
            } else {
                let i = normalized
                    .parse::<i64>()
                    .map_err(|_| LowerError::InvalidNumber(raw.clone()))?;
                Ok(Expr::value(Value::int(i)))
            }
        }
        SyntaxKind::ExprString => {
            let raw = direct_first_non_trivia_token_text(node)
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::ExprString))?;
            if raw.starts_with("f\"") {
                parse_f_string_literal(&raw)
            } else if raw.starts_with("t\"") {
                let decoded = decode_string_literal(&raw).unwrap_or(raw);
                Ok(Expr::value(Value::String(ValueString::new_ref(decoded))))
            } else {
                let decoded = decode_string_literal(&raw).unwrap_or(raw);
                // String literals should lower to borrowed `&'static str` equivalents by default.
                Ok(Expr::value(Value::String(ValueString::new_ref(decoded))))
            }
        }
        SyntaxKind::ExprUnary => {
            let op = direct_operator_token_text(node).ok_or(LowerError::MissingOperator)?;
            let expr = last_child_expr(node)?;
            let inner = lower_expr_from_cst(expr)?;
            match op.as_str() {
                "-" => Ok(ExprKind::UnOp(fp_core::ast::ExprUnOp {
                    op: UnOpKind::Neg,
                    val: Box::new(inner),
                })
                .into()),
                "!" => Ok(ExprKind::UnOp(fp_core::ast::ExprUnOp {
                    op: UnOpKind::Not,
                    val: Box::new(inner),
                })
                .into()),
                "+" => {
                    // Unary plus is a no-op.
                    Ok(inner)
                }
                "*" => Ok(ExprKind::Dereference(fp_core::ast::ExprDereference {
                    referee: Box::new(inner),
                })
                .into()),
                "&" => {
                    let is_mut = node.children.iter().any(|c| {
                        matches!(c, crate::syntax::SyntaxElement::Token(t) if !t.is_trivia() && t.text == "mut")
                    });
                    Ok(ExprKind::Reference(fp_core::ast::ExprReference {
                        referee: Box::new(inner),
                        mutable: if is_mut { Some(true) } else { None },
                    })
                    .into())
                }
                _ => Err(LowerError::MissingOperator),
            }
        }
        SyntaxKind::ExprTry => {
            let base = first_child_expr(node)?;
            let expr = lower_expr_from_cst(base)?;
            Ok(ExprKind::Try(ExprTry {
                expr: Box::new(expr),
            })
            .into())
        }
        SyntaxKind::ExprAwait => {
            let base = first_child_expr(node)?;
            let expr = lower_expr_from_cst(base)?;
            Ok(ExprKind::Await(ExprAwait {
                base: Box::new(expr),
            })
            .into())
        }
        SyntaxKind::ExprBinary => {
            let lhs = first_child_expr(node)?;
            let rhs = last_child_expr(node)?;
            let op = direct_operator_token_text(node).ok_or(LowerError::MissingOperator)?;
            let lhs = lower_expr_from_cst(lhs)?;
            let rhs = lower_expr_from_cst(rhs)?;

            if op == "=" {
                return Ok(ExprKind::Assign(fp_core::ast::ExprAssign {
                    target: Box::new(lhs),
                    value: Box::new(rhs),
                })
                .into());
            }

            if let Some(binop) = compound_assign_binop(&op) {
                let combined = ExprKind::BinOp(ExprBinOp {
                    kind: binop,
                    lhs: Box::new(lhs.clone()),
                    rhs: Box::new(rhs),
                })
                .into();
                return Ok(ExprKind::Assign(fp_core::ast::ExprAssign {
                    target: Box::new(lhs),
                    value: Box::new(combined),
                })
                .into());
            }

            let kind = binop_from_text(&op).ok_or(LowerError::MissingOperator)?;
            Ok(ExprKind::BinOp(ExprBinOp {
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
                obj: Box::new(obj),
                index: Box::new(index),
            })
            .into())
        }
        SyntaxKind::ExprCall => {
            let mut nodes = node_children_exprs(node);
            let callee = nodes
                .next()
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::ExprCall))?;
            let callee = lower_expr_from_cst(callee)?;
            let mut args = Vec::new();
            for arg in nodes {
                args.push(lower_expr_from_cst(arg)?);
            }

            let target = match callee.kind() {
                ExprKind::Locator(locator) => ExprInvokeTarget::Function(locator.clone()),
                ExprKind::Select(select) => ExprInvokeTarget::Method(select.clone()),
                _ => ExprInvokeTarget::expr(callee),
            };

            Ok(ExprKind::Invoke(ExprInvoke { target, args }).into())
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

            let (delimiter, tokens) = macro_group_delimiter_and_tokens(node)
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::ExprMacroCall))?;

            Ok(ExprKind::Macro(fp_core::ast::ExprMacro::new(
                MacroInvocation::new(path, delimiter, tokens).with_span(node.span),
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
                expr: Box::new(expr),
                ty,
            })
            .into())
        }
        other => Err(LowerError::UnexpectedNode(other)),
    }?;

    if !matches!(node.kind, SyntaxKind::Root | SyntaxKind::ExprParen) {
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
            name: name_ident,
            value,
        });
    }

    let mut update: Option<Expr> = None;
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
        if update.is_none() {
            return Err(LowerError::UnexpectedNode(SyntaxKind::ExprStruct));
        }
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
                        ) =>
                    {
                        Some(n.as_ref())
                    }
                    _ => None,
                })
                .map(lower_pattern_from_cst)
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Pattern::new(PatternKind::Tuple(PatternTuple { patterns })))
        }
        SyntaxKind::PatternType => {
            let pat_node = node
                .children
                .iter()
                .find_map(|c| match c {
                    crate::syntax::SyntaxElement::Node(n)
                        if n.kind.category() == CstCategory::Pattern =>
                    {
                        Some(n.as_ref())
                    }
                    _ => None,
                })
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::PatternType))?;
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
                .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::PatternType))?;
            let pat = lower_pattern_from_cst(pat_node)?;
            let ty = lower_type_from_cst(ty_node)?;
            Ok(Pattern::new(PatternKind::Type(PatternType::new(pat, ty))))
        }
        other => Err(LowerError::UnexpectedNode(other)),
    }
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
                let item = crate::ast::items::lower_item_from_cst(item_node)?;
                stmts.push(BlockStmt::item(item));
            }
            SyntaxKind::BlockStmtLet => stmts.push(lower_let_stmt(stmt)?),
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
    Ok(ExprBlock::new_stmts(stmts))
}

fn lower_let_stmt(node: &SyntaxNode) -> Result<BlockStmt, LowerError> {
    let mut saw_mut = false;
    let mut name: Option<String> = None;
    let mut ty: Option<Ty> = None;
    let mut init_expr: Option<Expr> = None;

    for child in &node.children {
        match child {
            crate::syntax::SyntaxElement::Token(t) if !t.is_trivia() && t.text == "mut" => {
                saw_mut = true;
            }
            crate::syntax::SyntaxElement::Token(t)
                if !t.is_trivia()
                    && name.is_none()
                    && t.text != "let"
                    && t.text != "mut"
                    && t.text
                        .chars()
                        .next()
                        .is_some_and(|c| c.is_ascii_alphabetic() || c == '_') =>
            {
                name = Some(t.text.clone());
            }
            crate::syntax::SyntaxElement::Node(n) if n.kind.category() == CstCategory::Type => {
                ty = Some(lower_type_from_cst(n)?);
            }
            crate::syntax::SyntaxElement::Node(n) if n.kind.category() == CstCategory::Expr => {
                init_expr = Some(lower_expr_from_cst(n)?);
            }
            crate::syntax::SyntaxElement::Node(n) => {
                if matches!(
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
                ) {
                    init_expr = Some(lower_expr_from_cst(n)?);
                }
            }
            _ => {}
        }
    }

    let Some(name) = name else {
        return Err(LowerError::UnexpectedNode(SyntaxKind::BlockStmtLet));
    };
    let pat = if name == "_" {
        Pattern::new(PatternKind::Wildcard(PatternWildcard {}))
    } else {
        let ident = Ident::new(name);
        let mut pat = Pattern::new(PatternKind::Ident(PatternIdent::new(ident)));
        if saw_mut {
            pat.make_mut();
        }
        pat
    };

    let stmt = match (ty, init_expr) {
        (Some(ty), Some(init)) => {
            if let PatternKind::Ident(id) = pat.kind().clone() {
                StmtLet::new_typed(id.ident, ty, init)
            } else {
                StmtLet::new(pat, Some(init), None)
            }
        }
        (Some(_ty), None) => StmtLet::new(pat, None, None),
        (None, init) => StmtLet::new(pat, init, None),
    };
    Ok(BlockStmt::Let(stmt))
}

fn split_match_arm<'a>(
    arm: &'a SyntaxNode,
) -> Result<(&'a SyntaxNode, Option<&'a SyntaxNode>, &'a SyntaxNode), LowerError> {
    let mut exprs = node_children_exprs(arm);
    let pat = exprs
        .next()
        .ok_or(LowerError::UnexpectedNode(SyntaxKind::MatchArm))?;
    let second = exprs.next();
    let third = exprs.next();
    match (second, third) {
        (Some(guard), Some(body)) => Ok((pat, Some(guard), body)),
        (Some(body), None) => Ok((pat, None, body)),
        _ => Err(LowerError::UnexpectedNode(SyntaxKind::MatchArm)),
    }
}

fn lower_match_pattern_from_cst(node: &SyntaxNode) -> Result<Pattern, LowerError> {
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
        SyntaxKind::ExprPath => {
            let expr = lower_expr_from_cst(node)?;
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
                ExprKind::Locator(locator) => locator.clone(),
                _ => return Err(LowerError::UnexpectedNode(SyntaxKind::ExprCall)),
            };
            let patterns = exprs
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

            if let ExprKind::Locator(locator) = name_expr.kind() {
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

            if let ExprKind::Locator(locator) = name_expr.kind() {
                let ident = match locator {
                    Locator::Ident(ident) => Some(ident.clone()),
                    Locator::Path(path) if path.segments.len() == 1 => {
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

fn decode_string_literal(raw: &str) -> Option<String> {
    fn unescape_cooked(s: &str) -> Option<String> {
        let mut out = String::with_capacity(s.len());
        let mut chars = s.chars();
        while let Some(c) = chars.next() {
            if c != '\\' {
                out.push(c);
                continue;
            }
            let esc = chars.next()?;
            match esc {
                'n' => out.push('\n'),
                'r' => out.push('\r'),
                't' => out.push('\t'),
                '0' => out.push('\0'),
                '\\' => out.push('\\'),
                '"' => out.push('"'),
                other => {
                    // Conservative fallback: keep the escape as-is.
                    out.push('\\');
                    out.push(other);
                }
            }
        }
        Some(out)
    }

    // Cooked string literal: "..."
    if raw.starts_with('"') && raw.ends_with('"') && raw.len() >= 2 {
        let inner = &raw[1..raw.len() - 1];
        return unescape_cooked(inner);
    }

    // Raw string literals: r"...", r#"..."#, br"...", br#"..."#
    let (prefix, rest) = if let Some(r) = raw.strip_prefix("br") {
        ("br", r)
    } else if let Some(r) = raw.strip_prefix('r') {
        ("r", r)
    } else {
        return None;
    };
    let hash_count = rest.chars().take_while(|c| *c == '#').count();
    let after_hashes = &rest[hash_count..];
    let Some(after_quote) = after_hashes.strip_prefix('"') else {
        return None;
    };

    let closing = format!("\"{}", "#".repeat(hash_count));
    let Some(end_idx) = after_quote.rfind(&closing) else {
        return None;
    };
    if end_idx + closing.len() != after_quote.len() {
        return None;
    }
    let inner = &after_quote[..end_idx];

    // `br"..."` is a byte string in Rust; FerroPhase currently models strings as UTF-8 `&str`.
    // Keep the contents as-is.
    let _ = prefix;
    Some(inner.to_string())
}

fn parse_f_string_literal(raw: &str) -> Result<Expr, LowerError> {
    let Some(decoded) = strip_string_prefix(raw, "f") else {
        return Err(LowerError::Unsupported(
            "invalid f-string literal".to_string(),
        ));
    };
    let (template, args) = parse_f_string_template(&decoded)?;
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

fn parse_f_string_template(input: &str) -> Result<(ExprStringTemplate, Vec<Expr>), LowerError> {
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
            let expr = parse_f_string_expr(expr_src)?;
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

fn parse_f_string_expr(src: &str) -> Result<Expr, LowerError> {
    let lexemes = lex_lexemes(src).map_err(|err| {
        LowerError::Unsupported(format!("failed to tokenize f-string expression: {err}"))
    })?;
    let (cst, consumed) = parse_expr_lexemes_prefix_to_cst(&lexemes, 0).map_err(|err| {
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

pub(crate) fn lower_type_from_cst(node: &SyntaxNode) -> Result<fp_core::ast::Ty, LowerError> {
    if node.kind.category() != CstCategory::Type {
        return Err(LowerError::UnexpectedNode(node.kind));
    }

    match node.kind {
        SyntaxKind::TyUnit => Ok(Ty::unit()),
        SyntaxKind::TyUnknown => Ok(Ty::unknown()),
        SyntaxKind::TyPath => lower_ty_path(node),
        SyntaxKind::TyRef => lower_ty_ref(node),
        SyntaxKind::TySlice => lower_ty_slice(node),
        SyntaxKind::TyArray => lower_ty_array(node),
        SyntaxKind::TyFn => lower_ty_fn(node),
        SyntaxKind::TyTuple => lower_ty_tuple(node),
        SyntaxKind::TyStructural => lower_ty_structural(node),
        SyntaxKind::TyBinary => lower_ty_binary(node),
        SyntaxKind::TyOptional => lower_ty_optional(node),
        SyntaxKind::TyValue => lower_ty_value(node),
        SyntaxKind::TyExpr => lower_ty_expr(node),
        SyntaxKind::TyImplTraits => lower_ty_impl_traits(node),
        SyntaxKind::TyMacroCall => lower_ty_macro_call(node),
        other => Err(LowerError::UnexpectedNode(other)),
    }
}

fn lower_ty_value(node: &SyntaxNode) -> Result<Ty, LowerError> {
    let raw = direct_first_non_trivia_token_text(node)
        .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::TyValue))?;
    let value = match raw.as_str() {
        "true" => Value::bool(true),
        "false" => Value::bool(false),
        "null" => Value::null(),
        _ => {
            if raw.starts_with('"') || raw.starts_with('\'') {
                let decoded = decode_string_literal(&raw).unwrap_or(raw);
                Value::String(ValueString::new_ref(decoded))
            } else {
                let stripped = strip_number_suffix(&raw);
                let normalized = stripped.replace('_', "");
                if normalized.contains('.') {
                    let d = normalized
                        .parse::<f64>()
                        .map_err(|_| LowerError::InvalidNumber(raw.clone()))?;
                    Value::decimal(d)
                } else {
                    let i = normalized
                        .parse::<i64>()
                        .map_err(|_| LowerError::InvalidNumber(raw.clone()))?;
                    Value::int(i)
                }
            }
        }
    };
    Ok(Ty::value(value))
}

fn lower_ty_expr(node: &SyntaxNode) -> Result<Ty, LowerError> {
    let expr_node = node
        .children
        .iter()
        .find_map(|child| match child {
            crate::syntax::SyntaxElement::Node(n) if n.kind.category() == CstCategory::Expr => {
                Some(n.as_ref())
            }
            _ => None,
        })
        .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::TyExpr))?;
    let expr = lower_expr_from_cst(expr_node)?;
    Ok(Ty::expr(expr))
}

fn lower_ty_macro_call(node: &SyntaxNode) -> Result<Ty, LowerError> {
    let mut segments: Vec<Ident> = Vec::new();
    for child in &node.children {
        let crate::syntax::SyntaxElement::Token(tok) = child else {
            continue;
        };
        if tok.is_trivia() {
            continue;
        }
        if tok.text == "!" {
            break;
        }
        match tok.text.as_str() {
            "::" | "<" | ">" | "," | "=" => {}
            _ => {
                if tok
                    .text
                    .chars()
                    .next()
                    .is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
                {
                    segments.push(Ident::new(tok.text.clone()));
                }
            }
        }
    }
    if segments.is_empty() {
        return Err(LowerError::UnexpectedNode(SyntaxKind::TyMacroCall));
    }
    let (delimiter, tokens) = macro_group_delimiter_and_tokens(node)
        .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::TyMacroCall))?;

    // `t! { ... }` is used as a type-level quoting wrapper in existing examples; lower it by
    // parsing the inner token stream as a type expression.
    if segments.len() == 1 && segments[0].as_str() == "t" {
        let lexemes = crate::lexer::tokenizer::lex_lexemes(&tokens)
            .map_err(|_| LowerError::UnexpectedNode(SyntaxKind::TyMacroCall))?;
        let (ty_cst, consumed) = crate::cst::parse_type_lexemes_prefix_to_cst(&lexemes, 0, &[])
            .map_err(|_| LowerError::UnexpectedNode(SyntaxKind::TyMacroCall))?;
        if lexemes[consumed..]
            .iter()
            .any(|l| l.kind == crate::lexer::LexemeKind::Token)
        {
            return Err(LowerError::UnexpectedNode(SyntaxKind::TyMacroCall));
        }
        return lower_type_from_cst(&ty_cst);
    }

    let path = Path::new(segments);
    let expr: Expr = ExprKind::Macro(fp_core::ast::ExprMacro::new(
        MacroInvocation::new(path, delimiter, tokens).with_span(node.span),
    ))
    .into();
    Ok(Ty::expr(expr))
}

fn node_children_types<'a>(node: &'a SyntaxNode) -> impl Iterator<Item = &'a SyntaxNode> {
    node.children.iter().filter_map(|c| match c {
        crate::syntax::SyntaxElement::Node(n) if n.kind.category() == CstCategory::Type => {
            Some(n.as_ref())
        }
        _ => None,
    })
}

fn quote_type_from_ident(name: &str, args: &[Ty]) -> Option<Ty> {
    match name {
        "expr" => {
            if args.len() > 1 {
                return None;
            }
            let inner = args.get(0).cloned().map(Box::new);
            Some(Ty::Quote(TypeQuote {
                kind: QuoteFragmentKind::Expr,
                item: None,
                inner,
            }))
        }
        "stmt" => {
            if !args.is_empty() {
                return None;
            }
            Some(Ty::Quote(TypeQuote {
                kind: QuoteFragmentKind::Stmt,
                item: None,
                inner: None,
            }))
        }
        "item" => {
            if !args.is_empty() {
                return None;
            }
            Some(Ty::Quote(TypeQuote {
                kind: QuoteFragmentKind::Item,
                item: None,
                inner: None,
            }))
        }
        "type" => {
            if !args.is_empty() {
                return None;
            }
            Some(Ty::Quote(TypeQuote {
                kind: QuoteFragmentKind::Type,
                item: None,
                inner: None,
            }))
        }
        "fn" => {
            if !args.is_empty() {
                return None;
            }
            Some(Ty::Quote(TypeQuote {
                kind: QuoteFragmentKind::Item,
                item: Some(QuoteItemKind::Function),
                inner: None,
            }))
        }
        "struct" => {
            if !args.is_empty() {
                return None;
            }
            Some(Ty::Quote(TypeQuote {
                kind: QuoteFragmentKind::Item,
                item: Some(QuoteItemKind::Struct),
                inner: None,
            }))
        }
        "enum" => {
            if !args.is_empty() {
                return None;
            }
            Some(Ty::Quote(TypeQuote {
                kind: QuoteFragmentKind::Item,
                item: Some(QuoteItemKind::Enum),
                inner: None,
            }))
        }
        "trait" => {
            if !args.is_empty() {
                return None;
            }
            Some(Ty::Quote(TypeQuote {
                kind: QuoteFragmentKind::Item,
                item: Some(QuoteItemKind::Trait),
                inner: None,
            }))
        }
        "impl" => {
            if !args.is_empty() {
                return None;
            }
            Some(Ty::Quote(TypeQuote {
                kind: QuoteFragmentKind::Item,
                item: Some(QuoteItemKind::Impl),
                inner: None,
            }))
        }
        "const" => {
            if !args.is_empty() {
                return None;
            }
            Some(Ty::Quote(TypeQuote {
                kind: QuoteFragmentKind::Item,
                item: Some(QuoteItemKind::Const),
                inner: None,
            }))
        }
        "static" => {
            if !args.is_empty() {
                return None;
            }
            Some(Ty::Quote(TypeQuote {
                kind: QuoteFragmentKind::Item,
                item: Some(QuoteItemKind::Static),
                inner: None,
            }))
        }
        "mod" => {
            if !args.is_empty() {
                return None;
            }
            Some(Ty::Quote(TypeQuote {
                kind: QuoteFragmentKind::Item,
                item: Some(QuoteItemKind::Module),
                inner: None,
            }))
        }
        "use" => {
            if !args.is_empty() {
                return None;
            }
            Some(Ty::Quote(TypeQuote {
                kind: QuoteFragmentKind::Item,
                item: Some(QuoteItemKind::Use),
                inner: None,
            }))
        }
        "macro" => {
            if !args.is_empty() {
                return None;
            }
            Some(Ty::Quote(TypeQuote {
                kind: QuoteFragmentKind::Item,
                item: Some(QuoteItemKind::Macro),
                inner: None,
            }))
        }
        _ => None,
    }
}

fn quote_type_from_type_arg(arg: &Ty) -> Option<Ty> {
    match arg {
        Ty::Quote(_) => Some(arg.clone()),
        Ty::Expr(expr) => match expr.kind() {
            ExprKind::Locator(locator) => {
                let ident = locator.as_ident()?.as_str().to_string();
                quote_type_from_ident(&ident, &[])
            }
            _ => None,
        },
        _ => None,
    }
}

fn lower_ty_path(node: &SyntaxNode) -> Result<Ty, LowerError> {
    let mut segments: Vec<Ident> = Vec::new();
    let mut saw_generic_start = false;
    for child in &node.children {
        let crate::syntax::SyntaxElement::Token(tok) = child else {
            continue;
        };
        if tok.is_trivia() {
            continue;
        }
        match tok.text.as_str() {
            "::" => continue,
            "<" => {
                saw_generic_start = true;
                break;
            }
            _ => {
                // Accept keyword segments like `crate`/`super` as well.
                if tok
                    .text
                    .chars()
                    .next()
                    .is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
                {
                    segments.push(Ident::new(tok.text.clone()));
                }
            }
        }
    }

    let args = node_children_types(node)
        .map(lower_type_from_cst)
        .collect::<Result<Vec<_>, _>>()?;

    let path = Path::new(segments.clone());
    if segments.len() == 1 && segments[0].as_str() == "quote" && args.len() == 1 {
        let Some(quote_ty) = quote_type_from_type_arg(&args[0]) else {
            return Err(LowerError::UnexpectedNode(node.kind));
        };
        return Ok(quote_ty);
    }

    if segments.len() == 1 {
        if let Some(quote_ty) = quote_type_from_ident(segments[0].as_str(), &args) {
            return Ok(quote_ty);
        }
    }

    if segments.last().map(|seg| seg.as_str()) == Some("Vec") && args.len() == 1 {
        return Ok(Ty::Vec(TypeVec {
            ty: Box::new(args[0].clone()),
        }));
    }

    if !saw_generic_start || args.is_empty() {
        return Ok(Ty::expr(Expr::path(path).with_span(node.span)));
    }

    let mut param_segments: Vec<ParameterPathSegment> = segments
        .into_iter()
        .map(ParameterPathSegment::from_ident)
        .collect();
    if let Some(last) = param_segments.last_mut() {
        last.args = args;
    }
    let ppath = ParameterPath::new(param_segments);
    Ok(Ty::expr(
        Expr::locator(Locator::parameter_path(ppath)).with_span(node.span),
    ))
}

fn lower_ty_ref(node: &SyntaxNode) -> Result<Ty, LowerError> {
    let mut is_mut = false;
    let mut lifetime: Option<Ident> = None;
    for child in &node.children {
        let crate::syntax::SyntaxElement::Token(tok) = child else {
            continue;
        };
        if tok.is_trivia() {
            continue;
        }
        if tok.text == "mut" {
            is_mut = true;
        }
        if tok.text.starts_with('\'') {
            lifetime = Some(Ident::new(tok.text.clone()));
        }
    }

    let inner = node_children_types(node)
        .next()
        .ok_or_else(|| LowerError::UnexpectedNode(node.kind))?;
    let inner = lower_type_from_cst(inner)?;
    Ok(Ty::Reference(
        TypeReference {
            ty: Box::new(inner),
            mutability: is_mut.then_some(true),
            lifetime,
        }
        .into(),
    ))
}

fn lower_ty_slice(node: &SyntaxNode) -> Result<Ty, LowerError> {
    let elem = node_children_types(node)
        .next()
        .ok_or_else(|| LowerError::UnexpectedNode(node.kind))?;
    let elem = lower_type_from_cst(elem)?;
    Ok(Ty::Slice(
        TypeSlice {
            elem: Box::new(elem),
        }
        .into(),
    ))
}

fn lower_ty_array(node: &SyntaxNode) -> Result<Ty, LowerError> {
    let elem = node_children_types(node)
        .next()
        .ok_or_else(|| LowerError::UnexpectedNode(node.kind))?;
    let elem = lower_type_from_cst(elem)?;
    let len_expr = node
        .children
        .iter()
        .find_map(|c| match c {
            crate::syntax::SyntaxElement::Node(n) if n.kind.category() == CstCategory::Expr => {
                Some(n.as_ref())
            }
            _ => None,
        })
        .ok_or_else(|| LowerError::UnexpectedNode(node.kind))?;
    let len_expr = lower_expr_from_cst(len_expr)?;
    Ok(Ty::Array(
        TypeArray {
            elem: Box::new(elem),
            len: Box::new(len_expr),
        }
        .into(),
    ))
}

fn lower_ty_fn(node: &SyntaxNode) -> Result<Ty, LowerError> {
    let mut types = node_children_types(node)
        .map(lower_type_from_cst)
        .collect::<Result<Vec<_>, _>>()?;

    let has_arrow = node.children.iter().any(
        |c| matches!(c, crate::syntax::SyntaxElement::Token(t) if !t.is_trivia() && t.text == "->"),
    );

    let ret_ty = if has_arrow {
        types.pop().map(|t| Box::new(t))
    } else {
        None
    };

    Ok(Ty::Function(
        TypeFunction {
            params: types,
            generics_params: Vec::new(),
            ret_ty,
        }
        .into(),
    ))
}

fn lower_ty_tuple(node: &SyntaxNode) -> Result<Ty, LowerError> {
    let types = node_children_types(node)
        .map(lower_type_from_cst)
        .collect::<Result<Vec<_>, _>>()?;
    Ok(Ty::Tuple(TypeTuple { types }.into()))
}

fn lower_ty_structural(node: &SyntaxNode) -> Result<Ty, LowerError> {
    let mut fields = Vec::new();
    for child in &node.children {
        let crate::syntax::SyntaxElement::Node(field) = child else {
            continue;
        };
        if field.kind != SyntaxKind::TyField {
            continue;
        }
        let name = field
            .children
            .iter()
            .find_map(|c| match c {
                crate::syntax::SyntaxElement::Token(t) if !t.is_trivia() && t.text != ":" => {
                    Some(t.text.clone())
                }
                _ => None,
            })
            .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::TyField))?;
        let ty_node = node_children_types(field)
            .next()
            .ok_or_else(|| LowerError::UnexpectedNode(SyntaxKind::TyField))?;
        let value = lower_type_from_cst(ty_node)?;
        fields.push(StructuralField::new(Ident::new(name), value));
    }
    let mut update: Option<Ty> = None;
    for (idx, child) in node.children.iter().enumerate() {
        let crate::syntax::SyntaxElement::Token(tok) = child else {
            continue;
        };
        if tok.is_trivia() || tok.text != ".." {
            continue;
        }
        if update.is_some() {
            return Err(LowerError::UnexpectedNode(SyntaxKind::TyStructural));
        }
        for next in node.children.iter().skip(idx + 1) {
            if let crate::syntax::SyntaxElement::Node(n) = next {
                if n.kind.category() == CstCategory::Type {
                    update = Some(lower_type_from_cst(n.as_ref())?);
                    break;
                }
            }
        }
        if update.is_none() {
            return Err(LowerError::UnexpectedNode(SyntaxKind::TyStructural));
        }
    }

    if let Some(update) = update {
        let lhs = Ty::Structural(TypeStructural { fields }.into());
        return Ok(Ty::TypeBinaryOp(
            TypeBinaryOp {
                kind: TypeBinaryOpKind::Add,
                lhs: Box::new(lhs),
                rhs: Box::new(update),
            }
            .into(),
        ));
    }

    Ok(Ty::Structural(TypeStructural { fields }.into()))
}

fn lower_ty_binary(node: &SyntaxNode) -> Result<Ty, LowerError> {
    let mut tys = node_children_types(node);
    let lhs = tys
        .next()
        .ok_or_else(|| LowerError::UnexpectedNode(node.kind))?;
    let rhs = tys
        .next()
        .ok_or_else(|| LowerError::UnexpectedNode(node.kind))?;
    let lhs = lower_type_from_cst(lhs)?;
    let rhs = lower_type_from_cst(rhs)?;

    let op = direct_operator_token_text(node).ok_or(LowerError::MissingOperator)?;
    let kind = match op.as_str() {
        "+" => TypeBinaryOpKind::Add,
        "|" => TypeBinaryOpKind::Union,
        "&" => TypeBinaryOpKind::Intersect,
        "-" => TypeBinaryOpKind::Subtract,
        _ => return Err(LowerError::MissingOperator),
    };
    Ok(Ty::TypeBinaryOp(
        TypeBinaryOp {
            kind,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
        .into(),
    ))
}

fn lower_ty_optional(node: &SyntaxNode) -> Result<Ty, LowerError> {
    let inner = node_children_types(node)
        .next()
        .ok_or_else(|| LowerError::UnexpectedNode(node.kind))?;
    let inner = lower_type_from_cst(inner)?;
    // `T?` lowers to `T | None` so later passes can materialize a tagged union.
    Ok(Ty::TypeBinaryOp(
        TypeBinaryOp {
            kind: TypeBinaryOpKind::Union,
            lhs: Box::new(inner),
            rhs: Box::new(Ty::value(Value::None(ValueNone))),
        }
        .into(),
    ))
}

fn lower_ty_impl_traits(node: &SyntaxNode) -> Result<Ty, LowerError> {
    let mut type_nodes = node_children_types(node);
    let bound_node = type_nodes
        .next()
        .ok_or_else(|| LowerError::UnexpectedNode(node.kind))?;
    let bound_ty = lower_type_from_cst(bound_node)?;

    // Special-case `impl Fn(T) -> U` into a first-class function type.
    // This matches the surface syntax used in `examples/09_higher_order_functions.fp`.
    let is_fn_trait = match &bound_ty {
        Ty::Expr(expr) => matches!(expr.kind(), ExprKind::Locator(loc) if loc.to_string() == "Fn"),
        _ => false,
    };
    let mut trailing_types = type_nodes
        .map(lower_type_from_cst)
        .collect::<Result<Vec<_>, _>>()?;
    if is_fn_trait && !trailing_types.is_empty() {
        let has_arrow = node.children.iter().any(|c| match c {
            crate::syntax::SyntaxElement::Token(t) if !t.is_trivia() => t.text == "->",
            _ => false,
        });
        let ret_ty = if has_arrow {
            Some(Box::new(
                trailing_types
                    .pop()
                    .ok_or(LowerError::UnexpectedNode(node.kind))?,
            ))
        } else {
            None
        };
        return Ok(Ty::Function(
            TypeFunction {
                params: trailing_types,
                generics_params: Vec::new(),
                ret_ty,
            }
            .into(),
        ));
    }

    // Default: keep the representation aligned with the old token-based parser: bounds as a
    // locator path (or a value-wrapped type).
    let bound_expr = match bound_ty {
        Ty::Expr(expr) => (*expr).clone(),
        other => Expr::value(Value::Type(other)),
    };

    Ok(Ty::ImplTraits(
        ImplTraits {
            bounds: TypeBounds::new(bound_expr),
        }
        .into(),
    ))
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
    for child in &node.children {
        let crate::syntax::SyntaxElement::Token(tok) = child else {
            continue;
        };
        if tok.is_trivia() {
            continue;
        }
        if tok.text == "::" || tok.text == "." {
            continue;
        }
        let text = tok.text.strip_prefix("r#").unwrap_or(&tok.text);
        if text == "crate" || text == "self" || text == "super" {
            segments.push(Ident::new(text.to_string()));
            continue;
        }
        if text
            .chars()
            .next()
            .is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
        {
            segments.push(Ident::new(text.to_string()));
        }
    }
    if segments.is_empty() {
        return Err(LowerError::UnexpectedNode(SyntaxKind::ExprMacroCall));
    }
    Ok(Path::new(segments))
}

fn direct_last_ident_token_text(node: &SyntaxNode) -> Option<String> {
    node.children.iter().rev().find_map(|child| match child {
        crate::syntax::SyntaxElement::Token(t)
            if !t.is_trivia()
                && t.text
                    .chars()
                    .next()
                    .is_some_and(|c| c.is_ascii_alphabetic() || c == '_') =>
        {
            Some(t.text.clone())
        }
        _ => None,
    })
}

fn macro_group_delimiter_and_tokens(node: &SyntaxNode) -> Option<(MacroDelimiter, String)> {
    // Find the first delimiter token after '!'.
    let mut seen_bang = false;
    let mut open_idx = None;
    let mut close_idx = None;
    let mut tokens: Vec<&crate::syntax::SyntaxToken> = Vec::new();
    crate::syntax::collect_tokens(node, &mut tokens);
    for (idx, tok) in tokens.iter().enumerate() {
        if tok.is_trivia() {
            continue;
        }
        if !seen_bang {
            if tok.text == "!" {
                seen_bang = true;
            }
            continue;
        }
        if open_idx.is_none() {
            if tok.text == "(" || tok.text == "{" || tok.text == "[" {
                open_idx = Some(idx);
            }
            continue;
        }
        // Track the last closing delimiter; parse ensures it's balanced.
        if tok.text == ")" || tok.text == "}" || tok.text == "]" {
            close_idx = Some(idx);
        }
    }

    let open_idx = open_idx?;
    let close_idx = close_idx?;
    let open = tokens[open_idx].text.as_str();
    let delimiter = match open {
        "(" => MacroDelimiter::Parenthesis,
        "{" => MacroDelimiter::Brace,
        "[" => MacroDelimiter::Bracket,
        _ => return None,
    };

    fn is_ident_like_char(c: char) -> bool {
        c.is_ascii_alphanumeric() || c == '_'
    }
    fn needs_space(prev: &str, next: &str) -> bool {
        let Some(p) = prev.chars().last() else {
            return false;
        };
        let Some(n) = next.chars().next() else {
            return false;
        };
        is_ident_like_char(p) && is_ident_like_char(n)
    }

    let mut inner = String::new();
    let mut prev: Option<&str> = None;
    for t in &tokens[(open_idx + 1)..close_idx] {
        let text = t.text.as_str();
        if let Some(prev_text) = prev {
            if needs_space(prev_text, text) {
                inner.push(' ');
            }
        }
        inner.push_str(text);
        prev = Some(text);
    }
    Some((delimiter, inner))
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
