use super::*;

use fp_core::error::Result;

/// Expand static quote values before AST-to-HIR lowering.
///
/// Quotes are source-level code values. Like Rust macro expansion, their
/// splice sites must be resolved while the source is still AST, so HIR never
/// needs a fake runtime representation for an `expr`/`stmt`/`item` token.
/// This phase intentionally knows nothing about `TypeBuilder`: type values
/// continue through the ordinary std declarations and intrinsic declarations
/// used by those declarations.
pub(super) fn expand_quote_splices(items: &mut [ast::Item]) -> Result<()> {
    let quotes = collect_quotes(items);
    rewrite_items(items, &quotes)
}

fn collect_quotes(items: &[ast::Item]) -> HashMap<String, ast::ExprQuote> {
    let mut quotes = HashMap::new();
    for item in items {
        match item.kind() {
            ast::ItemKind::DefConst(def) if let ast::ExprKind::Quote(quote) = def.value.kind() => {
                quotes.insert(def.name.name.clone(), quote.clone());
            }
            ast::ItemKind::Module(module) => quotes.extend(collect_quotes(&module.items)),
            ast::ItemKind::Impl(impl_block) => quotes.extend(collect_quotes(&impl_block.items)),
            ast::ItemKind::DefTrait(trait_def) => quotes.extend(collect_quotes(&trait_def.items)),
            _ => {}
        }
    }
    quotes
}

fn rewrite_items(items: &mut [ast::Item], quotes: &HashMap<String, ast::ExprQuote>) -> Result<()> {
    for item in items {
        match item.kind_mut() {
            ast::ItemKind::DefFunction(function) => rewrite_block(&mut function.body, quotes)?,
            ast::ItemKind::DefConst(constant) => rewrite_expr(&mut constant.value, quotes)?,
            ast::ItemKind::Module(module) => rewrite_items(&mut module.items, quotes)?,
            ast::ItemKind::Impl(impl_block) => rewrite_items(&mut impl_block.items, quotes)?,
            ast::ItemKind::DefTrait(trait_def) => rewrite_items(&mut trait_def.items, quotes)?,
            _ => {}
        }
    }
    Ok(())
}

fn rewrite_block(
    block: &mut ast::ExprBlock,
    quotes: &HashMap<String, ast::ExprQuote>,
) -> Result<()> {
    let mut rewritten = Vec::with_capacity(block.stmts.len());
    for mut stmt in std::mem::take(&mut block.stmts) {
        match &mut stmt {
            ast::BlockStmt::Expr(expr_stmt) => {
                if let Some(name) = splice_name(expr_stmt.expr.as_ref()) {
                    let quote = quotes.get(name).ok_or_else(|| {
                        fp_core::error::Error::from(format!(
                            "splice expression refers to unknown quote value `{name}`"
                        ))
                    })?;
                    match quote_kind(quote) {
                        ast::QuoteFragmentKind::Item => {
                            for quoted_stmt in &quote.block.stmts {
                                let ast::BlockStmt::Item(item) = quoted_stmt else {
                                    return Err(fp_core::error::Error::from(format!(
                                        "item splice `{name}` contains a non-item statement"
                                    )));
                                };
                                rewritten.push(ast::BlockStmt::Item(item.clone()));
                            }
                        }
                        ast::QuoteFragmentKind::Stmt => {
                            rewritten.extend(quote.block.stmts.iter().cloned());
                        }
                        ast::QuoteFragmentKind::Expr => {
                            expr_stmt.expr = Box::new(quoted_expr(quote, name)?);
                            rewrite_expr(&mut expr_stmt.expr, quotes)?;
                            rewritten.push(stmt);
                        }
                        ast::QuoteFragmentKind::Type => {
                            return Err(fp_core::error::Error::from(format!(
                                "type quote `{name}` cannot be spliced as a statement"
                            )));
                        }
                    }
                    continue;
                }
                rewrite_expr(&mut expr_stmt.expr, quotes)?;
                rewritten.push(stmt);
            }
            ast::BlockStmt::Let(let_stmt) => {
                if let Some(init) = &mut let_stmt.init {
                    rewrite_expr(init, quotes)?;
                }
                if let Some(diverge) = &mut let_stmt.diverge {
                    rewrite_expr(diverge, quotes)?;
                }
                rewritten.push(stmt);
            }
            ast::BlockStmt::Defer(defer_stmt) => {
                rewrite_expr(&mut defer_stmt.expr, quotes)?;
                rewritten.push(stmt);
            }
            ast::BlockStmt::Item(_) | ast::BlockStmt::Noop => rewritten.push(stmt),
        }
    }
    block.stmts = rewritten;
    Ok(())
}

fn rewrite_expr(expr: &mut ast::Expr, quotes: &HashMap<String, ast::ExprQuote>) -> Result<()> {
    if let ast::ExprKind::Splice(splice) = expr.kind() {
        let name = quote_name(splice.token.as_ref()).ok_or_else(|| {
            fp_core::error::Error::from("splice expression must name a quote value")
        })?;
        let quote = quotes.get(name).ok_or_else(|| {
            fp_core::error::Error::from(format!(
                "splice expression refers to unknown quote value `{name}` (token: {splice:?})"
            ))
        })?;
        let replacement = quoted_expr(quote, name)?;
        *expr = replacement;
        return Ok(());
    }

    match expr.kind_mut() {
        ast::ExprKind::Block(block) => rewrite_block(block, quotes),
        ast::ExprKind::ConstBlock(block) => rewrite_expr(&mut block.expr, quotes),
        ast::ExprKind::BinOp(binop) => {
            rewrite_expr(&mut binop.lhs, quotes)?;
            rewrite_expr(&mut binop.rhs, quotes)
        }
        ast::ExprKind::UnOp(unop) => rewrite_expr(&mut unop.val, quotes),
        ast::ExprKind::Invoke(invoke) => {
            if let ast::ExprInvokeTarget::Method(select) = &mut invoke.target {
                if select.field.name == "len"
                    && select.generic_args.is_none()
                    && invoke.args.is_empty()
                    && let Some(name) = quote_value_name(select.obj.as_ref())
                    && let Some(quote) = quotes.get(name)
                {
                    *expr = ast::Expr::value(ast::Value::int(quote_item_count(quote) as i64));
                    return Ok(());
                }
                rewrite_expr(&mut select.obj, quotes)?;
            } else if let ast::ExprInvokeTarget::Expr(target) = &mut invoke.target {
                rewrite_expr(target, quotes)?;
            }
            for arg in &mut invoke.args {
                rewrite_expr(arg, quotes)?;
            }
            for arg in &mut invoke.kwargs {
                rewrite_expr(&mut arg.value, quotes)?;
            }
            Ok(())
        }
        ast::ExprKind::If(if_expr) => {
            rewrite_expr(&mut if_expr.cond, quotes)?;
            rewrite_expr(&mut if_expr.then, quotes)?;
            if let Some(else_expr) = &mut if_expr.elze {
                rewrite_expr(else_expr, quotes)?;
            }
            Ok(())
        }
        ast::ExprKind::Reference(reference) => rewrite_expr(&mut reference.referee, quotes),
        ast::ExprKind::Dereference(deref) => rewrite_expr(&mut deref.referee, quotes),
        ast::ExprKind::Cast(cast) => rewrite_expr(&mut cast.expr, quotes),
        ast::ExprKind::Paren(paren) => rewrite_expr(&mut paren.expr, quotes),
        ast::ExprKind::Tuple(tuple) => {
            for value in &mut tuple.values {
                rewrite_expr(value, quotes)?;
            }
            Ok(())
        }
        ast::ExprKind::Array(array) => {
            for value in &mut array.values {
                rewrite_expr(value, quotes)?;
            }
            Ok(())
        }
        ast::ExprKind::ArrayRepeat(repeat) => {
            rewrite_expr(&mut repeat.elem, quotes)?;
            rewrite_expr(&mut repeat.len, quotes)
        }
        ast::ExprKind::FieldAccess(select) => rewrite_expr(&mut select.obj, quotes),
        ast::ExprKind::Index(index) => {
            rewrite_expr(&mut index.obj, quotes)?;
            rewrite_expr(&mut index.index, quotes)
        }
        ast::ExprKind::Splice(_) => unreachable!("splice was handled before descending"),
        _ => Ok(()),
    }
}

fn splice_name(expr: &ast::Expr) -> Option<&str> {
    let ast::ExprKind::Splice(splice) = expr.kind() else {
        return None;
    };
    quote_name(splice.token.as_ref())
}

fn quote_value_name(expr: &ast::Expr) -> Option<&str> {
    quote_name(expr).or_else(|| splice_name(expr))
}

fn quote_name(expr: &ast::Expr) -> Option<&str> {
    let ast::ExprKind::Name(ast::Name { path, .. }) = expr.kind() else {
        return None;
    };
    Some(path.last().ident.as_str())
}

fn quote_kind(quote: &ast::ExprQuote) -> ast::QuoteFragmentKind {
    quote.kind.unwrap_or_else(|| {
        if quote.block.last_expr().is_some() {
            ast::QuoteFragmentKind::Expr
        } else if quote
            .block
            .stmts
            .iter()
            .all(|stmt| matches!(stmt, ast::BlockStmt::Item(_)))
        {
            ast::QuoteFragmentKind::Item
        } else {
            ast::QuoteFragmentKind::Stmt
        }
    })
}

fn quoted_expr(quote: &ast::ExprQuote, name: &str) -> Result<ast::Expr> {
    quote
        .block
        .last_expr()
        .cloned()
        .ok_or_else(|| fp_core::error::Error::from(format!("quote `{name}` has no expression")))
}

fn quote_item_count(quote: &ast::ExprQuote) -> usize {
    quote
        .block
        .stmts
        .iter()
        .filter(|stmt| matches!(stmt, ast::BlockStmt::Item(_)))
        .count()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn expands_quote_item_count() {
        let parser = fp_lang::ast::FerroPhaseParser::new();
        let mut items = parser
            .parse_items_ast(
                r#"
                const GROUP: quote<[item]> = quote<[item]> {
                    fn first() {}
                    fn second() {}
                };
                const COUNT: i64 = GROUP.len();
                "#,
            )
            .expect("quote test source should parse");
        expand_quote_splices(&mut items).expect("quote expansion should succeed");
        let count = items
            .iter()
            .find_map(|item| match item.kind() {
                ast::ItemKind::DefConst(constant) if constant.name.name == "COUNT" => {
                    Some(constant.value.clone())
                }
                _ => None,
            })
            .expect("COUNT should remain in the item list");
        assert!(
            matches!(count.kind(), ast::ExprKind::Value(value) if matches!(value.as_ref(), ast::Value::Int(value) if value.value == 2))
        );
    }
}
