use crate::ast::{
    BlockStmt, Expr, ExprBlock, ExprConstBlock, ExprFor, ExprIf, ExprIntrinsicContainer,
    ExprInvokeTarget, ExprKind, ExprMatch, ExprTry, ExprWhile, File, Item, ItemChunk,
    ItemDefFunction, ItemDefTrait, ItemImpl, ItemKind, Module, Value,
};
use crate::module::path::QualifiedPath;

pub fn annotate_collected_items(file: &mut File) {
    let root = QualifiedPath::new(Vec::new());
    annotate_file(file, &root);
}

fn annotate_file(file: &mut File, module_path: &QualifiedPath) {
    file.collected_items = direct_items(&file.items);
    for item in &mut file.items {
        annotate_item(item, module_path);
    }
}

fn annotate_item(item: &mut Item, module_path: &QualifiedPath) {
    match item.kind_mut() {
        ItemKind::Module(module) => annotate_module(module, module_path),
        ItemKind::DefFunction(function) => annotate_function(function, module_path),
        ItemKind::DefConst(def) => annotate_expr(def.value.as_mut(), module_path),
        ItemKind::DefStatic(def) => annotate_expr(def.value.as_mut(), module_path),
        ItemKind::DefTrait(def) => annotate_trait(def, module_path),
        ItemKind::Impl(impl_block) => annotate_impl(impl_block, module_path),
        ItemKind::Expr(expr) => annotate_expr(expr, module_path),
        ItemKind::ConstBlock(block) => annotate_const_block(block, module_path),
        ItemKind::Macro(_)
        | ItemKind::DefStruct(_)
        | ItemKind::DefStructural(_)
        | ItemKind::DefEnum(_)
        | ItemKind::DefType(_)
        | ItemKind::OpaqueType(_)
        | ItemKind::DeclType(_)
        | ItemKind::DeclConst(_)
        | ItemKind::DeclStatic(_)
        | ItemKind::DeclFunction(_)
        | ItemKind::Import(_)
        | ItemKind::Any(_) => {}
    }
}

fn annotate_module(module: &mut Module, module_path: &QualifiedPath) {
    let next_module = module_path.with_segment(module.name.as_str().to_string());
    module.collected_items = direct_items(&module.items);
    for item in &mut module.items {
        annotate_item(item, &next_module);
    }
}

fn annotate_function(function: &mut ItemDefFunction, module_path: &QualifiedPath) {
    function.collected_items = direct_block_items(&function.body);
    annotate_block(&mut function.body, module_path);
}

fn annotate_trait(def: &mut ItemDefTrait, module_path: &QualifiedPath) {
    def.collected_items = direct_items(&def.items);
    for item in &mut def.items {
        annotate_item(item, module_path);
    }
}

fn annotate_impl(impl_block: &mut ItemImpl, module_path: &QualifiedPath) {
    impl_block.collected_items = direct_items(&impl_block.items);
    for item in &mut impl_block.items {
        annotate_item(item, module_path);
    }
}

fn annotate_block(block: &mut ExprBlock, module_path: &QualifiedPath) {
    block.collected_items = direct_block_items(block);
    for stmt in &mut block.stmts {
        match stmt {
            BlockStmt::Item(item) => annotate_item(item.as_mut(), module_path),
            BlockStmt::Expr(stmt) => annotate_expr(stmt.expr.as_mut(), module_path),
            BlockStmt::Let(stmt) => {
                if let Some(init) = stmt.init.as_mut() {
                    annotate_expr(init, module_path);
                }
                if let Some(diverge) = stmt.diverge.as_mut() {
                    annotate_expr(diverge, module_path);
                }
            }
            BlockStmt::Defer(stmt) => annotate_expr(stmt.expr.as_mut(), module_path),
            BlockStmt::Noop | BlockStmt::Any(_) => {}
        }
    }
}

fn annotate_const_block(const_block: &mut ExprConstBlock, module_path: &QualifiedPath) {
    const_block.collected_items = direct_expr_items(const_block.expr.as_ref());
    annotate_expr(const_block.expr.as_mut(), module_path);
}

fn annotate_expr(expr: &mut Expr, module_path: &QualifiedPath) {
    match expr.kind_mut() {
        ExprKind::Block(block) => annotate_block(block, module_path),
        ExprKind::If(expr_if) => annotate_if(expr_if, module_path),
        ExprKind::Loop(expr_loop) => annotate_expr(expr_loop.body.as_mut(), module_path),
        ExprKind::While(expr_while) => annotate_while(expr_while, module_path),
        ExprKind::With(expr_with) => {
            annotate_expr(expr_with.context.as_mut(), module_path);
            annotate_expr(expr_with.body.as_mut(), module_path);
        }
        ExprKind::Return(expr_return) => {
            if let Some(value) = expr_return.value.as_mut() {
                annotate_expr(value.as_mut(), module_path);
            }
        }
        ExprKind::Break(expr_break) => {
            if let Some(value) = expr_break.value.as_mut() {
                annotate_expr(value.as_mut(), module_path);
            }
        }
        ExprKind::ConstBlock(const_block) => annotate_const_block(const_block, module_path),
        ExprKind::Match(expr_match) => annotate_match(expr_match, module_path),
        ExprKind::Let(expr_let) => annotate_expr(expr_let.expr.as_mut(), module_path),
        ExprKind::Assign(assign) => {
            annotate_expr(assign.target.as_mut(), module_path);
            annotate_expr(assign.value.as_mut(), module_path);
        }
        ExprKind::Cast(cast) => annotate_expr(cast.expr.as_mut(), module_path),
        ExprKind::Invoke(invoke) => {
            match &mut invoke.target {
                ExprInvokeTarget::Expr(inner) => annotate_expr(inner.as_mut(), module_path),
                ExprInvokeTarget::Method(select) => annotate_expr(select.obj.as_mut(), module_path),
                ExprInvokeTarget::Closure(closure) => {
                    annotate_expr(closure.body.as_mut(), module_path)
                }
                ExprInvokeTarget::Function(_)
                | ExprInvokeTarget::Type(_)
                | ExprInvokeTarget::BinOp(_) => {}
            }
            for arg in &mut invoke.args {
                annotate_expr(arg, module_path);
            }
            for kwarg in &mut invoke.kwargs {
                annotate_expr(&mut kwarg.value, module_path);
            }
        }
        ExprKind::Await(expr_await) => annotate_expr(expr_await.base.as_mut(), module_path),
        ExprKind::Select(select) => annotate_expr(select.obj.as_mut(), module_path),
        ExprKind::Struct(struct_expr) => {
            for field in &mut struct_expr.fields {
                if let Some(value) = field.value.as_mut() {
                    annotate_expr(value, module_path);
                }
            }
        }
        ExprKind::Structural(struct_expr) => {
            for field in &mut struct_expr.fields {
                if let Some(value) = field.value.as_mut() {
                    annotate_expr(value, module_path);
                }
            }
        }
        ExprKind::Array(array_expr) => {
            for value in &mut array_expr.values {
                annotate_expr(value, module_path);
            }
        }
        ExprKind::ArrayRepeat(repeat) => {
            annotate_expr(repeat.elem.as_mut(), module_path);
            annotate_expr(repeat.len.as_mut(), module_path);
        }
        ExprKind::Tuple(tuple_expr) => {
            for value in &mut tuple_expr.values {
                annotate_expr(value, module_path);
            }
        }
        ExprKind::BinOp(binop) => {
            annotate_expr(binop.lhs.as_mut(), module_path);
            annotate_expr(binop.rhs.as_mut(), module_path);
        }
        ExprKind::UnOp(unop) => annotate_expr(unop.val.as_mut(), module_path),
        ExprKind::Reference(reference) => annotate_expr(reference.referee.as_mut(), module_path),
        ExprKind::Dereference(deref) => annotate_expr(deref.referee.as_mut(), module_path),
        ExprKind::Index(index) => {
            annotate_expr(index.obj.as_mut(), module_path);
            annotate_expr(index.index.as_mut(), module_path);
        }
        ExprKind::Splat(splat) => annotate_expr(splat.iter.as_mut(), module_path),
        ExprKind::SplatDict(splat) => annotate_expr(splat.dict.as_mut(), module_path),
        ExprKind::Try(expr_try) => annotate_try(expr_try, module_path),
        ExprKind::Async(async_expr) => annotate_expr(async_expr.expr.as_mut(), module_path),
        ExprKind::Closure(closure) => annotate_expr(closure.body.as_mut(), module_path),
        ExprKind::Closured(closured) => annotate_expr(closured.expr.as_mut(), module_path),
        ExprKind::Paren(paren) => annotate_expr(paren.expr.as_mut(), module_path),
        ExprKind::For(expr_for) => annotate_for(expr_for, module_path),
        ExprKind::Item(item) => annotate_item(item.as_mut(), module_path),
        ExprKind::IntrinsicCall(call) => {
            for arg in &mut call.args {
                annotate_expr(arg, module_path);
            }
            for kwarg in &mut call.kwargs {
                annotate_expr(&mut kwarg.value, module_path);
            }
        }
        ExprKind::IntrinsicContainer(container) => match container {
            ExprIntrinsicContainer::VecElements { elements } => {
                for element in elements {
                    annotate_expr(element, module_path);
                }
            }
            ExprIntrinsicContainer::VecRepeat { elem, len } => {
                annotate_expr(elem.as_mut(), module_path);
                annotate_expr(len.as_mut(), module_path);
            }
            ExprIntrinsicContainer::HashMapEntries { entries } => {
                for entry in entries {
                    annotate_expr(&mut entry.key, module_path);
                    annotate_expr(&mut entry.value, module_path);
                }
            }
        },
        ExprKind::Range(range) => {
            if let Some(start) = range.start.as_mut() {
                annotate_expr(start, module_path);
            }
            if let Some(end) = range.end.as_mut() {
                annotate_expr(end, module_path);
            }
            if let Some(step) = range.step.as_mut() {
                annotate_expr(step, module_path);
            }
        }
        ExprKind::Quote(quote) => {
            quote.collected_items = direct_block_items(&quote.block);
            annotate_block(&mut quote.block, module_path);
        }
        ExprKind::Splice(splice) => annotate_expr(splice.token.as_mut(), module_path),
        ExprKind::SplicePending(pending) => annotate_expr(pending.token.as_mut(), module_path),
        ExprKind::Value(value) => annotate_value(value.as_mut(), module_path),
        ExprKind::Id(_)
        | ExprKind::Name(_)
        | ExprKind::Continue(_)
        | ExprKind::FormatString(_)
        | ExprKind::Macro(_)
        | ExprKind::Any(_) => {}
    }
}

fn annotate_if(expr_if: &mut ExprIf, module_path: &QualifiedPath) {
    annotate_expr(expr_if.cond.as_mut(), module_path);
    annotate_expr(expr_if.then.as_mut(), module_path);
    if let Some(elze) = expr_if.elze.as_mut() {
        annotate_expr(elze.as_mut(), module_path);
    }
}

fn annotate_while(expr_while: &mut ExprWhile, module_path: &QualifiedPath) {
    annotate_expr(expr_while.cond.as_mut(), module_path);
    annotate_expr(expr_while.body.as_mut(), module_path);
}

fn annotate_match(expr_match: &mut ExprMatch, module_path: &QualifiedPath) {
    if let Some(scrutinee) = expr_match.scrutinee.as_mut() {
        annotate_expr(scrutinee.as_mut(), module_path);
    }
    for case in &mut expr_match.cases {
        annotate_expr(case.cond.as_mut(), module_path);
        if let Some(guard) = case.guard.as_mut() {
            annotate_expr(guard.as_mut(), module_path);
        }
        annotate_expr(case.body.as_mut(), module_path);
    }
}

fn annotate_try(expr_try: &mut ExprTry, module_path: &QualifiedPath) {
    annotate_expr(expr_try.expr.as_mut(), module_path);
    for catch in &mut expr_try.catches {
        annotate_expr(catch.body.as_mut(), module_path);
    }
    if let Some(elze) = expr_try.elze.as_mut() {
        annotate_expr(elze.as_mut(), module_path);
    }
    if let Some(finally) = expr_try.finally.as_mut() {
        annotate_expr(finally.as_mut(), module_path);
    }
}

fn annotate_for(expr_for: &mut ExprFor, module_path: &QualifiedPath) {
    annotate_expr(expr_for.iter.as_mut(), module_path);
    annotate_expr(expr_for.body.as_mut(), module_path);
}

fn annotate_value(value: &mut Value, module_path: &QualifiedPath) {
    match value {
        Value::Expr(expr) => annotate_expr(expr.as_mut(), module_path),
        Value::Function(function) => annotate_expr(function.body.as_mut(), module_path),
        _ => {}
    }
}

fn direct_items(items: &[Item]) -> ItemChunk {
    items.to_vec()
}

fn direct_block_items(block: &ExprBlock) -> ItemChunk {
    let mut items = Vec::new();
    for stmt in &block.stmts {
        match stmt {
            BlockStmt::Item(item) => items.push(item.as_ref().clone()),
            BlockStmt::Expr(stmt) => items.extend(direct_expr_items(stmt.expr.as_ref())),
            BlockStmt::Let(_) | BlockStmt::Defer(_) | BlockStmt::Noop | BlockStmt::Any(_) => {}
        }
    }
    items
}

fn direct_expr_items(expr: &Expr) -> ItemChunk {
    match expr.kind() {
        ExprKind::Item(item) => vec![item.as_ref().clone()],
        ExprKind::Paren(paren) => direct_expr_items(paren.expr.as_ref()),
        _ => Vec::new(),
    }
}
