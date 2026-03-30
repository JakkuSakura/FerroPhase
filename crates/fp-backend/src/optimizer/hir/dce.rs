use std::collections::{HashMap, HashSet, VecDeque};

use fp_core::hir;

pub fn eliminate_dead_code(program: &mut hir::Program) -> usize {
    if has_unresolved_paths(program) {
        return 0;
    }

    let full_map = build_full_name_map(program);
    let tail_map = build_tail_name_map(program);
    let root_ids: Vec<_> = program
        .items
        .iter()
        .filter_map(|item| match &item.kind {
            hir::ItemKind::Function(function) if function.sig.name.as_str() == "main" => {
                Some(item.def_id)
            }
            hir::ItemKind::Expr(_) => Some(item.def_id),
            _ => None,
        })
        .collect();

    if root_ids.is_empty() {
        return 0;
    }

    let mut reachable = HashSet::new();
    let mut work = VecDeque::from(root_ids);
    while let Some(def_id) = work.pop_front() {
        if !reachable.insert(def_id) {
            continue;
        }
        let Some(item) = program.def_map.get(&def_id) else {
            continue;
        };
        collect_item_refs(item, &full_map, &tail_map, &mut work);
    }

    let before = program.items.len();
    program.items.retain(|item| match item.kind {
        hir::ItemKind::Function(_) => reachable.contains(&item.def_id),
        _ => true,
    });
    program.def_map.retain(|def_id, item| match item.kind {
        hir::ItemKind::Function(_) => reachable.contains(def_id),
        _ => true,
    });
    before.saturating_sub(program.items.len())
}

fn has_unresolved_paths(program: &hir::Program) -> bool {
    program.items.iter().any(item_has_unresolved_paths)
}

fn item_has_unresolved_paths(item: &hir::Item) -> bool {
    match &item.kind {
        hir::ItemKind::Function(function) => {
            function.sig.inputs.iter().any(|param| {
                type_has_unresolved_paths(&param.ty)
                    || param
                        .default
                        .as_ref()
                        .is_some_and(expr_has_unresolved_paths)
            }) || type_has_unresolved_paths(&function.sig.output)
                || function
                    .body
                    .as_ref()
                    .is_some_and(|body| expr_has_unresolved_paths(&body.value))
        }
        hir::ItemKind::Const(def) => {
            type_has_unresolved_paths(&def.ty) || expr_has_unresolved_paths(&def.body.value)
        }
        hir::ItemKind::Struct(def) => def
            .fields
            .iter()
            .any(|field| type_has_unresolved_paths(&field.ty)),
        hir::ItemKind::Enum(def) => def.variants.iter().any(|variant| {
            variant
                .payload
                .as_ref()
                .is_some_and(type_has_unresolved_paths)
                || variant
                    .discriminant
                    .as_ref()
                    .is_some_and(expr_has_unresolved_paths)
        }),
        hir::ItemKind::Impl(_) => true,
        hir::ItemKind::Expr(expr) => expr_has_unresolved_paths(expr),
    }
}

fn expr_has_unresolved_paths(expr: &hir::Expr) -> bool {
    match &expr.kind {
        hir::ExprKind::Path(path) => path_has_unresolved_segments(path),
        hir::ExprKind::Binary(_, lhs, rhs) | hir::ExprKind::Assign(lhs, rhs) => {
            expr_has_unresolved_paths(lhs) || expr_has_unresolved_paths(rhs)
        }
        hir::ExprKind::Unary(_, value)
        | hir::ExprKind::FieldAccess(value, _)
        | hir::ExprKind::Cast(value, _)
        | hir::ExprKind::Return(Some(value))
        | hir::ExprKind::Break(Some(value)) => expr_has_unresolved_paths(value),
        hir::ExprKind::Reference(reference) => expr_has_unresolved_paths(&reference.expr),
        hir::ExprKind::Call(callee, args) => {
            expr_has_unresolved_paths(callee)
                || args.iter().any(|arg| expr_has_unresolved_paths(&arg.value))
        }
        hir::ExprKind::MethodCall(receiver, _, args) => {
            expr_has_unresolved_paths(receiver)
                || args.iter().any(|arg| expr_has_unresolved_paths(&arg.value))
        }
        hir::ExprKind::Index(base, index) => {
            expr_has_unresolved_paths(base) || expr_has_unresolved_paths(index)
        }
        hir::ExprKind::Slice(slice) => {
            expr_has_unresolved_paths(&slice.base)
                || slice
                    .start
                    .as_ref()
                    .is_some_and(|expr| expr_has_unresolved_paths(expr))
                || slice
                    .end
                    .as_ref()
                    .is_some_and(|expr| expr_has_unresolved_paths(expr))
        }
        hir::ExprKind::Struct(path, fields) => {
            path_has_unresolved_segments(path)
                || fields
                    .iter()
                    .any(|field| expr_has_unresolved_paths(&field.expr))
        }
        hir::ExprKind::If(cond, then_branch, else_branch) => {
            expr_has_unresolved_paths(cond)
                || expr_has_unresolved_paths(then_branch)
                || else_branch
                    .as_ref()
                    .is_some_and(|expr| expr_has_unresolved_paths(expr))
        }
        hir::ExprKind::Match(scrutinee, arms) => {
            expr_has_unresolved_paths(scrutinee)
                || arms.iter().any(|arm| {
                    arm.guard.as_ref().is_some_and(expr_has_unresolved_paths)
                        || expr_has_unresolved_paths(&arm.body)
                })
        }
        hir::ExprKind::Try(expr_try) => {
            expr_has_unresolved_paths(&expr_try.expr)
                || expr_try
                    .catches
                    .iter()
                    .any(|catch| expr_has_unresolved_paths(&catch.body))
                || expr_try
                    .elze
                    .as_ref()
                    .is_some_and(|expr| expr_has_unresolved_paths(expr))
                || expr_try
                    .finally
                    .as_ref()
                    .is_some_and(|expr| expr_has_unresolved_paths(expr))
        }
        hir::ExprKind::Block(block) | hir::ExprKind::Loop(block) => {
            block.stmts.iter().any(stmt_has_unresolved_paths)
                || block
                    .expr
                    .as_ref()
                    .is_some_and(|expr| expr_has_unresolved_paths(expr))
        }
        hir::ExprKind::While(cond, block) => {
            expr_has_unresolved_paths(cond)
                || block.stmts.iter().any(stmt_has_unresolved_paths)
                || block
                    .expr
                    .as_ref()
                    .is_some_and(|expr| expr_has_unresolved_paths(expr))
        }
        hir::ExprKind::With(context, body) => {
            expr_has_unresolved_paths(context) || expr_has_unresolved_paths(body)
        }
        hir::ExprKind::IntrinsicCall(call) => call
            .callargs
            .iter()
            .any(|arg| expr_has_unresolved_paths(&arg.value)),
        hir::ExprKind::Let(_, ty, init) => {
            type_has_unresolved_paths(ty)
                || init
                    .as_ref()
                    .is_some_and(|expr| expr_has_unresolved_paths(expr))
        }
        hir::ExprKind::Array(elements) => elements.iter().any(expr_has_unresolved_paths),
        hir::ExprKind::ArrayRepeat { elem, len } => {
            expr_has_unresolved_paths(elem) || expr_has_unresolved_paths(len)
        }
        hir::ExprKind::Literal(_)
        | hir::ExprKind::FormatString(_)
        | hir::ExprKind::Continue
        | hir::ExprKind::Return(None)
        | hir::ExprKind::Break(None) => false,
    }
}

fn stmt_has_unresolved_paths(stmt: &hir::Stmt) -> bool {
    match &stmt.kind {
        hir::StmtKind::Local(local) => local.init.as_ref().is_some_and(expr_has_unresolved_paths),
        hir::StmtKind::Item(item) => item_has_unresolved_paths(item),
        hir::StmtKind::Expr(expr) | hir::StmtKind::Semi(expr) => expr_has_unresolved_paths(expr),
    }
}

fn type_has_unresolved_paths(ty: &hir::TypeExpr) -> bool {
    match &ty.kind {
        hir::TypeExprKind::Path(path) => path_has_unresolved_segments(path),
        hir::TypeExprKind::Structural(structural) => structural
            .fields
            .iter()
            .any(|field| type_has_unresolved_paths(&field.ty)),
        hir::TypeExprKind::TypeBinaryOp(binop) => {
            type_has_unresolved_paths(&binop.lhs) || type_has_unresolved_paths(&binop.rhs)
        }
        hir::TypeExprKind::Tuple(items) => items.iter().any(|item| type_has_unresolved_paths(item)),
        hir::TypeExprKind::Array(elem, len) => {
            type_has_unresolved_paths(elem)
                || len
                    .as_ref()
                    .is_some_and(|expr| expr_has_unresolved_paths(expr))
        }
        hir::TypeExprKind::Slice(inner)
        | hir::TypeExprKind::Ptr(inner)
        | hir::TypeExprKind::Ref(inner) => type_has_unresolved_paths(inner),
        hir::TypeExprKind::FnPtr(function) => {
            function
                .inputs
                .iter()
                .any(|input| type_has_unresolved_paths(input))
                || type_has_unresolved_paths(&function.output)
        }
        hir::TypeExprKind::Primitive(_)
        | hir::TypeExprKind::Never
        | hir::TypeExprKind::Infer
        | hir::TypeExprKind::Error => false,
    }
}

fn path_has_unresolved_segments(path: &hir::Path) -> bool {
    path.res.is_none() && path.segments.len() > 1
}

fn build_full_name_map(program: &hir::Program) -> HashMap<String, hir::DefId> {
    let mut names = HashMap::new();
    for item in &program.items {
        if let Some(name) = item_name(item) {
            names.insert(name.to_string(), item.def_id);
        }
    }
    names
}

fn build_tail_name_map(program: &hir::Program) -> HashMap<String, hir::DefId> {
    let mut names = HashMap::new();
    for item in &program.items {
        if let Some(name) = item_name(item) {
            let tail = name.rsplit("::").next().unwrap_or(name);
            names.entry(tail.to_string()).or_insert(item.def_id);
        }
    }
    names
}

fn item_name(item: &hir::Item) -> Option<&str> {
    match &item.kind {
        hir::ItemKind::Function(function) => Some(function.sig.name.as_str()),
        hir::ItemKind::Struct(def) => Some(def.name.as_str()),
        hir::ItemKind::Enum(def) => Some(def.name.as_str()),
        hir::ItemKind::Const(def) => Some(def.name.as_str()),
        hir::ItemKind::Impl(_) => None,
        hir::ItemKind::Expr(_) => None,
    }
}

fn collect_item_refs(
    item: &hir::Item,
    full_map: &HashMap<String, hir::DefId>,
    tail_map: &HashMap<String, hir::DefId>,
    work: &mut VecDeque<hir::DefId>,
) {
    match &item.kind {
        hir::ItemKind::Function(function) => {
            for param in &function.sig.inputs {
                collect_type_refs(&param.ty, full_map, tail_map, work);
                if let Some(default) = &param.default {
                    collect_expr_refs(default, full_map, tail_map, work);
                }
            }
            collect_type_refs(&function.sig.output, full_map, tail_map, work);
            if let Some(body) = &function.body {
                collect_expr_refs(&body.value, full_map, tail_map, work);
            }
        }
        hir::ItemKind::Const(def) => {
            collect_type_refs(&def.ty, full_map, tail_map, work);
            collect_expr_refs(&def.body.value, full_map, tail_map, work);
        }
        hir::ItemKind::Struct(def) => {
            for field in &def.fields {
                collect_type_refs(&field.ty, full_map, tail_map, work);
            }
        }
        hir::ItemKind::Enum(def) => {
            for variant in &def.variants {
                if let Some(payload) = &variant.payload {
                    collect_type_refs(payload, full_map, tail_map, work);
                }
                if let Some(discriminant) = &variant.discriminant {
                    collect_expr_refs(discriminant, full_map, tail_map, work);
                }
            }
        }
        hir::ItemKind::Impl(_) => {}
        hir::ItemKind::Expr(expr) => collect_expr_refs(expr, full_map, tail_map, work),
    }
}

fn collect_expr_refs(
    expr: &hir::Expr,
    full_map: &HashMap<String, hir::DefId>,
    tail_map: &HashMap<String, hir::DefId>,
    work: &mut VecDeque<hir::DefId>,
) {
    match &expr.kind {
        hir::ExprKind::Path(path) => collect_path_refs(path, full_map, tail_map, work),
        hir::ExprKind::Binary(_, lhs, rhs) | hir::ExprKind::Assign(lhs, rhs) => {
            collect_expr_refs(lhs, full_map, tail_map, work);
            collect_expr_refs(rhs, full_map, tail_map, work);
        }
        hir::ExprKind::Unary(_, value)
        | hir::ExprKind::FieldAccess(value, _)
        | hir::ExprKind::Cast(value, _)
        | hir::ExprKind::Return(Some(value))
        | hir::ExprKind::Break(Some(value)) => collect_expr_refs(value, full_map, tail_map, work),
        hir::ExprKind::Reference(reference) => {
            collect_expr_refs(&reference.expr, full_map, tail_map, work)
        }
        hir::ExprKind::Call(callee, args) => {
            collect_expr_refs(callee, full_map, tail_map, work);
            for arg in args {
                collect_expr_refs(&arg.value, full_map, tail_map, work);
            }
        }
        hir::ExprKind::MethodCall(receiver, _, args) => {
            collect_expr_refs(receiver, full_map, tail_map, work);
            for arg in args {
                collect_expr_refs(&arg.value, full_map, tail_map, work);
            }
        }
        hir::ExprKind::Index(base, index) => {
            collect_expr_refs(base, full_map, tail_map, work);
            collect_expr_refs(index, full_map, tail_map, work);
        }
        hir::ExprKind::Slice(slice) => {
            collect_expr_refs(&slice.base, full_map, tail_map, work);
            if let Some(start) = &slice.start {
                collect_expr_refs(start.as_ref(), full_map, tail_map, work);
            }
            if let Some(end) = &slice.end {
                collect_expr_refs(end.as_ref(), full_map, tail_map, work);
            }
        }
        hir::ExprKind::Struct(path, fields) => {
            collect_path_refs(path, full_map, tail_map, work);
            for field in fields {
                collect_expr_refs(&field.expr, full_map, tail_map, work);
            }
        }
        hir::ExprKind::If(cond, then_branch, else_branch) => {
            collect_expr_refs(cond, full_map, tail_map, work);
            collect_expr_refs(then_branch, full_map, tail_map, work);
            if let Some(elze) = else_branch {
                collect_expr_refs(elze, full_map, tail_map, work);
            }
        }
        hir::ExprKind::Match(scrutinee, arms) => {
            collect_expr_refs(scrutinee, full_map, tail_map, work);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    collect_expr_refs(guard, full_map, tail_map, work);
                }
                collect_expr_refs(&arm.body, full_map, tail_map, work);
            }
        }
        hir::ExprKind::Try(expr_try) => {
            collect_expr_refs(&expr_try.expr, full_map, tail_map, work);
            for catch in &expr_try.catches {
                collect_expr_refs(&catch.body, full_map, tail_map, work);
            }
            if let Some(elze) = &expr_try.elze {
                collect_expr_refs(elze, full_map, tail_map, work);
            }
            if let Some(finally) = &expr_try.finally {
                collect_expr_refs(finally, full_map, tail_map, work);
            }
        }
        hir::ExprKind::Block(block) | hir::ExprKind::Loop(block) => {
            collect_block_refs(block, full_map, tail_map, work)
        }
        hir::ExprKind::While(cond, block) => {
            collect_expr_refs(cond, full_map, tail_map, work);
            collect_block_refs(block, full_map, tail_map, work);
        }
        hir::ExprKind::With(context, body) => {
            collect_expr_refs(context, full_map, tail_map, work);
            collect_expr_refs(body, full_map, tail_map, work);
        }
        hir::ExprKind::IntrinsicCall(call) => {
            for arg in &call.callargs {
                collect_expr_refs(&arg.value, full_map, tail_map, work);
            }
        }
        hir::ExprKind::FormatString(_) | hir::ExprKind::Continue | hir::ExprKind::Literal(_) => {}
        hir::ExprKind::Let(_, ty, init) => {
            collect_type_refs(ty, full_map, tail_map, work);
            if let Some(init) = init {
                collect_expr_refs(init, full_map, tail_map, work);
            }
        }
        hir::ExprKind::Return(None) | hir::ExprKind::Break(None) => {}
        hir::ExprKind::Array(elements) => {
            for elem in elements {
                collect_expr_refs(elem, full_map, tail_map, work);
            }
        }
        hir::ExprKind::ArrayRepeat { elem, len } => {
            collect_expr_refs(elem, full_map, tail_map, work);
            collect_expr_refs(len, full_map, tail_map, work);
        }
    }
}

fn collect_block_refs(
    block: &hir::Block,
    full_map: &HashMap<String, hir::DefId>,
    tail_map: &HashMap<String, hir::DefId>,
    work: &mut VecDeque<hir::DefId>,
) {
    for stmt in &block.stmts {
        match &stmt.kind {
            hir::StmtKind::Local(local) => {
                if let Some(init) = &local.init {
                    collect_expr_refs(init, full_map, tail_map, work);
                }
            }
            hir::StmtKind::Item(item) => collect_item_refs(item, full_map, tail_map, work),
            hir::StmtKind::Expr(expr) | hir::StmtKind::Semi(expr) => {
                collect_expr_refs(expr, full_map, tail_map, work)
            }
        }
    }
    if let Some(expr) = &block.expr {
        collect_expr_refs(expr, full_map, tail_map, work);
    }
}

fn collect_type_refs(
    ty: &hir::TypeExpr,
    full_map: &HashMap<String, hir::DefId>,
    tail_map: &HashMap<String, hir::DefId>,
    work: &mut VecDeque<hir::DefId>,
) {
    match &ty.kind {
        hir::TypeExprKind::Path(path) => collect_path_refs(path, full_map, tail_map, work),
        hir::TypeExprKind::Structural(structural) => {
            for field in &structural.fields {
                collect_type_refs(&field.ty, full_map, tail_map, work);
            }
        }
        hir::TypeExprKind::TypeBinaryOp(binop) => {
            collect_type_refs(&binop.lhs, full_map, tail_map, work);
            collect_type_refs(&binop.rhs, full_map, tail_map, work);
        }
        hir::TypeExprKind::Tuple(items) => {
            for item in items {
                collect_type_refs(item, full_map, tail_map, work);
            }
        }
        hir::TypeExprKind::Array(elem, len) => {
            collect_type_refs(elem, full_map, tail_map, work);
            if let Some(len) = len {
                collect_expr_refs(len, full_map, tail_map, work);
            }
        }
        hir::TypeExprKind::Slice(inner)
        | hir::TypeExprKind::Ptr(inner)
        | hir::TypeExprKind::Ref(inner) => collect_type_refs(inner, full_map, tail_map, work),
        hir::TypeExprKind::FnPtr(function) => {
            for input in &function.inputs {
                collect_type_refs(input, full_map, tail_map, work);
            }
            collect_type_refs(&function.output, full_map, tail_map, work);
        }
        hir::TypeExprKind::Primitive(_)
        | hir::TypeExprKind::Never
        | hir::TypeExprKind::Infer
        | hir::TypeExprKind::Error => {}
    }
}

fn collect_path_refs(
    path: &hir::Path,
    full_map: &HashMap<String, hir::DefId>,
    tail_map: &HashMap<String, hir::DefId>,
    work: &mut VecDeque<hir::DefId>,
) {
    if let Some(hir::Res::Def(def_id)) = path.res {
        work.push_back(def_id);
        return;
    }
    let segments = path.segments.as_slice();
    if segments.is_empty() {
        return;
    }
    let full = segments
        .iter()
        .map(|seg| seg.name.as_str())
        .collect::<Vec<_>>()
        .join("::");
    if let Some(def_id) = full_map.get(&full) {
        work.push_back(*def_id);
        return;
    }
    let tail = segments
        .last()
        .map(|seg| seg.name.as_str())
        .unwrap_or_default();
    if let Some(def_id) = tail_map.get(tail) {
        work.push_back(*def_id);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fp_core::hir::{
        self, Body, Expr, ExprKind, Function, FunctionSig, Item, ItemKind, Path, PathSegment,
        Program, Symbol, TypeExpr, TypeExprKind, Visibility,
    };
    use fp_core::span::Span;

    fn symbol(name: &str) -> Symbol {
        Symbol::new(name)
    }

    fn unit_ty() -> TypeExpr {
        TypeExpr {
            hir_id: 0,
            kind: TypeExprKind::Tuple(Vec::new()),
            span: Span::null(),
        }
    }

    fn literal_expr(hir_id: hir::HirId, value: i64) -> Expr {
        Expr {
            hir_id,
            kind: ExprKind::Literal(hir::Lit::Integer(value)),
            span: Span::null(),
        }
    }

    fn path_expr(hir_id: hir::HirId, segments: &[&str], res: Option<hir::Res>) -> Expr {
        Expr {
            hir_id,
            kind: ExprKind::Path(Path {
                segments: segments
                    .iter()
                    .map(|segment| PathSegment {
                        name: symbol(segment),
                        args: None,
                    })
                    .collect(),
                res,
            }),
            span: Span::null(),
        }
    }

    fn call_expr(
        hir_id: hir::HirId,
        callee_segments: &[&str],
        callee_res: Option<hir::Res>,
    ) -> Expr {
        Expr {
            hir_id,
            kind: ExprKind::Call(
                Box::new(path_expr(hir_id + 1, callee_segments, callee_res)),
                Vec::new(),
            ),
            span: Span::null(),
        }
    }

    fn function_item(def_id: hir::DefId, name: &str) -> Item {
        Item {
            hir_id: def_id,
            def_id,
            visibility: Visibility::Private,
            kind: ItemKind::Function(Function {
                sig: FunctionSig {
                    name: symbol(name),
                    inputs: Vec::new(),
                    output: unit_ty(),
                    generics: Default::default(),
                    abi: hir::Abi::Rust,
                },
                body: Some(Body {
                    hir_id: def_id,
                    params: Vec::new(),
                    value: literal_expr(def_id, 0),
                }),
                is_const: false,
                is_extern: false,
                attrs: Vec::new(),
            }),
            span: Span::null(),
        }
    }

    fn expr_item(def_id: hir::DefId, expr: Expr) -> Item {
        Item {
            hir_id: def_id,
            def_id,
            visibility: Visibility::Private,
            kind: ItemKind::Expr(expr),
            span: Span::null(),
        }
    }

    fn program(items: Vec<Item>) -> Program {
        let def_map = items
            .iter()
            .map(|item| (item.def_id, item.clone()))
            .collect();
        Program {
            items,
            def_map,
            next_hir_id: 100,
        }
    }

    #[test]
    fn dce_keeps_functions_reached_from_top_level_expr_items() {
        let used = function_item(1, "shell");
        let unused = function_item(2, "copy");
        let root = expr_item(3, call_expr(30, &["shell"], Some(hir::Res::Def(1))));
        let mut program = program(vec![used, unused, root]);

        let removed = eliminate_dead_code(&mut program);

        assert_eq!(removed, 1);
        assert!(program.items.iter().any(|item| matches!(
            &item.kind,
            ItemKind::Function(function) if function.sig.name.as_str() == "shell"
        )));
        assert!(!program.items.iter().any(|item| matches!(
            &item.kind,
            ItemKind::Function(function) if function.sig.name.as_str() == "copy"
        )));
        assert!(program
            .items
            .iter()
            .any(|item| matches!(&item.kind, ItemKind::Expr(_))));
    }

    #[test]
    fn dce_skips_pruning_when_example_like_program_has_unresolved_paths() {
        let helper = function_item(1, "unused_helper");
        let root = expr_item(2, call_expr(20, &["std", "server", "shell"], None));
        let mut program = program(vec![helper, root]);

        let removed = eliminate_dead_code(&mut program);

        assert_eq!(removed, 0);
        assert!(program.items.iter().any(|item| matches!(
            &item.kind,
            ItemKind::Function(function) if function.sig.name.as_str() == "unused_helper"
        )));
    }
}
