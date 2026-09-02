use fp_core::Result;
use fp_core::ast::{
    BlockStmt, Expr, ExprBlock, ExprIntrinsicCall, ExprInvoke, ExprInvokeTarget, ExprKind, File,
    FunctionSignature, Item, ItemKind, Name, Value,
};
use fp_core::intrinsics::{CallKind, IntrinsicMaterializer, MaterializeOutcome};
use std::cell::RefCell;
use std::collections::HashMap;

pub struct ShellMaterializer<'a> {
    inventory: Option<&'a File>,
    sigs: RefCell<Option<HashMap<String, FunctionSignature>>>,
}

impl<'a> ShellMaterializer<'a> {
    pub fn new(inventory: Option<&'a File>) -> Self {
        Self {
            inventory,
            sigs: RefCell::new(None),
        }
    }

    fn host_transport_for(&self, host: &str) -> Option<String> {
        if host == "localhost" {
            return Some("local".into());
        }
        let file = self.inventory?;
        let item = file.items.iter().find_map(|i| match i.kind() {
            ItemKind::DefFunction(f) if f.name.as_str() == "inventory" => Some(f),
            _ => None,
        })?;
        let hosts_expr = struct_field_from_block(&item.body, "hosts")?;
        let map = match hosts_expr.kind() {
            ExprKind::Value(v) => match v.as_ref() {
                Value::Map(map) => Some(map),
                _ => None,
            },
            _ => None,
        }?;
        let entry = map
            .entries
            .iter()
            .find(|e| matches!(&e.key, Value::String(s) if s.value == host))?;
        match &entry.value {
            Value::Struct(s) => s.structural.fields.iter().find_map(|f| {
                if f.name.as_str() == "transport" {
                    match &f.value {
                        Value::String(s) => Some(s.value.clone()),
                        _ => None,
                    }
                } else {
                    None
                }
            }),
            Value::Map(map) => map.entries.iter().find_map(|e| match &e.key {
                Value::String(s) if s.value == "transport" => match &e.value {
                    Value::String(s) => Some(s.value.clone()),
                    _ => None,
                },
                _ => None,
            }),
            _ => None,
        }
    }
}

impl ShellMaterializer<'_> {
    fn prepare_file(&self, file: &mut File) {
        // Scan signatures from the AST
        *self.sigs.borrow_mut() = Some(scan_all_signatures(file));
        if let Some(sigs) = self.sigs.borrow().as_ref() {
            for item in &mut file.items {
                inject_with_contexts_in_item(item, sigs);
            }
        }

        // Flatten main body to top-level Expr items
        let mut new_items = Vec::new();
        let mut i = 0;
        while i < file.items.len() {
            match file.items[i].kind() {
                ItemKind::DefFunction(f) if f.name.as_str() == "main" => {
                    push_main_body_from_block(&f.body, &mut new_items);
                }
                ItemKind::DefConst(c) if c.name.as_str() == "main" => {
                    if let ExprKind::Block(block) = c.value.kind() {
                        push_main_body_from_block(block, &mut new_items);
                    }
                }
                _ => new_items.push(file.items[i].clone()),
            }
            i += 1;
        }
        file.items = new_items;
    }

    fn lower_invoke(
        &self,
        invoke: &mut ExprInvoke,
        _expr_ty: &fp_core::ast::TySlot,
    ) -> Result<Option<Expr>> {
        // Fill missing args from function signature (before mangling so sig lookup works)
        if let Some(ref sigs) = *self.sigs.borrow() {
            fill_args(invoke, sigs);
        }

        // Rewrite known shell calls to intrinsic calls (before mangling)
        if let Some(expr) = try_rewrite_to_intrinsic(invoke) {
            if let ExprKind::IntrinsicCall(mut call) = expr.into_parts().2 {
                // Convert intrinsic call to final mangled invoke
                return self.lower_intrinsic_call(&mut call, &None);
            }
        }

        // Normalize invoke target to Function form with an Ident name
        // (fp-bash only handles identifier targets, not paths)
        let name = invoke_target_name(&invoke.target).unwrap_or_default();
        let mangled = mangle_name(&name);
        invoke.target = ExprInvokeTarget::Function(Name::ident(mangled));

        Ok(None)
    }

    fn lower_intrinsic_call(
        &self,
        call: &mut ExprIntrinsicCall,
        _expr_ty: &fp_core::ast::TySlot,
    ) -> Result<Option<Expr>> {
        match call.kind {
            CallKind::ShellExec => {
                let host = call.args.get(1).and_then(string_val);
                let transport = host
                    .as_deref()
                    .and_then(|host| self.host_transport_for(host));
                let suffix = match transport.as_deref() {
                    Some("ssh") => "shell_ssh",
                    Some("docker") => "shell_docker",
                    Some("kubectl") => "shell_kubectl",
                    Some("winrm") => "shell_winrm",
                    Some("chroot") => "shell_chroot",
                    Some("local") => "shell_local",
                    Some(_) => "shell_local",
                    None if host.is_none() => "shell",
                    None => "shell_local",
                };
                Ok(Some(invoke_to(
                    call.span,
                    &mangle_name(&format!("std::ops::server::{suffix}")),
                    &call.args,
                    &call.kwargs,
                )))
            }
            CallKind::ShellFileCopy => {
                let host = call.args.get(2).and_then(string_val).unwrap_or_default();
                let transport = self.host_transport_for(&host);
                let suffix = match transport.as_deref() {
                    Some("ssh") => "copy_ssh",
                    Some("docker") => "copy_docker",
                    Some("kubectl") => "copy_kubectl",
                    Some("winrm") => "copy_winrm",
                    Some("chroot") => "copy_chroot",
                    _ => "copy_local",
                };
                Ok(Some(invoke_to(
                    call.span,
                    &mangle_name(&format!("std::ops::files::{suffix}")),
                    &call.args,
                    &call.kwargs,
                )))
            }
            CallKind::ShellFileTemplate => {
                let host = call.args.get(2).and_then(string_val).unwrap_or_default();
                let transport = self.host_transport_for(&host);
                let suffix = match transport.as_deref() {
                    Some("ssh") => "template_ssh",
                    Some("chroot") => "template_chroot",
                    _ => "template_local",
                };
                Ok(Some(invoke_to(
                    call.span,
                    &mangle_name(&format!("std::ops::files::{suffix}")),
                    &call.args,
                    &call.kwargs,
                )))
            }
            CallKind::ShellFileRsync => {
                let host = call.args.get(2).and_then(string_val);
                let transport = host
                    .as_deref()
                    .and_then(|host| self.host_transport_for(host));
                let suffix = match transport.as_deref() {
                    Some("chroot") => "rsync_chroot",
                    _ => "rsync_remote",
                };
                Ok(Some(invoke_to(
                    call.span,
                    &mangle_name(&format!("std::ops::files::{suffix}")),
                    &call.args,
                    &call.kwargs,
                )))
            }
            _ => Ok(None),
        }
    }
}

impl IntrinsicMaterializer for ShellMaterializer<'_> {
    fn prepare_file(&self, file: &mut File) {
        ShellMaterializer::prepare_file(self, file);
    }

    fn materialize_invoke_expression(
        &self,
        invoke: ExprInvoke,
        ty: &fp_core::ast::TySlot,
    ) -> Result<MaterializeOutcome<Expr>> {
        Ok(self
            .lower_invoke(&mut invoke.clone(), ty)?
            .map_or(MaterializeOutcome::Unchanged, MaterializeOutcome::Replaced))
    }

    fn materialize_intrinsic_call(
        &self,
        call: ExprIntrinsicCall,
        ty: &fp_core::ast::TySlot,
    ) -> Result<MaterializeOutcome<Expr>> {
        Ok(self
            .lower_intrinsic_call(&mut call.clone(), ty)?
            .map_or(MaterializeOutcome::Unchanged, MaterializeOutcome::Replaced))
    }
}

fn inject_with_contexts_in_item(item: &mut Item, sigs: &HashMap<String, FunctionSignature>) {
    match item.kind_mut() {
        ItemKind::DefFunction(function) => {
            inject_with_contexts_in_block(&mut function.body, sigs, None);
        }
        ItemKind::DefConst(def) => inject_with_contexts(def.value.as_mut(), sigs, None),
        ItemKind::Expr(expr) => inject_with_contexts(expr, sigs, None),
        ItemKind::Module(module) => {
            for child in &mut module.items {
                inject_with_contexts_in_item(child, sigs);
            }
        }
        _ => {}
    }
}

fn inject_with_contexts(
    expr: &mut Expr,
    sigs: &HashMap<String, FunctionSignature>,
    context: Option<&Expr>,
) {
    match expr.kind_mut() {
        ExprKind::With(with) => {
            inject_with_contexts(with.body.as_mut(), sigs, Some(with.context.as_ref()));
            inject_with_contexts(with.context.as_mut(), sigs, context);
        }
        ExprKind::Block(block) => {
            inject_with_contexts_in_block(block, sigs, context);
        }
        ExprKind::If(branch) => {
            inject_with_contexts(branch.cond.as_mut(), sigs, context);
            inject_with_contexts(branch.then.as_mut(), sigs, context);
            if let Some(elze) = branch.elze.as_mut() {
                inject_with_contexts(elze, sigs, context);
            }
        }
        ExprKind::Loop(loop_expr) => {
            inject_with_contexts(loop_expr.body.as_mut(), sigs, context);
        }
        ExprKind::While(while_expr) => {
            inject_with_contexts(while_expr.cond.as_mut(), sigs, context);
            inject_with_contexts(while_expr.body.as_mut(), sigs, context);
        }
        ExprKind::For(for_expr) => {
            inject_with_contexts(for_expr.iter.as_mut(), sigs, context);
            inject_with_contexts(for_expr.body.as_mut(), sigs, context);
        }
        ExprKind::Match(match_expr) => {
            if let Some(scrutinee) = match_expr.scrutinee.as_mut() {
                inject_with_contexts(scrutinee, sigs, context);
            }
            for case in &mut match_expr.cases {
                inject_with_contexts(case.cond.as_mut(), sigs, context);
                if let Some(guard) = case.guard.as_mut() {
                    inject_with_contexts(guard, sigs, context);
                }
                inject_with_contexts(case.body.as_mut(), sigs, context);
            }
        }
        ExprKind::Invoke(invoke) => {
            if let Some(context) = context {
                inject_context_arg(invoke, context, sigs);
            }
            for arg in &mut invoke.args {
                inject_with_contexts(arg, sigs, context);
            }
            for kwarg in &mut invoke.kwargs {
                inject_with_contexts(&mut kwarg.value, sigs, context);
            }
        }
        ExprKind::Assign(assign) => {
            inject_with_contexts(assign.target.as_mut(), sigs, context);
            inject_with_contexts(assign.value.as_mut(), sigs, context);
        }
        ExprKind::Select(select) => inject_with_contexts(select.obj.as_mut(), sigs, context),
        ExprKind::Index(index) => {
            inject_with_contexts(index.obj.as_mut(), sigs, context);
            inject_with_contexts(index.index.as_mut(), sigs, context);
        }
        ExprKind::Struct(struct_expr) => {
            inject_with_contexts(struct_expr.name.as_mut(), sigs, context);
            for field in &mut struct_expr.fields {
                if let Some(value) = field.value.as_mut() {
                    inject_with_contexts(value, sigs, context);
                }
            }
            if let Some(update) = struct_expr.update.as_mut() {
                inject_with_contexts(update, sigs, context);
            }
        }
        ExprKind::Structural(struct_expr) => {
            for field in &mut struct_expr.fields {
                if let Some(value) = field.value.as_mut() {
                    inject_with_contexts(value, sigs, context);
                }
            }
        }
        ExprKind::Cast(cast) => inject_with_contexts(cast.expr.as_mut(), sigs, context),
        ExprKind::Reference(reference) => {
            inject_with_contexts(reference.referee.as_mut(), sigs, context)
        }
        ExprKind::Dereference(deref) => inject_with_contexts(deref.referee.as_mut(), sigs, context),
        ExprKind::Tuple(tuple) => {
            for value in &mut tuple.values {
                inject_with_contexts(value, sigs, context);
            }
        }
        ExprKind::Return(return_expr) => {
            if let Some(value) = return_expr.value.as_mut() {
                inject_with_contexts(value, sigs, context);
            }
        }
        ExprKind::Break(break_expr) => {
            if let Some(value) = break_expr.value.as_mut() {
                inject_with_contexts(value, sigs, context);
            }
        }
        ExprKind::Try(try_expr) => {
            inject_with_contexts(try_expr.expr.as_mut(), sigs, context);
            for catch in &mut try_expr.catches {
                inject_with_contexts(catch.body.as_mut(), sigs, context);
            }
            if let Some(elze) = try_expr.elze.as_mut() {
                inject_with_contexts(elze, sigs, context);
            }
            if let Some(finally) = try_expr.finally.as_mut() {
                inject_with_contexts(finally, sigs, context);
            }
        }
        ExprKind::Async(async_expr) => {
            inject_with_contexts(async_expr.expr.as_mut(), sigs, context)
        }
        ExprKind::Let(let_expr) => inject_with_contexts(let_expr.expr.as_mut(), sigs, context),
        ExprKind::Closure(closure) => inject_with_contexts(closure.body.as_mut(), sigs, context),
        ExprKind::Array(array) => {
            for value in &mut array.values {
                inject_with_contexts(value, sigs, context);
            }
        }
        ExprKind::ArrayRepeat(repeat) => {
            inject_with_contexts(repeat.elem.as_mut(), sigs, context);
            inject_with_contexts(repeat.len.as_mut(), sigs, context);
        }
        ExprKind::ConstBlock(const_block) => {
            inject_with_contexts(const_block.expr.as_mut(), sigs, context);
        }
        ExprKind::Paren(paren) => inject_with_contexts(paren.expr.as_mut(), sigs, context),
        ExprKind::BinOp(binop) => {
            inject_with_contexts(binop.lhs.as_mut(), sigs, context);
            inject_with_contexts(binop.rhs.as_mut(), sigs, context);
        }
        ExprKind::UnOp(unop) => inject_with_contexts(unop.val.as_mut(), sigs, context),
        ExprKind::Range(range) => {
            if let Some(start) = range.start.as_mut() {
                inject_with_contexts(start, sigs, context);
            }
            if let Some(end) = range.end.as_mut() {
                inject_with_contexts(end, sigs, context);
            }
            if let Some(step) = range.step.as_mut() {
                inject_with_contexts(step, sigs, context);
            }
        }
        ExprKind::Splat(splat) => inject_with_contexts(splat.iter.as_mut(), sigs, context),
        ExprKind::SplatDict(splat) => inject_with_contexts(splat.dict.as_mut(), sigs, context),
        _ => {}
    }
}

fn inject_with_contexts_in_block(
    block: &mut ExprBlock,
    sigs: &HashMap<String, FunctionSignature>,
    context: Option<&Expr>,
) {
    for statement in &mut block.stmts {
        match statement {
            BlockStmt::Expr(statement) => {
                inject_with_contexts(statement.expr.as_mut(), sigs, context)
            }
            BlockStmt::Let(statement) => {
                if let Some(init) = statement.init.as_mut() {
                    inject_with_contexts(init, sigs, context);
                }
            }
            BlockStmt::Defer(statement) => {
                inject_with_contexts(statement.expr.as_mut(), sigs, context)
            }
            BlockStmt::Item(item) => inject_with_contexts_in_item(item, sigs),
            BlockStmt::Noop => {}
        }
    }
}

fn inject_context_arg(
    invoke: &mut ExprInvoke,
    context: &Expr,
    sigs: &HashMap<String, FunctionSignature>,
) {
    let name = invoke_target_name(&invoke.target).unwrap_or_default();
    let index = if name.starts_with("__fp_")
        && [
            "_shell_local_",
            "_shell_ssh_",
            "_shell_docker_",
            "_shell_kubectl_",
            "_shell_winrm_",
            "_shell_chroot_",
        ]
        .iter()
        .any(|suffix| name.contains(suffix))
    {
        1
    } else {
        let signature = sigs.get(&name).or_else(|| {
            name.rsplit_once("::")
                .and_then(|(_, function)| sigs.get(function))
        });
        let Some(signature) = signature else {
            return;
        };
        let Some(index) = signature.params.iter().position(|param| param.is_context) else {
            return;
        };
        index
    };
    if invoke
        .kwargs
        .iter()
        .any(|kwarg| kwarg.name == "hosts" || kwarg.name == "target")
        || invoke.args.len() > index
    {
        return;
    }
    invoke.args.insert(index, context.clone());
}

// ── helpers ──

fn invoke_to(
    span: fp_core::span::Span,
    name: &str,
    args: &[Expr],
    kwargs: &[fp_core::ast::ExprKwArg],
) -> Expr {
    Expr::new(ExprKind::Invoke(ExprInvoke {
        span,
        target: ExprInvokeTarget::Function(Name::ident(name)),
        args: args.to_vec(),
        kwargs: kwargs.to_vec(),
    }))
}

fn push_main_body_from_block(block: &ExprBlock, out: &mut Vec<Item>) {
    for stmt in &block.stmts {
        if let BlockStmt::Expr(e) = stmt {
            out.push(Item::from(ItemKind::Expr(e.expr.as_ref().clone())));
        }
    }
}

fn fill_args(invoke: &mut ExprInvoke, sigs: &HashMap<String, FunctionSignature>) {
    let name = invoke_target_name(&invoke.target).unwrap_or_default();
    let Some(sig) = sigs.get(&name) else { return };
    while invoke.args.len() < sig.params.len() {
        let idx = invoke.args.len();
        let param = &sig.params[idx];
        // Try kwarg first
        if let Some(kw) = invoke.kwargs.iter().find(|k| k.name == param.name.as_str()) {
            invoke.args.push(kw.value.clone());
            continue;
        }
        if param.is_context {
            invoke
                .args
                .push(Expr::value(Value::string("localhost".into())));
        } else if let Some(d) = &param.default {
            invoke.args.push(Expr::value(d.clone()));
        } else {
            let val = match &param.ty {
                fp_core::ast::Ty::Primitive(fp_core::ast::TypePrimitive::Bool) => {
                    Value::bool(false)
                }
                fp_core::ast::Ty::Primitive(fp_core::ast::TypePrimitive::Int(_)) => Value::int(0),
                _ => Value::string(String::new()),
            };
            invoke.args.push(Expr::value(val));
        }
    }
}

fn try_rewrite_to_intrinsic(invoke: &mut ExprInvoke) -> Option<Expr> {
    let name = invoke_target_name(&invoke.target)?;
    // Only rewrite unmangled names (the OUTPUT of lower_intrinsic_call is already mangled,
    // so skip __fp_ prefixed names to avoid infinite recursion)
    let kind = match name.as_str() {
        "std::ops::server::shell" | "std::ops::server::shell_local" => CallKind::ShellExec,
        "std::ops::files::copy" | "std::ops::files::copy_local" => CallKind::ShellFileCopy,
        "std::ops::files::template" | "std::ops::files::template_local" => {
            CallKind::ShellFileTemplate
        }
        "std::ops::files::rsync"
        | "std::ops::files::rsync_local"
        | "std::ops::files::rsync_remote" => CallKind::ShellFileRsync,
        _ => {
            if name.starts_with("__fp_") {
                return None; // Already materialized, skip
            }
            return None;
        }
    };
    Some(Expr::new(ExprKind::IntrinsicCall(ExprIntrinsicCall {
        span: invoke.span,
        kind,
        args: std::mem::take(&mut invoke.args),
        kwargs: std::mem::take(&mut invoke.kwargs),
    })))
}

fn string_val(expr: &Expr) -> Option<String> {
    match expr.kind() {
        ExprKind::Value(v) => match v.as_ref() {
            Value::String(s) => Some(s.value.clone()),
            _ => None,
        },
        _ => None,
    }
}

fn invoke_target_name(target: &ExprInvokeTarget) -> Option<String> {
    match target {
        ExprInvokeTarget::Function(name) => name
            .to_path()
            .segments
            .iter()
            .map(|s| s.as_str().to_string())
            .collect::<Vec<_>>()
            .join("::")
            .into(),
        ExprInvokeTarget::Method(select) => {
            let obj = invoke_target_name(&ExprInvokeTarget::Expr(select.obj.clone()))?;
            Some(format!("{}::{}", obj, select.field))
        }
        ExprInvokeTarget::Expr(expr) => match expr.kind() {
            ExprKind::Name(name) => Some(
                name.to_path()
                    .segments
                    .iter()
                    .map(|s| s.as_str().to_string())
                    .collect::<Vec<_>>()
                    .join("::"),
            ),
            _ => None,
        },
        _ => None,
    }
}

fn mangle_name(name: &str) -> String {
    if !name.contains("::") {
        return name.to_string();
    }
    let mut out = String::from("__fp_");
    for seg in name.split("::") {
        if !out.ends_with('_') {
            out.push('_');
        }
        for ch in seg.chars() {
            if ch.is_alphanumeric() || ch == '_' {
                out.push(ch);
            } else {
                out.push('_');
            }
        }
    }
    out.push('_');
    out
}

fn struct_field_from_block(block: &ExprBlock, field: &str) -> Option<Expr> {
    let expr = block.last_expr()?;
    match expr.kind() {
        ExprKind::Struct(s) => s.fields.iter().find_map(|f| {
            if f.name.as_str() == field {
                f.value.clone()
            } else {
                None
            }
        }),
        _ => None,
    }
}

fn scan_all_signatures(file: &File) -> HashMap<String, FunctionSignature> {
    let mut sigs = HashMap::new();
    scan_sigs(&file.items, &[], &mut sigs);
    sigs
}

fn scan_sigs(items: &[Item], path: &[String], out: &mut HashMap<String, FunctionSignature>) {
    for item in items {
        match item.kind() {
            ItemKind::DefFunction(f) => {
                let name = if path.is_empty() {
                    f.name.as_str().to_string()
                } else {
                    format!("{}::{}", path.join("::"), f.name.as_str())
                };
                out.insert(name, f.sig.clone());
            }
            ItemKind::DeclFunction(f) => {
                let name = if path.is_empty() {
                    f.name.as_str().to_string()
                } else {
                    format!("{}::{}", path.join("::"), f.name.as_str())
                };
                out.insert(name, f.sig.clone());
            }
            ItemKind::Module(m) => {
                let mut child = path.to_vec();
                child.push(m.name.as_str().to_string());
                scan_sigs(&m.items, &child, out);
            }
            _ => {}
        }
    }
}

pub fn flatten_keep_externs(items: Vec<Item>) -> Vec<Item> {
    let mut out = Vec::new();
    for item in items {
        match item.kind() {
            ItemKind::Module(m) => out.extend(flatten_keep_externs(m.items.clone())),
            ItemKind::DeclFunction(d) => out.push(Item::from(ItemKind::DeclFunction(d.clone()))),
            ItemKind::Expr(e) => out.push(Item::from(ItemKind::Expr(e.clone()))),
            _ => {}
        }
    }
    out
}
