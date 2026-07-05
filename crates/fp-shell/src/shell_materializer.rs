use fp_core::ast::{BlockStmt, Expr, ExprBlock, ExprIntrinsicCall, ExprInvoke, ExprInvokeTarget, ExprKind, FunctionSignature, Item, ItemDefFunction, ItemKind, Name, NodeKind, Value};
use fp_core::intrinsics::{IntrinsicMaterializer, IntrinsicCallKind};
use fp_core::Result;
use std::collections::HashMap;

pub struct ShellMaterializer<'a> {
    inventory: Option<&'a fp_core::ast::Node>,
}

impl<'a> ShellMaterializer<'a> {
    pub fn new(inventory: Option<&'a fp_core::ast::Node>) -> Self {
        Self { inventory }
    }

    fn host_transport_for(&self, host: &str) -> Option<String> {
        if host == "localhost" {
            return Some("local".into());
        }
        let node = self.inventory?;
        let NodeKind::File(file) = node.kind() else { return None };
        let item = file.items.iter().find_map(|i| match i.kind() {
            ItemKind::DefFunction(f) if f.name.as_str() == "inventory" => Some(f),
            _ => None,
        })?;
        let body = &item.body;
        let hosts_expr = struct_field(body, "hosts")?;
        let map = match hosts_expr.kind() {
            ExprKind::Value(v) => match v.as_ref() {
                Value::Map(map) => Some(map),
                _ => None,
            },
            _ => None,
        }?;
        let entry = map.entries.iter().find(|e| {
            match &e.key {
                Value::String(s) => s.value == host,
                _ => false,
            }
        })?;
        match &entry.value {
            Value::Struct(s) => {
                s.structural.fields.iter().find_map(|f| {
                    if f.name.as_str() == "transport" {
                        match &f.value {
                            Value::String(s) => Some(s.value.clone()),
                            _ => None,
                        }
                    } else {
                        None
                    }
                })
            }
            Value::Map(map) => {
                map.entries.iter().find_map(|e| {
                    match &e.key {
                        Value::String(s) if s.value == "transport" => match &e.value {
                            Value::String(s) => Some(s.value.clone()),
                            _ => None,
                        },
                        _ => None,
                    }
                })
            }
            _ => None,
        }
    }
}

impl IntrinsicMaterializer for ShellMaterializer<'_> {
    fn materialize_call(
        &self,
        call: &mut ExprIntrinsicCall,
        _expr_ty: &fp_core::ast::TySlot,
    ) -> Result<Option<Expr>> {
        match call.kind {
            IntrinsicCallKind::ShellExec => {
                let host = call.args.get(1).and_then(string_value).unwrap_or_default();
                let cmd = call.args.first().cloned().unwrap_or(Expr::unit());
                let transport = self.host_transport_for(&host);
                let suffix = match transport.as_deref() {
                    Some("ssh") => "shell_ssh",
                    Some("docker") => "shell_docker",
                    Some("kubectl") => "shell_kubectl",
                    Some("winrm") => "shell_winrm",
                    Some("chroot") => "shell_chroot",
                    _ => "shell_local",
                };
                Ok(Some(Expr::new(fp_core::ast::ExprKind::Invoke(ExprInvoke {
                    span: call.span,
                    target: ExprInvokeTarget::Function(Name::ident(format!(
                        "std::ops::server::{suffix}"
                    ))),
                    args: call.args.clone(),
                    kwargs: call.kwargs.clone(),
                }))))
            }
            IntrinsicCallKind::ShellFileCopy => {
                let host = call.args.get(2).and_then(string_value).unwrap_or_default();
                let transport = self.host_transport_for(&host);
                let suffix = match transport.as_deref() {
                    Some("ssh") => "copy_ssh",
                    Some("docker") => "copy_docker",
                    Some("kubectl") => "copy_kubectl",
                    Some("winrm") => "copy_winrm",
                    Some("chroot") => "copy_chroot",
                    _ => "copy_local",
                };
                Ok(Some(Expr::new(fp_core::ast::ExprKind::Invoke(ExprInvoke {
                    span: call.span,
                    target: ExprInvokeTarget::Function(Name::ident(format!(
                        "std::ops::files::{suffix}"
                    ))),
                    args: call.args.clone(),
                    kwargs: call.kwargs.clone(),
                }))))
            }
            IntrinsicCallKind::ShellFileTemplate => {
                let host = call.args.get(2).and_then(string_value).unwrap_or_default();
                let transport = self.host_transport_for(&host);
                let suffix = match transport.as_deref() {
                    Some("ssh") => "template_ssh",
                    Some("chroot") => "template_chroot",
                    _ => "template_local",
                };
                Ok(Some(Expr::new(fp_core::ast::ExprKind::Invoke(ExprInvoke {
                    span: call.span,
                    target: ExprInvokeTarget::Function(Name::ident(format!(
                        "std::ops::files::{suffix}"
                    ))),
                    args: call.args.clone(),
                    kwargs: call.kwargs.clone(),
                }))))
            }
            IntrinsicCallKind::ShellFileRsync => {
                let host = call.args.get(2).and_then(string_value).unwrap_or_default();
                let transport = self.host_transport_for(&host);
                let suffix = match transport.as_deref() {
                    Some("chroot") => "rsync_chroot",
                    _ => "rsync_remote",
                };
                Ok(Some(Expr::new(fp_core::ast::ExprKind::Invoke(ExprInvoke {
                    span: call.span,
                    target: ExprInvokeTarget::Function(Name::ident(format!(
                        "std::ops::files::{suffix}"
                    ))),
                    args: call.args.clone(),
                    kwargs: call.kwargs.clone(),
                }))))
            }
            _ => Ok(None),
        }
    }
}

fn fill_missing_args(invoke: &mut ExprInvoke, sigs: &HashMap<String, FunctionSignature>) {
    let name = invoke_target_name(&invoke.target).unwrap_or_default();
    let Some(sig) = sigs.get(&name) else { return };
    while invoke.args.len() < sig.params.len() {
        let idx = invoke.args.len();
        let param = &sig.params[idx];
        if let Some(kw) = invoke.kwargs.iter().find(|k| k.name == param.name.as_str()) {
            invoke.args.push(kw.value.clone());
            continue;
        }
        if param.is_context {
            invoke.args.push(Expr::value(Value::string("localhost".into())));
        } else if let Some(d) = &param.default {
            invoke.args.push(Expr::value(d.clone()));
        } else {
            let val = match &param.ty {
                fp_core::ast::Ty::Primitive(fp_core::ast::TypePrimitive::Bool) => Value::bool(false),
                fp_core::ast::Ty::Primitive(fp_core::ast::TypePrimitive::Int(_)) => Value::int(0),
                _ => Value::string(String::new()),
            };
            invoke.args.push(Expr::value(val));
        }
    }
}

fn scan_signatures(node: &fp_core::ast::Node) -> HashMap<String, FunctionSignature> {
    let mut sigs = HashMap::new();
    if let NodeKind::File(file) = node.kind() {
        scan_sigs(&file.items, &[], &mut sigs);
    }
    sigs
}

fn scan_sigs(items: &[Item], path: &[String], out: &mut HashMap<String, FunctionSignature>) {
    for item in items {
        match item.kind() {
            ItemKind::DefFunction(f) => {
                let name = if path.is_empty() { f.name.as_str().to_string() } else { format!("{}::{}", path.join("::"), f.name.as_str()) };
                out.insert(name, f.sig.clone());
            }
            ItemKind::DeclFunction(f) => {
                let name = if path.is_empty() { f.name.as_str().to_string() } else { format!("{}::{}", path.join("::"), f.name.as_str()) };
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


fn string_value(expr: &Expr) -> Option<String> {
    match expr.kind() {
        ExprKind::Value(v) => match v.as_ref() {
            Value::String(s) => Some(s.value.clone()),
            _ => None,
        },
        _ => None,
    }
}

fn struct_field(expr: &Expr, field: &str) -> Option<Expr> {
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

fn invoke_target_name(target: &ExprInvokeTarget) -> Option<String> {
    match target {
        ExprInvokeTarget::Function(name) => {
            let segs: Vec<String> = name.to_path().segments.iter().map(|s| s.as_str().to_string()).collect();
            Some(segs.join("::"))
        }
        _ => None,
    }
}

fn try_rewrite_invoke_to_intrinsic(invoke: &mut ExprInvoke) -> Option<Expr> {
    let name = invoke_target_name(&invoke.target)?;
    let kind = match name.as_str() {
        "std::ops::server::shell" | "__fp_std_ops_server_shell_" => IntrinsicCallKind::ShellExec,
        "std::ops::files::copy" => IntrinsicCallKind::ShellFileCopy,
        "std::ops::files::template" => IntrinsicCallKind::ShellFileTemplate,
        "std::ops::files::rsync" => IntrinsicCallKind::ShellFileRsync,
        _ => return None,
    };

    // Fill missing args with defaults. Context params default to "localhost"
    let count = match kind {
        IntrinsicCallKind::ShellExec => 10,
        IntrinsicCallKind::ShellFileCopy => 10,
        IntrinsicCallKind::ShellFileTemplate => 8,
        IntrinsicCallKind::ShellFileRsync => 9,
        _ => invoke.args.len(),
    };
    while invoke.args.len() < count {
        let idx = invoke.args.len();
        // Second arg is the context host param
        let val = if idx == 1 {
            Expr::value(Value::string("localhost".into()))
        } else if idx == 8 {
            Expr::value(Value::bool(false)) // sudo default
        } else {
            Expr::value(Value::string(String::new())) // empty string default
        };
        invoke.args.push(val);
    }

    Some(Expr::new(ExprKind::IntrinsicCall(ExprIntrinsicCall {
        span: invoke.span,
        kind,
        args: std::mem::take(&mut invoke.args),
        kwargs: std::mem::take(&mut invoke.kwargs),
    })))
}

pub fn materialize_ast(node: &mut fp_core::ast::Node, materializer: &dyn IntrinsicMaterializer) -> Result<()> {
    let sigs = scan_signatures(node);
    flatten_main_body(node);
    let NodeKind::File(file) = node.kind_mut() else {
        return Ok(());
    };
    materialize_items(&mut file.items, materializer, &sigs)
}

fn materialize_items(items: &mut [fp_core::ast::Item], materializer: &dyn IntrinsicMaterializer, sigs: &HashMap<String, FunctionSignature>) -> Result<()> {
    for item in items {
        match item.kind_mut() {
            ItemKind::Module(module) => materialize_items(&mut module.items, materializer, sigs)?,
            ItemKind::DefFunction(func) => materialize_expr(&mut func.body, materializer, sigs)?,
            ItemKind::Expr(expr) => materialize_expr(expr, materializer, sigs)?,
            _ => {}
        }
    }
    Ok(())
}

fn materialize_expr(expr: &mut Expr, materializer: &dyn IntrinsicMaterializer, sigs: &HashMap<String, FunctionSignature>) -> Result<()> {
    // Expand with blocks: replace `with host { ... }` with the body directly
    // (the context is handled by the callee via host resolution)
    if let ExprKind::With(w) = expr.kind() {
        let body = w.body.as_ref().clone();
        *expr = body;
        return materialize_expr(expr, materializer, sigs);
    }

    match expr.kind_mut() {
        ExprKind::IntrinsicCall(call) => {
            if let Some(replacement) = materializer.materialize_call(call, &fp_core::ast::TySlot::None)? {
                *expr = replacement;
            }
        }
        ExprKind::Block(block) => {
            for stmt in &mut block.stmts {
                match stmt {
                    fp_core::ast::BlockStmt::Expr(e) => materialize_expr(&mut e.expr, materializer, sigs)?,
                    fp_core::ast::BlockStmt::Let(s) => {
                        if let Some(init) = &mut s.init {
                            materialize_expr(init, materializer, sigs)?;
                        }
                    }
                    fp_core::ast::BlockStmt::Defer(d) => materialize_expr(&mut d.expr, materializer, sigs)?,
                    fp_core::ast::BlockStmt::Item(i) => materialize_expr_in_item(i, materializer, sigs)?,
                    _ => {}
                }
            }
        }
        ExprKind::If(e) => {
            materialize_expr(&mut e.cond, materializer, sigs)?;
            materialize_expr(&mut e.then, materializer, sigs)?;
            if let Some(elze) = &mut e.elze {
                materialize_expr(elze, materializer, sigs)?;
            }
        }
        ExprKind::Invoke(invoke) => {
            for arg in &mut invoke.args {
                materialize_expr(arg, materializer, sigs)?;
            }
            for kwarg in &mut invoke.kwargs {
                materialize_expr(&mut kwarg.value, materializer, sigs)?;
            }
            // Rewrite known shell calls to intrinsic calls
            fill_missing_args(invoke, sigs);
            if let Some(call) = try_rewrite_invoke_to_intrinsic(invoke) {
                *expr = call;
                return materialize_expr(expr, materializer, sigs);
            }
            if let Some(replacement) = materializer.materialize_invoke(invoke, &fp_core::ast::TySlot::None)? {
                *expr = replacement;
            }
        }
        ExprKind::Try(e) => {
            materialize_expr(&mut e.expr, materializer, sigs)?;
            for c in &mut e.catches {
                materialize_expr(&mut c.body, materializer, sigs)?;
            }
            if let Some(elze) = &mut e.elze {
                materialize_expr(elze, materializer, sigs)?;
            }
            if let Some(finally) = &mut e.finally {
                materialize_expr(finally, materializer, sigs)?;
            }
        }
        ExprKind::While(w) => {
            materialize_expr(&mut w.cond, materializer, sigs)?;
            materialize_expr(&mut w.body, materializer, sigs)?;
        }
        ExprKind::For(f) => {
            materialize_expr(&mut f.body, materializer, sigs)?;
        }
        ExprKind::Match(m) => {
            for case in &mut m.cases {
                materialize_expr(&mut case.body, materializer, sigs)?;
                if let Some(guard) = &mut case.guard {
                    materialize_expr(guard, materializer, sigs)?;
                }
            }
        }
        ExprKind::With(w) => {
            materialize_expr(&mut w.body, materializer, sigs)?;
        }
        ExprKind::Let(l) => {
            materialize_expr(&mut l.expr, materializer, sigs)?;
        }
        _ => {}
    }
    Ok(())
}

fn materialize_expr_in_item(item: &mut fp_core::ast::Item, materializer: &dyn IntrinsicMaterializer, sigs: &HashMap<String, FunctionSignature>) -> Result<()> {
    match item.kind_mut() {
        ItemKind::DefFunction(func) => materialize_expr(&mut func.body, materializer, sigs)?,
        ItemKind::Module(module) => materialize_items(&mut module.items, materializer, sigs)?,
        _ => {}
    }
    Ok(())
}

fn flatten_main_body(node: &mut fp_core::ast::Node) {
    let NodeKind::File(file) = node.kind_mut() else { return };
    let mut main_body: Option<fp_core::ast::Expr> = None;
    file.items.retain(|item| {
        match item.kind() {
            ItemKind::DefFunction(f) if f.name.as_str() == "main" => {
                main_body = Some(f.body.as_ref().clone());
                false // remove main from items
            }
            ItemKind::DefConst(c) if c.name.as_str() == "main" => {
                main_body = Some((*c.value).clone());
                false
            }
            _ => true
        }
    });
    if let Some(body) = main_body {
        if let ExprKind::Block(block) = body.kind() {
            for stmt in &block.stmts {
                match stmt {
                    BlockStmt::Expr(e) => {
                        file.items.push(Item::from(ItemKind::Expr(e.expr.as_ref().clone())));
                    }
                    _ => {}
                }
            }
        }
    }
}
