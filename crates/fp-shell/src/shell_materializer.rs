use fp_core::ast::{
    BlockStmt, Expr, ExprIntrinsicCall, ExprInvoke, ExprInvokeTarget, ExprKind,
    File, FunctionSignature, Item, ItemKind, Name, NodeKind, Value,
};
use fp_core::intrinsics::{IntrinsicCallKind, IntrinsicMaterializer};
use fp_core::Result;
use std::cell::RefCell;
use std::collections::HashMap;

pub struct ShellMaterializer<'a> {
    inventory: Option<&'a fp_core::ast::Node>,
    sigs: RefCell<Option<HashMap<String, FunctionSignature>>>,
}

impl<'a> ShellMaterializer<'a> {
    pub fn new(inventory: Option<&'a fp_core::ast::Node>) -> Self {
        Self {
            inventory,
            sigs: RefCell::new(None),
        }
    }

    fn host_transport_for(&self, host: &str) -> Option<String> {
        if host == "localhost" {
            return Some("local".into());
        }
        let node = self.inventory?;
        let NodeKind::File(file) = node.kind() else {
            return None;
        };
        let item = file.items.iter().find_map(|i| match i.kind() {
            ItemKind::DefFunction(f) if f.name.as_str() == "inventory" => Some(f),
            _ => None,
        })?;
        let hosts_expr = struct_field(&item.body, "hosts")?;
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

impl IntrinsicMaterializer for ShellMaterializer<'_> {
    fn prepare_file(&self, file: &mut File) {
        // Scan signatures from the AST
        *self.sigs.borrow_mut() = Some(scan_all_signatures(file));

        // Flatten main body to top-level Expr items
        let mut new_items = Vec::new();
        let mut i = 0;
        while i < file.items.len() {
            match file.items[i].kind() {
                ItemKind::DefFunction(f) if f.name.as_str() == "main" => {
                    push_main_body(&f.body, &mut new_items);
                }
                ItemKind::DefConst(c) if c.name.as_str() == "main" => {
                    push_main_body(&c.value, &mut new_items);
                }
                _ => new_items.push(file.items[i].clone()),
            }
            i += 1;
        }
        file.items = new_items;
    }

    fn materialize_invoke(
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
            if let ExprKind::IntrinsicCall(mut call) = expr.into_parts().1 {
                // Convert intrinsic call to final mangled invoke
                return self.materialize_call(&mut call, &None);
            }
        }

        // Normalize invoke target to Function form with an Ident name
        // (fp-bash only handles identifier targets, not paths)
        let name = invoke_target_name(&invoke.target).unwrap_or_default();
        let mangled = mangle_name(&name);
        invoke.target = ExprInvokeTarget::Function(Name::ident(mangled));

        Ok(None)
    }

    fn materialize_call(
        &self,
        call: &mut ExprIntrinsicCall,
        _expr_ty: &fp_core::ast::TySlot,
    ) -> Result<Option<Expr>> {
        match call.kind {
            IntrinsicCallKind::ShellExec => {
                let host = call.args.get(1).and_then(string_val).unwrap_or_default();
                let transport = self.host_transport_for(&host);
                let suffix = match transport.as_deref() {
                    Some("ssh") => "shell_ssh",
                    Some("docker") => "shell_docker",
                    Some("kubectl") => "shell_kubectl",
                    Some("winrm") => "shell_winrm",
                    Some("chroot") => "shell_chroot",
                    _ => "shell_local",
                };
                Ok(Some(invoke_to(
                    call.span,
                    &mangle_name(&format!("std::ops::server::{suffix}")),
                    &call.args,
                    &call.kwargs,
                )))
            }
            IntrinsicCallKind::ShellFileCopy => {
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
            IntrinsicCallKind::ShellFileTemplate => {
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
            IntrinsicCallKind::ShellFileRsync => {
                let host = call.args.get(2).and_then(string_val).unwrap_or_default();
                let transport = self.host_transport_for(&host);
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

fn push_main_body(body: &Expr, out: &mut Vec<Item>) {
    if let ExprKind::Block(block) = body.kind() {
        for stmt in &block.stmts {
            if let BlockStmt::Expr(e) = stmt {
                out.push(Item::from(ItemKind::Expr(e.expr.as_ref().clone())));
            }
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
        if let Some(kw) = invoke
            .kwargs
            .iter()
            .find(|k| k.name == param.name.as_str())
        {
            invoke.args.push(kw.value.clone());
            continue;
        }
        if param.is_context {
            invoke.args.push(Expr::value(Value::string("localhost".into())));
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
    // Only rewrite unmangled names (the OUTPUT of materialize_call is already mangled,
    // so skip __fp_ prefixed names to avoid infinite recursion)
    let kind = match name.as_str() {
        "std::ops::server::shell" | "std::ops::server::shell_local" => IntrinsicCallKind::ShellExec,
        "std::ops::files::copy" | "std::ops::files::copy_local" => IntrinsicCallKind::ShellFileCopy,
        "std::ops::files::template" | "std::ops::files::template_local" => IntrinsicCallKind::ShellFileTemplate,
        "std::ops::files::rsync" | "std::ops::files::rsync_local" | "std::ops::files::rsync_remote" => IntrinsicCallKind::ShellFileRsync,
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
        ExprInvokeTarget::Function(name) => {
            name.to_path()
                .segments
                .iter()
                .map(|s| s.as_str().to_string())
                .collect::<Vec<_>>()
                .join("::")
                .into()
        }
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

fn scan_all_signatures(file: &File) -> HashMap<String, FunctionSignature> {
    let mut sigs = HashMap::new();
    scan_sigs(&file.items, &[], &mut sigs);
    sigs
}

fn scan_sigs(
    items: &[Item],
    path: &[String],
    out: &mut HashMap<String, FunctionSignature>,
) {
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
