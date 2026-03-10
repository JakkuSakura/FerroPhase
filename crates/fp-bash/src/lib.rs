use fp_core::ast::{
    AstTarget, AstTargetOutput, BlockStmt, Expr, ExprBlock, ExprFor, ExprIf, ExprIntrinsicCall,
    ExprInvoke, ExprInvokeTarget, ExprKind, ExprStringTemplate, FormatArgRef, FormatTemplatePart,
    Item, ItemKind, Name, Node, NodeKind, PatternKind, Value,
};
use fp_core::intrinsics::IntrinsicCallKind;
use fp_core::ops::{BinOpKind, UnOpKind};
use std::collections::HashMap;
use std::collections::HashSet;

#[derive(Debug, Clone, Default)]
pub struct ShellInventory {
    pub groups: HashMap<String, Vec<String>>,
    pub hosts: HashMap<String, InventoryHost>,
}

#[derive(Debug, Clone, Default)]
pub struct InventoryHost {
    pub address: Option<String>,
    pub user: Option<String>,
    pub port: Option<u16>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum HostSelector {
    Localhost,
    Remote(String),
}

#[derive(Debug, Clone)]
enum VarValue {
    String(String),
    Int(i64),
    Bool(bool),
    StringList(Vec<String>),
}

#[derive(Debug, Clone, Default)]
struct EmitContext {
    hosts: Option<Vec<HostSelector>>,
}

pub struct BashTarget;

impl BashTarget {
    pub fn new() -> Self {
        Self
    }

    pub fn with_inventory(inventory: ShellInventory) -> BashTargetWithInventory {
        BashTargetWithInventory { inventory }
    }
}

pub struct BashTargetWithInventory {
    inventory: ShellInventory,
}

impl Default for BashTarget {
    fn default() -> Self {
        Self::new()
    }
}

impl AstTarget for BashTarget {
    fn emit_node(&self, node: &Node) -> Result<AstTargetOutput, fp_core::Error> {
        let mut renderer = BashRenderer::new();
        renderer.emit_node(node, &EmitContext::default(), 0);
        if let Some(first_error) = renderer.errors.first() {
            return Err(fp_core::Error::from(first_error.clone()));
        }
        Ok(AstTargetOutput {
            code: renderer.finish(),
            side_files: Vec::new(),
        })
    }
}

impl AstTarget for BashTargetWithInventory {
    fn emit_node(&self, node: &Node) -> Result<AstTargetOutput, fp_core::Error> {
        let mut renderer = BashRenderer::with_inventory(self.inventory.clone());
        renderer.emit_node(node, &EmitContext::default(), 0);
        if let Some(first_error) = renderer.errors.first() {
            return Err(fp_core::Error::from(first_error.clone()));
        }
        Ok(AstTargetOutput {
            code: renderer.finish(),
            side_files: Vec::new(),
        })
    }
}

struct BashRenderer {
    lines: Vec<String>,
    errors: Vec<String>,
    scopes: Vec<HashMap<String, VarValue>>,
    pending_remote: Option<PendingRemote>,
    known_functions: HashSet<String>,
    inventory: ShellInventory,
}

#[derive(Debug, Clone, Default)]
struct OperationMeta {
    only_if: Option<String>,
    unless: Option<String>,
    creates: Option<String>,
    removes: Option<String>,
}

#[derive(Debug, Clone)]
struct PendingRemote {
    indent: usize,
    host: String,
    commands: Vec<String>,
}

#[derive(Debug, Clone, Copy)]
struct RsyncOptions {
    archive: bool,
    compress: bool,
    delete: bool,
    checksum: bool,
}

impl RsyncOptions {
    fn to_flags(self) -> String {
        let mut short = String::new();
        if self.archive {
            short.push('a');
        }
        if self.compress {
            short.push('z');
        }

        let mut flags = if short.is_empty() {
            "-r".to_string()
        } else {
            format!("-{}", short)
        };

        if self.delete {
            flags.push_str(" --delete");
        }
        if self.checksum {
            flags.push_str(" --checksum");
        }
        flags
    }
}

impl BashRenderer {
    fn new() -> Self {
        Self::with_inventory(ShellInventory::default())
    }

    fn with_inventory(inventory: ShellInventory) -> Self {
        Self {
            lines: Vec::new(),
            errors: Vec::new(),
            scopes: vec![HashMap::new()],
            pending_remote: None,
            known_functions: HashSet::new(),
            inventory,
        }
    }

    fn finish(mut self) -> String {
        self.flush_pending_remote();

        let mut script = String::new();
        script.push_str("#!/usr/bin/env bash\n");
        script.push_str("set -euo pipefail\n\n");
        script.push_str("__fp_last_changed=0\n\n");
        script.push_str("SSH_CONTROL_PATH=\"${TMPDIR:-/tmp}/fp-shell-%r@%h:%p\"\n\n");
        script.push_str("ssh_cmd() {\n");
        script.push_str("  ssh -o ControlMaster=auto -o ControlPersist=60 -o ControlPath=\"$SSH_CONTROL_PATH\" -- \"$@\"\n");
        script.push_str("}\n\n");
        script.push_str("scp_cmd() {\n");
        script.push_str("  scp -o ControlMaster=auto -o ControlPersist=60 -o ControlPath=\"$SSH_CONTROL_PATH\" -- \"$@\"\n");
        script.push_str("}\n\n");
        script.push_str("rsync_cmd() {\n");
        script.push_str("  rsync -e \"ssh -o ControlMaster=auto -o ControlPersist=60 -o ControlPath=$SSH_CONTROL_PATH\" \"$@\"\n");
        script.push_str("}\n\n");
        script.push_str("run_remote() {\n");
        script.push_str("  local host=\"$1\"\n");
        script.push_str("  local cmd=\"$2\"\n");
        script.push_str("  ssh_cmd \"$host\" \"$cmd\"\n");
        script.push_str("}\n\n");

        for line in self.lines {
            script.push_str(&line);
            script.push('\n');
        }

        script
    }

    fn emit_node(&mut self, node: &Node, context: &EmitContext, indent: usize) {
        match node.kind() {
            NodeKind::File(file) => {
                for item in &file.items {
                    self.discover_functions(item);
                }
                for item in &file.items {
                    self.emit_item(item, context, indent);
                }
            }
            NodeKind::Item(item) => self.emit_item(item, context, indent),
            NodeKind::Expr(expr) => self.emit_expr(expr, context, indent),
            NodeKind::Query(_) | NodeKind::Schema(_) | NodeKind::Workspace(_) => {}
        }
    }

    fn emit_item(&mut self, item: &Item, context: &EmitContext, indent: usize) {
        match item.kind() {
            ItemKind::DefFunction(function) => {
                let name = function.name.as_str();
                if name == "main" {
                    self.emit_expr(&function.body, context, indent);
                } else {
                    self.emit_function(function, context, indent);
                }
            }
            ItemKind::DefConst(def) => self.emit_expr(&def.value, context, indent),
            ItemKind::DefStatic(def) => self.emit_expr(&def.value, context, indent),
            ItemKind::Module(module) => {
                for child in &module.items {
                    self.emit_item(child, context, indent);
                }
            }
            ItemKind::Expr(expr) => self.emit_expr(expr, context, indent),
            _ => {}
        }
    }

    fn emit_expr(&mut self, expr: &Expr, context: &EmitContext, indent: usize) {
        match expr.kind() {
            ExprKind::Block(block) => self.emit_block(block, context, indent),
            ExprKind::Invoke(invoke) => {
                if self.emit_shell_invoke(invoke, context, indent) {
                    return;
                }
                for arg in &invoke.args {
                    self.emit_expr(arg, context, indent);
                }
                for kwarg in &invoke.kwargs {
                    self.emit_expr(&kwarg.value, context, indent);
                }
            }
            ExprKind::If(expr_if) => self.emit_if(expr_if, context, indent),
            ExprKind::While(expr_while) => {
                let condition = self.render_condition(&expr_while.cond);
                self.push_line(indent, &format!("while {}; do", condition));
                self.emit_expr(&expr_while.body, context, indent + 1);
                self.push_line(indent, "done");
            }
            ExprKind::Loop(expr_loop) => {
                self.push_line(indent, "while true; do");
                self.emit_expr(&expr_loop.body, context, indent + 1);
                self.push_line(indent, "done");
            }
            ExprKind::For(expr_for) => self.emit_for(expr_for, context, indent),
            ExprKind::Return(ret) => {
                if let Some(value) = &ret.value {
                    self.emit_expr(value, context, indent);
                }
            }
            ExprKind::Assign(assign) => {
                self.emit_expr(&assign.value, context, indent);
            }
            ExprKind::Paren(paren) => self.emit_expr(&paren.expr, context, indent),
            ExprKind::Array(array) => {
                for element in &array.values {
                    self.emit_expr(element, context, indent);
                }
            }
            ExprKind::Tuple(tuple) => {
                for element in &tuple.values {
                    self.emit_expr(element, context, indent);
                }
            }
            ExprKind::Match(expr_match) => {
                if let Some(scrutinee) = &expr_match.scrutinee {
                    self.emit_expr(scrutinee, context, indent);
                }
                for case in &expr_match.cases {
                    self.emit_expr(&case.body, context, indent);
                }
            }
            _ => {}
        }
    }

    fn emit_function(
        &mut self,
        function: &fp_core::ast::ItemDefFunction,
        context: &EmitContext,
        indent: usize,
    ) {
        self.push_line(indent, &format!("{}() {{", function.name.as_str()));
        self.push_scope();
        for (index, param) in function.sig.params.iter().enumerate() {
            let arg_index = index + 1;
            self.push_line(
                indent + 1,
                &format!("local {}=\"${}\"", param.name.as_str(), arg_index),
            );
            self.bind_var(
                param.name.as_str().to_string(),
                VarValue::String(format!("${}", arg_index)),
            );
        }
        self.emit_expr(&function.body, context, indent + 1);
        self.pop_scope();
        self.push_line(indent, "}");
    }

    fn discover_functions(&mut self, item: &Item) {
        match item.kind() {
            ItemKind::DefFunction(function) => {
                self.known_functions
                    .insert(function.name.as_str().to_string());
            }
            ItemKind::Module(module) => {
                for child in &module.items {
                    self.discover_functions(child);
                }
            }
            _ => {}
        }
    }

    fn emit_block(&mut self, block: &ExprBlock, context: &EmitContext, indent: usize) {
        self.push_scope();
        for statement in &block.stmts {
            match statement {
                BlockStmt::Expr(expr_stmt) => {
                    self.emit_expr(expr_stmt.expr.as_ref(), context, indent)
                }
                BlockStmt::Let(stmt_let) => {
                    if let Some(init) = &stmt_let.init {
                        if let Some(ident) = stmt_let.pat.as_ident() {
                            if let Some(value) = self.eval_var_value(init) {
                                self.bind_var(ident.as_str().to_string(), value);
                                continue;
                            }
                        }
                        self.emit_expr(init, context, indent);
                    }
                    if let Some(diverge) = &stmt_let.diverge {
                        self.emit_expr(diverge, context, indent);
                    }
                }
                BlockStmt::Item(item) => self.emit_item(item, context, indent),
                BlockStmt::Noop | BlockStmt::Any(_) => {}
            }
        }
        self.pop_scope();
    }

    fn emit_if(&mut self, expr_if: &ExprIf, context: &EmitContext, indent: usize) {
        let condition = self.render_condition(&expr_if.cond);
        self.push_line(indent, &format!("if {}; then", condition));
        self.emit_expr(&expr_if.then, context, indent + 1);
        if let Some(elze) = &expr_if.elze {
            self.push_line(indent, "else");
            self.emit_expr(elze, context, indent + 1);
        }
        self.push_line(indent, "fi");
    }

    fn emit_for(&mut self, expr_for: &ExprFor, context: &EmitContext, indent: usize) {
        let PatternKind::Ident(pattern_ident) = expr_for.pat.kind() else {
            self.push_line(indent, "# unsupported for-pattern in bash emission");
            return;
        };
        let Some(values) = self.resolve_string_list_expr(&expr_for.iter) else {
            self.push_line(indent, "# unsupported for-iterator in bash emission");
            return;
        };
        let joined = values
            .iter()
            .map(|value| shell_quote(value))
            .collect::<Vec<_>>()
            .join(" ");
        self.push_line(
            indent,
            &format!("for {} in {}; do", pattern_ident.ident.as_str(), joined),
        );
        self.push_scope();
        self.bind_var(
            pattern_ident.ident.as_str().to_string(),
            VarValue::String(format!("${}", pattern_ident.ident.as_str())),
        );
        self.emit_expr(&expr_for.body, context, indent + 1);
        self.pop_scope();
        self.push_line(indent, "done");
    }

    fn emit_shell_invoke(
        &mut self,
        invoke: &ExprInvoke,
        context: &EmitContext,
        indent: usize,
    ) -> bool {
        let Some(path) = invoke_target_segments(&invoke.target) else {
            return false;
        };

        if path == ["std", "host", "on"] || path == ["std", "shell", "on"] {
            return self.emit_host_scope(invoke, indent);
        }

        if path == ["std", "server", "shell"] || path == ["std", "shell", "run"] {
            let command_expr = invoke
                .args
                .first()
                .or_else(|| kwarg_value(invoke, &["command", "cmd"]));
            let Some(command_expr) = command_expr else {
                return false;
            };
            let Some(raw_command) = self.resolve_string_expr(command_expr) else {
                self.errors.push(
                    "std::server::shell command must resolve to string/int/bool (including f\"...\"/format!(...))"
                        .to_string(),
                );
                return false;
            };
            let sudo = kwarg_value(invoke, &["sudo"])
                .and_then(|expr| self.resolve_bool_expr(expr))
                .unwrap_or(false);
            let cwd = kwarg_value(invoke, &["cwd"]).and_then(|expr| self.resolve_string_expr(expr));
            let command = apply_shell_options(raw_command, cwd.as_deref(), sudo);
            let hosts = self.resolve_hosts(invoke, context);
            let meta = self.operation_meta(invoke);
            self.emit_run_command(&hosts, &command, &meta, indent);
            return true;
        }

        if path == ["std", "files", "copy"] || path == ["std", "shell", "copy_file"] {
            let source_expr = invoke
                .args
                .first()
                .or_else(|| kwarg_value(invoke, &["src", "source"]));
            let destination_expr = invoke
                .args
                .get(1)
                .or_else(|| kwarg_value(invoke, &["dest", "destination"]));
            let (Some(source_expr), Some(destination_expr)) = (source_expr, destination_expr)
            else {
                return false;
            };
            let Some(source) = self.resolve_string_expr(source_expr) else {
                return false;
            };
            let Some(destination) = self.resolve_string_expr(destination_expr) else {
                return false;
            };
            let hosts = self.resolve_hosts(invoke, context);
            let meta = self.operation_meta(invoke);
            self.emit_copy_command(&hosts, &source, &destination, &meta, indent);
            return true;
        }

        if path == ["std", "files", "template"] {
            let source_expr = invoke
                .args
                .first()
                .or_else(|| kwarg_value(invoke, &["src", "source"]));
            let destination_expr = invoke
                .args
                .get(1)
                .or_else(|| kwarg_value(invoke, &["dest", "destination"]));
            let (Some(source_expr), Some(destination_expr)) = (source_expr, destination_expr)
            else {
                return false;
            };
            let Some(source) = self.resolve_string_expr(source_expr) else {
                return false;
            };
            let Some(destination) = self.resolve_string_expr(destination_expr) else {
                return false;
            };
            let vars = kwarg_value(invoke, &["vars"]).and_then(|expr| self.resolve_vars_map(expr));
            let hosts = self.resolve_hosts(invoke, context);
            let meta = self.operation_meta(invoke);
            self.emit_template_command(&hosts, &source, &destination, vars, &meta, indent);
            return true;
        }

        if path == ["std", "files", "rsync"] || path == ["std", "shell", "rsync"] {
            let source_expr = invoke
                .args
                .first()
                .or_else(|| kwarg_value(invoke, &["src", "source"]));
            let destination_expr = invoke
                .args
                .get(1)
                .or_else(|| kwarg_value(invoke, &["dest", "destination"]));
            let (Some(source_expr), Some(destination_expr)) = (source_expr, destination_expr)
            else {
                return false;
            };
            let Some(source) = self.resolve_string_expr(source_expr) else {
                return false;
            };
            let Some(destination) = self.resolve_string_expr(destination_expr) else {
                return false;
            };

            let options = RsyncOptions {
                archive: kwarg_value(invoke, &["archive"])
                    .and_then(|expr| self.resolve_bool_expr(expr))
                    .unwrap_or(true),
                compress: kwarg_value(invoke, &["compress"])
                    .and_then(|expr| self.resolve_bool_expr(expr))
                    .unwrap_or(true),
                delete: kwarg_value(invoke, &["delete"])
                    .and_then(|expr| self.resolve_bool_expr(expr))
                    .unwrap_or(false),
                checksum: kwarg_value(invoke, &["checksum"])
                    .and_then(|expr| self.resolve_bool_expr(expr))
                    .unwrap_or(false),
            };

            let hosts = self.resolve_hosts(invoke, context);
            let meta = self.operation_meta(invoke);
            self.emit_rsync_command(&hosts, &source, &destination, options, &meta, indent);
            return true;
        }

        if path == ["std", "service", "restart"] {
            let service_expr = invoke
                .args
                .first()
                .or_else(|| kwarg_value(invoke, &["name", "service"]));
            let Some(service_expr) = service_expr else {
                return false;
            };
            let Some(service_name) = self.resolve_string_expr(service_expr) else {
                return false;
            };
            let sudo = kwarg_value(invoke, &["sudo"])
                .and_then(|expr| self.resolve_bool_expr(expr))
                .unwrap_or(true);
            let command = if sudo {
                format!("sudo systemctl restart {}", service_name)
            } else {
                format!("systemctl restart {}", service_name)
            };
            let hosts = self.resolve_hosts(invoke, context);
            let meta = self.operation_meta(invoke);
            self.emit_run_command(&hosts, &command, &meta, indent);
            return true;
        }

        if path.len() == 1 {
            let callee = &path[0];
            if self.known_functions.contains(callee) {
                let mut rendered_args = Vec::new();
                for arg in &invoke.args {
                    if let Some(value) = self.resolve_string_expr(arg) {
                        rendered_args.push(shell_arg_quote(&value));
                    } else {
                        self.errors.push(format!(
                            "function '{}' only supports string/int/bool arguments in fp-bash emitter",
                            callee
                        ));
                        return false;
                    }
                }
                let suffix = if rendered_args.is_empty() {
                    String::new()
                } else {
                    format!(" {}", rendered_args.join(" "))
                };
                self.push_line(indent, &format!("{}{}", callee, suffix));
                return true;
            }
        }

        false
    }

    fn emit_host_scope(&mut self, invoke: &ExprInvoke, indent: usize) -> bool {
        if invoke.args.len() < 2 {
            self.errors
                .push("std::host::on expects host selector and closure body".to_string());
            return false;
        }
        let Some(hosts) = self.parse_host_selector(&invoke.args[0]) else {
            self.errors.push(
                "std::host::on host selector must be a string or list/tuple/array of strings"
                    .to_string(),
            );
            return false;
        };
        let scoped = EmitContext { hosts: Some(hosts) };

        for action in invoke.args.iter().skip(1) {
            let ExprKind::Closure(closure) = action.kind() else {
                self.errors.push(
                    "std::host::on requires closure body syntax: std::host::on(hosts, || { ... })"
                        .to_string(),
                );
                return false;
            };
            self.emit_expr(&closure.body, &scoped, indent);
        }
        for kwarg in &invoke.kwargs {
            let ExprKind::Closure(closure) = kwarg.value.kind() else {
                self.errors.push(
                    "std::host::on requires closure body syntax: std::host::on(hosts, || { ... })"
                        .to_string(),
                );
                return false;
            };
            self.emit_expr(&closure.body, &scoped, indent);
        }
        true
    }

    fn emit_run_command(
        &mut self,
        hosts: &[HostSelector],
        command: &str,
        meta: &OperationMeta,
        indent: usize,
    ) {
        for host in hosts {
            match host {
                HostSelector::Localhost => {
                    self.emit_command_with_meta(command.to_string(), host, meta, indent);
                }
                HostSelector::Remote(name) => {
                    self.emit_command_with_meta(
                        command.to_string(),
                        &HostSelector::Remote(name.clone()),
                        meta,
                        indent,
                    );
                }
            }
        }
    }

    fn emit_copy_command(
        &mut self,
        hosts: &[HostSelector],
        source: &str,
        destination: &str,
        meta: &OperationMeta,
        indent: usize,
    ) {
        for host in hosts {
            match host {
                HostSelector::Localhost => {
                    self.emit_local_operation_with_meta(
                        format!("cp -- {} {}", shell_quote(source), shell_quote(destination)),
                        meta,
                        indent,
                    );
                }
                HostSelector::Remote(name) => {
                    self.emit_local_operation_with_meta(
                        format!(
                            "scp_cmd {} {}",
                            shell_quote(source),
                            shell_quote(&format!("{}:{}", name, destination))
                        ),
                        meta,
                        indent,
                    );
                }
            }
        }
    }

    fn emit_rsync_command(
        &mut self,
        hosts: &[HostSelector],
        source: &str,
        destination: &str,
        options: RsyncOptions,
        meta: &OperationMeta,
        indent: usize,
    ) {
        let flags = options.to_flags();
        for host in hosts {
            match host {
                HostSelector::Localhost => {
                    self.emit_local_operation_with_meta(
                        format!(
                            "rsync {} -- {} {}",
                            flags,
                            shell_quote(source),
                            shell_quote(destination)
                        ),
                        meta,
                        indent,
                    );
                }
                HostSelector::Remote(name) => {
                    self.emit_local_operation_with_meta(
                        format!(
                            "rsync_cmd {} -- {} {}",
                            flags,
                            shell_quote(source),
                            shell_quote(&format!("{}:{}", name, destination))
                        ),
                        meta,
                        indent,
                    );
                }
            }
        }
    }

    fn emit_template_command(
        &mut self,
        hosts: &[HostSelector],
        source: &str,
        destination: &str,
        vars: Option<HashMap<String, String>>,
        meta: &OperationMeta,
        indent: usize,
    ) {
        let env_prefix = render_env_prefix(vars.as_ref());
        for host in hosts {
            match host {
                HostSelector::Localhost => {
                    self.emit_local_operation_with_meta(
                        format!(
                            "{}envsubst < {} > {}",
                            env_prefix,
                            shell_quote(source),
                            shell_quote(destination)
                        ),
                        meta,
                        indent,
                    );
                }
                HostSelector::Remote(name) => {
                    self.emit_local_operation_with_meta(
                        format!(
                            "{}envsubst < {} | ssh_cmd {} 'cat > {}'",
                            env_prefix,
                            shell_quote(source),
                            shell_quote(name),
                            shell_quote(destination)
                        ),
                        meta,
                        indent,
                    );
                }
            }
        }
    }

    fn emit_local_operation_with_meta(
        &mut self,
        command: String,
        meta: &OperationMeta,
        indent: usize,
    ) {
        let needs_wrapper = meta.only_if.is_some()
            || meta.unless.is_some()
            || meta.creates.is_some()
            || meta.removes.is_some();

        if !needs_wrapper {
            self.push_line(indent, &command);
            return;
        }

        self.push_line(indent, "__fp_changed=0");
        let mut condition_parts = Vec::new();
        if let Some(only_if) = &meta.only_if {
            condition_parts.push(format!("({})", only_if));
        }
        if let Some(unless) = &meta.unless {
            condition_parts.push(format!("! ({})", unless));
        }
        if let Some(creates) = &meta.creates {
            condition_parts.push(format!("[ ! -e {} ]", shell_quote(creates)));
        }
        if let Some(removes) = &meta.removes {
            condition_parts.push(format!("[ -e {} ]", shell_quote(removes)));
        }

        if condition_parts.is_empty() {
            self.push_line(indent + 1, &command);
            self.push_line(indent + 1, "__fp_changed=1");
        } else {
            self.push_line(
                indent,
                &format!("if {}; then", condition_parts.join(" && ")),
            );
            self.push_line(indent + 1, &command);
            self.push_line(indent + 1, "__fp_changed=1");
            self.push_line(indent, "fi");
        }

        self.push_line(indent, "__fp_last_changed=\"$__fp_changed\"");
    }

    fn resolve_hosts(&self, invoke: &ExprInvoke, context: &EmitContext) -> Vec<HostSelector> {
        invoke
            .args
            .get(1)
            .and_then(|expr| self.parse_host_selector(expr))
            .or_else(|| {
                kwarg_value(invoke, &["hosts", "host"])
                    .and_then(|expr| self.parse_host_selector(expr))
            })
            .or_else(|| context.hosts.clone())
            .unwrap_or_else(|| vec![HostSelector::Localhost])
    }

    fn operation_meta(&mut self, invoke: &ExprInvoke) -> OperationMeta {
        let mut meta = OperationMeta::default();
        meta.only_if =
            kwarg_value(invoke, &["only_if"]).and_then(|expr| self.resolve_string_expr(expr));
        meta.unless =
            kwarg_value(invoke, &["unless"]).and_then(|expr| self.resolve_string_expr(expr));
        meta.creates =
            kwarg_value(invoke, &["creates"]).and_then(|expr| self.resolve_string_expr(expr));
        meta.removes =
            kwarg_value(invoke, &["removes"]).and_then(|expr| self.resolve_string_expr(expr));

        meta
    }

    fn emit_command_with_meta(
        &mut self,
        command: String,
        host: &HostSelector,
        meta: &OperationMeta,
        indent: usize,
    ) {
        let needs_wrapper = meta.only_if.is_some()
            || meta.unless.is_some()
            || meta.creates.is_some()
            || meta.removes.is_some();

        let emit_base_command = |this: &mut BashRenderer, line: String| match host {
            HostSelector::Localhost => {
                this.push_line(indent + if needs_wrapper { 1 } else { 0 }, &line)
            }
            HostSelector::Remote(name) => {
                if needs_wrapper {
                    this.push_line(
                        indent + 1,
                        &format!(
                            "run_remote {} {}",
                            shell_arg_quote(name),
                            shell_arg_quote(&line)
                        ),
                    );
                } else {
                    this.queue_remote_command(indent, name.clone(), line);
                }
            }
        };

        if !needs_wrapper {
            emit_base_command(self, command);
            return;
        }

        self.push_line(indent, "__fp_changed=0");
        let mut condition_parts = Vec::new();
        if let Some(only_if) = &meta.only_if {
            condition_parts.push(format!("({})", only_if));
        }
        if let Some(unless) = &meta.unless {
            condition_parts.push(format!("! ({})", unless));
        }
        if let Some(creates) = &meta.creates {
            condition_parts.push(format!("[ ! -e {} ]", shell_quote(creates)));
        }
        if let Some(removes) = &meta.removes {
            condition_parts.push(format!("[ -e {} ]", shell_quote(removes)));
        }

        if condition_parts.is_empty() {
            emit_base_command(self, command);
            self.push_line(indent + 1, "__fp_changed=1");
        } else {
            self.push_line(
                indent,
                &format!("if {}; then", condition_parts.join(" && ")),
            );
            emit_base_command(self, command);
            self.push_line(indent + 1, "__fp_changed=1");
            self.push_line(indent, "fi");
        }

        self.push_line(indent, "__fp_last_changed=\"$__fp_changed\"");
    }

    fn resolve_vars_map(&self, expr: &Expr) -> Option<HashMap<String, String>> {
        match expr.kind() {
            ExprKind::Value(value) => match value.as_ref() {
                Value::Map(map) => {
                    let mut vars = HashMap::new();
                    for entry in &map.entries {
                        let key = match &entry.key {
                            Value::String(text) => text.value.clone(),
                            _ => return None,
                        };
                        let value = match &entry.value {
                            Value::String(text) => text.value.clone(),
                            Value::Int(int_value) => int_value.value.to_string(),
                            Value::Bool(bool_value) => bool_value.value.to_string(),
                            _ => return None,
                        };
                        vars.insert(key, value);
                    }
                    Some(vars)
                }
                _ => None,
            },
            _ => None,
        }
    }

    fn render_condition(&self, expr: &Expr) -> String {
        if let Some(flag) = self.resolve_bool_expr(expr) {
            return if flag {
                "true".to_string()
            } else {
                "false".to_string()
            };
        }

        if let ExprKind::Invoke(invoke) = expr.kind() {
            if let Some(path) = invoke_target_segments(&invoke.target) {
                if path == ["std", "server", "shell"] || path == ["std", "shell", "run"] {
                    if let Some(command_expr) = invoke
                        .args
                        .first()
                        .or_else(|| kwarg_value(invoke, &["command", "cmd"]))
                    {
                        if let Some(command) = self.resolve_string_expr(command_expr) {
                            return command;
                        }
                    }
                }
            }
        }

        if let Some(text) = self.resolve_string_expr(expr) {
            return format!("[[ {} == 'true' ]]", shell_quote(&text));
        }

        "true".to_string()
    }

    fn eval_var_value(&self, expr: &Expr) -> Option<VarValue> {
        if let Some(value) = self.resolve_string_list_expr(expr) {
            if value.len() > 1 {
                return Some(VarValue::StringList(value));
            }
        }

        if let Some(value) = self.resolve_bool_expr(expr) {
            return Some(VarValue::Bool(value));
        }

        if let Some(value) = self.eval_int_expr(expr) {
            return Some(VarValue::Int(value));
        }

        self.resolve_string_expr(expr).map(VarValue::String)
    }

    fn resolve_string_expr(&self, expr: &Expr) -> Option<String> {
        if let Some(value) = self.eval_int_expr(expr) {
            return Some(value.to_string());
        }

        match expr.kind() {
            ExprKind::Value(value) => match value.as_ref() {
                Value::String(text) => Some(text.value.clone()),
                Value::Int(int_value) => Some(int_value.value.to_string()),
                Value::Bool(bool_value) => Some(bool_value.value.to_string()),
                _ => None,
            },
            ExprKind::Name(Name::Ident(ident)) => {
                self.lookup_var(ident.as_str()).and_then(|var| match var {
                    VarValue::String(value) => Some(value.clone()),
                    VarValue::Int(value) => Some(value.to_string()),
                    VarValue::Bool(value) => Some(value.to_string()),
                    VarValue::StringList(_) => None,
                })
            }
            ExprKind::FormatString(template) => self.resolve_format_string(template, &[]),
            ExprKind::IntrinsicCall(call) => self.resolve_intrinsic_string(call),
            ExprKind::Paren(paren) => self.resolve_string_expr(&paren.expr),
            _ => None,
        }
    }

    fn resolve_intrinsic_string(&self, call: &ExprIntrinsicCall) -> Option<String> {
        if call.kind == IntrinsicCallKind::Format {
            let ExprKind::FormatString(template) = call.args.first()?.kind() else {
                return None;
            };
            return self.resolve_format_string(template, &call.args[1..]);
        }
        None
    }

    fn resolve_format_string(
        &self,
        template: &ExprStringTemplate,
        args: &[Expr],
    ) -> Option<String> {
        let mut rendered = String::new();
        let mut implicit_index = 0usize;

        for part in &template.parts {
            match part {
                FormatTemplatePart::Literal(text) => rendered.push_str(text),
                FormatTemplatePart::Placeholder(placeholder) => {
                    let value = match &placeholder.arg_ref {
                        FormatArgRef::Named(name) => {
                            self.lookup_var(name).and_then(|var| match var {
                                VarValue::String(value) => Some(value.clone()),
                                VarValue::Int(value) => Some(value.to_string()),
                                VarValue::Bool(value) => Some(value.to_string()),
                                VarValue::StringList(_) => None,
                            })
                        }
                        FormatArgRef::Implicit => {
                            let arg = args.get(implicit_index)?;
                            implicit_index += 1;
                            self.resolve_string_expr(arg)
                        }
                        FormatArgRef::Positional(index) => args
                            .get(*index)
                            .and_then(|arg| self.resolve_string_expr(arg)),
                    }?;
                    rendered.push_str(&value);
                }
            }
        }

        Some(rendered)
    }

    fn resolve_bool_expr(&self, expr: &Expr) -> Option<bool> {
        if let ExprKind::BinOp(bin_op) = expr.kind() {
            let lhs = self.eval_int_expr(&bin_op.lhs)?;
            let rhs = self.eval_int_expr(&bin_op.rhs)?;
            return match bin_op.kind {
                BinOpKind::Gt => Some(lhs > rhs),
                BinOpKind::Lt => Some(lhs < rhs),
                BinOpKind::Ge => Some(lhs >= rhs),
                BinOpKind::Le => Some(lhs <= rhs),
                BinOpKind::Eq => Some(lhs == rhs),
                BinOpKind::Ne => Some(lhs != rhs),
                _ => None,
            };
        }

        match expr.kind() {
            ExprKind::Value(value) => match value.as_ref() {
                Value::Bool(flag) => Some(flag.value),
                _ => None,
            },
            ExprKind::Name(Name::Ident(ident)) => {
                self.lookup_var(ident.as_str()).and_then(|var| match var {
                    VarValue::Bool(value) => Some(*value),
                    _ => None,
                })
            }
            ExprKind::Paren(paren) => self.resolve_bool_expr(&paren.expr),
            _ => None,
        }
    }

    fn eval_int_expr(&self, expr: &Expr) -> Option<i64> {
        match expr.kind() {
            ExprKind::Value(value) => match value.as_ref() {
                Value::Int(int_value) => Some(int_value.value),
                _ => None,
            },
            ExprKind::Name(Name::Ident(ident)) => {
                self.lookup_var(ident.as_str()).and_then(|var| match var {
                    VarValue::Int(value) => Some(*value),
                    _ => None,
                })
            }
            ExprKind::Paren(paren) => self.eval_int_expr(&paren.expr),
            ExprKind::UnOp(un_op) => {
                let value = self.eval_int_expr(&un_op.val)?;
                match un_op.op {
                    UnOpKind::Neg => Some(-value),
                    _ => None,
                }
            }
            ExprKind::BinOp(bin_op) => {
                let lhs = self.eval_int_expr(&bin_op.lhs)?;
                let rhs = self.eval_int_expr(&bin_op.rhs)?;
                match bin_op.kind {
                    BinOpKind::Add | BinOpKind::AddTrait => Some(lhs + rhs),
                    BinOpKind::Sub => Some(lhs - rhs),
                    BinOpKind::Mul => Some(lhs * rhs),
                    BinOpKind::Div => (rhs != 0).then_some(lhs / rhs),
                    BinOpKind::Mod => (rhs != 0).then_some(lhs % rhs),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    fn resolve_string_list_expr(&self, expr: &Expr) -> Option<Vec<String>> {
        match expr.kind() {
            ExprKind::Name(Name::Ident(ident)) => {
                let var = self.lookup_var(ident.as_str())?;
                match var {
                    VarValue::StringList(values) => Some(values.clone()),
                    VarValue::String(value) => Some(vec![value.clone()]),
                    VarValue::Int(value) => Some(vec![value.to_string()]),
                    VarValue::Bool(value) => Some(vec![value.to_string()]),
                }
            }
            ExprKind::Value(value) => match value.as_ref() {
                Value::String(text) => Some(vec![text.value.clone()]),
                Value::Int(int_value) => Some(vec![int_value.value.to_string()]),
                Value::List(list) => {
                    let mut values = Vec::new();
                    for value in &list.values {
                        match value {
                            Value::String(text) => values.push(text.value.clone()),
                            Value::Int(int_value) => values.push(int_value.value.to_string()),
                            _ => return None,
                        }
                    }
                    Some(values)
                }
                _ => None,
            },
            ExprKind::Array(array) => {
                let mut values = Vec::new();
                for item in &array.values {
                    let value = self.resolve_string_expr(item)?;
                    values.push(value);
                }
                Some(values)
            }
            ExprKind::Tuple(tuple) => {
                let mut values = Vec::new();
                for item in &tuple.values {
                    let value = self.resolve_string_expr(item)?;
                    values.push(value);
                }
                Some(values)
            }
            ExprKind::Paren(paren) => self.resolve_string_list_expr(&paren.expr),
            _ => None,
        }
    }

    fn parse_host_selector(&self, expr: &Expr) -> Option<Vec<HostSelector>> {
        self.resolve_string_list_expr(expr).map(|hosts| {
            let mut selectors = Vec::new();
            for name in hosts {
                if let Some(group_hosts) = self.inventory.groups.get(&name) {
                    for host_name in group_hosts {
                        selectors.push(self.host_from_inventory(host_name));
                    }
                } else {
                    selectors.push(self.host_from_inventory(&name));
                }
            }
            selectors
        })
    }

    fn host_from_inventory(&self, name: &str) -> HostSelector {
        if name == "localhost" {
            return HostSelector::Localhost;
        }

        if let Some(host) = self.inventory.hosts.get(name) {
            let mut target = host.address.clone().unwrap_or_else(|| name.to_string());
            if let Some(user) = &host.user {
                target = format!("{}@{}", user, target);
            }
            return HostSelector::Remote(target);
        }

        host_from_name(name)
    }

    fn bind_var(&mut self, name: String, value: VarValue) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, value);
        }
    }

    fn lookup_var(&self, name: &str) -> Option<&VarValue> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value);
            }
        }
        None
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            let _ = self.scopes.pop();
        }
    }

    fn push_line(&mut self, indent: usize, text: &str) {
        self.flush_pending_remote();
        self.lines
            .push(format!("{}{}", "    ".repeat(indent), text));
    }

    fn queue_remote_command(&mut self, indent: usize, host: String, command: String) {
        if let Some(pending) = self.pending_remote.as_mut() {
            if pending.indent == indent && pending.host == host {
                pending.commands.push(command);
                return;
            }
        }

        self.flush_pending_remote();
        self.pending_remote = Some(PendingRemote {
            indent,
            host,
            commands: vec![command],
        });
    }

    fn flush_pending_remote(&mut self) {
        let Some(pending) = self.pending_remote.take() else {
            return;
        };

        let merged = pending.commands.join(" && ");
        self.lines.push(format!(
            "{}run_remote {} {}",
            "    ".repeat(pending.indent),
            shell_arg_quote(&pending.host),
            shell_arg_quote(&merged)
        ));
    }
}

fn invoke_target_segments(target: &ExprInvokeTarget) -> Option<Vec<String>> {
    match target {
        ExprInvokeTarget::Function(name) => Some(name_to_segments(name)),
        _ => None,
    }
}

fn name_to_segments(name: &Name) -> Vec<String> {
    name.to_path()
        .segments
        .iter()
        .map(|ident| ident.as_str().to_string())
        .collect()
}

fn kwarg_value<'a>(invoke: &'a ExprInvoke, names: &[&str]) -> Option<&'a Expr> {
    invoke
        .kwargs
        .iter()
        .find(|kwarg| names.iter().any(|name| *name == kwarg.name))
        .map(|kwarg| &kwarg.value)
}

fn host_from_name(name: &str) -> HostSelector {
    if name == "localhost" {
        HostSelector::Localhost
    } else {
        HostSelector::Remote(name.to_string())
    }
}

fn apply_shell_options(command: String, cwd: Option<&str>, sudo: bool) -> String {
    let with_cwd = if let Some(cwd) = cwd {
        format!("cd {} && {}", shell_quote(cwd), command)
    } else {
        command
    };

    if sudo {
        format!("sudo {}", with_cwd)
    } else {
        with_cwd
    }
}

fn shell_quote(raw: &str) -> String {
    let escaped = raw.replace('\'', "'\\''");
    format!("'{}'", escaped)
}

fn shell_double_quote(raw: &str) -> String {
    let escaped = raw
        .replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('`', "\\`");
    format!("\"{}\"", escaped)
}

fn shell_arg_quote(raw: &str) -> String {
    if raw.contains('$') {
        shell_double_quote(raw)
    } else {
        shell_quote(raw)
    }
}

fn render_env_prefix(vars: Option<&HashMap<String, String>>) -> String {
    let Some(vars) = vars else {
        return String::new();
    };
    if vars.is_empty() {
        return String::new();
    }

    let mut parts = Vec::new();
    for (key, value) in vars {
        parts.push(format!("{}={}", key, shell_quote(value)));
    }
    format!("env {} ", parts.join(" "))
}

#[cfg(test)]
mod tests {
    use super::*;
    use fp_core::frontend::LanguageFrontend;
    use fp_lang::FerroFrontend;
    use std::path::Path;

    fn emit(source: &str) -> String {
        let frontend = FerroFrontend::new();
        let result = frontend
            .parse(source, Some(Path::new("test.fp")))
            .expect("parse should succeed");
        BashTarget::new()
            .emit_node(&result.ast)
            .expect("emit should succeed")
            .code
    }

    #[test]
    fn emits_module_style_commands() {
        let source = r#"
const fn main() {
    std::host::on("web-1", || {
        std::server::shell("uptime");
        std::files::copy(src="./app.conf", dest="/etc/app.conf");
        std::service::restart(name="app");
    });
}
"#;
        let script = emit(source);
        assert!(script.contains("run_remote 'web-1' 'uptime'"));
        assert!(script.contains("scp_cmd './app.conf' 'web-1:/etc/app.conf'"));
        assert!(script.contains("run_remote 'web-1' 'sudo systemctl restart app'"));
    }

    #[test]
    fn batches_consecutive_remote_shell_calls_per_host() {
        let source = r#"
const fn main() {
    std::host::on("web-1", || {
        std::server::shell("echo a");
        std::server::shell("echo b");
        std::server::shell("echo c");
    });
}
"#;
        let script = emit(source);
        assert!(script.contains("run_remote 'web-1' 'echo a && echo b && echo c'"));
        assert_eq!(script.matches("run_remote 'web-1'").count(), 1);
    }

    #[test]
    fn supports_rsync_operation() {
        let source = r#"
const fn main() {
    std::files::rsync(src="./assets/", dest="/srv/app/assets/", hosts="web-1", delete=true);
}
"#;
        let script = emit(source);
        assert!(script.contains("rsync_cmd -az --delete -- './assets/' 'web-1:/srv/app/assets/'"));
    }

    #[test]
    fn supports_user_defined_functions_and_calls() {
        let source = r#"
const fn deploy(host: str, service: str) {
    std::files::rsync(src="./dist/", dest="/srv/app/", hosts=host, delete=true);
    std::host::on(host, || {
        std::service::restart(name=service);
    });
}

const fn main() {
    deploy("web-1", "fp-service");
}
"#;
        let script = emit(source);
        assert!(script.contains("deploy() {"));
        assert!(script.contains("deploy 'web-1' 'fp-service'"));
        assert!(script.contains("local host=\"$1\""));
        assert!(script.contains("local service=\"$2\""));
    }

    #[test]
    fn supports_string_variables() {
        let source = r#"
const fn main() {
    let cmd = "uptime";
    let target = "web-1";
    std::server::shell(cmd, hosts=target);
}
"#;
        let script = emit(source);
        assert!(script.contains("run_remote 'web-1' 'uptime'"));
    }

    #[test]
    fn supports_string_template_variables() {
        let source = r#"
const fn main() {
    let cmd = "uptime";
    let retries = 3;
    std::server::shell(f"echo cmd={cmd} retries={retries}");
}
"#;
        let script = emit(source);
        assert!(script.contains("echo cmd=uptime retries=3"));
    }

    #[test]
    fn supports_runtime_loop_variable_interpolation() {
        let source = r#"
const fn main() {
    for host in ["web-1", "web-2"] {
        std::server::shell(f"echo host={host}");
    }
}
"#;
        let script = emit(source);
        assert!(script.contains("for host in 'web-1' 'web-2'; do"));
        assert!(script.contains("echo host=$host"));
    }

    #[test]
    fn supports_runtime_loop_variable_in_remote_hosts() {
        let source = r#"
const fn main() {
    for host in ["web-1", "web-2"] {
        std::server::shell("hostname", hosts=[host]);
    }
}
"#;
        let script = emit(source);
        assert!(script.contains("run_remote \"$host\" 'hostname'"));
    }

    #[test]
    fn supports_runtime_arguments_in_function_calls() {
        let source = r#"
const fn deploy(host: str) {
    std::server::shell("hostname", hosts=host);
}

const fn main() {
    for host in ["web-1", "web-2"] {
        deploy(host);
    }
}
"#;
        let script = emit(source);
        assert!(script.contains("deploy \"$host\""));
    }

    #[test]
    fn supports_integer_variables() {
        let source = r#"
const fn main() {
    let retries = 3;
    std::server::shell(retries);
}
"#;
        let script = emit(source);
        assert!(script.contains("3"));
    }

    #[test]
    fn emits_runtime_if_structure() {
        let source = r#"
const fn main() {
    if true {
        std::server::shell("echo yes");
    } else {
        std::server::shell("echo no");
    }
}
"#;
        let script = emit(source);
        assert!(script.contains("if true; then"));
        assert!(script.contains("echo yes"));
        assert!(script.contains("else"));
        assert!(script.contains("echo no"));
        assert!(script.contains("fi"));
    }

    #[test]
    fn emits_runtime_for_structure() {
        let source = r#"
const fn main() {
    for host in ["web-1", "web-2"] {
        std::server::shell("hostname", hosts=[host]);
    }
}
"#;
        let script = emit(source);
        assert!(script.contains("for host in 'web-1' 'web-2'; do"));
        assert!(script.contains("done"));
    }

    #[test]
    fn keeps_backwards_compat_for_std_shell_calls() {
        let source = r#"
const fn main() {
    std::shell::run("echo local");
    std::shell::on("web-1", || {
        std::shell::run("echo remote");
    });
}
"#;
        let script = emit(source);
        assert!(script.contains("echo local"));
        assert!(script.contains("run_remote 'web-1' 'echo remote'"));
    }

    #[test]
    fn supports_integer_arithmetic_expressions() {
        let source = r#"
const fn main() {
    let base = 10;
    let step = 5;
    let total = base + step * 2;
    std::server::shell(total);
}
"#;
        let script = emit(source);
        assert!(script.contains("20"));
    }

    #[test]
    fn supports_idempotency_guards() {
        let source = r#"
const fn main() {
    std::server::shell(
        "touch /tmp/flag",
        creates="/tmp/flag",
        only_if="test -d /tmp",
        unless="test -f /tmp/skip",
    );
}
"#;
        let script = emit(source);
        assert!(script.contains("__fp_changed=0"));
        assert!(script.contains("[ ! -e '/tmp/flag' ]"));
        assert!(script.contains("(test -d /tmp)"));
        assert!(script.contains("! (test -f /tmp/skip)"));
    }
    #[test]
    fn tracks_last_changed_result() {
        let source = r#"
const fn main() {
    std::files::copy(src="./app.conf", dest="/etc/app.conf", creates="/etc/app.conf");
}
"#;
        let script = emit(source);
        assert!(script.contains("__fp_last_changed=0"));
        assert!(script.contains("__fp_changed=0"));
        assert!(script.contains("__fp_last_changed=\"$__fp_changed\""));
    }

    #[test]
    fn supports_template_operation() {
        let source = r#"
const fn main() {
    std::files::template(src="./templates/app.conf.tpl", dest="/etc/app.conf");
}
"#;
        let script = emit(source);
        assert!(script.contains("envsubst < './templates/app.conf.tpl' > '/etc/app.conf'"));
    }
}
