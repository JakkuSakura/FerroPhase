use fp_core::ast::{
    BlockStmt, Expr, ExprBlock, ExprInvokeTarget, ExprKind, ExprMatch, ItemDefFunction, PatternKind,
};
use fp_shell_core::{
    ArithmeticOp, BoolExpr, ComparisonOp, ConditionExpr, ExternalFunction, ScriptItem,
    ScriptProgram, ScriptRenderer, ShellInventory, StringComparisonOp, StringExpr, StringPart,
    TransportKind, ValueExpr,
};
use std::collections::{BTreeSet, HashMap};

pub struct BashTarget;

impl BashTarget {
    pub fn new() -> Self {
        Self
    }
}

impl Default for BashTarget {
    fn default() -> Self {
        Self::new()
    }
}

impl ScriptRenderer for BashTarget {
    type Error = String;

    fn render(
        &self,
        program: &ScriptProgram,
        inventory: &ShellInventory,
    ) -> Result<String, Self::Error> {
        let mut renderer = BashRenderer::new(inventory);
        renderer.render_program(program)?;
        Ok(renderer.finish())
    }
}

struct BashRenderer<'a> {
    inventory: &'a ShellInventory,
    externs: HashMap<String, ExternalFunction>,
    used_externs: BTreeSet<String>,
    lines: Vec<String>,
}

impl<'a> BashRenderer<'a> {
    fn new(inventory: &'a ShellInventory) -> Self {
        Self {
            inventory,
            externs: HashMap::new(),
            used_externs: BTreeSet::new(),
            lines: Vec::new(),
        }
    }

    fn finish(self) -> String {
        let mut script = String::new();
        script.push_str("#!/usr/bin/env bash\n");
        script.push_str("set -xeuo pipefail\n\n");
        script.push_str("__fp_last_changed=0\n\n");
        script.push_str("declare -A FP_HOST_TRANSPORT=()\n");
        script.push_str("declare -A FP_SSH_ADDRESS=()\n");
        script.push_str("declare -A FP_SSH_USER=()\n");
        script.push_str("declare -A FP_SSH_PORT=()\n");
        script.push_str("declare -A FP_DOCKER_CONTAINER=()\n");
        script.push_str("declare -A FP_DOCKER_USER=()\n");
        script.push_str("declare -A FP_K8S_POD=()\n");
        script.push_str("declare -A FP_K8S_NAMESPACE=()\n");
        script.push_str("declare -A FP_K8S_CONTAINER=()\n");
        script.push_str("declare -A FP_K8S_CONTEXT=()\n");
        script.push_str("declare -A FP_WINRM_ADDRESS=()\n");
        script.push_str("declare -A FP_WINRM_USER=()\n");
        script.push_str("declare -A FP_WINRM_PASSWORD=()\n");
        script.push_str("declare -A FP_WINRM_PORT=()\n");
        script.push_str("declare -A FP_WINRM_SCHEME=()\n\n");
        for (name, host) in &self.inventory.hosts {
            script.push_str(&format!(
                "FP_HOST_TRANSPORT[{}]={}\n",
                shell_arg_quote(name),
                shell_arg_quote(match host.transport {
                    TransportKind::Local => "local",
                    TransportKind::Ssh => "ssh",
                    TransportKind::Docker => "docker",
                    TransportKind::Kubectl => "kubectl",
                    TransportKind::Winrm => "winrm",
                })
            ));
            if let Some(ssh) = &host.ssh {
                if let Some(address) = &ssh.address {
                    script.push_str(&format!(
                        "FP_SSH_ADDRESS[{}]={}\n",
                        shell_arg_quote(name),
                        shell_arg_quote(address)
                    ));
                }
                if let Some(user) = &ssh.user {
                    script.push_str(&format!(
                        "FP_SSH_USER[{}]={}\n",
                        shell_arg_quote(name),
                        shell_arg_quote(user)
                    ));
                }
                if let Some(port) = ssh.port {
                    script.push_str(&format!(
                        "FP_SSH_PORT[{}]={}\n",
                        shell_arg_quote(name),
                        shell_arg_quote(&port.to_string())
                    ));
                }
            }
            if let Some(docker) = &host.docker {
                script.push_str(&format!(
                    "FP_DOCKER_CONTAINER[{}]={}\n",
                    shell_arg_quote(name),
                    shell_arg_quote(&docker.container)
                ));
                if let Some(user) = &docker.user {
                    script.push_str(&format!(
                        "FP_DOCKER_USER[{}]={}\n",
                        shell_arg_quote(name),
                        shell_arg_quote(user)
                    ));
                }
            }
            if let Some(kubectl) = &host.kubectl {
                script.push_str(&format!(
                    "FP_K8S_POD[{}]={}\n",
                    shell_arg_quote(name),
                    shell_arg_quote(&kubectl.pod)
                ));
                if let Some(namespace) = &kubectl.namespace {
                    script.push_str(&format!(
                        "FP_K8S_NAMESPACE[{}]={}\n",
                        shell_arg_quote(name),
                        shell_arg_quote(namespace)
                    ));
                }
                if let Some(container) = &kubectl.container {
                    script.push_str(&format!(
                        "FP_K8S_CONTAINER[{}]={}\n",
                        shell_arg_quote(name),
                        shell_arg_quote(container)
                    ));
                }
                if let Some(context) = &kubectl.context {
                    script.push_str(&format!(
                        "FP_K8S_CONTEXT[{}]={}\n",
                        shell_arg_quote(name),
                        shell_arg_quote(context)
                    ));
                }
            }
            if let Some(winrm) = &host.winrm {
                script.push_str(&format!(
                    "FP_WINRM_ADDRESS[{}]={}\n",
                    shell_arg_quote(name),
                    shell_arg_quote(&winrm.address)
                ));
                script.push_str(&format!(
                    "FP_WINRM_USER[{}]={}\n",
                    shell_arg_quote(name),
                    shell_arg_quote(&winrm.user)
                ));
                if let Some(password) = &winrm.password {
                    script.push_str(&format!(
                        "FP_WINRM_PASSWORD[{}]={}\n",
                        shell_arg_quote(name),
                        shell_arg_quote(password)
                    ));
                }
                if let Some(port) = winrm.port {
                    script.push_str(&format!(
                        "FP_WINRM_PORT[{}]={}\n",
                        shell_arg_quote(name),
                        shell_arg_quote(&port.to_string())
                    ));
                }
                if let Some(scheme) = &winrm.scheme {
                    script.push_str(&format!(
                        "FP_WINRM_SCHEME[{}]={}\n",
                        shell_arg_quote(name),
                        shell_arg_quote(scheme)
                    ));
                }
            }
        }
        script.push_str("\nSSH_CONTROL_PATH=\"${TMPDIR:-/tmp}/fp-shell-%r@%h:%p\"\n\n");
        script.push_str(&self.render_runtime_validator());
        script.push_str(BASH_RUNTIME_UTILITIES);
        for line in self.lines {
            script.push_str(&line);
            script.push('\n');
        }
        script
    }

    fn render_program(&mut self, program: &ScriptProgram) -> Result<(), String> {
        self.externs = program.externs.clone();
        self.used_externs = program
            .used_externs()
            .into_iter()
            .map(str::to_string)
            .collect();
        for item in &program.items {
            match item {
                ScriptItem::Function(def) => self.render_function(def)?,
                ScriptItem::Expr(expr) => self.render_expr_statement(expr, 0)?,
            }
        }
        Ok(())
    }

    fn render_function(&mut self, def: &ItemDefFunction) -> Result<(), String> {
        self.push_line(0, &format!("{}() {{", def.name));
        for (index, param) in def.sig.params.iter().enumerate() {
            self.push_line(1, &format!("local {}=\"${}\"", param.name, index + 1));
        }
        match def.body.kind() {
            ExprKind::Block(block) => self.render_block(block, 1)?,
            _ => self.render_expr_statement(&def.body, 1)?,
        }
        self.push_line(0, "}");
        self.push_line(0, "");
        Ok(())
    }

    fn render_block(&mut self, block: &ExprBlock, indent: usize) -> Result<(), String> {
        for statement in &block.stmts {
            self.render_block_stmt(statement, indent)?;
        }
        Ok(())
    }

    fn render_match_expr(&mut self, expr_match: &ExprMatch, indent: usize) -> Result<(), String> {
        let scrutinee = expr_match
            .scrutinee
            .as_deref()
            .ok_or_else(|| "bash match requires scrutinee".to_string())?;
        let ValueExpr::String(scrutinee) = self.extract_value_expr(scrutinee)? else {
            return Err("bash match scrutinee must be lowered string".to_string());
        };
        self.push_line(
            indent,
            &format!("case {} in", self.render_word(&scrutinee)),
        );
        for case in &expr_match.cases {
            let pattern = match case.pat.as_ref().and_then(|pat| extract_match_case_string(pat)) {
                Some(pattern) => self.render_case_pattern(&pattern)?,
                None => "*".to_string(),
            };
            self.push_line(indent + 1, &format!("{})", pattern));
            self.render_expr_statement(&case.body, indent + 2)?;
            self.push_line(indent + 2, ";;");
        }
        self.push_line(indent, "esac");
        Ok(())
    }

    fn render_invoke_statement(
        &mut self,
        name: &str,
        args: &[ValueExpr],
        indent: usize,
    ) -> Result<(), String> {
        if self.externs.get(name).map(|decl| decl.abi.as_str()) == Some("bash") {
            let text = self.render_bash_extern_statement(name, args)?;
            self.push_line(indent, &text);
            return Ok(());
        }
        let args = args
            .iter()
            .map(|arg| self.render_value(arg))
            .collect::<Result<Vec<_>, _>>()?
            .join(" ");
        self.push_line(indent, &format!("{} {}", name, args));
        Ok(())
    }

    fn render_block_stmt(&mut self, statement: &BlockStmt, indent: usize) -> Result<(), String> {
        match statement {
            BlockStmt::Expr(expr) => self.render_expr_statement(&expr.expr, indent),
            BlockStmt::Let(stmt) => {
                let Some(name) = stmt.pat.as_ident() else {
                    return Err("bash renderer only supports identifier let bindings".to_string());
                };
                let Some(init) = &stmt.init else {
                    return Ok(());
                };
                let value = self.render_expr_as_value(init)?;
                self.push_line(indent, &format!("local {}={}", name, value));
                Ok(())
            }
            BlockStmt::Any(_) | BlockStmt::Item(_) | BlockStmt::Noop => Ok(()),
        }
    }

    fn render_expr_statement(&mut self, expr: &Expr, indent: usize) -> Result<(), String> {
        match expr.kind() {
            ExprKind::Block(block) => self.render_block(block, indent),
            ExprKind::Match(expr_match) => self.render_match_expr(expr_match, indent),
            ExprKind::If(expr_if) => {
                self.push_line(
                    indent,
                    &format!("if {}; then", self.render_expr_as_condition(&expr_if.cond)?),
                );
                self.render_expr_statement(&expr_if.then, indent + 1)?;
                if let Some(elze) = &expr_if.elze {
                    self.push_line(indent, "else");
                    self.render_expr_statement(elze, indent + 1)?;
                }
                self.push_line(indent, "fi");
                Ok(())
            }
            ExprKind::While(expr_while) => {
                self.push_line(
                    indent,
                    &format!(
                        "while {}; do",
                        self.render_expr_as_condition(&expr_while.cond)?
                    ),
                );
                self.render_expr_statement(&expr_while.body, indent + 1)?;
                self.push_line(indent, "done");
                Ok(())
            }
            ExprKind::For(expr_for) => {
                let PatternKind::Ident(pattern) = expr_for.pat.kind() else {
                    return Err("bash renderer only supports identifier for bindings".to_string());
                };
                let ValueExpr::StringList(values) = self.extract_value_expr(&expr_for.iter)? else {
                    return Err("bash renderer only supports string-list for iterables".to_string());
                };
                let values = values
                    .iter()
                    .map(|value| self.render_word(value))
                    .collect::<Vec<_>>()
                    .join(" ");
                self.push_line(indent, &format!("for {} in {}; do", pattern.ident, values));
                self.render_expr_statement(&expr_for.body, indent + 1)?;
                self.push_line(indent, "done");
                Ok(())
            }
            ExprKind::Let(expr_let) => {
                let Some(name) = expr_let.pat.as_ident() else {
                    return Err("bash renderer only supports identifier let bindings".to_string());
                };
                let value = self.render_expr_as_value(&expr_let.expr)?;
                self.push_line(indent, &format!("local {}={}", name, value));
                Ok(())
            }
            ExprKind::Invoke(invoke) => {
                let ExprInvokeTarget::Function(name) = &invoke.target else {
                    return Err(
                        "bash renderer only supports function invocation targets".to_string()
                    );
                };
                let Some(ident) = name.as_ident() else {
                    return Err("bash renderer only supports identifier invocation targets".to_string());
                };
                let args = invoke
                    .args
                    .iter()
                    .map(|arg| self.extract_value_expr(arg))
                    .collect::<Result<Vec<_>, _>>()?;
                self.render_invoke_statement(ident.as_str(), &args, indent)
            }
            ExprKind::Any(any) if any.downcast_ref::<ValueExpr>().is_some() => {
                self.push_line(indent, &self.render_expr_as_value(expr)?);
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn render_expr_as_condition(&self, expr: &Expr) -> Result<String, String> {
        let ExprKind::Any(any) = expr.kind() else {
            return Err("bash renderer expected lowered shell condition".to_string());
        };
        let Some(condition) = any.downcast_ref::<ConditionExpr>() else {
            return Err("bash renderer expected shell condition payload".to_string());
        };
        self.render_condition(condition)
    }

    fn render_expr_as_value(&self, expr: &Expr) -> Result<String, String> {
        self.render_value(&self.extract_value_expr(expr)?)
    }

    fn extract_value_expr(&self, expr: &Expr) -> Result<ValueExpr, String> {
        let ExprKind::Any(any) = expr.kind() else {
            return Err("bash renderer expected lowered shell value".to_string());
        };
        any.downcast_ref::<ValueExpr>()
            .cloned()
            .ok_or_else(|| "bash renderer expected shell value payload".to_string())
    }

    fn render_condition(&self, condition: &ConditionExpr) -> Result<String, String> {
        Ok(match condition {
            ConditionExpr::Bool(BoolExpr::Literal(true)) => "true".to_string(),
            ConditionExpr::Bool(BoolExpr::Literal(false)) => "false".to_string(),
            ConditionExpr::Bool(BoolExpr::Variable(name)) => {
                format!("[[ \"${{{}}}\" == 'true' ]]", name)
            }
            ConditionExpr::Bool(BoolExpr::IntComparison { lhs, op, rhs }) => {
                format!(
                    "[[ {} {} {} ]]",
                    self.render_int(lhs),
                    render_comparison(*op),
                    self.render_int(rhs)
                )
            }
            ConditionExpr::Bool(BoolExpr::StringComparison { lhs, op, rhs }) => {
                format!(
                    "[[ {} {} {} ]]",
                    self.render_word(lhs),
                    render_string_comparison(*op),
                    self.render_word(rhs)
                )
            }
            ConditionExpr::Command(command) => self.render_command_expr(command),
            ConditionExpr::StringTruthy(expr) => {
                format!("[[ {} == 'true' ]]", self.render_word(expr))
            }
        })
    }

    fn render_value(&self, value: &ValueExpr) -> Result<String, String> {
        Ok(match value {
            ValueExpr::String(expr) => self.render_word(expr),
            ValueExpr::Int(expr) => self.render_int(expr),
            ValueExpr::Bool(BoolExpr::Literal(value)) => shell_arg_quote(&value.to_string()),
            ValueExpr::Bool(BoolExpr::Variable(name)) => format!("\"${{{}}}\"", name),
            ValueExpr::Bool(BoolExpr::IntComparison { lhs, op, rhs }) => shell_arg_quote(&format!(
                "{} {} {}",
                self.render_int(lhs),
                render_comparison(*op),
                self.render_int(rhs)
            )),
            ValueExpr::Bool(BoolExpr::StringComparison { lhs, op, rhs }) => {
                shell_arg_quote(&format!(
                    "{} {} {}",
                    self.render_word(lhs),
                    render_string_comparison(*op),
                    self.render_word(rhs)
                ))
            }
            ValueExpr::StringList(values) => values
                .iter()
                .map(|value| self.render_word(value))
                .collect::<Vec<_>>()
                .join(" "),
        })
    }

    fn render_int(&self, expr: &fp_shell_core::IntExpr) -> String {
        match expr {
            fp_shell_core::IntExpr::Literal(value) => value.to_string(),
            fp_shell_core::IntExpr::Variable(name) => format!("${{{}}}", name),
            fp_shell_core::IntExpr::UnaryNeg(value) => format!("-{}", self.render_int(value)),
            fp_shell_core::IntExpr::Binary { lhs, op, rhs } => {
                format!(
                    "$(({} {} {}))",
                    self.render_int(lhs),
                    render_arithmetic(*op),
                    self.render_int(rhs)
                )
            }
        }
    }

    fn render_word(&self, expr: &StringExpr) -> String {
        match expr {
            StringExpr::Literal(text) => shell_arg_quote(text),
            StringExpr::Variable(name) => format!("\"${{{}}}\"", name),
            StringExpr::Call { name, args } => format!("\"$({})\"", self.render_call(name, args)),
            StringExpr::Interpolated(parts) => {
                let mut out = String::from("\"");
                for part in parts {
                    match part {
                        StringPart::Literal(text) => out.push_str(&escape_double_quotes(text)),
                        StringPart::Variable(name) => out.push_str(&format!("${{{}}}", name)),
                        StringPart::Call { name, args } => {
                            out.push_str(&format!("$({})", self.render_call(name, args)))
                        }
                    }
                }
                out.push('"');
                out
            }
        }
    }

    fn render_command_expr(&self, expr: &StringExpr) -> String {
        match expr {
            StringExpr::Literal(text) => text.clone(),
            StringExpr::Variable(name) => format!("${{{}}}", name),
            StringExpr::Call { name, args } => format!("$({})", self.render_call(name, args)),
            StringExpr::Interpolated(parts) => {
                let mut out = String::new();
                for part in parts {
                    match part {
                        StringPart::Literal(text) => out.push_str(text),
                        StringPart::Variable(name) => out.push_str(&format!("${{{}}}", name)),
                        StringPart::Call { name, args } => {
                            out.push_str(&format!("$({})", self.render_call(name, args)))
                        }
                    }
                }
                out
            }
        }
    }

    fn render_case_pattern(&self, expr: &StringExpr) -> Result<String, String> {
        match expr {
            StringExpr::Literal(text) => Ok(shell_case_quote(text)),
            other => Err(format!(
                "bash case patterns must be string literals, found {:?}",
                other
            )),
        }
    }

    fn push_line(&mut self, indent: usize, text: &str) {
        self.lines
            .push(format!("{}{}", "    ".repeat(indent), text));
    }

    fn render_call(&self, name: &str, args: &[ValueExpr]) -> String {
        if self.externs.get(name).map(|decl| decl.abi.as_str()) == Some("bash") {
            return self.render_bash_extern_call(name, args);
        }
        let args = args
            .iter()
            .map(|arg| self.render_value(arg))
            .collect::<Result<Vec<_>, _>>()
            .expect("call arguments should render");
        if args.is_empty() {
            name.to_string()
        } else {
            format!("{} {}", name, args.join(" "))
        }
    }

    fn render_bash_extern_call(&self, name: &str, args: &[ValueExpr]) -> String {
        match name {
            "runtime_host_transport" => {
                let host = self.expect_string_arg(args, 0);
                format!(
                    "printf '%s\\n' \"${{FP_HOST_TRANSPORT[{}]:-ssh}}\"",
                    self.render_word(host)
                )
            }
            "runtime_temp_path" => "mktemp".to_string(),
            other => {
                let rendered = args
                    .iter()
                    .map(|arg| self.render_value(arg))
                    .collect::<Result<Vec<_>, _>>()
                    .expect("extern arguments should render");
                if rendered.is_empty() {
                    other.to_string()
                } else {
                    format!("{} {}", other, rendered.join(" "))
                }
            }
        }
    }

    fn render_bash_extern_statement(
        &mut self,
        name: &str,
        args: &[ValueExpr],
    ) -> Result<String, String> {
        Ok(match name {
            "bash" => format!("bash -lc {}", self.render_value(&args[0])?),
            "ssh" => {
                let setup = self.emit_ssh_target_setup_inline(self.expect_value_string(args, 0));
                format!(
                    "{setup}; if [[ -n \"$__fp_port\" ]]; then ssh_cmd -p \"$__fp_port\" \"$__fp_target\" {}; else ssh_cmd \"$__fp_target\" {}; fi",
                    self.render_value(&args[1])?,
                    self.render_value(&args[1])?
                )
            }
            "docker_exec" => {
                let host = self.expect_value_string(args, 0);
                format!(
                    "__fp_host={}; __fp_container=\"${{FP_DOCKER_CONTAINER[$__fp_host]}}\"; __fp_user=\"${{FP_DOCKER_USER[$__fp_host]:-}}\"; if [[ -n \"$__fp_user\" ]]; then docker exec --user \"$__fp_user\" \"$__fp_container\" sh -lc {}; else docker exec \"$__fp_container\" sh -lc {}; fi",
                    self.render_word(host),
                    self.render_value(&args[1])?,
                    self.render_value(&args[1])?
                )
            }
            "kubectl_exec" => {
                let host = self.expect_value_string(args, 0);
                format!(
                    "__fp_host={}; __fp_kubectl_args=(); __fp_context=\"${{FP_K8S_CONTEXT[$__fp_host]:-}}\"; __fp_namespace=\"${{FP_K8S_NAMESPACE[$__fp_host]:-}}\"; __fp_container=\"${{FP_K8S_CONTAINER[$__fp_host]:-}}\"; __fp_pod=\"${{FP_K8S_POD[$__fp_host]}}\"; [[ -n \"$__fp_context\" ]] && __fp_kubectl_args+=(--context \"$__fp_context\"); [[ -n \"$__fp_namespace\" ]] && __fp_kubectl_args+=(-n \"$__fp_namespace\"); __fp_kubectl_args+=(exec); [[ -n \"$__fp_container\" ]] && __fp_kubectl_args+=(-c \"$__fp_container\"); __fp_kubectl_args+=(\"$__fp_pod\" -- sh -lc {}); kubectl \"${{__fp_kubectl_args[@]}}\"",
                    self.render_word(host),
                    self.render_value(&args[1])?
                )
            }
            "winrm_run" => format!(
                "winrm_pwsh {} run {}",
                self.render_value(&args[0])?,
                self.render_value(&args[1])?
            ),
            "cp" => format!(
                "cp -- {} {}",
                self.render_value(&args[0])?,
                self.render_value(&args[1])?
            ),
            "scp" => {
                let setup = self.emit_ssh_target_setup_inline(self.expect_value_string(args, 0));
                let src = self.render_value(&args[1])?;
                let dest = self.render_value(&args[2])?;
                format!(
                    "{setup}; __fp_remote_path={dest}; if [[ -n \"$__fp_port\" ]]; then scp_cmd -P \"$__fp_port\" {src} \"$__fp_target:$__fp_remote_path\"; else scp_cmd {src} \"$__fp_target:$__fp_remote_path\"; fi"
                )
            }
            "docker_cp" => {
                let host = self.expect_value_string(args, 0);
                format!(
                    "__fp_host={}; __fp_container=\"${{FP_DOCKER_CONTAINER[$__fp_host]}}\"; __fp_remote_path={}; docker cp {} \"$__fp_container:$__fp_remote_path\"",
                    self.render_word(host),
                    self.render_value(&args[2])?,
                    self.render_value(&args[1])?
                )
            }
            "kubectl_cp" => {
                let host = self.expect_value_string(args, 0);
                format!(
                    "__fp_host={}; __fp_kubectl_args=(); __fp_context=\"${{FP_K8S_CONTEXT[$__fp_host]:-}}\"; __fp_namespace=\"${{FP_K8S_NAMESPACE[$__fp_host]:-}}\"; __fp_pod=\"${{FP_K8S_POD[$__fp_host]}}\"; __fp_remote_path={}; [[ -n \"$__fp_context\" ]] && __fp_kubectl_args+=(--context \"$__fp_context\"); [[ -n \"$__fp_namespace\" ]] && __fp_kubectl_args+=(-n \"$__fp_namespace\"); kubectl cp \"${{__fp_kubectl_args[@]}}\" {} \"$__fp_pod:$__fp_remote_path\"",
                    self.render_word(host),
                    self.render_value(&args[2])?,
                    self.render_value(&args[1])?
                )
            }
            "winrm_copy" => format!(
                "winrm_pwsh {} copy {} {}",
                self.render_value(&args[0])?,
                self.render_value(&args[1])?,
                self.render_value(&args[2])?
            ),
            "render_template" => format!(
                "eval {}",
                shell_arg_quote(&format!(
                    "{} envsubst < {} > {}",
                    self.render_command_expr(self.expect_string_arg(args, 2)),
                    self.render_command_expr(self.expect_string_arg(args, 0)),
                    self.render_command_expr(self.expect_string_arg(args, 1))
                ))
            ),
            "remove_file" => format!("rm -f {}", self.render_value(&args[0])?),
            "rsync_ssh" => {
                let setup = self.emit_ssh_target_setup_inline(self.expect_value_string(args, 0));
                format!(
                    "{setup}; __fp_remote_path={}; rsync_cmd {} -- {} \"$__fp_target:$__fp_remote_path\"",
                    self.render_value(&args[3])?,
                    self.render_command_expr(self.expect_string_arg(args, 1)),
                    self.render_value(&args[2])?
                )
            }
            "runtime_fail" => format!("echo {} >&2; return 1", self.render_value(&args[0])?),
            "runtime_set_changed" => format!(
                "__fp_last_changed={}",
                if matches!(args.first(), Some(ValueExpr::Bool(BoolExpr::Literal(true)))) {
                    "1"
                } else {
                    "0"
                }
            ),
            other => {
                let args = args
                    .iter()
                    .map(|arg| self.render_value(arg))
                    .collect::<Result<Vec<_>, _>>()?
                    .join(" ");
                format!("{} {}", other, args)
            }
        })
    }

    fn render_runtime_validator(&self) -> String {
        let mut commands = BTreeSet::new();
        for name in &self.used_externs {
            let Some(external) = self.externs.get(name) else {
                continue;
            };
            for command in external.runtime_requirements(fp_shell_core::ScriptTarget::Bash) {
                commands.insert(command);
            }
        }
        if commands.is_empty() {
            return String::new();
        }
        let mut script = String::new();
        script.push_str("fp_validate_runtime() {\n");
        for command in commands {
            script.push_str(&format!(
                "  command -v {} >/dev/null 2>&1 || {{ echo \"missing required command: {}\" >&2; exit 1; }}\n",
                shell_arg_quote(&command),
                escape_double_quotes(&command)
            ));
        }
        script.push_str("}\n\nfp_validate_runtime\n\n");
        script
    }

    fn emit_ssh_target_setup_inline(&self, host: &StringExpr) -> String {
        format!(
            "__fp_host={}; __fp_address=\"${{FP_SSH_ADDRESS[$__fp_host]:-$__fp_host}}\"; __fp_user=\"${{FP_SSH_USER[$__fp_host]:-}}\"; __fp_port=\"${{FP_SSH_PORT[$__fp_host]:-}}\"; __fp_target=\"$__fp_address\"; if [[ -n \"$__fp_user\" ]]; then __fp_target=\"$__fp_user@$__fp_target\"; fi",
            self.render_word(host)
        )
    }

    fn expect_value_string<'b>(&self, args: &'b [ValueExpr], index: usize) -> &'b StringExpr {
        match &args[index] {
            ValueExpr::String(expr) => expr,
            _ => panic!("expected string argument"),
        }
    }

    fn expect_string_arg<'b>(&self, args: &'b [ValueExpr], index: usize) -> &'b StringExpr {
        self.expect_value_string(args, index)
    }
}

fn extract_match_case_string(pattern: &fp_core::ast::Pattern) -> Option<StringExpr> {
    match pattern.kind() {
        PatternKind::Wildcard(_) => None,
        PatternKind::Variant(variant) if variant.pattern.is_none() => match variant.name.kind() {
            ExprKind::Value(value) => match &**value {
                fp_core::ast::Value::String(text) => Some(StringExpr::Literal(text.value.clone())),
                _ => None,
            },
            ExprKind::Any(any) => any.downcast_ref::<ValueExpr>().and_then(|value| match value {
                ValueExpr::String(value) => Some(value.clone()),
                _ => None,
            }),
            _ => None,
        },
        _ => None,
    }
}

fn render_arithmetic(op: ArithmeticOp) -> &'static str {
    match op {
        ArithmeticOp::Add => "+",
        ArithmeticOp::Sub => "-",
        ArithmeticOp::Mul => "*",
        ArithmeticOp::Div => "/",
        ArithmeticOp::Mod => "%",
    }
}

fn render_comparison(op: ComparisonOp) -> &'static str {
    match op {
        ComparisonOp::Gt => "-gt",
        ComparisonOp::Lt => "-lt",
        ComparisonOp::Ge => "-ge",
        ComparisonOp::Le => "-le",
        ComparisonOp::Eq => "-eq",
        ComparisonOp::Ne => "-ne",
    }
}

fn render_string_comparison(op: StringComparisonOp) -> &'static str {
    match op {
        StringComparisonOp::Eq => "==",
        StringComparisonOp::Ne => "!=",
    }
}

fn shell_arg_quote(value: &str) -> String {
    format!("'{}'", value.replace('\'', "'\"'\"'"))
}

fn shell_case_quote(value: &str) -> String {
    value.chars().fold(String::new(), |mut out, ch| {
        match ch {
            '*' | '?' | '[' | ']' | '\\' | '(' | ')' | '|' => {
                out.push('\\');
                out.push(ch);
            }
            _ => out.push(ch),
        }
        out
    })
}

fn escape_double_quotes(value: &str) -> String {
    value.replace('\\', "\\\\").replace('"', "\\\"")
}


const BASH_RUNTIME_UTILITIES: &str = r#"
ssh_cmd() {
  ssh -o ControlMaster=auto -o ControlPersist=60 -o ControlPath="$SSH_CONTROL_PATH" -- "$@"
}

scp_cmd() {
  scp -o ControlMaster=auto -o ControlPersist=60 -o ControlPath="$SSH_CONTROL_PATH" -- "$@"
}

rsync_cmd() {
  rsync -e "ssh -o ControlMaster=auto -o ControlPersist=60 -o ControlPath=$SSH_CONTROL_PATH" "$@"
}

winrm_pwsh() {
  local host="$1"
  local mode="$2"
  local command="${3:-}"
  local source="${4:-}"
  local destination="${5:-}"
  local address="${FP_WINRM_ADDRESS[$host]}"
  local user="${FP_WINRM_USER[$host]}"
  local password="${FP_WINRM_PASSWORD[$host]:-}"
  local scheme="${FP_WINRM_SCHEME[$host]:-http}"
  local port="${FP_WINRM_PORT[$host]:-}"

  if [[ -z "$password" ]]; then
    echo "winrm password is required for non-interactive bash target: $host" >&2
    return 1
  fi

  FP_WINRM_ADDRESS="$address" \
  FP_WINRM_USER="$user" \
  FP_WINRM_PASSWORD="$password" \
  FP_WINRM_SCHEME="$scheme" \
  FP_WINRM_PORT="$port" \
  FP_WINRM_MODE="$mode" \
  FP_WINRM_COMMAND="$command" \
  FP_WINRM_SOURCE="$source" \
  FP_WINRM_DESTINATION="$destination" \
  pwsh -NoProfile -NonInteractive -Command '
$ErrorActionPreference = "Stop"
$sessionArgs = @{
    ComputerName = $env:FP_WINRM_ADDRESS
}
if ($env:FP_WINRM_PORT) {
    $sessionArgs.Port = [int]$env:FP_WINRM_PORT
}
$scheme = if ([string]::IsNullOrWhiteSpace($env:FP_WINRM_SCHEME)) {
    "http"
} else {
    $env:FP_WINRM_SCHEME.ToLowerInvariant()
}
switch ($scheme) {
    "http" {}
    "https" { $sessionArgs.UseSSL = $true }
    default { throw "unsupported winrm scheme: $($env:FP_WINRM_SCHEME)" }
}
$securePassword = ConvertTo-SecureString $env:FP_WINRM_PASSWORD -AsPlainText -Force
$credential = New-Object System.Management.Automation.PSCredential($env:FP_WINRM_USER, $securePassword)
$session = New-PSSession -Credential $credential @sessionArgs
try {
    switch ($env:FP_WINRM_MODE) {
        "run" {
            Invoke-Command -Session $session -ScriptBlock ([scriptblock]::Create($env:FP_WINRM_COMMAND))
        }
        "copy" {
            $remoteDestination = $env:FP_WINRM_DESTINATION
            $remoteDirectory = [System.IO.Path]::GetDirectoryName($remoteDestination)
            if ($remoteDirectory) {
                Invoke-Command -Session $session -ScriptBlock {
                    param([string]$Directory)
                    [System.IO.Directory]::CreateDirectory($Directory) | Out-Null
                } -ArgumentList $remoteDirectory
            }
            Copy-Item -ToSession $session -Path $env:FP_WINRM_SOURCE -Destination $remoteDestination -Force
        }
        default {
            throw "unsupported winrm mode: $($env:FP_WINRM_MODE)"
        }
    }
}
finally {
    if ($null -ne $session) {
        Remove-PSSession -Session $session
    }
}
'
}
"#;

#[cfg(test)]
mod tests {
    use super::*;
    use fp_core::ast::{Expr, ExprInvoke, ExprInvokeTarget, ExprKind, Name};
    use fp_shell_core::{
        ExternalFunction, InventoryHost, ScriptProgram, SshInventory, StringExpr, TransportKind,
        WinRmInventory,
    };
    use std::collections::HashMap;

    #[test]
    fn renders_ssh_dispatch() {
        let mut externs = HashMap::new();
        externs.insert(
            "ssh".to_string(),
            ExternalFunction {
                name: "ssh".to_string(),
                abi: "bash".to_string(),
                param_count: 2,
                command: Some("ssh".to_string()),
            },
        );
        let program = ScriptProgram {
            externs,
            items: vec![ScriptItem::Expr(Expr::new(ExprKind::Invoke(ExprInvoke {
                span: Default::default(),
                target: ExprInvokeTarget::Function(Name::ident("ssh")),
                args: vec![
                    Expr::any(ValueExpr::String(StringExpr::Literal("web-1".to_string()))),
                    Expr::any(ValueExpr::String(StringExpr::Literal("uptime".to_string()))),
                ],
                kwargs: Vec::new(),
            })))],
        };
        let mut hosts = HashMap::new();
        hosts.insert(
            "web-1".to_string(),
            InventoryHost {
                transport: TransportKind::Ssh,
                ssh: Some(SshInventory {
                    address: Some("10.0.0.11".to_string()),
                    user: Some("deploy".to_string()),
                    port: None,
                }),
                docker: None,
                kubectl: None,
                winrm: None,
            },
        );
        let inventory = ShellInventory {
            groups: HashMap::new(),
            hosts,
        };
        let script = BashTarget::new()
            .render(&program, &inventory)
            .expect("render should succeed");
        assert!(script.contains("FP_HOST_TRANSPORT['web-1']='ssh'"));
        assert!(script.contains("ssh_cmd \"$__fp_target\" 'uptime'"));
    }

    #[test]
    fn renders_winrm_dispatch_via_pwsh() {
        let mut externs = HashMap::new();
        externs.insert(
            "winrm_copy".to_string(),
            ExternalFunction {
                name: "winrm_copy".to_string(),
                abi: "bash".to_string(),
                param_count: 3,
                command: Some("pwsh".to_string()),
            },
        );
        let program = ScriptProgram {
            externs,
            items: vec![ScriptItem::Expr(Expr::new(ExprKind::Invoke(ExprInvoke {
                span: Default::default(),
                target: ExprInvokeTarget::Function(Name::ident("winrm_copy")),
                args: vec![
                    Expr::any(ValueExpr::String(StringExpr::Literal("win-1".to_string()))),
                    Expr::any(ValueExpr::String(StringExpr::Literal(
                        "artifact.zip".to_string(),
                    ))),
                    Expr::any(ValueExpr::String(StringExpr::Literal(
                        r"C:\Temp\artifact.zip".to_string(),
                    ))),
                ],
                kwargs: Vec::new(),
            })))],
        };
        let mut hosts = HashMap::new();
        hosts.insert(
            "win-1".to_string(),
            InventoryHost {
                transport: TransportKind::Winrm,
                ssh: None,
                docker: None,
                kubectl: None,
                winrm: Some(WinRmInventory {
                    address: "10.0.0.21".to_string(),
                    user: "Administrator".to_string(),
                    password: Some("secret".to_string()),
                    port: Some(5986),
                    scheme: Some("https".to_string()),
                }),
            },
        );
        let inventory = ShellInventory {
            groups: HashMap::new(),
            hosts,
        };
        let script = BashTarget::new()
            .render(&program, &inventory)
            .expect("render should succeed");
        assert!(script.contains("pwsh -NoProfile -NonInteractive -Command"));
        assert!(script.contains("FP_WINRM_SCHEME['win-1']='https'"));
        assert!(script.contains("winrm_pwsh 'win-1' copy 'artifact.zip' 'C:\\Temp\\artifact.zip'"));
        assert!(!script.contains("backend_copy_winrm"));
        assert!(!script.contains("evil-winrm"));
    }
}
