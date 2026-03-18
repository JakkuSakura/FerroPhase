use fp_shell_core::{
    ArithmeticOp, Block, BoolExpr, ComparisonOp, ConditionExpr, FunctionDef, HostExpr,
    OperationCopy, OperationGuards, OperationRsync, OperationRun, OperationTemplate, RsyncOptions,
    ScriptItem, ScriptProgram, ScriptRenderer, ShellInventory, Statement, StringComparisonOp,
    StringExpr, StringPart, TransportKind, ValueExpr,
};
use std::collections::HashMap;

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
    externs: HashMap<String, String>,
    lines: Vec<String>,
}

impl<'a> BashRenderer<'a> {
    fn new(inventory: &'a ShellInventory) -> Self {
        Self {
            inventory,
            externs: HashMap::new(),
            lines: Vec::new(),
        }
    }

    fn finish(self) -> String {
        let mut script = String::new();
        script.push_str("#!/usr/bin/env bash\n");
        script.push_str("set -euo pipefail\n\n");
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
        script.push_str(BASH_RUNTIME_UTILITIES);
        for line in self.lines {
            script.push_str(&line);
            script.push('\n');
        }
        script
    }

    fn render_program(&mut self, program: &ScriptProgram) -> Result<(), String> {
        self.externs = program
            .externs
            .iter()
            .map(|(name, decl)| (name.clone(), decl.abi.clone()))
            .collect();
        for item in &program.items {
            match item {
                ScriptItem::Function(def) => self.render_function(def)?,
                ScriptItem::Statement(statement) => self.render_statement(statement, 0)?,
            }
        }
        Ok(())
    }

    fn render_function(&mut self, def: &FunctionDef) -> Result<(), String> {
        self.push_line(0, &format!("{}() {{", def.name));
        for (index, param) in def.params.iter().enumerate() {
            self.push_line(1, &format!("local {}=\"${}\"", param, index + 1));
        }
        self.render_block(&def.body, 1)?;
        self.push_line(0, "}");
        self.push_line(0, "");
        Ok(())
    }

    fn render_block(&mut self, block: &Block, indent: usize) -> Result<(), String> {
        for statement in &block.statements {
            self.render_statement(statement, indent)?;
        }
        Ok(())
    }

    fn render_statement(&mut self, statement: &Statement, indent: usize) -> Result<(), String> {
        match statement {
            Statement::Run(op) => self.render_run(op, indent),
            Statement::Copy(op) => self.render_copy(op, indent),
            Statement::Template(op) => self.render_template(op, indent),
            Statement::Rsync(op) => self.render_rsync(op, indent),
            Statement::If(stmt) => {
                self.push_line(
                    indent,
                    &format!("if {}; then", self.render_condition(&stmt.condition)?),
                );
                self.render_block(&stmt.then_block, indent + 1)?;
                if let Some(else_block) = &stmt.else_block {
                    self.push_line(indent, "else");
                    self.render_block(else_block, indent + 1)?;
                }
                self.push_line(indent, "fi");
                Ok(())
            }
            Statement::While(stmt) => {
                self.push_line(
                    indent,
                    &format!("while {}; do", self.render_condition(&stmt.condition)?),
                );
                self.render_block(&stmt.body, indent + 1)?;
                self.push_line(indent, "done");
                Ok(())
            }
            Statement::ForEach(stmt) => {
                let values = stmt
                    .values
                    .iter()
                    .map(|value| self.render_word(value))
                    .collect::<Vec<_>>()
                    .join(" ");
                self.push_line(indent, &format!("for {} in {}; do", stmt.binding, values));
                self.render_block(&stmt.body, indent + 1)?;
                self.push_line(indent, "done");
                Ok(())
            }
            Statement::Let(stmt) => {
                let value = self.render_value(&stmt.value)?;
                self.push_line(indent, &format!("local {}={}", stmt.name, value));
                Ok(())
            }
            Statement::Invoke(stmt) => self.render_invoke_statement(&stmt.name, &stmt.args, indent),
        }
    }

    fn render_run(&mut self, op: &OperationRun, indent: usize) -> Result<(), String> {
        for host in &op.hosts {
            self.render_guarded(indent, &op.guards, |renderer, guard_indent| {
                match host {
                    HostExpr::Localhost => {
                        renderer.push_line(guard_indent, &renderer.render_command_expr(&op.command))
                    }
                    HostExpr::Selector(selector) => renderer.push_line(
                        guard_indent,
                        &format!(
                            "run_host {} {}",
                            renderer.render_word(selector),
                            shell_arg_quote(&renderer.render_command_expr(&op.command))
                        ),
                    ),
                }
                renderer.push_line(guard_indent, "__fp_changed=1");
                Ok(())
            })?;
        }
        Ok(())
    }

    fn render_copy(&mut self, op: &OperationCopy, indent: usize) -> Result<(), String> {
        for host in &op.hosts {
            self.render_guarded(indent, &op.guards, |renderer, guard_indent| {
                match host {
                    HostExpr::Localhost => renderer.push_line(
                        guard_indent,
                        &format!(
                            "cp -- {} {}",
                            renderer.render_word(&op.source),
                            renderer.render_word(&op.destination)
                        ),
                    ),
                    HostExpr::Selector(selector) => renderer.push_line(
                        guard_indent,
                        &format!(
                            "copy_host {} {} {}",
                            renderer.render_word(selector),
                            renderer.render_word(&op.source),
                            renderer.render_word(&op.destination)
                        ),
                    ),
                }
                renderer.push_line(guard_indent, "__fp_changed=1");
                Ok(())
            })?;
        }
        Ok(())
    }

    fn render_template(&mut self, op: &OperationTemplate, indent: usize) -> Result<(), String> {
        for host in &op.hosts {
            self.render_guarded(indent, &op.guards, |renderer, guard_indent| {
                let mut vars = String::new();
                for (key, value) in &op.vars {
                    vars.push_str(&format!("{}={} ", key, renderer.render_word(value)));
                }
                match host {
                    HostExpr::Localhost => renderer.push_line(
                        guard_indent,
                        &format!(
                            "{}envsubst < {} > {}",
                            vars,
                            renderer.render_word(&op.source),
                            renderer.render_word(&op.destination)
                        ),
                    ),
                    HostExpr::Selector(selector) => renderer.push_line(
                        guard_indent,
                        &format!(
                            "template_host {} {} {} {}",
                            renderer.render_word(selector),
                            renderer.render_word(&op.source),
                            renderer.render_word(&op.destination),
                            shell_arg_quote(vars.trim())
                        ),
                    ),
                }
                renderer.push_line(guard_indent, "__fp_changed=1");
                Ok(())
            })?;
        }
        Ok(())
    }

    fn render_rsync(&mut self, op: &OperationRsync, indent: usize) -> Result<(), String> {
        let flags = rsync_flags(op.options);
        for host in &op.hosts {
            self.render_guarded(indent, &op.guards, |renderer, guard_indent| {
                match host {
                    HostExpr::Localhost => renderer.push_line(
                        guard_indent,
                        &format!(
                            "rsync {} -- {} {}",
                            flags,
                            renderer.render_word(&op.source),
                            renderer.render_word(&op.destination)
                        ),
                    ),
                    HostExpr::Selector(selector) => renderer.push_line(
                        guard_indent,
                        &format!(
                            "rsync_host {} {} {} {}",
                            renderer.render_word(selector),
                            shell_arg_quote(&flags),
                            renderer.render_word(&op.source),
                            renderer.render_word(&op.destination)
                        ),
                    ),
                }
                renderer.push_line(guard_indent, "__fp_changed=1");
                Ok(())
            })?;
        }
        Ok(())
    }

    fn render_invoke_statement(
        &mut self,
        name: &str,
        args: &[ValueExpr],
        indent: usize,
    ) -> Result<(), String> {
        if self.externs.get(name).map(String::as_str) == Some("bash") {
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

    fn render_guarded<F>(
        &mut self,
        indent: usize,
        guards: &OperationGuards,
        emit: F,
    ) -> Result<(), String>
    where
        F: FnOnce(&mut Self, usize) -> Result<(), String>,
    {
        self.push_line(indent, "__fp_changed=0");
        let mut condition_parts = Vec::new();
        if let Some(command) = &guards.only_if {
            condition_parts.push(format!("({})", self.render_command_expr(command)));
        }
        if let Some(command) = &guards.unless {
            condition_parts.push(format!("! ({})", self.render_command_expr(command)));
        }
        if let Some(path) = &guards.creates {
            condition_parts.push(format!("[ ! -e {} ]", self.render_word(path)));
        }
        if let Some(path) = &guards.removes {
            condition_parts.push(format!("[ -e {} ]", self.render_word(path)));
        }
        if condition_parts.is_empty() {
            emit(self, indent)?;
        } else {
            self.push_line(
                indent,
                &format!("if {}; then", condition_parts.join(" && ")),
            );
            emit(self, indent + 1)?;
            self.push_line(indent, "fi");
        }
        self.push_line(indent, "__fp_last_changed=\"$__fp_changed\"");
        Ok(())
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

    fn push_line(&mut self, indent: usize, text: &str) {
        self.lines
            .push(format!("{}{}", "    ".repeat(indent), text));
    }

    fn render_call(&self, name: &str, args: &[ValueExpr]) -> String {
        if self.externs.get(name).map(String::as_str) == Some("bash") {
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
            "shell_host_transport" => {
                let host = self.expect_string_arg(args, 0);
                format!(
                    "printf '%s\\n' \"${{FP_HOST_TRANSPORT[{}]:-ssh}}\"",
                    self.render_word(host)
                )
            }
            "shell_temp_path" => "mktemp".to_string(),
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
            "shell_run_local" => format!("bash -lc {}", self.render_value(&args[0])?),
            "shell_run_ssh" => {
                let setup = self.emit_ssh_target_setup_inline(self.expect_value_string(args, 0));
                format!(
                    "{setup}; if [[ -n \"$__fp_port\" ]]; then ssh_cmd -p \"$__fp_port\" \"$__fp_target\" {}; else ssh_cmd \"$__fp_target\" {}; fi",
                    self.render_value(&args[1])?,
                    self.render_value(&args[1])?
                )
            }
            "shell_run_docker" => {
                let host = self.expect_value_string(args, 0);
                format!(
                    "__fp_host={}; __fp_container=\"${{FP_DOCKER_CONTAINER[$__fp_host]}}\"; __fp_user=\"${{FP_DOCKER_USER[$__fp_host]:-}}\"; if [[ -n \"$__fp_user\" ]]; then docker exec --user \"$__fp_user\" \"$__fp_container\" sh -lc {}; else docker exec \"$__fp_container\" sh -lc {}; fi",
                    self.render_word(host),
                    self.render_value(&args[1])?,
                    self.render_value(&args[1])?
                )
            }
            "shell_run_kubectl" => {
                let host = self.expect_value_string(args, 0);
                format!(
                    "__fp_host={}; __fp_kubectl_args=(); __fp_context=\"${{FP_K8S_CONTEXT[$__fp_host]:-}}\"; __fp_namespace=\"${{FP_K8S_NAMESPACE[$__fp_host]:-}}\"; __fp_container=\"${{FP_K8S_CONTAINER[$__fp_host]:-}}\"; __fp_pod=\"${{FP_K8S_POD[$__fp_host]}}\"; [[ -n \"$__fp_context\" ]] && __fp_kubectl_args+=(--context \"$__fp_context\"); [[ -n \"$__fp_namespace\" ]] && __fp_kubectl_args+=(-n \"$__fp_namespace\"); __fp_kubectl_args+=(exec); [[ -n \"$__fp_container\" ]] && __fp_kubectl_args+=(-c \"$__fp_container\"); __fp_kubectl_args+=(\"$__fp_pod\" -- sh -lc {}); kubectl \"${{__fp_kubectl_args[@]}}\"",
                    self.render_word(host),
                    self.render_value(&args[1])?
                )
            }
            "shell_run_winrm" => format!(
                "winrm_pwsh {} run {}",
                self.render_value(&args[0])?,
                self.render_value(&args[1])?
            ),
            "shell_copy_local" => format!(
                "cp -- {} {}",
                self.render_value(&args[0])?,
                self.render_value(&args[1])?
            ),
            "shell_copy_ssh" => {
                let setup = self.emit_ssh_target_setup_inline(self.expect_value_string(args, 0));
                let src = self.render_value(&args[1])?;
                let dest = self.render_value(&args[2])?;
                format!(
                    "{setup}; __fp_remote_path={dest}; if [[ -n \"$__fp_port\" ]]; then scp_cmd -P \"$__fp_port\" {src} \"$__fp_target:$__fp_remote_path\"; else scp_cmd {src} \"$__fp_target:$__fp_remote_path\"; fi"
                )
            }
            "shell_copy_docker" => {
                let host = self.expect_value_string(args, 0);
                format!(
                    "__fp_host={}; __fp_container=\"${{FP_DOCKER_CONTAINER[$__fp_host]}}\"; __fp_remote_path={}; docker cp {} \"$__fp_container:$__fp_remote_path\"",
                    self.render_word(host),
                    self.render_value(&args[2])?,
                    self.render_value(&args[1])?
                )
            }
            "shell_copy_kubectl" => {
                let host = self.expect_value_string(args, 0);
                format!(
                    "__fp_host={}; __fp_kubectl_args=(); __fp_context=\"${{FP_K8S_CONTEXT[$__fp_host]:-}}\"; __fp_namespace=\"${{FP_K8S_NAMESPACE[$__fp_host]:-}}\"; __fp_pod=\"${{FP_K8S_POD[$__fp_host]}}\"; __fp_remote_path={}; [[ -n \"$__fp_context\" ]] && __fp_kubectl_args+=(--context \"$__fp_context\"); [[ -n \"$__fp_namespace\" ]] && __fp_kubectl_args+=(-n \"$__fp_namespace\"); kubectl cp \"${{__fp_kubectl_args[@]}}\" {} \"$__fp_pod:$__fp_remote_path\"",
                    self.render_word(host),
                    self.render_value(&args[2])?,
                    self.render_value(&args[1])?
                )
            }
            "shell_copy_winrm" => format!(
                "winrm_pwsh {} copy {} {}",
                self.render_value(&args[0])?,
                self.render_value(&args[1])?,
                self.render_value(&args[2])?
            ),
            "shell_render_template" => format!(
                "eval {}",
                shell_arg_quote(&format!(
                    "{} envsubst < {} > {}",
                    self.render_command_expr(self.expect_string_arg(args, 2)),
                    self.render_command_expr(self.expect_string_arg(args, 0)),
                    self.render_command_expr(self.expect_string_arg(args, 1))
                ))
            ),
            "shell_remove_file" => format!("rm -f {}", self.render_value(&args[0])?),
            "shell_rsync_ssh" => {
                let setup = self.emit_ssh_target_setup_inline(self.expect_value_string(args, 0));
                format!(
                    "{setup}; __fp_remote_path={}; rsync_cmd {} -- {} \"$__fp_target:$__fp_remote_path\"",
                    self.render_value(&args[3])?,
                    self.render_command_expr(self.expect_string_arg(args, 1)),
                    self.render_value(&args[2])?
                )
            }
            "shell_fail" => format!("echo {} >&2; return 1", self.render_value(&args[0])?),
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

fn escape_double_quotes(value: &str) -> String {
    value.replace('\\', "\\\\").replace('"', "\\\"")
}

fn rsync_flags(options: RsyncOptions) -> String {
    let mut short = String::new();
    if options.archive {
        short.push('a');
    }
    if options.compress {
        short.push('z');
    }
    let mut flags = if short.is_empty() {
        String::new()
    } else {
        format!("-{}", short)
    };
    if options.delete {
        if !flags.is_empty() {
            flags.push(' ');
        }
        flags.push_str("--delete");
    }
    if options.checksum {
        if !flags.is_empty() {
            flags.push(' ');
        }
        flags.push_str("--checksum");
    }
    flags
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
    use fp_shell_core::{
        ExternalFunction, InventoryHost, InvokeStmt, ScriptProgram, SshInventory, Statement,
        StringExpr, TransportKind, WinRmInventory,
    };
    use std::collections::HashMap;

    #[test]
    fn renders_ssh_dispatch() {
        let program = ScriptProgram {
            externs: HashMap::new(),
            items: vec![ScriptItem::Statement(Statement::Run(OperationRun {
                hosts: vec![HostExpr::Selector(StringExpr::Literal("web-1".to_string()))],
                command: StringExpr::Literal("uptime".to_string()),
                cwd: None,
                sudo: false,
                guards: OperationGuards::default(),
            }))],
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
        assert!(script.contains("run_host 'web-1' 'uptime'"));
    }

    #[test]
    fn renders_winrm_dispatch_via_pwsh() {
        let mut externs = HashMap::new();
        externs.insert(
            "shell_copy_winrm".to_string(),
            ExternalFunction {
                name: "shell_copy_winrm".to_string(),
                abi: "bash".to_string(),
            },
        );
        let program = ScriptProgram {
            externs,
            items: vec![ScriptItem::Statement(Statement::Invoke(InvokeStmt {
                name: "shell_copy_winrm".to_string(),
                args: vec![
                    ValueExpr::String(StringExpr::Literal("win-1".to_string())),
                    ValueExpr::String(StringExpr::Literal("artifact.zip".to_string())),
                    ValueExpr::String(StringExpr::Literal(r"C:\Temp\artifact.zip".to_string())),
                ],
            }))],
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
