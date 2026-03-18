use fp_core::ast::{
    BlockStmt, Expr, ExprBlock, ExprInvokeTarget, ExprKind, ExprMatch, ItemDefFunction, PatternKind,
};
use fp_shell_core::{
    ArithmeticOp, BoolExpr, ComparisonOp, ConditionExpr, ExternalFunction, ScriptItem,
    ScriptProgram, ScriptRenderer, ShellInventory, StringComparisonOp, StringExpr, StringPart,
    TransportKind, ValueExpr,
};
use std::collections::{BTreeSet, HashMap};

pub struct PowerShellTarget;

impl PowerShellTarget {
    pub fn new() -> Self {
        Self
    }
}

impl Default for PowerShellTarget {
    fn default() -> Self {
        Self::new()
    }
}

impl ScriptRenderer for PowerShellTarget {
    type Error = String;

    fn render(
        &self,
        program: &ScriptProgram,
        inventory: &ShellInventory,
    ) -> Result<String, Self::Error> {
        let mut renderer = PowerShellRenderer::new(inventory);
        renderer.render_program(program)?;
        Ok(renderer.finish())
    }
}

struct PowerShellRenderer<'a> {
    inventory: &'a ShellInventory,
    externs: HashMap<String, ExternalFunction>,
    used_externs: BTreeSet<String>,
    lines: Vec<String>,
}

impl<'a> PowerShellRenderer<'a> {
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
        script.push_str("Set-StrictMode -Version Latest\nSet-PSDebug -Trace 1\n$ErrorActionPreference = 'Stop'\n$script:fpLastChanged = $false\n\n");
        script.push_str("$script:FpHosts = @{}\n");
        for (name, host) in &self.inventory.hosts {
            let transport = match host.transport {
                TransportKind::Local => "local",
                TransportKind::Ssh => "ssh",
                TransportKind::Docker => "docker",
                TransportKind::Kubectl => "kubectl",
                TransportKind::Winrm => "winrm",
            };
            script.push_str(&format!(
                "$script:FpHosts[{0}] = @{{ transport = {1}",
                ps_string(name),
                ps_string(transport)
            ));
            if let Some(ssh) = &host.ssh {
                if let Some(address) = &ssh.address {
                    script.push_str(&format!("; address = {}", ps_string(address)));
                }
                if let Some(user) = &ssh.user {
                    script.push_str(&format!("; user = {}", ps_string(user)));
                }
                if let Some(port) = ssh.port {
                    script.push_str(&format!("; port = {}", port));
                }
            }
            if let Some(docker) = &host.docker {
                script.push_str(&format!("; container = {}", ps_string(&docker.container)));
                if let Some(user) = &docker.user {
                    script.push_str(&format!("; user = {}", ps_string(user)));
                }
            }
            if let Some(kubectl) = &host.kubectl {
                script.push_str(&format!("; pod = {}", ps_string(&kubectl.pod)));
                if let Some(namespace) = &kubectl.namespace {
                    script.push_str(&format!("; namespace = {}", ps_string(namespace)));
                }
                if let Some(container) = &kubectl.container {
                    script.push_str(&format!("; container = {}", ps_string(container)));
                }
                if let Some(context) = &kubectl.context {
                    script.push_str(&format!("; context = {}", ps_string(context)));
                }
            }
            if let Some(winrm) = &host.winrm {
                script.push_str(&format!("; address = {}", ps_string(&winrm.address)));
                script.push_str(&format!("; user = {}", ps_string(&winrm.user)));
                if let Some(password) = &winrm.password {
                    script.push_str(&format!("; password = {}", ps_string(password)));
                }
                if let Some(port) = winrm.port {
                    script.push_str(&format!("; port = {}", port));
                }
                if let Some(scheme) = &winrm.scheme {
                    script.push_str(&format!("; scheme = {}", ps_string(scheme)));
                }
            }
            script.push_str(" }\n");
        }
        script.push_str(&self.render_runtime_validator());
        script.push_str(POWERSHELL_RUNTIME_UTILITIES);
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
        let params = def
            .sig
            .params
            .iter()
            .map(|param| format!("[string]${}", param.name))
            .collect::<Vec<_>>()
            .join(", ");
        self.push_line(0, &format!("function {} {{", def.name));
        if !params.is_empty() {
            self.push_line(1, &format!("param({})", params));
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
            .ok_or_else(|| "powershell match requires scrutinee".to_string())?;
        let ValueExpr::String(scrutinee) = self.extract_value_expr(scrutinee)? else {
            return Err("powershell match scrutinee must be lowered string".to_string());
        };
        self.push_line(
            indent,
            &format!("switch -Exact ({}) {{", self.render_word(&scrutinee)),
        );
        for case in &expr_match.cases {
            let label = match case.pat.as_ref().and_then(|pat| extract_match_case_string(pat)) {
                Some(pattern) => self.render_word(&pattern),
                None => "default".to_string(),
            };
            if case
                .pat
                .as_ref()
                .and_then(|pat| extract_match_case_string(pat))
                .is_some()
            {
                self.push_line(indent + 1, &format!("{} {{", label));
            } else {
                self.push_line(indent + 1, "default {");
            }
            self.render_expr_statement(&case.body, indent + 2)?;
            self.push_line(indent + 1, "}");
        }
        self.push_line(indent, "}");
        Ok(())
    }


    fn render_invoke_statement(
        &mut self,
        name: &str,
        args: &[ValueExpr],
        indent: usize,
    ) -> Result<(), String> {
        if self.externs.get(name).map(|decl| decl.abi.as_str()) == Some("pwsh") {
            if self.render_pwsh_extern_statement(name, args, indent)? {
                return Ok(());
            }
        }
        let args = args
            .iter()
            .map(|arg| self.render_value(arg))
            .collect::<Vec<_>>()
            .join(" ");
        self.push_line(indent, &format!("{} {}", name, args));
        Ok(())
    }

    fn render_pwsh_extern_statement(
        &mut self,
        name: &str,
        args: &[ValueExpr],
        indent: usize,
    ) -> Result<bool, String> {
        match name {
            "invoke_expression" => {
                self.push_line(
                    indent,
                    &self.render_command_expr(self.expect_string_arg(args, 0)),
                );
            }
            "ssh" => {
                let host = self.expect_string_arg(args, 0).clone();
                let command = self.expect_string_arg(args, 1).clone();
                self.emit_ssh_entry(indent, &host);
                self.push_line(
                    indent,
                    "$__fpTarget = if ($__fpEntry.user) { \"$($__fpEntry.user)@$($__fpAddress)\" } else { $__fpAddress }",
                );
                self.push_line(indent, "if ($__fpEntry.port) {");
                self.push_line(
                    indent + 1,
                    &format!(
                        "& ssh -p $__fpEntry.port $__fpTarget {}",
                        self.render_word(&command)
                    ),
                );
                self.push_line(indent, "} else {");
                self.push_line(
                    indent + 1,
                    &format!("& ssh $__fpTarget {}", self.render_word(&command)),
                );
                self.push_line(indent, "}");
            }
            "docker_exec" => {
                let host = self.expect_string_arg(args, 0).clone();
                let command = self.expect_string_arg(args, 1).clone();
                self.emit_host_entry(indent, &host);
                self.push_line(indent, "if ($__fpEntry.user) {");
                self.push_line(
                    indent + 1,
                    &format!(
                        "& docker exec --user $__fpEntry.user $__fpEntry.container sh -lc {}",
                        self.render_word(&command)
                    ),
                );
                self.push_line(indent, "} else {");
                self.push_line(
                    indent + 1,
                    &format!(
                        "& docker exec $__fpEntry.container sh -lc {}",
                        self.render_word(&command)
                    ),
                );
                self.push_line(indent, "}");
            }
            "kubectl_exec" => {
                let host = self.expect_string_arg(args, 0).clone();
                let command = self.expect_string_arg(args, 1).clone();
                self.emit_host_entry(indent, &host);
                self.push_line(indent, "$__fpArgs = @()");
                self.push_line(
                    indent,
                    "if ($__fpEntry.context) { $__fpArgs += @('--context', $__fpEntry.context) }",
                );
                self.push_line(
                    indent,
                    "if ($__fpEntry.namespace) { $__fpArgs += @('-n', $__fpEntry.namespace) }",
                );
                self.push_line(indent, "$__fpArgs += 'exec'");
                self.push_line(
                    indent,
                    "if ($__fpEntry.container) { $__fpArgs += @('-c', $__fpEntry.container) }",
                );
                self.push_line(
                    indent,
                    &format!(
                        "$__fpArgs += @($__fpEntry.pod, '--', 'sh', '-lc', {})",
                        self.render_word(&command)
                    ),
                );
                self.push_line(indent, "& kubectl @__fpArgs");
            }
            "winrm_run" => {
                let host = self.expect_string_arg(args, 0).clone();
                let command = self.expect_string_arg(args, 1).clone();
                self.emit_host_entry(indent, &host);
                self.emit_winrm_session_setup(indent);
                self.push_line(indent, "try {");
                self.push_line(
                    indent + 1,
                    &format!(
                        "Invoke-Command -Session $__fpSession -ScriptBlock ([scriptblock]::Create({}))",
                        self.render_word(&command)
                    ),
                );
                self.push_line(indent, "} finally {");
                self.push_line(indent + 1, "Remove-PSSession -Session $__fpSession");
                self.push_line(indent, "}");
            }
            "copy_item" => {
                self.push_line(
                    indent,
                    &format!(
                        "Copy-Item -Force {} {}",
                        self.render_word(self.expect_string_arg(args, 0)),
                        self.render_word(self.expect_string_arg(args, 1))
                    ),
                );
            }
            "scp" => {
                let host = self.expect_string_arg(args, 0).clone();
                let source = self.expect_string_arg(args, 1).clone();
                let destination = self.expect_string_arg(args, 2).clone();
                self.emit_ssh_entry(indent, &host);
                self.push_line(
                    indent,
                    &format!("$__fpDestination = {}", self.render_word(&destination)),
                );
                self.push_line(
                    indent,
                    "$__fpTarget = if ($__fpEntry.user) { \"$($__fpEntry.user)@$($__fpAddress):$__fpDestination\" } else { \"$($__fpAddress):$__fpDestination\" }",
                );
                self.push_line(indent, "if ($__fpEntry.port) {");
                self.push_line(
                    indent + 1,
                    &format!(
                        "& scp -P $__fpEntry.port {} $__fpTarget",
                        self.render_word(&source)
                    ),
                );
                self.push_line(indent, "} else {");
                self.push_line(
                    indent + 1,
                    &format!("& scp {} $__fpTarget", self.render_word(&source)),
                );
                self.push_line(indent, "}");
            }
            "docker_cp" => {
                let host = self.expect_string_arg(args, 0).clone();
                let source = self.expect_string_arg(args, 1).clone();
                let destination = self.expect_string_arg(args, 2).clone();
                self.emit_host_entry(indent, &host);
                self.push_line(
                    indent,
                    &format!(
                        "& docker cp {} \"$($__fpEntry.container):{}\"",
                        self.render_word(&source),
                        self.render_command_expr(&destination)
                    ),
                );
            }
            "kubectl_cp" => {
                let host = self.expect_string_arg(args, 0).clone();
                let source = self.expect_string_arg(args, 1).clone();
                let destination = self.expect_string_arg(args, 2).clone();
                self.emit_host_entry(indent, &host);
                self.push_line(indent, "$__fpArgs = @()");
                self.push_line(
                    indent,
                    "if ($__fpEntry.context) { $__fpArgs += @('--context', $__fpEntry.context) }",
                );
                self.push_line(
                    indent,
                    "if ($__fpEntry.namespace) { $__fpArgs += @('-n', $__fpEntry.namespace) }",
                );
                self.push_line(
                    indent,
                    &format!(
                        "& kubectl cp @__fpArgs {} \"$($__fpEntry.pod):{}\"",
                        self.render_word(&source),
                        self.render_command_expr(&destination)
                    ),
                );
            }
            "winrm_copy" => {
                let host = self.expect_string_arg(args, 0).clone();
                let source = self.expect_string_arg(args, 1).clone();
                let destination = self.expect_string_arg(args, 2).clone();
                self.emit_host_entry(indent, &host);
                self.emit_winrm_session_setup(indent);
                self.push_line(
                    indent,
                    &format!("$__fpDestination = {}", self.render_word(&destination)),
                );
                self.push_line(
                    indent,
                    "$__fpDirectory = [System.IO.Path]::GetDirectoryName($__fpDestination)",
                );
                self.push_line(indent, "try {");
                self.push_line(indent + 1, "if ($__fpDirectory) {");
                self.push_line(indent + 2, "Invoke-Command -Session $__fpSession -ScriptBlock { param([string]$Directory) [System.IO.Directory]::CreateDirectory($Directory) | Out-Null } -ArgumentList $__fpDirectory");
                self.push_line(indent + 1, "}");
                self.push_line(
                    indent + 1,
                    &format!("Copy-Item -ToSession $__fpSession -Path {} -Destination $__fpDestination -Force", self.render_word(&source)),
                );
                self.push_line(indent, "} finally {");
                self.push_line(indent + 1, "Remove-PSSession -Session $__fpSession");
                self.push_line(indent, "}");
            }
            "render_template" => {
                let source = self.expect_string_arg(args, 0).clone();
                let destination = self.expect_string_arg(args, 1).clone();
                let vars = self.expect_string_arg(args, 2).clone();
                self.push_line(
                    indent,
                    &format!(
                        "$__fpContent = Get-Content -Raw {}",
                        self.render_word(&source)
                    ),
                );
                self.push_line(
                    indent,
                    &format!(
                        "foreach ($pair in ({} -split ';')) {{",
                        self.render_word(&vars)
                    ),
                );
                self.push_line(indent + 1, "if ($pair) {");
                self.push_line(indent + 2, "$name, $value = $pair -split '=', 2");
                self.push_line(
                    indent + 2,
                    "$__fpContent = $__fpContent.Replace(\"`${$name}\", $value)",
                );
                self.push_line(indent + 1, "}");
                self.push_line(indent, "}");
                self.push_line(
                    indent,
                    &format!(
                        "Set-Content -Path {} -Value $__fpContent",
                        self.render_word(&destination)
                    ),
                );
            }
            "remove_file" => {
                self.push_line(
                    indent,
                    &format!(
                        "Remove-Item -Force {} -ErrorAction SilentlyContinue",
                        self.render_word(self.expect_string_arg(args, 0))
                    ),
                );
            }
            "rsync_ssh" => {
                let host = self.expect_string_arg(args, 0).clone();
                let flags = self.expect_string_arg(args, 1).clone();
                let source = self.expect_string_arg(args, 2).clone();
                let destination = self.expect_string_arg(args, 3).clone();
                self.emit_ssh_entry(indent, &host);
                self.push_line(
                    indent,
                    &format!("$__fpDestination = {}", self.render_word(&destination)),
                );
                self.push_line(
                    indent,
                    "$__fpTarget = if ($__fpEntry.user) { \"$($__fpEntry.user)@$($__fpAddress):$__fpDestination\" } else { \"$($__fpAddress):$__fpDestination\" }",
                );
                self.push_line(
                    indent,
                    &format!(
                        "& rsync {} -- {} $__fpTarget",
                        self.render_word(&flags),
                        self.render_word(&source)
                    ),
                );
            }
            "runtime_fail" => {
                self.push_line(
                    indent,
                    &format!(
                        "throw {}",
                        self.render_word(self.expect_string_arg(args, 0))
                    ),
                );
            }
            "runtime_set_changed" => {
                self.push_line(
                    indent,
                    &format!(
                        "$script:fpLastChanged = {}",
                        match args.first() {
                            Some(ValueExpr::Bool(BoolExpr::Literal(true))) => "$true",
                            _ => "$false",
                        }
                    ),
                );
            }
            _ => return Ok(false),
        }
        Ok(true)
    }

    fn render_block_stmt(&mut self, statement: &BlockStmt, indent: usize) -> Result<(), String> {
        match statement {
            BlockStmt::Expr(expr) => self.render_expr_statement(&expr.expr, indent),
            BlockStmt::Let(stmt) => {
                let Some(name) = stmt.pat.as_ident() else {
                    return Err(
                        "powershell renderer only supports identifier let bindings".to_string()
                    );
                };
                let Some(init) = &stmt.init else {
                    return Ok(());
                };
                let value = self.render_expr_as_value(init)?;
                self.push_line(indent, &format!("${} = {}", name, value));
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
                    &format!("if ({}) {{", self.render_expr_as_condition(&expr_if.cond)?),
                );
                self.render_expr_statement(&expr_if.then, indent + 1)?;
                if let Some(elze) = &expr_if.elze {
                    self.push_line(indent, "} else {");
                    self.render_expr_statement(elze, indent + 1)?;
                }
                self.push_line(indent, "}");
                Ok(())
            }
            ExprKind::While(expr_while) => {
                self.push_line(
                    indent,
                    &format!(
                        "while ({}) {{",
                        self.render_expr_as_condition(&expr_while.cond)?
                    ),
                );
                self.render_expr_statement(&expr_while.body, indent + 1)?;
                self.push_line(indent, "}");
                Ok(())
            }
            ExprKind::For(expr_for) => {
                let PatternKind::Ident(pattern) = expr_for.pat.kind() else {
                    return Err(
                        "powershell renderer only supports identifier for bindings".to_string()
                    );
                };
                let ValueExpr::StringList(values) = self.extract_value_expr(&expr_for.iter)? else {
                    return Err(
                        "powershell renderer only supports string-list for iterables".to_string(),
                    );
                };
                let values = values
                    .iter()
                    .map(|value| self.render_word(value))
                    .collect::<Vec<_>>()
                    .join(", ");
                self.push_line(
                    indent,
                    &format!("foreach (${} in @({})) {{", pattern.ident, values),
                );
                self.render_expr_statement(&expr_for.body, indent + 1)?;
                self.push_line(indent, "}");
                Ok(())
            }
            ExprKind::Let(expr_let) => {
                let Some(name) = expr_let.pat.as_ident() else {
                    return Err(
                        "powershell renderer only supports identifier let bindings".to_string()
                    );
                };
                let value = self.render_expr_as_value(&expr_let.expr)?;
                self.push_line(indent, &format!("${} = {}", name, value));
                Ok(())
            }
            ExprKind::Invoke(invoke) => {
                let ExprInvokeTarget::Function(name) = &invoke.target else {
                    return Err(
                        "powershell renderer only supports function invocation targets".to_string(),
                    );
                };
                let Some(ident) = name.as_ident() else {
                    return Err(
                        "powershell renderer only supports identifier invocation targets".to_string(),
                    );
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
            return Err("powershell renderer expected lowered shell condition".to_string());
        };
        let Some(condition) = any.downcast_ref::<ConditionExpr>() else {
            return Err("powershell renderer expected shell condition payload".to_string());
        };
        Ok(self.render_condition(condition))
    }

    fn render_expr_as_value(&self, expr: &Expr) -> Result<String, String> {
        Ok(self.render_value(&self.extract_value_expr(expr)?))
    }

    fn extract_value_expr(&self, expr: &Expr) -> Result<ValueExpr, String> {
        let ExprKind::Any(any) = expr.kind() else {
            return Err("powershell renderer expected lowered shell value".to_string());
        };
        any.downcast_ref::<ValueExpr>()
            .cloned()
            .ok_or_else(|| "powershell renderer expected shell value payload".to_string())
    }

    fn render_condition(&self, condition: &ConditionExpr) -> String {
        match condition {
            ConditionExpr::Bool(BoolExpr::Literal(true)) => "$true".to_string(),
            ConditionExpr::Bool(BoolExpr::Literal(false)) => "$false".to_string(),
            ConditionExpr::Bool(BoolExpr::Variable(name)) => format!("${}", name),
            ConditionExpr::Bool(BoolExpr::IntComparison { lhs, op, rhs }) => {
                format!(
                    "{} {} {}",
                    self.render_int(lhs),
                    render_comparison(*op),
                    self.render_int(rhs)
                )
            }
            ConditionExpr::Bool(BoolExpr::StringComparison { lhs, op, rhs }) => {
                format!(
                    "{} {} {}",
                    self.render_word(lhs),
                    render_string_comparison(*op),
                    self.render_word(rhs)
                )
            }
            ConditionExpr::Command(command) => self.render_command_expr(command),
            ConditionExpr::StringTruthy(expr) => format!("({} -eq 'true')", self.render_word(expr)),
        }
    }

    fn render_value(&self, value: &ValueExpr) -> String {
        match value {
            ValueExpr::String(expr) => self.render_word(expr),
            ValueExpr::Int(expr) => self.render_int(expr),
            ValueExpr::Bool(BoolExpr::Literal(value)) => {
                if *value {
                    "$true".to_string()
                } else {
                    "$false".to_string()
                }
            }
            ValueExpr::Bool(BoolExpr::Variable(name)) => format!("${}", name),
            ValueExpr::Bool(BoolExpr::IntComparison { lhs, op, rhs }) => {
                format!(
                    "({} {} {})",
                    self.render_int(lhs),
                    render_comparison(*op),
                    self.render_int(rhs)
                )
            }
            ValueExpr::Bool(BoolExpr::StringComparison { lhs, op, rhs }) => {
                format!(
                    "({} {} {})",
                    self.render_word(lhs),
                    render_string_comparison(*op),
                    self.render_word(rhs)
                )
            }
            ValueExpr::StringList(values) => format!(
                "@({})",
                values
                    .iter()
                    .map(|value| self.render_word(value))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }

    fn render_int(&self, expr: &fp_shell_core::IntExpr) -> String {
        match expr {
            fp_shell_core::IntExpr::Literal(value) => value.to_string(),
            fp_shell_core::IntExpr::Variable(name) => format!("${}", name),
            fp_shell_core::IntExpr::UnaryNeg(value) => format!("-{}", self.render_int(value)),
            fp_shell_core::IntExpr::Binary { lhs, op, rhs } => {
                format!(
                    "({} {} {})",
                    self.render_int(lhs),
                    render_arithmetic(*op),
                    self.render_int(rhs)
                )
            }
        }
    }

    fn render_word(&self, expr: &StringExpr) -> String {
        match expr {
            StringExpr::Literal(text) => ps_string(text),
            StringExpr::Variable(name) => format!("${}", name),
            StringExpr::Call { name, args } => format!("$({})", self.render_call(name, args)),
            StringExpr::Interpolated(parts) => {
                let mut out = String::from("\"");
                for part in parts {
                    match part {
                        StringPart::Literal(text) => out.push_str(&text.replace('"', "`\"")),
                        StringPart::Variable(name) => out.push_str(&format!("${}", name)),
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
            StringExpr::Variable(name) => format!("${}", name),
            StringExpr::Call { name, args } => format!("$({})", self.render_call(name, args)),
            StringExpr::Interpolated(parts) => {
                let mut out = String::new();
                for part in parts {
                    match part {
                        StringPart::Literal(text) => out.push_str(text),
                        StringPart::Variable(name) => out.push_str(&format!("${}", name)),
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
        if self.externs.get(name).map(|decl| decl.abi.as_str()) == Some("pwsh") {
            return self.render_pwsh_extern_call(name, args);
        }
        let args = args
            .iter()
            .map(|arg| self.render_value(arg))
            .collect::<Vec<_>>();
        if args.is_empty() {
            name.to_string()
        } else {
            format!("{} {}", name, args.join(" "))
        }
    }

    fn render_pwsh_extern_call(&self, name: &str, args: &[ValueExpr]) -> String {
        match name {
            "runtime_host_transport" => {
                let host = self.expect_string_arg(args, 0);
                format!("$script:FpHosts[{}].transport", self.render_word(host))
            }
            "runtime_temp_path" => "[System.IO.Path]::GetTempFileName()".to_string(),
            other => {
                let args = args
                    .iter()
                    .map(|arg| self.render_value(arg))
                    .collect::<Vec<_>>();
                if args.is_empty() {
                    other.to_string()
                } else {
                    format!("{} {}", other, args.join(" "))
                }
            }
        }
    }

    fn render_runtime_validator(&self) -> String {
        let mut commands = BTreeSet::new();
        for name in &self.used_externs {
            let Some(external) = self.externs.get(name) else {
                continue;
            };
            for command in external.runtime_requirements(fp_shell_core::ScriptTarget::PowerShell) {
                commands.insert(command);
            }
        }
        if commands.is_empty() {
            return String::new();
        }
        let mut script = String::new();
        script.push_str("function Invoke-FpRuntimeValidation {\n");
        for command in commands {
            script.push_str(&format!(
                "    if (-not (Get-Command -Name {} -ErrorAction SilentlyContinue)) {{ throw {} }}\n",
                ps_string(&command),
                ps_string(&format!("missing required command: {}", command))
            ));
        }
        script.push_str("}\n\nInvoke-FpRuntimeValidation\n\n");
        script
    }

    fn emit_host_entry(&mut self, indent: usize, host: &StringExpr) {
        self.push_line(indent, &format!("$__fpHost = {}", self.render_word(host)));
        self.push_line(indent, "$__fpEntry = $script:FpHosts[$__fpHost]");
    }

    fn emit_ssh_entry(&mut self, indent: usize, host: &StringExpr) {
        self.emit_host_entry(indent, host);
        self.push_line(
            indent,
            "$__fpAddress = if ($__fpEntry.address) { $__fpEntry.address } else { $__fpHost }",
        );
    }

    fn emit_winrm_session_setup(&mut self, indent: usize) {
        self.push_line(
            indent,
            "$__fpSessionArgs = @{ ComputerName = $__fpEntry.address }",
        );
        self.push_line(
            indent,
            "if ($__fpEntry.port) { $__fpSessionArgs.Port = $__fpEntry.port }",
        );
        self.push_line(
            indent,
            "$__fpScheme = if ($__fpEntry.scheme) { $__fpEntry.scheme.ToLowerInvariant() } else { 'http' }",
        );
        self.push_line(indent, "switch ($__fpScheme) {");
        self.push_line(indent + 1, "'http' {}");
        self.push_line(indent + 1, "'https' { $__fpSessionArgs.UseSSL = $true }");
        self.push_line(
            indent + 1,
            "default { throw \"unsupported winrm scheme for $__fpHost: $($__fpEntry.scheme)\" }",
        );
        self.push_line(indent, "}");
        self.push_line(
            indent,
            "if (-not $__fpEntry.password) { throw \"winrm password is required for non-interactive PowerShell target: $__fpHost\" }",
        );
        self.push_line(
            indent,
            "$__fpSecurePassword = ConvertTo-SecureString $__fpEntry.password -AsPlainText -Force",
        );
        self.push_line(
            indent,
            "$__fpCredential = New-Object System.Management.Automation.PSCredential($__fpEntry.user, $__fpSecurePassword)",
        );
        self.push_line(
            indent,
            "$__fpSession = New-PSSession -Credential $__fpCredential @__fpSessionArgs",
        );
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
        StringComparisonOp::Eq => "-eq",
        StringComparisonOp::Ne => "-ne",
    }
}

fn ps_string(value: &str) -> String {
    format!("'{}'", value.replace('\'', "''"))
}


const POWERSHELL_RUNTIME_UTILITIES: &str = "";

#[cfg(test)]
mod tests {
    use super::*;
    use fp_core::ast::{Expr, ExprInvoke, ExprInvokeTarget, ExprKind, Name};
    use fp_shell_core::{ExternalFunction, InventoryHost, ScriptProgram, StringExpr, WinRmInventory};
    use std::collections::HashMap;

    #[test]
    fn renders_noninteractive_winrm_support() {
        let mut externs = HashMap::new();
        externs.insert(
            "winrm_run".to_string(),
            ExternalFunction {
                name: "winrm_run".to_string(),
                abi: "pwsh".to_string(),
                param_count: 2,
                command: Some("New-PSSession Invoke-Command Remove-PSSession".to_string()),
            },
        );
        let program = ScriptProgram {
            externs,
            items: vec![ScriptItem::Expr(Expr::new(ExprKind::Invoke(ExprInvoke {
                span: Default::default(),
                target: ExprInvokeTarget::Function(Name::ident("winrm_run")),
                args: vec![
                    Expr::any(ValueExpr::String(StringExpr::Literal("win-1".to_string()))),
                    Expr::any(ValueExpr::String(StringExpr::Literal("hostname".to_string()))),
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
                    port: Some(5985),
                    scheme: Some("http".to_string()),
                }),
            },
        );
        let inventory = ShellInventory {
            groups: HashMap::new(),
            hosts,
        };
        let script = PowerShellTarget::new()
            .render(&program, &inventory)
            .expect("render should succeed");
        assert!(script.contains("password = 'secret'"));
        assert!(!script.contains("Get-Credential"));
    }

    #[test]
    fn renders_https_winrm_support() {
        let mut externs = HashMap::new();
        externs.insert(
            "winrm_run".to_string(),
            ExternalFunction {
                name: "winrm_run".to_string(),
                abi: "pwsh".to_string(),
                param_count: 2,
                command: Some("New-PSSession Invoke-Command Remove-PSSession".to_string()),
            },
        );
        let program = ScriptProgram {
            externs,
            items: vec![ScriptItem::Expr(Expr::new(ExprKind::Invoke(ExprInvoke {
                span: Default::default(),
                target: ExprInvokeTarget::Function(Name::ident("winrm_run")),
                args: vec![
                    Expr::any(ValueExpr::String(StringExpr::Literal("win-1".to_string()))),
                    Expr::any(ValueExpr::String(StringExpr::Literal("hostname".to_string()))),
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
        let script = PowerShellTarget::new()
            .render(&program, &inventory)
            .expect("render should succeed");
        assert!(script.contains("scheme = 'https'"));
    }
}
