use fp_shell_core::{
    ArithmeticOp, Block, BoolExpr, ComparisonOp, ConditionExpr, FunctionDef, HostExpr,
    OperationCopy, OperationGuards, OperationRsync, OperationRun, OperationTemplate, PrimitiveStmt,
    RsyncOptions, ScriptItem, ScriptProgram, ScriptRenderer, ShellInventory, Statement,
    StringComparisonOp, StringExpr, StringPart, TransportKind, UnsupportedTransportOperation,
    ValueExpr,
};

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
    lines: Vec<String>,
}

impl<'a> PowerShellRenderer<'a> {
    fn new(inventory: &'a ShellInventory) -> Self {
        Self {
            inventory,
            lines: Vec::new(),
        }
    }

    fn finish(self) -> String {
        let mut script = String::new();
        script.push_str("Set-StrictMode -Version Latest\n$ErrorActionPreference = 'Stop'\n$script:fpLastChanged = $false\n\n");
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
        script.push_str(POWERSHELL_RUNTIME_UTILITIES);
        for line in self.lines {
            script.push_str(&line);
            script.push('\n');
        }
        script
    }

    fn render_program(&mut self, program: &ScriptProgram) -> Result<(), String> {
        for item in &program.items {
            match item {
                ScriptItem::Function(def) => self.render_function(def)?,
                ScriptItem::Statement(statement) => self.render_statement(statement, 0)?,
            }
        }
        Ok(())
    }

    fn render_function(&mut self, def: &FunctionDef) -> Result<(), String> {
        let params = def
            .params
            .iter()
            .map(|param| format!("[string]${}", param))
            .collect::<Vec<_>>()
            .join(", ");
        self.push_line(0, &format!("function {} {{", def.name));
        if !params.is_empty() {
            self.push_line(1, &format!("param({})", params));
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
            Statement::Primitive(stmt) => self.render_primitive(stmt, indent),
            Statement::If(stmt) => {
                self.push_line(
                    indent,
                    &format!("if ({}) {{", self.render_condition(&stmt.condition)),
                );
                self.render_block(&stmt.then_block, indent + 1)?;
                if let Some(else_block) = &stmt.else_block {
                    self.push_line(indent, "} else {");
                    self.render_block(else_block, indent + 1)?;
                }
                self.push_line(indent, "}");
                Ok(())
            }
            Statement::While(stmt) => {
                self.push_line(
                    indent,
                    &format!("while ({}) {{", self.render_condition(&stmt.condition)),
                );
                self.render_block(&stmt.body, indent + 1)?;
                self.push_line(indent, "}");
                Ok(())
            }
            Statement::ForEach(stmt) => {
                let values = stmt
                    .values
                    .iter()
                    .map(|value| self.render_word(value))
                    .collect::<Vec<_>>()
                    .join(", ");
                self.push_line(
                    indent,
                    &format!("foreach (${0} in @({1})) {{", stmt.binding, values),
                );
                self.render_block(&stmt.body, indent + 1)?;
                self.push_line(indent, "}");
                Ok(())
            }
            Statement::Let(stmt) => {
                self.push_line(
                    indent,
                    &format!("${} = {}", stmt.name, self.render_value(&stmt.value)),
                );
                Ok(())
            }
            Statement::Invoke(stmt) => {
                let args = stmt
                    .args
                    .iter()
                    .map(|arg| self.render_value(arg))
                    .collect::<Vec<_>>()
                    .join(" ");
                self.push_line(indent, &format!("{} {}", stmt.name, args));
                Ok(())
            }
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
                            renderer.render_word(&op.command)
                        ),
                    ),
                }
                renderer.push_line(guard_indent, "$script:fpChanged = $true");
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
                            "Copy-Item -Force {} {}",
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
                renderer.push_line(guard_indent, "$script:fpChanged = $true");
                Ok(())
            })?;
        }
        Ok(())
    }

    fn render_template(&mut self, op: &OperationTemplate, indent: usize) -> Result<(), String> {
        for host in &op.hosts {
            self.render_guarded(indent, &op.guards, |renderer, guard_indent| {
                let vars = op
                    .vars
                    .iter()
                    .map(|(key, value)| format!("{}={}", key, renderer.render_word(value)))
                    .collect::<Vec<_>>()
                    .join(";");
                match host {
                    HostExpr::Localhost => {
                        renderer.push_line(
                            guard_indent,
                            &format!(
                                "$content = Get-Content -Raw {}",
                                renderer.render_word(&op.source)
                            ),
                        );
                        renderer.push_line(
                            guard_indent,
                            &format!("foreach ($pair in ({} -split ';')) {{", ps_string(&vars)),
                        );
                        renderer.push_line(guard_indent + 1, "if ($pair) {");
                        renderer.push_line(guard_indent + 2, "$name, $value = $pair -split '=', 2");
                        renderer.push_line(
                            guard_indent + 2,
                            "$content = $content.Replace(\"`${$name}\", $value)",
                        );
                        renderer.push_line(guard_indent + 1, "}");
                        renderer.push_line(guard_indent, "}");
                        renderer.push_line(
                            guard_indent,
                            &format!(
                                "Set-Content -Path {} -Value $content",
                                renderer.render_word(&op.destination)
                            ),
                        );
                    }
                    HostExpr::Selector(selector) => renderer.push_line(
                        guard_indent,
                        &format!(
                            "template_host {} {} {} {}",
                            renderer.render_word(selector),
                            renderer.render_word(&op.source),
                            renderer.render_word(&op.destination),
                            ps_string(&vars)
                        ),
                    ),
                }
                renderer.push_line(guard_indent, "$script:fpChanged = $true");
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
                            ps_string(&flags),
                            renderer.render_word(&op.source),
                            renderer.render_word(&op.destination)
                        ),
                    ),
                }
                renderer.push_line(guard_indent, "$script:fpChanged = $true");
                Ok(())
            })?;
        }
        Ok(())
    }

    fn render_primitive(&mut self, stmt: &PrimitiveStmt, indent: usize) -> Result<(), String> {
        match stmt {
            PrimitiveStmt::RunLocal { command } => {
                self.push_line(indent, &self.render_command_expr(command));
            }
            PrimitiveStmt::RunSsh { host, command } => {
                self.emit_ssh_entry(indent, host);
                self.push_line(
                    indent,
                    &format!(
                        "$__fpTarget = if ($__fpEntry.user) {{ \"$($__fpEntry.user)@$($__fpAddress)\" }} else {{ $__fpAddress }}"
                    ),
                );
                self.push_line(indent, "if ($__fpEntry.port) {");
                self.push_line(
                    indent + 1,
                    &format!(
                        "& ssh -p $__fpEntry.port $__fpTarget {}",
                        self.render_word(command)
                    ),
                );
                self.push_line(indent, "} else {");
                self.push_line(
                    indent + 1,
                    &format!("& ssh $__fpTarget {}", self.render_word(command)),
                );
                self.push_line(indent, "}");
            }
            PrimitiveStmt::RunDocker { host, command } => {
                self.emit_host_entry(indent, host);
                self.push_line(indent, "if ($__fpEntry.user) {");
                self.push_line(
                    indent + 1,
                    &format!(
                        "& docker exec --user $__fpEntry.user $__fpEntry.container sh -lc {}",
                        self.render_word(command)
                    ),
                );
                self.push_line(indent, "} else {");
                self.push_line(
                    indent + 1,
                    &format!(
                        "& docker exec $__fpEntry.container sh -lc {}",
                        self.render_word(command)
                    ),
                );
                self.push_line(indent, "}");
            }
            PrimitiveStmt::RunKubectl { host, command } => {
                self.emit_host_entry(indent, host);
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
                        self.render_word(command)
                    ),
                );
                self.push_line(indent, "& kubectl @__fpArgs");
            }
            PrimitiveStmt::RunWinrm { host, command } => {
                self.emit_host_entry(indent, host);
                self.push_line(
                    indent,
                    "$__fpSession = New-FpWinRmSession -Entry $__fpEntry -Host $__fpHost",
                );
                self.push_line(indent, "try {");
                self.push_line(
                    indent + 1,
                    &format!(
                        "Invoke-Command -Session $__fpSession -ScriptBlock ([scriptblock]::Create({}))",
                        self.render_word(command)
                    ),
                );
                self.push_line(indent, "} finally {");
                self.push_line(indent + 1, "Remove-PSSession -Session $__fpSession");
                self.push_line(indent, "}");
            }
            PrimitiveStmt::CopyLocal {
                source,
                destination,
            } => {
                self.push_line(
                    indent,
                    &format!(
                        "Copy-Item -Force {} {}",
                        self.render_word(source),
                        self.render_word(destination)
                    ),
                );
            }
            PrimitiveStmt::CopySsh {
                host,
                source,
                destination,
            } => {
                self.emit_ssh_entry(indent, host);
                self.push_line(
                    indent,
                    "$__fpTarget = if ($__fpEntry.user) { \"$($__fpEntry.user)@$($__fpAddress):$__fpDestination\" } else { \"$($__fpAddress):$__fpDestination\" }",
                );
                self.push_line(
                    indent,
                    &format!("$__fpDestination = {}", self.render_word(destination)),
                );
                self.push_line(indent, "if ($__fpEntry.port) {");
                self.push_line(
                    indent + 1,
                    &format!(
                        "& scp -P $__fpEntry.port {} $__fpTarget",
                        self.render_word(source)
                    ),
                );
                self.push_line(indent, "} else {");
                self.push_line(
                    indent + 1,
                    &format!("& scp {} $__fpTarget", self.render_word(source)),
                );
                self.push_line(indent, "}");
            }
            PrimitiveStmt::CopyDocker {
                host,
                source,
                destination,
            } => {
                self.emit_host_entry(indent, host);
                self.push_line(
                    indent,
                    &format!(
                        "& docker cp {} \"$($__fpEntry.container):{}\"",
                        self.render_word(source),
                        self.render_command_expr(destination)
                    ),
                );
            }
            PrimitiveStmt::CopyKubectl {
                host,
                source,
                destination,
            } => {
                self.emit_host_entry(indent, host);
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
                        self.render_word(source),
                        self.render_command_expr(destination)
                    ),
                );
            }
            PrimitiveStmt::CopyWinrm {
                host,
                source,
                destination,
            } => {
                self.emit_host_entry(indent, host);
                self.push_line(
                    indent,
                    "$__fpSession = New-FpWinRmSession -Entry $__fpEntry -Host $__fpHost",
                );
                self.push_line(
                    indent,
                    &format!("$__fpDestination = {}", self.render_word(destination)),
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
                    &format!("Copy-Item -ToSession $__fpSession -Path {} -Destination $__fpDestination -Force", self.render_word(source)),
                );
                self.push_line(indent, "} finally {");
                self.push_line(indent + 1, "Remove-PSSession -Session $__fpSession");
                self.push_line(indent, "}");
            }
            PrimitiveStmt::RenderTemplate {
                source,
                destination,
                vars,
            } => {
                self.push_line(
                    indent,
                    &format!(
                        "$__fpContent = Get-Content -Raw {}",
                        self.render_word(source)
                    ),
                );
                self.push_line(
                    indent,
                    &format!(
                        "foreach ($pair in ({} -split ';')) {{",
                        self.render_word(vars)
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
                        self.render_word(destination)
                    ),
                );
            }
            PrimitiveStmt::RemoveFile { path } => {
                self.push_line(
                    indent,
                    &format!(
                        "Remove-Item -Force {} -ErrorAction SilentlyContinue",
                        self.render_word(path)
                    ),
                );
            }
            PrimitiveStmt::RsyncSsh {
                host,
                flags,
                source,
                destination,
            } => {
                self.emit_ssh_entry(indent, host);
                self.push_line(
                    indent,
                    &format!("$__fpDestination = {}", self.render_word(destination)),
                );
                self.push_line(
                    indent,
                    "$__fpTarget = if ($__fpEntry.user) { \"$($__fpEntry.user)@$($__fpAddress):$__fpDestination\" } else { \"$($__fpAddress):$__fpDestination\" }",
                );
                self.push_line(
                    indent,
                    &format!(
                        "& rsync {} -- {} $__fpTarget",
                        self.render_word(flags),
                        self.render_word(source)
                    ),
                );
            }
            PrimitiveStmt::UnsupportedTransport {
                operation,
                transport,
            } => {
                let prefix = match operation {
                    UnsupportedTransportOperation::Run => "unsupported transport: ",
                    UnsupportedTransportOperation::Copy => "unsupported transport for copy: ",
                    UnsupportedTransportOperation::Rsync => {
                        "rsync is only supported for ssh in powershell target, got: "
                    }
                };
                self.push_line(
                    indent,
                    &format!(
                        "throw \"{}{}\"",
                        prefix.replace('"', "`\""),
                        self.render_interpolated_fragment(transport)
                    ),
                );
            }
        }
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
        self.push_line(indent, "$script:fpChanged = $false");
        let mut conditions = Vec::new();
        if let Some(command) = &guards.only_if {
            conditions.push(format!("({})", self.render_command_expr(command)));
        }
        if let Some(command) = &guards.unless {
            conditions.push(format!("-not ({})", self.render_command_expr(command)));
        }
        if let Some(path) = &guards.creates {
            conditions.push(format!("-not (Test-Path {})", self.render_word(path)));
        }
        if let Some(path) = &guards.removes {
            conditions.push(format!("(Test-Path {})", self.render_word(path)));
        }
        if conditions.is_empty() {
            emit(self, indent)?;
        } else {
            self.push_line(indent, &format!("if ({}) {{", conditions.join(" -and ")));
            emit(self, indent + 1)?;
            self.push_line(indent, "}");
        }
        self.push_line(indent, "$script:fpLastChanged = $script:fpChanged");
        Ok(())
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
            StringExpr::HostTransport(host) => {
                format!("$script:FpHosts[{}].transport", self.render_word(host))
            }
            StringExpr::TempPath => "[System.IO.Path]::GetTempFileName()".to_string(),
            StringExpr::Interpolated(parts) => {
                let mut out = String::from("\"");
                for part in parts {
                    match part {
                        StringPart::Literal(text) => out.push_str(&text.replace('"', "`\"")),
                        StringPart::Variable(name) => out.push_str(&format!("${}", name)),
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
            StringExpr::HostTransport(host) => {
                format!("$script:FpHosts[{}].transport", self.render_word(host))
            }
            StringExpr::TempPath => "[System.IO.Path]::GetTempFileName()".to_string(),
            StringExpr::Interpolated(parts) => {
                let mut out = String::new();
                for part in parts {
                    match part {
                        StringPart::Literal(text) => out.push_str(text),
                        StringPart::Variable(name) => out.push_str(&format!("${}", name)),
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

    fn render_interpolated_fragment(&self, expr: &StringExpr) -> String {
        match expr {
            StringExpr::Literal(text) => text.replace('"', "`\""),
            StringExpr::Variable(name) => format!("${}", name),
            StringExpr::HostTransport(host) => {
                format!("$script:FpHosts[{}].transport", self.render_word(host))
            }
            StringExpr::TempPath => "$([System.IO.Path]::GetTempFileName())".to_string(),
            StringExpr::Interpolated(parts) => {
                let mut out = String::new();
                for part in parts {
                    match part {
                        StringPart::Literal(text) => out.push_str(&text.replace('"', "`\"")),
                        StringPart::Variable(name) => out.push_str(&format!("${}", name)),
                    }
                }
                out
            }
        }
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

const POWERSHELL_RUNTIME_UTILITIES: &str = r#"

function New-FpWinRmSession {
    param($Entry, [string]$Host)
    $sessionArgs = @{
        ComputerName = $Entry.address
    }
    if ($Entry.port) { $sessionArgs.Port = $Entry.port }
    $scheme = if ($Entry.scheme) { $Entry.scheme.ToLowerInvariant() } else { 'http' }
    switch ($scheme) {
        'http' {}
        'https' { $sessionArgs.UseSSL = $true }
        default { throw "unsupported winrm scheme for $Host: $($Entry.scheme)" }
    }
    if (-not $Entry.password) { throw "winrm password is required for non-interactive PowerShell target: $Host" }
    $securePassword = ConvertTo-SecureString $Entry.password -AsPlainText -Force
    $credential = New-Object System.Management.Automation.PSCredential($Entry.user, $securePassword)
    New-PSSession -Credential $credential @sessionArgs
}

"#;

#[cfg(test)]
mod tests {
    use super::*;
    use fp_shell_core::{
        InventoryHost, OperationGuards, OperationRun, ScriptProgram, WinRmInventory,
    };
    use std::collections::HashMap;

    #[test]
    fn renders_noninteractive_winrm_support() {
        let program = ScriptProgram {
            items: vec![ScriptItem::Statement(Statement::Run(OperationRun {
                hosts: vec![HostExpr::Selector(StringExpr::Literal("win-1".to_string()))],
                command: StringExpr::Literal("hostname".to_string()),
                cwd: None,
                sudo: false,
                guards: OperationGuards::default(),
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
        assert!(script.contains("ConvertTo-SecureString $Entry.password -AsPlainText -Force"));
        assert!(script.contains("function New-FpWinRmSession"));
        assert!(!script.contains("Get-Credential"));
    }

    #[test]
    fn renders_https_winrm_support() {
        let program = ScriptProgram {
            items: vec![ScriptItem::Statement(Statement::Run(OperationRun {
                hosts: vec![HostExpr::Selector(StringExpr::Literal("win-1".to_string()))],
                command: StringExpr::Literal("hostname".to_string()),
                cwd: None,
                sudo: false,
                guards: OperationGuards::default(),
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
        let script = PowerShellTarget::new()
            .render(&program, &inventory)
            .expect("render should succeed");
        assert!(script.contains("scheme = 'https'"));
        assert!(script.contains("'https' { $sessionArgs.UseSSL = $true }"));
    }
}
