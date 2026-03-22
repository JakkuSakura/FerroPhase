use fp_core::ast::{
    Abi, AttrMeta, AttributesExt, BlockStmt, Expr, ExprBlock, ExprInvokeTarget, ExprKind,
    ExprMatch, ExprStringTemplate, ExprTry, FormatArgRef, FormatTemplatePart, ItemDeclFunction,
    ItemDefFunction, ItemKind, Node, NodeKind, Pattern, PatternKind, Ty, Value,
};
use fp_core::intrinsics::IntrinsicCallKind;
use fp_core::ops::BinOpKind;
use std::cell::RefCell;
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

impl PowerShellTarget {
    pub fn render(&self, node: &Node, inventory: &ShellInventory) -> Result<String, String> {
        let mut renderer = PowerShellRenderer::new(inventory);
        renderer.render_program(node)?;
        Ok(renderer.finish())
    }
}

struct PowerShellRenderer<'a> {
    inventory: &'a ShellInventory,
    externs: HashMap<String, ItemDeclFunction>,
    required_commands: RefCell<BTreeSet<String>>,
    lines: Vec<String>,
    temp_counter: usize,
}

impl<'a> PowerShellRenderer<'a> {
    fn new(inventory: &'a ShellInventory) -> Self {
        Self {
            inventory,
            externs: HashMap::new(),
            required_commands: RefCell::new(BTreeSet::new()),
            lines: Vec::new(),
            temp_counter: 0,
        }
    }

    fn finish(self) -> String {
        let mut script = String::new();
        script.push_str("Set-StrictMode -Version Latest\nSet-PSDebug -Trace 1\n$ErrorActionPreference = 'Stop'\n$script:fpLastChanged = $false\n\n");
        script.push_str("$script:FpHosts = @{}\n");
        for (name, host) in &self.inventory.hosts {
            script.push_str(&format!(
                "$script:FpHosts[{0}] = @{{ transport = {1}",
                ps_string(name),
                ps_string(&host.transport)
            ));
            if let Some(address) = host.get_string("address") {
                script.push_str(&format!("; address = {}", ps_string(address)));
            }
            if let Some(user) = host.get_string("user") {
                script.push_str(&format!("; user = {}", ps_string(user)));
            }
            if let Some(port) = host.get_u16("port") {
                script.push_str(&format!("; port = {}", port));
            }
            if let Some(container) = host.get_string("container") {
                script.push_str(&format!("; container = {}", ps_string(container)));
            }
            if let Some(pod) = host.get_string("pod") {
                script.push_str(&format!("; pod = {}", ps_string(pod)));
            }
            if let Some(namespace) = host.get_string("namespace") {
                script.push_str(&format!("; namespace = {}", ps_string(namespace)));
            }
            if let Some(context) = host.get_string("context") {
                script.push_str(&format!("; context = {}", ps_string(context)));
            }
            if let Some(password) = host.get_string("password") {
                script.push_str(&format!("; password = {}", ps_string(password)));
            }
            if let Some(scheme) = host.get_string("scheme") {
                script.push_str(&format!("; scheme = {}", ps_string(scheme)));
            }
            if let Some(chroot_directory) = host.get_string("chroot_directory") {
                script.push_str(&format!(
                    "; chroot_directory = {}",
                    ps_string(chroot_directory)
                ));
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

    fn render_program(&mut self, node: &Node) -> Result<(), String> {
        let NodeKind::File(file) = node.kind() else {
            return Err("powershell renderer requires file AST".to_string());
        };
        self.externs = extern_decl_map(file.items.iter(), ScriptTarget::PowerShell)?;
        for item in &file.items {
            match item.kind() {
                ItemKind::DefFunction(def) => self.render_function(def)?,
                ItemKind::Expr(expr) => self.render_expr_statement(expr, 0)?,
                _ => {}
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
        if function_returns_value(def) {
            match def.body.kind() {
                ExprKind::Block(block) => self.render_function_block(block, 1)?,
                _ => self.render_result_expr(&def.body, 1)?,
            }
        } else {
            match def.body.kind() {
                ExprKind::Block(block) => self.render_block(block, 1)?,
                _ => self.render_expr_statement(&def.body, 1)?,
            }
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

    fn render_function_block(&mut self, block: &ExprBlock, indent: usize) -> Result<(), String> {
        let Some((last, rest)) = block.stmts.split_last() else {
            return Ok(());
        };
        for statement in rest {
            self.render_block_stmt(statement, indent)?;
        }
        match last {
            BlockStmt::Expr(expr) => self.render_result_expr(&expr.expr, indent),
            _ => self.render_block_stmt(last, indent),
        }
    }

    fn render_match_expr(&mut self, expr_match: &ExprMatch, indent: usize) -> Result<(), String> {
        let scrutinee = expr_match
            .scrutinee
            .as_deref()
            .ok_or_else(|| "powershell match requires scrutinee".to_string())?;
        self.push_line(
            indent,
            &format!("switch -Exact ({}) {{", self.render_word(scrutinee)?),
        );
        for case in &expr_match.cases {
            let label = match case
                .pat
                .as_ref()
                .and_then(|pat| extract_match_case_string(pat))
            {
                Some(pattern) => self.render_word(&pattern)?,
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
        args: &[Expr],
        indent: usize,
    ) -> Result<(), String> {
        if self.externs.contains_key(name) {
            self.note_extern_requirements(name);
            if self.render_pwsh_extern_statement(name, args, indent)? {
                return Ok(());
            }
        }
        let args = args
            .iter()
            .map(|arg| self.render_value(arg))
            .collect::<Result<Vec<_>, _>>()?
            .join(" ");
        self.push_line(indent, &format!("{} {}", name, args));
        Ok(())
    }

    fn render_pwsh_extern_statement(
        &mut self,
        name: &str,
        args: &[Expr],
        indent: usize,
    ) -> Result<bool, String> {
        match name {
            "winrm_run" => {
                let host = self.expect_string_arg(args, 0).clone();
                let command = self.expect_string_arg(args, 1).clone();
                self.emit_host_entry(indent, &host)?;
                self.emit_winrm_session_setup(indent);
                self.push_line(indent, "try {");
                self.push_line(
                    indent + 1,
                    &format!(
                        "Invoke-Command -Session $__fpSession -ScriptBlock ([scriptblock]::Create({}))",
                        self.render_word(&command)?
                    ),
                );
                self.push_line(indent, "} finally {");
                self.push_line(indent + 1, "Remove-PSSession -Session $__fpSession");
                self.push_line(indent, "}");
            }
            "winrm_copy" => {
                let host = self.expect_string_arg(args, 0).clone();
                let source = self.expect_string_arg(args, 1).clone();
                let destination = self.expect_string_arg(args, 2).clone();
                self.emit_host_entry(indent, &host)?;
                self.emit_winrm_session_setup(indent);
                self.push_line(
                    indent,
                    &format!("$__fpDestination = {}", self.render_word(&destination)?),
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
                    &format!(
                        "Copy-Item -ToSession $__fpSession -Path {} -Destination $__fpDestination -Force",
                        self.render_word(&source)?
                    ),
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
                        self.render_word(&source)?
                    ),
                );
                self.push_line(
                    indent,
                    &format!(
                        "foreach ($pair in ({} -split ';')) {{",
                        self.render_word(&vars)?
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
                        self.render_word(&destination)?
                    ),
                );
            }
            "runtime_fail" => {
                self.push_line(
                    indent,
                    &format!(
                        "throw {}",
                        self.render_word(self.expect_string_arg(args, 0))?
                    ),
                );
            }
            "runtime_set_changed" => {
                self.push_line(
                    indent,
                    &format!(
                        "$script:fpLastChanged = {}",
                        if is_true_expr(args.first()) {
                            "$true"
                        } else {
                            "$false"
                        }
                    ),
                );
            }
            _ => {
                self.push_line(indent, &self.render_generic_extern(name, args)?);
                return Ok(true);
            }
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
            BlockStmt::Defer(_) => Ok(()),
            BlockStmt::Any(_) | BlockStmt::Item(_) | BlockStmt::Noop => Ok(()),
        }
    }

    fn render_expr_statement(&mut self, expr: &Expr, indent: usize) -> Result<(), String> {
        match expr.kind() {
            ExprKind::Block(block) => self.render_block(block, indent),
            ExprKind::Try(expr_try) => self.render_try_expr(expr_try, indent, false),
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
                let values = self.extract_string_list(&expr_for.iter)?;
                let values = values
                    .iter()
                    .map(|value| self.render_word(value))
                    .collect::<Result<Vec<_>, _>>()?
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
                        "powershell renderer only supports identifier invocation targets"
                            .to_string(),
                    );
                };
                self.render_invoke_statement(ident.as_str(), &invoke.args, indent)
            }
            _ => Ok(()),
        }
    }

    fn render_result_expr(&mut self, expr: &Expr, indent: usize) -> Result<(), String> {
        match expr.kind() {
            ExprKind::Block(block) => self.render_function_block(block, indent),
            ExprKind::Try(expr_try) => self.render_try_expr(expr_try, indent, true),
            ExprKind::If(expr_if) => {
                self.push_line(
                    indent,
                    &format!("if ({}) {{", self.render_expr_as_condition(&expr_if.cond)?),
                );
                self.render_result_expr(&expr_if.then, indent + 1)?;
                if let Some(elze) = &expr_if.elze {
                    self.push_line(indent, "} else {");
                    self.render_result_expr(elze, indent + 1)?;
                }
                self.push_line(indent, "}");
                Ok(())
            }
            ExprKind::Match(expr_match) => {
                let scrutinee = expr_match
                    .scrutinee
                    .as_deref()
                    .ok_or_else(|| "powershell match requires scrutinee".to_string())?;
                self.push_line(
                    indent,
                    &format!("switch -Exact ({}) {{", self.render_word(scrutinee)?),
                );
                for case in &expr_match.cases {
                    let label = match case
                        .pat
                        .as_ref()
                        .and_then(|pat| extract_match_case_string(pat))
                    {
                        Some(pattern) => self.render_word(&pattern)?,
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
                    self.render_result_expr(&case.body, indent + 2)?;
                    self.push_line(indent + 1, "}");
                }
                self.push_line(indent, "}");
                Ok(())
            }
            _ => {
                self.push_line(
                    indent,
                    &format!("Write-Output {}", self.render_value(expr)?),
                );
                Ok(())
            }
        }
    }

    fn render_expr_as_condition(&self, expr: &Expr) -> Result<String, String> {
        self.render_condition(expr)
    }

    fn render_expr_as_value(&self, expr: &Expr) -> Result<String, String> {
        self.render_value(expr)
    }

    fn render_condition(&self, expr: &Expr) -> Result<String, String> {
        match expr.kind() {
            ExprKind::Value(value) => match &**value {
                Value::Bool(flag) => Ok(if flag.value { "$true" } else { "$false" }.to_string()),
                _ => Ok(format!("({} -eq 'true')", self.render_word(expr)?)),
            },
            ExprKind::Name(name) => {
                let ident = name.as_ident().ok_or_else(|| {
                    "powershell condition only supports identifier names".to_string()
                })?;
                Ok(format!("${}", ident))
            }
            ExprKind::BinOp(bin_op) => {
                if matches!(
                    bin_op.kind,
                    BinOpKind::Gt | BinOpKind::Lt | BinOpKind::Ge | BinOpKind::Le
                ) {
                    return Ok(format!(
                        "{} {} {}",
                        self.render_int(&bin_op.lhs)?,
                        render_comparison(bin_op.kind),
                        self.render_int(&bin_op.rhs)?
                    ));
                }
                if matches!(bin_op.kind, BinOpKind::Eq | BinOpKind::Ne) {
                    return Ok(format!(
                        "{} {} {}",
                        self.render_word(&bin_op.lhs)?,
                        render_string_comparison(bin_op.kind),
                        self.render_word(&bin_op.rhs)?
                    ));
                }
                Err("unsupported powershell condition expression".to_string())
            }
            ExprKind::Invoke(invoke) => {
                let name = invoke_function_name(invoke)?;
                self.render_call(name, &invoke.args)
            }
            ExprKind::Paren(paren) => self.render_condition(&paren.expr),
            _ => Ok(format!("({} -eq 'true')", self.render_word(expr)?)),
        }
    }

    fn render_value(&self, expr: &Expr) -> Result<String, String> {
        if let Ok(values) = self.extract_string_list(expr) {
            return values
                .iter()
                .map(|value| self.render_word(value))
                .collect::<Result<Vec<_>, _>>()
                .map(|values| format!("@({})", values.join(", ")));
        }
        match expr.kind() {
            ExprKind::Value(value) => match &**value {
                Value::Bool(flag) => Ok(if flag.value { "$true" } else { "$false" }.to_string()),
                _ => self.render_word(expr),
            },
            ExprKind::BinOp(_) => Ok(format!("({})", self.render_condition(expr)?)),
            _ => self.render_word(expr),
        }
    }

    fn render_int(&self, expr: &Expr) -> Result<String, String> {
        match expr.kind() {
            ExprKind::Value(value) => match &**value {
                Value::Int(value) => Ok(value.value.to_string()),
                _ => Err("expected int expression".to_string()),
            },
            ExprKind::Name(name) => {
                let ident = name.as_ident().ok_or_else(|| {
                    "powershell int expression only supports identifier names".to_string()
                })?;
                Ok(format!("${}", ident))
            }
            ExprKind::BinOp(bin_op)
                if matches!(
                    bin_op.kind,
                    BinOpKind::Add
                        | BinOpKind::AddTrait
                        | BinOpKind::Sub
                        | BinOpKind::Mul
                        | BinOpKind::Div
                        | BinOpKind::Mod
                ) =>
            {
                Ok(format!(
                    "({} {} {})",
                    self.render_int(&bin_op.lhs)?,
                    render_arithmetic(bin_op.kind),
                    self.render_int(&bin_op.rhs)?
                ))
            }
            ExprKind::Paren(paren) => self.render_int(&paren.expr),
            _ => Err("expected int expression".to_string()),
        }
    }

    fn render_word(&self, expr: &Expr) -> Result<String, String> {
        match expr.kind() {
            ExprKind::Value(value) => match &**value {
                Value::String(text) => Ok(ps_string(&text.value)),
                Value::Int(value) => Ok(ps_string(&value.value.to_string())),
                Value::Bool(value) => Ok(ps_string(&value.value.to_string())),
                _ => Err("unsupported powershell value expression".to_string()),
            },
            ExprKind::Name(name) => {
                let ident = name.as_ident().ok_or_else(|| {
                    "powershell string expression only supports identifier names".to_string()
                })?;
                Ok(format!("${}", ident))
            }
            ExprKind::Invoke(invoke) => {
                let name = invoke_function_name(invoke)?;
                Ok(format!("$({})", self.render_call(name, &invoke.args)?))
            }
            ExprKind::FormatString(template) => self.render_format_template_word(template),
            ExprKind::IntrinsicCall(call) if call.kind == IntrinsicCallKind::Format => {
                self.render_format_call_word(call)
            }
            ExprKind::Paren(paren) => self.render_word(&paren.expr),
            _ => Err("unsupported powershell string expression".to_string()),
        }
    }

    fn next_temp_name(&mut self, prefix: &str) -> String {
        self.temp_counter += 1;
        format!("__fp{}{}", prefix, self.temp_counter)
    }

    fn render_try_expr(
        &mut self,
        expr_try: &ExprTry,
        indent: usize,
        result_mode: bool,
    ) -> Result<(), String> {
        let success_name = self.next_temp_name("TrySuccess");
        let handled_name = self.next_temp_name("TryHandled");
        self.push_line(indent, &format!("${} = $false", success_name));
        self.push_line(indent, &format!("${} = $false", handled_name));
        self.push_line(indent, "try {");
        if result_mode && expr_try.elze.is_none() {
            self.render_result_expr(&expr_try.expr, indent + 1)?;
        } else {
            self.render_expr_statement(&expr_try.expr, indent + 1)?;
        }
        self.push_line(indent + 1, &format!("${} = $true", success_name));
        self.push_line(indent, "} catch {");
        for catch in &expr_try.catches {
            self.push_line(indent + 1, &format!("if (-not ${}) {{", handled_name));
            if let Some(name) = catch_binding_name(catch.pat.as_deref())? {
                self.push_line(indent + 2, &format!("${} = $_", name));
            }
            if result_mode {
                self.render_result_expr(&catch.body, indent + 2)?;
            } else {
                self.render_expr_statement(&catch.body, indent + 2)?;
            }
            self.push_line(indent + 2, &format!("${} = $true", handled_name));
            self.push_line(indent + 1, "}");
        }
        self.push_line(indent + 1, &format!("if (-not ${}) {{", handled_name));
        self.push_line(indent + 2, "throw");
        self.push_line(indent + 1, "}");
        self.push_line(indent, "} finally {");
        if let Some(finally) = &expr_try.finally {
            self.render_expr_statement(finally, indent + 1)?;
        }
        self.push_line(indent, "}");
        if let Some(elze) = &expr_try.elze {
            self.push_line(indent, &format!("if (${}) {{", success_name));
            if result_mode {
                self.render_result_expr(elze, indent + 1)?;
            } else {
                self.render_expr_statement(elze, indent + 1)?;
            }
            self.push_line(indent, "}");
        }
        Ok(())
    }

    fn push_line(&mut self, indent: usize, text: &str) {
        self.lines
            .push(format!("{}{}", "    ".repeat(indent), text));
    }

    fn render_call(&self, name: &str, args: &[Expr]) -> Result<String, String> {
        if self.externs.contains_key(name) {
            self.note_extern_requirements(name);
            return Ok(self.render_pwsh_extern_call(name, args)?);
        }
        let args = args
            .iter()
            .map(|arg| self.render_value(arg))
            .collect::<Result<Vec<_>, _>>()?;
        if args.is_empty() {
            Ok(name.to_string())
        } else {
            Ok(format!("{} {}", name, args.join(" ")))
        }
    }

    fn render_pwsh_extern_call(&self, name: &str, args: &[Expr]) -> Result<String, String> {
        Ok(match name {
            "runtime_host_transport" => {
                let host = self.expect_string_arg(args, 0);
                format!("$script:FpHosts[{}].transport", self.render_word(host)?)
            }
            "shell_status" => {
                format!(
                    "(& {{ pwsh -Command {}; $LASTEXITCODE -eq 0 }})",
                    self.render_word(self.expect_string_arg(args, 0))?
                )
            }
            "runtime_host_address" => self.render_host_field_lookup("address", args)?,
            "runtime_host_user" => self.render_host_field_lookup("user", args)?,
            "runtime_host_port" => self.render_host_field_lookup("port", args)?,
            "runtime_host_container" => self.render_host_field_lookup("container", args)?,
            "runtime_host_pod" => self.render_host_field_lookup("pod", args)?,
            "runtime_host_namespace" => self.render_host_field_lookup("namespace", args)?,
            "runtime_host_context" => self.render_host_field_lookup("context", args)?,
            "runtime_host_password" => self.render_host_field_lookup("password", args)?,
            "runtime_host_scheme" => self.render_host_field_lookup("scheme", args)?,
            "runtime_host_chroot_directory" => {
                self.render_host_field_lookup("chroot_directory", args)?
            }
            "runtime_temp_path" => "[System.IO.Path]::GetTempFileName()".to_string(),
            "runtime_last_changed" => {
                "if ($script:fpLastChanged) { 'true' } else { 'false' }".to_string()
            }
            other => self.render_generic_extern(other, args)?,
        })
    }

    fn render_generic_extern(&self, name: &str, args: &[Expr]) -> Result<String, String> {
        let command = self
            .externs
            .get(name)
            .and_then(extern_command)
            .unwrap_or_else(|| name.to_string());
        let args = args
            .iter()
            .map(|arg| self.render_value(arg))
            .collect::<Result<Vec<_>, _>>()?;
        if args.is_empty() {
            Ok(command)
        } else {
            Ok(format!("{} {}", command, args.join(" ")))
        }
    }

    fn render_host_field_lookup(&self, field: &str, args: &[Expr]) -> Result<String, String> {
        let host = self.expect_string_arg(args, 0);
        Ok(format!(
            "$script:FpHosts[{}].{}",
            self.render_word(host)?,
            field
        ))
    }

    fn render_runtime_validator(&self) -> String {
        let commands = self.required_commands.borrow();
        if commands.is_empty() {
            return String::new();
        }
        let mut script = String::new();
        script.push_str("function Invoke-FpRuntimeValidation {\n");
        for command in commands.iter() {
            script.push_str(&format!(
                "    if (-not (Get-Command -Name {} -ErrorAction SilentlyContinue)) {{ throw {} }}\n",
                ps_string(&command),
                ps_string(&format!("missing required command: {}", command))
            ));
        }
        script.push_str("}\n\nInvoke-FpRuntimeValidation\n\n");
        script
    }

    fn note_extern_requirements(&self, name: &str) {
        let Some(function) = self.externs.get(name) else {
            return;
        };
        let mut commands = self.required_commands.borrow_mut();
        for command in runtime_requirements(function, ScriptTarget::PowerShell) {
            commands.insert(command);
        }
    }

    fn emit_host_entry(&mut self, indent: usize, host: &Expr) -> Result<(), String> {
        self.push_line(indent, &format!("$__fpHost = {}", self.render_word(host)?));
        self.push_line(indent, "$__fpEntry = $script:FpHosts[$__fpHost]");
        Ok(())
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

    fn expect_string_arg<'b>(&self, args: &'b [Expr], index: usize) -> &'b Expr {
        &args[index]
    }

    fn extract_string_list<'b>(&self, expr: &'b Expr) -> Result<Vec<&'b Expr>, String> {
        match expr.kind() {
            ExprKind::Array(array) => Ok(array.values.iter().collect()),
            ExprKind::Tuple(tuple) => Ok(tuple.values.iter().collect()),
            _ => Err("powershell renderer only supports string-list for iterables".to_string()),
        }
    }

    fn render_format_template_word(&self, template: &ExprStringTemplate) -> Result<String, String> {
        let mut out = String::from("\"");
        for part in &template.parts {
            match part {
                FormatTemplatePart::Literal(text) => out.push_str(&text.replace('"', "`\"")),
                FormatTemplatePart::Placeholder(placeholder) => match &placeholder.arg_ref {
                    FormatArgRef::Named(name) => out.push_str(&format!("${}", name)),
                    _ => {
                        return Err(
                            "powershell format strings only support named placeholders".to_string()
                        );
                    }
                },
            }
        }
        out.push('"');
        Ok(out)
    }

    fn render_format_template_command(
        &self,
        template: &ExprStringTemplate,
    ) -> Result<String, String> {
        let mut out = String::new();
        for part in &template.parts {
            match part {
                FormatTemplatePart::Literal(text) => out.push_str(text),
                FormatTemplatePart::Placeholder(placeholder) => match &placeholder.arg_ref {
                    FormatArgRef::Named(name) => out.push_str(&format!("${}", name)),
                    _ => {
                        return Err(
                            "powershell format strings only support named placeholders".to_string()
                        );
                    }
                },
            }
        }
        Ok(out)
    }

    fn render_format_call_word(
        &self,
        call: &fp_core::ast::ExprIntrinsicCall,
    ) -> Result<String, String> {
        let Some(template) = call.args.first() else {
            return Err("powershell format call missing template".to_string());
        };
        let ExprKind::FormatString(template) = template.kind() else {
            return Err("powershell format call requires format template".to_string());
        };
        let mut out = String::from("\"");
        let mut implicit_index = 1usize;
        for part in &template.parts {
            match part {
                FormatTemplatePart::Literal(text) => out.push_str(&text.replace('"', "`\"")),
                FormatTemplatePart::Placeholder(placeholder) => {
                    let arg = match placeholder.arg_ref {
                        FormatArgRef::Implicit => {
                            let arg = call.args.get(implicit_index).ok_or_else(|| {
                                "powershell format call missing implicit argument".to_string()
                            })?;
                            implicit_index += 1;
                            arg
                        }
                        FormatArgRef::Positional(index) => call
                            .args
                            .get(index + 1)
                            .ok_or_else(|| "powershell format call missing positional argument".to_string())?,
                        FormatArgRef::Named(ref name) => call
                            .args
                            .iter()
                            .skip(1)
                            .find(|arg| matches!(arg.kind(), ExprKind::Name(found) if found.as_ident().is_some_and(|ident| ident.as_str() == name)))
                            .ok_or_else(|| "powershell format call missing named argument".to_string())?,
                    };
                    out.push_str(&self.render_word_fragment(arg)?);
                }
            }
        }
        out.push('"');
        Ok(out)
    }

    fn render_format_call_command(
        &self,
        call: &fp_core::ast::ExprIntrinsicCall,
    ) -> Result<String, String> {
        let Some(template) = call.args.first() else {
            return Err("powershell format call missing template".to_string());
        };
        let ExprKind::FormatString(template) = template.kind() else {
            return Err("powershell format call requires format template".to_string());
        };
        let mut out = String::new();
        let mut implicit_index = 1usize;
        for part in &template.parts {
            match part {
                FormatTemplatePart::Literal(text) => out.push_str(text),
                FormatTemplatePart::Placeholder(placeholder) => {
                    let arg = match placeholder.arg_ref {
                        FormatArgRef::Implicit => {
                            let arg = call.args.get(implicit_index).ok_or_else(|| {
                                "powershell format call missing implicit argument".to_string()
                            })?;
                            implicit_index += 1;
                            arg
                        }
                        FormatArgRef::Positional(index) => call
                            .args
                            .get(index + 1)
                            .ok_or_else(|| "powershell format call missing positional argument".to_string())?,
                        FormatArgRef::Named(ref name) => call
                            .args
                            .iter()
                            .skip(1)
                            .find(|arg| matches!(arg.kind(), ExprKind::Name(found) if found.as_ident().is_some_and(|ident| ident.as_str() == name)))
                            .ok_or_else(|| "powershell format call missing named argument".to_string())?,
                    };
                    out.push_str(&self.render_command_fragment(arg)?);
                }
            }
        }
        Ok(out)
    }

    fn render_word_fragment(&self, expr: &Expr) -> Result<String, String> {
        match expr.kind() {
            ExprKind::Value(value) => match &**value {
                Value::String(text) => Ok(text.value.replace('"', "`\"")),
                Value::Int(value) => Ok(value.value.to_string()),
                Value::Bool(value) => Ok(value.value.to_string()),
                _ => Err("unsupported powershell string fragment".to_string()),
            },
            ExprKind::Name(name) => {
                let ident = name.as_ident().ok_or_else(|| {
                    "powershell string fragment only supports identifier names".to_string()
                })?;
                Ok(format!("${}", ident))
            }
            ExprKind::Invoke(invoke) => {
                let name = invoke_function_name(invoke)?;
                Ok(format!("$({})", self.render_call(name, &invoke.args)?))
            }
            ExprKind::FormatString(template) => self.render_format_template_command(template),
            ExprKind::IntrinsicCall(call) if call.kind == IntrinsicCallKind::Format => {
                self.render_format_call_command(call)
            }
            ExprKind::Paren(paren) => self.render_word_fragment(&paren.expr),
            _ => Err("unsupported powershell string fragment".to_string()),
        }
    }

    fn render_command_fragment(&self, expr: &Expr) -> Result<String, String> {
        match expr.kind() {
            ExprKind::Value(value) => match &**value {
                Value::String(text) => Ok(text.value.clone()),
                Value::Int(value) => Ok(value.value.to_string()),
                Value::Bool(value) => Ok(value.value.to_string()),
                _ => Err("unsupported powershell command fragment".to_string()),
            },
            ExprKind::Name(name) => {
                let ident = name.as_ident().ok_or_else(|| {
                    "powershell command fragment only supports identifier names".to_string()
                })?;
                Ok(format!("${}", ident))
            }
            ExprKind::Invoke(invoke) => {
                let name = invoke_function_name(invoke)?;
                Ok(format!("$({})", self.render_call(name, &invoke.args)?))
            }
            ExprKind::FormatString(template) => self.render_format_template_command(template),
            ExprKind::IntrinsicCall(call) if call.kind == IntrinsicCallKind::Format => {
                self.render_format_call_command(call)
            }
            ExprKind::Paren(paren) => self.render_command_fragment(&paren.expr),
            _ => Err("unsupported powershell command fragment".to_string()),
        }
    }
}

fn extract_match_case_string(pattern: &fp_core::ast::Pattern) -> Option<Expr> {
    match pattern.kind() {
        PatternKind::Wildcard(_) => None,
        PatternKind::Variant(variant) if variant.pattern.is_none() => Some(variant.name.clone()),
        _ => None,
    }
}

fn render_arithmetic(op: BinOpKind) -> &'static str {
    match op {
        BinOpKind::Add | BinOpKind::AddTrait => "+",
        BinOpKind::Sub => "-",
        BinOpKind::Mul => "*",
        BinOpKind::Div => "/",
        BinOpKind::Mod => "%",
        _ => unreachable!(),
    }
}

fn render_comparison(op: BinOpKind) -> &'static str {
    match op {
        BinOpKind::Gt => "-gt",
        BinOpKind::Lt => "-lt",
        BinOpKind::Ge => "-ge",
        BinOpKind::Le => "-le",
        BinOpKind::Eq => "-eq",
        BinOpKind::Ne => "-ne",
        _ => unreachable!(),
    }
}

fn render_string_comparison(op: BinOpKind) -> &'static str {
    match op {
        BinOpKind::Eq => "-eq",
        BinOpKind::Ne => "-ne",
        _ => unreachable!(),
    }
}

fn function_returns_value(def: &ItemDefFunction) -> bool {
    !matches!(def.sig.ret_ty.as_ref(), None | Some(Ty::Unit(_)))
}

fn catch_binding_name(pattern: Option<&Pattern>) -> Result<Option<String>, String> {
    let Some(pattern) = pattern else {
        return Ok(None);
    };
    if let Some(ident) = pattern.as_ident() {
        return Ok(Some(ident.as_str().to_string()));
    }
    if matches!(pattern.kind(), PatternKind::Wildcard(_)) {
        return Ok(None);
    }
    Err("powershell try/catch only supports identifier and `_` catch patterns".to_string())
}

fn invoke_function_name<'a>(invoke: &'a fp_core::ast::ExprInvoke) -> Result<&'a str, String> {
    let ExprInvokeTarget::Function(name) = &invoke.target else {
        return Err("powershell renderer only supports function invocation targets".to_string());
    };
    let Some(ident) = name.as_ident() else {
        return Err("powershell renderer only supports identifier invocation targets".to_string());
    };
    Ok(ident.as_str())
}

fn is_true_expr(expr: Option<&Expr>) -> bool {
    matches!(
        expr.map(Expr::kind),
        Some(ExprKind::Value(value)) if matches!(&**value, Value::Bool(flag) if flag.value)
    )
}

fn ps_string(value: &str) -> String {
    format!("'{}'", value.replace('\'', "''"))
}

fn extern_decl_map<'a>(
    items: impl Iterator<Item = &'a fp_core::ast::Item>,
    target: ScriptTarget,
) -> Result<HashMap<String, ItemDeclFunction>, String> {
    let mut externs = HashMap::new();
    for item in items {
        match item.kind() {
            ItemKind::DeclFunction(function) => {
                if !matches!(&function.sig.abi, Abi::Named(abi) if abi == "pwsh") {
                    continue;
                }
                validate_extern_decl(function, target)?;
                externs.insert(function.name.as_str().to_string(), function.clone());
            }
            ItemKind::Module(module) => {
                externs.extend(extern_decl_map(module.items.iter(), target)?);
            }
            _ => {}
        }
    }
    Ok(externs)
}

const POWERSHELL_RUNTIME_UTILITIES: &str = "";

#[derive(Debug, Clone, Default)]
pub struct ShellInventory {
    pub hosts: HashMap<String, InventoryHost>,
}

#[derive(Debug, Clone, Default)]
pub struct InventoryHost {
    pub transport: String,
    pub fields: HashMap<String, InventoryValue>,
}

impl InventoryHost {
    pub fn get_string(&self, name: &str) -> Option<&str> {
        match self.fields.get(name) {
            Some(InventoryValue::String(value)) => Some(value.as_str()),
            _ => None,
        }
    }

    pub fn get_u16(&self, name: &str) -> Option<u16> {
        match self.fields.get(name) {
            Some(InventoryValue::U16(value)) => Some(*value),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InventoryValue {
    String(String),
    U16(u16),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ScriptTarget {
    PowerShell,
}

fn validate_extern_decl(function: &ItemDeclFunction, target: ScriptTarget) -> Result<(), String> {
    let expected_abi = match target {
        ScriptTarget::PowerShell => "pwsh",
    };
    let abi = match &function.sig.abi {
        Abi::Rust => {
            return Err(format!(
                "extern `{}` uses ABI `rust`, but shell target requires `{}`",
                function.name, expected_abi
            ));
        }
        Abi::Named(name) => name.as_str(),
    };
    if abi != expected_abi {
        return Err(format!(
            "extern `{}` uses ABI `{}`, but shell target requires `{}`",
            function.name, abi, expected_abi
        ));
    }
    let command = extern_command(function);
    if command.is_none() && !is_runtime_primitive(function.name.as_str()) {
        return Err(format!(
            "extern `{}` is missing #[command = \"...\"] for {} shell target",
            function.name, expected_abi
        ));
    }
    if command
        .as_deref()
        .is_some_and(|value| value.trim().is_empty())
    {
        return Err(format!(
            "extern `{}` has an empty #[command] annotation",
            function.name
        ));
    }
    Ok(())
}

fn runtime_requirements(function: &ItemDeclFunction, _target: ScriptTarget) -> Vec<String> {
    extern_command(function)
        .map(|command| {
            command
                .split_whitespace()
                .next()
                .map(|tool| vec![tool.to_string()])
                .unwrap_or_default()
        })
        .unwrap_or_default()
}

fn is_runtime_primitive(name: &str) -> bool {
    matches!(
        name,
        "runtime_temp_path" | "runtime_fail" | "runtime_set_changed" | "runtime_last_changed"
    )
}

fn extern_command(function: &ItemDeclFunction) -> Option<String> {
    let AttrMeta::NameValue(meta) = function.attrs.find_by_name("command")? else {
        return None;
    };
    let ExprKind::Value(value) = meta.value.kind() else {
        return None;
    };
    let Value::String(text) = &**value else {
        return None;
    };
    Some(text.value.clone())
}

#[cfg(test)]
mod tests {
    use super::*;
    use fp_core::ast::{
        Abi, AttrMeta, AttrMetaNameValue, AttrStyle, Attribute, Expr, ExprInvoke, ExprInvokeTarget,
        ExprKind, File, FunctionParam, FunctionSignature, Ident, Item, ItemDeclFunction, ItemKind,
        Name, Node, Path, Ty,
    };
    use std::collections::HashMap;
    use std::path::PathBuf;

    fn extern_decl(name: &str, abi: &str, command: &str, param_count: usize) -> Item {
        let mut sig = FunctionSignature::unit();
        sig.abi = Abi::Named(abi.to_string());
        sig.params = (0..param_count)
            .map(|index| {
                FunctionParam::new(
                    Ident::new(format!("arg{}", index)),
                    Ty::ident(Ident::new("str")),
                )
            })
            .collect();
        Item::from(ItemKind::DeclFunction(ItemDeclFunction {
            attrs: vec![Attribute {
                style: AttrStyle::Outer,
                meta: AttrMeta::NameValue(AttrMetaNameValue {
                    name: Path::from_ident(Ident::new("command")),
                    value: Expr::value(Value::string(command.to_string())).into(),
                }),
            }],
            ty_annotation: None,
            name: Ident::new(name),
            sig,
        }))
    }

    fn render_node(decls: Vec<Item>, expr: Expr, inventory: &ShellInventory) -> String {
        let mut items = decls;
        items.push(Item::from(ItemKind::Expr(expr)));
        let node = Node::file(File {
            path: PathBuf::from("test.fp"),
            items,
        });
        PowerShellTarget::new()
            .render(&node, inventory)
            .expect("render should succeed")
    }

    #[test]
    fn renders_noninteractive_winrm_support() {
        let expr = Expr::new(ExprKind::Invoke(ExprInvoke {
            span: Default::default(),
            target: ExprInvokeTarget::Function(Name::ident("winrm_run")),
            args: vec![
                Expr::value(Value::string("win-1".to_string())),
                Expr::value(Value::string("hostname".to_string())),
            ],
            kwargs: Vec::new(),
        }));
        let mut hosts = HashMap::new();
        hosts.insert(
            "win-1".to_string(),
            InventoryHost {
                transport: "winrm".to_string(),
                fields: HashMap::from([
                    (
                        "address".to_string(),
                        InventoryValue::String("10.0.0.21".to_string()),
                    ),
                    (
                        "user".to_string(),
                        InventoryValue::String("Administrator".to_string()),
                    ),
                    (
                        "password".to_string(),
                        InventoryValue::String("secret".to_string()),
                    ),
                    ("port".to_string(), InventoryValue::U16(5985)),
                    (
                        "scheme".to_string(),
                        InventoryValue::String("http".to_string()),
                    ),
                ]),
            },
        );
        let inventory = ShellInventory {
            groups: HashMap::new(),
            hosts,
        };
        let script = render_node(
            vec![extern_decl(
                "winrm_run",
                "pwsh",
                "New-PSSession Invoke-Command Remove-PSSession",
                2,
            )],
            expr,
            &inventory,
        );
        assert!(script.contains("password = 'secret'"));
        assert!(!script.contains("Get-Credential"));
    }

    #[test]
    fn renders_https_winrm_support() {
        let expr = Expr::new(ExprKind::Invoke(ExprInvoke {
            span: Default::default(),
            target: ExprInvokeTarget::Function(Name::ident("winrm_run")),
            args: vec![
                Expr::value(Value::string("win-1".to_string())),
                Expr::value(Value::string("hostname".to_string())),
            ],
            kwargs: Vec::new(),
        }));
        let mut hosts = HashMap::new();
        hosts.insert(
            "win-1".to_string(),
            InventoryHost {
                transport: "winrm".to_string(),
                fields: HashMap::from([
                    (
                        "address".to_string(),
                        InventoryValue::String("10.0.0.21".to_string()),
                    ),
                    (
                        "user".to_string(),
                        InventoryValue::String("Administrator".to_string()),
                    ),
                    (
                        "password".to_string(),
                        InventoryValue::String("secret".to_string()),
                    ),
                    ("port".to_string(), InventoryValue::U16(5986)),
                    (
                        "scheme".to_string(),
                        InventoryValue::String("https".to_string()),
                    ),
                ]),
            },
        );
        let inventory = ShellInventory {
            groups: HashMap::new(),
            hosts,
        };
        let script = render_node(
            vec![extern_decl(
                "winrm_run",
                "pwsh",
                "New-PSSession Invoke-Command Remove-PSSession",
                2,
            )],
            expr,
            &inventory,
        );
        assert!(script.contains("scheme = 'https'"));
    }
}
