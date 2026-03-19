use fp_core::ast::{
    Abi, BlockStmt, Expr, ExprBlock, ExprInvokeTarget, ExprKind, ExprMatch, ExprStringTemplate,
    FormatArgRef, FormatTemplatePart, ItemDeclFunction, ItemDefFunction, ItemKind, Node, NodeKind,
    PatternKind, Ty, Value,
};
use fp_core::intrinsics::IntrinsicCallKind;
use fp_core::ops::BinOpKind;
use fp_shell_core::{
    ScriptTarget, ShellInventory, TransportKind, extern_command, runtime_requirements,
    validate_extern_decl,
};
use std::cell::RefCell;
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

impl BashTarget {
    pub fn render(&self, node: &Node, inventory: &ShellInventory) -> Result<String, String> {
        let mut renderer = BashRenderer::new(inventory);
        renderer.render_program(node)?;
        Ok(renderer.finish())
    }
}

struct BashRenderer<'a> {
    inventory: &'a ShellInventory,
    externs: HashMap<String, ItemDeclFunction>,
    required_commands: RefCell<BTreeSet<String>>,
    lines: Vec<String>,
}

impl<'a> BashRenderer<'a> {
    fn new(inventory: &'a ShellInventory) -> Self {
        Self {
            inventory,
            externs: HashMap::new(),
            required_commands: RefCell::new(BTreeSet::new()),
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
            if let Some(address) = host.get_string("address") {
                script.push_str(&format!(
                    "FP_SSH_ADDRESS[{}]={}\n",
                    shell_arg_quote(name),
                    shell_arg_quote(address)
                ));
                script.push_str(&format!(
                    "FP_WINRM_ADDRESS[{}]={}\n",
                    shell_arg_quote(name),
                    shell_arg_quote(address)
                ));
            }
            if let Some(user) = host.get_string("user") {
                script.push_str(&format!(
                    "FP_SSH_USER[{}]={}\n",
                    shell_arg_quote(name),
                    shell_arg_quote(user)
                ));
                script.push_str(&format!(
                    "FP_DOCKER_USER[{}]={}\n",
                    shell_arg_quote(name),
                    shell_arg_quote(user)
                ));
                script.push_str(&format!(
                    "FP_WINRM_USER[{}]={}\n",
                    shell_arg_quote(name),
                    shell_arg_quote(user)
                ));
            }
            if let Some(port) = host.get_u16("port") {
                script.push_str(&format!(
                    "FP_SSH_PORT[{}]={}\n",
                    shell_arg_quote(name),
                    shell_arg_quote(&port.to_string())
                ));
                script.push_str(&format!(
                    "FP_WINRM_PORT[{}]={}\n",
                    shell_arg_quote(name),
                    shell_arg_quote(&port.to_string())
                ));
            }
            if let Some(container) = host.get_string("container") {
                script.push_str(&format!(
                    "FP_DOCKER_CONTAINER[{}]={}\n",
                    shell_arg_quote(name),
                    shell_arg_quote(container)
                ));
                script.push_str(&format!(
                    "FP_K8S_CONTAINER[{}]={}\n",
                    shell_arg_quote(name),
                    shell_arg_quote(container)
                ));
            }
            if let Some(pod) = host.get_string("pod") {
                script.push_str(&format!(
                    "FP_K8S_POD[{}]={}\n",
                    shell_arg_quote(name),
                    shell_arg_quote(pod)
                ));
            }
            if let Some(namespace) = host.get_string("namespace") {
                script.push_str(&format!(
                    "FP_K8S_NAMESPACE[{}]={}\n",
                    shell_arg_quote(name),
                    shell_arg_quote(namespace)
                ));
            }
            if let Some(context) = host.get_string("context") {
                script.push_str(&format!(
                    "FP_K8S_CONTEXT[{}]={}\n",
                    shell_arg_quote(name),
                    shell_arg_quote(context)
                ));
            }
            if let Some(password) = host.get_string("password") {
                script.push_str(&format!(
                    "FP_WINRM_PASSWORD[{}]={}\n",
                    shell_arg_quote(name),
                    shell_arg_quote(password)
                ));
            }
            if let Some(scheme) = host.get_string("scheme") {
                script.push_str(&format!(
                    "FP_WINRM_SCHEME[{}]={}\n",
                    shell_arg_quote(name),
                    shell_arg_quote(scheme)
                ));
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

    fn render_program(&mut self, node: &Node) -> Result<(), String> {
        let NodeKind::File(file) = node.kind() else {
            return Err("bash renderer requires file AST".to_string());
        };
        self.externs = extern_decl_map(file.items.iter(), ScriptTarget::Bash)?;
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
        self.push_line(0, &format!("{}() {{", def.name));
        for (index, param) in def.sig.params.iter().enumerate() {
            self.push_line(1, &format!("local {}=\"${}\"", param.name, index + 1));
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
            .ok_or_else(|| "bash match requires scrutinee".to_string())?;
        self.push_line(indent, &format!("case {} in", self.render_word(scrutinee)?));
        for case in &expr_match.cases {
            let pattern = match case
                .pat
                .as_ref()
                .and_then(|pat| extract_match_case_string(pat))
            {
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
        args: &[Expr],
        indent: usize,
    ) -> Result<(), String> {
        if self.externs.contains_key(name) {
            self.note_extern_requirements(name);
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
                let values = self.extract_string_list(&expr_for.iter)?;
                let values = values
                    .iter()
                    .map(|value| self.render_word(value))
                    .collect::<Result<Vec<_>, _>>()?
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
                    return Err(
                        "bash renderer only supports identifier invocation targets".to_string()
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
            ExprKind::If(expr_if) => {
                self.push_line(
                    indent,
                    &format!("if {}; then", self.render_expr_as_condition(&expr_if.cond)?),
                );
                self.render_result_expr(&expr_if.then, indent + 1)?;
                if let Some(elze) = &expr_if.elze {
                    self.push_line(indent, "else");
                    self.render_result_expr(elze, indent + 1)?;
                }
                self.push_line(indent, "fi");
                Ok(())
            }
            ExprKind::Match(expr_match) => {
                let scrutinee = expr_match
                    .scrutinee
                    .as_deref()
                    .ok_or_else(|| "bash match requires scrutinee".to_string())?;
                self.push_line(indent, &format!("case {} in", self.render_word(scrutinee)?));
                for case in &expr_match.cases {
                    let pattern = match case
                        .pat
                        .as_ref()
                        .and_then(|pat| extract_match_case_string(pat))
                    {
                        Some(pattern) => self.render_case_pattern(&pattern)?,
                        None => "*".to_string(),
                    };
                    self.push_line(indent + 1, &format!("{})", pattern));
                    self.render_result_expr(&case.body, indent + 2)?;
                    self.push_line(indent + 2, ";;");
                }
                self.push_line(indent, "esac");
                Ok(())
            }
            ExprKind::Invoke(invoke) => {
                let ExprInvokeTarget::Function(name) = &invoke.target else {
                    return Err(
                        "bash renderer only supports function invocation targets".to_string()
                    );
                };
                let Some(ident) = name.as_ident() else {
                    return Err(
                        "bash renderer only supports identifier invocation targets".to_string()
                    );
                };
                self.render_invoke_statement(ident.as_str(), &invoke.args, indent)
            }
            _ => {
                self.push_line(
                    indent,
                    &format!("printf '%s\\n' {}", self.render_value(expr)?),
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
                Value::Bool(flag) => Ok(if flag.value { "true" } else { "false" }.to_string()),
                _ => Ok(format!("[[ {} == 'true' ]]", self.render_word(expr)?)),
            },
            ExprKind::Name(name) => {
                let ident = name
                    .as_ident()
                    .ok_or_else(|| "bash condition only supports identifier names".to_string())?;
                Ok(format!("[[ \"${{{}}}\" == 'true' ]]", ident))
            }
            ExprKind::BinOp(bin_op) => {
                if matches!(
                    bin_op.kind,
                    BinOpKind::Gt | BinOpKind::Lt | BinOpKind::Ge | BinOpKind::Le
                ) {
                    return Ok(format!(
                        "[[ {} {} {} ]]",
                        self.render_int(&bin_op.lhs)?,
                        render_comparison(bin_op.kind),
                        self.render_int(&bin_op.rhs)?
                    ));
                }
                if matches!(bin_op.kind, BinOpKind::Eq | BinOpKind::Ne) {
                    return Ok(format!(
                        "[[ {} {} {} ]]",
                        self.render_word(&bin_op.lhs)?,
                        render_string_comparison(bin_op.kind),
                        self.render_word(&bin_op.rhs)?
                    ));
                }
                Err("unsupported bash condition expression".to_string())
            }
            ExprKind::Invoke(invoke) if invoke_is_name(invoke, "__fp_condition_command") => {
                Ok(self.render_command_expr(&invoke.args[0])?)
            }
            ExprKind::Paren(paren) => self.render_condition(&paren.expr),
            _ => Ok(format!("[[ {} == 'true' ]]", self.render_word(expr)?)),
        }
    }

    fn render_value(&self, expr: &Expr) -> Result<String, String> {
        if let Ok(values) = self.extract_string_list(expr) {
            return values
                .iter()
                .map(|value| self.render_word(value))
                .collect::<Result<Vec<_>, _>>()
                .map(|values| values.join(" "));
        }
        match expr.kind() {
            ExprKind::Value(value) => match &**value {
                Value::Bool(flag) => Ok(shell_arg_quote(&flag.value.to_string())),
                _ => self.render_word(expr),
            },
            ExprKind::BinOp(_) => Ok(shell_arg_quote(&self.render_condition(expr)?)),
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
                    "bash int expression only supports identifier names".to_string()
                })?;
                Ok(format!("${{{}}}", ident))
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
                    "$(({} {} {}))",
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
        match expr {
            Expr {
                kind: ExprKind::Value(value),
                ..
            } => match &**value {
                Value::String(text) => Ok(shell_arg_quote(&text.value)),
                Value::Int(value) => Ok(shell_arg_quote(&value.value.to_string())),
                Value::Bool(value) => Ok(shell_arg_quote(&value.value.to_string())),
                _ => Err("unsupported bash value expression".to_string()),
            },
            Expr {
                kind: ExprKind::Name(name),
                ..
            } => {
                let ident = name.as_ident().ok_or_else(|| {
                    "bash string expression only supports identifier names".to_string()
                })?;
                Ok(format!("\"${{{}}}\"", ident))
            }
            Expr {
                kind: ExprKind::Invoke(invoke),
                ..
            } => {
                let name = invoke_function_name(invoke)?;
                Ok(format!("\"$({})\"", self.render_call(name, &invoke.args)?))
            }
            Expr {
                kind: ExprKind::FormatString(template),
                ..
            } => self.render_format_template_word(template),
            Expr {
                kind: ExprKind::IntrinsicCall(call),
                ..
            } if call.kind == IntrinsicCallKind::Format => self.render_format_call_word(call),
            Expr {
                kind: ExprKind::Paren(paren),
                ..
            } => self.render_word(&paren.expr),
            _ => Err("unsupported bash string expression".to_string()),
        }
    }

    fn render_command_expr(&self, expr: &Expr) -> Result<String, String> {
        match expr {
            Expr {
                kind: ExprKind::Value(value),
                ..
            } => match &**value {
                Value::String(text) => Ok(text.value.clone()),
                Value::Int(value) => Ok(value.value.to_string()),
                Value::Bool(value) => Ok(value.value.to_string()),
                _ => Err("unsupported bash command expression".to_string()),
            },
            Expr {
                kind: ExprKind::Name(name),
                ..
            } => {
                let ident = name.as_ident().ok_or_else(|| {
                    "bash command expression only supports identifier names".to_string()
                })?;
                Ok(format!("${{{}}}", ident))
            }
            Expr {
                kind: ExprKind::Invoke(invoke),
                ..
            } => {
                let name = invoke_function_name(invoke)?;
                Ok(format!("$({})", self.render_call(name, &invoke.args)?))
            }
            Expr {
                kind: ExprKind::FormatString(template),
                ..
            } => self.render_format_template_command(template),
            Expr {
                kind: ExprKind::IntrinsicCall(call),
                ..
            } if call.kind == IntrinsicCallKind::Format => self.render_format_call_command(call),
            Expr {
                kind: ExprKind::Paren(paren),
                ..
            } => self.render_command_expr(&paren.expr),
            _ => Err("unsupported bash command expression".to_string()),
        }
    }

    fn render_format_template_word(&self, template: &ExprStringTemplate) -> Result<String, String> {
        let mut out = String::from("\"");
        for part in &template.parts {
            match part {
                FormatTemplatePart::Literal(text) => out.push_str(&escape_double_quotes(text)),
                FormatTemplatePart::Placeholder(placeholder) => match &placeholder.arg_ref {
                    FormatArgRef::Named(name) => out.push_str(&format!("${{{}}}", name)),
                    _ => {
                        return Err(
                            "bash format strings only support named placeholders".to_string()
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
                    FormatArgRef::Named(name) => out.push_str(&format!("${{{}}}", name)),
                    _ => {
                        return Err(
                            "bash format strings only support named placeholders".to_string()
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
            return Err("bash format call missing template".to_string());
        };
        let ExprKind::FormatString(template) = template.kind() else {
            return Err("bash format call requires format template".to_string());
        };
        let mut out = String::from("\"");
        let mut implicit_index = 1usize;
        for part in &template.parts {
            match part {
                FormatTemplatePart::Literal(text) => out.push_str(&escape_double_quotes(text)),
                FormatTemplatePart::Placeholder(placeholder) => {
                    let arg = match placeholder.arg_ref {
                        FormatArgRef::Implicit => {
                            let arg = call.args.get(implicit_index).ok_or_else(|| {
                                "bash format call missing implicit argument".to_string()
                            })?;
                            implicit_index += 1;
                            arg
                        }
                        FormatArgRef::Positional(index) => call
                            .args
                            .get(index + 1)
                            .ok_or_else(|| "bash format call missing positional argument".to_string())?,
                        FormatArgRef::Named(ref name) => call
                            .args
                            .iter()
                            .skip(1)
                            .find(|arg| matches!(arg.kind(), ExprKind::Name(found) if found.as_ident().is_some_and(|ident| ident.as_str() == name)))
                            .ok_or_else(|| "bash format call missing named argument".to_string())?,
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
            return Err("bash format call missing template".to_string());
        };
        let ExprKind::FormatString(template) = template.kind() else {
            return Err("bash format call requires format template".to_string());
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
                                "bash format call missing implicit argument".to_string()
                            })?;
                            implicit_index += 1;
                            arg
                        }
                        FormatArgRef::Positional(index) => call
                            .args
                            .get(index + 1)
                            .ok_or_else(|| "bash format call missing positional argument".to_string())?,
                        FormatArgRef::Named(ref name) => call
                            .args
                            .iter()
                            .skip(1)
                            .find(|arg| matches!(arg.kind(), ExprKind::Name(found) if found.as_ident().is_some_and(|ident| ident.as_str() == name)))
                            .ok_or_else(|| "bash format call missing named argument".to_string())?,
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
                Value::String(text) => Ok(escape_double_quotes(&text.value)),
                Value::Int(value) => Ok(value.value.to_string()),
                Value::Bool(value) => Ok(value.value.to_string()),
                _ => Err("unsupported bash string fragment".to_string()),
            },
            ExprKind::Name(name) => {
                let ident = name.as_ident().ok_or_else(|| {
                    "bash string fragment only supports identifier names".to_string()
                })?;
                Ok(format!("${{{}}}", ident))
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
            _ => Err("unsupported bash string fragment".to_string()),
        }
    }

    fn render_command_fragment(&self, expr: &Expr) -> Result<String, String> {
        match expr.kind() {
            ExprKind::Value(value) => match &**value {
                Value::String(text) => Ok(text.value.clone()),
                Value::Int(value) => Ok(value.value.to_string()),
                Value::Bool(value) => Ok(value.value.to_string()),
                _ => Err("unsupported bash command fragment".to_string()),
            },
            ExprKind::Name(name) => {
                let ident = name.as_ident().ok_or_else(|| {
                    "bash command fragment only supports identifier names".to_string()
                })?;
                Ok(format!("${{{}}}", ident))
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
            _ => Err("unsupported bash command fragment".to_string()),
        }
    }

    fn render_case_pattern(&self, expr: &Expr) -> Result<String, String> {
        let Some(text) = string_literal_value(expr) else {
            return Err(format!(
                "bash case patterns must be string literals, found {:?}",
                expr
            ));
        };
        Ok(shell_case_quote(&text))
    }

    fn push_line(&mut self, indent: usize, text: &str) {
        self.lines
            .push(format!("{}{}", "    ".repeat(indent), text));
    }

    fn render_call(&self, name: &str, args: &[Expr]) -> Result<String, String> {
        if self.externs.contains_key(name) {
            self.note_extern_requirements(name);
            return Ok(self.render_bash_extern_call(name, args)?);
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

    fn render_bash_extern_call(&self, name: &str, args: &[Expr]) -> Result<String, String> {
        Ok(match name {
            "runtime_host_transport" => {
                let host = self.expect_string_arg(args, 0);
                format!(
                    "printf '%s\\n' \"${{FP_HOST_TRANSPORT[{}]:-ssh}}\"",
                    self.render_word(host)?
                )
            }
            "runtime_host_address" => self.render_host_map_lookup("FP_SSH_ADDRESS", args)?,
            "runtime_host_user" => self.render_host_map_lookup("FP_SSH_USER", args)?,
            "runtime_host_port" => self.render_host_map_lookup("FP_SSH_PORT", args)?,
            "runtime_host_container" => self.render_host_map_lookup("FP_DOCKER_CONTAINER", args)?,
            "runtime_host_pod" => self.render_host_map_lookup("FP_K8S_POD", args)?,
            "runtime_host_namespace" => self.render_host_map_lookup("FP_K8S_NAMESPACE", args)?,
            "runtime_host_context" => self.render_host_map_lookup("FP_K8S_CONTEXT", args)?,
            "runtime_host_password" => self.render_host_map_lookup("FP_WINRM_PASSWORD", args)?,
            "runtime_host_scheme" => self.render_host_map_lookup("FP_WINRM_SCHEME", args)?,
            "runtime_temp_path" => "mktemp".to_string(),
            other => self.render_generic_extern(other, args)?,
        })
    }

    fn render_bash_extern_statement(
        &mut self,
        name: &str,
        args: &[Expr],
    ) -> Result<String, String> {
        Ok(match name {
            "winrm_run" => format!(
                "winrm_pwsh {} run {}",
                self.render_value(&args[0])?,
                self.render_value(&args[1])?
            ),
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
                    self.render_command_expr(self.expect_string_arg(args, 2))?,
                    self.render_command_expr(self.expect_string_arg(args, 0))?,
                    self.render_command_expr(self.expect_string_arg(args, 1))?
                ))
            ),
            "runtime_fail" => format!("echo {} >&2; return 1", self.render_value(&args[0])?),
            "runtime_set_changed" => format!(
                "__fp_last_changed={}",
                if is_true_expr(args.first()) { "1" } else { "0" }
            ),
            other => self.render_generic_extern(other, args)?,
        })
    }

    fn render_generic_extern(&self, name: &str, args: &[Expr]) -> Result<String, String> {
        let command = self
            .externs
            .get(name)
            .and_then(extern_command)
            .unwrap_or_else(|| name.to_string());
        let rendered = args
            .iter()
            .map(|arg| self.render_value(arg))
            .collect::<Result<Vec<_>, _>>()?;
        if rendered.is_empty() {
            Ok(command)
        } else {
            Ok(format!("{} {}", command, rendered.join(" ")))
        }
    }

    fn render_host_map_lookup(&self, map_name: &str, args: &[Expr]) -> Result<String, String> {
        let host = self.expect_string_arg(args, 0);
        Ok(format!(
            "printf '%s\\n' \"${{{}[{}]:-}}\"",
            map_name,
            self.render_word(host)?
        ))
    }

    fn render_runtime_validator(&self) -> String {
        let commands = self.required_commands.borrow();
        if commands.is_empty() {
            return String::new();
        }
        let mut script = String::new();
        script.push_str("fp_validate_runtime() {\n");
        for command in commands.iter() {
            script.push_str(&format!(
                "  command -v {} >/dev/null 2>&1 || {{ echo \"missing required command: {}\" >&2; exit 1; }}\n",
                shell_arg_quote(&command),
                escape_double_quotes(&command)
            ));
        }
        script.push_str("}\n\nfp_validate_runtime\n\n");
        script
    }

    fn note_extern_requirements(&self, name: &str) {
        let Some(function) = self.externs.get(name) else {
            return;
        };
        let mut commands = self.required_commands.borrow_mut();
        for command in runtime_requirements(function, ScriptTarget::Bash) {
            commands.insert(command);
        }
    }

    fn expect_string_arg<'b>(&self, args: &'b [Expr], index: usize) -> &'b Expr {
        &args[index]
    }

    fn extract_string_list<'b>(&self, expr: &'b Expr) -> Result<Vec<&'b Expr>, String> {
        match expr.kind() {
            ExprKind::Array(array) => Ok(array.values.iter().collect()),
            ExprKind::Tuple(tuple) => Ok(tuple.values.iter().collect()),
            _ => Err("bash renderer only supports string-list for iterables".to_string()),
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
        BinOpKind::Eq => "==",
        BinOpKind::Ne => "!=",
        _ => unreachable!(),
    }
}

fn string_literal_value(expr: &Expr) -> Option<String> {
    match expr.kind() {
        ExprKind::Value(value) => match &**value {
            Value::String(text) => Some(text.value.clone()),
            _ => None,
        },
        _ => None,
    }
}

fn invoke_function_name<'a>(invoke: &'a fp_core::ast::ExprInvoke) -> Result<&'a str, String> {
    let ExprInvokeTarget::Function(name) = &invoke.target else {
        return Err("bash renderer only supports function invocation targets".to_string());
    };
    let Some(ident) = name.as_ident() else {
        return Err("bash renderer only supports identifier invocation targets".to_string());
    };
    Ok(ident.as_str())
}

fn invoke_is_name(invoke: &fp_core::ast::ExprInvoke, expected: &str) -> bool {
    matches!(
        &invoke.target,
        ExprInvokeTarget::Function(name) if name.as_ident().is_some_and(|ident| ident.as_str() == expected)
    )
}

fn is_true_expr(expr: Option<&Expr>) -> bool {
    matches!(
        expr.map(Expr::kind),
        Some(ExprKind::Value(value)) if matches!(&**value, Value::Bool(flag) if flag.value)
    )
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

fn function_returns_value(def: &ItemDefFunction) -> bool {
    !matches!(def.sig.ret_ty.as_ref(), None | Some(Ty::Unit(_)))
}

fn extern_decl_map<'a>(
    items: impl Iterator<Item = &'a fp_core::ast::Item>,
    target: ScriptTarget,
) -> Result<HashMap<String, ItemDeclFunction>, String> {
    let mut externs = HashMap::new();
    for item in items {
        match item.kind() {
            ItemKind::DeclFunction(function) => {
                if !matches!(&function.sig.abi, Abi::Named(abi) if abi == "bash") {
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
    use fp_core::ast::{
        Abi, AttrMeta, AttrMetaNameValue, AttrStyle, Attribute, Expr, ExprInvoke, ExprInvokeTarget,
        ExprKind, File, FunctionParam, FunctionSignature, Ident, Item, ItemDeclFunction, ItemKind,
        Name, Node, Path, Ty,
    };
    use fp_shell_core::{InventoryHost, InventoryValue, TransportKind};
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
        BashTarget::new()
            .render(&node, inventory)
            .expect("render should succeed")
    }

    #[test]
    fn renders_ssh_dispatch() {
        let expr = Expr::new(ExprKind::Invoke(ExprInvoke {
            span: Default::default(),
            target: ExprInvokeTarget::Function(Name::ident("ssh")),
            args: vec![
                Expr::value(Value::string("web-1".to_string())),
                Expr::value(Value::string("uptime".to_string())),
            ],
            kwargs: Vec::new(),
        }));
        let mut hosts = HashMap::new();
        hosts.insert(
            "web-1".to_string(),
            InventoryHost {
                transport: TransportKind::Ssh,
                fields: HashMap::from([
                    (
                        "address".to_string(),
                        InventoryValue::String("10.0.0.11".to_string()),
                    ),
                    (
                        "user".to_string(),
                        InventoryValue::String("deploy".to_string()),
                    ),
                ]),
            },
        );
        let inventory = ShellInventory {
            groups: HashMap::new(),
            hosts,
        };
        let script = render_node(vec![extern_decl("ssh", "bash", "ssh", 2)], expr, &inventory);
        assert!(script.contains("FP_HOST_TRANSPORT['web-1']='ssh'"));
        assert!(script.contains("ssh 'web-1' 'uptime'"));
    }

    #[test]
    fn renders_winrm_dispatch_via_pwsh() {
        let expr = Expr::new(ExprKind::Invoke(ExprInvoke {
            span: Default::default(),
            target: ExprInvokeTarget::Function(Name::ident("winrm_copy")),
            args: vec![
                Expr::value(Value::string("win-1".to_string())),
                Expr::value(Value::string("artifact.zip".to_string())),
                Expr::value(Value::string(r"C:\Temp\artifact.zip".to_string())),
            ],
            kwargs: Vec::new(),
        }));
        let mut hosts = HashMap::new();
        hosts.insert(
            "win-1".to_string(),
            InventoryHost {
                transport: TransportKind::Winrm,
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
            vec![extern_decl("winrm_copy", "bash", "pwsh", 3)],
            expr,
            &inventory,
        );
        assert!(script.contains("pwsh -NoProfile -NonInteractive -Command"));
        assert!(script.contains("FP_WINRM_SCHEME['win-1']='https'"));
        assert!(script.contains("winrm_pwsh 'win-1' copy 'artifact.zip' 'C:\\Temp\\artifact.zip'"));
        assert!(!script.contains("backend_copy_winrm"));
        assert!(!script.contains("evil-winrm"));
    }
}
