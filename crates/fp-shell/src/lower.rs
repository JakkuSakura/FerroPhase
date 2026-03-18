use fp_core::ast::{
    Abi, BlockStmt, Expr, ExprBlock, ExprFor, ExprIf, ExprIntrinsicCall, ExprInvoke,
    ExprInvokeTarget, ExprMatch, ExprMatchCase, Item, ItemDefFunction, ItemKind, Name, Node,
    NodeKind, PatternKind, Value,
};
use fp_core::intrinsics::IntrinsicCallKind;
use fp_core::ops::{BinOpKind, UnOpKind};
use fp_shell_core::{
    ArithmeticOp, Block, BoolExpr, ComparisonOp, ConditionExpr, ExternalFunction, ForEachStmt,
    FunctionDef, HostExpr, IfStmt, IntExpr, InvokeStmt, LetStmt, OperationCopy, OperationGuards,
    OperationRsync, OperationRun, OperationTemplate, RsyncOptions, ScriptItem, ScriptProgram,
    ShellInventory, Statement, StringComparisonOp, StringExpr, StringPart, ValueExpr, WhileStmt,
};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone)]
enum VarValue {
    String(StringExpr),
    Int(i64),
    Bool(bool),
    StringList(Vec<StringExpr>),
}

#[derive(Debug, Clone, Default)]
struct EmitContext {
    hosts: Option<Vec<HostExpr>>,
}

pub fn lower_node(node: &Node, inventory: &ShellInventory) -> Result<ScriptProgram, String> {
    let mut lowerer = Lowerer::new(inventory);
    lowerer.lower_node(node, &EmitContext::default())?;
    Ok(lowerer.program)
}

struct Lowerer<'a> {
    program: ScriptProgram,
    scopes: Vec<HashMap<String, VarValue>>,
    known_functions: HashSet<String>,
    inventory: &'a ShellInventory,
}

impl<'a> Lowerer<'a> {
    fn new(inventory: &'a ShellInventory) -> Self {
        Self {
            program: ScriptProgram::default(),
            scopes: vec![HashMap::new()],
            known_functions: HashSet::new(),
            inventory,
        }
    }

    fn lower_node(&mut self, node: &Node, context: &EmitContext) -> Result<(), String> {
        match node.kind() {
            NodeKind::File(file) => {
                for item in &file.items {
                    self.discover_functions(item);
                }
                for item in &file.items {
                    self.lower_item(item, context, None)?;
                }
            }
            NodeKind::Item(item) => self.lower_item(item, context, None)?,
            NodeKind::Expr(expr) => {
                let mut out = Vec::new();
                self.lower_expr_into(expr, context, &mut out)?;
                self.program
                    .items
                    .extend(out.into_iter().map(ScriptItem::Statement));
            }
            NodeKind::Query(_) | NodeKind::Schema(_) | NodeKind::Workspace(_) => {}
        }
        Ok(())
    }

    fn discover_functions(&mut self, item: &Item) {
        match item.kind() {
            ItemKind::DefFunction(function) => {
                self.known_functions
                    .insert(function.name.as_str().to_string());
            }
            ItemKind::DeclFunction(function) => {
                let abi = match &function.sig.abi {
                    Abi::Rust => return,
                    Abi::Named(name) => name.clone(),
                };
                self.program.externs.insert(
                    function.name.as_str().to_string(),
                    ExternalFunction {
                        name: function.name.as_str().to_string(),
                        abi,
                    },
                );
            }
            ItemKind::Module(module) => {
                for child in &module.items {
                    self.discover_functions(child);
                }
            }
            _ => {}
        }
    }

    fn lower_item(
        &mut self,
        item: &Item,
        context: &EmitContext,
        mut target: Option<&mut Vec<Statement>>,
    ) -> Result<(), String> {
        match item.kind() {
            ItemKind::DefFunction(function) => {
                let name = function.name.as_str().to_string();
                if name == "main" {
                    if let Some(target) = target {
                        self.lower_expr_into(&function.body, context, target)?;
                    } else {
                        let mut temp = Vec::new();
                        self.lower_expr_into(&function.body, context, &mut temp)?;
                        self.program
                            .items
                            .extend(temp.into_iter().map(ScriptItem::Statement));
                    }
                } else {
                    let def = self.lower_function(function)?;
                    self.program.items.push(ScriptItem::Function(def));
                }
            }
            ItemKind::DefConst(def) => {
                if let Some(target) = target {
                    self.lower_expr_into(&def.value, context, target)?;
                }
            }
            ItemKind::DefStatic(def) => {
                if let Some(target) = target {
                    self.lower_expr_into(&def.value, context, target)?;
                }
            }
            ItemKind::Module(module) => {
                for child in &module.items {
                    self.lower_item(child, context, target.as_deref_mut())?;
                }
            }
            ItemKind::DeclFunction(_) => {}
            ItemKind::Expr(expr) => {
                if let Some(target) = target {
                    self.lower_expr_into(expr, context, target)?;
                } else {
                    let mut temp = Vec::new();
                    self.lower_expr_into(expr, context, &mut temp)?;
                    self.program
                        .items
                        .extend(temp.into_iter().map(ScriptItem::Statement));
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn lower_function(&mut self, function: &ItemDefFunction) -> Result<FunctionDef, String> {
        self.push_scope();
        let params = function
            .sig
            .params
            .iter()
            .map(|param| {
                let name = param.name.as_str().to_string();
                self.bind_var(
                    name.clone(),
                    VarValue::String(StringExpr::Variable(name.clone())),
                );
                name
            })
            .collect::<Vec<_>>();
        let mut body = Vec::new();
        self.lower_expr_into(&function.body, &EmitContext::default(), &mut body)?;
        self.pop_scope();
        Ok(FunctionDef {
            name: function.name.as_str().to_string(),
            params,
            body: Block { statements: body },
        })
    }

    fn lower_expr_into(
        &mut self,
        expr: &Expr,
        context: &EmitContext,
        out: &mut Vec<Statement>,
    ) -> Result<(), String> {
        match expr.kind() {
            fp_core::ast::ExprKind::Block(block) => self.lower_block(block, context, out),
            fp_core::ast::ExprKind::Invoke(invoke) => {
                if let Some(statement) = self.lower_shell_invoke(invoke, context)? {
                    out.extend(statement);
                    Ok(())
                } else if let Some(statement) = self.lower_function_invoke(invoke)? {
                    out.push(statement);
                    Ok(())
                } else {
                    Ok(())
                }
            }
            fp_core::ast::ExprKind::If(expr_if) => {
                out.push(Statement::If(self.lower_if(expr_if, context)?));
                Ok(())
            }
            fp_core::ast::ExprKind::Match(expr_match) => {
                let block = self.lower_match(expr_match, context)?;
                out.extend(block.statements);
                Ok(())
            }
            fp_core::ast::ExprKind::While(expr_while) => {
                let condition = self.lower_condition(&expr_while.cond);
                let mut body = Vec::new();
                self.lower_expr_into(&expr_while.body, context, &mut body)?;
                out.push(Statement::While(WhileStmt {
                    condition,
                    body: Block { statements: body },
                }));
                Ok(())
            }
            fp_core::ast::ExprKind::For(expr_for) => {
                out.push(Statement::ForEach(self.lower_for(expr_for, context)?));
                Ok(())
            }
            fp_core::ast::ExprKind::Let(expr_let) => {
                let Some(name) = expr_let.pat.as_ident() else {
                    return Ok(());
                };
                let value = self.lower_value_expr(&expr_let.expr)?;
                self.bind_value_expr(name.as_str().to_string(), &value);
                out.push(Statement::Let(LetStmt {
                    name: name.as_str().to_string(),
                    value,
                }));
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn lower_block(
        &mut self,
        block: &ExprBlock,
        context: &EmitContext,
        out: &mut Vec<Statement>,
    ) -> Result<(), String> {
        self.push_scope();
        for statement in &block.stmts {
            match statement {
                BlockStmt::Item(item) => self.lower_item(item, context, Some(out))?,
                BlockStmt::Expr(expr) => self.lower_expr_into(&expr.expr, context, out)?,
                BlockStmt::Let(stmt) => {
                    let Some(name) = stmt.pat.as_ident() else {
                        continue;
                    };
                    let Some(init) = &stmt.init else {
                        continue;
                    };
                    let value = self.lower_value_expr(init)?;
                    self.bind_value_expr(name.as_str().to_string(), &value);
                    out.push(Statement::Let(LetStmt {
                        name: name.as_str().to_string(),
                        value,
                    }));
                }
                BlockStmt::Noop | BlockStmt::Any(_) => {}
            }
        }
        self.pop_scope();
        Ok(())
    }

    fn lower_shell_invoke(
        &mut self,
        invoke: &ExprInvoke,
        context: &EmitContext,
    ) -> Result<Option<Vec<Statement>>, String> {
        let Some(path) = invoke_target_segments(&invoke.target) else {
            return Ok(None);
        };

        if path == ["std", "host", "on"] || path == ["std", "shell", "on"] {
            return self.lower_host_scope(invoke);
        }

        if path == ["std", "server", "shell"] || path == ["std", "shell", "run"] {
            let command_expr = invoke
                .args
                .first()
                .or_else(|| kwarg_value(invoke, &["command", "cmd"]));
            let Some(command_expr) = command_expr else {
                return Ok(None);
            };
            let command = self.lower_string_expr(command_expr).ok_or_else(|| {
                "std::server::shell command must resolve to string/int/bool (including f\"...\"/format!(...))".to_string()
            })?;
            let sudo = kwarg_value(invoke, &["sudo"])
                .and_then(|expr| self.resolve_bool_expr(expr))
                .unwrap_or(false);
            let cwd = kwarg_value(invoke, &["cwd"]).and_then(|expr| self.lower_string_expr(expr));
            let guards = self.lower_guards(invoke);
            let command = apply_command_options(command, cwd.clone(), sudo);
            let hosts = self.resolve_hosts(invoke, context);
            return Ok(Some(vec![Statement::Run(OperationRun {
                hosts,
                command,
                cwd,
                sudo,
                guards,
            })]));
        }

        if path == ["std", "files", "copy"] || path == ["std", "shell", "copy"] {
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
                return Ok(None);
            };
            let source = self
                .lower_string_expr(source_expr)
                .ok_or_else(|| "copy source must resolve to string".to_string())?;
            let destination = self
                .lower_string_expr(destination_expr)
                .ok_or_else(|| "copy destination must resolve to string".to_string())?;
            let hosts = self.resolve_hosts(invoke, context);
            return Ok(Some(vec![Statement::Copy(OperationCopy {
                hosts,
                source,
                destination,
                guards: self.lower_guards(invoke),
            })]));
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
                return Ok(None);
            };
            let source = self
                .lower_string_expr(source_expr)
                .ok_or_else(|| "template source must resolve to string".to_string())?;
            let destination = self
                .lower_string_expr(destination_expr)
                .ok_or_else(|| "template destination must resolve to string".to_string())?;
            let vars = kwarg_value(invoke, &["vars"])
                .and_then(|expr| self.resolve_vars_map(expr))
                .unwrap_or_default();
            let hosts = self.resolve_hosts(invoke, context);
            return Ok(Some(vec![Statement::Template(OperationTemplate {
                hosts,
                source,
                destination,
                vars,
                guards: self.lower_guards(invoke),
            })]));
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
                return Ok(None);
            };
            let source = self
                .lower_string_expr(source_expr)
                .ok_or_else(|| "rsync source must resolve to string".to_string())?;
            let destination = self
                .lower_string_expr(destination_expr)
                .ok_or_else(|| "rsync destination must resolve to string".to_string())?;
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
            return Ok(Some(vec![Statement::Rsync(OperationRsync {
                hosts,
                source,
                destination,
                options,
                guards: self.lower_guards(invoke),
            })]));
        }

        if path == ["std", "service", "restart"] {
            let service_expr = invoke
                .args
                .first()
                .or_else(|| kwarg_value(invoke, &["name", "service"]));
            let Some(service_expr) = service_expr else {
                return Ok(None);
            };
            let service_name = self
                .lower_string_expr(service_expr)
                .ok_or_else(|| "service name must resolve to string".to_string())?;
            let hosts = self.resolve_hosts(invoke, context);
            return Ok(Some(vec![Statement::Run(OperationRun {
                hosts,
                command: apply_command_options(
                    StringExpr::Interpolated(vec![
                        StringPart::Literal("systemctl restart ".to_string()),
                        part_from_string_expr(service_name.clone()),
                    ]),
                    None,
                    kwarg_value(invoke, &["sudo"])
                        .and_then(|expr| self.resolve_bool_expr(expr))
                        .unwrap_or(true),
                ),
                cwd: None,
                sudo: true,
                guards: self.lower_guards(invoke),
            })]));
        }

        Ok(None)
    }

    fn lower_host_scope(&mut self, invoke: &ExprInvoke) -> Result<Option<Vec<Statement>>, String> {
        if invoke.args.len() < 2 {
            return Err("std::host::on expects host selector and closure body".to_string());
        }
        let Some(hosts) = self.parse_host_selector(&invoke.args[0]) else {
            return Err(
                "std::host::on host selector must be a string or list/tuple/array of strings"
                    .to_string(),
            );
        };
        let scoped = EmitContext { hosts: Some(hosts) };
        let mut out = Vec::new();
        for action in invoke.args.iter().skip(1) {
            let fp_core::ast::ExprKind::Closure(closure) = action.kind() else {
                return Err(
                    "std::host::on requires closure body syntax: std::host::on(hosts, || { ... })"
                        .to_string(),
                );
            };
            self.lower_expr_into(&closure.body, &scoped, &mut out)?;
        }
        for kwarg in &invoke.kwargs {
            let fp_core::ast::ExprKind::Closure(closure) = kwarg.value.kind() else {
                return Err(
                    "std::host::on requires closure body syntax: std::host::on(hosts, || { ... })"
                        .to_string(),
                );
            };
            self.lower_expr_into(&closure.body, &scoped, &mut out)?;
        }
        Ok(Some(out))
    }

    fn lower_function_invoke(&mut self, invoke: &ExprInvoke) -> Result<Option<Statement>, String> {
        let Some(path) = invoke_target_segments(&invoke.target) else {
            return Ok(None);
        };
        if path.len() != 1 {
            return Ok(None);
        }
        let name = &path[0];
        if !self.is_known_callable(name) {
            return Ok(None);
        }
        let mut args = Vec::new();
        for arg in &invoke.args {
            args.push(self.lower_value_expr(arg)?);
        }
        Ok(Some(Statement::Invoke(InvokeStmt {
            name: name.clone(),
            args,
        })))
    }

    fn lower_if(&mut self, expr_if: &ExprIf, context: &EmitContext) -> Result<IfStmt, String> {
        let condition = self.lower_condition(&expr_if.cond);
        let mut then_block = Vec::new();
        self.lower_expr_into(&expr_if.then, context, &mut then_block)?;
        let else_block = if let Some(else_expr) = &expr_if.elze {
            let mut else_statements = Vec::new();
            self.lower_expr_into(else_expr, context, &mut else_statements)?;
            Some(Block {
                statements: else_statements,
            })
        } else {
            None
        };
        Ok(IfStmt {
            condition,
            then_block: Block {
                statements: then_block,
            },
            else_block,
        })
    }

    fn lower_match(
        &mut self,
        expr_match: &ExprMatch,
        context: &EmitContext,
    ) -> Result<Block, String> {
        let Some(scrutinee) = expr_match.scrutinee.as_deref() else {
            return Err("match expression requires a scrutinee".to_string());
        };
        let scrutinee = self
            .lower_string_expr(scrutinee)
            .ok_or_else(|| "match scrutinee must resolve to string".to_string())?;
        self.lower_match_cases(&scrutinee, &expr_match.cases, context)
    }

    fn lower_match_cases(
        &mut self,
        scrutinee: &StringExpr,
        cases: &[ExprMatchCase],
        context: &EmitContext,
    ) -> Result<Block, String> {
        let Some((first, rest)) = cases.split_first() else {
            return Ok(Block::default());
        };
        let fallback = self.lower_match_cases(scrutinee, rest, context)?;
        self.lower_match_case(scrutinee, first, context, fallback)
    }

    fn lower_match_case(
        &mut self,
        scrutinee: &StringExpr,
        case: &ExprMatchCase,
        context: &EmitContext,
        fallback: Block,
    ) -> Result<Block, String> {
        let mut body_statements = Vec::new();
        self.lower_expr_into(&case.body, context, &mut body_statements)?;
        let body_block = Block {
            statements: body_statements,
        };

        let fallback_opt = if fallback.statements.is_empty() {
            None
        } else {
            Some(fallback.clone())
        };

        let pattern_condition = self.lower_match_case_pattern(scrutinee, case)?;
        let guarded_body = if let Some(guard) = &case.guard {
            Block {
                statements: vec![Statement::If(IfStmt {
                    condition: self.lower_condition(guard),
                    then_block: body_block,
                    else_block: fallback_opt.clone(),
                })],
            }
        } else {
            body_block
        };

        if let Some(condition) = pattern_condition {
            Ok(Block {
                statements: vec![Statement::If(IfStmt {
                    condition,
                    then_block: guarded_body,
                    else_block: fallback_opt,
                })],
            })
        } else if case.guard.is_some() {
            Ok(guarded_body)
        } else {
            Ok(guarded_body)
        }
    }

    fn lower_match_case_pattern(
        &self,
        scrutinee: &StringExpr,
        case: &ExprMatchCase,
    ) -> Result<Option<ConditionExpr>, String> {
        let Some(pattern) = &case.pat else {
            return Ok(None);
        };
        match pattern.kind() {
            PatternKind::Wildcard(_) => Ok(None),
            PatternKind::Variant(variant) if variant.pattern.is_none() => {
                let rhs = self.lower_string_expr(&variant.name).ok_or_else(|| {
                    "match pattern must resolve to a string literal or string expression"
                        .to_string()
                })?;
                Ok(Some(ConditionExpr::Bool(BoolExpr::StringComparison {
                    lhs: scrutinee.clone(),
                    op: StringComparisonOp::Eq,
                    rhs,
                })))
            }
            _ => Err(
                "match patterns in fp-shell currently support string literals and `_` only"
                    .to_string(),
            ),
        }
    }

    fn lower_for(
        &mut self,
        expr_for: &ExprFor,
        context: &EmitContext,
    ) -> Result<ForEachStmt, String> {
        let PatternKind::Ident(pattern) = expr_for.pat.kind() else {
            return Err("for-loop pattern must be an identifier".to_string());
        };
        let binding = pattern.ident.as_str().to_string();
        let values = self
            .resolve_string_list_expr(&expr_for.iter)
            .ok_or_else(|| "for-loop iterable must resolve to a string list".to_string())?;
        self.push_scope();
        self.bind_var(
            binding.clone(),
            VarValue::String(StringExpr::Variable(binding.clone())),
        );
        let mut body = Vec::new();
        self.lower_expr_into(&expr_for.body, context, &mut body)?;
        self.pop_scope();
        Ok(ForEachStmt {
            binding,
            values,
            body: Block { statements: body },
        })
    }

    fn lower_condition(&self, expr: &Expr) -> ConditionExpr {
        if let Some(flag) = self.lower_bool_expr(expr) {
            return ConditionExpr::Bool(flag);
        }
        if let fp_core::ast::ExprKind::Invoke(invoke) = expr.kind() {
            if let Some(path) = invoke_target_segments(&invoke.target) {
                if path == ["std", "server", "shell"] || path == ["std", "shell", "run"] {
                    if let Some(command_expr) = invoke
                        .args
                        .first()
                        .or_else(|| kwarg_value(invoke, &["command", "cmd"]))
                    {
                        if let Some(command) = self.lower_string_expr(command_expr) {
                            return ConditionExpr::Command(command);
                        }
                    }
                }
            }
        }
        if let Some(text) = self.lower_string_expr(expr) {
            return ConditionExpr::StringTruthy(text);
        }
        ConditionExpr::Bool(BoolExpr::Literal(true))
    }

    fn lower_guards(&self, invoke: &ExprInvoke) -> OperationGuards {
        OperationGuards {
            only_if: kwarg_value(invoke, &["only_if"])
                .and_then(|expr| self.lower_string_expr(expr)),
            unless: kwarg_value(invoke, &["unless"]).and_then(|expr| self.lower_string_expr(expr)),
            creates: kwarg_value(invoke, &["creates"])
                .and_then(|expr| self.lower_string_expr(expr)),
            removes: kwarg_value(invoke, &["removes"])
                .and_then(|expr| self.lower_string_expr(expr)),
        }
    }

    fn lower_value_expr(&self, expr: &Expr) -> Result<ValueExpr, String> {
        if let fp_core::ast::ExprKind::Name(Name::Ident(ident)) = expr.kind() {
            if let Some(value) = self.lookup_var(ident.as_str()) {
                return Ok(match value {
                    VarValue::String(value) => ValueExpr::String(value.clone()),
                    VarValue::Int(value) => ValueExpr::Int(IntExpr::Literal(*value)),
                    VarValue::Bool(value) => ValueExpr::Bool(BoolExpr::Literal(*value)),
                    VarValue::StringList(values) => ValueExpr::StringList(values.clone()),
                });
            }
        }
        if let Some(values) = self.resolve_string_list_expr(expr) {
            if values.len() > 1 {
                return Ok(ValueExpr::StringList(values));
            }
        }
        if let Some(value) = self.lower_bool_expr(expr) {
            return Ok(ValueExpr::Bool(value));
        }
        if let Some(value) = self.lower_int_expr(expr) {
            return Ok(ValueExpr::Int(value));
        }
        self.lower_string_expr(expr)
            .map(ValueExpr::String)
            .ok_or_else(|| "expression could not be lowered".to_string())
    }

    fn lower_string_expr(&self, expr: &Expr) -> Option<StringExpr> {
        if let Some(value) = self.lower_int_expr(expr) {
            return Some(match value {
                IntExpr::Literal(v) => StringExpr::Literal(v.to_string()),
                IntExpr::Variable(name) => StringExpr::Variable(name),
                _ => return None,
            });
        }
        match expr.kind() {
            fp_core::ast::ExprKind::Invoke(invoke) => {
                let path = invoke_target_segments(&invoke.target)?;
                if path.len() != 1 {
                    return None;
                }
                let name = &path[0];
                if !self.is_known_callable(name) {
                    return None;
                }
                let mut args = Vec::new();
                for arg in &invoke.args {
                    args.push(self.lower_value_expr(arg).ok()?);
                }
                Some(StringExpr::Call {
                    name: name.clone(),
                    args,
                })
            }
            fp_core::ast::ExprKind::Value(value) => match value.as_ref() {
                Value::String(text) => Some(StringExpr::Literal(text.value.clone())),
                Value::Int(int_value) => Some(StringExpr::Literal(int_value.value.to_string())),
                Value::Bool(bool_value) => Some(StringExpr::Literal(bool_value.value.to_string())),
                _ => None,
            },
            fp_core::ast::ExprKind::Name(Name::Ident(ident)) => self
                .lookup_var(ident.as_str())
                .and_then(|var| match var {
                    VarValue::String(value) => Some(value.clone()),
                    VarValue::Int(value) => Some(StringExpr::Literal(value.to_string())),
                    VarValue::Bool(value) => Some(StringExpr::Literal(value.to_string())),
                    VarValue::StringList(_) => {
                        Some(StringExpr::Variable(ident.as_str().to_string()))
                    }
                })
                .or_else(|| Some(StringExpr::Variable(ident.as_str().to_string()))),
            fp_core::ast::ExprKind::FormatString(template) => {
                let mut parts = Vec::new();
                for part in &template.parts {
                    match part {
                        fp_core::ast::FormatTemplatePart::Literal(text) => {
                            parts.push(StringPart::Literal(text.clone()))
                        }
                        fp_core::ast::FormatTemplatePart::Placeholder(placeholder) => {
                            match &placeholder.arg_ref {
                                fp_core::ast::FormatArgRef::Named(name) => {
                                    parts.push(StringPart::Variable(name.clone()));
                                }
                                fp_core::ast::FormatArgRef::Implicit => return None,
                                fp_core::ast::FormatArgRef::Positional(_) => return None,
                            }
                        }
                    }
                }
                Some(StringExpr::Interpolated(parts))
            }
            fp_core::ast::ExprKind::IntrinsicCall(call) => self.lower_intrinsic_string(call),
            fp_core::ast::ExprKind::Paren(paren) => self.lower_string_expr(&paren.expr),
            _ => None,
        }
    }

    fn lower_intrinsic_string(&self, call: &ExprIntrinsicCall) -> Option<StringExpr> {
        if call.kind == IntrinsicCallKind::Format {
            let fp_core::ast::ExprKind::FormatString(template) = call.args.first()?.kind() else {
                return None;
            };
            let mut parts = Vec::new();
            let mut implicit_index = 0usize;
            for part in &template.parts {
                match part {
                    fp_core::ast::FormatTemplatePart::Literal(text) => {
                        parts.push(StringPart::Literal(text.clone()))
                    }
                    fp_core::ast::FormatTemplatePart::Placeholder(placeholder) => {
                        let string = match &placeholder.arg_ref {
                            fp_core::ast::FormatArgRef::Named(name) => {
                                StringExpr::Variable(name.clone())
                            }
                            fp_core::ast::FormatArgRef::Implicit => {
                                let arg = call.args.get(implicit_index + 1)?;
                                implicit_index += 1;
                                self.lower_string_expr(arg)?
                            }
                            fp_core::ast::FormatArgRef::Positional(index) => {
                                self.lower_string_expr(call.args.get(index + 1)?)?
                            }
                        };
                        parts.push(part_from_string_expr(string));
                    }
                }
            }
            return Some(StringExpr::Interpolated(parts));
        }
        None
    }

    fn lower_bool_expr(&self, expr: &Expr) -> Option<BoolExpr> {
        if let fp_core::ast::ExprKind::BinOp(bin_op) = expr.kind() {
            let comparison = match bin_op.kind {
                BinOpKind::Gt => Some(ComparisonOp::Gt),
                BinOpKind::Lt => Some(ComparisonOp::Lt),
                BinOpKind::Ge => Some(ComparisonOp::Ge),
                BinOpKind::Le => Some(ComparisonOp::Le),
                BinOpKind::Eq => Some(ComparisonOp::Eq),
                BinOpKind::Ne => Some(ComparisonOp::Ne),
                _ => None,
            };
            if let Some(op) = comparison {
                if let (Some(lhs), Some(rhs)) = (
                    self.lower_int_expr(&bin_op.lhs),
                    self.lower_int_expr(&bin_op.rhs),
                ) {
                    return Some(BoolExpr::IntComparison { lhs, op, rhs });
                }
            }
            let string_comparison = match bin_op.kind {
                BinOpKind::Eq => Some(StringComparisonOp::Eq),
                BinOpKind::Ne => Some(StringComparisonOp::Ne),
                _ => None,
            };
            if let Some(op) = string_comparison {
                let lhs = self.lower_string_expr(&bin_op.lhs)?;
                let rhs = self.lower_string_expr(&bin_op.rhs)?;
                return Some(BoolExpr::StringComparison { lhs, op, rhs });
            }
        }
        match expr.kind() {
            fp_core::ast::ExprKind::Value(value) => match value.as_ref() {
                Value::Bool(flag) => Some(BoolExpr::Literal(flag.value)),
                _ => None,
            },
            fp_core::ast::ExprKind::Name(Name::Ident(ident)) => {
                Some(BoolExpr::Variable(ident.as_str().to_string()))
            }
            fp_core::ast::ExprKind::Paren(paren) => self.lower_bool_expr(&paren.expr),
            _ => None,
        }
    }

    fn resolve_bool_expr(&self, expr: &Expr) -> Option<bool> {
        match self.lower_bool_expr(expr)? {
            BoolExpr::Literal(value) => Some(value),
            _ => None,
        }
    }

    fn lower_int_expr(&self, expr: &Expr) -> Option<IntExpr> {
        match expr.kind() {
            fp_core::ast::ExprKind::Value(value) => match value.as_ref() {
                Value::Int(int_value) => Some(IntExpr::Literal(int_value.value)),
                _ => None,
            },
            fp_core::ast::ExprKind::Name(Name::Ident(ident)) => {
                Some(IntExpr::Variable(ident.as_str().to_string()))
            }
            fp_core::ast::ExprKind::Paren(paren) => self.lower_int_expr(&paren.expr),
            fp_core::ast::ExprKind::UnOp(un_op) => {
                let value = self.lower_int_expr(&un_op.val)?;
                match un_op.op {
                    UnOpKind::Neg => Some(IntExpr::UnaryNeg(Box::new(value))),
                    _ => None,
                }
            }
            fp_core::ast::ExprKind::BinOp(bin_op) => {
                let lhs = self.lower_int_expr(&bin_op.lhs)?;
                let rhs = self.lower_int_expr(&bin_op.rhs)?;
                let op = match bin_op.kind {
                    BinOpKind::Add | BinOpKind::AddTrait => ArithmeticOp::Add,
                    BinOpKind::Sub => ArithmeticOp::Sub,
                    BinOpKind::Mul => ArithmeticOp::Mul,
                    BinOpKind::Div => ArithmeticOp::Div,
                    BinOpKind::Mod => ArithmeticOp::Mod,
                    _ => return None,
                };
                Some(IntExpr::Binary {
                    lhs: Box::new(lhs),
                    op,
                    rhs: Box::new(rhs),
                })
            }
            _ => None,
        }
    }

    fn resolve_string_list_expr(&self, expr: &Expr) -> Option<Vec<StringExpr>> {
        match expr.kind() {
            fp_core::ast::ExprKind::Name(Name::Ident(ident)) => {
                if let Some(var) = self.lookup_var(ident.as_str()) {
                    return match var {
                        VarValue::StringList(values) => Some(values.clone()),
                        VarValue::String(value) => Some(vec![value.clone()]),
                        VarValue::Int(value) => Some(vec![StringExpr::Literal(value.to_string())]),
                        VarValue::Bool(value) => Some(vec![StringExpr::Literal(value.to_string())]),
                    };
                }
                Some(vec![StringExpr::Variable(ident.as_str().to_string())])
            }
            fp_core::ast::ExprKind::Value(value) => match value.as_ref() {
                Value::String(text) => Some(vec![StringExpr::Literal(text.value.clone())]),
                Value::Int(int_value) => {
                    Some(vec![StringExpr::Literal(int_value.value.to_string())])
                }
                Value::List(list) => {
                    let mut values = Vec::new();
                    for value in &list.values {
                        match value {
                            Value::String(text) => {
                                values.push(StringExpr::Literal(text.value.clone()))
                            }
                            Value::Int(int_value) => {
                                values.push(StringExpr::Literal(int_value.value.to_string()))
                            }
                            _ => return None,
                        }
                    }
                    Some(values)
                }
                _ => None,
            },
            fp_core::ast::ExprKind::Array(array) => {
                let mut values = Vec::new();
                for item in &array.values {
                    values.push(self.lower_string_expr(item)?);
                }
                Some(values)
            }
            fp_core::ast::ExprKind::Tuple(tuple) => {
                let mut values = Vec::new();
                for item in &tuple.values {
                    values.push(self.lower_string_expr(item)?);
                }
                Some(values)
            }
            fp_core::ast::ExprKind::Paren(paren) => self.resolve_string_list_expr(&paren.expr),
            _ => None,
        }
    }

    fn resolve_vars_map(&self, expr: &Expr) -> Option<HashMap<String, StringExpr>> {
        match expr.kind() {
            fp_core::ast::ExprKind::Value(value) => match value.as_ref() {
                Value::Map(map) => {
                    let mut vars = HashMap::new();
                    for entry in &map.entries {
                        let key = match &entry.key {
                            Value::String(text) => text.value.clone(),
                            _ => return None,
                        };
                        let value = match &entry.value {
                            Value::String(text) => StringExpr::Literal(text.value.clone()),
                            Value::Int(int_value) => {
                                StringExpr::Literal(int_value.value.to_string())
                            }
                            Value::Bool(bool_value) => {
                                StringExpr::Literal(bool_value.value.to_string())
                            }
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

    fn parse_host_selector(&self, expr: &Expr) -> Option<Vec<HostExpr>> {
        self.resolve_string_list_expr(expr).map(|hosts| {
            let mut selectors = Vec::new();
            for host in hosts {
                match host {
                    StringExpr::Literal(name) if name == "localhost" => {
                        selectors.push(HostExpr::Localhost)
                    }
                    StringExpr::Literal(name) => {
                        if let Some(group_hosts) = self.inventory.groups.get(&name) {
                            selectors.extend(
                                group_hosts
                                    .iter()
                                    .cloned()
                                    .map(|host| HostExpr::Selector(StringExpr::Literal(host))),
                            );
                        } else {
                            selectors.push(HostExpr::Selector(StringExpr::Literal(name)));
                        }
                    }
                    other => selectors.push(HostExpr::Selector(other)),
                }
            }
            selectors
        })
    }

    fn resolve_hosts(&self, invoke: &ExprInvoke, context: &EmitContext) -> Vec<HostExpr> {
        kwarg_value(invoke, &["hosts", "host"])
            .and_then(|expr| self.parse_host_selector(expr))
            .or_else(|| context.hosts.clone())
            .unwrap_or_else(|| vec![HostExpr::Localhost])
    }

    fn bind_var(&mut self, name: String, value: VarValue) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, value);
        }
    }

    fn bind_value_expr(&mut self, name: String, value: &ValueExpr) {
        let entry = match value {
            ValueExpr::String(v) => VarValue::String(v.clone()),
            ValueExpr::Int(IntExpr::Literal(v)) => VarValue::Int(*v),
            ValueExpr::Bool(BoolExpr::Literal(v)) => VarValue::Bool(*v),
            ValueExpr::StringList(values) => VarValue::StringList(values.clone()),
            _ => return,
        };
        self.bind_var(name, entry);
    }

    fn lookup_var(&self, name: &str) -> Option<&VarValue> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value);
            }
        }
        None
    }

    fn is_known_callable(&self, name: &str) -> bool {
        self.known_functions.contains(name) || self.program.externs.contains_key(name)
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            let _ = self.scopes.pop();
        }
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

fn apply_command_options(command: StringExpr, cwd: Option<StringExpr>, sudo: bool) -> StringExpr {
    let mut parts = Vec::new();
    if sudo {
        parts.push(StringPart::Literal("sudo ".to_string()));
    }
    if let Some(cwd) = cwd {
        parts.push(StringPart::Literal("cd ".to_string()));
        parts.push(part_from_string_expr(cwd));
        parts.push(StringPart::Literal(" && ".to_string()));
    }
    match command {
        StringExpr::Literal(text) => {
            parts.push(StringPart::Literal(text));
            StringExpr::Interpolated(parts)
        }
        StringExpr::Variable(name) => {
            parts.push(StringPart::Variable(name));
            StringExpr::Interpolated(parts)
        }
        StringExpr::Call { name, args } => {
            parts.push(StringPart::Call { name, args });
            StringExpr::Interpolated(parts)
        }
        StringExpr::Interpolated(mut more) => {
            parts.append(&mut more);
            StringExpr::Interpolated(parts)
        }
    }
}

fn part_from_string_expr(expr: StringExpr) -> StringPart {
    match expr {
        StringExpr::Literal(text) => StringPart::Literal(text),
        StringExpr::Variable(name) => StringPart::Variable(name),
        StringExpr::Call { name, args } => StringPart::Call { name, args },
        StringExpr::Interpolated(parts) => {
            let mut flattened = String::new();
            for part in parts {
                match part {
                    StringPart::Literal(text) => flattened.push_str(&text),
                    StringPart::Variable(name) => {
                        if !flattened.is_empty() {
                            return StringPart::Literal(flattened);
                        }
                        return StringPart::Variable(name);
                    }
                    StringPart::Call { name, args } => {
                        if !flattened.is_empty() {
                            return StringPart::Literal(flattened);
                        }
                        return StringPart::Call { name, args };
                    }
                }
            }
            StringPart::Literal(flattened)
        }
    }
}
