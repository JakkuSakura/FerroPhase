use fp_core::ast::{
    Abi, BlockStmt, BlockStmtExpr, Expr, ExprArray, ExprBinOp, ExprBlock, ExprFor, ExprIf,
    ExprIntrinsicCall, ExprInvoke, ExprInvokeTarget, ExprKind, ExprLet, ExprMatch, ExprMatchCase,
    ExprStringTemplate, ExprWhile, File, FormatArgRef, FormatPlaceholder, FormatTemplatePart,
    Ident, Item, ItemDeclFunction, ItemDefFunction, ItemKind, Name, Node, NodeKind, Pattern,
    PatternKind, Value,
};
use fp_core::intrinsics::IntrinsicCallKind;
use fp_core::ops::BinOpKind;
use fp_shell_core::ShellInventory;
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

#[derive(Debug, Clone, Default)]
struct EmitContext {
    hosts: Option<Vec<Expr>>,
}

pub fn lower_node(node: &Node, inventory: &ShellInventory) -> Result<Node, String> {
    let mut lowerer = Lowerer::new(inventory);
    lowerer.lower_node(node, &EmitContext::default())?;
    Ok(Node::file(File {
        path: lowerer.path,
        items: lowerer.items,
    }))
}

struct Lowerer<'a> {
    path: PathBuf,
    items: Vec<Item>,
    known_functions: HashSet<String>,
    inventory: &'a ShellInventory,
}

impl<'a> Lowerer<'a> {
    fn new(inventory: &'a ShellInventory) -> Self {
        Self {
            path: PathBuf::new(),
            items: Vec::new(),
            known_functions: HashSet::new(),
            inventory,
        }
    }

    fn lower_node(&mut self, node: &Node, context: &EmitContext) -> Result<(), String> {
        match node.kind() {
            NodeKind::File(file) => {
                self.path = file.path.clone();
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
                self.items
                    .extend(out.into_iter().map(|expr| Item::from(ItemKind::Expr(expr))));
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
                if !matches!(function.sig.abi, Abi::Rust) {
                    self.known_functions
                        .insert(function.name.as_str().to_string());
                }
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
        mut target: Option<&mut Vec<Expr>>,
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
                        self.items.extend(
                            temp.into_iter()
                                .map(|expr| Item::from(ItemKind::Expr(expr))),
                        );
                    }
                } else {
                    let def = self.lower_function(function)?;
                    self.insert_function(def);
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
            ItemKind::DeclFunction(function) => {
                if matches!(function.sig.abi, Abi::Rust) {
                    return Ok(());
                }
                self.insert_decl(function.clone());
            }
            ItemKind::Expr(expr) => {
                if let Some(target) = target {
                    self.lower_expr_into(expr, context, target)?;
                } else {
                    let mut temp = Vec::new();
                    self.lower_expr_into(expr, context, &mut temp)?;
                    self.items.extend(
                        temp.into_iter()
                            .map(|expr| Item::from(ItemKind::Expr(expr))),
                    );
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn lower_function(&mut self, function: &ItemDefFunction) -> Result<ItemDefFunction, String> {
        let mut body = Vec::new();
        self.lower_expr_into(&function.body, &EmitContext::default(), &mut body)?;
        let mut lowered = function.clone();
        lowered.body = statements_to_block_expr(body).into_expr().into();
        Ok(lowered)
    }

    fn lower_expr_into(
        &mut self,
        expr: &Expr,
        context: &EmitContext,
        out: &mut Vec<Expr>,
    ) -> Result<(), String> {
        match expr.kind() {
            fp_core::ast::ExprKind::Block(block) => self.lower_block(block, context, out),
            fp_core::ast::ExprKind::Invoke(invoke) => {
                if let Some(exprs) = self.lower_shell_invoke(invoke, context)? {
                    out.extend(exprs);
                    Ok(())
                } else if let Some(invocation) = self.lower_function_invoke(invoke)? {
                    out.push(invocation);
                    Ok(())
                } else {
                    Ok(())
                }
            }
            fp_core::ast::ExprKind::If(expr_if) => {
                out.push(self.lower_if_expr(expr_if, context)?);
                Ok(())
            }
            fp_core::ast::ExprKind::Match(expr_match) => {
                out.extend(self.lower_match(expr_match, context)?);
                Ok(())
            }
            fp_core::ast::ExprKind::While(expr_while) => {
                let mut body = Vec::new();
                self.lower_expr_into(&expr_while.body, context, &mut body)?;
                out.push(Expr::new(ExprKind::While(ExprWhile {
                    span: expr_while.span,
                    cond: self.lower_condition(&expr_while.cond).into(),
                    body: statements_to_block_expr(body).into_expr().into(),
                })));
                Ok(())
            }
            fp_core::ast::ExprKind::For(expr_for) => {
                out.push(self.lower_for_expr(expr_for, context)?);
                Ok(())
            }
            fp_core::ast::ExprKind::Let(expr_let) => {
                let value = self.lower_value_expr(&expr_let.expr)?;
                out.push(Expr::new(ExprKind::Let(ExprLet {
                    span: expr_let.span,
                    pat: expr_let.pat.clone(),
                    expr: value.into(),
                })));
                Ok(())
            }
            _ => {
                if let Ok(value) = self.lower_value_expr(expr) {
                    out.push(value);
                }
                Ok(())
            }
        }
    }

    fn lower_block(
        &mut self,
        block: &ExprBlock,
        context: &EmitContext,
        out: &mut Vec<Expr>,
    ) -> Result<(), String> {
        for statement in &block.stmts {
            match statement {
                BlockStmt::Item(item) => self.lower_item(item, context, Some(out))?,
                BlockStmt::Expr(expr) => self.lower_expr_into(&expr.expr, context, out)?,
                BlockStmt::Let(stmt) => {
                    let Some(_name) = stmt.pat.as_ident() else {
                        continue;
                    };
                    let Some(init) = &stmt.init else {
                        continue;
                    };
                    let value = self.lower_value_expr(init)?;
                    out.push(Expr::new(ExprKind::Let(ExprLet {
                        span: init.span(),
                        pat: stmt.pat.clone().into(),
                        expr: value.into(),
                    })));
                }
                BlockStmt::Noop | BlockStmt::Any(_) => {}
            }
        }
        Ok(())
    }

    fn lower_shell_invoke(
        &mut self,
        invoke: &ExprInvoke,
        context: &EmitContext,
    ) -> Result<Option<Vec<Expr>>, String> {
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
            let command = apply_command_options(command, cwd.clone(), sudo);
            let hosts = self.resolve_hosts(invoke, context);
            return Ok(Some(
                hosts
                    .into_iter()
                    .map(|host| {
                        self.make_call(
                            "shell_run",
                            vec![
                                host,
                                command.clone(),
                                self.lower_guard_expr(invoke, "only_if"),
                                self.lower_guard_expr(invoke, "unless"),
                                self.lower_guard_expr(invoke, "creates"),
                                self.lower_guard_expr(invoke, "removes"),
                            ],
                        )
                    })
                    .collect(),
            ));
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
            return Ok(Some(
                hosts
                    .into_iter()
                    .map(|host| {
                        self.make_call(
                            "shell_copy",
                            vec![
                                host,
                                source.clone(),
                                destination.clone(),
                                self.lower_guard_expr(invoke, "only_if"),
                                self.lower_guard_expr(invoke, "unless"),
                                self.lower_guard_expr(invoke, "creates"),
                                self.lower_guard_expr(invoke, "removes"),
                            ],
                        )
                    })
                    .collect(),
            ));
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
                .map(|vars| self.serialize_vars_map(vars))
                .unwrap_or_else(empty_string_expr);
            let hosts = self.resolve_hosts(invoke, context);
            return Ok(Some(
                hosts
                    .into_iter()
                    .map(|host| {
                        self.make_call(
                            "shell_template",
                            vec![
                                host,
                                source.clone(),
                                destination.clone(),
                                vars.clone(),
                                self.lower_guard_expr(invoke, "only_if"),
                                self.lower_guard_expr(invoke, "unless"),
                                self.lower_guard_expr(invoke, "creates"),
                                self.lower_guard_expr(invoke, "removes"),
                            ],
                        )
                    })
                    .collect(),
            ));
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
            let flags = string_literal_expr(self.rsync_flags(invoke));
            let hosts = self.resolve_hosts(invoke, context);
            return Ok(Some(
                hosts
                    .into_iter()
                    .map(|host| {
                        self.make_call(
                            "shell_rsync",
                            vec![
                                host,
                                flags.clone(),
                                source.clone(),
                                destination.clone(),
                                self.lower_guard_expr(invoke, "only_if"),
                                self.lower_guard_expr(invoke, "unless"),
                                self.lower_guard_expr(invoke, "creates"),
                                self.lower_guard_expr(invoke, "removes"),
                            ],
                        )
                    })
                    .collect(),
            ));
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
            let sudo = kwarg_value(invoke, &["sudo"])
                .and_then(|expr| self.resolve_bool_expr(expr))
                .unwrap_or(true);
            let command = apply_command_options(
                concat_string_exprs(vec![
                    string_literal_expr("systemctl restart ".to_string()),
                    service_name.clone(),
                ]),
                None,
                sudo,
            );
            return Ok(Some(
                hosts
                    .into_iter()
                    .map(|host| {
                        self.make_call(
                            "shell_run",
                            vec![
                                host,
                                command.clone(),
                                self.lower_guard_expr(invoke, "only_if"),
                                self.lower_guard_expr(invoke, "unless"),
                                self.lower_guard_expr(invoke, "creates"),
                                self.lower_guard_expr(invoke, "removes"),
                            ],
                        )
                    })
                    .collect(),
            ));
        }

        Ok(None)
    }

    fn lower_host_scope(&mut self, invoke: &ExprInvoke) -> Result<Option<Vec<Expr>>, String> {
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

    fn lower_function_invoke(&mut self, invoke: &ExprInvoke) -> Result<Option<Expr>, String> {
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
        Ok(Some(Expr::new(ExprKind::Invoke(ExprInvoke {
            span: invoke.span,
            target: ExprInvokeTarget::Function(Name::ident(name)),
            args,
            kwargs: Vec::new(),
        }))))
    }

    fn lower_if_expr(&mut self, expr_if: &ExprIf, context: &EmitContext) -> Result<Expr, String> {
        let mut then_block = Vec::new();
        self.lower_expr_into(&expr_if.then, context, &mut then_block)?;
        let else_block = if let Some(else_expr) = &expr_if.elze {
            let mut else_statements = Vec::new();
            self.lower_expr_into(else_expr, context, &mut else_statements)?;
            Some(statements_to_block_expr(else_statements).into_expr().into())
        } else {
            None
        };
        Ok(Expr::new(ExprKind::If(ExprIf {
            span: expr_if.span,
            cond: self.lower_condition(&expr_if.cond).into(),
            then: statements_to_block_expr(then_block).into_expr().into(),
            elze: else_block,
        })))
    }

    fn lower_match(
        &mut self,
        expr_match: &ExprMatch,
        context: &EmitContext,
    ) -> Result<Vec<Expr>, String> {
        let Some(scrutinee) = expr_match.scrutinee.as_deref() else {
            return Err("match expression requires a scrutinee".to_string());
        };
        let scrutinee = self
            .lower_string_expr(scrutinee)
            .ok_or_else(|| "match scrutinee must resolve to string".to_string())?;
        if expr_match.cases.iter().all(|case| case.guard.is_none()) {
            return self.lower_match_native(&scrutinee, &expr_match.cases, context);
        }
        self.lower_match_cases(&scrutinee, &expr_match.cases, context)
    }

    fn lower_match_native(
        &mut self,
        scrutinee: &Expr,
        cases: &[ExprMatchCase],
        context: &EmitContext,
    ) -> Result<Vec<Expr>, String> {
        let lowered_cases = cases
            .iter()
            .map(|case| -> Result<ExprMatchCase, String> {
                let mut body = Vec::new();
                self.lower_expr_into(&case.body, context, &mut body)?;
                Ok(ExprMatchCase {
                    span: case.span,
                    pat: lower_string_pattern(self.lower_match_case_value(case)?).map(Box::new),
                    cond: Expr::unit().into(),
                    guard: None,
                    body: statements_to_block_expr(body).into_expr().into(),
                })
            })
            .collect::<Result<Vec<_>, _>>()?;
        Ok(vec![Expr::new(ExprKind::Match(ExprMatch {
            span: expr_match_span(cases),
            scrutinee: Some(scrutinee.clone().into()),
            cases: lowered_cases,
        }))])
    }

    fn lower_match_cases(
        &mut self,
        scrutinee: &Expr,
        cases: &[ExprMatchCase],
        context: &EmitContext,
    ) -> Result<Vec<Expr>, String> {
        let Some((first, rest)) = cases.split_first() else {
            return Ok(Vec::new());
        };
        let fallback = self.lower_match_cases(scrutinee, rest, context)?;
        self.lower_match_case(scrutinee, first, context, fallback)
    }

    fn lower_match_case(
        &mut self,
        scrutinee: &Expr,
        case: &ExprMatchCase,
        context: &EmitContext,
        fallback: Vec<Expr>,
    ) -> Result<Vec<Expr>, String> {
        let mut body_statements = Vec::new();
        self.lower_expr_into(&case.body, context, &mut body_statements)?;
        let body_block = statements_to_block_expr(body_statements);

        let fallback_opt = if fallback.is_empty() {
            None
        } else {
            Some(
                statements_to_block_expr(fallback.clone())
                    .into_expr()
                    .into(),
            )
        };

        let pattern_condition = self.lower_match_case_pattern(scrutinee, case)?;
        let guarded_body = if let Some(guard) = &case.guard {
            statements_to_block_expr(vec![Expr::new(ExprKind::If(ExprIf {
                span: guard.span(),
                cond: self.lower_condition(guard).into(),
                then: body_block.into_expr().into(),
                elze: fallback_opt.clone(),
            }))])
        } else {
            body_block
        };

        if let Some(condition) = pattern_condition {
            Ok(vec![Expr::new(ExprKind::If(ExprIf {
                span: case.span,
                cond: condition.into(),
                then: guarded_body.into_expr().into(),
                elze: fallback_opt,
            }))])
        } else {
            Ok(block_to_statements(guarded_body))
        }
    }

    fn lower_match_case_pattern(
        &self,
        scrutinee: &Expr,
        case: &ExprMatchCase,
    ) -> Result<Option<Expr>, String> {
        let Some(rhs) = self.lower_match_case_value(case)? else {
            return Ok(None);
        };
        Ok(Some(Expr::new(ExprKind::BinOp(ExprBinOp {
            span: Default::default(),
            kind: BinOpKind::Eq,
            lhs: scrutinee.clone().into(),
            rhs: rhs.into(),
        }))))
    }

    fn lower_match_case_value(&self, case: &ExprMatchCase) -> Result<Option<Expr>, String> {
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
                Ok(Some(rhs))
            }
            _ => Err(
                "match patterns in fp-shell currently support string literals and `_` only"
                    .to_string(),
            ),
        }
    }

    fn lower_for_expr(
        &mut self,
        expr_for: &ExprFor,
        context: &EmitContext,
    ) -> Result<Expr, String> {
        let PatternKind::Ident(_pattern) = expr_for.pat.kind() else {
            return Err("for-loop pattern must be an identifier".to_string());
        };
        let values = self
            .resolve_string_list_expr(&expr_for.iter)
            .ok_or_else(|| "for-loop iterable must resolve to a string list".to_string())?;
        let mut body = Vec::new();
        self.lower_expr_into(&expr_for.body, context, &mut body)?;
        Ok(Expr::new(ExprKind::For(ExprFor {
            span: expr_for.span,
            pat: expr_for.pat.clone(),
            iter: Expr::new(ExprKind::Array(ExprArray {
                span: Default::default(),
                values,
            }))
            .into(),
            body: statements_to_block_expr(body).into_expr().into(),
        })))
    }

    fn lower_condition(&self, expr: &Expr) -> Expr {
        if let Some(flag) = self.lower_bool_expr(expr) {
            return flag;
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
                            return condition_command_expr(command);
                        }
                    }
                }
            }
        }
        if let Some(text) = self.lower_string_expr(expr) {
            return text;
        }
        Expr::value(Value::bool(true))
    }

    fn lower_value_expr(&self, expr: &Expr) -> Result<Expr, String> {
        match expr.kind() {
            ExprKind::Value(value) => match value.as_ref() {
                Value::String(_) | Value::Int(_) | Value::Bool(_) => Ok(expr.clone()),
                Value::List(list) => {
                    let values = list
                        .values
                        .iter()
                        .map(value_to_string_expr)
                        .collect::<Option<Vec<_>>>()
                        .ok_or_else(|| "expression could not be lowered".to_string())?;
                    Ok(Expr::new(ExprKind::Array(ExprArray {
                        span: Default::default(),
                        values,
                    })))
                }
                _ => Err("expression could not be lowered".to_string()),
            },
            ExprKind::Name(_) => Ok(expr.clone()),
            ExprKind::Array(array) => Ok(Expr::new(ExprKind::Array(ExprArray {
                span: array.span,
                values: array
                    .values
                    .iter()
                    .map(|item| self.lower_string_expr(item))
                    .collect::<Option<Vec<_>>>()
                    .ok_or_else(|| "expression could not be lowered".to_string())?,
            }))),
            ExprKind::Tuple(tuple) => Ok(Expr::new(ExprKind::Array(ExprArray {
                span: tuple.span,
                values: tuple
                    .values
                    .iter()
                    .map(|item| self.lower_string_expr(item))
                    .collect::<Option<Vec<_>>>()
                    .ok_or_else(|| "expression could not be lowered".to_string())?,
            }))),
            ExprKind::BinOp(_) => self
                .lower_bool_expr(expr)
                .or_else(|| self.lower_int_expr(expr))
                .ok_or_else(|| "expression could not be lowered".to_string()),
            ExprKind::FormatString(_)
            | ExprKind::IntrinsicCall(_)
            | ExprKind::Invoke(_)
            | ExprKind::Paren(_) => self
                .lower_string_expr(expr)
                .ok_or_else(|| "expression could not be lowered".to_string()),
            _ => Err("expression could not be lowered".to_string()),
        }
    }

    fn lower_string_expr(&self, expr: &Expr) -> Option<Expr> {
        match expr.kind() {
            ExprKind::Value(value) => value_to_string_expr(value),
            ExprKind::Name(_) => Some(expr.clone()),
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
                Some(Expr::new(ExprKind::Invoke(ExprInvoke {
                    span: invoke.span,
                    target: ExprInvokeTarget::Function(Name::ident(name)),
                    args,
                    kwargs: Vec::new(),
                })))
            }
            fp_core::ast::ExprKind::FormatString(template) => {
                if template.parts.iter().all(|part| {
                    matches!(
                        part,
                        FormatTemplatePart::Literal(_)
                            | FormatTemplatePart::Placeholder(FormatPlaceholder {
                                arg_ref: FormatArgRef::Named(_),
                                ..
                            })
                    )
                }) {
                    Some(expr.clone())
                } else {
                    None
                }
            }
            fp_core::ast::ExprKind::IntrinsicCall(call) => self.lower_intrinsic_string(call),
            fp_core::ast::ExprKind::Paren(paren) => self.lower_string_expr(&paren.expr),
            _ => None,
        }
    }

    fn lower_intrinsic_string(&self, call: &ExprIntrinsicCall) -> Option<Expr> {
        if call.kind != IntrinsicCallKind::Format {
            return None;
        }
        let fp_core::ast::ExprKind::FormatString(template) = call.args.first()?.kind() else {
            return None;
        };
        let mut args = Vec::with_capacity(call.args.len());
        args.push(Expr::new(ExprKind::FormatString(template.clone())));
        let mut implicit_index = 0usize;
        for part in &template.parts {
            let FormatTemplatePart::Placeholder(placeholder) = part else {
                continue;
            };
            let arg = match &placeholder.arg_ref {
                FormatArgRef::Named(name) => Expr::ident(Ident::new(name.clone())),
                FormatArgRef::Implicit => {
                    let arg = call.args.get(implicit_index + 1)?;
                    implicit_index += 1;
                    self.lower_string_expr(arg)?
                }
                FormatArgRef::Positional(index) => {
                    self.lower_string_expr(call.args.get(index + 1)?)?
                }
            };
            args.push(arg);
        }
        Some(Expr::new(ExprKind::IntrinsicCall(ExprIntrinsicCall {
            span: call.span,
            kind: IntrinsicCallKind::Format,
            args,
            kwargs: Vec::new(),
        })))
    }

    fn lower_bool_expr(&self, expr: &Expr) -> Option<Expr> {
        match expr.kind() {
            fp_core::ast::ExprKind::BinOp(bin_op) => {
                let is_comparison = matches!(
                    bin_op.kind,
                    BinOpKind::Gt
                        | BinOpKind::Lt
                        | BinOpKind::Ge
                        | BinOpKind::Le
                        | BinOpKind::Eq
                        | BinOpKind::Ne
                );
                if !is_comparison {
                    return None;
                }
                let lhs = if matches!(
                    bin_op.kind,
                    BinOpKind::Gt | BinOpKind::Lt | BinOpKind::Ge | BinOpKind::Le
                ) {
                    self.lower_int_expr(&bin_op.lhs)?
                } else {
                    self.lower_value_expr(&bin_op.lhs).ok()?
                };
                let rhs = if matches!(
                    bin_op.kind,
                    BinOpKind::Gt | BinOpKind::Lt | BinOpKind::Ge | BinOpKind::Le
                ) {
                    self.lower_int_expr(&bin_op.rhs)?
                } else {
                    self.lower_value_expr(&bin_op.rhs).ok()?
                };
                Some(Expr::new(ExprKind::BinOp(ExprBinOp {
                    span: Default::default(),
                    kind: bin_op.kind,
                    lhs: lhs.into(),
                    rhs: rhs.into(),
                })))
            }
            fp_core::ast::ExprKind::Value(value) => match value.as_ref() {
                Value::Bool(flag) => Some(Expr::value(Value::bool(flag.value))),
                _ => None,
            },
            fp_core::ast::ExprKind::Name(_) => Some(expr.clone()),
            fp_core::ast::ExprKind::Paren(paren) => self.lower_bool_expr(&paren.expr),
            _ => None,
        }
    }

    fn resolve_bool_expr(&self, expr: &Expr) -> Option<bool> {
        match self.lower_bool_expr(expr)?.kind() {
            ExprKind::Value(value) => match &**value {
                Value::Bool(flag) => Some(flag.value),
                _ => None,
            },
            _ => None,
        }
    }

    fn lower_int_expr(&self, expr: &Expr) -> Option<Expr> {
        match expr.kind() {
            fp_core::ast::ExprKind::Value(value) => match value.as_ref() {
                Value::Int(int_value) => Some(Expr::value(Value::int(int_value.value))),
                _ => None,
            },
            fp_core::ast::ExprKind::Name(_) => Some(expr.clone()),
            fp_core::ast::ExprKind::Paren(paren) => self.lower_int_expr(&paren.expr),
            fp_core::ast::ExprKind::BinOp(bin_op) => match bin_op.kind {
                BinOpKind::Add
                | BinOpKind::AddTrait
                | BinOpKind::Sub
                | BinOpKind::Mul
                | BinOpKind::Div
                | BinOpKind::Mod => Some(Expr::new(ExprKind::BinOp(ExprBinOp {
                    span: Default::default(),
                    kind: bin_op.kind,
                    lhs: self.lower_int_expr(&bin_op.lhs)?.into(),
                    rhs: self.lower_int_expr(&bin_op.rhs)?.into(),
                }))),
                _ => return None,
            },
            _ => None,
        }
    }

    fn resolve_string_list_expr(&self, expr: &Expr) -> Option<Vec<Expr>> {
        match expr.kind() {
            fp_core::ast::ExprKind::Name(_) => Some(vec![expr.clone()]),
            fp_core::ast::ExprKind::Value(value) => match value.as_ref() {
                Value::String(_) | Value::Int(_) | Value::Bool(_) => {
                    Some(vec![value_to_string_expr(value)?])
                }
                Value::List(list) => list.values.iter().map(value_to_string_expr).collect(),
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

    fn resolve_vars_map(&self, expr: &Expr) -> Option<HashMap<String, Expr>> {
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
                            Value::String(text) => string_literal_expr(text.value.clone()),
                            Value::Int(int_value) => {
                                string_literal_expr(int_value.value.to_string())
                            }
                            Value::Bool(bool_value) => {
                                string_literal_expr(bool_value.value.to_string())
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

    fn parse_host_selector(&self, expr: &Expr) -> Option<Vec<Expr>> {
        self.resolve_string_list_expr(expr).map(|hosts| {
            let mut selectors = Vec::new();
            for host in hosts {
                match string_literal_value(&host) {
                    Some(name) if name == "localhost" => selectors.push(host),
                    Some(name) => {
                        if let Some(group_hosts) = self.inventory.groups.get(&name) {
                            selectors.extend(group_hosts.iter().cloned().map(string_literal_expr));
                        } else {
                            selectors.push(string_literal_expr(name));
                        }
                    }
                    None => selectors.push(host),
                }
            }
            selectors
        })
    }

    fn resolve_hosts(&self, invoke: &ExprInvoke, context: &EmitContext) -> Vec<Expr> {
        kwarg_value(invoke, &["hosts", "host"])
            .and_then(|expr| self.parse_host_selector(expr))
            .or_else(|| context.hosts.clone())
            .unwrap_or_else(|| vec![string_literal_expr("localhost".to_string())])
    }

    fn insert_function(&mut self, def: ItemDefFunction) {
        let Some(index) = self.items.iter().position(|item| {
            matches!(item.kind(), ItemKind::DefFunction(existing) if existing.name == def.name)
        }) else {
            self.items.push(Item::from(ItemKind::DefFunction(def)));
            return;
        };
        self.items[index] = Item::from(ItemKind::DefFunction(def));
    }

    fn insert_decl(&mut self, decl: ItemDeclFunction) {
        let Some(index) = self.items.iter().position(|item| {
            matches!(item.kind(), ItemKind::DeclFunction(existing) if existing.name == decl.name)
        }) else {
            self.items.push(Item::from(ItemKind::DeclFunction(decl)));
            return;
        };
        self.items[index] = Item::from(ItemKind::DeclFunction(decl));
    }

    fn lower_guard_expr(&self, invoke: &ExprInvoke, name: &str) -> Expr {
        kwarg_value(invoke, &[name])
            .and_then(|expr| self.lower_string_expr(expr))
            .unwrap_or_else(empty_string_expr)
    }

    fn make_call(&self, name: &str, args: Vec<Expr>) -> Expr {
        Expr::new(ExprKind::Invoke(ExprInvoke {
            span: Default::default(),
            target: ExprInvokeTarget::Function(Name::ident(name)),
            args,
            kwargs: Vec::new(),
        }))
    }

    fn serialize_vars_map(&self, vars: HashMap<String, Expr>) -> Expr {
        let mut entries = vars.into_iter().collect::<Vec<_>>();
        entries.sort_by(|lhs, rhs| lhs.0.cmp(&rhs.0));
        let mut parts = Vec::new();
        let mut args = Vec::new();
        for (index, (key, value)) in entries.into_iter().enumerate() {
            if index > 0 {
                parts.push(FormatTemplatePart::Literal(";".to_string()));
            }
            parts.push(FormatTemplatePart::Literal(format!("{}=", key)));
            parts.push(FormatTemplatePart::Placeholder(FormatPlaceholder {
                arg_ref: FormatArgRef::Implicit,
                format_spec: None,
            }));
            args.push(value);
        }
        if parts.is_empty() {
            empty_string_expr()
        } else {
            format_call_expr(parts, args)
        }
    }

    fn rsync_flags(&self, invoke: &ExprInvoke) -> String {
        let archive = kwarg_value(invoke, &["archive"])
            .and_then(|expr| self.resolve_bool_expr(expr))
            .unwrap_or(true);
        let compress = kwarg_value(invoke, &["compress"])
            .and_then(|expr| self.resolve_bool_expr(expr))
            .unwrap_or(true);
        let delete = kwarg_value(invoke, &["delete"])
            .and_then(|expr| self.resolve_bool_expr(expr))
            .unwrap_or(false);
        let checksum = kwarg_value(invoke, &["checksum"])
            .and_then(|expr| self.resolve_bool_expr(expr))
            .unwrap_or(false);
        let mut short = String::new();
        if archive {
            short.push('a');
        }
        if compress {
            short.push('z');
        }
        let mut flags = if short.is_empty() {
            String::new()
        } else {
            format!("-{}", short)
        };
        if delete {
            if !flags.is_empty() {
                flags.push(' ');
            }
            flags.push_str("--delete");
        }
        if checksum {
            if !flags.is_empty() {
                flags.push(' ');
            }
            flags.push_str("--checksum");
        }
        flags
    }

    fn is_known_callable(&self, name: &str) -> bool {
        self.known_functions.contains(name)
    }
}

fn statements_to_block_expr(statements: Vec<Expr>) -> ExprBlock {
    ExprBlock::new_stmts(
        statements
            .into_iter()
            .map(|expr| BlockStmt::Expr(BlockStmtExpr::new(expr).with_semicolon(true)))
            .collect(),
    )
}

fn block_to_statements(block: ExprBlock) -> Vec<Expr> {
    block
        .stmts
        .into_iter()
        .filter_map(|stmt| match stmt {
            BlockStmt::Expr(expr) => Some(*expr.expr),
            BlockStmt::Any(_) | BlockStmt::Let(_) | BlockStmt::Item(_) | BlockStmt::Noop => None,
        })
        .collect()
}

fn lower_string_pattern(value: Option<Expr>) -> Option<Pattern> {
    value.map(|value| match value {
        Expr {
            kind: ExprKind::Value(value),
            ..
        } => match *value {
            Value::String(text) => {
                Pattern::from(PatternKind::Variant(fp_core::ast::PatternVariant {
                    name: Expr::value(Value::string(text.value)),
                    pattern: None,
                }))
            }
            other => Pattern::from(PatternKind::Variant(fp_core::ast::PatternVariant {
                name: Expr::value(other),
                pattern: None,
            })),
        },
        other => Pattern::from(PatternKind::Variant(fp_core::ast::PatternVariant {
            name: other,
            pattern: None,
        })),
    })
}

fn expr_match_span(cases: &[ExprMatchCase]) -> fp_core::span::Span {
    fp_core::span::Span::union(cases.iter().map(|case| case.span))
}

fn string_literal_expr(value: impl Into<String>) -> Expr {
    Expr::value(Value::string(value.into()))
}

fn value_to_string_expr(value: &Value) -> Option<Expr> {
    match value {
        Value::String(text) => Some(string_literal_expr(text.value.clone())),
        Value::Int(int_value) => Some(string_literal_expr(int_value.value.to_string())),
        Value::Bool(bool_value) => Some(string_literal_expr(bool_value.value.to_string())),
        _ => None,
    }
}

fn empty_string_expr() -> Expr {
    string_literal_expr(String::new())
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

fn format_call_expr(parts: Vec<FormatTemplatePart>, mut args: Vec<Expr>) -> Expr {
    let mut call_args = Vec::with_capacity(args.len() + 1);
    call_args.push(Expr::new(ExprKind::FormatString(ExprStringTemplate {
        parts,
    })));
    call_args.append(&mut args);
    Expr::new(ExprKind::IntrinsicCall(ExprIntrinsicCall {
        span: Default::default(),
        kind: IntrinsicCallKind::Format,
        args: call_args,
        kwargs: Vec::new(),
    }))
}

fn concat_string_exprs(values: Vec<Expr>) -> Expr {
    format_call_expr(
        values
            .iter()
            .map(|_| {
                FormatTemplatePart::Placeholder(FormatPlaceholder {
                    arg_ref: FormatArgRef::Implicit,
                    format_spec: None,
                })
            })
            .collect(),
        values,
    )
}

fn condition_command_expr(command: Expr) -> Expr {
    Expr::new(ExprKind::Invoke(ExprInvoke {
        span: Default::default(),
        target: ExprInvokeTarget::Function(Name::ident("__fp_condition_command")),
        args: vec![command],
        kwargs: Vec::new(),
    }))
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

fn apply_command_options(command: Expr, cwd: Option<Expr>, sudo: bool) -> Expr {
    let mut parts = Vec::new();
    let mut args = Vec::new();
    if sudo {
        parts.push(FormatTemplatePart::Literal("sudo ".to_string()));
    }
    if let Some(cwd) = cwd {
        parts.push(FormatTemplatePart::Literal("cd ".to_string()));
        parts.push(FormatTemplatePart::Placeholder(FormatPlaceholder {
            arg_ref: FormatArgRef::Implicit,
            format_spec: None,
        }));
        args.push(cwd);
        parts.push(FormatTemplatePart::Literal(" && ".to_string()));
    }
    args.push(command);
    parts.push(FormatTemplatePart::Placeholder(FormatPlaceholder {
        arg_ref: FormatArgRef::Implicit,
        format_spec: None,
    }));
    format_call_expr(parts, args)
}
