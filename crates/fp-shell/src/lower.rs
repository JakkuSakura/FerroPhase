use fp_core::ast::{
    Abi, BlockStmt, BlockStmtExpr, Expr, ExprArray, ExprBinOp, ExprBlock, ExprFor, ExprIf,
    ExprIntrinsicCall, ExprInvoke, ExprInvokeTarget, ExprKind, ExprLet, ExprMatch, ExprMatchCase,
    ExprSelect, ExprSelectType, ExprTry, ExprTryCatch, ExprWhile, File, FormatArgRef,
    FormatPlaceholder, FormatTemplatePart, FunctionSignature, Ident, Item, ItemDeclFunction,
    ItemDefFunction, ItemKind, Name, Node, NodeKind, Pattern, PatternKind, Ty, TypePrimitive,
    Value,
};
use fp_core::intrinsics::IntrinsicCallKind;
use fp_core::ops::BinOpKind;
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

pub fn lower_node(node: &Node) -> Result<Node, String> {
    let mut lowerer = Lowerer::new(Some(node));
    lowerer.lower_node(node)?;
    Ok(Node::file(File {
        path: lowerer.path,
        attrs: Vec::new(),
        items: lowerer.items,
    }))
}

struct Lowerer<'a> {
    path: PathBuf,
    items: Vec<Item>,
    known_functions: HashSet<String>,
    callable_aliases: HashMap<String, Option<String>>,
    functions: HashMap<String, FunctionInfo>,
    inventory: Option<&'a Node>,
    current_module_path: Vec<String>,
    host_context: Vec<Expr>,
}

#[derive(Clone)]
struct FunctionInfo {
    signature: FunctionSignature,
    emitted_name: String,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum HostTransport {
    Local,
    Ssh,
    Docker,
    Kubectl,
    Winrm,
    Chroot,
}

impl<'a> Lowerer<'a> {
    fn new(inventory: Option<&'a Node>) -> Self {
        Self {
            path: PathBuf::new(),
            items: Vec::new(),
            known_functions: HashSet::new(),
            callable_aliases: HashMap::new(),
            functions: HashMap::new(),
            inventory,
            current_module_path: Vec::new(),
            host_context: Vec::new(),
        }
    }

    fn lower_node(&mut self, node: &Node) -> Result<(), String> {
        match node.kind() {
            NodeKind::File(file) => {
                self.path = file.path.clone();
                for item in &file.items {
                    self.discover_functions(item, &[]);
                }
                for item in &file.items {
                    self.lower_item(item, None, &[])?;
                }
            }
            NodeKind::Item(item) => self.lower_item(item, None, &[])?,
            NodeKind::Expr(expr) => {
                let mut out = Vec::new();
                self.lower_expr_into(expr, &mut out)?;
                self.items
                    .extend(out.into_iter().map(|expr| Item::from(ItemKind::Expr(expr))));
            }
            NodeKind::Query(_) | NodeKind::Schema(_) | NodeKind::Workspace(_) => {}
        }
        Ok(())
    }

    fn discover_functions(&mut self, item: &Item, module_path: &[String]) {
        match item.kind() {
            ItemKind::DefFunction(function) => {
                let name = qualify_name(module_path, function.name.as_str());
                self.known_functions.insert(name.clone());
                self.register_callable_alias(function.name.as_str(), &name);
                self.functions.insert(
                    name,
                    FunctionInfo {
                        signature: function.sig.clone(),
                        emitted_name: emitted_call_name(&qualify_name(
                            module_path,
                            function.name.as_str(),
                        )),
                    },
                );
            }
            ItemKind::DeclFunction(function) => {
                if !matches!(function.sig.abi, Abi::Rust) {
                    let name = qualify_name(module_path, function.name.as_str());
                    self.known_functions.insert(name.clone());
                    self.register_callable_alias(function.name.as_str(), &name);
                    self.functions.insert(
                        name,
                        FunctionInfo {
                            signature: function.sig.clone(),
                            emitted_name: function.name.as_str().to_string(),
                        },
                    );
                }
            }
            ItemKind::Module(module) => {
                let mut child_path = module_path.to_vec();
                child_path.push(module.name.as_str().to_string());
                for child in &module.items {
                    self.discover_functions(child, &child_path);
                }
            }
            _ => {}
        }
    }

    fn register_callable_alias(&mut self, alias: &str, full_name: &str) {
        match self.callable_aliases.get(alias) {
            None => {
                self.callable_aliases
                    .insert(alias.to_string(), Some(full_name.to_string()));
            }
            Some(Some(existing)) if existing == full_name => {}
            _ => {
                self.callable_aliases.insert(alias.to_string(), None);
            }
        }
    }

    fn lower_item(
        &mut self,
        item: &Item,
        mut target: Option<&mut Vec<Expr>>,
        module_path: &[String],
    ) -> Result<(), String> {
        match item.kind() {
            ItemKind::DefFunction(function) => {
                let name = qualify_name(module_path, function.name.as_str());
                if name == "main" {
                    if let Some(target) = target {
                        self.lower_expr_into(&function.body, target)?;
                    } else {
                        let mut temp = Vec::new();
                        self.lower_expr_into(&function.body, &mut temp)?;
                        self.items.extend(
                            temp.into_iter()
                                .map(|expr| Item::from(ItemKind::Expr(expr))),
                        );
                    }
                } else {
                    let previous_module_path =
                        std::mem::replace(&mut self.current_module_path, module_path.to_vec());
                    let def = self.lower_function(function, &name)?;
                    self.current_module_path = previous_module_path;
                    self.insert_function(def);
                }
            }
            ItemKind::DefConst(def) => {
                if let Some(target) = target {
                    self.lower_expr_into(&def.value, target)?;
                }
            }
            ItemKind::DefStatic(def) => {
                if let Some(target) = target {
                    self.lower_expr_into(&def.value, target)?;
                }
            }
            ItemKind::Module(module) => {
                let mut child_path = module_path.to_vec();
                child_path.push(module.name.as_str().to_string());
                let previous_module_path =
                    std::mem::replace(&mut self.current_module_path, child_path.clone());
                for child in &module.items {
                    self.lower_item(child, target.as_deref_mut(), &child_path)?;
                }
                self.current_module_path = previous_module_path;
            }
            ItemKind::DeclFunction(function) => {
                if matches!(function.sig.abi, Abi::Rust) {
                    return Ok(());
                }
                let decl =
                    self.lower_decl(function, &qualify_name(module_path, function.name.as_str()));
                self.insert_decl(decl);
            }
            ItemKind::Expr(expr) => {
                if let Some(target) = target {
                    self.lower_expr_into(expr, target)?;
                } else {
                    let mut temp = Vec::new();
                    self.lower_expr_into(expr, &mut temp)?;
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

    fn lower_function(
        &mut self,
        function: &ItemDefFunction,
        logical_name: &str,
    ) -> Result<ItemDefFunction, String> {
        let mut body = Vec::new();
        self.lower_expr_into(&function.body, &mut body)?;
        let mut lowered = function.clone();
        lowered.name = Ident::new(emitted_call_name(logical_name));
        lowered.body = statements_to_block_expr(body).into_expr().into();
        Ok(lowered)
    }

    fn lower_decl(&self, function: &ItemDeclFunction, logical_name: &str) -> ItemDeclFunction {
        let _ = logical_name;
        function.clone()
    }

    fn lower_expr_into(&mut self, expr: &Expr, out: &mut Vec<Expr>) -> Result<(), String> {
        match expr.kind() {
            fp_core::ast::ExprKind::Block(block) => self.lower_block(block, out),
            fp_core::ast::ExprKind::Invoke(invoke) => {
                if let Some(hosts) = self.expand_invoke_hosts(invoke)? {
                    for host in hosts {
                        if let Some(invocation) =
                            self.lower_function_invoke_with_host(invoke, Some(host))?
                        {
                            out.push(invocation);
                        }
                    }
                    Ok(())
                } else if let Some(host) = self.current_host_override().cloned() {
                    if let Some(invocation) =
                        self.lower_function_invoke_with_host(invoke, Some(host))?
                    {
                        out.push(invocation);
                    }
                    Ok(())
                } else if let Some(invocation) = self.lower_function_invoke(invoke)? {
                    out.push(invocation);
                    Ok(())
                } else {
                    Ok(())
                }
            }
            fp_core::ast::ExprKind::With(expr_with) => {
                if let Some(hosts) = self.parse_host_selector(&expr_with.context) {
                    for host in hosts {
                        self.host_context.push(host);
                        self.lower_expr_into(&expr_with.body, out)?;
                        self.host_context.pop();
                    }
                    Ok(())
                } else {
                    self.lower_expr_into(&expr_with.body, out)
                }
            }
            fp_core::ast::ExprKind::If(expr_if) => {
                out.push(self.lower_if_expr(expr_if)?);
                Ok(())
            }
            fp_core::ast::ExprKind::Try(expr_try) => {
                out.push(self.lower_try_expr(expr_try)?);
                Ok(())
            }
            fp_core::ast::ExprKind::Match(expr_match) => {
                out.extend(self.lower_match(expr_match)?);
                Ok(())
            }
            fp_core::ast::ExprKind::While(expr_while) => {
                let mut body = Vec::new();
                self.lower_expr_into(&expr_while.body, &mut body)?;
                out.push(Expr::new(ExprKind::While(ExprWhile {
                    span: expr_while.span,
                    cond: self.lower_condition(&expr_while.cond).into(),
                    body: statements_to_block_expr(body).into_expr().into(),
                })));
                Ok(())
            }
            fp_core::ast::ExprKind::For(expr_for) => {
                out.push(self.lower_for_expr(expr_for)?);
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

    fn lower_block(&mut self, block: &ExprBlock, out: &mut Vec<Expr>) -> Result<(), String> {
        if block
            .stmts
            .iter()
            .any(|statement| matches!(statement, BlockStmt::Defer(_)))
        {
            out.push(self.lower_block_with_defer(block)?);
            return Ok(());
        }
        for statement in &block.stmts {
            match statement {
                BlockStmt::Item(item) => self.lower_item(item, Some(out), &[])?,
                BlockStmt::Expr(expr) => self.lower_expr_into(&expr.expr, out)?,
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
                BlockStmt::Defer(_) => {}
                BlockStmt::Noop | BlockStmt::Any(_) => {}
            }
        }
        Ok(())
    }

    fn lower_block_with_defer(&mut self, block: &ExprBlock) -> Result<Expr, String> {
        let mut body = Vec::new();
        let mut deferred = Vec::new();
        for statement in &block.stmts {
            match statement {
                BlockStmt::Defer(stmt_defer) => {
                    self.lower_expr_into(&stmt_defer.expr, &mut deferred)?;
                }
                BlockStmt::Item(item) => self.lower_item(item, Some(&mut body), &[])?,
                BlockStmt::Expr(expr) => self.lower_expr_into(&expr.expr, &mut body)?,
                BlockStmt::Let(stmt) => {
                    let Some(_name) = stmt.pat.as_ident() else {
                        continue;
                    };
                    let Some(init) = &stmt.init else {
                        continue;
                    };
                    let value = self.lower_value_expr(init)?;
                    body.push(Expr::new(ExprKind::Let(ExprLet {
                        span: init.span(),
                        pat: stmt.pat.clone().into(),
                        expr: value.into(),
                    })));
                }
                BlockStmt::Noop | BlockStmt::Any(_) => {}
            }
        }
        deferred.reverse();
        Ok(Expr::new(ExprKind::Try(ExprTry {
            span: block.span,
            expr: statements_to_block_expr(body).into_expr().into(),
            catches: Vec::new(),
            elze: None,
            finally: Some(statements_to_block_expr(deferred).into_expr().into()),
        })))
    }

    fn lower_function_invoke(&mut self, invoke: &ExprInvoke) -> Result<Option<Expr>, String> {
        self.lower_function_invoke_with_host(invoke, None)
    }

    fn lower_function_invoke_with_host(
        &mut self,
        invoke: &ExprInvoke,
        host_override: Option<Expr>,
    ) -> Result<Option<Expr>, String> {
        let Some(path) = invoke_target_segments(&invoke.target) else {
            return Ok(None);
        };
        let Some(name) = self.call_target_name(&path) else {
            return Ok(None);
        };
        let Some(info) = self.functions.get(name.as_str()) else {
            return Ok(None);
        };
        let args = self.lower_call_arguments(invoke, info, host_override.as_ref())?;
        let emitted_name = self
            .specialized_emitted_call_name(&name, info, &args)
            .unwrap_or_else(|| info.emitted_name.clone());
        Ok(Some(Expr::new(ExprKind::Invoke(ExprInvoke {
            span: invoke.span,
            target: ExprInvokeTarget::Function(Name::ident(emitted_name)),
            args,
            kwargs: Vec::new(),
        }))))
    }

    fn lower_call_arguments(
        &self,
        invoke: &ExprInvoke,
        info: &FunctionInfo,
        host_override: Option<&Expr>,
    ) -> Result<Vec<Expr>, String> {
        let signature = &info.signature;
        let mut args = Vec::with_capacity(signature.params.len());
        for (index, param) in signature.params.iter().enumerate() {
            let value = if let Some(arg) = invoke.args.get(index) {
                self.lower_typed_expr(arg, &param.ty)?
            } else if param.is_context && host_override.is_some() {
                self.lower_typed_expr(host_override.expect("host override"), &param.ty)?
            } else if let Some(kwarg) = invoke
                .kwargs
                .iter()
                .find(|kwarg| kwarg.name == param.name.as_str())
            {
                self.lower_typed_expr(&kwarg.value, &param.ty)?
            } else if let Some(default) = &param.default {
                Expr::value(default.clone())
            } else if param.is_context {
                string_literal_expr("localhost")
            } else {
                self.default_value_for_type(&param.ty)?
            };
            args.push(value);
        }
        Ok(args)
    }

    fn expand_invoke_hosts(&self, invoke: &ExprInvoke) -> Result<Option<Vec<Expr>>, String> {
        let Some(path) = invoke_target_segments(&invoke.target) else {
            return Ok(None);
        };
        let Some(name) = self.call_target_name(&path) else {
            return Ok(None);
        };
        let Some(info) = self.functions.get(name.as_str()) else {
            return Ok(None);
        };
        let accepts_host = info.signature.params.iter().any(|param| param.is_context);
        if !accepts_host {
            return Ok(None);
        }
        let selector = info
            .signature
            .params
            .iter()
            .enumerate()
            .find(|(_, param)| param.is_context)
            .and_then(|(index, _)| invoke.args.get(index))
            .or_else(|| {
                invoke
                    .kwargs
                    .iter()
                    .find(|kwarg| {
                        info.signature
                            .params
                            .iter()
                            .any(|param| param.is_context && kwarg.name == param.name.as_str())
                    })
                    .map(|kwarg| &kwarg.value)
            });
        let hosts = if let Some(selector) = selector {
            let Some(hosts) = self.parse_host_selector(selector) else {
                return Err(
                    "host selector must be a string or list/tuple/array of strings".to_string(),
                );
            };
            hosts
        } else {
            return Ok(None);
        };
        Ok(Some(hosts))
    }

    fn lower_typed_expr(&self, expr: &Expr, ty: &Ty) -> Result<Expr, String> {
        match ty {
            Ty::Primitive(TypePrimitive::Bool) => self
                .lower_bool_expr(expr)
                .ok_or_else(|| "expression could not be lowered to bool".to_string()),
            Ty::Primitive(TypePrimitive::Int(_)) => self
                .lower_int_expr(expr)
                .ok_or_else(|| "expression could not be lowered to int".to_string()),
            _ => self.lower_value_expr(expr),
        }
    }

    fn default_value_for_type(&self, ty: &Ty) -> Result<Expr, String> {
        match ty {
            Ty::Primitive(TypePrimitive::Bool) => Ok(Expr::value(Value::bool(false))),
            Ty::Primitive(TypePrimitive::Int(_)) => Ok(Expr::value(Value::int(0))),
            _ => Ok(empty_string_expr()),
        }
    }

    fn call_target_name(&self, path: &[String]) -> Option<String> {
        let full = path.join("::");
        match (self.is_known_callable(&full), path.last()) {
            (true, _) => Some(full),
            (false, Some(name)) if path.len() == 1 => {
                let scoped = qualify_name(&self.current_module_path, name);
                if self.is_known_callable(&scoped) {
                    Some(scoped)
                } else {
                    self.callable_aliases
                        .get(name)
                        .and_then(|resolved| resolved.clone())
                }
            }
            (false, Some(_name)) => None,
            _ => None,
        }
    }

    fn lower_if_expr(&mut self, expr_if: &ExprIf) -> Result<Expr, String> {
        let mut then_block = Vec::new();
        self.lower_expr_into(&expr_if.then, &mut then_block)?;
        let else_block = if let Some(else_expr) = &expr_if.elze {
            let mut else_statements = Vec::new();
            self.lower_expr_into(else_expr, &mut else_statements)?;
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

    fn lower_try_expr(&mut self, expr_try: &ExprTry) -> Result<Expr, String> {
        let mut body = Vec::new();
        self.lower_expr_into(&expr_try.expr, &mut body)?;

        let catches = expr_try
            .catches
            .iter()
            .map(|catch| -> Result<ExprTryCatch, String> {
                let mut catch_body = Vec::new();
                self.lower_expr_into(&catch.body, &mut catch_body)?;
                Ok(ExprTryCatch {
                    span: catch.span,
                    pat: catch.pat.clone(),
                    body: statements_to_block_expr(catch_body).into_expr().into(),
                })
            })
            .collect::<Result<Vec<_>, _>>()?;

        let elze = if let Some(elze) = &expr_try.elze {
            let mut else_body = Vec::new();
            self.lower_expr_into(elze, &mut else_body)?;
            Some(statements_to_block_expr(else_body).into_expr().into())
        } else {
            None
        };

        let finally = if let Some(finally) = &expr_try.finally {
            let mut finally_body = Vec::new();
            self.lower_expr_into(finally, &mut finally_body)?;
            Some(statements_to_block_expr(finally_body).into_expr().into())
        } else {
            None
        };

        Ok(Expr::new(ExprKind::Try(ExprTry {
            span: expr_try.span,
            expr: statements_to_block_expr(body).into_expr().into(),
            catches,
            elze,
            finally,
        })))
    }

    fn lower_match(&mut self, expr_match: &ExprMatch) -> Result<Vec<Expr>, String> {
        let Some(scrutinee) = expr_match.scrutinee.as_deref() else {
            return Err("match expression requires a scrutinee".to_string());
        };
        let scrutinee = self
            .lower_string_expr(scrutinee)
            .ok_or_else(|| "match scrutinee must resolve to string".to_string())?;
        if expr_match.cases.iter().all(|case| case.guard.is_none()) {
            return self.lower_match_native(&scrutinee, &expr_match.cases);
        }
        self.lower_match_cases(&scrutinee, &expr_match.cases)
    }

    fn lower_match_native(
        &mut self,
        scrutinee: &Expr,
        cases: &[ExprMatchCase],
    ) -> Result<Vec<Expr>, String> {
        let lowered_cases = cases
            .iter()
            .map(|case| -> Result<ExprMatchCase, String> {
                let mut body = Vec::new();
                self.lower_expr_into(&case.body, &mut body)?;
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
    ) -> Result<Vec<Expr>, String> {
        let Some((first, rest)) = cases.split_first() else {
            return Ok(Vec::new());
        };
        let fallback = self.lower_match_cases(scrutinee, rest)?;
        self.lower_match_case(scrutinee, first, fallback)
    }

    fn lower_match_case(
        &mut self,
        scrutinee: &Expr,
        case: &ExprMatchCase,
        fallback: Vec<Expr>,
    ) -> Result<Vec<Expr>, String> {
        let mut body_statements = Vec::new();
        self.lower_expr_into(&case.body, &mut body_statements)?;
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

    fn lower_for_expr(&mut self, expr_for: &ExprFor) -> Result<Expr, String> {
        let PatternKind::Ident(_pattern) = expr_for.pat.kind() else {
            return Err("for-loop pattern must be an identifier".to_string());
        };
        let values = self
            .resolve_string_list_expr(&expr_for.iter)
            .ok_or_else(|| "for-loop iterable must resolve to a string list".to_string())?;
        let mut body = Vec::new();
        self.lower_expr_into(&expr_for.body, &mut body)?;
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
            ExprKind::Select(_)
            | ExprKind::FormatString(_)
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
            ExprKind::Select(select) => self.lower_host_field_select(select),
            ExprKind::Name(_) => Some(expr.clone()),
            fp_core::ast::ExprKind::Invoke(invoke) => {
                let path = invoke_target_segments(&invoke.target)?;
                let name = self.call_target_name(&path)?;
                let info = self.functions.get(&name)?;
                let mut args = Vec::new();
                args.extend(self.lower_call_arguments(invoke, info, None).ok()?);
                let emitted_name = info.emitted_name.clone();
                Some(Expr::new(ExprKind::Invoke(ExprInvoke {
                    span: invoke.span,
                    target: ExprInvokeTarget::Function(Name::ident(emitted_name)),
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

    fn lower_host_field_select(&self, select: &ExprSelect) -> Option<Expr> {
        if select.select != ExprSelectType::Field {
            return None;
        }
        let field = select.field.as_str();
        if !matches!(
            field,
            "transport"
                | "address"
                | "user"
                | "port"
                | "container"
                | "pod"
                | "namespace"
                | "context"
                | "password"
                | "scheme"
                | "chroot_directory"
        ) {
            return None;
        }
        let target = Name::path(fp_core::ast::Path::plain(vec![
            Ident::new("std"),
            Ident::new("hosts"),
            Ident::new(field),
        ]));
        Some(Expr::new(ExprKind::Invoke(ExprInvoke {
            span: select.span,
            target: ExprInvokeTarget::Function(target),
            args: vec![(*select.obj.clone())],
            kwargs: Vec::new(),
        })))
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
            fp_core::ast::ExprKind::Invoke(invoke) => {
                let path = invoke_target_segments(&invoke.target)?;
                let name = self.call_target_name(&path)?;
                let info = self.functions.get(&name)?;
                let mut args = Vec::new();
                args.extend(self.lower_call_arguments(invoke, info, None).ok()?);
                let emitted_name = info.emitted_name.clone();
                Some(Expr::new(ExprKind::Invoke(ExprInvoke {
                    span: invoke.span,
                    target: ExprInvokeTarget::Function(Name::ident(emitted_name)),
                    args,
                    kwargs: Vec::new(),
                })))
            }
            fp_core::ast::ExprKind::Paren(paren) => self.lower_bool_expr(&paren.expr),
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

    fn parse_host_selector(&self, expr: &Expr) -> Option<Vec<Expr>> {
        self.resolve_string_list_expr(expr).map(|hosts| {
            let mut selectors = Vec::new();
            for host in hosts {
                match string_literal_value(&host) {
                    Some(name) if name == "localhost" => selectors.push(host),
                    Some(name) => {
                        if let Some(group_hosts) = inventory_group_hosts(self.inventory, &name) {
                            selectors.extend(group_hosts.into_iter().map(string_literal_expr));
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
            matches!(
                item.kind(),
                ItemKind::DeclFunction(existing)
                    if existing.name == decl.name && existing.sig.abi == decl.sig.abi
            )
        }) else {
            self.items.push(Item::from(ItemKind::DeclFunction(decl)));
            return;
        };
        self.items[index] = Item::from(ItemKind::DeclFunction(decl));
    }

    fn is_known_callable(&self, name: &str) -> bool {
        self.known_functions.contains(name)
    }

    fn current_host_override(&self) -> Option<&Expr> {
        self.host_context.last()
    }

    fn specialized_emitted_call_name(
        &self,
        name: &str,
        info: &FunctionInfo,
        args: &[Expr],
    ) -> Option<String> {
        let host = info
            .signature
            .params
            .iter()
            .enumerate()
            .find(|(_, param)| param.is_context)
            .and_then(|(index, _)| args.get(index))
            .and_then(string_literal_value)?;
        let transport = self.host_transport_for_name(&host)?;
        match name {
            "std::ops::server::shell" => Some(
                emitted_call_name(match transport {
                    HostTransport::Local => "std::ops::server::shell_local",
                    HostTransport::Ssh => "std::ops::server::shell_ssh",
                    HostTransport::Docker => "std::ops::server::shell_docker",
                    HostTransport::Kubectl => "std::ops::server::shell_kubectl",
                    HostTransport::Winrm => "std::ops::server::shell_winrm",
                    HostTransport::Chroot => "std::ops::server::shell_chroot",
                })
                .to_string(),
            ),
            "std::ops::files::copy" => Some(
                emitted_call_name(match transport {
                    HostTransport::Local => "std::ops::files::copy_local",
                    HostTransport::Ssh => "std::ops::files::copy_ssh",
                    HostTransport::Docker => "std::ops::files::copy_docker",
                    HostTransport::Kubectl => "std::ops::files::copy_kubectl",
                    HostTransport::Winrm => "std::ops::files::copy_winrm",
                    HostTransport::Chroot => "std::ops::files::copy_chroot",
                })
                .to_string(),
            ),
            "std::ops::files::template" => Some(
                emitted_call_name(match transport {
                    HostTransport::Local => "std::ops::files::template_local",
                    HostTransport::Ssh => "std::ops::files::template_ssh",
                    HostTransport::Chroot => "std::ops::files::template_chroot",
                    _ => "std::ops::files::template",
                })
                .to_string(),
            ),
            "std::ops::files::rsync" => Some(
                emitted_call_name(match transport {
                    HostTransport::Local => "std::ops::files::rsync_local",
                    HostTransport::Chroot => "std::ops::files::rsync_chroot",
                    _ => "std::ops::files::rsync_remote",
                })
                .to_string(),
            ),
            _ => None,
        }
    }

    fn host_transport_for_name(&self, host: &str) -> Option<HostTransport> {
        if host == "localhost" {
            return Some(HostTransport::Local);
        }
        let transport = inventory_host_transport(self.inventory, host)?;
        Some(match transport.as_str() {
            "local" => HostTransport::Local,
            "ssh" => HostTransport::Ssh,
            "docker" => HostTransport::Docker,
            "kubectl" => HostTransport::Kubectl,
            "winrm" => HostTransport::Winrm,
            "chroot" => HostTransport::Chroot,
            _ => return None,
        })
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
            BlockStmt::Defer(_)
            | BlockStmt::Any(_)
            | BlockStmt::Let(_)
            | BlockStmt::Item(_)
            | BlockStmt::Noop => None,
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

fn invoke_target_segments(target: &ExprInvokeTarget) -> Option<Vec<String>> {
    match target {
        ExprInvokeTarget::Function(name) => Some(name_to_segments(name)),
        _ => None,
    }
}

fn emitted_call_name(name: &str) -> String {
    if name.contains("::") {
        return mangle_emitted_name(name);
    }
    name.to_string()
}

fn mangle_emitted_name(name: &str) -> String {
    let mut out = String::from("__fp_");
    for segment in name.split("::") {
        if !out.ends_with('_') {
            out.push('_');
        }
        for ch in segment.chars() {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                out.push(ch);
            } else {
                out.push('_');
            }
        }
        out.push('_');
    }
    out
}

fn qualify_name(module_path: &[String], name: &str) -> String {
    if module_path.is_empty() {
        name.to_string()
    } else {
        format!("{}::{}", module_path.join("::"), name)
    }
}

fn name_to_segments(name: &Name) -> Vec<String> {
    name.to_path()
        .segments
        .iter()
        .map(|ident| ident.as_str().to_string())
        .collect()
}

fn inventory_group_hosts(inventory: Option<&Node>, group: &str) -> Option<Vec<String>> {
    let inventory_expr = inventory_root_expr(inventory?)?;
    let groups = struct_field_expr(inventory_expr, "groups")?;
    let entries = hashmap_from_entries(groups)?;
    for (key, value) in entries {
        if string_literal_value(key).as_deref() != Some(group) {
            continue;
        }
        return string_list_literal_values(value);
    }
    None
}

fn inventory_host_transport(inventory: Option<&Node>, host: &str) -> Option<String> {
    let inventory_expr = inventory_root_expr(inventory?)?;
    let hosts = struct_field_expr(inventory_expr, "hosts")?;
    let entries = hashmap_from_entries(hosts)?;
    for (key, value) in entries {
        if string_literal_value(key).as_deref() != Some(host) {
            continue;
        }
        return struct_field_expr(value, "transport").and_then(string_literal_value);
    }
    None
}

fn inventory_root_expr(node: &Node) -> Option<&Expr> {
    let NodeKind::File(file) = node.kind() else {
        return None;
    };
    let std_module = file.items.iter().find_map(|item| match item.kind() {
        ItemKind::Module(module) if module.name.as_str() == "std" => Some(module),
        _ => None,
    })?;
    let hosts_module = std_module.items.iter().find_map(|item| match item.kind() {
        ItemKind::Module(module) if module.name.as_str() == "hosts" => Some(module),
        _ => None,
    })?;
    let function = hosts_module
        .items
        .iter()
        .find_map(|item| match item.kind() {
            ItemKind::DefFunction(function) if function.name.as_str() == "inventory" => {
                Some(function)
            }
            _ => None,
        })?;
    function_result_expr(&function.body)
}

fn function_result_expr(expr: &Expr) -> Option<&Expr> {
    match expr.kind() {
        ExprKind::Block(block) => block.stmts.iter().rev().find_map(|stmt| match stmt {
            BlockStmt::Expr(expr) => Some(expr.expr.as_ref()),
            _ => None,
        }),
        _ => Some(expr),
    }
}

fn struct_field_expr<'a>(expr: &'a Expr, field: &str) -> Option<&'a Expr> {
    let fields = match expr.kind() {
        ExprKind::Struct(expr_struct) => &expr_struct.fields,
        ExprKind::Structural(expr_structural) => &expr_structural.fields,
        _ => return None,
    };
    fields
        .iter()
        .find(|candidate| candidate.name.as_str() == field)
        .and_then(|candidate| candidate.value.as_ref())
}

fn hashmap_from_entries(expr: &Expr) -> Option<Vec<(&Expr, &Expr)>> {
    match expr.kind() {
        ExprKind::IntrinsicContainer(container) => match container {
            fp_core::ast::ExprIntrinsicContainer::HashMapEntries { entries } => Some(
                entries
                    .iter()
                    .map(|entry| (&entry.key, &entry.value))
                    .collect::<Vec<_>>(),
            ),
            _ => None,
        },
        ExprKind::Invoke(invoke) => {
            let target = invoke_target_segments(&invoke.target)?;
            match target.as_slice() {
                [.., owner, method] if owner == "HashMap" && method == "from" => {}
                _ => return None,
            }
            let [entries_expr] = invoke.args.as_slice() else {
                return None;
            };
            tuple_like_values(entries_expr)?
                .into_iter()
                .map(|entry| match entry.kind() {
                    ExprKind::Tuple(tuple) if tuple.values.len() == 2 => {
                        Some((&tuple.values[0], &tuple.values[1]))
                    }
                    _ => None,
                })
                .collect()
        }
        _ => None,
    }
}

fn string_list_literal_values(expr: &Expr) -> Option<Vec<String>> {
    tuple_like_values(expr)?
        .into_iter()
        .map(string_literal_value)
        .collect()
}

fn tuple_like_values(expr: &Expr) -> Option<Vec<&Expr>> {
    match expr.kind() {
        ExprKind::Array(array) => Some(array.values.iter().collect()),
        ExprKind::Tuple(tuple) => Some(tuple.values.iter().collect()),
        ExprKind::IntrinsicContainer(container) => match container {
            fp_core::ast::ExprIntrinsicContainer::VecElements { elements } => {
                Some(elements.iter().collect())
            }
            _ => None,
        },
        ExprKind::Paren(paren) => tuple_like_values(&paren.expr),
        ExprKind::Struct(expr_struct) if expr_struct.fields.is_empty() => Some(Vec::new()),
        ExprKind::Structural(expr_structural) if expr_structural.fields.is_empty() => {
            Some(Vec::new())
        }
        _ => None,
    }
}
