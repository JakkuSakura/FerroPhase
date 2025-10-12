use std::collections::{HashMap, HashSet};

use fp_core::ast::*;
use fp_core::ast::{Ident, Locator};
use fp_core::ast::{Pattern, PatternKind};
use fp_core::error::{Error as CoreError, Result};

const DUMMY_CAPTURE_NAME: &str = "__fp_no_capture";

fn expand_intrinsic_collection(expr: &mut Expr) -> bool {
    if let ExprKind::IntrinsicCollection(collection) = expr.kind() {
        let new_expr = collection.clone().into_const_expr();
        *expr = new_expr;
        true
    } else {
        false
    }
}

#[derive(Clone)]
struct ClosureInfo {
    env_struct_ident: Ident,
    env_struct_ty: Ty,
    call_fn_ident: Ident,
    call_ret_ty: Ty,
    #[allow(dead_code)]
    fn_ty: Ty,
}

#[derive(Clone)]
struct Capture {
    name: Ident,
    ty: Ty,
}

pub fn lower_closures(node: &mut Node) -> Result<()> {
    let NodeKind::File(file) = node.kind_mut() else {
        return Ok(());
    };

    let mut pass = ClosureLowering::new();
    pass.find_and_transform_functions(&mut file.items)?;
    pass.rewrite_usage(&mut file.items)?;

    if !pass.generated_items.is_empty() {
        let mut new_items = pass.generated_items;
        new_items.append(&mut file.items);
        file.items = new_items;
    }
    Ok(())
}

struct ClosureLowering {
    counter: usize,
    function_infos: HashMap<String, ClosureInfo>,
    struct_infos: HashMap<String, ClosureInfo>,
    variable_infos: HashMap<String, ClosureInfo>,
    generated_items: Vec<Item>,
}

impl ClosureLowering {
    fn new() -> Self {
        Self {
            counter: 0,
            function_infos: HashMap::new(),
            struct_infos: HashMap::new(),
            variable_infos: HashMap::new(),
            generated_items: Vec::new(),
        }
    }

    fn find_and_transform_functions(&mut self, items: &mut [Item]) -> Result<()> {
        for item in items {
            match item.kind_mut() {
                ItemKind::Module(module) => {
                    self.find_and_transform_functions(&mut module.items)?;
                }
                ItemKind::DefFunction(func) => {
                    if let Some(info) = self.transform_function(func)? {
                        self.function_infos
                            .insert(func.name.as_str().to_string(), info.clone());
                        self.struct_infos
                            .insert(info.env_struct_ident.as_str().to_string(), info);
                    }
                }
                _ => {}
            }
        }
        Ok(())
    }

    fn transform_function(&mut self, func: &mut ItemDefFunction) -> Result<Option<ClosureInfo>> {
        if let Some(info) = self.transform_closure_expr(func.body.as_mut())? {
            // Update function return typing to reflect lowered environment struct.
            let env_ret_ty = info.env_struct_ty.clone();

            if let Some(ty_fn) = func.ty.as_mut() {
                ty_fn.ret_ty = Some(Box::new(env_ret_ty.clone()));
            }

            if func.ty.is_none() {
                func.ty = Some(TypeFunction {
                    params: func
                        .sig
                        .params
                        .iter()
                        .map(|param| param.ty.clone())
                        .collect(),
                    generics_params: func.sig.generics_params.clone(),
                    ret_ty: Some(Box::new(env_ret_ty.clone())),
                });
            }

            if func.ty_annotation.is_some() || func.ty.is_some() {
                func.ty_annotation = func.ty.as_ref().map(|ty_fn| Ty::Function(ty_fn.clone()));
            }

            if let Some(ret_slot) = func.sig.ret_ty.as_mut() {
                *ret_slot = env_ret_ty.clone();
            } else {
                func.sig.ret_ty = Some(env_ret_ty.clone());
            }

            return Ok(Some(info));
        }

        if let ExprKind::Block(block) = func.body.kind_mut() {
            if let Some(last_expr) = block.last_expr_mut() {
                if let Some(info) = self.transform_closure_expr(last_expr)? {
                    return Ok(Some(info));
                }
            }
        }

        Ok(None)
    }

    fn transform_closure_expr(&mut self, expr: &mut Expr) -> Result<Option<ClosureInfo>> {
        let Some(expr_ty) = expr.ty().cloned() else {
            return Ok(None);
        };
        let Ty::Function(fn_ty) = expr_ty.clone() else {
            return Ok(None);
        };

        let ExprKind::Closure(closure) = expr.kind_mut() else {
            return Ok(None);
        };

        let mut param_names = Vec::new();
        let mut param_set = HashSet::new();
        for param in &closure.params {
            if let PatternKind::Ident(ident) = param.kind() {
                let name = ident.ident.name.as_str().to_string();
                param_set.insert(name.clone());
                param_names.push(name);
            } else {
                return Err(CoreError::from(
                    "only simple identifier parameters are supported in closures",
                ));
            }
        }

        let captures = self.collect_captures(closure.body.as_ref(), &param_set)?;

        let struct_ident = Ident::new(format!("__Closure{}", self.counter));
        let call_ident = Ident::new(format!("__closure{}_call", self.counter));
        self.counter += 1;

        let mut struct_fields: Vec<StructuralField> = captures
            .iter()
            .map(|capture| StructuralField::new(capture.name.clone(), capture.ty.clone()))
            .collect();
        if struct_fields.is_empty() {
            struct_fields.push(StructuralField::new(
                Ident::new(DUMMY_CAPTURE_NAME),
                Ty::Primitive(TypePrimitive::Int(TypeInt::I8)),
            ));
        }
        let struct_decl = TypeStruct {
            name: struct_ident.clone(),
            fields: struct_fields,
        };
        let env_struct_ty = Ty::Struct(struct_decl.clone());

        let mut struct_item = Item::new(ItemKind::DefStruct(ItemDefStruct {
            visibility: Visibility::Private,
            name: struct_ident.clone(),
            value: struct_decl.clone(),
        }));
        struct_item.set_ty(Ty::Struct(struct_decl.clone()));
        let env_param_ident = Ident::new("__env");
        let mut fn_params = Vec::new();
        let mut fn_param_tys = Vec::new();
        fn_params.push(FunctionParam::new(
            env_param_ident.clone(),
            env_struct_ty.clone(),
        ));
        fn_param_tys.push(env_struct_ty.clone());
        for (idx, name) in param_names.iter().enumerate() {
            let ty = fn_ty
                .params
                .get(idx)
                .cloned()
                .unwrap_or_else(|| Ty::Any(TypeAny));
            fn_params.push(FunctionParam::new(Ident::new(name.clone()), ty.clone()));
            fn_param_tys.push(ty);
        }

        let mut rewritten_body = (*closure.body).clone();
        let inferred_ret_ty = fn_ty
            .ret_ty
            .as_ref()
            .and_then(|ty| {
                if matches!(ty.as_ref(), Ty::Unknown(_)) {
                    None
                } else {
                    Some(ty.as_ref().clone())
                }
            })
            .or_else(|| {
                closure
                    .body
                    .ty()
                    .cloned()
                    .or_else(|| rewritten_body.ty().cloned())
                    .and_then(|ty| {
                        if matches!(ty, Ty::Unknown(_)) {
                            None
                        } else {
                            Some(ty)
                        }
                    })
            });
        let fallback_ret_ty = fn_ty.ret_ty.as_ref().and_then(|ty| {
            if matches!(ty.as_ref(), Ty::Unknown(_)) {
                None
            } else {
                Some(ty.as_ref().clone())
            }
        });
        let call_ret_ty = inferred_ret_ty
            .clone()
            .or(fallback_ret_ty)
            .unwrap_or_else(|| Ty::Unknown(TypeUnknown));

        self.rewrite_captured_usage(
            &mut rewritten_body,
            &captures,
            &env_param_ident,
            &env_struct_ty,
        );

        let mut fn_item_ast =
            ItemDefFunction::new_simple(call_ident.clone(), rewritten_body.into());
        fn_item_ast.visibility = Visibility::Private;
        fn_item_ast.sig.params = fn_params;
        fn_item_ast.sig.ret_ty = Some(call_ret_ty.clone());
        fn_item_ast.ty = Some(TypeFunction {
            params: fn_param_tys.clone(),
            generics_params: Vec::new(),
            ret_ty: Some(Box::new(call_ret_ty.clone())),
        });
        fn_item_ast.ty_annotation = fn_item_ast.ty.clone().map(|ty_fn| Ty::Function(ty_fn));

        let fn_item = Item::new(ItemKind::DefFunction(fn_item_ast));

        self.generated_items.push(struct_item);
        self.generated_items.push(fn_item);

        let mut fields = Vec::new();
        for capture in &captures {
            let mut value_expr = Expr::ident(capture.name.clone());
            value_expr.set_ty(capture.ty.clone());
            fields.push(ExprField::new(capture.name.clone(), value_expr));
        }
        if fields.is_empty() {
            let mut value_expr = Expr::value(Value::int(0));
            value_expr.set_ty(Ty::Primitive(TypePrimitive::Int(TypeInt::I8)));
            fields.push(ExprField::new(Ident::new(DUMMY_CAPTURE_NAME), value_expr));
        }

        let struct_name_expr = Expr::ident(struct_ident.clone());

        let mut struct_expr = Expr::new(ExprKind::Struct(ExprStruct {
            name: struct_name_expr.into(),
            fields,
        }));
        struct_expr.set_ty(env_struct_ty.clone());

        *expr = struct_expr;

        let info = ClosureInfo {
            env_struct_ident: struct_ident,
            env_struct_ty,
            call_fn_ident: call_ident,
            call_ret_ty: call_ret_ty.clone(),
            fn_ty: expr_ty,
        };

        Ok(Some(info))
    }

    fn rewrite_usage(&mut self, items: &mut [Item]) -> Result<()> {
        for item in items {
            match item.kind_mut() {
                ItemKind::Module(module) => self.rewrite_usage(&mut module.items)?,
                ItemKind::DefFunction(func) => {
                    self.rewrite_in_expr(func.body.as_mut())?;
                }
                ItemKind::DefConst(def) => self.rewrite_in_expr(def.value.as_mut())?,
                ItemKind::DefStatic(def) => self.rewrite_in_expr(def.value.as_mut())?,
                ItemKind::Expr(expr) => self.rewrite_in_expr(expr)?,
                _ => {}
            }
        }
        Ok(())
    }

    fn rewrite_in_expr(&mut self, expr: &mut Expr) -> Result<()> {
        if expand_intrinsic_collection(expr) {
            return self.rewrite_in_expr(expr);
        }

        if let Some(info) = self.transform_closure_expr(expr)? {
            self.struct_infos
                .insert(info.env_struct_ident.as_str().to_string(), info);
            return self.rewrite_in_expr(expr);
        }

        match expr.kind_mut() {
            ExprKind::Block(block) => {
                for stmt in &mut block.stmts {
                    self.rewrite_in_stmt(stmt)?;
                }
                if let Some(last) = block.last_expr_mut() {
                    self.rewrite_in_expr(last)?;
                }
            }
            ExprKind::If(expr_if) => {
                self.rewrite_in_expr(expr_if.cond.as_mut())?;
                self.rewrite_in_expr(expr_if.then.as_mut())?;
                if let Some(elze) = expr_if.elze.as_mut() {
                    self.rewrite_in_expr(elze)?;
                }
            }
            ExprKind::Loop(expr_loop) => self.rewrite_in_expr(expr_loop.body.as_mut())?,
            ExprKind::While(expr_while) => {
                self.rewrite_in_expr(expr_while.cond.as_mut())?;
                self.rewrite_in_expr(expr_while.body.as_mut())?;
            }
            ExprKind::Match(expr_match) => {
                for case in &mut expr_match.cases {
                    self.rewrite_in_expr(case.cond.as_mut())?;
                    self.rewrite_in_expr(case.body.as_mut())?;
                }
            }
            ExprKind::Let(expr_let) => self.rewrite_in_expr(expr_let.expr.as_mut())?,
            ExprKind::Invoke(invoke) => {
                for arg in &mut invoke.args {
                    self.rewrite_in_expr(arg)?;
                }
                match &mut invoke.target {
                    ExprInvokeTarget::Expr(target) => {
                        self.rewrite_in_expr(target.as_mut())?;
                        if let Some(info) = self.closure_info_from_expr(target.as_ref()) {
                            let call_locator = Locator::ident(info.call_fn_ident.clone());
                            let mut new_args = Vec::with_capacity(invoke.args.len() + 1);
                            new_args.push(*target.clone());
                            new_args.extend(invoke.args.iter().cloned());
                            invoke.target = ExprInvokeTarget::Function(call_locator);
                            invoke.args = new_args;
                            expr.set_ty(info.call_ret_ty.clone());
                        }
                    }
                    ExprInvokeTarget::Function(locator) => {
                        if let Some(ident) = locator.as_ident() {
                            let info = self
                                .variable_infos
                                .get(ident.as_str())
                                .cloned()
                                .or_else(|| self.struct_infos.get(ident.as_str()).cloned());
                            if let Some(info) = info {
                                let mut env_expr = Expr::new(ExprKind::Locator(locator.clone()));
                                env_expr.set_ty(info.env_struct_ty.clone());
                                let call_locator = Locator::ident(info.call_fn_ident.clone());
                                let mut new_args = Vec::with_capacity(invoke.args.len() + 1);
                                new_args.push(env_expr);
                                new_args.extend(invoke.args.iter().cloned());
                                invoke.target = ExprInvokeTarget::Function(call_locator);
                                invoke.args = new_args;
                                expr.set_ty(info.call_ret_ty.clone());
                            }
                        }
                    }
                    _ => {}
                }
            }
            ExprKind::Assign(assign) => {
                self.rewrite_in_expr(assign.target.as_mut())?;
                self.rewrite_in_expr(assign.value.as_mut())?;
            }
            ExprKind::Select(select) => self.rewrite_in_expr(select.obj.as_mut())?,
            ExprKind::Struct(struct_expr) => {
                self.rewrite_in_expr(struct_expr.name.as_mut())?;
                for field in &mut struct_expr.fields {
                    if let Some(value) = field.value.as_mut() {
                        self.rewrite_in_expr(value)?;
                    }
                }
            }
            ExprKind::Structural(struct_expr) => {
                for field in &mut struct_expr.fields {
                    if let Some(value) = field.value.as_mut() {
                        self.rewrite_in_expr(value)?;
                    }
                }
            }
            ExprKind::Array(array) => {
                for value in &mut array.values {
                    self.rewrite_in_expr(value)?;
                }
            }
            ExprKind::ArrayRepeat(array_repeat) => {
                self.rewrite_in_expr(array_repeat.elem.as_mut())?;
                self.rewrite_in_expr(array_repeat.len.as_mut())?;
            }
            ExprKind::Tuple(tuple) => {
                for value in &mut tuple.values {
                    self.rewrite_in_expr(value)?;
                }
            }
            ExprKind::Reference(reference) => self.rewrite_in_expr(reference.referee.as_mut())?,
            ExprKind::Dereference(deref) => self.rewrite_in_expr(deref.referee.as_mut())?,
            ExprKind::Cast(cast) => self.rewrite_in_expr(cast.expr.as_mut())?,
            ExprKind::Index(index) => {
                self.rewrite_in_expr(index.obj.as_mut())?;
                self.rewrite_in_expr(index.index.as_mut())?;
            }
            ExprKind::BinOp(binop) => {
                self.rewrite_in_expr(binop.lhs.as_mut())?;
                self.rewrite_in_expr(binop.rhs.as_mut())?;
            }
            ExprKind::UnOp(unop) => self.rewrite_in_expr(unop.val.as_mut())?,
            ExprKind::Range(range) => {
                if let Some(start) = range.start.as_mut() {
                    self.rewrite_in_expr(start.as_mut())?;
                }
                if let Some(end) = range.end.as_mut() {
                    self.rewrite_in_expr(end.as_mut())?;
                }
                if let Some(step) = range.step.as_mut() {
                    self.rewrite_in_expr(step.as_mut())?;
                }
            }
            ExprKind::FormatString(format) => {
                for arg in &mut format.args {
                    self.rewrite_in_expr(arg)?;
                }
                for kwarg in &mut format.kwargs {
                    self.rewrite_in_expr(&mut kwarg.value)?;
                }
            }
            ExprKind::Try(expr_try) => self.rewrite_in_expr(expr_try.expr.as_mut())?,
            ExprKind::Value(value) => match value.as_mut() {
                Value::Expr(expr) => self.rewrite_in_expr(expr.as_mut())?,
                Value::Function(func) => self.rewrite_in_expr(func.body.as_mut())?,
                _ => {}
            },
            ExprKind::Splat(splat) => self.rewrite_in_expr(splat.iter.as_mut())?,
            ExprKind::SplatDict(dict) => self.rewrite_in_expr(dict.dict.as_mut())?,
            ExprKind::Item(item) => self.rewrite_in_item(item.as_mut())?,
            ExprKind::IntrinsicCall(call) => match &mut call.payload {
                fp_core::intrinsics::IntrinsicCallPayload::Args { args } => {
                    for arg in args {
                        self.rewrite_in_expr(arg)?;
                    }
                }
                fp_core::intrinsics::IntrinsicCallPayload::Format { template } => {
                    for arg in &mut template.args {
                        self.rewrite_in_expr(arg)?;
                    }
                    for kwarg in &mut template.kwargs {
                        self.rewrite_in_expr(&mut kwarg.value)?;
                    }
                }
            },
            ExprKind::Paren(paren) => self.rewrite_in_expr(paren.expr.as_mut())?,
            ExprKind::IntrinsicCollection(_) => {
                unreachable!("intrinsic collections should have been expanded")
            }
            ExprKind::Locator(_) | ExprKind::Closured(_) => {}
            ExprKind::Closure(_) | ExprKind::Any(_) | ExprKind::Id(_) => {}
        }
        Ok(())
    }
    fn rewrite_in_stmt(&mut self, stmt: &mut BlockStmt) -> Result<()> {
        match stmt {
            BlockStmt::Expr(expr_stmt) => self.rewrite_in_expr(expr_stmt.expr.as_mut())?,
            BlockStmt::Let(stmt_let) => {
                if let Some(init) = stmt_let.init.as_mut() {
                    self.rewrite_in_expr(init)?;
                    if let Some(info) = self.closure_info_from_expr(init) {
                        let mut names = Vec::new();
                        collect_pattern_idents(&stmt_let.pat, &mut names);
                        for name in names {
                            self.variable_infos.insert(name, info.clone());
                        }
                        stmt_let.pat.set_ty(info.env_struct_ty.clone());
                        init.set_ty(info.env_struct_ty.clone());
                    }
                }
                if let Some(diverge) = stmt_let.diverge.as_mut() {
                    self.rewrite_in_expr(diverge)?;
                }
            }
            BlockStmt::Item(item) => self.rewrite_in_item(item.as_mut())?,
            BlockStmt::Noop | BlockStmt::Any(_) => {}
        }
        Ok(())
    }
    fn rewrite_in_item(&mut self, item: &mut Item) -> Result<()> {
        match item.kind_mut() {
            ItemKind::Expr(expr) => self.rewrite_in_expr(expr)?,
            ItemKind::DefConst(def) => {
                self.rewrite_in_expr(def.value.as_mut())?;
                if let Some(info) = self.closure_info_from_expr(def.value.as_ref()) {
                    self.variable_infos
                        .insert(def.name.as_str().to_string(), info.clone());
                    def.ty = Some(info.env_struct_ty.clone());
                    def.ty_annotation = Some(info.env_struct_ty.clone());
                    def.value.set_ty(info.env_struct_ty.clone());
                }
            }
            ItemKind::DefStatic(def) => {
                self.rewrite_in_expr(def.value.as_mut())?;
                if let Some(info) = self.closure_info_from_expr(def.value.as_ref()) {
                    self.variable_infos
                        .insert(def.name.as_str().to_string(), info.clone());
                    def.ty = info.env_struct_ty.clone();
                    def.ty_annotation = Some(info.env_struct_ty.clone());
                    def.value.set_ty(info.env_struct_ty.clone());
                }
            }
            ItemKind::DefFunction(func) => self.rewrite_in_expr(func.body.as_mut())?,
            ItemKind::Module(module) => self.rewrite_usage(&mut module.items)?,
            _ => {}
        }
        Ok(())
    }

    fn closure_info_from_expr(&self, expr: &Expr) -> Option<ClosureInfo> {
        match expr.kind() {
            ExprKind::Struct(struct_expr) => extract_ident(struct_expr.name.as_ref())
                .and_then(|ident| self.struct_infos.get(ident.as_str()).cloned()),
            ExprKind::Invoke(invoke) => {
                if let ExprInvokeTarget::Function(locator) = &invoke.target {
                    locator
                        .as_ident()
                        .and_then(|ident| self.function_infos.get(ident.as_str()).cloned())
                } else {
                    None
                }
            }
            ExprKind::Locator(locator) => locator
                .as_ident()
                .and_then(|ident| self.variable_infos.get(ident.as_str()).cloned()),
            ExprKind::Paren(paren) => self.closure_info_from_expr(paren.expr.as_ref()),
            _ => None,
        }
    }

    fn collect_captures(&self, expr: &Expr, params: &HashSet<String>) -> Result<Vec<Capture>> {
        let mut collector = CaptureCollector::new(params.clone());
        collector.visit(expr);
        Ok(collector.into_captures())
    }

    fn rewrite_captured_usage(
        &self,
        expr: &mut Expr,
        captures: &[Capture],
        env_ident: &Ident,
        env_ty: &Ty,
    ) {
        let mut replacer = CaptureReplacer::new(captures, env_ident.clone(), env_ty.clone());
        replacer.visit(expr);
    }
}

struct CaptureCollector {
    scope: Vec<HashSet<String>>,
    captures: Vec<(String, Ty)>,
    seen: HashSet<String>,
}

impl CaptureCollector {
    fn new(params: HashSet<String>) -> Self {
        Self {
            scope: vec![params],
            captures: Vec::new(),
            seen: HashSet::new(),
        }
    }

    fn visit(&mut self, expr: &Expr) {
        match expr.kind() {
            ExprKind::Closure(_) | ExprKind::Closured(_) => {}
            ExprKind::IntrinsicCollection(collection) => {
                let expanded = collection.clone().into_const_expr();
                self.visit(&expanded);
            }
            ExprKind::Block(block) => {
                self.scope.push(HashSet::new());
                for stmt in &block.stmts {
                    self.visit_stmt(stmt);
                }
                if let Some(last) = block.last_expr() {
                    self.visit(last);
                }
                self.scope.pop();
            }
            ExprKind::Let(expr_let) => {
                self.visit(expr_let.expr.as_ref());
                let mut names = Vec::new();
                collect_pattern_idents(&expr_let.pat, &mut names);
                if let Some(scope) = self.scope.last_mut() {
                    for name in names {
                        scope.insert(name);
                    }
                }
            }
            ExprKind::Invoke(invoke) => {
                if let ExprInvokeTarget::Expr(target) = &invoke.target {
                    self.visit(target.as_ref());
                }
                for arg in &invoke.args {
                    self.visit(arg);
                }
            }
            ExprKind::Assign(assign) => {
                self.visit(assign.target.as_ref());
                self.visit(assign.value.as_ref());
            }
            ExprKind::BinOp(binop) => {
                self.visit(binop.lhs.as_ref());
                self.visit(binop.rhs.as_ref());
            }
            ExprKind::UnOp(unop) => self.visit(unop.val.as_ref()),
            ExprKind::Select(select) => self.visit(select.obj.as_ref()),
            ExprKind::Struct(struct_expr) => {
                self.visit(struct_expr.name.as_ref());
                for field in &struct_expr.fields {
                    if let Some(value) = field.value.as_ref() {
                        self.visit(value);
                    }
                }
            }
            ExprKind::Structural(struct_expr) => {
                for field in &struct_expr.fields {
                    if let Some(value) = field.value.as_ref() {
                        self.visit(value);
                    }
                }
            }
            ExprKind::Array(array) => {
                for value in &array.values {
                    self.visit(value);
                }
            }
            ExprKind::ArrayRepeat(array_repeat) => {
                self.visit(array_repeat.elem.as_ref());
                self.visit(array_repeat.len.as_ref());
            }
            ExprKind::Tuple(tuple) => {
                for value in &tuple.values {
                    self.visit(value);
                }
            }
            ExprKind::Reference(reference) => self.visit(reference.referee.as_ref()),
            ExprKind::Dereference(deref) => self.visit(deref.referee.as_ref()),
            ExprKind::Cast(cast) => self.visit(cast.expr.as_ref()),
            ExprKind::Index(index) => {
                self.visit(index.obj.as_ref());
                self.visit(index.index.as_ref());
            }
            ExprKind::If(expr_if) => {
                self.visit(expr_if.cond.as_ref());
                self.visit(expr_if.then.as_ref());
                if let Some(elze) = expr_if.elze.as_ref() {
                    self.visit(elze);
                }
            }
            ExprKind::Loop(expr_loop) => self.visit(expr_loop.body.as_ref()),
            ExprKind::While(expr_while) => {
                self.visit(expr_while.cond.as_ref());
                self.visit(expr_while.body.as_ref());
            }
            ExprKind::Match(expr_match) => {
                for case in &expr_match.cases {
                    self.visit(case.cond.as_ref());
                    self.visit(case.body.as_ref());
                }
            }
            ExprKind::FormatString(format) => {
                for arg in &format.args {
                    self.visit(arg);
                }
                for kwarg in &format.kwargs {
                    self.visit(&kwarg.value);
                }
            }
            ExprKind::Range(range) => {
                if let Some(start) = range.start.as_ref() {
                    self.visit(start.as_ref());
                }
                if let Some(end) = range.end.as_ref() {
                    self.visit(end.as_ref());
                }
                if let Some(step) = range.step.as_ref() {
                    self.visit(step.as_ref());
                }
            }
            ExprKind::Try(expr_try) => self.visit(expr_try.expr.as_ref()),
            ExprKind::Value(value) => match value.as_ref() {
                Value::Expr(expr) => self.visit(expr.as_ref()),
                Value::Function(func) => self.visit(func.body.as_ref()),
                _ => {}
            },
            ExprKind::Paren(paren) => self.visit(paren.expr.as_ref()),
            ExprKind::Locator(locator) => {
                if let Some(ident) = locator.as_ident() {
                    let name = ident.as_str();
                    if !self.is_in_scope(name) && !self.seen.contains(name) {
                        let ty = expr.ty().cloned().unwrap_or_else(|| Ty::Any(TypeAny));
                        self.seen.insert(name.to_string());
                        self.captures.push((name.to_string(), ty));
                    }
                }
            }
            ExprKind::Splat(splat) => self.visit(splat.iter.as_ref()),
            ExprKind::SplatDict(dict) => self.visit(dict.dict.as_ref()),
            ExprKind::Item(item) => self.visit_item(item.as_ref()),
            ExprKind::IntrinsicCall(call) => match &call.payload {
                fp_core::intrinsics::IntrinsicCallPayload::Args { args } => {
                    for arg in args {
                        self.visit(arg);
                    }
                }
                fp_core::intrinsics::IntrinsicCallPayload::Format { template } => {
                    for arg in &template.args {
                        self.visit(arg);
                    }
                    for kwarg in &template.kwargs {
                        self.visit(&kwarg.value);
                    }
                }
            },
            ExprKind::Any(_) | ExprKind::Id(_) => {}
        }
    }

    fn visit_stmt(&mut self, stmt: &BlockStmt) {
        match stmt {
            BlockStmt::Expr(expr_stmt) => self.visit(expr_stmt.expr.as_ref()),
            BlockStmt::Let(stmt_let) => {
                if let Some(init) = stmt_let.init.as_ref() {
                    self.visit(init);
                }
                let mut names = Vec::new();
                collect_pattern_idents(&stmt_let.pat, &mut names);
                if let Some(scope) = self.scope.last_mut() {
                    for name in names {
                        scope.insert(name);
                    }
                }
                if let Some(diverge) = stmt_let.diverge.as_ref() {
                    self.visit(diverge);
                }
            }
            BlockStmt::Item(item) => self.visit_item(item.as_ref()),
            BlockStmt::Noop | BlockStmt::Any(_) => {}
        }
    }

    fn visit_item(&mut self, item: &Item) {
        if let ItemKind::Expr(expr) = item.kind() {
            self.visit(expr);
        }
    }

    fn is_in_scope(&self, name: &str) -> bool {
        self.scope.iter().rev().any(|scope| scope.contains(name))
    }

    fn into_captures(self) -> Vec<Capture> {
        self.captures
            .into_iter()
            .map(|(name, ty)| Capture {
                name: Ident::new(name),
                ty,
            })
            .collect()
    }
}

struct CaptureReplacer {
    captures: HashMap<String, Ty>,
    env_ident: Ident,
    env_ty: Ty,
}

impl CaptureReplacer {
    fn new(info: &[Capture], env_ident: Ident, env_ty: Ty) -> Self {
        let mut captures = HashMap::new();
        for capture in info {
            captures.insert(capture.name.as_str().to_string(), capture.ty.clone());
        }
        Self {
            captures,
            env_ident,
            env_ty,
        }
    }

    fn visit(&mut self, expr: &mut Expr) {
        if expand_intrinsic_collection(expr) {
            self.visit(expr);
            return;
        }

        match expr.kind_mut() {
            ExprKind::Block(block) => {
                for stmt in &mut block.stmts {
                    self.visit_stmt(stmt);
                }
                if let Some(last) = block.last_expr_mut() {
                    self.visit(last);
                }
            }
            ExprKind::Let(expr_let) => self.visit(expr_let.expr.as_mut()),
            ExprKind::Invoke(invoke) => {
                if let ExprInvokeTarget::Expr(target) = &mut invoke.target {
                    self.visit(target.as_mut());
                }
                for arg in &mut invoke.args {
                    self.visit(arg);
                }
            }
            ExprKind::Select(select) => self.visit(select.obj.as_mut()),
            ExprKind::Assign(assign) => {
                self.visit(assign.target.as_mut());
                self.visit(assign.value.as_mut());
            }
            ExprKind::BinOp(binop) => {
                self.visit(binop.lhs.as_mut());
                self.visit(binop.rhs.as_mut());
            }
            ExprKind::UnOp(unop) => self.visit(unop.val.as_mut()),
            ExprKind::Struct(struct_expr) => {
                self.visit(struct_expr.name.as_mut());
                for field in &mut struct_expr.fields {
                    if let Some(value) = field.value.as_mut() {
                        self.visit(value);
                    }
                }
            }
            ExprKind::Structural(struct_expr) => {
                for field in &mut struct_expr.fields {
                    if let Some(value) = field.value.as_mut() {
                        self.visit(value);
                    }
                }
            }
            ExprKind::Array(array) => {
                for value in &mut array.values {
                    self.visit(value);
                }
            }
            ExprKind::ArrayRepeat(array_repeat) => {
                self.visit(array_repeat.elem.as_mut());
                self.visit(array_repeat.len.as_mut());
            }
            ExprKind::Tuple(tuple) => {
                for value in &mut tuple.values {
                    self.visit(value);
                }
            }
            ExprKind::Reference(reference) => self.visit(reference.referee.as_mut()),
            ExprKind::Dereference(deref) => self.visit(deref.referee.as_mut()),
            ExprKind::Cast(cast) => self.visit(cast.expr.as_mut()),
            ExprKind::Index(index) => {
                self.visit(index.obj.as_mut());
                self.visit(index.index.as_mut());
            }
            ExprKind::If(expr_if) => {
                self.visit(expr_if.cond.as_mut());
                self.visit(expr_if.then.as_mut());
                if let Some(elze) = expr_if.elze.as_mut() {
                    self.visit(elze);
                }
            }
            ExprKind::Loop(expr_loop) => self.visit(expr_loop.body.as_mut()),
            ExprKind::While(expr_while) => {
                self.visit(expr_while.cond.as_mut());
                self.visit(expr_while.body.as_mut());
            }
            ExprKind::Match(expr_match) => {
                for case in &mut expr_match.cases {
                    self.visit(case.cond.as_mut());
                    self.visit(case.body.as_mut());
                }
            }
            ExprKind::FormatString(format) => {
                for arg in &mut format.args {
                    self.visit(arg);
                }
                for kwarg in &mut format.kwargs {
                    self.visit(&mut kwarg.value);
                }
            }
            ExprKind::Range(range) => {
                if let Some(start) = range.start.as_mut() {
                    self.visit(start.as_mut());
                }
                if let Some(end) = range.end.as_mut() {
                    self.visit(end.as_mut());
                }
                if let Some(step) = range.step.as_mut() {
                    self.visit(step.as_mut());
                }
            }
            ExprKind::Try(expr_try) => self.visit(expr_try.expr.as_mut()),
            ExprKind::Value(value) => match value.as_mut() {
                Value::Expr(expr) => self.visit(expr.as_mut()),
                Value::Function(func) => self.visit(func.body.as_mut()),
                _ => {}
            },
            ExprKind::IntrinsicCall(call) => match &mut call.payload {
                fp_core::intrinsics::IntrinsicCallPayload::Args { args } => {
                    for arg in args {
                        self.visit(arg);
                    }
                }
                fp_core::intrinsics::IntrinsicCallPayload::Format { template } => {
                    for arg in &mut template.args {
                        self.visit(arg);
                    }
                    for kwarg in &mut template.kwargs {
                        self.visit(&mut kwarg.value);
                    }
                }
            },
            ExprKind::Paren(paren) => self.visit(paren.expr.as_mut()),
            ExprKind::IntrinsicCollection(_) => {
                unreachable!("intrinsic collections should have been expanded")
            }
            ExprKind::Locator(locator) => {
                if let Some(ident) = locator.as_ident() {
                    if let Some(field_ty) = self.captures.get(ident.as_str()) {
                        let mut env_expr = Expr::ident(self.env_ident.clone());
                        env_expr.set_ty(self.env_ty.clone());
                        let mut select_expr = Expr::new(ExprKind::Select(ExprSelect {
                            obj: env_expr.into(),
                            field: Ident::new(ident.as_str()),
                            select: ExprSelectType::Field,
                        }));
                        select_expr.set_ty(field_ty.clone());
                        *expr = select_expr;
                    }
                }
            }
            ExprKind::Splat(splat) => self.visit(splat.iter.as_mut()),
            ExprKind::SplatDict(dict) => self.visit(dict.dict.as_mut()),
            ExprKind::Item(item) => self.visit_item(item.as_mut()),
            ExprKind::Closure(_) | ExprKind::Closured(_) | ExprKind::Any(_) | ExprKind::Id(_) => {}
        }
    }

    fn visit_stmt(&mut self, stmt: &mut BlockStmt) {
        match stmt {
            BlockStmt::Expr(expr_stmt) => self.visit(expr_stmt.expr.as_mut()),
            BlockStmt::Let(stmt_let) => {
                if let Some(init) = stmt_let.init.as_mut() {
                    self.visit(init);
                }
                if let Some(diverge) = stmt_let.diverge.as_mut() {
                    self.visit(diverge);
                }
            }
            BlockStmt::Item(item) => self.visit_item(item.as_mut()),
            BlockStmt::Noop | BlockStmt::Any(_) => {}
        }
    }

    fn visit_item(&mut self, item: &mut Item) {
        if let ItemKind::Expr(expr) = item.kind_mut() {
            self.visit(expr);
        }
    }
}

fn collect_pattern_idents(pattern: &Pattern, out: &mut Vec<String>) {
    match pattern.kind() {
        PatternKind::Ident(ident) => out.push(ident.ident.name.as_str().to_string()),
        PatternKind::Tuple(tuple) => {
            for pat in &tuple.patterns {
                collect_pattern_idents(pat, out);
            }
        }
        PatternKind::Struct(struct_pat) => {
            for field in &struct_pat.fields {
                out.push(field.name.as_str().to_string());
            }
        }
        PatternKind::Type(pattern_type) => collect_pattern_idents(pattern_type.pat.as_ref(), out),
        PatternKind::TupleStruct(tuple_struct) => {
            for pat in &tuple_struct.patterns {
                collect_pattern_idents(pat, out);
            }
        }
        PatternKind::Structural(structural) => {
            for field in &structural.fields {
                out.push(field.name.as_str().to_string());
            }
        }
        PatternKind::Variant(variant) => {
            if let Some(pat) = variant.pattern.as_ref() {
                collect_pattern_idents(pat.as_ref(), out);
            }
        }
        PatternKind::Box(pattern_box) => collect_pattern_idents(pattern_box.pattern.as_ref(), out),
        PatternKind::Wildcard(_) => {}
    }
}

fn extract_ident(expr: &Expr) -> Option<Ident> {
    match expr.kind() {
        ExprKind::Locator(locator) => locator.as_ident().cloned(),
        ExprKind::Paren(paren) => extract_ident(paren.expr.as_ref()),
        _ => None,
    }
}
