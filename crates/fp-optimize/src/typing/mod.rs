use std::collections::{HashMap, HashSet};

use crate::error::optimization_error;
use fp_core::ast::*;
use fp_core::error::Result;
use fp_core::id::{Ident, Locator};
use fp_core::intrinsics::{IntrinsicCallKind, IntrinsicCallPayload};
use fp_core::ops::{BinOpKind, UnOpKind};
use fp_core::pat::{Pattern, PatternKind};

type TypeVarId = usize;

#[derive(Clone, Debug)]
struct TypeVar {
    kind: TypeVarKind,
}

#[derive(Clone, Debug)]
enum TypeVarKind {
    Unbound { level: usize },
    Link(TypeVarId),
    Bound(TypeTerm),
}

#[derive(Clone, Debug)]
struct FunctionTerm {
    params: Vec<TypeVarId>,
    ret: TypeVarId,
}

#[derive(Clone, Debug)]
enum TypeTerm {
    Primitive(TypePrimitive),
    Unit,
    Nothing,
    Tuple(Vec<TypeVarId>),
    Function(FunctionTerm),
    Struct(TypeStruct),
    Structural(TypeStructural),
    Enum(TypeEnum),
    Slice(TypeVarId),
    Vec(TypeVarId),
    Reference(TypeVarId),
    Any,
    Custom(Ty),
    Unknown,
}

#[derive(Clone, Debug)]
struct TypeScheme {
    #[allow(dead_code)]
    vars: usize,
    body: SchemeType,
}

#[derive(Clone, Debug)]
struct MethodRecord {
    receiver_ty: Option<Ty>,
    scheme: Option<TypeScheme>,
}

#[derive(Clone, Debug)]
struct ImplContext {
    struct_name: String,
    self_ty: Ty,
}

#[derive(Clone, Debug)]
enum SchemeType {
    Var(u32),
    Primitive(TypePrimitive),
    Unit,
    Nothing,
    Tuple(Vec<SchemeType>),
    Function(Vec<SchemeType>, Box<SchemeType>),
    Struct(TypeStruct),
    Structural(TypeStructural),
    Enum(TypeEnum),
    Slice(Box<SchemeType>),
    Vec(Box<SchemeType>),
    Reference(Box<SchemeType>),
    Any,
    Custom(Ty),
    Unknown,
}

#[derive(Clone)]
enum EnvEntry {
    Mono(TypeVarId),
    Poly(TypeScheme),
}

struct PatternBinding {
    name: String,
    var: TypeVarId,
}

struct PatternInfo {
    var: TypeVarId,
    bindings: Vec<PatternBinding>,
}

impl PatternInfo {
    fn new(var: TypeVarId) -> Self {
        Self {
            var,
            bindings: Vec::new(),
        }
    }

    fn with_binding(mut self, name: String, var: TypeVarId) -> Self {
        self.bindings.push(PatternBinding { name, var });
        self
    }

    #[allow(dead_code)]
    fn extend_bindings(&mut self, other: PatternInfo) {
        self.bindings.extend(other.bindings);
    }
}

#[derive(Clone, Copy)]
pub enum TypingDiagnosticLevel {
    Error,
    Warning,
}

pub struct TypingDiagnostic {
    pub level: TypingDiagnosticLevel,
    pub message: String,
}

impl TypingDiagnostic {
    pub fn error(message: impl Into<String>) -> Self {
        Self {
            level: TypingDiagnosticLevel::Error,
            message: message.into(),
        }
    }

    pub fn warning(message: impl Into<String>) -> Self {
        Self {
            level: TypingDiagnosticLevel::Warning,
            message: message.into(),
        }
    }
}

pub struct TypingOutcome {
    pub diagnostics: Vec<TypingDiagnostic>,
    pub has_errors: bool,
}

struct FunctionTypeInfo {
    params: Vec<TypeVarId>,
    ret: TypeVarId,
}

pub struct AstTypeInferencer {
    type_vars: Vec<TypeVar>,
    env: Vec<HashMap<String, EnvEntry>>,
    struct_defs: HashMap<String, TypeStruct>,
    struct_methods: HashMap<String, HashMap<String, MethodRecord>>,
    impl_stack: Vec<Option<ImplContext>>,
    current_level: usize,
    diagnostics: Vec<TypingDiagnostic>,
    has_errors: bool,
    literal_ints: HashSet<TypeVarId>,
}

impl AstTypeInferencer {
    pub fn new() -> Self {
        Self {
            type_vars: Vec::new(),
            env: vec![HashMap::new()],
            struct_defs: HashMap::new(),
            struct_methods: HashMap::new(),
            impl_stack: Vec::new(),
            current_level: 0,
            diagnostics: Vec::new(),
            has_errors: false,
            literal_ints: HashSet::new(),
        }
    }

    pub fn infer(&mut self, node: &mut Node) -> Result<TypingOutcome> {
        match node.kind_mut() {
            NodeKind::Expr(expr) => {
                let var = self.infer_expr(expr)?;
                let ty = self.resolve_to_ty(var)?;
                node.set_ty(ty);
            }
            NodeKind::Item(item) => {
                self.predeclare_item(item);
                self.infer_item(item)?;
                let ty = item.ty().cloned().unwrap_or_else(|| Ty::Unit(TypeUnit));
                node.set_ty(ty);
            }
            NodeKind::File(file) => {
                for item in &file.items {
                    self.predeclare_item(item);
                }
                for item in &mut file.items {
                    self.infer_item(item)?;
                }
                node.set_ty(Ty::Unit(TypeUnit));
            }
        }
        Ok(self.finish())
    }

    fn finish(&mut self) -> TypingOutcome {
        TypingOutcome {
            diagnostics: std::mem::take(&mut self.diagnostics),
            has_errors: std::mem::replace(&mut self.has_errors, false),
        }
    }

    fn resolve_impl_context(&mut self, self_ty: &Expr) -> Option<ImplContext> {
        match self.struct_name_from_expr(self_ty) {
            Some(name) => {
                if let Some(def) = self.struct_defs.get(&name).cloned() {
                    Some(ImplContext {
                        struct_name: name,
                        self_ty: Ty::Struct(def),
                    })
                } else {
                    self.emit_error(format!("impl target {} is not a known struct", name));
                    None
                }
            }
            None => {
                self.emit_error("impl self type must resolve to a struct");
                None
            }
        }
    }

    fn ty_for_receiver(&self, ctx: &ImplContext, receiver: &FunctionParamReceiver) -> Ty {
        match receiver {
            FunctionParamReceiver::Implicit
            | FunctionParamReceiver::Value
            | FunctionParamReceiver::MutValue => ctx.self_ty.clone(),
            FunctionParamReceiver::Ref | FunctionParamReceiver::RefStatic => Ty::Reference(
                TypeReference {
                    ty: Box::new(ctx.self_ty.clone()),
                    mutability: Some(false),
                    lifetime: None,
                }
                .into(),
            ),
            FunctionParamReceiver::RefMut | FunctionParamReceiver::RefMutStatic => Ty::Reference(
                TypeReference {
                    ty: Box::new(ctx.self_ty.clone()),
                    mutability: Some(true),
                    lifetime: None,
                }
                .into(),
            ),
        }
    }

    fn register_method_stub(&mut self, ctx: &ImplContext, func: &ItemDefFunction) {
        let receiver_ty = func
            .sig
            .receiver
            .as_ref()
            .map(|receiver| self.ty_for_receiver(ctx, receiver));
        let entry = self
            .struct_methods
            .entry(ctx.struct_name.clone())
            .or_default();
        entry
            .entry(func.name.as_str().to_string())
            .or_insert(MethodRecord {
                receiver_ty,
                scheme: None,
            });
    }

    fn peel_reference(mut ty: Ty) -> Ty {
        loop {
            match ty {
                Ty::Reference(reference) => {
                    ty = (*reference.ty).clone();
                }
                other => return other,
            }
        }
    }

    fn predeclare_item(&mut self, item: &Item) {
        match item.kind() {
            ItemKind::DefStruct(def) => {
                self.struct_defs
                    .insert(def.name.as_str().to_string(), def.value.clone());
                self.register_symbol(&def.name);
            }
            ItemKind::DefConst(def) => {
                self.register_symbol(&def.name);
            }
            ItemKind::DefStatic(def) => {
                self.register_symbol(&def.name);
                if let Ty::Struct(ty) = &def.ty {
                    self.struct_defs
                        .insert(ty.name.as_str().to_string(), ty.clone());
                }
            }
            ItemKind::DefFunction(def) => {
                self.register_symbol(&def.name);
            }
            ItemKind::DeclFunction(decl) => {
                self.register_symbol(&decl.name);
            }
            ItemKind::Module(module) => {
                self.enter_scope();
                for child in &module.items {
                    self.predeclare_item(child);
                }
                self.exit_scope();
            }
            ItemKind::Impl(impl_block) => {
                let ctx = self.resolve_impl_context(&impl_block.self_ty);
                self.impl_stack.push(ctx.clone());
                self.enter_scope();
                if let Some(ref ctx) = ctx {
                    for child in &impl_block.items {
                        if let ItemKind::DefFunction(func) = child.kind() {
                            self.register_method_stub(ctx, func);
                        }
                    }
                }
                for child in &impl_block.items {
                    self.predeclare_item(child);
                }
                self.exit_scope();
                self.impl_stack.pop();
            }
            ItemKind::Expr(expr) => {
                if let ExprKind::Struct(struct_expr) = expr.kind() {
                    if let Some(name) = self.struct_name_from_expr(&struct_expr.name) {
                        if let Some(def) = self.struct_defs.get(&name).cloned() {
                            self.struct_defs.insert(name, def);
                        }
                    }
                }
            }
            _ => {}
        }
    }

    fn infer_item(&mut self, item: &mut Item) -> Result<()> {
        let ty = match item.kind_mut() {
            ItemKind::DefStruct(def) => {
                let ty = Ty::Struct(def.value.clone());
                let placeholder = self.symbol_var(&def.name);
                let var = self.type_from_ast_ty(&ty)?;
                self.unify(placeholder, var)?;
                self.generalize_symbol(def.name.as_str(), placeholder)?;
                ty
            }
            ItemKind::DefConst(def) => {
                let placeholder = self.symbol_var(&def.name);
                let expr_var = {
                    let mut value = def.value.as_mut();
                    self.infer_expr(&mut value)?
                };

                if let Some(annot) = &def.ty {
                    let annot_var = self.type_from_ast_ty(annot)?;
                    self.unify(expr_var, annot_var)?;
                }

                self.unify(placeholder, expr_var)?;
                let ty = self.resolve_to_ty(expr_var)?;
                def.ty_annotation = Some(ty.clone());
                def.ty.get_or_insert(ty.clone());
                self.generalize_symbol(def.name.as_str(), placeholder)?;
                ty
            }
            ItemKind::DefStatic(def) => {
                let placeholder = self.symbol_var(&def.name);
                let expr_var = {
                    let mut value = def.value.as_mut();
                    self.infer_expr(&mut value)?
                };
                let ty_var = self.type_from_ast_ty(&def.ty)?;
                self.unify(expr_var, ty_var)?;
                self.unify(placeholder, expr_var)?;
                let ty = self.resolve_to_ty(expr_var)?;
                def.ty_annotation = Some(ty.clone());
                self.generalize_symbol(def.name.as_str(), placeholder)?;
                ty
            }
            ItemKind::DefFunction(func) => self.infer_function(func)?,
            ItemKind::DeclConst(decl) => {
                let ty = decl.ty.clone();
                decl.ty_annotation = Some(ty.clone());
                ty
            }
            ItemKind::DeclStatic(decl) => {
                let ty = decl.ty.clone();
                decl.ty_annotation = Some(ty.clone());
                ty
            }
            ItemKind::DeclType(decl) => {
                let ty = Ty::TypeBounds(decl.bounds.clone());
                decl.ty_annotation = Some(ty.clone());
                ty
            }
            ItemKind::DeclFunction(decl) => {
                let ty = self.ty_from_function_signature(&decl.sig)?;
                decl.ty_annotation = Some(ty.clone());
                ty
            }
            ItemKind::Module(module) => {
                self.enter_scope();
                for child in &module.items {
                    self.predeclare_item(child);
                }
                for child in &mut module.items {
                    self.infer_item(child)?;
                }
                self.exit_scope();
                Ty::Unit(TypeUnit)
            }
            ItemKind::Impl(impl_block) => {
                let ctx = self.resolve_impl_context(&impl_block.self_ty);
                self.impl_stack.push(ctx.clone());
                self.enter_scope();
                for child in &impl_block.items {
                    self.predeclare_item(child);
                }
                for child in &mut impl_block.items {
                    self.infer_item(child)?;
                }
                self.exit_scope();
                self.impl_stack.pop();
                Ty::Unit(TypeUnit)
            }
            ItemKind::Expr(expr) => {
                let var = self.infer_expr(expr)?;
                self.resolve_to_ty(var)?
            }
            _ => {
                self.emit_error("type inference for item not implemented");
                Ty::Unknown(TypeUnknown)
            }
        };

        item.set_ty(ty);
        Ok(())
    }

    fn infer_function(&mut self, func: &mut ItemDefFunction) -> Result<Ty> {
        let fn_var = self.symbol_var(&func.name);
        self.enter_scope();

        let impl_ctx = self.impl_stack.last().cloned().flatten();
        let mut receiver_ty: Option<Ty> = None;
        if let Some(receiver) = func.sig.receiver.as_ref() {
            if let Some(ctx) = impl_ctx.as_ref() {
                let receiver_type = self.ty_for_receiver(ctx, receiver);
                let self_var = self.fresh_type_var();
                let expected = self.type_from_ast_ty(&receiver_type)?;
                self.unify(self_var, expected)?;
                self.insert_env("self".to_string(), EnvEntry::Mono(self_var));
                receiver_ty = Some(receiver_type);
            } else {
                self.emit_error(format!(
                    "method {} defined without an impl context",
                    func.name
                ));
            }
        }

        if !func.sig.generics_params.is_empty() {
            self.emit_error(format!(
                "generic function {} is not yet supported by the AST type inferencer",
                func.name
            ));
        }

        let mut param_vars = Vec::new();
        for param in &mut func.sig.params {
            let var = self.fresh_type_var();
            let annot_var = self.type_from_ast_ty(&param.ty)?;
            self.unify(var, annot_var)?;
            self.insert_env(param.name.as_str().to_string(), EnvEntry::Mono(var));
            let resolved = self.resolve_to_ty(var)?;
            param.ty_annotation = Some(resolved);
            param_vars.push(var);
        }

        let body_var = {
            let mut body = func.body.as_mut();
            self.infer_expr(&mut body)?
        };

        let ret_var = if let Some(ret) = &func.sig.ret_ty {
            let annot_var = self.type_from_ast_ty(ret)?;
            self.unify(body_var, annot_var)?;
            annot_var
        } else {
            body_var
        };

        let ret_ty = self.resolve_to_ty(ret_var.clone())?;
        func.sig.ret_ty.get_or_insert(ret_ty.clone());

        self.exit_scope();

        let mut param_tys = Vec::new();
        for var in &param_vars {
            param_tys.push(self.resolve_to_ty(*var)?);
        }

        self.bind(
            fn_var,
            TypeTerm::Function(FunctionTerm {
                params: param_vars.clone(),
                ret: ret_var,
            }),
        );

        let scheme = self.generalize(fn_var)?;
        let scheme_env = scheme.clone();
        self.replace_env_entry(func.name.as_str(), EnvEntry::Poly(scheme_env));

        if let Some(ctx) = impl_ctx.as_ref() {
            let entry = self
                .struct_methods
                .entry(ctx.struct_name.clone())
                .or_default();
            let record = entry
                .entry(func.name.as_str().to_string())
                .or_insert(MethodRecord {
                    receiver_ty: receiver_ty.clone(),
                    scheme: None,
                });
            if record.receiver_ty.is_none() && receiver_ty.is_some() {
                record.receiver_ty = receiver_ty.clone();
            }
            record.scheme = Some(scheme.clone());
        }

        let func_ty = TypeFunction {
            params: param_tys.clone(),
            generics_params: func.sig.generics_params.clone(),
            ret_ty: Some(Box::new(ret_ty.clone())),
        };

        func.ty = Some(func_ty.clone());
        let ty = Ty::Function(func_ty);
        func.ty_annotation = Some(ty.clone());
        Ok(ty)
    }

    fn infer_expr(&mut self, expr: &mut Expr) -> Result<TypeVarId> {
        let var = match expr.kind_mut() {
            ExprKind::Value(value) => self.infer_value(value)?,
            ExprKind::Locator(locator) => self.lookup_locator(locator)?,
            ExprKind::Block(block) => self.infer_block(block)?,
            ExprKind::If(if_expr) => self.infer_if(if_expr)?,
            ExprKind::BinOp(binop) => self.infer_binop(binop)?,
            ExprKind::UnOp(unop) => self.infer_unop(unop)?,
            ExprKind::Assign(assign) => {
                let target = self.infer_expr(assign.target.as_mut())?;
                let value = self.infer_expr(assign.value.as_mut())?;
                self.unify(target, value)?;
                value
            }
            ExprKind::Let(expr_let) => {
                let value = self.infer_expr(expr_let.expr.as_mut())?;
                let pattern_info = self.infer_pattern(expr_let.pat.as_mut())?;
                self.unify(pattern_info.var, value)?;
                self.apply_pattern_generalization(&pattern_info)?;
                value
            }
            ExprKind::Invoke(invoke) => self.infer_invoke(invoke)?,
            ExprKind::Select(select) => {
                let obj_var = self.infer_expr(select.obj.as_mut())?;
                self.lookup_struct_field(obj_var, &select.field)?
            }
            ExprKind::Struct(struct_expr) => self.resolve_struct_literal(struct_expr)?,
            ExprKind::Tuple(tuple) => {
                let mut element_vars = Vec::new();
                for expr in &mut tuple.values {
                    element_vars.push(self.infer_expr(expr)?);
                }
                let tuple_var = self.fresh_type_var();
                self.bind(tuple_var, TypeTerm::Tuple(element_vars));
                tuple_var
            }
            ExprKind::Array(array) => {
                let mut iter = array.values.iter_mut();
                let elem_var = if let Some(first) = iter.next() {
                    let first_var = self.infer_expr(first)?;
                    for value in iter {
                        let next = self.infer_expr(value)?;
                        self.unify(first_var, next)?;
                    }
                    first_var
                } else {
                    self.fresh_type_var()
                };
                let array_var = self.fresh_type_var();
                self.bind(array_var, TypeTerm::Vec(elem_var));
                array_var
            }
            ExprKind::ArrayRepeat(array_repeat) => {
                let elem_var = self.infer_expr(array_repeat.elem.as_mut())?;
                let len_var = self.infer_expr(array_repeat.len.as_mut())?;
                let expected_len = self.fresh_type_var();
                self.bind(
                    expected_len,
                    TypeTerm::Primitive(TypePrimitive::Int(TypeInt::U64)),
                );
                self.unify(len_var, expected_len)?;

                let elem_ty = self.resolve_to_ty(elem_var)?;
                let length_expr = array_repeat.len.as_ref().get();
                let array_ty = Ty::Array(TypeArray {
                    elem: Box::new(elem_ty.clone()),
                    len: length_expr.into(),
                });
                let array_var = self.fresh_type_var();
                self.bind(array_var, TypeTerm::Custom(array_ty.clone()));
                expr.set_ty(array_ty);
                array_var
            }
            ExprKind::Paren(paren) => self.infer_expr(paren.expr.as_mut())?,
            ExprKind::FormatString(_) => {
                let var = self.fresh_type_var();
                self.bind(var, TypeTerm::Primitive(TypePrimitive::String));
                var
            }
            ExprKind::Match(match_expr) => self.infer_match(match_expr)?,
            ExprKind::Loop(loop_expr) => self.infer_loop(loop_expr)?,
            ExprKind::While(while_expr) => self.infer_while(while_expr)?,
            ExprKind::Try(try_expr) => self.infer_expr(try_expr.expr.as_mut())?,
            ExprKind::Reference(reference) => self.infer_reference(reference)?,
            ExprKind::Dereference(dereference) => self.infer_dereference(dereference)?,
            ExprKind::Index(index) => self.infer_index(index)?,
            ExprKind::Closure(closure) => self.infer_closure(closure)?,
            ExprKind::IntrinsicCall(call) => self.infer_intrinsic(call)?,
            ExprKind::Range(range) => self.infer_range(range)?,
            ExprKind::Splat(splat) => self.infer_splat(splat)?,
            ExprKind::SplatDict(splat) => self.infer_splat_dict(splat)?,
            ExprKind::Any(_)
            | ExprKind::Item(_)
            | ExprKind::Closured(_)
            | ExprKind::Structural(_) => {
                self.emit_error("dynamic AST nodes are not yet supported by the type inferencer");
                self.error_type_var()
            }
            ExprKind::Id(_) => {
                self.emit_error("detached expression identifiers are not supported");
                self.error_type_var()
            }
        };

        let ty = self.resolve_to_ty(var)?;
        expr.set_ty(ty);
        Ok(var)
    }

    fn infer_block(&mut self, block: &mut ExprBlock) -> Result<TypeVarId> {
        self.enter_scope();
        let mut last = self.fresh_type_var();
        self.bind(last, TypeTerm::Unit);
        for stmt in &mut block.stmts {
            match stmt {
                BlockStmt::Item(item) => {
                    self.predeclare_item(item);
                    self.infer_item(item)?;
                    last = self.fresh_type_var();
                    self.bind(last, TypeTerm::Unit);
                }
                BlockStmt::Let(stmt_let) => {
                    let init_var = if let Some(init) = stmt_let.init.as_mut() {
                        self.infer_expr(init)?
                    } else {
                        let unit = self.fresh_type_var();
                        self.bind(unit, TypeTerm::Unit);
                        unit
                    };
                    let pattern_info = self.infer_pattern(&mut stmt_let.pat)?;
                    self.unify(pattern_info.var, init_var)?;
                    self.apply_pattern_generalization(&pattern_info)?;
                    last = self.fresh_type_var();
                    self.bind(last, TypeTerm::Unit);
                }
                BlockStmt::Expr(expr_stmt) => {
                    let expr_var = self.infer_expr(expr_stmt.expr.as_mut())?;
                    if expr_stmt.has_value() {
                        last = expr_var;
                    } else {
                        last = self.fresh_type_var();
                        self.bind(last, TypeTerm::Unit);
                    }
                }
                BlockStmt::Noop => {
                    last = self.fresh_type_var();
                    self.bind(last, TypeTerm::Unit);
                }
                BlockStmt::Any(_) => {
                    let message =
                        "custom block statements are not supported by type inference".to_string();
                    self.emit_error(message);
                    last = self.error_type_var();
                }
            }
        }
        self.exit_scope();
        Ok(last)
    }

    fn infer_if(&mut self, if_expr: &mut ExprIf) -> Result<TypeVarId> {
        let cond = self.infer_expr(if_expr.cond.as_mut())?;
        self.ensure_bool(cond, "if condition")?;
        let then_ty = self.infer_expr(if_expr.then.as_mut())?;
        if let Some(elze) = if_expr.elze.as_mut() {
            let else_ty = self.infer_expr(elze)?;
            self.unify(then_ty, else_ty)?;
        }
        Ok(then_ty)
    }

    fn infer_binop(&mut self, binop: &mut ExprBinOp) -> Result<TypeVarId> {
        let lhs = self.infer_expr(binop.lhs.as_mut())?;
        let rhs = self.infer_expr(binop.rhs.as_mut())?;
        match binop.kind {
            BinOpKind::Add | BinOpKind::Sub | BinOpKind::Mul | BinOpKind::Div => {
                self.ensure_numeric(lhs, "binary operand")?;
                self.unify(lhs, rhs)?;
                Ok(lhs)
            }
            BinOpKind::Eq
            | BinOpKind::Ne
            | BinOpKind::Lt
            | BinOpKind::Le
            | BinOpKind::Gt
            | BinOpKind::Ge => {
                self.unify(lhs, rhs)?;
                let bool_var = self.fresh_type_var();
                self.bind(bool_var, TypeTerm::Primitive(TypePrimitive::Bool));
                Ok(bool_var)
            }
            BinOpKind::And | BinOpKind::Or => {
                self.ensure_bool(lhs, "logical operand")?;
                self.ensure_bool(rhs, "logical operand")?;
                let bool_var = self.fresh_type_var();
                self.bind(bool_var, TypeTerm::Primitive(TypePrimitive::Bool));
                Ok(bool_var)
            }
            _ => Ok(lhs),
        }
    }

    fn infer_unop(&mut self, unop: &mut ExprUnOp) -> Result<TypeVarId> {
        let value_var = self.infer_expr(unop.val.as_mut())?;
        match unop.op {
            UnOpKind::Not => {
                self.ensure_bool(value_var, "unary not")?;
                Ok(value_var)
            }
            UnOpKind::Neg => {
                self.ensure_numeric(value_var, "unary negation")?;
                Ok(value_var)
            }
            UnOpKind::Deref | UnOpKind::Any(_) => {
                let message = "unsupported unary operator in type inference".to_string();
                self.emit_error(message.clone());
                Err(optimization_error(message))
            }
        }
    }

    fn infer_loop(&mut self, expr_loop: &mut ExprLoop) -> Result<TypeVarId> {
        let body_var = self.infer_expr(expr_loop.body.as_mut())?;
        let unit_var = self.fresh_type_var();
        self.bind(unit_var, TypeTerm::Unit);
        self.unify(body_var, unit_var)?;
        Ok(unit_var)
    }

    fn infer_while(&mut self, expr_while: &mut ExprWhile) -> Result<TypeVarId> {
        let cond_var = self.infer_expr(expr_while.cond.as_mut())?;
        self.ensure_bool(cond_var, "while condition")?;
        let body_var = self.infer_expr(expr_while.body.as_mut())?;
        let unit_var = self.fresh_type_var();
        self.bind(unit_var, TypeTerm::Unit);
        self.unify(body_var, unit_var)?;
        Ok(unit_var)
    }

    fn infer_reference(&mut self, reference: &mut ExprReference) -> Result<TypeVarId> {
        let inner_var = self.infer_expr(reference.referee.as_mut())?;
        let reference_var = self.fresh_type_var();
        self.bind(reference_var, TypeTerm::Reference(inner_var));
        Ok(reference_var)
    }

    fn infer_dereference(&mut self, dereference: &mut ExprDereference) -> Result<TypeVarId> {
        let target_var = self.infer_expr(dereference.referee.as_mut())?;
        self.expect_reference(target_var, "dereference expression")
    }

    fn infer_index(&mut self, index: &mut ExprIndex) -> Result<TypeVarId> {
        let object_var = self.infer_expr(index.obj.as_mut())?;
        let idx_var = self.infer_expr(index.index.as_mut())?;
        self.ensure_integer(idx_var, "index expression")?;

        let elem_vec_var = self.fresh_type_var();
        let vec_var = self.fresh_type_var();
        self.bind(vec_var, TypeTerm::Vec(elem_vec_var));
        if self.unify(object_var, vec_var).is_ok() {
            return Ok(elem_vec_var);
        }

        let elem_slice_var = self.fresh_type_var();
        let slice_var = self.fresh_type_var();
        self.bind(slice_var, TypeTerm::Slice(elem_slice_var));
        if self.unify(object_var, slice_var).is_err() {
            self.emit_error("indexing is only supported on vector or slice types");
            return Ok(self.error_type_var());
        }
        Ok(elem_slice_var)
    }

    fn infer_range(&mut self, range: &mut ExprRange) -> Result<TypeVarId> {
        let element_var = self.fresh_type_var();

        if let Some(start) = range.start.as_mut() {
            let start_var = self.infer_expr(start)?;
            self.unify(element_var, start_var)?;
        }

        if let Some(end) = range.end.as_mut() {
            let end_var = self.infer_expr(end)?;
            self.unify(element_var, end_var)?;
        }

        if let Some(step) = range.step.as_mut() {
            let step_var = self.infer_expr(step)?;
            self.ensure_numeric(step_var, "range step")?;
        }

        self.ensure_numeric(element_var, "range bounds")?;

        let range_var = self.fresh_type_var();
        self.bind(range_var, TypeTerm::Vec(element_var));
        Ok(range_var)
    }

    fn infer_splat(&mut self, splat: &mut ExprSplat) -> Result<TypeVarId> {
        self.infer_expr(splat.iter.as_mut())
    }

    fn infer_splat_dict(&mut self, splat: &mut ExprSplatDict) -> Result<TypeVarId> {
        self.infer_expr(splat.dict.as_mut())
    }

    fn infer_intrinsic(&mut self, call: &mut ExprIntrinsicCall) -> Result<TypeVarId> {
        let mut arg_vars = Vec::new();

        match &mut call.payload {
            IntrinsicCallPayload::Args { args } => {
                for arg in args {
                    arg_vars.push(self.infer_expr(arg)?);
                }
            }
            IntrinsicCallPayload::Format { template } => {
                for arg in &mut template.args {
                    arg_vars.push(self.infer_expr(arg)?);
                }
                for kwarg in &mut template.kwargs {
                    arg_vars.push(self.infer_expr(&mut kwarg.value)?);
                }
            }
        }

        match call.kind {
            IntrinsicCallKind::ConstBlock => {
                if let Some(&body_var) = arg_vars.first() {
                    return Ok(body_var);
                }
                self.emit_error("const block intrinsic expects a body expression");
                return Ok(self.error_type_var());
            }
            IntrinsicCallKind::Break | IntrinsicCallKind::Continue | IntrinsicCallKind::Return => {
                self.emit_error(format!(
                    "control-flow intrinsic {:?} must be lowered before typing",
                    call.kind
                ));
                return Ok(self.error_type_var());
            }
            _ => {}
        }

        let result_var = self.fresh_type_var();
        match call.kind {
            IntrinsicCallKind::Print | IntrinsicCallKind::Println => {
                self.bind(result_var, TypeTerm::Unit);
            }
            IntrinsicCallKind::Len
            | IntrinsicCallKind::SizeOf
            | IntrinsicCallKind::FieldCount
            | IntrinsicCallKind::MethodCount
            | IntrinsicCallKind::StructSize => {
                if arg_vars.len() != 1 {
                    self.emit_error(format!(
                        "intrinsic {:?} expects 1 argument, found {}",
                        call.kind,
                        arg_vars.len()
                    ));
                }
                self.bind(
                    result_var,
                    TypeTerm::Primitive(TypePrimitive::Int(TypeInt::U64)),
                );
            }
            IntrinsicCallKind::DebugAssertions
            | IntrinsicCallKind::HasField
            | IntrinsicCallKind::HasMethod => {
                let expected = if matches!(call.kind, IntrinsicCallKind::DebugAssertions) {
                    0
                } else {
                    2
                };
                if arg_vars.len() != expected {
                    self.emit_error(format!(
                        "intrinsic {:?} expects {} argument(s), found {}",
                        call.kind,
                        expected,
                        arg_vars.len()
                    ));
                }
                self.bind(result_var, TypeTerm::Primitive(TypePrimitive::Bool));
            }
            IntrinsicCallKind::Input => {
                if arg_vars.len() > 1 {
                    self.emit_error(format!(
                        "intrinsic {:?} expects at most 1 argument, found {}",
                        call.kind,
                        arg_vars.len()
                    ));
                }
                self.bind(result_var, TypeTerm::Primitive(TypePrimitive::String));
            }
            IntrinsicCallKind::TypeName => {
                if arg_vars.len() != 1 {
                    self.emit_error(format!(
                        "intrinsic {:?} expects 1 argument, found {}",
                        call.kind,
                        arg_vars.len()
                    ));
                }
                self.bind(result_var, TypeTerm::Primitive(TypePrimitive::String));
            }
            IntrinsicCallKind::ReflectFields => {
                if arg_vars.len() != 1 {
                    self.emit_error(format!(
                        "intrinsic {:?} expects 1 argument, found {}",
                        call.kind,
                        arg_vars.len()
                    ));
                }
                self.bind(result_var, TypeTerm::Any);
            }
            IntrinsicCallKind::CreateStruct
            | IntrinsicCallKind::CloneStruct
            | IntrinsicCallKind::AddField
            | IntrinsicCallKind::FieldType => {
                let expected = match call.kind {
                    IntrinsicCallKind::AddField => 3,
                    IntrinsicCallKind::FieldType => 2,
                    _ => 1,
                };
                if arg_vars.len() != expected {
                    self.emit_error(format!(
                        "intrinsic {:?} expects {} argument(s), found {}",
                        call.kind,
                        expected,
                        arg_vars.len()
                    ));
                }
                self.bind(result_var, TypeTerm::Custom(Ty::Type(TypeType)));
            }
            IntrinsicCallKind::GenerateMethod => {
                if arg_vars.len() != 2 {
                    self.emit_error(format!(
                        "intrinsic {:?} expects 2 arguments, found {}",
                        call.kind,
                        arg_vars.len()
                    ));
                }
                self.bind(result_var, TypeTerm::Unit);
            }
            IntrinsicCallKind::CompileError => {
                if arg_vars.len() != 1 {
                    self.emit_error(format!(
                        "intrinsic {:?} expects 1 argument, found {}",
                        call.kind,
                        arg_vars.len()
                    ));
                }
                self.bind(result_var, TypeTerm::Nothing);
            }
            IntrinsicCallKind::CompileWarning => {
                if arg_vars.len() != 1 {
                    self.emit_error(format!(
                        "intrinsic {:?} expects 1 argument, found {}",
                        call.kind,
                        arg_vars.len()
                    ));
                }
                self.bind(result_var, TypeTerm::Unit);
            }
            _ => {
                self.bind(result_var, TypeTerm::Any);
            }
        }

        Ok(result_var)
    }

    fn infer_closure(&mut self, closure: &mut ExprClosure) -> Result<TypeVarId> {
        self.enter_scope();
        let mut param_vars = Vec::new();
        for param in &mut closure.params {
            let info = self.infer_pattern(param)?;
            param_vars.push(info.var);
        }

        let body_var = self.infer_expr(closure.body.as_mut())?;
        let ret_var = if let Some(ret_ty) = &closure.ret_ty {
            let annot_var = self.type_from_ast_ty(ret_ty)?;
            self.unify(body_var, annot_var)?;
            annot_var
        } else {
            body_var
        };

        self.exit_scope();

        let closure_var = self.fresh_type_var();
        self.bind(
            closure_var,
            TypeTerm::Function(FunctionTerm {
                params: param_vars,
                ret: ret_var,
            }),
        );
        Ok(closure_var)
    }

    fn infer_match(&mut self, match_expr: &mut ExprMatch) -> Result<TypeVarId> {
        let mut result_var: Option<TypeVarId> = None;

        for case in &mut match_expr.cases {
            let cond_var = self.infer_expr(case.cond.as_mut())?;
            self.ensure_bool(cond_var, "match case condition")?;

            let body_var = self.infer_expr(case.body.as_mut())?;
            if let Some(existing) = result_var {
                self.unify(existing, body_var)?;
            } else {
                result_var = Some(body_var);
            }
        }

        match result_var {
            Some(var) => Ok(var),
            None => {
                self.emit_error("match expression requires at least one case");
                Ok(self.error_type_var())
            }
        }
    }

    fn infer_invoke(&mut self, invoke: &mut ExprInvoke) -> Result<TypeVarId> {
        if let ExprInvokeTarget::Function(locator) = &mut invoke.target {
            if let Some(ident) = locator.as_ident() {
                if ident.as_str() == "printf" {
                    return self.infer_builtin_printf(invoke);
                }
            }
        }

        let func_var = match &mut invoke.target {
            ExprInvokeTarget::Function(locator) => {
                if let Some(var) = self.lookup_associated_function(locator)? {
                    var
                } else {
                    self.lookup_locator(locator)?
                }
            }
            ExprInvokeTarget::Expr(expr) => self.infer_expr(expr.as_mut())?,
            ExprInvokeTarget::Closure(_) => {
                let message = "invoking closure values is not yet supported".to_string();
                self.emit_error(message.clone());
                return Ok(self.error_type_var());
            }
            ExprInvokeTarget::BinOp(_) => {
                let message = "invoking binary operators as functions is not supported".to_string();
                self.emit_error(message.clone());
                return Ok(self.error_type_var());
            }
            ExprInvokeTarget::Type(ty) => self.type_from_ast_ty(ty)?,
            ExprInvokeTarget::Method(select) => {
                let obj_var = self.infer_expr(select.obj.as_mut())?;
                if select.field.name.as_str() == "len" && invoke.args.is_empty() {
                    if let Ok(obj_ty) = self.resolve_to_ty(obj_var) {
                        if matches!(obj_ty, Ty::Array(_)) {
                            let result_var = self.fresh_type_var();
                            self.bind(
                                result_var,
                                TypeTerm::Primitive(TypePrimitive::Int(TypeInt::U64)),
                            );
                            return Ok(result_var);
                        }
                    }
                }
                self.lookup_struct_method(obj_var, &select.field)?
            }
        };

        let func_info = self.ensure_function(func_var, invoke.args.len())?;
        for (arg_expr, param_var) in invoke.args.iter_mut().zip(func_info.params.iter()) {
            let arg_var = self.infer_expr(arg_expr)?;
            self.unify(*param_var, arg_var)?;
        }
        Ok(func_info.ret)
    }

    fn infer_builtin_printf(&mut self, invoke: &mut ExprInvoke) -> Result<TypeVarId> {
        if invoke.args.is_empty() {
            self.emit_error("printf requires a format string argument");
            return Ok(self.error_type_var());
        }

        let format_var = self.infer_expr(&mut invoke.args[0])?;
        let expected_format = self.fresh_type_var();
        self.bind(expected_format, TypeTerm::Primitive(TypePrimitive::String));
        self.unify(format_var, expected_format)?;

        for arg in invoke.args.iter_mut().skip(1) {
            let _ = self.infer_expr(arg)?;
        }

        let result_var = self.fresh_type_var();
        self.bind(result_var, TypeTerm::Unit);
        Ok(result_var)
    }

    fn infer_value(&mut self, value: &Value) -> Result<TypeVarId> {
        let var = self.fresh_type_var();
        match value {
            Value::Int(_) => {
                self.literal_ints.insert(var);
                self.bind(var, TypeTerm::Primitive(TypePrimitive::Int(TypeInt::I64)));
            }
            Value::Bool(_) => self.bind(var, TypeTerm::Primitive(TypePrimitive::Bool)),
            Value::Decimal(_) => self.bind(
                var,
                TypeTerm::Primitive(TypePrimitive::Decimal(DecimalType::F64)),
            ),
            Value::String(_) => {
                let inner = self.fresh_type_var();
                self.bind(inner, TypeTerm::Primitive(TypePrimitive::String));
                self.bind(var, TypeTerm::Reference(inner));
            }
            Value::List(list) => {
                let elem_var = if let Some(first) = list.values.first() {
                    self.infer_value(first)?
                } else {
                    let fresh = self.fresh_type_var();
                    self.bind(fresh, TypeTerm::Any);
                    fresh
                };
                for value in list.values.iter().skip(1) {
                    let next_var = self.infer_value(value)?;
                    self.unify(elem_var, next_var)?;
                }
                let len = list.values.len() as i64;
                let elem_ty = self.resolve_to_ty(elem_var)?;
                let array_ty = Ty::Array(TypeArray {
                    elem: Box::new(elem_ty),
                    len: Expr::value(Value::int(len)).into(),
                });
                self.bind(var, TypeTerm::Custom(array_ty));
            }
            Value::Char(_) => self.bind(var, TypeTerm::Primitive(TypePrimitive::Char)),
            Value::Unit(_) => self.bind(var, TypeTerm::Unit),
            Value::Null(_) | Value::None(_) => self.bind(var, TypeTerm::Nothing),
            Value::Struct(struct_val) => {
                self.bind(var, TypeTerm::Struct(struct_val.ty.clone()));
            }
            Value::Structural(structural) => {
                let fields = structural
                    .fields
                    .iter()
                    .map(|field| StructuralField::new(field.name.clone(), Ty::Any(TypeAny)))
                    .collect();
                self.bind(var, TypeTerm::Structural(TypeStructural { fields }));
            }
            Value::Tuple(tuple) => {
                let mut vars = Vec::new();
                for elem in &tuple.values {
                    vars.push(self.infer_value(elem)?);
                }
                self.bind(var, TypeTerm::Tuple(vars));
            }
            Value::Function(func) => {
                let fn_ty = self.ty_from_function_signature(&func.sig)?;
                let fn_var = self.type_from_ast_ty(&fn_ty)?;
                self.unify(var, fn_var)?;
            }
            Value::Type(ty) => {
                let ty_var = self.type_from_ast_ty(ty)?;
                self.unify(var, ty_var)?;
            }
            Value::Expr(_) => {
                let message = "embedded expression values are not yet supported".to_string();
                self.emit_error(message.clone());
                return Ok(self.error_type_var());
            }
            _ => {
                let message = format!("value {:?} is not supported by type inference", value);
                self.emit_error(message.clone());
                return Ok(self.error_type_var());
            }
        }
        Ok(var)
    }

    fn infer_pattern(&mut self, pattern: &mut Pattern) -> Result<PatternInfo> {
        let info = match pattern.kind_mut() {
            PatternKind::Ident(ident) => {
                let var = self.fresh_type_var();
                self.insert_env(ident.ident.as_str().to_string(), EnvEntry::Mono(var));
                PatternInfo::new(var).with_binding(ident.ident.as_str().to_string(), var)
            }
            PatternKind::Type(inner) => {
                let inner_info = self.infer_pattern(inner.pat.as_mut())?;
                let annot_var = self.type_from_ast_ty(&inner.ty)?;
                self.unify(inner_info.var, annot_var)?;
                inner_info
            }
            PatternKind::Wildcard(_) => PatternInfo::new(self.fresh_type_var()),
            PatternKind::Tuple(tuple) => {
                let mut vars = Vec::new();
                let mut bindings = Vec::new();
                for pat in &mut tuple.patterns {
                    let child = self.infer_pattern(pat)?;
                    vars.push(child.var);
                    bindings.extend(child.bindings);
                }
                let tuple_var = self.fresh_type_var();
                self.bind(tuple_var, TypeTerm::Tuple(vars));
                PatternInfo {
                    var: tuple_var,
                    bindings,
                }
            }
            PatternKind::Struct(struct_pat) => {
                let struct_name = struct_pat.name.as_str().to_string();
                let struct_var = self.fresh_type_var();
                if let Some(struct_def) = self.struct_defs.get(&struct_name).cloned() {
                    self.bind(struct_var, TypeTerm::Struct(struct_def.clone()));
                    let mut bindings = Vec::new();
                    for field in &mut struct_pat.fields {
                        if let Some(rename) = field.rename.as_mut() {
                            let child = self.infer_pattern(rename)?;
                            bindings.extend(child.bindings);
                            if let Some(def_field) =
                                struct_def.fields.iter().find(|f| f.name == field.name)
                            {
                                let expected = self.type_from_ast_ty(&def_field.value)?;
                                self.unify(child.var, expected)?;
                            }
                        } else if let Some(def_field) =
                            struct_def.fields.iter().find(|f| f.name == field.name)
                        {
                            let var = self.fresh_type_var();
                            self.insert_env(field.name.as_str().to_string(), EnvEntry::Mono(var));
                            let expected = self.type_from_ast_ty(&def_field.value)?;
                            self.unify(var, expected)?;
                            bindings.push(PatternBinding {
                                name: field.name.as_str().to_string(),
                                var,
                            });
                        }
                    }
                    PatternInfo {
                        var: struct_var,
                        bindings,
                    }
                } else {
                    let message = format!("unknown struct pattern: {}", struct_name);
                    self.emit_error(message.clone());
                    PatternInfo::new(self.error_type_var())
                }
            }
            _ => {
                self.emit_error("pattern is not supported by type inference");
                PatternInfo::new(self.error_type_var())
            }
        };
        let ty = self.resolve_to_ty(info.var)?;
        pattern.set_ty(ty);
        Ok(info)
    }

    fn apply_pattern_generalization(&mut self, info: &PatternInfo) -> Result<()> {
        for binding in &info.bindings {
            let scheme = self.generalize(binding.var)?;
            self.replace_env_entry(&binding.name, EnvEntry::Poly(scheme));
        }
        Ok(())
    }

    fn generalize(&mut self, var: TypeVarId) -> Result<TypeScheme> {
        let mut mapping = HashMap::new();
        let mut next = 0u32;
        let body = self.build_scheme_type(var, &mut mapping, &mut next)?;
        Ok(TypeScheme {
            vars: next as usize,
            body,
        })
    }

    fn build_scheme_type(
        &mut self,
        var: TypeVarId,
        mapping: &mut HashMap<TypeVarId, u32>,
        next: &mut u32,
    ) -> Result<SchemeType> {
        let root = self.find(var);
        match self.type_vars[root].kind.clone() {
            TypeVarKind::Unbound { level } => {
                if level > self.current_level {
                    if let Some(idx) = mapping.get(&root) {
                        Ok(SchemeType::Var(*idx))
                    } else {
                        let idx = *next;
                        mapping.insert(root, idx);
                        *next += 1;
                        Ok(SchemeType::Var(idx))
                    }
                } else {
                    Ok(SchemeType::Unknown)
                }
            }
            TypeVarKind::Bound(term) => self.scheme_from_term(term, mapping, next),
            TypeVarKind::Link(next_var) => self.build_scheme_type(next_var, mapping, next),
        }
    }

    fn scheme_from_term(
        &mut self,
        term: TypeTerm,
        mapping: &mut HashMap<TypeVarId, u32>,
        next: &mut u32,
    ) -> Result<SchemeType> {
        Ok(match term {
            TypeTerm::Primitive(prim) => SchemeType::Primitive(prim),
            TypeTerm::Unit => SchemeType::Unit,
            TypeTerm::Nothing => SchemeType::Nothing,
            TypeTerm::Any => SchemeType::Any,
            TypeTerm::Struct(struct_ty) => SchemeType::Struct(struct_ty),
            TypeTerm::Structural(structural) => SchemeType::Structural(structural),
            TypeTerm::Enum(enum_ty) => SchemeType::Enum(enum_ty),
            TypeTerm::Custom(ty) => SchemeType::Custom(ty),
            TypeTerm::Unknown => SchemeType::Unknown,
            TypeTerm::Tuple(elements) => {
                let mut converted = Vec::new();
                for elem in elements {
                    converted.push(self.build_scheme_type(elem, mapping, next)?);
                }
                SchemeType::Tuple(converted)
            }
            TypeTerm::Function(function) => {
                let mut converted = Vec::new();
                for param in function.params {
                    converted.push(self.build_scheme_type(param, mapping, next)?);
                }
                let ret = self.build_scheme_type(function.ret, mapping, next)?;
                SchemeType::Function(converted, Box::new(ret))
            }
            TypeTerm::Slice(elem) => {
                let elem = self.build_scheme_type(elem, mapping, next)?;
                SchemeType::Slice(Box::new(elem))
            }
            TypeTerm::Vec(elem) => {
                let elem = self.build_scheme_type(elem, mapping, next)?;
                SchemeType::Vec(Box::new(elem))
            }
            TypeTerm::Reference(elem) => {
                let elem = self.build_scheme_type(elem, mapping, next)?;
                SchemeType::Reference(Box::new(elem))
            }
        })
    }

    fn instantiate_scheme(&mut self, scheme: &TypeScheme) -> TypeVarId {
        let mut mapping = HashMap::new();
        self.instantiate_scheme_type(&scheme.body, &mut mapping)
    }

    fn instantiate_scheme_type(
        &mut self,
        scheme: &SchemeType,
        mapping: &mut HashMap<u32, TypeVarId>,
    ) -> TypeVarId {
        match scheme {
            SchemeType::Var(idx) => {
                if let Some(var) = mapping.get(idx) {
                    *var
                } else {
                    let var = self.fresh_type_var();
                    mapping.insert(*idx, var);
                    var
                }
            }
            SchemeType::Primitive(prim) => {
                let var = self.fresh_type_var();
                self.bind(var, TypeTerm::Primitive(*prim));
                var
            }
            SchemeType::Unit => {
                let var = self.fresh_type_var();
                self.bind(var, TypeTerm::Unit);
                var
            }
            SchemeType::Nothing => {
                let var = self.fresh_type_var();
                self.bind(var, TypeTerm::Nothing);
                var
            }
            SchemeType::Any => {
                let var = self.fresh_type_var();
                self.bind(var, TypeTerm::Any);
                var
            }
            SchemeType::Struct(struct_ty) => {
                let var = self.fresh_type_var();
                self.bind(var, TypeTerm::Struct(struct_ty.clone()));
                var
            }
            SchemeType::Structural(structural) => {
                let var = self.fresh_type_var();
                self.bind(var, TypeTerm::Structural(structural.clone()));
                var
            }
            SchemeType::Enum(enum_ty) => {
                let var = self.fresh_type_var();
                self.bind(var, TypeTerm::Enum(enum_ty.clone()));
                var
            }
            SchemeType::Custom(ty) => {
                let var = self.fresh_type_var();
                self.bind(var, TypeTerm::Custom(ty.clone()));
                var
            }
            SchemeType::Unknown => self.fresh_type_var(),
            SchemeType::Tuple(elements) => {
                let mut vars = Vec::new();
                for elem in elements {
                    vars.push(self.instantiate_scheme_type(elem, mapping));
                }
                let var = self.fresh_type_var();
                self.bind(var, TypeTerm::Tuple(vars));
                var
            }
            SchemeType::Function(params, ret) => {
                let param_vars: Vec<_> = params
                    .iter()
                    .map(|param| self.instantiate_scheme_type(param, mapping))
                    .collect();
                let ret_var = self.instantiate_scheme_type(ret, mapping);
                let var = self.fresh_type_var();
                self.bind(
                    var,
                    TypeTerm::Function(FunctionTerm {
                        params: param_vars,
                        ret: ret_var,
                    }),
                );
                var
            }
            SchemeType::Slice(elem) => {
                let elem_var = self.instantiate_scheme_type(elem, mapping);
                let var = self.fresh_type_var();
                self.bind(var, TypeTerm::Slice(elem_var));
                var
            }
            SchemeType::Vec(elem) => {
                let elem_var = self.instantiate_scheme_type(elem, mapping);
                let var = self.fresh_type_var();
                self.bind(var, TypeTerm::Vec(elem_var));
                var
            }
            SchemeType::Reference(elem) => {
                let elem_var = self.instantiate_scheme_type(elem, mapping);
                let var = self.fresh_type_var();
                self.bind(var, TypeTerm::Reference(elem_var));
                var
            }
        }
    }

    fn insert_env(&mut self, name: String, entry: EnvEntry) {
        if let Some(scope) = self.env.last_mut() {
            scope.insert(name, entry);
        }
    }

    fn replace_env_entry(&mut self, name: &str, entry: EnvEntry) {
        for scope in self.env.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name.to_string(), entry);
                return;
            }
        }
        if let Some(scope) = self.env.last_mut() {
            scope.insert(name.to_string(), entry);
        }
    }

    fn enter_scope(&mut self) {
        self.current_level += 1;
        self.env.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.env.pop();
        if self.current_level > 0 {
            self.current_level -= 1;
        }
    }

    fn fresh_type_var(&mut self) -> TypeVarId {
        let id = self.type_vars.len();
        self.type_vars.push(TypeVar {
            kind: TypeVarKind::Unbound {
                level: self.current_level,
            },
        });
        id
    }

    fn bind(&mut self, var: TypeVarId, term: TypeTerm) {
        let root = self.find(var);
        self.type_vars[root].kind = TypeVarKind::Bound(term);
    }

    fn find(&mut self, var: TypeVarId) -> TypeVarId {
        match self.type_vars[var].kind.clone() {
            TypeVarKind::Link(next) => {
                let root = self.find(next);
                self.type_vars[var].kind = TypeVarKind::Link(root);
                root
            }
            _ => var,
        }
    }

    fn unify(&mut self, a: TypeVarId, b: TypeVarId) -> Result<()> {
        let a_root = self.find(a);
        let b_root = self.find(b);
        if a_root == b_root {
            return Ok(());
        }

        match (
            self.type_vars[a_root].kind.clone(),
            self.type_vars[b_root].kind.clone(),
        ) {
            (TypeVarKind::Unbound { .. }, TypeVarKind::Unbound { .. }) => {
                self.type_vars[a_root].kind = TypeVarKind::Link(b_root);
                Ok(())
            }
            (TypeVarKind::Unbound { .. }, TypeVarKind::Bound(term)) => {
                if self.occurs_in_term(a_root, &term) {
                    return Err(optimization_error("occurs check failed".to_string()));
                }
                self.type_vars[a_root].kind = TypeVarKind::Bound(term);
                Ok(())
            }
            (TypeVarKind::Bound(term), TypeVarKind::Unbound { .. }) => {
                if self.occurs_in_term(b_root, &term) {
                    return Err(optimization_error("occurs check failed".to_string()));
                }
                self.type_vars[b_root].kind = TypeVarKind::Bound(term);
                Ok(())
            }
            (
                TypeVarKind::Bound(TypeTerm::Primitive(TypePrimitive::Int(int_a))),
                TypeVarKind::Bound(TypeTerm::Primitive(TypePrimitive::Int(int_b))),
            ) => {
                if int_a == int_b {
                    Ok(())
                } else if self.literal_ints.remove(&a_root) {
                    self.type_vars[a_root].kind = TypeVarKind::Link(b_root);
                    Ok(())
                } else if self.literal_ints.remove(&b_root) {
                    self.type_vars[b_root].kind = TypeVarKind::Link(a_root);
                    Ok(())
                } else {
                    Err(optimization_error("primitive type mismatch".to_string()))
                }
            }
            (TypeVarKind::Bound(term_a), TypeVarKind::Bound(term_b)) => {
                self.unify_terms(term_a, term_b)
            }
            (TypeVarKind::Link(next), _) => self.unify(next, b_root),
            (_, TypeVarKind::Link(next)) => self.unify(a_root, next),
        }
    }

    fn occurs_in_term(&mut self, var: TypeVarId, term: &TypeTerm) -> bool {
        match term {
            TypeTerm::Tuple(elements) => elements.iter().any(|elem| self.occurs_in(var, *elem)),
            TypeTerm::Function(func) => {
                func.params.iter().any(|param| self.occurs_in(var, *param))
                    || self.occurs_in(var, func.ret)
            }
            TypeTerm::Slice(elem) | TypeTerm::Vec(elem) | TypeTerm::Reference(elem) => {
                self.occurs_in(var, *elem)
            }
            _ => false,
        }
    }

    fn occurs_in(&mut self, needle: TypeVarId, haystack: TypeVarId) -> bool {
        let root = self.find(haystack);
        if root == needle {
            return true;
        }
        match self.type_vars[root].kind.clone() {
            TypeVarKind::Bound(term) => self.occurs_in_term(needle, &term),
            TypeVarKind::Link(next) => self.occurs_in(needle, next),
            _ => false,
        }
    }

    fn unify_terms(&mut self, a: TypeTerm, b: TypeTerm) -> Result<()> {
        match (a, b) {
            (TypeTerm::Primitive(pa), TypeTerm::Primitive(pb)) => {
                if pa == pb {
                    Ok(())
                } else {
                    Err(optimization_error("primitive type mismatch".to_string()))
                }
            }
            (TypeTerm::Unit, TypeTerm::Unit)
            | (TypeTerm::Nothing, TypeTerm::Nothing)
            | (TypeTerm::Any, TypeTerm::Any)
            | (TypeTerm::Unknown, TypeTerm::Unknown) => Ok(()),
            (TypeTerm::Struct(sa), TypeTerm::Struct(sb)) => {
                if sa == sb {
                    Ok(())
                } else {
                    Err(optimization_error("struct type mismatch".to_string()))
                }
            }
            (TypeTerm::Structural(sa), TypeTerm::Structural(sb)) => {
                if sa == sb {
                    Ok(())
                } else {
                    Err(optimization_error("structural type mismatch".to_string()))
                }
            }
            (TypeTerm::Enum(ae), TypeTerm::Enum(be)) => {
                if ae == be {
                    Ok(())
                } else {
                    Err(optimization_error("enum type mismatch".to_string()))
                }
            }
            (TypeTerm::Custom(a), TypeTerm::Custom(b)) => {
                if a == b {
                    Ok(())
                } else if matches!(a, Ty::Array(_)) && matches!(b, Ty::Array(_)) {
                    if format!("{}", a) == format!("{}", b) {
                        Ok(())
                    } else {
                        Err(optimization_error(format!(
                            "custom type mismatch: {} vs {}",
                            a, b
                        )))
                    }
                } else {
                    Err(optimization_error(format!(
                        "custom type mismatch: {} vs {}",
                        a, b
                    )))
                }
            }
            (TypeTerm::Tuple(a_elems), TypeTerm::Tuple(b_elems)) => {
                if a_elems.len() != b_elems.len() {
                    return Err(optimization_error("tuple length mismatch".to_string()));
                }
                for (a_elem, b_elem) in a_elems.into_iter().zip(b_elems.into_iter()) {
                    self.unify(a_elem, b_elem)?;
                }
                Ok(())
            }
            (TypeTerm::Function(a_func), TypeTerm::Function(b_func)) => {
                if a_func.params.len() != b_func.params.len() {
                    return Err(optimization_error("function arity mismatch".to_string()));
                }
                for (a_param, b_param) in a_func.params.into_iter().zip(b_func.params.into_iter()) {
                    self.unify(a_param, b_param)?;
                }
                self.unify(a_func.ret, b_func.ret)
            }
            (TypeTerm::Slice(a), TypeTerm::Slice(b))
            | (TypeTerm::Vec(a), TypeTerm::Vec(b))
            | (TypeTerm::Reference(a), TypeTerm::Reference(b)) => self.unify(a, b),
            (TypeTerm::Reference(inner), TypeTerm::Primitive(TypePrimitive::String)) => {
                let temp = self.fresh_type_var();
                self.bind(temp, TypeTerm::Primitive(TypePrimitive::String));
                self.unify(inner, temp)
            }
            (TypeTerm::Primitive(TypePrimitive::String), TypeTerm::Reference(inner)) => {
                let temp = self.fresh_type_var();
                self.bind(temp, TypeTerm::Primitive(TypePrimitive::String));
                self.unify(inner, temp)
            }
            (TypeTerm::Unknown, other) | (other, TypeTerm::Unknown) => {
                if let TypeTerm::Unknown = other {
                    Ok(())
                } else {
                    Ok(())
                }
            }
            (TypeTerm::Any, _other) | (_other, TypeTerm::Any) => Ok(()),
            (left, right) => Err(optimization_error(format!(
                "type mismatch: {:?} vs {:?}",
                left, right
            ))),
        }
    }

    fn resolve_to_ty(&mut self, var: TypeVarId) -> Result<Ty> {
        let root = self.find(var);
        match self.type_vars[root].kind.clone() {
            TypeVarKind::Unbound { .. } => Ok(Ty::Unknown(TypeUnknown)),
            TypeVarKind::Bound(term) => self.term_to_ty(term),
            TypeVarKind::Link(next) => self.resolve_to_ty(next),
        }
    }

    fn term_to_ty(&mut self, term: TypeTerm) -> Result<Ty> {
        Ok(match term {
            TypeTerm::Primitive(prim) => Ty::Primitive(prim),
            TypeTerm::Unit => Ty::Unit(TypeUnit),
            TypeTerm::Nothing => Ty::Nothing(TypeNothing),
            TypeTerm::Any => Ty::Any(TypeAny),
            TypeTerm::Struct(struct_ty) => Ty::Struct(struct_ty),
            TypeTerm::Structural(structural) => Ty::Structural(structural),
            TypeTerm::Enum(enum_ty) => Ty::Enum(enum_ty),
            TypeTerm::Custom(ty) => ty,
            TypeTerm::Unknown => Ty::Unknown(TypeUnknown),
            TypeTerm::Tuple(elements) => {
                let types = elements
                    .into_iter()
                    .map(|elem| self.resolve_to_ty(elem))
                    .collect::<Result<Vec<_>>>()?;
                Ty::Tuple(TypeTuple { types })
            }
            TypeTerm::Function(function) => {
                let params = function
                    .params
                    .into_iter()
                    .map(|param| self.resolve_to_ty(param))
                    .collect::<Result<Vec<_>>>()?;
                let ret = self.resolve_to_ty(function.ret)?;
                Ty::Function(TypeFunction {
                    params,
                    generics_params: Vec::new(),
                    ret_ty: Some(Box::new(ret)),
                })
            }
            TypeTerm::Slice(elem) => {
                let elem_ty = self.resolve_to_ty(elem)?;
                Ty::Slice(TypeSlice {
                    elem: Box::new(elem_ty),
                })
            }
            TypeTerm::Vec(elem) => {
                let elem_ty = self.resolve_to_ty(elem)?;
                Ty::Vec(TypeVec {
                    ty: Box::new(elem_ty),
                })
            }
            TypeTerm::Reference(elem) => {
                let elem_ty = self.resolve_to_ty(elem)?;
                Ty::Reference(TypeReference {
                    ty: Box::new(elem_ty),
                    mutability: None,
                    lifetime: None,
                })
            }
        })
    }

    fn type_from_ast_ty(&mut self, ty: &Ty) -> Result<TypeVarId> {
        let var = self.fresh_type_var();
        match ty {
            Ty::Primitive(prim) => self.bind(var, TypeTerm::Primitive(*prim)),
            Ty::Unit(_) => self.bind(var, TypeTerm::Unit),
            Ty::Nothing(_) => self.bind(var, TypeTerm::Nothing),
            Ty::Any(_) => self.bind(var, TypeTerm::Any),
            Ty::Struct(struct_ty) => {
                self.struct_defs
                    .insert(struct_ty.name.as_str().to_string(), struct_ty.clone());
                self.bind(var, TypeTerm::Struct(struct_ty.clone()));
            }
            Ty::Structural(structural) => self.bind(var, TypeTerm::Structural(structural.clone())),
            Ty::Enum(enum_ty) => self.bind(var, TypeTerm::Enum(enum_ty.clone())),
            Ty::Function(function) => {
                let mut params = Vec::new();
                for param in &function.params {
                    params.push(self.type_from_ast_ty(param)?);
                }
                let ret_var = if let Some(ret) = &function.ret_ty {
                    self.type_from_ast_ty(ret)?
                } else {
                    let unit = self.fresh_type_var();
                    self.bind(unit, TypeTerm::Unit);
                    unit
                };
                self.bind(
                    var,
                    TypeTerm::Function(FunctionTerm {
                        params,
                        ret: ret_var,
                    }),
                );
            }
            Ty::Tuple(tuple) => {
                let mut vars = Vec::new();
                for elem in &tuple.types {
                    vars.push(self.type_from_ast_ty(elem)?);
                }
                self.bind(var, TypeTerm::Tuple(vars));
            }
            Ty::Slice(slice) => {
                let elem_var = self.type_from_ast_ty(&slice.elem)?;
                self.bind(var, TypeTerm::Slice(elem_var));
            }
            Ty::Vec(vec_ty) => {
                let elem_var = self.type_from_ast_ty(&vec_ty.ty)?;
                self.bind(var, TypeTerm::Vec(elem_var));
            }
            Ty::Reference(reference) => {
                let elem_var = self.type_from_ast_ty(&reference.ty)?;
                self.bind(var, TypeTerm::Reference(elem_var));
            }
            Ty::Array(_) => {
                self.bind(var, TypeTerm::Custom(ty.clone()));
            }
            Ty::Expr(expr) => {
                if let ExprKind::Locator(locator) = expr.kind() {
                    if let Some(ident) = locator.as_ident() {
                        if let Some(primitive) = Self::primitive_from_name(ident.as_str()) {
                            self.bind(var, TypeTerm::Primitive(primitive));
                            return Ok(var);
                        }
                    }

                    let locator_name = locator.to_string();
                    if locator_name == "Self" {
                        if let Some(ctx) = self.impl_stack.last().and_then(|ctx| ctx.as_ref()) {
                            if let Ty::Struct(struct_ty) = ctx.self_ty.clone() {
                                self.bind(var, TypeTerm::Struct(struct_ty));
                                return Ok(var);
                            }
                        }
                    }
                    if let Some(struct_def) = self.struct_defs.get(&locator_name).cloned() {
                        self.bind(var, TypeTerm::Struct(struct_def));
                        return Ok(var);
                    }
                }
                self.bind(var, TypeTerm::Custom(ty.clone()));
            }
            Ty::TypeBounds(_) | Ty::ImplTraits(_) => self.bind(var, TypeTerm::Any),
            Ty::Type(_) => self.bind(var, TypeTerm::Custom(ty.clone())),
            Ty::Value(_) | Ty::Unknown(_) | Ty::AnyBox(_) => {
                self.bind(var, TypeTerm::Custom(ty.clone()));
            }
        }
        Ok(var)
    }

    fn lookup_associated_function(&mut self, locator: &Locator) -> Result<Option<TypeVarId>> {
        if let Locator::Path(path) = locator {
            if path.segments.len() >= 2 {
                if let (Some(struct_segment), Some(method_segment)) = (
                    path.segments.get(path.segments.len() - 2),
                    path.segments.last(),
                ) {
                    let struct_name = struct_segment.as_str();
                    let method_name = method_segment.as_str();
                    if let Some(methods) = self.struct_methods.get(struct_name) {
                        if let Some(record) = methods.get(method_name) {
                            if let Some(scheme) = record.scheme.as_ref() {
                                return Ok(Some(self.instantiate_scheme(&scheme.clone())));
                            }
                            if let Some(var) = self.lookup_env_var(method_name) {
                                return Ok(Some(var));
                            }
                        }
                    }
                }
            }
        }
        Ok(None)
    }

    fn lookup_locator(&mut self, locator: &Locator) -> Result<TypeVarId> {
        let key = locator.to_string();
        if let Some(var) = self.lookup_env_var(&key) {
            return Ok(var);
        }
        if let Some(ident) = locator.as_ident() {
            if let Some(var) = self.lookup_env_var(ident.as_str()) {
                return Ok(var);
            }
        }
        if let Locator::Path(path) = locator {
            if let Some(first) = path.segments.first() {
                if let Some(var) = self.lookup_env_var(first.as_str()) {
                    return Ok(var);
                }
            }
        }
        self.emit_error(format!("unresolved symbol: {}", key));
        Ok(self.error_type_var())
    }

    fn lookup_env_var(&mut self, name: &str) -> Option<TypeVarId> {
        for scope in self.env.iter().rev() {
            if let Some(entry) = scope.get(name) {
                return Some(match entry {
                    EnvEntry::Mono(var) => *var,
                    EnvEntry::Poly(scheme) => {
                        let scheme_clone = scheme.clone();
                        self.instantiate_scheme(&scheme_clone)
                    }
                });
            }
        }
        None
    }

    fn symbol_var(&mut self, name: &Ident) -> TypeVarId {
        let key = name.as_str().to_string();
        if let Some(var) = self.lookup_env_var(&key) {
            return var;
        }
        let var = self.fresh_type_var();
        self.insert_env(key, EnvEntry::Mono(var));
        var
    }

    fn register_symbol(&mut self, name: &Ident) {
        let key = name.as_str().to_string();
        if self.lookup_env_var(&key).is_none() {
            let var = self.fresh_type_var();
            self.insert_env(key, EnvEntry::Mono(var));
        }
    }

    fn emit_error(&mut self, message: impl Into<String>) {
        self.has_errors = true;
        self.diagnostics.push(TypingDiagnostic::error(message));
    }

    #[allow(dead_code)]
    fn emit_warning(&mut self, message: impl Into<String>) {
        self.diagnostics.push(TypingDiagnostic::warning(message));
    }

    fn error_type_var(&mut self) -> TypeVarId {
        let var = self.fresh_type_var();
        self.bind(var, TypeTerm::Unknown);
        var
    }

    fn primitive_from_name(name: &str) -> Option<TypePrimitive> {
        match name {
            "i8" => Some(TypePrimitive::Int(TypeInt::I8)),
            "u8" => Some(TypePrimitive::Int(TypeInt::U8)),
            "i16" => Some(TypePrimitive::Int(TypeInt::I16)),
            "u16" => Some(TypePrimitive::Int(TypeInt::U16)),
            "i32" => Some(TypePrimitive::Int(TypeInt::I32)),
            "u32" => Some(TypePrimitive::Int(TypeInt::U32)),
            "i64" => Some(TypePrimitive::Int(TypeInt::I64)),
            "u64" => Some(TypePrimitive::Int(TypeInt::U64)),
            "isize" => Some(TypePrimitive::Int(TypeInt::I64)),
            "usize" => Some(TypePrimitive::Int(TypeInt::U64)),
            "f32" => Some(TypePrimitive::Decimal(DecimalType::F32)),
            "f64" => Some(TypePrimitive::Decimal(DecimalType::F64)),
            "bool" => Some(TypePrimitive::Bool),
            "char" => Some(TypePrimitive::Char),
            "str" | "String" => Some(TypePrimitive::String),
            "list" | "List" => Some(TypePrimitive::List),
            _ => None,
        }
    }

    fn expect_reference(&mut self, var: TypeVarId, context: &str) -> Result<TypeVarId> {
        let root = self.find(var);
        match self.type_vars[root].kind.clone() {
            TypeVarKind::Unbound { .. } => {
                let inner = self.fresh_type_var();
                self.type_vars[root].kind = TypeVarKind::Bound(TypeTerm::Reference(inner));
                Ok(inner)
            }
            TypeVarKind::Bound(TypeTerm::Reference(inner)) => Ok(inner),
            TypeVarKind::Link(next) => self.expect_reference(next, context),
            _other => {
                self.emit_error(format!("expected reference value for {}", context));
                let placeholder = self.error_type_var();
                self.type_vars[root].kind = TypeVarKind::Bound(TypeTerm::Reference(placeholder));
                Ok(placeholder)
            }
        }
    }

    fn generalize_symbol(&mut self, name: &str, var: TypeVarId) -> Result<()> {
        let scheme = self.generalize(var)?;
        self.replace_env_entry(name, EnvEntry::Poly(scheme));
        Ok(())
    }

    fn ensure_numeric(&mut self, var: TypeVarId, context: &str) -> Result<()> {
        let root = self.find(var);
        match self.type_vars[root].kind.clone() {
            TypeVarKind::Unbound { .. } => {
                self.type_vars[root].kind =
                    TypeVarKind::Bound(TypeTerm::Primitive(TypePrimitive::Int(TypeInt::I64)));
                Ok(())
            }
            TypeVarKind::Bound(TypeTerm::Primitive(TypePrimitive::Int(_)))
            | TypeVarKind::Bound(TypeTerm::Primitive(TypePrimitive::Decimal(_))) => Ok(()),
            TypeVarKind::Link(next) => self.ensure_numeric(next, context),
            other => {
                self.emit_error(format!("expected numeric value for {}", context));
                Err(optimization_error(format!(
                    "expected numeric type, found {:?}",
                    other
                )))
            }
        }
    }

    fn ensure_bool(&mut self, var: TypeVarId, context: &str) -> Result<()> {
        let root = self.find(var);
        match self.type_vars[root].kind.clone() {
            TypeVarKind::Unbound { .. } => {
                self.type_vars[root].kind =
                    TypeVarKind::Bound(TypeTerm::Primitive(TypePrimitive::Bool));
                Ok(())
            }
            TypeVarKind::Bound(TypeTerm::Primitive(TypePrimitive::Bool)) => Ok(()),
            TypeVarKind::Link(next) => self.ensure_bool(next, context),
            other => {
                self.emit_error(format!("expected boolean for {}", context));
                Err(optimization_error(format!(
                    "expected bool, found {:?}",
                    other
                )))
            }
        }
    }

    fn ensure_integer(&mut self, var: TypeVarId, context: &str) -> Result<()> {
        let root = self.find(var);
        match self.type_vars[root].kind.clone() {
            TypeVarKind::Unbound { .. } => {
                self.type_vars[root].kind =
                    TypeVarKind::Bound(TypeTerm::Primitive(TypePrimitive::Int(TypeInt::I64)));
                Ok(())
            }
            TypeVarKind::Bound(TypeTerm::Primitive(TypePrimitive::Int(_))) => Ok(()),
            TypeVarKind::Link(next) => self.ensure_integer(next, context),
            other => {
                self.emit_error(format!("expected integer value for {}", context));
                Err(optimization_error(format!(
                    "expected integer, found {:?}",
                    other
                )))
            }
        }
    }

    fn ensure_function(&mut self, var: TypeVarId, arity: usize) -> Result<FunctionTypeInfo> {
        let root = self.find(var);
        match self.type_vars[root].kind.clone() {
            TypeVarKind::Unbound { .. } => {
                let params: Vec<_> = (0..arity).map(|_| self.fresh_type_var()).collect();
                let ret = self.fresh_type_var();
                self.type_vars[root].kind = TypeVarKind::Bound(TypeTerm::Function(FunctionTerm {
                    params: params.clone(),
                    ret,
                }));
                Ok(FunctionTypeInfo { params, ret })
            }
            TypeVarKind::Bound(TypeTerm::Function(func)) => {
                if func.params.len() != arity {
                    self.emit_error(format!(
                        "function arity mismatch: expected {}, found {}",
                        arity,
                        func.params.len()
                    ));
                    let params: Vec<_> = (0..arity).map(|_| self.error_type_var()).collect();
                    let ret = self.error_type_var();
                    self.type_vars[root].kind =
                        TypeVarKind::Bound(TypeTerm::Function(FunctionTerm {
                            params: params.clone(),
                            ret,
                        }));
                    return Ok(FunctionTypeInfo { params, ret });
                }
                Ok(FunctionTypeInfo {
                    params: func.params,
                    ret: func.ret,
                })
            }
            TypeVarKind::Link(next) => self.ensure_function(next, arity),
            other => {
                self.emit_error(format!("expected function, found {:?}", other));
                let params: Vec<_> = (0..arity).map(|_| self.error_type_var()).collect();
                let ret = self.error_type_var();
                self.type_vars[root].kind = TypeVarKind::Bound(TypeTerm::Function(FunctionTerm {
                    params: params.clone(),
                    ret,
                }));
                Ok(FunctionTypeInfo { params, ret })
            }
        }
    }

    fn lookup_struct_method(&mut self, obj_var: TypeVarId, field: &Ident) -> Result<TypeVarId> {
        let ty = self.resolve_to_ty(obj_var)?;
        let struct_name = match ty {
            Ty::Struct(struct_ty) => struct_ty.name.as_str().to_string(),
            other => {
                self.emit_error(format!(
                    "cannot call method {} on value of type {}",
                    field, other
                ));
                return Ok(self.error_type_var());
            }
        };

        if let Some(methods) = self.struct_methods.get(&struct_name) {
            if let Some(record) = methods.get(field.as_str()) {
                if let Some(scheme) = record.scheme.as_ref() {
                    return Ok(self.instantiate_scheme(&scheme.clone()));
                }
                if let Some(var) = self.lookup_env_var(field.as_str()) {
                    return Ok(var);
                }
            }
        }

        self.emit_error(format!(
            "unknown method {} on struct {}",
            field, struct_name
        ));
        Ok(self.error_type_var())
    }

    fn lookup_struct_field(&mut self, obj_var: TypeVarId, field: &Ident) -> Result<TypeVarId> {
        let ty = self.resolve_to_ty(obj_var)?;
        let resolved_ty = Self::peel_reference(ty);
        match resolved_ty {
            Ty::Struct(struct_ty) => {
                if let Some(def_field) = struct_ty.fields.iter().find(|f| f.name == *field) {
                    let var = self.type_from_ast_ty(&def_field.value)?;
                    Ok(var)
                } else {
                    self.emit_error(format!(
                        "unknown field {} on struct {}",
                        field, struct_ty.name
                    ));
                    Ok(self.error_type_var())
                }
            }
            Ty::Structural(structural) => {
                if let Some(def_field) = structural.fields.iter().find(|f| f.name == *field) {
                    let var = self.type_from_ast_ty(&def_field.value)?;
                    Ok(var)
                } else {
                    self.emit_error(format!("unknown field {}", field));
                    Ok(self.error_type_var())
                }
            }
            other => {
                self.emit_error(format!(
                    "cannot access field {} on value of type {}",
                    field, other
                ));
                Ok(self.error_type_var())
            }
        }
    }

    fn resolve_struct_literal(&mut self, struct_expr: &mut ExprStruct) -> Result<TypeVarId> {
        let struct_name = match self.struct_name_from_expr(&struct_expr.name) {
            Some(name) => name,
            None => {
                self.emit_error("struct literal target could not be resolved");
                return Ok(self.error_type_var());
            }
        };
        if let Some(def) = self.struct_defs.get(&struct_name).cloned() {
            let var = self.fresh_type_var();
            self.bind(var, TypeTerm::Struct(def.clone()));
            for field in &mut struct_expr.fields {
                if let Some(value) = field.value.as_mut() {
                    let value_var = self.infer_expr(value)?;
                    if let Some(struct_field) = def.fields.iter().find(|f| f.name == field.name) {
                        let ty_var = self.type_from_ast_ty(&struct_field.value)?;
                        self.unify(value_var, ty_var)?;
                    } else {
                        self.emit_error(format!(
                            "unknown field {} on struct {}",
                            field.name, def.name
                        ));
                        return Ok(self.error_type_var());
                    }
                }
            }
            Ok(var)
        } else {
            self.emit_error(format!("unknown struct literal target: {}", struct_name));
            Ok(self.error_type_var())
        }
    }

    fn ty_from_function_signature(&mut self, sig: &FunctionSignature) -> Result<Ty> {
        let mut params = Vec::new();
        for param in &sig.params {
            params.push(param.ty.clone());
        }
        let ret_ty = sig.ret_ty.clone().unwrap_or_else(|| Ty::Unit(TypeUnit));
        Ok(Ty::Function(TypeFunction {
            params,
            generics_params: sig.generics_params.clone(),
            ret_ty: Some(Box::new(ret_ty)),
        }))
    }

    fn struct_name_from_expr(&self, expr: &Expr) -> Option<String> {
        match expr.kind() {
            ExprKind::Locator(locator) => {
                let name = locator.to_string();
                if name == "Self" {
                    self.impl_stack
                        .last()
                        .and_then(|ctx| ctx.as_ref())
                        .map(|ctx| ctx.struct_name.clone())
                } else {
                    Some(name)
                }
            }
            ExprKind::Value(value) => match &**value {
                Value::Type(Ty::Struct(struct_ty)) => Some(struct_ty.name.as_str().to_string()),
                _ => None,
            },
            _ => None,
        }
    }
}

pub fn annotate(node: &mut Node) -> Result<TypingOutcome> {
    let mut inferencer = AstTypeInferencer::new();
    inferencer.infer(node)
}
