use std::collections::{HashMap, HashSet};

use fp_core::ast::{
    BlockStmt, Expr, ExprBlock, ExprFormatString, ExprInvoke, ExprInvokeTarget, ExprKind, Item,
    ItemDeclFunction, ItemDefFunction, ItemKind, Node, NodeKind, Ty, TypeArray, TypeFunction,
    TypePrimitive, TypeReference, TypeSlice, TypeStruct, TypeTuple, TypeUnit, TypeVec,
};
use fp_core::error::{Error as CoreError, Result};
use fp_core::id::{Ident, Locator};

/// Specialize generic function definitions based on the concrete call sites that
/// appear in the typed AST. This is a best-effort monomorphization pass that
/// only handles relatively simple cases (free functions with type parameters
/// and function-pointer arguments). Trait bounds are ignored – callers are
/// responsible for ensuring the substituted types provide the required
/// behaviour.
pub fn specialize(node: &mut Node) -> Result<()> {
    match node.kind_mut() {
        NodeKind::File(file) => {
            let mut specializer = Specializer::new();
            specializer.collect_templates(&file.items);
            specializer.specialize_items(&mut file.items)?;
            specializer.finalize(&mut file.items)?;
        }
        NodeKind::Item(item) => {
            let mut specializer = Specializer::new();
            specializer.collect_templates(std::slice::from_ref(item));
            specializer.specialize_item(item)?;
        }
        NodeKind::Expr(expr) => {
            let mut specializer = Specializer::new();
            specializer.specialize_expr(expr)?;
        }
    }
    Ok(())
}

#[derive(Clone)]
struct TemplateData {
    function: ItemDefFunction,
    generic_order: Vec<String>,
    generic_set: HashSet<String>,
    item_index: usize,
}

struct Specializer {
    templates: HashMap<String, TemplateData>,
    instantiations: HashMap<String, String>,
    generated: Vec<Item>,
    specialized_bases: HashSet<String>,
}

impl Specializer {
    fn new() -> Self {
        Self {
            templates: HashMap::new(),
            instantiations: HashMap::new(),
            generated: Vec::new(),
            specialized_bases: HashSet::new(),
        }
    }

    fn collect_templates(&mut self, items: &[Item]) {
        for (idx, item) in items.iter().enumerate() {
            if let ItemKind::DefFunction(func) = item.kind() {
                if func.sig.generics_params.is_empty() {
                    continue;
                }
                let name = func.name.as_str().to_string();
                let generic_order: Vec<String> = func
                    .sig
                    .generics_params
                    .iter()
                    .map(|param| param.name.as_str().to_string())
                    .collect();
                let generic_set = generic_order.iter().cloned().collect();
                self.templates.insert(
                    name,
                    TemplateData {
                        function: func.clone(),
                        generic_order,
                        generic_set,
                        item_index: idx,
                    },
                );
            }
        }
    }

    fn specialize_items(&mut self, items: &mut Vec<Item>) -> Result<()> {
        let original_len = items.len();
        for idx in 0..original_len {
            self.specialize_item(&mut items[idx])?;
        }
        Ok(())
    }

    fn specialize_item(&mut self, item: &mut Item) -> Result<()> {
        match item.kind_mut() {
            ItemKind::Module(module) => {
                let original_len = module.items.len();
                for idx in 0..original_len {
                    self.specialize_item(&mut module.items[idx])?;
                }
            }
            ItemKind::DefFunction(function) => {
                if function.sig.generics_params.is_empty() {
                    self.specialize_expr(function.body.as_mut())?;
                }
            }
            ItemKind::DefConst(def) => {
                self.specialize_expr(def.value.as_mut())?;
            }
            ItemKind::DefStatic(def) => {
                self.specialize_expr(def.value.as_mut())?;
            }
            ItemKind::Expr(expr) => {
                self.specialize_expr(expr)?;
            }
            ItemKind::Impl(impl_block) => {
                for item in &mut impl_block.items {
                    self.specialize_item(item)?;
                }
            }
            ItemKind::DefStruct(_)
            | ItemKind::DefStructural(_)
            | ItemKind::DefEnum(_)
            | ItemKind::DefType(_)
            | ItemKind::DeclConst(_)
            | ItemKind::DeclStatic(_)
            | ItemKind::DeclFunction(_)
            | ItemKind::DeclType(_)
            | ItemKind::Import(_)
            | ItemKind::DefTrait(_)
            | ItemKind::Any(_) => {}
        }
        Ok(())
    }

    fn specialize_expr(&mut self, expr: &mut Expr) -> Result<()> {
        match expr.kind_mut() {
            ExprKind::Block(block) => self.specialize_block(block)?,
            ExprKind::If(expr_if) => {
                self.specialize_expr(expr_if.cond.as_mut())?;
                self.specialize_expr(expr_if.then.as_mut())?;
                if let Some(elze) = expr_if.elze.as_mut() {
                    self.specialize_expr(elze)?;
                }
            }
            ExprKind::Loop(expr_loop) => self.specialize_expr(expr_loop.body.as_mut())?,
            ExprKind::While(expr_while) => {
                self.specialize_expr(expr_while.cond.as_mut())?;
                self.specialize_expr(expr_while.body.as_mut())?;
            }
            ExprKind::Match(expr_match) => {
                for case in &mut expr_match.cases {
                    self.specialize_expr(case.cond.as_mut())?;
                    self.specialize_expr(case.body.as_mut())?;
                }
            }
            ExprKind::Let(expr_let) => self.specialize_expr(expr_let.expr.as_mut())?,
            ExprKind::Assign(assign) => {
                self.specialize_expr(assign.target.as_mut())?;
                self.specialize_expr(assign.value.as_mut())?;
            }
            ExprKind::Invoke(invoke) => self.specialize_invoke(invoke)?,
            ExprKind::Select(select) => self.specialize_expr(select.obj.as_mut())?,
            ExprKind::Struct(struct_expr) => {
                for field in &mut struct_expr.fields {
                    if let Some(value) = field.value.as_mut() {
                        self.specialize_expr(value)?;
                    }
                }
            }
            ExprKind::Structural(struct_expr) => {
                for field in &mut struct_expr.fields {
                    if let Some(value) = field.value.as_mut() {
                        self.specialize_expr(value)?;
                    }
                }
            }
            ExprKind::Item(item) => self.specialize_item(item.as_mut())?,
            ExprKind::Array(array) => {
                for value in &mut array.values {
                    self.specialize_expr(value)?;
                }
            }
            ExprKind::ArrayRepeat(array_repeat) => {
                self.specialize_expr(array_repeat.elem.as_mut())?;
                self.specialize_expr(array_repeat.len.as_mut())?;
            }
            ExprKind::Tuple(tuple) => {
                for value in &mut tuple.values {
                    self.specialize_expr(value)?;
                }
            }
            ExprKind::BinOp(binop) => {
                self.specialize_expr(binop.lhs.as_mut())?;
                self.specialize_expr(binop.rhs.as_mut())?;
            }
            ExprKind::UnOp(unop) => self.specialize_expr(unop.val.as_mut())?,
            ExprKind::Reference(reference) => self.specialize_expr(reference.referee.as_mut())?,
            ExprKind::Dereference(deref) => self.specialize_expr(deref.referee.as_mut())?,
            ExprKind::Index(index) => {
                self.specialize_expr(index.obj.as_mut())?;
                self.specialize_expr(index.index.as_mut())?;
            }
            ExprKind::Splat(splat) => self.specialize_expr(splat.iter.as_mut())?,
            ExprKind::SplatDict(splat) => self.specialize_expr(splat.dict.as_mut())?,
            ExprKind::Try(expr_try) => self.specialize_expr(expr_try.expr.as_mut())?,
            ExprKind::Closure(closure) => self.specialize_expr(closure.body.as_mut())?,
            ExprKind::Closured(closured) => self.specialize_expr(closured.expr.as_mut())?,
            ExprKind::Paren(paren) => self.specialize_expr(paren.expr.as_mut())?,
            ExprKind::FormatString(format) => self.specialize_format_string(format)?,
            ExprKind::IntrinsicCall(call) => match &mut call.payload {
                fp_core::intrinsics::IntrinsicCallPayload::Args { args } => {
                    for arg in args {
                        self.specialize_expr(arg)?;
                    }
                }
                fp_core::intrinsics::IntrinsicCallPayload::Format { template } => {
                    self.specialize_format_string(template)?;
                }
            },
            ExprKind::Value(value) => match value.as_mut() {
                fp_core::ast::Value::Expr(inner) => self.specialize_expr(inner.as_mut())?,
                fp_core::ast::Value::Function(func) => self.specialize_expr(func.body.as_mut())?,
                _ => {}
            },
            _ => {}
        }
        Ok(())
    }

    fn specialize_block(&mut self, block: &mut ExprBlock) -> Result<()> {
        for stmt in &mut block.stmts {
            match stmt {
                BlockStmt::Expr(expr_stmt) => self.specialize_expr(expr_stmt.expr.as_mut())?,
                BlockStmt::Let(stmt_let) => {
                    if let Some(init) = stmt_let.init.as_mut() {
                        self.specialize_expr(init)?;
                    }
                    if let Some(diverge) = stmt_let.diverge.as_mut() {
                        self.specialize_expr(diverge)?;
                    }
                }
                BlockStmt::Item(item) => self.specialize_item(item.as_mut())?,
                BlockStmt::Noop | BlockStmt::Any(_) => {}
            }
        }
        Ok(())
    }

    fn specialize_format_string(&mut self, format: &mut ExprFormatString) -> Result<()> {
        for arg in &mut format.args {
            self.specialize_expr(arg)?;
        }
        for kwarg in &mut format.kwargs {
            self.specialize_expr(&mut kwarg.value)?;
        }
        Ok(())
    }

    fn specialize_invoke(&mut self, invoke: &mut ExprInvoke) -> Result<()> {
        for arg in &mut invoke.args {
            self.specialize_expr(arg)?;
        }

        let ExprInvokeTarget::Function(locator) = &mut invoke.target else {
            return Ok(());
        };

        let target_name = locator.to_string();
        let Some(template) = self.templates.get(&target_name).cloned() else {
            return Ok(());
        };

        if invoke.args.len() != template.function.sig.params.len() {
            return Ok(());
        }

        let mut substitution = HashMap::new();

        let template_params = template.function.sig.params.clone();
        let template_generics = template.generic_order.clone();
        let template_generic_set = template.generic_set.clone();
        if !template_generics.is_empty() {
            for (param, arg) in template_params.iter().zip(invoke.args.iter()) {
                if let Some(actual) = arg.ty() {
                    if !self.match_type(&param.ty, actual, &template_generic_set, &mut substitution)
                    {
                        return Ok(());
                    }
                } else {
                    return Ok(());
                }
            }

            if !template_generics
                .iter()
                .all(|name| substitution.contains_key(name))
            {
                return Ok(());
            }

            let new_name =
                self.ensure_specialization(&target_name, &template_generics, &substitution)?;
            self.specialized_bases.insert(target_name.clone());
            *locator = Locator::ident(Ident::new(new_name.clone()));
        }

        for (param, arg) in template_params.iter().zip(invoke.args.iter_mut()) {
            let specialized_param_ty = self.substitute_ty(&param.ty, &substitution);
            if let ExprKind::Locator(arg_locator) = arg.kind_mut() {
                let arg_name = arg_locator.to_string();
                let Some(arg_template) = self.templates.get(&arg_name).cloned() else {
                    continue;
                };

                let mut nested_subst = HashMap::new();
                if let Ty::Function(fn_ty) = &specialized_param_ty {
                    if self.match_function_signature(
                        &arg_template.function,
                        fn_ty,
                        &arg_template.generic_set,
                        &mut nested_subst,
                    ) {
                        if arg_template
                            .generic_order
                            .iter()
                            .all(|name| nested_subst.contains_key(name))
                        {
                            let new_arg_name = self.ensure_specialization(
                                &arg_name,
                                &arg_template.generic_order,
                                &nested_subst,
                            )?;
                            self.specialized_bases.insert(arg_name.clone());
                            *arg_locator = Locator::ident(Ident::new(new_arg_name.clone()));
                            arg.set_ty(Ty::Function(fn_ty.clone()));
                        }
                    }
                }
            }
        }

        Ok(())
    }

    fn ensure_specialization(
        &mut self,
        function_name: &str,
        generic_order: &[String],
        substitution: &HashMap<String, Ty>,
    ) -> Result<String> {
        let key = self.monomorph_key(function_name, generic_order, substitution)?;
        if let Some(existing) = self.instantiations.get(&key) {
            return Ok(existing.clone());
        }

        let template = self.templates.get(function_name).ok_or_else(|| {
            CoreError::from(format!(
                "missing template for specialization target: {}",
                function_name
            ))
        })?;

        let mut specialized = template.function.clone();
        let new_ident =
            Ident::new(self.mangled_name(function_name, generic_order, substitution)?);

        specialized.name = new_ident.clone();
        specialized.sig.name = Some(new_ident.clone());
        specialized.sig.generics_params.clear();

        for param in &mut specialized.sig.params {
            let ty = self.substitute_ty(&param.ty, substitution);
            param.ty = ty.clone();
            param.ty_annotation = Some(ty);
        }

        if let Some(ret_ty) = specialized.sig.ret_ty.as_mut() {
            let ty = self.substitute_ty(ret_ty, substitution);
            *ret_ty = ty;
        }

        let function_ty = TypeFunction {
            params: specialized
                .sig
                .params
                .iter()
                .map(|p| p.ty.clone())
                .collect(),
            generics_params: Vec::new(),
            ret_ty: specialized
                .sig
                .ret_ty
                .as_ref()
                .map(|ty| Box::new(ty.clone())),
        };
        specialized.ty = Some(function_ty.clone());
        specialized.ty_annotation = Some(Ty::Function(function_ty.clone()));

        self.clear_expr_types(specialized.body.as_mut());

        let mut new_item = Item::new(ItemKind::DefFunction(specialized));
        new_item.set_ty(Ty::Function(function_ty.clone()));

        self.generated.push(new_item);
        self.instantiations
            .insert(key.clone(), new_ident.as_str().to_string());

        eprintln!(
            "[specialize] instantiated {} -> {}",
            function_name, new_ident
        );

        Ok(new_ident.as_str().to_string())
    }

    fn match_function_signature(
        &self,
        function: &ItemDefFunction,
        expected: &TypeFunction,
        generics: &HashSet<String>,
        subst: &mut HashMap<String, Ty>,
    ) -> bool {
        if function.sig.params.len() != expected.params.len() {
            return false;
        }

        for (param, actual) in function.sig.params.iter().zip(expected.params.iter()) {
            if !self.match_type(&param.ty, actual, generics, subst) {
                return false;
            }
        }

        match (&function.sig.ret_ty, &expected.ret_ty) {
            (Some(ret_pattern), Some(ret_actual)) => {
                self.match_type(ret_pattern, ret_actual.as_ref(), generics, subst)
            }
            (None, Some(ret_actual)) => {
                self.match_type(&Ty::Unit(TypeUnit), ret_actual.as_ref(), generics, subst)
            }
            (Some(_), None) => true,
            (None, None) => true,
        }
    }

    fn match_type(
        &self,
        pattern: &Ty,
        actual: &Ty,
        generics: &HashSet<String>,
        subst: &mut HashMap<String, Ty>,
    ) -> bool {
        if let Some(name) = self.generic_name(pattern, generics) {
            if let Some(existing) = subst.get(&name) {
                return self.types_compatible(existing, actual);
            }
            if self.is_unknown(actual) {
                return true;
            }
            subst.insert(name, actual.clone());
            return true;
        }

        match (pattern, actual) {
            (Ty::Primitive(p), Ty::Primitive(a)) => p == a,
            (Ty::Unit(_), Ty::Unit(_)) => true,
            (Ty::Nothing(_), Ty::Nothing(_)) => true,
            (Ty::Struct(p), Ty::Struct(a)) => self.match_struct_type(p, a, generics, subst),
            (Ty::Reference(p), Ty::Reference(a)) => self.match_type(&p.ty, &a.ty, generics, subst),
            (Ty::Vec(p), Ty::Vec(a)) => self.match_type(&p.ty, &a.ty, generics, subst),
            (Ty::Slice(p), Ty::Slice(a)) => self.match_type(&p.elem, &a.elem, generics, subst),
            (Ty::Tuple(p), Ty::Tuple(a)) => {
                if p.types.len() != a.types.len() {
                    return false;
                }
                p.types
                    .iter()
                    .zip(a.types.iter())
                    .all(|(pt, at)| self.match_type(pt, at, generics, subst))
            }
            (Ty::Function(p), Ty::Function(a)) => {
                if p.params.len() != a.params.len() {
                    return false;
                }
                for (pp, ap) in p.params.iter().zip(a.params.iter()) {
                    if !self.match_type(pp, ap, generics, subst) {
                        return false;
                    }
                }
                match (&p.ret_ty, &a.ret_ty) {
                    (Some(ret_p), Some(ret_a)) => {
                        self.match_type(ret_p.as_ref(), ret_a.as_ref(), generics, subst)
                    }
                    (None, Some(ret_a)) => {
                        self.match_type(&Ty::Unit(TypeUnit), ret_a.as_ref(), generics, subst)
                    }
                    (Some(_), None) => true,
                    (None, None) => true,
                }
            }
            (_, other) => self.is_unknown(other) || pattern == other,
        }
    }

    fn match_struct_type(
        &self,
        pattern: &TypeStruct,
        actual: &TypeStruct,
        _generics: &HashSet<String>,
        _subst: &mut HashMap<String, Ty>,
    ) -> bool {
        pattern.name == actual.name
    }

    fn substitute_ty(&self, ty: &Ty, subst: &HashMap<String, Ty>) -> Ty {
        if let Ty::Expr(expr) = ty {
            if let ExprKind::Locator(locator) = expr.kind() {
                let name = locator.to_string();
                if let Some(mapped) = subst.get(&name) {
                    return mapped.clone();
                }
            }
        }

        match ty {
            Ty::Primitive(_) | Ty::Unit(_) | Ty::Nothing(_) | Ty::Any(_) | Ty::Unknown(_) => {
                ty.clone()
            }
            Ty::Struct(strct) => Ty::Struct(self.substitute_struct(strct, subst)),
            Ty::Reference(reference) => Ty::Reference(TypeReference {
                ty: Box::new(self.substitute_ty(&reference.ty, subst)),
                mutability: reference.mutability,
                lifetime: reference.lifetime.clone(),
            }),
            Ty::Vec(vec_ty) => Ty::Vec(TypeVec {
                ty: Box::new(self.substitute_ty(&vec_ty.ty, subst)),
            }),
            Ty::Slice(slice) => Ty::Slice(TypeSlice {
                elem: Box::new(self.substitute_ty(&slice.elem, subst)),
            }),
            Ty::Tuple(tuple) => Ty::Tuple(TypeTuple {
                types: tuple
                    .types
                    .iter()
                    .map(|ty| self.substitute_ty(ty, subst))
                    .collect(),
            }),
            Ty::Function(function) => Ty::Function(TypeFunction {
                params: function
                    .params
                    .iter()
                    .map(|ty| self.substitute_ty(ty, subst))
                    .collect(),
                generics_params: Vec::new(),
                ret_ty: function
                    .ret_ty
                    .as_ref()
                    .map(|ret| Box::new(self.substitute_ty(ret.as_ref(), subst))),
            }),
            Ty::Expr(_) => ty.clone(),
            Ty::Structural(structural) => Ty::Structural(structural.clone()),
            Ty::Enum(enm) => Ty::Enum(enm.clone()),
            Ty::Array(array) => Ty::Array(TypeArray {
                elem: Box::new(self.substitute_ty(&array.elem, subst)),
                len: array.len.clone(),
            }),
            Ty::ImplTraits(_) | Ty::TypeBounds(_) | Ty::Type(_) | Ty::Value(_) | Ty::AnyBox(_) => {
                ty.clone()
            }
        }
    }

    fn substitute_struct(&self, ty: &TypeStruct, _subst: &HashMap<String, Ty>) -> TypeStruct {
        ty.clone()
    }

    fn generic_name<'a>(&self, ty: &'a Ty, generics: &HashSet<String>) -> Option<String> {
        if let Ty::Expr(expr) = ty {
            if let ExprKind::Locator(locator) = expr.kind() {
                let name = locator.to_string();
                if generics.contains(&name) {
                    return Some(name);
                }
            }
        }
        None
    }

    fn types_compatible(&self, a: &Ty, b: &Ty) -> bool {
        a == b || self.is_unknown(b)
    }

    fn is_unknown(&self, ty: &Ty) -> bool {
        matches!(ty, Ty::Unknown(_) | Ty::Any(_))
    }

    fn mangle_type(&self, ty: &Ty) -> String {
        match ty {
            Ty::Primitive(TypePrimitive::Int(int_ty)) => format!("{}", int_ty),
            Ty::Primitive(TypePrimitive::Decimal(dec_ty)) => format!("{}", dec_ty),
            Ty::Primitive(TypePrimitive::Bool) => "bool".to_string(),
            Ty::Primitive(TypePrimitive::Char) => "char".to_string(),
            Ty::Primitive(TypePrimitive::String) => "string".to_string(),
            Ty::Primitive(other) => format!("{}", other),
            Ty::Struct(struct_ty) => format!("struct_{}", struct_ty.name),
            Ty::Enum(enum_ty) => format!("enum_{}", enum_ty.name),
            Ty::Reference(reference) => format!("ref_{}", self.mangle_type(&reference.ty)),
            Ty::Vec(vec_ty) => format!("vec_{}", self.mangle_type(&vec_ty.ty)),
            Ty::Slice(slice) => format!("slice_{}", self.mangle_type(&slice.elem)),
            Ty::Tuple(tuple) => format!(
                "tuple_{}",
                tuple
                    .types
                    .iter()
                    .map(|ty| self.mangle_type(ty))
                    .collect::<Vec<_>>()
                    .join("_"),
            ),
            Ty::Function(function) => {
                let params = function
                    .params
                    .iter()
                    .map(|ty| self.mangle_type(ty))
                    .collect::<Vec<_>>()
                    .join("_");
                let ret = function
                    .ret_ty
                    .as_ref()
                    .map(|ty| self.mangle_type(ty))
                    .unwrap_or_else(|| "unit".to_string());
                format!("fn_{}_to_{}", params, ret)
            }
            Ty::Unit(_) => "unit".to_string(),
            Ty::Nothing(_) => "nothing".to_string(),
            Ty::Any(_) => "any".to_string(),
            Ty::Unknown(_) => "unknown".to_string(),
            Ty::Expr(expr) => format!("expr_{}", expr),
            Ty::ImplTraits(_) => "impl_traits".to_string(),
            Ty::TypeBounds(_) => "type_bounds".to_string(),
            Ty::Structural(_) => "structural".to_string(),
            Ty::Array(array) => format!("array_{}", self.mangle_type(&array.elem)),
            Ty::Type(_) => "type".to_string(),
            Ty::Value(_) => "value".to_string(),
            Ty::AnyBox(_) => "anybox".to_string(),
        }
    }

    fn mangled_name(
        &self,
        function_name: &str,
        generic_order: &[String],
        subst: &HashMap<String, Ty>,
    ) -> Result<String> {
        let mut parts = Vec::new();
        for name in generic_order {
            let ty = subst.get(name).ok_or_else(|| {
                CoreError::from(format!(
                    "missing substitution for generic {} in {}",
                    name, function_name
                ))
            })?;
            parts.push(self.sanitize(&self.mangle_type(ty)));
        }
        Ok(format!("{}__{}", function_name, parts.join("_")))
    }

    fn monomorph_key(
        &self,
        function_name: &str,
        generic_order: &[String],
        subst: &HashMap<String, Ty>,
    ) -> Result<String> {
        let mut parts = Vec::new();
        for name in generic_order {
            let ty = subst.get(name).ok_or_else(|| {
                CoreError::from(format!(
                    "missing substitution for generic {} in {}",
                    name, function_name
                ))
            })?;
            parts.push(self.sanitize(&format!("{}", ty)));
        }
        Ok(format!("{}|{}", function_name, parts.join("|")))
    }

    fn sanitize(&self, value: &str) -> String {
        value
            .chars()
            .map(|ch| if ch.is_alphanumeric() { ch } else { '_' })
            .collect()
    }

    fn clear_expr_types(&self, expr: &mut Expr) {
        *expr.ty_mut() = None;
        match expr.kind_mut() {
            ExprKind::Block(block) => {
                for stmt in &mut block.stmts {
                    if let BlockStmt::Expr(expr_stmt) = stmt {
                        self.clear_expr_types(expr_stmt.expr.as_mut());
                    }
                }
            }
            ExprKind::If(expr_if) => {
                self.clear_expr_types(expr_if.cond.as_mut());
                self.clear_expr_types(expr_if.then.as_mut());
                if let Some(elze) = expr_if.elze.as_mut() {
                    self.clear_expr_types(elze);
                }
            }
            ExprKind::Loop(expr_loop) => self.clear_expr_types(expr_loop.body.as_mut()),
            ExprKind::While(expr_while) => {
                self.clear_expr_types(expr_while.cond.as_mut());
                self.clear_expr_types(expr_while.body.as_mut());
            }
            ExprKind::Match(expr_match) => {
                for case in &mut expr_match.cases {
                    self.clear_expr_types(case.cond.as_mut());
                    self.clear_expr_types(case.body.as_mut());
                }
            }
            ExprKind::Let(expr_let) => self.clear_expr_types(expr_let.expr.as_mut()),
            ExprKind::Assign(assign) => {
                self.clear_expr_types(assign.target.as_mut());
                self.clear_expr_types(assign.value.as_mut());
            }
            ExprKind::Invoke(invoke) => {
                if let ExprInvokeTarget::Expr(inner) = &mut invoke.target {
                    self.clear_expr_types(inner.as_mut());
                }
                for arg in &mut invoke.args {
                    self.clear_expr_types(arg);
                }
            }
            ExprKind::Struct(struct_expr) => {
                for field in &mut struct_expr.fields {
                    if let Some(value) = field.value.as_mut() {
                        self.clear_expr_types(value);
                    }
                }
            }
            ExprKind::Structural(struct_expr) => {
                for field in &mut struct_expr.fields {
                    if let Some(value) = field.value.as_mut() {
                        self.clear_expr_types(value);
                    }
                }
            }
            ExprKind::Select(select) => self.clear_expr_types(select.obj.as_mut()),
            ExprKind::Item(item) => {
                *item.as_mut().ty_mut() = None;
            }
            ExprKind::Array(array) => {
                for value in &mut array.values {
                    self.clear_expr_types(value);
                }
            }
            ExprKind::ArrayRepeat(array_repeat) => {
                self.clear_expr_types(array_repeat.elem.as_mut());
                self.clear_expr_types(array_repeat.len.as_mut());
            }
            ExprKind::Tuple(tuple) => {
                for value in &mut tuple.values {
                    self.clear_expr_types(value);
                }
            }
            ExprKind::BinOp(binop) => {
                self.clear_expr_types(binop.lhs.as_mut());
                self.clear_expr_types(binop.rhs.as_mut());
            }
            ExprKind::UnOp(unop) => self.clear_expr_types(unop.val.as_mut()),
            ExprKind::Reference(reference) => self.clear_expr_types(reference.referee.as_mut()),
            ExprKind::Dereference(deref) => self.clear_expr_types(deref.referee.as_mut()),
            ExprKind::Index(index) => {
                self.clear_expr_types(index.obj.as_mut());
                self.clear_expr_types(index.index.as_mut());
            }
            ExprKind::Splat(splat) => self.clear_expr_types(splat.iter.as_mut()),
            ExprKind::SplatDict(splat) => self.clear_expr_types(splat.dict.as_mut()),
            ExprKind::Try(expr_try) => self.clear_expr_types(expr_try.expr.as_mut()),
            ExprKind::Closure(closure) => self.clear_expr_types(closure.body.as_mut()),
            ExprKind::Closured(closured) => self.clear_expr_types(closured.expr.as_mut()),
            ExprKind::Paren(paren) => self.clear_expr_types(paren.expr.as_mut()),
            ExprKind::FormatString(format) => {
                for arg in &mut format.args {
                    self.clear_expr_types(arg);
                }
                for kwarg in &mut format.kwargs {
                    self.clear_expr_types(&mut kwarg.value);
                }
            }
            ExprKind::IntrinsicCall(call) => match &mut call.payload {
                fp_core::intrinsics::IntrinsicCallPayload::Args { args } => {
                    for arg in args {
                        self.clear_expr_types(arg);
                    }
                }
                fp_core::intrinsics::IntrinsicCallPayload::Format { template } => {
                    for arg in &mut template.args {
                        self.clear_expr_types(arg);
                    }
                    for kwarg in &mut template.kwargs {
                        self.clear_expr_types(&mut kwarg.value);
                    }
                }
            },
            ExprKind::Value(value) => match value.as_mut() {
                fp_core::ast::Value::Expr(inner) => self.clear_expr_types(inner.as_mut()),
                fp_core::ast::Value::Function(func) => self.clear_expr_types(func.body.as_mut()),
                _ => {}
            },
            _ => {}
        }
    }

    fn finalize(mut self, items: &mut Vec<Item>) -> Result<()> {
        self.apply_generated_functions(items)
    }

    fn apply_generated_functions(&mut self, items: &mut Vec<Item>) -> Result<()> {
        for (name, template) in &self.templates {
            if !self.specialized_bases.contains(name) {
                continue;
            }

            if let Some(item) = items.get_mut(template.item_index) {
                if let ItemKind::DefFunction(function) = item.kind_mut() {
                    let decl = ItemDeclFunction {
                        ty_annotation: function.ty_annotation.clone(),
                        name: function.name.clone(),
                        sig: function.sig.clone(),
                    };
                    let mut new_item = Item::new(ItemKind::DeclFunction(decl));
                    if let Some(ty) = &function.ty {
                        new_item.set_ty(Ty::Function(ty.clone()));
                    }
                    *item = new_item;
                }
            }
        }

        items.extend(self.generated.drain(..));
        Ok(())
    }
}
