use crate::{AstTypeInferencer, TypeVarId};
use fp_core::ast::*;
use fp_core::error::{Error, Result};
use fp_core::module::path::{PathPrefix, QualifiedPath};

fn is_std_task_future_ty(ty: &Ty) -> bool {
    let Ty::Expr(expr) = ty else {
        return false;
    };
    let ExprKind::Name(Name::ParameterPath(path)) = expr.kind() else {
        return false;
    };
    if path.segments.len() < 3 {
        return false;
    }
    let n = path.segments.len();
    path.segments[n - 3].ident.as_str() == "std"
        && path.segments[n - 2].ident.as_str() == "task"
        && path.segments[n - 1].ident.as_str() == "Future"
}

fn std_task_future_inner_ty(ty: &Ty) -> Option<Ty> {
    let Ty::Expr(expr) = ty else {
        return None;
    };
    let ExprKind::Name(Name::ParameterPath(path)) = expr.kind() else {
        return None;
    };
    if path.segments.len() < 3 {
        return None;
    }
    let n = path.segments.len();
    if path.segments[n - 3].ident.as_str() != "std"
        || path.segments[n - 2].ident.as_str() != "task"
        || path.segments[n - 1].ident.as_str() != "Future"
    {
        return None;
    }
    let future_seg = &path.segments[n - 1];
    if future_seg.args.len() != 1 {
        return None;
    }
    Some(future_seg.args[0].clone())
}

#[derive(Clone, Debug)]
pub(crate) struct TypeVar {
    pub(crate) kind: TypeVarKind,
}

#[derive(Clone, Debug)]
pub(crate) enum TypeVarKind {
    Unbound { level: usize },
    Link(TypeVarId),
    Bound(Ty),
}

#[derive(Clone, Debug)]
pub(crate) struct FunctionTerm {
    pub(crate) params: Vec<TypeVarId>,
    pub(crate) ret: TypeVarId,
}

fn primitive_ty(ty: &Ty) -> Option<TypePrimitive> {
    match ty {
        Ty::Primitive(primitive) => Some(*primitive),
        _ => None,
    }
}

impl<'ctx> AstTypeInferencer<'ctx> {
    pub(crate) fn bind_reference_term(&mut self, var: TypeVarId, inner: TypeVarId) {
        self.bind(
            var,
            Ty::Reference(TypeReference {
                ty: Box::new(Ty::infer_var(inner)),
                mutability: None,
                lifetime: None,
            }),
        );
    }

    pub(crate) fn bind_function_term(
        &mut self,
        var: TypeVarId,
        params: Vec<TypeVarId>,
        ret: TypeVarId,
    ) {
        self.bind(
            var,
            Ty::Function(TypeFunction {
                params: params.into_iter().map(Ty::infer_var).collect(),
                generics_params: Vec::new(),
                ret_ty: Some(Box::new(Ty::infer_var(ret))),
            }),
        );
    }

    pub(crate) fn bind_tuple_term(&mut self, var: TypeVarId, elements: Vec<TypeVarId>) {
        self.bind(
            var,
            Ty::Tuple(TypeTuple {
                types: elements.into_iter().map(Ty::infer_var).collect(),
            }),
        );
    }

    pub(crate) fn bind_slice_term(&mut self, var: TypeVarId, elem: TypeVarId) {
        self.bind(
            var,
            Ty::Slice(TypeSlice {
                elem: Box::new(Ty::infer_var(elem)),
            }),
        );
    }

    pub(crate) fn bind_vec_term(&mut self, var: TypeVarId, elem: TypeVarId) {
        self.bind(
            var,
            Ty::Vec(TypeVec {
                ty: Box::new(Ty::infer_var(elem)),
            }),
        );
    }

    pub(crate) fn bind_array_term(&mut self, var: TypeVarId, elem: TypeVarId, len: Option<BExpr>) {
        self.bind(
            var,
            Ty::Array(TypeArray {
                elem: Box::new(Ty::infer_var(elem)),
                len: len.unwrap_or_else(|| Expr::value(Value::int(0)).into()),
            }),
        );
    }

    pub(crate) fn reference_inner_from_ty(&mut self, ty: &Ty) -> Option<TypeVarId> {
        let Ty::Reference(reference) = ty else {
            return None;
        };
        match reference.ty.as_ref() {
            Ty::InferVar(infer) => Some(infer.id),
            other => self.type_from_ast_ty(other).ok(),
        }
    }

    pub(crate) fn function_term_from_ty(&mut self, ty: &Ty) -> Option<FunctionTerm> {
        match ty {
            Ty::Function(func) => {
                let mut params = Vec::with_capacity(func.params.len());
                for param in &func.params {
                    match param {
                        Ty::InferVar(infer) => params.push(infer.id),
                        other => params.push(self.type_from_ast_ty(other).ok()?),
                    }
                }
                let ret = match func.ret_ty.as_deref() {
                    Some(Ty::InferVar(infer)) => infer.id,
                    Some(other) => self.type_from_ast_ty(other).ok()?,
                    None => self.unit_type_var(),
                };
                Some(FunctionTerm { params, ret })
            }
            _ => None,
        }
    }

    pub(crate) fn lookup_struct(&self, path: &QualifiedPath) -> Option<TypeStruct> {
        self.struct_defs.get(path).cloned()
            .or_else(|| self.typing_ctx.env_ctx.find_struct(path).cloned())
    }

    fn lower_infer_vars_in_ty(
        &mut self,
        ty: Ty,
        mapping: &mut std::collections::HashMap<TypeVarId, u32>,
        next: &mut u32,
    ) -> Result<Ty> {
        Ok(match ty {
            Ty::InferVar(infer) => self.build_generalized_ty(infer.id, mapping, next)?,
            Ty::Tuple(tuple) => Ty::Tuple(TypeTuple {
                types: tuple
                    .types
                    .into_iter()
                    .map(|elem| self.lower_infer_vars_in_ty(elem, mapping, next))
                    .collect::<Result<Vec<_>>>()?,
            }),
            Ty::Function(function) => Ty::Function(TypeFunction {
                params: function
                    .params
                    .into_iter()
                    .map(|param| self.lower_infer_vars_in_ty(param, mapping, next))
                    .collect::<Result<Vec<_>>>()?,
                generics_params: function.generics_params,
                ret_ty: function
                    .ret_ty
                    .map(|ret| {
                        self.lower_infer_vars_in_ty(*ret, mapping, next)
                            .map(Box::new)
                    })
                    .transpose()?,
            }),
            Ty::TypeBinaryOp(op) => Ty::TypeBinaryOp(Box::new(TypeBinaryOp {
                kind: op.kind,
                lhs: Box::new(self.lower_infer_vars_in_ty(*op.lhs, mapping, next)?),
                rhs: Box::new(self.lower_infer_vars_in_ty(*op.rhs, mapping, next)?),
            })),
            Ty::Slice(slice) => Ty::Slice(TypeSlice {
                elem: Box::new(self.lower_infer_vars_in_ty(*slice.elem, mapping, next)?),
            }),
            Ty::Vec(vec) => Ty::Vec(TypeVec {
                ty: Box::new(self.lower_infer_vars_in_ty(*vec.ty, mapping, next)?),
            }),
            Ty::Array(array) => Ty::Array(TypeArray {
                elem: Box::new(self.lower_infer_vars_in_ty(*array.elem, mapping, next)?),
                len: array.len,
            }),
            Ty::Reference(reference) => Ty::Reference(TypeReference {
                ty: Box::new(self.lower_infer_vars_in_ty(*reference.ty, mapping, next)?),
                mutability: reference.mutability,
                lifetime: reference.lifetime,
            }),
            Ty::RawPtr(raw_ptr) => Ty::RawPtr(TypeRawPtr {
                ty: Box::new(self.lower_infer_vars_in_ty(*raw_ptr.ty, mapping, next)?),
                mutability: raw_ptr.mutability,
            }),
            other => other,
        })
    }

    fn resolve_infer_vars_in_ty(&mut self, ty: Ty) -> Result<Ty> {
        Ok(match ty {
            Ty::InferVar(infer) => self.resolve_to_ty(infer.id)?,
            Ty::Tuple(tuple) => Ty::Tuple(TypeTuple {
                types: tuple
                    .types
                    .into_iter()
                    .map(|elem| self.resolve_infer_vars_in_ty(elem))
                    .collect::<Result<Vec<_>>>()?,
            }),
            Ty::Function(function) => Ty::Function(TypeFunction {
                params: function
                    .params
                    .into_iter()
                    .map(|param| self.resolve_infer_vars_in_ty(param))
                    .collect::<Result<Vec<_>>>()?,
                generics_params: function.generics_params,
                ret_ty: function
                    .ret_ty
                    .map(|ret| self.resolve_infer_vars_in_ty(*ret).map(Box::new))
                    .transpose()?,
            }),
            Ty::TypeBinaryOp(op) => Ty::TypeBinaryOp(Box::new(TypeBinaryOp {
                kind: op.kind,
                lhs: Box::new(self.resolve_infer_vars_in_ty(*op.lhs)?),
                rhs: Box::new(self.resolve_infer_vars_in_ty(*op.rhs)?),
            })),
            Ty::Slice(slice) => Ty::Slice(TypeSlice {
                elem: Box::new(self.resolve_infer_vars_in_ty(*slice.elem)?),
            }),
            Ty::Vec(vec) => Ty::Vec(TypeVec {
                ty: Box::new(self.resolve_infer_vars_in_ty(*vec.ty)?),
            }),
            Ty::Array(array) => Ty::Array(TypeArray {
                elem: Box::new(self.resolve_infer_vars_in_ty(*array.elem)?),
                len: array.len,
            }),
            Ty::Reference(reference) => Ty::Reference(TypeReference {
                ty: Box::new(self.resolve_infer_vars_in_ty(*reference.ty)?),
                mutability: reference.mutability,
                lifetime: reference.lifetime,
            }),
            Ty::RawPtr(raw_ptr) => Ty::RawPtr(TypeRawPtr {
                ty: Box::new(self.resolve_infer_vars_in_ty(*raw_ptr.ty)?),
                mutability: raw_ptr.mutability,
            }),
            other => other,
        })
    }

    fn occurs_in_ty(&mut self, needle: TypeVarId, ty: &Ty) -> bool {
        match ty {
            Ty::InferVar(infer) => self.occurs_in(needle, infer.id),
            Ty::Tuple(tuple) => tuple
                .types
                .iter()
                .any(|elem| self.occurs_in_ty(needle, elem)),
            Ty::Function(function) => {
                function
                    .params
                    .iter()
                    .any(|param| self.occurs_in_ty(needle, param))
                    || function
                        .ret_ty
                        .as_ref()
                        .is_some_and(|ret| self.occurs_in_ty(needle, ret))
            }
            Ty::TypeBinaryOp(op) => {
                self.occurs_in_ty(needle, &op.lhs) || self.occurs_in_ty(needle, &op.rhs)
            }
            Ty::Slice(slice) => self.occurs_in_ty(needle, &slice.elem),
            Ty::Vec(vec) => self.occurs_in_ty(needle, &vec.ty),
            Ty::Array(array) => self.occurs_in_ty(needle, &array.elem),
            Ty::Reference(reference) => self.occurs_in_ty(needle, &reference.ty),
            Ty::RawPtr(raw_ptr) => self.occurs_in_ty(needle, &raw_ptr.ty),
            _ => false,
        }
    }

    fn bind_concrete_ty(&mut self, ty: Ty) -> TypeVarId {
        if let Ty::InferVar(infer) = ty {
            return infer.id;
        }
        let var = self.fresh_type_var();
        self.bind(var, ty);
        var
    }

    fn build_generic_arg_map(
        &self,
        params: &[GenericParam],
        args: &[Ty],
    ) -> std::collections::HashMap<String, Ty> {
        let mut mapping = std::collections::HashMap::new();
        for (param, arg) in params.iter().zip(args.iter()) {
            mapping.insert(param.name.as_str().to_string(), arg.clone());
        }
        mapping
    }

    fn ty_contains_generic_param(&self, ty: &Ty) -> bool {
        match ty {
            Ty::Expr(expr) => match expr.kind() {
                ExprKind::Name(name) => {
                    if let Some(key) = self.generic_name_from_locator(name) {
                        return self
                            .generic_scopes
                            .iter()
                            .rev()
                            .any(|scope| scope.contains(key));
                    }
                    false
                }
                _ => false,
            },
            Ty::Reference(reference) => self.ty_contains_generic_param(&reference.ty),
            Ty::RawPtr(ptr) => self.ty_contains_generic_param(&ptr.ty),
            Ty::Slice(slice) => self.ty_contains_generic_param(&slice.elem),
            Ty::Vec(vec) => self.ty_contains_generic_param(&vec.ty),
            Ty::Array(array) => self.ty_contains_generic_param(&array.elem),
            Ty::Tuple(tuple) => tuple
                .types
                .iter()
                .any(|elem| self.ty_contains_generic_param(elem)),
            Ty::TypeBinaryOp(op) => {
                self.ty_contains_generic_param(&op.lhs) || self.ty_contains_generic_param(&op.rhs)
            }
            Ty::Function(func) => {
                func.params
                    .iter()
                    .any(|param| self.ty_contains_generic_param(param))
                    || func
                        .ret_ty
                        .as_ref()
                        .map(|ret| self.ty_contains_generic_param(ret))
                        .unwrap_or(false)
            }
            _ => false,
        }
    }

    pub(crate) fn generic_name_from_locator<'a>(&self, name: &'a Name) -> Option<&'a str> {
        match name {
            Name::Ident(ident) => Some(ident.as_str()),
            Name::Path(path) if path.segments.len() == 1 => Some(path.segments[0].as_str()),
            Name::ParameterPath(path)
                if path.segments.len() == 1 && path.segments[0].args.is_empty() =>
            {
                Some(path.segments[0].ident.as_str())
            }
            _ => None,
        }
    }

    pub(crate) fn substitute_generic_ty(
        &self,
        ty: &Ty,
        mapping: &std::collections::HashMap<String, Ty>,
    ) -> Ty {
        match ty {
            Ty::Expr(expr) => {
                if let ExprKind::Name(name) = expr.kind() {
                    if let Some(key) = self.generic_name_from_locator(name) {
                        if let Some(replacement) = mapping.get(key) {
                            return replacement.clone();
                        }
                    }
                }
                ty.clone()
            }
            Ty::Reference(reference) => Ty::Reference(TypeReference {
                ty: Box::new(self.substitute_generic_ty(&reference.ty, mapping)),
                mutability: reference.mutability,
                lifetime: reference.lifetime.clone(),
            }),
            Ty::RawPtr(ptr) => Ty::RawPtr(TypeRawPtr {
                ty: Box::new(self.substitute_generic_ty(&ptr.ty, mapping)),
                mutability: ptr.mutability,
            }),
            Ty::Slice(slice) => Ty::Slice(TypeSlice {
                elem: Box::new(self.substitute_generic_ty(&slice.elem, mapping)),
            }),
            Ty::Vec(vec) => Ty::Vec(TypeVec {
                ty: Box::new(self.substitute_generic_ty(&vec.ty, mapping)),
            }),
            Ty::Array(array) => Ty::Array(TypeArray {
                elem: Box::new(self.substitute_generic_ty(&array.elem, mapping)),
                len: array.len.clone(),
            }),
            Ty::Tuple(tuple) => Ty::Tuple(TypeTuple {
                types: tuple
                    .types
                    .iter()
                    .map(|elem| self.substitute_generic_ty(elem, mapping))
                    .collect(),
            }),
            Ty::Struct(struct_ty) => {
                let mut cloned = struct_ty.clone();
                cloned.fields = cloned
                    .fields
                    .iter()
                    .map(|field| StructuralField {
                        name: field.name.clone(),
                        value: self.substitute_generic_ty(&field.value, mapping),
                    })
                    .collect();
                Ty::Struct(cloned)
            }
            Ty::Structural(structural) => Ty::Structural(TypeStructural {
                fields: structural
                    .fields
                    .iter()
                    .map(|field| StructuralField {
                        name: field.name.clone(),
                        value: self.substitute_generic_ty(&field.value, mapping),
                    })
                    .collect(),
            }),
            Ty::Enum(enum_ty) => {
                let mut cloned = enum_ty.clone();
                cloned.variants = cloned
                    .variants
                    .iter()
                    .map(|variant| EnumTypeVariant {
                        name: variant.name.clone(),
                        value: self.substitute_generic_ty(&variant.value, mapping),
                        discriminant: variant.discriminant.clone(),
                    })
                    .collect();
                Ty::Enum(cloned)
            }
            Ty::Function(func) => Ty::Function(TypeFunction {
                params: func
                    .params
                    .iter()
                    .map(|param| self.substitute_generic_ty(param, mapping))
                    .collect(),
                generics_params: func.generics_params.clone(),
                ret_ty: func
                    .ret_ty
                    .as_ref()
                    .map(|ret| Box::new(self.substitute_generic_ty(ret, mapping))),
            }),
            Ty::TypeBinaryOp(op) => Ty::TypeBinaryOp(Box::new(TypeBinaryOp {
                kind: op.kind,
                lhs: Box::new(self.substitute_generic_ty(&op.lhs, mapping)),
                rhs: Box::new(self.substitute_generic_ty(&op.rhs, mapping)),
            })),
            Ty::Quote(quote) => Ty::Quote(TypeQuote {
                span: quote.span,
                kind: quote.kind,
                item: quote.item,
                inner: quote
                    .inner
                    .as_ref()
                    .map(|inner| Box::new(self.substitute_generic_ty(inner, mapping))),
            }),
            _ => ty.clone(),
        }
    }

    pub(crate) fn apply_generic_args_to_enum(&self, enum_ty: &TypeEnum, args: &[Ty]) -> TypeEnum {
        let mapping = self.build_generic_arg_map(&enum_ty.generics_params, args);
        match self.substitute_generic_ty(&Ty::Enum(enum_ty.clone()), &mapping) {
            Ty::Enum(concrete) => concrete,
            _ => enum_ty.clone(),
        }
    }

    fn apply_generic_args_to_struct(&self, struct_ty: &TypeStruct, args: &[Ty]) -> TypeStruct {
        let mapping = self.build_generic_arg_map(&struct_ty.generics_params, args);
        match self.substitute_generic_ty(&Ty::Struct(struct_ty.clone()), &mapping) {
            Ty::Struct(concrete) => concrete,
            _ => struct_ty.clone(),
        }
    }

    pub(crate) fn generalize(&mut self, var: TypeVarId) -> Result<Ty> {
        let mut mapping = std::collections::HashMap::new();
        let mut next = 0u32;
        self.build_generalized_ty(var, &mut mapping, &mut next)
    }

    fn build_generalized_ty(
        &mut self,
        var: TypeVarId,
        mapping: &mut std::collections::HashMap<TypeVarId, u32>,
        next: &mut u32,
    ) -> Result<Ty> {
        let root = self.find(var);
        match self.type_vars[root].kind.clone() {
            TypeVarKind::Unbound { level } => {
                if level > self.current_level {
                    if let Some(idx) = mapping.get(&root) {
                        Ok(Ty::generic_var(*idx))
                    } else {
                        let idx = *next;
                        mapping.insert(root, idx);
                        *next += 1;
                        Ok(Ty::generic_var(idx))
                    }
                } else {
                    Err(self
                        .error_with_current_span("unresolved type variable during generalization"))
                }
            }
            TypeVarKind::Bound(Ty::ErrorType(_)) => {
                Err(self.error_with_current_span("error type variable during generalization"))
            }
            TypeVarKind::Bound(ty) => self.lower_infer_vars_in_ty(ty, mapping, next),
            TypeVarKind::Link(next_var) => self.build_generalized_ty(next_var, mapping, next),
        }
    }

    pub(crate) fn instantiate_scheme(&mut self, scheme: &Ty) -> TypeVarId {
        let mut mapping = std::collections::HashMap::new();
        self.instantiate_poly_ty(scheme, &mut mapping)
    }

    fn instantiate_poly_ty(
        &mut self,
        scheme: &Ty,
        mapping: &mut std::collections::HashMap<u32, TypeVarId>,
    ) -> TypeVarId {
        match scheme {
            Ty::GenericVar(generic) => {
                if let Some(var) = mapping.get(&generic.index) {
                    *var
                } else {
                    let var = self.fresh_type_var();
                    mapping.insert(generic.index, var);
                    var
                }
            }
            Ty::Primitive(prim) => {
                let var = self.fresh_type_var();
                self.bind(var, Ty::Primitive(*prim));
                var
            }
            Ty::Unit(_) => {
                let var = self.fresh_type_var();
                self.bind(var, Ty::Unit(TypeUnit));
                var
            }
            Ty::Nothing(_) => {
                let var = self.fresh_type_var();
                self.bind(var, Ty::Nothing(TypeNothing));
                var
            }
            Ty::Any(_) => {
                let var = self.fresh_type_var();
                self.bind(var, Ty::Any(TypeAny));
                var
            }
            Ty::Struct(struct_ty) => {
                let var = self.fresh_type_var();
                self.bind(var, Ty::Struct(struct_ty.clone()));
                var
            }
            Ty::Structural(structural) => {
                let var = self.fresh_type_var();
                self.bind(var, Ty::Structural(structural.clone()));
                var
            }
            Ty::Enum(enum_ty) => {
                let var = self.fresh_type_var();
                self.bind(var, Ty::Enum(enum_ty.clone()));
                var
            }
            Ty::TypeBinaryOp(op) if op.kind == TypeBinaryOpKind::Union => {
                let lhs_var = self.instantiate_poly_ty(&op.lhs, mapping);
                let rhs_var = self.instantiate_poly_ty(&op.rhs, mapping);
                let var = self.fresh_type_var();
                self.bind(
                    var,
                    Ty::TypeBinaryOp(Box::new(TypeBinaryOp {
                        kind: TypeBinaryOpKind::Union,
                        lhs: Box::new(Ty::infer_var(lhs_var)),
                        rhs: Box::new(Ty::infer_var(rhs_var)),
                    })),
                );
                var
            }
            Ty::Unknown(_) => {
                let var = self.fresh_type_var();
                self.bind(var, Ty::Unknown(TypeUnknown));
                var
            }
            Ty::Tuple(elements) => {
                let mut vars = Vec::new();
                for elem in &elements.types {
                    vars.push(self.instantiate_poly_ty(elem, mapping));
                }
                let var = self.fresh_type_var();
                self.bind(
                    var,
                    Ty::Tuple(TypeTuple {
                        types: vars.into_iter().map(Ty::infer_var).collect(),
                    }),
                );
                var
            }
            Ty::Function(function) => {
                let param_vars: Vec<_> = function
                    .params
                    .iter()
                    .map(|param| self.instantiate_poly_ty(param, mapping))
                    .collect();
                let ret_var = function
                    .ret_ty
                    .as_ref()
                    .map(|ret| self.instantiate_poly_ty(ret, mapping))
                    .unwrap_or_else(|| self.unit_type_var());
                let var = self.fresh_type_var();
                self.bind_function_term(var, param_vars, ret_var);
                var
            }
            Ty::Slice(elem) => {
                let elem_var = self.instantiate_poly_ty(&elem.elem, mapping);
                let var = self.fresh_type_var();
                self.bind(
                    var,
                    Ty::Slice(TypeSlice {
                        elem: Box::new(Ty::infer_var(elem_var)),
                    }),
                );
                var
            }
            Ty::Vec(elem) => {
                let elem_var = self.instantiate_poly_ty(&elem.ty, mapping);
                let var = self.fresh_type_var();
                self.bind(
                    var,
                    Ty::Vec(TypeVec {
                        ty: Box::new(Ty::infer_var(elem_var)),
                    }),
                );
                var
            }
            Ty::Array(array) => {
                let elem_var = self.instantiate_poly_ty(&array.elem, mapping);
                let var = self.fresh_type_var();
                self.bind(
                    var,
                    Ty::Array(TypeArray {
                        elem: Box::new(Ty::infer_var(elem_var)),
                        len: array.len.clone(),
                    }),
                );
                var
            }
            Ty::Reference(elem) => {
                let elem_var = self.instantiate_poly_ty(&elem.ty, mapping);
                let var = self.fresh_type_var();
                self.bind(
                    var,
                    Ty::Reference(TypeReference {
                        ty: Box::new(Ty::infer_var(elem_var)),
                        mutability: None,
                        lifetime: None,
                    }),
                );
                var
            }
            Ty::RawPtr(elem) => {
                let elem_var = self.instantiate_poly_ty(&elem.ty, mapping);
                let var = self.fresh_type_var();
                self.bind(
                    var,
                    Ty::RawPtr(TypeRawPtr {
                        ty: Box::new(Ty::infer_var(elem_var)),
                        mutability: elem.mutability,
                    }),
                );
                var
            }
            _ => self
                .type_from_ast_ty(scheme)
                .unwrap_or_else(|_| self.error_type_var()),
        }
    }

    pub(crate) fn fresh_type_var(&mut self) -> TypeVarId {
        let id = self.type_vars.len();
        self.type_vars.push(TypeVar {
            kind: TypeVarKind::Unbound {
                level: self.current_level,
            },
        });
        id
    }

    pub(crate) fn unit_type_var(&mut self) -> TypeVarId {
        let var = self.fresh_type_var();
        self.bind(var, Ty::Unit(TypeUnit));
        var
    }

    pub(crate) fn nothing_type_var(&mut self) -> TypeVarId {
        let var = self.fresh_type_var();
        self.bind(var, Ty::Nothing(TypeNothing));
        var
    }

    pub(crate) fn bind(&mut self, var: TypeVarId, ty: Ty) {
        let root = self.find(var);
        self.type_vars[root].kind = TypeVarKind::Bound(ty);
    }

    pub(crate) fn bind_error(&mut self, var: TypeVarId) {
        self.bind(var, Ty::ErrorType(TypeError));
    }

    pub(crate) fn find(&mut self, var: TypeVarId) -> TypeVarId {
        match self.type_vars[var].kind.clone() {
            TypeVarKind::Link(next) => {
                let root = self.find(next);
                self.type_vars[var].kind = TypeVarKind::Link(root);
                root
            }
            _ => var,
        }
    }

    pub(crate) fn unify(&mut self, a: TypeVarId, b: TypeVarId) -> Result<()> {
        let a_root = self.find(a);
        let b_root = self.find(b);
        if a_root == b_root {
            return Ok(());
        }
        let a_prim = match &self.type_vars[a_root].kind {
            TypeVarKind::Bound(ty) => primitive_ty(ty),
            _ => None,
        };
        let b_prim = match &self.type_vars[b_root].kind {
            TypeVarKind::Bound(ty) => primitive_ty(ty),
            _ => None,
        };
        if let (Some(TypePrimitive::Int(int_a)), Some(TypePrimitive::Int(int_b))) = (a_prim, b_prim)
        {
            return if int_a == int_b {
                Ok(())
            } else if self.literal_ints.remove(&a_root) {
                self.type_vars[a_root].kind = TypeVarKind::Link(b_root);
                Ok(())
            } else if self.literal_ints.remove(&b_root) {
                self.type_vars[b_root].kind = TypeVarKind::Link(a_root);
                Ok(())
            } else if Self::same_width(&int_a, &int_b) {
                self.type_vars[a_root].kind = TypeVarKind::Link(b_root);
                Ok(())
            } else {
                Err(self.error_with_current_span(format!(
                    "primitive type mismatch: {} vs {}",
                    int_a, int_b
                )))
            };
        }
        match (
            self.type_vars[a_root].kind.clone(),
            self.type_vars[b_root].kind.clone(),
        ) {
            (TypeVarKind::Unbound { .. }, TypeVarKind::Unbound { .. }) => {
                self.type_vars[a_root].kind = TypeVarKind::Link(b_root);
                self.merge_trait_bounds_into(b_root, a_root, true);
                Ok(())
            }
            (TypeVarKind::Unbound { .. }, TypeVarKind::Bound(ty)) => {
                if self.occurs_in_ty(a_root, &ty) {
                    return Err(self.error_with_current_span("occurs check failed"));
                }
                self.type_vars[a_root].kind = TypeVarKind::Bound(ty);
                self.merge_trait_bounds_into(a_root, b_root, false);
                Ok(())
            }
            (TypeVarKind::Bound(ty), TypeVarKind::Unbound { .. }) => {
                if self.occurs_in_ty(b_root, &ty) {
                    return Err(self.error_with_current_span("occurs check failed"));
                }
                self.type_vars[b_root].kind = TypeVarKind::Bound(ty);
                self.merge_trait_bounds_into(b_root, a_root, false);
                Ok(())
            }
            (TypeVarKind::Bound(Ty::ErrorType(_)), _)
            | (_, TypeVarKind::Bound(Ty::ErrorType(_))) => Ok(()),
            (TypeVarKind::Bound(ty_a), TypeVarKind::Bound(ty_b)) => {
                self.unify_concrete_tys(ty_a, ty_b)
            }
            (TypeVarKind::Link(next), _) => self.unify(next, b_root),
            (_, TypeVarKind::Link(next)) => self.unify(a_root, next),
        }
    }

    fn same_width(a: &fp_core::ast::TypeInt, b: &fp_core::ast::TypeInt) -> bool {
        use fp_core::ast::TypeInt::*;
        matches!(
            (a, b),
            (I64, U64)
                | (U64, I64)
                | (I32, U32)
                | (U32, I32)
                | (I16, U16)
                | (U16, I16)
                | (I8, U8)
                | (U8, I8)
                | (I128, U128)
                | (U128, I128)
        )
    }

    fn merge_trait_bounds_into(
        &mut self,
        target: TypeVarId,
        source: TypeVarId,
        remove_source: bool,
    ) {
        let bounds = if remove_source {
            self.generic_trait_bounds.remove(&source)
        } else {
            self.generic_trait_bounds.get(&source).cloned()
        };
        let Some(bounds) = bounds else {
            return;
        };
        if let Some(existing) = self.generic_trait_bounds.get_mut(&target) {
            for bound in bounds {
                if !existing.contains(&bound) {
                    existing.push(bound);
                }
            }
        } else {
            self.generic_trait_bounds.insert(target, bounds);
        }
    }

    fn occurs_in(&mut self, needle: TypeVarId, haystack: TypeVarId) -> bool {
        let root = self.find(haystack);
        if root == needle {
            return true;
        }
        match self.type_vars[root].kind.clone() {
            TypeVarKind::Bound(ty) => self.occurs_in_ty(needle, &ty),
            TypeVarKind::Link(next) => self.occurs_in(needle, next),
            _ => false,
        }
    }

    fn unify_concrete_tys(&mut self, a: Ty, b: Ty) -> Result<()> {
        match (a, b) {
            (Ty::InferVar(a), Ty::InferVar(b)) => self.unify(a.id, b.id),
            (Ty::InferVar(infer), other) | (other, Ty::InferVar(infer)) => {
                let other_var = self.bind_concrete_ty(other);
                self.unify(infer.id, other_var)
            }
            (Ty::Tuple(a_tuple), Ty::Tuple(b_tuple)) => {
                if a_tuple.types.len() != b_tuple.types.len() {
                    return Err(self.error_with_current_span("tuple length mismatch"));
                }
                for (a_elem, b_elem) in a_tuple.types.into_iter().zip(b_tuple.types.into_iter()) {
                    let a_var = self.bind_concrete_ty(a_elem);
                    let b_var = self.bind_concrete_ty(b_elem);
                    self.unify(a_var, b_var)?;
                }
                Ok(())
            }
            (Ty::Function(a_func), Ty::Function(b_func)) => {
                if a_func.params.len() != b_func.params.len() {
                    return Err(self.error_with_current_span("function arity mismatch"));
                }
                for (a_param, b_param) in a_func.params.into_iter().zip(b_func.params.into_iter()) {
                    let a_var = self.bind_concrete_ty(a_param);
                    let b_var = self.bind_concrete_ty(b_param);
                    self.unify(a_var, b_var)?;
                }
                match (a_func.ret_ty, b_func.ret_ty) {
                    (Some(a_ret), Some(b_ret)) => {
                        let a_var = self.bind_concrete_ty(*a_ret);
                        let b_var = self.bind_concrete_ty(*b_ret);
                        self.unify(a_var, b_var)
                    }
                    (None, None) => Ok(()),
                    _ => Err(self.error_with_current_span("function return mismatch")),
                }
            }
            (Ty::TypeBinaryOp(a_op), Ty::TypeBinaryOp(b_op))
                if a_op.kind == TypeBinaryOpKind::Union && b_op.kind == TypeBinaryOpKind::Union =>
            {
                let a_lhs = self.bind_concrete_ty(*a_op.lhs);
                let b_lhs = self.bind_concrete_ty(*b_op.lhs);
                self.unify(a_lhs, b_lhs)?;
                let a_rhs = self.bind_concrete_ty(*a_op.rhs);
                let b_rhs = self.bind_concrete_ty(*b_op.rhs);
                self.unify(a_rhs, b_rhs)
            }
            (Ty::Slice(a_slice), Ty::Slice(b_slice)) => {
                let a_var = self.bind_concrete_ty(*a_slice.elem);
                let b_var = self.bind_concrete_ty(*b_slice.elem);
                self.unify(a_var, b_var)
            }
            (Ty::Vec(a_vec), Ty::Vec(b_vec)) => {
                let a_var = self.bind_concrete_ty(*a_vec.ty);
                let b_var = self.bind_concrete_ty(*b_vec.ty);
                self.unify(a_var, b_var)
            }
            (Ty::Array(a_arr), Ty::Array(b_arr)) => {
                let a_var = self.bind_concrete_ty(*a_arr.elem);
                let b_var = self.bind_concrete_ty(*b_arr.elem);
                self.unify(a_var, b_var)
            }
            (Ty::Reference(a_ref), Ty::Reference(b_ref)) => {
                let a_var = self.bind_concrete_ty(*a_ref.ty);
                let b_var = self.bind_concrete_ty(*b_ref.ty);
                self.unify(a_var, b_var)
            }
            (Ty::RawPtr(a_ptr), Ty::RawPtr(b_ptr)) => {
                if matches!((a_ptr.mutability, b_ptr.mutability), (Some(a), Some(b)) if a != b) {
                    return Err(self.error_with_current_span("raw pointer mutability mismatch"));
                }
                let a_var = self.bind_concrete_ty(*a_ptr.ty);
                let b_var = self.bind_concrete_ty(*b_ptr.ty);
                self.unify(a_var, b_var)
            }
            (Ty::Struct(sa), Ty::Struct(sb)) => {
                if sa == sb {
                    Ok(())
                } else if sa.name == sb.name {
                    self.unify_struct_fields(&sa, &sb)
                } else {
                    Err(self.error_with_current_span(format!(
                        "struct type mismatch: {} vs {}",
                        sa.name, sb.name
                    )))
                }
            }
            (Ty::Structural(sa), Ty::Structural(sb)) => {
                if sa == sb {
                    Ok(())
                } else {
                    Err(self.error_with_current_span("structural type mismatch"))
                }
            }
            (Ty::Enum(ae), Ty::Enum(be)) => {
                let ae_name = ae.name.as_str();
                let be_name = be.name.as_str();
                let ae_tail = ae_name.rsplit("::").next().unwrap_or(ae_name);
                let be_tail = be_name.rsplit("::").next().unwrap_or(be_name);
                if ae == be {
                    Ok(())
                } else if ae_tail == be_tail {
                    match self.unify_enum_variants(&ae, &be) {
                        Ok(()) => Ok(()),
                        Err(err) => {
                            let mut resolved = false;
                            let mut resolved_a = ae.clone();
                            let mut resolved_b = be.clone();
                            if (resolved_a.variants.is_empty() || resolved_b.variants.is_empty())
                                && self.lookup_enum_def_by_name(ae_tail).is_some()
                            {
                                if let Some((_, def)) = self.lookup_enum_def_by_name(ae_tail) {
                                    if resolved_a.variants.is_empty() {
                                        resolved_a = def.clone();
                                        resolved = true;
                                    }
                                    if resolved_b.variants.is_empty() {
                                        resolved_b = def;
                                        resolved = true;
                                    }
                                }
                            }
                            if resolved {
                                self.unify_enum_variants(&resolved_a, &resolved_b)
                            } else if !ae.generics_params.is_empty()
                                || !be.generics_params.is_empty()
                            {
                                Ok(())
                            } else {
                                Err(err)
                            }
                        }
                    }
                } else {
                    Err(self.error_with_current_span("enum type mismatch"))
                }
            }
            (left, right) if is_std_task_future_ty(&left) && is_std_task_future_ty(&right) => {
                let left_inner = std_task_future_inner_ty(&left);
                let right_inner = std_task_future_inner_ty(&right);
                if let (Some(left_inner), Some(right_inner)) = (left_inner, right_inner) {
                    if matches!(left_inner, Ty::Nothing(_)) || matches!(right_inner, Ty::Nothing(_))
                    {
                        Ok(())
                    } else {
                        let left_var = self.type_from_ast_ty(&left_inner)?;
                        let right_var = self.type_from_ast_ty(&right_inner)?;
                        self.unify(left_var, right_var)
                    }
                } else {
                    Ok(())
                }
            }
            (Ty::Struct(struct_ty), custom)
                if struct_ty.name.as_str() == "Future" && is_std_task_future_ty(&custom) =>
            {
                Ok(())
            }
            (custom, Ty::Struct(struct_ty))
                if is_std_task_future_ty(&custom) && struct_ty.name.as_str() == "Future" =>
            {
                Ok(())
            }
            (Ty::Nothing(_), _) | (_, Ty::Nothing(_)) => Ok(()),
            (Ty::Type(a), Ty::Type(b)) => {
                match (&a.inner, &b.inner) {
                    (None, None) => Ok(()),
                    (None, Some(_)) | (Some(_), None) => Ok(()),
                    (Some(a_inner), Some(b_inner)) => {
                        let a_var = self.bind_concrete_ty((**a_inner).clone());
                        let b_var = self.bind_concrete_ty((**b_inner).clone());
                        self.unify(a_var, b_var)
                    }
                }
            }
            (left, right) if left == right || quote_item_compatible(&left, &right) => Ok(()),
            (left, right) => Err(self.error_with_current_span(format!(
                "concrete type mismatch: {} vs {}{}",
                left,
                right,
                self.easy_fix_hint_for_mismatch(&left, &right)
            ))),
        }
    }

    fn easy_fix_hint_for_mismatch(&self, left: &Ty, right: &Ty) -> String {
        match (left, right) {
            (Ty::Reference(reference), other) if reference.ty.as_ref() == other => {
                " (hint: references do not coerce implicitly here; add `&`/`&mut` on the value or remove the reference annotation)".to_string()
            }
            (other, Ty::Reference(reference)) if other == reference.ty.as_ref() => {
                " (hint: references do not coerce implicitly here; add `&`/`&mut` on the value or remove the reference annotation)".to_string()
            }
            (Ty::Reference(reference), Ty::Primitive(TypePrimitive::String))
                if matches!(reference.ty.as_ref(), Ty::Primitive(TypePrimitive::String)) =>
            {
                " (hint: use `str` consistently instead of mixing `str` and `&str`)".to_string()
            }
            (Ty::Primitive(TypePrimitive::String), Ty::Reference(reference))
                if matches!(reference.ty.as_ref(), Ty::Primitive(TypePrimitive::String)) =>
            {
                " (hint: use `str` consistently instead of mixing `str` and `&str`)".to_string()
            }
            (Ty::Slice(_), Ty::Vec(_)) | (Ty::Vec(_), Ty::Slice(_)) => {
                " (hint: Vec and Slice are distinct under the strict solver; declare the exact collection type you want)".to_string()
            }
            _ => String::new(),
        }
    }

    fn unify_struct_fields(&mut self, sa: &TypeStruct, sb: &TypeStruct) -> Result<()> {
        if sa.fields.len() != sb.fields.len() {
            return Err(self.error_with_current_span(format!(
                "struct type mismatch: {} vs {}",
                sa.name, sb.name
            )));
        }
        for field in &sa.fields {
            let Some(other) = sb.fields.iter().find(|f| f.name == field.name) else {
                return Err(self.error_with_current_span(format!(
                    "struct type mismatch: {} vs {}",
                    sa.name, sb.name
                )));
            };
            let a_var = self.type_from_ast_ty(&field.value)?;
            let b_var = self.type_from_ast_ty(&other.value)?;
            self.unify(a_var, b_var)?;
        }
        Ok(())
    }

    fn unify_enum_variants(&mut self, ae: &TypeEnum, be: &TypeEnum) -> Result<()> {
        if ae.variants.len() != be.variants.len() {
            return Err(self.error_with_current_span("enum type mismatch"));
        }
        self.enter_scope();
        let mut registered: Vec<(String, TypeVarId)> = Vec::new();
        for param in ae.generics_params.iter().chain(be.generics_params.iter()) {
            let name = param.name.as_str();
            let var = if let Some((_, var)) = registered.iter().find(|(n, _)| n == name) {
                *var
            } else {
                let var = self.register_generic_param(name);
                registered.push((name.to_string(), var));
                var
            };
            let bounds = Self::extract_trait_bounds(&param.bounds);
            if !bounds.is_empty() {
                if let Some(existing) = self.generic_trait_bounds.get_mut(&var) {
                    existing.extend(bounds);
                } else {
                    self.generic_trait_bounds.insert(var, bounds);
                }
            }
        }
        let result = (|| {
            for variant in &ae.variants {
                let Some(other) = be.variants.iter().find(|v| v.name == variant.name) else {
                    return Err(self.error_with_current_span("enum type mismatch"));
                };
                let a_var = self.type_from_ast_ty(&variant.value)?;
                let b_var = self.type_from_ast_ty(&other.value)?;
                self.unify(a_var, b_var)?;
            }
            Ok(())
        })();
        self.exit_scope();
        result
    }

    pub(crate) fn resolve_to_ty(&mut self, var: TypeVarId) -> Result<Ty> {
        let root = self.find(var);
        match self.type_vars[root].kind.clone() {
            TypeVarKind::Unbound { .. } => {
                Err(self.error_with_current_span("unresolved type variable"))
            }
            TypeVarKind::Bound(Ty::ErrorType(_)) => {
                Err(self.error_with_current_span("error type variable"))
            }
            TypeVarKind::Bound(ty) => self.resolve_infer_vars_in_ty(ty),
            TypeVarKind::Link(next) => self.resolve_to_ty(next),
        }
    }

    pub(crate) fn type_from_ast_ty(&mut self, ty: &Ty) -> Result<TypeVarId> {
        let var = self.fresh_type_var();
        match ty {
            Ty::Primitive(prim) => self.bind(var, Ty::Primitive(*prim)),
            Ty::Unit(_) => self.bind(var, Ty::Unit(TypeUnit)),
            Ty::GenericVar(_) => return Ok(var),
            Ty::Nothing(_) => self.bind(var, Ty::Nothing(TypeNothing)),
            Ty::Any(_) => self.bind(var, Ty::Any(TypeAny)),
            Ty::ErrorType(_) => self.bind_error(var),
            Ty::InferVar(infer) => return Ok(infer.id),
            Ty::Wildcard(_) => {
                let var = self.fresh_type_var();
                return Ok(var);
            }
            Ty::TypeBinaryOp(op) => {
                let op = op.as_ref();
                match op.kind {
                    TypeBinaryOpKind::Add => {
                        // Resolve both operand types first so that aliases
                        // and other indirections are taken into account.
                        let lhs_var = self.type_from_ast_ty(&op.lhs)?;
                        let rhs_var = self.type_from_ast_ty(&op.rhs)?;
                        let lhs_ty = self.resolve_to_ty(lhs_var)?;
                        let rhs_ty = self.resolve_to_ty(rhs_var)?;

                        let lhs_fields = match lhs_ty {
                            Ty::Struct(ref s) => s.fields.clone(),
                            Ty::Structural(ref st) => st.fields.clone(),
                            _ => {
                                // Unsupported operand kinds fall back to a
                                // symbolic custom type for now.
                                self.bind(var, ty.clone());
                                return Ok(var);
                            }
                        };
                        let rhs_fields = match rhs_ty {
                            Ty::Struct(ref s) => s.fields.clone(),
                            Ty::Structural(ref st) => st.fields.clone(),
                            _ => {
                                self.bind(var, ty.clone());
                                return Ok(var);
                            }
                        };

                        // Merge fields, requiring that any overlapping
                        // names have identical types. When both sides are
                        // compatible, produce a structural type.
                        let mut merged = lhs_fields;
                        for rhs_field in rhs_fields {
                            if let Some(existing) = merged
                                .iter()
                                .find(|f| f.name.as_str() == rhs_field.name.as_str())
                            {
                                let existing_var = self.type_from_ast_ty(&existing.value)?;
                                let rhs_var = self.type_from_ast_ty(&rhs_field.value)?;
                                if self.unify(existing_var, rhs_var).is_err() {
                                    return Err(Error::from(format!(
                                        "cannot merge struct fields: field '{}' has incompatible types",
                                        rhs_field.name
                                    )));
                                }
                            } else {
                                merged.push(rhs_field);
                            }
                        }
                        self.bind(var, Ty::Structural(TypeStructural { fields: merged }));
                    }
                    TypeBinaryOpKind::Intersect => {
                        let lhs_var = self.type_from_ast_ty(&op.lhs)?;
                        let rhs_var = self.type_from_ast_ty(&op.rhs)?;
                        let lhs_ty = self.resolve_to_ty(lhs_var)?;
                        let rhs_ty = self.resolve_to_ty(rhs_var)?;

                        let lhs_fields = match lhs_ty {
                            Ty::Struct(ref s) => s.fields.clone(),
                            Ty::Structural(ref st) => st.fields.clone(),
                            _ => {
                                self.bind(var, ty.clone());
                                return Ok(var);
                            }
                        };
                        let rhs_fields = match rhs_ty {
                            Ty::Struct(ref s) => s.fields.clone(),
                            Ty::Structural(ref st) => st.fields.clone(),
                            _ => {
                                self.bind(var, ty.clone());
                                return Ok(var);
                            }
                        };

                        let mut merged = Vec::new();
                        for lhs_field in lhs_fields {
                            if let Some(rhs_field) = rhs_fields
                                .iter()
                                .find(|f| f.name.as_str() == lhs_field.name.as_str())
                            {
                                let lhs_var = self.type_from_ast_ty(&lhs_field.value)?;
                                let rhs_var = self.type_from_ast_ty(&rhs_field.value)?;
                                if self.unify(lhs_var, rhs_var).is_err() {
                                    return Err(Error::from(format!(
                                        "cannot intersect struct fields: field '{}' has incompatible types",
                                        lhs_field.name
                                    )));
                                }
                                merged.push(lhs_field.clone());
                            }
                        }

                        self.bind(var, Ty::Structural(TypeStructural { fields: merged }));
                    }
                    TypeBinaryOpKind::Union => {
                        let lhs_var = self.type_from_ast_ty(&op.lhs)?;
                        let rhs_var = self.type_from_ast_ty(&op.rhs)?;
                        self.bind(
                            var,
                            Ty::TypeBinaryOp(Box::new(TypeBinaryOp {
                                kind: TypeBinaryOpKind::Union,
                                lhs: Box::new(Ty::infer_var(lhs_var)),
                                rhs: Box::new(Ty::infer_var(rhs_var)),
                            })),
                        );
                    }
                    TypeBinaryOpKind::Subtract => {
                        let lhs_var = self.type_from_ast_ty(&op.lhs)?;
                        let rhs_var = self.type_from_ast_ty(&op.rhs)?;
                        let lhs_ty = self.resolve_to_ty(lhs_var)?;
                        let rhs_ty = self.resolve_to_ty(rhs_var)?;

                        let lhs_fields = match lhs_ty {
                            Ty::Struct(ref s) => s.fields.clone(),
                            Ty::Structural(ref st) => st.fields.clone(),
                            _ => {
                                self.bind(var, ty.clone());
                                return Ok(var);
                            }
                        };
                        let rhs_fields = match rhs_ty {
                            Ty::Struct(ref s) => s.fields.clone(),
                            Ty::Structural(ref st) => st.fields.clone(),
                            _ => {
                                self.bind(var, ty.clone());
                                return Ok(var);
                            }
                        };

                        let to_remove: std::collections::HashSet<String> = rhs_fields
                            .iter()
                            .map(|f| f.name.as_str().to_string())
                            .collect();

                        let merged: Vec<StructuralField> = lhs_fields
                            .into_iter()
                            .filter(|f| !to_remove.contains(f.name.as_str()))
                            .collect();

                        self.bind(var, Ty::Structural(TypeStructural { fields: merged }));
                    }
                }
            }
            Ty::AnyBox(_) => {
                // Treat AnyBox payloads as fully dynamic until a specific handler exists.
                self.bind_error(var);
            }
            Ty::TokenStream(_) => {
                self.bind(var, ty.clone());
            }
            Ty::Struct(struct_ty) => {
                self.insert_struct_def(&struct_ty.name, struct_ty.clone());
                self.bind(var, Ty::Struct(struct_ty.clone()));
            }
            Ty::Structural(structural) => self.bind(var, Ty::Structural(structural.clone())),
            Ty::Enum(enum_ty) => self.bind(var, Ty::Enum(enum_ty.clone())),
            Ty::Value(value_ty) => {
                let resolved = match value_ty.value.as_ref() {
                    Value::Int(_) => Ty::Primitive(TypePrimitive::Int(TypeInt::I64)),
                    Value::Bool(_) => Ty::Primitive(TypePrimitive::Bool),
                    Value::Decimal(_) => Ty::Primitive(TypePrimitive::Decimal(DecimalType::F64)),
                    Value::String(_) => Ty::Reference(TypeReference {
                        ty: Box::new(Ty::Primitive(TypePrimitive::String)),
                        mutability: None,
                        lifetime: None,
                    }),
                    Value::Char(_) => Ty::Primitive(TypePrimitive::Char),
                    Value::Unit(_) => Ty::Unit(TypeUnit),
                    Value::Null(_) | Value::None(_) => Ty::Nothing(TypeNothing),
                    _ => ty.clone(),
                };
                self.bind(var, resolved);
            }
            Ty::Type(tt) => {
                match &tt.inner {
                    None => {
                        self.bind(var, Ty::Type(TypeType { span: tt.span, inner: None }));
                    }
                    Some(inner) if matches!(inner.as_ref(), Ty::Wildcard(_)) => {
                        let inner_var = self.fresh_type_var();
                        self.bind(var, Ty::Type(TypeType {
                            span: tt.span,
                            inner: Some(Box::new(Ty::InferVar(TypeInferVar { id: inner_var }))),
                        }));
                    }
                    Some(inner_ty) => {
                        let inner_var = self.type_from_ast_ty(inner_ty)?;
                        let resolved = self.resolve_to_ty(inner_var)?;
                        self.bind(var, Ty::Type(TypeType {
                            span: tt.span,
                            inner: Some(Box::new(resolved)),
                        }));
                    }
                }
            }
            Ty::RequestedType(_) => {
                self.bind(var, ty.clone());
            }
            Ty::TypeBounds(_) => {
                // Higher-ranked or bounded types are treated as opaque for now.
                self.bind_error(var);
            }
            // No Ty::Custom in current AST types; treat all remaining cases via fallback below
            Ty::Unknown(_) => self.bind(var, Ty::Unknown(TypeUnknown)),
            Ty::Tuple(tuple) => {
                let mut vars = Vec::new();
                for elem in &tuple.types {
                    vars.push(self.type_from_ast_ty(elem)?);
                }
                self.bind(
                    var,
                    Ty::Tuple(TypeTuple {
                        types: vars.into_iter().map(Ty::infer_var).collect(),
                    }),
                );
            }
            Ty::Reference(r) => {
                let inner = self.type_from_ast_ty(&r.ty)?;
                self.bind(
                    var,
                    Ty::Reference(TypeReference {
                        ty: Box::new(Ty::infer_var(inner)),
                        mutability: r.mutability,
                        lifetime: r.lifetime.clone(),
                    }),
                );
            }
            Ty::RawPtr(r) => {
                let inner = self.type_from_ast_ty(&r.ty)?;
                self.bind(
                    var,
                    Ty::RawPtr(TypeRawPtr {
                        ty: Box::new(Ty::infer_var(inner)),
                        mutability: r.mutability,
                    }),
                );
            }
            Ty::Slice(s) => {
                let inner = self.type_from_ast_ty(&s.elem)?;
                self.bind(
                    var,
                    Ty::Slice(TypeSlice {
                        elem: Box::new(Ty::infer_var(inner)),
                    }),
                );
            }
            Ty::Vec(v) => {
                let inner = self.type_from_ast_ty(&v.ty)?;
                self.bind(
                    var,
                    Ty::Vec(TypeVec {
                        ty: Box::new(Ty::infer_var(inner)),
                    }),
                );
            }
            Ty::Array(array_ty) => {
                let elem_var = self.type_from_ast_ty(&array_ty.elem)?;
                self.bind(
                    var,
                    Ty::Array(TypeArray {
                        elem: Box::new(Ty::infer_var(elem_var)),
                        len: array_ty.len.clone(),
                    }),
                );
            }
            Ty::Quote(_) => {
                // Quote tokens are currently opaque to the typer.
                self.bind(var, ty.clone());
            }
            Ty::ConstBlock(block) => {
                let mut inner = (*block.expr).clone();
                
                let var = self.infer_expr(&mut inner)?;
                return Ok(var);
            }
            Ty::Expr(expr) => {
                if let ExprKind::Value(value) = expr.kind() {
                    if let Value::Type(ty) = value.as_ref() {
                        return self.type_from_ast_ty(ty);
                    }
                    if matches!(value.as_ref(), Value::Unit(_)) {
                        return Ok(var);
                    }
                }
                // Handle path-like type expressions (e.g., i64, bool, usize, str).
                if let ExprKind::Name(loc) = expr.kind() {
                    if self.check_unimplemented_locator(loc) {
                        return Ok(self.error_type_var());
                    }
                    if let Some((key_var, value_var)) = self.hashmap_args_from_locator(loc)? {
                        let map_var = self.fresh_type_var();
                    if let Some(key) = self.resolve_locator_key(loc) {
                            if let Some(struct_ty) = self.lookup_struct(&key) {
                                self.bind(map_var, Ty::Struct(struct_ty.clone()));
                            } else if let Some(s) = self.typing_ctx.env_ctx.find_struct(&key) {
                                self.bind(map_var, Ty::Struct(s.clone()));
                            } else {
                                let map_ty = self.make_hashmap_struct();
                                self.bind(map_var, Ty::Struct(map_ty));
                            }
                        } else {
                            let map_ty = self.make_hashmap_struct();
                            self.bind(map_var, Ty::Struct(map_ty));
                        }
                        self.record_hashmap_args(map_var, key_var, value_var);
                        return Ok(map_var);
                    }
                    if let Name::ParameterPath(path) = loc {
                        if let Some(segment) = path.segments.last() {
                            if segment.ident.as_str() == "Vec" && segment.args.len() == 1 {
                                let elem_var = self.type_from_ast_ty(&segment.args[0])?;
                                self.bind(
                                    var,
                                    Ty::Vec(TypeVec {
                                        ty: Box::new(Ty::infer_var(elem_var)),
                                    }),
                                );
                                return Ok(var);
                            }
                            if segment.ident.as_str() == "Box" && segment.args.len() == 1 {
                                let elem_var = self.type_from_ast_ty(&segment.args[0])?;
                                let segment = ParameterPathSegment::new(
                                    Ident::new("Box"),
                                    vec![Ty::infer_var(elem_var)],
                                );
                                let path = ParameterPath::new(PathPrefix::Plain, vec![segment]);
                                self.bind(var, Ty::locator(Name::ParameterPath(path)));
                                return Ok(var);
                            }
                            if segment.ident.as_str() == "Future"
                                && segment.args.len() == 1
                                && path.segments.len() >= 3
                                && path.segments[path.segments.len() - 3].ident.as_str() == "std"
                                && path.segments[path.segments.len() - 2].ident.as_str() == "task"
                            {
                                self.bind(var, ty.clone());
                                return Ok(var);
                            }
                            if !segment.args.is_empty() {
                                let name = segment.ident.as_str();
                                let mut concrete_args = Vec::with_capacity(segment.args.len());
                                for arg in &segment.args {
                                    let arg_var = self.type_from_ast_ty(arg)?;
                                    let concrete = match self.resolve_to_ty(arg_var) {
                                        Ok(resolved)
                                            if matches!(arg, Ty::ImplTraits(_))
                                                && matches!(
                                                    resolved,
                                                    Ty::Any(_) | Ty::Unknown(_)
                                                ) =>
                                        {
                                            arg.clone()
                                        }
                                        Ok(resolved)
                                            if matches!(resolved, Ty::Any(_) | Ty::Unknown(_))
                                                && self.ty_contains_generic_param(arg) =>
                                        {
                                            arg.clone()
                                        }
                                        Ok(resolved) => resolved,
                                        Err(_) => arg.clone(),
                                    };
                                    concrete_args.push(concrete);
                                }
                                let mut handled = false;
                                if let Some(key) = self.resolve_locator_key(loc) {
                                    if let Some(enum_ty) = self.enum_defs.get(&key) {
                                        if enum_ty.generics_params.len() == concrete_args.len() {
                                            let concrete = self.apply_generic_args_to_enum(
                                                enum_ty,
                                                &concrete_args,
                                            );
                                            self.bind(var, Ty::Enum(concrete));
                                            handled = true;
                                        }
                                    }
                                    if !handled {
                                        if let Some(struct_ty) = self.lookup_struct(&key) {
                                            if struct_ty.generics_params.len()
                                                == concrete_args.len()
                                            {
                                                let concrete = self.apply_generic_args_to_struct(
                                                    &struct_ty,
                                                    &concrete_args,
                                                );
                                                self.bind(var, Ty::Struct(concrete));
                                                handled = true;
                                            }
                                        }
                                    }
                                }
                                if handled {
                                    return Ok(var);
                                }
                                if let Some((_, enum_ty)) = self.lookup_enum_def_by_name(name) {
                                    if enum_ty.generics_params.len() == concrete_args.len() {
                                        let concrete = self
                                            .apply_generic_args_to_enum(&enum_ty, &concrete_args);
                                        self.bind(var, Ty::Enum(concrete));
                                        return Ok(var);
                                    }
                                }
                                if name == "Option" && concrete_args.len() == 1 {
                                    let std_option = QualifiedPath::new(vec![
                                        "std".to_string(),
                                        "option".to_string(),
                                        "Option".to_string(),
                                    ]);
                                    if let Some(enum_ty) = self.enum_defs.get(&std_option) {
                                        let concrete = self
                                            .apply_generic_args_to_enum(enum_ty, &concrete_args);
                                        self.bind(var, Ty::Enum(concrete));
                                        return Ok(var);
                                    }
                                }
                                if name == "Result" && concrete_args.len() == 2 {
                                    let std_result = QualifiedPath::new(vec![
                                        "std".to_string(),
                                        "result".to_string(),
                                        "Result".to_string(),
                                    ]);
                                    if let Some(enum_ty) = self.enum_defs.get(&std_result) {
                                        let concrete = self
                                            .apply_generic_args_to_enum(enum_ty, &concrete_args);
                                        self.bind(var, Ty::Enum(concrete));
                                        return Ok(var);
                                    }
                                }
                                if let Some((_, struct_ty)) = self.lookup_struct_def_by_name(name) {
                                    if struct_ty.generics_params.len() == concrete_args.len() {
                                        let concrete = self.apply_generic_args_to_struct(
                                            &struct_ty,
                                            &concrete_args,
                                        );
                                        self.bind(var, Ty::Struct(concrete));
                                        return Ok(var);
                                    }
                                }
                                if name == "Option" && concrete_args.len() == 1 {
                                    let enum_ty = TypeEnum {
                                        name: Ident::new("Option"),
                                        generics_params: Vec::new(),
                                        repr: ReprOptions::default(),
                                        variants: vec![
                                            EnumTypeVariant {
                                                name: Ident::new("Some"),
                                                value: concrete_args[0].clone(),
                                                discriminant: None,
                                            },
                                            EnumTypeVariant {
                                                name: Ident::new("None"),
                                                value: Ty::Unit(TypeUnit),
                                                discriminant: None,
                                            },
                                        ],
                                    };
                                    self.bind(var, Ty::Enum(enum_ty));
                                    return Ok(var);
                                }
                                if name == "Result" && concrete_args.len() == 2 {
                                    let enum_ty = TypeEnum {
                                        name: Ident::new("Result"),
                                        generics_params: Vec::new(),
                                        repr: ReprOptions::default(),
                                        variants: vec![
                                            EnumTypeVariant {
                                                name: Ident::new("Ok"),
                                                value: concrete_args[0].clone(),
                                                discriminant: None,
                                            },
                                            EnumTypeVariant {
                                                name: Ident::new("Err"),
                                                value: concrete_args[1].clone(),
                                                discriminant: None,
                                            },
                                        ],
                                    };
                                    self.bind(var, Ty::Enum(enum_ty));
                                    return Ok(var);
                                }
                            }
                        }
                    }
                    let name = match loc {
                        Name::ParameterPath(path) => path
                            .segments
                            .last()
                            .map(|seg| seg.ident.as_str().to_string())
                            .unwrap_or_default(),
                        Name::Path(path) => path
                            .segments
                            .last()
                            .map(|seg| seg.as_str().to_string())
                            .unwrap_or_default(),
                        Name::Ident(ident) => ident.as_str().to_string(),
                    };
                    let resolved = self.resolve_locator_key(loc);
                    if is_token_stream_name(&name) {
                        self.bind(var, Ty::TokenStream(TypeTokenStream));
                        return Ok(var);
                    }
                    if name == "Self" {
                        if let Some(ctx) = self.impl_stack.last().and_then(|ctx| ctx.as_ref()) {
                            match &ctx.self_ty {
                                Ty::Struct(struct_ty) => {
                                    self.bind(var, Ty::Struct(struct_ty.clone()));
                                    return Ok(var);
                                }
                                Ty::Enum(enum_ty) => {
                                    self.bind(var, Ty::Enum(enum_ty.clone()));
                                    return Ok(var);
                                }
                                _ => {}
                            }
                        }
                    }

                    if self
                        .generic_scopes
                        .iter()
                        .rev()
                        .any(|scope| scope.contains(&name))
                    {
                        if let Some(existing) = self.lookup_env_var(&name) {
                            return Ok(existing);
                        }
                    }
                    if name == "&str" {
                        let inner = self.fresh_type_var();
                        self.bind(inner, Ty::Primitive(TypePrimitive::String));
                        self.bind_reference_term(var, inner);
                        return Ok(var);
                    }
                    if name == "type" {
                        self.bind(var, Ty::Type(TypeType { span: fp_core::span::Span::null(), inner: None }));
                        return Ok(var);
                    }
                    if let Some(prim) = primitive_from_name(&name) {
                        self.bind(var, Ty::Primitive(prim));
                        return Ok(var);
                    }
                    if let Some(key) = resolved.clone() {
                        if let Some(struct_ty) = self.lookup_struct(&key) {
                            self.bind(var, Ty::Struct(struct_ty.clone()));
                            return Ok(var);
                        }
                        if let Some(enum_ty) = self.enum_defs.get(&key) {
                            self.bind(var, Ty::Enum(enum_ty.clone()));
                            return Ok(var);
                        }
                        if let Some(stripped) = Self::strip_std_prefix(&key) {
                            if let Some(struct_ty) = self.lookup_struct(&stripped) {
                                self.bind(var, Ty::Struct(struct_ty.clone()));
                                return Ok(var);
                            }
                            if let Some(enum_ty) = self.enum_defs.get(&stripped) {
                                self.bind(var, Ty::Enum(enum_ty.clone()));
                                return Ok(var);
                            }
                        }
                    }
                    if let Some(parsed) = self.resolution_parsed_path(loc) {
                        let name_path = QualifiedPath::new(parsed.segments);
                        let is_unqualified =
                            parsed.prefix == PathPrefix::Plain && name_path.segments.len() == 1;
                        let mut candidates =
                            self.struct_name_variants_for_path(&name_path, is_unqualified);
                        if let Some(stripped) = Self::strip_std_prefix(&name_path) {
                            if !candidates.contains(&stripped) {
                                candidates.push(stripped);
                            }
                        }
                        for candidate in &candidates {
                            if let Some(struct_ty) = self.lookup_struct(candidate) {
                                self.bind(var, Ty::Struct(struct_ty.clone()));
                                return Ok(var);
                            }
                        }
                        for candidate in &candidates {
                            if let Some(enum_ty) = self.enum_defs.get(candidate) {
                                self.bind(var, Ty::Enum(enum_ty.clone()));
                                return Ok(var);
                            }
                        }
                    }
                    if let Some((_, struct_ty)) = self.lookup_struct_def_by_name(&name) {
                        self.bind(var, Ty::Struct(struct_ty));
                        return Ok(var);
                    }
                    if let Some((_, enum_ty)) = self.lookup_enum_def_by_name(&name) {
                        self.bind(var, Ty::Enum(enum_ty));
                        return Ok(var);
                    }
                    if name == "HashMap" {
                        self.bind(var, Ty::Struct(self.make_hashmap_struct()));
                        return Ok(var);
                    }
                }
                if let ExprKind::Invoke(invoke) = expr.kind() {
                    if let Some(name) = invoke_target_name(invoke) {
                        if name == "HashMap" {
                            let struct_ty = TypeStruct {
                                name: Ident::new("HashMap"),
                                generics_params: Vec::new(),
                                repr: ReprOptions::default(),
                                method_sigs: Vec::new(),
                                fields: Vec::new(),
                            };
                            self.bind(var, Ty::Struct(struct_ty));
                            return Ok(var);
                        }
                    }
                }
                // Fallback unresolved named types stay symbolic until later constraints refine them.
                return Ok(var);
            }
            Ty::Function(f) => {
                let params = f
                    .params
                    .iter()
                    .map(|p| self.type_from_ast_ty(p))
                    .collect::<Result<Vec<_>>>()?;
                let ret = if let Some(ret_ty) = f.ret_ty.as_ref() {
                    self.type_from_ast_ty(ret_ty)
                } else {
                    self.type_from_ast_ty(&Ty::Unit(TypeUnit))
                }?;
                self.bind(
                    var,
                    Ty::Function(TypeFunction {
                        params: params.into_iter().map(Ty::infer_var).collect(),
                        generics_params: f.generics_params.clone(),
                        ret_ty: Some(Box::new(Ty::infer_var(ret))),
                    }),
                );
            }
            Ty::ImplTraits(traits) => {
                // `impl Trait` / `dyn Trait` are currently treated as opaque, but we still
                // record trait bounds so method lookup on dyn traits can succeed.
                let bounds = Self::extract_trait_bounds(&traits.bounds);
                if !bounds.is_empty() {
                    self.generic_trait_bounds.insert(var, bounds);
                }
                self.bind_error(var);
            }
        }
        Ok(var)
    }

    pub(crate) fn type_from_ast_ty_in_module(
        &mut self,
        ty: &Ty,
        module_path: &QualifiedPath,
    ) -> Result<TypeVarId> {
        let saved = self.module_path.clone();
        self.module_path = module_path.clone();
        let result = self.type_from_ast_ty(ty);
        self.module_path = saved;
        result
    }

    fn hashmap_args_from_locator(
        &mut self,
        locator: &Name,
    ) -> Result<Option<(TypeVarId, TypeVarId)>> {
        let Name::ParameterPath(path) = locator else {
            return Ok(None);
        };
        let Some(segment) = path.segments.last() else {
            return Ok(None);
        };
        if segment.ident.as_str() != "HashMap" || segment.args.len() != 2 {
            return Ok(None);
        }
        let key_var = self.type_from_ast_ty(&segment.args[0])?;
        let value_var = self.type_from_ast_ty(&segment.args[1])?;
        Ok(Some((key_var, value_var)))
    }
}

fn quote_item_compatible(a: &Ty, b: &Ty) -> bool {
    let a_kind = quote_item_kind(a);
    let b_kind = quote_item_kind(b);
    matches!(
        (a_kind, b_kind),
        (Some("item"), Some(_)) | (Some(_), Some("item"))
    )
}

fn quote_item_kind(ty: &Ty) -> Option<&'static str> {
    match ty {
        Ty::Quote(quote) if quote.kind == QuoteFragmentKind::Item => match quote.item {
            Some(QuoteItemKind::Function) => Some("fn"),
            Some(QuoteItemKind::Struct) => Some("struct"),
            Some(QuoteItemKind::Enum) => Some("enum"),
            Some(QuoteItemKind::Trait) => Some("trait"),
            Some(QuoteItemKind::Impl) => Some("impl"),
            Some(QuoteItemKind::Const) => Some("const"),
            Some(QuoteItemKind::Static) => Some("static"),
            Some(QuoteItemKind::Module) => Some("mod"),
            Some(QuoteItemKind::Use) => Some("use"),
            Some(QuoteItemKind::Macro) => Some("macro"),
            None => Some("item"),
            Some(QuoteItemKind::Type) => Some("type"),
        },
        _ => None,
    }
}

pub(crate) fn primitive_from_name(name: &str) -> Option<TypePrimitive> {
    use TypePrimitive::Int;
    match name {
        "i8" => Some(Int(TypeInt::I8)),
        "i16" => Some(Int(TypeInt::I16)),
        "i32" => Some(Int(TypeInt::I32)),
        "i64" => Some(Int(TypeInt::I64)),
        "i128" => Some(Int(TypeInt::I128)),
        "isize" => Some(Int(TypeInt::I64)),
        "u8" => Some(Int(TypeInt::U8)),
        "u16" => Some(Int(TypeInt::U16)),
        "u32" => Some(Int(TypeInt::U32)),
        "u64" => Some(Int(TypeInt::U64)),
        "u128" => Some(Int(TypeInt::U128)),
        "usize" => Some(Int(TypeInt::U64)),
        "bool" => Some(TypePrimitive::Bool),
        "char" => Some(TypePrimitive::Char),
        "str" => Some(TypePrimitive::String),
        "f32" => Some(TypePrimitive::Decimal(DecimalType::F32)),
        "f64" => Some(TypePrimitive::Decimal(DecimalType::F64)),
        _ => None,
    }
}

fn is_token_stream_name(name: &str) -> bool {
    name == "TokenStream" || name.ends_with("::TokenStream")
}

fn invoke_target_name(invoke: &ExprInvoke) -> Option<String> {
    match &invoke.target {
        ExprInvokeTarget::Function(locator) => locator_tail_name(locator),
        ExprInvokeTarget::Expr(expr) => {
            if let ExprKind::Name(locator) = expr.kind() {
                locator_tail_name(locator)
            } else {
                None
            }
        }
        ExprInvokeTarget::Type(ty) => match ty {
            Ty::Struct(struct_ty) => Some(struct_ty.name.as_str().to_string()),
            Ty::Expr(expr) => {
                if let ExprKind::Name(locator) = expr.kind() {
                    locator_tail_name(locator)
                } else {
                    None
                }
            }
            _ => None,
        },
        ExprInvokeTarget::Method(_) | ExprInvokeTarget::Closure(_) | ExprInvokeTarget::BinOp(_) => {
            None
        }
    }
}

fn locator_tail_name(locator: &Name) -> Option<String> {
    match locator {
        Name::Ident(ident) => Some(ident.as_str().to_string()),
        Name::Path(path) => path.segments.last().map(|seg| seg.as_str().to_string()),
        Name::ParameterPath(path) => path
            .segments
            .last()
            .map(|seg| seg.ident.as_str().to_string()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fp_core::span::Span;

    #[test]
    fn merges_structural_types_with_plus() {
        let mut typer = AstTypeInferencer::new(std::rc::Rc::new(TypingContext::new()));

        let lhs = Ty::Structural(TypeStructural {
            fields: vec![StructuralField::new(
                Ident::new("a".to_string()),
                Ty::Primitive(TypePrimitive::Int(TypeInt::I64)),
            )],
        });
        let rhs = Ty::Structural(TypeStructural {
            fields: vec![StructuralField::new(
                Ident::new("b".to_string()),
                Ty::Primitive(TypePrimitive::Int(TypeInt::I64)),
            )],
        });
        let op = Ty::TypeBinaryOp(Box::new(TypeBinaryOp {
            kind: TypeBinaryOpKind::Add,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }));

        let var = typer.type_from_ast_ty(&op).expect("type_from_ast_ty");
        let ty = typer.resolve_to_ty(var).expect("resolve_to_ty");

        match ty {
            Ty::Structural(s) => {
                assert_eq!(s.fields.len(), 2);
                assert_eq!(s.fields[0].name.as_str(), "a");
                assert_eq!(s.fields[1].name.as_str(), "b");
            }
            other => panic!("expected structural type, got {:?}", other),
        }
    }

    #[test]
    fn rejects_conflicting_field_types_on_merge() {
        let mut typer = AstTypeInferencer::new(std::rc::Rc::new(TypingContext::new()));

        let lhs = Ty::Structural(TypeStructural {
            fields: vec![StructuralField::new(
                Ident::new("x".to_string()),
                Ty::Primitive(TypePrimitive::Int(TypeInt::I64)),
            )],
        });
        let rhs = Ty::Structural(TypeStructural {
            fields: vec![StructuralField::new(
                Ident::new("x".to_string()),
                Ty::Primitive(TypePrimitive::Int(TypeInt::I32)),
            )],
        });
        let op = Ty::TypeBinaryOp(Box::new(TypeBinaryOp {
            kind: TypeBinaryOpKind::Add,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }));

        let result = typer.type_from_ast_ty(&op);
        assert!(
            result.is_err(),
            "expected error for conflicting field types"
        );
    }

    #[test]
    fn intersects_structural_types_with_ampersand() {
        let mut typer = AstTypeInferencer::new(std::rc::Rc::new(TypingContext::new()));

        let lhs = Ty::Structural(TypeStructural {
            fields: vec![
                StructuralField::new(
                    Ident::new("a".to_string()),
                    Ty::Primitive(TypePrimitive::Int(TypeInt::I64)),
                ),
                StructuralField::new(
                    Ident::new("b".to_string()),
                    Ty::Primitive(TypePrimitive::Int(TypeInt::I64)),
                ),
            ],
        });
        let rhs = Ty::Structural(TypeStructural {
            fields: vec![
                StructuralField::new(
                    Ident::new("b".to_string()),
                    Ty::Primitive(TypePrimitive::Int(TypeInt::I64)),
                ),
                StructuralField::new(
                    Ident::new("c".to_string()),
                    Ty::Primitive(TypePrimitive::Int(TypeInt::I64)),
                ),
            ],
        });
        let op = Ty::TypeBinaryOp(Box::new(TypeBinaryOp {
            kind: TypeBinaryOpKind::Intersect,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }));

        let var = typer.type_from_ast_ty(&op).expect("type_from_ast_ty");
        let ty = typer.resolve_to_ty(var).expect("resolve_to_ty");

        match ty {
            Ty::Structural(s) => {
                assert_eq!(s.fields.len(), 1);
                assert_eq!(s.fields[0].name.as_str(), "b");
            }
            other => panic!("expected structural type, got {:?}", other),
        }
    }

    #[test]
    fn unify_errors_carry_active_span() {
        let mut typer = AstTypeInferencer::new(std::rc::Rc::new(TypingContext::new()));
        let span = Span::new(1, 10, 12);
        typer.current_span = Some(span);

        let struct_var = typer.fresh_type_var();
        let func_var = typer.fresh_type_var();
        typer.bind(
            struct_var,
            Ty::Struct(TypeStruct {
                name: Ident::new("Parser".to_string()),
                generics_params: Vec::new(),
                repr: Default::default(),
                fields: Vec::new(),
            }),
        );
        let ret_var = typer.unit_type_var();
        typer.bind_function_term(func_var, Vec::new(), ret_var);

        let err = typer
            .unify(struct_var, func_var)
            .expect_err("expected mismatch");
        match err {
            Error::Diagnostic(diag) => {
                assert_eq!(diag.span, Some(span));
                assert!(diag.message.contains("type mismatch"));
            }
            other => panic!("expected diagnostic error, got {other:?}"),
        }
    }

    #[test]
    fn subtracts_fields_with_minus() {
        let mut typer = AstTypeInferencer::new(std::rc::Rc::new(TypingContext::new()));

        let lhs = Ty::Structural(TypeStructural {
            fields: vec![
                StructuralField::new(
                    Ident::new("a".to_string()),
                    Ty::Primitive(TypePrimitive::Int(TypeInt::I64)),
                ),
                StructuralField::new(
                    Ident::new("b".to_string()),
                    Ty::Primitive(TypePrimitive::Int(TypeInt::I64)),
                ),
                StructuralField::new(
                    Ident::new("c".to_string()),
                    Ty::Primitive(TypePrimitive::Int(TypeInt::I64)),
                ),
            ],
        });
        let rhs = Ty::Structural(TypeStructural {
            fields: vec![StructuralField::new(
                Ident::new("b".to_string()),
                Ty::Primitive(TypePrimitive::Int(TypeInt::I64)),
            )],
        });
        let op = Ty::TypeBinaryOp(Box::new(TypeBinaryOp {
            kind: TypeBinaryOpKind::Subtract,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }));

        let var = typer.type_from_ast_ty(&op).expect("type_from_ast_ty");
        let ty = typer.resolve_to_ty(var).expect("resolve_to_ty");

        match ty {
            Ty::Structural(s) => {
                assert_eq!(s.fields.len(), 2);
                assert_eq!(s.fields[0].name.as_str(), "a");
                assert_eq!(s.fields[1].name.as_str(), "c");
            }
            other => panic!("expected structural type, got {:?}", other),
        }
    }
}
