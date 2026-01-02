use crate::typing::scheme::{SchemeType, TypeScheme};
use crate::{AstTypeInferencer, TypeVarId};
use fp_core::ast::*;
use fp_core::error::{Error, Result};

#[derive(Clone, Debug)]
pub(crate) struct TypeVar {
    pub(crate) kind: TypeVarKind,
}

#[derive(Clone, Debug)]
pub(crate) enum TypeVarKind {
    Unbound { level: usize },
    Link(TypeVarId),
    Bound(TypeTerm),
}

#[derive(Clone, Debug)]
pub(crate) struct FunctionTerm {
    pub(crate) params: Vec<TypeVarId>,
    pub(crate) ret: TypeVarId,
}

#[derive(Clone, Debug)]
pub(crate) enum TypeTerm {
    Primitive(TypePrimitive),
    Unit,
    Nothing,
    Tuple(Vec<TypeVarId>),
    Function(FunctionTerm),
    Struct(TypeStruct),
    Structural(TypeStructural),
    Enum(TypeEnum),
    // Compile-time union of two types from `A | B`.
    Union(TypeVarId, TypeVarId),
    Slice(TypeVarId),
    Vec(TypeVarId),
    // NOTE(jakku): Array terms now keep the element var plus the length expr
    // (if known). This avoids forcing arrays through TypeTerm::Custom, which
    // caused mismatches with slice/array unification and generic substitution.
    Array(TypeVarId, Option<BExpr>),
    Reference(TypeVarId),
    Any,
    Custom(Ty),
    Unknown,
}

impl<'ctx> AstTypeInferencer<'ctx> {
    pub(crate) fn generalize(&mut self, var: TypeVarId) -> Result<TypeScheme> {
        let mut mapping = std::collections::HashMap::new();
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
        mapping: &mut std::collections::HashMap<TypeVarId, u32>,
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
        mapping: &mut std::collections::HashMap<TypeVarId, u32>,
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
            TypeTerm::Union(lhs, rhs) => {
                let lhs = self.build_scheme_type(lhs, mapping, next)?;
                let rhs = self.build_scheme_type(rhs, mapping, next)?;
                SchemeType::Union(Box::new(lhs), Box::new(rhs))
            }
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
            TypeTerm::Array(elem, _) => {
                let elem = self.build_scheme_type(elem, mapping, next)?;
                SchemeType::Array(Box::new(elem))
            }
            TypeTerm::Reference(elem) => {
                let elem = self.build_scheme_type(elem, mapping, next)?;
                SchemeType::Reference(Box::new(elem))
            }
        })
    }

    pub(crate) fn instantiate_scheme(&mut self, scheme: &TypeScheme) -> TypeVarId {
        let mut mapping = std::collections::HashMap::new();
        self.instantiate_scheme_type(&scheme.body, &mut mapping)
    }

    fn instantiate_scheme_type(
        &mut self,
        scheme: &SchemeType,
        mapping: &mut std::collections::HashMap<u32, TypeVarId>,
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
            SchemeType::Union(lhs, rhs) => {
                let lhs_var = self.instantiate_scheme_type(lhs, mapping);
                let rhs_var = self.instantiate_scheme_type(rhs, mapping);
                let var = self.fresh_type_var();
                self.bind(var, TypeTerm::Union(lhs_var, rhs_var));
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
            SchemeType::Array(elem) => {
                let elem_var = self.instantiate_scheme_type(elem, mapping);
                let var = self.fresh_type_var();
                self.bind(var, TypeTerm::Array(elem_var, None));
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
        self.bind(var, TypeTerm::Unit);
        var
    }

    pub(crate) fn nothing_type_var(&mut self) -> TypeVarId {
        let var = self.fresh_type_var();
        self.bind(var, TypeTerm::Nothing);
        var
    }

    pub(crate) fn bind(&mut self, var: TypeVarId, term: TypeTerm) {
        let root = self.find(var);
        self.type_vars[root].kind = TypeVarKind::Bound(term);
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
        match (
            self.type_vars[a_root].kind.clone(),
            self.type_vars[b_root].kind.clone(),
        ) {
            (TypeVarKind::Bound(TypeTerm::Unknown), _) => {
                self.type_vars[a_root].kind = TypeVarKind::Link(b_root);
                Ok(())
            }
            (_, TypeVarKind::Bound(TypeTerm::Unknown)) => {
                self.type_vars[b_root].kind = TypeVarKind::Link(a_root);
                Ok(())
            }
            (TypeVarKind::Unbound { .. }, TypeVarKind::Unbound { .. }) => {
                self.type_vars[a_root].kind = TypeVarKind::Link(b_root);
                Ok(())
            }
            (TypeVarKind::Unbound { .. }, TypeVarKind::Bound(term)) => {
                if self.occurs_in_term(a_root, &term) {
                    return Err(Error::from("occurs check failed"));
                }
                self.type_vars[a_root].kind = TypeVarKind::Bound(term);
                Ok(())
            }
            (TypeVarKind::Bound(term), TypeVarKind::Unbound { .. }) => {
                if self.occurs_in_term(b_root, &term) {
                    return Err(Error::from("occurs check failed"));
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
                    Err(Error::from("primitive type mismatch"))
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
            TypeTerm::Union(lhs, rhs) => self.occurs_in(var, *lhs) || self.occurs_in(var, *rhs),
            TypeTerm::Slice(elem)
            | TypeTerm::Vec(elem)
            | TypeTerm::Array(elem, _)
            | TypeTerm::Reference(elem) => {
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
                    Err(Error::from("primitive type mismatch"))
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
                    Err(Error::from("struct type mismatch"))
                }
            }
            (TypeTerm::Structural(sa), TypeTerm::Structural(sb)) => {
                if sa == sb {
                    Ok(())
                } else {
                    Err(Error::from("structural type mismatch"))
                }
            }
            (TypeTerm::Enum(ae), TypeTerm::Enum(be)) => {
                if ae == be {
                    Ok(())
                } else {
                    Err(Error::from("enum type mismatch"))
                }
            }
            (TypeTerm::Union(a_lhs, a_rhs), TypeTerm::Union(b_lhs, b_rhs)) => {
                self.unify_union_pair(a_lhs, a_rhs, b_lhs, b_rhs)
            }
            (TypeTerm::Union(lhs, rhs), other) => self.unify_union_with(lhs, rhs, other),
            (other, TypeTerm::Union(lhs, rhs)) => self.unify_union_with(lhs, rhs, other),
            (TypeTerm::Array(a_elem, _), TypeTerm::Array(b_elem, _)) => {
                self.unify(a_elem, b_elem)
            }
            (TypeTerm::Array(array_elem, _), TypeTerm::Slice(slice_elem))
            | (TypeTerm::Slice(slice_elem), TypeTerm::Array(array_elem, _)) => {
                self.unify(array_elem, slice_elem)
            }
            (TypeTerm::Array(array_elem, _), TypeTerm::Custom(ty))
            | (TypeTerm::Custom(ty), TypeTerm::Array(array_elem, _))
                if matches!(ty, Ty::Array(_)) =>
            {
                if let Ty::Array(array_ty) = ty {
                    let elem_var = self.type_from_ast_ty(&array_ty.elem)?;
                    self.unify(array_elem, elem_var)
                } else {
                    Ok(())
                }
            }
            (TypeTerm::Custom(a), TypeTerm::Custom(b)) => {
                if a == b {
                    Ok(())
                } else if let (Ty::Array(a_arr), Ty::Array(b_arr)) = (&a, &b) {
                    // Be permissive about array lengths during typing; const-eval
                    // may normalize lengths into literals at different times.
                    let a_elem = self.type_from_ast_ty(&a_arr.elem)?;
                    let b_elem = self.type_from_ast_ty(&b_arr.elem)?;
                    self.unify(a_elem, b_elem)
                } else {
                    Err(Error::from(format!("custom type mismatch: {} vs {}", a, b)))
                }
            }
            (TypeTerm::Custom(array_ty), TypeTerm::Slice(slice_elem))
                if matches!(array_ty, Ty::Array(_)) =>
            {
                if let Ty::Array(array_ty) = array_ty {
                    let array_elem = self.type_from_ast_ty(&array_ty.elem)?;
                    self.unify(array_elem, slice_elem)
                } else {
                    Ok(())
                }
            }
            (TypeTerm::Slice(slice_elem), TypeTerm::Custom(array_ty))
                if matches!(array_ty, Ty::Array(_)) =>
            {
                if let Ty::Array(array_ty) = array_ty {
                    let array_elem = self.type_from_ast_ty(&array_ty.elem)?;
                    self.unify(array_elem, slice_elem)
                } else {
                    Ok(())
                }
            }
            (TypeTerm::Tuple(a_elems), TypeTerm::Tuple(b_elems)) => {
                if a_elems.len() != b_elems.len() {
                    return Err(Error::from("tuple length mismatch"));
                }
                for (a_elem, b_elem) in a_elems.into_iter().zip(b_elems.into_iter()) {
                    self.unify(a_elem, b_elem)?;
                }
                Ok(())
            }
            (TypeTerm::Function(a_func), TypeTerm::Function(b_func)) => {
                if a_func.params.len() != b_func.params.len() {
                    return Err(Error::from("function arity mismatch"));
                }
                for (a_param, b_param) in a_func.params.into_iter().zip(b_func.params.into_iter()) {
                    self.unify(a_param, b_param)?;
                }
                self.unify(a_func.ret, b_func.ret)
            }
            (TypeTerm::Slice(a), TypeTerm::Slice(b))
            | (TypeTerm::Vec(a), TypeTerm::Vec(b))
            | (TypeTerm::Reference(a), TypeTerm::Reference(b)) => self.unify(a, b),
            (TypeTerm::Slice(a), TypeTerm::Vec(b)) | (TypeTerm::Vec(a), TypeTerm::Slice(b)) => {
                self.unify(a, b)
            }
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
            (TypeTerm::Reference(inner), other) => {
                let other_var = self.fresh_type_var();
                self.bind(other_var, other);
                self.unify(inner, other_var)
            }
            (other, TypeTerm::Reference(inner)) => {
                let other_var = self.fresh_type_var();
                self.bind(other_var, other);
                self.unify(inner, other_var)
            }
            (TypeTerm::Unknown, _other) | (_other, TypeTerm::Unknown) => Ok(()),
            (TypeTerm::Any, _other) | (_other, TypeTerm::Any) => Ok(()),
            (left, right) => Err(Error::from(format!(
                "type mismatch: {:?} vs {:?}",
                left, right
            ))),
        }
    }

    fn unify_union_with(
        &mut self,
        lhs: TypeVarId,
        rhs: TypeVarId,
        other: TypeTerm,
    ) -> Result<()> {
        // Union typing: allow either branch to match, without committing if it fails.
        let snapshot = self.type_vars.clone();
        let other_var = self.fresh_type_var();
        self.bind(other_var, other.clone());
        if self.unify(lhs, other_var).is_ok() {
            return Ok(());
        }
        self.type_vars = snapshot;
        let other_var = self.fresh_type_var();
        self.bind(other_var, other);
        if self.unify(rhs, other_var).is_ok() {
            return Ok(());
        }
        Err(Error::from("union type mismatch"))
    }

    fn unify_union_pair(
        &mut self,
        a_lhs: TypeVarId,
        a_rhs: TypeVarId,
        b_lhs: TypeVarId,
        b_rhs: TypeVarId,
    ) -> Result<()> {
        // Accept either ordering: (A|B) == (C|D) if A==C and B==D or A==D and B==C.
        let snapshot = self.type_vars.clone();
        if self.unify(a_lhs, b_lhs).is_ok() && self.unify(a_rhs, b_rhs).is_ok() {
            return Ok(());
        }
        self.type_vars = snapshot;
        if self.unify(a_lhs, b_rhs).is_ok() && self.unify(a_rhs, b_lhs).is_ok() {
            return Ok(());
        }
        Err(Error::from("union type mismatch"))
    }

    pub(crate) fn resolve_to_ty(&mut self, var: TypeVarId) -> Result<Ty> {
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
            TypeTerm::Union(lhs, rhs) => {
                let lhs_ty = self.resolve_to_ty(lhs)?;
                let rhs_ty = self.resolve_to_ty(rhs)?;
                Ty::TypeBinaryOp(
                    TypeBinaryOp {
                        kind: TypeBinaryOpKind::Union,
                        lhs: Box::new(lhs_ty),
                        rhs: Box::new(rhs_ty),
                    }
                    .into(),
                )
            }
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
            TypeTerm::Array(elem, len_expr) => {
                let elem_ty = self.resolve_to_ty(elem)?;
                let len = len_expr.unwrap_or_else(|| Expr::value(Value::int(0)).into());
                Ty::Array(TypeArray {
                    elem: Box::new(elem_ty),
                    len,
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

    pub(crate) fn type_from_ast_ty(&mut self, ty: &Ty) -> Result<TypeVarId> {
        let var = self.fresh_type_var();
        match ty {
            Ty::Primitive(prim) => self.bind(var, TypeTerm::Primitive(*prim)),
            Ty::Unit(_) => self.bind(var, TypeTerm::Unit),
            Ty::Nothing(_) => self.bind(var, TypeTerm::Nothing),
            Ty::Any(_) => self.bind(var, TypeTerm::Any),
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
                                self.bind(var, TypeTerm::Custom(ty.clone()));
                                return Ok(var);
                            }
                        };
                        let rhs_fields = match rhs_ty {
                            Ty::Struct(ref s) => s.fields.clone(),
                            Ty::Structural(ref st) => st.fields.clone(),
                            _ => {
                                self.bind(var, TypeTerm::Custom(ty.clone()));
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
                                if existing.value != rhs_field.value {
                                    return Err(Error::from(format!(
                                            "cannot merge struct fields: field '{}' has incompatible types",
                                            rhs_field.name
                                        )));
                                }
                            } else {
                                merged.push(rhs_field);
                            }
                        }
                        self.bind(var, TypeTerm::Structural(TypeStructural { fields: merged }));
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
                                self.bind(var, TypeTerm::Custom(ty.clone()));
                                return Ok(var);
                            }
                        };
                        let rhs_fields = match rhs_ty {
                            Ty::Struct(ref s) => s.fields.clone(),
                            Ty::Structural(ref st) => st.fields.clone(),
                            _ => {
                                self.bind(var, TypeTerm::Custom(ty.clone()));
                                return Ok(var);
                            }
                        };

                        let mut merged = Vec::new();
                        for lhs_field in lhs_fields {
                            if let Some(rhs_field) = rhs_fields
                                .iter()
                                .find(|f| f.name.as_str() == lhs_field.name.as_str())
                            {
                                if lhs_field.value != rhs_field.value {
                                    return Err(Error::from(format!(
                                            "cannot intersect struct fields: field '{}' has incompatible types",
                                            lhs_field.name
                                        )));
                                }
                                merged.push(lhs_field.clone());
                            }
                        }

                        self.bind(var, TypeTerm::Structural(TypeStructural { fields: merged }));
                    }
                    TypeBinaryOpKind::Union => {
                        let lhs_var = self.type_from_ast_ty(&op.lhs)?;
                        let rhs_var = self.type_from_ast_ty(&op.rhs)?;
                        self.bind(var, TypeTerm::Union(lhs_var, rhs_var));
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
                                self.bind(var, TypeTerm::Custom(ty.clone()));
                                return Ok(var);
                            }
                        };
                        let rhs_fields = match rhs_ty {
                            Ty::Struct(ref s) => s.fields.clone(),
                            Ty::Structural(ref st) => st.fields.clone(),
                            _ => {
                                self.bind(var, TypeTerm::Custom(ty.clone()));
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

                        self.bind(var, TypeTerm::Structural(TypeStructural { fields: merged }));
                    }
                }
            }
            Ty::AnyBox(_) => {
                // Unknown AnyBox payloads are treated as opaque custom
                // types until a dedicated handler is added.
                self.bind(var, TypeTerm::Custom(ty.clone()));
            }
            Ty::Struct(struct_ty) => {
                self.struct_defs
                    .insert(struct_ty.name.as_str().to_string(), struct_ty.clone());
                self.bind(var, TypeTerm::Struct(struct_ty.clone()));
            }
            Ty::Structural(structural) => self.bind(var, TypeTerm::Structural(structural.clone())),
            Ty::Enum(enum_ty) => self.bind(var, TypeTerm::Enum(enum_ty.clone())),
            Ty::Value(value_ty) => {
                let term = match value_ty.value.as_ref() {
                    Value::Int(_) => TypeTerm::Primitive(TypePrimitive::Int(TypeInt::I64)),
                    Value::Bool(_) => TypeTerm::Primitive(TypePrimitive::Bool),
                    Value::Decimal(_) => TypeTerm::Primitive(TypePrimitive::Decimal(DecimalType::F64)),
                    Value::String(_) => TypeTerm::Primitive(TypePrimitive::String),
                    Value::Char(_) => TypeTerm::Primitive(TypePrimitive::Char),
                    Value::Unit(_) => TypeTerm::Unit,
                    Value::Null(_) | Value::None(_) => TypeTerm::Nothing,
                    _ => TypeTerm::Custom(ty.clone()),
                };
                self.bind(var, term);
            }
            // No Ty::Custom in current AST types; treat all remaining cases via fallback below
            Ty::Unknown(_) => {}
            Ty::Tuple(tuple) => {
                let mut vars = Vec::new();
                for elem in &tuple.types {
                    vars.push(self.type_from_ast_ty(elem)?);
                }
                self.bind(var, TypeTerm::Tuple(vars));
            }
            Ty::Reference(r) => {
                let inner = self.type_from_ast_ty(&r.ty)?;
                self.bind(var, TypeTerm::Reference(inner));
            }
            Ty::Slice(s) => {
                let inner = self.type_from_ast_ty(&s.elem)?;
                self.bind(var, TypeTerm::Slice(inner));
            }
            Ty::Vec(v) => {
                let inner = self.type_from_ast_ty(&v.ty)?;
                self.bind(var, TypeTerm::Vec(inner));
            }
            Ty::Array(array_ty) => {
                let elem_var = self.type_from_ast_ty(&array_ty.elem)?;
                self.bind(var, TypeTerm::Array(elem_var, Some(array_ty.len.clone())));
            }
            Ty::QuoteToken(_) => {
                // Quote tokens are currently opaque to the typer.
                self.bind(var, TypeTerm::Custom(ty.clone()));
            }
            Ty::Expr(expr) => {
                // Handle path-like type expressions (e.g., i64, bool, usize, str).
                if let ExprKind::Locator(loc) = expr.kind() {
                    let name = match loc {
                        Locator::ParameterPath(path) => path
                            .segments
                            .last()
                            .map(|seg| seg.ident.as_str().to_string())
                            .unwrap_or_default(),
                        other => other.to_string(),
                    };
                    if name == "Self" {
                        if let Some(ctx) = self.impl_stack.last().and_then(|ctx| ctx.as_ref()) {
                            match &ctx.self_ty {
                                Ty::Struct(struct_ty) => {
                                    self.bind(var, TypeTerm::Struct(struct_ty.clone()));
                                    return Ok(var);
                                }
                                Ty::Enum(enum_ty) => {
                                    self.bind(var, TypeTerm::Enum(enum_ty.clone()));
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
                    if let Some(prim) = primitive_from_name(&name) {
                        self.bind(var, TypeTerm::Primitive(prim));
                        return Ok(var);
                    }
                    if let Some(struct_ty) = self.struct_defs.get(&name) {
                        self.bind(var, TypeTerm::Struct(struct_ty.clone()));
                        return Ok(var);
                    }
                    if let Some(enum_ty) = self.enum_defs.get(&name) {
                        self.bind(var, TypeTerm::Enum(enum_ty.clone()));
                        return Ok(var);
                    }
                }
                // Fallback: treat as any to allow later constraints to refine.
                self.bind(var, TypeTerm::Any);
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
                self.bind(var, TypeTerm::Function(FunctionTerm { params, ret }));
            }
            Ty::ImplTraits(_traits) => {
                // `impl Trait` is currently treated as an opaque/unknown type.
                // This keeps typing permissive for higher-level experiments and transpilation.
                self.bind(var, TypeTerm::Any);
            }
            other => {
                // Error out loudly instead of silently defaulting, to avoid masking bugs.
                return Err(Error::from(format!(
                    "unsupported AST type in type_from_ast_ty: {:?}",
                    other
                )));
            }
        }
        Ok(var)
    }
}

fn primitive_from_name(name: &str) -> Option<TypePrimitive> {
    use TypePrimitive::Int;
    match name {
        "i8" => Some(Int(TypeInt::I8)),
        "i16" => Some(Int(TypeInt::I16)),
        "i32" => Some(Int(TypeInt::I32)),
        "i64" => Some(Int(TypeInt::I64)),
        "isize" => Some(Int(TypeInt::I64)),
        "u8" => Some(Int(TypeInt::U8)),
        "u16" => Some(Int(TypeInt::U16)),
        "u32" => Some(Int(TypeInt::U32)),
        "u64" => Some(Int(TypeInt::U64)),
        "usize" => Some(Int(TypeInt::U64)),
        "bool" => Some(TypePrimitive::Bool),
        "char" => Some(TypePrimitive::Char),
        "str" | "&str" => Some(TypePrimitive::String),
        "f32" => Some(TypePrimitive::Decimal(DecimalType::F32)),
        "f64" => Some(TypePrimitive::Decimal(DecimalType::F64)),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn merges_structural_types_with_plus() {
        let mut typer = AstTypeInferencer::new();

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
        let mut typer = AstTypeInferencer::new();

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
        let mut typer = AstTypeInferencer::new();

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
    fn subtracts_fields_with_minus() {
        let mut typer = AstTypeInferencer::new();

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
