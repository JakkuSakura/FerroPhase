use crate::{AstTypeInferencer, TypeVarId};
use crate::typing::scheme::{SchemeType, TypeScheme};
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
    Slice(TypeVarId),
    Vec(TypeVarId),
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
        Ok(TypeScheme { vars: next as usize, body })
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
                    TypeTerm::Function(FunctionTerm { params: param_vars, ret: ret_var }),
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

    pub(crate) fn fresh_type_var(&mut self) -> TypeVarId {
        let id = self.type_vars.len();
        self.type_vars.push(TypeVar { kind: TypeVarKind::Unbound { level: self.current_level } });
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
        match (self.type_vars[a_root].kind.clone(), self.type_vars[b_root].kind.clone()) {
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
            (TypeVarKind::Bound(term_a), TypeVarKind::Bound(term_b)) => self.unify_terms(term_a, term_b),
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
                if pa == pb { Ok(()) } else { Err(Error::from("primitive type mismatch")) }
            }
            (TypeTerm::Unit, TypeTerm::Unit)
            | (TypeTerm::Nothing, TypeTerm::Nothing)
            | (TypeTerm::Any, TypeTerm::Any)
            | (TypeTerm::Unknown, TypeTerm::Unknown) => Ok(()),
            (TypeTerm::Struct(sa), TypeTerm::Struct(sb)) => {
                if sa == sb { Ok(()) } else { Err(Error::from("struct type mismatch")) }
            }
            (TypeTerm::Structural(sa), TypeTerm::Structural(sb)) => {
                if sa == sb { Ok(()) } else { Err(Error::from("structural type mismatch")) }
            }
            (TypeTerm::Enum(ae), TypeTerm::Enum(be)) => {
                if ae == be { Ok(()) } else { Err(Error::from("enum type mismatch")) }
            }
            (TypeTerm::Custom(a), TypeTerm::Custom(b)) => {
                if a == b {
                    Ok(())
                } else if matches!(a, Ty::Array(_)) && matches!(b, Ty::Array(_)) {
                    if format!("{}", a) == format!("{}", b) { Ok(()) } else { Err(Error::from(
                        format!("custom type mismatch: {} vs {}", a, b))) }
                } else {
                    Err(Error::from(format!("custom type mismatch: {} vs {}", a, b)))
                }
            }
            (TypeTerm::Tuple(a_elems), TypeTerm::Tuple(b_elems)) => {
                if a_elems.len() != b_elems.len() { return Err(Error::from("tuple length mismatch")); }
                for (a_elem, b_elem) in a_elems.into_iter().zip(b_elems.into_iter()) {
                    self.unify(a_elem, b_elem)?;
                }
                Ok(())
            }
            (TypeTerm::Function(a_func), TypeTerm::Function(b_func)) => {
                if a_func.params.len() != b_func.params.len() { return Err(Error::from("function arity mismatch")); }
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
            (TypeTerm::Unknown, _other) | (_other, TypeTerm::Unknown) => Ok(()),
            (TypeTerm::Any, _other) | (_other, TypeTerm::Any) => Ok(()),
            (left, right) => Err(Error::from(format!("type mismatch: {:?} vs {:?}", left, right))),
        }
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
                Ty::Function(TypeFunction { params, generics_params: Vec::new(), ret_ty: Some(Box::new(ret)) })
            }
            TypeTerm::Slice(elem) => {
                let elem_ty = self.resolve_to_ty(elem)?;
                Ty::Slice(TypeSlice { elem: Box::new(elem_ty) })
            }
            TypeTerm::Vec(elem) => {
                let elem_ty = self.resolve_to_ty(elem)?;
                Ty::Vec(TypeVec { ty: Box::new(elem_ty) })
            }
            TypeTerm::Reference(elem) => {
                let elem_ty = self.resolve_to_ty(elem)?;
                Ty::Reference(TypeReference { ty: Box::new(elem_ty), mutability: None, lifetime: None })
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
            Ty::Struct(struct_ty) => {
                self.struct_defs.insert(struct_ty.name.as_str().to_string(), struct_ty.clone());
                self.bind(var, TypeTerm::Struct(struct_ty.clone()));
            }
            Ty::Structural(structural) => self.bind(var, TypeTerm::Structural(structural.clone())),
            Ty::Enum(enum_ty) => self.bind(var, TypeTerm::Enum(enum_ty.clone())),
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
            other => {
                // Fallback for types not yet covered
                self.bind(var, TypeTerm::Custom(other.clone()));
            }
        }
        Ok(var)
    }
}
