use super::*;

impl HirTypeChecker {
    pub(super) fn method_call_actuals(&self, signature: &Ty, actuals: &[Ty]) -> Vec<Ty> {
        let TyKind::FnPtr(signature) = &signature.kind else {
            return actuals.to_vec();
        };
        let Some(expected_receiver) = signature.binder.value.inputs.first() else {
            return actuals.to_vec();
        };
        let Some(actual_receiver) = actuals.first() else {
            return actuals.to_vec();
        };
        if !matches!(expected_receiver.kind, TyKind::Ref(_, _, _)) {
            let mut adjusted = actuals.to_vec();
            let mut receiver = actual_receiver.clone();
            while let TyKind::Ref(_, inner, _) = &receiver.kind {
                receiver = (**inner).clone();
            }
            adjusted[0] = receiver;
            return adjusted;
        }
        let TyKind::Ref(_, _, mutability) = &expected_receiver.kind else {
            return actuals.to_vec();
        };
        if matches!(actual_receiver.kind, TyKind::Ref(_, _, _)) {
            return actuals.to_vec();
        }
        let mut adjusted = actuals.to_vec();
        adjusted[0] = Ty {
            kind: TyKind::Ref(
                ty::Region::ReErased,
                Box::new(actual_receiver.clone()),
                *mutability,
            ),
        };
        adjusted
    }

    pub(super) fn instantiate_call(
        &self,
        callable: &Ty,
        actuals: &[Ty],
        generics: Option<&hir::Generics>,
    ) -> Result<Option<(HashMap<ty::ParamTy, Ty>, Ty)>> {
        self.instantiate_call_with_expected(callable, actuals, generics, None)
    }

    /// Instantiates an ordinary function call using both argument and result
    /// constraints. Rustc collects the obligations created by the arguments
    /// and by the expected result in the same inference context; this matters
    /// for a type parameter such as `Dst` in `from_raw_parts_mut`, whose only
    /// occurrence is in the return type. Keeping the result constraint here
    /// also lets callers retain the completed substitution for monomorphization.
    pub(super) fn instantiate_call_with_expected(
        &self,
        callable: &Ty,
        actuals: &[Ty],
        generics: Option<&hir::Generics>,
        expected: Option<&Ty>,
    ) -> Result<Option<(HashMap<ty::ParamTy, Ty>, Ty)>> {
        self.instantiate_call_with_explicit_args_and_expected(
            callable, actuals, generics, None, expected,
        )
    }

    pub(super) fn instantiate_call_with_explicit_args(
        &self,
        callable: &Ty,
        actuals: &[Ty],
        generics: Option<&hir::Generics>,
        explicit_args: Option<&[Ty]>,
    ) -> Result<Option<(HashMap<ty::ParamTy, Ty>, Ty)>> {
        self.instantiate_call_with_explicit_args_and_expected(
            callable,
            actuals,
            generics,
            explicit_args,
            None,
        )
    }

    fn instantiate_call_with_explicit_args_and_expected(
        &self,
        callable: &Ty,
        actuals: &[Ty],
        generics: Option<&hir::Generics>,
        explicit_args: Option<&[Ty]>,
        expected: Option<&Ty>,
    ) -> Result<Option<(HashMap<ty::ParamTy, Ty>, Ty)>> {
        let TyKind::FnPtr(signature) = &callable.kind else {
            return Ok(None);
        };
        if signature.binder.value.inputs.len() != actuals.len() {
            // Candidate selection probes several declarations in one
            // receiver bucket. Arity is part of that probe, so a mismatch
            // must stay local to this candidate rather than becoming a
            // permanent diagnostic before another candidate is considered.
            return Ok(None);
        }
        let mut substitutions: HashMap<ty::ParamTy, Ty> = HashMap::new();
        if let (Some(generics), Some(explicit_args)) = (generics, explicit_args) {
            let type_params = generics.params.iter().enumerate().filter(|(_, parameter)| {
                matches!(parameter.kind, hir::GenericParamKind::Type { .. })
            });
            let mut params = type_params.peekable();
            for argument in explicit_args {
                let Some((index, parameter)) = params.next() else {
                    return Ok(None);
                };
                substitutions.insert(
                    ty::ParamTy {
                        index: parameter.def_id.index,
                        name: parameter.name.clone(),
                    },
                    argument.clone(),
                );
            }
        }
        for (expected, actual) in signature.binder.value.inputs.iter().zip(actuals) {
            self.unify_call_types(expected, actual, &mut substitutions)?;
        }
        if let Some(generics) = generics {
            self.infer_fn_bound_outputs(generics, &mut substitutions);
        }
        if let Some(expected) = expected.filter(|ty| !ty_contains_error(ty)) {
            // This is deliberately speculative. A result mismatch is a
            // normal later type error, while a successful unification must
            // feed the same substitution map as argument inference.
            let mut trial = substitutions.clone();
            if self
                .unify_call_types_probe(&signature.binder.value.output, expected, &mut trial)
                .is_ok()
            {
                substitutions = trial;
            }
        }
        let output = self.substitute_param_map(&signature.binder.value.output, &substitutions);
        Ok(Some((substitutions, output)))
    }

    pub(super) fn infer_fn_bound_outputs(
        &self,
        generics: &hir::Generics,
        substitutions: &mut HashMap<ty::ParamTy, Ty>,
    ) {
        let mut params = HashMap::new();
        for (index, parameter) in generics.params.iter().enumerate() {
            if matches!(parameter.kind, hir::GenericParamKind::Type { .. }) {
                params.insert(
                    parameter.name.clone(),
                    ty::ParamTy {
                        index: parameter.def_id.index,
                        name: parameter.name.clone(),
                    },
                );
            }
        }
        for (index, parameter) in generics.params.iter().enumerate() {
            if !matches!(parameter.kind, hir::GenericParamKind::Type { .. }) {
                continue;
            }
            let source = ty::ParamTy {
                index: parameter.def_id.index,
                name: parameter.name.clone(),
            };
            let Some(Ty {
                kind: TyKind::FnPtr(signature),
            }) = substitutions.get(&source)
            else {
                continue;
            };
            let output = (*signature.binder.value.output).clone();
            let mut bounds = Vec::new();
            fn collect<'a>(bound: &'a hir::TypeExpr, out: &mut Vec<&'a hir::TypeExpr>) {
                if let hir::TypeExprKind::TypeBinaryOp(op) = &bound.kind
                    && matches!(op.kind, fp_core::ast::TypeBinaryOpKind::Add)
                {
                    collect(&op.lhs, out);
                    collect(&op.rhs, out);
                } else {
                    out.push(bound);
                }
            }
            for bound in &parameter.bounds {
                collect(bound, &mut bounds);
            }
            for bound in bounds {
                let hir::TypeExprKind::FnPtr(_) = &bound.kind else {
                    continue;
                };
                let hir::TypeExprKind::FnPtr(fn_ptr) = &bound.kind else {
                    unreachable!()
                };
                let mut outputs = Vec::new();
                collect(&fn_ptr.output, &mut outputs);
                for output_path in outputs {
                    let hir::TypeExprKind::Path(path) = &output_path.kind else {
                        continue;
                    };
                    if path.segments.len() != 1 {
                        continue;
                    }
                    if let Some(target) = params.get(&path.segments[0].name) {
                        let replace = match substitutions.get(target) {
                            None => true,
                            Some(existing) => {
                                matches!(existing.kind, TyKind::Param(_))
                                    && !matches!(output.kind, TyKind::Param(_))
                            }
                        };
                        if replace {
                            substitutions.insert(target.clone(), output.clone());
                        }
                    }
                }
            }
        }
    }

    pub(super) fn generic_call_args(
        &self,
        def_id: hir::DefId,
        substitutions: &HashMap<ty::ParamTy, Ty>,
    ) -> Result<Option<Vec<Ty>>> {
        let function = match self.program_rc().item(def_id.clone()) {
            Some(item) => match &item.kind {
                hir::ItemKind::Function(function) => Some(function.clone()),
                _ => None,
            },
            // `item`/`def_map` only ever holds *top-level* items
            // (struct/enum/fn/const/impl) — an `impl` block's own methods
            // are never flattened into it as their own entries. A UFCS
            // associated-function call (`HashMap::from(..)`) resolves
            // straight to such an impl-member `DefId`, so the lookup above
            // always misses for it — `impl_method_item_index` (built once
            // per package) gives the enclosing impl's item index directly,
            // same-package or cross-package alike, since `def_id` already
            // carries its owning `package_id`.
            None => self
                .program_rc()
                .package(&def_id.package_id)
                .and_then(|package| {
                    let impl_def_id = package.impl_method_item_index.get(&def_id)?;
                    let item = package.def_map.get(impl_def_id)?;
                    let hir::ItemKind::Impl(impl_item) = &item.kind else {
                        return None;
                    };
                    impl_item.items.iter().find_map(|impl_member| {
                        if impl_member.def_id != def_id {
                            return None;
                        }
                        match &impl_member.kind {
                            hir::ImplItemKind::Method(function) => Some(function.clone()),
                            _ => None,
                        }
                    })
                }),
        };
        let Some(function) = function else {
            return Ok(None);
        };
        if function.sig.generics.params.is_empty() {
            return Ok(None);
        }
        let mut args = Vec::with_capacity(function.sig.generics.params.len());
        for (index, parameter) in function.sig.generics.params.iter().enumerate() {
            let param = ty::ParamTy {
                index: parameter.def_id.index,
                name: parameter.name.clone(),
            };
            let Some(argument) = substitutions.get(&param) else {
                self.record_error(format!(
                    "could not infer generic parameter `{}` for `{def_id}`",
                    parameter.name
                ));
                return Ok(None);
            };
            args.push(argument.clone());
        }
        Ok(Some(args))
    }

    /// Like `require_same_hard`, but tolerates `TyKind::Infer` holes on the
    /// `annotation` side (e.g. a `let x: Vec<_> = ...;`'s elided `_`) by
    /// treating them as matching whatever `concrete` has in that position —
    /// an elided `_` never contradicts the resolved type, unlike an
    /// explicitly-written, disagreeing one. Recurses structurally so a hole
    /// nested inside a generic argument (`Vec<_>`, `Option<_>`, ...) is
    /// tolerated the same way a top-level one is; any position where
    /// `annotation` wrote something concrete still has to agree with
    /// `concrete` exactly.
    pub(super) fn ty_matches_with_infer_holes(annotation: &Ty, concrete: &Ty) -> bool {
        match (&annotation.kind, &concrete.kind) {
            // rustc's error type is a recovery type: once an earlier
            // resolution/type-checking error has produced it, later
            // compatibility checks must accept it without manufacturing a
            // second mismatch diagnostic. It is not an inference wildcard;
            // it simply prevents error recovery from poisoning the enclosing
            // expression.
            (TyKind::Error(_), _) | (_, TyKind::Error(_)) => true,
            (TyKind::Infer(_), _) => true,
            (TyKind::Ref(_, a, am), TyKind::Ref(_, c, cm)) if am == cm => {
                Self::ty_matches_with_infer_holes(a, c)
            }
            (TyKind::RawPtr(a), TyKind::RawPtr(c)) if a.mutbl == c.mutbl => {
                Self::ty_matches_with_infer_holes(&a.ty, &c.ty)
            }
            (TyKind::Tuple(a), TyKind::Tuple(c)) if a.len() == c.len() => a
                .iter()
                .zip(c)
                .all(|(a, c)| Self::ty_matches_with_infer_holes(a, c)),
            (TyKind::Array(a, a_len), TyKind::Array(c, c_len)) => {
                Self::ty_matches_with_infer_holes(a, c) && Self::consts_compatible(a_len, c_len)
            }
            (TyKind::Slice(a), TyKind::Slice(c)) => Self::ty_matches_with_infer_holes(a, c),
            (TyKind::Adt(a_def, a_args), TyKind::Adt(c_def, c_args))
                if a_def.did == c_def.did && a_args.len() == c_args.len() =>
            {
                a_args
                    .iter()
                    .zip(c_args)
                    .all(|(a, c)| Self::generic_arg_matches_with_infer_holes(a, c))
            }
            (TyKind::FnDef(a_def, a_args), TyKind::FnDef(c_def, c_args))
            | (TyKind::Closure(a_def, a_args), TyKind::Closure(c_def, c_args))
            | (TyKind::Generator(a_def, a_args, _), TyKind::Generator(c_def, c_args, _))
            | (TyKind::Opaque(a_def, a_args), TyKind::Opaque(c_def, c_args))
                if a_def == c_def && a_args.len() == c_args.len() =>
            {
                a_args
                    .iter()
                    .zip(c_args)
                    .all(|(a, c)| Self::generic_arg_matches_with_infer_holes(a, c))
            }
            (TyKind::Projection(a), TyKind::Projection(c))
                if a.item_def_id == c.item_def_id && a.substs.len() == c.substs.len() =>
            {
                a.substs
                    .iter()
                    .zip(&c.substs)
                    .all(|(a, c)| Self::generic_arg_matches_with_infer_holes(a, c))
            }
            (TyKind::Dynamic(a, _), TyKind::Dynamic(c, _)) => a == c,
            _ => annotation == concrete,
        }
    }

    fn generic_arg_matches_with_infer_holes(
        annotation: &GenericArg,
        concrete: &GenericArg,
    ) -> bool {
        match (annotation, concrete) {
            (GenericArg::Type(annotation), GenericArg::Type(concrete)) => {
                Self::ty_matches_with_infer_holes(annotation, concrete)
            }
            (GenericArg::Const(annotation), GenericArg::Const(concrete)) => {
                Self::const_matches_with_infer_holes(annotation, concrete)
            }
            (GenericArg::Lifetime(_), GenericArg::Lifetime(_)) => true,
            _ => annotation == concrete,
        }
    }

    fn const_matches_with_infer_holes(
        annotation: &ty::ConstKind,
        concrete: &ty::ConstKind,
    ) -> bool {
        match (annotation, concrete) {
            (ty::ConstKind::Infer(_), _) => true,
            (ty::ConstKind::Unevaluated(annotation), ty::ConstKind::Unevaluated(concrete))
                if annotation.def == concrete.def
                    && annotation.substs.len() == concrete.substs.len() =>
            {
                annotation
                    .substs
                    .iter()
                    .zip(&concrete.substs)
                    .all(|(a, c)| Self::generic_arg_matches_with_infer_holes(a, c))
            }
            _ => annotation == concrete,
        }
    }

    /// A strict, top-level structural compatibility check — deliberately
    /// narrower than `unify_call_types`, which is a lenient *substitution*
    /// helper whose catch-all (`require_same`) always returns `Ok(())`
    /// even on a genuine mismatch (it only records a diagnostic; several
    /// callers rely on that non-failing behavior). Using
    /// `unify_call_types(..).is_ok()` alone as a receiver-matching gate is
    /// therefore unsound: an unrelated pairing like `Adt(Option, [T])` vs
    /// `Array(i64, 3)` falls through every real structural arm to
    /// `require_same` and reports a false match. This function is only
    /// used to gate *which* impl candidate's self-type is even eligible to
    /// unify against a receiver in the first place — real substitution
    /// still happens via `unify_call_types` afterward, once this confirms
    /// the two are the same kind of type.
    pub(super) fn ty_shapes_compatible(a: &TyKind, b: &TyKind) -> bool {
        match (a, b) {
            (TyKind::Param(_), _) | (_, TyKind::Param(_)) => true,
            (TyKind::Ref(_, a, _), TyKind::Ref(_, b, _)) => {
                Self::ty_shapes_compatible(&a.kind, &b.kind)
            }
            (TyKind::Ref(_, a, _), b) => Self::ty_shapes_compatible(&a.kind, b),
            (a, TyKind::Ref(_, b, _)) => Self::ty_shapes_compatible(a, &b.kind),
            (TyKind::Slice(_), TyKind::Slice(_) | TyKind::Array(_, _)) => true,
            (TyKind::Array(_, _), TyKind::Array(_, _)) => true,
            (TyKind::Tuple(a), TyKind::Tuple(b)) => a.len() == b.len(),
            (TyKind::RawPtr(expected), TyKind::RawPtr(actual)) => {
                // Raw-pointer inherent method lookup keeps the receiver's
                // mutability exact. Rustc may coerce a pointer argument from
                // `*mut T` to `*const T`, but it does not use that coercion to
                // select a different receiver impl (`*mut T` must prefer its
                // own inherent method over the `*const T` one).
                expected.mutbl == actual.mutbl
            }
            (TyKind::FnPtr(_), TyKind::FnPtr(_)) => true,
            (TyKind::Adt(a, _), TyKind::Adt(b, _)) => a.did == b.did,
            (TyKind::Bool, TyKind::Bool)
            | (TyKind::Char, TyKind::Char)
            | (TyKind::Int(_), TyKind::Int(_))
            | (TyKind::Uint(_), TyKind::Uint(_))
            | (TyKind::Float(_), TyKind::Float(_))
            | (TyKind::Never, TyKind::Never)
            | (TyKind::Any, TyKind::Any)
            | (TyKind::Type, TyKind::Type) => true,
            _ => a == b,
        }
    }

    pub(super) fn unify_call_types(
        &self,
        expected: &Ty,
        actual: &Ty,
        substitutions: &mut HashMap<ty::ParamTy, Ty>,
    ) -> Result<()> {
        self.unify_call_types_impl(expected, actual, substitutions, true)
    }

    /// `unify_call_types`, but purely speculative: used by candidate-impl
    /// matching (`method_output_at`'s `matches_receiver`, `generic_args_
    /// compatible`, ...) to test compatibility without committing to it —
    /// the caller always discards the actual `Result` via `.is_ok()`/
    /// `.is_err()` and moves on to the next candidate on failure. Real
    /// Rust's own method-resolution candidate search never turns a
    /// rejected candidate into a diagnostic; only "no candidate matched
    /// at all" is a real error. Plain `unify_call_types` records every
    /// mismatch as a permanent diagnostic (`require_same`'s whole point,
    /// for *committed* checks) — reused unchanged for a speculative probe,
    /// every non-matching candidate among (for example) a numeric type's
    /// dozen near-identical width-specific impls would permanently pollute
    /// the diagnostic list once per rejected candidate per call site,
    /// even though the search goes on to find the right one and the
    /// overall typecheck never actually fails.
    pub(super) fn unify_call_types_probe(
        &self,
        expected: &Ty,
        actual: &Ty,
        substitutions: &mut HashMap<ty::ParamTy, Ty>,
    ) -> Result<()> {
        self.unify_call_types_impl(expected, actual, substitutions, false)
    }

    pub(super) fn unify_call_types_impl(
        &self,
        expected: &Ty,
        actual: &Ty,
        substitutions: &mut HashMap<ty::ParamTy, Ty>,
        record: bool,
    ) -> Result<()> {
        let expected = self.resolve_infer(expected);
        let actual = self.resolve_infer(actual);
        match (&expected.kind, &actual.kind) {
            // Match rustc's `Ty::new_error` propagation. An error recovered
            // from name/type resolution satisfies the surrounding relation,
            // but must not bind inference variables or participate in
            // structural matching as if it were a concrete type.
            (TyKind::Error(_), _) | (_, TyKind::Error(_)) => Ok(()),
            (TyKind::Infer(var), _) => {
                self.bind_infer(var.clone(), &actual);
                Ok(())
            }
            (_, TyKind::Infer(var)) => {
                self.bind_infer(var.clone(), &expected);
                Ok(())
            }
            (TyKind::Param(param), _) => {
                if let Some(previous) = substitutions.get(param) {
                    if matches!(&previous.kind, TyKind::Param(previous_param) if previous_param == param)
                    {
                        return if *previous == actual {
                            Ok(())
                        } else if record {
                            self.require_same(previous, &actual)
                        } else {
                            Err(Error::from("speculative type mismatch"))
                        };
                    }
                    let mut trial = substitutions.clone();
                    trial.remove(param);
                    self.unify_call_types_impl(previous, &actual, &mut trial, record)?;
                    *substitutions = trial;
                } else {
                    substitutions.insert(param.clone(), actual.clone());
                }
                Ok(())
            }
            (_, TyKind::Param(param)) => {
                if let Some(previous) = substitutions.get(param) {
                    if matches!(&previous.kind, TyKind::Param(previous_param) if previous_param == param)
                    {
                        return if *previous == expected {
                            Ok(())
                        } else if record {
                            self.require_same(previous, &expected)
                        } else {
                            Err(Error::from("speculative type mismatch"))
                        };
                    }
                    let mut trial = substitutions.clone();
                    trial.remove(param);
                    self.unify_call_types_impl(previous, &expected, &mut trial, record)?;
                    *substitutions = trial;
                } else {
                    substitutions.insert(param.clone(), expected.clone());
                }
                Ok(())
            }
            (TyKind::Ref(_, expected, expected_mut), TyKind::Ref(_, actual, actual_mut))
                if expected_mut == actual_mut
                    || (*expected_mut == ty::Mutability::Not
                        && *actual_mut == ty::Mutability::Mut) =>
            {
                self.unify_call_types_impl(expected, &actual, substitutions, record)
            }
            // String literals are represented as unsized slice values in HIR,
            // while APIs expose them through the ordinary shared-reference
            // spelling `&str` (`&[u8]`). This is the one-way borrow adjustment
            // needed at call sites; mutable references must not be synthesized
            // from a literal.
            (TyKind::Ref(_, expected, expected_mut), TyKind::Slice(actual))
                if *expected_mut == ty::Mutability::Not
                    && matches!(expected.kind, TyKind::Slice(_)) =>
            {
                self.unify_call_types_impl(
                    expected,
                    &Ty {
                        kind: TyKind::Slice(actual.clone()),
                    },
                    substitutions,
                    record,
                )
            }
            (TyKind::FnPtr(expected), TyKind::FnPtr(actual))
                if expected.binder.value.inputs.len() == actual.binder.value.inputs.len() =>
            {
                for (expected, actual) in expected
                    .binder
                    .value
                    .inputs
                    .iter()
                    .zip(&actual.binder.value.inputs)
                {
                    self.unify_call_types_impl(expected, actual, substitutions, record)?;
                }
                self.unify_call_types_impl(
                    &expected.binder.value.output,
                    &actual.binder.value.output,
                    substitutions,
                    record,
                )
            }
            (TyKind::Dynamic(expected_predicates, _), TyKind::Dynamic(actual_predicates, _)) => {
                let Some(expected_principal) = expected_predicates.first() else {
                    return if actual_predicates.is_empty() {
                        Ok(())
                    } else {
                        Err(Error::from("trait-object principal mismatch"))
                    };
                };
                if actual_predicates.first() != Some(expected_principal)
                    || !expected_predicates
                        .iter()
                        .skip(1)
                        .all(|bound| actual_predicates.contains(bound))
                {
                    return if record {
                        self.require_same(&expected, &actual)
                    } else {
                        Err(Error::from("trait-object upcast predicate mismatch"))
                    };
                }
                Ok(())
            }
            (TyKind::Tuple(expected), TyKind::Tuple(actual)) if expected.len() == actual.len() => {
                expected
                    .iter()
                    .zip(actual)
                    .try_for_each(|(expected, actual)| {
                        self.unify_call_types_impl(expected, actual, substitutions, record)
                    })
            }
            (TyKind::Array(expected, expected_len), TyKind::Array(actual, actual_len)) => {
                if !Self::consts_compatible(expected_len, actual_len) {
                    return if record {
                        self.require_same(&expected, &actual)
                    } else {
                        Err(Error::from("speculative array-length mismatch"))
                    };
                }
                self.unify_call_types_impl(expected, actual, substitutions, record)
            }
            (TyKind::Slice(expected), TyKind::Slice(actual))
            | (TyKind::Slice(expected), TyKind::Array(actual, _)) => {
                self.unify_call_types_impl(expected, actual, substitutions, record)
            }
            (TyKind::Adt(expected, expected_args), TyKind::Adt(actual, actual_args))
                if expected.did == actual.did && expected_args.len() == actual_args.len() =>
            {
                for (expected, actual) in expected_args.iter().zip(actual_args) {
                    self.unify_generic_arg(expected, actual, substitutions, record)?;
                }
                Ok(())
            }
            (TyKind::Projection(expected), TyKind::Projection(actual))
                if expected.item_def_id == actual.item_def_id
                    && expected.substs.len() == actual.substs.len() =>
            {
                for (expected, actual) in expected.substs.iter().zip(&actual.substs) {
                    self.unify_generic_arg(expected, actual, substitutions, record)?;
                }
                Ok(())
            }
            (
                TyKind::FnDef(expected_def, expected_args),
                TyKind::FnDef(actual_def, actual_args),
            )
            | (
                TyKind::Closure(expected_def, expected_args),
                TyKind::Closure(actual_def, actual_args),
            )
            | (
                TyKind::Generator(expected_def, expected_args, _),
                TyKind::Generator(actual_def, actual_args, _),
            )
            | (
                TyKind::Opaque(expected_def, expected_args),
                TyKind::Opaque(actual_def, actual_args),
            ) if expected_def == actual_def && expected_args.len() == actual_args.len() => {
                for (expected, actual) in expected_args.iter().zip(actual_args) {
                    self.unify_generic_arg(expected, actual, substitutions, record)?;
                }
                Ok(())
            }
            // Raw-pointer pointees must structurally unify. Equal
            // mutability is required except for Rust's one-way
            // `*mut T`-to-`*const T` argument coercion. Keeping the pointee
            // probe transactional prevents a failed match from leaking
            // substitutions.
            (TyKind::RawPtr(expected), TyKind::RawPtr(actual))
                if expected.mutbl == actual.mutbl
                    || (expected.mutbl == ty::Mutability::Not
                        && actual.mutbl == ty::Mutability::Mut) =>
            {
                let mut trial = substitutions.clone();
                self.unify_call_types_impl(&expected.ty, &actual.ty, &mut trial, record)?;
                *substitutions = trial;
                Ok(())
            }
            (TyKind::RawPtr(expected), TyKind::RawPtr(actual)) => {
                let expected = Ty {
                    kind: TyKind::RawPtr(expected.clone()),
                };
                let actual = Ty {
                    kind: TyKind::RawPtr(actual.clone()),
                };
                if record {
                    self.require_same(&expected, &actual)
                } else {
                    Err(Error::from("speculative raw-pointer mismatch"))
                }
            }
            (TyKind::Ref(_, actual, _), TyKind::RawPtr(expected)) => {
                // Local initializers call the unifier with the inferred
                // expression type first and the annotation second. Mirror
                // the reference-to-raw-pointer coercion in that direction.
                let mut trial = substitutions.clone();
                if self
                    .unify_call_types_impl(actual, &expected.ty, &mut trial, record)
                    .is_ok()
                {
                    *substitutions = trial;
                }
                Ok(())
            }
            _ => {
                if record {
                    self.require_same(&expected, &actual)
                } else if expected == actual
                    || matches!(expected.kind, TyKind::Never)
                    || matches!(actual.kind, TyKind::Never)
                {
                    Ok(())
                } else {
                    Err(Error::from("speculative type mismatch"))
                }
            }
        }
    }

    /// Whether an impl block's generic arguments (e.g. `Vec`'s `[&str]` in
    /// `impl Vec<&str> { .. }`) are compatible with a call site's actual
    /// receiver arguments (e.g. `Vec`'s `[str]` for a `Vec<str>` value) —
    /// used by `method_output` to pick the right specialization instead of
    /// letting any `impl SomeGeneric<T> { .. }` match every instantiation.
    /// Generic impl arguments are checked with the same structural probe as
    /// call arguments. This permits `impl<T> Vec<T>` while still rejecting a
    /// partially concrete candidate whose nested arguments conflict.
    pub(super) fn generic_args_compatible(
        &self,
        impl_args: &[GenericArg],
        receiver_args: &[GenericArg],
    ) -> bool {
        if impl_args.len() != receiver_args.len() {
            return false;
        }
        let mut substitutions = HashMap::new();
        impl_args
            .iter()
            .zip(receiver_args)
            .all(|(impl_arg, receiver_arg)| {
                self.unify_generic_arg(impl_arg, receiver_arg, &mut substitutions, false)
                    .is_ok()
            })
    }

    fn unify_generic_arg(
        &self,
        expected: &GenericArg,
        actual: &GenericArg,
        substitutions: &mut HashMap<ty::ParamTy, Ty>,
        record: bool,
    ) -> Result<()> {
        match (expected, actual) {
            (GenericArg::Type(expected), GenericArg::Type(actual)) => {
                // Array-to-slice is a coercion-site adjustment. It is valid
                // for a function argument, but it cannot be applied inside a
                // nominal generic argument (for example, `Vec<[T; 3]>` is
                // not compatible with `Vec<[T]>`). The general call unifier
                // intentionally models the former, so reject the latter
                // before delegating to it.
                if Self::contains_nested_array_to_slice_coercion(expected, actual) {
                    return Err(Error::from("array-to-slice coercion in generic argument"));
                }
                self.unify_call_types_impl(expected, actual, substitutions, record)
            }
            (GenericArg::Const(expected), GenericArg::Const(actual))
                if Self::consts_compatible(expected, actual) =>
            {
                Ok(())
            }
            // Regions constrain borrowing but are not part of the nominal
            // identity of an instantiated ADT. This HIR checker erases
            // regions, so requiring their syntax to be equal rejects valid
            // instantiations that rustc accepts.
            (GenericArg::Lifetime(_), GenericArg::Lifetime(_)) => Ok(()),
            _ if expected == actual => Ok(()),
            _ => Err(Error::from("generic argument mismatch")),
        }
    }

    fn contains_nested_array_to_slice_coercion(expected: &Ty, actual: &Ty) -> bool {
        match (&expected.kind, &actual.kind) {
            (TyKind::Slice(_), TyKind::Array(_, _)) => true,
            (TyKind::Ref(_, expected, _), TyKind::Ref(_, actual, _)) => {
                Self::contains_nested_array_to_slice_coercion(expected, actual)
            }
            (TyKind::RawPtr(expected), TyKind::RawPtr(actual)) => {
                Self::contains_nested_array_to_slice_coercion(&expected.ty, &actual.ty)
            }
            (TyKind::Tuple(expected), TyKind::Tuple(actual)) if expected.len() == actual.len() => {
                expected.iter().zip(actual).any(|(expected, actual)| {
                    Self::contains_nested_array_to_slice_coercion(expected, actual)
                })
            }
            (TyKind::Array(expected, _), TyKind::Array(actual, _))
            | (TyKind::Slice(expected), TyKind::Slice(actual)) => {
                Self::contains_nested_array_to_slice_coercion(expected, actual)
            }
            (TyKind::FnPtr(expected), TyKind::FnPtr(actual))
                if expected.binder.value.inputs.len() == actual.binder.value.inputs.len() =>
            {
                expected
                    .binder
                    .value
                    .inputs
                    .iter()
                    .zip(&actual.binder.value.inputs)
                    .any(|(expected, actual)| {
                        Self::contains_nested_array_to_slice_coercion(expected, actual)
                    })
                    || Self::contains_nested_array_to_slice_coercion(
                        &expected.binder.value.output,
                        &actual.binder.value.output,
                    )
            }
            (TyKind::Adt(expected, expected_args), TyKind::Adt(actual, actual_args))
                if expected.did == actual.did && expected_args.len() == actual_args.len() =>
            {
                expected_args
                    .iter()
                    .zip(actual_args)
                    .any(|(expected, actual)| {
                        Self::generic_arg_contains_array_slice_coercion(expected, actual)
                    })
            }
            (TyKind::Projection(expected), TyKind::Projection(actual))
                if expected.item_def_id == actual.item_def_id
                    && expected.substs.len() == actual.substs.len() =>
            {
                expected
                    .substs
                    .iter()
                    .zip(&actual.substs)
                    .any(|(expected, actual)| {
                        Self::generic_arg_contains_array_slice_coercion(expected, actual)
                    })
            }
            _ => false,
        }
    }

    fn generic_arg_contains_array_slice_coercion(
        expected: &GenericArg,
        actual: &GenericArg,
    ) -> bool {
        match (expected, actual) {
            (GenericArg::Type(expected), GenericArg::Type(actual)) => {
                Self::contains_nested_array_to_slice_coercion(expected, actual)
            }
            (GenericArg::Const(expected), GenericArg::Const(actual)) => {
                let (ty::ConstKind::Unevaluated(expected), ty::ConstKind::Unevaluated(actual)) =
                    (expected, actual)
                else {
                    return false;
                };
                expected
                    .substs
                    .iter()
                    .zip(&actual.substs)
                    .any(|(expected, actual)| {
                        Self::generic_arg_contains_array_slice_coercion(expected, actual)
                    })
            }
            _ => false,
        }
    }

    /// Const arguments participate in type identity structurally. A const
    /// parameter or inference variable is an unresolved value, but a
    /// parameter is not a wildcard that makes two distinct concrete values
    /// equal. This mirrors rustc's const-argument relation without inventing
    /// a second substitution table in the HIR checker.
    fn consts_compatible(expected: &ty::ConstKind, actual: &ty::ConstKind) -> bool {
        match (expected, actual) {
            (ty::ConstKind::Infer(_), _) | (_, ty::ConstKind::Infer(_)) => true,
            (ty::ConstKind::Unevaluated(expected), ty::ConstKind::Unevaluated(actual))
                if expected.def == actual.def && expected.substs.len() == actual.substs.len() =>
            {
                expected
                    .substs
                    .iter()
                    .zip(&actual.substs)
                    .all(|(expected, actual)| match (expected, actual) {
                        (GenericArg::Type(expected), GenericArg::Type(actual)) => {
                            Self::ty_matches_with_infer_holes(expected, actual)
                        }
                        (GenericArg::Const(expected), GenericArg::Const(actual)) => {
                            Self::consts_compatible(expected, actual)
                        }
                        (GenericArg::Lifetime(_), GenericArg::Lifetime(_)) => true,
                        _ => expected == actual,
                    })
            }
            _ => expected == actual,
        }
    }

    /// Resolves a parameter through the substitution chain for the callers
    /// that need to inspect the immediate resolved value without rebuilding
    /// the surrounding type.  Cyclic or excessively deep chains terminate at
    /// their last value; full type substitution uses the cycle-aware walker
    /// below so nested generic arguments are normalized as well.
    pub(super) fn resolve_param_transitively<'a>(
        &self,
        param: &'a ty::ParamTy,
        substitutions: &'a HashMap<ty::ParamTy, Ty>,
    ) -> Option<&'a Ty> {
        let mut resolved = substitutions.get(param)?;
        for _ in 0..8 {
            let TyKind::Param(next_param) = &resolved.kind else {
                return Some(resolved);
            };
            let Some(next) = substitutions.get(next_param) else {
                return Some(resolved);
            };
            resolved = next;
        }
        Some(resolved)
    }

    /// Unify two branches of the same control-flow join (`if`/`else`,
    /// `match` arms) that are expected to produce the same type. Unlike
    /// `require_same`, this tolerates one side still carrying an
    /// uninstantiated `TyKind::Param` (e.g. `Option::None`'s type, which has
    /// no argument of its own to infer `T` from) against the other side's
    /// fully concrete type (e.g. `Option::Some(x)`'s `Option<i64>`) —
    /// exactly the same substitution `unify_call_types` already performs for
    /// call arguments, just tried in both directions since either branch
    /// could be the concrete one.
    pub(super) fn unify_branch_types(&self, a: &Ty, b: &Ty) -> Result<Ty> {
        if a == b {
            return Ok(a.clone());
        }
        let mut substitutions = HashMap::new();
        if self
            .unify_call_types_probe(a, b, &mut substitutions)
            .is_ok()
        {
            return Ok(self.substitute_param_map(a, &substitutions));
        }
        let mut substitutions = HashMap::new();
        if self
            .unify_call_types_probe(b, a, &mut substitutions)
            .is_ok()
        {
            return Ok(self.substitute_param_map(b, &substitutions));
        }
        self.require_same(a, b)?;
        Ok(a.clone())
    }

    pub(super) fn substitute_param_map(
        &self,
        ty: &Ty,
        substitutions: &HashMap<ty::ParamTy, Ty>,
    ) -> Ty {
        self.substitute_param_map_inner(ty, substitutions, &mut HashSet::new())
    }

    /// Applies substitutions recursively, including inside a replacement
    /// type.  Generic inference commonly produces a chain such as
    /// `T -> Vec<U>, U -> i32`; resolving only the outer `T` leaves `Vec<U>`
    /// behind and makes later associated-type projection lookups operate on
    /// a stale generic argument.  The `seen` set only guards malformed
    /// cyclic substitutions; rustc's inference normally produces an
    /// acyclic substitution table.
    fn substitute_param_map_inner(
        &self,
        ty: &Ty,
        substitutions: &HashMap<ty::ParamTy, Ty>,
        seen: &mut HashSet<ty::ParamTy>,
    ) -> Ty {
        match &ty.kind {
            TyKind::Param(param) => {
                let Some(resolved) = substitutions.get(param) else {
                    return ty.clone();
                };
                if !seen.insert(param.clone()) {
                    return ty.clone();
                }
                let result = self.substitute_param_map_inner(resolved, substitutions, seen);
                seen.remove(param);
                result
            }
            TyKind::Ref(region, inner, mutable) => Ty {
                kind: TyKind::Ref(
                    region.clone(),
                    Box::new(self.substitute_param_map_inner(inner, substitutions, seen)),
                    *mutable,
                ),
            },
            TyKind::RawPtr(value) => Ty {
                kind: TyKind::RawPtr(ty::TypeAndMut {
                    ty: Box::new(self.substitute_param_map_inner(&value.ty, substitutions, seen)),
                    mutbl: value.mutbl,
                }),
            },
            TyKind::Tuple(fields) => Ty {
                kind: TyKind::Tuple(
                    fields
                        .iter()
                        .map(|field| {
                            Box::new(self.substitute_param_map_inner(field, substitutions, seen))
                        })
                        .collect(),
                ),
            },
            TyKind::Array(inner, length) => Ty {
                kind: TyKind::Array(
                    Box::new(self.substitute_param_map_inner(inner, substitutions, seen)),
                    self.substitute_const_kind(length, substitutions, seen),
                ),
            },
            TyKind::Slice(inner) => Ty {
                kind: TyKind::Slice(Box::new(self.substitute_param_map_inner(
                    inner,
                    substitutions,
                    seen,
                ))),
            },
            TyKind::Adt(def, args) => Ty {
                kind: TyKind::Adt(
                    def.clone(),
                    args.iter()
                        .map(|arg| self.substitute_generic_arg(arg, substitutions, seen))
                        .collect(),
                ),
            },
            TyKind::FnDef(def, args) => Ty {
                kind: TyKind::FnDef(
                    def.clone(),
                    args.iter()
                        .map(|arg| self.substitute_generic_arg(arg, substitutions, seen))
                        .collect(),
                ),
            },
            TyKind::Closure(def, args) => Ty {
                kind: TyKind::Closure(
                    def.clone(),
                    args.iter()
                        .map(|arg| self.substitute_generic_arg(arg, substitutions, seen))
                        .collect(),
                ),
            },
            TyKind::Generator(def, args, movability) => Ty {
                kind: TyKind::Generator(
                    def.clone(),
                    args.iter()
                        .map(|arg| self.substitute_generic_arg(arg, substitutions, seen))
                        .collect(),
                    movability.clone(),
                ),
            },
            TyKind::Opaque(def, args) => Ty {
                kind: TyKind::Opaque(
                    def.clone(),
                    args.iter()
                        .map(|arg| self.substitute_generic_arg(arg, substitutions, seen))
                        .collect(),
                ),
            },
            TyKind::Projection(projection) => Ty {
                kind: TyKind::Projection(ty::ProjectionTy {
                    item_def_id: projection.item_def_id.clone(),
                    substs: projection
                        .substs
                        .iter()
                        .map(|arg| self.substitute_generic_arg(arg, substitutions, seen))
                        .collect(),
                }),
            },
            TyKind::GeneratorWitness(types) => Ty {
                kind: TyKind::GeneratorWitness(
                    types
                        .iter()
                        .map(|ty| {
                            Box::new(self.substitute_param_map_inner(ty, substitutions, seen))
                        })
                        .collect(),
                ),
            },
            _ => ty.clone(),
        }
    }

    fn substitute_generic_arg(
        &self,
        arg: &GenericArg,
        substitutions: &HashMap<ty::ParamTy, Ty>,
        seen: &mut HashSet<ty::ParamTy>,
    ) -> GenericArg {
        match arg {
            GenericArg::Type(ty) => {
                GenericArg::Type(self.substitute_param_map_inner(ty, substitutions, seen))
            }
            GenericArg::Const(constant) => {
                GenericArg::Const(self.substitute_const_kind(constant, substitutions, seen))
            }
            other => other.clone(),
        }
    }

    fn substitute_const_kind(
        &self,
        constant: &ty::ConstKind,
        substitutions: &HashMap<ty::ParamTy, Ty>,
        seen: &mut HashSet<ty::ParamTy>,
    ) -> ty::ConstKind {
        match constant {
            ty::ConstKind::Unevaluated(value) => ty::ConstKind::Unevaluated(ty::UnevaluatedConst {
                def: value.def.clone(),
                substs: value
                    .substs
                    .iter()
                    .map(|arg| self.substitute_generic_arg(arg, substitutions, seen))
                    .collect(),
            }),
            other => other.clone(),
        }
    }

    /// Finds the impl method `method_output` would eventually resolve for
    /// `receiver_ty`, returning just its *declared* signature (`Self`
    /// substituted from the already-known receiver type; the method's own
    /// generics, e.g. `map_or`'s `U`, left as unresolved `Param`s) —
    /// without needing any argument types. `Self`'s position always
    /// substitutes cleanly from `receiver_ty` alone, entirely independent
    /// of the call's other arguments, so this doesn't need to wait for
    /// them the way `instantiate_call`'s full unification does.
    ///
    /// This exists so `MethodCall` can seed a real expected-type hint for
    /// each argument *before* checking it (mirroring `Call`'s existing
    /// per-parameter hint) — the load-bearing case being a closure
    /// argument to a generic method like `Option::map_or(self, default: U,
    /// f: fn(T) -> U) -> U`: without this, `T` is unknown when the closure
    /// literal is checked, so its parameter silently gets an unusable
    /// placeholder type. Mirrors the matching loop in `method_output`
    /// (kept in sync deliberately, not factored into one shared loop,
    /// since the two stop at different points and diverging their control
    /// flow — `Result` vs best-effort `None` — reads more clearly split).
    pub(super) async fn method_declared_signature(
        &mut self,
        receiver_ty: &Ty,
        method: &hir::Symbol,
    ) -> Result<Option<Ty>> {
        let mut current = receiver_ty.clone();
        for _ in 0..8 {
            if let Some(signature) = self.method_declared_signature_at(&current, method).await? {
                return Ok(Some(signature));
            }
            match self.deref_target(&current).await {
                Some(target) => current = target,
                None => break,
            }
        }
        Ok(None)
    }

    /// `find_signature`'s logic, factored out of `method_declared_signature_at`
    /// as a plain associated fn (rather than a captured closure) so it can
    /// be `async`/awaited at both of that function's call sites.
    async fn method_declared_signature_apply_receiver(
        scope: &mut HirTypeChecker,
        receiver_ty: &Ty,
        function: &hir::Function,
    ) -> Result<Option<Ty>> {
        let signature = scope.function_signature(function).await?;
        let TyKind::FnPtr(sig) = &signature.kind else {
            return Ok(None);
        };
        // Associated functions have no `self` input, but their signature can
        // still mention the impl's generic parameters. Resolve those
        // parameters from the impl self type exactly as method lookup
        // resolves a receiver; otherwise `Cap<T>::new_unchecked` keeps `T`
        // unresolved even when the selected receiver is `Cap<u8>`.
        let mut substitutions = HashMap::new();
        if let Some(impl_self_ty) = scope.self_type.as_ref() {
            let impl_self_ty = match &impl_self_ty.kind {
                TyKind::Ref(_, inner, _) => inner.as_ref(),
                _ => impl_self_ty,
            };
            if scope
                .unify_call_types_probe(impl_self_ty, receiver_ty, &mut substitutions)
                .is_err()
            {
                return Ok(None);
            }
        }
        let has_self_param = matches!(
            function.sig.inputs.first().map(|param| &param.pat.kind),
            Some(hir::PatKind::Binding { name, .. }) if name.as_str() == "self"
        );
        if !has_self_param {
            // An associated function called via `Self::name(..)` (e.g.
            // `Layout::is_size_alignment_valid(size, alignment)`) takes no
            // receiver at all — the caller already supplies every argument
            // explicitly, so there's no `Self` position to unify here.
            // The caller's own `instantiate_call` against the explicit call
            // arguments does the method-generic unification. Impl-generic
            // substitutions, however, come from the receiver above and
            // must be applied before returning this signature.
            let substituted = scope.substitute_param_map_fn_sig(&sig.binder.value, &substitutions);
            return Ok(Some(Ty {
                kind: TyKind::FnPtr(ty::PolyFnSig {
                    binder: ty::Binder {
                        value: substituted,
                        bound_vars: sig.binder.bound_vars.clone(),
                    },
                }),
            }));
        }
        let self_input = &sig.binder.value.inputs[0];
        // Method lookup may insert the receiver borrow required by the
        // declared `&self`/`&mut self` receiver. Keep this adjustment local
        // to the method receiver position; ordinary call-type unification
        // must not dereference arbitrary reference/value pairs.
        let (self_input_probe, receiver_probe) = match (&self_input.kind, &receiver_ty.kind) {
            (TyKind::Ref(_, inner, _), kind) if !matches!(kind, TyKind::Ref(_, _, _)) => {
                (&**inner, receiver_ty)
            }
            _ => (self_input.as_ref(), receiver_ty),
        };
        // `Self`'s position, substituted from the *actual*
        // receiver — everything else in the signature stays
        // in terms of the method's own generics for now.
        // Speculative: the caller (`method_output_at`/`method_declared_
        // signature_at`) tries every candidate impl in turn and silently
        // moves on when this returns `None` — a rejected candidate here is
        // never a real type error, so this must not permanently record one
        // (see `unify_call_types_probe`'s own doc comment).
        if scope
            .unify_call_types_probe(self_input_probe, receiver_probe, &mut substitutions)
            .is_err()
        {
            return Ok(None);
        }
        let substituted = scope.substitute_param_map_fn_sig(&sig.binder.value, &substitutions);
        Ok(Some(Ty {
            kind: TyKind::FnPtr(ty::PolyFnSig {
                binder: ty::Binder {
                    value: substituted,
                    bound_vars: sig.binder.bound_vars.clone(),
                },
            }),
        }))
    }

    pub(super) async fn method_declared_signature_at(
        &mut self,
        receiver_ty: &Ty,
        method: &hir::Symbol,
    ) -> Result<Option<Ty>> {
        let receiver_ty = match &receiver_ty.kind {
            TyKind::Ref(_, inner, _) => inner.as_ref(),
            _ => receiver_ty,
        };
        // A still-generic receiver (`self: T` inside a default trait
        // method body) has no impl to search at all — `T` is abstract,
        // not a concrete type any impl's self-type could ever unify
        // against. Resolve directly from `T`'s own trait bounds instead,
        // the same way rustc resolves a generic receiver's method (from
        // `ParamEnv`, never impl search) — mirrors `T::method(..)`'s own
        // resolution via `generic_param_bound_method_signature` for the
        // identical underlying case, just reached through `.method()`
        // call syntax instead of an explicit type-relative path.
        if let TyKind::Param(param) = &receiver_ty.kind {
            // A bound lookup is authoritative when it finds the method, but
            // a miss must fall through. Rust permits an associated function
            // on a generic receiver to come from a blanket impl whose trait
            // is not a bound on the receiver (the local `ConvertVec` impl in
            // `slice::to_vec_in` is the motivating case).
            if let Some(signature) = self
                .generic_param_bound_method_signature(&param.name, method)
                .await?
            {
                return Ok(Some(signature));
            }
        }
        // `hir::HirProgram::impls_for_adt` is the fast-reject path for an
        // ADT receiver (self-type also resolves to `TyKind::Adt` with the
        // same `did`); a concrete non-ADT receiver (primitive/tuple/
        // slice/etc.) uses the shape-bucketed counterpart instead —
        // either way, a bounded, indexed lookup, never a scan over every
        // impl in the workspace (see `shape_and_blanket_candidates`'s doc
        // comment for why that's a hard requirement, not a nicety).
        // `program` is cloned out first so the borrow doesn't outlive the
        // `&mut self` calls below.
        let program = self.program_rc();
        let receiver_def = match &receiver_ty.kind {
            TyKind::Adt(receiver, _) => Some(receiver.did.clone()),
            _ => None,
        };
        let candidates = method_candidates(&program, &receiver_ty.kind);
        let expected_output = self.expected_expr_type.clone();
        for item in candidates {
            let hir::ItemKind::Impl(impl_item) = &item.kind else {
                continue;
            };
            // Reject impls that cannot provide this item before entering a
            // generic scope. Candidate buckets are indexed by receiver
            // shape, not by member name, and std contains many impls for the
            // same receiver. The old order cloned the full checker for every
            // one of those irrelevant impls.
            let declares_item = impl_item.items.iter().any(|item| item.name == *method);
            let declares_trait_default = if declares_item {
                false
            } else {
                impl_item
                    .trait_ty
                    .as_ref()
                    .and_then(|trait_ty| match &trait_ty.kind {
                        hir::TypeExprKind::Path(path) => match path.res.as_ref()? {
                            hir::Res::Def(def_id) => program.item(def_id.clone()),
                            _ => None,
                        },
                        _ => None,
                    })
                    .and_then(|item| match &item.kind {
                        hir::ItemKind::Trait(trait_def) => Some(trait_def),
                        _ => None,
                    })
                    .is_some_and(|trait_def| {
                        trait_def.items.iter().any(|item| {
                            item.name == *method
                                && matches!(
                                    &item.kind,
                                    hir::TraitItemKind::Method(function)
                                        if function.body.is_some()
                                )
                        })
                    })
            };
            if !declares_item && !declares_trait_default {
                continue;
            }
            let mut scope = self.with_generics(&impl_item.generics);
            let checked_self_ty = scope.checked_impl_self_ty(&impl_item.self_ty).await?;
            let self_ty = match &checked_self_ty.kind {
                TyKind::Ref(_, inner, _) => inner.as_ref(),
                _ => &checked_self_ty,
            };
            let matches_receiver = match (receiver_def.clone(), &receiver_ty.kind, &self_ty.kind) {
                (
                    Some(receiver_def),
                    TyKind::Adt(_, receiver_args),
                    TyKind::Adt(impl_receiver, impl_args),
                ) => {
                    impl_receiver.did == receiver_def
                        && scope.generic_args_compatible(impl_args, receiver_args)
                }
                (None, TyKind::Adt(_, _), _) => false,
                (None, _, _) => {
                    Self::ty_shapes_compatible(&self_ty.kind, &receiver_ty.kind)
                        && scope
                            .unify_call_types_probe(self_ty, receiver_ty, &mut HashMap::new())
                            .is_ok()
                }
                (Some(_), _, _) => false,
            };
            if !matches_receiver {
                continue;
            }
            let mut scope = scope.with_self_type(checked_self_ty.clone());
            let assoc_types = scope
                .impl_assoc_types(&impl_item.items, impl_item.self_ty.hir_id.clone())
                .await?;
            let mut scope = scope.with_assoc_types(assoc_types);
            for impl_item in &impl_item.items {
                match &impl_item.kind {
                    hir::ImplItemKind::Method(function) if impl_item.name == *method => {
                        let signature = Self::method_declared_signature_apply_receiver(
                            &mut scope,
                            receiver_ty,
                            function,
                        )
                        .await?;
                        if Self::signature_matches_expected_output(
                            &scope,
                            signature.as_ref(),
                            expected_output.as_ref(),
                        ) {
                            return Ok(signature);
                        }
                    }
                    // An associated const looked up through the same
                    // type-relative path shape (`u8::MAX`, `Layout::
                    // MIN_SIZE`) — not a callable method, but the same
                    // "name declared inside this receiver's own impl"
                    // lookup answers it: the const's own declared type.
                    hir::ImplItemKind::AssocConst(constant) if impl_item.name == *method => {
                        // `checked_self_ty` still contains the impl's own
                        // generic parameters.  A receiver such as
                        // `Vec<u8>` can therefore select `impl<T> ... for
                        // Vec<T>` while the const declaration's type still
                        // reads as `T`; carry the same unification used by
                        // candidate matching into the declaration type.
                        let mut substitutions = HashMap::new();
                        if scope
                            .unify_call_types_probe(self_ty, receiver_ty, &mut substitutions)
                            .is_err()
                        {
                            continue;
                        }
                        let mut scope = scope.with_self_type(checked_self_ty.clone());
                        let ty = scope.check_type_expr(&constant.ty).await?;
                        let ty = scope.substitute_param_map(&ty, &substitutions);
                        if expected_output.as_ref().is_none_or(|expected| {
                            scope
                                .unify_call_types_probe(&ty, expected, &mut HashMap::new())
                                .is_ok()
                        }) {
                            return Ok(Some(ty));
                        }
                    }
                    _ => {}
                }
            }
            // Not redeclared in this impl's own items — same
            // trait-default-method fallback as `method_output` (kept in
            // sync deliberately; see that function's matching block for
            // why). `Self::AssocType`-shaped types in a default method's
            // signature (e.g. `Iterator::map`'s `F: FnMut(Self::Item) ->
            // B`) only resolve if `scope.self_type`/`scope.assoc_types`
            // are populated first — this function doesn't need that for
            // the inherent-method case above (no associated types
            // involved there), but does here.
            if let Some(trait_ty) = &impl_item.trait_ty {
                if let Some(trait_def) = scope.resolve_trait_def(trait_ty) {
                    for trait_item in &trait_def.items {
                        let hir::TraitItemKind::Method(function) = &trait_item.kind else {
                            continue;
                        };
                        if trait_item.name == *method && function.body.is_some() {
                            let signature = Self::method_declared_signature_apply_receiver(
                                &mut scope,
                                receiver_ty,
                                function,
                            )
                            .await?;
                            if Self::signature_matches_expected_output(
                                &scope,
                                signature.as_ref(),
                                expected_output.as_ref(),
                            ) {
                                return Ok(signature);
                            }
                        }
                    }
                }
            }
        }
        Ok(None)
    }

    fn signature_matches_expected_output(
        scope: &HirTypeChecker,
        signature: Option<&Ty>,
        expected: Option<&Ty>,
    ) -> bool {
        let (
            Some(Ty {
                kind: TyKind::FnPtr(signature),
            }),
            Some(expected),
        ) = (signature, expected)
        else {
            return true;
        };
        scope
            .unify_call_types_probe(
                &signature.binder.value.output,
                expected,
                &mut HashMap::new(),
            )
            .is_ok()
    }
}
