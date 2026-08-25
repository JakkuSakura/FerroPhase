use super::*;

impl HirTypeChecker {
    pub(super) fn instantiate_call(
        &self,
        callable: &Ty,
        actuals: &[Ty],
    ) -> Result<Option<(HashMap<ty::ParamTy, Ty>, Ty)>> {
        let TyKind::FnPtr(signature) = &callable.kind else {
            return Ok(None);
        };
        if signature.binder.value.inputs.len() != actuals.len() {
            // `None` already means "not callable with these args" to every
            // caller (see the `TyKind::FnPtr` mismatch case just above) —
            // an arity mismatch is the same kind of "doesn't match", not a
            // hard error, so report it the same way instead of aborting.
            self.record_error("call argument count does not match function signature");
            return Ok(None);
        }
        let mut substitutions: HashMap<ty::ParamTy, Ty> = HashMap::new();
        for (expected, actual) in signature.binder.value.inputs.iter().zip(actuals) {
            self.unify_call_types(expected, actual, &mut substitutions)?;
        }
        let output = self.substitute_param_map(&signature.binder.value.output, &substitutions);
        Ok(Some((substitutions, output)))
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
                index: index as u32,
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
            (TyKind::Infer(_), _) => true,
            (TyKind::Ref(_, a, _), TyKind::Ref(_, c, _)) => Self::ty_matches_with_infer_holes(a, c),
            (TyKind::Tuple(a), TyKind::Tuple(c)) if a.len() == c.len() => a
                .iter()
                .zip(c)
                .all(|(a, c)| Self::ty_matches_with_infer_holes(a, c)),
            (TyKind::Array(a, a_len), TyKind::Array(c, c_len)) => {
                Self::ty_matches_with_infer_holes(a, c) && a_len == c_len
            }
            (TyKind::Slice(a), TyKind::Slice(c)) | (TyKind::Array(a, _), TyKind::Slice(c)) => {
                Self::ty_matches_with_infer_holes(a, c)
            }
            (TyKind::Adt(a_def, a_args), TyKind::Adt(c_def, c_args))
                if a_def.did == c_def.did && a_args.len() == c_args.len() =>
            {
                a_args.iter().zip(c_args).all(|(a, c)| match (a, c) {
                    (GenericArg::Type(a), GenericArg::Type(c)) => {
                        Self::ty_matches_with_infer_holes(a, c)
                    }
                    _ => a == c,
                })
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
            (TyKind::Slice(_) | TyKind::Array(_, _), TyKind::Slice(_) | TyKind::Array(_, _)) => {
                true
            }
            (TyKind::Tuple(a), TyKind::Tuple(b)) => a.len() == b.len(),
            (TyKind::RawPtr(_), TyKind::RawPtr(_)) => true,
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
            (TyKind::Infer(var), _) => { self.bind_infer(var.clone(), &actual); Ok(()) }
            (_, TyKind::Infer(var)) => { self.bind_infer(var.clone(), &expected); Ok(()) }
            (TyKind::Param(param), _) => {
                if let Some(previous) = substitutions.get(param) {
                    if record {
                        self.require_same(previous, &actual)?;
                    } else if *previous != actual {
                        return Err(Error::from("speculative type mismatch"));
                    }
                } else {
                    substitutions.insert(param.clone(), actual.clone());
                }
                Ok(())
            }
            (_, TyKind::Param(param)) => {
                if let Some(previous) = substitutions.get(param) {
                    if record {
                        self.require_same(previous, &expected)?;
                    } else if *previous != expected {
                        return Err(Error::from("speculative type mismatch"));
                    }
                } else {
                    substitutions.insert(param.clone(), expected.clone());
                }
                Ok(())
            }
            (TyKind::Ref(_, expected, _), TyKind::Ref(_, actual, _)) => {
                self.unify_call_types_impl(expected, &actual, substitutions, record)
            }
            (TyKind::Ref(_, expected, _), _) => {
                self.unify_call_types_impl(&expected, &actual, substitutions, record)
            }
            // Symmetric to the rule above: a bare-expected/`Ref`-actual pair
            // (e.g. a `str`-returning call's result reconciled against a
            // `&str` expected-type hint) derefs the actual side the same
            // way. Safe as a general rule — if the underlying shapes still
            // don't match after peeling, the recursive call's own catch-all
            // still reports a genuine mismatch.
            (_, TyKind::Ref(_, actual, _)) => {
                self.unify_call_types_impl(&expected, &actual, substitutions, record)
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
            (TyKind::Tuple(expected), TyKind::Tuple(actual)) if expected.len() == actual.len() => {
                expected
                    .iter()
                    .zip(actual)
                    .try_for_each(|(expected, actual)| {
                        self.unify_call_types_impl(expected, actual, substitutions, record)
                    })
            }
            (TyKind::Array(expected, _), TyKind::Array(actual, _))
            | (TyKind::Slice(expected), TyKind::Slice(actual))
            | (TyKind::Array(expected, _), TyKind::Slice(actual))
            | (TyKind::Slice(expected), TyKind::Array(actual, _)) => {
                self.unify_call_types_impl(expected, actual, substitutions, record)
            }
            (TyKind::Adt(expected, expected_args), TyKind::Adt(actual, actual_args))
                if expected.did == actual.did && expected_args.len() == actual_args.len() =>
            {
                for (expected, actual) in expected_args.iter().zip(actual_args) {
                    if let (GenericArg::Type(expected), GenericArg::Type(actual)) =
                        (expected, actual)
                    {
                        self.unify_call_types_impl(expected, actual, substitutions, record)?;
                    }
                }
                Ok(())
            }
            // C-string/FFI decay: a `&str`/string-literal argument may be
            // passed where a raw pointer (`*const char`/`*mut char`, an
            // `extern "C"` parameter) is expected — the same implicit
            // decay C itself performs for string literals and `&str`'s
            // byte-slice representation already matches a C string's byte
            // layout at the FFI boundary.
            (TyKind::RawPtr(_), TyKind::Slice(_)) => Ok(()),
            // `void*`/any-object-pointer decay, same as C: a raw pointer of
            // one pointee type may be passed where a raw pointer of another
            // is expected (e.g. `*mut u8` into `memcpy`'s `*mut void`
            // parameter) — this compiler has no real `void`/opaque-pointer
            // distinction, just an ordinary `RawPtr(())`.
            (TyKind::RawPtr(_), TyKind::RawPtr(_)) => Ok(()),
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
    /// A still-generic impl argument (containing an uninstantiated
    /// `Param`, e.g. `impl<T> Vec<T>`) always matches, since it has nothing
    /// concrete to conflict with; a concrete one must coerce the same way
    /// a call argument would (the same `Ref`-peeling rules as everywhere
    /// else), not require exact structural equality.
    pub(super) fn generic_args_compatible(
        &self,
        impl_args: &[GenericArg],
        receiver_args: &[GenericArg],
    ) -> bool {
        if impl_args.len() != receiver_args.len() {
            return false;
        }
        impl_args
            .iter()
            .zip(receiver_args)
            .all(|(impl_arg, receiver_arg)| match (impl_arg, receiver_arg) {
                (GenericArg::Type(impl_ty), GenericArg::Type(receiver_ty)) => {
                    if ty_contains_param(impl_ty) {
                        return true;
                    }
                    let mut substitutions = HashMap::new();
                    self.unify_call_types_probe(impl_ty, receiver_ty, &mut substitutions)
                        .is_ok()
                }
                _ => impl_arg == receiver_arg,
            })
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

    /// A substitution can itself resolve to another still-unsubstituted
    /// `Param` (e.g. one generic scope's `T` bound to an enclosing scope's
    /// own, not-yet-closed-over `U`) — a single `.get()` lookup doesn't
    /// walk that chain to its end. Depth-bounded the same way the
    /// autoderef chain in `method_output` is (a real chain this deep would
    /// be pathological): fails closed (returns the last-seen `Param`
    /// unresolved) rather than looping forever on a genuine cycle.
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

    pub(super) fn substitute_param_map(
        &self,
        ty: &Ty,
        substitutions: &HashMap<ty::ParamTy, Ty>,
    ) -> Ty {
        match &ty.kind {
            TyKind::Param(param) => match self.resolve_param_transitively(param, substitutions) {
                Some(resolved) => resolved.clone(),
                None => ty.clone(),
            },
            TyKind::Ref(region, inner, mutable) => Ty {
                kind: TyKind::Ref(
                    region.clone(),
                    Box::new(self.substitute_param_map(inner, substitutions)),
                    *mutable,
                ),
            },
            TyKind::RawPtr(value) => Ty {
                kind: TyKind::RawPtr(ty::TypeAndMut {
                    ty: Box::new(self.substitute_param_map(&value.ty, substitutions)),
                    mutbl: value.mutbl,
                }),
            },
            TyKind::Tuple(fields) => Ty {
                kind: TyKind::Tuple(
                    fields
                        .iter()
                        .map(|field| Box::new(self.substitute_param_map(field, substitutions)))
                        .collect(),
                ),
            },
            TyKind::Array(inner, length) => Ty {
                kind: TyKind::Array(
                    Box::new(self.substitute_param_map(inner, substitutions)),
                    length.clone(),
                ),
            },
            TyKind::Slice(inner) => Ty {
                kind: TyKind::Slice(Box::new(self.substitute_param_map(inner, substitutions))),
            },
            TyKind::Adt(def, args) => Ty {
                kind: TyKind::Adt(
                    def.clone(),
                    args.iter()
                        .map(|arg| match arg {
                            GenericArg::Type(ty) => {
                                GenericArg::Type(self.substitute_param_map(ty, substitutions))
                            }
                            other => other.clone(),
                        })
                        .collect(),
                ),
            },
            _ => ty.clone(),
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
        let has_self_param = matches!(
            function.sig.inputs.first().map(|param| &param.pat.kind),
            Some(hir::PatKind::Binding { name, .. }) if name.as_str() == "self"
        );
        if !has_self_param {
            // An associated function called via `Self::name(..)` (e.g.
            // `Layout::is_size_alignment_valid(size, alignment)`) takes no
            // receiver at all — the caller already supplies every argument
            // explicitly, so there's no `Self` position to unify here.
            // Return the declared signature verbatim; the caller's own
            // `instantiate_call` against the explicit call arguments does
            // the real unification.
            return Ok(Some(signature));
        }
        let self_input = &sig.binder.value.inputs[0];
        // `Self`'s position, substituted from the *actual*
        // receiver — everything else in the signature stays
        // in terms of the method's own generics for now.
        // Speculative: the caller (`method_output_at`/`method_declared_
        // signature_at`) tries every candidate impl in turn and silently
        // moves on when this returns `None` — a rejected candidate here is
        // never a real type error, so this must not permanently record one
        // (see `unify_call_types_probe`'s own doc comment).
        let mut substitutions = HashMap::new();
        if scope
            .unify_call_types_probe(self_input, receiver_ty, &mut substitutions)
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
            return self
                .generic_param_bound_method_signature(&param.name, method)
                .await;
        }
        let receiver_def = match &receiver_ty.kind {
            TyKind::Adt(receiver, _) => Some(receiver.did.clone()),
            _ => None,
        };
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
        let candidates: Box<dyn Iterator<Item = &hir::Item> + '_> = match &receiver_def {
            Some(def_id) => Box::new(program.impls_for_adt(def_id.clone())),
            None => Box::new(shape_and_blanket_candidates(&program, &receiver_ty.kind)),
        };
        for item in candidates {
            let hir::ItemKind::Impl(impl_item) = &item.kind else {
                continue;
            };
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
            for impl_item in &impl_item.items {
                match &impl_item.kind {
                    hir::ImplItemKind::Method(function) if impl_item.name == *method => {
                        return Self::method_declared_signature_apply_receiver(
                            &mut scope,
                            receiver_ty,
                            function,
                        )
                        .await;
                    }
                    // An associated const looked up through the same
                    // type-relative path shape (`u8::MAX`, `Layout::
                    // MIN_SIZE`) — not a callable method, but the same
                    // "name declared inside this receiver's own impl"
                    // lookup answers it: the const's own declared type.
                    hir::ImplItemKind::AssocConst(constant) if impl_item.name == *method => {
                        let mut scope = scope.with_self_type(checked_self_ty.clone());
                        return Ok(Some(scope.check_type_expr(&constant.ty).await?));
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
                            let mut scope = scope.with_self_type(checked_self_ty.clone());
                            let assoc_types = scope
                                .impl_assoc_types(
                                    &impl_item.items,
                                    impl_item.self_ty.hir_id.clone(),
                                )
                                .await?;
                            let mut scope = scope.with_assoc_types(assoc_types);
                            return Self::method_declared_signature_apply_receiver(
                                &mut scope,
                                receiver_ty,
                                function,
                            )
                            .await;
                        }
                    }
                }
            }
        }
        Ok(None)
    }
}
