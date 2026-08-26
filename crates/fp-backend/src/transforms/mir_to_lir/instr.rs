use crate::abi;
use fp_core::error::Result;
use fp_core::intrinsics::IntrinsicKind;
use fp_core::mir::ty::{FloatTy, IntTy, Ty, TyKind, TypeAndMut, UintTy};
use fp_core::{lir, mir};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet, VecDeque};
use std::rc::Rc;

// Internal submodules; items are used via inherent methods

/// Generator for transforming MIR to LIR (Low-level IR)
pub struct MirToLirLowerer {
    package_id: fp_core::ast::package::PackageId,
    pub(super) data_layout: lir::LirDataLayout,
    next_lir_id: lir::LirId,
    pub(super) next_label: u32,
    pub(super) register_map: HashMap<mir::LocalId, lir::LirValue>,
    pub(super) current_function: Option<lir::LirFunction>,
    pub(crate) const_values: HashMap<mir::LocalId, lir::LirConstant>,
    pub(super) extra_globals: Vec<lir::LirGlobal>,
    pub(super) const_global_counter: u64,
    pub(super) const_string_globals: HashMap<String, lir::Name>,
    pub(super) local_types: Vec<Ty>,
    pub(super) current_return_type: Option<lir::LirType>,
    pub(super) return_local: Option<mir::LocalId>,
    pub(super) mutable_locals: HashSet<mir::LocalId>,
    pub(super) local_storage: HashMap<mir::LocalId, LocalStorage>,
    pub(super) entry_allocas: Vec<lir::LirInstruction>,
    pub(super) queued_instructions: Vec<lir::LirInstruction>,
    name_counters: HashMap<String, usize>,
    pub(super) struct_layouts:
        RefCell<HashMap<(mir::DefId, Vec<mir::Ty>), Vec<Option<lir::LirType>>>>,
    function_symbol_map: HashMap<String, String>,
    function_def_map: HashMap<(mir::DefId, mir::ty::SubstsRef), String>,
    function_signatures: HashMap<String, lir::LirFunctionSignature>,
    function_call_conventions: HashMap<String, lir::CallingConvention>,
    function_declarations: HashMap<String, bool>,
    /// Package a predeclared function actually belongs to, for functions
    /// predeclared from a *dependency* package's MIR (see
    /// `predeclare_dependency_function_signatures`) — absent entries are
    /// assumed local (`self.package_id`), so this only needs entries for
    /// cross-package functions.
    function_package_ids: HashMap<String, fp_core::ast::package::PackageId>,
    runtime_symbol_map: fn(&str) -> Option<lir::RuntimeSymbol>,
    /// The whole session's MIR, owned directly (`new_with_programs`)
    /// instead of the driver cloning `dependency_packages`/`own_units` out
    /// of it on every call — `lookup_adt_def`, `transform_unit`, and the
    /// lazy cross-package callee-signature fallback (`transform_operand`'s
    /// `FnDef` arm) all read straight off this. Includes this generator's
    /// own package too (see `driver.rs`'s callers, which call
    /// `extend_mir_package` for this exact package's freshly-computed ADT
    /// defs/struct fields before handing this `Rc` over), so there's no
    /// separate local map to check first.
    mir_program: Rc<mir::MirProgram>,
    /// The whole session's already-lowered LIR, owned directly for the same
    /// reason as `mir_program` — supplied by the driver at construction
    /// time (`new_with_programs`) rather than read back out of
    /// `CompilerState` on demand.
    lir_program: Rc<lir::LirProgram>,
}

#[derive(Clone)]
pub(super) struct LocalStorage {
    pub(super) ptr_value: lir::LirValue,
    pub(super) element_type: lir::LirType,
    pub(super) alignment: u32,
}

#[derive(Clone)]
pub(super) struct PlaceAddress {
    pub(super) ptr: lir::LirValue,
    pub(super) ty: Ty,
    pub(super) lir_ty: lir::LirType,
    pub(super) alignment: u32,
}

#[derive(Clone)]
pub(super) enum PlaceAccess {
    Address(PlaceAddress),
    Value {
        value: lir::LirValue,
        ty: Ty,
        lir_ty: lir::LirType,
    },
}

impl MirToLirLowerer {
    /// Create a new LIR generator, owning `mir_program`/`lir_program`
    /// directly (see their own doc comments) — the caller (`driver.rs`'s
    /// `new_lir_generator`) supplies the whole session's current
    /// `Rc<mir::MirProgram>`/`Rc<lir::LirProgram>` handles up front instead
    /// of setting them post-construction; a caller with nothing to see yet
    /// (e.g. a test) just passes fresh, empty ones.
    pub fn new(
        data_layout: lir::LirDataLayout,
        mir_program: Rc<mir::MirProgram>,
        lir_program: Rc<lir::LirProgram>,
    ) -> Self {
        Self {
            package_id: fp_core::ast::package::PackageId::new(""),
            data_layout,
            next_lir_id: 0,
            next_label: 0,
            register_map: HashMap::new(),
            current_function: None,
            const_values: HashMap::new(),
            extra_globals: Vec::new(),
            const_global_counter: 0,
            const_string_globals: HashMap::new(),
            local_types: Vec::new(),
            current_return_type: None,
            return_local: None,
            mutable_locals: HashSet::new(),
            local_storage: HashMap::new(),
            entry_allocas: Vec::new(),
            queued_instructions: Vec::new(),
            name_counters: HashMap::new(),
            struct_layouts: RefCell::new(HashMap::new()),
            function_symbol_map: HashMap::new(),
            function_def_map: HashMap::new(),
            function_signatures: HashMap::new(),
            function_call_conventions: HashMap::new(),
            function_declarations: HashMap::new(),
            function_package_ids: HashMap::new(),
            runtime_symbol_map: |_| None,
            mir_program,
            lir_program,
        }
    }

    pub fn with_package_id(mut self, package_id: fp_core::ast::package::PackageId) -> Self {
        self.package_id = package_id;
        self
    }

    pub(super) fn lookup_adt_def(&self, def_id: &mir::DefId) -> Option<mir::ty::AdtDef> {
        self.mir_program
            .packages
            .values()
            .find_map(|package| package.borrow().adt_defs.get(def_id).cloned())
    }

    /// A concrete struct/enum instantiation's own field types, by
    /// `(DefId, generic args)` — read straight off `mir::MirPackage::
    /// full_layouts` (this package's own first, then every other loaded
    /// package's, same search order as `lookup_adt_def`), instead of a
    /// private copy of the same map handed to this generator up front.
    pub(super) fn lookup_full_layout(
        &self,
        key: &(mir::DefId, Vec<mir::Ty>),
    ) -> Option<Vec<mir::Ty>> {
        self.mir_program
            .packages
            .values()
            .find_map(|package| package.borrow().full_layouts.get(key).cloned())
    }

    /// Byte size for an opaque enum-payload-slot placeholder, by its own
    /// synthetic variant name — same idea as `lookup_full_layout`, off
    /// `mir::MirPackage::opaque_payload_sizes`.
    pub(super) fn lookup_opaque_payload_size(&self, name: &str) -> Option<u64> {
        self.mir_program
            .packages
            .values()
            .find_map(|package| package.borrow().opaque_payload_sizes.get(name).copied())
    }

    pub(super) fn resolve_global_symbol(&self, path: &mir::Path) -> lir::Name {
        lir::Name::new(path.to_string())
    }

    pub(super) fn function_value(&self, name: String) -> Result<lir::LirValue> {
        let symbol_name = self
            .function_symbol_map
            .get(&name)
            .cloned()
            .unwrap_or_else(|| name.clone());
        let signature = self.function_signatures.get(&name).or_else(|| {
            self.function_signatures.get(&symbol_name)
        }).ok_or_else(|| {
            fp_core::error::Error::from(format!("missing LIR signature for function `{name}`"))
        })?;
        let ty = lir::LirType::Ptr(Box::new(lir::LirType::Function {
            return_type: Box::new(signature.return_type.clone()),
            param_types: signature.params.clone(),
            is_variadic: signature.is_variadic,
        }));
        let package_id = self
            .function_package_ids
            .get(&symbol_name)
            .cloned()
            .unwrap_or_else(|| self.package_id.clone());
        Ok(lir::LirValue::function(
            lir::LirFunctionRef::Package {
                package_id,
                name: lir::Name::new(symbol_name),
            },
            ty,
        ))
    }

    /// Emit a call to a C-ABI extern function by name (e.g. `malloc`,
    /// `memcpy`), registering a declaration-only signature for it on first
    /// use if one isn't already present — mirroring the generic extern-item
    /// handling `predeclare_function_signatures_impl` already does for real
    /// MIR items, just invoked directly from Rust-side lowering code rather
    /// than from an actual `mir::ItemKind::Function{is_extern: true, ..}`.
    /// Returns the id of the pushed `Call` instruction's result register,
    /// typed as `return_type` (the exact param/return types used to
    /// register the signature only matter for `function_declarations`
    /// bookkeeping — the `Call` instruction built here always uses the
    /// types the caller actually needs).
    pub(super) fn call_extern_c_function(
        &mut self,
        name: &str,
        args: Vec<(lir::LirValue, lir::LirType)>,
        return_type: lir::LirType,
        instructions: &mut Vec<lir::LirInstruction>,
    ) -> Result<u32> {
        let arg_types: Vec<lir::LirType> = args.iter().map(|(_, ty)| ty.clone()).collect();
        self.function_signatures
            .entry(name.to_string())
            .or_insert_with(|| lir::LirFunctionSignature {
                params: arg_types,
                return_type: return_type.clone(),
                is_variadic: false,
            });
        self.function_call_conventions
            .entry(name.to_string())
            .or_insert(lir::CallingConvention::C);
        self.function_declarations
            .entry(name.to_string())
            .or_insert(true);
        let function = self.function_value(name.to_string())?;
        let call_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: call_id,
            kind: lir::LirInstructionKind::Call {
                function,
                args: args.into_iter().map(|(value, _)| value).collect(),
                calling_convention: lir::CallingConvention::C,
                tail_call: false,
            },
            result: Some(lir::LirRegister {
                id: call_id,
                ty: return_type,
            }),
            debug_info: None,
        });
        Ok(call_id)
    }

    pub fn prepare_program(&mut self, mir_program: &mir::MirCodeUnit) {
        self.predeclare_function_signatures(mir_program);
    }

    pub fn prepare_package(&mut self, package_id: &fp_core::ast::package::PackageId) {
        let units = self
            .mir_program
            .package(package_id)
            .map(|package| package.borrow().units.values().cloned().collect::<Vec<_>>())
            .unwrap_or_default();
        for unit in &units {
            self.predeclare_function_signatures_impl(unit, Some(package_id.clone()));
        }
    }

    /// Lower one MIR declaration into independently publishable LIR.
    pub fn transform_item(
        &mut self,
        mir_item: mir::Item,
        bodies: &std::collections::HashMap<mir::BodyId, mir::Body>,
    ) -> Result<lir::LirBlob> {
        let mut lir_program = lir::LirBlob::new(self.data_layout.clone());
        match mir_item.kind {
            mir::ItemKind::Function(mir_func) => {
                lir_program
                    .functions
                    .push(self.transform_function_with_bodies(mir_func, bodies)?);
            }
            mir::ItemKind::Static(mir_static) => {
                lir_program.globals.push(self.transform_static(mir_static)?);
            }
            mir::ItemKind::ExecutableConst(konst) => {
                let mir_func = mir::Function {
                    name: konst.function_name,
                    def_id: Some(konst.def_id),
                    substs: Vec::new(),
                    sig: mir::FunctionSig {
                        inputs: Vec::new(),
                        output: konst.ty,
                    },
                    body_id: konst.body_id,
                    abi: mir::ty::Abi::Rust,
                    is_extern: false,
                    attrs: Vec::new(),
                };
                let lir_func = self.transform_function_with_bodies(mir_func, bodies)?;
                lir_program.functions.push(lir_func);
            }
            mir::ItemKind::Query(query) => {
                lir_program.queries.push(lir::LirQuery {
                    query_id: mir_item.mir_id,
                    origin: query.origin,
                    ir: query.ir,
                    span: query.span,
                });
            }
        }
        if !self.extra_globals.is_empty() {
            let mut extras: Vec<_> = self.extra_globals.drain(..).collect();
            extras.append(&mut lir_program.globals);
            lir_program.globals = extras;
        }
        Ok(lir_program)
    }

    /// Lowers `def_id`'s own unit (looked up in this generator's own
    /// `package_id`'s entry of `mir_program`) straight to its independently
    /// publishable LIR blobs — the def-id-only counterpart to
    /// `transform_items` a driver loop can call with just the `DefId` it
    /// wants lowered, no separate unit/bodies fetch of its own. A `DefId`
    /// with no unit (nothing lowered for it, or already consumed) lowers to
    /// no blobs, not an error. Deliberately doesn't go through
    /// `transform_items`: that predeclares every function in `unit` up
    /// front (`prepare_program`), which for the real, per-`DefId` driver
    /// path this serves is *already* redundant — `mir::MirPackage::
    /// insert_unit` records this exact unit's own function signature into
    /// `sigs` the moment it's stored, before this ever runs, and the lazy
    /// callee-signature fallback (`transform_operand`'s `FnDef` arm)
    /// already reads straight off `sigs` on first reference. Only the
    /// legacy whole-module `transform`/`transform_items` path (test-only,
    /// no shared `mir_program` to fall back to at all) still needs the
    /// eager sweep.
    pub fn transform_unit(&mut self, def_id: mir::DefId) -> Result<Vec<lir::LirBlob>> {
        let Some(unit) = self
            .mir_program
            .package(&self.package_id)
            .and_then(|package| package.borrow().unit(def_id).cloned())
        else {
            return Ok(Vec::new());
        };
        unit.items
            .into_iter()
            .map(|item| self.transform_item(item, &unit.bodies))
            .collect()
    }

    pub fn transform_items(&mut self, mir_program: mir::MirCodeUnit) -> Result<Vec<lir::LirBlob>> {
        self.prepare_program(&mir_program);
        mir_program
            .items
            .into_iter()
            .map(|item| self.transform_item(item, &mir_program.bodies))
            .collect()
    }

    /// Transform MIR to a flat LIR program for legacy backend callers.
    pub fn transform(&mut self, mir_program: mir::MirCodeUnit) -> Result<lir::LirBlob> {
        let mut lir_program = lir::LirBlob::new(self.data_layout.clone());
        self.prepare_program(&mir_program);
        for item in mir_program.items {
            let mut item_blob = self.transform_item(item, &mir_program.bodies)?;
            lir_program.functions.append(&mut item_blob.functions);
            lir_program.globals.append(&mut item_blob.globals);
            lir_program
                .type_definitions
                .append(&mut item_blob.type_definitions);
            lir_program.queries.append(&mut item_blob.queries);
        }
        Ok(lir_program)
    }

    pub(super) fn predeclare_function_signatures(&mut self, program: &mir::MirCodeUnit) {
        self.predeclare_function_signatures_impl(program, None);
    }

    /// Predeclares functions from a *dependency* package's MIR so a
    /// cross-package call (e.g. `json::parse`) resolves during this
    /// package's own MIR-to-LIR lowering instead of failing with "missing
    /// MIR function definition" — `function_def_map`/`function_signatures`/
    /// etc. were previously only ever populated from this package's own
    /// MIR (see `transform`/`prepare_program`). Also records `package_id`
    /// in `function_package_ids` so `function_value` tags the resulting
    /// `LirFunctionRef::Package` with the *callee's* package, not this
    /// (caller's) one.
    pub fn predeclare_dependency_function_signatures(
        &mut self,
        program: &mir::MirCodeUnit,
        package_id: fp_core::ast::package::PackageId,
    ) {
        self.predeclare_function_signatures_impl(program, Some(package_id));
    }

    /// Whether `ty` still contains an unresolved generic type parameter
    /// (`TyKind::Param`) — true for a generic function's own template
    /// signature (e.g. `impl<T> Vec<T> { fn push(&mut self, value: T) }`'s
    /// literal `sig.inputs`/`sig.output`, still `Param("T")` since no
    /// concrete `T` applies to the un-specialized item itself). Such a
    /// signature can never be given a concrete LIR type — only its
    /// monomorphized specializations (separate MIR items with `T`
    /// substituted throughout) can be.
    ///
    /// For an ADT reference whose own generic args are already fully
    /// resolved (e.g. `Vec<BenchCase>`), this isn't enough on its own:
    /// `lir_type_from_ty` still needs to resolve *that ADT's own field
    /// list* — via `full_layouts`/`struct_layouts` for this exact
    /// `(def_id, args)` if already cached, or else by instantiating the
    /// registered declaration's generic fields with these `substs` (see
    /// `instantiate_ty`). That instantiation only fails when
    /// `lookup_adt_def` has never heard of this `DefId` at all — check
    /// that condition here too, recursively, so predeclaring a function
    /// that merely *references* an unregistered ADT is caught here rather
    /// than crashing deep inside `lir_type_from_ty`'s field expansion.
    pub(super) fn contains_unresolved_param(&self, ty: &Ty) -> bool {
        match &ty.kind {
            // An unconstrained inference variable (e.g. `build_type`'s own
            // `-> type<_>` — a stub declaration whose body is dropped and
            // whose real calls are always intercepted as the `BuildType`
            // intrinsic before ever reaching this signature, so nothing
            // ever unifies `_` with a concrete type) is exactly as
            // "not concrete yet" as an unresolved generic parameter — skip
            // predeclaring its LIR signature the same way, rather than
            // reaching MIR-to-LIR with a raw `Infer` and panicking.
            TyKind::Param(_) | TyKind::Infer(_) => true,
            TyKind::Ref(_, inner, _) | TyKind::RawPtr(TypeAndMut { ty: inner, .. }) => {
                self.contains_unresolved_param(inner)
            }
            TyKind::Slice(inner) | TyKind::Array(inner, _) => self.contains_unresolved_param(inner),
            TyKind::Tuple(elements) => elements.iter().any(|e| self.contains_unresolved_param(e)),
            TyKind::Adt(adt, substs) => {
                let substs_types: Vec<mir::Ty> = substs
                    .iter()
                    .filter_map(|arg| match arg {
                        mir::ty::GenericArg::Type(inner) => Some(inner.clone()),
                        _ => None,
                    })
                    .collect();
                if substs_types
                    .iter()
                    .any(|inner| self.contains_unresolved_param(inner))
                {
                    return true;
                }
                if self
                    .struct_layouts
                    .borrow()
                    .contains_key(&(adt.did.clone(), substs_types.clone()))
                    || self
                        .lookup_full_layout(&(adt.did.clone(), substs_types))
                        .is_some()
                {
                    return false;
                }
                // Not yet cached for this exact instantiation, but
                // `lir_type_from_ty` can still compute it on demand (via
                // `lookup_adt_def` + `instantiate_ty`) as long as the
                // declaration is registered at all — the template's own
                // fields always mention a bare `Param` (that's what makes
                // it generic) and substituting it is exactly the point,
                // so that alone is never a reason to call this unresolved.
                self.lookup_adt_def(&adt.did).is_none()
            }
            _ => false,
        }
    }

    pub(super) fn predeclare_function_signatures_impl(
        &mut self,
        program: &mir::MirCodeUnit,
        package_id: Option<fp_core::ast::package::PackageId>,
    ) {
        for item in &program.items {
            if let mir::ItemKind::Function(func) = &item.kind {
                self.register_function_signature(func, package_id.clone());
            }
        }
    }

    /// Registers one function's mangled name/LIR signature/calling
    /// convention/package ownership into the generator's lookup maps —
    /// shared body of `predeclare_function_signatures_impl`'s whole-module
    /// sweep and `resolve_signature`'s lazy, per-`DefId` fallback (see
    /// `transform_operand`'s `FnDef` arm). Returns `None` (registering
    /// nothing) for a still-generic template signature — only a concrete,
    /// fully-substituted `mir::Function` can be given a real LIR type.
    pub(super) fn register_function_signature(
        &mut self,
        func: &mir::Function,
        package_id: Option<fp_core::ast::package::PackageId>,
    ) -> Option<String> {
        if func
            .sig
            .inputs
            .iter()
            .chain(std::iter::once(&func.sig.output))
            .any(|ty| self.contains_unresolved_param(ty))
        {
            return None;
        }
        let name = self.mangle_function_name(func);
        if let Some(ref def_id) = func.def_id {
            self.function_def_map
                .entry((def_id.clone(), func.substs.clone()))
                .or_insert_with(|| name.clone());
        }
        let signature = lir::LirFunctionSignature {
            params: func
                .sig
                .inputs
                .iter()
                .map(|ty| self.lir_type_from_ty(ty))
                .collect(),
            return_type: self.lir_type_from_ty(&func.sig.output),
            is_variadic: false,
        };
        self.function_signatures
            .entry(name.clone())
            .or_insert(signature.clone());
        self.function_signatures
            .entry(func.name.as_str().to_string())
            .or_insert(signature);
        self.function_symbol_map
            .entry(func.name.as_str().to_string())
            .or_insert_with(|| name.clone());
        if let Some(short_name) = func.name.as_str().rsplit("::").next() {
            let short_signature = self
                .function_signatures
                .get(&name)
                .cloned()
                .expect("function signature was just registered");
            self.function_signatures
                .entry(short_name.to_string())
                .or_insert(short_signature);
            self.function_symbol_map
                .insert(short_name.to_string(), name.clone());
        }
        let cc = self.calling_convention_for_abi(&func.abi);
        self.function_call_conventions
            .entry(func.name.as_str().to_string())
            .or_insert(cc.clone());
        self.function_call_conventions
            .entry(name.clone())
            .or_insert(cc);
        self.function_declarations
            .entry(name.clone())
            .or_insert(func.is_extern);
        if let Some(package_id) = &package_id {
            self.function_package_ids
                .entry(name.clone())
                .or_insert_with(|| package_id.clone());
        }
        Some(name)
    }

    /// Transform a MIR function to LIR
    pub(super) fn transform_function_with_bodies(
        &mut self,
        mir_func: mir::Function,
        bodies: &std::collections::HashMap<mir::BodyId, mir::Body>,
    ) -> Result<lir::LirFunction> {
        // Reset generator state for new function
        self.reset_for_new_function();

        if let Some(mir_body) = bodies.get(&mir_func.body_id) {
            self.collect_struct_layouts(mir_body);
        }

        let function_name = self.mangle_function_name(&mir_func);
        let param_types: Vec<lir::LirType> = mir_func
            .sig
            .inputs
            .iter()
            .map(|ty| self.lir_type_from_ty(ty))
            .collect();
        let return_type = self.lir_type_from_ty(&mir_func.sig.output);
        self.current_return_type = Some(return_type.clone());

        let signature = lir::LirFunctionSignature {
            params: param_types.clone(),
            return_type: return_type.clone(),
            is_variadic: false,
        };
        self.function_signatures
            .insert(function_name.clone(), signature.clone());

        let calling_convention = self.calling_convention_for_abi(&mir_func.abi);
        let linkage = if mir_func.is_extern {
            lir::Linkage::External
        } else if matches!(
            mir_func.abi,
            mir::ty::Abi::C { .. } | mir::ty::Abi::System { .. }
        ) {
            lir::Linkage::External
        } else {
            lir::Linkage::Internal
        };
        let is_declaration = mir_func.is_extern;

        let mut lir_func = lir::LirFunction {
            def_id: mir_func.def_id,
            name: lir::Name::new(function_name),
            signature,
            basic_blocks: Vec::new(),
            locals: Vec::new(),
            stack_slots: Vec::new(),
            calling_convention,
            linkage,
            is_declaration,
        };

        if lir_func.is_declaration {
            return Ok(lir_func);
        }

        // Transform MIR body if present
        if let Some(mir_body) = bodies.get(&mir_func.body_id) {
            // First pass: analyze const values
            self.analyze_const_values(mir_body)?;
            self.local_types = mir_body.locals.iter().map(|decl| decl.ty.clone()).collect();
            self.return_local = Some(mir_body.return_local);
            self.mutable_locals = self.compute_mutable_locals(mir_body);
            if let Some(ret_local) = self.return_local {
                self.mutable_locals.insert(ret_local);
            }
            self.initialize_local_storage(mir_body);
            lir_func.locals = self.build_lir_locals(mir_body);
            self.seed_argument_registers(mir_body);

            let block_order = self.compute_block_order(mir_body);
            for &bb_idx in &block_order {
                let bb = &mir_body.basic_blocks[bb_idx];
                let lir_block = self.transform_basic_block(bb_idx as u32, bb)?;
                lir_func.basic_blocks.push(lir_block);
            }
            // Ensure at least one block exists
            if lir_func.basic_blocks.is_empty() {
                lir_func.basic_blocks.push(lir::LirBasicBlock {
                    id: 0,
                    label: Some(lir::Name::new("entry")),
                    instructions: Vec::new(),
                    terminator: lir::LirTerminator::Return(None),
                    predecessors: Vec::new(),
                    successors: Vec::new(),
                });
            }
        } else {
            // Fallback: create a minimal function with a return
            lir_func.basic_blocks.push(lir::LirBasicBlock {
                id: 0,
                label: Some(lir::Name::new("entry")),
                instructions: Vec::new(),
                terminator: lir::LirTerminator::Return(None),
                predecessors: Vec::new(),
                successors: Vec::new(),
            });
        }

        self.populate_block_edges(&mut lir_func.basic_blocks);
        self.function_signatures.insert(
            String::from(lir_func.name.clone()),
            lir_func.signature.clone(),
        );

        self.current_function = Some(lir_func.clone());
        Ok(lir_func)
    }

    pub(super) fn mangle_function_name(&mut self, mir_func: &mir::Function) -> String {
        let base = if !mir_func.name.as_str().is_empty() {
            String::from(mir_func.name.clone())
        } else {
            "anonymous_fn".to_string()
        };

        if let Some(existing) = self.function_symbol_map.get(&base) {
            return existing.clone();
        }

        if mir_func.is_extern || abi::is_c_abi_mir(&mir_func.abi) {
            let extern_name = abi::extern_symbol_name_with_attrs(&base, &mir_func.attrs);
            self.function_symbol_map
                .insert(base.clone(), extern_name.clone());
            if !mir_func.name.as_str().is_empty() {
                self.function_symbol_map
                    .entry(String::from(mir_func.name.clone()))
                    .or_insert(extern_name.clone());
                let short_name =
                    abi::extern_symbol_name_with_attrs(mir_func.name.as_str(), &mir_func.attrs);
                self.function_symbol_map
                    .entry(short_name)
                    .or_insert(extern_name.clone());
            }
            return extern_name;
        }

        let sanitized = Self::sanitize_symbol(&base);
        let entry = self
            .name_counters
            .entry(sanitized.clone())
            .or_insert(0_usize);
        let suffix = *entry;
        *entry += 1;

        let final_name = if suffix == 0 {
            sanitized
        } else {
            format!("{sanitized}__{suffix}")
        };

        self.function_symbol_map
            .insert(base.clone(), final_name.clone());
        if !mir_func.name.as_str().is_empty() {
            self.function_symbol_map
                .entry(String::from(mir_func.name.clone()))
                .or_insert(final_name.clone());
        }

        final_name
    }

    pub(super) fn sanitize_symbol(name: &str) -> String {
        let mut result = String::with_capacity(name.len());
        for ch in name.chars() {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                result.push(ch);
            } else {
                result.push('_');
            }
        }

        if result.is_empty() {
            return "anonymous_fn".to_string();
        }

        if matches!(result.chars().next(), Some(c) if c.is_ascii_digit()) {
            let mut prefixed = String::with_capacity(result.len() + 1);
            prefixed.push('_');
            prefixed.push_str(&result);
            prefixed
        } else {
            result
        }
    }

    pub(super) fn calling_convention_for_abi(&self, abi: &mir::ty::Abi) -> lir::CallingConvention {
        match abi {
            mir::ty::Abi::Rust => lir::CallingConvention::C,
            mir::ty::Abi::C { .. } => lir::CallingConvention::C,
            mir::ty::Abi::System { .. } => lir::CallingConvention::C,
            _ => lir::CallingConvention::C,
        }
    }

    /// Transform a basic block
    pub(super) fn transform_basic_block(
        &mut self,
        bb_id: u32,
        bb_data: &mir::BasicBlockData,
    ) -> Result<lir::LirBasicBlock> {
        let mut lir_block = lir::LirBasicBlock {
            id: bb_id,
            label: Some(lir::Name::new(format!("bb{}", bb_id))),
            instructions: Vec::new(),
            terminator: lir::LirTerminator::Return(None),
            predecessors: Vec::new(),
            successors: Vec::new(),
        };

        if bb_data.is_cleanup {
            let landingpad_ty = lir::LirType::Struct {
                fields: vec![
                    lir::LirType::Ptr(Box::new(lir::LirType::I8)),
                    lir::LirType::I32,
                ],
                packed: false,
                name: None,
            };
            let landingpad_id = self.next_id();
            lir_block.instructions.push(lir::LirInstruction {
                id: landingpad_id,
                kind: lir::LirInstructionKind::LandingPad {
                    result_type: landingpad_ty.clone(),
                    personality: None,
                    cleanup: true,
                    clauses: vec![lir::LandingPadClause::Catch(lir::LirValue::constant(
                        lir::LirConstant::null(lir::LirType::Ptr(Box::new(lir::LirType::I8))),
                    ))],
                },
                result: Some(lir::LirRegister {
                    id: landingpad_id,
                    ty: landingpad_ty,
                }),
                debug_info: None,
            });
        }

        // Transform all MIR statements into LIR instructions
        for stmt in &bb_data.statements {
            if bb_id == 0 && !self.entry_allocas.is_empty() {
                lir_block.instructions.extend(self.entry_allocas.clone());
                self.entry_allocas.clear();
            }
            let lir_insts = self.transform_statement(stmt)?;
            for inst in lir_insts {
                lir_block.instructions.push(inst);
            }
        }

        if bb_id == 0 && !self.entry_allocas.is_empty() {
            lir_block.instructions.extend(self.entry_allocas.clone());
            self.entry_allocas.clear();
        }

        // Transform the terminator
        let terminator = if let Some(terminator) = &bb_data.terminator {
            self.transform_terminator(terminator, &mut lir_block)?
        } else {
            // Some MIR producers omit an explicit return on the final block when the
            // value has already been written to the designated return local. In
            // that case, synthesize a return terminator and let `prepare_return_value`
            // materialize the value (loading from the return slot if needed).
            lir::LirTerminator::Return(self.prepare_return_value(&mut lir_block)?)
        };

        lir_block.terminator = terminator;
        Ok(lir_block)
    }

    /// Transform a MIR statement to LIR instructions
    pub(super) fn transform_statement(
        &mut self,
        stmt: &mir::Statement,
    ) -> Result<Vec<lir::LirInstruction>> {
        match &stmt.kind {
            mir::StatementKind::Assign(place, rvalue) => self.transform_assign(place, rvalue),
            mir::StatementKind::IntrinsicCall { kind, format, args } => {
                let lir_kind = match kind {
                    IntrinsicKind::Print => lir::LirIntrinsicKind::Print,
                    IntrinsicKind::Println => lir::LirIntrinsicKind::Println,
                    IntrinsicKind::Format => {
                        return Err(fp_core::error::Error::from(
                            "format intrinsic must be assigned to a place".to_string(),
                        ));
                    }
                    _ => {
                        return Err(fp_core::error::Error::from(format!(
                            "unsupported MIR intrinsic in statement lowering: {:?}",
                            kind
                        )));
                    }
                };
                let mut instructions = Vec::new();
                let mut lir_args = Vec::with_capacity(args.len());
                for arg in args {
                    let value = self.transform_operand(arg)?;
                    instructions.extend(self.take_queued_instructions());
                    let adjusted = match arg {
                        mir::Operand::Move(place) | mir::Operand::Copy(place) => {
                            let slice_elem =
                                self.lookup_place_type(place).and_then(|ty| match ty.kind {
                                    TyKind::Slice(elem) => Some(*elem),
                                    _ => None,
                                });
                            if let Some(elem_ty) = slice_elem {
                                let elem_lir = self.lir_type_from_ty(&elem_ty);
                                let ptr_ty = lir::LirType::Ptr(Box::new(elem_lir));
                                self.extract_slice_field(value, 0, ptr_ty, &mut instructions)
                            } else {
                                value
                            }
                        }
                        _ => value,
                    };
                    lir_args.push(adjusted);
                }
                instructions.push(lir::LirInstruction {
                    id: self.next_id(),
                    kind: lir::LirInstructionKind::IntrinsicCall {
                        kind: lir_kind,
                        format: format.clone(),
                        args: lir_args,
                    },
                    result: None,
                    debug_info: None,
                });
                Ok(instructions)
            }
            mir::StatementKind::StorageLive(_) => Ok(Vec::new()),
            mir::StatementKind::StorageDead(_) => Ok(Vec::new()),
            _ => Ok(vec![lir::LirInstruction {
                id: self.next_id(),
                kind: lir::LirInstructionKind::Unreachable,
                result: None,
                debug_info: None,
            }]),
        }
    }

    /// Transform an assignment
    #[allow(unused_assignments)]
    pub(super) fn transform_assign(
        &mut self,
        place: &mir::Place,
        rvalue: &mir::Rvalue,
    ) -> Result<Vec<lir::LirInstruction>> {
        let mut instructions = Vec::new();
        let target_access = self.resolve_place(place)?;
        instructions.extend(self.take_queued_instructions());
        let assign_whole_place = place.projection.is_empty();
        let place_ty = self.lookup_place_type(place);
        let destination_lir_ty = place_ty.as_ref().map(|ty| self.lir_type_from_ty(ty));
        let mut result_value: Option<lir::LirValue> = None;

        match rvalue {
            mir::Rvalue::Use(operand) => match operand {
                mir::Operand::Move(op_place) | mir::Operand::Copy(op_place) => {
                    let operand_access = self.resolve_place(op_place)?;
                    instructions.extend(self.take_queued_instructions());
                    let value = match operand_access {
                        PlaceAccess::Address(addr) => {
                            let load_id = self.next_id();
                            instructions.push(lir::LirInstruction {
                                id: load_id,
                                kind: lir::LirInstructionKind::Load {
                                    address: addr.ptr,
                                    alignment: Some(addr.alignment),
                                    volatile: false,
                                },
                                result: Some(lir::LirRegister {
                                    id: load_id,
                                    ty: addr.lir_ty.clone(),
                                }),
                                debug_info: None,
                            });
                            lir::LirValue::register(load_id, addr.lir_ty)
                        }
                        PlaceAccess::Value { value, lir_ty, .. } => {
                            let expects_pointer =
                                matches!(destination_lir_ty, Some(lir::LirType::Ptr(_)));
                            if !expects_pointer && matches!(lir_ty, lir::LirType::Ptr(_)) {
                                let load_ty = destination_lir_ty.clone().expect(
                                    "destination LIR type must be known for load operation",
                                );
                                let load_id = self.next_id();
                                instructions.push(lir::LirInstruction {
                                    id: load_id,
                                    kind: lir::LirInstructionKind::Load {
                                        address: value.clone(),
                                        alignment: Some(self.alignment_for_lir_type(&load_ty)),
                                        volatile: false,
                                    },
                                    result: Some(lir::LirRegister {
                                        id: load_id,
                                        ty: load_ty.clone(),
                                    }),
                                    debug_info: None,
                                });
                                lir::LirValue::register(load_id, load_ty)
                            } else {
                                value
                            }
                        }
                    };
                    result_value = Some(value);
                }
                mir::Operand::Constant(constant) => {
                    if let Some(place_ty) = place_ty.as_ref() {
                        let constant_value = self.constant_to_lir_constant(constant, place_ty)?;
                        result_value = Some(lir::LirValue::constant(constant_value));
                    } else {
                        let value = self.transform_operand(operand)?;
                        instructions.extend(self.take_queued_instructions());
                        result_value = Some(value);
                    }
                }
            },
            mir::Rvalue::Query(query) => {
                let query_id = self.next_id();
                let query_ty = destination_lir_ty.clone().ok_or_else(|| {
                    fp_core::error::Error::from("query assignment has no destination type")
                })?;
                instructions.push(lir::LirInstruction {
                    id: query_id,
                    kind: lir::LirInstructionKind::ExecQuery(lir::LirQuery {
                        query_id,
                        origin: query.origin.clone(),
                        ir: query.ir.clone(),
                        span: query.span,
                    }),
                    result: Some(lir::LirRegister {
                        id: query_id,
                        ty: query_ty.clone(),
                    }),
                    debug_info: None,
                });
                result_value = Some(lir::LirValue::register(query_id, query_ty));
            }
            mir::Rvalue::IntrinsicCall { kind, format, args } => {
                // Deliberately a *separate* vector from the outer
                // `instructions` (not just a differently-scoped shadow of
                // it) — the `CreateStruct`/`AddField`/`BuildType`/`Slice`
                // sub-cases below return it directly via `return
                // Ok(intrinsic_instructions)`, bypassing the outer
                // `instructions` entirely (intentional, pre-existing
                // behavior). `Format`/`TimeNow` are the only sub-cases that
                // *fall through* instead of returning early, so they need
                // their accumulated instructions merged into the outer
                // vector below — this used to be silently lost when this
                // vector shadowed the outer one under the same name.
                let mut intrinsic_instructions = Vec::new();

                if matches!(
                    kind,
                    IntrinsicKind::CreateStruct
                        | IntrinsicKind::AddField
                        | IntrinsicKind::BuildType
                        | IntrinsicKind::PrimitiveType
                        | IntrinsicKind::CompileWarning
                        | IntrinsicKind::CompileError
                        | IntrinsicKind::Unionify
                ) {
                    let mut lir_args = Vec::with_capacity(args.len());
                    for arg in args {
                        let value = self.transform_operand(arg)?;
                        intrinsic_instructions.extend(self.take_queued_instructions());
                        lir_args.push(value);
                    }
                    let comptime_op = match kind {
                        IntrinsicKind::CreateStruct => lir::ComptimeOp::CreateStruct {
                            name: lir_args.into_iter().next().ok_or_else(|| {
                                fp_core::error::Error::from("CreateStruct requires one argument")
                            })?,
                        },
                        IntrinsicKind::AddField => {
                            let mut iter = lir_args.into_iter();
                            lir::ComptimeOp::AddField {
                                struct_handle: iter.next().ok_or_else(|| {
                                    fp_core::error::Error::from("AddField requires three arguments")
                                })?,
                                field_name: iter.next().ok_or_else(|| {
                                    fp_core::error::Error::from("AddField requires three arguments")
                                })?,
                                field_type: iter.next().ok_or_else(|| {
                                    fp_core::error::Error::from("AddField requires three arguments")
                                })?,
                            }
                        }
                        IntrinsicKind::BuildType => lir::ComptimeOp::IntoType {
                            value: lir_args.into_iter().next().ok_or_else(|| {
                                fp_core::error::Error::from("BuildType requires one argument")
                            })?,
                        },
                        IntrinsicKind::PrimitiveType => lir::ComptimeOp::PrimitiveType {
                            name: lir_args.into_iter().next().ok_or_else(|| {
                                fp_core::error::Error::from("PrimitiveType requires one argument")
                            })?,
                        },
                        IntrinsicKind::CompileWarning => lir::ComptimeOp::CompileWarning {
                            message: lir_args.into_iter().next().ok_or_else(|| {
                                fp_core::error::Error::from(
                                    "compile_warning! requires one argument",
                                )
                            })?,
                        },
                        IntrinsicKind::CompileError => lir::ComptimeOp::CompileError {
                            message: lir_args.into_iter().next().ok_or_else(|| {
                                fp_core::error::Error::from("compile_error! requires one argument")
                            })?,
                        },
                        IntrinsicKind::Unionify => lir::ComptimeOp::Unionify {
                            function: lir_args.into_iter().next().ok_or_else(|| {
                                fp_core::error::Error::from("unionify requires one argument")
                            })?,
                        },
                        _ => unreachable!(),
                    };
                    let instr_id = self.next_id();
                    intrinsic_instructions.push(lir::LirInstruction {
                        id: instr_id,
                        kind: lir::LirInstructionKind::ComptimeOp(comptime_op),
                        result: destination_lir_ty
                            .clone()
                            .map(|ty| lir::LirRegister { id: instr_id, ty }),
                        debug_info: None,
                    });
                    result_value = Some(lir::LirValue::register(
                        instr_id,
                        destination_lir_ty.clone().ok_or_else(|| {
                            fp_core::error::Error::from("comptime intrinsic has no result type")
                        })?,
                    ));
                    return Ok(intrinsic_instructions);
                }

                let lir_kind = match kind {
                    IntrinsicKind::Format => lir::LirIntrinsicKind::Format,
                    IntrinsicKind::TimeNow => lir::LirIntrinsicKind::TimeNow,
                    IntrinsicKind::ProcMacroTokenStreamFromStr => {
                        lir::LirIntrinsicKind::ProcMacroTokenStreamFromStr
                    }
                    IntrinsicKind::ProcMacroTokenStreamToString => {
                        lir::LirIntrinsicKind::ProcMacroTokenStreamToString
                    }
                    IntrinsicKind::Print | IntrinsicKind::Println => {
                        return Err(fp_core::error::Error::from(
                            "print/println must be emitted as statements".to_string(),
                        ));
                    }
                    IntrinsicKind::Slice => {
                        if args.len() != 3 {
                            return Err(fp_core::error::Error::from(format!(
                                "slice intrinsic expects 3 arguments, got {}",
                                args.len()
                            )));
                        }

                        let base_op = &args[0];
                        let start_op = &args[1];
                        let end_op = &args[2];

                        let base_value = self.transform_operand(base_op)?;
                        intrinsic_instructions.extend(self.take_queued_instructions());
                        let start_value = self.transform_operand(start_op)?;
                        intrinsic_instructions.extend(self.take_queued_instructions());
                        let end_value = self.transform_operand(end_op)?;
                        intrinsic_instructions.extend(self.take_queued_instructions());

                        let base_lir_ty = self.type_of_operand(base_op);
                        let elem_lir_ty = destination_lir_ty
                            .as_ref()
                            .and_then(Self::slice_element_type)
                            .or_else(|| match base_lir_ty.as_ref() {
                                Some(lir::LirType::Struct { .. }) => {
                                    base_lir_ty.as_ref().and_then(Self::slice_element_type)
                                }
                                Some(lir::LirType::Ptr(elem)) => Some((**elem).clone()),
                                _ => None,
                            })
                            .ok_or_else(|| {
                                fp_core::error::Error::from("slice intrinsic has no element type")
                            })?;

                        let ptr_ty = lir::LirType::Ptr(Box::new(elem_lir_ty.clone()));
                        let base_ptr = match base_lir_ty.as_ref() {
                            Some(lir::LirType::Struct { .. })
                                if base_lir_ty
                                    .as_ref()
                                    .and_then(Self::slice_element_type)
                                    .is_some() =>
                            {
                                self.extract_slice_field(
                                    base_value,
                                    0,
                                    ptr_ty.clone(),
                                    &mut intrinsic_instructions,
                                )
                            }
                            Some(lir::LirType::Ptr(_)) => base_value,
                            other => {
                                return Err(fp_core::error::Error::from(format!(
                                    "slice intrinsic base type {:?} is not supported in MIR→LIR lowering",
                                    other
                                )));
                            }
                        };

                        let len_id = self.next_id();
                        intrinsic_instructions.push(lir::LirInstruction {
                            id: len_id,
                            kind: lir::LirInstructionKind::Sub(
                                end_value.clone(),
                                start_value.clone(),
                            ),
                            result: Some(lir::LirRegister {
                                id: len_id,
                                ty: lir::LirType::I64,
                            }),
                            debug_info: None,
                        });
                        let len_value = lir::LirValue::register(len_id, lir::LirType::I64);

                        let gep_id = self.next_id();
                        intrinsic_instructions.push(lir::LirInstruction {
                            id: gep_id,
                            kind: lir::LirInstructionKind::GetElementPtr {
                                ptr: base_ptr,
                                indices: vec![start_value],
                                inbounds: true,
                            },
                            result: Some(lir::LirRegister {
                                id: gep_id,
                                ty: ptr_ty.clone(),
                            }),
                            debug_info: None,
                        });
                        let slice_ptr = lir::LirValue::register(gep_id, ptr_ty);

                        if matches!(destination_lir_ty, Some(lir::LirType::Ptr(_))) {
                            result_value = Some(slice_ptr);
                            return Ok(intrinsic_instructions);
                        }

                        let slice_value = self.build_slice_value_with_len_value(
                            slice_ptr,
                            len_value,
                            &elem_lir_ty,
                            &mut intrinsic_instructions,
                        )?;
                        result_value = Some(slice_value);
                        return Ok(intrinsic_instructions);
                    }
                    _ => {
                        return Err(fp_core::error::Error::from(format!(
                            "unsupported intrinsic in assignment: {:?}",
                            kind
                        )));
                    }
                };
                let mut lir_args = Vec::with_capacity(args.len());
                for arg in args {
                    let value = self.transform_operand(arg)?;
                    intrinsic_instructions.extend(self.take_queued_instructions());
                    lir_args.push(value);
                }

                let instr_id = self.next_id();
                intrinsic_instructions.push(lir::LirInstruction {
                    id: instr_id,
                    kind: lir::LirInstructionKind::IntrinsicCall {
                        kind: lir_kind,
                        format: format.clone(),
                        args: lir_args,
                    },
                    result: destination_lir_ty
                        .clone()
                        .map(|ty| lir::LirRegister { id: instr_id, ty }),
                    debug_info: None,
                });
                result_value = Some(lir::LirValue::register(
                    instr_id,
                    destination_lir_ty.clone().ok_or_else(|| {
                        fp_core::error::Error::from("intrinsic has no destination type")
                    })?,
                ));
                // `Format`/`TimeNow`/`ProcMacroTokenStream{FromStr,ToString}`
                // (the only sub-cases reaching here, since every other
                // sub-case above returns early) — merge into the outer
                // `instructions` so they aren't dropped.
                instructions.append(&mut intrinsic_instructions);
            }
            mir::Rvalue::BinaryOp(bin_op, lhs, rhs) => {
                let lhs_value = self.transform_operand(lhs)?;
                instructions.extend(self.take_queued_instructions());
                let rhs_value = self.transform_operand(rhs)?;
                instructions.extend(self.take_queued_instructions());

                let instr_id = self.next_id();
                let lir_kind =
                    self.lower_binary_op(bin_op.clone(), lhs_value.clone(), rhs_value.clone());
                let result_ty = destination_lir_ty.clone().ok_or_else(|| {
                    fp_core::error::Error::from("binary operation has no destination type")
                })?;

                instructions.push(lir::LirInstruction {
                    id: instr_id,
                    kind: lir_kind,
                    result: Some(lir::LirRegister {
                        id: instr_id,
                        ty: result_ty.clone(),
                    }),
                    debug_info: None,
                });

                result_value = Some(lir::LirValue::register(instr_id, result_ty));
            }
            mir::Rvalue::Repeat(operand, len) => {
                let elem_ty = match place_ty.as_ref().map(|ty| &ty.kind) {
                    Some(TyKind::Array(elem, _)) => *elem.clone(),
                    other => {
                        return Err(fp_core::error::Error::from(format!(
                            "MIR→LIR: repeat expects array destination, found {:?}",
                            other
                        )));
                    }
                };
                let mut fields = Vec::with_capacity(*len as usize);
                for _ in 0..*len {
                    fields.push(operand.clone());
                }
                let (mut aggregate_insts, aggregate_value) =
                    self.handle_aggregate(place, &mir::AggregateKind::Array(elem_ty), &fields)?;
                instructions.append(&mut aggregate_insts);
                result_value = aggregate_value;
            }
            mir::Rvalue::UnaryOp(un_op, operand) => {
                let operand_value = self.transform_operand(operand)?;
                instructions.extend(self.take_queued_instructions());

                let result_ty = destination_lir_ty.clone().ok_or_else(|| {
                    fp_core::error::Error::from("unary operation has no destination type")
                })?;

                let instr_id = self.next_id();
                let lir_kind =
                    self.lower_unary_op(un_op.clone(), operand_value.clone(), &result_ty)?;

                instructions.push(lir::LirInstruction {
                    id: instr_id,
                    kind: lir_kind,
                    result: Some(lir::LirRegister {
                        id: instr_id,
                        ty: result_ty.clone(),
                    }),
                    debug_info: None,
                });

                result_value = Some(lir::LirValue::register(instr_id, result_ty));
            }
            mir::Rvalue::Aggregate(kind, fields) => {
                let mut handled = false;
                if let mir::AggregateKind::Array(elem_ty) = kind {
                    let place_is_slice =
                        matches!(place_ty.as_ref().map(|ty| &ty.kind), Some(TyKind::Slice(_)));
                    let slice_wrapper = destination_lir_ty.as_ref().and_then(|ty| {
                        let lir::LirType::Struct { fields, .. } = ty else {
                            return None;
                        };
                        if fields.len() != 1 {
                            return None;
                        }
                        let elem = Self::slice_element_type(&fields[0])?;
                        Some((fields[0].clone(), elem))
                    });
                    if place_is_slice || slice_wrapper.is_some() {
                        let elem_lir_ty = self.lir_type_from_ty(elem_ty);
                        let align = self.alignment_for_lir_type(&elem_lir_ty);
                        let len = fields.len() as u64;
                        let ptr_ty = lir::LirType::Ptr(Box::new(elem_lir_ty.clone()));
                        let size_value = lir::LirValue::constant(
                            self.integer_constant(&lir::LirType::I32, len as i64)?,
                        );

                        let alloca_id = self.next_id();
                        instructions.push(lir::LirInstruction {
                            id: alloca_id,
                            kind: lir::LirInstructionKind::Alloca {
                                size: size_value,
                                alignment: align,
                            },
                            result: Some(lir::LirRegister {
                                id: alloca_id,
                                ty: ptr_ty.clone(),
                            }),
                            debug_info: None,
                        });
                        let array_ptr = lir::LirValue::register(alloca_id, ptr_ty);

                        for (idx, operand) in fields.iter().enumerate() {
                            let value = self.transform_operand(operand)?;
                            instructions.extend(self.take_queued_instructions());
                            let coerced = self.coerce_aggregate_value_with_source(
                                value,
                                self.type_of_operand(operand).as_ref(),
                                &elem_lir_ty,
                                &mut instructions,
                            )?;
                            let index_value = lir::LirValue::constant(
                                self.unsigned_constant(&lir::LirType::I64, idx as u64)?,
                            );
                            let elem_ptr = self.element_ptr_at(
                                array_ptr.clone(),
                                &elem_lir_ty,
                                index_value,
                                &mut instructions,
                            );
                            instructions.push(lir::LirInstruction {
                                id: self.next_id(),
                                kind: lir::LirInstructionKind::Store {
                                    value: coerced,
                                    address: elem_ptr,
                                    alignment: Some(align),
                                    volatile: false,
                                },
                                result: None,
                                debug_info: None,
                            });
                        }

                        result_value = Some(self.build_slice_value(
                            array_ptr,
                            len,
                            &elem_lir_ty,
                            &mut instructions,
                        )?);
                        if slice_wrapper.is_some() {
                            let wrapper_ty = destination_lir_ty.clone().ok_or_else(|| {
                                fp_core::error::Error::from("slice wrapper has no destination type")
                            })?;
                            let wrapper_value = lir::LirValue::constant(lir::LirConstant::undef(
                                wrapper_ty.clone(),
                            ));
                            let insert_id = self.next_id();
                            instructions.push(lir::LirInstruction {
                                id: insert_id,
                                kind: lir::LirInstructionKind::InsertValue {
                                    aggregate: wrapper_value,
                                    element: result_value.clone().ok_or_else(|| {
                                        fp_core::error::Error::from("slice value was not built")
                                    })?,
                                    indices: vec![0],
                                },
                                result: Some(lir::LirRegister {
                                    id: insert_id,
                                    ty: wrapper_ty.clone(),
                                }),
                                debug_info: None,
                            });
                            result_value = Some(lir::LirValue::register(insert_id, wrapper_ty));
                        }
                        handled = true;
                    }
                    if !handled {
                        if let PlaceAccess::Address(addr) = &target_access {
                            if let lir::LirType::Ptr(inner) = &addr.lir_ty {
                                if !matches!(inner.as_ref(), lir::LirType::Array(..)) {
                                    let elem_lir_ty = self.lir_type_from_ty(elem_ty);
                                    let align = self.alignment_for_lir_type(&elem_lir_ty);
                                    for (idx, operand) in fields.iter().enumerate() {
                                        let value = self.transform_operand(operand)?;
                                        instructions.extend(self.take_queued_instructions());
                                        let coerced = self.coerce_aggregate_value_with_source(
                                            value,
                                            self.type_of_operand(operand).as_ref(),
                                            &elem_lir_ty,
                                            &mut instructions,
                                        )?;
                                        let index_value = lir::LirValue::constant(
                                            self.unsigned_constant(&lir::LirType::I64, idx as u64)?,
                                        );
                                        let elem_ptr = self.element_ptr_at(
                                            addr.ptr.clone(),
                                            &elem_lir_ty,
                                            index_value,
                                            &mut instructions,
                                        );
                                        instructions.push(lir::LirInstruction {
                                            id: self.next_id(),
                                            kind: lir::LirInstructionKind::Store {
                                                value: coerced,
                                                address: elem_ptr,
                                                alignment: Some(align),
                                                volatile: false,
                                            },
                                            result: None,
                                            debug_info: None,
                                        });
                                    }
                                    return Ok(instructions);
                                }
                            }
                        }
                    }
                }
                if !handled {
                    let (mut aggregate_insts, aggregate_value) =
                        self.handle_aggregate(place, kind, fields)?;
                    instructions.append(&mut aggregate_insts);
                    result_value = aggregate_value;
                }
            }
            mir::Rvalue::ContainerLiteral { kind, elements } => {
                let elem_lir_ty = self.container_element_lir_type(kind);
                let len = self.container_len(kind);
                let align = self.alignment_for_lir_type(&elem_lir_ty);
                let ptr_ty = lir::LirType::Ptr(Box::new(elem_lir_ty.clone()));
                let size_value =
                    lir::LirValue::constant(self.integer_constant(&lir::LirType::I32, len as i64)?);

                let alloca_id = self.next_id();
                instructions.push(lir::LirInstruction {
                    id: alloca_id,
                    kind: lir::LirInstructionKind::Alloca {
                        size: size_value,
                        alignment: align,
                    },
                    result: Some(lir::LirRegister {
                        id: alloca_id,
                        ty: ptr_ty.clone(),
                    }),
                    debug_info: None,
                });
                let array_ptr = lir::LirValue::register(alloca_id, ptr_ty);

                for (idx, operand) in elements.iter().enumerate() {
                    let value = self.transform_operand(operand)?;
                    instructions.extend(self.take_queued_instructions());
                    let coerced = self.coerce_aggregate_value_with_source(
                        value,
                        self.type_of_operand(operand).as_ref(),
                        &elem_lir_ty,
                        &mut instructions,
                    )?;
                    let index_value = lir::LirValue::constant(
                        self.unsigned_constant(&lir::LirType::I64, idx as u64)?,
                    );
                    let elem_ptr = self.element_ptr_at(
                        array_ptr.clone(),
                        &elem_lir_ty,
                        index_value,
                        &mut instructions,
                    );
                    instructions.push(lir::LirInstruction {
                        id: self.next_id(),
                        kind: lir::LirInstructionKind::Store {
                            value: coerced,
                            address: elem_ptr,
                            alignment: Some(align),
                            volatile: false,
                        },
                        result: None,
                        debug_info: None,
                    });
                }

                result_value = Some(self.build_slice_value(
                    array_ptr,
                    len,
                    &elem_lir_ty,
                    &mut instructions,
                )?);
            }
            mir::Rvalue::ContainerMapLiteral { kind, entries } => {
                let entry_lir_ty = self.container_element_lir_type(kind);
                let len = self.container_len(kind);
                let align = self.alignment_for_lir_type(&entry_lir_ty);
                let ptr_ty = lir::LirType::Ptr(Box::new(entry_lir_ty.clone()));
                let size_value =
                    lir::LirValue::constant(self.integer_constant(&lir::LirType::I32, len as i64)?);

                let alloca_id = self.next_id();
                instructions.push(lir::LirInstruction {
                    id: alloca_id,
                    kind: lir::LirInstructionKind::Alloca {
                        size: size_value,
                        alignment: align,
                    },
                    result: Some(lir::LirRegister {
                        id: alloca_id,
                        ty: ptr_ty.clone(),
                    }),
                    debug_info: None,
                });
                let array_ptr = lir::LirValue::register(alloca_id, ptr_ty);

                let entry_fields = match &entry_lir_ty {
                    lir::LirType::Struct { fields, .. } => fields.clone(),
                    _ => {
                        return Err(fp_core::error::Error::from(
                            "map entry type must be a struct",
                        ));
                    }
                };
                let key_ty = entry_fields
                    .get(0)
                    .cloned()
                    .ok_or_else(|| fp_core::error::Error::from("map entry has no key type"))?;
                let value_ty = entry_fields
                    .get(1)
                    .cloned()
                    .ok_or_else(|| fp_core::error::Error::from("map entry has no value type"))?;

                for (idx, (key_op, value_op)) in entries.iter().enumerate() {
                    let key_val = self.transform_operand(key_op)?;
                    instructions.extend(self.take_queued_instructions());
                    let value_val = self.transform_operand(value_op)?;
                    instructions.extend(self.take_queued_instructions());

                    let key_val = self.coerce_aggregate_value_with_source(
                        key_val,
                        self.type_of_operand(key_op).as_ref(),
                        &key_ty,
                        &mut instructions,
                    )?;
                    let value_val = self.coerce_aggregate_value_with_source(
                        value_val,
                        self.type_of_operand(value_op).as_ref(),
                        &value_ty,
                        &mut instructions,
                    )?;

                    let mut entry_value =
                        lir::LirValue::constant(lir::LirConstant::undef(entry_lir_ty.clone()));
                    let key_insert = self.next_id();
                    instructions.push(lir::LirInstruction {
                        id: key_insert,
                        kind: lir::LirInstructionKind::InsertValue {
                            aggregate: entry_value,
                            element: key_val,
                            indices: vec![0],
                        },
                        result: Some(lir::LirRegister {
                            id: key_insert,
                            ty: entry_lir_ty.clone(),
                        }),
                        debug_info: None,
                    });
                    entry_value = lir::LirValue::register(key_insert, entry_lir_ty.clone());
                    let value_insert = self.next_id();
                    instructions.push(lir::LirInstruction {
                        id: value_insert,
                        kind: lir::LirInstructionKind::InsertValue {
                            aggregate: entry_value,
                            element: value_val,
                            indices: vec![1],
                        },
                        result: Some(lir::LirRegister {
                            id: value_insert,
                            ty: entry_lir_ty.clone(),
                        }),
                        debug_info: None,
                    });
                    let entry_value = lir::LirValue::register(value_insert, entry_lir_ty.clone());

                    let index_value = lir::LirValue::constant(
                        self.unsigned_constant(&lir::LirType::I64, idx as u64)?,
                    );
                    let entry_ptr = self.element_ptr_at(
                        array_ptr.clone(),
                        &entry_lir_ty,
                        index_value,
                        &mut instructions,
                    );
                    instructions.push(lir::LirInstruction {
                        id: self.next_id(),
                        kind: lir::LirInstructionKind::Store {
                            value: entry_value,
                            address: entry_ptr,
                            alignment: Some(align),
                            volatile: false,
                        },
                        result: None,
                        debug_info: None,
                    });
                }

                result_value = Some(self.build_slice_value(
                    array_ptr,
                    len,
                    &entry_lir_ty,
                    &mut instructions,
                )?);
            }
            mir::Rvalue::ContainerLen { kind, container } => {
                if let mir::ContainerKind::List { len: 0, .. } = kind {
                    let slice_value = self.transform_operand(container)?;
                    instructions.extend(self.take_queued_instructions());
                    let len_value = self.extract_slice_field(
                        slice_value,
                        1,
                        lir::LirType::I64,
                        &mut instructions,
                    );
                    result_value = Some(len_value);
                } else {
                    let len = self.container_len(kind);
                    result_value = Some(lir::LirValue::constant(
                        self.unsigned_constant(&lir::LirType::I64, len)?,
                    ));
                }
            }
            mir::Rvalue::ContainerGet {
                kind,
                container,
                key,
            } => {
                let elem_lir_ty = self.container_element_lir_type(kind);
                let slice_value = self.transform_operand(container)?;
                instructions.extend(self.take_queued_instructions());
                let slice_ptr_ty = lir::LirType::Ptr(Box::new(elem_lir_ty.clone()));
                let slice_ptr = self.extract_slice_field(
                    slice_value,
                    0,
                    slice_ptr_ty.clone(),
                    &mut instructions,
                );

                match kind {
                    mir::ContainerKind::List { .. } => {
                        let idx_value = self.transform_operand(key)?;
                        instructions.extend(self.take_queued_instructions());
                        let elem_ptr = self.element_ptr_at(
                            slice_ptr,
                            &elem_lir_ty,
                            idx_value,
                            &mut instructions,
                        );
                        let load_id = self.next_id();
                        instructions.push(lir::LirInstruction {
                            id: load_id,
                            kind: lir::LirInstructionKind::Load {
                                address: elem_ptr,
                                alignment: Some(self.alignment_for_lir_type(&elem_lir_ty)),
                                volatile: false,
                            },
                            result: Some(lir::LirRegister {
                                id: load_id,
                                ty: elem_lir_ty.clone(),
                            }),
                            debug_info: None,
                        });
                        result_value = Some(lir::LirValue::register(load_id, elem_lir_ty));
                    }
                    mir::ContainerKind::Map {
                        key_ty,
                        value_ty,
                        len,
                    } => {
                        let query_value = self.transform_operand(key)?;
                        instructions.extend(self.take_queued_instructions());
                        let key_lir_ty = self.lir_type_from_ty(key_ty);
                        let value_lir_ty = self.lir_type_from_ty(value_ty);
                        let query_value = self.coerce_aggregate_value_with_source(
                            query_value,
                            self.type_of_operand(key).as_ref(),
                            &key_lir_ty,
                            &mut instructions,
                        )?;

                        let mut current =
                            lir::LirValue::constant(lir::LirConstant::undef(value_lir_ty.clone()));
                        let found_zero = lir::LirValue::constant(
                            lir::LirConstant::integer(lir::LirType::I1, lir::LirInteger::I1(false))
                                .map_err(|error| fp_core::error::Error::from(error.to_string()))?,
                        );
                        let mut found = found_zero.clone();

                        for idx in 0..*len {
                            let index_value = lir::LirValue::constant(
                                self.unsigned_constant(&lir::LirType::I64, idx)?,
                            );
                            let entry_ptr = self.element_ptr_at(
                                slice_ptr.clone(),
                                &elem_lir_ty,
                                index_value,
                                &mut instructions,
                            );
                            let load_id = self.next_id();
                            instructions.push(lir::LirInstruction {
                                id: load_id,
                                kind: lir::LirInstructionKind::Load {
                                    address: entry_ptr,
                                    alignment: Some(self.alignment_for_lir_type(&elem_lir_ty)),
                                    volatile: false,
                                },
                                result: Some(lir::LirRegister {
                                    id: load_id,
                                    ty: elem_lir_ty.clone(),
                                }),
                                debug_info: None,
                            });
                            let entry_value = lir::LirValue::register(load_id, elem_lir_ty.clone());

                            let key_id = self.next_id();
                            instructions.push(lir::LirInstruction {
                                id: key_id,
                                kind: lir::LirInstructionKind::ExtractValue {
                                    aggregate: entry_value.clone(),
                                    indices: vec![0],
                                },
                                result: Some(lir::LirRegister {
                                    id: key_id,
                                    ty: key_lir_ty.clone(),
                                }),
                                debug_info: None,
                            });
                            let entry_key = lir::LirValue::register(key_id, key_lir_ty.clone());

                            let value_id = self.next_id();
                            instructions.push(lir::LirInstruction {
                                id: value_id,
                                kind: lir::LirInstructionKind::ExtractValue {
                                    aggregate: entry_value,
                                    indices: vec![1],
                                },
                                result: Some(lir::LirRegister {
                                    id: value_id,
                                    ty: value_lir_ty.clone(),
                                }),
                                debug_info: None,
                            });
                            let entry_value =
                                lir::LirValue::register(value_id, value_lir_ty.clone());

                            let cmp_id = self.next_id();
                            instructions.push(lir::LirInstruction {
                                id: cmp_id,
                                kind: lir::LirInstructionKind::Eq(entry_key, query_value.clone()),
                                result: Some(lir::LirRegister {
                                    id: cmp_id,
                                    ty: lir::LirType::I1,
                                }),
                                debug_info: None,
                            });
                            let cmp_val = lir::LirValue::register(cmp_id, lir::LirType::I1);

                            let select_id = self.next_id();
                            instructions.push(lir::LirInstruction {
                                id: select_id,
                                kind: lir::LirInstructionKind::Select {
                                    condition: cmp_val.clone(),
                                    if_true: entry_value,
                                    if_false: current,
                                },
                                result: Some(lir::LirRegister {
                                    id: select_id,
                                    ty: value_lir_ty.clone(),
                                }),
                                debug_info: None,
                            });
                            current = lir::LirValue::register(select_id, value_lir_ty.clone());

                            let found_id = self.next_id();
                            instructions.push(lir::LirInstruction {
                                id: found_id,
                                kind: lir::LirInstructionKind::Or(cmp_val, found),
                                result: Some(lir::LirRegister {
                                    id: found_id,
                                    ty: lir::LirType::I1,
                                }),
                                debug_info: None,
                            });
                            found = lir::LirValue::register(found_id, lir::LirType::I1);
                        }

                        let value_slot = self.next_id();
                        instructions.push(lir::LirInstruction {
                            id: value_slot,
                            kind: lir::LirInstructionKind::Alloca {
                                size: lir::LirValue::constant(
                                    self.unsigned_constant(&lir::LirType::I64, 8)?,
                                ),
                                alignment: 8,
                            },
                            result: Some(lir::LirRegister {
                                id: value_slot,
                                ty: lir::LirType::Ptr(Box::new(value_lir_ty.clone())),
                            }),
                            debug_info: None,
                        });
                        let slot_ptr = lir::LirValue::register(
                            value_slot,
                            lir::LirType::Ptr(Box::new(value_lir_ty.clone())),
                        );
                        instructions.push(lir::LirInstruction {
                            id: self.next_id(),
                            kind: lir::LirInstructionKind::Store {
                                value: current,
                                address: slot_ptr.clone(),
                                alignment: Some(8),
                                volatile: false,
                            },
                            result: None,
                            debug_info: None,
                        });

                        let load_addr_id = self.next_id();
                        instructions.push(lir::LirInstruction {
                            id: load_addr_id,
                            kind: lir::LirInstructionKind::Select {
                                condition: found,
                                if_true: slot_ptr,
                                if_false: found_zero,
                            },
                            result: Some(lir::LirRegister {
                                id: load_addr_id,
                                ty: lir::LirType::Ptr(Box::new(value_lir_ty.clone())),
                            }),
                            debug_info: None,
                        });
                        let recheck_id = self.next_id();
                        instructions.push(lir::LirInstruction {
                            id: recheck_id,
                            kind: lir::LirInstructionKind::Load {
                                address: lir::LirValue::register(
                                    load_addr_id,
                                    lir::LirType::Ptr(Box::new(value_lir_ty.clone())),
                                ),
                                alignment: Some(self.alignment_for_lir_type(&value_lir_ty)),
                                volatile: false,
                            },
                            result: Some(lir::LirRegister {
                                id: recheck_id,
                                ty: value_lir_ty.clone(),
                            }),
                            debug_info: None,
                        });
                        result_value = Some(lir::LirValue::register(recheck_id, value_lir_ty));
                    }
                }
            }
            mir::Rvalue::ContainerPush {
                kind,
                container,
                value,
            } => {
                let elem_lir_ty = self.container_element_lir_type(kind);
                let elem_size = self.size_of_lir_type(&elem_lir_ty).max(1);
                let ptr_ty = lir::LirType::Ptr(Box::new(elem_lir_ty.clone()));

                let slice_value = self.transform_operand(container)?;
                instructions.extend(self.take_queued_instructions());
                let old_ptr = self.extract_slice_field(
                    slice_value.clone(),
                    0,
                    ptr_ty.clone(),
                    &mut instructions,
                );
                let old_len =
                    self.extract_slice_field(slice_value, 1, lir::LirType::I64, &mut instructions);

                let value_operand = self.transform_operand(value)?;
                instructions.extend(self.take_queued_instructions());
                let value_operand = self.coerce_aggregate_value_with_source(
                    value_operand,
                    self.type_of_operand(value).as_ref(),
                    &elem_lir_ty,
                    &mut instructions,
                )?;

                let one = lir::LirValue::constant(self.integer_constant(&lir::LirType::I64, 1)?);
                let new_len_id = self.next_id();
                instructions.push(lir::LirInstruction {
                    id: new_len_id,
                    kind: lir::LirInstructionKind::Add(old_len.clone(), one),
                    result: Some(lir::LirRegister {
                        id: new_len_id,
                        ty: lir::LirType::I64,
                    }),
                    debug_info: None,
                });
                let new_len = lir::LirValue::register(new_len_id, lir::LirType::I64);

                let elem_size_val =
                    lir::LirValue::constant(self.unsigned_constant(&lir::LirType::I64, elem_size)?);
                let new_size_id = self.next_id();
                instructions.push(lir::LirInstruction {
                    id: new_size_id,
                    kind: lir::LirInstructionKind::Mul(new_len.clone(), elem_size_val),
                    result: Some(lir::LirRegister {
                        id: new_size_id,
                        ty: lir::LirType::I64,
                    }),
                    debug_info: None,
                });
                let new_byte_size = lir::LirValue::register(new_size_id, lir::LirType::I64);

                let malloc_id = self.call_extern_c_function(
                    "malloc",
                    vec![(new_byte_size, lir::LirType::I64)],
                    ptr_ty.clone(),
                    &mut instructions,
                )?;
                let new_ptr = lir::LirValue::register(malloc_id, ptr_ty.clone());

                let elem_size_val2 =
                    lir::LirValue::constant(self.unsigned_constant(&lir::LirType::I64, elem_size)?);
                let old_size_id = self.next_id();
                instructions.push(lir::LirInstruction {
                    id: old_size_id,
                    kind: lir::LirInstructionKind::Mul(old_len.clone(), elem_size_val2),
                    result: Some(lir::LirRegister {
                        id: old_size_id,
                        ty: lir::LirType::I64,
                    }),
                    debug_info: None,
                });
                let old_byte_size = lir::LirValue::register(old_size_id, lir::LirType::I64);

                // Always copy, even when `old_byte_size` is 0 (a fresh/empty
                // container) — `memcpy` with `n == 0` is a well-defined
                // no-op, so no branch is needed here.
                self.call_extern_c_function(
                    "memcpy",
                    vec![
                        (new_ptr.clone(), ptr_ty.clone()),
                        (old_ptr, ptr_ty.clone()),
                        (old_byte_size, lir::LirType::I64),
                    ],
                    ptr_ty.clone(),
                    &mut instructions,
                )?;

                let new_elem_ptr =
                    self.element_ptr_at(new_ptr.clone(), &elem_lir_ty, old_len, &mut instructions);
                instructions.push(lir::LirInstruction {
                    id: self.next_id(),
                    kind: lir::LirInstructionKind::Store {
                        value: value_operand,
                        address: new_elem_ptr,
                        alignment: Some(self.alignment_for_lir_type(&elem_lir_ty)),
                        volatile: false,
                    },
                    result: None,
                    debug_info: None,
                });

                result_value = Some(self.build_slice_value_with_len_value(
                    new_ptr,
                    new_len,
                    &elem_lir_ty,
                    &mut instructions,
                )?);
            }
            mir::Rvalue::StrFromRawParts { ptr, len } => {
                let ptr_value = self.transform_operand(ptr)?;
                instructions.extend(self.take_queued_instructions());
                let len_value = self.transform_operand(len)?;
                instructions.extend(self.take_queued_instructions());

                result_value = Some(self.build_slice_value_with_len_value(
                    ptr_value,
                    len_value,
                    &lir::LirType::I8,
                    &mut instructions,
                )?);
            }
            mir::Rvalue::Ref(_, _, borrowed_place) => {
                let borrowed_access = self.resolve_place(borrowed_place)?;
                instructions.extend(self.take_queued_instructions());
                let pointer = match borrowed_access {
                    PlaceAccess::Address(addr) => addr.ptr,
                    PlaceAccess::Value { value, lir_ty, .. } => {
                        if matches!(lir_ty, lir::LirType::Ptr(_)) {
                            value
                        } else {
                            let alloca_id = self.next_id();
                            let pointer_type = lir::LirType::Ptr(Box::new(lir_ty.clone()));
                            let size_value = lir::LirValue::constant(
                                self.integer_constant(&lir::LirType::I32, 1)?,
                            );
                            let alignment = self.alignment_for_lir_type(&lir_ty);
                            instructions.push(lir::LirInstruction {
                                id: alloca_id,
                                kind: lir::LirInstructionKind::Alloca {
                                    size: size_value,
                                    alignment,
                                },
                                result: Some(lir::LirRegister {
                                    id: alloca_id,
                                    ty: pointer_type.clone(),
                                }),
                                debug_info: None,
                            });
                            let ptr_value = lir::LirValue::register(alloca_id, pointer_type);
                            instructions.push(lir::LirInstruction {
                                id: self.next_id(),
                                kind: lir::LirInstructionKind::Store {
                                    value,
                                    address: ptr_value.clone(),
                                    alignment: Some(alignment),
                                    volatile: false,
                                },
                                result: None,
                                debug_info: None,
                            });
                            ptr_value
                        }
                    }
                };
                result_value = Some(pointer);
            }
            mir::Rvalue::AddressOf(_, borrowed_place) => {
                let borrowed_access = self.resolve_place(borrowed_place)?;
                instructions.extend(self.take_queued_instructions());
                let pointer = match borrowed_access {
                    PlaceAccess::Address(addr) => addr.ptr,
                    PlaceAccess::Value { value, lir_ty, .. } => {
                        if matches!(lir_ty, lir::LirType::Ptr(_)) {
                            value
                        } else {
                            let alloca_id = self.next_id();
                            let pointer_type = lir::LirType::Ptr(Box::new(lir_ty.clone()));
                            let size_value = lir::LirValue::constant(
                                self.integer_constant(&lir::LirType::I32, 1)?,
                            );
                            let alignment = self.alignment_for_lir_type(&lir_ty);
                            instructions.push(lir::LirInstruction {
                                id: alloca_id,
                                kind: lir::LirInstructionKind::Alloca {
                                    size: size_value,
                                    alignment,
                                },
                                result: Some(lir::LirRegister {
                                    id: alloca_id,
                                    ty: pointer_type.clone(),
                                }),
                                debug_info: None,
                            });
                            let ptr_value = lir::LirValue::register(alloca_id, pointer_type);
                            instructions.push(lir::LirInstruction {
                                id: self.next_id(),
                                kind: lir::LirInstructionKind::Store {
                                    value,
                                    address: ptr_value.clone(),
                                    alignment: Some(alignment),
                                    volatile: false,
                                },
                                result: None,
                                debug_info: None,
                            });
                            ptr_value
                        }
                    }
                };
                result_value = Some(pointer);
            }
            mir::Rvalue::Len(place) => {
                let place_ty = self.lookup_place_type(place).ok_or_else(|| {
                    crate::error::optimization_error(
                        "MIR→LIR: missing type information for len() operand",
                    )
                })?;
                match &place_ty.kind {
                    TyKind::Array(_, len) => {
                        let len_value = self.array_length_from_const(len) as i64;
                        result_value = Some(lir::LirValue::constant(
                            self.unsigned_constant(&lir::LirType::I64, len_value as u64)?,
                        ));
                    }
                    TyKind::Slice(_) => {
                        let access = self.resolve_place(place)?;
                        instructions.extend(self.take_queued_instructions());
                        let slice_value = match access {
                            PlaceAccess::Address(addr) => {
                                let load_id = self.next_id();
                                instructions.push(lir::LirInstruction {
                                    id: load_id,
                                    kind: lir::LirInstructionKind::Load {
                                        address: addr.ptr,
                                        alignment: Some(addr.alignment),
                                        volatile: false,
                                    },
                                    result: Some(lir::LirRegister {
                                        id: load_id,
                                        ty: addr.lir_ty.clone(),
                                    }),
                                    debug_info: None,
                                });
                                lir::LirValue::register(load_id, addr.lir_ty)
                            }
                            PlaceAccess::Value { value, .. } => value,
                        };

                        let extract_id = self.next_id();
                        instructions.push(lir::LirInstruction {
                            id: extract_id,
                            kind: lir::LirInstructionKind::ExtractValue {
                                aggregate: slice_value,
                                indices: vec![1],
                            },
                            result: Some(lir::LirRegister {
                                id: extract_id,
                                ty: lir::LirType::I64,
                            }),
                            debug_info: None,
                        });
                        result_value = Some(lir::LirValue::register(extract_id, lir::LirType::I64));
                    }
                    _ => {
                        return Err(crate::error::optimization_error(
                            "MIR→LIR: len() expects array or slice operand",
                        ));
                    }
                }
            }
            mir::Rvalue::Cast(cast_kind, operand, _ty) => {
                let operand_value = self.transform_operand(operand)?;
                instructions.extend(self.take_queued_instructions());
                let target_ty = destination_lir_ty.clone().ok_or_else(|| {
                    fp_core::error::Error::from("cast operation has no destination type")
                })?;

                if matches!(target_ty, lir::LirType::Void) {
                    result_value =
                        Some(lir::LirValue::constant(lir::LirConstant::undef(target_ty)));
                    return Ok(instructions);
                }

                {
                    let src_ty = operand_value.ty.clone();
                    let target_is_ptr = matches!(target_ty, lir::LirType::Ptr(_));
                    if self.is_float_type(&src_ty) && target_is_ptr {
                        let int_ty = lir::LirType::I64;
                        let fp_to_int_id = self.next_id();
                        instructions.push(lir::LirInstruction {
                            id: fp_to_int_id,
                            kind: lir::LirInstructionKind::FPToSI(
                                operand_value.clone(),
                                int_ty.clone(),
                            ),
                            result: Some(lir::LirRegister {
                                id: fp_to_int_id,
                                ty: int_ty.clone(),
                            }),
                            debug_info: None,
                        });
                        let ptr_id = self.next_id();
                        instructions.push(lir::LirInstruction {
                            id: ptr_id,
                            kind: lir::LirInstructionKind::IntToPtr(lir::LirValue::register(
                                fp_to_int_id,
                                int_ty.clone(),
                            )),
                            result: Some(lir::LirRegister {
                                id: ptr_id,
                                ty: target_ty.clone(),
                            }),
                            debug_info: None,
                        });
                        result_value = Some(lir::LirValue::register(ptr_id, target_ty));
                        return Ok(instructions);
                    }
                    if matches!(src_ty, lir::LirType::Ptr(_)) && self.is_float_type(&target_ty) {
                        let int_ty = lir::LirType::I64;
                        let ptr_to_int_id = self.next_id();
                        instructions.push(lir::LirInstruction {
                            id: ptr_to_int_id,
                            kind: lir::LirInstructionKind::PtrToInt(operand_value.clone()),
                            result: Some(lir::LirRegister {
                                id: ptr_to_int_id,
                                ty: int_ty.clone(),
                            }),
                            debug_info: None,
                        });
                        let fp_id = self.next_id();
                        instructions.push(lir::LirInstruction {
                            id: fp_id,
                            kind: lir::LirInstructionKind::SIToFP(
                                lir::LirValue::register(ptr_to_int_id, int_ty.clone()),
                                target_ty.clone(),
                            ),
                            result: Some(lir::LirRegister {
                                id: fp_id,
                                ty: target_ty.clone(),
                            }),
                            debug_info: None,
                        });
                        result_value = Some(lir::LirValue::register(fp_id, target_ty));
                        return Ok(instructions);
                    }
                }

                let instr_id = self.next_id();
                let instr_kind =
                    self.lower_cast(cast_kind.clone(), operand_value.clone(), target_ty.clone());

                instructions.push(lir::LirInstruction {
                    id: instr_id,
                    kind: instr_kind,
                    result: Some(lir::LirRegister {
                        id: instr_id,
                        ty: target_ty.clone(),
                    }),
                    debug_info: None,
                });

                result_value = Some(lir::LirValue::register(instr_id, target_ty));
            }
            _ => {
                instructions.push(lir::LirInstruction {
                    id: self.next_id(),
                    kind: lir::LirInstructionKind::Unreachable,
                    result: None,
                    debug_info: None,
                });
                return Ok(instructions);
            }
        }

        if let Some(value) = result_value.clone() {
            let (target_lir_ty, target_is_zst) = match &target_access {
                PlaceAccess::Address(addr) => (addr.lir_ty.clone(), Self::is_zero_sized(&addr.ty)),
                PlaceAccess::Value { ty, lir_ty, .. } => (lir_ty.clone(), Self::is_zero_sized(ty)),
            };
            let mut adjusted_value = value;

            if !target_is_zst {
                adjusted_value = self.coerce_assignment_value(
                    adjusted_value,
                    &target_lir_ty,
                    &mut instructions,
                )?;
            }

            if let PlaceAccess::Address(addr) = &target_access {
                if matches!(
                    adjusted_value,
                    lir::LirValue {
                        kind: lir::LirValueKind::Function(_),
                        ..
                    }
                ) {
                    self.local_storage.remove(&place.local);
                } else if !target_is_zst {
                    let store_id = self.next_id();
                    instructions.push(lir::LirInstruction {
                        id: store_id,
                        kind: lir::LirInstructionKind::Store {
                            value: adjusted_value.clone(),
                            address: addr.ptr.clone(),
                            alignment: Some(addr.alignment),
                            volatile: false,
                        },
                        result: None,
                        debug_info: None,
                    });
                }
            }

            let mut should_update_register_map = assign_whole_place;
            if matches!(target_access, PlaceAccess::Address(_))
                && self.local_storage.contains_key(&place.local)
            {
                should_update_register_map = false;
            }
            if let Some(return_local) = self.return_local {
                if place.local == return_local {
                    should_update_register_map = true;
                }
            }

            if matches!(
                adjusted_value,
                lir::LirValue {
                    kind: lir::LirValueKind::Function(_),
                    ..
                }
            ) {
                should_update_register_map = true;
            }

            if should_update_register_map {
                self.register_map.insert(place.local, adjusted_value);
            }
        }

        Ok(instructions)
    }

    /// Transform a MIR terminator to LIR terminator
    /// Transform a MIR operand to LIR value
    pub(super) fn transform_operand(&mut self, operand: &mir::Operand) -> Result<lir::LirValue> {
        match operand {
            mir::Operand::Move(place) | mir::Operand::Copy(place) => {
                let access = self.resolve_place(place)?;
                match access {
                    PlaceAccess::Address(addr) => {
                        let load_id = self.next_id();
                        self.queued_instructions.push(lir::LirInstruction {
                            id: load_id,
                            kind: lir::LirInstructionKind::Load {
                                address: addr.ptr.clone(),
                                alignment: Some(addr.alignment),
                                volatile: false,
                            },
                            result: Some(lir::LirRegister {
                                id: load_id,
                                ty: addr.lir_ty.clone(),
                            }),
                            debug_info: None,
                        });
                        Ok(lir::LirValue::register(load_id, addr.lir_ty))
                    }
                    PlaceAccess::Value { value, .. } => Ok(value),
                }
            }
            mir::Operand::Constant(constant) => match &constant.literal {
                mir::ConstantKind::FnDef(def_id, substs) => {
                    let name = match self.function_def_map.get(&(def_id.clone(), substs.clone())) {
                        Some(name) => name.clone(),
                        // Never predeclared (no whole-program sweep ran, or
                        // this is a forward/cross-package reference that
                        // sweep wouldn't have covered anyway) — resolve it
                        // lazily off `self.mir_program` instead, exactly
                        // mirroring `HirToMirLowerer::resolve_callee_path`'s
                        // own signature-only registration on demand: this
                        // package's own `sigs` first (a forward reference
                        // within the same package), then every other
                        // loaded package's (a cross-package call). Cached
                        // into `function_def_map`/`function_signatures`/
                        // `function_package_ids` by `register_function_signature`,
                        // so a second reference to the same `def_id` hits
                        // the ordinary fast path above.
                        None => {
                            let resolved = self
                                .mir_program
                                .package(&self.package_id)
                                .and_then(|package| package.borrow().sigs.get(def_id).cloned())
                                .map(|func| (func, None))
                                .or_else(|| {
                                    self.mir_program.packages.iter().find_map(
                                        |(dep_id, dep_package)| {
                                            if dep_id == &self.package_id {
                                                return None;
                                            }
                                            dep_package
                                                .borrow()
                                                .sigs
                                                .get(def_id)
                                                .cloned()
                                                .map(|func| (func, Some(dep_id.clone())))
                                        },
                                    )
                                });
                            let (func, package_id) = resolved.ok_or_else(|| {
                                fp_core::error::Error::from(format!(
                                    "missing MIR function definition {} with substitutions {:?}",
                                    def_id, substs
                                ))
                            })?;
                            self.register_function_signature(&func, package_id).ok_or_else(|| {
                                fp_core::error::Error::from(format!(
                                    "unresolved generic signature for function definition {} with substitutions {:?}",
                                    def_id, substs
                                ))
                            })?
                        }
                    };
                    self.function_value(name)
                }
                mir::ConstantKind::Fn(name) => {
                    let mut function_name = self
                        .function_symbol_map
                        .get(&String::from(name.clone()))
                        .cloned()
                        .unwrap_or_else(|| String::from(name.clone()));
                    let suffix = format!("::{name}");
                    let matches: Vec<String> = self
                        .function_signatures
                        .keys()
                        .filter(|candidate| candidate.ends_with(&suffix))
                        .cloned()
                        .collect();
                    if matches.len() == 1 {
                        function_name = matches[0].clone();
                    }
                    self.function_value(function_name)
                }
                mir::ConstantKind::Global(path) => {
                    let name = path.to_string();
                    let mapped_name = self.function_symbol_map.get(&name).cloned().unwrap_or(name);
                    if self.function_signatures.contains_key(&mapped_name) {
                        return self.function_value(mapped_name);
                    }
                    if let Some(runtime_target) = (self.runtime_symbol_map)(&mapped_name) {
                        return self.function_value(runtime_target.as_str().to_owned());
                    }
                    Ok(lir::LirValue::global(
                        self.resolve_global_symbol(path),
                        self.lir_type_from_ty(&constant.ty),
                    ))
                }
                mir::ConstantKind::Str(_) => Ok(lir::LirValue::constant(
                    self.constant_to_lir_constant(constant, &constant.ty)?,
                )),
                mir::ConstantKind::Int(value) => Ok(lir::LirValue::constant(
                    self.integer_constant(&self.lir_type_from_ty(&constant.ty), *value)
                        .map_err(|error| {
                            fp_core::error::Error::from(format!(
                                "constant at {:?} with MIR type {:?}: {}",
                                constant.span, constant.ty, error
                            ))
                        })?,
                )),
                mir::ConstantKind::UInt(value) => Ok(lir::LirValue::constant(
                    self.unsigned_constant(&self.lir_type_from_ty(&constant.ty), *value)
                        .map_err(|error| {
                            fp_core::error::Error::from(format!(
                                "constant at {:?} with MIR type {:?}: {}",
                                constant.span, constant.ty, error
                            ))
                        })?,
                )),
                mir::ConstantKind::Float(value) => Ok(lir::LirValue::constant(
                    self.float_constant(&self.lir_type_from_ty(&constant.ty), *value)?,
                )),
                mir::ConstantKind::Bool(value) => Ok(lir::LirValue::constant(
                    lir::LirConstant::integer(lir::LirType::I1, lir::LirInteger::I1(*value))
                        .map_err(|error| fp_core::error::Error::from(error.to_string()))?,
                )),
                mir::ConstantKind::Null => Ok(lir::LirValue::constant(lir::LirConstant::null(
                    self.lir_type_from_ty(&constant.ty),
                ))),
                mir::ConstantKind::Undef => Ok(lir::LirValue::constant(lir::LirConstant::undef(
                    self.lir_type_from_ty(&constant.ty),
                ))),
                mir::ConstantKind::Val(value) => Ok(lir::LirValue::constant(
                    self.const_value_to_lir_constant(value, &constant.ty)?,
                )),
                _ => {
                    return Err(crate::error::optimization_error(
                        "Unsupported constant kind for MIR→LIR",
                    ));
                }
            },
        }
    }

    /// Helper methods
    /// Extracts just the type arguments from an ADT's generic-arg list —
    /// used as (part of) the cache key for per-instantiation layout caches
    /// (`struct_layouts`, `full_layouts`), since two instantiations of the
    /// same generic struct/enum (e.g. `Option<i64>` vs.
    /// `Option<CommandMockMatch>`) can have entirely different field
    /// layouts and must never share a cache entry keyed by `DefId` alone.
    pub(super) fn adt_substs_types(substs: &[mir::ty::GenericArg]) -> Vec<mir::Ty> {
        substs
            .iter()
            .filter_map(|arg| match arg {
                mir::ty::GenericArg::Type(ty) => Some(ty.clone()),
                _ => None,
            })
            .collect()
    }

    pub(super) fn compute_mutable_locals(&self, mir_body: &mir::Body) -> HashSet<mir::LocalId> {
        let mut assignment_counts: HashMap<mir::LocalId, usize> = HashMap::new();
        for basic_block in &mir_body.basic_blocks {
            for stmt in &basic_block.statements {
                if let mir::StatementKind::Assign(place, _) = &stmt.kind {
                    *assignment_counts.entry(place.local).or_insert(0) += 1;
                }
            }
        }

        assignment_counts
            .into_iter()
            .filter_map(|(local, count)| if count > 1 { Some(local) } else { None })
            .collect()
    }

    pub(super) fn initialize_local_storage(&mut self, mir_body: &mir::Body) {
        self.entry_allocas.clear();
        self.local_storage.clear();

        let locals: Vec<_> = self.mutable_locals.clone().into_iter().collect();
        for local in locals {
            let local_index = local as usize;
            if local_index >= self.local_types.len() {
                continue;
            }

            let ty = &self.local_types[local_index];
            if Self::is_zero_sized(ty) {
                continue;
            }

            let lir_ty = self.lir_type_from_ty(ty);
            let alignment = self.alignment_for_lir_type(&lir_ty);
            if alignment == 0 {
                continue;
            }

            let alloca_id = self.next_id();
            let pointer_type = lir::LirType::Ptr(Box::new(lir_ty.clone()));
            let size_value = lir::LirValue::constant(
                self.integer_constant(&lir::LirType::I32, 1)
                    .expect("one must fit i32"),
            );
            self.entry_allocas.push(lir::LirInstruction {
                id: alloca_id,
                kind: lir::LirInstructionKind::Alloca {
                    size: size_value,
                    alignment,
                },
                result: Some(lir::LirRegister {
                    id: alloca_id,
                    ty: pointer_type.clone(),
                }),
                debug_info: None,
            });

            self.local_storage.insert(
                local,
                LocalStorage {
                    ptr_value: lir::LirValue::register(alloca_id, pointer_type.clone()),
                    element_type: lir_ty.clone(),
                    alignment,
                },
            );

            if local > 0 && (local as usize) <= mir_body.arg_count {
                let store_id = self.next_id();
                self.entry_allocas.push(lir::LirInstruction {
                    id: store_id,
                    kind: lir::LirInstructionKind::Store {
                        value: lir::LirValue::local(local, lir_ty.clone()),
                        address: lir::LirValue::register(alloca_id, pointer_type.clone()),
                        alignment: Some(alignment),
                        volatile: false,
                    },
                    result: None,
                    debug_info: None,
                });
            }
        }

        // Ensure entry allocas appear once at the top of the entry block
        if self.entry_allocas.is_empty() {
            return;
        }
    }

    pub(super) fn slice_lir_type(&self, elem_lir: &lir::LirType) -> lir::LirType {
        lir::LirType::Struct {
            fields: vec![
                lir::LirType::Ptr(Box::new(elem_lir.clone())),
                lir::LirType::I64,
            ],
            packed: false,
            name: Some("__slice".to_string()),
        }
    }

    pub(super) fn container_element_lir_type(&self, kind: &mir::ContainerKind) -> lir::LirType {
        match kind {
            mir::ContainerKind::List { elem_ty, .. } => self.lir_type_from_ty(elem_ty),
            mir::ContainerKind::Map {
                key_ty, value_ty, ..
            } => {
                let key_lir = self.lir_type_from_ty(key_ty);
                let value_lir = self.lir_type_from_ty(value_ty);
                lir::LirType::Struct {
                    fields: vec![key_lir, value_lir],
                    packed: false,
                    name: Some("__map_entry".to_string()),
                }
            }
        }
    }

    pub(super) fn container_len(&self, kind: &mir::ContainerKind) -> u64 {
        match kind {
            mir::ContainerKind::List { len, .. } => *len,
            mir::ContainerKind::Map { len, .. } => *len,
        }
    }

    pub(super) fn ensure_i64_value(
        &mut self,
        value: lir::LirValue,
        instructions: &mut Vec<lir::LirInstruction>,
    ) -> lir::LirValue {
        let current_ty = value.ty.clone();
        if current_ty == lir::LirType::I64 {
            return value;
        }
        let cast_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: cast_id,
            kind: lir::LirInstructionKind::SextOrTrunc(value, lir::LirType::I64),
            result: Some(lir::LirRegister {
                id: cast_id,
                ty: lir::LirType::I64,
            }),
            debug_info: None,
        });
        lir::LirValue::register(cast_id, lir::LirType::I64)
    }

    pub(super) fn element_ptr_at(
        &mut self,
        base_ptr: lir::LirValue,
        element_ty: &lir::LirType,
        index_value: lir::LirValue,
        instructions: &mut Vec<lir::LirInstruction>,
    ) -> lir::LirValue {
        let index_i64 = self.ensure_i64_value(index_value, instructions);
        let element_size = self.size_of_lir_type(element_ty);
        let offset_value = if element_size == 1 {
            index_i64
        } else {
            let scale = lir::LirValue::constant(
                self.integer_constant(&lir::LirType::I64, element_size as i64)
                    .expect("element size must fit i64"),
            );
            let mul_id = self.next_id();
            instructions.push(lir::LirInstruction {
                id: mul_id,
                kind: lir::LirInstructionKind::Mul(index_i64, scale),
                result: Some(lir::LirRegister {
                    id: mul_id,
                    ty: lir::LirType::I64,
                }),
                debug_info: None,
            });
            lir::LirValue::register(mul_id, lir::LirType::I64)
        };

        let i8_ptr_ty = lir::LirType::Ptr(Box::new(lir::LirType::I8));
        let base_i8_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: base_i8_id,
            kind: lir::LirInstructionKind::Bitcast(base_ptr, i8_ptr_ty.clone()),
            result: Some(lir::LirRegister {
                id: base_i8_id,
                ty: i8_ptr_ty.clone(),
            }),
            debug_info: None,
        });
        let base_i8 = lir::LirValue::register(base_i8_id, i8_ptr_ty.clone());

        let gep_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: gep_id,
            kind: lir::LirInstructionKind::GetElementPtr {
                ptr: base_i8,
                indices: vec![offset_value],
                inbounds: true,
            },
            result: Some(lir::LirRegister {
                id: gep_id,
                ty: i8_ptr_ty.clone(),
            }),
            debug_info: None,
        });

        let elem_ptr_ty = lir::LirType::Ptr(Box::new(element_ty.clone()));
        let cast_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: cast_id,
            kind: lir::LirInstructionKind::Bitcast(
                lir::LirValue::register(gep_id, i8_ptr_ty.clone()),
                elem_ptr_ty.clone(),
            ),
            result: Some(lir::LirRegister {
                id: cast_id,
                ty: elem_ptr_ty.clone(),
            }),
            debug_info: None,
        });
        lir::LirValue::register(cast_id, elem_ptr_ty)
    }

    pub(super) fn build_slice_value(
        &mut self,
        ptr: lir::LirValue,
        len: u64,
        elem_lir: &lir::LirType,
        instructions: &mut Vec<lir::LirInstruction>,
    ) -> Result<lir::LirValue> {
        let slice_ty = self.slice_lir_type(elem_lir);
        let mut current = lir::LirValue::constant(lir::LirConstant::undef(slice_ty.clone()));
        let ptr_insert = self.next_id();
        instructions.push(lir::LirInstruction {
            id: ptr_insert,
            kind: lir::LirInstructionKind::InsertValue {
                aggregate: current,
                element: ptr,
                indices: vec![0],
            },
            result: Some(lir::LirRegister {
                id: ptr_insert,
                ty: slice_ty.clone(),
            }),
            debug_info: None,
        });
        current = lir::LirValue::register(ptr_insert, slice_ty.clone());

        let len_value = lir::LirValue::constant(
            lir::LirConstant::integer(lir::LirType::I64, lir::LirInteger::I64(len))
                .map_err(|error| fp_core::error::Error::from(error.to_string()))?,
        );
        let len_insert = self.next_id();
        instructions.push(lir::LirInstruction {
            id: len_insert,
            kind: lir::LirInstructionKind::InsertValue {
                aggregate: current,
                element: len_value,
                indices: vec![1],
            },
            result: Some(lir::LirRegister {
                id: len_insert,
                ty: slice_ty.clone(),
            }),
            debug_info: None,
        });
        Ok(lir::LirValue::register(len_insert, slice_ty))
    }

    pub(super) fn build_slice_value_with_len_value(
        &mut self,
        ptr: lir::LirValue,
        len: lir::LirValue,
        elem_lir: &lir::LirType,
        instructions: &mut Vec<lir::LirInstruction>,
    ) -> Result<lir::LirValue> {
        let slice_ty = self.slice_lir_type(elem_lir);
        let ptr_insert = self.next_id();
        instructions.push(lir::LirInstruction {
            id: ptr_insert,
            kind: lir::LirInstructionKind::InsertValue {
                aggregate: lir::LirValue::constant(lir::LirConstant::undef(slice_ty.clone())),
                element: ptr,
                indices: vec![0],
            },
            result: Some(lir::LirRegister {
                id: ptr_insert,
                ty: slice_ty.clone(),
            }),
            debug_info: None,
        });
        let len_insert = self.next_id();
        instructions.push(lir::LirInstruction {
            id: len_insert,
            kind: lir::LirInstructionKind::InsertValue {
                aggregate: lir::LirValue::register(ptr_insert, slice_ty.clone()),
                element: len,
                indices: vec![1],
            },
            result: Some(lir::LirRegister {
                id: len_insert,
                ty: slice_ty.clone(),
            }),
            debug_info: None,
        });
        Ok(lir::LirValue::register(len_insert, slice_ty))
    }

    pub(super) fn slice_element_type(expected: &lir::LirType) -> Option<lir::LirType> {
        let lir::LirType::Struct { fields, name, .. } = expected else {
            return None;
        };
        if name.as_deref() != Some("__slice") || fields.len() != 2 {
            return None;
        }
        if !matches!(fields[1], lir::LirType::I64) {
            return None;
        }
        let lir::LirType::Ptr(elem) = &fields[0] else {
            return None;
        };
        Some((**elem).clone())
    }

    pub(super) fn extract_slice_field(
        &mut self,
        value: lir::LirValue,
        field_index: u32,
        field_ty: lir::LirType,
        instructions: &mut Vec<lir::LirInstruction>,
    ) -> lir::LirValue {
        let extract_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: extract_id,
            kind: lir::LirInstructionKind::ExtractValue {
                aggregate: value,
                indices: vec![field_index],
            },
            result: Some(lir::LirRegister {
                id: extract_id,
                ty: field_ty.clone(),
            }),
            debug_info: None,
        });
        lir::LirValue::register(extract_id, field_ty)
    }

    pub(super) fn size_of_lir_type(&self, ty: &lir::LirType) -> u64 {
        self.data_layout.size_of(ty).unwrap_or(0)
    }

    pub(super) fn alignment_for_lir_type(&self, ty: &lir::LirType) -> u32 {
        self.data_layout.align_of(ty).unwrap_or(1).max(1)
    }

    pub(super) fn emit_load_from_address(
        &mut self,
        addr: PlaceAddress,
        block: &mut lir::LirBasicBlock,
    ) -> lir::LirValue {
        if Self::is_zero_sized(&addr.ty) {
            return lir::LirValue::constant(lir::LirConstant::undef(addr.lir_ty));
        }
        let load_id = self.next_id();
        block.instructions.push(lir::LirInstruction {
            id: load_id,
            kind: lir::LirInstructionKind::Load {
                address: addr.ptr,
                alignment: Some(addr.alignment),
                volatile: false,
            },
            result: Some(lir::LirRegister {
                id: load_id,
                ty: addr.lir_ty.clone(),
            }),
            debug_info: None,
        });
        lir::LirValue::register(load_id, addr.lir_ty)
    }

    pub(super) fn materialize_pointer_from_value(
        &mut self,
        value: lir::LirValue,
        value_ty: lir::LirType,
        block: &mut lir::LirBasicBlock,
    ) -> Result<lir::LirValue> {
        let alloca_id = self.next_id();
        let pointer_type = lir::LirType::Ptr(Box::new(value_ty.clone()));
        let size_value = lir::LirValue::constant(
            lir::LirConstant::integer(lir::LirType::I32, lir::LirInteger::I32(1))
                .map_err(|error| fp_core::error::Error::from(error.to_string()))?,
        );
        let alignment = self.alignment_for_lir_type(&value_ty);
        block.instructions.push(lir::LirInstruction {
            id: alloca_id,
            kind: lir::LirInstructionKind::Alloca {
                size: size_value,
                alignment,
            },
            result: Some(lir::LirRegister {
                id: alloca_id,
                ty: pointer_type.clone(),
            }),
            debug_info: None,
        });

        let ptr_value = lir::LirValue::register(alloca_id, pointer_type);
        let store_id = self.next_id();
        block.instructions.push(lir::LirInstruction {
            id: store_id,
            kind: lir::LirInstructionKind::Store {
                value,
                address: ptr_value.clone(),
                alignment: Some(alignment),
                volatile: false,
            },
            result: None,
            debug_info: None,
        });

        Ok(ptr_value)
    }

    pub(super) fn adjust_call_argument(
        &mut self,
        value: lir::LirValue,
        source_ty: Option<&Ty>,
        source_lir_ty: &lir::LirType,
        expected_ty: Option<&lir::LirType>,
        block: &mut lir::LirBasicBlock,
    ) -> Result<lir::LirValue> {
        if let Some(expected) = expected_ty {
            if let (Some(elem_lir_ty), lir::LirType::Array(_, len)) =
                (Self::slice_element_type(expected), source_lir_ty)
            {
                return self.build_slice_from_array_value(value, elem_lir_ty, *len, block);
            }
            if matches!(expected, lir::LirType::Ptr(_)) {
                if let lir::LirValueKind::Constant(lir::LirConstantKind::Data(
                    lir::LirConstantData::Integer(integer),
                )) = &value.kind
                {
                    if integer.is_zero() {
                        return Ok(lir::LirValue::constant(lir::LirConstant::null(
                            expected.clone(),
                        )));
                    }
                }
            }

            if matches!(source_lir_ty, lir::LirType::Void) {
                return Ok(if matches!(expected, lir::LirType::Ptr(_)) {
                    lir::LirValue::constant(lir::LirConstant::null(expected.clone()))
                } else {
                    lir::LirValue::constant(lir::LirConstant::undef(expected.clone()))
                });
            }

            if source_lir_ty == expected {
                return Ok(value);
            }
            return self.cast_value_to_type(value, source_lir_ty.clone(), expected.clone(), block);
        }

        Ok(self.promote_vararg_argument(value, source_ty, source_lir_ty, block))
    }

    pub(super) fn build_slice_from_array_ptr(
        &mut self,
        array_ptr: lir::LirValue,
        elem_lir_ty: lir::LirType,
        len: u64,
        block: &mut lir::LirBasicBlock,
    ) -> Result<lir::LirValue> {
        let slice_ty = self.slice_lir_type(&elem_lir_ty);
        let zero = lir::LirValue::constant(
            lir::LirConstant::integer(lir::LirType::I64, lir::LirInteger::I64(0))
                .map_err(|error| fp_core::error::Error::from(error.to_string()))?,
        );
        let array_ptr_ty = lir::LirType::Ptr(Box::new(lir::LirType::Array(
            Box::new(elem_lir_ty.clone()),
            len,
        )));

        let gep_id = self.next_id();
        block.instructions.push(lir::LirInstruction {
            id: gep_id,
            kind: lir::LirInstructionKind::GetElementPtr {
                ptr: array_ptr,
                indices: vec![zero.clone(), zero],
                inbounds: true,
            },
            result: Some(lir::LirRegister {
                id: gep_id,
                ty: array_ptr_ty.clone(),
            }),
            debug_info: None,
        });
        let elem_ptr = lir::LirValue::register(gep_id, array_ptr_ty);

        let insert_ptr_id = self.next_id();
        block.instructions.push(lir::LirInstruction {
            id: insert_ptr_id,
            kind: lir::LirInstructionKind::InsertValue {
                aggregate: lir::LirValue::constant(lir::LirConstant::undef(slice_ty.clone())),
                element: elem_ptr,
                indices: vec![0],
            },
            result: Some(lir::LirRegister {
                id: insert_ptr_id,
                ty: slice_ty.clone(),
            }),
            debug_info: None,
        });

        let len_value = lir::LirValue::constant(
            lir::LirConstant::integer(lir::LirType::I64, lir::LirInteger::I64(len))
                .map_err(|error| fp_core::error::Error::from(error.to_string()))?,
        );
        let insert_len_id = self.next_id();
        block.instructions.push(lir::LirInstruction {
            id: insert_len_id,
            kind: lir::LirInstructionKind::InsertValue {
                aggregate: lir::LirValue::register(insert_ptr_id, slice_ty.clone()),
                element: len_value,
                indices: vec![1],
            },
            result: Some(lir::LirRegister {
                id: insert_len_id,
                ty: slice_ty.clone(),
            }),
            debug_info: None,
        });

        Ok(lir::LirValue::register(insert_len_id, slice_ty))
    }

    pub(super) fn build_slice_from_array_value(
        &mut self,
        value: lir::LirValue,
        elem_lir_ty: lir::LirType,
        len: u64,
        block: &mut lir::LirBasicBlock,
    ) -> Result<lir::LirValue> {
        let array_ty = lir::LirType::Array(Box::new(elem_lir_ty.clone()), len);
        let array_ptr = self.materialize_pointer_from_value(value, array_ty, block)?;
        self.build_slice_from_array_ptr(array_ptr, elem_lir_ty, len, block)
    }

    pub(super) fn promote_vararg_argument(
        &mut self,
        value: lir::LirValue,
        source_ty: Option<&Ty>,
        source_lir_ty: &lir::LirType,
        block: &mut lir::LirBasicBlock,
    ) -> lir::LirValue {
        match source_lir_ty {
            lir::LirType::I1 | lir::LirType::I8 | lir::LirType::I16 => {
                let signed = matches!(source_ty.map(|ty| &ty.kind), Some(TyKind::Int(_)));
                self.extend_integer_value(
                    value,
                    source_lir_ty.clone(),
                    lir::LirType::I32,
                    signed,
                    block,
                )
            }
            lir::LirType::F32 => self.extend_float_value(value, lir::LirType::F64, block),
            _ => value,
        }
    }

    pub(super) fn cast_value_to_type(
        &mut self,
        value: lir::LirValue,
        from_ty: lir::LirType,
        target_ty: lir::LirType,
        block: &mut lir::LirBasicBlock,
    ) -> Result<lir::LirValue> {
        if matches!(target_ty, lir::LirType::Void) {
            return Ok(lir::LirValue::constant(lir::LirConstant::undef(target_ty)));
        }
        if from_ty == target_ty {
            return Ok(value);
        }
        if let (
            lir::LirType::Struct {
                fields: from_fields,
                ..
            },
            lir::LirType::Struct {
                fields: target_fields,
                ..
            },
        ) = (&from_ty, &target_ty)
        {
            return self.cast_struct_value_to_struct_type(
                value,
                from_fields,
                target_fields,
                target_ty.clone(),
                block,
            );
        }
        if self.is_float_type(&from_ty) && matches!(target_ty, lir::LirType::Ptr(_)) {
            let int_ty = lir::LirType::I64;
            let fp_to_int_id = self.next_id();
            block.instructions.push(lir::LirInstruction {
                id: fp_to_int_id,
                kind: lir::LirInstructionKind::FPToSI(value.clone(), int_ty.clone()),
                result: Some(lir::LirRegister {
                    id: fp_to_int_id,
                    ty: int_ty.clone(),
                }),
                debug_info: None,
            });
            let ptr_id = self.next_id();
            block.instructions.push(lir::LirInstruction {
                id: ptr_id,
                kind: lir::LirInstructionKind::IntToPtr(lir::LirValue::register(
                    fp_to_int_id,
                    int_ty.clone(),
                )),
                result: Some(lir::LirRegister {
                    id: ptr_id,
                    ty: target_ty.clone(),
                }),
                debug_info: None,
            });
            return Ok(lir::LirValue::register(ptr_id, target_ty));
        }
        if matches!(from_ty, lir::LirType::Ptr(_)) && self.is_float_type(&target_ty) {
            let int_ty = lir::LirType::I64;
            let ptr_to_int_id = self.next_id();
            block.instructions.push(lir::LirInstruction {
                id: ptr_to_int_id,
                kind: lir::LirInstructionKind::PtrToInt(value.clone()),
                result: Some(lir::LirRegister {
                    id: ptr_to_int_id,
                    ty: int_ty.clone(),
                }),
                debug_info: None,
            });
            let fp_id = self.next_id();
            block.instructions.push(lir::LirInstruction {
                id: fp_id,
                kind: lir::LirInstructionKind::SIToFP(
                    lir::LirValue::register(ptr_to_int_id, int_ty.clone()),
                    target_ty.clone(),
                ),
                result: Some(lir::LirRegister {
                    id: fp_id,
                    ty: target_ty.clone(),
                }),
                debug_info: None,
            });
            return Ok(lir::LirValue::register(fp_id, target_ty));
        }
        let id = self.next_id();
        let kind = if matches!(from_ty, lir::LirType::Ptr(_)) && self.is_integral_type(&target_ty) {
            lir::LirInstructionKind::PtrToInt(value.clone())
        } else if self.is_integral_type(&from_ty) && matches!(target_ty, lir::LirType::Ptr(_)) {
            lir::LirInstructionKind::IntToPtr(value.clone())
        } else if self.is_integral_type(&from_ty) && self.is_integral_type(&target_ty) {
            let src_w = self.type_bit_width(&from_ty);
            let dst_w = self.type_bit_width(&target_ty);
            if src_w == dst_w {
                lir::LirInstructionKind::Bitcast(value.clone(), target_ty.clone())
            } else {
                lir::LirInstructionKind::SextOrTrunc(value.clone(), target_ty.clone())
            }
        } else if self.is_float_type(&from_ty) && self.is_float_type(&target_ty) {
            let src_w = self.type_bit_width(&from_ty);
            let dst_w = self.type_bit_width(&target_ty);
            match (src_w, dst_w) {
                (Some(s), Some(d)) if d > s => {
                    lir::LirInstructionKind::FPExt(value.clone(), target_ty.clone())
                }
                (Some(s), Some(d)) if d < s => {
                    lir::LirInstructionKind::FPTrunc(value.clone(), target_ty.clone())
                }
                _ => lir::LirInstructionKind::Bitcast(value.clone(), target_ty.clone()),
            }
        } else if self.is_float_type(&from_ty) && self.is_integral_type(&target_ty) {
            lir::LirInstructionKind::FPToSI(value.clone(), target_ty.clone())
        } else if self.is_integral_type(&from_ty) && self.is_float_type(&target_ty) {
            lir::LirInstructionKind::SIToFP(value.clone(), target_ty.clone())
        } else {
            lir::LirInstructionKind::Bitcast(value.clone(), target_ty.clone())
        };
        block.instructions.push(lir::LirInstruction {
            id,
            kind,
            result: Some(lir::LirRegister {
                id,
                ty: target_ty.clone(),
            }),
            debug_info: None,
        });
        Ok(lir::LirValue::register(id, target_ty))
    }

    pub(super) fn cast_struct_value_to_struct_type(
        &mut self,
        value: lir::LirValue,
        from_fields: &[lir::LirType],
        target_fields: &[lir::LirType],
        target_ty: lir::LirType,
        block: &mut lir::LirBasicBlock,
    ) -> Result<lir::LirValue> {
        if let lir::LirValue {
            kind:
                lir::LirValueKind::Constant(lir::LirConstantKind::Aggregate(
                    lir::LirConstantAggregate::Struct(fields),
                )),
            ..
        } = &value
        {
            let mut adjusted = Vec::with_capacity(target_fields.len());
            for (index, target_field) in target_fields.iter().enumerate() {
                if let Some(field) = fields.get(index) {
                    adjusted.push(self.require_constant_type(field.clone(), target_field)?);
                } else if let Some(zero) = self.zero_constant_for_lir_type(target_field) {
                    adjusted.push(zero);
                } else {
                    adjusted.push(lir::LirConstant::undef(target_field.clone()));
                }
            }
            return Ok(lir::LirValue::constant(lir::LirConstant::aggregate(
                target_ty,
                lir::LirConstantAggregate::Struct(adjusted),
            )));
        }

        let mut current = lir::LirValue::constant(lir::LirConstant::undef(target_ty.clone()));
        for (index, target_field) in target_fields.iter().enumerate() {
            let element = if let Some(source_field) = from_fields.get(index) {
                let extract_id = self.next_id();
                block.instructions.push(lir::LirInstruction {
                    id: extract_id,
                    kind: lir::LirInstructionKind::ExtractValue {
                        aggregate: value.clone(),
                        indices: vec![index as u32],
                    },
                    result: Some(lir::LirRegister {
                        id: extract_id,
                        ty: source_field.clone(),
                    }),
                    debug_info: None,
                });
                let extracted = lir::LirValue::register(extract_id, source_field.clone());
                self.cast_value_to_type(
                    extracted,
                    source_field.clone(),
                    target_field.clone(),
                    block,
                )?
            } else if let Some(zero) = self.zero_value_for_lir_type(target_field) {
                zero
            } else {
                lir::LirValue::constant(lir::LirConstant::undef(target_field.clone()))
            };

            let insert_id = self.next_id();
            block.instructions.push(lir::LirInstruction {
                id: insert_id,
                kind: lir::LirInstructionKind::InsertValue {
                    aggregate: current,
                    element,
                    indices: vec![index as u32],
                },
                result: Some(lir::LirRegister {
                    id: insert_id,
                    ty: target_ty.clone(),
                }),
                debug_info: None,
            });
            current = lir::LirValue::register(insert_id, target_ty.clone());
        }
        Ok(current)
    }

    pub(super) fn extend_integer_value(
        &mut self,
        value: lir::LirValue,
        from_ty: lir::LirType,
        target_ty: lir::LirType,
        signed: bool,
        block: &mut lir::LirBasicBlock,
    ) -> lir::LirValue {
        if from_ty == target_ty {
            return value;
        }
        let id = self.next_id();
        let kind = if signed {
            lir::LirInstructionKind::SExt(value.clone(), target_ty.clone())
        } else {
            lir::LirInstructionKind::ZExt(value.clone(), target_ty.clone())
        };
        block.instructions.push(lir::LirInstruction {
            id,
            kind,
            result: Some(lir::LirRegister {
                id,
                ty: target_ty.clone(),
            }),
            debug_info: None,
        });
        lir::LirValue::register(id, target_ty)
    }

    pub(super) fn extend_float_value(
        &mut self,
        value: lir::LirValue,
        target_ty: lir::LirType,
        block: &mut lir::LirBasicBlock,
    ) -> lir::LirValue {
        let id = self.next_id();
        block.instructions.push(lir::LirInstruction {
            id,
            kind: lir::LirInstructionKind::FPExt(value.clone(), target_ty.clone()),
            result: Some(lir::LirRegister {
                id,
                ty: target_ty.clone(),
            }),
            debug_info: None,
        });
        lir::LirValue::register(id, target_ty)
    }

    pub(super) fn take_queued_instructions(&mut self) -> Vec<lir::LirInstruction> {
        std::mem::take(&mut self.queued_instructions)
    }

    pub(super) fn next_id(&mut self) -> lir::LirId {
        let id = self.next_lir_id;
        self.next_lir_id += 1;
        id
    }

    /* moved to aggregates.rs
    pub(super) fn handle_aggregate(
        &mut self,
        place: &mir::Place,
        kind: &mir::AggregateKind,
        fields: &[mir::Operand],
    ) -> Result<(Vec<lir::LirInstruction>, Option<lir::LirValue>)> {
        let mut instructions = Vec::new();
        let mut raw_values = Vec::with_capacity(fields.len());
        let mut constants = Vec::with_capacity(fields.len());
        // Track operand types so we can coerce register values into aggregate field types.
        // Without this, registers have no local type info and we can emit invalid insertvalue
        // operands (e.g. inserting i64 into a ptr field).
        let mut operand_types = Vec::with_capacity(fields.len());
        let mut all_constants = true;

        for operand in fields {
            let value = self.transform_operand(operand)?;
            instructions.extend(self.take_queued_instructions());
            operand_types.push(self.type_of_operand(operand));
            let is_constant = matches!(value.kind, lir::LirValueKind::Constant(_));
            if let lir::LirValueKind::Constant(ref constant_kind) = value.kind {
                constants.push(lir::LirConstant {
                    ty: value.ty.clone(),
                    kind: constant_kind.clone(),
                });
            }
            if !is_constant {
                all_constants = false;
            }
            raw_values.push(value);
        }

        let place_ty = self.lookup_place_type(place);
        let aggregate_ty = place_ty.as_ref().map(|ty| self.lir_type_from_ty(ty));
        let mut expected_field_tys = self.expected_aggregate_element_types(
            place_ty.as_ref(),
            aggregate_ty.as_ref(),
            raw_values.len(),
        );

        // If we could not infer field types from place/aggregate, derive them from operands.
        // This avoids falling back to Ptr(I8) for non-constant array elements (e.g. `-1`),
        // which can corrupt aggregate layouts and cause invalid insertvalue operands.
        if expected_field_tys.is_empty()
            || (matches!(aggregate_ty, Some(lir::LirType::Ptr(_)))
                && raw_values.len() == expected_field_tys.len()
                && expected_field_tys
                    .iter()
                    .all(|t| matches!(t, lir::LirType::Ptr(_))))
        {
            expected_field_tys = operand_types
                .iter()
                .zip(raw_values.iter())
                .map(|(operand_ty, value)| operand_ty.clone().unwrap_or_else(|| value.ty.clone()))
                .collect();
        }

        for (idx, ty) in expected_field_tys.iter_mut().enumerate() {
            if matches!(ty, lir::LirType::Void) {
                if let Some(operand) = fields.get(idx) {
                    if let Some(operand_ty) = self.type_of_operand(operand) {
                        *ty = operand_ty;
                    }
                }
            }
        }

        if fields.is_empty() {
            if let Some(lir_ty) = aggregate_ty {
                let value = match &lir_ty {
                    lir::LirType::Struct { .. } => {
                        lir::LirValue::constant(lir::LirConstant::aggregate(
                            lir_ty.clone(),
                            lir::LirConstantAggregate::Struct(Vec::new()),
                        ))
                    }
                    lir::LirType::Array(_, _len) => {
                        lir::LirValue::constant(lir::LirConstant::aggregate(
                            lir_ty.clone(),
                            lir::LirConstantAggregate::Array(Vec::new()),
                        ))
                    }
                    _ => {
                        // Non-aggregate zero-field values should not emit struct constants.
                        return Ok((instructions, None));
                    }
                };
                return Ok((instructions, Some(value)));
            }
            return Ok((instructions, None));
        }

        if all_constants {
            let adjusted_consts =
                self.adjust_constants_for_aggregate(constants, &expected_field_tys)?;
            if let Some(place_ty) = place_ty.as_ref() {
                if let Some(constant) =
                    self.constant_from_aggregate(kind, adjusted_consts, place_ty)
                {
                    return Ok((instructions, Some(lir::LirValue::constant(constant))));
                }
            }
        }

        // Choose an aggregate type suitable for InsertValue construction.
        // Prefer a real struct/array type; otherwise synthesize a struct from expected fields.
        let agg_construction_ty: Option<lir::LirType> =
            if matches!(kind, mir::AggregateKind::Array(_)) {
                match aggregate_ty.clone() {
                    Some(lir::LirType::Array(elem, _n)) => {
                        Some(lir::LirType::Array(elem, raw_values.len() as u64))
                    }
                    _ => {
                        let elem_ty = expected_field_tys
                            .get(0)
                            .cloned()
                            .unwrap_or_else(|| lir::LirType::I64);
                        Some(lir::LirType::Array(
                            Box::new(elem_ty),
                            raw_values.len() as u64,
                        ))
                    }
                }
            } else {
                match aggregate_ty.clone() {
                    Some(lir::LirType::Struct {
                        fields,
                        packed,
                        name,
                    }) => {
                        if fields.len() == raw_values.len() {
                            Some(lir::LirType::Struct {
                                fields,
                                packed,
                                name,
                            })
                        } else {
                            Some(lir::LirType::Struct {
                                fields: expected_field_tys.clone(),
                                packed: false,
                                name: None,
                            })
                        }
                    }
                    Some(lir::LirType::Array(elem, _n)) => {
                        Some(lir::LirType::Array(elem, raw_values.len() as u64))
                    }
                    Some(_other) => {
                        // Not an aggregate; synthesize a struct if multiple fields; if single field, just return it below
                        if raw_values.len() > 1 {
                            Some(lir::LirType::Struct {
                                fields: expected_field_tys.clone(),
                                packed: false,
                                name: None,
                            })
                        } else {
                            None
                        }
                    }
                    None => {
                        if raw_values.len() > 1 {
                            Some(lir::LirType::Struct {
                                fields: expected_field_tys.clone(),
                                packed: false,
                                name: None,
                            })
                        } else {
                            None
                        }
                    }
                }
            };

        if let Some(agg_ty) = agg_construction_ty {
            let mut current_value =
                lir::LirValue::constant(lir::LirConstant::undef(agg_ty.clone()));

            for (index, value) in raw_values.into_iter().enumerate() {
                let mut element = value;
                if let Some(field_ty) = expected_field_tys.get(index) {
                    let source_ty = operand_types.get(index).and_then(|ty| ty.clone());
                    element = self.coerce_aggregate_value_with_source(
                        element,
                        source_ty.as_ref(),
                        field_ty,
                        &mut instructions,
                    )?;
                }
                let instr_id = self.next_id();
                instructions.push(lir::LirInstruction {
                    id: instr_id,
                    kind: lir::LirInstructionKind::InsertValue {
                        aggregate: current_value.clone(),
                        element,
                        indices: vec![index as u32],
                    },
                    result: Some(lir::LirRegister {
                        id: instr_id,
                        ty: agg_ty.clone(),
                    }),
                    debug_info: None,
                });
                current_value = lir::LirValue::register(instr_id, agg_ty.clone());
            }

            return Ok((instructions, Some(current_value)));
        }

        // If we couldn't build an aggregate and there is exactly one element, return it directly
        if raw_values.len() == 1 {
            return Ok((instructions, raw_values.into_iter().next()));
        }

        Ok((instructions, None))
    }

    */
    pub(super) fn expected_aggregate_element_types(
        &self,
        place_ty: Option<&Ty>,
        aggregate_ty: Option<&lir::LirType>,
        element_count: usize,
    ) -> Vec<lir::LirType> {
        if let Some(ty) = place_ty {
            match &ty.kind {
                TyKind::Tuple(elements) => {
                    if elements.len() == element_count {
                        return elements
                            .iter()
                            .map(|elem| self.lir_type_from_ty(elem))
                            .collect();
                    }
                }
                TyKind::Array(element_ty, _) => {
                    let lir_elem_ty = self.lir_type_from_ty(element_ty);
                    return (0..element_count).map(|_| lir_elem_ty.clone()).collect();
                }
                _ => {}
            }
        }

        if let Some(lir::LirType::Struct { fields, .. }) = aggregate_ty {
            if fields.len() == element_count {
                return fields.clone();
            }
        }

        if let Some(lir::LirType::Array(element_ty, _)) = aggregate_ty {
            let elem_ty = *element_ty.clone();
            return (0..element_count).map(|_| elem_ty.clone()).collect();
        }

        aggregate_ty
            .cloned()
            .map(|ty| (0..element_count).map(|_| ty.clone()).collect())
            .unwrap_or_default()
    }

    pub(super) fn adjust_constants_for_aggregate(
        &self,
        constants: Vec<lir::LirConstant>,
        expected_field_tys: &[lir::LirType],
    ) -> Result<Vec<lir::LirConstant>> {
        constants
            .into_iter()
            .enumerate()
            .map(|(index, constant)| {
                if let Some(field_ty) = expected_field_tys.get(index) {
                    self.require_constant_type(constant, field_ty)
                } else {
                    Ok(constant)
                }
            })
            .collect::<Result<Vec<_>>>()
    }

    pub(super) fn coerce_aggregate_value_with_source(
        &mut self,
        value: lir::LirValue,
        source_ty: Option<&lir::LirType>,
        target_ty: &lir::LirType,
        instructions: &mut Vec<lir::LirInstruction>,
    ) -> Result<lir::LirValue> {
        match value.kind.clone() {
            lir::LirValueKind::Constant(constant_kind) => {
                let constant = lir::LirConstant {
                    ty: value.ty,
                    kind: constant_kind,
                };
                Ok(lir::LirValue::constant(
                    self.require_constant_type(constant, target_ty)?,
                ))
            }
            _ => self.cast_runtime_value_to_lir_type_with_source(
                value,
                source_ty,
                target_ty.clone(),
                instructions,
            ),
        }
    }

    pub(super) fn cast_runtime_value_to_lir_type_with_source(
        &mut self,
        value: lir::LirValue,
        source_ty: Option<&lir::LirType>,
        target_ty: lir::LirType,
        instructions: &mut Vec<lir::LirInstruction>,
    ) -> Result<lir::LirValue> {
        let current_ty = source_ty.cloned().unwrap_or_else(|| value.ty.clone());
        if current_ty == target_ty {
            return Ok(value);
        }

        if let lir::LirType::Ptr(pointee) = &current_ty {
            if pointee.as_ref() == &target_ty {
                let load_id = self.next_id();
                instructions.push(lir::LirInstruction {
                    id: load_id,
                    kind: lir::LirInstructionKind::Load {
                        address: value,
                        alignment: Some(self.alignment_for_lir_type(&target_ty)),
                        volatile: false,
                    },
                    result: Some(lir::LirRegister {
                        id: load_id,
                        ty: target_ty.clone(),
                    }),
                    debug_info: None,
                });
                return Ok(lir::LirValue::register(load_id, target_ty));
            }
        }

        if self.is_integral_type(&current_ty) && self.is_integral_type(&target_ty) {
            let current_bits = self.type_bit_width(&current_ty).unwrap_or(64);
            let target_bits = self.type_bit_width(&target_ty).unwrap_or(64);
            let instr_id = self.next_id();
            let kind = if target_bits < current_bits {
                lir::LirInstructionKind::Trunc(value.clone(), target_ty.clone())
            } else if target_bits > current_bits {
                lir::LirInstructionKind::ZExt(value.clone(), target_ty.clone())
            } else {
                lir::LirInstructionKind::Bitcast(value.clone(), target_ty.clone())
            };
            instructions.push(lir::LirInstruction {
                id: instr_id,
                kind,
                result: Some(lir::LirRegister {
                    id: instr_id,
                    ty: target_ty.clone(),
                }),
                debug_info: None,
            });
            return Ok(lir::LirValue::register(instr_id, target_ty.clone()));
        }

        if self.is_float_type(&current_ty) && self.is_float_type(&target_ty) {
            let current_bits = self.type_bit_width(&current_ty).unwrap_or(64);
            let target_bits = self.type_bit_width(&target_ty).unwrap_or(64);
            let instr_id = self.next_id();
            let kind = if target_bits > current_bits {
                lir::LirInstructionKind::FPExt(value.clone(), target_ty.clone())
            } else if target_bits < current_bits {
                lir::LirInstructionKind::FPTrunc(value.clone(), target_ty.clone())
            } else {
                lir::LirInstructionKind::Bitcast(value.clone(), target_ty.clone())
            };
            instructions.push(lir::LirInstruction {
                id: instr_id,
                kind,
                result: Some(lir::LirRegister {
                    id: instr_id,
                    ty: target_ty.clone(),
                }),
                debug_info: None,
            });
            return Ok(lir::LirValue::register(instr_id, target_ty.clone()));
        }

        // Pointer/integer interop is emitted only for explicit typed pairs.
        let current_is_int = self.is_integral_type(&current_ty);
        let target_is_int = self.is_integral_type(&target_ty);
        let current_is_ptr = matches!(&current_ty, lir::LirType::Ptr(_));
        let target_is_ptr = matches!(&target_ty, lir::LirType::Ptr(_));
        if current_is_int && target_is_ptr {
            let instr_id = self.next_id();
            instructions.push(lir::LirInstruction {
                id: instr_id,
                kind: lir::LirInstructionKind::IntToPtr(value.clone()),
                result: Some(lir::LirRegister {
                    id: instr_id,
                    ty: target_ty.clone(),
                }),
                debug_info: None,
            });
            return Ok(lir::LirValue::register(instr_id, target_ty.clone()));
        }
        if current_is_ptr && target_is_int {
            let instr_id = self.next_id();
            instructions.push(lir::LirInstruction {
                id: instr_id,
                kind: lir::LirInstructionKind::PtrToInt(value.clone()),
                result: Some(lir::LirRegister {
                    id: instr_id,
                    ty: target_ty.clone(),
                }),
                debug_info: None,
            });
            return Ok(lir::LirValue::register(instr_id, target_ty));
        }
        Err(fp_core::error::Error::from(format!(
            "unsupported runtime value conversion: {:?} to {:?}",
            current_ty, target_ty
        )))
    }

    pub(super) fn require_constant_type(
        &self,
        constant: lir::LirConstant,
        target_ty: &lir::LirType,
    ) -> Result<lir::LirConstant> {
        if constant.ty != *target_ty {
            if matches!(target_ty, lir::LirType::Ptr(_))
                && matches!(
                    constant.kind,
                    lir::LirConstantKind::Data(lir::LirConstantData::Integer(ref value))
                        if value.is_zero()
                )
            {
                return Ok(lir::LirConstant::null(target_ty.clone()));
            }
            // A zero-sized-type value (e.g. `()`, the payload of a
            // `Result<(), E>::Ok`) is represented, generically, as the
            // empty-field placeholder constant minted by
            // `get_or_create_register_for_place` — its exact shape doesn't
            // matter since it holds no data. When it lands in a field slot
            // that expects a real shape (e.g. an enum's opaque payload slot,
            // sized to fit the *other* variants), any bit pattern is
            // equally valid there — coerce to `undef` of the expected type
            // instead of treating this as a genuine type mismatch.
            if matches!(
                &constant,
                lir::LirConstant {
                    ty: lir::LirType::Struct { fields, .. },
                    kind: lir::LirConstantKind::Aggregate(lir::LirConstantAggregate::Struct(values)),
                } if fields.is_empty() && values.is_empty()
            ) {
                return Ok(lir::LirConstant::undef(target_ty.clone()));
            }
            // See `single_field_struct_tag_ty`'s doc comment: a fieldless
            // (C-like) enum's variant literal is sometimes const-folded
            // straight to its bare discriminant scalar rather than the
            // enum's own canonical `Struct{fields:[tag_ty]}` shape. Both
            // describe the same value; wrap the bare scalar to match.
            if let Some(tag_ty) = Self::single_field_struct_tag_ty(target_ty) {
                if *tag_ty == constant.ty {
                    return Ok(lir::LirConstant::aggregate(
                        target_ty.clone(),
                        lir::LirConstantAggregate::Struct(vec![constant]),
                    ));
                }
            }
            return Err(fp_core::error::Error::from(format!(
                "typed constant mismatch: {:?} versus {:?}",
                constant.ty, target_ty
            )));
        }
        Ok(constant)
    }

    pub(super) fn coerce_assignment_value(
        &mut self,
        value: lir::LirValue,
        expected_ty: &lir::LirType,
        instructions: &mut Vec<lir::LirInstruction>,
    ) -> Result<lir::LirValue> {
        if matches!(expected_ty, lir::LirType::Void) {
            return Ok(lir::LirValue::constant(lir::LirConstant::undef(
                expected_ty.clone(),
            )));
        }
        self.cast_runtime_value_to_lir_type_with_source(
            value,
            None,
            expected_ty.clone(),
            instructions,
        )
    }

    pub(super) fn lower_call_argument(
        &mut self,
        operand: &mir::Operand,
        expected_ty: Option<&lir::LirType>,
        block: &mut lir::LirBasicBlock,
    ) -> Result<lir::LirValue> {
        let expects_pointer = matches!(expected_ty, Some(lir::LirType::Ptr(_)));
        match operand {
            mir::Operand::Move(place) | mir::Operand::Copy(place) => {
                let access = self.resolve_place(place)?;
                block.instructions.extend(self.take_queued_instructions());
                match access {
                    PlaceAccess::Address(addr) => {
                        if let Some(expected) = expected_ty {
                            if let (Some(elem_lir_ty), TyKind::Array(_, len)) =
                                (Self::slice_element_type(expected), &addr.ty.kind)
                            {
                                let length = self.array_length_from_const(len);
                                return self.build_slice_from_array_ptr(
                                    addr.ptr,
                                    elem_lir_ty,
                                    length,
                                    block,
                                );
                            }
                        }
                        if expects_pointer {
                            if matches!(addr.lir_ty, lir::LirType::Ptr(_)) {
                                Ok(self.emit_load_from_address(addr.clone(), block))
                            } else {
                                Ok(addr.ptr)
                            }
                        } else {
                            let loaded = self.emit_load_from_address(addr.clone(), block);
                            self.adjust_call_argument(
                                loaded,
                                Some(&addr.ty),
                                &addr.lir_ty,
                                expected_ty,
                                block,
                            )
                        }
                    }
                    PlaceAccess::Value { value, ty, lir_ty } => {
                        if let (lir::LirType::Ptr(pointee), Some(expected)) = (&lir_ty, expected_ty)
                        {
                            if pointee.as_ref() == expected {
                                let load_id = self.next_id();
                                block.instructions.push(lir::LirInstruction {
                                    id: load_id,
                                    kind: lir::LirInstructionKind::Load {
                                        address: value,
                                        alignment: Some(self.alignment_for_lir_type(expected)),
                                        volatile: false,
                                    },
                                    result: Some(lir::LirRegister {
                                        id: load_id,
                                        ty: expected.clone(),
                                    }),
                                    debug_info: None,
                                });
                                return Ok(lir::LirValue::register(load_id, expected.clone()));
                            }
                        }
                        if expects_pointer {
                            if matches!(lir_ty, lir::LirType::Ptr(_)) {
                                Ok(value)
                            } else {
                                self.materialize_pointer_from_value(value, lir_ty, block)
                            }
                        } else {
                            self.adjust_call_argument(value, Some(&ty), &lir_ty, expected_ty, block)
                        }
                    }
                }
            }
            _ => {
                let value = self.transform_operand(operand)?;
                block.instructions.extend(self.take_queued_instructions());
                self.adjust_call_argument(value.clone(), None, &value.ty, expected_ty, block)
            }
        }
    }

    pub(super) fn build_lir_locals(&self, mir_body: &mir::Body) -> Vec<lir::LirLocal> {
        let arg_count = mir_body.arg_count;
        mir_body
            .locals
            .iter()
            .enumerate()
            .map(|(idx, decl)| {
                if matches!(decl.ty.kind, mir::ty::TyKind::Infer(_)) {
                    panic!(
                        "MIR-to-LIR ICE: unresolved local type at local {idx}: {:?}",
                        decl.ty
                    );
                }
                lir::LirLocal {
                    id: idx as u32,
                    ty: self.lir_type_from_ty(&decl.ty),
                    name: None,
                    is_argument: idx > 0 && idx <= arg_count,
                }
            })
            .collect()
    }

    pub(super) fn seed_argument_registers(&mut self, mir_body: &mir::Body) {
        for arg_index in 0..mir_body.arg_count {
            let local_id = (arg_index as mir::LocalId) + 1;
            let local_ty = self.lir_type_from_ty(&mir_body.locals[local_id as usize].ty);
            self.register_map
                .entry(local_id)
                .or_insert_with(|| lir::LirValue::local(local_id, local_ty));
        }
    }

    pub(super) fn populate_block_edges(&self, blocks: &mut Vec<lir::LirBasicBlock>) {
        let mut predecessors: HashMap<lir::BasicBlockId, Vec<lir::BasicBlockId>> = HashMap::new();

        for block in blocks.iter_mut() {
            let successors = Self::successors_from_terminator(&block.terminator);
            block.successors = successors.clone();
            for succ in successors {
                predecessors.entry(succ).or_default().push(block.id);
            }
        }

        for block in blocks.iter_mut() {
            if let Some(preds) = predecessors.remove(&block.id) {
                block.predecessors = preds;
            }
        }
    }

    pub(super) fn successors_from_terminator(
        terminator: &lir::LirTerminator,
    ) -> Vec<lir::BasicBlockId> {
        match terminator {
            lir::LirTerminator::Br(target) => vec![*target],
            lir::LirTerminator::CondBr {
                if_true, if_false, ..
            } => vec![*if_true, *if_false],
            lir::LirTerminator::Switch { default, cases, .. } => {
                let mut targets: Vec<lir::BasicBlockId> = cases.iter().map(|(_, bb)| *bb).collect();
                targets.push(*default);
                targets.sort_unstable();
                targets.dedup();
                targets
            }
            _ => Vec::new(),
        }
    }

    pub(super) fn transform_call_terminator(
        &mut self,
        func: &mir::Operand,
        args: &[mir::Operand],
        destination: &Option<(mir::Place, mir::BasicBlockId)>,
        cleanup: &Option<mir::BasicBlockId>,
        block: &mut lir::LirBasicBlock,
    ) -> Result<lir::LirTerminator> {
        // Every successful path below requires a destination (the cleanup/
        // invoke path enforces this itself, with a more specific message);
        // check it first so a malformed callee/args doesn't produce a
        // misleading error before this more fundamental one is reached.
        if destination.is_none() && cleanup.is_none() {
            return Err(crate::error::optimization_error(
                "MIR→LIR: call terminator without destination",
            ));
        }

        let mut function_value = self.transform_operand(func)?;
        block.instructions.extend(self.take_queued_instructions());

        function_value = self.normalize_callee_value(func, function_value)?;
        let callee_name = match &function_value.kind {
            lir::LirValueKind::Function(lir::LirFunctionRef::Name(name)) => {
                Some(name.as_str().to_owned())
            }
            lir::LirValueKind::Function(lir::LirFunctionRef::Package { name, .. }) => {
                Some(name.as_str().to_owned())
            }
            lir::LirValueKind::Function(lir::LirFunctionRef::Definition(_)) => None,
            _ => None,
        };
        let expected_params = callee_name
            .as_ref()
            .and_then(|name| self.function_signatures.get(name))
            .map(|sig| sig.params.clone());
        let signature_return = callee_name
            .as_ref()
            .and_then(|name| self.function_signatures.get(name))
            .map(|sig| sig.return_type.clone());
        let calling_convention = callee_name
            .as_ref()
            .and_then(|name| self.function_call_conventions.get(name))
            .cloned()
            .unwrap_or(lir::CallingConvention::C);

        let mut lowered_args = Vec::with_capacity(args.len());
        for (idx, arg) in args.iter().enumerate() {
            let expected_ty = expected_params.as_ref().and_then(|params| params.get(idx));
            let value = self.lower_call_argument(arg, expected_ty, block)?;
            lowered_args.push(value);
        }

        let call_id = self.next_id();
        let mut result_type = destination
            .as_ref()
            .and_then(|(place, _)| self.lookup_place_type(place))
            .map(|ty| self.lir_type_from_ty(&ty));
        if let Some(sig_ty) = signature_return.clone() {
            result_type = Some(sig_ty);
        }

        if cleanup.is_some() {
            let Some((_, dest_bb)) = destination.as_ref() else {
                return Err(fp_core::error::Error::from(
                    "invoke lowering requires a destination basic block",
                ));
            };
            let unwind_bb = cleanup.expect("invoke lowering requires a cleanup block");
            return Ok(lir::LirTerminator::Invoke {
                function: function_value,
                args: lowered_args,
                normal_dest: *dest_bb,
                unwind_dest: unwind_bb,
                calling_convention,
            });
        }

        block.instructions.push(lir::LirInstruction {
            id: call_id,
            kind: lir::LirInstructionKind::Call {
                function: function_value,
                args: lowered_args,
                calling_convention,
                tail_call: false,
            },
            result: result_type
                .clone()
                .filter(|ty| !matches!(ty, lir::LirType::Void))
                .map(|ty| lir::LirRegister { id: call_id, ty }),
            debug_info: None,
        });

        if let Some((dest_place, dest_bb)) = destination.as_ref() {
            if let Some(ref ty) = result_type {
                if matches!(
                    ty,
                    lir::LirType::Struct { .. }
                        | lir::LirType::Array(_, _)
                        | lir::LirType::Vector(_, _)
                ) {
                    let alignment = self.alignment_for_lir_type(ty);
                    let ptr = if let Some(storage) = self.local_storage.get(&dest_place.local) {
                        storage.ptr_value.clone()
                    } else {
                        let pointer_type = lir::LirType::Ptr(Box::new(ty.clone()));
                        let size_value =
                            lir::LirValue::constant(self.integer_constant(&lir::LirType::I32, 1)?);
                        let alloca_id = self.next_id();
                        block.instructions.push(lir::LirInstruction {
                            id: alloca_id,
                            kind: lir::LirInstructionKind::Alloca {
                                size: size_value,
                                alignment,
                            },
                            result: Some(lir::LirRegister {
                                id: alloca_id,
                                ty: pointer_type.clone(),
                            }),
                            debug_info: None,
                        });
                        let ptr_value = lir::LirValue::register(alloca_id, pointer_type);
                        self.local_storage.insert(
                            dest_place.local,
                            LocalStorage {
                                ptr_value: ptr_value.clone(),
                                element_type: ty.clone(),
                                alignment,
                            },
                        );
                        ptr_value
                    };
                    block.instructions.push(lir::LirInstruction {
                        id: self.next_id(),
                        kind: lir::LirInstructionKind::Store {
                            value: lir::LirValue::register(call_id, ty.clone()),
                            address: ptr,
                            alignment: Some(alignment),
                            volatile: false,
                        },
                        result: None,
                        debug_info: None,
                    });
                    self.register_map.remove(&dest_place.local);
                } else if !matches!(ty, lir::LirType::Void) {
                    self.register_map.insert(
                        dest_place.local,
                        lir::LirValue::register(call_id, ty.clone()),
                    );

                    if let Some(storage) = self.local_storage.get(&dest_place.local) {
                        let ptr = storage.ptr_value.clone();
                        let alignment = storage.alignment;
                        block.instructions.push(lir::LirInstruction {
                            id: self.next_id(),
                            kind: lir::LirInstructionKind::Store {
                                value: lir::LirValue::register(call_id, ty.clone()),
                                address: ptr,
                                alignment: Some(alignment),
                                volatile: false,
                            },
                            result: None,
                            debug_info: None,
                        });
                    }
                } else {
                    self.register_map.insert(
                        dest_place.local,
                        lir::LirValue::constant(lir::LirConstant::undef(ty.clone())),
                    );
                }
            } else {
                self.register_map.insert(
                    dest_place.local,
                    lir::LirValue::constant(lir::LirConstant::undef(lir::LirType::Void)),
                );
            }
            return Ok(lir::LirTerminator::Br(*dest_bb));
        }

        Err(crate::error::optimization_error(
            "MIR→LIR: call terminator without destination",
        ))
    }

    pub(super) fn normalize_callee_value(
        &mut self,
        func_operand: &mir::Operand,
        value: lir::LirValue,
    ) -> Result<lir::LirValue> {
        match &value.kind {
            lir::LirValueKind::Register(_) => {
                if let Some(place) = Self::operand_place(func_operand) {
                    if let Some(existing) = self.register_map.get(&place.local) {
                        if let lir::LirValueKind::Function(lir::LirFunctionRef::Package {
                            name,
                            ..
                        }) = &existing.kind
                        {
                            return self
                                .function_value(self.resolve_function_symbol(name.as_str()));
                        }
                    }
                }
                Ok(value)
            }
            lir::LirValueKind::Global(name) => {
                self.function_value(self.resolve_function_symbol(name.as_str()))
            }
            lir::LirValueKind::Function(lir::LirFunctionRef::Name(name)) => {
                self.function_value(self.resolve_function_symbol(name.as_str()))
            }
            lir::LirValueKind::Function(lir::LirFunctionRef::Package { name, .. }) => {
                self.function_value(self.resolve_function_symbol(name.as_str()))
            }
            lir::LirValueKind::Function(lir::LirFunctionRef::Definition(_)) => Ok(value),
            _ => Ok(value),
        }
    }

    pub(super) fn resolve_function_symbol(&self, name: &str) -> String {
        let logical = self
            .function_symbol_map
            .get(name)
            .cloned()
            .unwrap_or_else(|| name.to_string());

        if self.function_signatures.contains_key(&logical) {
            return logical;
        }

        if let Some(mapped) = (self.runtime_symbol_map)(&logical) {
            return mapped.as_str().to_string();
        }

        logical
    }

    pub(super) fn operand_place(operand: &mir::Operand) -> Option<&mir::Place> {
        match operand {
            mir::Operand::Move(place) | mir::Operand::Copy(place) => Some(place),
            _ => None,
        }
    }

    pub(super) fn prepare_return_value(
        &mut self,
        block: &mut lir::LirBasicBlock,
    ) -> Result<Option<lir::LirValue>> {
        let return_ty = self
            .current_return_type
            .clone()
            .ok_or_else(|| crate::error::optimization_error("MIR→LIR: no return type set"))?;
        if matches!(return_ty, lir::LirType::Void) {
            return Ok(None);
        }

        if let Some(local) = self.return_local {
            if let Some(storage) = self.local_storage.get(&local) {
                let ptr_value = storage.ptr_value.clone();
                let element_ty = storage.element_type.clone();
                let alignment = storage.alignment;

                let load_id = self.next_id();
                block.instructions.push(lir::LirInstruction {
                    id: load_id,
                    kind: lir::LirInstructionKind::Load {
                        address: ptr_value,
                        alignment: Some(alignment),
                        volatile: false,
                    },
                    result: Some(lir::LirRegister {
                        id: load_id,
                        ty: element_ty.clone(),
                    }),
                    debug_info: None,
                });

                if element_ty == return_ty {
                    return Ok(Some(lir::LirValue::register(load_id, element_ty)));
                } else if let Some(zero) = self.zero_value_for_lir_type(&return_ty) {
                    return Ok(Some(zero));
                } else {
                    return Err(crate::error::optimization_error(format!(
                        "MIR→LIR: return type mismatch — loaded {element_ty:?}, expected {return_ty:?}"
                    )));
                }
            }

            if let Some(value) = self.register_map.get(&local) {
                if value.ty == return_ty {
                    return Ok(Some(value.clone()));
                } else if let Some(zero) = self.zero_value_for_lir_type(&return_ty) {
                    return Ok(Some(zero));
                }
                if let Some(local_ty) = self
                    .local_types
                    .get(local as usize)
                    .map(|ty| self.lir_type_from_ty(ty))
                {
                    if local_ty == return_ty {
                        return Ok(Some(value.clone()));
                    }
                }
                return Err(crate::error::optimization_error(format!(
                    "MIR→LIR: return value type mismatch for local {local} — expected {return_ty:?}"
                )));
            }
        }

        Err(crate::error::optimization_error(
            "MIR→LIR: could not determine return value".to_string(),
        ))
    }

    pub(super) fn compute_block_order(&self, mir_body: &mir::Body) -> Vec<usize> {
        let mut order = Vec::new();
        let block_count = mir_body.basic_blocks.len();
        if block_count == 0 {
            return order;
        }

        let mut visited = vec![false; block_count];
        let mut queue = VecDeque::new();
        queue.push_back(0usize);
        visited[0] = true;

        while let Some(bb_idx) = queue.pop_front() {
            order.push(bb_idx);
            let successors = Self::mir_successors(&mir_body.basic_blocks[bb_idx]);
            for succ in successors {
                let succ_idx = succ as usize;
                if succ_idx < block_count && !visited[succ_idx] {
                    visited[succ_idx] = true;
                    queue.push_back(succ_idx);
                }
            }
        }

        // Append any unreachable blocks deterministically to maintain coverage
        for idx in 0..block_count {
            if !visited[idx] {
                order.push(idx);
            }
        }

        order
    }

    pub(super) fn mir_successors(bb: &mir::BasicBlockData) -> Vec<mir::BasicBlockId> {
        let mut successors = Vec::new();
        if let Some(terminator) = &bb.terminator {
            match &terminator.kind {
                mir::TerminatorKind::Goto { target } => successors.push(*target),
                mir::TerminatorKind::SwitchInt { targets, .. } => {
                    successors.extend(targets.targets.iter().copied());
                    successors.push(targets.otherwise);
                }
                mir::TerminatorKind::Call {
                    destination,
                    cleanup,
                    ..
                } => {
                    if let Some((_, dest_bb)) = destination {
                        successors.push(*dest_bb);
                    }
                    if let Some(cleanup_bb) = cleanup {
                        successors.push(*cleanup_bb);
                    }
                }
                mir::TerminatorKind::Drop { target, unwind, .. }
                | mir::TerminatorKind::DropAndReplace { target, unwind, .. } => {
                    successors.push(*target);
                    if let Some(unwind_bb) = unwind {
                        successors.push(*unwind_bb);
                    }
                }
                mir::TerminatorKind::Assert {
                    target, cleanup, ..
                } => {
                    successors.push(*target);
                    if let Some(cleanup_bb) = cleanup {
                        successors.push(*cleanup_bb);
                    }
                }
                mir::TerminatorKind::Yield { resume, drop, .. } => {
                    successors.push(*resume);
                    if let Some(drop_bb) = drop {
                        successors.push(*drop_bb);
                    }
                }
                mir::TerminatorKind::FalseEdge {
                    real_target,
                    imaginary_target,
                } => {
                    successors.push(*real_target);
                    successors.push(*imaginary_target);
                }
                mir::TerminatorKind::FalseUnwind {
                    real_target,
                    unwind,
                } => {
                    successors.push(*real_target);
                    if let Some(unwind_bb) = unwind {
                        successors.push(*unwind_bb);
                    }
                }
                mir::TerminatorKind::InlineAsm {
                    destination,
                    cleanup,
                    ..
                } => {
                    if let Some(dest_bb) = destination {
                        successors.push(*dest_bb);
                    }
                    if let Some(cleanup_bb) = cleanup {
                        successors.push(*cleanup_bb);
                    }
                }
                _ => {}
            }
        }

        successors.sort_unstable();
        successors.dedup();
        successors
    }

    pub(super) fn constant_from_aggregate(
        &self,
        kind: &mir::AggregateKind,
        constants: Vec<lir::LirConstant>,
        place_ty: &Ty,
    ) -> Option<lir::LirConstant> {
        match kind {
            mir::AggregateKind::Tuple => {
                // Only emit struct constants when the place itself is a tuple.
                // If the place type is non-aggregate, returning a struct constant
                // produces invalid LLVM IR like "struct i64 { i64 1 }".
                if !matches!(place_ty.kind, TyKind::Tuple(_)) {
                    return None;
                }
                let lir_ty = self.lir_type_from_ty(place_ty);
                Some(lir::LirConstant::aggregate(
                    lir_ty,
                    lir::LirConstantAggregate::Struct(constants),
                ))
            }
            mir::AggregateKind::Array(_elem_ty) => {
                if let TyKind::Array(_, len) = &place_ty.kind {
                    let lir_ty = self.lir_type_from_ty(place_ty);
                    let expected = self.array_length_from_const(len);
                    if expected != 0 && expected != constants.len() as u64 {
                        tracing::warn!(
                            "MIR→LIR: array constant length {} differs from {} elements",
                            expected,
                            constants.len()
                        );
                    }
                    Some(lir::LirConstant::aggregate(
                        lir_ty,
                        lir::LirConstantAggregate::Array(constants),
                    ))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    pub(super) fn lookup_place_type(&self, place: &mir::Place) -> Option<Ty> {
        let mut ty = self.local_types.get(place.local as usize)?.clone();
        for elem in &place.projection {
            match elem {
                mir::PlaceElem::Deref => match ty.kind {
                    TyKind::Ref(_, inner, _) => {
                        ty = (*inner).clone();
                    }
                    TyKind::RawPtr(TypeAndMut { ty: inner, .. }) => {
                        ty = (*inner).clone();
                    }
                    _ => {
                        return None;
                    }
                },
                mir::PlaceElem::Field(_, field_ty) => {
                    ty = field_ty.clone();
                }
                mir::PlaceElem::Index(_) => match &ty.kind {
                    TyKind::Array(elem, _) => {
                        ty = *elem.clone();
                    }
                    TyKind::Slice(elem) => {
                        ty = *elem.clone();
                    }
                    _ => {
                        return None;
                    }
                },
                mir::PlaceElem::ConstantIndex { .. } | mir::PlaceElem::Subslice { .. } => {
                    return None;
                }
                mir::PlaceElem::Downcast(_, _) => {}
            }
        }
        Some(ty)
    }

    pub(super) fn is_zero_sized(ty: &Ty) -> bool {
        super::type_helpers::is_zero_sized(ty)
    }

    pub(super) fn instantiate_ty(ty: &Ty, substs: &[mir::ty::GenericArg]) -> Ty {
        super::type_helpers::instantiate_ty(ty, substs)
    }

    pub(super) fn slice_ref_element_ty(ty: &Ty) -> Option<&Ty> {
        super::type_helpers::slice_ref_element_ty(ty)
    }

    pub(super) fn lower_binary_op(
        &self,
        bin_op: mir::BinOp,
        lhs: lir::LirValue,
        rhs: lir::LirValue,
    ) -> lir::LirInstructionKind {
        match bin_op {
            mir::BinOp::Add => lir::LirInstructionKind::Add(lhs, rhs),
            mir::BinOp::Sub => lir::LirInstructionKind::Sub(lhs, rhs),
            mir::BinOp::Mul => lir::LirInstructionKind::Mul(lhs, rhs),
            mir::BinOp::Div => lir::LirInstructionKind::Div(lhs, rhs),
            mir::BinOp::Rem => lir::LirInstructionKind::Rem(lhs, rhs),
            mir::BinOp::And => lir::LirInstructionKind::And(lhs, rhs),
            mir::BinOp::Or => lir::LirInstructionKind::Or(lhs, rhs),
            mir::BinOp::BitAnd => lir::LirInstructionKind::And(lhs, rhs),
            mir::BinOp::BitOr => lir::LirInstructionKind::Or(lhs, rhs),
            mir::BinOp::BitXor => lir::LirInstructionKind::Xor(lhs, rhs),
            mir::BinOp::Shl => lir::LirInstructionKind::Shl(lhs, rhs),
            mir::BinOp::Shr => lir::LirInstructionKind::Shr(lhs, rhs),
            mir::BinOp::Eq => lir::LirInstructionKind::Eq(lhs, rhs),
            mir::BinOp::Ne => lir::LirInstructionKind::Ne(lhs, rhs),
            mir::BinOp::Lt => lir::LirInstructionKind::Lt(lhs, rhs),
            mir::BinOp::Le => lir::LirInstructionKind::Le(lhs, rhs),
            mir::BinOp::Gt => lir::LirInstructionKind::Gt(lhs, rhs),
            mir::BinOp::Ge => lir::LirInstructionKind::Ge(lhs, rhs),
            _ => lir::LirInstructionKind::Unreachable,
        }
    }

    pub(super) fn lower_unary_op(
        &self,
        op: mir::UnOp,
        operand: lir::LirValue,
        result_ty: &lir::LirType,
    ) -> Result<lir::LirInstructionKind> {
        match op {
            mir::UnOp::Not => Ok(lir::LirInstructionKind::Not(operand)),
            mir::UnOp::Neg => {
                let Some(zero) = self.zero_value_for_lir_type(result_ty) else {
                    return Ok(lir::LirInstructionKind::Unreachable);
                };
                Ok(lir::LirInstructionKind::Sub(zero, operand))
            }
        }
    }

    pub(super) fn lower_cast(
        &self,
        cast_kind: mir::CastKind,
        source: lir::LirValue,
        target_ty: lir::LirType,
    ) -> lir::LirInstructionKind {
        let source_ty = source.ty.clone();
        match cast_kind {
            mir::CastKind::Misc => {
                let src_ty = source_ty;
                if matches!(src_ty, lir::LirType::Ptr(_)) && self.is_integral_type(&target_ty) {
                    return lir::LirInstructionKind::PtrToInt(source);
                }
                if self.is_integral_type(&src_ty) && matches!(target_ty, lir::LirType::Ptr(_)) {
                    return lir::LirInstructionKind::IntToPtr(source);
                }
                if self.is_integral_type(&src_ty) && self.is_integral_type(&target_ty) {
                    let src_w = self.type_bit_width(&src_ty);
                    let dst_w = self.type_bit_width(&target_ty);
                    if src_w == dst_w {
                        lir::LirInstructionKind::Bitcast(source, target_ty)
                    } else {
                        lir::LirInstructionKind::SextOrTrunc(source, target_ty)
                    }
                } else if self.is_float_type(&src_ty) && self.is_float_type(&target_ty) {
                    let src_w = self.type_bit_width(&src_ty);
                    let dst_w = self.type_bit_width(&target_ty);
                    match (src_w, dst_w) {
                        (Some(s), Some(d)) if d > s => {
                            lir::LirInstructionKind::FPExt(source, target_ty)
                        }
                        (Some(s), Some(d)) if d < s => {
                            lir::LirInstructionKind::FPTrunc(source, target_ty)
                        }
                        _ => lir::LirInstructionKind::Bitcast(source, target_ty),
                    }
                } else if self.is_float_type(&src_ty) && self.is_integral_type(&target_ty) {
                    lir::LirInstructionKind::FPToSI(source, target_ty)
                } else if self.is_integral_type(&src_ty) && self.is_float_type(&target_ty) {
                    lir::LirInstructionKind::SIToFP(source, target_ty)
                } else {
                    lir::LirInstructionKind::Bitcast(source, target_ty)
                }
            }
            mir::CastKind::Pointer(pointer_cast) => match pointer_cast {
                mir::PointerCast::ReifyFnPointer
                | mir::PointerCast::UnsafeFnPointer
                | mir::PointerCast::ClosureFnPointer
                | mir::PointerCast::MutToConstPointer
                | mir::PointerCast::ArrayToPointer
                | mir::PointerCast::Unsize => {
                    if matches!(&source_ty, lir::LirType::Ptr(_))
                        && self.is_integral_type(&target_ty)
                    {
                        lir::LirInstructionKind::PtrToInt(source)
                    } else if self.is_integral_type(&source_ty)
                        && matches!(target_ty, lir::LirType::Ptr(_))
                    {
                        lir::LirInstructionKind::IntToPtr(source)
                    } else {
                        lir::LirInstructionKind::Bitcast(source, target_ty)
                    }
                }
            },
        }
    }

    pub(super) fn switch_constant_for_value(
        &self,
        switch_ty: &Ty,
        value: u128,
        lir_ty: &lir::LirType,
    ) -> Result<lir::LirConstant> {
        let constant = match &switch_ty.kind {
            TyKind::Bool => {
                lir::LirConstant::integer(lir_ty.clone(), lir::LirInteger::I1(value != 0))
                    .map_err(|error| fp_core::error::Error::from(error.to_string()))?
            }
            TyKind::Uint(_) => self.unsigned_constant(lir_ty, value as u64)?,
            TyKind::Int(_) => self.integer_constant(lir_ty, value as i64)?,
            _ => {
                return Err(fp_core::error::Error::from(
                    "switch value is not integer-like",
                ));
            }
        };
        Ok(self.require_constant_type(constant, lir_ty)?)
    }
}
