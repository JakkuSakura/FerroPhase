use std::cell::RefCell;
use std::collections::{HashMap, HashSet};

use fp_core::ast::path::PathPrefix;
use fp_core::ast::{
    self, BlockStmt, BlockStmtExpr, Expr, ExprArray, ExprAssign, ExprBinOp, ExprBlock, ExprBreak,
    ExprCast, ExprClosure, ExprContinue, ExprFieldAccess, ExprFor, ExprIf, ExprIndex,
    ExprIntrinsicCall, ExprKwArg, ExprLet, ExprLoop, ExprMatch, ExprMatchCase, ExprReference,
    ExprReturn, ExprStringTemplate, ExprStruct, ExprTry, ExprTryCatch, ExprTuple, ExprUnOp,
    ExprWhile, ExprWith, FunctionParam, FunctionSignature, Ident, Item, ItemDeclFunction,
    ItemDefConst, ItemDefEnum, ItemDefFunction, ItemDefStruct, ItemKind, Name, Path, PathSegment,
    Pattern, PatternIdent, PatternKind, PatternStruct, PatternStructField, PatternTuple,
    PatternTupleStruct, PatternVariant, StmtLet, StructuralField, Ty, TypeArray, TypeEnum,
    TypeFunction, TypeReference, TypeSlice, TypeStruct, TypeTuple, Value,
};
use fp_core::error::Result;
use fp_core::hir;
use fp_core::hir::DefId;
use fp_core::intrinsics::{IntrinsicMaterializer, PortableOpCall};
use fp_core::ops::{BinOpKind, UnOpKind};
use fp_core::span::Span;

/// Converts operations in either direction: source expressions are first
/// recognized as the common `PortableOpCall`, then common calls are lowered
/// to target AST using target declarations.
pub struct PortableOpAstConverter {
    operations: fp_core::lang::LangItemRegistry,
}

impl PortableOpAstConverter {
    pub fn new(operations: fp_core::lang::LangItemRegistry) -> Self {
        Self { operations }
    }

    fn resolve_identity(&self, identity: &str) -> Option<fp_core::intrinsics::PortableOp> {
        self.operations.resolve(identity)
    }

    fn convert_from_source_path(
        &self,
        segments: &[&str],
    ) -> Option<fp_core::intrinsics::PortableOp> {
        self.operations.find_op_by_call_segments(segments)
    }

    pub fn convert(&self, call: PortableOpCall, expr_ty: &ast::TySlot) -> Option<Expr> {
        fp_core::tracing::info!(
            operation = call.op.name(),
            "looking up target operation mapping"
        );
        let binding =
            self.operations
                .resolve_operation(fp_core::lang::OperationSelector::PortableName(
                    call.op.name(),
                ))?;
        let path = binding.path.clone();
        fp_core::tracing::info!(
            operation = call.op.name(),
            target_path = %path,
            "mapping common portable operation to target AST"
        );
        if call.op.arity.receiver {
            let (receiver, args) = call.args.split_first()?;
            let field = path.segments().last()?.ident.clone();
            let node = Expr::new(ast::ExprKind::Invoke(ast::ExprInvoke {
                span: call.span,
                target: ast::ExprInvokeTarget::Method(ast::ExprFieldAccess {
                    obj: Box::new(receiver.clone()),
                    field,
                    generic_args: None,
                    span: call.span,
                }),
                args: args.to_vec(),
                kwargs: call.kwargs,
            }));
            Some(node)
        } else {
            let node = Expr::new(ast::ExprKind::Invoke(ast::ExprInvoke {
                span: call.span,
                target: ast::ExprInvokeTarget::Function(Name { qself: None, path }),
                args: call.args,
                kwargs: call.kwargs,
            }));
            Some(node)
        }
    }

    /// Recognize a target/source AST invocation as a declared portable
    /// operation. This is the inverse direction of `convert`: callers can
    /// feed the resulting common `PortableOpCall` into the other converter.
    pub fn convert_from_ast(&self, expr: &Expr, identity: &str) -> Option<PortableOpCall> {
        let ast::ExprKind::Invoke(invoke) = &expr.kind else {
            return None;
        };
        let (op, args) = match &invoke.target {
            ast::ExprInvokeTarget::Function(name) => {
                let op = self.operations.resolve(identity)?;
                (op, invoke.args.clone())
            }
            ast::ExprInvokeTarget::Method(select) => {
                let op = self.operations.resolve(identity)?;
                let mut args = Vec::with_capacity(invoke.args.len() + 1);
                args.push((*select.obj).clone());
                args.extend(invoke.args.clone());
                (op, args)
            }
            _ => return None,
        };
        Some(PortableOpCall {
            span: invoke.span,
            op,
            args,
            kwargs: invoke.kwargs.clone(),
        })
    }
}

/// Lifts a typechecked `hir::HirPackage` back into a plain item list — the
/// shape every backend serializer (Kotlin, Python, Go, ...) already knows
/// how to consume, so `PipelineMode::Transpile` can reuse those
/// serializers unchanged rather than each needing its own HIR-consuming path.
///
/// Carries the source `&hir::HirPackage` for package-local item traversal and
/// type facts, plus the workspace `HirProgram` for every `DefId` lookup. The
/// workspace is authoritative for declaration identity, including the current
/// package; this avoids a separate local-then-global resolution policy.
///
/// The package also provides the typer's
/// own resolved types directly — `expr_type`/`pat_type` naturally return
/// `None` for the two of three call sites that never run the typer at all
/// (`fp-backend`'s own roundtrip helpers), so there's no separate `Option`
/// to thread for that case anymore).
pub struct HirToAstLifter<'a> {
    package: &'a hir::HirPackage,
    /// Authoritative workspace-wide declaration metadata. Every resolved
    /// `DefId`, including one owned by `package`, is queried through here.
    hir_program: &'a hir::HirProgram,
    /// Controls whether lifting may recognize declaration-tagged portable
    /// operations. HIR always preserves ordinary calls; this is the single
    /// point where a target-facing AST consumes a portable operation through
    /// the configured materializer.
    capabilities: fp_core::capabilities::LanguageCapabilities,
    materializer: Option<std::sync::Arc<dyn IntrinsicMaterializer>>,
    /// Operation declarations from the destination language's standard
    /// library.  A hit is lowered to an ordinary target AST call; only a miss
    /// reaches the target materializer.
    target_converter: Option<std::sync::Arc<PortableOpAstConverter>>,
    source_converter: Option<std::sync::Arc<PortableOpAstConverter>>,
    /// The source standard-library operation declarations. HIR identity is
    /// authoritative, while this registry verifies the source declaration
    /// participated in the same attribute-driven mapping.
    /// Target-language (Kotlin, ...) lexical scopes currently open during a
    /// lift, one frame per emitted block — tracks which surface names have
    /// already been declared directly in that block (not nested ones),
    /// since unlike Rust, most target languages reject two `val`/`var`
    /// declarations of the same name in one block (`let x = 1; let x = 2;`
    /// is valid Rust shadowing, but `val x = 1; val x = 2` is a Kotlin
    /// "conflicting declarations" error). A function's own parameters seed
    /// its body's top-level frame (see `lift_function_item`), since Kotlin
    /// parameters and the body's own top-level `val`s share one scope too.
    scope_names: RefCell<Vec<HashSet<String>>>,
    /// `HirId` of a binding whose surface name collided with something
    /// already active in its target-language scope -> the fresh name it
    /// was given instead (same `HirId`-suffixing convention already used
    /// for synthetic loop-desugaring variables, e.g. `__fp_idx123`).
    /// Consulted by `lift_path` so later references to a renamed binding
    /// (resolved via `hir::Res::Local`) use the new name too.
    renamed_locals: RefCell<HashMap<hir::HirId, String>>,
}

impl<'a> HirToAstLifter<'a> {
    pub fn new(package: &'a hir::HirPackage, hir_program: &'a hir::HirProgram) -> Self {
        Self {
            package,
            hir_program,
            capabilities: fp_core::capabilities::LanguageCapabilities::NATIVE,
            materializer: None,
            target_converter: None,
            source_converter: None,
            scope_names: RefCell::new(Vec::new()),
            renamed_locals: RefCell::new(HashMap::new()),
        }
    }

    pub fn with_capabilities(
        mut self,
        capabilities: fp_core::capabilities::LanguageCapabilities,
    ) -> Self {
        self.capabilities = capabilities;
        self
    }

    pub fn with_materializer(
        mut self,
        materializer: std::sync::Arc<dyn IntrinsicMaterializer>,
    ) -> Self {
        self.materializer = Some(materializer);
        self
    }

    pub fn with_target_converter(
        mut self,
        converter: std::sync::Arc<PortableOpAstConverter>,
    ) -> Self {
        self.target_converter = Some(converter);
        self
    }

    pub fn with_target_operations(self, operations: fp_core::lang::LangItemRegistry) -> Self {
        self.with_target_converter(std::sync::Arc::new(PortableOpAstConverter::new(operations)))
    }

    pub fn with_source_operations(mut self, operations: fp_core::lang::LangItemRegistry) -> Self {
        self.source_converter = Some(std::sync::Arc::new(PortableOpAstConverter::new(operations)));
        self
    }

    fn materialize_portable_op(
        &self,
        span: Span,
        op: fp_core::intrinsics::PortableOp,
        args: Vec<Expr>,
        kwargs: Vec<ExprKwArg>,
        ty: &hir::HirId,
    ) -> Result<Expr> {
        let expr_ty = self
            .package
            .expr_type(ty.clone())
            .and_then(|ty| self.hir_ty_to_ast(&ty));
        let call = PortableOpCall {
            span,
            op,
            args,
            kwargs,
        };
        fp_core::tracing::info!(
            operation = call.op.name(),
            argument_count = call.args.len(),
            "source operation is available as common portable call"
        );
        // Source normalization has already produced this common operation;
        // convert it to target AST before invoking the fallback materializer.
        if let Some(converter) = &self.target_converter {
            if let Some(expr) = converter.convert(call.clone(), &expr_ty) {
                fp_core::tracing::info!(
                    operation = call.op.name(),
                    "target operation mapping succeeded"
                );
                return Ok(expr);
            }
            fp_core::tracing::info!(
                operation = call.op.name(),
                "target operation mapping unavailable; trying materializer fallback"
            );
        } else {
            fp_core::tracing::info!(
                operation = call.op.name(),
                "no target operation converter configured; trying materializer fallback"
            );
        }
        let Some(materializer) = &self.materializer else {
            return Err(fp_core::error::Error::from(
                "portable operation reached HIR-to-AST without a target materializer",
            ));
        };
        match materializer.materialize_portable_operation(call.clone(), &expr_ty)? {
            fp_core::intrinsics::MaterializeOutcome::Replaced(expr) => {
                fp_core::tracing::info!(
                    operation = call.op.name(),
                    "materialized portable operation as fallback"
                );
                Ok(expr)
            }
            fp_core::intrinsics::MaterializeOutcome::Unchanged => {
                fp_core::tracing::info!(
                    operation = call.op.name(),
                    "materializer left portable operation unchanged"
                );
                Err(fp_core::error::Error::from(
                    "target materializer did not handle portable operation",
                ))
            }
        }
    }

    fn portable_op_for_def(&self, def_id: &hir::DefId) -> Option<fp_core::intrinsics::PortableOp> {
        if !self.capabilities.portable_operations {
            return None;
        }
        let path = self.hir_program.source_path(def_id.clone())?;
        let segments = path
            .segments()
            .iter()
            .map(|segment| segment.as_str())
            .collect::<Vec<_>>();
        let operation = self
            .source_converter
            .as_ref()
            .and_then(|converter| converter.convert_from_source_path(&segments));
        if let Some(operation) = &operation {
            fp_core::tracing::info!(
                source_path = path.to_key(),
                operation = operation.name(),
                "mapping source declaration to common portable operation"
            );
        } else {
            fp_core::tracing::info!(
                source_path = path.to_key(),
                "source operation mapping not found"
            );
        }
        operation
    }

    fn portable_operations_enabled(&self) -> bool {
        self.capabilities.portable_operations
    }

    fn portable_op_for_path(&self, path: &hir::Path) -> Option<fp_core::intrinsics::PortableOp> {
        let hir::Res::Def(def_id) = path.res_ref() else {
            return None;
        };
        self.portable_op_for_def(def_id)
    }

    /// Resolve a declaration-tagged call through the definition's owning
    /// package.  The current package only owns local declarations; a call to
    /// `std::fs::read_to_string`, for example, carries std's `DefId` and its
    /// intrinsic declaration metadata lives in the published std snapshot.
    fn intrinsic_call_for_def(&self, def_id: &hir::DefId) -> Option<fp_core::intrinsics::CallKind> {
        self.hir_program.intrinsic_def(def_id.clone())
    }

    /// Declares `name` (for the binding identified by `hir_id`) in the
    /// innermost open scope, returning the name to actually emit — `name`
    /// itself if this is the first declaration of it in this scope, or a
    /// freshly suffixed alternative (recorded in `renamed_locals`) if it
    /// collides with one already declared here.
    fn declare_binding_name(&self, hir_id: hir::HirId, name: &str) -> String {
        let mut scopes = self.scope_names.borrow_mut();
        let frame = scopes
            .last_mut()
            .expect("declare_binding_name called outside any lifted block scope");
        if frame.insert(name.to_string()) {
            name.to_string()
        } else {
            let renamed = format!("{name}{}", hir_id.local_id.0);
            frame.insert(renamed.clone());
            self.renamed_locals
                .borrow_mut()
                .insert(hir_id, renamed.clone());
            renamed
        }
    }

    /// Lifts a typechecked `hir::HirPackage` back into a plain item list — the
    /// shape every backend serializer already knows how to consume.
    /// Strict: propagates the first per-item lift error rather than
    /// tolerating it (unlike the lenient, per-item-tolerant
    /// [`lift_items_by_def_id`](Self::lift_items_by_def_id) used by the real
    /// typed-splice pipeline) — appropriate for a correctness-oriented
    /// roundtrip/test, where a silently-dropped item would hide a real bug.
    pub fn lift_items(&self) -> Result<Vec<Item>> {
        if let [item] = self.package.items.as_slice() {
            if let hir::ItemKind::Query(_query) = &item.kind {
                // A whole program that's just one query document has no
                // items to lift for now.
                return Ok(Vec::new());
            }
        }
        let mut items = Vec::with_capacity(self.package.items.len());
        for item in &self.package.items {
            items.push(self.lift_item(item)?);
        }
        // Reconstruct closure expressions with typed params from lowered closure pairs
        self.reconstruct_closures(items)
    }

    /// Best-effort variant of [`lift_program`](Self::lift_program) for
    /// splicing typed content back onto an existing source AST
    /// (`fp-cli::compiler::typecheck_package`), keyed by each item's
    /// semantic `DefId` rather than list position.
    ///
    /// Unlike `lift_program`, a single item that fails to lift (e.g. a
    /// nested `hir::ExprKind::Query`, or any other not-yet-supported
    /// shape) is simply omitted from the result instead of aborting the
    /// whole program — the caller keeps that one item's original,
    /// untyped source form rather than losing typed info for every other
    /// item in the package. Items without a source-path entry (e.g.
    /// synthetic struct definitions for anonymous/structural literals,
    /// `register_structural_value_def`/`materialize_enum_struct_payload`
    /// in `ast_to_hir/mod.rs`) have no source counterpart to splice onto
    /// and are likewise omitted.
    pub fn lift_items_by_def_id(&self) -> HashMap<hir::DefId, Item> {
        let mut lifted = Vec::new();
        for item in &self.package.items {
            // Synthetic HIR definitions have no corresponding source item to
            // splice back into. Traits are no longer placeholders: lifting
            // their typed method signatures keeps interface suspension in
            // lockstep with implementations lowered from the same HIR.
            if self.package.placeholder_defs.contains(&item.def_id)
                && !matches!(item.kind, hir::ItemKind::Trait(_))
            {
                continue;
            }
            let Ok(ast_item) = self.lift_item(item) else {
                continue;
            };
            lifted.push((item.def_id.clone(), ast_item));
        }
        let (paths, items): (Vec<_>, Vec<_>) = lifted.into_iter().unzip();
        let items = self.reconstruct_closures(items.clone()).unwrap_or(items);
        paths.into_iter().zip(items).collect()
    }

    /// `lift_items_by_def_id` treats an `hir::ItemKind::Impl` as an opaque,
    /// un-lifted placeholder (real per-target impl-block emission happens
    /// downstream, per-backend) — so a typed-splice consumer keyed only by
    /// `lift_items_by_def_id`'s map never sees any impl *method*'s typed
    /// body at all, and permanently falls back to that method's original,
    /// untyped, pre-typecheck source form (confirmed: this is why
    /// `Ok(...)`/`Some(...)` calls inside impl methods never reached
    /// `KotlinMaterializer` — the typed HIR was real, but nothing ever
    /// spliced it back in). This method fills that gap:
    /// each `Method` inside every `impl` block, keyed by its own `DefId`.
    ///
    /// Per-method visibility isn't tracked on `hir::ImplItem` today (only
    /// the enclosing `hir::Item`/impl block carries a `Visibility`, and
    /// that's for the *impl*, not each method) — lifted methods are
    /// conservatively marked `Public`, matching how they're already
    /// reachable in practice (an impl method callable from outside its
    /// own module needs to already be public; a method that unexpectedly
    /// becomes visible here doesn't break anything Kotlin-side the way an
    /// incorrectly-hidden one would).
    pub fn lift_impl_methods_by_def_id(&self) -> HashMap<hir::DefId, Item> {
        let mut lifted = Vec::new();
        for item in &self.package.items {
            let hir::ItemKind::Impl(imp) = &item.kind else {
                continue;
            };
            for impl_item in &imp.items {
                let hir::ImplItemKind::Method(function) = &impl_item.kind else {
                    continue;
                };
                let synthetic_item = hir::Item {
                    hir_id: impl_item.hir_id.clone(),
                    def_id: impl_item.def_id.clone(),
                    visibility: hir::Visibility::Public,
                    kind: hir::ItemKind::Function(function.clone()),
                    span: item.span,
                };
                let Ok(ast_item) = self.lift_function_item(&synthetic_item, function) else {
                    continue;
                };
                lifted.push((impl_item.def_id.clone(), ast_item));
            }
        }
        let (paths, items): (Vec<_>, Vec<_>) = lifted.into_iter().unzip();
        let items = self.reconstruct_closures(items.clone()).unwrap_or(items);
        paths.into_iter().zip(items).collect()
    }

    /// For each item (keyed by its own qualified path, same key shape as
    /// [`lift_items_by_def_id`](Self::lift_items_by_def_id)), the source
    /// paths of every OTHER definition it references — used to compute
    /// which imports a target backend actually needs for spliced-in
    /// content, instead of only ever echoing whatever `use` items
    /// happened to already exist in the source file (`fp-kotlin`'s
    /// `emit_import`). Deliberately just facts (fully-qualified paths),
    /// not a target-specific "is this external" classification — that
    /// judgment belongs in each backend, not here.
    pub fn referenced_defs_by_def_id(&self) -> HashMap<hir::DefId, Vec<hir::DefId>> {
        let empty_tail_map = HashMap::new();
        let mut result = HashMap::new();
        for item in &self.package.items {
            let mut work = std::collections::VecDeque::new();
            crate::optimizer::hir::collect_item_refs(item, &empty_tail_map, &mut work);
            let referenced = work
                .into_iter()
                .filter(|def_id| *def_id != item.def_id)
                .filter(|def_id| self.package.def_map.contains_key(def_id))
                .collect::<Vec<_>>();
            result.insert(item.def_id.clone(), referenced);
        }
        result
    }

    pub fn referenced_source_paths(&self) -> HashMap<Vec<String>, Vec<Vec<String>>> {
        self.referenced_defs_by_def_id()
            .into_iter()
            .filter_map(|(def_id, refs)| {
                let key = self.source_path_for(&def_id)?.segments;
                let values = refs
                    .into_iter()
                    .filter_map(|ref_id| self.source_path_for(&ref_id).map(|path| path.segments))
                    .collect();
                Some((key, values))
            })
            .collect()
    }

    fn lift_item(&self, item: &hir::Item) -> Result<Item> {
        let lifted = match &item.kind {
            hir::ItemKind::Function(function) => self.lift_function_item(item, function)?,
            hir::ItemKind::Struct(def) => Item::from(ItemKind::DefStruct(ItemDefStruct {
                attrs: Vec::new(),
                visibility: lift_visibility(&item.visibility),
                name: Ident::new(def.name.as_str()),
                value: TypeStruct {
                    name: Ident::new(def.name.as_str()),
                    generics_params: Vec::new(),
                    repr: def.repr.clone(),
                    fields: def
                        .fields
                        .iter()
                        .map(|field| {
                            Ok(StructuralField::new(
                                Ident::new(field.name.as_str()),
                                self.lift_resolved_type(&field.ty)?,
                            ))
                        })
                        .collect::<Result<Vec<_>>>()?,
                },
            })),
            hir::ItemKind::Enum(def) => Item::from(ItemKind::DefEnum(ItemDefEnum {
                attrs: def.attrs.clone(),
                visibility: lift_visibility(&item.visibility),
                name: Ident::new(def.name.as_str()),
                value: TypeEnum {
                    name: Ident::new(def.name.as_str()),
                    generics_params: Vec::new(),
                    repr: def.repr.clone(),
                    variants: def
                        .variants
                        .iter()
                        .map(|variant| -> Result<ast::EnumTypeVariant> {
                            Ok(ast::EnumTypeVariant {
                                attrs: variant.attrs.clone(),
                                name: Ident::new(variant.name.as_str()),
                                value: variant
                                    .payload
                                    .as_ref()
                                    .map(|ty| self.lift_type(ty))
                                    .transpose()?
                                    .unwrap_or_else(Ty::unit),
                                discriminant: variant
                                    .discriminant
                                    .as_ref()
                                    .map(|expr| self.lift_expr(expr).map(Box::new))
                                    .transpose()?,
                            })
                        })
                        .collect::<Result<Vec<_>>>()?,
                },
            })),
            hir::ItemKind::TypeAlias(alias) => Item::from(ItemKind::DefType(ast::ItemDefType {
                attrs: Vec::new(),
                visibility: lift_visibility(&item.visibility),
                name: Ident::new(alias.name.as_str()),
                generics_params: Vec::new(),
                value: self.lift_type(&alias.target)?,
            })),
            hir::ItemKind::Const(def) => Item::from(ItemKind::DefConst(ItemDefConst {
                attrs: Vec::new(),
                mutable: None,
                ty_annotation: None,
                visibility: lift_visibility(&item.visibility),
                name: Ident::new(def.name.as_str()),
                ty: Some(self.lift_type(&def.ty)?),
                value: Box::new(self.lift_body_value(&def.body.value)?),
            })),
            hir::ItemKind::Impl(_) => Item::from(ItemKind::Expr(ast::Expr::unit())),
            hir::ItemKind::Trait(def) => self.lift_trait_item(item, def)?,
            hir::ItemKind::Query(query) => {
                return Err(fp_core::error::Error::Generic(eyre::eyre!(
                    "HIR->AST lifting for query item '{}' requires lift_program root handling",
                    query.ir.name.as_deref().unwrap_or("<query>")
                )));
            }
            hir::ItemKind::Expr(expr) => Item::from(ItemKind::Expr(self.lift_expr(expr)?)),
        };
        Ok(lifted.with_span(item.span))
    }

    fn lift_function_item(&self, item: &hir::Item, function: &hir::Function) -> Result<Item> {
        let mut sig = self.lift_signature(&function.sig)?;
        sig.is_const = function.is_const;
        if function.is_extern || function.body.is_none() {
            Ok(Item::from(ItemKind::DeclFunction(ItemDeclFunction {
                attrs: function.attrs.clone(),
                ty_annotation: None,
                name: Ident::new(function.sig.name.as_str()),
                sig,
                is_async: function.is_async,
            }))
            .with_span(item.span))
        } else {
            let block = function.body.as_ref().expect("checked body presence");
            // Parameters and the body's own top-level `val`s share one
            // scope in Kotlin and friends — seed the body block's scope
            // frame with them so a same-named top-level `let` gets renamed
            // instead of colliding (see `declare_binding_name`).
            let param_names: HashSet<String> = sig
                .params
                .iter()
                .map(|param| param.name.name.clone())
                .collect();
            // A parameter reassigned in the body (e.g. Rust's `&mut`/`mut`
            // parameter + `Option::take()`) can't be emitted as a direct
            // reassignment in Kotlin and friends — parameters are always an
            // implicit `val`. Give each such parameter a renamed mutable
            // local shadow instead (see `lift_function_body`).
            let reassigned_params: Vec<(hir::HirId, String)> = function
                .sig
                .inputs
                .iter()
                .filter_map(|param| match &param.pat.kind {
                    hir::PatKind::Binding { name, .. }
                        if block_assigns_local(block, param.pat.hir_id.clone()) =>
                    {
                        Some((param.pat.hir_id.clone(), name.as_str().to_string()))
                    }
                    _ => None,
                })
                .collect();
            Ok(Item::from(ItemKind::DefFunction(ItemDefFunction {
                ty_annotation: None,
                attrs: function.attrs.clone(),
                name: Ident::new(function.sig.name.as_str()),
                collected_items: Vec::new(),
                ty: Some(TypeFunction {
                    params: function
                        .sig
                        .inputs
                        .iter()
                        .map(|param| self.lift_type(&param.ty))
                        .collect::<Result<Vec<_>>>()?,
                    generics_params: Vec::new(),
                    ret_ty: Some(Box::new(self.lift_type(&function.sig.output)?)),
                }),
                sig,
                body: self.lift_function_body(block, param_names, &reassigned_params)?,
                is_async: function.is_async,
                visibility: lift_visibility(&item.visibility),
            }))
            .with_span(item.span))
        }
    }

    fn lift_trait_item(&self, item: &hir::Item, trait_def: &hir::Trait) -> Result<Item> {
        let trait_name = self
            .hir_program
            .source_path(item.def_id.clone())
            .and_then(|path| path.segments().last().cloned().map(hir::Symbol::new))
            .unwrap_or_else(|| hir::Symbol::new("Trait"));
        let mut items = Vec::new();
        for trait_item in &trait_def.items {
            let hir::TraitItemKind::Method(function) = &trait_item.kind else {
                continue;
            };
            let synthetic_item = hir::Item {
                hir_id: trait_item.hir_id.clone(),
                def_id: trait_item.def_id.clone(),
                visibility: item.visibility.clone(),
                kind: hir::ItemKind::Function(function.clone()),
                span: item.span,
            };
            items.push(self.lift_function_item(&synthetic_item, function)?);
        }
        Ok(Item::from(ItemKind::DefTrait(ast::ItemDefTrait {
            attrs: Vec::new(),
            name: Ident::new(trait_name.as_str()),
            generics_params: Vec::new(),
            bounds: ast::TypeBounds {
                bounds: trait_def
                    .supertraits
                    .iter()
                    .map(|bound| {
                        self.lift_path(bound)
                            .map(|path| Expr::name(Name::path(path)))
                    })
                    .collect::<Result<Vec<_>>>()?,
            },
            collected_items: Vec::new(),
            items,
            visibility: lift_visibility(&item.visibility),
        }))
        .with_span(item.span))
    }

    fn lift_signature(&self, sig: &hir::FunctionSig) -> Result<FunctionSignature> {
        // A method's `self` parameter has no dedicated HIR representation
        // of its own (see `make_self_param` on the AST→HIR side, which just
        // inserts an ordinary `Param` named `self` at index 0) — recover
        // `ast::FunctionSignature`'s separate `receiver` field from it here,
        // rather than leaving it `None` unconditionally, since backends
        // (`fp-kotlin`'s `collect_impl_methods`) key instance-vs-static
        // method classification directly off `receiver.is_none()`: losing
        // this turns every instance method into a mis-rendered "static"
        // one once real typed HIR→AST lifting is reached, rather than the
        // pre-typecheck AST fallback that never lost it.
        let is_self_param = |param: &hir::Param| matches!(&param.pat.kind, hir::PatKind::Binding { name, .. } if name.as_str() == "self");
        // HIR's `Param.ty` for a lowered `self` (`make_self_param`, on the
        // AST→HIR side) carries no `&` vs `&mut` distinction at all — both
        // wrap in the same `TypeExprKind::Ref`, with the actual mutability
        // only ever recorded on the pattern binding, which `make_self_param`
        // itself always hardcodes to `false` regardless of receiver kind.
        // That distinction is therefore unrecoverable here; every backend
        // that reads `receiver` today only checks `is_none()`/`is_some()`
        // (instance-vs-static classification), never Ref-vs-RefMut, so
        // collapsing to a plain `Ref`/`Value` split loses nothing any
        // current consumer observes.
        let (receiver, rest) = match sig.inputs.split_first() {
            Some((first, rest)) if is_self_param(first) => {
                let receiver = match &first.ty.kind {
                    hir::TypeExprKind::Ref(_) => ast::FunctionParamReceiver::Ref,
                    _ => ast::FunctionParamReceiver::Value,
                };
                (Some(receiver), rest)
            }
            _ => (None, sig.inputs.as_slice()),
        };
        Ok(FunctionSignature {
            name: Some(Ident::new(sig.name.as_str())),
            receiver,
            params: rest
                .iter()
                .enumerate()
                .map(|(index, param)| self.lift_param(param, index))
                .collect::<Result<Vec<_>>>()?,
            generics_params: Vec::new(),
            is_const: false,
            abi: lift_abi(&sig.abi),
            quote_kind: None,
            ret_ty: Some(self.lift_resolved_type(&sig.output)?),
        })
    }

    fn lift_param(&self, param: &hir::Param, index: usize) -> Result<FunctionParam> {
        let name = match &param.pat.kind {
            hir::PatKind::Binding { name, .. } => Ident::new(name.as_str()),
            _ => Ident::new(format!("arg{index}")),
        };
        Ok(FunctionParam {
            ty_annotation: None,
            name,
            ty: self.lift_resolved_type(&param.ty)?,
            is_const: false,
            is_context: param.is_context,
            default: param
                .default
                .as_ref()
                .map(|expr| Value::expr(self.lift_expr(expr).unwrap_or_else(|_| Expr::unit()))),
            as_tuple: false,
            as_dict: false,
            positional_only: false,
            keyword_only: false,
        })
    }

    fn lift_body_value(&self, expr: &hir::Expr) -> Result<Expr> {
        self.lift_expr(expr)
    }

    fn lift_expr(&self, expr: &hir::Expr) -> Result<Expr> {
        let lifted = match &expr.kind {
            hir::ExprKind::Literal(lit) => Expr::value(match lit {
                hir::Lit::Bool(v) => Value::bool(*v),
                hir::Lit::Integer(v) => Value::int(*v),
                hir::Lit::Float(v) => Value::decimal(*v),
                hir::Lit::Str(v) => Value::string(v.clone()),
                hir::Lit::Char(v) => Value::Char(ast::ValueChar::new(*v)),
                hir::Lit::Null => Value::null(),
                hir::Lit::Bytes(bytes) | hir::Lit::CStr(bytes) => {
                    Value::Bytes(ast::ValueBytes::from(bytes.as_slice()))
                }
            }),
            hir::ExprKind::Path(path) => {
                let portable_op = match path.res() {
                    hir::Res::Def(ref def_id) => self.portable_op_for_def(def_id),
                    _ => None,
                };
                if let Some(op) = portable_op {
                    self.materialize_portable_op(
                        expr.span,
                        op,
                        Vec::new(),
                        Vec::new(),
                        &expr.hir_id,
                    )?
                } else {
                    Expr::name(self.lift_qpath(path)?)
                }
            }
            hir::ExprKind::Query(_) => {
                return Err(fp_core::error::Error::from(
                    "HIR query expressions cannot be lifted back into AST expressions".to_string(),
                ));
            }
            hir::ExprKind::Binary(op, lhs, rhs) => Expr::new(ast::ExprKind::BinOp(ExprBinOp {
                span: expr.span,
                kind: lift_binop(op),
                lhs: Box::new(self.lift_expr(lhs)?),
                rhs: Box::new(self.lift_expr(rhs)?),
            })),
            hir::ExprKind::Unary(op, value) => Expr::new(ast::ExprKind::UnOp(ExprUnOp {
                span: expr.span,
                op: lift_unop(op),
                val: Box::new(self.lift_expr(value)?),
            })),
            hir::ExprKind::Reference(reference) => {
                Expr::new(ast::ExprKind::Reference(ExprReference {
                    span: expr.span,
                    referee: Box::new(self.lift_expr(&reference.expr)?),
                    mutable: Some(matches!(reference.mutable, hir::ty::Mutability::Mut)),
                    raw: reference.raw,
                }))
            }
            hir::ExprKind::Call(callee, args) => {
                let lifted_args = self.lift_positional_args(args)?;
                let lifted_kwargs = self.lift_keyword_args(args)?;
                // Type-relative calls are resolved by type checking, not by
                // the source path: `String::from_utf8_lossy` initially
                // resolves the path to `String`, while the selected impl
                // member has its own DefId.  The call-resolution table is
                // therefore authoritative for both dot calls and associated
                // calls.  Direct free-function paths simply have no entry.
                let resolved_callee = self.hir_program.method_resolution(expr.hir_id.clone());
                let portable_op = resolved_callee
                    .as_ref()
                    .and_then(|def_id| self.portable_op_for_def(def_id))
                    .or_else(|| match &callee.kind {
                        hir::ExprKind::Path(path) => match path.res() {
                            hir::Res::Def(ref def_id) => self.portable_op_for_def(def_id),
                            _ => None,
                        },
                        _ => None,
                    });
                if let Some(op) = portable_op {
                    self.materialize_portable_op(
                        expr.span,
                        op,
                        lifted_args,
                        lifted_kwargs,
                        &expr.hir_id,
                    )?
                } else if let Some(kind) = resolved_callee
                    .as_ref()
                    .and_then(|def_id| self.intrinsic_call_for_def(def_id))
                    .or_else(|| match &callee.kind {
                        hir::ExprKind::Path(path) => match path.res() {
                            hir::Res::Def(ref def_id) => self.intrinsic_call_for_def(def_id),
                            _ => None,
                        },
                        _ => None,
                    })
                {
                    Expr::new(ast::ExprKind::IntrinsicCall(ExprIntrinsicCall {
                        span: expr.span,
                        kind,
                        args: lifted_args,
                        kwargs: lifted_kwargs,
                    }))
                } else {
                    Expr::new(ast::ExprKind::Invoke(ast::ExprInvoke {
                        span: expr.span,
                        target: ast::ExprInvokeTarget::expr(self.lift_expr(callee)?),
                        args: lifted_args,
                        kwargs: lifted_kwargs,
                    }))
                }
            }
            hir::ExprKind::MethodCall(receiver, name, generic_args, args) => {
                let lifted_args = self.lift_positional_args(args)?;
                let lifted_kwargs = self.lift_keyword_args(args)?;
                let resolved_method = self.hir_program.method_resolution(expr.hir_id.clone());
                if let Some(op) = resolved_method
                    .as_ref()
                    .and_then(|def_id| self.portable_op_for_def(def_id))
                {
                    self.materialize_portable_op(
                        expr.span,
                        op,
                        std::iter::once(self.lift_expr(receiver)?)
                            .chain(lifted_args)
                            .collect(),
                        lifted_kwargs,
                        &expr.hir_id,
                    )?
                } else if let Some(kind) = resolved_method
                    .as_ref()
                    .and_then(|def_id| self.intrinsic_call_for_def(def_id))
                {
                    Expr::new(ast::ExprKind::IntrinsicCall(ExprIntrinsicCall {
                        span: expr.span,
                        kind,
                        args: std::iter::once(self.lift_expr(receiver)?)
                            .chain(lifted_args)
                            .collect(),
                        kwargs: lifted_kwargs,
                    }))
                } else {
                    Expr::new(ast::ExprKind::Invoke(ast::ExprInvoke {
                        span: expr.span,
                        target: ast::ExprInvokeTarget::Method(ExprFieldAccess {
                            span: expr.span,
                            obj: Box::new(self.lift_expr(receiver)?),
                            field: Ident::new(name.as_str()),
                            generic_args: generic_args
                                .as_ref()
                                .map(|args| self.lift_hir_generic_args(args))
                                .transpose()?,
                        }),
                        args: lifted_args,
                        kwargs: lifted_kwargs,
                    }))
                }
            }
            hir::ExprKind::FieldAccess(base, field) => {
                Expr::new(ast::ExprKind::FieldAccess(ExprFieldAccess {
                    span: expr.span,
                    obj: Box::new(self.lift_expr(base)?),
                    field: Ident::new(field.as_str()),
                    generic_args: None,
                }))
            }
            hir::ExprKind::Index(base, index) => Expr::new(ast::ExprKind::Index(ExprIndex {
                span: expr.span,
                obj: Box::new(self.lift_expr(base)?),
                index: Box::new(self.lift_expr(index)?),
            })),
            hir::ExprKind::Slice(slice) => {
                let range = Expr::new(ast::ExprKind::Range(ast::ExprRange {
                    span: expr.span,
                    start: slice
                        .start
                        .as_ref()
                        .map(|expr| self.lift_expr(expr.as_ref()).map(Box::new))
                        .transpose()?,
                    limit: if slice.inclusive {
                        ast::ExprRangeLimit::Inclusive
                    } else {
                        ast::ExprRangeLimit::Exclusive
                    },
                    end: slice
                        .end
                        .as_ref()
                        .map(|expr| self.lift_expr(expr.as_ref()).map(Box::new))
                        .transpose()?,
                    step: None,
                }));
                Expr::new(ast::ExprKind::Index(ExprIndex {
                    span: expr.span,
                    obj: Box::new(self.lift_expr(&slice.base)?),
                    index: Box::new(range),
                }))
            }
            hir::ExprKind::Cast(value, ty) => Expr::new(ast::ExprKind::Cast(ExprCast {
                span: expr.span,
                expr: Box::new(self.lift_expr(value)?),
                ty: self.lift_type(ty)?,
            })),
            hir::ExprKind::Struct(path, fields) => {
                let portable_op = match path.res() {
                    hir::Res::Def(ref def_id) => self.portable_op_for_def(def_id),
                    _ => None,
                };
                if let Some(op) = portable_op {
                    self.materialize_portable_op(
                        expr.span,
                        op,
                        fields
                            .iter()
                            .map(|field| self.lift_expr(&field.expr))
                            .collect::<Result<Vec<_>>>()?,
                        Vec::new(),
                        &expr.hir_id,
                    )?
                } else {
                    Expr::new(ast::ExprKind::Struct(ExprStruct {
                        span: expr.span,
                        name: Box::new(Expr::name(self.lift_qpath(path)?)),
                        fields: fields
                            .iter()
                            .map(|field| {
                                Ok(ast::ExprField::new(
                                    Ident::new(field.name.as_str()),
                                    self.lift_expr(&field.expr)?,
                                ))
                            })
                            .collect::<Result<Vec<_>>>()?,
                        update: None,
                    }))
                }
            }
            hir::ExprKind::If(cond, then_branch, else_branch) => {
                Expr::new(ast::ExprKind::If(ExprIf {
                    span: expr.span,
                    cond: Box::new(self.lift_expr(cond)?),
                    then: Box::new(self.lift_expr(then_branch)?),
                    elze: else_branch
                        .as_ref()
                        .map(|expr| self.lift_expr(expr).map(Box::new))
                        .transpose()?,
                }))
            }
            hir::ExprKind::Match(scrutinee, arms) => Expr::new(ast::ExprKind::Match(ExprMatch {
                span: expr.span,
                scrutinee: Some(Box::new(self.lift_expr(scrutinee)?)),
                cases: arms
                    .iter()
                    .map(|arm| {
                        Ok(ExprMatchCase {
                            span: arm.body.span,
                            pat: Some(Box::new(self.lift_pat(&arm.pat)?)),
                            // The scrutinee belongs to `ExprMatch`; an arm has only
                            // its pattern and optional guard.  Copying the scrutinee
                            // here made downstream passes treat every arm as a boolean
                            // condition and obscured the pattern's resolved identity.
                            cond: Box::new(Expr::unit()),
                            guard: arm
                                .guard
                                .as_ref()
                                .map(|expr| self.lift_expr(expr).map(Box::new))
                                .transpose()?,
                            body: Box::new(self.lift_expr(&arm.body)?),
                        })
                    })
                    .collect::<Result<Vec<_>>>()?,
            })),
            hir::ExprKind::Try(expr_try)
                if expr_try.catches.is_empty()
                    && expr_try.elze.is_none()
                    && expr_try.finally.is_none()
                    && self.portable_operations_enabled()
                    && self.is_standard_result_expr(&expr_try.expr) =>
            {
                let op = self
                    .source_converter
                    .as_ref()
                    .and_then(|converter| converter.resolve_identity("Result.propagate"))
                    .ok_or_else(|| {
                        fp_core::error::Error::from(
                            "source std does not declare Result.propagate for try expressions",
                        )
                    })?;
                self.materialize_portable_op(
                    expr.span,
                    op,
                    vec![self.lift_expr(&expr_try.expr)?],
                    Vec::new(),
                    &expr.hir_id,
                )?
            }
            hir::ExprKind::Try(expr_try) => Expr::new(ast::ExprKind::Try(ExprTry {
                span: expr.span,
                expr: Box::new(self.lift_expr(&expr_try.expr)?),
                catches: expr_try
                    .catches
                    .iter()
                    .map(|catch| {
                        Ok(ExprTryCatch {
                            span: catch.body.span,
                            pat: catch
                                .pat
                                .as_ref()
                                .map(|pat| self.lift_pat(pat).map(Box::new))
                                .transpose()?,
                            body: Box::new(self.lift_expr(&catch.body)?),
                        })
                    })
                    .collect::<Result<Vec<_>>>()?,
                elze: expr_try
                    .elze
                    .as_ref()
                    .map(|expr| self.lift_expr(expr).map(Box::new))
                    .transpose()?,
                finally: expr_try
                    .finally
                    .as_ref()
                    .map(|expr| self.lift_expr(expr).map(Box::new))
                    .transpose()?,
            })),
            hir::ExprKind::Block(block) => Expr::new(ast::ExprKind::Block(self.lift_block(block)?)),
            hir::ExprKind::IntrinsicCall(call) => {
                Expr::new(ast::ExprKind::IntrinsicCall(ExprIntrinsicCall {
                    span: expr.span,
                    kind: call.kind.clone(),
                    args: self.lift_positional_args(&call.callargs)?,
                    kwargs: self.lift_keyword_args(&call.callargs)?,
                }))
            }
            hir::ExprKind::FormatString(format) => {
                Expr::new(ast::ExprKind::FormatString(ExprStringTemplate {
                    parts: format
                        .parts
                        .iter()
                        .map(|part| match part {
                            hir::FormatTemplatePart::Literal(text) => {
                                ast::FormatTemplatePart::Literal(text.clone())
                            }
                            hir::FormatTemplatePart::Placeholder(placeholder) => {
                                ast::FormatTemplatePart::Placeholder(ast::FormatPlaceholder {
                                    arg_ref: match &placeholder.arg_ref {
                                        hir::FormatArgRef::Implicit => ast::FormatArgRef::Implicit,
                                        hir::FormatArgRef::Positional(index) => {
                                            ast::FormatArgRef::Positional(*index)
                                        }
                                        hir::FormatArgRef::Named(name) => {
                                            ast::FormatArgRef::Named(name.clone())
                                        }
                                    },
                                    format_spec: placeholder.format_spec.clone(),
                                })
                            }
                        })
                        .collect(),
                }))
            }
            hir::ExprKind::Let(pat, _ty, value) => Expr::new(ast::ExprKind::Let(ExprLet {
                span: expr.span,
                pat: Box::new(self.lift_pat(pat)?),
                expr: Box::new(
                    value
                        .as_deref()
                        .map(|value| self.lift_expr(value))
                        .transpose()?
                        .unwrap_or_else(Expr::unit),
                ),
            })),
            hir::ExprKind::Assign(lhs, rhs) => Expr::new(ast::ExprKind::Assign(ExprAssign {
                span: expr.span,
                target: Box::new(self.lift_expr(lhs)?),
                value: Box::new(self.lift_expr(rhs)?),
            })),
            hir::ExprKind::Return(value) => Expr::new(ast::ExprKind::Return(ExprReturn {
                span: expr.span,
                value: value
                    .as_ref()
                    .map(|expr| self.lift_expr(expr).map(Box::new))
                    .transpose()?,
            })),
            hir::ExprKind::Break(value) => Expr::new(ast::ExprKind::Break(ExprBreak {
                span: expr.span,
                value: value
                    .as_ref()
                    .map(|expr| self.lift_expr(expr).map(Box::new))
                    .transpose()?,
            })),
            hir::ExprKind::Continue => {
                Expr::new(ast::ExprKind::Continue(ExprContinue { span: expr.span }))
            }
            hir::ExprKind::Loop(block) => Expr::new(ast::ExprKind::Loop(ExprLoop {
                span: expr.span,
                label: None,
                body: Box::new(Expr::new(ast::ExprKind::Block(self.lift_block(block)?))),
            })),
            hir::ExprKind::While(cond, block) => Expr::new(ast::ExprKind::While(ExprWhile {
                span: expr.span,
                cond: Box::new(self.lift_expr(cond)?),
                body: Box::new(Expr::new(ast::ExprKind::Block(self.lift_block(block)?))),
            })),
            // Only ever produced for a target with
            // `LanguageCapabilities::first_class_for_loops` set — lifted
            // plainly, no special-case detection needed: `iter` recurses
            // as an ordinary expression (e.g. `list.iter().take(n)` lifts
            // to an ordinary method-call chain; the existing generic
            // `Op(Iter)` promotion already reduces `.iter()` itself to a
            // no-op passthrough, same as `.as_ref()`/`.to_owned()` — see
            // `kotlin_materializer.rs`), and the target's own serializer
            // renders its native `for`/`foreach` construct from whatever
            // `iter` expression results.
            hir::ExprKind::For(pat, iter, body) => Expr::new(ast::ExprKind::For(ExprFor {
                span: expr.span,
                pat: Box::new(self.lift_pat(pat)?),
                iter: Box::new(self.lift_expr(iter)?),
                body: Box::new(Expr::new(ast::ExprKind::Block(self.lift_block(body)?))),
            })),
            hir::ExprKind::With(context, body) => Expr::new(ast::ExprKind::With(ExprWith {
                span: expr.span,
                context: Box::new(self.lift_expr(context)?),
                body: Box::new(self.lift_expr(body)?),
            })),
            hir::ExprKind::Array(values) => Expr::new(ast::ExprKind::Array(ExprArray {
                span: expr.span,
                values: values
                    .iter()
                    .map(|v| self.lift_expr(v))
                    .collect::<Result<Vec<_>>>()?,
            })),
            hir::ExprKind::ArrayRepeat { elem, len } => {
                Expr::new(ast::ExprKind::ArrayRepeat(ast::ExprArrayRepeat {
                    span: expr.span,
                    elem: Box::new(self.lift_expr(elem)?),
                    len: Box::new(self.lift_expr(len)?),
                }))
            }
            hir::ExprKind::Tuple(values) => Expr::new(ast::ExprKind::Tuple(ExprTuple {
                span: expr.span,
                values: values
                    .iter()
                    .map(|v| self.lift_expr(v))
                    .collect::<Result<Vec<_>>>()?,
            })),
            hir::ExprKind::ConstBlock(const_block) => {
                Expr::new(ast::ExprKind::ConstBlock(ast::ExprConstBlock {
                    span: expr.span,
                    expr: Box::new(self.lift_expr(&const_block.body)?),
                }))
            }
            hir::ExprKind::Closure(closure) => {
                // Each param's own resolved type (if the typechecker
                // recorded one — `HirPackage::pat_type`, keyed by the
                // pattern's own `HirId`) gets promoted into a real
                // `PatternKind::Type` annotation here, since `lift_pat`
                // itself has no typeck access and a closure param is a
                // genuine annotation-shaped position (mirrors the `Local`
                // lifting fix for `let` bindings). This is what backends
                // needing per-parameter types (e.g. fp-kotlin's lambda
                // renderer) read via the ordinary `PatternKind::Type` case —
                // an unresolved/never-recorded param (no hint reached the
                // closure at typecheck time) simply keeps a bare pattern.
                let params = closure
                    .params
                    .iter()
                    .map(|param| {
                        let pattern = self.lift_pat(&param.pat)?;
                        if matches!(pattern.kind(), PatternKind::Type(_)) {
                            return Ok(pattern);
                        }
                        if let Some(ty) = self
                            .package
                            .pat_type(param.pat.hir_id.clone())
                            .and_then(|ty| self.hir_ty_to_ast(&ty))
                        {
                            Ok(Pattern::from(PatternKind::Type(ast::PatternType::new(
                                pattern, ty,
                            ))))
                        } else {
                            Ok(pattern)
                        }
                    })
                    .collect::<Result<Vec<_>>>()?;
                Expr::new(ast::ExprKind::Closure(ExprClosure {
                    span: expr.span,
                    params,
                    ret_ty: None,
                    movability: None,
                    body: Box::new(self.lift_expr(&closure.body)?),
                }))
            }
        };
        Ok(lifted.with_span(expr.span))
    }

    fn lift_block(&self, block: &hir::Block) -> Result<ExprBlock> {
        self.lift_block_with_scope(block, HashSet::new())
    }

    /// Like `lift_block`, but seeds the block's own target-language scope
    /// frame with `seed` before lifting its statements — used to seed a
    /// function body's top-level scope with its parameter names (see
    /// `lift_function_item`), since they share one scope in most targets.
    fn lift_block_with_scope(
        &self,
        block: &hir::Block,
        seed: HashSet<String>,
    ) -> Result<ExprBlock> {
        self.scope_names.borrow_mut().push(seed);
        let result = self.lift_block_stmts(block);
        self.scope_names.borrow_mut().pop();
        result
    }

    fn lift_block_stmts(&self, block: &hir::Block) -> Result<ExprBlock> {
        let mut stmts = Vec::with_capacity(block.stmts.len() + usize::from(block.expr.is_some()));
        for stmt in &block.stmts {
            stmts.push(self.lift_stmt(stmt)?);
        }
        if let Some(expr) = &block.expr {
            stmts.push(BlockStmt::Expr(
                BlockStmtExpr::new(self.lift_expr(expr)?).with_semicolon(false),
            ));
        }
        Ok(ExprBlock {
            span: Span::null(),
            stmts,
        })
    }

    /// Like `lift_block_with_scope`, but for a function body specifically:
    /// `reassigned_params` (computed by `lift_function_item` via
    /// `block_assigns_local`) names parameters the source body reassigns —
    /// impossible to emit directly in Kotlin and friends, since a parameter
    /// is always an implicit `val`. Each such parameter is given a renamed
    /// mutable local shadow (through the same `declare_binding_name` collision
    /// path a same-block shadowing `let` uses), declared as the very first
    /// statement of the lifted body; `lift_path`'s `renamed_locals` lookup
    /// then routes every later read/write of that parameter (matched by its
    /// pattern `HirId`) to the shadow instead of the parameter itself.
    fn lift_function_body(
        &self,
        block: &hir::Block,
        param_names: HashSet<String>,
        reassigned_params: &[(hir::HirId, String)],
    ) -> Result<ExprBlock> {
        self.scope_names.borrow_mut().push(param_names);
        let shadows: Vec<(String, String)> = reassigned_params
            .iter()
            .map(|(hir_id, name)| {
                (
                    self.declare_binding_name(hir_id.clone(), name),
                    name.clone(),
                )
            })
            .collect();
        let result = self.lift_block_stmts(block);
        self.scope_names.borrow_mut().pop();
        let mut lifted = result?;
        if !shadows.is_empty() {
            let mut stmts: Vec<BlockStmt> = shadows
                .into_iter()
                .map(|(shadow_name, original_name)| {
                    BlockStmt::Let(StmtLet {
                        pat: Pattern::new(PatternKind::Ident(PatternIdent {
                            ident: Ident::new(shadow_name),
                            mutability: Some(true),
                        })),
                        init: Some(Expr::ident(Ident::new(original_name))),
                        diverge: None,
                    })
                })
                .collect();
            stmts.append(&mut lifted.stmts);
            lifted.stmts = stmts;
        }
        Ok(lifted)
    }

    fn lift_stmt(&self, stmt: &hir::Stmt) -> Result<BlockStmt> {
        match &stmt.kind {
            hir::StmtKind::Local(local) => {
                let pat = self.lift_pat(&local.pat)?;
                // A simple `let x = ...` declares `x` in this block's own
                // target-language scope — rename it if that collides with
                // something already declared here (shadowing an outer
                // scope's binding is fine in Kotlin and friends; declaring
                // the same name twice in the *same* block isn't). Other
                // pattern shapes (destructuring) aren't renamed here; they
                // remain as-is, matching prior behavior.
                let pat = match pat.kind() {
                    PatternKind::Ident(ident_pat) => {
                        let resolved = self.declare_binding_name(
                            local.pat.hir_id.clone(),
                            ident_pat.ident.name.as_str(),
                        );
                        Pattern::new(PatternKind::Ident(PatternIdent {
                            ident: Ident::new(resolved),
                            mutability: ident_pat.mutability,
                        }))
                    }
                    _ => pat,
                };
                // An explicit annotation is a declaration boundary. Prefer
                // the type checker's resolved fact for that exact annotation,
                // because it carries the nominal DefId selected during path
                // resolution. Re-lifting the written `TypeExpr` here can
                // retain an alias or a stale source path and corrupt target
                // types (for example, a `Command` annotation becoming a
                // `Path`). Otherwise fall back to the resolved binding type
                // (`HirPackage::pat_type`, keyed by the pattern's `HirId`) —
                // needed both for bindings like
                // `let mut x = None;` whose real type is only known once
                // later reassignments/usage are unified, not from the
                // initializer expression alone, and for an annotation that
                // itself elides part of its shape (`let x: Vec<_> = ...;`) —
                // re-emitting a literal `_` as a backend type (e.g. Kotlin's
                // `MutableList<_>`) isn't valid target-language syntax, so an
                // annotation containing a hole must defer to the resolved
                // type the same way a missing annotation does. Without this,
                // backends (`fp-kotlin`) have to *guess* a var's type from the
                // literal `null` initializer alone and can't.
                let ty_ann = match &local.ty {
                    Some(ty) if !type_expr_contains_infer(ty) => Some(
                        self.package
                            .type_expr_type(ty.hir_id.clone())
                            .and_then(|ty| self.hir_ty_to_ast(&ty))
                            .unwrap_or(self.lift_type(ty)?),
                    ),
                    Some(_) => self
                        .package
                        .pat_type(local.pat.hir_id.clone())
                        .and_then(|ty| self.hir_ty_to_ast(&ty)),
                    _ => self
                        .package
                        .pat_type(local.pat.hir_id.clone())
                        .and_then(|ty| self.hir_ty_to_ast(&ty)),
                };
                // Kotlin destructuring declarations (`val (a, b) = ...`)
                // can't carry an explicit type annotation at all (the
                // component types are always inferred from `component1()`/
                // `component2()`) — only attach the resolved type for a
                // simple single-name binding.
                let pat = match (ty_ann, pat.kind()) {
                    (Some(ty), PatternKind::Ident(_)) => {
                        Pattern::new(PatternKind::Type(ast::PatternType::new(pat, ty)))
                    }
                    _ => pat,
                };
                Ok(BlockStmt::Let(ast::StmtLet {
                    pat,
                    init: local
                        .init
                        .as_ref()
                        .map(|expr| self.lift_expr(expr))
                        .transpose()?,
                    diverge: None,
                }))
            }
            hir::StmtKind::Item(item) => Ok(BlockStmt::Item(Box::new(self.lift_item(item)?))),
            hir::StmtKind::Expr(expr) => Ok(BlockStmt::Expr(
                BlockStmtExpr::new(self.lift_expr(expr)?).with_semicolon(false),
            )),
            hir::StmtKind::Semi(expr) => Ok(BlockStmt::Expr(
                BlockStmtExpr::new(self.lift_expr(expr)?).with_semicolon(true),
            )),
        }
    }

    fn lift_positional_args(&self, args: &[hir::CallArg]) -> Result<Vec<Expr>> {
        args.iter().map(|arg| self.lift_expr(&arg.value)).collect()
    }

    fn lift_keyword_args(&self, args: &[hir::CallArg]) -> Result<Vec<ExprKwArg>> {
        args.iter()
            .filter(|arg| !matches!(arg.name.as_str().strip_prefix("arg"), Some(suffix) if suffix.parse::<usize>().is_ok()))
            .map(|arg| {
                Ok(ExprKwArg {
                    name: arg.name.as_str().to_string(),
                    value: self.lift_expr(&arg.value)?,
                })
            })
            .collect()
    }

    fn lift_pat(&self, pat: &hir::Pat) -> Result<Pattern> {
        Ok(match &pat.kind {
            hir::PatKind::Wild => Pattern::new(PatternKind::Wildcard(ast::PatternWildcard {})),
            hir::PatKind::Binding { name, mutable } => {
                Pattern::new(PatternKind::Ident(PatternIdent {
                    ident: Ident::new(name.as_str()),
                    mutability: Some(*mutable),
                }))
            }
            hir::PatKind::Tuple(items) => Pattern::new(PatternKind::Tuple(PatternTuple {
                patterns: items
                    .iter()
                    .map(|p| self.lift_pat(p))
                    .collect::<Result<Vec<_>>>()?,
            })),
            hir::PatKind::TupleStruct(path, items) => {
                let mut lifted = Pattern::new(PatternKind::TupleStruct(PatternTupleStruct {
                    name: Name::path(self.lift_path(path)?),
                    patterns: items
                        .iter()
                        .map(|p| self.lift_pat(p))
                        .collect::<Result<Vec<_>>>()?,
                }));
                if let Some(op) = self.portable_op_for_path(path) {
                    lifted.set_resolved_op(op);
                }
                lifted
            }
            hir::PatKind::Struct(path, fields, has_rest) => {
                Pattern::new(PatternKind::Struct(PatternStruct {
                    name: Ident::new(
                        path.segments()
                            .last()
                            .map(|seg| seg.ident.as_str())
                            .unwrap_or("_"),
                    ),
                    fields: fields
                        .iter()
                        .map(|field| {
                            Ok(PatternStructField {
                                name: Ident::new(field.name.as_str()),
                                rename: Some(Box::new(self.lift_pat(&field.pat)?)),
                            })
                        })
                        .collect::<Result<Vec<_>>>()?,
                    has_rest: *has_rest,
                }))
            }
            hir::PatKind::Variant(path) => {
                let mut lifted = Pattern::new(PatternKind::Variant(PatternVariant {
                    name: Expr::path(self.lift_path(path)?),
                    pattern: None,
                }));
                if let Some(op) = self.portable_op_for_path(path) {
                    lifted.set_resolved_op(op);
                }
                lifted
            }
            hir::PatKind::Lit(lit) => Pattern::new(PatternKind::Variant(PatternVariant {
                name: Expr::value(match lit {
                    hir::Lit::Bool(v) => Value::bool(*v),
                    hir::Lit::Integer(v) => Value::int(*v),
                    hir::Lit::Float(v) => Value::decimal(*v),
                    hir::Lit::Str(v) => Value::string(v.clone()),
                    hir::Lit::Char(v) => Value::Char(ast::ValueChar::new(*v)),
                    hir::Lit::Null => Value::null(),
                    hir::Lit::Bytes(bytes) | hir::Lit::CStr(bytes) => {
                        Value::Bytes(ast::ValueBytes::from(bytes.as_slice()))
                    }
                }),
                pattern: None,
            })),
        })
    }

    fn lift_type(&self, ty: &hir::TypeExpr) -> Result<Ty> {
        Ok(match &ty.kind {
            hir::TypeExprKind::Primitive(primitive) => Ty::Primitive(*primitive),
            // A written type reference's generic arguments (`Vec<Hunk>`,
            // `Result<(), CoreError>`, ...) live on each `PathSegment`.
            // Preserve them structurally rather than flattening them into a
            // source string: flattening drops arguments that do not have a
            // printable name (notably `()`), silently changing generic
            // parameter order before a target backend can materialize it.
            hir::TypeExprKind::Path(path) => match self.inline_synthetic_struct_ty_qpath(path)? {
                Some(ty) => ty,
                None => Ty::expr(Expr::name(self.lift_qpath(path)?)),
            },
            hir::TypeExprKind::Projection(projection) => {
                Ty::Projection(Box::new(ast::TypeProjection {
                    self_ty: Box::new(self.lift_type(&projection.self_ty)?),
                    trait_ty: Box::new(Ty::path(self.lift_path(&projection.trait_path)?)),
                    assoc: Ident::new(projection.assoc.as_str()),
                }))
            }
            hir::TypeExprKind::Tuple(items) => Ty::Tuple(TypeTuple {
                types: items
                    .iter()
                    .map(|ty| self.lift_type(ty))
                    .collect::<Result<Vec<_>>>()?,
            }),
            hir::TypeExprKind::Array(elem, Some(len)) => Ty::Array(TypeArray {
                elem: Box::new(self.lift_type(elem)?),
                len: Box::new(self.lift_expr(len)?),
            }),
            hir::TypeExprKind::Array(elem, None) => Ty::Vec(ast::TypeVec {
                ty: Box::new(self.lift_type(elem)?),
            }),
            hir::TypeExprKind::Slice(elem) => Ty::Slice(TypeSlice {
                elem: Box::new(self.lift_type(elem)?),
            }),
            hir::TypeExprKind::Ref(inner) => Ty::Reference(TypeReference {
                ty: Box::new(self.lift_type(inner)?),
                mutability: None,
                lifetime: None,
            }),
            hir::TypeExprKind::Ptr { inner, mutable } => Ty::RawPtr(ast::TypeRawPtr {
                ty: Box::new(self.lift_type(inner)?),
                mutability: (*mutable).then_some(true),
            }),
            hir::TypeExprKind::FnPtr(function) => Ty::Function(TypeFunction {
                params: function
                    .inputs
                    .iter()
                    .map(|param| self.lift_type(param))
                    .collect::<Result<Vec<_>>>()?,
                generics_params: Vec::new(),
                ret_ty: Some(Box::new(self.lift_type(&function.output)?)),
            }),
            hir::TypeExprKind::Dynamic(bounds) => Ty::TypeBounds(ast::TypeBounds {
                bounds: bounds
                    .iter()
                    .map(|bound| {
                        self.lift_path(bound)
                            .map(|path| ast::Expr::name(ast::Name::path(path)))
                    })
                    .collect::<Result<Vec<_>>>()?,
            }),
            hir::TypeExprKind::Never => Ty::Nothing(ast::TypeNothing),
            hir::TypeExprKind::Infer | hir::TypeExprKind::Error => Ty::Unknown(ast::TypeUnknown),
            hir::TypeExprKind::Structural(structural) => Ty::Structural(ast::TypeStructural {
                fields: structural
                    .fields
                    .iter()
                    .map(|field| {
                        Ok(StructuralField::new(
                            Ident::new(field.name.as_str()),
                            self.lift_type(&field.ty)?,
                        ))
                    })
                    .collect::<Result<Vec<_>>>()?,
            }),
            hir::TypeExprKind::TypeBinaryOp(_) => Ty::Unknown(ast::TypeUnknown),
            hir::TypeExprKind::ConstBlock(_, body) => Ty::ConstBlock(ast::ExprConstBlock {
                span: ty.span,
                expr: Box::new(self.lift_expr(body)?),
            }),
            hir::TypeExprKind::Type => Ty::Type(ast::TypeType {
                span: ty.span,
                inner: None,
            }),
            hir::TypeExprKind::Any => Ty::Any(ast::TypeAny),
            hir::TypeExprKind::Refinement {
                base,
                binder,
                predicate,
            } => Ty::Refinement(Box::new(ast::TypeRefinement::new(
                self.lift_type(base)?,
                Ident::new(binder.as_str()),
                self.lift_expr(predicate)?,
            ))),
            hir::TypeExprKind::LiteralString(value) => Ty::Literal(ast::TypeLiteralString {
                value: value.clone(),
            }),
        })
    }

    /// Declaration-shaped types already retain resolved path identities in
    /// HIR. Preserve their written structure rather than substituting an
    /// inference result, which may describe an internal implementation type.
    fn lift_resolved_type(&self, ty: &hir::TypeExpr) -> Result<Ty> {
        self.lift_type(ty)
    }

    /// Source-shaped name (`"Vec<Hunk>"`) for a written type path if its
    /// last segment carries generic arguments — `None` if there are none
    /// (the caller falls back to `lift_path`'s plain-path behavior, which
    /// is already correct for a non-generic reference).
    /// A struct-shaped enum-variant payload or anonymous/structural
    /// literal gets a synthesized, source-less nominal struct
    /// (`register_structural_value_def`/`materialize_enum_struct_payload`
    /// in `ast_to_hir/mod.rs`) purely so it has a real `DefId` to carry
    /// through type-checking — nothing ever emits a standalone class for
    /// it (it has no source item to attach to via the qualified-path
    /// splice), so a plain by-name reference to it would dangle. Detected
    /// by its `__enum_payload_`/`__structural_value_` naming convention;
    /// inline its real fields directly (`Ty::Structural`, which
    /// `fp-kotlin`'s `emit_enum` already expands inline for a struct-
    /// shaped variant) instead of referencing it by a name nothing defines.
    fn inline_synthetic_struct_ty(&self, path: &hir::Path) -> Result<Option<Ty>> {
        let hir::Res::Def(def_id) = path.res_ref() else {
            return Ok(None);
        };
        let Some(item) = self.hir_program.item(def_id.clone()) else {
            return Ok(None);
        };
        let hir::ItemKind::Struct(def) = &item.kind else {
            return Ok(None);
        };
        let is_synthetic = def.name.as_str().starts_with("__enum_payload_")
            || def.name.as_str().starts_with("__structural_value_");
        if !is_synthetic {
            return Ok(None);
        }
        let fields = def
            .fields
            .iter()
            .map(|field| {
                Ok(StructuralField::new(
                    Ident::new(field.name.as_str()),
                    self.lift_type(&field.ty)?,
                ))
            })
            .collect::<Result<Vec<_>>>()?;
        Ok(Some(Ty::Structural(ast::TypeStructural { fields })))
    }

    fn inline_synthetic_struct_ty_qpath(&self, path: &hir::QPath) -> Result<Option<Ty>> {
        path.path()
            .map_or(Ok(None), |path| self.inline_synthetic_struct_ty(path))
    }

    fn lift_qpath(&self, path: &hir::QPath) -> Result<Name> {
        match path {
            hir::QPath::Resolved(qself, path) => Ok(Name {
                qself: qself
                    .as_ref()
                    .map(|ty| {
                        // HIR keeps the complete `Trait::Assoc` path in a
                        // resolved QPath. Recover the AST insertion point
                        // from the segment resolved as a trait; the segment
                        // immediately following it is the associated item.
                        let position = path
                            .segments
                            .iter()
                            .enumerate()
                            .find_map(|(index, segment)| {
                                let hir::Res::Def(def_id) = &segment.res else {
                                    return None;
                                };
                                self.hir_program
                                    .item(def_id.clone())
                                    .filter(|item| matches!(&item.kind, hir::ItemKind::Trait(_)))
                                    .map(|_| index + 1)
                            })
                            .unwrap_or(path.segments.len());
                        Ok::<ast::QSelf, fp_core::error::Error>(ast::QSelf {
                            ty: Box::new(self.lift_type(ty)?),
                            path_span: Span::null(),
                            position,
                        })
                    })
                    .transpose()?,
                path: self.lift_ast_path(path)?,
            }),
            hir::QPath::TypeRelative(receiver, segment) => {
                let mut associated = vec![segment.clone()];
                let mut base = receiver.as_ref();
                while let hir::TypeExprKind::Path(hir::QPath::TypeRelative(
                    nested_receiver,
                    nested_segment,
                )) = &base.kind
                {
                    associated.push(nested_segment.clone());
                    base = nested_receiver.as_ref();
                }
                associated.reverse();
                if let hir::TypeExprKind::Path(hir::QPath::Resolved(Some(qself), trait_path)) =
                    &base.kind
                {
                    let mut path = trait_path.segments.clone();
                    path.extend(associated);
                    // `QSelf::position` counts the trait-path segments only;
                    // `trait_path` may already contain one or more associated
                    // segments when this is a nested type-relative path.
                    let position = trait_path
                        .segments
                        .iter()
                        .enumerate()
                        .find_map(|(index, segment)| {
                            let hir::Res::Def(def_id) = &segment.res else {
                                return None;
                            };
                            self.hir_program
                                .item(def_id.clone())
                                .filter(|item| matches!(&item.kind, hir::ItemKind::Trait(_)))
                                .map(|_| index + 1)
                        })
                        .unwrap_or(trait_path.segments.len());
                    return Ok(Name {
                        qself: Some(ast::QSelf {
                            ty: Box::new(self.lift_type(qself)?),
                            path_span: Span::null(),
                            position,
                        }),
                        path: self.lift_ast_path(&hir::Path::with_span(
                            trait_path.span(),
                            trait_path.res.clone(),
                            path,
                        ))?,
                    });
                }
                Ok(Name {
                    qself: Some(ast::QSelf {
                        ty: Box::new(self.lift_type(base)?),
                        path_span: Span::null(),
                        position: 0,
                    }),
                    path: self.lift_ast_path(&hir::Path::new(segment.res.clone(), associated))?,
                })
            }
        }
    }

    fn lift_hir_generic_args(&self, args: &hir::GenericArgs) -> Result<ast::GenericArgs> {
        if matches!(
            args.parenthesized,
            hir::GenericArgsParentheses::ReturnTypeNotation
        ) {
            return Ok(ast::GenericArgs::ParenthesizedElided(args.span_ext));
        }
        if matches!(args.parenthesized, hir::GenericArgsParentheses::ParenSugar) {
            let (hir_inputs, hir_output) = args
                .paren_sugar_inputs_output()
                .ok_or_else(|| {
                    fp_core::error::Error::from(
                        "malformed parenthesized HIR generic arguments".to_owned(),
                    )
                })?;
            let inputs = hir_inputs
                .iter()
                .map(|input| self.lift_type(input))
                .collect::<Result<Vec<_>>>()?;
            let output = if matches!(
                &hir_output.kind,
                hir::TypeExprKind::Tuple(inputs) if inputs.is_empty()
            ) && hir_output.span() == args.span_ext
            {
                ast::FnRetTy::Default(Span::new(
                    args.span_ext.file,
                    args.span_ext.hi,
                    args.span_ext.hi,
                ))
            } else {
                ast::FnRetTy::Ty(Box::new(self.lift_type(hir_output)?))
            };
            let span = Span::union([args.span_ext, hir_output.span()]);
            return Ok(ast::GenericArgs::Parenthesized(ast::ParenthesizedArgs {
                span,
                inputs,
                inputs_span: args.span_ext,
                output,
            }));
        }

        let mut lifted = args
            .args
            .iter()
            .map(|arg| match arg {
                hir::GenericArg::Lifetime(lifetime) => Ok(ast::AngleBracketedArg::Arg(
                    ast::GenericArg::Lifetime(ast::Lifetime::from_name(
                        lifetime.as_str(),
                        lifetime.span(),
                    )),
                )),
                hir::GenericArg::Type(ty) => self
                    .lift_type(ty)
                    .map(|ty| ast::AngleBracketedArg::Arg(ast::GenericArg::Type(Box::new(ty)))),
                hir::GenericArg::Const(expr) => self.lift_expr(expr).map(|expr| {
                    ast::AngleBracketedArg::Arg(ast::GenericArg::Const(Box::new(expr)))
                }),
                hir::GenericArg::Infer(infer) => {
                    let arg = match infer.kind {
                        hir::InferArgKind::TypeOrConst => ast::GenericArg::Type(Box::new(
                            ast::Ty::Wildcard(fp_core::ast::TypeWildcard),
                        )),
                        hir::InferArgKind::Const => {
                            ast::GenericArg::Const(Box::new(ast::Expr::ident(Ident::new("_"))))
                        }
                    };
                    Ok(ast::AngleBracketedArg::Arg(arg))
                }
            })
            .collect::<Result<Vec<_>>>()?;
        for binding in &args.constraints {
            let gen_args = if binding.gen_args.args.is_empty()
                && binding.gen_args.constraints.is_empty()
                && matches!(
                    binding.gen_args.parenthesized,
                    hir::GenericArgsParentheses::No
                ) {
                None
            } else {
                Some(self.lift_hir_generic_args(&binding.gen_args)?)
            };
            lifted.push(match binding {
                hir::AssocItemConstraint {
                    ident,
                    kind: hir::AssocItemConstraintKind::Equality { term },
                    ..
                } => ast::AngleBracketedArg::Constraint(ast::AssocItemConstraint {
                    span: binding.span,
                    ident: Ident::new(ident.as_str()),
                    gen_args,
                    kind: ast::AssocItemConstraintKind::Equality {
                        term: match term {
                            hir::Term::Ty(ty) => ast::Term::Ty(Box::new(self.lift_type(ty)?)),
                            hir::Term::Const(expr) => {
                                ast::Term::Const(Box::new(self.lift_expr(expr)?))
                            }
                        },
                    },
                }),
                hir::AssocItemConstraint {
                    ident,
                    kind: hir::AssocItemConstraintKind::Bound { bounds },
                    ..
                } => ast::AngleBracketedArg::Constraint(ast::AssocItemConstraint {
                    span: binding.span,
                    ident: Ident::new(ident.as_str()),
                    gen_args,
                    kind: ast::AssocItemConstraintKind::Bound {
                        bounds: bounds
                            .iter()
                            .map(|bound| self.lift_type(bound))
                            .collect::<Result<Vec<_>>>()?,
                    },
                }),
            });
        }
        Ok(ast::GenericArgs::AngleBracketed(
            ast::AngleBracketedArgs {
                span: args.span_ext,
                args: lifted,
            },
        ))
    }

    fn lift_ast_path(&self, path: &hir::Path) -> Result<Path> {
        let segments = path
            .segments()
            .iter()
            .map(|segment| {
                let arguments = segment
                    .args
                    .as_ref()
                    .map(|args| self.lift_hir_generic_args(args))
                    .transpose()?;
                Ok(PathSegment::with_args(
                    Ident::new(segment.ident.as_str()),
                    arguments,
                ))
            })
            .collect::<Result<Vec<_>>>()?;
        Ok(Path::with_span(path.span(), PathPrefix::Plain, segments))
    }

    /// Converts a *resolved* (post-typecheck) HIR type — `fp_core::hir::ty::Ty`,
    /// a distinct, rustc-style representation from `hir::TypeExpr` (the
    /// source-shaped annotation `lift_type` above converts) — into an
    /// `ast::Ty`, so `lift_expr` can attach real inferred types instead of
    /// only ever carrying through source annotations. No existing code
    /// converts to this target type (the closest thing, `hir_to_mir`'s
    /// `lower_hir_ty`, targets `mir::ty::Ty`, a different `DefId`-keyed
    /// sibling — useful only as a shape reference for which `TyKind`
    /// variants exist). `DefId`-keyed variants (`Adt`/`FnDef`/`Closure`)
    /// resolve through `self.hir_program`; anything not resolvable
    /// there, or too exotic to matter for real code (`Dynamic`/
    /// `Generator`/`Projection`/etc.), falls back to `None` rather than a
    /// wrong guess — same principle as this file's existing `Infer`/`Error`
    /// → `Ty::Unknown` handling in `lift_type`, just returning `None`
    /// instead since the caller already treats "no real type" as the
    /// baseline case.
    fn hir_ty_to_ast(&self, ty: &hir::ty::Ty) -> Option<ast::Ty> {
        use fp_core::ast::{DecimalType, TypeInt, TypePrimitive};
        use hir::ty::TyKind;
        match &ty.kind {
            TyKind::Bool => Some(Ty::Primitive(TypePrimitive::Bool)),
            TyKind::Char => Some(Ty::Primitive(TypePrimitive::Char)),
            TyKind::Str => Some(Ty::Primitive(TypePrimitive::String)),
            TyKind::Int(int_ty) => Some(Ty::Primitive(TypePrimitive::Int(match int_ty {
                hir::ty::IntTy::I8 => TypeInt::I8,
                hir::ty::IntTy::I16 => TypeInt::I16,
                hir::ty::IntTy::I32 => TypeInt::I32,
                hir::ty::IntTy::I64 => TypeInt::I64,
                hir::ty::IntTy::I128 => TypeInt::I128,
                // No dedicated `isize` variant on `ast::TypeInt` — treat as `i64`,
                // matching how this codebase already treats `usize` (see below).
                hir::ty::IntTy::Isize => TypeInt::I64,
            }))),
            TyKind::Uint(uint_ty) => Some(Ty::Primitive(TypePrimitive::Int(match uint_ty {
                hir::ty::UintTy::U8 => TypeInt::U8,
                hir::ty::UintTy::U16 => TypeInt::U16,
                hir::ty::UintTy::U32 => TypeInt::U32,
                hir::ty::UintTy::U64 => TypeInt::U64,
                hir::ty::UintTy::U128 => TypeInt::U128,
                hir::ty::UintTy::Usize => TypeInt::U64,
            }))),
            TyKind::Float(float_ty) => {
                Some(Ty::Primitive(TypePrimitive::Decimal(match float_ty {
                    // `ast::DecimalType` has no narrower/wider variants than
                    // f32/f64, so f16/f128 are lossily folded into their
                    // nearest ast-representable width. This only affects
                    // codegen backends that go through the ast layer; HIR
                    // typechecking (the primary target for f16/f128 support)
                    // keeps the precise width via `hir::ty::FloatTy`.
                    hir::ty::FloatTy::F16 => DecimalType::F32,
                    hir::ty::FloatTy::F32 => DecimalType::F32,
                    hir::ty::FloatTy::F64 => DecimalType::F64,
                    hir::ty::FloatTy::F128 => DecimalType::F64,
                })))
            }
            TyKind::Never => Some(Ty::Nothing(ast::TypeNothing)),
            TyKind::Tuple(items) => {
                let types: Vec<Ty> = items.iter().filter_map(|t| self.hir_ty_to_ast(t)).collect();
                (types.len() == items.len()).then(|| Ty::Tuple(TypeTuple { types }))
            }
            TyKind::Slice(elem) => self.hir_ty_to_ast(elem).map(|elem| {
                Ty::Slice(TypeSlice {
                    elem: Box::new(elem),
                })
            }),
            // Array's const-generic length isn't carried here (no HIR expr
            // available from a resolved `HirTy` alone) — approximate as a
            // `Vec`, matching `lift_type`'s own treatment of a length-less array.
            TyKind::Array(elem, _len) => self
                .hir_ty_to_ast(elem)
                .map(|elem| Ty::Vec(ast::TypeVec { ty: Box::new(elem) })),
            TyKind::RawPtr(tm) => self.hir_ty_to_ast(&tm.ty).map(|elem| {
                Ty::RawPtr(ast::TypeRawPtr {
                    ty: Box::new(elem),
                    mutability: Some(matches!(tm.mutbl, hir::ty::Mutability::Mut)),
                })
            }),
            TyKind::Ref(_region, inner, mutability) => self.hir_ty_to_ast(inner).map(|inner| {
                Ty::Reference(TypeReference {
                    ty: Box::new(inner),
                    mutability: Some(matches!(mutability, hir::ty::Mutability::Mut)),
                    lifetime: None,
                })
            }),
            // A bare `def_id_to_ty` lookup (no generic args) is correct for
            // a plain, non-generic struct/enum reference. When `substs`
            // carries real type arguments (`Vec<Hunk>`, `Arc<GitBackend>`,
            // `Option<Foo>`, ...), dropping them here would let a struct
            // field or local variable's declared type lose its element/
            // wrapped type entirely (previously unnoticed since typed
            // content never flowed through this path for a real
            // multi-file package before). Render as a source-shaped name
            // (`"Vec<Hunk>"`) instead and let `kotlin_type_from_ty`'s
            // `Ty::Expr` case (`map_name_to_kt`) do the actual Kotlin
            // mapping — it already recognizes `Vec`/`Option`/`HashMap`/
            // `HashSet`/`Arc`/`Rc`/`Box`/etc. wrapper names and unwraps/
            // renders them correctly, so this reuses that instead of
            // duplicating it here.
            TyKind::Adt(adt, substs) => {
                let args: Vec<String> = substs
                    .iter()
                    .filter_map(|arg| match arg {
                        hir::ty::GenericArg::Type(t) => self.resolved_ty_source_name(t),
                        _ => None,
                    })
                    .collect();
                if args.is_empty() {
                    self.def_id_to_ty(&adt.did)
                } else {
                    let path = self.source_path_for(&adt.did)?;
                    let name = path.segments().last()?.as_str().to_owned();
                    Some(Ty::expr(Expr::name(Name::path(Path::plain(vec![
                        Ident::new(format!("{}<{}>", name, args.join(", "))),
                    ])))))
                }
            }
            TyKind::FnDef(def_id, _) | TyKind::Closure(def_id, _) | TyKind::Opaque(def_id, _) => {
                self.def_id_to_ty(def_id)
            }
            // Rare/not meaningfully resolvable without more context than a
            // bare `HirTy` carries: dyn trait objects, generators, associated-
            // type projections, generic params, higher-ranked bound/placeholder
            // types, unresolved inference vars, and already-errored types.
            TyKind::Dynamic(..)
            | TyKind::FnPtr(_)
            | TyKind::Generator(..)
            | TyKind::GeneratorWitness(_)
            | TyKind::Projection(_)
            | TyKind::Param(_)
            | TyKind::Bound(..)
            | TyKind::Placeholder(_)
            | TyKind::Infer(_)
            | TyKind::Error(_) => None,
            TyKind::Type => Some(Ty::Type(ast::TypeType {
                span: fp_core::span::Span::default(),
                inner: None,
            })),
            TyKind::Any => Some(Ty::Any(ast::TypeAny)),
        }
    }

    /// Renders a resolved (post-typecheck) `hir::ty::Ty` as a Rust-syntax
    /// shaped name (`"Vec<Hunk>"`, `"Option<GitBackend>"`, ...) — NOT a
    /// Kotlin name. Used only to embed into a `Ty::Expr` so
    /// `kotlin_type_from_ty`'s existing `map_name_to_kt`-based wrapper
    /// recognition (Vec/Option/HashMap/HashSet/Arc/Rc/Box/Result/...) can
    /// do the actual Kotlin rendering, instead of duplicating that table
    /// here. `None` for anything not nominally named (primitives are
    /// handled directly by the caller before ever reaching here).
    fn resolved_ty_source_name(&self, ty: &hir::ty::Ty) -> Option<String> {
        use hir::ty::TyKind;
        match &ty.kind {
            TyKind::Adt(adt, substs) => {
                let path = self.source_path_for(&adt.did)?;
                let name = path.segments().last()?.as_str().to_owned();
                let type_substs: Vec<&hir::ty::Ty> = substs
                    .iter()
                    .filter_map(|arg| match arg {
                        hir::ty::GenericArg::Type(t) => Some(t),
                        _ => None,
                    })
                    .collect();
                if type_substs.is_empty() {
                    Some(name.to_string())
                } else {
                    // A real generic argument that itself can't be
                    // resolved (an unresolved inference variable, or any
                    // other not-yet-handled `TyKind`) falls back to `Any`
                    // rather than being silently dropped — dropping it
                    // would shift the remaining args over (wrong for
                    // multi-param wrappers like `HashMap<K, V>`) and,
                    // when it's the only arg, collapse the whole
                    // reference to a bare, ungenericized (and always
                    // invalid) wrapper name.
                    let args: Vec<String> = type_substs
                        .iter()
                        .map(|t| {
                            self.resolved_ty_source_name(t)
                                .unwrap_or_else(|| "Any".to_string())
                        })
                        .collect();
                    Some(format!("{}<{}>", name, args.join(", ")))
                }
            }
            TyKind::Ref(_, inner, _) => self.resolved_ty_source_name(inner),
            TyKind::Str => Some("str".to_string()),
            TyKind::Slice(elem) => self
                .resolved_ty_source_name(elem)
                .map(|inner| format!("[{}]", inner)),
            TyKind::Bool => Some("bool".to_string()),
            TyKind::Char => Some("char".to_string()),
            TyKind::Int(_) | TyKind::Uint(_) => Some("i64".to_string()),
            TyKind::Float(_) => Some("f64".to_string()),
            // `dyn Trait` (as in `Arc<dyn GitTransport>`) — Kotlin has no
            // trait-object syntax of its own, but its interface types work
            // the same way at the use site, so the trait's own name is
            // the right Kotlin name too. Without this, `Arc<dyn Trait>`'s
            // inner arg silently resolves to nothing, `args` ends up
            // empty, and the caller falls back to bare, wrapper-only
            // "Arc" (dropping the trait name entirely).
            TyKind::Dynamic(predicates, _) => predicates.iter().find_map(|p| match p {
                hir::ty::ExistentialPredicate::Trait(trait_ref) => {
                    self.source_path_for(&trait_ref.def_id).and_then(|path| {
                        path.segments()
                            .last()
                            .map(|segment| segment.as_str().to_owned())
                    })
                }
                _ => None,
            }),
            _ => None,
        }
    }

    fn def_id_to_ty(&self, def_id: &DefId) -> Option<ast::Ty> {
        let path = self.source_path_for(def_id)?;
        if path.segments().is_empty() {
            return None;
        }
        Some(Ty::path(path.to_ast_path()))
    }

    /// The workspace owns authoritative `DefId` paths for both the current
    /// package and dependencies.
    fn source_path_for(&self, def_id: &DefId) -> Option<fp_core::ast::path::InPackagePath> {
        self.hir_program.source_path(def_id.clone())
    }

    /// `?` has several source-language implementations. Only Rust's
    /// standard `Result` gets Kotlin's `Result<T>` propagation convention;
    /// other try expressions retain their generic AST representation.
    fn is_standard_result_expr(&self, expr: &hir::Expr) -> bool {
        let Some(hir::ty::Ty {
            kind: hir::ty::TyKind::Adt(adt, _),
        }) = self.package.expr_type(expr.hir_id.clone())
        else {
            return false;
        };
        let Some(path) = self.source_path_for(&adt.did) else {
            return false;
        };
        path.segments()
            .iter()
            .map(|segment| segment.as_str())
            .eq(["core", "result", "Result"])
    }

    /// After HIR→AST lifting, closures have been lowered to `__Closure{N}`
    /// struct + `__closure{N}_call` function pairs. This pass detects those
    /// pairs, extracts the HIR-typed parameter info from the program, and
    /// reconstructs `ExprClosure` expressions with populated `Pattern.ty` slots.
    fn reconstruct_closures(&self, mut items: Vec<Item>) -> Result<Vec<Item>> {
        let mut closure_types: HashMap<String, Vec<Ty>> = HashMap::new();

        for hir_item in &self.package.items {
            if let hir::ItemKind::Function(func) = &hir_item.kind {
                let name = &func.sig.name;
                if let Some(rest) = name.strip_prefix("__closure") {
                    if let Some(num_end) = rest.find("_call") {
                        let num = &rest[..num_end];
                        let struct_name = format!("__Closure{}", num);
                        let param_types: Vec<Ty> = func
                            .sig
                            .inputs
                            .iter()
                            .skip(1) // skip closure env (self)
                            .map(|param| self.lift_type(&param.ty))
                            .collect::<Result<Vec<_>>>()?;
                        if !param_types.is_empty() {
                            closure_types.insert(struct_name, param_types);
                        }
                    }
                }
            }
        }

        if closure_types.is_empty() {
            return Ok(items);
        }

        for item in &mut items {
            recon_closures_in_item(item, &closure_types);
        }

        Ok(items)
    }

    /// Verbatim conversion from a HIR path to its AST equivalent — no
    /// resolution, no lookup, just copying each segment's own name across.
    /// The correct conversion for any path whose identity needs no further
    /// resolution (a local, a plain function/const/struct reference, ...).
    fn lift_path_verbatim(&self, path: &hir::Path) -> Result<Path> {
        self.lift_ast_path(path)
    }

    /// Lifts an `hir::Path` to an `ast::Path`, substituting a renamed
    /// local's new name (see `declare_binding_name`) for its original one
    /// when this path is a reference to it (`hir::Res::Local`) — a bare
    /// single-segment local variable reference is the only path shape
    /// `renamed_locals` ever has an entry for. A path resolving
    /// (`hir::Res::Def`) to a real enum variant is rewritten to its real
    /// declaring enum's name (via `HirProgram::find_hir_enum_for_variant`,
    /// a confirmed structural fact from the compiler's own name
    /// resolution) rather than trusting the source text's own segments,
    /// which may be module-qualified in ways Kotlin (flat-imports
    /// everything, no "module" object) has no equivalent for — a `Res::Def`
    /// that ISN'T a variant (a function/const/struct reference) falls
    /// through to the plain conversion below, which is simply correct for
    /// those, not a guess.
    fn lift_path(&self, path: &hir::Path) -> Result<Path> {
        if let hir::Res::Local(hir_id) = &path.res() {
            if let Some(renamed) = self.renamed_locals.borrow().get(hir_id) {
                return Ok(Path::plain(vec![Ident::new(renamed.clone())]));
            }
        }
        if let hir::Res::Def(def_id) = &path.res() {
            let enum_name = self
                .hir_program
                .member_owner(def_id.clone())
                .and_then(|owner| self.hir_program.item(owner))
                .and_then(|item| match item.kind {
                    hir::ItemKind::Enum(def) => Some(def.name.as_str().to_string()),
                    _ => None,
                });
            if let Some(enum_name) = enum_name {
                if let Some(variant_name) = path.segments().last() {
                    return Ok(Path::plain(vec![
                        Ident::new(enum_name),
                        Ident::new(variant_name.ident.as_str()),
                    ]));
                }
            }
        }
        self.lift_path_verbatim(path)
    }
}

/// True if `ty` elides part of its shape with `_` anywhere in its structure
/// (e.g. `Vec<_>`, `(i32, _)`) — such a hole is only ever meaningful to the
/// type *checker*; it isn't valid syntax to hand a backend serializer
/// verbatim, so a `Local`'s declared annotation containing one must defer to
/// the resolved type instead (see `lift_stmt`'s `hir::StmtKind::Local` arm).
fn type_expr_contains_infer(ty: &hir::TypeExpr) -> bool {
    match &ty.kind {
        hir::TypeExprKind::Infer => true,
        hir::TypeExprKind::Tuple(elems) => elems.iter().any(|e| type_expr_contains_infer(e)),
        hir::TypeExprKind::Array(elem, _) | hir::TypeExprKind::Slice(elem) => {
            type_expr_contains_infer(elem)
        }
        hir::TypeExprKind::Ptr { inner, .. } | hir::TypeExprKind::Ref(inner) => {
            type_expr_contains_infer(inner)
        }
        hir::TypeExprKind::Path(path) => path.segments().iter().any(|seg| {
            seg.args.as_ref().is_some_and(|args| {
                args.args.iter().any(|arg| match arg {
                    hir::GenericArg::Lifetime(_) => false,
                    hir::GenericArg::Type(t) => type_expr_contains_infer(t),
                    hir::GenericArg::Const(_) => false,
                    hir::GenericArg::Infer(_) => true,
                })
            })
        }),
        _ => false,
    }
}

/// True if `target` (some binding's pattern `HirId`) is ever the direct LHS
/// of an assignment anywhere within `block` — see `lift_function_body`.
/// Exhaustive over `hir::ExprKind` on purpose: HIR (unlike the full surface
/// AST) has few enough variants that missing one is unlikely, and a missed
/// variant only fails *open* (the parameter keeps its unfixed, pre-existing
/// "reassigning a val" codegen error) rather than silently emitting wrong
/// behavior.
fn block_assigns_local(block: &hir::Block, target: hir::HirId) -> bool {
    block
        .stmts
        .iter()
        .any(|stmt| stmt_assigns_local(stmt, target.clone()))
        || block
            .expr
            .as_deref()
            .is_some_and(|expr| expr_assigns_local(expr, target))
}

fn stmt_assigns_local(stmt: &hir::Stmt, target: hir::HirId) -> bool {
    match &stmt.kind {
        hir::StmtKind::Local(local) => local
            .init
            .as_ref()
            .is_some_and(|expr| expr_assigns_local(expr, target)),
        hir::StmtKind::Item(_) => false,
        hir::StmtKind::Expr(expr) | hir::StmtKind::Semi(expr) => expr_assigns_local(expr, target),
    }
}

fn expr_assigns_local(expr: &hir::Expr, target: hir::HirId) -> bool {
    match &expr.kind {
        hir::ExprKind::Assign(lhs, rhs) => {
            let assigns_target = matches!(
                &lhs.kind,
                hir::ExprKind::Path(path) if matches!(path.res_ref(), hir::Res::Local(id) if *id == target)
            );
            assigns_target
                || expr_assigns_local(lhs, target.clone())
                || expr_assigns_local(rhs, target)
        }
        hir::ExprKind::Literal(_)
        | hir::ExprKind::Path(_)
        | hir::ExprKind::Continue
        | hir::ExprKind::FormatString(_)
        | hir::ExprKind::Query(_) => false,
        hir::ExprKind::Binary(_, lhs, rhs) => {
            expr_assigns_local(lhs, target.clone()) || expr_assigns_local(rhs, target)
        }
        hir::ExprKind::Unary(_, inner) => expr_assigns_local(inner, target),
        hir::ExprKind::Reference(r) => expr_assigns_local(&r.expr, target),
        hir::ExprKind::Call(callee, args) => {
            expr_assigns_local(callee, target.clone())
                || args
                    .iter()
                    .any(|arg| expr_assigns_local(&arg.value, target.clone()))
        }
        hir::ExprKind::MethodCall(recv, method, _, args) => {
            // `Option::take()` has no real HIR-level assignment node — its
            // "reset the receiver to `None`" half is only synthesized as
            // text by the Kotlin serializer's own `.take()` special case
            // (`run { val __t = recv; recv = null; __t }`). A parameter
            // receiver needs the same shadow-rename treatment an explicit
            // `Assign` does, or that synthesized reassignment targets an
            // unassignable Kotlin `val` parameter.
            let resets_target = method.as_str() == "take"
                && args.is_empty()
                && matches!(
                    &recv.kind,
                    hir::ExprKind::Path(path) if matches!(path.res_ref(), hir::Res::Local(id) if *id == target)
                );
            resets_target
                || expr_assigns_local(recv, target.clone())
                || args
                    .iter()
                    .any(|arg| expr_assigns_local(&arg.value, target.clone()))
        }
        hir::ExprKind::FieldAccess(inner, _) => expr_assigns_local(inner, target),
        hir::ExprKind::Index(base, index) => {
            expr_assigns_local(base, target.clone()) || expr_assigns_local(index, target)
        }
        hir::ExprKind::Slice(s) => {
            expr_assigns_local(&s.base, target.clone())
                || s.start
                    .as_deref()
                    .is_some_and(|e| expr_assigns_local(e, target.clone()))
                || s.end
                    .as_deref()
                    .is_some_and(|e| expr_assigns_local(e, target))
        }
        hir::ExprKind::Cast(inner, _) => expr_assigns_local(inner, target),
        hir::ExprKind::Struct(_, fields) => fields
            .iter()
            .any(|field| expr_assigns_local(&field.expr, target.clone())),
        hir::ExprKind::If(cond, then_expr, else_expr) => {
            expr_assigns_local(cond, target.clone())
                || expr_assigns_local(then_expr, target.clone())
                || else_expr
                    .as_deref()
                    .is_some_and(|e| expr_assigns_local(e, target))
        }
        hir::ExprKind::Match(scrutinee, arms) => {
            expr_assigns_local(scrutinee, target.clone())
                || arms.iter().any(|arm| {
                    arm.guard
                        .as_ref()
                        .is_some_and(|g| expr_assigns_local(g, target.clone()))
                        || expr_assigns_local(&arm.body, target.clone())
                })
        }
        hir::ExprKind::Try(t) => {
            expr_assigns_local(&t.expr, target.clone())
                || t.catches
                    .iter()
                    .any(|c| expr_assigns_local(&c.body, target.clone()))
                || t.elze
                    .as_deref()
                    .is_some_and(|e| expr_assigns_local(e, target.clone()))
                || t.finally
                    .as_deref()
                    .is_some_and(|e| expr_assigns_local(e, target))
        }
        hir::ExprKind::Block(b) => block_assigns_local(b, target),
        hir::ExprKind::IntrinsicCall(ic) => ic
            .callargs
            .iter()
            .any(|arg| expr_assigns_local(&arg.value, target.clone())),
        hir::ExprKind::Let(_, _, init) => init
            .as_deref()
            .is_some_and(|e| expr_assigns_local(e, target)),
        hir::ExprKind::Return(e) | hir::ExprKind::Break(e) => {
            e.as_deref().is_some_and(|e| expr_assigns_local(e, target))
        }
        hir::ExprKind::Loop(b) => block_assigns_local(b, target),
        hir::ExprKind::While(cond, b) => {
            expr_assigns_local(cond, target.clone()) || block_assigns_local(b, target)
        }
        hir::ExprKind::For(_pat, iter, b) => {
            expr_assigns_local(iter, target.clone()) || block_assigns_local(b, target)
        }
        hir::ExprKind::With(a, b) => {
            expr_assigns_local(a, target.clone()) || expr_assigns_local(b, target)
        }
        hir::ExprKind::Array(items) | hir::ExprKind::Tuple(items) => {
            items.iter().any(|e| expr_assigns_local(e, target.clone()))
        }
        hir::ExprKind::ArrayRepeat { elem, len } => {
            expr_assigns_local(elem, target.clone()) || expr_assigns_local(len, target)
        }
        hir::ExprKind::ConstBlock(cb) => expr_assigns_local(&cb.body, target),
        hir::ExprKind::Closure(closure) => expr_assigns_local(&closure.body, target),
    }
}

fn lift_visibility(vis: &hir::Visibility) -> ast::Visibility {
    match vis {
        hir::Visibility::Public => ast::Visibility::Public,
        hir::Visibility::Private => ast::Visibility::Private,
    }
}

fn lift_abi(abi: &hir::Abi) -> ast::Abi {
    match abi {
        hir::Abi::Rust => ast::Abi::Rust,
        hir::Abi::C { .. } => ast::Abi::Named("C".to_string()),
        hir::Abi::Named(name) => ast::Abi::Named(name.clone()),
        other => ast::Abi::Named(format!("{other:?}").to_ascii_lowercase()),
    }
}

fn lift_binop(op: &hir::BinOp) -> BinOpKind {
    match op {
        hir::BinOp::Add => BinOpKind::Add,
        hir::BinOp::Sub => BinOpKind::Sub,
        hir::BinOp::Mul => BinOpKind::Mul,
        hir::BinOp::Div => BinOpKind::Div,
        hir::BinOp::Rem => BinOpKind::Mod,
        hir::BinOp::And => BinOpKind::And,
        hir::BinOp::Or => BinOpKind::Or,
        hir::BinOp::BitXor => BinOpKind::BitXor,
        hir::BinOp::BitAnd => BinOpKind::BitAnd,
        hir::BinOp::BitOr => BinOpKind::BitOr,
        hir::BinOp::Shl => BinOpKind::Shl,
        hir::BinOp::Shr => BinOpKind::Shr,
        hir::BinOp::Eq => BinOpKind::Eq,
        hir::BinOp::Ne => BinOpKind::Ne,
        hir::BinOp::Lt => BinOpKind::Lt,
        hir::BinOp::Le => BinOpKind::Le,
        hir::BinOp::Gt => BinOpKind::Gt,
        hir::BinOp::Ge => BinOpKind::Ge,
    }
}

fn lift_unop(op: &hir::UnOp) -> UnOpKind {
    match op {
        hir::UnOp::Not => UnOpKind::Not,
        hir::UnOp::Neg => UnOpKind::Neg,
        hir::UnOp::Deref => UnOpKind::Deref,
        hir::UnOp::Box => UnOpKind::Any(Ident::new("box")),
    }
}

// ── Closure reconstruction (post-lift AST-to-AST pass) ──────────────────────
// Operates purely on the already-lifted `ast` tree plus the closure-type
// map built above — never touches `hir`/`typeck`, so these stay free
// functions rather than becoming `HirToAstLifter` methods.

fn recon_closures_in_item(item: &mut Item, types: &HashMap<String, Vec<Ty>>) {
    match item.kind_mut() {
        ItemKind::DefFunction(f) => {
            for stmt in &mut f.body.stmts {
                recon_closures_in_stmt(stmt, types);
            }
        }
        ItemKind::DefConst(c) => {
            recon_closures_in_expr(&mut c.value, types);
        }
        ItemKind::Module(m) => {
            for child in &mut m.items {
                recon_closures_in_item(child, types);
            }
        }
        ItemKind::Impl(impl_) => {
            for child in &mut impl_.items {
                recon_closures_in_item(child, types);
            }
        }
        ItemKind::Expr(e) => {
            if let ast::ExprKind::Block(block) = e.kind_mut() {
                for stmt in &mut block.stmts {
                    recon_closures_in_stmt(stmt, types);
                }
            } else {
                recon_closures_in_expr(e, types);
            }
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::rc::Rc;
    use std::sync::Arc;

    struct TestMaterializer;

    fn test_operation(name: &str) -> fp_core::intrinsics::PortableOp {
        let mut registry = fp_core::lang::LangItemRegistry::default();
        registry.insert_op(name, ast::Path::plain(vec![ast::Ident::new(name)]));
        registry.resolve(name).expect("test operation declaration")
    }

    #[test]
    fn portable_op_converter_emits_target_ast_from_declared_path() {
        let op = test_operation("fs_read");
        let mut operations = fp_core::lang::LangItemRegistry::default();
        operations.insert_op(
            op.name(),
            ast::Path::plain(vec![
                ast::Ident::new("std"),
                ast::Ident::new("fs"),
                ast::Ident::new("read"),
            ]),
        );
        let converter = PortableOpAstConverter::new(operations);
        let expr = converter
            .convert(
                PortableOpCall {
                    span: Span::null(),
                    op,
                    args: vec![Expr::name(Name::ident("path"))],
                    kwargs: Vec::new(),
                },
                &None,
            )
            .expect("declared operation path");
        let ast::ExprKind::Invoke(invoke) = expr.kind else {
            panic!("expected an AST invoke");
        };
        let ast::ExprInvokeTarget::Function(name) = invoke.target else {
            panic!("expected a target function");
        };
        assert_eq!(name.to_string(), "std::fs::read");
    }

    #[test]
    fn portable_op_converter_emits_method_ast_from_declared_operation() {
        let op = test_operation("option_unwrap");
        let mut operations = fp_core::lang::LangItemRegistry::default();
        operations.insert_method_op(
            "Option",
            "unwrapOrTarget",
            op,
            ast::Path::plain(vec![
                ast::Ident::new("kotlin"),
                ast::Ident::new("Option"),
                ast::Ident::new("unwrapOrTarget"),
            ]),
        );
        let converter = PortableOpAstConverter::new(operations);
        let expr = converter
            .convert(
                PortableOpCall {
                    span: Span::null(),
                    op: test_operation("option_unwrap"),
                    args: vec![Expr::name(Name::ident("value"))],
                    kwargs: Vec::new(),
                },
                &None,
            )
            .expect("declared method operation");
        let ast::ExprKind::Invoke(invoke) = expr.kind else {
            panic!("expected an AST invoke");
        };
        let ast::ExprInvokeTarget::Method(select) = invoke.target else {
            panic!("expected a target method");
        };
        assert_eq!(select.field.as_str(), "unwrapOrTarget");
        assert_eq!(invoke.args.len(), 0);
    }

    #[test]
    fn method_lifting_preserves_structured_generic_arguments() {
        let package_id = hir::PackageId::new("root");
        let owner = hir::OwnerId::root(package_id.clone());
        let receiver_id = hir::HirId::new(owner.clone(), 1);
        let call_id = hir::HirId::new(owner.clone(), 2);
        let type_id = hir::HirId::new(owner.clone(), 3);
        let receiver = hir::Expr::new(
            receiver_id,
            hir::ExprKind::Literal(hir::Lit::Integer(1)),
            Span::null(),
        );
        let generic_args = hir::GenericArgs {
            args: vec![
                hir::GenericArg::Lifetime("'a".into()),
                hir::GenericArg::Type(Box::new(hir::TypeExpr::new(
                    type_id,
                    hir::TypeExprKind::Primitive(fp_core::ast::TypePrimitive::Int(
                        fp_core::ast::TypeInt::U8,
                    )),
                    Span::null(),
                ))),
                hir::GenericArg::Const(Box::new(hir::Expr::new(
                    hir::HirId::new(owner.clone(), 4),
                    hir::ExprKind::Literal(hir::Lit::Integer(3)),
                    Span::null(),
                ))),
                hir::GenericArg::Infer(hir::InferArg {
                    hir_id: hir::HirId::new(owner.clone(), 5),
                    span: Span::null(),
                    kind: hir::InferArgKind::TypeOrConst,
                }),
            ],
            constraints: Vec::new(),
            parenthesized: hir::GenericArgsParentheses::No,
            span_ext: Span::null(),
        };
        let call = hir::Expr::new(
            call_id,
            hir::ExprKind::MethodCall(
                Box::new(receiver),
                "method".into(),
                Some(generic_args),
                Vec::new(),
            ),
            Span::null(),
        );
        let package = hir::HirPackage::new(package_id.clone());
        let mut workspace = hir::HirProgram::new();
        workspace.publish_package(package.clone());
        let lifter = HirToAstLifter::new(&package, &workspace);
        let lifted = lifter.lift_expr(&call).expect("lift method call");
        let ast::ExprKind::Invoke(invoke) = lifted.kind else {
            panic!("expected lifted method invocation");
        };
        let ast::ExprInvokeTarget::Method(select) = invoke.target else {
            panic!("expected lifted method target");
        };
        let Some(ast::GenericArgs::AngleBracketed(args)) = select.generic_args else {
            panic!("expected lifted angle-bracketed arguments");
        };
        assert!(matches!(
            args.args.as_slice(),
            [
                ast::AngleBracketedArg::Arg(ast::GenericArg::Lifetime(lifetime)),
                ast::AngleBracketedArg::Arg(ast::GenericArg::Type(ty)),
                ast::AngleBracketedArg::Arg(ast::GenericArg::Const(_)),
                ast::AngleBracketedArg::Arg(ast::GenericArg::Type(infer)),
            ] if lifetime == "'a"
                && matches!(ty.as_ref(), ast::Ty::Primitive(_))
                && matches!(infer.as_ref(), ast::Ty::Wildcard(_))
        ));
    }

    #[test]
    fn path_lifting_preserves_structured_generic_arguments() {
        let package_id = hir::PackageId::new("root");
        let owner = hir::OwnerId::root(package_id.clone());
        let type_id = hir::HirId::new(owner.clone(), 1);
        let generic_args = hir::GenericArgs {
            args: vec![hir::GenericArg::Type(Box::new(hir::TypeExpr::new(
                type_id,
                hir::TypeExprKind::Primitive(fp_core::ast::TypePrimitive::Int(
                    fp_core::ast::TypeInt::U8,
                )),
                Span::null(),
            )))],
            constraints: Vec::new(),
            parenthesized: hir::GenericArgsParentheses::No,
            span_ext: Span::null(),
        };
        let path = hir::Path::new(
            hir::Res::Error,
            vec![hir::PathSegment {
                ident: "Vec".into(),
                hir_id: Default::default(),
                args: Some(generic_args),
                infer_args: false,
                res: hir::Res::Error,
            }],
        );
        let package = hir::HirPackage::new(package_id.clone());
        let mut workspace = hir::HirProgram::new();
        workspace.publish_package(package.clone());
        let lifter = HirToAstLifter::new(&package, &workspace);
        let lifted = lifter.lift_path(&path).expect("lift generic path");
        let Some(ast::GenericArgs::AngleBracketed(args)) =
            lifted.segments[0].args.as_deref()
        else {
            panic!("expected lifted generic arguments");
        };
        assert!(matches!(
            args.args.as_slice(),
            [ast::AngleBracketedArg::Arg(ast::GenericArg::Type(ty))]
                if matches!(ty.as_ref(), ast::Ty::Primitive(_))
        ));
    }

    #[test]
    fn path_lifting_preserves_hir_path_span() {
        let package_id = hir::PackageId::new("root");
        let path_span = Span::new(7, 11, 24);
        let path = hir::Path::with_span(
            path_span,
            hir::Res::Error,
            vec![hir::PathSegment::with_hir_id(
                "Vec",
                Default::default(),
                None,
                hir::Res::Error,
                true,
            )],
        );
        let package = hir::HirPackage::new(package_id.clone());
        let mut workspace = hir::HirProgram::new();
        workspace.publish_package(package.clone());
        let lifter = HirToAstLifter::new(&package, &workspace);

        let lifted = lifter.lift_path(&path).expect("lift path");

        assert_eq!(lifted.span(), path_span);
    }

    #[test]
    fn path_lifting_preserves_const_inference_kind() {
        let package_id = hir::PackageId::new("root");
        let owner = hir::OwnerId::root(package_id.clone());
        let generic_args = hir::GenericArgs {
            args: vec![hir::GenericArg::Infer(hir::InferArg {
                hir_id: hir::HirId::new(owner.clone(), 1),
                span: Span::null(),
                kind: hir::InferArgKind::Const,
            })],
            constraints: Vec::new(),
            parenthesized: hir::GenericArgsParentheses::No,
            span_ext: Span::null(),
        };
        let path = hir::Path::new(
            hir::Res::Error,
            vec![hir::PathSegment {
                ident: "Array".into(),
                hir_id: Default::default(),
                args: Some(generic_args),
                infer_args: false,
                res: hir::Res::Error,
            }],
        );
        let package = hir::HirPackage::new(package_id.clone());
        let mut workspace = hir::HirProgram::new();
        workspace.publish_package(package.clone());
        let lifter = HirToAstLifter::new(&package, &workspace);
        let lifted = lifter.lift_path(&path).expect("lift const-infer path");
        let Some(ast::GenericArgs::AngleBracketed(args)) =
            lifted.segments[0].args.as_deref()
        else {
            panic!("expected lifted generic arguments");
        };
        assert!(matches!(
            args.args.as_slice(),
            [ast::AngleBracketedArg::Arg(ast::GenericArg::Const(expr))]
                if matches!(
                    expr.kind(),
                    ast::ExprKind::Name(name)
                        if name.as_ident().is_some_and(|ident| ident.as_str() == "_")
                )
        ));
    }

    #[test]
    fn lifts_nested_explicit_qself_with_trait_position() -> Result<()> {
        let package_id = hir::PackageId::new("root");
        let owner = hir::OwnerId::root(package_id.clone());
        let trait_id = hir::DefId::new(package_id.clone(), 1);
        let package = {
            let mut package = hir::HirPackage::new(package_id.clone());
            package.add_item(hir::Item {
                hir_id: hir::HirId::new(owner.clone(), 1),
                def_id: trait_id.clone(),
                visibility: hir::Visibility::Public,
                kind: hir::ItemKind::Trait(hir::Trait {
                    generics: hir::Generics {
                        params: Vec::new(),
                        where_clause: None,
                    },
                    items: Vec::new(),
                    supertraits: Vec::new(),
                }),
                span: Span::null(),
            });
            package
        };
        let mut workspace = hir::HirProgram::new();
        workspace.publish_package(package.clone());
        let lifter = HirToAstLifter::new(&package, &workspace);

        let qself_ty = hir::TypeExpr::new(
            hir::HirId::new(owner.clone(), 2),
            hir::TypeExprKind::Primitive(fp_core::ast::TypePrimitive::Int(
                fp_core::ast::TypeInt::U8,
            )),
            Span::null(),
        );
        let path = hir::Path::new(
            hir::Res::Error,
            vec![
                hir::PathSegment {
                    ident: "Trait".into(),
                    hir_id: Default::default(),
                    args: None,
                    infer_args: true,
                    res: hir::Res::Def(trait_id),
                },
                hir::PathSegment {
                    ident: "Item".into(),
                    hir_id: Default::default(),
                    args: None,
                    infer_args: true,
                    res: hir::Res::Error,
                },
            ],
        );
        let receiver = hir::TypeExpr::new(
            hir::HirId::new(owner.clone(), 3),
            hir::TypeExprKind::Path(hir::QPath::Resolved(Some(Box::new(qself_ty)), path)),
            Span::null(),
        );
        let qpath = hir::QPath::TypeRelative(
            Box::new(receiver),
            hir::PathSegment {
                ident: "Nested".into(),
                hir_id: Default::default(),
                args: None,
                infer_args: true,
                res: hir::Res::Error,
            },
        );

        let lifted = lifter.lift_qpath(&qpath)?;
        let qself = lifted.qself.expect("explicit qself");
        assert_eq!(qself.position, 1);
        assert_eq!(lifted.path.segments.len(), 3);
        assert_eq!(lifted.path.segments[0].ident.as_str(), "Trait");
        assert_eq!(lifted.path.segments[1].ident.as_str(), "Item");
        assert_eq!(lifted.path.segments[2].ident.as_str(), "Nested");
        Ok(())
    }

    impl IntrinsicMaterializer for TestMaterializer {
        fn materialize_portable_operation(
            &self,
            call: PortableOpCall,
            _ty: &ast::TySlot,
        ) -> Result<fp_core::intrinsics::MaterializeOutcome<Expr>> {
            Ok(fp_core::intrinsics::MaterializeOutcome::Replaced(
                Expr::name(Name::ident(call.op.name().to_string())),
            ))
        }
    }

    #[test]
    fn dependency_source_path_metadata_is_enough_for_lifting_type_names() {
        let root_id = hir::PackageId::new("root");
        let dependency_id = hir::PackageId::new("dependency");
        let dependency_def = hir::DefId::new(dependency_id.clone(), 7);
        let mut dependency = hir::HirPackage::new(dependency_id);
        dependency.source_paths.insert(
            dependency_def.clone(),
            fp_core::ast::path::InPackagePath::new(vec!["dependency".into(), "Widget".into()]),
        );

        let mut workspace = hir::HirProgram::new();
        workspace.add_package(Rc::new(RefCell::new(dependency)));
        let root = hir::HirPackage::new(root_id);
        workspace.publish_package(root.clone());
        let lifter = HirToAstLifter::new(&root, &workspace);

        let lifted = lifter
            .def_id_to_ty(&dependency_def)
            .expect("dependency source-path metadata should be sufficient");
        let Ty::Expr(expr) = lifted else {
            panic!("expected a path-shaped AST type");
        };
        let ast::ExprKind::Name(ast::Name { path: path, .. }) = &expr.kind else {
            panic!("expected a path-shaped AST expression");
        };
        assert_eq!(path.segments()[0].ident.name, "dependency");
        assert_eq!(path.segments()[1].ident.name, "Widget");
    }

    #[test]
    fn associated_call_resolution_lifts_to_portable_operation() {
        let package_id = hir::PackageId::new("root");
        let receiver_id = hir::DefId::new(package_id.clone(), 1);
        let associated_id = hir::DefId::new(package_id.clone(), 2);
        let call_id = hir::HirId::new(hir::OwnerId::root(package_id.clone()), 3);
        let callee_id = hir::HirId::new(hir::OwnerId::root(package_id.clone()), 4);

        let mut package = hir::HirPackage::new(package_id.clone());
        package.source_paths.insert(
            associated_id.clone(),
            fp_core::ast::path::InPackagePath::new(vec!["String".into(), "from_utf8_lossy".into()]),
        );
        package.record_method_resolution(call_id.clone(), associated_id);
        let call = hir::Expr {
            hir_id: call_id,
            kind: hir::ExprKind::Call(
                Box::new(hir::Expr {
                    hir_id: callee_id,
                    kind: hir::ExprKind::Path(hir::QPath::resolved(hir::Path {
                        span: Default::default(),
                        segments: vec![
                            hir::PathSegment {
                                ident: "String".into(),
                                hir_id: Default::default(),
                                args: None,
                                infer_args: true,
                                res: hir::Res::Def(receiver_id.clone()),
                            },
                            hir::PathSegment {
                                ident: "from_utf8_lossy".into(),
                                hir_id: Default::default(),
                                args: None,
                                infer_args: true,
                                res: hir::Res::Error,
                            },
                        ],
                        // This deliberately remains the nominal type DefId:
                        // the selected associated member comes only from
                        // type checking's call-resolution table.
                        res: hir::Res::Def(receiver_id),
                    })),
                    span: Span::null(),
                }),
                Vec::new(),
            ),
            span: Span::null(),
        };
        let mut workspace = hir::HirProgram::new();
        workspace.publish_package(package.clone());
        let target_path = ast::Path::plain(vec![
            ast::Ident::new("kotlin"),
            ast::Ident::new("String"),
            ast::Ident::new("fromUtf8Lossy"),
        ]);
        let mut source_operations = fp_core::lang::LangItemRegistry::default();
        source_operations.insert_op(
            "string_from_utf8_lossy",
            ast::Path::plain(vec![
                ast::Ident::new("String"),
                ast::Ident::new("from_utf8_lossy"),
            ]),
        );
        let mut target_operations = fp_core::lang::LangItemRegistry::default();
        target_operations.insert_op("string_from_utf8_lossy", target_path);
        let lifter = HirToAstLifter::new(&package, &workspace)
            .with_capabilities(fp_core::capabilities::LanguageCapabilities {
                portable_operations: true,
                ..fp_core::capabilities::LanguageCapabilities::NATIVE
            })
            .with_source_operations(source_operations)
            .with_target_operations(target_operations)
            .with_materializer(Arc::new(TestMaterializer));

        let lifted = lifter.lift_expr(&call).expect("lift associated call");
        assert!(
            matches!(lifted.kind, ast::ExprKind::Invoke(ast::ExprInvoke { target: ast::ExprInvokeTarget::Function(ast::Name { path, .. }), .. }) if path.to_string() == "kotlin::String::fromUtf8Lossy")
        );
    }

    #[test]
    fn native_lifting_preserves_an_op_tagged_call_as_an_invoke() {
        let package_id = hir::PackageId::new("root");
        let function_id = hir::DefId::new(package_id.clone(), 1);
        let call_id = hir::HirId::new(hir::OwnerId::root(package_id.clone()), 2);
        let callee_id = hir::HirId::new(hir::OwnerId::root(package_id.clone()), 3);

        let mut package = hir::HirPackage::new(package_id.clone());
        let call = hir::Expr {
            hir_id: call_id,
            kind: hir::ExprKind::Call(
                Box::new(hir::Expr {
                    hir_id: callee_id,
                    kind: hir::ExprKind::Path(hir::QPath::resolved(hir::Path {
                        span: Default::default(),
                        segments: vec![hir::PathSegment {
                            ident: "from_utf8_lossy".into(),
                            hir_id: Default::default(),
                            args: None,
                            infer_args: true,
                            res: hir::Res::Def(function_id.clone()),
                        }],
                        res: hir::Res::Def(function_id),
                    })),
                    span: Span::null(),
                }),
                Vec::new(),
            ),
            span: Span::null(),
        };
        let mut workspace = hir::HirProgram::new();
        workspace.publish_package(package.clone());

        let lifted = HirToAstLifter::new(&package, &workspace)
            .lift_expr(&call)
            .expect("lift native call");
        assert!(matches!(lifted.kind, ast::ExprKind::Invoke(_)));
    }

    #[test]
    fn resolved_method_intrinsic_lifts_without_a_source_name_fallback() {
        let package_id = hir::PackageId::new("root");
        let receiver_id = hir::DefId::new(package_id.clone(), 1);
        let method_id = hir::DefId::new(package_id.clone(), 2);
        let call_id = hir::HirId::new(hir::OwnerId::root(package_id.clone()), 3);
        let receiver_hir_id = hir::HirId::new(hir::OwnerId::root(package_id.clone()), 4);

        let mut package = hir::HirPackage::new(package_id.clone());
        package
            .intrinsic_defs
            .insert(method_id.clone(), fp_core::intrinsics::CallKind::FsExists);
        package.record_method_resolution(call_id.clone(), method_id);
        let call = hir::Expr {
            hir_id: call_id,
            kind: hir::ExprKind::MethodCall(
                Box::new(hir::Expr {
                    hir_id: receiver_hir_id,
                    kind: hir::ExprKind::Path(hir::QPath::resolved(hir::Path {
                        span: Default::default(),
                        segments: vec![hir::PathSegment {
                            ident: "path".into(),
                            hir_id: Default::default(),
                            args: None,
                            infer_args: true,
                            res: hir::Res::Def(receiver_id.clone()),
                        }],
                        res: hir::Res::Def(receiver_id),
                    })),
                    span: Span::null(),
                }),
                hir::Symbol::new("exists"),
                None,
                Vec::new(),
            ),
            span: Span::null(),
        };
        let mut workspace = hir::HirProgram::new();
        workspace.publish_package(package.clone());
        let lifter = HirToAstLifter::new(&package, &workspace);

        let lifted = lifter
            .lift_expr(&call)
            .expect("lift resolved intrinsic call");
        assert!(matches!(lifted.kind, ast::ExprKind::IntrinsicCall(_)));
    }

    #[test]
    fn resolved_process_receivers_lift_through_portable_operation_identity() {
        let package_id = hir::PackageId::new("root");
        let owner = hir::OwnerId::root(package_id.clone());
        let command_id = hir::DefId::new(package_id.clone(), 1);
        let command_output_id = hir::DefId::new(package_id.clone(), 10);
        let command_status_id = hir::DefId::new(package_id.clone(), 11);
        let command_spawn_id = hir::DefId::new(package_id.clone(), 12);
        let status_success_id = hir::DefId::new(package_id.clone(), 13);
        let child_id = hir::DefId::new(package_id.clone(), 2);
        let child_wait_id = hir::DefId::new(package_id.clone(), 14);
        let child_output_id = hir::DefId::new(package_id.clone(), 15);
        let dir_entry_id = hir::DefId::new(package_id.clone(), 3);
        let dir_entry_file_type_id = hir::DefId::new(package_id.clone(), 16);
        let file_type_id = hir::DefId::new(package_id.clone(), 4);
        let file_type_is_dir_id = hir::DefId::new(package_id.clone(), 17);

        let receiver = |index: u32, name: &str, def_id: hir::DefId| {
            hir::Expr::new(
                hir::HirId::new(owner.clone(), index),
                hir::ExprKind::Path(hir::QPath::resolved(hir::Path {
                    span: Default::default(),
                    segments: vec![hir::PathSegment {
                        ident: name.into(),
                        hir_id: Default::default(),
                        args: None,
                        infer_args: true,
                        res: hir::Res::Def(def_id.clone()),
                    }],
                    res: hir::Res::Def(def_id),
                })),
                Span::null(),
            )
        };
        let method = |index: u32, receiver: hir::Expr, name: &str| {
            hir::Expr::new(
                hir::HirId::new(owner.clone(), index),
                hir::ExprKind::MethodCall(Box::new(receiver), name.into(), None, Vec::new()),
                Span::null(),
            )
        };

        let output_call = method(20, receiver(21, "command", command_id.clone()), "output");
        let status_call = method(22, receiver(23, "command", command_id.clone()), "status");
        let spawn_call = method(24, receiver(25, "command", command_id.clone()), "spawn");
        let output_local = hir::Expr::new(
            hir::HirId::new(owner.clone(), 26),
            hir::ExprKind::Path(hir::QPath::resolved(hir::Path {
                span: Default::default(),
                segments: vec![hir::PathSegment {
                    ident: "output".into(),
                    hir_id: Default::default(),
                    args: None,
                    infer_args: true,
                    res: hir::Res::Local(hir::HirId::new(owner.clone(), 27)),
                }],
                res: hir::Res::Local(hir::HirId::new(owner.clone(), 27)),
            })),
            Span::null(),
        );
        let output_status = hir::Expr::new(
            hir::HirId::new(owner.clone(), 28),
            hir::ExprKind::FieldAccess(Box::new(output_local), "status".into()),
            Span::null(),
        );
        let success_call = method(29, output_status, "success");
        let child_wait_call = method(30, receiver(31, "child", child_id.clone()), "wait");
        let child_output_call = method(32, receiver(33, "child", child_id), "wait_with_output");
        let file_type_call = method(34, receiver(35, "entry", dir_entry_id), "file_type");
        let is_dir_call = method(36, receiver(37, "file_type", file_type_id), "is_dir");

        let mut package = hir::HirPackage::new(package_id.clone());
        let mut registry = fp_core::lang::LangItemRegistry::default();
        for name in [
            "command_output",
            "command_status",
            "command_spawn",
            "exit_status_success",
            "child_wait",
            "child_wait_with_output",
            "dir_entry_file_type",
            "file_type_is_dir",
        ] {
            registry.insert_op(name, ast::Path::plain(vec![ast::Ident::new(name)]));
        }
        for (def_id, operation) in [
            (command_output_id.clone(), "command_output"),
            (command_status_id.clone(), "command_status"),
            (command_spawn_id.clone(), "command_spawn"),
            (status_success_id.clone(), "exit_status_success"),
            (child_wait_id.clone(), "child_wait"),
            (child_output_id.clone(), "child_wait_with_output"),
            (dir_entry_file_type_id.clone(), "dir_entry_file_type"),
            (file_type_is_dir_id.clone(), "file_type_is_dir"),
        ] {
            package.source_paths.insert(
                def_id,
                fp_core::ast::path::InPackagePath::new(vec![operation.to_string()]),
            );
        }
        for (expr, def_id) in [
            (&output_call, command_output_id),
            (&status_call, command_status_id),
            (&spawn_call, command_spawn_id),
            (&success_call, status_success_id),
            (&child_wait_call, child_wait_id),
            (&child_output_call, child_output_id),
            (&file_type_call, dir_entry_file_type_id),
            (&is_dir_call, file_type_is_dir_id),
        ] {
            package.record_method_resolution(expr.hir_id.clone(), def_id);
        }
        let mut workspace = hir::HirProgram::new();
        workspace.publish_package(package.clone());
        let lifter = HirToAstLifter::new(&package, &workspace)
            .with_capabilities(fp_core::capabilities::LanguageCapabilities {
                portable_operations: true,
                ..fp_core::capabilities::LanguageCapabilities::NATIVE
            })
            .with_source_operations(registry)
            .with_materializer(Arc::new(TestMaterializer));

        for (expr, operation) in [
            (&output_call, "command_output"),
            (&status_call, "command_status"),
            (&spawn_call, "command_spawn"),
            (&success_call, "exit_status_success"),
            (&child_wait_call, "child_wait"),
            (&child_output_call, "child_wait_with_output"),
            (&file_type_call, "dir_entry_file_type"),
            (&is_dir_call, "file_type_is_dir"),
        ] {
            let lifted = lifter.lift_expr(expr).expect("lift process receiver call");
            let ast::ExprKind::Name(ast::Name { path, .. }) = lifted.kind else {
                panic!("resolved process receiver must not fall back to a source method");
            };
            assert_eq!(path.last().ident.name, operation);
        }

        // Struct fields do not currently have DefIds in HIR. Their typed
        // value is nevertheless sufficient for the following `success`
        // method to resolve by identity above; lifting the field itself must
        // remain a plain selection until HIR gains field declarations.
        let status_field = lifter
            .lift_expr(match &success_call.kind {
                hir::ExprKind::MethodCall(receiver, _, _, _) => receiver,
                _ => unreachable!(),
            })
            .expect("lift Output.status field");
        assert!(matches!(status_field.kind, ast::ExprKind::FieldAccess(_)));
    }

    #[test]
    fn typed_vec_and_command_locals_lift_with_resolved_nominal_types() {
        let package_id = hir::PackageId::new("root");
        let vec_def_id = hir::DefId::new(hir::PackageId::new("alloc"), 1);
        let command_def_id = hir::DefId::new(hir::PackageId::new("std"), 2);
        let owner = hir::OwnerId::root(package_id.clone());
        let vec_ty_id = hir::HirId::new(owner.clone(), 1);
        let command_ty_id = hir::HirId::new(owner.clone(), 2);
        let vec_pat_id = hir::HirId::new(owner.clone(), 3);
        let command_pat_id = hir::HirId::new(owner.clone(), 4);

        let vec_ty = hir::TypeExpr::new(
            vec_ty_id.clone(),
            hir::TypeExprKind::Path(hir::QPath::resolved(hir::Path {
                span: Default::default(),
                segments: vec![hir::PathSegment {
                    ident: "ListAlias".into(),
                    hir_id: Default::default(),
                    args: None,
                    infer_args: true,
                    res: hir::Res::Def(vec_def_id.clone()),
                }],
                res: hir::Res::Def(vec_def_id.clone()),
            })),
            Span::null(),
        );
        let command_ty = hir::TypeExpr::new(
            command_ty_id.clone(),
            hir::TypeExprKind::Path(hir::QPath::resolved(hir::Path {
                span: Default::default(),
                segments: vec![hir::PathSegment {
                    ident: "FileAlias".into(),
                    hir_id: Default::default(),
                    args: None,
                    infer_args: true,
                    res: hir::Res::Def(command_def_id.clone()),
                }],
                res: hir::Res::Def(command_def_id.clone()),
            })),
            Span::null(),
        );
        let local = |pat_id: hir::HirId, name: &str, ty: hir::TypeExpr| hir::Stmt {
            hir_id: hir::HirId::new(owner.clone(), pat_id.local_id() + 10),
            kind: hir::StmtKind::Local(hir::Local {
                hir_id: hir::HirId::new(owner.clone(), pat_id.local_id() + 20),
                pat: hir::Pat {
                    hir_id: pat_id,
                    kind: hir::PatKind::Binding {
                        name: name.into(),
                        mutable: false,
                    },
                },
                ty: Some(ty),
                init: None,
            }),
        };
        let block = hir::Block {
            hir_id: hir::HirId::new(owner.clone(), 30),
            stmts: vec![
                local(vec_pat_id, "entries", vec_ty),
                local(command_pat_id, "command", command_ty),
            ],
            expr: None,
        };
        let mut alloc = hir::HirPackage::new(hir::PackageId::new("alloc"));
        alloc.source_paths.insert(
            vec_def_id.clone(),
            fp_core::ast::path::InPackagePath::new(vec!["alloc".into(), "Vec".into()]),
        );
        let mut std = hir::HirPackage::new(hir::PackageId::new("std"));
        std.source_paths.insert(
            command_def_id.clone(),
            fp_core::ast::path::InPackagePath::new(vec!["std".into(), "Command".into()]),
        );
        let package = hir::HirPackage::new(package_id);
        package.record_type_expr_type(
            vec_ty_id,
            hir::ty::Ty {
                kind: hir::ty::TyKind::Adt(
                    hir::ty::AdtDef {
                        did: vec_def_id,
                        variants: Vec::new(),
                        flags: hir::ty::AdtFlags::empty(),
                        repr: hir::ty::ReprOptions {
                            int: None,
                            align: None,
                            pack: None,
                            flags: hir::ty::ReprFlags::empty(),
                            field_shuffle_seed: 0,
                        },
                    },
                    Vec::new(),
                ),
            },
        );
        package.record_type_expr_type(
            command_ty_id,
            hir::ty::Ty {
                kind: hir::ty::TyKind::Adt(
                    hir::ty::AdtDef {
                        did: command_def_id,
                        variants: Vec::new(),
                        flags: hir::ty::AdtFlags::empty(),
                        repr: hir::ty::ReprOptions {
                            int: None,
                            align: None,
                            pack: None,
                            flags: hir::ty::ReprFlags::empty(),
                            field_shuffle_seed: 0,
                        },
                    },
                    Vec::new(),
                ),
            },
        );
        let mut workspace = hir::HirProgram::new();
        workspace.publish_package(alloc);
        workspace.publish_package(std);
        workspace.publish_package(package.clone());
        let lifter = HirToAstLifter::new(&package, &workspace);

        let lifted = lifter.lift_block(&block).expect("lift typed locals");
        let types = lifted
            .stmts
            .iter()
            .map(|stmt| match stmt {
                BlockStmt::Let(stmt) => match stmt.pat.kind() {
                    PatternKind::Type(typed) => match &typed.ty {
                        Ty::Expr(expr) => match expr.kind() {
                            ast::ExprKind::Name(Name { path: path, .. }) => path.join("."),
                            other => panic!("expected source path type name, got {other:?}"),
                        },
                        other => panic!("expected nominal type, got {other:?}"),
                    },
                    other => panic!("expected typed local, got {other:?}"),
                },
                _ => panic!("expected local statement"),
            })
            .collect::<Vec<_>>();
        assert_eq!(types, ["alloc.Vec", "std.Command"]);

        // Exercise the actual item-lifting path used by transpilation. The
        // written annotation names deliberately differ from their resolved
        // definitions, so this catches a regression that re-emits source
        // spelling instead of the type checker's nominal type fact.
        let function = hir::Item {
            hir_id: hir::HirId::new(owner.clone(), 40),
            def_id: hir::DefId::new(hir::PackageId::new("root"), 40),
            visibility: hir::Visibility::Private,
            kind: hir::ItemKind::Function(hir::Function {
                sig: hir::FunctionSig {
                    name: "typed_locals".into(),
                    inputs: Vec::new(),
                    output: hir::TypeExpr::new(
                        hir::HirId::new(owner.clone(), 41),
                        hir::TypeExprKind::Tuple(Vec::new()),
                        Span::null(),
                    ),
                    generics: hir::Generics::default(),
                    abi: hir::ty::Abi::Rust,
                },
                body: Some(block),
                is_const: false,
                is_extern: false,
                is_async: false,
                attrs: Vec::new(),
            }),
            span: Span::null(),
        };
        let lifted_function = lifter.lift_item(&function).expect("lift function");
        let ast::ItemKind::DefFunction(function) = lifted_function.kind() else {
            panic!("expected lifted function");
        };
        let local_types = function
            .body
            .stmts
            .iter()
            .filter_map(|stmt| match stmt {
                BlockStmt::Let(stmt) => match stmt.pat.kind() {
                    PatternKind::Type(typed) => match &typed.ty {
                        Ty::Expr(expr) => match expr.kind() {
                            ast::ExprKind::Name(Name { path: path, .. }) => Some(path.join(".")),
                            _ => None,
                        },
                        _ => None,
                    },
                    _ => None,
                },
                _ => None,
            })
            .collect::<Vec<_>>();
        assert_eq!(local_types, ["alloc.Vec", "std.Command"]);
    }

    #[test]
    fn result_try_lifts_to_typed_propagation_operation() {
        let package_id = hir::PackageId::new("root");
        let result_package_id = hir::PackageId::new("core");
        let result_def_id = hir::DefId::new(result_package_id.clone(), 1);
        let operand_id = hir::HirId::new(hir::OwnerId::root(package_id.clone()), 1);
        let try_id = hir::HirId::new(hir::OwnerId::root(package_id.clone()), 2);

        let mut result_package = hir::HirPackage::new(result_package_id);
        result_package.source_paths.insert(
            result_def_id.clone(),
            fp_core::ast::path::InPackagePath::new(vec![
                "core".into(),
                "result".into(),
                "Result".into(),
            ]),
        );
        let root = hir::HirPackage::new(package_id);
        root.record_expr_type(
            operand_id.clone(),
            hir::ty::Ty {
                kind: hir::ty::TyKind::Adt(
                    hir::ty::AdtDef {
                        did: result_def_id,
                        variants: Vec::new(),
                        flags: hir::ty::AdtFlags::IS_ENUM,
                        repr: hir::ty::ReprOptions {
                            int: None,
                            align: None,
                            pack: None,
                            flags: hir::ty::ReprFlags::empty(),
                            field_shuffle_seed: 0,
                        },
                    },
                    Vec::new(),
                ),
            },
        );
        let try_expr = hir::Expr::new(
            try_id,
            hir::ExprKind::Try(hir::TryExpr {
                expr: Box::new(hir::Expr::new(
                    operand_id,
                    hir::ExprKind::Literal(hir::Lit::Null),
                    Span::null(),
                )),
                catches: Vec::new(),
                elze: None,
                finally: None,
            }),
            Span::null(),
        );

        let mut workspace = hir::HirProgram::new();
        workspace.publish_package(result_package);
        workspace.publish_package(root.clone());
        let lifter = HirToAstLifter::new(&root, &workspace)
            .with_capabilities(fp_core::capabilities::LanguageCapabilities {
                portable_operations: true,
                ..fp_core::capabilities::LanguageCapabilities::NATIVE
            })
            .with_materializer(Arc::new(TestMaterializer));

        let lifted = lifter.lift_expr(&try_expr).expect("lift typed Result try");
        let ast::ExprKind::Name(ast::Name { path, .. }) = lifted.kind else {
            panic!("typed Result try must not remain a generic Try expression");
        };
        assert_eq!(path.last().ident.name, "result_propagate");
    }

    #[test]
    fn lifts_async_trait_methods_as_async_declarations() {
        let package_id = hir::PackageId::new("root");
        let trait_id = hir::DefId::new(package_id.clone(), 1);
        let method_id = hir::DefId::new(package_id.clone(), 2);
        let owner = hir::OwnerId::root(package_id.clone());
        let function = hir::Function {
            sig: hir::FunctionSig {
                name: hir::Symbol::new("browse"),
                inputs: Vec::new(),
                output: hir::TypeExpr::new(
                    hir::HirId::new(owner.clone(), 3),
                    hir::TypeExprKind::Tuple(Vec::new()),
                    Span::null(),
                ),
                generics: hir::Generics {
                    params: Vec::new(),
                    where_clause: None,
                },
                abi: hir::Abi::Rust,
            },
            body: None,
            is_const: false,
            is_extern: false,
            is_async: true,
            attrs: Vec::new(),
        };
        let trait_item = hir::Item {
            hir_id: hir::HirId::new(owner.clone(), 4),
            def_id: trait_id.clone(),
            visibility: hir::Visibility::Public,
            kind: hir::ItemKind::Trait(hir::Trait {
                generics: hir::Generics {
                    params: Vec::new(),
                    where_clause: None,
                },
                items: vec![hir::TraitItem {
                    def_id: method_id,
                    hir_id: hir::HirId::new(owner, 5),
                    name: hir::Symbol::new("browse"),
                    kind: hir::TraitItemKind::Method(function),
                }],
                supertraits: Vec::new(),
            }),
            span: Span::null(),
        };
        let mut package = hir::HirPackage::new(package_id);
        package.items.push(trait_item);
        package.source_paths.insert(
            trait_id,
            fp_core::ast::path::InPackagePath::new(vec!["RepoBrowse".into()]),
        );
        let mut workspace = hir::HirProgram::new();
        workspace.publish_package(package.clone());
        let lifter = HirToAstLifter::new(&package, &workspace);

        let lifted = lifter.lift_items_by_def_id();
        let trait_item = lifted
            .values()
            .next()
            .expect("trait must be lifted from typed HIR");
        let ItemKind::DefTrait(trait_def) = trait_item.kind() else {
            panic!("expected a trait declaration");
        };
        let ItemKind::DeclFunction(method) = trait_def.items[0].kind() else {
            panic!("expected an abstract trait method declaration");
        };
        assert!(method.is_async);
    }
}

fn recon_closures_in_stmt(stmt: &mut BlockStmt, types: &HashMap<String, Vec<Ty>>) {
    match stmt {
        BlockStmt::Expr(se) => recon_closures_in_expr(&mut se.expr, types),
        BlockStmt::Let(l) => {
            if let Some(ref mut init) = l.init {
                recon_closures_in_expr(init, types);
            }
        }
        BlockStmt::Item(item) => recon_closures_in_item(item, types),
        _ => {}
    }
}

fn recon_closures_in_expr(expr: &mut Expr, types: &HashMap<String, Vec<Ty>>) {
    match expr.kind_mut() {
        ast::ExprKind::Struct(st) => {
            let struct_name = match st.name.kind() {
                ast::ExprKind::Name(Name { path: p, .. }) => p
                    .segments
                    .iter()
                    .map(|s| s.ident.as_str())
                    .collect::<Vec<_>>()
                    .join("::"),
                _ => {
                    return;
                }
            };
            let last_seg = struct_name.rsplit("::").next().unwrap_or(&struct_name);
            if let Some(param_types) = types.get(last_seg) {
                if !param_types.is_empty() {
                    // Each synthesized param's type is a genuine annotation
                    // position (same as any other closure param — see the
                    // `hir::ExprKind::Closure` lifting arm above), so it's
                    // promoted into `PatternKind::Type` directly rather than
                    // stamped onto a since-removed `Pattern.ty` cache field.
                    let params: Vec<Pattern> = param_types
                        .iter()
                        .enumerate()
                        .map(|(i, ty)| {
                            let ident_pat = Pattern::from(PatternKind::Ident(PatternIdent {
                                ident: Ident::new(format!("__p{}", i)),
                                mutability: None,
                            }));
                            Pattern::from(PatternKind::Type(ast::PatternType::new(
                                ident_pat,
                                ty.clone(),
                            )))
                        })
                        .collect();
                    let span = expr.span;
                    // Replace this struct with a closure — the body is a placeholder
                    expr.kind = ast::ExprKind::Closure(ExprClosure {
                        span: span.unwrap_or_default(),
                        params,
                        ret_ty: None,
                        movability: None,
                        body: Box::new(Expr::unit()),
                    });
                    return;
                }
            }
            for field in &mut st.fields {
                if let Some(ref mut val) = field.value {
                    recon_closures_in_expr(val, types);
                }
            }
        }
        ast::ExprKind::Invoke(inv) => {
            for arg in &mut inv.args {
                recon_closures_in_expr(arg, types);
            }
            match &mut inv.target {
                ast::ExprInvokeTarget::Method(sel) => recon_closures_in_expr(&mut sel.obj, types),
                ast::ExprInvokeTarget::Expr(be) => recon_closures_in_expr(be, types),
                _ => {}
            }
        }
        ast::ExprKind::Block(block) => {
            for stmt in &mut block.stmts {
                recon_closures_in_stmt(stmt, types);
            }
        }
        ast::ExprKind::If(if_expr) => {
            recon_closures_in_expr(&mut if_expr.cond, types);
            recon_closures_in_expr(&mut if_expr.then, types);
            if let Some(ref mut elze) = if_expr.elze {
                recon_closures_in_expr(elze, types);
            }
        }
        ast::ExprKind::Match(mt) => {
            if let Some(ref mut s) = mt.scrutinee {
                recon_closures_in_expr(s, types);
            }
            for case in &mut mt.cases {
                recon_closures_in_expr(&mut case.body, types);
            }
        }
        ast::ExprKind::Let(l) => {
            recon_closures_in_expr(&mut l.expr, types);
        }
        ast::ExprKind::Assign(a) => {
            recon_closures_in_expr(&mut a.value, types);
            recon_closures_in_expr(&mut a.target, types);
        }
        ast::ExprKind::Return(r) => {
            if let Some(ref mut v) = r.value {
                recon_closures_in_expr(v, types);
            }
        }
        ast::ExprKind::BinOp(bin) => {
            recon_closures_in_expr(&mut bin.lhs, types);
            recon_closures_in_expr(&mut bin.rhs, types);
        }
        ast::ExprKind::UnOp(un) => {
            recon_closures_in_expr(&mut un.val, types);
        }
        ast::ExprKind::FieldAccess(sel) => {
            recon_closures_in_expr(&mut sel.obj, types);
        }
        ast::ExprKind::Index(idx) => {
            recon_closures_in_expr(&mut idx.obj, types);
            recon_closures_in_expr(&mut idx.index, types);
        }
        ast::ExprKind::Closure(cl) => {
            recon_closures_in_expr(&mut cl.body, types);
        }
        ast::ExprKind::Cast(c) => {
            recon_closures_in_expr(&mut c.expr, types);
        }
        ast::ExprKind::Reference(r) => {
            recon_closures_in_expr(&mut r.referee, types);
        }
        ast::ExprKind::While(wh) => {
            recon_closures_in_expr(&mut wh.cond, types);
            recon_closures_in_expr(&mut wh.body, types);
        }
        ast::ExprKind::For(fr) => {
            recon_closures_in_expr(&mut fr.iter, types);
            recon_closures_in_expr(&mut fr.body, types);
        }
        ast::ExprKind::Loop(lp) => {
            recon_closures_in_expr(&mut lp.body, types);
        }
        ast::ExprKind::Try(tr) => {
            recon_closures_in_expr(&mut tr.expr, types);
            for catch in &mut tr.catches {
                recon_closures_in_expr(&mut catch.body, types);
            }
        }
        ast::ExprKind::Array(arr) => {
            for val in &mut arr.values {
                recon_closures_in_expr(val, types);
            }
        }
        ast::ExprKind::Tuple(tup) => {
            for val in &mut tup.values {
                recon_closures_in_expr(val, types);
            }
        }
        _ => {}
    }
}
