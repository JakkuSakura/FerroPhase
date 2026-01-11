use std::collections::{HashMap, HashSet};
use std::sync::{Arc, Mutex};

use crate::error::interpretation_error;
use crate::intrinsics::IntrinsicsRegistry;
use fp_core::ast::DecimalType;
use fp_core::ast::Pattern;
use fp_core::ast::{
    AttrMeta, Attribute, BlockStmt, Expr, ExprBlock, ExprClosure, ExprField, ExprIntrinsicCall,
    ExprInvoke, ExprInvokeTarget, ExprKind, ExprQuote, ExprRange, ExprRangeLimit,
    ExprStringTemplate, FormatArgRef, FormatTemplatePart, FunctionParam, Item, ItemDefFunction,
    ItemImport, ItemImportTree, ItemKind, Node, NodeKind, Path, QuoteFragmentKind, QuoteTokenValue,
    StmtLet, StructuralField, Ty, TypeAny, TypeArray, TypeBinaryOpKind, TypeFunction, TypeInt,
    TypePrimitive, TypeQuote, TypeReference, TypeSlice, TypeStruct, TypeStructural, TypeTuple,
    TypeUnit, TypeVec, Value, ValueField, ValueFunction, ValueList, ValueStruct, ValueStructural,
    ValueTuple,
};
use fp_core::ast::{Ident, Locator};
use fp_core::context::SharedScopedContext;
use fp_core::diagnostics::{Diagnostic, DiagnosticLevel, DiagnosticManager};
use fp_core::error::Result;
use fp_core::intrinsics::IntrinsicCallKind;
use fp_core::module::resolver::{ModuleImport, ResolvedSymbol, ResolverError, ResolverRegistry};
use fp_core::module::{ModuleId, ModuleLanguage, SymbolDescriptor, SymbolKind};
use fp_core::ops::{format_runtime_string, format_value_with_spec, BinOpKind, UnOpKind};
use fp_core::package::graph::PackageGraph;
use fp_core::utils::anybox::AnyBox;
use fp_typing::AstTypeInferencer;
mod blocks;
mod closures;
mod const_regions;
mod env;
mod eval_expr;
mod eval_stmt;
mod intrinsics;
mod operators;
mod quote;

const DEFAULT_DIAGNOSTIC_CONTEXT: &str = "ast-interpreter";

// === Quote fragment representation (interpreter-internal) ===
#[derive(Debug, Clone, PartialEq)]
pub enum QuotedFragment {
    Expr(Expr),
    Stmts(Vec<BlockStmt>),
    Items(Vec<Item>),
    Type(Ty),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InterpreterMode {
    /// Compile-time evaluation for const regions: allow most operations that do
    /// not require external access (IO, host bindings, runtime-only intrinsics).
    CompileTime,
    /// Runtime evaluation with full semantics and side effects.
    RunTime,
}

#[derive(Debug, Clone)]
pub struct InterpreterOptions {
    pub mode: InterpreterMode,
    pub debug_assertions: bool,
    pub diagnostics: Option<Arc<DiagnosticManager>>,
    pub diagnostic_context: &'static str,
    // Optional module resolution context for handling `use`/imports during evaluation.
    pub module_resolution: Option<ModuleResolutionContext>,
}

impl Default for InterpreterOptions {
    fn default() -> Self {
        Self {
            mode: InterpreterMode::CompileTime,
            debug_assertions: false,
            diagnostics: None,
            diagnostic_context: DEFAULT_DIAGNOSTIC_CONTEXT,
            module_resolution: None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ModuleResolutionContext {
    pub graph: Arc<PackageGraph>,
    pub resolvers: Arc<ResolverRegistry>,
    pub current_module: Option<ModuleId>,
}

#[derive(Debug, Default, Clone)]
pub struct InterpreterOutcome {
    pub evaluated_constants: HashMap<String, Value>,
    pub diagnostics: Vec<Diagnostic>,
    pub stdout: Vec<String>,
    pub has_errors: bool,
    pub mutations_applied: bool,
    pub closure_types: HashMap<String, Ty>,
}

#[derive(Debug, Clone, PartialEq)]
struct ConstClosure {
    params: Vec<Pattern>,
    ret_ty: Option<Ty>,
    body: Expr,
    captured_values: Vec<HashMap<String, StoredValue>>,
    captured_types: Vec<HashMap<String, Ty>>,
    module_stack: Vec<String>,
    function_ty: Option<Ty>,
}

#[derive(Debug, Clone)]
enum StoredValue {
    Plain(Value),
    Shared(Arc<Mutex<Value>>),
}

impl StoredValue {
    fn value(&self) -> Value {
        match self {
            StoredValue::Plain(value) => value.clone(),
            StoredValue::Shared(shared) => shared
                .lock()
                .map(|value| value.clone())
                .unwrap_or_else(|err| err.into_inner().clone()),
        }
    }

    fn shared(value: Value) -> Self {
        StoredValue::Shared(Arc::new(Mutex::new(value)))
    }

    fn shared_handle(&self) -> Option<Arc<Mutex<Value>>> {
        match self {
            StoredValue::Shared(shared) => Some(Arc::clone(shared)),
            StoredValue::Plain(_) => None,
        }
    }

    fn assign(&mut self, value: Value) -> bool {
        match self {
            StoredValue::Plain(_) => false,
            StoredValue::Shared(shared) => {
                match shared.lock() {
                    Ok(mut guard) => {
                        *guard = value;
                    }
                    Err(err) => {
                        *err.into_inner() = value;
                    }
                }
                true
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct ImportedSymbol {
    module: ModuleId,
    symbol: SymbolDescriptor,
}

#[derive(Debug, Clone, PartialEq)]
struct ImportedModule {
    module: ModuleId,
}

impl PartialEq for StoredValue {
    fn eq(&self, other: &Self) -> bool {
        self.value() == other.value()
    }
}

#[derive(Debug, Clone)]
struct ImplContext {
    self_ty: Option<String>,
    trait_ty: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
struct RuntimeEnum {
    enum_name: String,
    variant_name: String,
    payload: Option<Value>,
    discriminant: Option<i64>,
}

#[derive(Debug, Clone)]
enum EnumVariantPayload {
    Unit,
    Tuple(usize),
    Struct(Vec<Ident>),
}

#[derive(Debug, Clone)]
struct EnumVariantInfo {
    enum_name: String,
    variant_name: String,
    payload: EnumVariantPayload,
    discriminant: Option<i64>,
}

#[derive(Debug, Clone)]
struct MutableConstTarget {
    expr_ptr: *mut Expr,
    ty: Option<Ty>,
}

#[derive(Debug, Clone)]
enum StructLiteralType {
    Struct(TypeStruct),
    Structural(TypeStructural),
}

#[derive(Debug, Clone)]
enum RuntimeFlow {
    Value(Value),
    Break(Option<Value>),
    Continue,
    Return(Option<Value>),
    Panic(Value),
}

#[derive(Debug, Clone)]
struct ReceiverBinding {
    value: Value,
    shared: Option<Arc<Mutex<Value>>>,
}

#[derive(Debug, Clone)]
struct GenericTemplate {
    function: ItemDefFunction,
    generics: Vec<String>,
}

#[derive(Debug, Clone)]
struct ImportDirective {
    module_spec: String,
    binding: ImportBinding,
}

#[derive(Debug, Clone)]
enum ImportBinding {
    Module { alias: Option<String> },
    Symbol { name: String, alias: Option<String> },
    Glob,
}

#[derive(Debug, Clone)]
enum ImportSegment {
    Root,
    SelfMod,
    Super,
    Crate,
    Ident(String),
}

pub struct AstInterpreter<'ctx> {
    ctx: &'ctx SharedScopedContext,
    diag_manager: Option<Arc<DiagnosticManager>>,
    intrinsics: IntrinsicsRegistry,
    mode: InterpreterMode,
    debug_assertions: bool,
    diagnostic_context: &'static str,
    module_resolution: Option<ModuleResolutionContext>,

    module_stack: Vec<String>,
    value_env: Vec<HashMap<String, StoredValue>>,
    type_env: Vec<HashMap<String, Ty>>,
    global_types: HashMap<String, Ty>,
    diagnostics: Vec<Diagnostic>,
    has_errors: bool,
    evaluated_constants: HashMap<String, Value>,
    mutable_const_targets: HashMap<String, MutableConstTarget>,
    stdout: Vec<String>,
    functions: HashMap<String, ItemDefFunction>,
    generic_functions: HashMap<String, GenericTemplate>,
    specialization_cache: HashMap<String, HashMap<String, String>>,
    specialization_counter: HashMap<String, usize>,
    pending_items: Vec<Vec<Item>>,
    pending_stmt_splices: Vec<Vec<BlockStmt>>,
    mutations_applied: bool,
    pending_closure: Option<ConstClosure>,
    pending_expr_ty: Option<Ty>,
    closure_types: HashMap<String, Ty>,
    typer: Option<AstTypeInferencer<'ctx>>,
    in_const_region: usize,
    impl_stack: Vec<ImplContext>,
    trait_impls: HashMap<String, HashSet<String>>,
    impl_methods: HashMap<String, HashMap<String, ItemDefFunction>>,
    trait_methods: HashMap<String, HashMap<String, ItemDefFunction>>,
    enum_variants: HashMap<String, EnumVariantInfo>,
    imported_modules: HashMap<String, ModuleId>,
    imported_symbols: HashMap<String, SymbolDescriptor>,
    imported_types: HashSet<String>,
    loop_depth: usize,
    function_depth: usize,
}

impl<'ctx> AstInterpreter<'ctx> {
    pub fn new(ctx: &'ctx SharedScopedContext, options: InterpreterOptions) -> Self {
        let in_const_region = if matches!(options.mode, InterpreterMode::CompileTime) {
            1
        } else {
            0
        };
        Self {
            ctx,
            diag_manager: options.diagnostics.clone(),
            intrinsics: IntrinsicsRegistry::new(),
            mode: options.mode,
            debug_assertions: options.debug_assertions,
            diagnostic_context: options.diagnostic_context,
            module_resolution: options.module_resolution.clone(),
            module_stack: Vec::new(),
            value_env: vec![HashMap::new()],
            type_env: vec![HashMap::new()],
            global_types: HashMap::new(),
            diagnostics: Vec::new(),
            has_errors: false,
            evaluated_constants: HashMap::new(),
            mutable_const_targets: HashMap::new(),
            stdout: Vec::new(),
            functions: HashMap::new(),
            generic_functions: HashMap::new(),
            specialization_cache: HashMap::new(),
            specialization_counter: HashMap::new(),
            pending_items: Vec::new(),
            pending_stmt_splices: Vec::new(),
            mutations_applied: false,
            pending_closure: None,
            pending_expr_ty: None,
            closure_types: HashMap::new(),
            typer: None,
            in_const_region,
            impl_stack: Vec::new(),
            trait_impls: HashMap::new(),
            impl_methods: HashMap::new(),
            trait_methods: HashMap::new(),
            enum_variants: HashMap::new(),
            imported_modules: HashMap::new(),
            imported_symbols: HashMap::new(),
            imported_types: HashSet::new(),
            loop_depth: 0,
            function_depth: 0,
        }
    }

    pub fn set_typer(&mut self, typer: AstTypeInferencer<'ctx>) {
        self.typer = Some(typer);
    }

    pub fn interpret(&mut self, node: &mut Node) {
        match node.kind_mut() {
            NodeKind::File(file) => {
                self.pending_items.push(Vec::new());
                let mut idx = 0;
                while idx < file.items.len() {
                    self.evaluate_item(&mut file.items[idx]);
                    if matches!(self.mode, InterpreterMode::CompileTime)
                        && matches!(
                            file.items.get(idx).map(|item| item.kind()),
                            Some(ItemKind::DefFunction(func))
                                if func.sig.params.iter().any(|param| param.is_const)
                        )
                    {
                        file.items.remove(idx);
                        self.mark_mutated();
                        continue;
                    }
                    if matches!(self.mode, InterpreterMode::CompileTime)
                        && is_quote_only_item(&file.items[idx])
                    {
                        file.items.remove(idx);
                        self.mark_mutated();
                        continue;
                    }
                    let pending = self.pending_items.last_mut().and_then(|items| {
                        if items.is_empty() {
                            None
                        } else {
                            Some(std::mem::take(items))
                        }
                    });
                    if let Some(pending) = pending {
                        let insert_at = idx + 1;
                        let count = pending.len();
                        file.items.splice(insert_at..insert_at, pending);
                        self.mark_mutated();
                        idx += count;
                    }
                    idx += 1;
                }
                self.pending_items.pop();
            }
            NodeKind::Item(item) => self.evaluate_item(item),
            NodeKind::Expr(expr) => {
                self.eval_expr(expr);
            }
            NodeKind::Query(_) => {
                self.emit_error("Query documents cannot be interpreted");
            }
            NodeKind::Schema(_) => {
                self.emit_error("Schema documents cannot be interpreted");
            }
            NodeKind::Workspace(_) => {
                self.emit_error("Workspace documents cannot be interpreted");
            }
        }
    }

    pub fn execute_main(&mut self) -> Option<Value> {
        let function = self.functions.get("main").cloned()?;
        match self.mode {
            InterpreterMode::CompileTime => {
                let mut body_expr = (*function.body).clone();
                self.push_scope();
                let value = self.eval_expr(&mut body_expr);
                self.pop_scope();
                Some(value)
            }
            InterpreterMode::RunTime => {
                let flow = self.call_function_runtime(function, Vec::new());
                Some(self.finish_runtime_flow(flow))
            }
        }
    }

    pub fn take_outcome(&mut self) -> InterpreterOutcome {
        InterpreterOutcome {
            evaluated_constants: std::mem::take(&mut self.evaluated_constants),
            diagnostics: std::mem::take(&mut self.diagnostics),
            stdout: std::mem::take(&mut self.stdout),
            has_errors: self.has_errors,
            mutations_applied: std::mem::take(&mut self.mutations_applied),
            closure_types: std::mem::take(&mut self.closure_types),
        }
    }

    pub fn has_errors(&self) -> bool {
        self.has_errors
    }

    fn mark_mutated(&mut self) {
        self.mutations_applied = true;
    }

    fn in_std_module(&self) -> bool {
        self.module_stack
            .first()
            .map(|m| m == "std")
            .unwrap_or(false)
    }

    fn append_pending_items(&mut self, items: Vec<Item>) {
        if let Some(scope_pending) = self.pending_items.last_mut() {
            scope_pending.extend(items);
        } else {
            self.pending_items.push(items);
        }
    }

    pub fn evaluate_expression(&mut self, expr: &mut Expr) -> Value {
        self.push_scope();
        let value = self.eval_expr_with_mode(expr);
        self.pop_scope();
        value
    }

    fn eval_expr_with_mode(&mut self, expr: &mut Expr) -> Value {
        match self.mode {
            InterpreterMode::CompileTime => self.eval_expr(expr),
            InterpreterMode::RunTime => {
                let flow = self.eval_expr_runtime(expr);
                self.finish_runtime_flow(flow)
            }
        }
    }

    fn attr_to_locator(&mut self, attr: &Attribute) -> Option<Locator> {
        match &attr.meta {
            AttrMeta::Path(path) => Some(Locator::path(path.clone())),
            _ => {
                self.emit_error("attribute must be a simple path");
                None
            }
        }
    }

    fn fallback_attr_locator(&self, locator: &Locator) -> Option<Locator> {
        let ident = locator.as_ident()?;
        let name = ident.as_str();
        if name != "test" && name != "bench" {
            return None;
        }
        let module = if name == "bench" { "bench" } else { "test" };
        let path = Path::new(vec![
            Ident::new("std"),
            Ident::new(module),
            Ident::new(name),
        ]);
        Some(Locator::path(path))
    }

    fn apply_item_attributes(&mut self, item: &mut Item, attrs: &[Attribute]) -> bool {
        if attrs.is_empty() {
            return false;
        }
        if !self.in_const_region() && !matches!(self.mode, InterpreterMode::CompileTime) {
            self.emit_error("attributes require const evaluation");
            return true;
        }

        let mut quoted_item = item.clone();
        if let ItemKind::DefFunction(func) = quoted_item.kind_mut() {
            func.attrs.clear();
        }

        let quote = ExprQuote {
            block: ExprBlock::new_stmts(vec![BlockStmt::Item(Box::new(quoted_item))]),
            kind: Some(QuoteFragmentKind::Item),
        };
        let mut expr = Expr::new(ExprKind::Quote(quote));

        for attr in attrs {
            let Some(mut locator) = self.attr_to_locator(attr) else {
                return true;
            };
            let mut invoke = ExprInvoke {
                target: ExprInvokeTarget::Function(locator.clone()),
                args: vec![expr],
            };
            let function = if let Some(function) =
                self.resolve_function_call(&mut locator, &mut invoke.args)
            {
                Some(function)
            } else if let Some(fallback) = self.fallback_attr_locator(&locator) {
                locator = fallback;
                invoke.target = ExprInvokeTarget::Function(locator.clone());
                self.resolve_function_call(&mut locator, &mut invoke.args)
            } else {
                None
            };
            let Some(function) = function else {
                self.emit_error(format!("attribute function `{}` not found", locator));
                return true;
            };
            if !function.sig.is_const {
                self.emit_error(format!("attribute function `{}` must be const", locator));
                return true;
            }
            invoke.target = ExprInvokeTarget::Function(locator);
            expr = Expr::new(ExprKind::Invoke(invoke));
        }

        let mut token_expr = expr;
        let Some(fragments) = self.resolve_splice_fragments(&mut token_expr) else {
            return true;
        };
        let mut pending = Vec::new();
        for fragment in fragments {
            match fragment {
                QuotedFragment::Items(items) => pending.extend(items),
                _ => {
                    self.emit_error("attribute expansion must return item fragments");
                    return true;
                }
            }
        }

        if !pending.is_empty() {
            self.append_pending_items(pending);
        }
        *item = Item::unit();
        self.mark_mutated();
        true
    }

    fn evaluate_item(&mut self, item: &mut Item) {
        match item.kind_mut() {
            ItemKind::Macro(_mac) => {
                // Item macros are compile-time constructs; interpreter skips them.
            }
            ItemKind::DefStruct(def) => {
                let ty = Ty::Struct(def.value.clone());
                self.insert_type(def.name.as_str(), ty);
            }
            ItemKind::DefStructural(def) => {
                let ty = Ty::Structural(def.value.clone());
                self.insert_type(def.name.as_str(), ty);
            }
            ItemKind::DefEnum(def) => {
                let ty = Ty::Enum(def.value.clone());
                self.insert_type(def.name.as_str(), ty);

                // Make unit enum variants available to const evaluation (e.g. `Enum::Variant as i32`).
                // We approximate enum values by their discriminant integer.
                let mut next_discriminant: i64 = 0;
                for variant in &mut def.value.variants {
                    let payload = self.enum_payload_from_ty(&variant.value);
                    let mut discriminant_value: Option<i64> = None;
                    if let Some(discriminant_expr) = variant.discriminant.as_mut() {
                        let mut disc_expr = discriminant_expr.get().clone();
                        let disc_value = self.eval_expr(&mut disc_expr);
                        if let Value::Int(int_value) = disc_value {
                            next_discriminant = int_value.value;
                            discriminant_value = Some(next_discriminant);
                        } else {
                            self.emit_error(format!(
                                "enum discriminant for {}::{} must be an integer",
                                def.name, variant.name
                            ));
                        }
                    } else {
                        discriminant_value = Some(next_discriminant);
                    }

                    let qualified = self.qualified_name(&format!("{}::{}", def.name, variant.name));
                    self.evaluated_constants
                        .insert(qualified, Value::int(next_discriminant));
                    let enum_key = self.qualified_name(&format!("{}::{}", def.name, variant.name));
                    self.enum_variants.insert(
                        enum_key,
                        EnumVariantInfo {
                            enum_name: def.name.as_str().to_string(),
                            variant_name: variant.name.as_str().to_string(),
                            payload,
                            discriminant: discriminant_value,
                        },
                    );
                    next_discriminant += 1;
                }
            }
            ItemKind::DefType(def) => {
                self.evaluate_ty(&mut def.value);
                self.insert_type(def.name.as_str(), def.value.clone());
            }
            ItemKind::DefConst(def) => {
                let is_mutable = def.mutable.unwrap_or(false);
                if is_mutable && !matches!(self.mode, InterpreterMode::CompileTime) {
                    self.emit_error("const mut is only supported during const evaluation");
                    return;
                }
                if let Some(inner_ty) = def.ty_annotation_mut().as_mut() {
                    self.evaluate_ty(inner_ty);
                }
                if let Some(inner_ty) = def.ty.as_mut() {
                    self.evaluate_ty(inner_ty);
                }
                let value = {
                    let expr_ref = def.value.as_mut();
                    self.eval_expr(expr_ref)
                };
                if self.pending_closure.is_some() {
                    let mut function_ty = {
                        let expr_ref = def.value.as_mut();
                        self.annotate_pending_closure(None, Some(expr_ref));
                        expr_ref.ty().cloned()
                    };
                    if function_ty.is_none() {
                        function_ty = def.ty.clone().or_else(|| def.ty_annotation().cloned());
                    }

                    if let Some(fn_ty) = function_ty {
                        def.ty = Some(fn_ty.clone());
                        def.ty_annotation = Some(fn_ty.clone());
                        def.value.set_ty(fn_ty.clone());
                    }
                }
                let qualified = self.qualified_name(def.name.as_str());
                if is_mutable {
                    self.insert_mutable_value(def.name.as_str(), value.clone());
                } else {
                    self.insert_value(def.name.as_str(), value.clone());
                }
                self.evaluated_constants
                    .insert(qualified.clone(), value.clone());

                // Preserve unevaluated placeholders (e.g., closures) so later lowering
                // can still see the original expression shape.
                let mut should_replace =
                    !matches!(value, Value::Undefined(_) | Value::Unit(_) | Value::Any(_));
                let is_const_block_expr = matches!(def.value.kind(), ExprKind::ConstBlock(_));
                if is_mutable && !matches!(value, Value::Undefined(_) | Value::Any(_)) {
                    should_replace = true;
                }
                if is_const_block_expr && !matches!(value, Value::Undefined(_) | Value::Any(_)) {
                    // Const blocks must not reach AST→HIR; materialize their evaluated value.
                    should_replace = true;
                }
                if !is_mutable && !is_const_block_expr {
                    if matches!(value, Value::Map(_)) {
                        should_replace = false;
                    }
                    if matches!(value, Value::List(_)) {
                        let mut is_array = false;
                        if let Some(ty) = def.ty_annotation().or_else(|| def.ty.as_ref()) {
                            is_array = matches!(ty, Ty::Array(_));
                        }
                        if !is_array {
                            should_replace = false;
                        }
                    }
                }

                if should_replace {
                    let mut expr_value = Expr::value(value);
                    if let Some(ty) = def.ty.clone().or_else(|| def.ty_annotation().cloned()) {
                        expr_value.set_ty(ty);
                    }
                    *def.value = expr_value;
                    self.mark_mutated();
                }
                if is_mutable {
                    let expr_ptr = def.value.as_mut() as *mut Expr;
                    let ty = def.ty.clone().or_else(|| def.ty_annotation().cloned());
                    self.mutable_const_targets
                        .insert(qualified, MutableConstTarget { expr_ptr, ty });
                    def.mutable = None;
                }
            }
            ItemKind::DefStatic(def) => {
                let value = {
                    let expr_ref = def.value.as_mut();
                    self.eval_expr(expr_ref)
                };
                if self.pending_closure.is_some() {
                    let mut function_ty = {
                        let expr_ref = def.value.as_mut();
                        self.annotate_pending_closure(None, Some(expr_ref));
                        expr_ref.ty().cloned()
                    };
                    if function_ty.is_none() {
                        function_ty = def
                            .ty_annotation()
                            .cloned()
                            .or_else(|| Some(def.ty.clone()));
                    }

                    if let Some(fn_ty) = function_ty {
                        def.ty = fn_ty.clone();
                        def.ty_annotation = Some(fn_ty.clone());
                        def.value.set_ty(fn_ty.clone());
                    }
                }
                self.insert_value(def.name.as_str(), value.clone());
                if !matches!(value, Value::Any(_) | Value::Undefined(_) | Value::Unit(_)) {
                    let mut expr_value = Expr::value(value.clone());
                    expr_value.set_ty(def.ty.clone());
                    *def.value = expr_value;
                    self.mark_mutated();
                }
            }
            ItemKind::Module(module) => {
                self.module_stack.push(module.name.as_str().to_string());
                self.push_scope();
                self.pending_items.push(Vec::new());
                let mut idx = 0;
                while idx < module.items.len() {
                    self.evaluate_item(&mut module.items[idx]);
                    if matches!(self.mode, InterpreterMode::CompileTime)
                        && matches!(
                            module.items.get(idx).map(|item| item.kind()),
                            Some(ItemKind::DefFunction(func))
                                if func.sig.params.iter().any(|param| param.is_const)
                        )
                    {
                        module.items.remove(idx);
                        self.mark_mutated();
                        continue;
                    }
                    if matches!(self.mode, InterpreterMode::CompileTime)
                        && is_quote_only_item(&module.items[idx])
                    {
                        module.items.remove(idx);
                        self.mark_mutated();
                        continue;
                    }
                    let pending = self.pending_items.last_mut().and_then(|items| {
                        if items.is_empty() {
                            None
                        } else {
                            Some(std::mem::take(items))
                        }
                    });
                    if let Some(pending) = pending {
                        let insert_at = idx + 1;
                        let count = pending.len();
                        module.items.splice(insert_at..insert_at, pending);
                        self.mark_mutated();
                        idx += count;
                    }
                    idx += 1;
                }
                self.pending_items.pop();
                self.pop_scope();
                self.module_stack.pop();
            }
            ItemKind::Impl(impl_block) => {
                let context = self.resolve_impl_context(impl_block);
                if let Some(ref context) = context {
                    if let Some(trait_name) = &context.trait_ty {
                        if let Some(self_name) = &context.self_ty {
                            self.trait_impls
                                .entry(self_name.clone())
                                .or_default()
                                .insert(trait_name.clone());
                        }
                    }
                }
                self.impl_stack.push(context.unwrap_or(ImplContext {
                    self_ty: None,
                    trait_ty: None,
                }));
                self.push_scope();
                self.pending_items.push(Vec::new());
                let mut idx = 0;
                while idx < impl_block.items.len() {
                    self.evaluate_item(&mut impl_block.items[idx]);
                    if matches!(self.mode, InterpreterMode::CompileTime)
                        && matches!(
                            impl_block.items.get(idx).map(|item| item.kind()),
                            Some(ItemKind::DefFunction(func))
                                if func.sig.params.iter().any(|param| param.is_const)
                        )
                    {
                        impl_block.items.remove(idx);
                        self.mark_mutated();
                        continue;
                    }
                    if matches!(self.mode, InterpreterMode::CompileTime)
                        && is_quote_only_item(&impl_block.items[idx])
                    {
                        impl_block.items.remove(idx);
                        self.mark_mutated();
                        continue;
                    }
                    let pending = self.pending_items.last_mut().and_then(|items| {
                        if items.is_empty() {
                            None
                        } else {
                            Some(std::mem::take(items))
                        }
                    });
                    if let Some(pending) = pending {
                        let insert_at = idx + 1;
                        let count = pending.len();
                        impl_block.items.splice(insert_at..insert_at, pending);
                        self.mark_mutated();
                        idx += count;
                    }
                    idx += 1;
                }
                self.pending_items.pop();
                self.pop_scope();
                self.impl_stack.pop();
            }
            ItemKind::DefFunction(func) => {
                if matches!(self.mode, InterpreterMode::CompileTime) && !func.attrs.is_empty() {
                    let attrs = func.attrs.clone();
                    let _ = self.apply_item_attributes(item, &attrs);
                    return;
                }
                let base_name = func.name.as_str().to_string();
                let qualified_name = self.qualified_name(func.name.as_str());
                if let Some(context) = self
                    .impl_stack
                    .last()
                    .cloned()
                    .filter(|ctx| ctx.self_ty.is_some())
                {
                    if let Some(self_ty) = context.self_ty {
                        let method_key = format!("{}::{}", self_ty, func.name.as_str());
                        self.impl_methods
                            .entry(self_ty.clone())
                            .or_default()
                            .insert(func.name.as_str().to_string(), func.clone());
                        if func.sig.generics_params.is_empty() {
                            self.functions.insert(method_key.clone(), func.clone());
                        } else {
                            self.generic_functions.insert(
                                method_key.clone(),
                                GenericTemplate {
                                    function: func.clone(),
                                    generics: func
                                        .sig
                                        .generics_params
                                        .iter()
                                        .map(|param| param.name.as_str().to_string())
                                        .collect(),
                                },
                            );
                        }
                    }
                }
                if func.sig.generics_params.is_empty() {
                    self.functions.insert(base_name, func.clone());
                    self.functions.insert(qualified_name, func.clone());
                    let has_const_params = func.sig.params.iter().any(|param| param.is_const);
                    if !has_const_params {
                        self.evaluate_function_body(func.body.as_mut());
                    }
                } else {
                    let generics = func
                        .sig
                        .generics_params
                        .iter()
                        .map(|param| param.name.as_str().to_string())
                        .collect();
                    let template = GenericTemplate {
                        function: func.clone(),
                        generics,
                    };
                    self.generic_functions
                        .insert(func.name.as_str().to_string(), template.clone());
                    self.generic_functions.insert(qualified_name, template);
                }
            }
            ItemKind::Expr(expr) => {
                if matches!(self.mode, InterpreterMode::CompileTime) {
                    if let ExprKind::Splice(splice) = expr.kind_mut() {
                        let Some(fragments) = self.resolve_splice_fragments(splice.token.as_mut())
                        else {
                            return;
                        };
                        let mut pending = Vec::new();
                        for fragment in fragments {
                            match fragment {
                                QuotedFragment::Items(items) => pending.extend(items),
                                _ => {
                                    self.emit_error(
                                        "module-level splice only supports item fragments",
                                    );
                                    return;
                                }
                            }
                        }
                        if !pending.is_empty() {
                            self.append_pending_items(pending);
                            *expr = Expr::unit();
                            self.mark_mutated();
                        }
                        return;
                    }
                }
                self.eval_expr(expr);
            }
            ItemKind::DefTrait(trait_def) => {
                let trait_name = trait_def.name.as_str().to_string();
                let entry = self.trait_methods.entry(trait_name).or_default();
                for member in &trait_def.items {
                    if let ItemKind::DefFunction(func) = member.kind() {
                        entry.insert(func.name.as_str().to_string(), func.clone());
                    }
                }
            }
            ItemKind::DeclConst(_)
            | ItemKind::DeclStatic(_)
            | ItemKind::DeclFunction(_)
            | ItemKind::DeclType(_)
            | ItemKind::Any(_) => {}
            ItemKind::Import(import) => {
                self.handle_import(import);
            }
        }
    }

    // Determine enum payload shape for runtime construction/matching.
    fn enum_payload_from_ty(&self, ty: &Ty) -> EnumVariantPayload {
        match ty {
            Ty::Unit(_) => EnumVariantPayload::Unit,
            Ty::Tuple(tuple) => EnumVariantPayload::Tuple(tuple.types.len()),
            Ty::Struct(def) => EnumVariantPayload::Struct(
                def.fields.iter().map(|field| field.name.clone()).collect(),
            ),
            Ty::Structural(def) => EnumVariantPayload::Struct(
                def.fields.iter().map(|field| field.name.clone()).collect(),
            ),
            _ => EnumVariantPayload::Unit,
        }
    }

    // Extract impl context names to resolve method dispatch and trait bindings.
    fn resolve_impl_context(&self, impl_block: &fp_core::ast::ItemImpl) -> Option<ImplContext> {
        let self_ty = self.type_name_from_expr(&impl_block.self_ty);
        let trait_ty = impl_block
            .trait_ty
            .as_ref()
            .map(|locator| Self::locator_base_name(locator));
        Some(ImplContext { self_ty, trait_ty })
    }

    fn type_name_from_expr(&self, expr: &Expr) -> Option<String> {
        match expr.kind() {
            ExprKind::Locator(locator) => Some(Self::locator_base_name(locator)),
            ExprKind::Reference(reference) => self.type_name_from_expr(reference.referee.as_ref()),
            ExprKind::Paren(paren) => self.type_name_from_expr(paren.expr.as_ref()),
            _ => None,
        }
    }

    fn locator_base_name(locator: &Locator) -> String {
        match locator {
            Locator::Ident(ident) => ident.as_str().trim_start_matches('#').to_string(),
            Locator::Path(path) => path
                .segments
                .last()
                .map(|ident| ident.as_str().trim_start_matches('#').to_string())
                .unwrap_or_default(),
            Locator::ParameterPath(path) => path
                .segments
                .last()
                .map(|seg| seg.ident.as_str().trim_start_matches('#').to_string())
                .unwrap_or_default(),
        }
    }

    fn cast_value_to_type(&mut self, value: Value, target_ty: &Ty) -> Value {
        let primitive_target = match target_ty {
            Ty::Primitive(primitive) => Some(*primitive),
            Ty::Expr(expr) => match expr.kind() {
                ExprKind::Locator(locator) => {
                    let name = match locator {
                        Locator::Ident(ident) => Some(ident.as_str()),
                        Locator::Path(path) if path.segments.len() == 1 => {
                            Some(path.segments[0].as_str())
                        }
                        Locator::ParameterPath(path) if path.segments.len() == 1 => {
                            Some(path.segments[0].ident.as_str())
                        }
                        _ => None,
                    };

                    match name {
                        Some("i64") => Some(TypePrimitive::Int(TypeInt::I64)),
                        Some("u64") => Some(TypePrimitive::Int(TypeInt::U64)),
                        Some("i32") => Some(TypePrimitive::Int(TypeInt::I32)),
                        Some("u32") => Some(TypePrimitive::Int(TypeInt::U32)),
                        Some("i16") => Some(TypePrimitive::Int(TypeInt::I16)),
                        Some("u16") => Some(TypePrimitive::Int(TypeInt::U16)),
                        Some("i8") => Some(TypePrimitive::Int(TypeInt::I8)),
                        Some("u8") => Some(TypePrimitive::Int(TypeInt::U8)),
                        Some("isize") => Some(TypePrimitive::Int(TypeInt::I64)),
                        Some("usize") => Some(TypePrimitive::Int(TypeInt::U64)),
                        Some("bool") => Some(TypePrimitive::Bool),
                        _ => None,
                    }
                }
                _ => None,
            },
            _ => None,
        };

        match primitive_target {
            Some(TypePrimitive::Int(_)) => match value {
                Value::Int(int_val) => Value::int(int_val.value),
                Value::Bool(bool_val) => Value::int(if bool_val.value { 1 } else { 0 }),
                Value::Decimal(decimal_val) => Value::int(decimal_val.value as i64),
                Value::Any(any) => {
                    if let Some(enum_value) = any.downcast_ref::<RuntimeEnum>() {
                        if let Some(discriminant) = enum_value.discriminant {
                            return Value::int(discriminant);
                        }
                    }
                    self.emit_error(format!(
                        "cannot cast value {} to integer type {}",
                        Value::Any(any.clone()),
                        target_ty
                    ));
                    Value::undefined()
                }
                other => {
                    self.emit_error(format!(
                        "cannot cast value {} to integer type {}",
                        other, target_ty
                    ));
                    Value::undefined()
                }
            },
            Some(TypePrimitive::Bool) => match value {
                Value::Bool(bool_val) => Value::bool(bool_val.value),
                Value::Int(int_val) => Value::bool(int_val.value != 0),
                other => {
                    self.emit_error(format!(
                        "cannot cast value {} to bool during const evaluation",
                        other
                    ));
                    Value::undefined()
                }
            },
            _ => {
                self.emit_error(format!(
                    "unsupported cast target type {} in const evaluation",
                    target_ty
                ));
                Value::undefined()
            }
        }
    }

    fn locator_segments(locator: &Locator) -> Vec<String> {
        match locator {
            Locator::Ident(ident) => vec![ident.as_str().to_string()],
            Locator::Path(path) => path
                .segments
                .iter()
                .map(|segment| segment.as_str().to_string())
                .collect(),
            Locator::ParameterPath(path) => path
                .segments
                .iter()
                .map(|segment| segment.ident.as_str().to_string())
                .collect(),
        }
    }

    /// Helper to annotate a slice of arguments
    fn annotate_invoke_args_slice(&mut self, args: &mut [Expr], params: &[FunctionParam]) {
        for (arg, param) in args.iter_mut().zip(params.iter()) {
            let should_annotate = match arg.ty() {
                None => true,
                Some(Ty::Unknown(_)) => true,
                Some(Ty::Function(fn_ty)) => {
                    // Replace function types with Unknown parameters
                    fn_ty.params.iter().any(|p| matches!(p, Ty::Unknown(_)))
                        || fn_ty
                            .ret_ty
                            .as_ref()
                            .map_or(false, |r| matches!(r.as_ref(), Ty::Unknown(_)))
                }
                _ => false,
            };

            if should_annotate {
                arg.set_ty(param.ty.clone());
            }
        }
    }

    // removed unused apply_callable (no callers)

    fn instantiate_generic_function(
        &mut self,
        lookup_name: &str,
        template: GenericTemplate,
        locator: &mut Locator,
        args: &[Expr],
    ) -> Option<ItemDefFunction> {
        let base_name = template.function.name.as_str().to_string();
        let substitution = match self.build_generic_substitution(&template, args) {
            Some(subst) => subst,
            None => {
                return None;
            }
        };
        let key = self.build_specialization_key(&template.generics, &substitution)?;

        if let Some(existing) = self.lookup_cached_specialization(&[lookup_name, &base_name], &key)
        {
            self.update_locator_name(locator, &existing);
            self.mark_mutated();
            return self.functions.get(&existing).cloned();
        }

        let sanitized_base = Self::sanitize_symbol(&base_name);
        let counter_entry = self
            .specialization_counter
            .entry(base_name.clone())
            .or_insert(0);
        let mut new_name;
        loop {
            new_name = format!("{}__spec{}", sanitized_base, *counter_entry);
            *counter_entry += 1;
            if !self.functions.contains_key(&new_name) {
                break;
            }
        }

        let mut specialized = template.function.clone();
        specialized.name = Ident::new(new_name.clone());
        specialized.sig.name = Some(specialized.name.clone());
        specialized.sig.generics_params.clear();

        for param in &mut specialized.sig.params {
            let substituted = self.substitute_ty(&param.ty, &substitution);
            param.ty = substituted.clone();
            param.ty_annotation = Some(substituted);
        }

        if let Some(ret_ty) = specialized.sig.ret_ty.as_mut() {
            let substituted = self.substitute_ty(ret_ty, &substitution);
            *ret_ty = substituted;
        }

        let function_type = TypeFunction {
            params: specialized
                .sig
                .params
                .iter()
                .map(|param| param.ty.clone())
                .collect(),
            generics_params: Vec::new(),
            ret_ty: specialized
                .sig
                .ret_ty
                .as_ref()
                .map(|ty| Box::new(ty.clone())),
        };
        let function_ty = Ty::Function(function_type.clone());
        specialized.ty = Some(function_type.clone());
        specialized.ty_annotation = Some(function_ty.clone());

        let mut local_types: HashMap<String, Ty> = HashMap::new();
        for param in &specialized.sig.params {
            let ty = param.ty.clone();
            local_types.insert(param.name.as_str().to_string(), ty.clone());
            local_types.insert(format!("#{}", param.name.as_str()), ty);
        }

        self.rewrite_expr_types(specialized.body.as_mut(), &substitution, &local_types);

        let mut new_item = Item::new(ItemKind::DefFunction(specialized.clone()));
        new_item.set_ty(function_ty);
        if let Some(scope_pending) = self.pending_items.last_mut() {
            scope_pending.push(new_item);
        } else {
            self.pending_items.push(vec![new_item]);
        }

        self.functions.insert(new_name.clone(), specialized.clone());
        let qualified_new = self.qualified_name(&new_name);
        self.functions.insert(qualified_new, specialized.clone());

        self.register_specialization(&base_name, lookup_name, key, &new_name);
        self.update_locator_name(locator, &new_name);
        self.mark_mutated();

        Some(specialized)
    }

    /// Specialize a generic function reference based on expected type
    /// Used when a generic function is referenced (not called) and we have type information
    fn specialize_function_reference(
        &mut self,
        locator: &mut Locator,
        expected_ty: &Ty,
    ) -> Option<ItemDefFunction> {
        // Extract function name
        let lookup_name = locator.to_string();
        let candidate_names = if let Some(ident) = locator.as_ident() {
            vec![lookup_name.clone(), ident.as_str().to_string()]
        } else {
            vec![lookup_name.clone()]
        };

        // Check if this is a generic function
        let (name, template) = candidate_names.iter().find_map(|name| {
            self.generic_functions
                .get(name)
                .cloned()
                .map(|t| (name.clone(), t))
        })?;

        // Extract parameter types from expected function type
        let expected_fn_ty = match expected_ty {
            Ty::Function(fn_ty) => fn_ty,
            _ => return None,
        };

        // Build substitution from expected function type
        let substitution = self.build_generic_substitution_from_type(&template, expected_fn_ty)?;

        let base_name = template.function.name.as_str().to_string();
        let key = self.build_specialization_key(&template.generics, &substitution)?;

        // Check if already specialized
        if let Some(existing) = self.lookup_cached_specialization(&[&name, &base_name], &key) {
            self.update_locator_name(locator, &existing);
            self.mark_mutated();
            return self.functions.get(&existing).cloned();
        }

        // Create new specialization
        let sanitized_base = Self::sanitize_symbol(&base_name);
        let counter_entry = self
            .specialization_counter
            .entry(base_name.clone())
            .or_insert(0);
        let mut new_name;
        loop {
            new_name = format!("{}__spec{}", sanitized_base, *counter_entry);
            *counter_entry += 1;
            if !self.functions.contains_key(&new_name) {
                break;
            }
        }

        let mut specialized = template.function.clone();
        specialized.name = Ident::new(new_name.clone());
        specialized.sig.name = Some(specialized.name.clone());
        specialized.sig.generics_params.clear();

        // Substitute parameter types
        for param in &mut specialized.sig.params {
            let substituted = self.substitute_ty(&param.ty, &substitution);
            param.ty = substituted.clone();
            param.ty_annotation = Some(substituted);
        }

        // Substitute return type
        if let Some(ret_ty) = specialized.sig.ret_ty.as_mut() {
            let substituted = self.substitute_ty(ret_ty, &substitution);
            *ret_ty = substituted;
        }

        let function_type = TypeFunction {
            params: specialized
                .sig
                .params
                .iter()
                .map(|param| param.ty.clone())
                .collect(),
            generics_params: Vec::new(),
            ret_ty: specialized
                .sig
                .ret_ty
                .as_ref()
                .map(|ty| Box::new(ty.clone())),
        };
        let function_ty = Ty::Function(function_type.clone());
        specialized.ty = Some(function_type.clone());
        specialized.ty_annotation = Some(function_ty.clone());

        let mut local_types: HashMap<String, Ty> = HashMap::new();
        for param in &specialized.sig.params {
            let ty = param.ty.clone();
            local_types.insert(param.name.as_str().to_string(), ty.clone());
            local_types.insert(format!("#{}", param.name.as_str()), ty);
        }

        self.rewrite_expr_types(specialized.body.as_mut(), &substitution, &local_types);

        let mut new_item = Item::new(ItemKind::DefFunction(specialized.clone()));
        new_item.set_ty(function_ty);
        if let Some(scope_pending) = self.pending_items.last_mut() {
            scope_pending.push(new_item);
        } else {
            self.pending_items.push(vec![new_item]);
        }

        self.functions.insert(new_name.clone(), specialized.clone());
        let qualified_new = self.qualified_name(&new_name);
        self.functions.insert(qualified_new, specialized.clone());

        self.register_specialization(&base_name, &name, key, &new_name);
        self.update_locator_name(locator, &new_name);
        self.mark_mutated();

        Some(specialized)
    }

    /// Build generic substitution from expected function type
    fn build_generic_substitution_from_type(
        &self,
        template: &GenericTemplate,
        expected_fn_ty: &TypeFunction,
    ) -> Option<HashMap<String, Ty>> {
        let generics_set: HashSet<String> = template.generics.iter().cloned().collect();
        let mut subst = HashMap::new();

        // Match parameter types
        if template.function.sig.params.len() != expected_fn_ty.params.len() {
            return None;
        }

        for (param, expected_ty) in template
            .function
            .sig
            .params
            .iter()
            .zip(&expected_fn_ty.params)
        {
            if !self.match_type(&param.ty, expected_ty, &generics_set, &mut subst) {
                return None;
            }
        }

        // Match return type if specified
        if let (Some(template_ret), Some(expected_ret)) =
            (&template.function.sig.ret_ty, &expected_fn_ty.ret_ty)
        {
            if !self.match_type(template_ret, expected_ret, &generics_set, &mut subst) {
                return None;
            }
        }

        // Verify all generics are resolved
        if template
            .generics
            .iter()
            .all(|name| subst.contains_key(name))
        {
            Some(subst)
        } else {
            None
        }
    }

    fn build_generic_substitution(
        &self,
        template: &GenericTemplate,
        args: &[Expr],
    ) -> Option<HashMap<String, Ty>> {
        let generics_set: HashSet<String> = template.generics.iter().cloned().collect();
        let mut subst = HashMap::new();
        for (param, arg) in template.function.sig.params.iter().zip(args.iter()) {
            let Some(arg_ty) = arg.ty().cloned() else {
                continue;
            };
            if !self.match_type(&param.ty, &arg_ty, &generics_set, &mut subst) {
                return None;
            }
        }

        if template
            .generics
            .iter()
            .all(|name| subst.contains_key(name))
        {
            Some(subst)
        } else {
            None
        }
    }

    fn build_specialization_key(
        &self,
        generics: &[String],
        subst: &HashMap<String, Ty>,
    ) -> Option<String> {
        let mut parts = Vec::with_capacity(generics.len());
        for name in generics {
            let ty = subst.get(name)?;
            parts.push(format!("{}={}", name, ty));
        }
        Some(parts.join(";"))
    }

    fn lookup_cached_specialization(&self, names: &[&str], key: &str) -> Option<String> {
        for name in names {
            if let Some(cache) = self.specialization_cache.get(*name) {
                if let Some(symbol) = cache.get(key) {
                    return Some(symbol.clone());
                }
            }
        }
        None
    }

    fn register_specialization(
        &mut self,
        base_name: &str,
        lookup_name: &str,
        key: String,
        symbol: &str,
    ) {
        self.specialization_cache
            .entry(base_name.to_string())
            .or_default()
            .insert(key.clone(), symbol.to_string());
        if lookup_name != base_name {
            self.specialization_cache
                .entry(lookup_name.to_string())
                .or_default()
                .insert(key, symbol.to_string());
        }
    }

    fn update_locator_name(&self, locator: &mut Locator, new_name: &str) {
        let mut path = locator.to_path();
        if let Some(last) = path.segments.last_mut() {
            *last = Ident::new(new_name.to_string());
            *locator = Locator::path(path);
        }
    }

    fn sanitize_locator(locator: &mut Locator) -> String {
        match locator {
            Locator::Ident(ident) => {
                let current = ident.as_str().to_string();
                if let Some(trimmed) = current.strip_prefix('#') {
                    let trimmed_owned = trimmed.to_string();
                    *ident = Ident::new(trimmed_owned.clone());
                    trimmed_owned
                } else {
                    current
                }
            }
            Locator::Path(path) => {
                if let Some(last) = path.segments.last_mut() {
                    let current = last.as_str().to_string();
                    if let Some(trimmed) = current.strip_prefix('#') {
                        let trimmed_owned = trimmed.to_string();
                        *last = Ident::new(trimmed_owned.clone());
                        trimmed_owned
                    } else {
                        current
                    }
                } else {
                    String::new()
                }
            }
            Locator::ParameterPath(path) => {
                if let Some(last) = path.segments.last_mut() {
                    let current = last.ident.as_str().to_string();
                    if let Some(trimmed) = current.strip_prefix('#') {
                        let trimmed_owned = trimmed.to_string();
                        last.ident = Ident::new(trimmed_owned.clone());
                        trimmed_owned
                    } else {
                        current
                    }
                } else {
                    String::new()
                }
            }
        }
    }

    fn locator_key(locator: &Locator) -> String {
        match locator {
            Locator::Ident(ident) => ident.as_str().trim_start_matches('#').to_string(),
            Locator::Path(path) => path
                .segments
                .last()
                .map(|ident| ident.as_str().trim_start_matches('#').to_string())
                .unwrap_or_default(),
            Locator::ParameterPath(path) => path
                .segments
                .last()
                .map(|segment| segment.ident.as_str().trim_start_matches('#').to_string())
                .unwrap_or_default(),
        }
    }

    // === Environment helpers (reintroduced after refactor) ===
    // closure helpers moved to closures.rs

    fn sanitize_symbol(name: &str) -> String {
        let mut result = String::with_capacity(name.len());
        for ch in name.chars() {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                result.push(ch);
            } else {
                result.push('_');
            }
        }
        if result.is_empty() {
            result.push_str("specialized_fn");
        }
        if result
            .chars()
            .next()
            .map(|c| c.is_ascii_digit())
            .unwrap_or(false)
        {
            result.insert(0, '_');
        }
        result
    }

    // === Operators (reintroduced for eval_expr) ===

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
            self.record_generic_binding(subst, &name, actual);
            return true;
        }

        match (pattern, actual) {
            (Ty::Primitive(p), Ty::Primitive(a)) => p == a,
            (Ty::Unit(_), Ty::Unit(_)) => true,
            (Ty::Nothing(_), Ty::Nothing(_)) => true,
            (Ty::Struct(p), Ty::Struct(a)) => self.match_struct_type(p, a),
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
            (Ty::Array(p), Ty::Array(a)) => self.match_type(&p.elem, &a.elem, generics, subst),
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
            (Ty::Expr(_), Ty::Expr(_)) => pattern == actual,
            (_, other) => self.is_unknown(other) || pattern == other,
        }
    }

    fn match_struct_type(&self, pattern: &TypeStruct, actual: &TypeStruct) -> bool {
        pattern.name == actual.name
    }

    fn record_generic_binding(&self, subst: &mut HashMap<String, Ty>, name: &str, ty: &Ty) {
        subst.insert(name.to_string(), ty.clone());
        if !name.starts_with('#') {
            subst.insert(format!("#{}", name), ty.clone());
        }
    }

    fn substitute_ty(&self, ty: &Ty, subst: &HashMap<String, Ty>) -> Ty {
        if let Ty::Expr(expr) = ty {
            if let ExprKind::Locator(locator) = expr.kind() {
                let name = Self::locator_key(locator);
                if let Some(mapped) = subst.get(&name) {
                    return mapped.clone();
                }
                if let Some(mapped) = subst.get(&format!("#{}", name)) {
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
            Ty::Array(array) => Ty::Array(TypeArray {
                elem: Box::new(self.substitute_ty(&array.elem, subst)),
                len: array.len.clone(),
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
            Ty::Quote(quote) => Ty::Quote(TypeQuote {
                kind: quote.kind,
                item: quote.item,
                inner: quote
                    .inner
                    .as_ref()
                    .map(|inner| Box::new(self.substitute_ty(inner.as_ref(), subst))),
            }),
            Ty::Structural(structural) => Ty::Structural(structural.clone()),
            Ty::Enum(enm) => Ty::Enum(enm.clone()),
            Ty::ImplTraits(_)
            | Ty::TypeBounds(_)
            | Ty::Type(_)
            | Ty::Value(_)
            | Ty::TypeBinaryOp(_)
            | Ty::AnyBox(_) => ty.clone(),
        }
    }

    fn substitute_struct(&self, ty: &TypeStruct, _subst: &HashMap<String, Ty>) -> TypeStruct {
        ty.clone()
    }

    fn generic_name(&self, ty: &Ty, generics: &HashSet<String>) -> Option<String> {
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

    fn rewrite_expr_types(
        &self,
        expr: &mut Expr,
        subst: &HashMap<String, Ty>,
        local_types: &HashMap<String, Ty>,
    ) {
        let mut rewritten_ty = expr.ty().cloned().map(|ty| self.substitute_ty(&ty, subst));

        let needs_inference = rewritten_ty.as_ref().map_or(true, |ty| self.is_unknown(ty));

        if needs_inference {
            match expr.kind_mut() {
                ExprKind::Locator(locator) => {
                    if let Some(local) = self.resolve_local_ty(locator, local_types) {
                        rewritten_ty = Some(local);
                    }
                }
                ExprKind::Value(value) => {
                    if matches!(**value, Value::String(_)) {
                        rewritten_ty = Some(Ty::Primitive(TypePrimitive::String));
                    }
                }
                _ => {}
            }
        }

        if let Some(ty) = rewritten_ty {
            expr.set_ty(ty);
        }

        let expr_type_unknown = expr.ty().map_or(true, |ty| self.is_unknown(ty));

        match expr.kind_mut() {
            ExprKind::Block(block) => {
                for stmt in &mut block.stmts {
                    if let BlockStmt::Expr(expr_stmt) = stmt {
                        self.rewrite_expr_types(expr_stmt.expr.as_mut(), subst, local_types);
                    }
                }
            }
            ExprKind::If(expr_if) => {
                self.rewrite_expr_types(expr_if.cond.as_mut(), subst, local_types);
                self.rewrite_expr_types(expr_if.then.as_mut(), subst, local_types);
                if let Some(elze) = expr_if.elze.as_mut() {
                    self.rewrite_expr_types(elze, subst, local_types);
                }
            }
            ExprKind::Loop(expr_loop) => {
                self.rewrite_expr_types(expr_loop.body.as_mut(), subst, local_types);
            }
            ExprKind::While(expr_while) => {
                self.rewrite_expr_types(expr_while.cond.as_mut(), subst, local_types);
                self.rewrite_expr_types(expr_while.body.as_mut(), subst, local_types);
            }
            ExprKind::Match(expr_match) => {
                for case in &mut expr_match.cases {
                    self.rewrite_expr_types(case.cond.as_mut(), subst, local_types);
                    self.rewrite_expr_types(case.body.as_mut(), subst, local_types);
                }
            }
            ExprKind::Let(expr_let) => {
                self.rewrite_expr_types(expr_let.expr.as_mut(), subst, local_types);
            }
            ExprKind::Assign(assign) => {
                self.rewrite_expr_types(assign.target.as_mut(), subst, local_types);
                self.rewrite_expr_types(assign.value.as_mut(), subst, local_types);
            }
            ExprKind::Invoke(invoke) => {
                match &mut invoke.target {
                    ExprInvokeTarget::Expr(inner) => {
                        self.rewrite_expr_types(inner.as_mut(), subst, local_types);
                    }
                    ExprInvokeTarget::Closure(func) => {
                        self.rewrite_expr_types(func.body.as_mut(), subst, local_types);
                    }
                    _ => {}
                }
                for arg in &mut invoke.args {
                    self.rewrite_expr_types(arg, subst, local_types);
                }

                let target_fn_ty = match &mut invoke.target {
                    ExprInvokeTarget::Function(locator) => self
                        .resolve_local_ty(locator, local_types)
                        .and_then(|ty| match ty {
                            Ty::Function(fn_ty) => Some(fn_ty),
                            _ => None,
                        }),
                    ExprInvokeTarget::Expr(inner) => {
                        let ty = inner.ty().cloned().or_else(|| {
                            if let ExprKind::Locator(locator) = inner.kind_mut() {
                                self.resolve_local_ty(locator, local_types)
                            } else {
                                None
                            }
                        });
                        ty.and_then(|ty| match ty {
                            Ty::Function(fn_ty) => Some(fn_ty),
                            _ => None,
                        })
                    }
                    ExprInvokeTarget::Closure(func) => Some(TypeFunction {
                        params: func
                            .sig
                            .params
                            .iter()
                            .map(|param| param.ty.clone())
                            .collect(),
                        generics_params: Vec::new(),
                        ret_ty: func.sig.ret_ty.as_ref().map(|ret| Box::new(ret.clone())),
                    }),
                    _ => None,
                };

                let mut inferred_ret_ty: Option<Ty> = None;
                let mut param_types: Option<Vec<Ty>> = None;

                if let Some(function_ty) = target_fn_ty {
                    if expr_type_unknown {
                        inferred_ret_ty = Self::type_function_ret_ty(&function_ty)
                            .or_else(|| Some(Ty::Unit(TypeUnit)));
                    }
                    param_types = Some(function_ty.params.clone());
                }

                if let Some(fn_params) = param_types {
                    for (idx, arg) in invoke.args.iter_mut().enumerate() {
                        if let Some(param_ty) = fn_params.get(idx) {
                            if arg.ty().map_or(true, |ty| self.is_unknown(ty)) {
                                let substituted = self.substitute_ty(param_ty, subst);
                                arg.set_ty(substituted);
                            }
                        }
                    }
                }

                if let Some(ret_ty) = inferred_ret_ty {
                    expr.set_ty(ret_ty);
                }
            }
            ExprKind::Struct(struct_expr) => {
                for field in &mut struct_expr.fields {
                    if let Some(value) = field.value.as_mut() {
                        self.rewrite_expr_types(value, subst, local_types);
                    }
                }
            }
            ExprKind::Structural(struct_expr) => {
                for field in &mut struct_expr.fields {
                    if let Some(value) = field.value.as_mut() {
                        self.rewrite_expr_types(value, subst, local_types);
                    }
                }
            }
            ExprKind::Select(select) => {
                self.rewrite_expr_types(select.obj.as_mut(), subst, local_types);
            }
            ExprKind::Array(array) => {
                for value in &mut array.values {
                    self.rewrite_expr_types(value, subst, local_types);
                }
            }
            ExprKind::ArrayRepeat(array_repeat) => {
                self.rewrite_expr_types(array_repeat.elem.as_mut(), subst, local_types);
                self.rewrite_expr_types(array_repeat.len.as_mut(), subst, local_types);
            }
            ExprKind::Tuple(tuple) => {
                for value in &mut tuple.values {
                    self.rewrite_expr_types(value, subst, local_types);
                }
            }
            ExprKind::BinOp(binop) => {
                self.rewrite_expr_types(binop.lhs.as_mut(), subst, local_types);
                self.rewrite_expr_types(binop.rhs.as_mut(), subst, local_types);
            }
            ExprKind::UnOp(unop) => {
                self.rewrite_expr_types(unop.val.as_mut(), subst, local_types);
            }
            ExprKind::Reference(reference) => {
                self.rewrite_expr_types(reference.referee.as_mut(), subst, local_types);
            }
            ExprKind::Dereference(deref) => {
                self.rewrite_expr_types(deref.referee.as_mut(), subst, local_types);
            }
            ExprKind::Index(index) => {
                self.rewrite_expr_types(index.obj.as_mut(), subst, local_types);
                self.rewrite_expr_types(index.index.as_mut(), subst, local_types);
            }
            ExprKind::Splat(splat) => {
                self.rewrite_expr_types(splat.iter.as_mut(), subst, local_types);
            }
            ExprKind::SplatDict(splat) => {
                self.rewrite_expr_types(splat.dict.as_mut(), subst, local_types);
            }
            ExprKind::Try(expr_try) => {
                self.rewrite_expr_types(expr_try.expr.as_mut(), subst, local_types);
            }
            ExprKind::Closure(closure) => {
                self.rewrite_expr_types(closure.body.as_mut(), subst, local_types);
            }
            ExprKind::Closured(closured) => {
                self.rewrite_expr_types(closured.expr.as_mut(), subst, local_types);
            }
            ExprKind::Paren(paren) => {
                self.rewrite_expr_types(paren.expr.as_mut(), subst, local_types);
            }
            ExprKind::FormatString(format) => {
                let _ = format;
            }
            ExprKind::IntrinsicCall(call) => {
                for arg in &mut call.args {
                    self.rewrite_expr_types(arg, subst, local_types);
                }
                for kwarg in &mut call.kwargs {
                    self.rewrite_expr_types(&mut kwarg.value, subst, local_types);
                }
            }
            ExprKind::Value(value) => match value.as_mut() {
                Value::Expr(inner) => {
                    self.rewrite_expr_types(inner.as_mut(), subst, local_types);
                }
                Value::Function(func) => {
                    self.rewrite_expr_types(func.body.as_mut(), subst, local_types);
                }
                _ => {}
            },
            _ => {}
        }
    }

    fn resolve_local_ty(
        &self,
        locator: &mut Locator,
        local_types: &HashMap<String, Ty>,
    ) -> Option<Ty> {
        let key = Self::sanitize_locator(locator);
        if let Some(ty) = local_types.get(&key) {
            return Some(ty.clone());
        }
        if let Some(ty) = local_types.get(&format!("#{}", key)) {
            return Some(ty.clone());
        }
        tracing::debug!(target: DEFAULT_DIAGNOSTIC_CONTEXT, "resolve_local_ty: missing key {}", key);
        None
    }

    // moved to closures.rs: capture_closure

    // moved to closures.rs: call_const_closure

    // moved to closures.rs: call_function

    // moved to closures.rs: call_value_function

    fn eval_block(&mut self, block: &mut ExprBlock) -> Value {
        self.push_scope();
        let mut last_value = Value::unit();
        for stmt in &mut block.stmts {
            if let Some(value) = self.eval_stmt(stmt) {
                last_value = value;
            }
        }
        self.pop_scope();
        last_value
    }

    // Convert a control-flow result into a value, emitting diagnostics for stray flow.
    fn finish_runtime_flow(&mut self, flow: RuntimeFlow) -> Value {
        match flow {
            RuntimeFlow::Value(value) => value,
            RuntimeFlow::Break(_) => {
                self.emit_error("`break` is not allowed outside of a loop");
                Value::undefined()
            }
            RuntimeFlow::Continue => {
                self.emit_error("`continue` is not allowed outside of a loop");
                Value::undefined()
            }
            RuntimeFlow::Return(value) => {
                self.emit_error("`return` is not allowed outside of a function");
                value.unwrap_or_else(Value::unit)
            }
            RuntimeFlow::Panic(message) => {
                self.emit_error(format!("panic: {}", message));
                Value::undefined()
            }
        }
    }

    // Evaluate a block in runtime-capable mode, propagating control-flow.
    fn eval_block_runtime(&mut self, block: &mut ExprBlock) -> RuntimeFlow {
        self.push_scope();
        let mut last_value = Value::unit();
        for stmt in &mut block.stmts {
            match stmt {
                BlockStmt::Expr(expr_stmt) => {
                    if self.in_const_region() {
                        if let ExprKind::Splice(splice) = expr_stmt.expr.kind_mut() {
                            if !self.pending_stmt_splices.is_empty() {
                                let Some(fragments) =
                                    self.resolve_splice_fragments(splice.token.as_mut())
                                else {
                                    continue;
                                };
                                if fragments
                                    .iter()
                                    .any(|fragment| matches!(fragment, QuotedFragment::Type(_)))
                                {
                                    self.emit_error(
                                        "splice<type> is not valid in statement position",
                                    );
                                    continue;
                                }
                                let mut to_append = Vec::new();
                                for fragment in fragments {
                                    match fragment {
                                        QuotedFragment::Stmts(stmts) => to_append.extend(stmts),
                                        QuotedFragment::Expr(e) => {
                                            let mut es = expr_stmt.clone();
                                            es.expr = e.into();
                                            es.semicolon = Some(true);
                                            to_append.push(BlockStmt::Expr(es));
                                        }
                                        QuotedFragment::Items(items) => {
                                            for item in items {
                                                to_append.push(BlockStmt::Item(Box::new(item)));
                                            }
                                        }
                                        QuotedFragment::Type(_) => {}
                                    }
                                }
                                if !to_append.is_empty() {
                                    if let Some(pending) = self.pending_stmt_splices.last_mut() {
                                        pending.extend(to_append);
                                    }
                                    self.mark_mutated();
                                }
                                continue;
                            }
                        }
                    }
                    let flow = self.eval_expr_runtime(expr_stmt.expr.as_mut());
                    match flow {
                        RuntimeFlow::Value(value) => {
                            if expr_stmt.has_value() {
                                last_value = value;
                            }
                        }
                        other => {
                            self.pop_scope();
                            return other;
                        }
                    }
                }
                BlockStmt::Let(stmt_let) => {
                    if let Some(init) = stmt_let.init.as_mut() {
                        let flow = self.eval_expr_runtime(init);
                        match flow {
                            RuntimeFlow::Value(value) => {
                                self.bind_pattern(&stmt_let.pat, value);
                            }
                            other => {
                                self.pop_scope();
                                return other;
                            }
                        }
                    } else {
                        self.emit_error(
                            "let bindings without initializer are not supported in runtime",
                        );
                    }
                }
                BlockStmt::Item(item) => {
                    self.evaluate_item(item.as_mut());
                }
                BlockStmt::Noop | BlockStmt::Any(_) => {}
            }
        }
        self.pop_scope();
        RuntimeFlow::Value(last_value)
    }

    // Resolve receiver bindings, preserving mutability if possible.
    fn resolve_receiver_binding(&mut self, expr: &mut Expr) -> ReceiverBinding {
        if let ExprKind::Locator(locator) = expr.kind_mut() {
            if let Some(ident) = locator.as_ident() {
                if let Some(stored) = self.lookup_stored_value(ident.as_str()) {
                    return ReceiverBinding {
                        value: stored.value(),
                        shared: stored.shared_handle(),
                    };
                }
            }
        }
        let flow = self.eval_expr_runtime(expr);
        ReceiverBinding {
            value: self.finish_runtime_flow(flow),
            shared: None,
        }
    }

    // Evaluate assignment with mutation tracking.
    fn eval_assign_runtime(&mut self, assign: &mut fp_core::ast::ExprAssign) -> RuntimeFlow {
        let value = match self.eval_expr_runtime(assign.value.as_mut()) {
            RuntimeFlow::Value(value) => value,
            other => return other,
        };
        match assign.target.kind_mut() {
            ExprKind::Locator(locator) => {
                let name = locator
                    .as_ident()
                    .map(|ident| ident.as_str().to_string())
                    .unwrap_or_else(|| locator.to_string());
                if let Some(stored) = self.lookup_stored_value_mut(&name) {
                    if stored.assign(value.clone()) {
                        return RuntimeFlow::Value(value);
                    }
                }
                self.emit_error(format!("assignment target '{}' is not mutable", locator));
                RuntimeFlow::Value(Value::undefined())
            }
            ExprKind::Select(select) => {
                if let ExprKind::Locator(locator) = select.obj.kind_mut() {
                    let name = locator
                        .as_ident()
                        .map(|ident| ident.as_str().to_string())
                        .unwrap_or_else(|| locator.to_string());
                    if let Some(stored) = self.lookup_stored_value_mut(&name) {
                        if let Some(shared) = stored.shared_handle() {
                            match shared.lock() {
                                Ok(mut guard) => {
                                    if self.assign_field_value(
                                        &mut guard,
                                        &select.field,
                                        value.clone(),
                                    ) {
                                        return RuntimeFlow::Value(value);
                                    }
                                }
                                Err(err) => {
                                    let mut guard = err.into_inner();
                                    if self.assign_field_value(
                                        &mut guard,
                                        &select.field,
                                        value.clone(),
                                    ) {
                                        return RuntimeFlow::Value(value);
                                    }
                                }
                            }
                        }
                    }
                }
                self.emit_error("cannot assign to field on non-mutable target");
                RuntimeFlow::Value(Value::undefined())
            }
            _ => {
                self.emit_error("unsupported assignment target in runtime mode");
                RuntimeFlow::Value(Value::undefined())
            }
        }
    }

    // Run a `loop {}` expression with break/continue support.
    fn eval_loop_runtime(&mut self, loop_expr: &mut fp_core::ast::ExprLoop) -> RuntimeFlow {
        self.loop_depth += 1;
        loop {
            match self.eval_expr_runtime(loop_expr.body.as_mut()) {
                RuntimeFlow::Value(_) | RuntimeFlow::Continue => {}
                RuntimeFlow::Break(value) => {
                    self.loop_depth -= 1;
                    return RuntimeFlow::Value(value.unwrap_or_else(Value::unit));
                }
                RuntimeFlow::Return(value) => {
                    self.loop_depth -= 1;
                    return RuntimeFlow::Return(value);
                }
                RuntimeFlow::Panic(message) => {
                    self.loop_depth -= 1;
                    return RuntimeFlow::Panic(message);
                }
            }
        }
    }

    // Run a `while` expression.
    fn eval_while_runtime(&mut self, while_expr: &mut fp_core::ast::ExprWhile) -> RuntimeFlow {
        self.loop_depth += 1;
        loop {
            let cond = match self.eval_expr_runtime(while_expr.cond.as_mut()) {
                RuntimeFlow::Value(value) => value,
                other => {
                    self.loop_depth -= 1;
                    return other;
                }
            };
            match cond {
                Value::Bool(flag) => {
                    if !flag.value {
                        self.loop_depth -= 1;
                        return RuntimeFlow::Value(Value::unit());
                    }
                }
                _ => {
                    self.emit_error("expected boolean condition in while loop");
                    self.loop_depth -= 1;
                    return RuntimeFlow::Value(Value::undefined());
                }
            }

            match self.eval_expr_runtime(while_expr.body.as_mut()) {
                RuntimeFlow::Value(_) | RuntimeFlow::Continue => {}
                RuntimeFlow::Break(value) => {
                    self.loop_depth -= 1;
                    return RuntimeFlow::Value(value.unwrap_or_else(Value::unit));
                }
                RuntimeFlow::Return(value) => {
                    self.loop_depth -= 1;
                    return RuntimeFlow::Return(value);
                }
                RuntimeFlow::Panic(message) => {
                    self.loop_depth -= 1;
                    return RuntimeFlow::Panic(message);
                }
            }
        }
    }

    // Run a `for` expression over lists or ranges.
    fn eval_for_runtime(&mut self, for_expr: &mut fp_core::ast::ExprFor) -> RuntimeFlow {
        let iter_value = match self.eval_expr_runtime(for_expr.iter.as_mut()) {
            RuntimeFlow::Value(value) => value,
            other => return other,
        };
        let values = match iter_value {
            Value::List(list) => list.values,
            other => {
                self.emit_error(format!("for loop expects iterable list, found {}", other));
                return RuntimeFlow::Value(Value::undefined());
            }
        };

        self.loop_depth += 1;
        for value in values {
            self.push_scope();
            self.bind_pattern(&for_expr.pat, value);
            match self.eval_expr_runtime(for_expr.body.as_mut()) {
                RuntimeFlow::Value(_) => {
                    self.pop_scope();
                }
                RuntimeFlow::Continue => {
                    self.pop_scope();
                    continue;
                }
                RuntimeFlow::Break(value) => {
                    self.pop_scope();
                    self.loop_depth -= 1;
                    return RuntimeFlow::Value(value.unwrap_or_else(Value::unit));
                }
                RuntimeFlow::Return(value) => {
                    self.pop_scope();
                    self.loop_depth -= 1;
                    return RuntimeFlow::Return(value);
                }
                RuntimeFlow::Panic(message) => {
                    self.pop_scope();
                    self.loop_depth -= 1;
                    return RuntimeFlow::Panic(message);
                }
            }
        }
        self.loop_depth -= 1;
        RuntimeFlow::Value(Value::unit())
    }

    // Runtime-friendly array repeat evaluation.
    fn eval_array_repeat_runtime(&mut self, repeat: &mut fp_core::ast::ExprArrayRepeat) -> Value {
        let elem_value = match self.eval_expr_runtime(repeat.elem.as_mut()) {
            RuntimeFlow::Value(value) => value,
            other => return self.finish_runtime_flow(other),
        };
        let len_value = match self.eval_expr_runtime(repeat.len.as_mut()) {
            RuntimeFlow::Value(value) => value,
            other => return self.finish_runtime_flow(other),
        };

        let len = match len_value {
            Value::Int(int) if int.value >= 0 => int.value as usize,
            Value::Int(_) => {
                self.emit_error("array repeat length must be non-negative");
                return Value::undefined();
            }
            Value::Decimal(decimal) if decimal.value >= 0.0 => decimal.value as usize,
            other => {
                self.emit_error(format!(
                    "array repeat length must be an integer constant, found {}",
                    other
                ));
                return Value::undefined();
            }
        };

        let mut values = Vec::with_capacity(len);
        for _ in 0..len {
            values.push(elem_value.clone());
        }
        Value::List(ValueList::new(values))
    }

    // Build a struct literal, handling enum struct variants when needed.
    fn evaluate_struct_literal_runtime(
        &mut self,
        struct_expr: &mut fp_core::ast::ExprStruct,
    ) -> Value {
        if let ExprKind::Locator(locator) = struct_expr.name.kind_mut() {
            if let Some(info) = self.lookup_enum_variant(locator) {
                if let EnumVariantPayload::Struct(field_names) = &info.payload {
                    let fields = self.build_struct_literal_fields_runtime(
                        Some(field_names),
                        &mut struct_expr.fields,
                    );
                    return self.build_enum_value(
                        &info,
                        Some(Value::Structural(ValueStructural::new(fields))),
                    );
                }
            }
        }

        let ty_value = match self.eval_expr_runtime(struct_expr.name.as_mut()) {
            RuntimeFlow::Value(value) => value,
            other => return self.finish_runtime_flow(other),
        };
        let struct_ty = match self.resolve_struct_literal_type(ty_value) {
            Some(struct_ty) => struct_ty,
            None => {
                self.emit_error("expected struct type in literal");
                return Value::undefined();
            }
        };

        let expected_names = match &struct_ty {
            StructLiteralType::Struct(struct_ty) => Some(
                struct_ty
                    .fields
                    .iter()
                    .map(|field| field.name.clone())
                    .collect::<Vec<_>>(),
            ),
            StructLiteralType::Structural(structural) => Some(
                structural
                    .fields
                    .iter()
                    .map(|field| field.name.clone())
                    .collect::<Vec<_>>(),
            ),
        };
        let fields = self.build_struct_literal_fields_runtime(
            expected_names.as_deref(),
            &mut struct_expr.fields,
        );

        match struct_ty {
            StructLiteralType::Struct(struct_ty) => {
                Value::Struct(ValueStruct::new(struct_ty, fields))
            }
            StructLiteralType::Structural(_) => Value::Structural(ValueStructural::new(fields)),
        }
    }

    fn build_struct_literal_fields_runtime(
        &mut self,
        expected_names: Option<&[Ident]>,
        fields: &mut [ExprField],
    ) -> Vec<ValueField> {
        let mut value_fields = Vec::new();
        if let Some(expected) = expected_names {
            let mut field_indices = HashMap::new();
            for (idx, field) in fields.iter().enumerate() {
                let name = field.name.as_str().to_string();
                if field_indices.insert(name.clone(), idx).is_some() {
                    self.emit_error(format!(
                        "duplicate field '{}' in struct literal",
                        field.name
                    ));
                }
            }
            for expected_name in expected {
                if let Some(index) = field_indices.get(expected_name.as_str()) {
                    let field = &mut fields[*index];
                    let value = field
                        .value
                        .as_mut()
                        .map(|expr| {
                            let flow = self.eval_expr_runtime(expr);
                            match flow {
                                RuntimeFlow::Value(value) => value,
                                other => self.finish_runtime_flow(other),
                            }
                        })
                        .unwrap_or_else(|| {
                            self.emit_error(format!(
                                "missing initializer for field '{}' in struct literal",
                                field.name
                            ));
                            Value::undefined()
                        });
                    value_fields.push(ValueField::new(field.name.clone(), value));
                } else {
                    self.emit_error(format!(
                        "missing initializer for field '{}' in struct literal",
                        expected_name
                    ));
                    value_fields.push(ValueField::new(expected_name.clone(), Value::undefined()));
                }
            }
            for field in fields.iter() {
                if !expected.iter().any(|name| name == &field.name) {
                    self.emit_error(format!(
                        "field '{}' does not exist on this struct",
                        field.name
                    ));
                }
            }
            return value_fields;
        }

        for field in fields {
            let value = field
                .value
                .as_mut()
                .map(|expr| {
                    let flow = self.eval_expr_runtime(expr);
                    self.finish_runtime_flow(flow)
                })
                .unwrap_or_else(|| {
                    self.lookup_value(field.name.as_str()).unwrap_or_else(|| {
                        self.emit_error(format!(
                            "missing initializer for field '{}' in struct literal",
                            field.name
                        ));
                        Value::undefined()
                    })
                });
            value_fields.push(ValueField::new(field.name.clone(), value));
        }

        value_fields
    }

    // Evaluate indexing on list/tuple values.
    fn evaluate_index(&mut self, target: Value, index: Value) -> Value {
        match target {
            Value::List(list) => {
                let idx = match index {
                    Value::Int(int) if int.value >= 0 => int.value as usize,
                    other => {
                        self.emit_error(format!(
                            "index must be a non-negative integer, found {}",
                            other
                        ));
                        return Value::undefined();
                    }
                };
                list.values.get(idx).cloned().unwrap_or_else(|| {
                    self.emit_error("index out of bounds for list");
                    Value::undefined()
                })
            }
            Value::Tuple(tuple) => {
                let idx = match index {
                    Value::Int(int) if int.value >= 0 => int.value as usize,
                    other => {
                        self.emit_error(format!(
                            "index must be a non-negative integer, found {}",
                            other
                        ));
                        return Value::undefined();
                    }
                };
                tuple.values.get(idx).cloned().unwrap_or_else(|| {
                    self.emit_error("index out of bounds for tuple");
                    Value::undefined()
                })
            }
            Value::Map(map) => {
                let key = index;
                match map.get(&key) {
                    Some(value) => value.clone(),
                    None => {
                        self.emit_error(format!("HashMap constant does not contain key '{}'", key));
                        Value::undefined()
                    }
                }
            }
            other => {
                self.emit_error(format!("cannot index into value {}", other));
                Value::undefined()
            }
        }
    }

    fn evaluate_range_index(&mut self, target: Value, range: &mut ExprRange) -> Value {
        let start = match range.start.as_mut() {
            Some(expr) => match self.eval_expr(expr.as_mut()) {
                Value::Int(int) if int.value >= 0 => Some(int.value as usize),
                other => {
                    self.emit_error(format!(
                        "range start must be a non-negative integer, found {}",
                        other
                    ));
                    return Value::undefined();
                }
            },
            None => None,
        };
        let end = match range.end.as_mut() {
            Some(expr) => match self.eval_expr(expr.as_mut()) {
                Value::Int(int) if int.value >= 0 => Some(int.value as usize),
                other => {
                    self.emit_error(format!(
                        "range end must be a non-negative integer, found {}",
                        other
                    ));
                    return Value::undefined();
                }
            },
            None => None,
        };

        match target {
            Value::String(text) => {
                let chars: Vec<char> = text.value.chars().collect();
                let len = chars.len();
                let start_idx = start.unwrap_or(0);
                let mut end_idx = end.unwrap_or(len);
                if matches!(range.limit, ExprRangeLimit::Inclusive) {
                    end_idx = end_idx.saturating_add(1);
                }
                if start_idx > end_idx || end_idx > len {
                    self.emit_error("range slice is out of bounds");
                    return Value::undefined();
                }
                let slice: String = chars[start_idx..end_idx].iter().collect();
                Value::string(slice)
            }
            Value::List(list) => {
                let len = list.values.len();
                let start_idx = start.unwrap_or(0);
                let mut end_idx = end.unwrap_or(len);
                if matches!(range.limit, ExprRangeLimit::Inclusive) {
                    end_idx = end_idx.saturating_add(1);
                }
                if start_idx > end_idx || end_idx > len {
                    self.emit_error("range slice is out of bounds");
                    return Value::undefined();
                }
                let values = list.values[start_idx..end_idx].to_vec();
                Value::List(ValueList::new(values))
            }
            other => {
                self.emit_error(format!("cannot apply range index to value {}", other));
                Value::undefined()
            }
        }
    }

    // Assign into a struct/structural value.
    fn assign_field_value(&mut self, target: &mut Value, field: &Ident, value: Value) -> bool {
        match target {
            Value::Struct(struct_value) => {
                if let Some(existing) = struct_value
                    .structural
                    .fields
                    .iter_mut()
                    .find(|f| f.name.as_str() == field.as_str())
                {
                    existing.value = value;
                    return true;
                }
            }
            Value::Structural(structural) => {
                if let Some(existing) = structural
                    .fields
                    .iter_mut()
                    .find(|f| f.name.as_str() == field.as_str())
                {
                    existing.value = value;
                    return true;
                }
            }
            _ => {}
        }

        self.emit_error(format!("field '{}' not found on struct", field));
        false
    }

    fn lookup_enum_variant(&self, locator: &Locator) -> Option<EnumVariantInfo> {
        let mut candidates = vec![locator.to_string()];
        if let Some(ident) = locator.as_ident() {
            candidates.push(ident.as_str().to_string());
        }
        for candidate in candidates {
            if let Some(info) = self.enum_variants.get(&candidate) {
                return Some(info.clone());
            }
        }
        None
    }

    fn resolve_enum_variant(&self, locator: &Locator) -> Option<Value> {
        let info = self.lookup_enum_variant(locator)?;
        if !matches!(info.payload, EnumVariantPayload::Unit) {
            return None;
        }
        Some(self.build_enum_value(&info, None))
    }

    fn build_enum_value(&self, info: &EnumVariantInfo, payload: Option<Value>) -> Value {
        Value::Any(AnyBox::new(RuntimeEnum {
            enum_name: info.enum_name.clone(),
            variant_name: info.variant_name.clone(),
            payload,
            discriminant: info.discriminant,
        }))
    }

    fn value_type_name(&self, value: &Value) -> Option<String> {
        match value {
            Value::Struct(struct_value) => Some(struct_value.ty.name.as_str().to_string()),
            Value::Type(Ty::Struct(struct_ty)) => Some(struct_ty.name.as_str().to_string()),
            Value::Any(any) => any
                .downcast_ref::<RuntimeEnum>()
                .map(|enm| enm.enum_name.clone()),
            _ => None,
        }
    }

    fn infer_value_ty(&self, value: &Value) -> Option<Ty> {
        match value {
            Value::Int(_) => Some(Ty::Primitive(TypePrimitive::Int(TypeInt::I64))),
            Value::Decimal(_) => Some(Ty::Primitive(TypePrimitive::Decimal(DecimalType::F64))),
            Value::Bool(_) => Some(Ty::Primitive(TypePrimitive::Bool)),
            Value::Char(_) => Some(Ty::Primitive(TypePrimitive::Char)),
            Value::String(_) => Some(Ty::Primitive(TypePrimitive::String)),
            Value::List(_) => Some(Ty::Primitive(TypePrimitive::List)),
            Value::Struct(struct_value) => Some(Ty::Struct(struct_value.ty.clone())),
            Value::Function(function) => Some(Ty::Function(TypeFunction {
                params: function
                    .sig
                    .params
                    .iter()
                    .map(|param| param.ty.clone())
                    .collect(),
                generics_params: function.sig.generics_params.clone(),
                ret_ty: function.sig.ret_ty.as_ref().map(|ty| Box::new(ty.clone())),
            })),
            Value::Any(any) => any
                .downcast_ref::<ConstClosure>()
                .and_then(|closure| closure.function_ty.clone()),
            _ => None,
        }
    }

    fn resolve_method_function(
        &mut self,
        receiver: &ReceiverBinding,
        method_name: &str,
        args: &mut [Expr],
    ) -> Option<ItemDefFunction> {
        let type_name = self.value_type_name(&receiver.value)?;
        let method_key = format!("{}::{}", type_name, method_name);

        let local_method = self
            .impl_methods
            .get(&type_name)
            .and_then(|methods| methods.get(method_name))
            .filter(|function| function.sig.generics_params.is_empty())
            .cloned();
        if let Some(function) = local_method {
            self.annotate_invoke_args_slice(args, &function.sig.params);
            return Some(function);
        }

        if let Some(function) = self.functions.get(&method_key).cloned() {
            self.annotate_invoke_args_slice(args, &function.sig.params);
            return Some(function);
        }

        if let Some(template) = self.generic_functions.get(&method_key).cloned() {
            let mut locator = Locator::path(Path::new(vec![
                Ident::new(type_name.clone()),
                Ident::new(method_name.to_string()),
            ]));
            if let Some(function) =
                self.instantiate_generic_function(&method_key, template, &mut locator, args)
            {
                self.annotate_invoke_args_slice(args, &function.sig.params);
                return Some(function);
            }
        }

        if let Some(traits) = self.trait_impls.get(&type_name) {
            for trait_name in traits {
                if let Some(methods) = self.trait_methods.get(trait_name) {
                    if let Some(function) = methods.get(method_name) {
                        return Some(function.clone());
                    }
                }
            }
        }

        None
    }

    fn evaluate_struct_literal(&mut self, struct_expr: &mut fp_core::ast::ExprStruct) -> Value {
        let ty_value = self.eval_expr(struct_expr.name.as_mut());
        let struct_ty = match self.resolve_struct_literal_type(ty_value) {
            Some(struct_ty) => struct_ty,
            None => {
                self.emit_error("expected struct type in literal");
                return Value::undefined();
            }
        };

        let mut fields = Vec::new();
        for field in &mut struct_expr.fields {
            let value = field
                .value
                .as_mut()
                .map(|expr| self.eval_expr(expr))
                .unwrap_or_else(|| {
                    self.lookup_value(field.name.as_str()).unwrap_or_else(|| {
                        self.emit_error(format!(
                            "missing initializer for field '{}' in struct literal",
                            field.name
                        ));
                        Value::undefined()
                    })
                });
            fields.push(ValueField::new(field.name.clone(), value));
        }

        match struct_ty {
            StructLiteralType::Struct(struct_ty) => {
                Value::Struct(ValueStruct::new(struct_ty, fields))
            }
            StructLiteralType::Structural(_) => Value::Structural(ValueStructural::new(fields)),
        }
    }

    fn resolve_struct_literal_type(&mut self, ty_value: Value) -> Option<StructLiteralType> {
        match ty_value {
            Value::Type(Ty::Struct(struct_ty)) => Some(StructLiteralType::Struct(struct_ty)),
            Value::Type(Ty::Structural(structural)) => {
                Some(StructLiteralType::Structural(structural))
            }
            Value::Type(other_ty) => {
                let mut visiting = HashSet::new();
                let fields = self.resolve_structural_fields(&other_ty, &mut visiting)?;
                Some(StructLiteralType::Structural(TypeStructural { fields }))
            }
            _ => None,
        }
    }

    fn resolve_structural_fields(
        &self,
        ty: &Ty,
        visiting: &mut HashSet<String>,
    ) -> Option<Vec<StructuralField>> {
        match ty {
            Ty::Struct(struct_ty) => Some(struct_ty.fields.clone()),
            Ty::Structural(structural) => Some(structural.fields.clone()),
            Ty::TypeBinaryOp(op) => {
                let lhs = self.resolve_structural_fields(op.lhs.as_ref(), visiting)?;
                let rhs = self.resolve_structural_fields(op.rhs.as_ref(), visiting)?;
                match op.kind {
                    TypeBinaryOpKind::Add => Some(self.merge_structural_fields(lhs, rhs)),
                    TypeBinaryOpKind::Intersect => Some(self.intersect_structural_fields(lhs, rhs)),
                    TypeBinaryOpKind::Subtract => Some(self.subtract_structural_fields(lhs, rhs)),
                    TypeBinaryOpKind::Union => None,
                }
            }
            Ty::Expr(expr) => match expr.kind() {
                ExprKind::Locator(locator) => {
                    let key = Self::locator_base_name(locator);
                    if visiting.contains(&key) {
                        return None;
                    }
                    let ty = self
                        .lookup_type(&key)
                        .or_else(|| self.global_types.get(&key).cloned())?;
                    visiting.insert(key.clone());
                    let result = self.resolve_structural_fields(&ty, visiting);
                    visiting.remove(&key);
                    result
                }
                _ => None,
            },
            _ => None,
        }
    }

    fn merge_structural_fields(
        &self,
        mut lhs: Vec<StructuralField>,
        rhs: Vec<StructuralField>,
    ) -> Vec<StructuralField> {
        for rhs_field in rhs {
            if lhs
                .iter()
                .any(|field| field.name.as_str() == rhs_field.name.as_str())
            {
                continue;
            }
            lhs.push(rhs_field);
        }
        lhs
    }

    fn intersect_structural_fields(
        &self,
        lhs: Vec<StructuralField>,
        rhs: Vec<StructuralField>,
    ) -> Vec<StructuralField> {
        let rhs_names: HashSet<&str> = rhs.iter().map(|f| f.name.as_str()).collect();
        lhs.into_iter()
            .filter(|f| rhs_names.contains(f.name.as_str()))
            .collect()
    }

    fn subtract_structural_fields(
        &self,
        lhs: Vec<StructuralField>,
        rhs: Vec<StructuralField>,
    ) -> Vec<StructuralField> {
        let rhs_names: HashSet<&str> = rhs.iter().map(|f| f.name.as_str()).collect();
        lhs.into_iter()
            .filter(|f| !rhs_names.contains(f.name.as_str()))
            .collect()
    }

    fn eval_array_repeat(&mut self, repeat: &mut fp_core::ast::ExprArrayRepeat) -> Value {
        let elem_value = self.eval_expr(repeat.elem.as_mut());
        let len_value = self.eval_expr(repeat.len.as_mut());

        let len = match len_value {
            Value::Int(int) if int.value >= 0 => int.value as usize,
            Value::Int(_) => {
                self.emit_error("array repeat length must be non-negative");
                return Value::undefined();
            }
            Value::Decimal(decimal) if decimal.value >= 0.0 => decimal.value as usize,
            other => {
                self.emit_error(format!(
                    "array repeat length must be an integer constant, found {}",
                    other
                ));
                return Value::undefined();
            }
        };

        let mut values = Vec::with_capacity(len);
        for _ in 0..len {
            values.push(elem_value.clone());
        }
        Value::List(ValueList::new(values))
    }

    fn evaluate_ty(&mut self, ty: &mut Ty) {
        match ty {
            Ty::Array(array_ty) => {
                let len_value = self.eval_expr(array_ty.len.as_mut());
                match len_value {
                    Value::Int(int_value) => {
                        let replacement = Expr::value(Value::Int(int_value.clone()));
                        *array_ty.len = replacement.into();
                    }
                    Value::Decimal(decimal) => {
                        let as_int = decimal.value as i64;
                        let replacement = Expr::value(Value::int(as_int));
                        *array_ty.len = replacement.into();
                    }
                    other => {
                        self.emit_error(format!(
                            "array length type must be an integer literal, found {}",
                            other
                        ));
                    }
                }
            }
            Ty::Expr(expr) => {
                if let ExprKind::ConstBlock(_) = expr.kind() {
                    let value = self.eval_expr(expr.as_mut());
                    match value {
                        Value::Type(resolved_ty) => {
                            *ty = resolved_ty;
                        }
                        other => {
                            self.emit_error(format!(
                                "type expression must evaluate to a type, found {}",
                                other
                            ));
                        }
                    }
                }
            }
            _ => {}
        }
    }

    fn evaluate_select(&mut self, target: Value, field: &str) -> Value {
        match target {
            Value::Struct(value_struct) => value_struct
                .structural
                .fields
                .iter()
                .find(|f| f.name.as_str() == field)
                .map(|f| f.value.clone())
                .unwrap_or_else(|| {
                    self.emit_error(format!("field '{}' not found on struct", field));
                    Value::undefined()
                }),
            Value::Structural(structural) => structural
                .fields
                .iter()
                .find(|f| f.name.as_str() == field)
                .map(|f| f.value.clone())
                .unwrap_or_else(|| {
                    self.emit_error(format!("field '{}' not found", field));
                    Value::undefined()
                }),
            Value::QuoteToken(token) => {
                let is_item_kind = matches!(token.kind, QuoteFragmentKind::Item);
                if !is_item_kind {
                    let kind = match token.kind {
                        QuoteFragmentKind::Expr => "expr",
                        QuoteFragmentKind::Stmt => "stmt",
                        QuoteFragmentKind::Item => "item",
                        QuoteFragmentKind::Type => "type",
                    };
                    self.emit_error(format!(
                        "cannot access field '{}' on quote<{}> token",
                        field, kind
                    ));
                    return Value::undefined();
                }
                let QuoteTokenValue::Items(items) = token.value else {
                    self.emit_error(format!(
                        "quote<item> token did not contain items for field '{}'",
                        field
                    ));
                    return Value::undefined();
                };
                if items.len() != 1 {
                    self.emit_error(format!(
                        "quote<item> field access requires a single item, found {}",
                        items.len()
                    ));
                    return Value::undefined();
                }
                let item = &items[0];
                match field {
                    "name" => item
                        .get_ident()
                        .map(|ident| Value::string(ident.name.clone()))
                        .unwrap_or_else(|| {
                            self.emit_error("quote<item> item has no name");
                            Value::undefined()
                        }),
                    "fn" | "value" => match item.kind() {
                        ItemKind::DefFunction(func) => Value::Function(ValueFunction {
                            sig: func.sig.clone(),
                            body: func.body.clone(),
                        }),
                        _ => {
                            self.emit_error("quote<item> function access requires a function item");
                            Value::undefined()
                        }
                    },
                    _ => {
                        self.emit_error(format!(
                            "field '{}' not available on quote<item> values",
                            field
                        ));
                        Value::undefined()
                    }
                }
            }
            other => {
                self.emit_error(format!(
                    "cannot access field '{}' on value {} in const context",
                    field, other
                ));
                Value::undefined()
            }
        }
    }

    fn replace_expr_with_value(&mut self, expr: &mut Expr, value: Value) {
        let mut replacement = Expr::value(value);
        if let Some(ty) = expr.ty().cloned() {
            replacement.set_ty(ty);
        }
        *expr = replacement;
        self.mark_mutated();
    }
    #[allow(unused)]
    fn replace_expr_with_todo(&mut self, expr: &mut Expr, message: &str) {
        self.emit_error(format!("todo: {}", message));
        self.replace_expr_with_value(expr, Value::undefined());
    }

    fn lookup_const_collection_value(&self, locator: &Locator) -> Option<Value> {
        if let Some(ident) = locator.as_ident() {
            if let Some(value) = self.lookup_value(ident.as_str()) {
                if matches!(value, Value::List(_) | Value::Map(_)) {
                    return Some(value);
                }
            }
        }

        let mut names = vec![locator.to_string()];
        if let Some(ident) = locator.as_ident() {
            let simple = ident.as_str().to_string();
            if !names.contains(&simple) {
                names.push(simple);
            }
            let qualified = self.qualified_name(ident.as_str());
            if !names.contains(&qualified) {
                names.push(qualified);
            }
        }

        for name in names {
            if let Some(value) = self.evaluated_constants.get(&name) {
                if matches!(value, Value::List(_) | Value::Map(_)) {
                    return Some(value.clone());
                }
            }
        }

        None
    }

    fn lookup_const_collection_from_expr(&self, expr: &Expr) -> Option<Value> {
        let ExprKind::Locator(locator) = expr.kind() else {
            return None;
        };
        self.lookup_const_collection_value(locator)
    }

    fn literal_value_from_expr(&self, expr: &Expr) -> Option<Value> {
        let ExprKind::Value(value) = expr.kind() else {
            return None;
        };
        self.literal_value_from_value(value.as_ref())
    }

    fn literal_value_from_value(&self, value: &Value) -> Option<Value> {
        match value {
            Value::Int(_)
            | Value::Bool(_)
            | Value::Decimal(_)
            | Value::Char(_)
            | Value::String(_) => Some(value.clone()),
            _ => None,
        }
    }

    fn const_scalar_from_locator(&self, locator: &Locator) -> Option<Value> {
        if let Some(ident) = locator.as_ident() {
            if let Some(value) = self.lookup_value(ident.as_str()) {
                if let Some(literal) = self.literal_value_from_value(&value) {
                    return Some(literal);
                }
            }
        }

        let mut names = vec![locator.to_string()];
        if let Some(ident) = locator.as_ident() {
            let simple = ident.as_str().to_string();
            if !names.contains(&simple) {
                names.push(simple);
            }
            let qualified = self.qualified_name(ident.as_str());
            if !names.contains(&qualified) {
                names.push(qualified);
            }
        }

        for name in names {
            if let Some(value) = self.evaluated_constants.get(&name) {
                if let Some(literal) = self.literal_value_from_value(value) {
                    return Some(literal);
                }
            }
        }

        None
    }

    fn const_fold_expr_value(&self, expr: &Expr) -> Option<Value> {
        match expr.kind() {
            ExprKind::Value(_) => self.literal_value_from_expr(expr),
            ExprKind::Locator(locator) => self.const_scalar_from_locator(locator),
            ExprKind::Paren(paren) => self.const_fold_expr_value(paren.expr.as_ref()),
            ExprKind::UnOp(unop) => {
                let value = self.const_fold_expr_value(unop.val.as_ref())?;
                self.evaluate_unary(unop.op.clone(), value).ok()
            }
            ExprKind::BinOp(binop) => {
                let lhs = self.const_fold_expr_value(binop.lhs.as_ref())?;
                let rhs = self.const_fold_expr_value(binop.rhs.as_ref())?;
                self.evaluate_binop(binop.kind, lhs, rhs).ok()
            }
            _ => None,
        }
    }

    fn try_fold_runtime_const_collection_expr(&mut self, expr: &mut Expr) -> bool {
        if self.in_const_region() {
            return false;
        }

        match expr.kind_mut() {
            ExprKind::Invoke(invoke) => {
                if let ExprInvokeTarget::Method(select) = &mut invoke.target {
                    if select.field.name.as_str() == "len" && invoke.args.is_empty() {
                        if let Some(value) =
                            self.lookup_const_collection_from_expr(select.obj.as_ref())
                        {
                            let len = match value {
                                Value::List(list) => list.values.len(),
                                Value::Map(map) => map.len(),
                                _ => return false,
                            };
                            self.replace_expr_with_value(expr, Value::int(len as i64));
                            return true;
                        }
                    }
                }
            }
            ExprKind::Index(index_expr) => {
                if let Some(value) = self.lookup_const_collection_from_expr(index_expr.obj.as_ref())
                {
                    let Some(key) = self.const_fold_expr_value(index_expr.index.as_ref()) else {
                        return false;
                    };

                    let replacement = match value {
                        Value::List(list) => match key {
                            Value::Int(int) if int.value >= 0 => list
                                .values
                                .get(int.value as usize)
                                .cloned()
                                .unwrap_or_else(|| {
                                    self.emit_error("index out of bounds for list");
                                    Value::undefined()
                                }),
                            _ => {
                                self.emit_error("list index must be a non-negative integer");
                                Value::undefined()
                            }
                        },
                        Value::Map(map) => map.get(&key).cloned().unwrap_or_else(|| {
                            self.emit_error(format!(
                                "HashMap constant does not contain key '{}'",
                                key
                            ));
                            Value::undefined()
                        }),
                        _ => return false,
                    };
                    self.replace_expr_with_value(expr, replacement);
                    return true;
                }
            }
            _ => {}
        }

        false
    }

    // removed unused function_ret_ty_from_ty (no callers)

    fn type_function_ret_ty(ty: &TypeFunction) -> Option<Ty> {
        ty.ret_ty.as_ref().map(|ret| *ret.clone())
    }

    fn lookup_type(&self, name: &str) -> Option<Ty> {
        for scope in self.type_env.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty.clone());
            }
        }
        None
    }

    fn resolve_type_binding(&mut self, name: &str) -> Option<Ty> {
        for idx in (0..self.type_env.len()).rev() {
            if let Some(ty) = self.type_env[idx].get(name).cloned() {
                let resolved = self.materialize_const_type(ty);
                self.type_env[idx].insert(name.to_string(), resolved.clone());
                return Some(resolved);
            }
        }
        if let Some(ty) = self.global_types.get(name).cloned() {
            let resolved = self.materialize_const_type(ty);
            self.global_types.insert(name.to_string(), resolved.clone());
            return Some(resolved);
        }
        if self.imported_types.contains(name) {
            self.emit_error(format!(
                "imported type '{}' cannot be resolved without module execution",
                name
            ));
        }
        None
    }

    fn materialize_const_type(&mut self, ty: Ty) -> Ty {
        let Ty::Expr(mut expr) = ty else {
            return ty;
        };
        if let ExprKind::ConstBlock(_) = expr.kind() {
            if let Value::Type(resolved) = self.eval_expr(expr.as_mut()) {
                return resolved;
            }
        }
        Ty::Expr(expr)
    }

    fn handle_import(&mut self, import: &ItemImport) {
        let context = match self.module_resolution.clone() {
            Some(context) => context,
            None => {
                // Imports are resolved during later lowering stages; const-eval ignores them
                // when no module graph is available.
                return;
            }
        };

        let current_module = context
            .current_module
            .as_ref()
            .and_then(|module_id| context.graph.module(module_id));
        let language = current_module
            .map(|module| module.language.clone())
            .unwrap_or(ModuleLanguage::Ferro);
        let resolver = match context.resolvers.resolver_for(&language) {
            Some(resolver) => resolver,
            None => {
                self.emit_error(format!(
                    "no resolver registered for module language {:?}",
                    language
                ));
                return;
            }
        };

        for directive in self.expand_import_tree(&import.tree) {
            let module_spec = ModuleImport::new(directive.module_spec.clone());
            let module_id =
                match resolver.resolve_module(&module_spec, current_module, &context.graph) {
                    Ok(module_id) => module_id,
                    Err(err) => {
                        self.emit_resolver_error("import", err);
                        continue;
                    }
                };
            let Some(module) = context.graph.module(&module_id) else {
                self.emit_error(format!("import resolved to unknown module {}", module_id));
                continue;
            };

            match directive.binding {
                ImportBinding::Module { alias } => {
                    let name =
                        alias.or_else(|| self.module_alias_from_spec(&directive.module_spec));
                    let Some(name) = name else {
                        self.emit_error(format!(
                            "cannot import module '{}' without a binding name",
                            directive.module_spec
                        ));
                        continue;
                    };
                    self.register_imported_module(name, module_id.clone());
                }
                ImportBinding::Glob => {
                    let exports = match resolver.list_exports(module, &context.graph) {
                        Ok(exports) => exports,
                        Err(err) => {
                            self.emit_resolver_error("import", err);
                            continue;
                        }
                    };
                    for (name, symbol) in exports {
                        self.register_imported_symbol(name, module_id.clone(), symbol);
                    }
                }
                ImportBinding::Symbol { name, alias } => {
                    match resolver.resolve_symbol(module, &name, &context.graph) {
                        Ok(ResolvedSymbol::Symbol(symbol)) => {
                            let binding = alias.unwrap_or_else(|| name.clone());
                            self.register_imported_symbol(binding, module_id.clone(), symbol);
                        }
                        Ok(ResolvedSymbol::Module(symbol_module)) => {
                            let binding = alias.unwrap_or_else(|| name.clone());
                            self.register_imported_module(binding, symbol_module);
                        }
                        Err(err) => {
                            self.emit_resolver_error("import", err);
                        }
                    }
                }
            }
        }
    }

    fn expand_import_tree(&mut self, tree: &ItemImportTree) -> Vec<ImportDirective> {
        let mut out = Vec::new();
        let prefix = Vec::new();
        self.collect_import_tree(prefix, tree, &mut out);
        out
    }

    fn collect_import_tree(
        &mut self,
        prefix: Vec<ImportSegment>,
        tree: &ItemImportTree,
        out: &mut Vec<ImportDirective>,
    ) {
        match tree {
            ItemImportTree::Root => self.push_segment(prefix, ImportSegment::Root, out),
            ItemImportTree::SelfMod => self.push_segment(prefix, ImportSegment::SelfMod, out),
            ItemImportTree::SuperMod => self.push_segment(prefix, ImportSegment::Super, out),
            ItemImportTree::Crate => self.push_segment(prefix, ImportSegment::Crate, out),
            ItemImportTree::Ident(ident) => self.push_segment(
                prefix,
                ImportSegment::Ident(ident.as_str().to_string()),
                out,
            ),
            ItemImportTree::Rename(rename) => {
                let module_spec = self.render_import_path(&prefix);
                out.push(ImportDirective {
                    module_spec,
                    binding: ImportBinding::Symbol {
                        name: rename.from.as_str().to_string(),
                        alias: Some(rename.to.as_str().to_string()),
                    },
                });
            }
            ItemImportTree::Glob => {
                let module_spec = self.render_import_path(&prefix);
                out.push(ImportDirective {
                    module_spec,
                    binding: ImportBinding::Glob,
                });
            }
            ItemImportTree::Group(group) => {
                for item in &group.items {
                    self.collect_import_tree(prefix.clone(), item, out);
                }
            }
            ItemImportTree::Path(path) => {
                let mut cursor = prefix;
                for segment in &path.segments {
                    match segment {
                        ItemImportTree::Group(group) => {
                            for item in &group.items {
                                self.collect_import_tree(cursor.clone(), item, out);
                            }
                            return;
                        }
                        ItemImportTree::Glob => {
                            let module_spec = self.render_import_path(&cursor);
                            out.push(ImportDirective {
                                module_spec,
                                binding: ImportBinding::Glob,
                            });
                            return;
                        }
                        ItemImportTree::Rename(rename) => {
                            let module_spec = self.render_import_path(&cursor);
                            out.push(ImportDirective {
                                module_spec,
                                binding: ImportBinding::Symbol {
                                    name: rename.from.as_str().to_string(),
                                    alias: Some(rename.to.as_str().to_string()),
                                },
                            });
                            return;
                        }
                        ItemImportTree::Root => cursor.push(ImportSegment::Root),
                        ItemImportTree::SelfMod => cursor.push(ImportSegment::SelfMod),
                        ItemImportTree::SuperMod => cursor.push(ImportSegment::Super),
                        ItemImportTree::Crate => cursor.push(ImportSegment::Crate),
                        ItemImportTree::Ident(ident) => {
                            cursor.push(ImportSegment::Ident(ident.as_str().to_string()))
                        }
                        ItemImportTree::Path(inner) => {
                            self.collect_import_tree(
                                cursor.clone(),
                                &ItemImportTree::Path(inner.clone()),
                                out,
                            );
                            return;
                        }
                    }
                }
                if cursor.is_empty() {
                    self.emit_error("import path cannot be empty");
                    return;
                }
                let module_spec = self.render_import_path(&cursor);
                let alias = cursor.iter().rev().find_map(|segment| match segment {
                    ImportSegment::Ident(name) => Some(name.clone()),
                    _ => None,
                });
                out.push(ImportDirective {
                    module_spec,
                    binding: ImportBinding::Module { alias },
                });
            }
        }
    }

    fn push_segment(
        &mut self,
        mut prefix: Vec<ImportSegment>,
        segment: ImportSegment,
        out: &mut Vec<ImportDirective>,
    ) {
        prefix.push(segment);
        if prefix.is_empty() {
            self.emit_error("import path cannot be empty");
            return;
        }
        let module_spec = self.render_import_path(&prefix);
        let alias = prefix.iter().rev().find_map(|seg| match seg {
            ImportSegment::Ident(name) => Some(name.clone()),
            _ => None,
        });
        out.push(ImportDirective {
            module_spec,
            binding: ImportBinding::Module { alias },
        });
    }

    fn render_import_path(&self, segments: &[ImportSegment]) -> String {
        let mut parts = Vec::new();
        let mut has_root = false;
        for segment in segments {
            match segment {
                ImportSegment::Root => has_root = true,
                ImportSegment::SelfMod => parts.push("self".to_string()),
                ImportSegment::Super => parts.push("super".to_string()),
                ImportSegment::Crate => parts.push("crate".to_string()),
                ImportSegment::Ident(name) => parts.push(name.clone()),
            }
        }
        let joined = parts.join("::");
        if has_root {
            if joined.is_empty() {
                "::".to_string()
            } else {
                format!("::{}", joined)
            }
        } else {
            joined
        }
    }

    fn module_alias_from_spec(&self, spec: &str) -> Option<String> {
        spec.rsplit("::").next().and_then(|segment| {
            if segment.is_empty() {
                None
            } else {
                Some(segment.to_string())
            }
        })
    }

    fn register_imported_symbol(
        &mut self,
        name: String,
        module: ModuleId,
        symbol: SymbolDescriptor,
    ) {
        if matches!(
            symbol.symbol_kind,
            SymbolKind::Struct | SymbolKind::Enum | SymbolKind::Trait | SymbolKind::Type
        ) {
            self.imported_types.insert(name.clone());
        }
        self.imported_symbols.insert(name.clone(), symbol.clone());
        self.insert_value(
            &name,
            Value::Any(AnyBox::new(ImportedSymbol { module, symbol })),
        );
    }

    fn register_imported_module(&mut self, name: String, module: ModuleId) {
        self.imported_modules.insert(name.clone(), module.clone());
        self.insert_value(&name, Value::Any(AnyBox::new(ImportedModule { module })));
    }

    fn emit_resolver_error(&mut self, context: &str, err: ResolverError) {
        self.emit_error(format!("{} resolution failed: {}", context, err));
    }

    fn imported_placeholder_message(&self, value: &Value) -> Option<String> {
        if let Value::Any(any) = value {
            if let Some(symbol) = any.downcast_ref::<ImportedSymbol>() {
                return Some(format!(
                    "imported symbol '{}' from module {} cannot be evaluated without module execution",
                    symbol.symbol.name,
                    symbol.module
                ));
            }
            if let Some(module) = any.downcast_ref::<ImportedModule>() {
                return Some(format!(
                    "imported module {} cannot be evaluated as a value",
                    module.module
                ));
            }
        }
        None
    }

    fn imported_placeholder_value(&mut self, value: Value) -> Option<Value> {
        if let Some(message) = self.imported_placeholder_message(&value) {
            self.emit_error(message);
            return Some(Value::undefined());
        }
        None
    }

    fn resolve_imported_symbol_path(&mut self, symbol: &str) -> Option<Value> {
        let (module_spec, name) = symbol.rsplit_once("::")?;
        let context = self.module_resolution.as_ref()?;
        let current_module = context
            .current_module
            .as_ref()
            .and_then(|module_id| context.graph.module(module_id));
        let language = current_module
            .map(|module| module.language.clone())
            .unwrap_or(ModuleLanguage::Ferro);
        let resolver = context.resolvers.resolver_for(&language)?;
        let module_import = ModuleImport::new(module_spec);
        let module_id = resolver
            .resolve_module(&module_import, current_module, &context.graph)
            .ok()?;
        let module = context.graph.module(&module_id)?;
        match resolver.resolve_symbol(module, name, &context.graph) {
            Ok(ResolvedSymbol::Symbol(symbol_desc)) => {
                Some(Value::Any(AnyBox::new(ImportedSymbol {
                    module: module_id,
                    symbol: symbol_desc,
                })))
            }
            Ok(ResolvedSymbol::Module(symbol_module)) => {
                Some(Value::Any(AnyBox::new(ImportedModule {
                    module: symbol_module,
                })))
            }
            Err(_) => None,
        }
    }

    fn resolve_qualified(&mut self, symbol: String) -> Value {
        if let Some(primitive) = Self::primitive_type_from_name(&symbol) {
            return Value::Type(Ty::Primitive(primitive));
        }
        if let Some(value) = self.evaluated_constants.get(&symbol) {
            return value.clone();
        }
        if let Some(symbol) = self.imported_symbols.get(&symbol) {
            self.emit_error(format!(
                "imported symbol '{}' cannot be evaluated without module execution",
                symbol.name
            ));
            return Value::undefined();
        }
        if symbol == "Self" {
            if let Some(self_ty) = self.impl_stack.last().and_then(|ctx| ctx.self_ty.clone()) {
                if let Some(ty) = self.resolve_type_binding(&self_ty) {
                    return Value::Type(ty);
                }
            }
        }
        if let Some(ty) = self.resolve_type_binding(&symbol) {
            return Value::Type(ty);
        }
        if symbol == "printf" {
            return Value::unit();
        }
        if symbol.contains("::") {
            if let Some(value) = self.resolve_imported_symbol_path(&symbol) {
                if let Some(placeholder) = self.imported_placeholder_value(value.clone()) {
                    return placeholder;
                }
                return value;
            }
        }
        self.emit_error(format!(
            "unresolved symbol '{}' in const evaluation",
            symbol
        ));
        Value::undefined()
    }

    fn primitive_type_from_name(name: &str) -> Option<TypePrimitive> {
        match name {
            "i64" => Some(TypePrimitive::Int(TypeInt::I64)),
            "u64" => Some(TypePrimitive::Int(TypeInt::U64)),
            "i32" => Some(TypePrimitive::Int(TypeInt::I32)),
            "u32" => Some(TypePrimitive::Int(TypeInt::U32)),
            "i16" => Some(TypePrimitive::Int(TypeInt::I16)),
            "u16" => Some(TypePrimitive::Int(TypeInt::U16)),
            "i8" => Some(TypePrimitive::Int(TypeInt::I8)),
            "u8" => Some(TypePrimitive::Int(TypeInt::U8)),
            "isize" => Some(TypePrimitive::Int(TypeInt::I64)),
            "usize" => Some(TypePrimitive::Int(TypeInt::U64)),
            "bool" => Some(TypePrimitive::Bool),
            "char" => Some(TypePrimitive::Char),
            "str" | "String" => Some(TypePrimitive::String),
            _ => None,
        }
    }

    fn qualified_name(&self, name: &str) -> String {
        if self.is_printf_symbol(name) {
            return name.trim_start_matches("#").to_string();
        }

        if self.module_stack.is_empty() {
            name.to_string()
        } else {
            let mut qualified = self.module_stack.join("::");
            qualified.push_str("::");
            qualified.push_str(name);
            qualified
        }
    }

    fn update_mutable_constant(&mut self, name: &str, value: Value) {
        let qualified = self.qualified_name(name);
        let mut keys = Vec::new();
        if self.evaluated_constants.contains_key(&qualified) {
            keys.push(qualified.clone());
        }
        if self.evaluated_constants.contains_key(name) && name != qualified {
            keys.push(name.to_string());
        }
        for key in keys {
            self.evaluated_constants.insert(key, value.clone());
        }
        let mut target_keys = Vec::new();
        if self.mutable_const_targets.contains_key(&qualified) {
            target_keys.push(qualified);
        }
        if self.mutable_const_targets.contains_key(name) {
            target_keys.push(name.to_string());
        }
        for key in target_keys {
            if let Some(target) = self.mutable_const_targets.get(&key) {
                let mut expr_value = Expr::value(value.clone());
                if let Some(ty) = target.ty.clone() {
                    expr_value.set_ty(ty);
                }
                unsafe {
                    *target.expr_ptr = expr_value;
                }
                self.mark_mutated();
            }
        }
    }

    fn is_printf_symbol(&self, name: &str) -> bool {
        let base = name.trim_start_matches("#");
        base == "printf" || base.ends_with("::printf")
    }

    fn push_scope(&mut self) {
        self.value_env.push(HashMap::new());
        self.type_env.push(HashMap::new());
        if let Some(typer) = self.typer.as_mut() {
            typer.push_scope();
        }
    }

    fn pop_scope(&mut self) {
        self.value_env.pop();
        self.type_env.pop();
        if let Some(typer) = self.typer.as_mut() {
            typer.pop_scope();
        }
    }

    fn emit_error(&mut self, message: impl Into<String>) {
        let diagnostic =
            Diagnostic::error(message.into()).with_source_context(self.diagnostic_context);
        self.push_diagnostic(diagnostic);
    }

    fn emit_warning(&mut self, message: impl Into<String>) {
        let diagnostic =
            Diagnostic::warning(message.into()).with_source_context(self.diagnostic_context);
        self.push_diagnostic(diagnostic);
    }

    fn push_diagnostic(&mut self, diagnostic: Diagnostic) {
        if let Some(manager) = &self.diag_manager {
            manager.add_diagnostic(diagnostic.clone());
        }
        if diagnostic.level == DiagnosticLevel::Error {
            self.has_errors = true;
        }
        self.diagnostics.push(diagnostic);
    }

    fn handle_result(&mut self, result: Result<Value>) -> Value {
        match result {
            Ok(value) => value,
            Err(err) => {
                self.emit_error(err.to_string());
                Value::undefined()
            }
        }
    }

    // build_quoted_fragment moved to quote.rs
}

fn is_quote_only_item(item: &Item) -> bool {
    match item.kind() {
        ItemKind::DefFunction(func) => {
            func.sig.quote_kind.is_some() || matches!(func.sig.ret_ty.as_ref(), Some(Ty::Quote(_)))
        }
        ItemKind::DeclFunction(func) => {
            func.sig.quote_kind.is_some() || matches!(func.sig.ret_ty.as_ref(), Some(Ty::Quote(_)))
        }
        ItemKind::DefConst(def) => {
            let has_quote_ty = def
                .ty_annotation()
                .or_else(|| def.ty.as_ref())
                .map(|ty| matches!(ty, Ty::Quote(_)))
                .unwrap_or(false);
            let has_quote_value = match def.value.kind() {
                ExprKind::Value(value) => match value.as_ref() {
                    Value::QuoteToken(_) => true,
                    Value::List(list) => {
                        !list.values.is_empty()
                            && list
                                .values
                                .iter()
                                .all(|value| matches!(value, Value::QuoteToken(_)))
                    }
                    _ => false,
                },
                _ => false,
            };
            has_quote_ty || has_quote_value
        }
        _ => false,
    }
}

fn intrinsic_symbol(kind: IntrinsicCallKind) -> Option<&'static str> {
    match kind {
        IntrinsicCallKind::SizeOf => Some("sizeof!"),
        IntrinsicCallKind::ReflectFields => Some("reflect_fields!"),
        IntrinsicCallKind::HasMethod => Some("hasmethod!"),
        IntrinsicCallKind::TypeName => Some("type_name!"),
        IntrinsicCallKind::CreateStruct => Some("create_struct!"),
        IntrinsicCallKind::CloneStruct => Some("clone_struct!"),
        IntrinsicCallKind::AddField => Some("addfield!"),
        IntrinsicCallKind::HasField => Some("hasfield!"),
        IntrinsicCallKind::FieldCount => Some("field_count!"),
        IntrinsicCallKind::MethodCount => Some("method_count!"),
        IntrinsicCallKind::FieldType => Some("field_type!"),
        IntrinsicCallKind::StructSize => Some("struct_size!"),
        IntrinsicCallKind::GenerateMethod => Some("generate_method!"),
        IntrinsicCallKind::CompileError => Some("compile_error!"),
        IntrinsicCallKind::CompileWarning => Some("compile_warning!"),
        _ => None,
    }
}
