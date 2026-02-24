use std::collections::{HashMap, HashSet, VecDeque};
use std::io::Write;
use std::str::FromStr;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

use crate::engine::ffi::FfiRuntime;
use crate::engine::macro_rules::{expand_macro, parse_macro_rules, MacroRulesDefinition};
use crate::error::interpretation_error;
use crate::intrinsics::IntrinsicFunction;
use crate::intrinsics::IntrinsicsRegistry;
use fp_core::ast::DecimalType;
use fp_core::ast::{
    Abi, AttrMeta, AttrMetaNameValue, Attribute, BlockStmt, Expr, ExprBlock, ExprClosure,
    ExprField, ExprIntrinsicCall, ExprInvoke, ExprInvokeTarget, ExprKind, ExprQuote, ExprRange,
    ExprRangeLimit, ExprStringTemplate, FormatArgRef, FormatTemplatePart, FunctionParam,
    FunctionSignature, Item, ItemDeclFunction, ItemDefFunction, ItemImport, ItemImportTree,
    ItemKind, MacroDelimiter, MacroGroup, MacroInvocation, MacroToken, MacroTokenTree, Node,
    NodeKind, Path, QuoteFragmentKind, QuoteTokenValue, StmtLet, StructuralField, Ty, TypeAny,
    TypeArray, TypeBinaryOpKind, TypeFunction, TypeInt, TypePrimitive, TypeQuote, TypeReference,
    TypeSlice, TypeStruct, TypeStructural, TypeTokenStream, TypeTuple, TypeUnit, TypeVec,
    Value, ValueField, ValueFunction, ValueList, ValueStruct, ValueStructural, ValueTokenStream,
    ValueTuple,
};
use fp_core::ast::{Ident, Name};
use fp_core::ast::{Pattern, PatternKind};
use fp_core::context::SharedScopedContext;
use fp_core::cfg::TargetEnv;
use fp_core::diagnostics::{Diagnostic, DiagnosticLevel, DiagnosticManager};
use fp_core::error::Result;
use fp_core::intrinsics::{IntrinsicCallKind, IntrinsicNormalizer};
use fp_core::module::path::{parse_path, PathPrefix};
use fp_core::module::resolver::{ModuleImport, ResolvedSymbol, ResolverError, ResolverRegistry};
use fp_core::module::{ModuleId, ModuleLanguage, SymbolDescriptor, SymbolKind};
use fp_core::ops::{format_runtime_string, format_value_with_spec, BinOpKind, UnOpKind};
use fp_core::package::graph::PackageGraph;
use fp_core::span::Span;
use fp_core::utils::anybox::AnyBox;
use fp_typing::runtime_types::{resolve_type_binding_match, TypeBindingMatch};
use fp_typing::{AstTypeInferencer, TypeResolutionHook};
use num_traits::ToPrimitive;
use proc_macro2::{Delimiter, TokenTree};
mod blocks;
mod closures;
mod const_regions;
mod env;
mod eval_expr;
mod eval_stmt;
mod ffi;
mod intrinsics;
mod macro_rules;
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ResolutionMode {
    Default,
    Attribute,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MacroExpansionContext {
    Item,
    Expr,
    Type,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ProcMacroKind {
    FunctionLike,
    Attribute,
    Derive,
}

#[derive(Debug, Clone)]
struct ProcMacroDefinition {
    kind: ProcMacroKind,
    function: ItemDefFunction,
}

#[derive(Debug, Clone)]
struct SelectMacroArm {
    name: String,
    future: Vec<MacroTokenTree>,
    body: Vec<MacroTokenTree>,
}

struct InterpreterTypeHook<'ctx> {
    interpreter: *mut AstInterpreter<'ctx>,
}

impl<'ctx> TypeResolutionHook for InterpreterTypeHook<'ctx> {
    fn resolve_symbol(&mut self, name: &str) -> bool {
        unsafe {
            self.interpreter
                .as_mut()
                .map(|interpreter| interpreter.materialize_symbol(name))
                .unwrap_or(false)
        }
    }
}

#[derive(Clone)]
pub struct InterpreterOptions {
    pub mode: InterpreterMode,
    pub target_env: TargetEnv,
    pub debug_assertions: bool,
    pub diagnostics: Option<Arc<DiagnosticManager>>,
    pub diagnostic_context: &'static str,
    // Optional module resolution context for handling `use`/imports during evaluation.
    pub module_resolution: Option<ModuleResolutionContext>,
    pub macro_parser: Option<Arc<dyn fp_core::ast::MacroExpansionParser>>,
    pub intrinsic_normalizer: Option<Arc<dyn IntrinsicNormalizer>>,
    pub stdout_mode: StdoutMode,
}

impl Default for InterpreterOptions {
    fn default() -> Self {
        Self {
            mode: InterpreterMode::CompileTime,
            target_env: TargetEnv::host(),
            debug_assertions: false,
            diagnostics: None,
            diagnostic_context: DEFAULT_DIAGNOSTIC_CONTEXT,
            module_resolution: None,
            macro_parser: None,
            intrinsic_normalizer: None,
            stdout_mode: StdoutMode::Capture,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum StdoutMode {
    Capture,
    Inherit,
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
pub enum EvalStepOutcome {
    Yielded,
    Complete(Value),
}

#[derive(Debug, Clone, PartialEq)]
pub enum RuntimeStepOutcome {
    Yielded,
    Complete(RuntimeFlow),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InterpreterStackSnapshot {
    pub call_frames: usize,
    pub expr_frames: usize,
    pub const_values: usize,
    pub runtime_values: usize,
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

#[derive(Debug, Clone)]
struct RuntimeRef {
    shared: Arc<Mutex<Value>>,
}

impl PartialEq for RuntimeRef {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.shared, &other.shared)
    }
}

#[derive(Debug, Clone, PartialEq)]
struct RuntimeBox {
    value: Value,
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

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub enum RuntimeFlow {
    Value(Value),
    Break(Option<Value>),
    Continue,
    Return(Option<Value>),
    Panic(Value),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum EvalMode {
    Const,
    Runtime,
}

type ExprDiscriminant = std::mem::Discriminant<ExprKind>;

#[derive(Debug, Clone, Copy)]
struct ExprFrame {
    mode: EvalMode,
    span: Option<Span>,
    kind: ExprDiscriminant,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum CallFrameKind {
    Function(String),
    Method(String),
    ValueFunction,
    Closure,
    ConstClosure,
}

#[derive(Debug, Clone)]
struct CallFrame {
    mode: EvalMode,
    kind: CallFrameKind,
    span: Option<Span>,
    module_depth: usize,
    value_env_depth: usize,
    type_env_depth: usize,
    impl_depth: usize,
    loop_depth: usize,
    function_depth: usize,
}

struct ExprFrameGuard<'ctx> {
    interpreter: *mut AstInterpreter<'ctx>,
}

impl<'ctx> Drop for ExprFrameGuard<'ctx> {
    fn drop(&mut self) {
        unsafe {
            if let Some(interpreter) = self.interpreter.as_mut() {
                interpreter.expr_stack.pop();
            }
        }
    }
}

struct CallFrameGuard<'ctx> {
    interpreter: *mut AstInterpreter<'ctx>,
}

impl<'ctx> Drop for CallFrameGuard<'ctx> {
    fn drop(&mut self) {
        unsafe {
            if let Some(interpreter) = self.interpreter.as_mut() {
                interpreter.call_stack.pop();
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct RuntimeFuture {
    expr: Box<Expr>,
    value_env: Vec<HashMap<String, StoredValue>>,
    type_env: Vec<HashMap<String, Ty>>,
    module_stack: Vec<String>,
    impl_stack: Vec<ImplContext>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct TaskHandle {
    id: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TaskStatus {
    Pending,
    Ready,
    Panicked,
}

#[derive(Debug, Clone)]
struct TaskState {
    id: u64,
    expr: Box<Expr>,
    value_env: Vec<HashMap<String, StoredValue>>,
    type_env: Vec<HashMap<String, Ty>>,
    module_stack: Vec<String>,
    impl_stack: Vec<ImplContext>,
    runtime_tasks: Vec<RuntimeTask>,
    runtime_value_stack: Vec<RuntimeFlow>,
    block_stack: Vec<BlockFrame>,
    expr_stack: Vec<ExprFrame>,
    call_stack: Vec<CallFrame>,
    loop_depth: usize,
    function_depth: usize,
    in_const_region: usize,
    status: TaskStatus,
    result: Option<Value>,
    panic: Option<Value>,
    sleep_until: Option<Instant>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ActiveEval {
    Const,
    Runtime,
}

#[derive(Debug, Clone, Copy)]
enum CollectKind {
    Tuple,
    Array,
}

#[derive(Debug, Clone)]
enum ConstTask {
    Eval(*mut Expr),
    PushValue(Value),
    ApplyBinOp(BinOpKind),
    ApplyUnOp(UnOpKind),
    ApplyIf {
        then_expr: *mut Expr,
        else_expr: Option<*mut Expr>,
    },
    ApplyCollect {
        len: usize,
        kind: CollectKind,
    },
    ApplyCast {
        ty: Ty,
    },
    ApplyRange {
        inclusive: bool,
    },
    ApplySelect {
        field: String,
    },
    ApplyIndex {
        has_start: bool,
        has_end: bool,
        inclusive: bool,
    },
}

#[derive(Debug, Clone)]
enum RuntimeTask {
    Eval(*mut Expr),
    PushValue(Value),
    ApplyBinOp(BinOpKind),
    ApplyUnOp(UnOpKind),
    ApplyIf {
        then_expr: *mut Expr,
        else_expr: Option<*mut Expr>,
    },
    EvalBlock {
        block: *mut ExprBlock,
        idx: usize,
    },
    ApplyBlockValue {
        has_value: bool,
    },
    ApplyLet {
        pat: Pattern,
    },
    ApplyCollect {
        len: usize,
        kind: CollectKind,
    },
    ApplyCast {
        ty: Ty,
    },
    ApplyRange {
        inclusive: bool,
    },
    ApplySelect {
        field: String,
    },
    ApplyIndex {
        has_start: bool,
        has_end: bool,
        inclusive: bool,
    },
}

#[derive(Debug, Clone)]
struct BlockFrame {
    last_value: Value,
}

#[derive(Debug, Clone)]
struct ReceiverBinding {
    value: Value,
    shared: Option<Arc<Mutex<Value>>>,
}

struct SpanGuard<'ctx> {
    interpreter: *mut AstInterpreter<'ctx>,
    prev: Option<Span>,
}

impl<'ctx> Drop for SpanGuard<'ctx> {
    fn drop(&mut self) {
        unsafe {
            (*self.interpreter).current_span = self.prev;
        }
    }
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
    macro_parser: Option<Arc<dyn fp_core::ast::MacroExpansionParser>>,
    intrinsic_normalizer: Option<Arc<dyn IntrinsicNormalizer>>,
    stdout_mode: StdoutMode,

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
    extern_functions: HashMap<String, FunctionSignature>,
    ffi_runtime: Option<FfiRuntime>,
    specialization_cache: HashMap<String, HashMap<String, String>>,
    specialization_counter: HashMap<String, usize>,
    pending_items: Vec<Vec<Item>>,
    pending_stmt_splices: Vec<Vec<BlockStmt>>,
    mutations_applied: bool,
    macro_env: Vec<HashMap<String, MacroRulesDefinition>>,
    proc_macro_env: Vec<HashMap<String, ProcMacroDefinition>>,
    macro_depth: usize,
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
    local_imports: HashMap<String, String>,
    loop_depth: usize,
    function_depth: usize,
    current_span: Option<Span>,
    lazy_evaluated: HashSet<String>,
    item_scopes: Vec<*mut Vec<Item>>,
    root_items: Option<*mut Vec<Item>>,
    call_stack: Vec<CallFrame>,
    expr_stack: Vec<ExprFrame>,
    const_value_stack: Vec<Value>,
    runtime_value_stack: Vec<RuntimeFlow>,
    const_tasks: Vec<ConstTask>,
    runtime_tasks: Vec<RuntimeTask>,
    block_stack: Vec<BlockFrame>,
    stack_eval_active: bool,
    active_eval: Option<ActiveEval>,
    eval_scope_pushed: bool,
    task_counter: u64,
    tasks: HashMap<u64, TaskState>,
    ready_tasks: VecDeque<u64>,
    current_task: Option<u64>,
    task_should_yield: bool,
    current_task_sleep_until: Option<Instant>,
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
            macro_parser: options.macro_parser.clone(),
            intrinsic_normalizer: options.intrinsic_normalizer.clone(),
            stdout_mode: options.stdout_mode,
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
            extern_functions: HashMap::new(),
            ffi_runtime: None,
            specialization_cache: HashMap::new(),
            specialization_counter: HashMap::new(),
            pending_items: Vec::new(),
            pending_stmt_splices: Vec::new(),
            mutations_applied: false,
            macro_env: vec![HashMap::new()],
            proc_macro_env: vec![HashMap::new()],
            macro_depth: 0,
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
            local_imports: HashMap::new(),
            loop_depth: 0,
            function_depth: 0,
            current_span: None,
            lazy_evaluated: HashSet::new(),
            item_scopes: Vec::new(),
            root_items: None,
            call_stack: Vec::new(),
            expr_stack: Vec::new(),
            const_value_stack: Vec::new(),
            runtime_value_stack: Vec::new(),
            const_tasks: Vec::new(),
            runtime_tasks: Vec::new(),
            block_stack: Vec::new(),
            stack_eval_active: false,
            active_eval: None,
            eval_scope_pushed: false,
            task_counter: 0,
            tasks: HashMap::new(),
            ready_tasks: VecDeque::new(),
            current_task: None,
            task_should_yield: false,
            current_task_sleep_until: None,
        }
    }

    pub fn set_typer(&mut self, typer: AstTypeInferencer<'ctx>) {
        self.typer = Some(typer);
    }

    pub fn register_intrinsic(&mut self, name: impl Into<String>, func: IntrinsicFunction) {
        self.intrinsics.register(name, func);
    }

    pub fn enable_incremental_typing(&mut self, ast: &Node) {
        let mut typer = AstTypeInferencer::new().with_context(self.ctx);
        typer.initialize_from_node(ast);
        typer.initialize_imports_from_node(ast);
        typer.set_resolution_hook(Box::new(InterpreterTypeHook {
            interpreter: self as *mut _,
        }));
        self.typer = Some(typer);
    }

    pub fn interpret(&mut self, node: &mut Node) {
        match node.kind_mut() {
            NodeKind::File(file) => {
                let root_ptr = &mut file.items as *mut Vec<Item>;
                self.root_items = Some(root_ptr);
                self.push_item_scope(&mut file.items);
                self.pending_items.push(Vec::new());
                if let Some(typer) = self.typer.as_mut() {
                    for item in &file.items {
                        typer.initialize_from_item(item);
                    }
                }
                let mut idx = 0;
                while idx < file.items.len() {
                    if self.should_skip_lazy_item(&file.items[idx]) {
                        idx += 1;
                        continue;
                    }
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
                self.pop_item_scope();
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
                let value = self.finish_runtime_flow(flow);
                let should_await = self.extract_task_handle(&value).is_some()
                    || self.extract_runtime_future(&value).is_some();
                if should_await {
                    let awaited = self.await_runtime_value(value);
                    Some(self.finish_runtime_flow(awaited))
                } else {
                    Some(value)
                }
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

    pub fn begin_const_eval(&mut self, expr: &mut Expr) -> std::result::Result<(), String> {
        match self.active_eval {
            Some(ActiveEval::Const) => {
                return Err("const evaluation is already active".to_string());
            }
            Some(ActiveEval::Runtime) => {
                return Err("runtime evaluation is active; stop it before const eval".to_string());
            }
            None => {}
        }
        self.stack_eval_active = true;
        self.active_eval = Some(ActiveEval::Const);
        self.eval_scope_pushed = true;
        self.push_scope();
        self.const_tasks.clear();
        self.const_value_stack.clear();
        self.const_tasks.push(ConstTask::Eval(expr as *mut Expr));
        Ok(())
    }

    pub fn begin_runtime_eval(&mut self, expr: &mut Expr) -> std::result::Result<(), String> {
        match self.active_eval {
            Some(ActiveEval::Const) => {
                return Err("const evaluation is active; stop it before runtime eval".to_string());
            }
            Some(ActiveEval::Runtime) => {
                return Err("runtime evaluation is already active".to_string());
            }
            None => {}
        }
        self.stack_eval_active = true;
        self.active_eval = Some(ActiveEval::Runtime);
        self.eval_scope_pushed = true;
        self.push_scope();
        self.runtime_tasks.clear();
        self.runtime_value_stack.clear();
        self.block_stack.clear();
        self.runtime_tasks
            .push(RuntimeTask::Eval(expr as *mut Expr));
        Ok(())
    }

    pub fn step_const_eval(
        &mut self,
        max_steps: usize,
    ) -> std::result::Result<EvalStepOutcome, String> {
        match self.active_eval {
            Some(ActiveEval::Const) => {}
            Some(ActiveEval::Runtime) => {
                return Err("runtime evaluation is active; cannot step const eval".to_string());
            }
            None => return Err("const evaluation not started".to_string()),
        }
        let outcome = self.run_const_tasks_limit(max_steps);
        match outcome {
            EvalStepOutcome::Yielded => Ok(EvalStepOutcome::Yielded),
            EvalStepOutcome::Complete(value) => {
                self.finish_active_eval();
                Ok(EvalStepOutcome::Complete(value))
            }
        }
    }

    pub fn step_runtime_eval(
        &mut self,
        max_steps: usize,
    ) -> std::result::Result<RuntimeStepOutcome, String> {
        match self.active_eval {
            Some(ActiveEval::Runtime) => {}
            Some(ActiveEval::Const) => {
                return Err("const evaluation is active; cannot step runtime eval".to_string());
            }
            None => return Err("runtime evaluation not started".to_string()),
        }
        let outcome = self.run_runtime_tasks_limit(max_steps);
        match outcome {
            RuntimeStepOutcome::Yielded => Ok(RuntimeStepOutcome::Yielded),
            RuntimeStepOutcome::Complete(flow) => {
                self.finish_active_eval();
                Ok(RuntimeStepOutcome::Complete(flow))
            }
        }
    }

    pub fn stop_eval(&mut self) {
        self.finish_active_eval();
    }

    fn finish_active_eval(&mut self) {
        self.active_eval = None;
        self.stack_eval_active = false;
        self.const_tasks.clear();
        self.runtime_tasks.clear();
        self.block_stack.clear();
        if self.eval_scope_pushed {
            self.pop_scope();
            self.eval_scope_pushed = false;
        }
    }

    pub fn stack_snapshot(&self) -> InterpreterStackSnapshot {
        InterpreterStackSnapshot {
            call_frames: self.call_stack.len(),
            expr_frames: self.expr_stack.len(),
            const_values: self.const_value_stack.len(),
            runtime_values: self.runtime_value_stack.len(),
        }
    }

    fn capture_runtime_future(&self, expr: &Expr) -> RuntimeFuture {
        RuntimeFuture {
            expr: Box::new(expr.clone()),
            value_env: self.value_env.clone(),
            type_env: self.type_env.clone(),
            module_stack: self.module_stack.clone(),
            impl_stack: self.impl_stack.clone(),
        }
    }

    fn spawn_runtime_future(&mut self, future: RuntimeFuture) -> TaskHandle {
        let id = self.task_counter;
        self.task_counter += 1;
        let state = TaskState {
            id,
            expr: future.expr,
            value_env: future.value_env,
            type_env: future.type_env,
            module_stack: future.module_stack,
            impl_stack: future.impl_stack,
            runtime_tasks: Vec::new(),
            runtime_value_stack: Vec::new(),
            block_stack: Vec::new(),
            expr_stack: Vec::new(),
            call_stack: Vec::new(),
            loop_depth: 0,
            function_depth: 0,
            in_const_region: 0,
            status: TaskStatus::Pending,
            result: None,
            panic: None,
            sleep_until: None,
        };
        self.tasks.insert(id, state);
        self.ready_tasks.push_back(id);
        TaskHandle { id }
    }

    fn task_result(&self, id: u64) -> Option<RuntimeFlow> {
        let task = self.tasks.get(&id)?;
        match task.status {
            TaskStatus::Ready => task.result.clone().map(RuntimeFlow::Value),
            TaskStatus::Panicked => task.panic.clone().map(RuntimeFlow::Panic),
            TaskStatus::Pending => None,
        }
    }

    fn run_scheduler_until_task(&mut self, id: u64) -> RuntimeFlow {
        loop {
            if let Some(result) = self.task_result(id) {
                return result;
            }
            if !self.tick_scheduler() {
                if let Some(task) = self.tasks.get(&id) {
                    if task.status == TaskStatus::Pending && task.sleep_until.is_none() {
                        self.ready_tasks.push_back(id);
                        continue;
                    }
                }
                self.emit_error("no runnable tasks available during join");
                return RuntimeFlow::Value(Value::undefined());
            }
        }
    }

    fn run_scheduler_until_any(&mut self, ids: &[u64]) -> Option<(usize, RuntimeFlow)> {
        loop {
            for (idx, id) in ids.iter().enumerate() {
                if let Some(result) = self.task_result(*id) {
                    return Some((idx, result));
                }
            }
            if !self.tick_scheduler() {
                return None;
            }
        }
    }

    fn tick_scheduler(&mut self) -> bool {
        self.wake_sleeping_tasks();
        let Some(task_id) = self.ready_tasks.pop_front() else {
            return self.sleep_until_next_task();
        };
        if let Some(result) = self.run_task_steps(task_id, 128) {
            if let Some(task) = self.tasks.get_mut(&task_id) {
                match result {
                    RuntimeFlow::Value(value) => {
                        task.status = TaskStatus::Ready;
                        task.result = Some(value);
                    }
                    RuntimeFlow::Panic(value) => {
                        task.status = TaskStatus::Panicked;
                        task.panic = Some(value);
                    }
                    _ => {
                        task.status = TaskStatus::Ready;
                        task.result = Some(Value::undefined());
                    }
                }
            }
        }
        true
    }

    fn sleep_until_next_task(&mut self) -> bool {
        let mut next = None;
        for task in self.tasks.values() {
            if let Some(deadline) = task.sleep_until {
                next = match next {
                    Some(current) if current <= deadline => Some(current),
                    _ => Some(deadline),
                };
            }
        }
        let Some(deadline) = next else {
            return false;
        };
        let now = Instant::now();
        if deadline > now {
            std::thread::sleep(deadline - now);
        }
        self.wake_sleeping_tasks();
        true
    }

    fn wake_sleeping_tasks(&mut self) {
        let now = Instant::now();
        let ready: Vec<u64> = self
            .tasks
            .iter()
            .filter_map(|(id, task)| {
                if task.status != TaskStatus::Pending {
                    return None;
                }
                match task.sleep_until {
                    Some(deadline) if deadline <= now => Some(*id),
                    _ => None,
                }
            })
            .collect();
        for id in ready {
            if let Some(task) = self.tasks.get_mut(&id) {
                task.sleep_until = None;
            }
            self.ready_tasks.push_back(id);
        }
    }

    fn run_task_steps(&mut self, id: u64, steps: usize) -> Option<RuntimeFlow> {
        let Some(mut task) = self.tasks.remove(&id) else {
            return Some(RuntimeFlow::Value(Value::undefined()));
        };

        let saved_value_env = std::mem::replace(&mut self.value_env, task.value_env.clone());
        let saved_type_env = std::mem::replace(&mut self.type_env, task.type_env.clone());
        let saved_module_stack =
            std::mem::replace(&mut self.module_stack, task.module_stack.clone());
        let saved_impl_stack = std::mem::replace(&mut self.impl_stack, task.impl_stack.clone());
        let saved_runtime_tasks =
            std::mem::replace(&mut self.runtime_tasks, task.runtime_tasks.clone());
        let saved_runtime_stack = std::mem::replace(
            &mut self.runtime_value_stack,
            task.runtime_value_stack.clone(),
        );
        let saved_block_stack = std::mem::replace(&mut self.block_stack, task.block_stack.clone());
        let saved_expr_stack = std::mem::replace(&mut self.expr_stack, task.expr_stack.clone());
        let saved_call_stack = std::mem::replace(&mut self.call_stack, task.call_stack.clone());
        let saved_loop_depth = self.loop_depth;
        let saved_function_depth = self.function_depth;
        let saved_in_const = self.in_const_region;
        let saved_current_task = self.current_task;
        let saved_task_should_yield = self.task_should_yield;
        let saved_task_sleep_until = self.current_task_sleep_until;
        let saved_stack_eval_active = self.stack_eval_active;

        self.loop_depth = task.loop_depth;
        self.function_depth = task.function_depth;
        self.in_const_region = task.in_const_region;
        self.current_task = Some(id);
        self.task_should_yield = false;
        self.current_task_sleep_until = None;
        self.stack_eval_active = true;

        if self.runtime_tasks.is_empty() {
            self.runtime_tasks
                .push(RuntimeTask::Eval(task.expr.as_mut() as *mut Expr));
        }

        let outcome = self.run_runtime_tasks_limit(steps);

        task.value_env = std::mem::replace(&mut self.value_env, saved_value_env);
        task.type_env = std::mem::replace(&mut self.type_env, saved_type_env);
        task.module_stack = std::mem::replace(&mut self.module_stack, saved_module_stack);
        task.impl_stack = std::mem::replace(&mut self.impl_stack, saved_impl_stack);
        task.runtime_tasks = std::mem::replace(&mut self.runtime_tasks, saved_runtime_tasks);
        task.runtime_value_stack =
            std::mem::replace(&mut self.runtime_value_stack, saved_runtime_stack);
        task.block_stack = std::mem::replace(&mut self.block_stack, saved_block_stack);
        task.expr_stack = std::mem::replace(&mut self.expr_stack, saved_expr_stack);
        task.call_stack = std::mem::replace(&mut self.call_stack, saved_call_stack);
        task.loop_depth = self.loop_depth;
        task.function_depth = self.function_depth;
        task.in_const_region = self.in_const_region;

        self.loop_depth = saved_loop_depth;
        self.function_depth = saved_function_depth;
        self.in_const_region = saved_in_const;
        let task_yielded = self.task_should_yield;
        if let Some(deadline) = self.current_task_sleep_until {
            task.sleep_until = Some(deadline);
        }
        self.current_task = saved_current_task;
        self.task_should_yield = saved_task_should_yield;
        self.current_task_sleep_until = saved_task_sleep_until;
        self.stack_eval_active = saved_stack_eval_active;

        self.tasks.insert(id, task);

        if task_yielded {
            return None;
        }

        match outcome {
            RuntimeStepOutcome::Complete(flow) => Some(flow),
            RuntimeStepOutcome::Yielded => {
                if let Some(task) = self.tasks.get(&id) {
                    if task.sleep_until.is_none() {
                        self.ready_tasks.push_back(id);
                    }
                }
                None
            }
        }
    }

    fn mark_task_sleep(&mut self, duration: Duration) {
        let Some(task_id) = self.current_task else {
            std::thread::sleep(duration);
            return;
        };
        if let Some(task) = self.tasks.get_mut(&task_id) {
            task.sleep_until = Some(Instant::now() + duration);
        } else {
            self.current_task_sleep_until = Some(Instant::now() + duration);
        }
        self.task_should_yield = true;
    }

    fn unwind_block_scopes(&mut self) {
        while !self.block_stack.is_empty() {
            self.block_stack.pop();
            self.pop_scope();
        }
    }

    fn await_runtime_value(&mut self, value: Value) -> RuntimeFlow {
        if let Some(handle) = self.extract_task_handle(&value) {
            return self.run_scheduler_until_task(handle.id);
        }
        if let Some(future) = self.extract_runtime_future(&value) {
            let handle = self.spawn_runtime_future(future);
            return self.run_scheduler_until_task(handle.id);
        }
        self.emit_error("await expects a Future or Task value");
        RuntimeFlow::Value(Value::undefined())
    }

    fn extract_runtime_future(&mut self, value: &Value) -> Option<RuntimeFuture> {
        match value {
            Value::Any(any) => any.downcast_ref::<RuntimeFuture>().cloned(),
            Value::Struct(value_struct) => {
                self.extract_runtime_future_from_struct(&value_struct.structural)
            }
            Value::Structural(structural) => self.extract_runtime_future_from_struct(structural),
            _ => None,
        }
    }

    fn extract_task_handle(&mut self, value: &Value) -> Option<TaskHandle> {
        match value {
            Value::Any(any) => any.downcast_ref::<TaskHandle>().copied(),
            Value::Struct(value_struct) => {
                self.extract_task_handle_from_struct(&value_struct.structural)
            }
            Value::Structural(structural) => self.extract_task_handle_from_struct(structural),
            _ => None,
        }
    }

    fn extract_runtime_future_from_struct(
        &self,
        structural: &ValueStructural,
    ) -> Option<RuntimeFuture> {
        let handle = Ident::new("handle");
        let future = Ident::new("future");
        let field = structural
            .get_field(&handle)
            .or_else(|| structural.get_field(&future))?;
        match &field.value {
            Value::Any(any) => any.downcast_ref::<RuntimeFuture>().cloned(),
            _ => None,
        }
    }

    fn extract_task_handle_from_struct(&self, structural: &ValueStructural) -> Option<TaskHandle> {
        let handle = Ident::new("handle");
        let field = structural.get_field(&handle)?;
        match &field.value {
            Value::Any(any) => any.downcast_ref::<TaskHandle>().copied(),
            _ => None,
        }
    }

    fn push_expr_frame(&mut self, mode: EvalMode, expr: &Expr) -> ExprFrameGuard<'ctx> {
        let frame = ExprFrame {
            mode,
            span: expr.span,
            kind: std::mem::discriminant(expr.kind()),
        };
        self.expr_stack.push(frame);
        ExprFrameGuard {
            interpreter: self as *mut _,
        }
    }

    fn push_call_frame(
        &mut self,
        mode: EvalMode,
        kind: CallFrameKind,
        span: Option<Span>,
    ) -> CallFrameGuard<'ctx> {
        let frame = CallFrame {
            mode,
            kind,
            span,
            module_depth: self.module_stack.len(),
            value_env_depth: self.value_env.len(),
            type_env_depth: self.type_env.len(),
            impl_depth: self.impl_stack.len(),
            loop_depth: self.loop_depth,
            function_depth: self.function_depth,
        };
        self.call_stack.push(frame);
        CallFrameGuard {
            interpreter: self as *mut _,
        }
    }

    fn record_const_value(&mut self, base_len: usize, value: Value) -> Value {
        self.const_value_stack.truncate(base_len);
        self.const_value_stack.push(value.clone());
        value
    }

    fn record_runtime_value(&mut self, base_len: usize, flow: RuntimeFlow) -> RuntimeFlow {
        self.runtime_value_stack.truncate(base_len);
        self.runtime_value_stack.push(flow.clone());
        flow
    }

    fn mark_mutated(&mut self) {
        self.mutations_applied = true;
    }

    fn emit_stdout_fragment(&mut self, text: String) {
        match self.stdout_mode {
            StdoutMode::Capture => {
                if let Some(last) = self.stdout.last_mut() {
                    last.push_str(&text);
                } else {
                    self.stdout.push(text);
                }
            }
            StdoutMode::Inherit => {
                print!("{}", text);
                let _ = std::io::stdout().flush();
            }
        }
    }

    fn emit_stdout_line(&mut self, mut text: String) {
        match self.stdout_mode {
            StdoutMode::Capture => {
                text.push('\n');
                self.stdout.push(text);
            }
            StdoutMode::Inherit => {
                println!("{}", text);
                let _ = std::io::stdout().flush();
            }
        }
    }

    fn normalize_macro_expansion_expr(&mut self, expr: &mut Expr) -> Result<()> {
        let Some(normalizer) = self.intrinsic_normalizer.as_ref() else {
            return Ok(());
        };
        let mut node = Node::expr(expr.clone());
        fp_core::intrinsics::normalize_intrinsics_with(&mut node, normalizer.as_ref())?;
        let Node { kind, .. } = node;
        if let NodeKind::Expr(new_expr) = kind {
            *expr = new_expr;
        }
        Ok(())
    }

    fn in_std_module(&self) -> bool {
        self.module_stack
            .first()
            .map(|m| m == "std")
            .unwrap_or(false)
    }

    fn append_pending_items(&mut self, items: Vec<Item>) {
        self.register_items_with_typer(&items);
        if let Some(scope_pending) = self.pending_items.last_mut() {
            scope_pending.extend(items);
        } else {
            self.pending_items.push(items);
        }
        self.invalidate_all_types();
    }

    fn register_items_with_typer(&mut self, items: &[Item]) {
        if let Some(typer) = self.typer.as_mut() {
            for item in items {
                typer.initialize_from_item(item);
            }
        }
    }

    fn push_item_scope(&mut self, items: &mut Vec<Item>) {
        let ptr = items as *mut Vec<Item>;
        self.item_scopes.push(ptr);
    }

    fn pop_item_scope(&mut self) {
        self.item_scopes.pop();
    }

    fn ensure_expr_typed(&mut self, expr: &mut Expr) {
        let has_type = expr.ty().map(|ty| !self.is_unknown(ty)).unwrap_or(false);
        if has_type {
            return;
        }
        let Some(typer) = self.typer.as_mut() else {
            return;
        };
        if let Err(err) = typer.infer_expression(expr) {
            self.emit_error_at(expr.span, err.to_string());
        }
    }

    fn bind_typer_symbol(&mut self, name: &str, ty: &Ty) {
        if self.is_unknown(ty) {
            return;
        }
        if let Some(typer) = self.typer.as_mut() {
            typer.bind_variable(name, ty.clone());
        }
    }

    fn symbol_available(&self, name: &str) -> bool {
        if self.lookup_value(name).is_some() {
            return true;
        }
        if self.lookup_type(name).is_some() {
            return true;
        }
        if self.functions.contains_key(name) || self.generic_functions.contains_key(name) {
            return true;
        }
        false
    }

    fn materialize_symbol(&mut self, name: &str) -> bool {
        if !matches!(self.mode, InterpreterMode::CompileTime) {
            return false;
        }
        if self.symbol_available(name) {
            return true;
        }
        if self.imported_symbols.contains_key(name) {
            if let Some(typer) = self.typer.as_mut() {
                typer.bind_variable(name, Ty::Any(TypeAny));
            }
            return true;
        }
        let symbol_path = self.parse_symbol_path(name);
        let simple = symbol_path
            .as_ref()
            .and_then(|path| path.segments.last())
            .map(|ident| ident.as_str())
            .unwrap_or(name);
        if let Some(path) = symbol_path.as_ref().filter(|path| path.segments.len() > 1) {
            if let Some(root) = self.root_items {
                let segments = self.path_segments(path);
                if let Some(item_ptr) = self.find_item_by_path_mut(root, &segments) {
                    let qualified = name.to_string();
                    if !self.lazy_evaluated.contains(&qualified) {
                        unsafe {
                            self.evaluate_item(&mut *item_ptr);
                        }
                        self.lazy_evaluated.insert(qualified.clone());
                        unsafe {
                            self.register_items_with_typer(std::slice::from_ref(&*item_ptr));
                        }
                        self.invalidate_all_types();
                    }
                    return self.symbol_available(name) || self.symbol_available(simple);
                }
            }
        }
        let mut materialized = false;
        let scopes = self.item_scopes.clone();
        for scope_ptr in scopes.into_iter().rev() {
            let items = unsafe { &mut *scope_ptr };
            for item in items.iter_mut() {
                let item_name = match item.kind() {
                    ItemKind::DefStruct(def) => Some(def.name.as_str().to_string()),
                    ItemKind::DefStructural(def) => Some(def.name.as_str().to_string()),
                    ItemKind::DefEnum(def) => Some(def.name.as_str().to_string()),
                    ItemKind::DefType(def) => Some(def.name.as_str().to_string()),
                    ItemKind::DefConst(def) => Some(def.name.as_str().to_string()),
                    ItemKind::DefStatic(def) => Some(def.name.as_str().to_string()),
                    ItemKind::DefFunction(def) => Some(def.name.as_str().to_string()),
                    _ => None,
                };
                let Some(item_name) = item_name else {
                    continue;
                };
                let qualified = self.qualified_name(&item_name);
                if self.lazy_evaluated.contains(&qualified) {
                    continue;
                }
                if item_name == simple || qualified == name {
                    self.evaluate_item(item);
                    self.lazy_evaluated.insert(qualified.clone());
                    self.register_items_with_typer(std::slice::from_ref(item));
                    self.invalidate_all_types();
                    materialized = self.symbol_available(name)
                        || self.symbol_available(&item_name)
                        || self.symbol_available(&qualified);
                    if materialized {
                        return true;
                    }
                }
            }
        }
        materialized
    }

    fn should_skip_lazy_item(&self, item: &Item) -> bool {
        let Some(qualified) = self.item_qualified_name(item) else {
            return false;
        };
        self.lazy_evaluated.contains(&qualified)
    }

    fn item_qualified_name(&self, item: &Item) -> Option<String> {
        let name = match item.kind() {
            ItemKind::DefStruct(def) => Some(def.name.as_str()),
            ItemKind::DefStructural(def) => Some(def.name.as_str()),
            ItemKind::DefEnum(def) => Some(def.name.as_str()),
            ItemKind::DefType(def) => Some(def.name.as_str()),
            ItemKind::DefConst(def) => Some(def.name.as_str()),
            ItemKind::DefStatic(def) => Some(def.name.as_str()),
            ItemKind::DefFunction(def) => Some(def.name.as_str()),
            _ => None,
        };
        name.map(|name| self.qualified_name(name))
    }

    fn find_item_by_path_mut(&self, items_ptr: *mut Vec<Item>, path: &[&str]) -> Option<*mut Item> {
        if path.is_empty() {
            return None;
        }
        unsafe {
            let items = &mut *items_ptr;
            if path.len() == 1 {
                for item in items.iter_mut() {
                    let name = match item.kind() {
                        ItemKind::DefStruct(def) => Some(def.name.as_str()),
                        ItemKind::DefStructural(def) => Some(def.name.as_str()),
                        ItemKind::DefEnum(def) => Some(def.name.as_str()),
                        ItemKind::DefType(def) => Some(def.name.as_str()),
                        ItemKind::DefConst(def) => Some(def.name.as_str()),
                        ItemKind::DefStatic(def) => Some(def.name.as_str()),
                        ItemKind::DefFunction(def) => Some(def.name.as_str()),
                        ItemKind::Module(def) => Some(def.name.as_str()),
                        _ => None,
                    };
                    if name == Some(path[0]) {
                        return Some(item as *mut Item);
                    }
                }
                return None;
            }
            for item in items.iter_mut() {
                if let ItemKind::Module(module) = item.kind_mut() {
                    if module.name.as_str() == path[0] {
                        return self.find_item_by_path_mut(
                            &mut module.items as *mut Vec<Item>,
                            &path[1..],
                        );
                    }
                }
            }
            None
        }
    }

    fn invalidate_all_types(&mut self) {
        if self.typer.is_none() {
            return;
        }
        let Some(root) = self.root_items else {
            return;
        };
        unsafe {
            let items = &mut *root;
            for item in items.iter_mut() {
                self.clear_item_types(item);
            }
        }
    }

    fn clear_item_types(&mut self, item: &mut Item) {
        match item.kind_mut() {
            ItemKind::DefFunction(func) => {
                *func.ty_annotation_mut() = None;
                if let Some(ret_ty) = func.sig.ret_ty.as_mut() {
                    self.clear_ty(ret_ty);
                }
                for param in &mut func.sig.params {
                    *param.ty_annotation_mut() = None;
                    self.clear_ty(&mut param.ty);
                    if let Some(default) = param.default.as_mut() {
                        self.clear_value_types(default);
                    }
                }
                self.clear_expr_types(func.body.as_mut());
            }
            ItemKind::DefConst(def) => {
                if let Some(ty) = def.ty.as_mut() {
                    self.clear_ty(ty);
                }
                if let Some(ty) = def.ty_annotation_mut().as_mut() {
                    self.clear_ty(ty);
                }
                self.clear_expr_types(def.value.as_mut());
            }
            ItemKind::DefStatic(def) => {
                self.clear_ty(&mut def.ty);
                if let Some(ty) = def.ty_annotation_mut().as_mut() {
                    self.clear_ty(ty);
                }
                self.clear_expr_types(def.value.as_mut());
            }
            ItemKind::DefStruct(def) => {
                for field in &mut def.value.fields {
                    self.clear_ty(&mut field.value);
                }
            }
            ItemKind::DefStructural(def) => {
                for field in &mut def.value.fields {
                    self.clear_ty(&mut field.value);
                }
            }
            ItemKind::DefEnum(def) => {
                for variant in &mut def.value.variants {
                    self.clear_ty(&mut variant.value);
                    if let Some(expr) = variant.discriminant.as_mut() {
                        self.clear_expr_types(expr.as_mut());
                    }
                }
            }
            ItemKind::DefType(def) => {
                self.clear_ty(&mut def.value);
            }
            ItemKind::DefTrait(def) => {
                for member in &mut def.items {
                    self.clear_item_types(member);
                }
            }
            ItemKind::Impl(impl_block) => {
                self.clear_expr_types(&mut impl_block.self_ty);
                if let Some(trait_ty) = impl_block.trait_ty.as_mut() {
                    self.clear_locator_types(trait_ty);
                }
                for item in &mut impl_block.items {
                    self.clear_item_types(item);
                }
            }
            ItemKind::Module(module) => {
                for item in &mut module.items {
                    self.clear_item_types(item);
                }
            }
            ItemKind::Expr(expr) => {
                self.clear_expr_types(expr);
            }
            ItemKind::Import(_)
            | ItemKind::Macro(_)
            | ItemKind::DeclConst(_)
            | ItemKind::DeclStatic(_)
            | ItemKind::DeclFunction(_)
            | ItemKind::DeclType(_)
            | ItemKind::Any(_) => {}
        }
    }

    fn clear_expr_types(&mut self, expr: &mut Expr) {
        *expr.ty_mut() = None;
        match expr.kind_mut() {
            ExprKind::Block(block) => {
                for stmt in &mut block.stmts {
                    self.clear_stmt_types(stmt);
                }
            }
            ExprKind::Match(expr_match) => {
                if let Some(scrutinee) = expr_match.scrutinee.as_mut() {
                    self.clear_expr_types(scrutinee.as_mut());
                }
                for case in &mut expr_match.cases {
                    if let Some(pat) = case.pat.as_mut() {
                        self.clear_pattern_types(pat.as_mut());
                    }
                    self.clear_expr_types(case.cond.as_mut());
                    if let Some(guard) = case.guard.as_mut() {
                        self.clear_expr_types(guard.as_mut());
                    }
                    self.clear_expr_types(case.body.as_mut());
                }
            }
            ExprKind::If(expr_if) => {
                self.clear_expr_types(expr_if.cond.as_mut());
                self.clear_expr_types(expr_if.then.as_mut());
                if let Some(elze) = expr_if.elze.as_mut() {
                    self.clear_expr_types(elze.as_mut());
                }
            }
            ExprKind::Loop(expr_loop) => {
                self.clear_expr_types(expr_loop.body.as_mut());
            }
            ExprKind::While(expr_while) => {
                self.clear_expr_types(expr_while.cond.as_mut());
                self.clear_expr_types(expr_while.body.as_mut());
            }
            ExprKind::Return(expr_return) => {
                if let Some(value) = expr_return.value.as_mut() {
                    self.clear_expr_types(value.as_mut());
                }
            }
            ExprKind::Break(expr_break) => {
                if let Some(value) = expr_break.value.as_mut() {
                    self.clear_expr_types(value.as_mut());
                }
            }
            ExprKind::Invoke(invoke) => {
                match &mut invoke.target {
                    ExprInvokeTarget::Function(locator) => {
                        self.clear_locator_types(locator);
                    }
                    ExprInvokeTarget::Type(ty) => {
                        self.clear_ty(ty);
                    }
                    ExprInvokeTarget::Method(select) => {
                        self.clear_expr_types(select.obj.as_mut());
                    }
                    ExprInvokeTarget::Expr(expr) => {
                        self.clear_expr_types(expr.as_mut());
                    }
                    ExprInvokeTarget::Closure(_) | ExprInvokeTarget::BinOp(_) => {}
                }
                for arg in &mut invoke.args {
                    self.clear_expr_types(arg);
                }
            }
            ExprKind::BinOp(binop) => {
                self.clear_expr_types(binop.lhs.as_mut());
                self.clear_expr_types(binop.rhs.as_mut());
            }
            ExprKind::UnOp(unop) => {
                self.clear_expr_types(unop.val.as_mut());
            }
            ExprKind::Assign(assign) => {
                self.clear_expr_types(assign.target.as_mut());
                self.clear_expr_types(assign.value.as_mut());
            }
            ExprKind::Select(select) => {
                self.clear_expr_types(select.obj.as_mut());
            }
            ExprKind::Index(index) => {
                self.clear_expr_types(index.obj.as_mut());
                self.clear_expr_types(index.index.as_mut());
            }
            ExprKind::Struct(struct_expr) => {
                self.clear_expr_types(struct_expr.name.as_mut());
                for field in &mut struct_expr.fields {
                    if let Some(value) = field.value.as_mut() {
                        self.clear_expr_types(value);
                    }
                }
                if let Some(update) = struct_expr.update.as_mut() {
                    self.clear_expr_types(update.as_mut());
                }
            }
            ExprKind::Structural(struct_expr) => {
                for field in &mut struct_expr.fields {
                    if let Some(value) = field.value.as_mut() {
                        self.clear_expr_types(value);
                    }
                }
            }
            ExprKind::Cast(cast) => {
                self.clear_expr_types(cast.expr.as_mut());
                self.clear_ty(&mut cast.ty);
            }
            ExprKind::Reference(reference) => {
                self.clear_expr_types(reference.referee.as_mut());
            }
            ExprKind::Dereference(deref) => {
                self.clear_expr_types(deref.referee.as_mut());
            }
            ExprKind::Tuple(tuple) => {
                for value in &mut tuple.values {
                    self.clear_expr_types(value);
                }
            }
            ExprKind::Try(expr_try) => {
                self.clear_expr_types(expr_try.expr.as_mut());
            }
            ExprKind::For(expr_for) => {
                self.clear_pattern_types(expr_for.pat.as_mut());
                self.clear_expr_types(expr_for.iter.as_mut());
                self.clear_expr_types(expr_for.body.as_mut());
            }
            ExprKind::Async(expr_async) => {
                self.clear_expr_types(expr_async.expr.as_mut());
            }
            ExprKind::Let(expr_let) => {
                self.clear_pattern_types(expr_let.pat.as_mut());
                self.clear_expr_types(expr_let.expr.as_mut());
            }
            ExprKind::Closure(closure) => {
                for param in &mut closure.params {
                    self.clear_pattern_types(param);
                }
                if let Some(ret_ty) = closure.ret_ty.as_mut() {
                    self.clear_ty(ret_ty.as_mut());
                }
                self.clear_expr_types(closure.body.as_mut());
            }
            ExprKind::Array(array) => {
                for value in &mut array.values {
                    self.clear_expr_types(value);
                }
            }
            ExprKind::ArrayRepeat(repeat) => {
                self.clear_expr_types(repeat.elem.as_mut());
                self.clear_expr_types(repeat.len.as_mut());
            }
            ExprKind::ConstBlock(block) => {
                self.clear_expr_types(block.expr.as_mut());
            }
            ExprKind::IntrinsicContainer(container) => {
                container.for_each_expr_mut(|expr| self.clear_expr_types(expr));
            }
            ExprKind::IntrinsicCall(call) => {
                for arg in &mut call.args {
                    self.clear_expr_types(arg);
                }
                for kwarg in &mut call.kwargs {
                    self.clear_expr_types(&mut kwarg.value);
                }
            }
            ExprKind::Quote(quote) => {
                for stmt in &mut quote.block.stmts {
                    self.clear_stmt_types(stmt);
                }
            }
            ExprKind::Splice(splice) => {
                self.clear_expr_types(splice.token.as_mut());
            }
            ExprKind::Closured(closured) => {
                self.clear_expr_types(closured.expr.as_mut());
            }
            ExprKind::Await(await_expr) => {
                self.clear_expr_types(await_expr.base.as_mut());
            }
            ExprKind::Paren(paren) => {
                self.clear_expr_types(paren.expr.as_mut());
            }
            ExprKind::Range(range) => {
                if let Some(start) = range.start.as_mut() {
                    self.clear_expr_types(start.as_mut());
                }
                if let Some(end) = range.end.as_mut() {
                    self.clear_expr_types(end.as_mut());
                }
                if let Some(step) = range.step.as_mut() {
                    self.clear_expr_types(step.as_mut());
                }
            }
            ExprKind::FormatString(template) => {
                let _ = template;
            }
            ExprKind::Splat(splat) => {
                self.clear_expr_types(splat.iter.as_mut());
            }
            ExprKind::SplatDict(splat) => {
                self.clear_expr_types(splat.dict.as_mut());
            }
            ExprKind::Item(item) => {
                self.clear_item_types(item.as_mut());
            }
            ExprKind::Value(value) => {
                self.clear_value_types(value.as_mut());
            }
            ExprKind::Macro(_) | ExprKind::Any(_) | ExprKind::Id(_) | ExprKind::Name(_) => {}
            ExprKind::Continue(_) => {}
        }
    }

    fn clear_stmt_types(&mut self, stmt: &mut BlockStmt) {
        match stmt {
            BlockStmt::Item(item) => self.clear_item_types(item.as_mut()),
            BlockStmt::Let(stmt_let) => {
                self.clear_pattern_types(&mut stmt_let.pat);
                if let Some(init) = stmt_let.init.as_mut() {
                    self.clear_expr_types(init);
                }
                if let Some(diverge) = stmt_let.diverge.as_mut() {
                    self.clear_expr_types(diverge);
                }
            }
            BlockStmt::Expr(expr) => self.clear_expr_types(expr.expr.as_mut()),
            BlockStmt::Noop | BlockStmt::Any(_) => {}
        }
    }

    fn clear_pattern_types(&mut self, pattern: &mut Pattern) {
        *pattern.ty_mut() = None;
        match pattern.kind_mut() {
            PatternKind::Bind(bind) => {
                self.clear_pattern_types(bind.pattern.as_mut());
            }
            PatternKind::Tuple(tuple) => {
                for pat in &mut tuple.patterns {
                    self.clear_pattern_types(pat);
                }
            }
            PatternKind::TupleStruct(tuple) => {
                for pat in &mut tuple.patterns {
                    self.clear_pattern_types(pat);
                }
            }
            PatternKind::Struct(struct_pat) => {
                for field in &mut struct_pat.fields {
                    if let Some(pat) = field.rename.as_mut() {
                        self.clear_pattern_types(pat);
                    }
                }
            }
            PatternKind::Structural(struct_pat) => {
                for field in &mut struct_pat.fields {
                    if let Some(pat) = field.rename.as_mut() {
                        self.clear_pattern_types(pat);
                    }
                }
            }
            PatternKind::Box(boxed) => {
                self.clear_pattern_types(boxed.pattern.as_mut());
            }
            PatternKind::Variant(variant) => {
                self.clear_expr_types(&mut variant.name);
                if let Some(pat) = variant.pattern.as_mut() {
                    self.clear_pattern_types(pat);
                }
            }
            PatternKind::Quote(quote) => {
                for field in &mut quote.fields {
                    if let Some(pat) = field.rename.as_mut() {
                        self.clear_pattern_types(pat);
                    }
                }
            }
            PatternKind::QuotePlural(plural) => {
                for pat in &mut plural.patterns {
                    self.clear_pattern_types(pat);
                }
            }
            PatternKind::Type(typed) => {
                self.clear_pattern_types(typed.pat.as_mut());
                self.clear_ty(&mut typed.ty);
            }
            PatternKind::Ident(_) | PatternKind::Wildcard(_) => {}
        }
    }

    fn clear_ty(&mut self, ty: &mut Ty) {
        match ty {
            Ty::Tuple(tuple) => {
                for ty in &mut tuple.types {
                    self.clear_ty(ty);
                }
            }
            Ty::Array(array) => {
                self.clear_ty(array.elem.as_mut());
                self.clear_expr_types(array.len.as_mut());
            }
            Ty::Slice(slice) => self.clear_ty(&mut slice.elem),
            Ty::Vec(vec) => self.clear_ty(&mut vec.ty),
            Ty::Reference(reference) => self.clear_ty(&mut reference.ty),
            Ty::Type(_) => {}
            Ty::Struct(def) => {
                for field in &mut def.fields {
                    self.clear_ty(&mut field.value);
                }
            }
            Ty::Structural(def) => {
                for field in &mut def.fields {
                    self.clear_ty(&mut field.value);
                }
            }
            Ty::Enum(def) => {
                for variant in &mut def.variants {
                    self.clear_ty(&mut variant.value);
                    if let Some(expr) = variant.discriminant.as_mut() {
                        self.clear_expr_types(expr.as_mut());
                    }
                }
            }
            Ty::Function(func) => {
                for param in &mut func.params {
                    self.clear_ty(param);
                }
                if let Some(ret) = func.ret_ty.as_mut() {
                    self.clear_ty(ret.as_mut());
                }
            }
            Ty::Expr(expr) => {
                self.clear_expr_types(expr.as_mut());
            }
            Ty::Quote(quote) => {
                if let Some(inner) = quote.inner.as_mut() {
                    self.clear_ty(inner.as_mut());
                }
            }
            Ty::TypeBinaryOp(op) => {
                self.clear_ty(&mut op.lhs);
                self.clear_ty(&mut op.rhs);
            }
            Ty::TypeBounds(bounds) => {
                for expr in &mut bounds.bounds {
                    self.clear_expr_types(expr);
                }
            }
            Ty::Primitive(_)
            | Ty::Unit(_)
            | Ty::Unknown(_)
            | Ty::Any(_)
            | Ty::TokenStream(_)
            | Ty::ImplTraits(_)
            | Ty::Value(_)
            | Ty::Nothing(_)
            | Ty::AnyBox(_) | Ty::RawPtr(_) => {}
        }
    }

    fn clear_locator_types(&mut self, _locator: &mut Name) {}

    fn clear_value_types(&mut self, value: &mut Value) {
        match value {
            Value::Expr(expr) => self.clear_expr_types(expr.as_mut()),
            Value::List(list) => {
                for value in &mut list.values {
                    self.clear_value_types(value);
                }
            }
            Value::Tuple(tuple) => {
                for value in &mut tuple.values {
                    self.clear_value_types(value);
                }
            }
            Value::Struct(struct_value) => {
                for field in &mut struct_value.structural.fields {
                    self.clear_value_types(&mut field.value);
                }
            }
            Value::Structural(struct_value) => {
                for field in &mut struct_value.fields {
                    self.clear_value_types(&mut field.value);
                }
            }
            Value::QuoteToken(token) => match &mut token.value {
                QuoteTokenValue::Expr(expr) => self.clear_expr_types(expr),
                QuoteTokenValue::Stmts(stmts) => {
                    for stmt in stmts {
                        self.clear_stmt_types(stmt);
                    }
                }
                QuoteTokenValue::Items(items) => {
                    for item in items {
                        self.clear_item_types(item);
                    }
                }
                QuoteTokenValue::Type(ty) => self.clear_ty(ty),
            },
            _ => {}
        }
    }

    fn register_macro_rules(&mut self, name: &str, def: MacroRulesDefinition) {
        if let Some(scope) = self.macro_env.last_mut() {
            scope.insert(name.to_string(), def);
        }
    }

    fn lookup_macro_rules(&self, name: &str) -> Option<&MacroRulesDefinition> {
        for scope in self.macro_env.iter().rev() {
            if let Some(def) = scope.get(name) {
                return Some(def);
            }
        }
        None
    }

    fn register_proc_macro(&mut self, name: &str, def: ProcMacroDefinition) {
        if let Some(scope) = self.proc_macro_env.last_mut() {
            scope.insert(name.to_string(), def);
        }
    }

    fn lookup_proc_macro(&self, name: &str, kind: ProcMacroKind) -> Option<&ProcMacroDefinition> {
        for scope in self.proc_macro_env.iter().rev() {
            if let Some(def) = scope.get(name) {
                if def.kind == kind {
                    return Some(def);
                }
            }
        }
        None
    }

    fn expand_macro_invocation(
        &mut self,
        invocation: &MacroInvocation,
        context: MacroExpansionContext,
    ) -> Result<Vec<MacroTokenTree>> {
        let name = invocation
            .path
            .segments
            .last()
            .map(|seg| seg.as_str())
            .unwrap_or("");
        if let Some(expansion) = self.try_expand_select_macro(invocation, name)? {
            return Ok(expansion);
        }
        let expansion = if let Some(def) = self.lookup_macro_rules(name) {
            expand_macro(def, invocation)?
        } else if let Some(def) = self.lookup_proc_macro(name, ProcMacroKind::FunctionLike) {
            let input = Value::TokenStream(ValueTokenStream {
                tokens: invocation.token_trees.clone(),
            });
            let output = self.call_function(def.function.clone(), vec![input]);
            let Value::TokenStream(stream) = output else {
                return Err(fp_core::error::Error::from(format!(
                    "proc macro `{}` must return TokenStream",
                    invocation.path
                )));
            };
            stream.tokens
        } else {
            return Err(fp_core::error::Error::from(format!(
                "macro `{}` is not defined",
                invocation.path
            )));
        };
        if expansion.is_empty() {
            return Ok(expansion);
        }
        if expansion.len() > 10_000 {
            return Err(fp_core::error::Error::from(
                "macro expansion too large; aborting",
            ));
        }
        match context {
            MacroExpansionContext::Item
            | MacroExpansionContext::Expr
            | MacroExpansionContext::Type => Ok(expansion),
        }
    }

    fn try_expand_select_macro(
        &self,
        invocation: &MacroInvocation,
        name: &str,
    ) -> Result<Option<Vec<MacroTokenTree>>> {
        if name != "select" || invocation.delimiter != MacroDelimiter::Brace {
            return Ok(None);
        }
        let arms = match self.parse_select_macro_arms(&invocation.token_trees) {
            Ok(arms) if !arms.is_empty() => arms,
            Ok(_) => return Ok(None),
            Err(err) => return Err(err),
        };
        Ok(Some(self.build_select_macro_expansion(&arms)))
    }

    fn parse_select_macro_arms(&self, tokens: &[MacroTokenTree]) -> Result<Vec<SelectMacroArm>> {
        let mut idx = 0usize;
        let mut arms = Vec::new();
        while idx < tokens.len() {
            if self.is_token(tokens, idx, ",") {
                idx += 1;
                continue;
            }
            let Some(name) = self.read_ident(tokens, idx) else {
                return Err(fp_core::error::Error::from(
                    "select! expects `name = future => expr` arms",
                ));
            };
            idx += 1;
            if !self.is_token(tokens, idx, "=") {
                return Err(fp_core::error::Error::from(
                    "select! expects `=` after binding name",
                ));
            }
            idx += 1;
            let future_start = idx;
            while idx < tokens.len() && !self.is_token(tokens, idx, "=>") {
                idx += 1;
            }
            if idx == future_start || idx >= tokens.len() {
                return Err(fp_core::error::Error::from(
                    "select! expects `=>` after future expression",
                ));
            }
            let future = tokens[future_start..idx].to_vec();
            idx += 1;
            let body_start = idx;
            while idx < tokens.len() && !self.is_token(tokens, idx, ",") {
                idx += 1;
            }
            if idx == body_start {
                return Err(fp_core::error::Error::from(
                    "select! expects an expression after `=>`",
                ));
            }
            let body = tokens[body_start..idx].to_vec();
            arms.push(SelectMacroArm { name, future, body });
            if idx < tokens.len() && self.is_token(tokens, idx, ",") {
                idx += 1;
            }
        }
        Ok(arms)
    }

    fn build_select_macro_expansion(&self, arms: &[SelectMacroArm]) -> Vec<MacroTokenTree> {
        let mut block_tokens = Vec::new();
        block_tokens.push(self.token("let"));
        block_tokens.push(self.token("__select_result"));
        block_tokens.push(self.token("="));
        block_tokens.extend(self.select_call_tokens(arms));
        block_tokens.push(self.token(";"));
        block_tokens.push(self.token("match"));
        block_tokens.push(self.token("__select_result"));
        block_tokens.push(self.match_group_tokens(arms));
        vec![self.group(MacroDelimiter::Brace, block_tokens)]
    }

    fn select_call_tokens(&self, arms: &[SelectMacroArm]) -> Vec<MacroTokenTree> {
        let mut tokens = Vec::new();
        tokens.push(self.token("std"));
        tokens.push(self.token("::"));
        tokens.push(self.token("task"));
        tokens.push(self.token("::"));
        tokens.push(self.token("select"));
        let mut args = Vec::new();
        for (idx, arm) in arms.iter().enumerate() {
            args.extend(arm.future.clone());
            if idx + 1 < arms.len() {
                args.push(self.token(","));
            }
        }
        tokens.push(self.group(MacroDelimiter::Parenthesis, args));
        tokens
    }

    fn match_group_tokens(&self, arms: &[SelectMacroArm]) -> MacroTokenTree {
        let mut tokens = Vec::new();
        for (idx, arm) in arms.iter().enumerate() {
            let mut pattern_tokens = Vec::new();
            pattern_tokens.push(self.token(idx.to_string().as_str()));
            pattern_tokens.push(self.token(","));
            pattern_tokens.push(self.token("__select_value"));
            let pattern = self.group(MacroDelimiter::Parenthesis, pattern_tokens);

            let mut body_tokens = Vec::new();
            body_tokens.push(self.token("let"));
            body_tokens.push(self.token(arm.name.as_str()));
            body_tokens.push(self.token("="));
            body_tokens.push(self.token("__select_value"));
            body_tokens.push(self.token(";"));
            body_tokens.extend(arm.body.clone());
            let body_group = self.group(MacroDelimiter::Brace, body_tokens);

            tokens.push(pattern);
            tokens.push(self.token("=>"));
            tokens.push(body_group);
            if idx + 1 < arms.len() {
                tokens.push(self.token(","));
            }
        }
        self.group(MacroDelimiter::Brace, tokens)
    }

    fn token(&self, text: &str) -> MacroTokenTree {
        MacroTokenTree::Token(MacroToken {
            text: text.to_string(),
            span: Span::null(),
        })
    }

    fn group(&self, delimiter: MacroDelimiter, tokens: Vec<MacroTokenTree>) -> MacroTokenTree {
        MacroTokenTree::Group(MacroGroup {
            delimiter,
            tokens,
            span: Span::null(),
        })
    }

    fn is_token(&self, tokens: &[MacroTokenTree], idx: usize, text: &str) -> bool {
        matches!(
            tokens.get(idx),
            Some(MacroTokenTree::Token(tok)) if tok.text == text
        )
    }

    fn read_ident(&self, tokens: &[MacroTokenTree], idx: usize) -> Option<String> {
        match tokens.get(idx) {
            Some(MacroTokenTree::Token(tok)) if Self::token_is_ident(&tok.text) => {
                Some(tok.text.clone())
            }
            _ => None,
        }
    }

    fn token_is_ident(text: &str) -> bool {
        text.chars()
            .next()
            .is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
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

    fn attr_to_locator(&mut self, attr: &Attribute) -> Option<Name> {
        match &attr.meta {
            AttrMeta::Path(path) => Some(Name::path(path.clone())),
            _ => None,
        }
    }

    fn is_inert_attribute(&self, attr: &Attribute) -> bool {
        let AttrMeta::Path(path) = &attr.meta else {
            return false;
        };
        matches!(path.last().as_str(), "const" | "unimplemented")
    }

    fn fallback_attr_locator(&self, locator: &Name) -> Option<Name> {
        let ident = locator.as_ident()?;
        let name = ident.as_str();
        if name != "test" && name != "bench" {
            return None;
        }
        let module = if name == "bench" { "bench" } else { "test" };
        let path = Path::plain(vec![
            Ident::new("std"),
            Ident::new(module),
            Ident::new(name),
        ]);
        Some(Name::path(path))
    }

    fn apply_item_attributes(&mut self, item: &mut Item, attrs: &mut Vec<Attribute>) -> bool {
        if attrs.is_empty() {
            return false;
        }
        if !self.in_const_region() && !matches!(self.mode, InterpreterMode::CompileTime) {
            self.emit_error("attributes require const evaluation");
            return true;
        }

        let mut inert_attrs = Vec::new();
        let mut active_attrs = Vec::new();
        for attr in attrs.drain(..) {
            if self.is_inert_attribute(&attr) {
                inert_attrs.push(attr);
            } else {
                active_attrs.push(attr);
            }
        }
        if active_attrs.is_empty() {
            attrs.extend(inert_attrs);
            return false;
        }

        if self.apply_proc_macro_attributes(item, &mut active_attrs) {
            return true;
        }

        let has_applicable = active_attrs
            .iter()
            .any(|attr| matches!(attr.meta, AttrMeta::Path(_)));
        if !has_applicable {
            attrs.extend(active_attrs);
            attrs.extend(inert_attrs);
            return false;
        }

        let mut quoted_item = item.clone();
        if let ItemKind::DefFunction(func) = quoted_item.kind_mut() {
            func.attrs.clear();
        }

        let quote = ExprQuote {
            span: Span::null(),
            block: ExprBlock::new_stmts(vec![BlockStmt::Item(Box::new(quoted_item))]),
            kind: Some(QuoteFragmentKind::Item),
        };
        let mut expr = Expr::new(ExprKind::Quote(quote));

        for attr in &active_attrs {
            let Some(mut locator) = self.attr_to_locator(attr) else {
                continue;
            };
            let mut invoke = ExprInvoke {
                span: Span::null(),
                target: ExprInvokeTarget::Function(locator.clone()),
                args: vec![expr],
                kwargs: Vec::new(),
            };
            let function = if let Some(function) =
                self.resolve_function_call(&mut locator, &mut invoke, ResolutionMode::Attribute)
            {
                Some(function)
            } else if let Some(fallback) = self.fallback_attr_locator(&locator) {
                locator = fallback;
                invoke.target = ExprInvokeTarget::Function(locator.clone());
                self.resolve_function_call(&mut locator, &mut invoke, ResolutionMode::Attribute)
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

        if matches!(expr.kind(), ExprKind::Quote(_)) {
            attrs.extend(active_attrs);
            attrs.extend(inert_attrs);
            return false;
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

    fn proc_macro_registration(
        &mut self,
        func: &ItemDefFunction,
    ) -> Option<(String, ProcMacroKind)> {
        let mut registration: Option<(String, ProcMacroKind)> = None;
        for attr in &func.attrs {
            let name = self.attr_name(attr)?;
            let kind = match name.as_str() {
                "proc_macro" => ProcMacroKind::FunctionLike,
                "proc_macro_attribute" => ProcMacroKind::Attribute,
                "proc_macro_derive" => ProcMacroKind::Derive,
                _ => continue,
            };
            if registration.is_some() {
                self.emit_error(format!(
                    "proc macro `{}` cannot declare multiple proc-macro attributes",
                    func.name
                ));
                return None;
            }
            let reg_name = if kind == ProcMacroKind::Derive {
                match self.proc_macro_derive_name(attr) {
                    Some(name) => name,
                    None => {
                        self.emit_error("proc_macro_derive requires a derive name");
                        return None;
                    }
                }
            } else {
                func.name.as_str().to_string()
            };
            registration = Some((reg_name, kind));
        }
        registration
    }

    fn apply_proc_macro_attributes(&mut self, item: &mut Item, attrs: &mut Vec<Attribute>) -> bool {
        if self.proc_macro_env.is_empty() {
            return false;
        }

        let mut handled = false;
        let mut derive_names = Vec::new();
        let mut remaining = Vec::new();

        for attr in attrs.iter() {
            if let Some(name) = self.attr_name(attr) {
                if name == "derive" {
                    if let Some(names) = self.derive_names(attr) {
                        derive_names.extend(names);
                        handled = true;
                        continue;
                    }
                }
                if self
                    .lookup_proc_macro(&name, ProcMacroKind::Attribute)
                    .is_some()
                {
                    if handled {
                        self.emit_error(
                            "multiple proc-macro attributes on one item are not supported",
                        );
                        return true;
                    }
                    let Some(tokens) = self.proc_macro_attribute_tokens(attr, item) else {
                        return true;
                    };
                    let parser = match self.macro_parser.clone() {
                        Some(parser) => parser,
                        None => {
                            self.emit_error("macro expansion requires a parser hook");
                            return true;
                        }
                    };
                    let Some(def) = self.lookup_proc_macro(&name, ProcMacroKind::Attribute) else {
                        return true;
                    };
                    let (attr_tokens, item_tokens) = tokens;
                    let output = self.call_function(
                        def.function.clone(),
                        vec![
                            Value::TokenStream(ValueTokenStream {
                                tokens: attr_tokens,
                            }),
                            Value::TokenStream(ValueTokenStream {
                                tokens: item_tokens,
                            }),
                        ],
                    );
                    let Value::TokenStream(stream) = output else {
                        self.emit_error(format!(
                            "proc macro attribute `{}` must return TokenStream",
                            name
                        ));
                        return true;
                    };
                    match parser.parse_items(&stream.tokens) {
                        Ok(items) => {
                            if !items.is_empty() {
                                self.append_pending_items(items);
                            }
                            *item = Item::unit();
                            self.mark_mutated();
                            return true;
                        }
                        Err(err) => {
                            self.emit_error(err.to_string());
                            return true;
                        }
                    }
                }
            }
            remaining.push(attr.clone());
        }

        if !derive_names.is_empty() {
            let parser = match self.macro_parser.clone() {
                Some(parser) => parser,
                None => {
                    self.emit_error("macro expansion requires a parser hook");
                    return true;
                }
            };
            let item_tokens = match self.item_to_token_stream(item) {
                Ok(tokens) => tokens,
                Err(err) => {
                    self.emit_error(err.to_string());
                    return true;
                }
            };
            for name in derive_names {
                let Some(def) = self.lookup_proc_macro(&name, ProcMacroKind::Derive) else {
                    continue;
                };
                let output = self.call_function(
                    def.function.clone(),
                    vec![Value::TokenStream(ValueTokenStream {
                        tokens: item_tokens.clone(),
                    })],
                );
                let Value::TokenStream(stream) = output else {
                    self.emit_error(format!(
                        "proc macro derive `{}` must return TokenStream",
                        name
                    ));
                    return true;
                };
                match parser.parse_items(&stream.tokens) {
                    Ok(items) => {
                        if !items.is_empty() {
                            self.append_pending_items(items);
                        }
                        handled = true;
                    }
                    Err(err) => {
                        self.emit_error(err.to_string());
                        return true;
                    }
                }
            }
        }

        if handled {
            *attrs = remaining;
        }

        false
    }

    fn attr_name(&self, attr: &Attribute) -> Option<String> {
        match &attr.meta {
            AttrMeta::Path(path) => Some(path.last().as_str().to_string()),
            AttrMeta::List(list) => Some(list.name.last().as_str().to_string()),
            AttrMeta::NameValue(nv) => Some(nv.name.last().as_str().to_string()),
        }
    }

    fn proc_macro_derive_name(&self, attr: &Attribute) -> Option<String> {
        let AttrMeta::List(list) = &attr.meta else {
            return None;
        };
        let first = list.items.first()?;
        match first {
            AttrMeta::Path(path) => Some(path.last().as_str().to_string()),
            AttrMeta::List(list) => Some(list.name.last().as_str().to_string()),
            AttrMeta::NameValue(nv) => Some(nv.name.last().as_str().to_string()),
        }
    }

    fn derive_names(&self, attr: &Attribute) -> Option<Vec<String>> {
        let AttrMeta::List(list) = &attr.meta else {
            return None;
        };
        if list.name.last().as_str() != "derive" {
            return None;
        }
        let mut names = Vec::new();
        for item in &list.items {
            match item {
                AttrMeta::Path(path) => names.push(path.last().as_str().to_string()),
                AttrMeta::List(list) => names.push(list.name.last().as_str().to_string()),
                AttrMeta::NameValue(nv) => names.push(nv.name.last().as_str().to_string()),
            }
        }
        if names.is_empty() {
            None
        } else {
            Some(names)
        }
    }

    fn proc_macro_attribute_tokens(
        &self,
        attr: &Attribute,
        item: &Item,
    ) -> Option<(Vec<MacroTokenTree>, Vec<MacroTokenTree>)> {
        let attr_tokens = self.attr_args_to_tokens(attr);
        let item_tokens = self.item_to_token_stream(item).ok()?;
        Some((attr_tokens, item_tokens))
    }

    fn attr_args_to_tokens(&self, attr: &Attribute) -> Vec<MacroTokenTree> {
        match &attr.meta {
            AttrMeta::Path(_) => Vec::new(),
            AttrMeta::NameValue(nv) => meta_name_value_tokens(nv),
            AttrMeta::List(list) => meta_list_tokens(&list.items),
        }
    }

    fn item_to_token_stream(&self, item: &Item) -> Result<Vec<MacroTokenTree>> {
        let mut stripped = item.clone();
        if let ItemKind::DefFunction(func) = stripped.kind_mut() {
            func.attrs.clear();
        }
        let options = fp_core::pretty::PrettyOptions {
            show_types: false,
            show_spans: false,
            ..Default::default()
        };
        let text = format!("{}", fp_core::pretty::pretty(&stripped, options));
        let stream = proc_macro2::TokenStream::from_str(&text)
            .map_err(|err| fp_core::error::Error::from(err.to_string()))?;
        Ok(macro_token_trees_from_proc_macro_stream(stream))
    }

    fn evaluate_item(&mut self, item: &mut Item) {
        if matches!(self.mode, InterpreterMode::CompileTime) {
            if let ItemKind::DefFunction(func) = item.kind() {
                if let Some((name, kind)) = self.proc_macro_registration(func) {
                    self.register_proc_macro(
                        &name,
                        ProcMacroDefinition {
                            kind,
                            function: func.clone(),
                        },
                    );
                    *item = Item::unit();
                    self.mark_mutated();
                    return;
                }
            }
            let mut attrs_buffer = Self::item_attrs_mut(item)
                .map(std::mem::take)
                .unwrap_or_default();
            if !attrs_buffer.is_empty() {
                if self.apply_item_attributes(item, &mut attrs_buffer) {
                    return;
                }
                if let Some(attrs) = Self::item_attrs_mut(item) {
                    *attrs = attrs_buffer;
                }
            }
        }

        match item.kind_mut() {
            ItemKind::Macro(mac) => {
                if matches!(self.mode, InterpreterMode::CompileTime) {
                    let macro_name = mac
                        .invocation
                        .path
                        .segments
                        .last()
                        .map(|seg| seg.as_str())
                        .unwrap_or("");
                    if macro_name == "macro_rules" {
                        let Some(declared) = mac.declared_name.as_ref() else {
                            self.emit_error_at(
                                mac.invocation.span,
                                "macro_rules! requires a macro name",
                            );
                            return;
                        };
                        match parse_macro_rules(&mac.invocation, declared.as_str()) {
                            Ok(def) => {
                                self.register_macro_rules(declared.as_str(), def);
                                *item = Item::unit();
                                self.mark_mutated();
                            }
                            Err(err) => {
                                self.emit_error_at(mac.invocation.span, err.to_string());
                            }
                        }
                        return;
                    }

                    let parser = match self.macro_parser.clone() {
                        Some(parser) => parser,
                        None => {
                            self.emit_error_at(
                                mac.invocation.span,
                                "macro expansion requires a parser hook",
                            );
                            return;
                        }
                    };
                    match self.expand_macro_invocation(&mac.invocation, MacroExpansionContext::Item)
                    {
                        Ok(tokens) => match parser.parse_items(&tokens) {
                            Ok(items) => {
                                if !items.is_empty() {
                                    self.append_pending_items(items);
                                }
                                *item = Item::unit();
                                self.mark_mutated();
                            }
                            Err(err) => {
                                self.emit_error_at(mac.invocation.span, err.to_string());
                            }
                        },
                        Err(err) => {
                            self.emit_error_at(mac.invocation.span, err.to_string());
                        }
                    }
                }
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
                let inferred_ty = def
                    .ty_annotation()
                    .cloned()
                    .or_else(|| def.ty.clone())
                    .or_else(|| def.value.ty().cloned());
                if let Some(ty) = inferred_ty.as_ref() {
                    if def.ty_annotation().is_none() && !self.is_unknown(ty) {
                        def.ty_annotation = Some(ty.clone());
                    }
                    if def.ty.is_none() && !self.is_unknown(ty) {
                        def.ty = Some(ty.clone());
                    }
                    self.bind_typer_symbol(def.name.as_str(), ty);
                }
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
                let inferred_ty = def
                    .ty_annotation()
                    .cloned()
                    .or_else(|| Some(def.ty.clone()))
                    .or_else(|| def.value.ty().cloned());
                if let Some(ty) = inferred_ty.as_ref() {
                    if def.ty_annotation().is_none() && !self.is_unknown(ty) {
                        def.ty_annotation = Some(ty.clone());
                    }
                    self.bind_typer_symbol(def.name.as_str(), ty);
                }
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
                self.push_item_scope(&mut module.items);
                self.pending_items.push(Vec::new());
                if let Some(typer) = self.typer.as_mut() {
                    for item in &module.items {
                        typer.initialize_from_item(item);
                    }
                }
                let mut idx = 0;
                while idx < module.items.len() {
                    if self.should_skip_lazy_item(&module.items[idx]) {
                        idx += 1;
                        continue;
                    }
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
                self.pop_item_scope();
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
                    if !has_const_params && !func.sig.is_const {
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
            | ItemKind::DeclType(_)
            | ItemKind::Any(_) => {}
            ItemKind::DeclFunction(decl) => {
                self.register_extern_function(decl);
            }
            ItemKind::Import(import) => {
                self.handle_import(import);
            }
        }
    }

    fn item_attrs_mut(item: &mut Item) -> Option<&mut Vec<Attribute>> {
        match item.kind_mut() {
            ItemKind::Module(module) => Some(&mut module.attrs),
            ItemKind::DefStruct(def) => Some(&mut def.attrs),
            ItemKind::DefStructural(def) => Some(&mut def.attrs),
            ItemKind::DefEnum(def) => Some(&mut def.attrs),
            ItemKind::DefType(def) => Some(&mut def.attrs),
            ItemKind::DefConst(def) => Some(&mut def.attrs),
            ItemKind::DefStatic(def) => Some(&mut def.attrs),
            ItemKind::DefFunction(def) => Some(&mut def.attrs),
            ItemKind::DefTrait(def) => Some(&mut def.attrs),
            ItemKind::Import(import) => Some(&mut import.attrs),
            ItemKind::Impl(impl_block) => Some(&mut impl_block.attrs),
            _ => None,
        }
    }

    fn register_extern_function(&mut self, decl: &ItemDeclFunction) {
        if !matches!(decl.sig.abi, Abi::C) {
            return;
        }
        let base_name = decl.name.as_str().to_string();
        let qualified = self.qualified_name(decl.name.as_str());
        self.extern_functions.insert(base_name, decl.sig.clone());
        self.extern_functions.insert(qualified, decl.sig.clone());
    }

    fn ensure_ffi_runtime(&mut self) -> Result<&mut FfiRuntime> {
        if self.ffi_runtime.is_none() {
            self.ffi_runtime = Some(FfiRuntime::new()?);
        }
        Ok(self
            .ffi_runtime
            .as_mut()
            .expect("ffi runtime must be initialized"))
    }

    fn try_call_extern_function(&mut self, locator: &Name, args: &[Value]) -> Option<RuntimeFlow> {
        let mut candidate_names = vec![locator.to_string()];
        if let Some(ident) = locator.as_ident() {
            candidate_names.push(ident.as_str().to_string());
        }
        for name in candidate_names {
            if let Some(sig) = self.extern_functions.get(&name).cloned() {
                return Some(self.call_extern_function_runtime(&name, &sig, args));
            }
        }
        None
    }

    fn call_extern_function_runtime(
        &mut self,
        name: &str,
        sig: &FunctionSignature,
        args: &[Value],
    ) -> RuntimeFlow {
        let ffi = match self.ensure_ffi_runtime() {
            Ok(ffi) => ffi,
            Err(err) => {
                self.emit_error(err.to_string());
                return RuntimeFlow::Value(Value::undefined());
            }
        };
        match ffi.call(name, sig, args) {
            Ok(value) => RuntimeFlow::Value(value),
            Err(err) => {
                self.emit_error(err.to_string());
                RuntimeFlow::Value(Value::undefined())
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
            ExprKind::Name(locator) => Some(Self::locator_base_name(locator)),
            ExprKind::Reference(reference) => self.type_name_from_expr(reference.referee.as_ref()),
            ExprKind::Paren(paren) => self.type_name_from_expr(paren.expr.as_ref()),
            _ => None,
        }
    }

    fn locator_base_name(locator: &Name) -> String {
        match locator {
            Name::Ident(ident) => ident.as_str().trim_start_matches('#').to_string(),
            Name::Path(path) => path
                .segments
                .last()
                .map(|ident| ident.as_str().trim_start_matches('#').to_string())
                .unwrap_or_default(),
            Name::ParameterPath(path) => path
                .segments
                .last()
                .map(|seg| seg.ident.as_str().trim_start_matches('#').to_string())
                .unwrap_or_default(),
        }
    }

    fn locator_path(locator: &Name) -> Option<Path> {
        match locator {
            Name::Ident(ident) => Some(Path::plain(vec![Ident::new(
                ident.as_str().trim_start_matches('#'),
            )])),
            Name::Path(path) => Some(Path::new(
                path.prefix,
                path.segments
                    .iter()
                    .map(|segment| Ident::new(segment.as_str().trim_start_matches('#')))
                    .collect(),
            )),
            Name::ParameterPath(path) => Some(Path::new(
                path.prefix,
                path.segments
                    .iter()
                    .map(|segment| Ident::new(segment.ident.as_str().trim_start_matches('#')))
                    .collect(),
            )),
        }
    }

    fn cast_value_to_type(&mut self, value: Value, target_ty: &Ty) -> Value {
        let primitive_target = match target_ty {
            Ty::Primitive(primitive) => Some(*primitive),
            Ty::Expr(expr) => match expr.kind() {
                ExprKind::Name(locator) => {
                    let name = match locator {
                        Name::Ident(ident) => Some(ident.as_str()),
                        Name::Path(path)
                            if path.prefix == PathPrefix::Plain && path.segments.len() == 1 =>
                        {
                            Some(path.segments[0].as_str())
                        }
                        Name::ParameterPath(path)
                            if path.prefix == PathPrefix::Plain && path.segments.len() == 1 =>
                        {
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
                Value::BigInt(big_int) => match i64::try_from(&big_int.value) {
                    Ok(value) => Value::int(value),
                    Err(_) => {
                        self.emit_error(format!(
                            "cannot cast value {} to integer type {}",
                            Value::BigInt(big_int),
                            target_ty
                        ));
                        Value::undefined()
                    }
                },
                Value::BigDecimal(big_decimal) => match big_decimal.value.to_i64() {
                    Some(value) => Value::int(value),
                    None => {
                        self.emit_error(format!(
                            "cannot cast value {} to integer type {}",
                            Value::BigDecimal(big_decimal),
                            target_ty
                        ));
                        Value::undefined()
                    }
                },
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

    fn numeric_to_i64(&mut self, value: &Value, context: &str) -> Option<i64> {
        match value {
            Value::Int(int_val) => Some(int_val.value),
            Value::Decimal(decimal_val) => Some(decimal_val.value as i64),
            Value::BigInt(big_int) => match i64::try_from(&big_int.value) {
                Ok(value) => Some(value),
                Err(_) => {
                    self.emit_error(format!("{} is out of range for i64", context));
                    None
                }
            },
            Value::BigDecimal(big_decimal) => match big_decimal.value.to_i64() {
                Some(value) => Some(value),
                None => {
                    self.emit_error(format!("{} is out of range for i64", context));
                    None
                }
            },
            other => {
                self.emit_error(format!(
                    "{} must be an integer literal, found {}",
                    context, other
                ));
                None
            }
        }
    }

    fn numeric_to_non_negative_usize(&mut self, value: &Value, context: &str) -> Option<usize> {
        let numeric_value = self.numeric_to_i64(value, context)?;
        if numeric_value < 0 {
            self.emit_error(format!("{} must be non-negative", context));
            return None;
        }
        usize::try_from(numeric_value).ok().or_else(|| {
            self.emit_error(format!("{} is out of range for usize", context));
            None
        })
    }

    fn locator_segments(locator: &Name) -> Vec<String> {
        match locator {
            Name::Ident(ident) => vec![ident.as_str().to_string()],
            Name::Path(path) => path
                .segments
                .iter()
                .map(|segment| segment.as_str().to_string())
                .collect(),
            Name::ParameterPath(path) => path
                .segments
                .iter()
                .map(|segment| segment.ident.as_str().to_string())
                .collect(),
        }
    }

    /// Helper to annotate a slice of arguments
    fn annotate_invoke_args_slice(&mut self, args: &mut [Expr], params: &[FunctionParam]) {
        for (arg, param) in args.iter_mut().zip(params.iter()) {
            let force_annotation = self.should_force_generic_annotation(arg, &param.ty);
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

            if should_annotate || force_annotation {
                arg.set_ty(param.ty.clone());
            }
        }
    }

    fn should_force_generic_annotation(&self, arg: &Expr, expected: &Ty) -> bool {
        if !matches!(expected, Ty::Function(_)) {
            return false;
        }
        let ExprKind::Name(locator) = arg.kind() else {
            return false;
        };
        self.locator_is_generic_function(locator)
    }

    fn locator_is_generic_function(&self, locator: &Name) -> bool {
        let key = Self::locator_key(locator);
        if self.generic_functions.contains_key(&key) {
            return true;
        }
        let base = Self::locator_base_name(locator);
        self.generic_functions.contains_key(&base)
    }

    // removed unused apply_callable (no callers)

    fn instantiate_generic_function(
        &mut self,
        lookup_name: &str,
        template: GenericTemplate,
        locator: &mut Name,
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
        locator: &mut Name,
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
                if matches!(param.ty, Ty::Function(_)) {
                    if let ExprKind::Name(locator) = arg.kind() {
                        if self.locator_is_generic_function(locator) {
                            continue;
                        }
                    }
                }
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

    fn update_locator_name(&self, locator: &mut Name, new_name: &str) {
        let mut path = locator.to_path();
        if let Some(last) = path.segments.last_mut() {
            *last = Ident::new(new_name.to_string());
            *locator = Name::path(path);
        }
    }

    fn sanitize_locator(locator: &mut Name) -> String {
        match locator {
            Name::Ident(ident) => {
                let current = ident.as_str().to_string();
                if let Some(trimmed) = current.strip_prefix('#') {
                    let trimmed_owned = trimmed.to_string();
                    *ident = Ident::new(trimmed_owned.clone());
                    trimmed_owned
                } else {
                    current
                }
            }
            Name::Path(path) => {
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
            Name::ParameterPath(path) => {
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

    fn locator_key(locator: &Name) -> String {
        match locator {
            Name::Ident(ident) => ident.as_str().trim_start_matches('#').to_string(),
            Name::Path(path) => path
                .segments
                .last()
                .map(|ident| ident.as_str().trim_start_matches('#').to_string())
                .unwrap_or_default(),
            Name::ParameterPath(path) => path
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
            if let ExprKind::Name(locator) = expr.kind() {
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
            Ty::Primitive(_)
            | Ty::TokenStream(_)
            | Ty::Unit(_)
            | Ty::Nothing(_)
            | Ty::Any(_)
            | Ty::Unknown(_) => ty.clone(),
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
                span: quote.span,
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
            | Ty::AnyBox(_) | Ty::RawPtr(_) => ty.clone(),
        }
    }

    fn substitute_struct(&self, ty: &TypeStruct, _subst: &HashMap<String, Ty>) -> TypeStruct {
        ty.clone()
    }

    fn generic_name(&self, ty: &Ty, generics: &HashSet<String>) -> Option<String> {
        if let Ty::Expr(expr) = ty {
            if let ExprKind::Name(locator) = expr.kind() {
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
                ExprKind::Name(locator) => {
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
                            if let ExprKind::Name(locator) = inner.kind_mut() {
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
        locator: &mut Name,
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
        if let ExprKind::Name(locator) = expr.kind_mut() {
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
            ExprKind::Name(locator) => {
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
                if let ExprKind::Name(locator) = select.obj.kind_mut() {
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
            ExprKind::Dereference(deref) => {
                if let ExprKind::Name(locator) = deref.referee.kind() {
                    if let Some(ident) = locator.as_ident() {
                        if let Some(stored) = self.lookup_stored_value_mut(ident.as_str()) {
                            if stored.assign(value.clone()) {
                                return RuntimeFlow::Value(value);
                            }
                        }
                    }
                }
                let target = match self.eval_expr_runtime(deref.referee.as_mut()) {
                    RuntimeFlow::Value(value) => value,
                    other => return other,
                };
                if let Value::Any(any) = target {
                    if let Some(runtime_ref) = any.downcast_ref::<RuntimeRef>() {
                        match runtime_ref.shared.lock() {
                            Ok(mut guard) => {
                                *guard = value.clone();
                                return RuntimeFlow::Value(value);
                            }
                            Err(err) => {
                                *err.into_inner() = value.clone();
                                return RuntimeFlow::Value(value);
                            }
                        }
                    }
                }
                self.emit_error("cannot assign through non-mutable reference");
                RuntimeFlow::Value(Value::undefined())
            }
            ExprKind::Index(index_expr) => {
                let index_value = match self.eval_expr_runtime(index_expr.index.as_mut()) {
                    RuntimeFlow::Value(value) => value,
                    other => return other,
                };
                let idx = match self.numeric_to_non_negative_usize(&index_value, "index") {
                    Some(value) => value,
                    None => return RuntimeFlow::Value(Value::undefined()),
                };

                if let ExprKind::Name(locator) = index_expr.obj.kind() {
                    if let Some(ident) = locator.as_ident() {
                        let shared_handle = match self.lookup_stored_value_mut(ident.as_str()) {
                            Some(StoredValue::Shared(shared)) => Some(Arc::clone(shared)),
                            _ => None,
                        };
                        if let Some(shared) = shared_handle {
                            if self.assign_index_shared(&shared, idx, value.clone()) {
                                return RuntimeFlow::Value(value);
                            }
                        }
                    }
                }

                let target = match self.eval_expr_runtime(index_expr.obj.as_mut()) {
                    RuntimeFlow::Value(value) => value,
                    other => return other,
                };
                if let Value::Any(any) = target {
                    if let Some(runtime_ref) = any.downcast_ref::<RuntimeRef>() {
                        if self.assign_index_shared(&runtime_ref.shared, idx, value.clone()) {
                            return RuntimeFlow::Value(value);
                        }
                    }
                }

                self.emit_error("index assignment requires a mutable list binding");
                RuntimeFlow::Value(Value::undefined())
            }
            _ => {
                self.emit_error("unsupported assignment target in runtime mode");
                RuntimeFlow::Value(Value::undefined())
            }
        }
    }

    fn assign_index_shared(
        &mut self,
        shared: &Arc<Mutex<Value>>,
        idx: usize,
        value: Value,
    ) -> bool {
        let mut guard = match shared.lock() {
            Ok(guard) => guard,
            Err(err) => err.into_inner(),
        };
        match &mut *guard {
            Value::List(list) => {
                if idx >= list.values.len() {
                    self.emit_error("index out of bounds for list");
                    return false;
                }
                list.values[idx] = value;
                true
            }
            Value::Tuple(tuple) => {
                if idx >= tuple.values.len() {
                    self.emit_error("index out of bounds for tuple");
                    return false;
                }
                tuple.values[idx] = value;
                true
            }
            _ => {
                self.emit_error("index assignment requires list or tuple");
                false
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

        let len = match self.numeric_to_non_negative_usize(&len_value, "array repeat length") {
            Some(value) => value,
            None => return Value::undefined(),
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
        if let ExprKind::Name(locator) = struct_expr.name.kind_mut() {
            if let Some(info) = self.lookup_enum_variant(locator) {
                if let EnumVariantPayload::Struct(field_names) = &info.payload {
                    let mut update_fields = HashMap::new();
                    let fields = self.build_struct_literal_fields_runtime(
                        Some(field_names),
                        &mut struct_expr.fields,
                        &mut update_fields,
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

        let mut update_fields: HashMap<String, Value> = HashMap::new();
        if let Some(update_expr) = struct_expr.update.as_mut() {
            let update_value = match self.eval_expr_runtime(update_expr.as_mut()) {
                RuntimeFlow::Value(value) => value,
                other => return self.finish_runtime_flow(other),
            };
            match update_value {
                Value::Struct(value_struct) => {
                    for field in value_struct.structural.fields {
                        update_fields.insert(field.name.as_str().to_string(), field.value);
                    }
                }
                Value::Structural(structural) => {
                    for field in structural.fields {
                        update_fields.insert(field.name.as_str().to_string(), field.value);
                    }
                }
                other => {
                    self.emit_error(format!(
                        "struct update must be a struct or structural value, found {}",
                        other
                    ));
                }
            }
        }

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
            &mut update_fields,
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
        update_fields: &mut HashMap<String, Value>,
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
                        .or_else(|| update_fields.remove(field.name.as_str()))
                        .unwrap_or_else(|| {
                            self.emit_error(format!(
                                "missing initializer for field '{}' in struct literal",
                                field.name
                            ));
                            Value::undefined()
                        });
                    value_fields.push(ValueField::new(field.name.clone(), value));
                } else {
                    let value = update_fields
                        .remove(expected_name.as_str())
                        .unwrap_or_else(|| {
                            self.emit_error(format!(
                                "missing initializer for field '{}' in struct literal",
                                expected_name
                            ));
                            Value::undefined()
                        });
                    value_fields.push(ValueField::new(expected_name.clone(), value));
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
            for update_field in update_fields.keys() {
                if !expected
                    .iter()
                    .any(|name| name.as_str() == update_field.as_str())
                {
                    self.emit_error(format!(
                        "field '{}' does not exist on this struct",
                        update_field
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
                .or_else(|| update_fields.remove(field.name.as_str()))
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

        for (name, value) in update_fields.drain() {
            value_fields.push(ValueField::new(Ident::new(name), value));
        }

        value_fields
    }

    // Evaluate indexing on list/tuple values.
    fn evaluate_index(&mut self, target: Value, index: Value) -> Value {
        match target {
            Value::Any(any) => {
                if let Some(runtime_ref) = any.downcast_ref::<RuntimeRef>() {
                    let shared_value = runtime_ref
                        .shared
                        .lock()
                        .map(|value| value.clone())
                        .unwrap_or_else(|err| err.into_inner().clone());
                    return self.evaluate_index(shared_value, index);
                }
                self.emit_error("cannot index into non-collection reference");
                Value::undefined()
            }
            Value::List(list) => {
                let idx = match self.numeric_to_non_negative_usize(&index, "index") {
                    Some(value) => value,
                    None => return Value::undefined(),
                };
                list.values.get(idx).cloned().unwrap_or_else(|| {
                    self.emit_error("index out of bounds for list");
                    Value::undefined()
                })
            }
            Value::Tuple(tuple) => {
                let idx = match self.numeric_to_non_negative_usize(&index, "index") {
                    Some(value) => value,
                    None => return Value::undefined(),
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
                value => match self.numeric_to_non_negative_usize(&value, "range start") {
                    Some(value) => Some(value),
                    None => return Value::undefined(),
                },
            },
            None => None,
        };
        let end = match range.end.as_mut() {
            Some(expr) => match self.eval_expr(expr.as_mut()) {
                value => match self.numeric_to_non_negative_usize(&value, "range end") {
                    Some(value) => Some(value),
                    None => return Value::undefined(),
                },
            },
            None => None,
        };
        self.evaluate_range_index_slices(
            target,
            start,
            end,
            matches!(range.limit, ExprRangeLimit::Inclusive),
        )
    }

    fn evaluate_range_index_slices(
        &mut self,
        target: Value,
        start: Option<usize>,
        end: Option<usize>,
        inclusive: bool,
    ) -> Value {
        match target {
            Value::Any(any) => {
                if let Some(runtime_ref) = any.downcast_ref::<RuntimeRef>() {
                    let shared_value = runtime_ref
                        .shared
                        .lock()
                        .map(|value| value.clone())
                        .unwrap_or_else(|err| err.into_inner().clone());
                    return self.evaluate_range_index_slices(shared_value, start, end, inclusive);
                }
                self.emit_error("cannot slice non-collection reference");
                Value::undefined()
            }
            Value::String(text) => {
                let chars: Vec<char> = text.value.chars().collect();
                let len = chars.len();
                let start_idx = start.unwrap_or(0);
                let mut end_idx = end.unwrap_or(len);
                if inclusive {
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
                if inclusive {
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

    fn lookup_enum_variant(&self, locator: &Name) -> Option<EnumVariantInfo> {
        let mut candidates = vec![locator.to_string()];
        if let Some(ident) = locator.as_ident() {
            candidates.push(ident.as_str().to_string());
        }
        if !self.module_stack.is_empty() {
            let locator_name = locator.to_string();
            let qualified_prefix = self.module_stack.join("::");
            if !locator_name.starts_with(&qualified_prefix) {
                candidates.push(self.qualified_name(&locator_name));
            }
        }
        for candidate in candidates {
            if let Some(info) = self.enum_variants.get(&candidate) {
                return Some(info.clone());
            }
        }
        None
    }

    fn resolve_enum_variant(&self, locator: &Name) -> Option<Value> {
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
            Value::TokenStream(_) => Some("TokenStream".to_string()),
            Value::Any(any) => any
                .downcast_ref::<RuntimeEnum>()
                .map(|enm| enm.enum_name.clone()),
            _ => None,
        }
    }

    fn eval_type_method_call(
        &mut self,
        receiver: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Option<Value> {
        let Value::Type(raw_ty) = receiver else {
            return None;
        };
        let ty = self.materialize_type(raw_ty);

        let mut expect_no_args = |name: &str| {
            if !args.is_empty() {
                self.emit_error(format!("{} expects no arguments", name));
                Some(Value::undefined())
            } else {
                None
            }
        };

        match method {
            "has_field" => {
                if args.len() != 1 {
                    self.emit_error("has_field expects exactly one argument");
                    return Some(Value::undefined());
                }
                let Value::String(name) = &args[0] else {
                    self.emit_error("has_field expects a string field name");
                    return Some(Value::undefined());
                };
                let fields = match &ty {
                    Ty::Struct(struct_ty) => struct_ty.fields.clone(),
                    Ty::Structural(structural) => structural.fields.clone(),
                    _ => {
                        self.emit_error("has_field expects a struct type");
                        return Some(Value::undefined());
                    }
                };
                let has = fields.iter().any(|field| field.name.name == name.value);
                Some(Value::bool(has))
            }
            "fields" => {
                if let Some(value) = expect_no_args("fields") {
                    return Some(value);
                }
                let fields = match &ty {
                    Ty::Struct(struct_ty) => struct_ty.fields.clone(),
                    Ty::Structural(structural) => structural.fields.clone(),
                    _ => {
                        self.emit_error("fields expects a struct type");
                        return Some(Value::undefined());
                    }
                };
                let values = fields
                    .iter()
                    .map(|field| {
                        Value::Structural(ValueStructural::new(vec![
                            ValueField::new(
                                Ident::new("name".to_string()),
                                Value::string(field.name.name.clone()),
                            ),
                            ValueField::new(
                                Ident::new("ty".to_string()),
                                Value::Type(self.materialize_type(field.value.clone())),
                            ),
                        ]))
                    })
                    .collect();
                Some(Value::List(ValueList::new(values)))
            }
            "method_count" => {
                if let Some(value) = expect_no_args("method_count") {
                    return Some(value);
                }
                Some(Value::int(0))
            }
            "field_name_at" => {
                if args.len() != 1 {
                    self.emit_error("field_name_at expects exactly one argument");
                    return Some(Value::undefined());
                }
                let fields = match &ty {
                    Ty::Struct(struct_ty) => struct_ty.fields.clone(),
                    Ty::Structural(structural) => structural.fields.clone(),
                    _ => {
                        self.emit_error("field_name_at expects a struct type");
                        return Some(Value::undefined());
                    }
                };
                let idx = match &args[0] {
                    Value::Int(int_val) if int_val.value >= 0 => int_val.value as usize,
                    _ => {
                        self.emit_error("field_name_at expects a non-negative integer index");
                        return Some(Value::undefined());
                    }
                };
                let field = match fields.get(idx) {
                    Some(field) => field,
                    None => {
                        self.emit_error(format!(
                            "field_name_at index {} out of bounds ({} fields)",
                            idx,
                            fields.len()
                        ));
                        return Some(Value::undefined());
                    }
                };
                Some(Value::string(field.name.name.clone()))
            }
            "field_type" => {
                if args.len() != 1 {
                    self.emit_error("field_type expects exactly one argument");
                    return Some(Value::undefined());
                }
                let Value::String(name) = &args[0] else {
                    self.emit_error("field_type expects a string field name");
                    return Some(Value::undefined());
                };
                let fields = match &ty {
                    Ty::Struct(struct_ty) => struct_ty.fields.clone(),
                    Ty::Structural(structural) => structural.fields.clone(),
                    _ => {
                        self.emit_error("field_type expects a struct type");
                        return Some(Value::undefined());
                    }
                };
                let field = match fields.iter().find(|field| field.name.name == name.value) {
                    Some(field) => field,
                    None => {
                        self.emit_error(format!("field '{}' not found on struct", name.value));
                        return Some(Value::undefined());
                    }
                };
                Some(Value::Type(self.materialize_type(field.value.clone())))
            }
            "has_method" => {
                if args.len() != 1 {
                    self.emit_error("has_method expects exactly one argument");
                    return Some(Value::undefined());
                }
                let Value::String(name) = &args[0] else {
                    self.emit_error("has_method expects a string method name");
                    return Some(Value::undefined());
                };
                let has_method = matches!(name.value.as_str(), "to_string");
                Some(Value::bool(has_method))
            }
            "type_name" => {
                if let Some(value) = expect_no_args("type_name") {
                    return Some(value);
                }
                let name = match &ty {
                    Ty::Struct(struct_ty) => format!("struct {}", struct_ty.name),
                    Ty::Enum(enum_ty) => format!("enum {}", enum_ty.name),
                    Ty::Expr(expr) => match expr.kind() {
                        ExprKind::Name(locator) => locator.to_string(),
                        _ => format!("{}", ty),
                    },
                    _ => format!("{}", ty),
                };
                Some(Value::string(name))
            }
            "struct_size" => {
                if let Some(value) = expect_no_args("struct_size") {
                    return Some(value);
                }
                let fields = match &ty {
                    Ty::Struct(struct_ty) => struct_ty.fields.clone(),
                    Ty::Structural(structural) => structural.fields.clone(),
                    _ => {
                        self.emit_error("struct_size expects a struct type");
                        return Some(Value::undefined());
                    }
                };
                let size = fields
                    .iter()
                    .map(|field| self.calculate_field_size(&field.value))
                    .sum::<usize>();
                Some(Value::int(size as i64))
            }
            _ => None,
        }
    }

    fn calculate_field_size(&self, ty: &Ty) -> usize {
        match ty {
            Ty::Expr(expr) => {
                if let ExprKind::Name(locator) = expr.as_ref().kind() {
                    if let Some(ident) = locator.as_ident() {
                        match ident.name.as_str() {
                            "i8" | "u8" => 1,
                            "i16" | "u16" => 2,
                            "i32" | "u32" => 4,
                            "i64" | "u64" => 8,
                            "f32" => 4,
                            "f64" => 8,
                            "bool" => 1,
                            _ => 8,
                        }
                    } else {
                        8
                    }
                } else {
                    8
                }
            }
            Ty::Primitive(primitive) => match primitive {
                TypePrimitive::Int(int_ty) => match int_ty {
                    TypeInt::I8 | TypeInt::U8 => 1,
                    TypeInt::I16 | TypeInt::U16 => 2,
                    TypeInt::I32 | TypeInt::U32 => 4,
                    TypeInt::I64 | TypeInt::U64 => 8,
                    TypeInt::BigInt => 16,
                },
                TypePrimitive::Decimal(decimal_ty) => match decimal_ty {
                    DecimalType::F32 => 4,
                    DecimalType::F64 => 8,
                    DecimalType::BigDecimal => 16,
                    DecimalType::Decimal { .. } => 16,
                },
                TypePrimitive::Bool => 1,
                TypePrimitive::Char => 4,
                _ => 8,
            },
            Ty::Struct(struct_ty) => struct_ty
                .fields
                .iter()
                .map(|field| self.calculate_field_size(&field.value))
                .sum(),
            _ => 8,
        }
    }

    fn infer_value_ty(&self, value: &Value) -> Option<Ty> {
        match value {
            Value::Int(_) => Some(Ty::Primitive(TypePrimitive::Int(TypeInt::I64))),
            Value::BigInt(_) => Some(Ty::Primitive(TypePrimitive::Int(TypeInt::BigInt))),
            Value::Decimal(_) => Some(Ty::Primitive(TypePrimitive::Decimal(DecimalType::F64))),
            Value::BigDecimal(_) => Some(Ty::Primitive(TypePrimitive::Decimal(
                DecimalType::BigDecimal,
            ))),
            Value::Bool(_) => Some(Ty::Primitive(TypePrimitive::Bool)),
            Value::Char(_) => Some(Ty::Primitive(TypePrimitive::Char)),
            Value::String(_) => Some(Ty::Primitive(TypePrimitive::String)),
            Value::List(_) => Some(Ty::Primitive(TypePrimitive::List)),
            Value::Struct(struct_value) => Some(Ty::Struct(struct_value.ty.clone())),
            Value::TokenStream(_) => Some(Ty::TokenStream(TypeTokenStream)),
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
            let mut locator = Name::path(Path::plain(vec![
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

        let mut update_fields: HashMap<String, Value> = HashMap::new();
        if let Some(update_expr) = struct_expr.update.as_mut() {
            let update_value = self.eval_expr(update_expr.as_mut());
            match update_value {
                Value::Struct(value_struct) => {
                    for field in value_struct.structural.fields {
                        update_fields.insert(field.name.as_str().to_string(), field.value);
                    }
                }
                Value::Structural(structural) => {
                    for field in structural.fields {
                        update_fields.insert(field.name.as_str().to_string(), field.value);
                    }
                }
                other => {
                    self.emit_error(format!(
                        "struct update must be a struct or structural value, found {}",
                        other
                    ));
                }
            }
        }

        let mut fields = Vec::new();
        for field in &mut struct_expr.fields {
            let value = field
                .value
                .as_mut()
                .map(|expr| self.eval_expr(expr))
                .unwrap_or_else(|| {
                    update_fields
                        .remove(field.name.as_str())
                        .or_else(|| self.lookup_value(field.name.as_str()))
                        .unwrap_or_else(|| {
                            self.emit_error(format!(
                                "missing initializer for field '{}' in struct literal",
                                field.name
                            ));
                            Value::undefined()
                        })
                });
            fields.push(ValueField::new(field.name.clone(), value));
        }

        for (name, value) in update_fields {
            fields.push(ValueField::new(Ident::new(name), value));
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
                ExprKind::Name(locator) => {
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

        let len = match self.numeric_to_non_negative_usize(&len_value, "array repeat length") {
            Some(value) => value,
            None => return Value::undefined(),
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
                let len = match self.numeric_to_i64(&len_value, "array length") {
                    Some(value) => value,
                    None => return,
                };
                let replacement = Expr::value(Value::int(len));
                *array_ty.len = replacement.into();
            }
            Ty::Expr(expr) => match expr.kind() {
                ExprKind::ConstBlock(_) => {
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
                ExprKind::Macro(macro_expr) => {
                    let parser = match self.macro_parser.clone() {
                        Some(parser) => parser,
                        None => {
                            self.emit_error_at(
                                macro_expr.invocation.span,
                                "macro expansion requires a parser hook",
                            );
                            return;
                        }
                    };
                    if self.macro_depth > 64 {
                        self.emit_error_at(
                            macro_expr.invocation.span,
                            "macro expansion exceeded recursion limit",
                        );
                        return;
                    }
                    self.macro_depth += 1;
                    let expanded = self
                        .expand_macro_invocation(
                            &macro_expr.invocation,
                            MacroExpansionContext::Type,
                        )
                        .and_then(|tokens| parser.parse_type(&tokens));
                    self.macro_depth = self.macro_depth.saturating_sub(1);
                    match expanded {
                        Ok(new_ty) => {
                            *ty = new_ty;
                            self.mark_mutated();
                        }
                        Err(err) => {
                            self.emit_error_at(macro_expr.invocation.span, err.to_string());
                        }
                    }
                }
                _ => {}
            },
            _ => {}
        }
    }

    fn evaluate_select(&mut self, target: Value, field: &str) -> Value {
        match target {
            Value::Type(_) if field == "fields" => self
                .eval_type_method_call(target, "fields", Vec::new())
                .unwrap_or_else(|| Value::undefined()),
            Value::Type(_) if field == "name" => self
                .eval_type_method_call(target, "type_name", Vec::new())
                .unwrap_or_else(|| Value::undefined()),
            Value::Type(_) if field == "methods" => Value::List(ValueList::new(Vec::new())),
            Value::Type(_) if field == "size" => self
                .eval_type_method_call(target, "struct_size", Vec::new())
                .unwrap_or_else(|| Value::undefined()),
            Value::Type(_) => {
                self.emit_error(format!("cannot access field '{}' on type values", field));
                Value::undefined()
            }
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

    fn lookup_const_collection_value(&self, locator: &Name) -> Option<Value> {
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
        let ExprKind::Name(locator) = expr.kind() else {
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
            | Value::BigInt(_)
            | Value::BigDecimal(_)
            | Value::Char(_)
            | Value::String(_) => Some(value.clone()),
            _ => None,
        }
    }

    fn const_scalar_from_locator(&self, locator: &Name) -> Option<Value> {
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
            ExprKind::Name(locator) => self.const_scalar_from_locator(locator),
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
                            _ => match self.numeric_to_non_negative_usize(&key, "list index") {
                                Some(index) => {
                                    list.values.get(index).cloned().unwrap_or_else(|| {
                                        self.emit_error("index out of bounds for list");
                                        Value::undefined()
                                    })
                                }
                                None => Value::undefined(),
                            },
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

    fn resolve_type_binding(&mut self, path: &Path) -> Option<Ty> {
        match resolve_type_binding_match(
            &self.type_env,
            &self.global_types,
            &self.imported_types,
            path,
        ) {
            TypeBindingMatch::Scoped { index, key, ty } => {
                let resolved = self.materialize_const_type(ty);
                self.type_env[index].insert(key, resolved.clone());
                Some(resolved)
            }
            TypeBindingMatch::Global { key, ty } => {
                let resolved = self.materialize_const_type(ty);
                self.global_types.insert(key, resolved.clone());
                Some(resolved)
            }
            TypeBindingMatch::MissingImported { name } => {
                self.emit_error(format!(
                    "imported type '{}' cannot be resolved without module execution",
                    name
                ));
                None
            }
            TypeBindingMatch::Missing => None,
        }
    }

    fn resolve_type_binding_spec(&mut self, spec: &str) -> Option<Ty> {
        let path = self.parse_symbol_path(spec)?;
        self.resolve_type_binding(&path)
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

    fn parse_symbol_path(&self, spec: &str) -> Option<Path> {
        let parsed = parse_path(spec).ok()?;
        if parsed.segments.is_empty() {
            return None;
        }
        Some(Path::new(
            parsed.prefix,
            parsed.segments.into_iter().map(Ident::new).collect(),
        ))
    }

    fn path_segments<'a>(&self, path: &'a Path) -> Vec<&'a str> {
        path.segments.iter().map(|segment| segment.as_str()).collect()
    }

    fn handle_import(&mut self, import: &ItemImport) {
        let context = match self.module_resolution.clone() {
            Some(context) => context,
            None => {
                self.handle_local_import(import);
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

    fn handle_local_import(&mut self, import: &ItemImport) {
        for directive in self.expand_import_tree(&import.tree) {
            match directive.binding {
                ImportBinding::Module { alias } => {
                    if let Some(alias) = alias {
                        self.local_imports
                            .insert(alias, directive.module_spec.clone());
                    }
                }
                ImportBinding::Symbol { name, alias } => {
                    let binding = alias.unwrap_or_else(|| name.clone());
                    let target = if directive.module_spec.is_empty() {
                        name
                    } else {
                        format!("{}::{}", directive.module_spec, name)
                    };
                    self.local_imports.insert(binding, target);
                }
                ImportBinding::Glob => {
                    let Some(exports) = self.local_module_exports(&directive.module_spec) else {
                        continue;
                    };
                    for name in exports {
                        let target = if directive.module_spec.is_empty() {
                            name.clone()
                        } else {
                            format!("{}::{}", directive.module_spec, name)
                        };
                        self.local_imports.insert(name, target);
                    }
                }
            }
        }
    }

    fn local_module_exports(&self, module_spec: &str) -> Option<Vec<String>> {
        let root = self.root_items?;
        let path = self.parse_symbol_path(module_spec)?;
        let segments = self.path_segments(&path);
        if segments.is_empty() {
            return None;
        }
        let item_ptr = self.find_item_by_path_mut(root, &segments)?;
        let item = unsafe { &*item_ptr };
        let ItemKind::Module(module) = item.kind() else {
            return None;
        };
        let mut exports = Vec::new();
        for item in &module.items {
            let name = match item.kind() {
                ItemKind::DefStruct(def) => Some(def.name.as_str()),
                ItemKind::DefStructural(def) => Some(def.name.as_str()),
                ItemKind::DefEnum(def) => Some(def.name.as_str()),
                ItemKind::DefType(def) => Some(def.name.as_str()),
                ItemKind::DefConst(def) => Some(def.name.as_str()),
                ItemKind::DefStatic(def) => Some(def.name.as_str()),
                ItemKind::DefFunction(def) => Some(def.name.as_str()),
                ItemKind::DefTrait(def) => Some(def.name.as_str()),
                ItemKind::Module(def) => Some(def.name.as_str()),
                _ => None,
            };
            if let Some(name) = name {
                exports.push(name.to_string());
            }
        }
        Some(exports)
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
        self.parse_symbol_path(spec)
            .and_then(|path| path.segments.last().map(|segment| segment.as_str().to_string()))
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
                if let Some(ty) = self.resolve_type_binding_spec(&self_ty) {
                    return Value::Type(ty);
                }
            }
        }
        if let Some(ty) = self.resolve_type_binding_spec(&symbol) {
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
        if matches!(self.mode, InterpreterMode::CompileTime) && self.materialize_symbol(&symbol) {
            if let Some(value) = self.evaluated_constants.get(&symbol) {
                return value.clone();
            }
            let qualified = self.qualified_name(&symbol);
            if let Some(value) = self.evaluated_constants.get(&qualified) {
                return value.clone();
            }
            if let Some(ty) = self.resolve_type_binding_spec(&symbol) {
                return Value::Type(ty);
            }
            if let Some(value) = self.lookup_value(&symbol) {
                return value;
            }
        }
        let qualified = self.qualified_name(&symbol);
        if let Some(value) = self.evaluated_constants.get(&qualified) {
            return value.clone();
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
        self.macro_env.push(HashMap::new());
        self.proc_macro_env.push(HashMap::new());
        if let Some(typer) = self.typer.as_mut() {
            typer.push_scope();
        }
    }

    fn pop_scope(&mut self) {
        self.value_env.pop();
        self.type_env.pop();
        self.macro_env.pop();
        self.proc_macro_env.pop();
        if let Some(typer) = self.typer.as_mut() {
            typer.pop_scope();
        }
    }

    fn emit_error(&mut self, message: impl Into<String>) {
        self.emit_error_at(self.current_span, message);
    }

    fn emit_warning(&mut self, message: impl Into<String>) {
        let diagnostic =
            Diagnostic::warning(message.into()).with_source_context(self.diagnostic_context);
        self.push_diagnostic(diagnostic);
    }

    fn emit_error_at(&mut self, span: Option<Span>, message: impl Into<String>) {
        let mut diagnostic =
            Diagnostic::error(message.into()).with_source_context(self.diagnostic_context);
        if let Some(span) = span.or(self.current_span) {
            diagnostic = diagnostic.with_span(span);
        }
        self.push_diagnostic(diagnostic);
    }

    fn push_span(&mut self, span: Option<Span>) -> SpanGuard<'ctx> {
        let prev = self.current_span;
        if span.is_some() {
            self.current_span = span;
        }
        SpanGuard {
            interpreter: self as *mut _,
            prev,
        }
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

fn meta_list_tokens(items: &[AttrMeta]) -> Vec<MacroTokenTree> {
    let mut texts = Vec::new();
    for (idx, item) in items.iter().enumerate() {
        if idx > 0 {
            texts.push(",".to_string());
        }
        meta_to_token_texts(item, &mut texts);
    }
    texts
        .into_iter()
        .map(|text| {
            MacroTokenTree::Token(MacroToken {
                text,
                span: Span::null(),
            })
        })
        .collect()
}

fn meta_name_value_tokens(nv: &AttrMetaNameValue) -> Vec<MacroTokenTree> {
    let mut texts = Vec::new();
    meta_path_to_texts(&nv.name, &mut texts);
    texts.push("=".to_string());
    if let Some(value_text) = attr_value_text(nv.value.as_ref()) {
        texts.push(value_text);
    }
    texts
        .into_iter()
        .map(|text| {
            MacroTokenTree::Token(MacroToken {
                text,
                span: Span::null(),
            })
        })
        .collect()
}

fn meta_to_token_texts(meta: &AttrMeta, out: &mut Vec<String>) {
    match meta {
        AttrMeta::Path(path) => meta_path_to_texts(path, out),
        AttrMeta::NameValue(nv) => {
            meta_path_to_texts(&nv.name, out);
            out.push("=".to_string());
            if let Some(value_text) = attr_value_text(nv.value.as_ref()) {
                out.push(value_text);
            }
        }
        AttrMeta::List(list) => {
            meta_path_to_texts(&list.name, out);
            out.push("(".to_string());
            for (idx, item) in list.items.iter().enumerate() {
                if idx > 0 {
                    out.push(",".to_string());
                }
                meta_to_token_texts(item, out);
            }
            out.push(")".to_string());
        }
    }
}

fn meta_path_to_texts(path: &Path, out: &mut Vec<String>) {
    match path.prefix {
        PathPrefix::Root => out.push("::".to_string()),
        PathPrefix::Crate => out.push("crate".to_string()),
        PathPrefix::SelfMod => out.push("self".to_string()),
        PathPrefix::Super(depth) => {
            for idx in 0..depth {
                if idx > 0 {
                    out.push("::".to_string());
                }
                out.push("super".to_string());
            }
        }
        PathPrefix::Plain => {}
    }
    for (idx, seg) in path.segments.iter().enumerate() {
        if idx > 0 || path.prefix != PathPrefix::Plain {
            out.push("::".to_string());
        }
        out.push(seg.as_str().to_string());
    }
}

fn attr_value_text(expr: &Expr) -> Option<String> {
    match expr.kind() {
        ExprKind::Value(value) => match value.as_ref() {
            Value::String(value) => Some(format!("{:?}", value.value)),
            _ => None,
        },
        _ => None,
    }
}

fn macro_token_trees_from_proc_macro_stream(
    stream: proc_macro2::TokenStream,
) -> Vec<MacroTokenTree> {
    stream
        .into_iter()
        .flat_map(macro_token_trees_from_proc_macro_tree)
        .collect()
}

fn macro_token_trees_from_proc_macro_tree(tree: TokenTree) -> Vec<MacroTokenTree> {
    match tree {
        TokenTree::Group(group) => {
            let inner = macro_token_trees_from_proc_macro_stream(group.stream());
            let delimiter = match group.delimiter() {
                Delimiter::Parenthesis => Some(fp_core::ast::MacroDelimiter::Parenthesis),
                Delimiter::Brace => Some(fp_core::ast::MacroDelimiter::Brace),
                Delimiter::Bracket => Some(fp_core::ast::MacroDelimiter::Bracket),
                Delimiter::None => None,
            };
            if let Some(delimiter) = delimiter {
                vec![MacroTokenTree::Group(MacroGroup {
                    delimiter,
                    tokens: inner,
                    span: Span::null(),
                })]
            } else {
                inner
            }
        }
        TokenTree::Ident(ident) => vec![MacroTokenTree::Token(MacroToken {
            text: ident.to_string(),
            span: Span::null(),
        })],
        TokenTree::Punct(punct) => vec![MacroTokenTree::Token(MacroToken {
            text: punct.as_char().to_string(),
            span: Span::null(),
        })],
        TokenTree::Literal(literal) => vec![MacroTokenTree::Token(MacroToken {
            text: literal.to_string(),
            span: Span::null(),
        })],
    }
}

fn intrinsic_symbol(kind: IntrinsicCallKind) -> Option<&'static str> {
    match kind {
        IntrinsicCallKind::SizeOf => Some("sizeof!"),
        IntrinsicCallKind::ReflectFields => Some("reflect_fields!"),
        IntrinsicCallKind::HasMethod => Some("hasmethod!"),
        IntrinsicCallKind::TypeName => Some("type_name!"),
        IntrinsicCallKind::TypeOf => Some("type_of!"),
        IntrinsicCallKind::CreateStruct => Some("create_struct"),
        IntrinsicCallKind::CloneStruct => Some("clone_struct!"),
        IntrinsicCallKind::AddField => Some("addfield"),
        IntrinsicCallKind::HasField => Some("hasfield!"),
        IntrinsicCallKind::FieldCount => Some("field_count!"),
        IntrinsicCallKind::MethodCount => Some("method_count!"),
        IntrinsicCallKind::FieldType => Some("field_type!"),
        IntrinsicCallKind::VecType => Some("vec_type!"),
        IntrinsicCallKind::FieldNameAt => Some("field_name_at!"),
        IntrinsicCallKind::StructSize => Some("struct_size!"),
        IntrinsicCallKind::GenerateMethod => Some("generate_method!"),
        IntrinsicCallKind::CompileError => Some("compile_error!"),
        IntrinsicCallKind::CompileWarning => Some("compile_warning!"),
        _ => None,
    }
}
