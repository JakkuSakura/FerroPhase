//! Shared intrinsic and standard-library descriptors.
//!
//! Language-specific helpers are normalized at the AST-to-HIR boundary. This
//! module hosts the shared vocabulary and the single-expression hook used by
//! that lowering pass.

use crate::ast::{
    Abi, AttrMeta, Attribute, Expr, ExprIntrinsicCall, ExprIntrinsicContainer, ExprKind,
    ExprStruct, ExprStructural, File, FunctionParam, FunctionSignature, Ident, Item,
    ItemDeclFunction, ItemKind, Ty, TySlot, TypeFunction, Value,
};
use crate::error::Result;
use std::collections::HashMap;

/// Extracts `key`'s string value from a call-style `#[op(key = "value")]`
/// attribute (`#[op(class = "Foo")]`, `#[op(method = "bar")]`,
/// `#[op(func = "baz")]`) — the single, canonical portable-op marker,
/// recognized identically wherever a frontend's stdlib source needs to
/// declare one (`ast_to_hir`'s per-item lowering, `fp-core::lang`'s
/// legacy pre-HIR item scan).
pub fn extract_op_attr(attrs: &[Attribute], key: &str) -> Option<String> {
    for attr in attrs {
        let AttrMeta::List(list) = &attr.meta else {
            continue;
        };
        if list.name.last().as_str() != "op" {
            continue;
        }
        for item in &list.items {
            let AttrMeta::NameValue(meta) = item else {
                continue;
            };
            if meta.name.last().as_str() != key {
                continue;
            }
            if let ExprKind::Value(value) = meta.value.kind() {
                if let Value::String(string) = &**value {
                    return Some(string.value.clone());
                }
            }
        }
    }
    None
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NormalizeOutcome<T> {
    /// The strategy chose not to handle this node; the shared framework should
    /// continue normalizing it (including descending into children).
    Ignored(T),
    /// The strategy normalized this node and produced a replacement.
    Normalized(T),
}

impl<T> NormalizeOutcome<T> {
    pub fn into_inner(self) -> T {
        match self {
            NormalizeOutcome::Ignored(value) | NormalizeOutcome::Normalized(value) => value,
        }
    }

    pub fn is_normalized(&self) -> bool {
        matches!(self, NormalizeOutcome::Normalized(_))
    }
}

/// Default strategy that never performs language-specific normalization.
#[derive(Debug, Default, Clone, Copy)]
pub struct NoopIntrinsicNormalizer;

/// Strategy interface for language-specific intrinsic normalisation.
pub trait IntrinsicNormalizer {
    /// Supply macro_rules definitions collected by the frontend so expression
    /// expansion observes the same lexical macro environment as item
    /// expansion.
    fn set_macro_rules_defs(&mut self, _defs: HashMap<String, crate::ast::MacroRulesDef>) {}

    /// Normalize one expression root before a downstream lowering pass handles
    /// it. Child expressions are owned by that lowering pass, so this method
    /// deliberately does not walk the tree.
    fn normalize_expr(&self, expr: Expr) -> Result<NormalizeOutcome<Expr>> {
        let kind = expr.kind();
        match kind {
            crate::ast::ExprKind::Macro(_) => self.normalize_macro(expr),
            crate::ast::ExprKind::IntrinsicCall(_) => self.normalize_call(expr),
            crate::ast::ExprKind::IntrinsicContainer(_) => self.normalize_container(expr),
            crate::ast::ExprKind::Struct(_) => self.normalize_struct(expr),
            crate::ast::ExprKind::Structural(_) => self.normalize_structural(expr),
            crate::ast::ExprKind::Invoke(_) => self.normalize_invoke(expr),
            crate::ast::ExprKind::Match(_) => self.normalize_match(expr),
            _ => Ok(NormalizeOutcome::Ignored(expr)),
        }
    }

    /// Strategy hook for intrinsic call expressions.
    ///
    /// The framework guarantees `expr.kind()` is `ExprKind::IntrinsicCall`.
    fn normalize_call(&self, expr: Expr) -> Result<NormalizeOutcome<Expr>> {
        Ok(NormalizeOutcome::Ignored(expr))
    }

    /// Strategy hook for intrinsic container expressions.
    ///
    /// The framework guarantees `expr.kind()` is `ExprKind::IntrinsicContainer`.
    fn normalize_container(&self, expr: Expr) -> Result<NormalizeOutcome<Expr>> {
        Ok(NormalizeOutcome::Ignored(expr))
    }

    /// Strategy hook for struct literal expressions.
    ///
    /// The framework guarantees `expr.kind()` is `ExprKind::Struct`.
    fn normalize_struct(&self, expr: Expr) -> Result<NormalizeOutcome<Expr>> {
        Ok(NormalizeOutcome::Ignored(expr))
    }

    /// Strategy hook for structural literal expressions.
    ///
    /// The framework guarantees `expr.kind()` is `ExprKind::Structural`.
    fn normalize_structural(&self, expr: Expr) -> Result<NormalizeOutcome<Expr>> {
        Ok(NormalizeOutcome::Ignored(expr))
    }

    /// Strategy hook for invoke (method call) expressions.
    ///
    /// The framework guarantees `expr.kind()` is `ExprKind::Invoke`.
    fn normalize_invoke(&self, expr: Expr) -> Result<NormalizeOutcome<Expr>> {
        Ok(NormalizeOutcome::Ignored(expr))
    }

    /// Strategy hook for match expressions (e.g., if-let desugaring).
    /// The framework guarantees `expr.kind()` is `ExprKind::Match`.
    fn normalize_match(&self, expr: Expr) -> Result<NormalizeOutcome<Expr>> {
        Ok(NormalizeOutcome::Ignored(expr))
    }

    /// Language-specific macro lowering hook. When provided by a frontend, the
    /// shared intrinsic normalizer will delegate `ExprKind::Macro` to this
    /// implementation. Return `NormalizeOutcome::Normalized(expr)` to replace
    /// the macro with `expr`.
    /// Strategy hook for macro expressions.
    ///
    /// The framework guarantees `expr.kind()` is `ExprKind::Macro`.
    fn normalize_macro(&self, expr: Expr) -> Result<NormalizeOutcome<Expr>> {
        Ok(NormalizeOutcome::Ignored(expr))
    }

    /// Language-specific *item*-position macro expansion hook (e.g. real
    /// Rust std's own `macro_rules! alias_core_ffi { ($($t:ident)*) => {$(
    /// pub type $t = core::ffi::$t; )*} }` idiom for batch-generating C-FFI
    /// type aliases). `defs` is every `macro_rules!` definition the shared
    /// single-pass item walker (`ast_to_hir::expand_item_macros`, the only
    /// caller) has collected *so far* in its one descent — not necessarily
    /// every definition in the whole package, matching real Rust's own
    /// declare-before-use macro visibility instead of treating
    /// `macro_rules!` as globally order-independent. `invocation` is one
    /// item-position macro call site. Returns `Some(items)` when a
    /// frontend's real macro engine (fp-lang's
    /// `expand_item_macro_invocation`) matched a rule and re-parsed its
    /// substituted output into real items; `None` when the name is
    /// unknown yet or no rule matched, in which case the caller leaves the
    /// invocation as an unexpanded item (unchanged from this hook's
    /// absence).
    fn expand_item_macro(
        &self,
        _invocation: &crate::ast::MacroInvocation,
        _defs: &std::collections::HashMap<String, crate::ast::MacroRulesDef>,
    ) -> Option<Vec<Item>> {
        None
    }

    /// Called by the shared single-pass item walker
    /// (`ast_to_hir::expand_item_macros`) when it encounters a second
    /// `macro_rules!` definition for a name it's already seen earlier in
    /// the same walk — `existing_depth`/`new_depth` are each
    /// definition's own module-nesting depth (`PackageItem::module_path`
    /// length, plus one per genuinely inline `mod foo { .. }` block
    /// crossed). Returns whether the new definition should replace the
    /// existing one for subsequent invocations. Default: always replace
    /// (last one found in the walk wins) — correct wherever a macro name
    /// is unique in the package, the overwhelmingly common case; a
    /// language with a real same-name collision to resolve (see
    /// `fp_rust::RustIntrinsicNormalizer`'s own doc comment for vendored
    /// std's `uint_impl!`) overrides this with its own policy instead of
    /// needing a general scoping mechanism here.
    fn prefer_macro_rules_def(&self, _existing_depth: usize, _new_depth: usize) -> bool {
        true
    }

    /// Parses a `macro_rules! name { .. }` item's body into a matchable
    /// `MacroRulesDef`, for the shared single-pass walker
    /// (`ast_to_hir::expand_item_macros`) to insert into its running `defs`
    /// map as soon as the definition is encountered. Language-specific
    /// (each source language's own macro grammar), so there's no default
    /// parse here — a normalizer for a macro-free language just never
    /// hits this (no `ItemKind::Macro` definitions exist to call it on).
    fn parse_macro_rules_def(
        &self,
        name: &str,
        _tokens: &[crate::ast::MacroTokenTree],
    ) -> crate::ast::MacroRulesDef {
        crate::ast::MacroRulesDef {
            name: name.to_string(),
            rules: Vec::new(),
        }
    }
}

impl IntrinsicNormalizer for NoopIntrinsicNormalizer {}

impl IntrinsicNormalizer for Box<dyn IntrinsicNormalizer> {
    fn normalize_expr(&self, expr: Expr) -> Result<NormalizeOutcome<Expr>> {
        self.as_ref().normalize_expr(expr)
    }
    fn normalize_call(&self, expr: Expr) -> Result<NormalizeOutcome<Expr>> {
        self.as_ref().normalize_call(expr)
    }
    fn normalize_container(&self, expr: Expr) -> Result<NormalizeOutcome<Expr>> {
        self.as_ref().normalize_container(expr)
    }
    fn normalize_struct(&self, expr: Expr) -> Result<NormalizeOutcome<Expr>> {
        self.as_ref().normalize_struct(expr)
    }
    fn normalize_structural(&self, expr: Expr) -> Result<NormalizeOutcome<Expr>> {
        self.as_ref().normalize_structural(expr)
    }
    fn normalize_invoke(&self, expr: Expr) -> Result<NormalizeOutcome<Expr>> {
        self.as_ref().normalize_invoke(expr)
    }
    fn normalize_match(&self, expr: Expr) -> Result<NormalizeOutcome<Expr>> {
        self.as_ref().normalize_match(expr)
    }
    fn normalize_macro(&self, expr: Expr) -> Result<NormalizeOutcome<Expr>> {
        self.as_ref().normalize_macro(expr)
    }
    fn expand_item_macro(
        &self,
        invocation: &crate::ast::MacroInvocation,
        defs: &std::collections::HashMap<String, crate::ast::MacroRulesDef>,
    ) -> Option<Vec<Item>> {
        self.as_ref().expand_item_macro(invocation, defs)
    }
    fn parse_macro_rules_def(
        &self,
        name: &str,
        tokens: &[crate::ast::MacroTokenTree],
    ) -> crate::ast::MacroRulesDef {
        self.as_ref().parse_macro_rules_def(name, tokens)
    }
    fn prefer_macro_rules_def(&self, existing_depth: usize, new_depth: usize) -> bool {
        self.as_ref()
            .prefer_macro_rules_def(existing_depth, new_depth)
    }
}

/// Strategy interface for backend-specific intrinsic materialisation.
pub trait IntrinsicMaterializer {
    fn prepare_file(&self, _file: &mut File) {}

    fn materialize_invoke(
        &self,
        _invoke: &mut crate::ast::ExprInvoke,
        _expr_ty: &TySlot,
    ) -> Result<Option<Expr>> {
        Ok(None)
    }

    fn materialize_call(
        &self,
        _call: &mut ExprIntrinsicCall,
        _expr_ty: &TySlot,
    ) -> Result<Option<Expr>> {
        Ok(None)
    }

    fn materialize_struct(
        &self,
        _struct_expr: &mut ExprStruct,
        _expr_ty: &TySlot,
    ) -> Result<Option<Expr>> {
        Ok(None)
    }

    fn materialize_structural(
        &self,
        _struct_expr: &mut ExprStructural,
        _expr_ty: &TySlot,
    ) -> Result<Option<Expr>> {
        Ok(None)
    }

    fn materialize_container(
        &self,
        _container: &mut ExprIntrinsicContainer,
        _expr_ty: &TySlot,
    ) -> Result<Option<Expr>> {
        Ok(None)
    }
}

fn build_function_decl_item(
    name: &str,
    mut params: Vec<FunctionParam>,
    ret_ty: Ty,
) -> ItemDeclFunction {
    let name_ident = Ident::new(name);
    for param in params.iter_mut() {
        if param.ty_annotation.is_none() {
            param.ty_annotation = Some(param.ty.clone());
        }
    }

    let ty_annotation = Ty::Function(TypeFunction {
        params: params.iter().map(|p| p.ty.clone()).collect(),
        generics_params: Vec::new(),
        ret_ty: Some(Box::new(ret_ty.clone())),
    });

    let sig = FunctionSignature {
        name: Some(name_ident.clone()),
        receiver: None,
        params,
        generics_params: Vec::new(),
        is_const: false,
        abi: Abi::Rust,
        quote_kind: None,
        ret_ty: Some(ret_ty),
    };

    ItemDeclFunction {
        attrs: Vec::new(),
        ty_annotation: Some(ty_annotation),
        name: name_ident,
        sig,
    }
}

/// Insert a function declaration if one with the same name does not already exist.
pub fn make_function_decl(name: &str, params: Vec<FunctionParam>, ret_ty: Ty) -> ItemDeclFunction {
    build_function_decl_item(name, params, ret_ty)
}

pub fn ensure_function_decl(file: &mut File, decl: ItemDeclFunction) {
    let name = decl.name.clone();
    let exists = file.items.iter().any(|item| match item.kind() {
        ItemKind::DeclFunction(existing) if existing.name == name => true,
        ItemKind::DefFunction(existing) if existing.name == name => true,
        _ => false,
    });

    if exists {
        return;
    }

    file.items.insert(0, Item::from(decl));
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum StdIntrinsic {
    // I/O
    IoPrintln,
    IoPrint,
    IoEprint,
    IoEprintln,

    // Memory allocation
    AllocAlloc,
    AllocDealloc,
    AllocRealloc,

    // Math - f64
    F64Sin,
    F64Cos,
    F64Tan,
    F64Sqrt,
    F64Pow,
    F64Log,
    F64Exp,

    // Math - f32
    F32Sin,
    F32Cos,
    F32Tan,
    F32Sqrt,
    F32Pow,
    F32Log,
    F32Exp,

    // String operations
    StrLen,
    StrCmp,

    // Process control
    ProcessExit,
    ProcessAbort,
}

pub mod calls;
mod lang_intrinsic;
pub mod materialize;

pub use calls::{
    ArityShape, CallKind, IntrinsicKind, KnownClass, KnownPackage, PortableOp, PortableOpDef,
    PortableOpRegistry, ResultTypeRule,
};
pub use lang_intrinsic::{
    LangIntrinsic, LangIntrinsicCapability, LangIntrinsicSpec, lang_intrinsic_call_kind,
    lang_intrinsic_capability, lang_intrinsic_for_lang_item, lang_intrinsic_lang_item,
    lang_intrinsic_spec,
};
pub use materialize::{
    materialize_block, materialize_expr, materialize_file, materialize_invoke_target,
    materialize_item, materialize_stmt, materialize_value,
};
