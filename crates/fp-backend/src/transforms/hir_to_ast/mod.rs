use std::collections::HashMap;
use std::path::PathBuf;

use fp_core::ast::{
    self, BlockStmt, BlockStmtExpr, Expr, ExprArray, ExprAssign, ExprBinOp, ExprBlock, ExprBreak,
    ExprCast, ExprClosure, ExprContinue, ExprIf, ExprIndex, ExprIntrinsicCall, ExprKwArg, ExprLet, ExprLoop,
    ExprMatch, ExprMatchCase, ExprReference, ExprReturn, ExprSelect, ExprSelectType,
    ExprStringTemplate, ExprStruct, ExprTry, ExprTryCatch, ExprTuple, ExprUnOp, ExprWhile, ExprWith,
    FunctionParam, FunctionSignature, Ident, Item, ItemDeclFunction, ItemDefConst, ItemDefEnum,
    ItemDefFunction, ItemDefStruct, ItemKind, Name, Path, Pattern, PatternIdent, PatternKind,
    PatternStruct, PatternStructField, PatternTuple, PatternTupleStruct, PatternVariant,
    StructuralField, Ty, TypeArray, TypeEnum, TypeFunction, TypeReference, TypeSlice, TypeStruct,
    TypeTuple, Value,
};
use fp_core::error::Result;
use fp_core::hir;
use fp_core::hir::DefId;
use fp_core::intrinsics::CallKind;
use fp_core::ops::{BinOpKind, UnOpKind};
use fp_core::span::Span;
use fp_typing::TypeckResults;

/// Lifts a typechecked `hir::Program` back into a plain `ast::File` — the
/// shape every backend serializer (Kotlin, Python, Go, ...) already knows
/// how to consume, so `PipelineMode::TypecheckedTranspile` can reuse those
/// serializers unchanged rather than each needing its own HIR-consuming path.
///
/// Carries the source `&hir::Program` (needed for a couple of program-wide
/// lookups: the single-`Query`-item check, closure-signature reconstruction,
/// and now `DefId` → path resolution via `program.def_paths`) and an
/// optional `&TypeckResults` — optional because two of the three call sites
/// never run the typer at all (see `lift_program`'s free-function wrapper
/// below), so there's nothing to attach in those cases.
pub struct HirToAstLifter<'a> {
    program: &'a hir::Program,
    typeck: Option<&'a TypeckResults>,
}

impl<'a> HirToAstLifter<'a> {
    pub fn new(program: &'a hir::Program, typeck: Option<&'a TypeckResults>) -> Self {
        Self { program, typeck }
    }

    pub fn lift_program(&self, path: PathBuf) -> Result<ast::File> {
        if let [item] = self.program.items.as_slice() {
            if let hir::ItemKind::Query(_query) = &item.kind {
                // Queries are returned as a File with no items for now
                return Ok(ast::File {
                    path,
                    attrs: Vec::new(),
                    collected_items: Vec::new(),
                    items: Vec::new(),
                });
            }
        }
        let mut items = Vec::with_capacity(self.program.items.len());
        for item in &self.program.items {
            items.push(self.lift_item(item)?);
        }
        // Reconstruct closure expressions with typed params from lowered closure pairs
        let items = self.reconstruct_closures(items)?;
        Ok(ast::File {
            path,
            attrs: Vec::new(),
            collected_items: Vec::new(),
            items,
        })
    }

    /// Best-effort variant of [`lift_program`](Self::lift_program) for
    /// splicing typed content back onto an existing source AST
    /// (`fp-cli::compiler::typecheck_package`), keyed by each item's own
    /// qualified name (`program.def_paths`) rather than list position.
    ///
    /// Unlike `lift_program`, a single item that fails to lift (e.g. a
    /// nested `hir::ExprKind::Query`, or any other not-yet-supported
    /// shape) is simply omitted from the result instead of aborting the
    /// whole program — the caller keeps that one item's original,
    /// untyped source form rather than losing typed info for every other
    /// item in the package. Items with no entry in `def_paths` (e.g.
    /// synthetic struct definitions for anonymous/structural literals,
    /// `register_structural_value_def`/`materialize_enum_struct_payload`
    /// in `ast_to_hir/mod.rs`) have no source counterpart to splice onto
    /// and are likewise omitted.
    pub fn lift_items_by_path(&self) -> HashMap<Vec<hir::Symbol>, Item> {
        let mut lifted = Vec::new();
        for item in &self.program.items {
            let Some(path) = self.program.def_paths.get(&item.def_id) else {
                continue;
            };
            let Ok(ast_item) = self.lift_item(item) else {
                continue;
            };
            lifted.push((path.clone(), ast_item));
        }
        let (paths, items): (Vec<_>, Vec<_>) = lifted.into_iter().unzip();
        let items = self
            .reconstruct_closures(items.clone())
            .unwrap_or(items);
        paths.into_iter().zip(items).collect()
    }

    /// For each item (keyed by its own qualified path, same key shape as
    /// [`lift_items_by_path`](Self::lift_items_by_path)), the qualified
    /// paths of every OTHER definition it references — used to compute
    /// which imports a target backend actually needs for spliced-in
    /// content, instead of only ever echoing whatever `use` items
    /// happened to already exist in the source file (`fp-kotlin`'s
    /// `emit_import`). Deliberately just facts (fully-qualified paths),
    /// not a target-specific "is this external" classification — that
    /// judgment belongs in each backend, not here.
    pub fn referenced_paths_by_path(&self) -> HashMap<Vec<hir::Symbol>, Vec<Vec<hir::Symbol>>> {
        let empty_tail_map = HashMap::new();
        let mut result = HashMap::new();
        for item in &self.program.items {
            let Some(path) = self.program.def_paths.get(&item.def_id) else {
                continue;
            };
            let mut work = std::collections::VecDeque::new();
            crate::optimizer::hir::collect_item_refs(item, &empty_tail_map, &mut work);
            let referenced = work
                .into_iter()
                .filter(|def_id| *def_id != item.def_id)
                .filter_map(|def_id| self.program.def_paths.get(&def_id).cloned())
                .collect::<Vec<_>>();
            result.insert(path.clone(), referenced);
        }
        result
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
                                self.lift_type(&field.ty)?,
                            ))
                        })
                        .collect::<Result<Vec<_>>>()?,
                },
            })),
            hir::ItemKind::Enum(def) => Item::from(ItemKind::DefEnum(ItemDefEnum {
                attrs: Vec::new(),
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
            }))
            .with_span(item.span))
        } else {
            let block = function.body.as_ref().expect("checked body presence");
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
                body: self.lift_block(block)?,
                is_async: false,
                visibility: lift_visibility(&item.visibility),
            }))
            .with_span(item.span))
        }
    }

    fn lift_signature(&self, sig: &hir::FunctionSig) -> Result<FunctionSignature> {
        Ok(FunctionSignature {
            name: Some(Ident::new(sig.name.as_str())),
            receiver: None,
            params: sig
                .inputs
                .iter()
                .enumerate()
                .map(|(index, param)| self.lift_param(param, index))
                .collect::<Result<Vec<_>>>()?,
            generics_params: Vec::new(),
            is_const: false,
            abi: lift_abi(&sig.abi),
            quote_kind: None,
            ret_ty: Some(self.lift_type(&sig.output)?),
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
            ty: self.lift_type(&param.ty)?,
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
            hir::ExprKind::Path(path) => Expr::name(Name::path(lift_path(path))),
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
            hir::ExprKind::Reference(reference) => Expr::new(ast::ExprKind::Reference(ExprReference {
                span: expr.span,
                referee: Box::new(self.lift_expr(&reference.expr)?),
                mutable: Some(matches!(reference.mutable, hir::ty::Mutability::Mut)),
            })),
            hir::ExprKind::Call(callee, args) => Expr::new(ast::ExprKind::Invoke(ast::ExprInvoke {
                span: expr.span,
                target: ast::ExprInvokeTarget::expr(self.lift_expr(callee)?),
                args: self.lift_positional_args(args)?,
                kwargs: self.lift_keyword_args(args)?,
            })),
            hir::ExprKind::MethodCall(receiver, name, args) => {
                Expr::new(ast::ExprKind::Invoke(ast::ExprInvoke {
                    span: expr.span,
                    target: ast::ExprInvokeTarget::Method(ExprSelect {
                        span: expr.span,
                        obj: Box::new(self.lift_expr(receiver)?),
                        field: Ident::new(name.as_str()),
                        select: ExprSelectType::Method,
                    }),
                    args: self.lift_positional_args(args)?,
                    kwargs: self.lift_keyword_args(args)?,
                }))
            }
            hir::ExprKind::FieldAccess(base, field) => Expr::new(ast::ExprKind::Select(ExprSelect {
                span: expr.span,
                obj: Box::new(self.lift_expr(base)?),
                field: Ident::new(field.as_str()),
                select: ExprSelectType::Field,
            })),
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
            hir::ExprKind::Struct(path, fields) => Expr::new(ast::ExprKind::Struct(ExprStruct {
                span: expr.span,
                name: Box::new(Expr::path(lift_path(path))),
                fields: fields
                    .iter()
                    .map(|field| {
                        ast::ExprField::new(
                            Ident::new(field.name.as_str()),
                            self.lift_expr(&field.expr).unwrap_or_else(|_| Expr::unit()),
                        )
                    })
                    .collect(),
                update: None,
            })),
            hir::ExprKind::If(cond, then_branch, else_branch) => Expr::new(ast::ExprKind::If(ExprIf {
                span: expr.span,
                cond: Box::new(self.lift_expr(cond)?),
                then: Box::new(self.lift_expr(then_branch)?),
                elze: else_branch
                    .as_ref()
                    .map(|expr| self.lift_expr(expr).map(Box::new))
                    .transpose()?,
            })),
            hir::ExprKind::Match(scrutinee, arms) => Expr::new(ast::ExprKind::Match(ExprMatch {
                span: expr.span,
                scrutinee: Some(Box::new(self.lift_expr(scrutinee)?)),
                cases: arms
                    .iter()
                    .map(|arm| {
                        Ok(ExprMatchCase {
                            span: arm.body.span,
                            pat: Some(Box::new(self.lift_pat(&arm.pat)?)),
                            cond: Box::new(self.lift_expr(scrutinee)?),
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
                    kind: CallKind::Intrinsic(call.kind),
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
            hir::ExprKind::With(context, body) => Expr::new(ast::ExprKind::With(ExprWith {
                span: expr.span,
                context: Box::new(self.lift_expr(context)?),
                body: Box::new(self.lift_expr(body)?),
            })),
            hir::ExprKind::Array(values) => Expr::new(ast::ExprKind::Array(ExprArray {
                span: expr.span,
                values: values.iter().map(|v| self.lift_expr(v)).collect::<Result<Vec<_>>>()?,
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
                values: values.iter().map(|v| self.lift_expr(v)).collect::<Result<Vec<_>>>()?,
            })),
            hir::ExprKind::ConstBlock(const_block) => {
                Expr::new(ast::ExprKind::ConstBlock(ast::ExprConstBlock {
                    span: expr.span,
                    collected_items: Vec::new(),
                    expr: Box::new(self.lift_expr(&const_block.body)?),
                }))
            }
        };
        // Attach the typer's resolved type for this HIR node, if we have
        // typeck results and it resolved to something representable as an
        // `ast::Ty` — see `hir_ty_to_ast`. `None` either way just means the
        // lifted expr's `.ty()` stays `None`, same as before this existed.
        let ty_slot = self
            .typeck
            .and_then(|t| t.expr_types.get(&expr.hir_id))
            .and_then(|ty| self.hir_ty_to_ast(ty));
        Ok(lifted.with_ty_slot(ty_slot).with_span(expr.span))
    }

    fn lift_block(&self, block: &hir::Block) -> Result<ExprBlock> {
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
            collected_items: Vec::new(),
            stmts,
        })
    }

    fn lift_stmt(&self, stmt: &hir::Stmt) -> Result<BlockStmt> {
        match &stmt.kind {
            hir::StmtKind::Local(local) => {
                let pat = self.lift_pat(&local.pat)?;
                // Prefer an explicit source-level annotation (`let x: T = ...`);
                // otherwise fall back to the typer's own resolved binding type
                // (`TypeckResults::pat_types`, keyed by the pattern's `HirId`) —
                // needed for bindings like `let mut x = None;` whose real type
                // is only known once later reassignments/usage are unified,
                // not from the initializer expression alone. Without this,
                // backends (`fp-kotlin`) have to *guess* a var's type from the
                // literal `null` initializer alone and can't.
                let ty_ann = match &local.ty {
                    Some(ty) => Some(self.lift_type(ty)?),
                    None => self
                        .typeck
                        .and_then(|t| t.pat_types.get(&local.pat.hir_id))
                        .and_then(|ty| self.hir_ty_to_ast(ty)),
                };
                let pat = match ty_ann {
                    Some(ty) => Pattern::new(PatternKind::Type(ast::PatternType::new(pat, ty))),
                    None => pat,
                };
                Ok(BlockStmt::Let(ast::StmtLet {
                    pat,
                    init: local.init.as_ref().map(|expr| self.lift_expr(expr)).transpose()?,
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
            hir::PatKind::Binding { name, mutable } => Pattern::new(PatternKind::Ident(PatternIdent {
                ident: Ident::new(name.as_str()),
                mutability: Some(*mutable),
            })),
            hir::PatKind::Tuple(items) => Pattern::new(PatternKind::Tuple(PatternTuple {
                patterns: items.iter().map(|p| self.lift_pat(p)).collect::<Result<Vec<_>>>()?,
            })),
            hir::PatKind::TupleStruct(path, items) => {
                Pattern::new(PatternKind::TupleStruct(PatternTupleStruct {
                    name: Name::path(lift_path(path)),
                    patterns: items.iter().map(|p| self.lift_pat(p)).collect::<Result<Vec<_>>>()?,
                }))
            }
            hir::PatKind::Struct(path, fields, has_rest) => {
                Pattern::new(PatternKind::Struct(PatternStruct {
                    name: Ident::new(
                        path.segments
                            .last()
                            .map(|seg| seg.name.as_str())
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
            hir::PatKind::Variant(path) => Pattern::new(PatternKind::Variant(PatternVariant {
                name: Expr::path(lift_path(path)),
                pattern: None,
            })),
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
            // `Arc<GitBackend>`, ...) live on the path's last `PathSegment.
            // args` — `lift_path` alone drops them (it only carries
            // segment names), which would otherwise let a struct field or
            // parameter's declared type lose its element/wrapped type
            // entirely. Preserve them the same way `hir_ty_to_ast`'s `Adt`
            // case does for resolved types: render as a source-shaped name
            // (`"Vec<Hunk>"`) and let `kotlin_type_from_ty`'s `Ty::Expr`
            // case (`map_name_to_kt`) do the actual Kotlin mapping.
            hir::TypeExprKind::Path(path) => match self.inline_synthetic_struct_ty(path)? {
                Some(ty) => ty,
                None => match self.type_expr_path_source_name(path) {
                    Some(name) => Ty::expr(Expr::name(Name::path(Path::plain(vec![Ident::new(name)])))),
                    None => Ty::path(lift_path(path)),
                },
            },
            hir::TypeExprKind::Tuple(items) => Ty::Tuple(TypeTuple {
                types: items.iter().map(|ty| self.lift_type(ty)).collect::<Result<Vec<_>>>()?,
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
            hir::TypeExprKind::Ptr(inner) => Ty::RawPtr(ast::TypeRawPtr {
                ty: Box::new(self.lift_type(inner)?),
                mutability: None,
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
            hir::TypeExprKind::Never => Ty::Nothing(ast::TypeNothing),
            hir::TypeExprKind::Infer | hir::TypeExprKind::Error => Ty::Unknown(ast::TypeUnknown),
            hir::TypeExprKind::Structural(structural) => Ty::Structural(ast::TypeStructural {
                fields: structural
                    .fields
                    .iter()
                    .map(|field| {
                        Ok(StructuralField::new(Ident::new(field.name.as_str()), self.lift_type(&field.ty)?))
                    })
                    .collect::<Result<Vec<_>>>()?,
            }),
            hir::TypeExprKind::TypeBinaryOp(_) => Ty::Unknown(ast::TypeUnknown),
            hir::TypeExprKind::ConstBlock(body) => Ty::ConstBlock(ast::ExprConstBlock {
                span: ty.span,
                collected_items: Vec::new(),
                expr: Box::new(self.lift_expr(body)?),
            }),
        })
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
        let Some(hir::Res::Def(def_id)) = &path.res else {
            return Ok(None);
        };
        let Some(item) = self.program.def_map.get(def_id) else {
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
            .map(|field| Ok(StructuralField::new(Ident::new(field.name.as_str()), self.lift_type(&field.ty)?)))
            .collect::<Result<Vec<_>>>()?;
        Ok(Some(Ty::Structural(ast::TypeStructural { fields })))
    }

    fn type_expr_path_source_name(&self, path: &hir::Path) -> Option<String> {
        let last = path.segments.last()?;
        let args = last.args.as_ref()?;
        let arg_names: Vec<String> = args
            .args
            .iter()
            .filter_map(|arg| match arg {
                hir::GenericArg::Type(inner) => self.type_expr_source_name(inner),
                hir::GenericArg::Const(_) => None,
            })
            .collect();
        if arg_names.is_empty() {
            None
        } else {
            Some(format!("{}<{}>", last.name.as_str(), arg_names.join(", ")))
        }
    }

    /// Recursive helper for `type_expr_path_source_name` — same
    /// source-shaped-name rendering, for a generic argument position
    /// (which may itself be a further-nested generic type).
    fn type_expr_source_name(&self, ty: &hir::TypeExpr) -> Option<String> {
        match &ty.kind {
            hir::TypeExprKind::Path(path) => match self.type_expr_path_source_name(path) {
                Some(name) => Some(name),
                None => path.segments.last().map(|s| s.name.as_str().to_string()),
            },
            hir::TypeExprKind::Primitive(primitive) => Some(rust_primitive_source_name(primitive)),
            _ => None,
        }
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
    /// resolve through `self.program.def_paths`; anything not resolvable
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
            TyKind::Float(float_ty) => Some(Ty::Primitive(TypePrimitive::Decimal(match float_ty {
                hir::ty::FloatTy::F32 => DecimalType::F32,
                hir::ty::FloatTy::F64 => DecimalType::F64,
            }))),
            TyKind::Never => Some(Ty::Nothing(ast::TypeNothing)),
            TyKind::Tuple(items) => {
                let types: Vec<Ty> = items.iter().filter_map(|t| self.hir_ty_to_ast(t)).collect();
                (types.len() == items.len()).then(|| Ty::Tuple(TypeTuple { types }))
            }
            TyKind::Slice(elem) => self
                .hir_ty_to_ast(elem)
                .map(|elem| Ty::Slice(TypeSlice { elem: Box::new(elem) })),
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
                    let name = self.program.def_paths.get(&adt.did)?.last()?.as_str();
                    Some(Ty::expr(Expr::name(Name::path(Path::plain(vec![Ident::new(
                        format!("{}<{}>", name, args.join(", ")),
                    )])))))
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
                let name = self.program.def_paths.get(&adt.did)?.last()?.as_str();
                let args: Vec<String> = substs
                    .iter()
                    .filter_map(|arg| match arg {
                        hir::ty::GenericArg::Type(t) => self.resolved_ty_source_name(t),
                        _ => None,
                    })
                    .collect();
                if args.is_empty() {
                    Some(name.to_string())
                } else {
                    Some(format!("{}<{}>", name, args.join(", ")))
                }
            }
            TyKind::Ref(_, inner, _) => self.resolved_ty_source_name(inner),
            TyKind::Bool => Some("bool".to_string()),
            TyKind::Char => Some("char".to_string()),
            TyKind::Int(_) | TyKind::Uint(_) => Some("i64".to_string()),
            TyKind::Float(_) => Some("f64".to_string()),
            _ => None,
        }
    }

    fn def_id_to_ty(&self, def_id: &DefId) -> Option<ast::Ty> {
        let path = self.program.def_paths.get(def_id)?;
        if path.segments.is_empty() {
            return None;
        }
        Some(Ty::path(path.to_ast_path()))
    }

    /// After HIR→AST lifting, closures have been lowered to `__Closure{N}`
    /// struct + `__closure{N}_call` function pairs. This pass detects those
    /// pairs, extracts the HIR-typed parameter info from the program, and
    /// reconstructs `ExprClosure` expressions with populated `Pattern.ty` slots.
    fn reconstruct_closures(&self, mut items: Vec<Item>) -> Result<Vec<Item>> {
        let mut closure_types: HashMap<String, Vec<Ty>> = HashMap::new();

        for hir_item in &self.program.items {
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
}

/// Public entry point — unchanged for the two call sites that never run the
/// typer (`fp-backend`'s own roundtrip helpers), and used by the one that
/// does (`fp-cli`'s `typecheck_language_target`, `fp-compiler`'s driver)
/// via `Some(&typeck_results)`.
pub fn lift_program(
    program: &hir::Program,
    typeck: Option<&TypeckResults>,
    path: PathBuf,
) -> Result<ast::File> {
    HirToAstLifter::new(program, typeck).lift_program(path)
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

/// A `fp_core::ast::TypePrimitive` rendered as the Rust-syntax name
/// `map_name_to_kt` (in `fp-kotlin`) expects when it appears nested inside
/// a generic-wrapper source name built by `type_expr_source_name` (e.g.
/// the `i64` in `"Vec<i64>"`).
fn rust_primitive_source_name(primitive: &fp_core::ast::TypePrimitive) -> String {
    use fp_core::ast::{TypeInt, TypePrimitive};
    match primitive {
        TypePrimitive::Bool => "bool".to_string(),
        TypePrimitive::Char => "char".to_string(),
        TypePrimitive::String => "String".to_string(),
        TypePrimitive::Decimal(_) => "f64".to_string(),
        TypePrimitive::List => "Vec".to_string(),
        TypePrimitive::Int(int_ty) => match int_ty {
            TypeInt::I8 => "i8", TypeInt::I16 => "i16", TypeInt::I32 => "i32", TypeInt::I64 => "i64",
            TypeInt::U8 => "u8", TypeInt::U16 => "u16", TypeInt::U32 => "u32", TypeInt::U64 => "u64",
            _ => "i64",
        }
        .to_string(),
    }
}

fn lift_path(path: &hir::Path) -> Path {
    Path::plain(
        path.segments
            .iter()
            .map(|segment| Ident::new(segment.name.as_str()))
            .collect(),
    )
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
                ast::ExprKind::Name(Name::Path(p)) => {
                    p.segments.iter().map(|s| s.name.as_str()).collect::<Vec<_>>().join("::")
                }
                ast::ExprKind::Name(Name::Ident(id)) => id.name.clone(),
                _ => { return; }
            };
            let last_seg = struct_name.rsplit("::").next().unwrap_or(&struct_name);
            if let Some(param_types) = types.get(last_seg) {
                if !param_types.is_empty() {
                    let params: Vec<Pattern> = param_types.iter().enumerate().map(|(i, ty)| {
                        Pattern {
                            id: ast::fresh_pattern_id(),
                            ty: Some(ty.clone()),
                            kind: PatternKind::Ident(PatternIdent {
                                ident: Ident::new(format!("__p{}", i)),
                                mutability: None,
                            }),
                        }
                    }).collect();
                    let span = expr.span;
                    // Replace this struct with a closure — the body is a placeholder
                    expr.kind = ast::ExprKind::Closure(ExprClosure {
                        span: span.unwrap_or_default(),
                        params,
                        ret_ty: None,
                        movability: None,
                        body: Box::new(Expr::unit()),
                    });
                    expr.ty = Some(Ty::unknown());
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
        ast::ExprKind::Let(l) => { recon_closures_in_expr(&mut l.expr, types); }
        ast::ExprKind::Assign(a) => {
            recon_closures_in_expr(&mut a.value, types);
            recon_closures_in_expr(&mut a.target, types);
        }
        ast::ExprKind::Return(r) => {
            if let Some(ref mut v) = r.value { recon_closures_in_expr(v, types); }
        }
        ast::ExprKind::BinOp(bin) => {
            recon_closures_in_expr(&mut bin.lhs, types);
            recon_closures_in_expr(&mut bin.rhs, types);
        }
        ast::ExprKind::UnOp(un) => { recon_closures_in_expr(&mut un.val, types); }
        ast::ExprKind::Select(sel) => { recon_closures_in_expr(&mut sel.obj, types); }
        ast::ExprKind::Index(idx) => {
            recon_closures_in_expr(&mut idx.obj, types);
            recon_closures_in_expr(&mut idx.index, types);
        }
        ast::ExprKind::Closure(cl) => { recon_closures_in_expr(&mut cl.body, types); }
        ast::ExprKind::Cast(c) => { recon_closures_in_expr(&mut c.expr, types); }
        ast::ExprKind::Reference(r) => { recon_closures_in_expr(&mut r.referee, types); }
        ast::ExprKind::While(wh) => {
            recon_closures_in_expr(&mut wh.cond, types);
            recon_closures_in_expr(&mut wh.body, types);
        }
        ast::ExprKind::For(fr) => {
            recon_closures_in_expr(&mut fr.iter, types);
            recon_closures_in_expr(&mut fr.body, types);
        }
        ast::ExprKind::Loop(lp) => { recon_closures_in_expr(&mut lp.body, types); }
        ast::ExprKind::Try(tr) => {
            recon_closures_in_expr(&mut tr.expr, types);
            for catch in &mut tr.catches {
                recon_closures_in_expr(&mut catch.body, types);
            }
        }
        ast::ExprKind::Array(arr) => {
            for val in &mut arr.values { recon_closures_in_expr(val, types); }
        }
        ast::ExprKind::Tuple(tup) => {
            for val in &mut tup.values { recon_closures_in_expr(val, types); }
        }
        _ => {}
    }
}
