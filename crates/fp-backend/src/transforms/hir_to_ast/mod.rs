use std::path::PathBuf;

use fp_core::ast::{
    self, BlockStmt, BlockStmtExpr, Expr, ExprArray, ExprAssign, ExprBinOp, ExprBlock, ExprBreak,
    ExprCast, ExprContinue, ExprIf, ExprIndex, ExprIntrinsicCall, ExprKwArg, ExprLet, ExprLoop,
    ExprMatch, ExprMatchCase, ExprReference, ExprReturn, ExprSelect, ExprSelectType,
    ExprStringTemplate, ExprStruct, ExprTry, ExprTryCatch, ExprUnOp, ExprWhile, ExprWith,
    FunctionParam, FunctionSignature, Ident, Item, ItemDeclFunction, ItemDefConst, ItemDefEnum,
    ItemDefFunction, ItemDefStruct, ItemKind, Name, Path, Pattern, PatternIdent, PatternKind,
    PatternStruct, PatternStructField, PatternTuple, PatternTupleStruct, PatternVariant,
    StructuralField, Ty, TypeArray, TypeEnum, TypeFunction, TypeReference, TypeSlice, TypeStruct,
    TypeTuple, Value,
};
use fp_core::error::Result;
use fp_core::hir;
use fp_core::ops::{BinOpKind, UnOpKind};
use fp_core::span::Span;

pub fn lift_program(program: &hir::Program, path: PathBuf) -> Result<ast::Node> {
    let mut items = Vec::with_capacity(program.items.len());
    for item in &program.items {
        items.push(lift_item(item)?);
    }
    Ok(ast::Node::file(ast::File {
        path,
        attrs: Vec::new(),
        items,
    }))
}

fn lift_item(item: &hir::Item) -> Result<Item> {
    let lifted = match &item.kind {
        hir::ItemKind::Function(function) => lift_function_item(item, function)?,
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
                        StructuralField::new(Ident::new(field.name.as_str()), lift_type(&field.ty))
                    })
                    .collect(),
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
                                .map(lift_type)
                                .unwrap_or_else(Ty::unit),
                            discriminant: variant
                                .discriminant
                                .as_ref()
                                .map(|expr| lift_expr(expr).map(Box::new))
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
            ty: Some(lift_type(&def.ty)),
            value: Box::new(lift_body_value(&def.body.value)?),
        })),
        hir::ItemKind::Impl(_) => {
            return Err(fp_core::error::Error::Generic(eyre::eyre!(
                "HIR->AST lifting for impl items is not implemented"
            )))
        }
        hir::ItemKind::Expr(expr) => Item::from(ItemKind::Expr(lift_expr(expr)?)),
    };
    Ok(lifted.with_span(item.span))
}

fn lift_function_item(item: &hir::Item, function: &hir::Function) -> Result<Item> {
    let sig = lift_signature(&function.sig);
    if function.is_extern || function.body.is_none() {
        Ok(Item::from(ItemKind::DeclFunction(ItemDeclFunction {
            attrs: function.attrs.clone(),
            ty_annotation: None,
            name: Ident::new(function.sig.name.as_str()),
            sig,
        }))
        .with_span(item.span))
    } else {
        Ok(Item::from(ItemKind::DefFunction(ItemDefFunction {
            ty_annotation: None,
            attrs: function.attrs.clone(),
            name: Ident::new(function.sig.name.as_str()),
            ty: Some(TypeFunction {
                params: function
                    .sig
                    .inputs
                    .iter()
                    .map(|param| lift_type(&param.ty))
                    .collect(),
                generics_params: Vec::new(),
                ret_ty: Some(Box::new(lift_type(&function.sig.output))),
            }),
            sig,
            body: Box::new(lift_body_value(
                &function.body.as_ref().expect("checked body presence").value,
            )?),
            visibility: lift_visibility(&item.visibility),
        }))
        .with_span(item.span))
    }
}

fn lift_signature(sig: &hir::FunctionSig) -> FunctionSignature {
    FunctionSignature {
        name: Some(Ident::new(sig.name.as_str())),
        receiver: None,
        params: sig
            .inputs
            .iter()
            .enumerate()
            .map(|(index, param)| lift_param(param, index))
            .collect(),
        generics_params: Vec::new(),
        is_const: false,
        abi: lift_abi(&sig.abi),
        quote_kind: None,
        ret_ty: Some(lift_type(&sig.output)),
    }
}

fn lift_param(param: &hir::Param, index: usize) -> FunctionParam {
    let name = match &param.pat.kind {
        hir::PatKind::Binding { name, .. } => Ident::new(name.as_str()),
        _ => Ident::new(format!("arg{index}")),
    };
    FunctionParam {
        ty_annotation: None,
        name,
        ty: lift_type(&param.ty),
        is_const: false,
        is_context: param.is_context,
        default: param
            .default
            .as_ref()
            .map(|expr| Value::expr(lift_expr(expr).unwrap_or_else(|_| Expr::unit()))),
        as_tuple: false,
        as_dict: false,
        positional_only: false,
        keyword_only: false,
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

fn lift_body_value(expr: &hir::Expr) -> Result<Expr> {
    lift_expr(expr)
}

fn lift_expr(expr: &hir::Expr) -> Result<Expr> {
    let lifted = match &expr.kind {
        hir::ExprKind::Literal(lit) => Expr::value(match lit {
            hir::Lit::Bool(v) => Value::bool(*v),
            hir::Lit::Integer(v) => Value::int(*v),
            hir::Lit::Float(v) => Value::decimal(*v),
            hir::Lit::Str(v) => Value::string(v.clone()),
            hir::Lit::Char(v) => Value::Char(ast::ValueChar::new(*v)),
            hir::Lit::Null => Value::null(),
        }),
        hir::ExprKind::Path(path) => Expr::name(Name::path(lift_path(path))),
        hir::ExprKind::Binary(op, lhs, rhs) => Expr::new(ast::ExprKind::BinOp(ExprBinOp {
            span: expr.span,
            kind: lift_binop(op),
            lhs: Box::new(lift_expr(lhs)?),
            rhs: Box::new(lift_expr(rhs)?),
        })),
        hir::ExprKind::Unary(op, value) => Expr::new(ast::ExprKind::UnOp(ExprUnOp {
            span: expr.span,
            op: lift_unop(op),
            val: Box::new(lift_expr(value)?),
        })),
        hir::ExprKind::Reference(reference) => Expr::new(ast::ExprKind::Reference(ExprReference {
            span: expr.span,
            referee: Box::new(lift_expr(&reference.expr)?),
            mutable: Some(matches!(reference.mutable, hir::ty::Mutability::Mut)),
        })),
        hir::ExprKind::Call(callee, args) => Expr::new(ast::ExprKind::Invoke(ast::ExprInvoke {
            span: expr.span,
            target: ast::ExprInvokeTarget::expr(lift_expr(callee)?),
            args: lift_positional_args(args)?,
            kwargs: lift_keyword_args(args)?,
        })),
        hir::ExprKind::MethodCall(receiver, name, args) => {
            Expr::new(ast::ExprKind::Invoke(ast::ExprInvoke {
                span: expr.span,
                target: ast::ExprInvokeTarget::Method(ExprSelect {
                    span: expr.span,
                    obj: Box::new(lift_expr(receiver)?),
                    field: Ident::new(name.as_str()),
                    select: ExprSelectType::Method,
                }),
                args: lift_positional_args(args)?,
                kwargs: lift_keyword_args(args)?,
            }))
        }
        hir::ExprKind::FieldAccess(base, field) => Expr::new(ast::ExprKind::Select(ExprSelect {
            span: expr.span,
            obj: Box::new(lift_expr(base)?),
            field: Ident::new(field.as_str()),
            select: ExprSelectType::Field,
        })),
        hir::ExprKind::Index(base, index) => Expr::new(ast::ExprKind::Index(ExprIndex {
            span: expr.span,
            obj: Box::new(lift_expr(base)?),
            index: Box::new(lift_expr(index)?),
        })),
        hir::ExprKind::Slice(slice) => {
            let range = Expr::new(ast::ExprKind::Range(ast::ExprRange {
                span: expr.span,
                start: slice
                    .start
                    .as_ref()
                    .map(|expr| lift_expr(expr.as_ref()).map(Box::new))
                    .transpose()?,
                limit: if slice.inclusive {
                    ast::ExprRangeLimit::Inclusive
                } else {
                    ast::ExprRangeLimit::Exclusive
                },
                end: slice
                    .end
                    .as_ref()
                    .map(|expr| lift_expr(expr.as_ref()).map(Box::new))
                    .transpose()?,
                step: None,
            }));
            Expr::new(ast::ExprKind::Index(ExprIndex {
                span: expr.span,
                obj: Box::new(lift_expr(&slice.base)?),
                index: Box::new(range),
            }))
        }
        hir::ExprKind::Cast(value, ty) => Expr::new(ast::ExprKind::Cast(ExprCast {
            span: expr.span,
            expr: Box::new(lift_expr(value)?),
            ty: lift_type(ty),
        })),
        hir::ExprKind::Struct(path, fields) => Expr::new(ast::ExprKind::Struct(ExprStruct {
            span: expr.span,
            name: Box::new(Expr::path(lift_path(path))),
            fields: fields
                .iter()
                .map(|field| {
                    ast::ExprField::new(
                        Ident::new(field.name.as_str()),
                        lift_expr(&field.expr).unwrap_or_else(|_| Expr::unit()),
                    )
                })
                .collect(),
            update: None,
        })),
        hir::ExprKind::If(cond, then_branch, else_branch) => Expr::new(ast::ExprKind::If(ExprIf {
            span: expr.span,
            cond: Box::new(lift_expr(cond)?),
            then: Box::new(lift_expr(then_branch)?),
            elze: else_branch
                .as_ref()
                .map(|expr| lift_expr(expr).map(Box::new))
                .transpose()?,
        })),
        hir::ExprKind::Match(scrutinee, arms) => Expr::new(ast::ExprKind::Match(ExprMatch {
            span: expr.span,
            scrutinee: Some(Box::new(lift_expr(scrutinee)?)),
            cases: arms
                .iter()
                .map(|arm| {
                    Ok(ExprMatchCase {
                        span: arm.body.span,
                        pat: Some(Box::new(lift_pat(&arm.pat)?)),
                        cond: Box::new(lift_expr(scrutinee)?),
                        guard: arm
                            .guard
                            .as_ref()
                            .map(|expr| lift_expr(expr).map(Box::new))
                            .transpose()?,
                        body: Box::new(lift_expr(&arm.body)?),
                    })
                })
                .collect::<Result<Vec<_>>>()?,
        })),
        hir::ExprKind::Try(expr_try) => Expr::new(ast::ExprKind::Try(ExprTry {
            span: expr.span,
            expr: Box::new(lift_expr(&expr_try.expr)?),
            catches: expr_try
                .catches
                .iter()
                .map(|catch| {
                    Ok(ExprTryCatch {
                        span: catch.body.span,
                        pat: catch
                            .pat
                            .as_ref()
                            .map(|pat| lift_pat(pat).map(Box::new))
                            .transpose()?,
                        body: Box::new(lift_expr(&catch.body)?),
                    })
                })
                .collect::<Result<Vec<_>>>()?,
            elze: expr_try
                .elze
                .as_ref()
                .map(|expr| lift_expr(expr).map(Box::new))
                .transpose()?,
            finally: expr_try
                .finally
                .as_ref()
                .map(|expr| lift_expr(expr).map(Box::new))
                .transpose()?,
        })),
        hir::ExprKind::Block(block) => Expr::new(ast::ExprKind::Block(lift_block(block)?)),
        hir::ExprKind::IntrinsicCall(call) => {
            Expr::new(ast::ExprKind::IntrinsicCall(ExprIntrinsicCall {
                span: expr.span,
                kind: call.kind,
                args: lift_positional_args(&call.callargs)?,
                kwargs: lift_keyword_args(&call.callargs)?,
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
            pat: Box::new(lift_pat(pat)?),
            expr: Box::new(
                value
                    .as_deref()
                    .map(lift_expr)
                    .transpose()?
                    .unwrap_or_else(Expr::unit),
            ),
        })),
        hir::ExprKind::Assign(lhs, rhs) => Expr::new(ast::ExprKind::Assign(ExprAssign {
            span: expr.span,
            target: Box::new(lift_expr(lhs)?),
            value: Box::new(lift_expr(rhs)?),
        })),
        hir::ExprKind::Return(value) => Expr::new(ast::ExprKind::Return(ExprReturn {
            span: expr.span,
            value: value
                .as_ref()
                .map(|expr| lift_expr(expr).map(Box::new))
                .transpose()?,
        })),
        hir::ExprKind::Break(value) => Expr::new(ast::ExprKind::Break(ExprBreak {
            span: expr.span,
            value: value
                .as_ref()
                .map(|expr| lift_expr(expr).map(Box::new))
                .transpose()?,
        })),
        hir::ExprKind::Continue => {
            Expr::new(ast::ExprKind::Continue(ExprContinue { span: expr.span }))
        }
        hir::ExprKind::Loop(block) => Expr::new(ast::ExprKind::Loop(ExprLoop {
            span: expr.span,
            label: None,
            body: Box::new(Expr::new(ast::ExprKind::Block(lift_block(block)?))),
        })),
        hir::ExprKind::While(cond, block) => Expr::new(ast::ExprKind::While(ExprWhile {
            span: expr.span,
            cond: Box::new(lift_expr(cond)?),
            body: Box::new(Expr::new(ast::ExprKind::Block(lift_block(block)?))),
        })),
        hir::ExprKind::With(context, body) => Expr::new(ast::ExprKind::With(ExprWith {
            span: expr.span,
            context: Box::new(lift_expr(context)?),
            body: Box::new(lift_expr(body)?),
        })),
        hir::ExprKind::Array(values) => Expr::new(ast::ExprKind::Array(ExprArray {
            span: expr.span,
            values: values.iter().map(lift_expr).collect::<Result<Vec<_>>>()?,
        })),
        hir::ExprKind::ArrayRepeat { elem, len } => {
            Expr::new(ast::ExprKind::ArrayRepeat(ast::ExprArrayRepeat {
                span: expr.span,
                elem: Box::new(lift_expr(elem)?),
                len: Box::new(lift_expr(len)?),
            }))
        }
    };
    Ok(lifted.with_span(expr.span))
}

fn lift_block(block: &hir::Block) -> Result<ExprBlock> {
    let mut stmts = Vec::with_capacity(block.stmts.len() + usize::from(block.expr.is_some()));
    for stmt in &block.stmts {
        stmts.push(lift_stmt(stmt)?);
    }
    if let Some(expr) = &block.expr {
        stmts.push(BlockStmt::Expr(
            BlockStmtExpr::new(lift_expr(expr)?).with_semicolon(false),
        ));
    }
    Ok(ExprBlock {
        span: Span::null(),
        stmts,
    })
}

fn lift_stmt(stmt: &hir::Stmt) -> Result<BlockStmt> {
    match &stmt.kind {
        hir::StmtKind::Local(local) => Ok(BlockStmt::Let(ast::StmtLet {
            pat: lift_pat(&local.pat)?,
            init: local.init.as_ref().map(lift_expr).transpose()?,
            diverge: None,
        })),
        hir::StmtKind::Item(item) => Ok(BlockStmt::Item(Box::new(lift_item(item)?))),
        hir::StmtKind::Expr(expr) => Ok(BlockStmt::Expr(
            BlockStmtExpr::new(lift_expr(expr)?).with_semicolon(false),
        )),
        hir::StmtKind::Semi(expr) => Ok(BlockStmt::Expr(
            BlockStmtExpr::new(lift_expr(expr)?).with_semicolon(true),
        )),
    }
}

fn lift_positional_args(args: &[hir::CallArg]) -> Result<Vec<Expr>> {
    args.iter().map(|arg| lift_expr(&arg.value)).collect()
}

fn lift_keyword_args(args: &[hir::CallArg]) -> Result<Vec<ExprKwArg>> {
    args.iter()
        .filter(|arg| !matches!(arg.name.as_str().strip_prefix("arg"), Some(suffix) if suffix.parse::<usize>().is_ok()))
        .map(|arg| {
            Ok(ExprKwArg {
                name: arg.name.as_str().to_string(),
                value: lift_expr(&arg.value)?,
            })
        })
        .collect()
}

fn lift_pat(pat: &hir::Pat) -> Result<Pattern> {
    Ok(match &pat.kind {
        hir::PatKind::Wild => Pattern::new(PatternKind::Wildcard(ast::PatternWildcard {})),
        hir::PatKind::Binding { name, mutable } => Pattern::new(PatternKind::Ident(PatternIdent {
            ident: Ident::new(name.as_str()),
            mutability: Some(*mutable),
        })),
        hir::PatKind::Tuple(items) => Pattern::new(PatternKind::Tuple(PatternTuple {
            patterns: items.iter().map(lift_pat).collect::<Result<Vec<_>>>()?,
        })),
        hir::PatKind::TupleStruct(path, items) => {
            Pattern::new(PatternKind::TupleStruct(PatternTupleStruct {
                name: Name::path(lift_path(path)),
                patterns: items.iter().map(lift_pat).collect::<Result<Vec<_>>>()?,
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
                            rename: Some(Box::new(lift_pat(&field.pat)?)),
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
            }),
            pattern: None,
        })),
    })
}

fn lift_type(ty: &hir::TypeExpr) -> Ty {
    match &ty.kind {
        hir::TypeExprKind::Primitive(primitive) => Ty::Primitive(*primitive),
        hir::TypeExprKind::Path(path) => Ty::path(lift_path(path)),
        hir::TypeExprKind::Tuple(items) => Ty::Tuple(TypeTuple {
            types: items.iter().map(|ty| lift_type(ty)).collect(),
        }),
        hir::TypeExprKind::Array(elem, Some(len)) => Ty::Array(TypeArray {
            elem: Box::new(lift_type(elem)),
            len: Box::new(lift_expr(len).unwrap_or_else(|_| Expr::value(Value::int(0)))),
        }),
        hir::TypeExprKind::Array(elem, None) => Ty::Vec(ast::TypeVec {
            ty: Box::new(lift_type(elem)),
        }),
        hir::TypeExprKind::Slice(elem) => Ty::Slice(TypeSlice {
            elem: Box::new(lift_type(elem)),
        }),
        hir::TypeExprKind::Ref(inner) => Ty::Reference(TypeReference {
            ty: Box::new(lift_type(inner)),
            mutability: None,
            lifetime: None,
        }),
        hir::TypeExprKind::Ptr(inner) => Ty::RawPtr(ast::TypeRawPtr {
            ty: Box::new(lift_type(inner)),
            mutability: None,
        }),
        hir::TypeExprKind::FnPtr(function) => Ty::Function(TypeFunction {
            params: function
                .inputs
                .iter()
                .map(|param| lift_type(param))
                .collect(),
            generics_params: Vec::new(),
            ret_ty: Some(Box::new(lift_type(&function.output))),
        }),
        hir::TypeExprKind::Never => Ty::Nothing(ast::TypeNothing),
        hir::TypeExprKind::Infer | hir::TypeExprKind::Error => Ty::Unknown(ast::TypeUnknown),
        hir::TypeExprKind::Structural(structural) => Ty::Structural(ast::TypeStructural {
            fields: structural
                .fields
                .iter()
                .map(|field| {
                    StructuralField::new(Ident::new(field.name.as_str()), lift_type(&field.ty))
                })
                .collect(),
        }),
        hir::TypeExprKind::TypeBinaryOp(_) => Ty::Unknown(ast::TypeUnknown),
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
