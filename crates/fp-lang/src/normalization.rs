use fp_core::ast::{
    BlockStmt, BlockStmtExpr, Expr, ExprBinOp, ExprBlock, ExprIf, ExprIntrinsicCall,
    ExprIntrinsicContainer, ExprInvoke,
    ExprInvokeTarget, ExprKind, ExprStringTemplate, ExprUnOp, FormatArgRef, FormatPlaceholder,
    FormatSpec, FormatTemplatePart, Ident, MacroTokenTree, Name, Path, StmtLet, Ty, Value,
};
use fp_core::error::Result;
use fp_core::intrinsics::{
    CallKind, IntrinsicKind, IntrinsicNormalizationMode, IntrinsicNormalizer, NormalizeOutcome,
    OpKind,
};
use fp_core::ops::{BinOpKind, UnOpKind};
use fp_core::span::Span;

use crate::ast::lower_common::{macro_token_trees_to_lexemes, macro_tokens_file_id};
use crate::lexer::lexeme::LexemeKind;
use crate::macro_parser::{
    macro_token_trees_to_tokens, tokens_to_top_level_slices, wrap_tokens_in_group,
};

/// FerroPhase intrinsic normalizer that adds `t!` macro lowering for type expressions,
/// delegating all other macros to the Rust normalizer.
#[derive(Debug, Clone, Copy)]
pub struct FerroIntrinsicNormalizer {
    mode: IntrinsicNormalizationMode,
}

impl Default for FerroIntrinsicNormalizer {
    fn default() -> Self {
        Self::new(IntrinsicNormalizationMode::Transpile)
    }
}

impl FerroIntrinsicNormalizer {
    pub const fn new(mode: IntrinsicNormalizationMode) -> Self {
        Self { mode }
    }
}

impl IntrinsicNormalizer for FerroIntrinsicNormalizer {
    fn normalize_call(&self, expr: Expr) -> Result<NormalizeOutcome<Expr>> {
        let (id, ty_slot, span, kind) = expr.into_parts();
        let ExprKind::IntrinsicCall(call) = kind else {
            return Ok(NormalizeOutcome::Ignored(Expr::from_parts(
                id, ty_slot, span, kind,
            )));
        };
        let CallKind::Op(op) = call.kind else {
            return Ok(NormalizeOutcome::Ignored(Expr::from_parts(
                id,
                ty_slot,
                span,
                ExprKind::IntrinsicCall(call),
            )));
        };
        match self.mode {
            IntrinsicNormalizationMode::Compile => {
                let (args, kwargs) = if matches!(op, OpKind::Print | OpKind::Println) {
                    let (template, skip) = build_print_template_from_args(&call.args)?;
                    let mut args = Vec::with_capacity(1 + call.args.len().saturating_sub(skip));
                    args.push(Expr::new(ExprKind::FormatString(template)));
                    args.extend(call.args[skip..].iter().cloned());
                    (args, call.kwargs)
                } else {
                    (call.args, call.kwargs)
                };
                let replacement = match compile_mode_std_path(op) {
                    Some(path) => ExprKind::Invoke(ExprInvoke {
                        span: call.span,
                        target: ExprInvokeTarget::Function(Name::path(Path::plain(path))),
                        args,
                        kwargs,
                    }),
                    None => ExprKind::IntrinsicCall(ExprIntrinsicCall::new(
                        CallKind::Intrinsic(call.kind.intrinsic_kind().expect("operation mapping")),
                        args,
                        kwargs,
                    )),
                };
                Ok(NormalizeOutcome::Normalized(Expr::from_parts(
                    id,
                    ty_slot,
                    span,
                    replacement,
                )))
            }
            IntrinsicNormalizationMode::Transpile => Ok(NormalizeOutcome::Ignored(
                Expr::from_parts(id, ty_slot, span, ExprKind::IntrinsicCall(call)),
            )),
        }
    }

    fn normalize_macro(&self, expr: Expr) -> Result<NormalizeOutcome<Expr>> {
        let (id, ty_slot, span, kind) = expr.into_parts();
        let ExprKind::Macro(macro_expr) = kind else {
            return Ok(NormalizeOutcome::Ignored(Expr::from_parts(
                id, ty_slot, span, kind,
            )));
        };

        if let Some(name) = macro_expr.invocation.path.segments.last() {
            let macro_name = name.as_str().trim_end_matches('!');
            if macro_name == "t" {
                if let Ok(ty) = parse_type_macro_tokens(&macro_expr.invocation.token_trees) {
                    let replacement = Expr::value(Value::Type(ty)).with_ty_slot(ty_slot);
                    return Ok(NormalizeOutcome::Normalized(replacement));
                }
                return Ok(NormalizeOutcome::Ignored(Expr::from_parts(
                    id,
                    ty_slot,
                    span,
                    ExprKind::Macro(macro_expr),
                )));
            }
            if macro_name == "vec" {
                let expr =
                    parse_vec_macro_tokens(&macro_expr.invocation.token_trees, macro_expr.span())?;
                let invoke = ExprInvoke {
                    target: ExprInvokeTarget::Function(Name::path(Path::plain(vec![
                        Ident::new("Vec"),
                        Ident::new("from"),
                    ]))),
                    args: vec![expr],
                    kwargs: vec![],
                    span: macro_expr.span(),
                };
                let replacement = Expr::from(ExprKind::Invoke(invoke)).with_ty_slot(ty_slot);
                return Ok(NormalizeOutcome::Normalized(replacement));
            }
            if macro_name == "assert" {
                let args = parse_expr_macro_tokens(&macro_expr.invocation.token_trees)?;
                if args.is_empty() {
                    return Err(fp_core::error::Error::from(
                        "assert! requires at least one argument",
                    ));
                }
                let mut iter = args.into_iter();
                let cond = iter.next().unwrap();
                let panic_expr = if iter.len() == 0 {
                    panic_call_with_message("assertion failed")
                } else {
                    panic_call_from_args(iter.collect())
                };
                let replacement = assert_macro_with_panic(cond, panic_expr).with_ty_slot(ty_slot);
                return Ok(NormalizeOutcome::Normalized(replacement));
            }
            if macro_name == "assert_eq" {
                let args = parse_expr_macro_tokens(&macro_expr.invocation.token_trees)?;
                if args.len() < 2 {
                    return Err(fp_core::error::Error::from(
                        "assert_eq! requires at least two arguments",
                    ));
                }
                let mut iter = args.into_iter();
                let left = iter.next().unwrap();
                let right = iter.next().unwrap();
                let replacement = if iter.len() == 0 {
                    assert_compare_macro(
                        left,
                        right,
                        BinOpKind::Eq,
                        "assertion failed: left != right",
                    )
                } else {
                    let panic_expr = panic_call_from_args(iter.collect());
                    assert_compare_macro_with_panic(left, right, BinOpKind::Eq, panic_expr)
                }
                .with_ty_slot(ty_slot);
                return Ok(NormalizeOutcome::Normalized(replacement));
            }
            if macro_name == "assert_ne" {
                let args = parse_expr_macro_tokens(&macro_expr.invocation.token_trees)?;
                if args.len() < 2 {
                    return Err(fp_core::error::Error::from(
                        "assert_ne! requires at least two arguments",
                    ));
                }
                let mut iter = args.into_iter();
                let left = iter.next().unwrap();
                let right = iter.next().unwrap();
                let replacement = if iter.len() == 0 {
                    assert_compare_macro(
                        left,
                        right,
                        BinOpKind::Ne,
                        "assertion failed: left == right",
                    )
                } else {
                    let panic_expr = panic_call_from_args(iter.collect());
                    assert_compare_macro_with_panic(left, right, BinOpKind::Ne, panic_expr)
                }
                .with_ty_slot(ty_slot);
                return Ok(NormalizeOutcome::Normalized(replacement));
            }
            if macro_name == "panic" {
                let args = parse_expr_macro_tokens(&macro_expr.invocation.token_trees)?;
                let replacement = panic_macro(args).with_ty_slot(ty_slot);
                return Ok(NormalizeOutcome::Normalized(replacement));
            }
            if macro_name == "format" {
                let args = parse_expr_macro_tokens(&macro_expr.invocation.token_trees)?;
                if args.is_empty() {
                    return Err(fp_core::error::Error::from(
                        "format! requires at least one argument",
                    ));
                }
                let template = match args[0].kind() {
                    ExprKind::Value(value) => match value.as_ref() {
                        Value::String(string) => {
                            let parts = parse_format_template(&string.value)?;
                            ExprStringTemplate { parts }
                        }
                        _ => {
                            return Err(fp_core::error::Error::from(
                                "format! expects a string literal as the first argument",
                            ));
                        }
                    },
                    ExprKind::FormatString(format) => ExprStringTemplate {
                        parts: format.parts.clone(),
                    },
                    _ => {
                        return Err(fp_core::error::Error::from(
                            "format! expects a string literal as the first argument",
                        ));
                    }
                };

                let mut call_args = Vec::with_capacity(args.len());
                call_args.push(Expr::new(ExprKind::FormatString(template)));
                call_args.extend(args[1..].iter().cloned());
                let replacement = Expr::from_parts(
                    id,
                    ty_slot.clone(),
                    span,
                    ExprKind::IntrinsicCall(ExprIntrinsicCall::new(
                        CallKind::Op(OpKind::Format),
                        call_args,
                        Vec::new(),
                    )),
                );
                return Ok(NormalizeOutcome::Normalized(replacement));
            }
            if macro_name == "type_of" || macro_name == "typeof" {
                let args = parse_expr_macro_tokens(&macro_expr.invocation.token_trees)?;
                if args.len() != 1 {
                    return Err(fp_core::error::Error::from(
                        "type_of! requires exactly one argument",
                    ));
                }
                let replacement = Expr::from_parts(
                    id,
                    ty_slot.clone(),
                    span,
                    ExprKind::IntrinsicCall(ExprIntrinsicCall::new(
                        CallKind::Intrinsic(IntrinsicKind::TypeOf),
                        args,
                        Vec::new(),
                    )),
                );
                return Ok(NormalizeOutcome::Normalized(replacement));
            }
            if macro_name == "print" || macro_name == "println" {
                let args = parse_expr_macro_tokens(&macro_expr.invocation.token_trees)?;
                let kind = if macro_name == "println" {
                    CallKind::Op(OpKind::Println)
                } else {
                    CallKind::Op(OpKind::Print)
                };
                let (template, skip) = build_print_template_from_args(&args)?;
                let mut call_args = Vec::with_capacity(1 + args.len().saturating_sub(skip));
                call_args.push(Expr::new(ExprKind::FormatString(template)));
                call_args.extend(args[skip..].iter().cloned());
                let replacement = Expr::from_parts(
                    id,
                    ty_slot.clone(),
                    span,
                    ExprKind::IntrinsicCall(ExprIntrinsicCall::new(kind, call_args, Vec::new())),
                );
                return Ok(NormalizeOutcome::Normalized(replacement));
            }
            if let Some(kind) = intrinsic_macro_kind(macro_name) {
                let args = parse_expr_macro_tokens(&macro_expr.invocation.token_trees)?;
                let replacement = Expr::from_parts(
                    id,
                    ty_slot.clone(),
                    span,
                    ExprKind::IntrinsicCall(ExprIntrinsicCall::new(kind, args, Vec::new())),
                );
                return Ok(NormalizeOutcome::Normalized(replacement));
            }
        }

        Ok(NormalizeOutcome::Ignored(Expr::from_parts(
            id,
            ty_slot,
            span,
            ExprKind::Macro(macro_expr),
        )))
    }

    fn normalize_invoke(&self, expr: Expr) -> Result<NormalizeOutcome<Expr>> {
        let (id, ty_slot, span, kind) = expr.into_parts();
        let ExprKind::Invoke(invoke) = kind else {
            return Ok(NormalizeOutcome::Ignored(Expr::from_parts(
                id, ty_slot, span, kind,
            )));
        };

        // Keep the exact language-item registry as the source of truth for
        // operations exposed by loaded std modules. This also preserves the
        // call-specific argument shaping used by print and filesystem ops.
        if let Some(call) = fp_core::ast::intrinsic_call_from_invoke(&invoke) {
            return self.normalize_call(Expr::from_parts(
                id,
                ty_slot,
                span,
                ExprKind::IntrinsicCall(call),
            ));
        }

        let Some(intrinsic_kind) = resolve_lang_intrinsic(&invoke) else {
            return Ok(NormalizeOutcome::Ignored(Expr::from_parts(
                id,
                ty_slot,
                span,
                ExprKind::Invoke(invoke),
            )));
        };

        match self.mode {
            IntrinsicNormalizationMode::Compile => match intrinsic_kind {
                CallKind::Op(op) => match compile_mode_std_path(op) {
                    Some(path) => Ok(NormalizeOutcome::Normalized(Expr::from_parts(
                        id,
                        ty_slot,
                        span,
                        ExprKind::Invoke(ExprInvoke {
                            span: invoke.span,
                            target: ExprInvokeTarget::Function(Name::path(Path::plain(path))),
                            args: invoke.args,
                            kwargs: invoke.kwargs,
                        }),
                    ))),
                    None => Ok(NormalizeOutcome::Normalized(Expr::from_parts(
                        id,
                        ty_slot,
                        span,
                        ExprKind::IntrinsicCall(ExprIntrinsicCall::new(
                            CallKind::Intrinsic(
                                CallKind::Op(op)
                                    .intrinsic_kind()
                                    .expect("operation mapping"),
                            ),
                            invoke.args,
                            invoke.kwargs,
                        )),
                    ))),
                },
                CallKind::Intrinsic(kind) => Ok(NormalizeOutcome::Normalized(Expr::from_parts(
                    id,
                    ty_slot,
                    span,
                    ExprKind::IntrinsicCall(ExprIntrinsicCall::new(
                        CallKind::Intrinsic(kind),
                        invoke.args,
                        invoke.kwargs,
                    )),
                ))),
            },
            IntrinsicNormalizationMode::Transpile => match intrinsic_kind {
                CallKind::Op(OpKind::OptionSome) => {
                    Ok(NormalizeOutcome::Normalized(
                        invoke.args.first().cloned().unwrap_or_else(|| {
                            Expr::from_parts(0, None, Some(Span::default()),
                                ExprKind::Value(Box::new(Value::Null(Default::default()))))
                        })
                    ))
                }
                CallKind::Op(OpKind::OptionNone) => {
                    Ok(NormalizeOutcome::Normalized(Expr::from_parts(
                        id, ty_slot, span,
                        ExprKind::Value(Box::new(Value::Null(Default::default()))),
                    )))
                }
                CallKind::Op(OpKind::OptionUnwrap) => {
                    Ok(NormalizeOutcome::Normalized(
                        invoke.args.first().cloned().unwrap_or_else(|| {
                            Expr::from_parts(0, None, Some(Span::default()),
                                ExprKind::Value(Box::new(Value::Null(Default::default()))))
                        })
                    ))
                }
                CallKind::Op(OpKind::VecNew) => {
                    Ok(NormalizeOutcome::Normalized(Expr::from_parts(
                        id, ty_slot, span,
                        ExprKind::IntrinsicContainer(
                            ExprIntrinsicContainer::VecElements { elements: vec![] }
                        ),
                    )))
                }
                CallKind::Op(OpKind::Clone) => {
                    Ok(NormalizeOutcome::Normalized(
                        invoke.args.first().cloned().unwrap_or_else(|| {
                            Expr::from_parts(0, None, Some(Span::default()),
                                ExprKind::Value(Box::new(Value::Null(Default::default()))))
                        })
                    ))
                }
                _ => {
                    Ok(NormalizeOutcome::Normalized(Expr::from_parts(
                        id, ty_slot, span,
                        ExprKind::IntrinsicCall(ExprIntrinsicCall::new(
                            intrinsic_kind, invoke.args, invoke.kwargs,
                        )),
                    )))
                }
            },
        }
    }
}

fn compile_mode_std_path(kind: OpKind) -> Option<Vec<Ident>> {
    let path = match kind {
        // Printing and formatting are compiler intrinsics; the embedded std
        // surface only exposes their lower-level IO building blocks.
        OpKind::Format | OpKind::Print | OpKind::Println => return None,
        OpKind::TimeNow => &["std", "time", "now"][..],
        OpKind::Sleep => &["std", "time", "sleep"][..],
        OpKind::Spawn => &["std", "task", "spawn"][..],
        OpKind::Join => &["std", "task", "join"][..],
        OpKind::Select => &["std", "task", "select"][..],
        OpKind::FsReadDir => &["std", "fs", "read_dir"][..],
        OpKind::FsWalkDir => &["std", "fs", "walk_dir"][..],
        OpKind::FsReadToString => &["std", "fs", "read_to_string"][..],
        OpKind::FsWriteString => &["std", "fs", "write_string"][..],
        OpKind::FsAppendString => &["std", "fs", "append_string"][..],
        OpKind::FsExists => &["std", "fs", "exists"][..],
        OpKind::FsIsDir => &["std", "fs", "is_dir"][..],
        OpKind::FsIsFile => &["std", "fs", "is_file"][..],
        OpKind::FsCreateDirAll => &["std", "fs", "create_dir_all"][..],
        OpKind::FsRemoveFile => &["std", "fs", "remove_file"][..],
        OpKind::FsRemoveDirAll => &["std", "fs", "remove_dir_all"][..],
        OpKind::FsGlob => &["std", "fs", "glob"][..],
        OpKind::EnvCurrentDir => &["std", "env", "current_dir"][..],
        OpKind::EnvTempDir => &["std", "env", "temp_dir"][..],
        OpKind::EnvHomeDir => &["std", "env", "home_dir"][..],
        OpKind::EnvVar => &["std", "env", "var"][..],
        OpKind::EnvVarExists => &["std", "env", "exists"][..],
        OpKind::IoReadStdinToString => &["std", "io", "read_stdin_to_string"][..],
        OpKind::IoWriteStdout => &["std", "io", "write_stdout"][..],
        OpKind::IoWriteStderr => &["std", "io", "write_stderr"][..],
        OpKind::YamlToJson => &["std", "yaml", "to_json"][..],
        OpKind::JsonParse => &["std", "json", "parse"][..],
        OpKind::Input => &["std", "io", "input"][..],
        OpKind::ShellExec => &["std", "shell", "exec"][..],
        OpKind::ShellFileCopy => &["std", "shell", "file_copy"][..],
        OpKind::ShellFileTemplate => &["std", "shell", "file_template"][..],
        OpKind::ShellFileRsync => &["std", "shell", "file_rsync"][..],
        // Portable data ops — no std path, handled in normalize_invoke
        OpKind::OptionSome | OpKind::OptionNone | OpKind::OptionUnwrap
        | OpKind::VecNew | OpKind::Clone => return None,
    };
    Some(path.iter().map(|segment| Ident::new(*segment)).collect())
}

/// Apply the intrinsic normalizer to all expressions in AST items.
/// Used in transpile mode to normalize Rust-specific patterns before serialization.
pub fn normalize_items(items: &mut [fp_core::ast::Item], normalizer: &dyn IntrinsicNormalizer) -> Result<()> {
    for item in items {
        normalize_item(item, normalizer)?;
    }
    Ok(())
}

fn normalize_item(item: &mut fp_core::ast::Item, n: &dyn IntrinsicNormalizer) -> Result<()> {
    use fp_core::ast::ItemKind;
    match item.kind_mut() {
        ItemKind::Module(m) => { for c in &mut m.items { normalize_item(c, n)?; } }
        ItemKind::DefFunction(f) => {
            for stmt in &mut f.body.stmts { normalize_stmt(stmt, n)?; }
        }
        ItemKind::Impl(imp) => { for c in &mut imp.items { normalize_item(c, n)?; } }
        ItemKind::Expr(expr) => { normalize_expr(expr, n)?; }
        ItemKind::DefConst(c) => { normalize_expr(&mut c.value, n)?; }
        _ => {}
    }
    Ok(())
}

fn normalize_stmt(stmt: &mut BlockStmt, n: &dyn IntrinsicNormalizer) -> Result<()> {
    match stmt {
        BlockStmt::Let(l) => { if let Some(init) = &mut l.init { normalize_expr(init, n)?; } }
        BlockStmt::Expr(se) => { normalize_expr(&mut se.expr, n)?; }
        BlockStmt::Item(item) => { normalize_item(item, n)?; }
        _ => {}
    }
    Ok(())
}

fn normalize_expr(expr: &mut Expr, n: &dyn IntrinsicNormalizer) -> Result<()> {
    // Walk children first
    match expr.kind_mut() {
        ExprKind::Block(block) => { for s in &mut block.stmts { normalize_stmt(s, n)?; } }
        ExprKind::If(if_expr) => {
            normalize_expr(&mut if_expr.cond, n)?;
            normalize_expr(&mut if_expr.then, n)?;
            if let Some(e) = &mut if_expr.elze { normalize_expr(e, n)?; }
        }
        ExprKind::Match(mt) => {
            if let Some(s) = &mut mt.scrutinee { normalize_expr(s, n)?; }
            for c in &mut mt.cases { normalize_expr(&mut c.body, n)?; }
        }
        ExprKind::While(wh) => { normalize_expr(&mut wh.cond, n)?; normalize_expr(&mut wh.body, n)?; }
        ExprKind::For(fr) => { normalize_expr(&mut fr.iter, n)?; normalize_expr(&mut fr.body, n)?; }
        ExprKind::Loop(lp) => { normalize_expr(&mut lp.body, n)?; }
        ExprKind::BinOp(b) => { normalize_expr(&mut b.lhs, n)?; normalize_expr(&mut b.rhs, n)?; }
        ExprKind::UnOp(u) => { normalize_expr(&mut u.val, n)?; }
        ExprKind::Assign(a) => { normalize_expr(&mut a.target, n)?; normalize_expr(&mut a.value, n)?; }
        ExprKind::Return(r) => { if let Some(v) = &mut r.value { normalize_expr(v, n)?; } }
        ExprKind::Let(l) => { normalize_expr(&mut l.expr, n)?; }
        ExprKind::Closure(cl) => { normalize_expr(&mut cl.body, n)?; }
        ExprKind::Array(arr) => { for v in &mut arr.values { normalize_expr(v, n)?; } }
        ExprKind::Struct(st) => { for f in &mut st.fields { if let Some(v) = &mut f.value { normalize_expr(v, n)?; } } }
        ExprKind::Select(sel) => { normalize_expr(&mut sel.obj, n)?; }
        ExprKind::Index(idx) => { normalize_expr(&mut idx.obj, n)?; normalize_expr(&mut idx.index, n)?; }
        ExprKind::Paren(p) => { normalize_expr(&mut p.expr, n)?; }
        ExprKind::Reference(r) => { normalize_expr(&mut r.referee, n)?; }
        ExprKind::Dereference(d) => { normalize_expr(&mut d.referee, n)?; }
        ExprKind::Cast(c) => { normalize_expr(&mut c.expr, n)?; }
        _ => {}
    }

    // Apply normalizer to this expression
    let original = expr.clone();
    match n.normalize_expr(original) {
        Ok(outcome) if outcome.is_normalized() => {
            *expr = outcome.into_inner();
        }
        _ => {}
    }
    Ok(())
}

fn resolve_lang_intrinsic(invoke: &ExprInvoke) -> Option<CallKind> {
    let name = match &invoke.target {
        ExprInvokeTarget::Function(name) => name.to_string(),
        _ => return None,
    };
    let fn_name = match name.as_str() {
        "print" | "println" | "std::print" | "std::println" | "std::io::print"
        | "std::io::println" => name.rsplit("::").next().unwrap_or(&name),
        _ if !name.contains("::") => name.as_str(),
        _ => return None,
    };
    intrinsic_macro_kind(fn_name).or_else(|| {
        operation_kind(fn_name).map(CallKind::Op)
    })
}

fn operation_kind(name: &str) -> Option<OpKind> {
    match name {
        "format" => Some(OpKind::Format),
        "print" => Some(OpKind::Print),
        "println" => Some(OpKind::Println),
        "input" => Some(OpKind::Input),
        "now" => Some(OpKind::TimeNow),
        "sleep" => Some(OpKind::Sleep),
        "spawn" => Some(OpKind::Spawn),
        "join" => Some(OpKind::Join),
        "select" => Some(OpKind::Select),
        "read_dir" => Some(OpKind::FsReadDir),
        "walk_dir" => Some(OpKind::FsWalkDir),
        "read_to_string" => Some(OpKind::FsReadToString),
        "write_string" => Some(OpKind::FsWriteString),
        "append_string" => Some(OpKind::FsAppendString),
        "exists" => Some(OpKind::FsExists),
        "is_dir" => Some(OpKind::FsIsDir),
        "is_file" => Some(OpKind::FsIsFile),
        "create_dir_all" => Some(OpKind::FsCreateDirAll),
        "remove_file" => Some(OpKind::FsRemoveFile),
        "remove_dir_all" => Some(OpKind::FsRemoveDirAll),
        "glob" => Some(OpKind::FsGlob),
        "current_dir" => Some(OpKind::EnvCurrentDir),
        "temp_dir" => Some(OpKind::EnvTempDir),
        "home_dir" => Some(OpKind::EnvHomeDir),
        "var" => Some(OpKind::EnvVar),
        "read_stdin_to_string" => Some(OpKind::IoReadStdinToString),
        "write_stdout" => Some(OpKind::IoWriteStdout),
        "write_stderr" => Some(OpKind::IoWriteStderr),
        "to_json" => Some(OpKind::YamlToJson),
        "parse" => Some(OpKind::JsonParse),
        "exec" => Some(OpKind::ShellExec),
        "file_copy" => Some(OpKind::ShellFileCopy),
        "file_template" => Some(OpKind::ShellFileTemplate),
        "file_rsync" => Some(OpKind::ShellFileRsync),
        "some" | "Some" => Some(OpKind::OptionSome),
        "none" | "None" => Some(OpKind::OptionNone),
        "unwrap" | "Unwrap" => Some(OpKind::OptionUnwrap),
        "vec_new" | "Vec::new" | "Vec" | "vec" => Some(OpKind::VecNew),
        "clone" | "Clone" => Some(OpKind::Clone),
        _ => None,
    }
}

fn intrinsic_macro_kind(name: &str) -> Option<CallKind> {
    match name {
        "join" => Some(CallKind::Intrinsic(IntrinsicKind::Join)),
        "sizeof" => Some(CallKind::Intrinsic(IntrinsicKind::SizeOf)),
        "reflect_fields" => Some(CallKind::Intrinsic(IntrinsicKind::ReflectFields)),
        "hasmethod" => Some(CallKind::Intrinsic(IntrinsicKind::HasMethod)),
        "type_name" => Some(CallKind::Intrinsic(IntrinsicKind::TypeName)),
        "type_info" | "type_of" => Some(CallKind::Intrinsic(IntrinsicKind::TypeOf)),
        "clone_struct" => Some(CallKind::Intrinsic(IntrinsicKind::CloneStruct)),
        "create_struct" => Some(CallKind::Intrinsic(IntrinsicKind::CreateStruct)),
        "addfield" => Some(CallKind::Intrinsic(IntrinsicKind::AddField)),
        "hasfield" => Some(CallKind::Intrinsic(IntrinsicKind::HasField)),
        "count_fields" | "field_count" => Some(CallKind::Intrinsic(IntrinsicKind::FieldCount)),
        "method_count" => Some(CallKind::Intrinsic(IntrinsicKind::MethodCount)),
        "field_type" => Some(CallKind::Intrinsic(IntrinsicKind::FieldType)),
        "vec_type" => Some(CallKind::Intrinsic(IntrinsicKind::VecType)),
        "field_name_at" => Some(CallKind::Intrinsic(IntrinsicKind::FieldNameAt)),
        "struct_size" => Some(CallKind::Intrinsic(IntrinsicKind::StructSize)),
        "generate_method" => Some(CallKind::Intrinsic(IntrinsicKind::GenerateMethod)),
        "compile_error" => Some(CallKind::Intrinsic(IntrinsicKind::CompileError)),
        "compile_warning" => Some(CallKind::Intrinsic(IntrinsicKind::CompileWarning)),
        "some" | "Some" => Some(CallKind::Op(OpKind::OptionSome)),
        "none" | "None" => Some(CallKind::Op(OpKind::OptionNone)),
        "unwrap" | "Unwrap" => Some(CallKind::Op(OpKind::OptionUnwrap)),
        "vec_new" | "Vec::new" | "Vec" | "vec" => Some(CallKind::Op(OpKind::VecNew)),
        "clone" | "Clone" => Some(CallKind::Op(OpKind::Clone)),
        _ => None,
    }
}

fn parse_type_macro_tokens(tokens: &[MacroTokenTree]) -> Result<fp_core::ast::Ty> {
    let file_id = macro_tokens_file_id(tokens);
    let tokens = macro_token_trees_to_tokens(tokens);
    crate::ast::parse_type_tokens(&tokens, file_id)
        .map_err(|err| fp_core::error::Error::from(err.to_string()))
}

fn parse_expr_macro_tokens(tokens: &[MacroTokenTree]) -> Result<Vec<Expr>> {
    let file_id = macro_tokens_file_id(tokens);
    let mut args = Vec::new();
    let tokens = macro_token_trees_to_tokens(tokens);
    for slice in tokens_to_top_level_slices(&tokens) {
        if slice.is_empty() {
            continue;
        }
        let expr = crate::ast::parse_expr_tokens(slice, file_id)
            .map_err(|err| fp_core::error::Error::from(format!("macro expr parse error: {err}")))?;
        args.push(expr);
    }
    Ok(args)
}

fn parse_vec_macro_tokens(tokens: &[MacroTokenTree], span: Span) -> Result<Expr> {
    let file_id = macro_tokens_file_id(tokens);
    let tokens = macro_token_trees_to_tokens(tokens);
    let wrapped = wrap_tokens_in_group(&tokens, "[", "]", span);
    crate::ast::parse_expr_tokens(&wrapped, file_id)
        .map_err(|err| fp_core::error::Error::from(err.to_string()))
}

#[allow(dead_code)]
fn parse_macro_tokens_with_type_args(
    tokens: &[MacroTokenTree],
    type_positions: &[usize],
) -> Result<Vec<Expr>> {
    let lexemes = macro_token_trees_to_lexemes(tokens);
    let file_id = macro_tokens_file_id(tokens);
    let mut idx = 0;
    let mut args = Vec::new();
    let mut arg_index = 0;
    while idx < lexemes.len() {
        while idx < lexemes.len() && lexemes[idx].kind != LexemeKind::Token {
            idx += 1;
        }
        if idx >= lexemes.len() {
            break;
        }
        if lexemes[idx].text == "," {
            idx += 1;
            continue;
        }
        let is_type = type_positions.iter().any(|pos| *pos == arg_index);
        if is_type {
            let slice = lexeme_slice_to_tokens(&lexemes[idx..]);
            match crate::ast::parse_type_prefix_tokens(&slice, file_id) {
                Ok((ty, consumed)) => {
                    args.push(Expr::value(Value::Type(ty)));
                    idx += consumed;
                }
                Err(_) => {
                    let slice = lexeme_slice_to_tokens(&lexemes[idx..]);
                    let (expr, consumed) = parse_expr_prefix_tokens(slice.as_slice(), file_id)
                        .map_err(|err| {
                            fp_core::error::Error::from(format!("assert macro parse error: {err}"))
                        })?;
                    args.push(Expr::value(Value::Type(Ty::Expr(expr.into()))));
                    idx += consumed;
                }
            }
        } else {
            let slice = lexeme_slice_to_tokens(&lexemes[idx..]);
            let (expr, consumed) =
                parse_expr_prefix_tokens(slice.as_slice(), file_id).map_err(|err| {
                    fp_core::error::Error::from(format!("assert macro parse error: {err}"))
                })?;
            args.push(expr);
            idx += consumed;
        }
        arg_index += 1;
    }
    Ok(args)
}

fn lexeme_slice_to_tokens(
    lexemes: &[crate::lexer::lexeme::Lexeme],
) -> Vec<crate::lexer::tokenizer::Token> {
    lexemes
        .iter()
        .filter(|lex| lex.kind == LexemeKind::Token)
        .map(|lex| {
            let (kind, lexeme) = crate::lexer::tokenizer::classify_and_normalize_lexeme(&lex.text)
                .unwrap_or((crate::lexer::tokenizer::TokenKind::Symbol, lex.text.clone()));
            crate::lexer::tokenizer::Token {
                kind,
                lexeme,
                span: crate::lexer::Span {
                    start: lex.span.start,
                    end: lex.span.end,
                },
            }
        })
        .collect()
}

fn parse_expr_prefix_tokens(
    tokens: &[crate::lexer::tokenizer::Token],
    file_id: fp_core::span::FileId,
) -> Result<(Expr, usize)> {
    let mut best = None;
    for end in 1..=tokens.len() {
        match crate::ast::parse_expr_tokens(&tokens[..end], file_id) {
            Ok(expr) => best = Some((expr, end)),
            Err(_) => continue,
        }
    }
    best.ok_or_else(|| fp_core::error::Error::from("failed to parse expression prefix"))
}

fn parse_format_template(template: &str) -> Result<Vec<FormatTemplatePart>> {
    let mut parts = Vec::new();
    let mut current_literal = String::new();
    let mut chars = template.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch == '{' {
            if matches!(chars.peek(), Some('{')) {
                chars.next();
                current_literal.push('{');
                continue;
            }
            if !current_literal.is_empty() {
                parts.push(FormatTemplatePart::Literal(current_literal.clone()));
                current_literal.clear();
            }
            if matches!(chars.peek(), Some('}')) {
                chars.next();
                parts.push(FormatTemplatePart::Placeholder(FormatPlaceholder {
                    arg_ref: FormatArgRef::Implicit,
                    format_spec: None,
                }));
                continue;
            }
            let mut placeholder_content = String::new();
            while let Some(inner_ch) = chars.next() {
                if inner_ch == '}' {
                    break;
                }
                placeholder_content.push(inner_ch);
            }
            let placeholder = parse_placeholder_content(&placeholder_content)?;
            parts.push(FormatTemplatePart::Placeholder(placeholder));
            continue;
        }
        if ch == '}' {
            if matches!(chars.peek(), Some('}')) {
                chars.next();
                current_literal.push('}');
                continue;
            }
            current_literal.push('}');
            continue;
        }
        if ch == '%' {
            if matches!(chars.peek(), Some('%')) {
                chars.next();
                current_literal.push('%');
                continue;
            }

            if !current_literal.is_empty() {
                parts.push(FormatTemplatePart::Literal(current_literal.clone()));
                current_literal.clear();
            }

            let mut spec = String::new();
            while let Some(&next) = chars.peek() {
                spec.push(next);
                chars.next();
                if next.is_ascii_alphabetic() {
                    break;
                }
            }
            if spec.is_empty() {
                spec.push('s');
            }
            parts.push(FormatTemplatePart::Placeholder(FormatPlaceholder {
                arg_ref: FormatArgRef::Implicit,
                format_spec: Some(
                    FormatSpec::parse(&format!("%{}", spec))
                        .map_err(fp_core::error::Error::from)?,
                ),
            }));
            continue;
        }

        current_literal.push(ch);
    }

    if !current_literal.is_empty() {
        parts.push(FormatTemplatePart::Literal(current_literal));
    }

    Ok(parts)
}

fn build_print_template_from_args(args: &[Expr]) -> Result<(ExprStringTemplate, usize)> {
    if args.is_empty() {
        return Ok((
            ExprStringTemplate {
                parts: vec![FormatTemplatePart::Literal(String::new())],
            },
            0,
        ));
    }

    match args[0].kind() {
        ExprKind::FormatString(format) => Ok((format.clone(), 1)),
        ExprKind::Value(value) => {
            if let Value::String(string) = &**value {
                if args.len() == 1 {
                    return Ok((
                        ExprStringTemplate {
                            parts: vec![FormatTemplatePart::Literal(string.value.clone())],
                        },
                        1,
                    ));
                }

                let template = string.value.clone();
                let looks_like_format_template = template.contains('{') || template.contains('%');
                if looks_like_format_template {
                    let parts = parse_format_template(&template)?;
                    return Ok((ExprStringTemplate { parts }, 1));
                }

                let mut parts = vec![FormatTemplatePart::Literal(template)];
                if !matches!(
                    parts.last(),
                    Some(FormatTemplatePart::Literal(lit)) if lit.is_empty()
                ) {
                    parts.push(FormatTemplatePart::Literal(" ".to_string()));
                }
                for (idx, _arg) in args[1..].iter().enumerate() {
                    parts.push(FormatTemplatePart::Placeholder(FormatPlaceholder {
                        arg_ref: FormatArgRef::Implicit,
                        format_spec: None,
                    }));
                    if idx + 1 < args.len() - 1 {
                        parts.push(FormatTemplatePart::Literal(" ".to_string()));
                    }
                }
                Ok((ExprStringTemplate { parts }, 1))
            } else {
                let mut parts = Vec::new();
                for idx in 0..args.len() {
                    parts.push(FormatTemplatePart::Placeholder(FormatPlaceholder {
                        arg_ref: FormatArgRef::Implicit,
                        format_spec: None,
                    }));
                    if idx + 1 < args.len() {
                        parts.push(FormatTemplatePart::Literal(" ".to_string()));
                    }
                }
                Ok((ExprStringTemplate { parts }, 0))
            }
        }
        _ => {
            let mut parts = Vec::new();
            for idx in 0..args.len() {
                parts.push(FormatTemplatePart::Placeholder(FormatPlaceholder {
                    arg_ref: FormatArgRef::Implicit,
                    format_spec: None,
                }));
                if idx + 1 < args.len() {
                    parts.push(FormatTemplatePart::Literal(" ".to_string()));
                }
            }
            Ok((ExprStringTemplate { parts }, 0))
        }
    }
}

fn parse_placeholder_content(content: &str) -> Result<FormatPlaceholder> {
    if content.is_empty() {
        return Ok(FormatPlaceholder {
            arg_ref: FormatArgRef::Implicit,
            format_spec: None,
        });
    }

    if let Some(colon_pos) = content.find(':') {
        let arg_part = &content[..colon_pos];
        let format_spec = &content[colon_pos + 1..];

        let arg_ref = if arg_part.is_empty() {
            FormatArgRef::Implicit
        } else if let Ok(index) = arg_part.parse::<usize>() {
            FormatArgRef::Positional(index)
        } else {
            FormatArgRef::Named(arg_part.to_string())
        };

        Ok(FormatPlaceholder {
            arg_ref,
            format_spec: Some(FormatSpec::parse(format_spec).map_err(fp_core::error::Error::from)?),
        })
    } else {
        let arg_ref = if let Ok(index) = content.parse::<usize>() {
            FormatArgRef::Positional(index)
        } else {
            FormatArgRef::Named(content.to_string())
        };

        Ok(FormatPlaceholder {
            arg_ref,
            format_spec: None,
        })
    }
}

fn assert_macro_with_panic(cond: Expr, panic_expr: Expr) -> Expr {
    let negated = Expr::new(ExprKind::UnOp(ExprUnOp {
        span: fp_core::span::Span::null(),
        op: UnOpKind::Not,
        val: cond.into(),
    }));
    let if_expr = Expr::new(ExprKind::If(ExprIf {
        span: fp_core::span::Span::null(),
        cond: negated.into(),
        then: Expr::block(ExprBlock::new_stmts(vec![BlockStmt::Expr(
            BlockStmtExpr::new(panic_expr).with_semicolon(true),
        )]))
        .into(),
        elze: None,
    }));

    Expr::block(ExprBlock::new_stmts_expr(
        vec![BlockStmt::Expr(
            BlockStmtExpr::new(if_expr).with_semicolon(true),
        )],
        Expr::unit(),
    ))
}

fn assert_compare_macro(left: Expr, right: Expr, op: BinOpKind, message: &str) -> Expr {
    let left_ident = Ident::new("__fp_assert_left");
    let right_ident = Ident::new("__fp_assert_right");
    let left_binding = BlockStmt::Let(StmtLet::new_simple(left_ident.clone(), left));
    let right_binding = BlockStmt::Let(StmtLet::new_simple(right_ident.clone(), right));

    let comparison = Expr::new(ExprKind::BinOp(ExprBinOp {
        span: fp_core::span::Span::null(),
        kind: op,
        lhs: Expr::ident(left_ident).into(),
        rhs: Expr::ident(right_ident).into(),
    }));
    let negated = Expr::new(ExprKind::UnOp(ExprUnOp {
        span: fp_core::span::Span::null(),
        op: UnOpKind::Not,
        val: comparison.into(),
    }));
    let panic_expr = panic_call_with_message(message);
    let if_expr = Expr::new(ExprKind::If(ExprIf {
        span: fp_core::span::Span::null(),
        cond: negated.into(),
        then: Expr::block(ExprBlock::new_stmts(vec![BlockStmt::Expr(
            BlockStmtExpr::new(panic_expr).with_semicolon(true),
        )]))
        .into(),
        elze: None,
    }));

    Expr::block(ExprBlock::new_stmts_expr(
        vec![
            left_binding,
            right_binding,
            BlockStmt::Expr(BlockStmtExpr::new(if_expr).with_semicolon(true)),
        ],
        Expr::unit(),
    ))
}

fn assert_compare_macro_with_panic(
    left: Expr,
    right: Expr,
    op: BinOpKind,
    panic_expr: Expr,
) -> Expr {
    let left_ident = Ident::new("__fp_assert_left");
    let right_ident = Ident::new("__fp_assert_right");
    let left_binding = BlockStmt::Let(StmtLet::new_simple(left_ident.clone(), left));
    let right_binding = BlockStmt::Let(StmtLet::new_simple(right_ident.clone(), right));

    let comparison = Expr::new(ExprKind::BinOp(ExprBinOp {
        span: fp_core::span::Span::null(),
        kind: op,
        lhs: Expr::ident(left_ident).into(),
        rhs: Expr::ident(right_ident).into(),
    }));
    let negated = Expr::new(ExprKind::UnOp(ExprUnOp {
        span: fp_core::span::Span::null(),
        op: UnOpKind::Not,
        val: comparison.into(),
    }));
    let if_expr = Expr::new(ExprKind::If(ExprIf {
        span: fp_core::span::Span::null(),
        cond: negated.into(),
        then: Expr::block(ExprBlock::new_stmts(vec![BlockStmt::Expr(
            BlockStmtExpr::new(panic_expr).with_semicolon(true),
        )]))
        .into(),
        elze: None,
    }));

    Expr::block(ExprBlock::new_stmts_expr(
        vec![
            left_binding,
            right_binding,
            BlockStmt::Expr(BlockStmtExpr::new(if_expr).with_semicolon(true)),
        ],
        Expr::unit(),
    ))
}

fn panic_macro(args: Vec<Expr>) -> Expr {
    let message = panic_call_from_args(args);
    Expr::block(ExprBlock::new_stmts_expr(
        vec![BlockStmt::Expr(
            BlockStmtExpr::new(message).with_semicolon(true),
        )],
        Expr::unit(),
    ))
}

fn panic_call_from_args(args: Vec<Expr>) -> Expr {
    if args.is_empty() {
        panic_call_with_message("panic! macro triggered")
    } else {
        Expr::new(ExprKind::IntrinsicCall(ExprIntrinsicCall::new(
            CallKind::Intrinsic(IntrinsicKind::Panic),
            args,
            Vec::new(),
        )))
    }
}

fn panic_call_with_message(message: &str) -> Expr {
    Expr::new(ExprKind::IntrinsicCall(ExprIntrinsicCall::new(
        CallKind::Intrinsic(IntrinsicKind::Panic),
        vec![Expr::value(Value::string(message.to_string()))],
        Vec::new(),
    )))
}

#[cfg(test)]
mod tests {
    use super::*;
    use fp_core::frontend::LanguageFrontend;

    fn op_call(kind: OpKind) -> Expr {
        Expr::new(ExprKind::IntrinsicCall(ExprIntrinsicCall::new(
            CallKind::Op(kind),
            Vec::new(),
            Vec::new(),
        )))
    }

    fn intrinsic_call(kind: fp_core::intrinsics::IntrinsicKind) -> Expr {
        Expr::new(ExprKind::IntrinsicCall(ExprIntrinsicCall::new(
            CallKind::Intrinsic(kind),
            Vec::new(),
            Vec::new(),
        )))
    }

    #[test]
    fn compile_mode_preserves_intrinsics_but_restores_ops() {
        let normalizer = FerroIntrinsicNormalizer::new(IntrinsicNormalizationMode::Compile);

        let op = normalizer
            .normalize_call(op_call(OpKind::FsReadToString))
            .expect("normalize op call")
            .into_inner();
        assert!(matches!(op.kind(), ExprKind::Invoke(_)));

        let intrinsic = normalizer
            .normalize_call(intrinsic_call(
                fp_core::intrinsics::IntrinsicKind::FsReadToString,
            ))
            .expect("normalize intrinsic call")
            .into_inner();
        assert!(matches!(intrinsic.kind(), ExprKind::IntrinsicCall(_)));
    }

    #[test]
    fn compile_mode_shapes_direct_print_calls() {
        let normalizer = FerroIntrinsicNormalizer::new(IntrinsicNormalizationMode::Compile);
        let call = Expr::new(ExprKind::IntrinsicCall(ExprIntrinsicCall::new(
            CallKind::Op(OpKind::Print),
            vec![Expr::value(Value::string("value".to_string()))],
            Vec::new(),
        )));

        let normalized = normalizer
            .normalize_call(call)
            .expect("normalize print call")
            .into_inner();
        let ExprKind::IntrinsicCall(call) = normalized.kind() else {
            panic!("expected compiler print intrinsic");
        };
        assert!(matches!(
            call.kind,
            CallKind::Intrinsic(IntrinsicKind::Print)
        ));
        assert!(matches!(
            call.args.first().map(Expr::kind),
            Some(ExprKind::FormatString(_))
        ));
        assert_eq!(call.args.len(), 1);
    }

    #[test]
    fn compile_mode_does_not_capture_qualified_user_print() {
        let normalizer = FerroIntrinsicNormalizer::new(IntrinsicNormalizationMode::Compile);
        let invoke = Expr::new(ExprKind::Invoke(ExprInvoke {
            span: Span::null(),
            target: ExprInvokeTarget::Function(Name::path(Path::plain(vec![
                Ident::new("json"),
                Ident::new("print"),
            ]))),
            args: vec![Expr::value(Value::string("value".to_string()))],
            kwargs: Vec::new(),
        }));

        let normalized = normalizer
            .normalize_invoke(invoke)
            .expect("normalize qualified call")
            .into_inner();
        assert!(matches!(normalized.kind(), ExprKind::Invoke(_)));
    }

    #[test]
    fn transpile_mode_keeps_ops_canonical() {
        let normalizer = FerroIntrinsicNormalizer::new(IntrinsicNormalizationMode::Transpile);
        let normalized = normalizer
            .normalize_call(op_call(OpKind::FsReadToString))
            .expect("normalize op call")
            .into_inner();
        assert!(matches!(normalized.kind(), ExprKind::IntrinsicCall(_)));
    }

    #[test]
    fn compile_mode_restores_representative_std_paths() {
        let normalizer = FerroIntrinsicNormalizer::new(IntrinsicNormalizationMode::Compile);
        let cases = [
            (OpKind::FsWriteString, "std::fs::write_string"),
            (OpKind::EnvVar, "std::env::var"),
            (OpKind::IoWriteStdout, "std::io::write_stdout"),
            (OpKind::TimeNow, "std::time::now"),
            (OpKind::YamlToJson, "std::yaml::to_json"),
            (OpKind::JsonParse, "std::json::parse"),
        ];

        for (kind, expected_path) in cases {
            let normalized = normalizer
                .normalize_call(op_call(kind))
                .expect("normalize lang call")
                .into_inner();
            let ExprKind::Invoke(invoke) = normalized.kind() else {
                panic!("expected ordinary invoke for {kind:?}");
            };
            let ExprInvokeTarget::Function(name) = &invoke.target else {
                panic!("expected function target for {kind:?}");
            };
            assert_eq!(name.to_string(), expected_path);
        }
    }

    #[test]
    fn std_registry_keeps_intrinsic_and_op_marks_distinct() {
        let frontend = crate::FerroFrontend::new();
        let result = frontend
            .parse(
                "#[intrinsic = \"test_intrinsic\"] fn public_api() {}\n#[op = \"test_op\"] fn compiler_op() {}",
                None,
            )
            .expect("parse marked declarations");
        let registry = fp_core::lang::collect_lang_items(&result.ast);

        assert_eq!(
            registry
                .get_path("test_intrinsic")
                .expect("intrinsic declaration")
                .to_string(),
            "public_api"
        );
        assert_eq!(
            registry
                .get_op_path("test_op")
                .expect("op declaration")
                .to_string(),
            "compiler_op"
        );
        assert!(registry.get_path("test_op").is_none());
        assert!(registry.get_op_path("test_intrinsic").is_none());
    }
}
