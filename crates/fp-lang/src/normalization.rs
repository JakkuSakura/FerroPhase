use fp_core::ast::{
    BlockStmt, BlockStmtExpr, Expr, ExprBinOp, ExprBlock, ExprField, ExprIf, ExprIntrinsicCall,
    ExprIntrinsicContainer, ExprInvoke, ExprLet, ExprMatch,
    ExprInvokeTarget, ExprKind, ExprReference, ExprSelect, ExprSelectType, ExprStringTemplate,
    ExprStruct, ExprUnOp,
    FormatArgRef, FormatPlaceholder, FormatSpec, FormatTemplatePart, Ident, MacroTokenTree, Name,
    Path, PatternKind, StmtLet, Ty, Value,
};
use fp_core::error::Result;
use fp_core::intrinsics::{
    CallKind, IntrinsicKind, IntrinsicNormalizationMode, IntrinsicNormalizer, NormalizeOutcome,
    OpKind,
};
use fp_core::ops::{BinOpKind, UnOpKind};
use fp_core::span::Span;

use std::collections::HashMap;
use std::sync::Arc;

use fp_core::ast::MacroRulesDef;

use crate::ast::lower_common::{macro_token_trees_to_lexemes, macro_tokens_file_id};
use crate::lexer::lexeme::LexemeKind;
use crate::macro_parser::{
    macro_token_trees_to_tokens, match_macro_rule, substitute_template, tokens_to_top_level_slices,
    wrap_tokens_in_group,
};

/// FerroPhase intrinsic normalizer that adds `t!` macro lowering for type expressions,
/// delegating all other macros to the Rust normalizer.
#[derive(Debug, Clone)]
pub struct FerroIntrinsicNormalizer {
    mode: IntrinsicNormalizationMode,
    /// Every `macro_rules!` definition reachable in the package being
    /// compiled (see `collect_macro_rules_defs`), consulted by
    /// `normalize_macro`'s fallback for any macro name that isn't one of
    /// the compiler-intrinsic-like builtins (`cfg!`, `vec!`, etc.) handled
    /// above it. `Arc` so cloning this normalizer (it's `Clone`, stored
    /// behind `Option<Box<dyn IntrinsicNormalizer>>` elsewhere) doesn't
    /// deep-copy the whole map. Empty by default — populated via
    /// `with_macro_rules_defs`.
    macro_rules_defs: Arc<HashMap<String, MacroRulesDef>>,
}

impl Default for FerroIntrinsicNormalizer {
    fn default() -> Self {
        Self::new(IntrinsicNormalizationMode::Transpile)
    }
}

impl FerroIntrinsicNormalizer {
    pub fn new(mode: IntrinsicNormalizationMode) -> Self {
        Self {
            mode,
            macro_rules_defs: Arc::new(HashMap::new()),
        }
    }

    pub fn with_macro_rules_defs(mut self, defs: HashMap<String, MacroRulesDef>) -> Self {
        self.macro_rules_defs = Arc::new(defs);
        self
    }
}

impl IntrinsicNormalizer for FerroIntrinsicNormalizer {
    fn normalize_expr(&self, expr: Expr) -> Result<NormalizeOutcome<Expr>> {
        // Handle None / Some as bare name references (enum variants)
        if self.mode == IntrinsicNormalizationMode::Transpile {
            if let ExprKind::Name(name) = expr.kind() {
                let s = name.to_string();
                if s == "None" {
                    let (id, span, _) = expr.into_parts();
                    return Ok(NormalizeOutcome::Normalized(Expr::from_parts(id, span,
                        ExprKind::Value(Box::new(Value::Null(Default::default()))),
                    )));
                }
                if s == "Some" {
                    return Ok(NormalizeOutcome::Ignored(expr));
                }
            }
        }
        // Fall through to default dispatch
        let kind = expr.kind().clone();
        let moved = expr;
        match kind {
            fp_core::ast::ExprKind::Macro(_) => self.normalize_macro(moved),
            fp_core::ast::ExprKind::IntrinsicCall(_) => self.normalize_call(moved),
            fp_core::ast::ExprKind::IntrinsicContainer(_) => self.normalize_container(moved),
            fp_core::ast::ExprKind::Struct(_) => self.normalize_struct(moved),
            fp_core::ast::ExprKind::Structural(_) => self.normalize_structural(moved),
            fp_core::ast::ExprKind::Invoke(_) => self.normalize_invoke(moved),
            fp_core::ast::ExprKind::Match(_) => self.normalize_match(moved),
            _ => Ok(NormalizeOutcome::Ignored(moved)),
        }
    }

    fn normalize_call(&self, expr: Expr) -> Result<NormalizeOutcome<Expr>> {
        let (id, span, kind) = expr.into_parts();
        let ExprKind::IntrinsicCall(call) = kind else {
            return Ok(NormalizeOutcome::Ignored(Expr::from_parts(id, span, kind,
            )));
        };
        let CallKind::Op(op) = call.kind else {
            return Ok(NormalizeOutcome::Ignored(Expr::from_parts(id, span,
                ExprKind::IntrinsicCall(call),
            )));
        };
        // The op-defining declaration's own source path is never
        // reconstructed here (that was the design flaw in the retired
        // `compile_mode_std_path`): the resolved call target is discarded
        // upstream on purpose, and only `op` survives, to be consumed later
        // by a backend-specific materializer (e.g. `kotlin_materializer.rs`).
        // Every mode therefore just keeps the `IntrinsicCall(CallKind::Op)`
        // node unchanged.
        let _ = op;
        Ok(NormalizeOutcome::Ignored(Expr::from_parts(id, span,
            ExprKind::IntrinsicCall(call),
        )))
    }

    fn normalize_macro(&self, expr: Expr) -> Result<NormalizeOutcome<Expr>> {
        let (id, span, kind) = expr.into_parts();
        let ExprKind::Macro(macro_expr) = kind else {
            return Ok(NormalizeOutcome::Ignored(Expr::from_parts(id, span, kind,
            )));
        };

        if let Some(name) = macro_expr.invocation.path.segments.last() {
            let macro_name = name.as_str().trim_end_matches('!');
            if macro_name == "t" {
                if let Ok(ty) = parse_type_macro_tokens(&macro_expr.invocation.token_trees) {
                    let replacement = Expr::value(Value::Type(ty));
                    return Ok(NormalizeOutcome::Normalized(replacement));
                }
                return Ok(NormalizeOutcome::Ignored(Expr::from_parts(id, span,
                    ExprKind::Macro(macro_expr),
                )));
            }
            // `cfg!(...)` uses the identical grammar as `#[cfg(...)]`
            // attributes (`name`, `name = "value"`, `name(...)`) — parse its
            // tokens the same way (`parse_attr_meta_direct`, normally used
            // for attributes) and evaluate with the same predicate
            // (`cfg_meta_enabled`) already used for attribute-position
            // `#[cfg(...)]` item filtering. `TargetEnv::host()` matches
            // `HirGenerator`'s own default (`ast_to_hir/mod.rs`) — this
            // transpiler has no real cross-compilation target, so cfg
            // predicates are evaluated against the host machine either way.
            if macro_name == "cfg" {
                let tokens = macro_token_trees_to_tokens(&macro_expr.invocation.token_trees);
                let file_id = macro_tokens_file_id(&macro_expr.invocation.token_trees);
                let mut input = tokens.as_slice();
                return match crate::ast::parse_attr_meta_direct(&mut input, file_id) {
                    Ok(meta) => {
                        let enabled = fp_core::cfg::cfg_meta_enabled(
                            &meta,
                            &fp_core::cfg::TargetEnv::host(),
                        );
                        let replacement = Expr::value(Value::bool(enabled));
                        Ok(NormalizeOutcome::Normalized(replacement))
                    }
                    Err(_) => Ok(NormalizeOutcome::Ignored(Expr::from_parts(id, span,
                        ExprKind::Macro(macro_expr),
                    ))),
                };
            }
            // `cfg_select! { pred1 => { ... } pred2 => { ... } _ => { ... } }`
            // — a multi-arm cfg-gated selector (std's own polyfill wraps the
            // nightly builtin of the same name). Each arm's predicate uses
            // the same grammar as `cfg!`/`#[cfg(...)]`; pick the first arm
            // whose predicate holds (or a bare `_` wildcard) and normalize
            // to *that* arm's block — same "wrap raw tokens in the matching
            // delimiter, then run the real expression parser" trick already
            // used by `parse_vec_macro_tokens` below, just with `{`/`}`
            // instead of `[`/`]` (a block can mix items and statements, so
            // it needs the full parser, not a bespoke one here).
            if macro_name == "cfg_select" {
                return match select_cfg_select_arm(&macro_expr.invocation.token_trees) {
                    Some(arm_tokens) => {
                        let file_id = macro_tokens_file_id(&arm_tokens);
                        let flat = macro_token_trees_to_tokens(&arm_tokens);
                        let wrapped = wrap_tokens_in_group(&flat, "{", "}", macro_expr.span());
                        let block = crate::ast::parse_expr_tokens(&wrapped, file_id)
                            .map_err(|err| fp_core::error::Error::from(err.to_string()))?;
                        let replacement = block;
                        Ok(NormalizeOutcome::Normalized(replacement))
                    }
                    None => Ok(NormalizeOutcome::Ignored(Expr::from_parts(id, span,
                        ExprKind::Macro(macro_expr),
                    ))),
                };
            }
            // `io::const_error!(ErrorKind::X, "message")` — a std-internal
            // macro with no `macro_rules!` definition present in this
            // vendored snapshot to expand generically (confirmed absent),
            // so hand-expand it to match real Rust's actual expansion:
            // `Error::from_static_message(&SimpleMessage { kind, message })`
            // (`SimpleMessage { kind: ErrorKind, message: &'static str }`,
            // `Error::from_static_message(&'static SimpleMessage) -> Error`
            // — both plain, no custom constructor, confirmed in
            // `fp-rust/std/core/io/error.rs`).
            if macro_name == "const_error" {
                let args = parse_expr_macro_tokens(&macro_expr.invocation.token_trees)?;
                if args.len() != 2 {
                    return Ok(NormalizeOutcome::Ignored(Expr::from_parts(id, span,
                        ExprKind::Macro(macro_expr),
                    )));
                }
                let mut iter = args.into_iter();
                let kind_expr = iter.next().unwrap();
                let message_expr = iter.next().unwrap();
                let struct_lit = Expr::from(ExprKind::Struct(ExprStruct::new_ident(
                    Ident::new("SimpleMessage"),
                    vec![
                        ExprField::new(Ident::new("kind"), kind_expr),
                        ExprField::new(Ident::new("message"), message_expr),
                    ],
                )));
                let reference = Expr::from(ExprKind::Reference(ExprReference {
                    span: macro_expr.span(),
                    referee: Box::new(struct_lit),
                    mutable: None,
                }));
                let invoke = ExprInvoke {
                    target: ExprInvokeTarget::Function(Name::path(Path::plain(vec![
                        Ident::new("Error"),
                        Ident::new("from_static_message"),
                    ]))),
                    args: vec![reference],
                    kwargs: vec![],
                    span: macro_expr.span(),
                };
                let replacement = Expr::from_parts(id, span, ExprKind::Invoke(invoke));
                return Ok(NormalizeOutcome::Normalized(replacement));
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
                let replacement = Expr::from(ExprKind::Invoke(invoke));
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
                let replacement = assert_macro_with_panic(cond, panic_expr);
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
                ;
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
                ;
                return Ok(NormalizeOutcome::Normalized(replacement));
            }
            if macro_name == "matches" {
                // `matches!(scrutinee, pat)` — only the common literal-alternation shape
                // (`matches!(c, 'a' | 'b' | 'c')`) is rewritten here, to
                // `scrutinee == 'a' || scrutinee == 'b' || ...`. The pattern half isn't
                // valid expression syntax in general (guards, destructuring, ranges), so
                // anything else is left as `Ignored` — no worse than the previous
                // behavior of silently rendering as `null`.
                if let Ok(args) = parse_expr_macro_tokens(&macro_expr.invocation.token_trees) {
                    if args.len() == 2 {
                        let mut iter = args.into_iter();
                        let scrutinee = iter.next().unwrap();
                        let pattern = iter.next().unwrap();
                        if let Some(alts) = flatten_or_literal_pattern(&pattern) {
                            let mut alts = alts.into_iter();
                            let mut replacement = Expr::new(ExprKind::BinOp(ExprBinOp {
                                span: Span::default(),
                                kind: BinOpKind::Eq,
                                lhs: Box::new(scrutinee.clone()),
                                rhs: Box::new(alts.next().unwrap()),
                            }));
                            for alt in alts {
                                replacement = Expr::new(ExprKind::BinOp(ExprBinOp {
                                    span: Span::default(),
                                    kind: BinOpKind::Or,
                                    lhs: Box::new(replacement),
                                    rhs: Box::new(Expr::new(ExprKind::BinOp(ExprBinOp {
                                        span: Span::default(),
                                        kind: BinOpKind::Eq,
                                        lhs: Box::new(scrutinee.clone()),
                                        rhs: Box::new(alt),
                                    }))),
                                }));
                            }
                            return Ok(NormalizeOutcome::Normalized(
                                replacement,
                            ));
                        }
                    }
                }
                return Ok(NormalizeOutcome::Ignored(Expr::from_parts(id, span,
                    ExprKind::Macro(macro_expr),
                )));
            }
            if macro_name == "panic" {
                let args = parse_expr_macro_tokens(&macro_expr.invocation.token_trees)?;
                let replacement = panic_macro(args);
                return Ok(NormalizeOutcome::Normalized(replacement));
            }
            if macro_name == "format" {
                // Same rationale as `matches!` above: an argument that
                // isn't valid expression syntax on its own (e.g. a
                // qualified-path call like `<T as Trait>::method(..)`,
                // which this parser doesn't support as a *macro-argument*
                // re-parse even though it's a legitimate expression) is
                // left un-expanded rather than hard-failing the whole
                // enclosing item — and by extension, previously, the
                // *entire package's* typecheck (this macro's args are
                // reparsed from raw tokens independently of normal
                // top-down parsing, so one unsupported argument shape
                // anywhere in a large vendored file — even inside a test
                // module — used to poison everything).
                let Ok(args) = parse_expr_macro_tokens(&macro_expr.invocation.token_trees) else {
                    return Ok(NormalizeOutcome::Ignored(Expr::from_parts(id, span,
                        ExprKind::Macro(macro_expr),
                    )));
                };
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
                    span,
                    ExprKind::IntrinsicCall(ExprIntrinsicCall::new(
                        CallKind::Op(OpKind::Format),
                        call_args,
                        Vec::new(),
                    )),
                );
                return Ok(NormalizeOutcome::Normalized(replacement));
            }
            if macro_name == "write" || macro_name == "writeln" {
                // `write!(f, "template", args...)`/`writeln!(...)` — `f`
                // (`std::fmt::Formatter`) is modeled directly as Kotlin's
                // `StringBuilder` (see `kotlin_type_from_ty`), so this
                // becomes a real, valid method call on it —
                // `f.append(<the same portable Format-op string
                // `format!` already produces>)` — rather than needing any
                // fmt-specific codegen: `StringBuilder.append` already has
                // the right "mutate the receiver, return it" semantics
                // `write!`'s real expansion (`f.write_fmt(...)`) has, and
                // the enclosing `fmt` method (an ordinary `fn(&self, f:
                // &mut Formatter) -> fmt::Result` once both those types
                // are mapped) needs no special handling either.
                let Ok(args) = parse_expr_macro_tokens(&macro_expr.invocation.token_trees) else {
                    return Ok(NormalizeOutcome::Ignored(Expr::from_parts(id, span,
                        ExprKind::Macro(macro_expr),
                    )));
                };
                if args.is_empty() {
                    return Err(fp_core::error::Error::from(format!(
                        "{macro_name}! requires at least one argument (the formatter)"
                    )));
                }
                let (mut template, skip) = build_print_template_from_args(&args[1..])?;
                if macro_name == "writeln" {
                    template.parts.push(FormatTemplatePart::Literal("\n".to_string()));
                }
                let mut call_args = Vec::with_capacity(args.len());
                call_args.push(Expr::new(ExprKind::FormatString(template)));
                call_args.extend(args[1 + skip..].iter().cloned());
                let formatted = Expr::new(ExprKind::IntrinsicCall(ExprIntrinsicCall::new(
                    CallKind::Op(OpKind::Format),
                    call_args,
                    Vec::new(),
                )));
                let replacement = Expr::from_parts(
                    id,
                    span,
                    ExprKind::Invoke(ExprInvoke {
                        span: span.unwrap_or_default(),
                        target: ExprInvokeTarget::Method(ExprSelect {
                            span: span.unwrap_or_default(),
                            obj: Box::new(args[0].clone()),
                            field: Ident::new("append"),
                            select: ExprSelectType::Method,
                        }),
                        args: vec![formatted],
                        kwargs: Vec::new(),
                    }),
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
                    span,
                    ExprKind::IntrinsicCall(ExprIntrinsicCall::new(kind, call_args, Vec::new())),
                );
                return Ok(NormalizeOutcome::Normalized(replacement));
            }
            if let Some(kind) = intrinsic_macro_kind(macro_name) {
                let args = parse_expr_macro_tokens(&macro_expr.invocation.token_trees)?;
                let replacement = Expr::from_parts(
                    id,
                    span,
                    ExprKind::IntrinsicCall(ExprIntrinsicCall::new(kind, args, Vec::new())),
                );
                return Ok(NormalizeOutcome::Normalized(replacement));
            }
            // Not a compiler-intrinsic-like builtin (those are all handled
            // above) — try expanding it as a real user/std `macro_rules!`
            // definition, if one by this name was collected from the
            // package being compiled (`collect_macro_rules_defs`). Tries
            // each rule in declaration order, same as real `macro_rules!`
            // resolution; falls through to `Ignored` below if none match
            // (either the name is unknown, or every rule's matcher failed
            // to match this invocation's actual tokens).
            if let Some(def) = self.macro_rules_defs.get(macro_name) {
                let invocation_tokens = &macro_expr.invocation.token_trees;
                let file_id = macro_tokens_file_id(invocation_tokens);
                for rule in &def.rules {
                    let Some(bindings) = match_macro_rule(&rule.matcher, invocation_tokens, file_id)
                    else {
                        continue;
                    };
                    let substituted = substitute_template(&rule.transcriber, &bindings);
                    let flat = macro_token_trees_to_tokens(&substituted);
                    let wrapped = wrap_tokens_in_group(&flat, "{", "}", macro_expr.span());
                    if let Ok(replacement) = crate::ast::parse_expr_tokens(&wrapped, file_id) {
                        return Ok(NormalizeOutcome::Normalized(
                            replacement,
                        ));
                    }
                }
            }
        }

        Ok(NormalizeOutcome::Ignored(Expr::from_parts(id, span,
            ExprKind::Macro(macro_expr),
        )))
    }

    fn normalize_invoke(&self, expr: Expr) -> Result<NormalizeOutcome<Expr>> {
        let (id, span, kind) = expr.into_parts();
        let ExprKind::Invoke(invoke) = kind else {
            return Ok(NormalizeOutcome::Ignored(Expr::from_parts(id, span, kind,
            )));
        };

        // Under `TypedTranspile`, plain-call/method-call portable-op
        // detection is owned entirely by the post-typecheck
        // `HirToAstLifter`, consulting `hir::Program.op_defs` directly
        // (real resolved callee/method `DefId`s available there).
        // Reclassifying here too — before HIR lowering even runs, by name
        // alone — would just mutate the AST out from under that safer
        // pass, reintroducing the exact same-name-collision risk it
        // exists to close. Skip entirely.
        if self.mode == IntrinsicNormalizationMode::TypedTranspile {
            return Ok(NormalizeOutcome::Ignored(Expr::from_parts(id, span,
                ExprKind::Invoke(invoke),
            )));
        }

        // In transpile mode, let resolve_lang_intrinsic handle portable ops
        // directly (Some, None, Vec::new, etc.) instead of routing through
        // intrinsic_call_from_invoke → normalize_call which returns Ignored.
        if self.mode == IntrinsicNormalizationMode::Transpile {
            if let Some(kind) = resolve_lang_intrinsic(&invoke) {
                match kind {
                    CallKind::Op(OpKind::OptionSome) => {
                        return Ok(NormalizeOutcome::Normalized(
                            invoke.args.first().cloned().unwrap_or_else(|| {
                                Expr::from_parts(0, Some(Span::default()),
                                    ExprKind::Value(Box::new(Value::Null(Default::default()))))
                            })
                        ));
                    }
                    CallKind::Op(OpKind::OptionNone) => {
                        return Ok(NormalizeOutcome::Normalized(Expr::from_parts(id, span,
                            ExprKind::Value(Box::new(Value::Null(Default::default()))),
                        )));
                    }
                    CallKind::Op(OpKind::OptionUnwrap) => {
                        return Ok(NormalizeOutcome::Normalized(
                            invoke.args.first().cloned().unwrap_or_else(|| {
                                Expr::from_parts(0, Some(Span::default()),
                                    ExprKind::Value(Box::new(Value::Null(Default::default()))))
                            })
                        ));
                    }
                    CallKind::Op(OpKind::VecNew) => {
                        return Ok(NormalizeOutcome::Normalized(Expr::from_parts(id, span,
                            ExprKind::IntrinsicContainer(
                                ExprIntrinsicContainer::VecElements { elements: vec![] }
                            ),
                        )));
                    }
                    CallKind::Op(OpKind::Clone) => {
                        return Ok(NormalizeOutcome::Normalized(
                            invoke.args.first().cloned().unwrap_or_else(|| {
                                Expr::from_parts(0, Some(Span::default()),
                                    ExprKind::Value(Box::new(Value::Null(Default::default()))))
                            })
                        ));
                    }
                    // Method-like ops → drop method, keep receiver
                    CallKind::Op(OpKind::AsRef | OpKind::Iter | OpKind::ToOwned | OpKind::AsStr) => {
                        // These are method calls on the receiver — just keep args[0] (the receiver)
                        return Ok(NormalizeOutcome::Normalized(
                            invoke.args.first().cloned().unwrap_or_else(|| {
                                Expr::from_parts(0, Some(Span::default()),
                                    ExprKind::Value(Box::new(Value::Null(Default::default()))))
                            })
                        ));
                    }
                    // Wrapped as IntrinsicCall for serializer handling
                    CallKind::Op(OpKind::MapOr | OpKind::Collect | OpKind::Find | OpKind::UnwrapOr | OpKind::ToString | OpKind::AndThen) => {
                        return Ok(NormalizeOutcome::Normalized(Expr::from_parts(id, span,
                            ExprKind::IntrinsicCall(ExprIntrinsicCall::new(
                                kind, invoke.args, invoke.kwargs,
                            )),
                        )));
                    }
                    _ => {} // fall through to existing native path
                }
            }
        }

        // Keep the exact language-item registry as the source of truth for
        // operations exposed by loaded std modules. This also preserves the
        // call-specific argument shaping used by print and filesystem ops.
        if let Some(call) = fp_core::ast::intrinsic_call_from_invoke(&invoke) {
            return self.normalize_call(Expr::from_parts(id, span,
                ExprKind::IntrinsicCall(call),
            ));
        }

        let Some(intrinsic_kind) = resolve_lang_intrinsic(&invoke) else {
            return Ok(NormalizeOutcome::Ignored(Expr::from_parts(id, span,
                ExprKind::Invoke(invoke),
            )));
        };

        match self.mode {
            // Unreachable in practice — `TypedTranspile` returns early above,
            // before `intrinsic_call_from_invoke` is even consulted — but
            // `Compile`'s behavior is the safe default if that ever changes.
            //
            // No path reconstruction (the retired `compile_mode_std_path`'s
            // design flaw): the resolved call target is discarded on
            // purpose here, and only the `OpKind` survives, for a
            // backend-specific materializer to consume later.
            IntrinsicNormalizationMode::Compile | IntrinsicNormalizationMode::TypedTranspile => match intrinsic_kind {
                CallKind::Op(op) => match CallKind::Op(op).intrinsic_kind() {
                        Some(kind) => Ok(NormalizeOutcome::Normalized(Expr::from_parts(id, span,
                            ExprKind::IntrinsicCall(ExprIntrinsicCall::new(
                                CallKind::Intrinsic(kind),
                                invoke.args,
                                invoke.kwargs,
                            )),
                        ))),
                        None => Ok(NormalizeOutcome::Ignored(Expr::from_parts(id, span,
                            ExprKind::Invoke(invoke),
                        ))),
                },
                CallKind::Intrinsic(kind) => Ok(NormalizeOutcome::Normalized(Expr::from_parts(id, span,
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
                            Expr::from_parts(0, Some(Span::default()),
                                ExprKind::Value(Box::new(Value::Null(Default::default()))))
                        })
                    ))
                }
                CallKind::Op(OpKind::OptionNone) => {
                    Ok(NormalizeOutcome::Normalized(Expr::from_parts(id, span,
                        ExprKind::Value(Box::new(Value::Null(Default::default()))),
                    )))
                }
                CallKind::Op(OpKind::OptionUnwrap) => {
                    Ok(NormalizeOutcome::Normalized(
                        invoke.args.first().cloned().unwrap_or_else(|| {
                            Expr::from_parts(0, Some(Span::default()),
                                ExprKind::Value(Box::new(Value::Null(Default::default()))))
                        })
                    ))
                }
                CallKind::Op(OpKind::VecNew) => {
                    Ok(NormalizeOutcome::Normalized(Expr::from_parts(id, span,
                        ExprKind::IntrinsicContainer(
                            ExprIntrinsicContainer::VecElements { elements: vec![] }
                        ),
                    )))
                }
                CallKind::Op(OpKind::Clone) => {
                    Ok(NormalizeOutcome::Normalized(
                        invoke.args.first().cloned().unwrap_or_else(|| {
                            Expr::from_parts(0, Some(Span::default()),
                                ExprKind::Value(Box::new(Value::Null(Default::default()))))
                        })
                    ))
                }
                _ => {
                    Ok(NormalizeOutcome::Normalized(Expr::from_parts(id, span,
                        ExprKind::IntrinsicCall(ExprIntrinsicCall::new(
                            intrinsic_kind, invoke.args, invoke.kwargs,
                        )),
                    )))
                }
            },
        }
    }

    fn normalize_match(&self, expr: Expr) -> Result<NormalizeOutcome<Expr>> {
        if self.mode != IntrinsicNormalizationMode::Transpile {
            return Ok(NormalizeOutcome::Ignored(expr));
        }
        let (id, span, kind) = expr.into_parts();
        let ExprKind::Match(mut m) = kind else {
            return Ok(NormalizeOutcome::Ignored(Expr::from_parts(id, span, kind)));
        };

        // Find binding arm in 1 or 2-arm match
        let binding_case = if m.cases.len() == 1 {
            &m.cases[0]
        } else if m.cases.len() == 2 {
            let p0 = m.cases[0].pat.as_ref().map(|p| &p.kind);
            let p1 = m.cases[1].pat.as_ref().map(|p| &p.kind);
            let t0 = is_trivial_match_arm(p0);
            let t1 = is_trivial_match_arm(p1);
            if !t0 && t1 { &m.cases[1] } else if t0 && !t1 { &m.cases[0] } else {
                return Ok(NormalizeOutcome::Ignored(Expr::from_parts(id, span, ExprKind::Match(m))));
            }
        } else {
            return Ok(NormalizeOutcome::Ignored(Expr::from_parts(id, span, ExprKind::Match(m))));
        };

        let pat = match binding_case.pat.as_ref() {
            Some(p) if is_option_or_result_binding_pattern(&p.kind) => p,
            _ => return Ok(NormalizeOutcome::Ignored(Expr::from_parts(id, span, ExprKind::Match(m)))),
        };

        let scrutinee = match &m.scrutinee {
            Some(s) => s.as_ref().clone(),
            None => return Ok(NormalizeOutcome::Ignored(Expr::from_parts(id, span, ExprKind::Match(m)))),
        };

        // Build: if (scrutinee != null) { val x = scrutinee!!; body }
        let binding = match_binding_name(pat);
        let body = if let Some(b) = binding {
            let let_expr = Expr::from_parts(0, None,
                ExprKind::Let(ExprLet {
                    span: Span::default(),
                    pat: pat.clone(),
                    expr: Box::new(build_force_unwrap_expr(&scrutinee)),
                })
            );
            Expr::from_parts(0, None,
                ExprKind::Block(ExprBlock {
                    span: Span::default(),
                    collected_items: vec![],
                    stmts: vec![
                        BlockStmt::Expr(BlockStmtExpr {
                            expr: Box::new(let_expr),
                            semicolon: Some(true),
                        }),
                        BlockStmt::Expr(BlockStmtExpr {
                            expr: binding_case.body.clone(),
                            semicolon: Some(false),
                        }),
                    ],
                })
            )
        } else {
            binding_case.body.as_ref().clone()
        };

        let if_expr = Expr::from_parts(id, span,
            ExprKind::If(ExprIf {
                span: Span::default(),
                cond: Box::new(build_not_null_check_expr(&scrutinee)),
                then: Box::new(body),
                elze: None,
            })
        );
        Ok(NormalizeOutcome::Normalized(if_expr))
    }
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
        ExprKind::Invoke(inv) => {
            for arg in &mut inv.args { normalize_expr(arg, n)?; }
            if let ExprInvokeTarget::Method(sel) = &mut inv.target {
                normalize_expr(&mut sel.obj, n)?;
            }
        }
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
        ExprInvokeTarget::Function(name) => name,
        _ => return None,
    };
    match name {
        Name::Ident(ident) => {
            let fn_name = ident.name.as_str();
            intrinsic_macro_kind(fn_name).or_else(|| operation_kind(fn_name).map(CallKind::Op))
        }
        // Path-qualified builtins — FerroPhase's own std module layout
        // (`std::time::now`, `std::task::spawn`, etc.), so the segment
        // table lives here rather than in generic/shared call-resolution
        // code. Segment-matched (not string-matched) so a leading `::`
        // absolute-path prefix doesn't need special-casing.
        Name::Path(path) => {
            let segments: Vec<&str> = path.segments.iter().map(|seg| seg.name.as_str()).collect();
            match segments.as_slice() {
                ["std", "print"] | ["std", "io", "print"] => Some(CallKind::Print),
                ["std", "println"] | ["std", "io", "println"] => Some(CallKind::Println),
                ["std", "len"] | ["std", "builtins", "len"] | ["len"] => Some(CallKind::Len),
                ["type"] | ["std", "type"] | ["std", "builtins", "type"] => Some(CallKind::TypeOf),
                ["std", "time", "now"] => Some(CallKind::TimeNow),
                ["std", "task", "spawn"] => Some(CallKind::Spawn),
                ["std", "task", "join"] => Some(CallKind::Join),
                ["std", "task", "select"] => Some(CallKind::Select),
                ["proc_macro", "token_stream_from_str"]
                | ["std", "proc_macro", "token_stream_from_str"]
                | ["proc_macro", "TokenStream", "from_str"]
                | ["std", "proc_macro", "TokenStream", "from_str"] => {
                    Some(CallKind::ProcMacroTokenStreamFromStr)
                }
                ["proc_macro", "token_stream_to_string"]
                | ["std", "proc_macro", "token_stream_to_string"]
                | ["proc_macro", "TokenStream", "to_string"]
                | ["std", "proc_macro", "TokenStream", "to_string"] => {
                    Some(CallKind::ProcMacroTokenStreamToString)
                }
                _ => None,
            }
        }
        _ => None,
    }
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
        "as_ref" => Some(OpKind::AsRef),
        "map_or" => Some(OpKind::MapOr),
        "iter" => Some(OpKind::Iter),
        "collect" => Some(OpKind::Collect),
        "find" => Some(OpKind::Find),
        "unwrap_or" => Some(OpKind::UnwrapOr),
        "to_owned" => Some(OpKind::ToOwned),
        "as_str" => Some(OpKind::AsStr),
        "to_string" => Some(OpKind::ToString),
        "and_then" => Some(OpKind::AndThen),
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
        "catch_unwind" => Some(CallKind::Intrinsic(IntrinsicKind::CatchUnwind)),
        "catch_unwind_result" => Some(CallKind::Intrinsic(IntrinsicKind::CatchUnwindResult)),
        "some" | "Some" => Some(CallKind::Op(OpKind::OptionSome)),
        "none" | "None" => Some(CallKind::Op(OpKind::OptionNone)),
        "unwrap" | "Unwrap" => Some(CallKind::Op(OpKind::OptionUnwrap)),
        "vec_new" | "Vec::new" | "Vec" | "vec" => Some(CallKind::Op(OpKind::VecNew)),
        "clone" | "Clone" => Some(CallKind::Op(OpKind::Clone)),
        "as_ref" => Some(CallKind::Op(OpKind::AsRef)),
        "map_or" => Some(CallKind::Op(OpKind::MapOr)),
        "iter" => Some(CallKind::Op(OpKind::Iter)),
        "collect" => Some(CallKind::Op(OpKind::Collect)),
        "find" => Some(CallKind::Op(OpKind::Find)),
        "unwrap_or" => Some(CallKind::Op(OpKind::UnwrapOr)),
        "to_owned" => Some(CallKind::Op(OpKind::ToOwned)),
        "as_str" => Some(CallKind::Op(OpKind::AsStr)),
        "to_string" => Some(CallKind::Op(OpKind::ToString)),
        "and_then" => Some(CallKind::Op(OpKind::AndThen)),
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

/// Flatten a `'a' | 'b' | 'c'`-shaped expression (parsed from a `matches!` pattern
/// position, where `|` is really pattern alternation but parses as bitwise-or) into
/// its literal alternatives. Returns `None` if any leaf isn't a literal value.
fn flatten_or_literal_pattern(expr: &Expr) -> Option<Vec<Expr>> {
    match expr.kind() {
        ExprKind::BinOp(bin) if bin.kind == BinOpKind::BitOr => {
            let mut lhs = flatten_or_literal_pattern(&bin.lhs)?;
            let rhs = flatten_or_literal_pattern(&bin.rhs)?;
            lhs.extend(rhs);
            Some(lhs)
        }
        ExprKind::Value(_) => Some(vec![expr.clone()]),
        _ => None,
    }
}

/// Walks a `cfg_select!` invocation's top-level token trees — a flat
/// sequence of `[predicate tokens...] "=>" [body] ","?` repeated per arm —
/// and returns the inner tokens of the first arm whose predicate holds (a
/// bare `_` predicate always holds, matching Rust's wildcard arm). A body
/// is either one brace-delimited `MacroTokenTree::Group` (block form,
/// already captured as a single tree — no manual brace-depth tracking
/// needed) or a bare token sequence up to the next top-level comma (expr
/// form, e.g. `pred => true,`) — like a `match` arm, both are allowed.
/// The caller always wraps the returned tokens in `{ }` before parsing,
/// which is semantically identical to the bare form in expression
/// position, so both shapes return the same way.
fn select_cfg_select_arm(token_trees: &[MacroTokenTree]) -> Option<Vec<MacroTokenTree>> {
    let mut iter = token_trees.iter().peekable();
    loop {
        iter.peek()?;
        let mut predicate_tokens: Vec<MacroTokenTree> = Vec::new();
        loop {
            match iter.next() {
                Some(MacroTokenTree::Token(t)) if t.text == "=>" => break,
                Some(other) => predicate_tokens.push(other.clone()),
                None => return None,
            }
        }
        let body_tokens: Vec<MacroTokenTree> =
            if let Some(MacroTokenTree::Group(group)) = iter.peek() {
                let tokens = group.tokens.clone();
                iter.next();
                tokens
            } else {
                let mut tokens = Vec::new();
                while !matches!(iter.peek(), Some(MacroTokenTree::Token(t)) if t.text == ",")
                    && iter.peek().is_some()
                {
                    tokens.push(iter.next().unwrap().clone());
                }
                tokens
            };
        // Optional trailing comma between arms.
        if matches!(iter.peek(), Some(MacroTokenTree::Token(t)) if t.text == ",") {
            iter.next();
        }

        let is_wildcard = matches!(
            predicate_tokens.as_slice(),
            [MacroTokenTree::Token(t)] if t.text == "_"
        );
        let matched = if is_wildcard {
            true
        } else {
            let file_id = macro_tokens_file_id(&predicate_tokens);
            let flat = macro_token_trees_to_tokens(&predicate_tokens);
            let mut input = flat.as_slice();
            match crate::ast::parse_attr_meta_direct(&mut input, file_id) {
                Ok(meta) => {
                    fp_core::cfg::cfg_meta_enabled(&meta, &fp_core::cfg::TargetEnv::host())
                }
                Err(_) => false,
            }
        };
        if matched {
            return Some(body_tokens);
        }
    }
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
                let template = string.value.clone();
                let looks_like_format_template = template.contains('{') || template.contains('%');
                if args.len() == 1 && !looks_like_format_template {
                    return Ok((
                        ExprStringTemplate {
                            parts: vec![FormatTemplatePart::Literal(template)],
                        },
                        1,
                    ));
                }
                // Even with no trailing args, a `{name}`-style placeholder can
                // still be a real one — Rust's inline-captured-identifier
                // format syntax (`write!(f, "{name}")`) needs no separate
                // argument at all, unlike `{}` (positional/implicit), which
                // *does* require one; `parse_format_template` (and this
                // template's own `FormatArgRef::Named` resolution downstream)
                // already distinguishes the two, so the single-arg case must
                // still attempt real parsing rather than assuming "no args
                // after the template" means "no placeholders in it".
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
    // `panic!` diverges (its intrinsic call types as `!`, see
    // `IntrinsicKind::Panic` in `fp-typing`'s `check_intrinsic`) — the
    // wrapping block's own result must be that same call, not a hardcoded
    // `()` tail, so `panic!(...)` still type-checks when used as a match
    // arm or `if`/`else` branch alongside a real value.
    Expr::block(ExprBlock::new_stmts_expr(Vec::new(), message))
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

/// Whether a match arm pattern is trivial (no binding).
fn is_trivial_match_arm(pat: Option<&PatternKind>) -> bool {
    match pat {
        Some(PatternKind::Wildcard(_)) => true,
        Some(PatternKind::Ident(id)) => id.ident.name == "None" || id.ident.name == "Err",
        None => true,
        _ => false,
    }
}

/// Whether a pattern is shaped like the binding side of an Option/Result
/// match (`Some(x)`/`Ok(x)`, or a bare binding identifier) — required
/// before `normalize_match` rewrites a 1-or-2-arm match into an
/// `if (scrutinee != null)` check. Without this, any match with a
/// wildcard/`None`/`Err` arm plus *any* other pattern (e.g. a bare enum
/// variant like `ChangesLineKind::Add`) gets misidentified as an
/// Option/Result if-let.
fn is_option_or_result_binding_pattern(pat: &PatternKind) -> bool {
    match pat {
        PatternKind::TupleStruct(ts) => {
            let name = ts.name.to_string();
            let variant = name.rsplit("::").next().unwrap_or(name.as_str());
            variant == "Some" || variant == "Ok"
        }
        PatternKind::Ident(id) => id.ident.name != "None" && id.ident.name != "Err",
        _ => false,
    }
}

/// Extract the binding variable name from a pattern.
fn match_binding_name(pat: &fp_core::ast::Pattern) -> Option<String> {
    match &pat.kind {
        PatternKind::Ident(id) => Some(id.ident.name.clone()),
        PatternKind::TupleStruct(ts) => ts.patterns.first().and_then(|p| match_binding_name(p)),
        _ => None,
    }
}

fn build_not_null_check_expr(expr: &Expr) -> Expr {
    Expr::from_parts(0, None,
        ExprKind::BinOp(ExprBinOp {
            span: Span::default(),
            kind: fp_core::ops::BinOpKind::Ne,
            lhs: Box::new(expr.clone()),
            rhs: Box::new(Expr::from_parts(0, None,
                ExprKind::Value(Box::new(Value::Null(Default::default()))),
            )),
        })
    )
}

fn build_force_unwrap_expr(expr: &Expr) -> Expr {
    Expr::from_parts(0, None,
        ExprKind::IntrinsicCall(ExprIntrinsicCall::new(
            fp_core::intrinsics::CallKind::Op(fp_core::intrinsics::calls::OpKind::OptionUnwrap),
            vec![expr.clone()],
            vec![],
        ))
    )
}

// Allow returning Ok(None) from normalize_match without extra type annotations
struct NoneOutcome;
impl From<NoneOutcome> for NormalizeOutcome<Expr> {
    fn from(_: NoneOutcome) -> Self { NormalizeOutcome::Ignored(Expr::from_parts(0, None, ExprKind::Value(Box::new(Value::Null(Default::default()))))) }
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

        // No path reconstruction: the op stays a bare `IntrinsicCall(Op(_))`
        // node for a backend-specific materializer to consume later.
        let op = normalizer
            .normalize_call(op_call(OpKind::FsReadToString))
            .expect("normalize op call")
            .into_inner();
        assert!(matches!(
            op.kind(),
            ExprKind::IntrinsicCall(call) if matches!(call.kind, CallKind::Op(OpKind::FsReadToString))
        ));

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
        // No path/shape reconstruction in Compile mode anymore — a pre-formed
        // `IntrinsicCall(Op(_))` node is kept exactly as-is, for a
        // backend-specific materializer to consume later.
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
            panic!("expected intrinsic call to be preserved");
        };
        assert!(matches!(call.kind, CallKind::Op(OpKind::Print)));
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
        // The op-defining declaration's own source path is never
        // reconstructed by the normalizer (that was the retired
        // `compile_mode_std_path`'s design flaw) — every representative op
        // stays a bare `IntrinsicCall(Op(_))` node, for a backend-specific
        // materializer to consume later.
        let normalizer = FerroIntrinsicNormalizer::new(IntrinsicNormalizationMode::Compile);
        let cases = [
            OpKind::FsWriteString,
            OpKind::EnvVar,
            OpKind::IoWriteStdout,
            OpKind::TimeNow,
            OpKind::YamlToJson,
            OpKind::JsonParse,
        ];

        for kind in cases {
            let normalized = normalizer
                .normalize_call(op_call(kind))
                .expect("normalize lang call")
                .into_inner();
            let ExprKind::IntrinsicCall(call) = normalized.kind() else {
                panic!("expected intrinsic call to be preserved for {kind:?}");
            };
            assert_eq!(call.kind, CallKind::Op(kind));
        }
    }

    #[test]
    fn std_registry_keeps_intrinsic_and_op_marks_distinct() {
        let frontend = crate::FerroFrontend::new();
        let result = frontend
            .parse(
                "#[intrinsic = \"test_intrinsic\"] fn public_api() {}\n#[op(func = \"format\")] fn compiler_op() {}",
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
                .get_op_path(fp_core::intrinsics::OpKind::Format)
                .expect("op declaration")
                .to_string(),
            "compiler_op"
        );
        assert!(registry.get_path("format").is_none());
    }
}
