use super::{lossy_normalization_mode, Diagnostics};
use crate::{parser::RustParser, RawExprMacro};
use fp_core::ast::{
    Expr, ExprBlock, ExprKind, ExprMacro, ExprQuote, ExprSplice, MacroDelimiter, MacroInvocation,
    Path,
};
use fp_core::diagnostics::Diagnostic;
use proc_macro2::{Span, TokenStream};
use std::str::FromStr;

pub(crate) fn lower_macro_expression(macro_expr: &ExprMacro, diagnostics: Diagnostics<'_>) -> Expr {
    // FerroPhase macros: fp_quote!( { … } ) → quote { … }
    if let Some(name) = macro_expr.invocation.path.segments.last() {
        if name.as_str() == "fp_quote" {
            return lower_fp_quote(&macro_expr.invocation, diagnostics);
        }
        if name.as_str() == "fp_splice" {
            return lower_fp_splice(&macro_expr.invocation, diagnostics);
        }
    }

    // Builtin sugar: emit! { … } => splice(quote { … })
    if let Some(name) = macro_expr.invocation.path.segments.last() {
        if name.as_str() == "emit" {
            return lower_emit_macro(&macro_expr.invocation, diagnostics);
        }
    }
    let syn_macro = build_syn_expr_macro(&macro_expr.invocation, diagnostics);
    let mac_clone = syn_macro.clone();
    let lossy = lossy_normalization_mode();
    let parser = RustParser::new_with_lossy(lossy);
    let result = parser.lower_expr_macro(syn_macro);

    forward_parser_diagnostics(&parser, diagnostics);

    match result {
        Ok(expr) => expr,
        Err(err) => {
            if let Some(manager) = diagnostics {
                manager.add_diagnostic(
                    Diagnostic::error(format!(
                        "Failed to lower macro `{}`: {}",
                        macro_expr.invocation.path, err
                    ))
                    .with_source_context(super::NORMALIZATION_CONTEXT),
                );
            }
            Expr::any(RawExprMacro { raw: mac_clone })
        }
    }
}

fn lower_fp_quote(invocation: &MacroInvocation, diagnostics: Diagnostics<'_>) -> Expr {
    // Expect a block payload: fp_quote!({ ... })
    let stream = match TokenStream::from_str(&invocation.tokens) {
        Ok(ts) => ts,
        Err(err) => {
            if let Some(manager) = diagnostics {
                manager.add_diagnostic(
                    Diagnostic::error(format!("failed to parse fp_quote! tokens: {}", err))
                        .with_source_context(super::NORMALIZATION_CONTEXT),
                );
            }
            TokenStream::new()
        }
    };
    let syn_block = match syn::parse2::<syn::Block>(stream) {
        Ok(b) => b,
        Err(err) => {
            if let Some(manager) = diagnostics {
                manager.add_diagnostic(
                    Diagnostic::error(format!("fp_quote! expects a block: {}", err))
                        .with_source_context(super::NORMALIZATION_CONTEXT),
                );
            }
            syn::parse_quote!({})
        }
    };
    let parser = RustParser::new();
    let ast_block: ExprBlock = match parser.parse_block(syn_block) {
        Ok(b) => b,
        Err(err) => {
            if let Some(manager) = diagnostics {
                manager.add_diagnostic(
                    Diagnostic::error(format!("failed to lower fp_quote! block: {}", err))
                        .with_source_context(super::NORMALIZATION_CONTEXT),
                );
            }
            ExprBlock::new_stmts(Vec::new())
        }
    };
    Expr::from(ExprKind::Quote(ExprQuote {
        block: ast_block,
        kind: None,
    }))
}

fn lower_fp_splice(invocation: &MacroInvocation, diagnostics: Diagnostics<'_>) -> Expr {
    // Expect a single expression payload: fp_splice!( expr )
    let stream = match TokenStream::from_str(&invocation.tokens) {
        Ok(ts) => ts,
        Err(err) => {
            if let Some(manager) = diagnostics {
                manager.add_diagnostic(
                    Diagnostic::error(format!("failed to parse fp_splice! tokens: {}", err))
                        .with_source_context(super::NORMALIZATION_CONTEXT),
                );
            }
            TokenStream::new()
        }
    };
    let syn_expr = match syn::parse2::<syn::Expr>(stream) {
        Ok(e) => e,
        Err(err) => {
            if let Some(manager) = diagnostics {
                manager.add_diagnostic(
                    Diagnostic::error(format!("fp_splice! expects an expression: {}", err))
                        .with_source_context(super::NORMALIZATION_CONTEXT),
                );
            }
            syn::parse_quote!(())
        }
    };
    let parser = RustParser::new();
    let token_expr = match parser.parse_expr(syn_expr) {
        Ok(e) => e,
        Err(err) => {
            if let Some(manager) = diagnostics {
                manager.add_diagnostic(
                    Diagnostic::error(format!("failed to lower fp_splice! expression: {}", err))
                        .with_source_context(super::NORMALIZATION_CONTEXT),
                );
            }
            Expr::unit()
        }
    };
    Expr::from(ExprKind::Splice(ExprSplice {
        token: Box::new(token_expr),
    }))
}

fn lower_emit_macro(invocation: &MacroInvocation, diagnostics: Diagnostics<'_>) -> Expr {
    // Only brace-delimited form is supported: emit! { … }
    if !matches!(invocation.delimiter, MacroDelimiter::Brace) {
        if let Some(manager) = diagnostics {
            manager.add_diagnostic(
                Diagnostic::error("emit! expects a brace-delimited block: emit! { … }".to_string())
                    .with_source_context(super::NORMALIZATION_CONTEXT),
            );
        }
        return Expr::unit();
    }

    let tokens = match TokenStream::from_str(&invocation.tokens) {
        Ok(ts) => ts,
        Err(err) => {
            if let Some(manager) = diagnostics {
                manager.add_diagnostic(
                    Diagnostic::error(format!("failed to parse emit! tokens: {}", err))
                        .with_source_context(super::NORMALIZATION_CONTEXT),
                );
            }
            TokenStream::new()
        }
    };

    // Accept either a raw block `{ … }` or a sequence of statements; wrap if needed.
    // Try parse as block; if it fails, wrap as `{ <tokens> }` and parse.
    let block = match syn::parse2::<syn::Block>(tokens.clone()) {
        Ok(b) => b,
        Err(_) => {
            // Wrap as block
            let wrapped =
                TokenStream::from_str(&format!("{{ {} }}", invocation.tokens)).unwrap_or_default();
            match syn::parse2::<syn::Block>(wrapped) {
                Ok(b) => b,
                Err(err) => {
                    if let Some(manager) = diagnostics {
                        manager.add_diagnostic(
                            Diagnostic::error(format!(
                                "failed to parse emit! body as block: {}",
                                err
                            ))
                            .with_source_context(super::NORMALIZATION_CONTEXT),
                        );
                    }
                    syn::parse_quote!({})
                }
            }
        }
    };

    let parser = RustParser::new();
    let ast_block: ExprBlock = match parser.parse_block(block) {
        Ok(b) => b,
        Err(err) => {
            if let Some(manager) = diagnostics {
                manager.add_diagnostic(
                    Diagnostic::error(format!("failed to lower emit! block: {}", err))
                        .with_source_context(super::NORMALIZATION_CONTEXT),
                );
            }
            ExprBlock::new_stmts(Vec::new())
        }
    };

    let quote = ExprKind::Quote(ExprQuote {
        block: ast_block,
        kind: None,
    });
    let splice = ExprKind::Splice(ExprSplice {
        token: Box::new(Expr::from(quote)),
    });
    Expr::from(splice)
}

fn forward_parser_diagnostics(parser: &RustParser, diagnostics: Diagnostics<'_>) {
    if let Some(manager) = diagnostics {
        let parser_diags = parser.diagnostics();
        for diagnostic in parser_diags.get_diagnostics() {
            manager.add_diagnostic(diagnostic);
        }
        parser_diags.clear();
    } else {
        parser.clear_diagnostics();
    }
}

fn build_syn_expr_macro(
    invocation: &MacroInvocation,
    diagnostics: Diagnostics<'_>,
) -> syn::ExprMacro {
    let path = path_to_syn(&invocation.path);
    let delimiter = match invocation.delimiter {
        MacroDelimiter::Parenthesis => syn::MacroDelimiter::Paren(Default::default()),
        MacroDelimiter::Brace => syn::MacroDelimiter::Brace(Default::default()),
        MacroDelimiter::Bracket => syn::MacroDelimiter::Bracket(Default::default()),
    };

    let tokens = match TokenStream::from_str(&invocation.tokens) {
        Ok(stream) => stream,
        Err(err) => {
            if let Some(manager) = diagnostics {
                manager.add_diagnostic(
                    Diagnostic::warning(format!(
                        "Failed to parse macro tokens for `{}`: {}; lowering may be lossy",
                        invocation.path, err
                    ))
                    .with_source_context(super::NORMALIZATION_CONTEXT),
                );
            }
            TokenStream::new()
        }
    };

    let mac = syn::Macro {
        path,
        bang_token: Default::default(),
        delimiter,
        tokens,
    };

    syn::ExprMacro {
        attrs: Vec::new(),
        mac,
    }
}

fn path_to_syn(path: &Path) -> syn::Path {
    let mut segments = syn::punctuated::Punctuated::new();
    for ident in &path.segments {
        segments.push(syn::PathSegment {
            ident: syn::Ident::new(ident.as_str(), Span::call_site()),
            arguments: syn::PathArguments::None,
        });
    }

    syn::Path {
        leading_colon: None,
        segments,
    }
}
