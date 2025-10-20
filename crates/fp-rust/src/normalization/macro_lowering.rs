use super::{lossy_normalization_mode, Diagnostics};
use crate::{parser::RustParser, RawExprMacro};
use fp_core::ast::{Expr, ExprMacro, MacroDelimiter, MacroInvocation, Path};
use fp_core::diagnostics::Diagnostic;
use proc_macro2::{Span, TokenStream};
use std::str::FromStr;

pub(crate) fn lower_macro_expression(macro_expr: &ExprMacro, diagnostics: Diagnostics<'_>) -> Expr {
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
