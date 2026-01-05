use eyre;
use itertools::Itertools;
use proc_macro2::TokenStream;
use quote::*;

const RUST_KEYWORDS: &[&str] = &[
    "as", "break", "const", "continue", "crate", "else", "enum", "extern", "false", "fn", "for",
    "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut", "pub", "ref", "return",
    "self", "Self", "static", "struct", "super", "trait", "true", "type", "unsafe", "use", "where",
    "while", "async", "await", "dyn", "abstract", "become", "box", "do", "final", "macro",
    "override", "priv", "typeof", "unsized", "virtual", "yield",
];

use crate::{RawExpr, RawExprMacro, RawItemMacro, RawStmtMacro};
use fp_core::ast::*;
use fp_core::ast::{Ident, Locator, ParameterPath, ParameterPathSegment, Path};
use fp_core::bail;
use fp_core::printer::AstSerializerConfig;
use fp_core::utils::anybox::AnyBox;
use fp_core::{Error, Result};

fn fallback_layout(code: &str) -> String {
    let mut out = String::with_capacity(code.len() + 16);
    let mut indent = 0usize;
    let indent_unit = "    ";
    let mut in_string = false;
    let mut escape = false;
    let mut chars = code.chars().peekable();

    while let Some(ch) = chars.next() {
        if in_string {
            out.push(ch);
            if escape {
                escape = false;
            } else if ch == '\\' {
                escape = true;
            } else if ch == '"' {
                in_string = false;
            }
            continue;
        }

        match ch {
            '"' => {
                ensure_indent(&mut out, indent, indent_unit);
                out.push('"');
                in_string = true;
                escape = false;
            }
            '{' => {
                trim_trailing_space(&mut out);
                ensure_indent(&mut out, indent, indent_unit);
                out.push('{');
                indent += 1;
                out.push('\n');
            }
            '}' => {
                indent = indent.saturating_sub(1);
                trim_trailing_space(&mut out);
                if !out.is_empty() && last_char(&out) != Some('\n') {
                    out.push('\n');
                }
                ensure_indent(&mut out, indent, indent_unit);
                out.push('}');
                let mut lookahead = chars.clone();
                let mut next_non_ws = None;
                while let Some(next) = lookahead.next() {
                    if next.is_whitespace() {
                        continue;
                    }
                    next_non_ws = Some(next);
                    break;
                }
                if matches!(next_non_ws, Some(';')) {
                    // let semicolon branch handle newline
                } else if next_non_ws.is_some() {
                    out.push('\n');
                }
            }
            ';' => {
                out.push(';');
                out.push('\n');
            }
            ',' => {
                out.push(',');
                if let Some(next) = chars.peek() {
                    if !matches!(next, &' ' | &')' | &']' | &'}') {
                        out.push(' ');
                    }
                }
            }
            '\n' | '\r' => {}
            ' ' | '\t' => {
                if let Some(prev) = last_char(&out) {
                    if !prev.is_whitespace() && !matches!(prev, '{' | '(' | '[' | '}') {
                        out.push(' ');
                    }
                }
            }
            _ => {
                ensure_indent(&mut out, indent, indent_unit);
                out.push(ch);
            }
        }
    }

    let mut formatted = out.trim_end().to_string();
    if !formatted.is_empty() && !formatted.ends_with('\n') {
        formatted.push('\n');
    }
    formatted
}

fn ensure_indent(buf: &mut String, indent: usize, indent_unit: &str) {
    if buf.is_empty() || matches!(last_char(buf), Some('\n')) {
        for _ in 0..indent {
            buf.push_str(indent_unit);
        }
    }
}

fn trim_trailing_space(buf: &mut String) {
    while matches!(last_char(buf), Some(' ' | '\t')) {
        buf.pop();
    }
}

fn last_char(buf: &str) -> Option<char> {
    buf.chars().rev().next()
}

#[cfg(test)]
mod tests {
    use super::fallback_layout;

    #[test]
    fn fallback_layout_inserts_newlines() {
        let formatted = fallback_layout("{ const A : i32 = 1 ; const B : i32 = 2 ; }");
        let lines: Vec<_> = formatted.lines().collect();
        assert!(lines.len() > 3, "expected multiple lines, got: {formatted}");
        assert!(lines.iter().any(|line| line.contains("const A")));
        assert!(lines.iter().any(|line| line.contains("const B")));
    }
}

mod attr;
mod expr;
mod item;
mod ty;
mod value;

pub mod rustfmt;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct RustPrinter {
    pub rustfmt: bool,
    pub config: AstSerializerConfig,
}

impl RustPrinter {
    pub fn new() -> Self {
        Self {
            rustfmt: false,
            config: AstSerializerConfig::standard(),
        }
    }

    pub fn new_with_rustfmt() -> Self {
        let mut printer = Self::new();
        printer.set_rustfmt(true);
        printer
    }

    pub fn with_config(config: AstSerializerConfig) -> Self {
        Self {
            rustfmt: false,
            config,
        }
    }

    pub fn config(&self) -> &AstSerializerConfig {
        &self.config
    }
    pub fn set_rustfmt(&mut self, rustfmt: bool) {
        self.rustfmt = rustfmt;
    }
    pub fn maybe_rustfmt_token_stream(&self, code: &TokenStream) -> fp_core::error::Result<String> {
        self.maybe_rustfmt(&code.to_string())
    }
    pub fn maybe_rustfmt(&self, code: &str) -> fp_core::error::Result<String> {
        if self.rustfmt {
            if let Ok(ok) = rustfmt::format_code(code) {
                return Ok(ok);
            }
        }

        Ok(fallback_layout(code))
    }
    pub fn print_ident(&self, i: &Ident) -> TokenStream {
        let raw = i.as_str();
        match raw {
            "+" => quote!(+),
            "*" => quote!(*),
            ">" => quote!(>),
            ">=" => quote!(>=),
            "<" => quote!(<),
            "<=" => quote!(<=),
            "==" => quote!(==),
            "!=" => quote!(!=),
            "|" => quote!(|),
            "self" | "Self" => format_ident!("{}", raw).into_token_stream(),
            a if RUST_KEYWORDS.contains(&a) => format_ident!("r#{}", raw).into_token_stream(),
            _ => format_ident!("{}", raw).into_token_stream(),
        }
    }
    pub fn print_pat_ident(&self, i: &PatternIdent) -> Result<TokenStream> {
        let mut_ = if i.mutability.unwrap_or_default() {
            quote!(mut)
        } else {
            quote!()
        };
        let name = self.print_ident(&i.ident);
        Ok(quote!(#mut_ #name))
    }
    pub fn print_trait_bound(&self, n: &ItemDefTrait) -> Result<TokenStream> {
        let name = self.print_ident(&n.name);
        let bounds = self.print_type_bounds(&n.bounds)?;
        Ok(quote!(
            #name: #bounds
        ))
    }

    pub fn print_pattern(&self, pat: &Pattern) -> Result<TokenStream> {
        match pat.kind() {
            PatternKind::Ident(ident) => self.print_pat_ident(ident),
            PatternKind::Bind(bind) => {
                let name = self.print_pat_ident(&bind.ident)?;
                let inner = self.print_pattern(&bind.pattern)?;
                Ok(quote!(#name @ #inner))
            }
            PatternKind::Tuple(tuple) => {
                let elems: Vec<_> = tuple
                    .patterns
                    .iter()
                    .map(|p| self.print_pattern(p))
                    .try_collect()?;
                match elems.len() {
                    0 => Ok(quote!(())),
                    1 => {
                        let elem = &elems[0];
                        Ok(quote!((#elem,)))
                    }
                    _ => Ok(quote!((#(#elems),*))),
                }
            }
            PatternKind::TupleStruct(tuple) => {
                let name = self.print_locator(&tuple.name)?;
                let elems: Vec<_> = tuple
                    .patterns
                    .iter()
                    .map(|p| self.print_pattern(p))
                    .try_collect()?;
                Ok(quote!(#name(#(#elems), *)))
            }
            PatternKind::Struct(stru) => {
                let name = self.print_ident(&stru.name);
                let fields: Vec<_> = stru
                    .fields
                    .iter()
                    .map(|field| {
                        let ident = self.print_ident(&field.name);
                        if let Some(rename) = &field.rename {
                            let inner = self.print_pattern(rename)?;
                            Ok::<_, Error>(quote!(#ident: #inner))
                        } else {
                            Ok::<_, Error>(quote!(#ident))
                        }
                    })
                    .try_collect()?;
                if stru.has_rest {
                    if fields.is_empty() {
                        Ok(quote!(#name { .. }))
                    } else {
                        Ok(quote!(#name { #(#fields), *, .. }))
                    }
                } else {
                    Ok(quote!(#name { #(#fields), * }))
                }
            }
            PatternKind::Structural(stru) => {
                let fields: Vec<_> = stru
                    .fields
                    .iter()
                    .map(|field| {
                        let ident = self.print_ident(&field.name);
                        if let Some(rename) = &field.rename {
                            let inner = self.print_pattern(rename)?;
                            Ok::<_, Error>(quote!(#ident: #inner))
                        } else {
                            Ok::<_, Error>(quote!(#ident))
                        }
                    })
                    .try_collect()?;
                if stru.has_rest {
                    if fields.is_empty() {
                        Ok(quote!({ .. }))
                    } else {
                        Ok(quote!({ #(#fields), *, .. }))
                    }
                } else {
                    Ok(quote!({ #(#fields), * }))
                }
            }
            PatternKind::Box(box_pat) => {
                let inner = self.print_pattern(&box_pat.pattern)?;
                Ok(quote!(box #inner))
            }
            PatternKind::Variant(variant) => {
                let name = self.print_expr(&variant.name)?;
                if let Some(pattern) = &variant.pattern {
                    let inner = self.print_pattern(pattern)?;
                    match pattern.kind() {
                        PatternKind::Structural(_) | PatternKind::Struct(_) => {
                            Ok(quote!(#name #inner))
                        }
                        _ => Ok(quote!(#name(#inner))),
                    }
                } else {
                    Ok(quote!(#name))
                }
            }
            PatternKind::Type(pattern_type) => {
                let pat_tokens = self.print_pattern(&pattern_type.pat)?;
                let ty_tokens = self.print_type(&pattern_type.ty)?;
                Ok(quote!(#pat_tokens: #ty_tokens))
            }
            PatternKind::Quote(_) => Ok(quote!(_)),
            PatternKind::Wildcard(_) => Ok(quote!(_)),
        }
    }

    pub fn print_vis(&self, vis: &Visibility) -> TokenStream {
        match vis {
            Visibility::Public => quote!(pub),
            Visibility::Crate => quote!(pub(crate)),
            Visibility::Restricted(_) => quote!(pub(crate)),
            Visibility::Private => quote!(),
            Visibility::Inherited => quote!(),
        }
    }
    pub fn print_receiver(&self, receiver: &FunctionParamReceiver) -> Result<TokenStream> {
        match receiver {
            FunctionParamReceiver::Implicit => bail!("Implicit receiver not supported"),
            FunctionParamReceiver::Value => Ok(quote!(self)),
            FunctionParamReceiver::MutValue => Ok(quote!(mut self)),
            FunctionParamReceiver::Ref => Ok(quote!(&self)),
            FunctionParamReceiver::RefStatic => Ok(quote!(&'static self)),
            FunctionParamReceiver::RefMut => Ok(quote!(&mut self)),
            FunctionParamReceiver::RefMutStatic => Ok(quote!(&'static mut self)),
        }
    }
    pub fn print_function(
        &self,
        sig: &FunctionSignature,
        body: &Expr,
        vis: &Visibility,
    ) -> Result<TokenStream> {
        let name = if let Some(name) = &sig.name {
            self.print_ident(name)
        } else {
            quote!()
        };
        let closure_return_ty: Option<&fp_core::ast::TypeFunction> = match body.kind() {
            ExprKind::Closure(_) => match body.ty() {
                Some(Ty::Function(fun_ty)) => Some(fun_ty),
                _ => None,
            },
            ExprKind::Block(block) => block
                .last_expr()
                .and_then(|expr| matches!(expr.kind(), ExprKind::Closure(_)).then_some(expr))
                .and_then(|expr| match expr.ty() {
                    Some(Ty::Function(fun_ty)) => Some(fun_ty),
                    _ => None,
                }),
            _ => None,
        };

        let ret = if let Some(fun_ty) = closure_return_ty {
            let args: Vec<_> = fun_ty
                .params
                .iter()
                .map(|ty| self.print_type(ty))
                .try_collect()?;
            let ret_ty = fun_ty.ret_ty.as_deref().unwrap_or(&Ty::UNIT);
            let ret_ty = self.print_type(ret_ty)?;
            quote!(-> impl Fn(#(#args),*) -> #ret_ty)
        } else {
            self.print_return_type(sig.ret_ty.as_ref())?
        };
        let receiver = if let Some(receiver) = &sig.receiver {
            let rec = self.print_receiver(receiver)?;
            quote!(#rec,)
        } else {
            quote!()
        };
        let param_names: Vec<_> = sig
            .params
            .iter()
            .map(|x| self.print_ident(&x.name))
            .collect();
        let param_types: Vec<_> = sig
            .params
            .iter()
            .map(|x| self.print_type(&x.ty))
            .try_collect()?;
        let stmts = self.print_expr_no_braces(&body)?;
        let gg;
        if !sig.generics_params.is_empty() {
            let gt: Vec<_> = sig
                .generics_params
                .iter()
                .map(|x| self.print_ident(&x.name))
                .collect();
            let gb: Vec<_> = sig
                .generics_params
                .iter()
                .map(|x| self.print_type_bounds(&x.bounds))
                .try_collect()?;
            gg = quote!(<#(#gt: #gb), *>)
        } else {
            gg = quote!();
        }
        let vis = self.print_vis(vis);
        // let attrs = self.print_attrs(&func.attrs)?;
        return Ok(quote!(
            // #attrs
            #vis fn #name #gg(#receiver #(#param_names: #param_types), *) #ret {
                #stmts
            }
        ));
    }
    pub fn print_value_function(
        &self,
        fun: &ValueFunction,
        vis: &Visibility,
    ) -> Result<TokenStream> {
        let sig = &fun.sig;
        let body = &fun.body;
        self.print_function(sig, body, vis)
    }
    pub fn print_func_type_param(&self, param: &FunctionParam) -> Result<TokenStream> {
        let name = self.print_ident(&param.name);
        let ty = if let Ty::Slice(slice) = &param.ty {
            let elem = self.print_type(slice.elem.as_ref())?;
            quote!(&[#elem])
        } else {
            self.print_type(&param.ty)?
        };
        Ok(quote!(#name: #ty))
    }
    pub fn print_return_type(&self, node: Option<&Ty>) -> Result<TokenStream> {
        if let Some(node) = node {
            // `&str` without an input lifetime is rejected by Rust (no elision target).
            // When FP uses string literals, the most reasonable mapping is `&'static str`.
            if let Ty::Reference(reference) = node {
                if reference.lifetime.is_none() && reference.mutability != Some(true) {
                    if let Ty::Expr(expr) = reference.ty.as_ref() {
                        if let fp_core::ast::ExprKind::Locator(locator) = expr.kind() {
                            let is_str = match locator {
                                fp_core::ast::Locator::Ident(id) => id.as_str() == "str",
                                fp_core::ast::Locator::Path(path) => {
                                    path.segments.last().is_some_and(|s| s.as_str() == "str")
                                }
                                fp_core::ast::Locator::ParameterPath(path) => path
                                    .segments
                                    .last()
                                    .is_some_and(|s| s.ident.as_str() == "str"),
                            };
                            if is_str {
                                return Ok(quote!(-> &'static str));
                            }
                        }
                    }
                }
            }

            // Some frontends represent `&str` as a type expression (`Ty::Expr`) instead
            // of a `Ty::Reference`. Handle that representation in return position too.
            if let Ty::Expr(expr) = node {
                if let fp_core::ast::ExprKind::Reference(reference) = expr.kind() {
                    if reference.mutable != Some(true) {
                        if let fp_core::ast::ExprKind::Locator(locator) = reference.referee.kind() {
                            let is_str = match locator {
                                fp_core::ast::Locator::Ident(id) => id.as_str() == "str",
                                fp_core::ast::Locator::Path(path) => {
                                    path.segments.last().is_some_and(|s| s.as_str() == "str")
                                }
                                fp_core::ast::Locator::ParameterPath(path) => path
                                    .segments
                                    .last()
                                    .is_some_and(|s| s.ident.as_str() == "str"),
                            };
                            if is_str {
                                return Ok(quote!(-> &'static str));
                            }
                        }
                    }
                }
            }
            let ty = self.print_type(node)?;
            Ok(quote!(-> #ty))
        } else {
            Ok(quote!())
        }
    }
    pub fn print_func_value(&self, fun: &ValueFunction) -> Result<TokenStream> {
        self.print_value_function(fun, &Visibility::Private)
    }
    pub fn print_func_type(&self, fun: &TypeFunction) -> Result<TokenStream> {
        let args: Vec<_> = fun
            .params
            .iter()
            .map(|x| self.print_type(x))
            .try_collect()?;
        let ret = self.print_return_type(fun.ret_ty.as_deref())?;
        Ok(quote!(
            fn(#(#args), *) #ret
        ))
    }
    pub fn print_module(&self, m: &Module) -> Result<TokenStream> {
        let stmts = self.print_items_chunk(&m.items)?;

        let mod_name = format_ident!("{}", m.name.as_str());
        Ok(quote!(
            pub mod #mod_name {
                #stmts
            }
        ))
    }
    pub fn print_import(&self, node: &ItemImport) -> Result<TokenStream> {
        let import: syn::UseTree =
            syn::parse_str(&node.tree.to_string()).map_err(|e| eyre::eyre!(e.to_string()))?;
        let vis = self.print_vis(&node.visibility);

        Ok(quote!(#vis use #import;))
    }
    pub fn print_field_value(&self, s: &ValueField) -> Result<TokenStream> {
        let name = self.print_ident(&s.name);
        let value = self.print_value(&s.value)?;
        Ok(quote!(#name: #value))
    }

    pub fn print_any(&self, n: &AnyBox) -> Result<TokenStream> {
        if let Some(n) = n.downcast_ref::<RawExprMacro>() {
            return Ok(n.raw.to_token_stream());
        }
        if let Some(n) = n.downcast_ref::<RawExpr>() {
            return Ok(n.raw.to_token_stream());
        }
        if let Some(n) = n.downcast_ref::<RawStmtMacro>() {
            return Ok(n.raw.to_token_stream());
        }
        if let Some(n) = n.downcast_ref::<RawItemMacro>() {
            return Ok(n.raw.to_token_stream());
        }
        bail!("Not supported {:?}", n)
    }
    pub fn print_impl_traits(&self, traits: &ImplTraits) -> Result<TokenStream> {
        let bounds = self.print_type_bounds(&traits.bounds)?;
        Ok(quote!(impl #bounds))
    }
    pub fn print_type_bounds(&self, bounds: &TypeBounds) -> Result<TokenStream> {
        let bounds: Vec<_> = bounds
            .bounds
            .iter()
            .map(|x| self.print_expr(&x))
            .try_collect()?;
        Ok(quote!(#(#bounds)+ *))
    }

    pub fn print_path(&self, path: &Path) -> TokenStream {
        let segments: Vec<_> = path.segments.iter().map(|x| self.print_ident(x)).collect();
        quote!(#(#segments)::*)
    }
    fn print_parameter_path_segment(&self, segment: &ParameterPathSegment) -> Result<TokenStream> {
        let ident = self.print_ident(&segment.ident);
        if segment.args.is_empty() {
            return Ok(ident);
        }
        let args: Vec<_> = segment
            .args
            .iter()
            .map(|x| self.print_type(x))
            .try_collect()?;
        Ok(quote!(#ident::<#(#args), *>))
    }
    pub fn print_parameter_path(&self, path: &ParameterPath) -> Result<TokenStream> {
        let segments: Vec<_> = path
            .segments
            .iter()
            .map(|x| self.print_parameter_path_segment(x))
            .try_collect()?;
        Ok(quote!(#(#segments)::*))
    }
    pub fn print_locator(&self, pat: &Locator) -> Result<TokenStream> {
        Ok(match pat {
            Locator::Ident(n) => self.print_ident(n),
            Locator::Path(n) => self.print_path(n),
            Locator::ParameterPath(n) => self.print_parameter_path(n)?,
        })
    }

    pub fn print_file(&self, file: &File) -> Result<TokenStream> {
        let items = self.print_items_chunk(&file.items)?;
        Ok(quote!(#items))
    }
    pub fn print_node(&self, node: &Node) -> fp_core::Result<TokenStream> {
        match node.kind() {
            NodeKind::Item(n) => self.print_item(n),
            NodeKind::Expr(n) => self.print_expr(n),
            NodeKind::File(n) => self.print_file(n),
            NodeKind::Query(_) => Err(fp_core::error::Error::Generic(eyre::eyre!(
                "Query documents cannot be printed as Rust tokens"
            ))),
            NodeKind::Schema(_) => Err(fp_core::error::Error::Generic(eyre::eyre!(
                "Schema documents cannot be printed as Rust tokens"
            ))),
            NodeKind::Workspace(_) => Err(fp_core::error::Error::Generic(eyre::eyre!(
                "Workspace snapshots cannot be printed as Rust tokens"
            ))),
        }
    }
}

impl AstSerializer for RustPrinter {
    fn serialize_node(&self, node: &Node) -> fp_core::error::Result<String> {
        self.print_node(node)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))
    }

    fn serialize_expr(&self, node: &Expr) -> fp_core::error::Result<String> {
        Ok(self
            .print_expr(node)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))?)
    }

    fn serialize_invoke(&self, node: &ExprInvoke) -> fp_core::error::Result<String> {
        Ok(self
            .print_invoke(node)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))?)
    }

    fn serialize_item(&self, node: &Item) -> fp_core::error::Result<String> {
        Ok(self
            .print_item(node)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))?)
    }

    fn serialize_block(&self, node: &ExprBlock) -> fp_core::error::Result<String> {
        Ok(self
            .print_block(node)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))?)
    }

    fn serialize_file(&self, node: &File) -> fp_core::error::Result<String> {
        Ok(self
            .print_file(node)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))?)
    }
    fn serialize_module(&self, node: &Module) -> fp_core::error::Result<String> {
        Ok(self
            .print_module(node)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))?)
    }

    fn serialize_value(&self, node: &Value) -> fp_core::error::Result<String> {
        self.print_value(node)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))
    }

    fn serialize_type(&self, node: &Ty) -> fp_core::error::Result<String> {
        self.print_type(node)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))
    }

    fn serialize_stmt(&self, node: &BlockStmt) -> fp_core::error::Result<String> {
        self.print_statement(node)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))
    }

    fn serialize_value_function(&self, node: &ValueFunction) -> fp_core::error::Result<String> {
        self.print_value_function(node, &Visibility::Private)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))
    }
    fn serialize_def_function(&self, node: &ItemDefFunction) -> fp_core::error::Result<String> {
        self.print_def_function(node)
            .and_then(|x| self.maybe_rustfmt_token_stream(&x))
    }
}
