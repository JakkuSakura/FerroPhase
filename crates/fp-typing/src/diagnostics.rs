use crate::unify::TypeVarKind;
use crate::{AstTypeInferencer, BoxFuture, TypeVarId, TypingDiagnostic};
use fp_core::ast::*;
use fp_core::diagnostics::Diagnostic;
use fp_core::error::{Error, Result};
use fp_core::module::path::QualifiedPath;
use fp_core::span::Span;

impl AstTypeInferencer {
    pub(crate) fn emit_error(&self, message: impl Into<String>) {
        let span = self.inner.borrow().current_span;
        self.emit_error_with_span(span, message);
    }

    pub(crate) fn emit_error_with_span(&self, span: Option<Span>, message: impl Into<String>) {
        let message = message.into();
        let mut inner = self.inner.borrow_mut();
        if inner.lossy_mode {
            if let Some(span) = span {
                inner
                    .diagnostics
                    .push(TypingDiagnostic::warning_with_span(message, span));
            } else {
                inner.diagnostics.push(TypingDiagnostic::warning(message));
            }
        } else {
            inner.has_errors = true;
            if let Some(span) = span {
                inner
                    .diagnostics
                    .push(TypingDiagnostic::error_with_span(message, span));
            } else {
                inner.diagnostics.push(TypingDiagnostic::error(message));
            }
        }
    }

    pub(crate) fn span_option(&self, span: Span) -> Option<Span> {
        (!span.is_null()).then_some(span)
    }

    pub(crate) fn span_or_previous(&self, span: Span, previous: Option<Span>) -> Option<Span> {
        if span.is_null() {
            previous
        } else {
            Some(span)
        }
    }

    pub(crate) fn error_with_span(&self, err: Error, span: Option<Span>) -> Error {
        let Some(span) = span else { return err };
        if let Error::Diagnostic(ref diagnostic) = err {
            if diagnostic.span.is_some() {
                return err;
            }
        }
        Error::diagnostic(Diagnostic::error(err.to_string()).with_span(span))
    }

    pub(crate) fn error_with_current_span(&self, message: impl Into<String>) -> Error {
        let message = message.into();
        let span = self.inner.borrow().current_span;
        if let Some(span) = span {
            Error::diagnostic(Diagnostic::error(message).with_span(span))
        } else {
            Error::from(message)
        }
    }

    pub(crate) fn emit_warning(&self, message: impl Into<String>) {
        self.inner
            .borrow_mut()
            .diagnostics
            .push(TypingDiagnostic::warning(message));
    }

    pub(crate) fn error_type_var(&self) -> TypeVarId {
        let var = self.fresh_type_var();
        self.bind_error(var);
        var
    }

    pub(crate) fn expect_reference<'a>(
        &self,
        var: TypeVarId,
        context: &'a str,
    ) -> BoxFuture<'a, Result<TypeVarId>> {
        let this = self.clone();
        Box::pin(async move {
            let root = this.find(var);
            let root_kind = this.inner.borrow().type_vars[root].kind.clone();
            match root_kind {
                TypeVarKind::Unbound { .. } => {
                    let inner = this.fresh_type_var();
                    this.inner.borrow_mut().type_vars[root].kind =
                        TypeVarKind::Bound(Ty::Reference(TypeReference {
                            ty: Box::new(Ty::infer_var(inner)),
                            mutability: None,
                            lifetime: None,
                        }));
                    Ok(inner)
                }
                TypeVarKind::Bound(Ty::Reference(reference)) => match reference.ty.as_ref() {
                    Ty::InferVar(infer) => Ok(infer.id),
                    other => this.type_from_ast_ty(other).await,
                },
                TypeVarKind::Link(next) => this.expect_reference(next, context).await,
                _other => {
                    this.emit_error(format!(
                        "expected reference value for {} (hint: add `&`/`&mut` or change the annotation to a non-reference type)",
                        context
                    ));
                    let placeholder = this.error_type_var();
                    this.inner.borrow_mut().type_vars[root].kind =
                        TypeVarKind::Bound(Ty::Reference(TypeReference {
                            ty: Box::new(Ty::infer_var(placeholder)),
                            mutability: None,
                            lifetime: None,
                        }));
                    Ok(placeholder)
                }
            }
        })
    }

    pub(crate) fn ty_from_function_signature(&self, sig: &FunctionSignature) -> Result<Ty> {
        self.validate_extern_c_signature(sig);
        let params = sig.params.iter().map(|param| param.ty.clone()).collect();
        let ret_ty = sig.ret_ty.clone().unwrap_or_else(|| Ty::Unit(TypeUnit));
        Ok(Ty::Function(TypeFunction {
            params,
            generics_params: sig.generics_params.clone(),
            ret_ty: Some(Box::new(ret_ty)),
        }))
    }

    pub(crate) fn validate_extern_c_signature(&self, sig: &FunctionSignature) {
        if !sig.abi.is_c() {
            return;
        }
        for param in &sig.params {
            if self.is_disallowed_c_string_type(&param.ty) {
                self.emit_error(format!(
                    "extern \"C\" functions must use &CStr for string parameters: {}",
                    param.name
                ));
            }
        }
        if let Some(ret_ty) = &sig.ret_ty {
            if self.is_disallowed_c_string_type(ret_ty) {
                self.emit_error("extern \"C\" functions must use &CStr for string return types");
            }
        }
    }

    fn is_disallowed_c_string_type(&self, ty: &Ty) -> bool {
        if self.is_cstr_reference(ty) {
            return false;
        }
        if self.is_string_like_type(ty) {
            return true;
        }
        matches!(ty, Ty::Reference(reference) if self.is_string_like_type(reference.ty.as_ref()))
    }

    fn is_cstr_reference(&self, ty: &Ty) -> bool {
        let Ty::Reference(reference) = ty else {
            return false;
        };
        self.type_name(reference.ty.as_ref()) == Some("CStr")
    }

    pub(crate) fn is_string_like_type(&self, ty: &Ty) -> bool {
        match ty {
            Ty::Primitive(TypePrimitive::String) => true,
            _ => matches!(
                self.type_name(ty),
                Some("str") | Some("String") | Some("string")
            ),
        }
    }

    fn type_name<'a>(&self, ty: &'a Ty) -> Option<&'a str> {
        match ty {
            Ty::Struct(struct_ty) => Some(struct_ty.name.as_str()),
            Ty::Expr(expr) => match expr.kind() {
                ExprKind::Name(name) => match name {
                    Name::Ident(ident) => Some(ident.as_str()),
                    Name::Path(path) => path.segments.last().map(|seg| seg.as_str()),
                    Name::ParameterPath(path) => path.last().map(|seg| seg.ident.as_str()),
                },
                _ => None,
            },
            _ => None,
        }
    }

    pub(crate) fn struct_name_from_expr(&self, expr: &Expr) -> Option<QualifiedPath> {
        match expr.kind() {
            ExprKind::Name(name) => {
                let name = match name {
                    Name::ParameterPath(path) => path
                        .segments
                        .last()
                        .map(|seg| seg.ident.as_str().to_string())?,
                    Name::Path(path) => path.segments.last().map(|seg| seg.as_str().to_string())?,
                    Name::Ident(ident) => ident.as_str().to_string(),
                };
                if name == "Self" {
                    self.inner
                        .borrow()
                        .impl_stack
                        .last()
                        .and_then(|ctx| ctx.as_ref())
                        .map(|ctx| ctx.struct_name.clone())
                } else {
                    Some(QualifiedPath::new(vec![name]))
                }
            }
            ExprKind::Value(value) => match &**value {
                Value::Type(Ty::Struct(struct_ty)) => Some(QualifiedPath::new(vec![struct_ty
                    .name
                    .as_str()
                    .to_string()])),
                Value::Type(Ty::Enum(enum_ty)) => {
                    Some(QualifiedPath::new(vec![enum_ty.name.as_str().to_string()]))
                }
                _ => None,
            },
            _ => None,
        }
    }
}
