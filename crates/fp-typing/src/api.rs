use crate::{HirTypeInferencer, ContextBinding, EnvEntry};
use fp_core::hir::*;
use fp_core::error::Result;
use fp_core::module::path::{ParsedPath, PathPrefix};
use std::collections::{HashMap, HashSet};

impl HirTypeInferencer {
    pub async fn infer_expression(&self, expr: &mut Expr) -> Result<()> {
        let var = self.infer_expr_inner(expr).await?;
        let ty = self.resolve_to_ty(var).await?;
        expr.set_ty(ty);
        Ok(())
    }

    pub fn push_scope(&self) {
        let mut inner = self.inner.borrow_mut();
        inner.env.push(HashMap::new());
        inner.generic_scopes.push(HashSet::new());
        inner.context_env.push(Vec::new());
        inner.current_level += 1;
    }

    pub fn pop_scope(&self) {
        let mut inner = self.inner.borrow_mut();
        inner.env.pop();
        inner.generic_scopes.pop();
        inner.context_env.pop();
        if inner.current_level > 0 {
            inner.current_level -= 1;
        }
    }

    pub async fn bind_variable(&self, name: &str, ty: Ty) {
        let type_var = match self.type_from_ast_ty(&ty).await {
            Ok(var) => var,
            Err(_) => self.fresh_type_var(),
        };
        if let Some(current_env) = self.inner.borrow_mut().env.last_mut() {
            current_env.insert(name.to_string(), EnvEntry::Mono(type_var));
        }
    }

    pub(crate) fn push_context_binding(&self, ty: Ty, expr: Expr) {
        if let Some(scope) = self.inner.borrow_mut().context_env.last_mut() {
            scope.push(ContextBinding { ty, expr });
        }
    }

    pub(crate) fn resolve_context_argument(&self, param: &FunctionParam) -> Option<Expr> {
        if !param.is_context {
            return None;
        }
        self.inner
            .borrow()
            .context_env
            .iter()
            .rev()
            .flat_map(|scope| scope.iter().rev())
            .find(|binding| binding.ty == param.ty)
            .map(|binding| binding.expr.clone())
    }

    pub(crate) fn name_tail(&self, name: &Name) -> Option<String> {
        match name {
            Name::Ident(ident) => Some(ident.as_str().to_string()),
            Name::Path(path) => path.segments.last().map(|seg| seg.as_str().to_string()),
            Name::ParameterPath(path) => path
                .segments
                .last()
                .map(|seg| seg.ident.as_str().to_string()),
        }
    }

    pub(crate) fn resolution_parsed_path(&self, name: &Name) -> Option<ParsedPath> {
        let (prefix, segments) = match name {
            Name::Ident(ident) => (PathPrefix::Plain, vec![ident.as_str().to_string()]),
            Name::Path(path) => (
                path.prefix,
                path.segments
                    .iter()
                    .map(|seg| seg.as_str().to_string())
                    .collect(),
            ),
            Name::ParameterPath(path) => (
                path.prefix,
                path.segments
                    .iter()
                    .map(|seg| seg.ident.as_str().to_string())
                    .collect(),
            ),
        };
        if segments.is_empty() && matches!(prefix, PathPrefix::Plain | PathPrefix::Root) {
            return None;
        }
        Some(ParsedPath { prefix, segments })
    }
}
