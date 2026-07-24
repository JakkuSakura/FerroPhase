use std::collections::HashMap;

use crate::ast::{Expr, ExprId, Value};

pub trait ExprResolution {
    fn source_expr(&self, expr_id: ExprId) -> Option<&Expr>;
    fn resolved_value(&self, expr_id: ExprId) -> Option<&Value>;
}

#[derive(Debug, Clone, Default)]
pub struct ExprResolutionTable {
    entries: HashMap<ExprId, ExprResolutionEntry>,
}

#[derive(Debug, Clone)]
pub struct ExprResolutionEntry {
    pub source: Expr,
    pub value: Option<Value>,
}

impl ExprResolutionTable {
    pub fn insert_source(&mut self, expr_id: ExprId, source: Expr) {
        self.entries
            .entry(expr_id)
            .and_modify(|entry| entry.source = source.clone())
            .or_insert(ExprResolutionEntry {
                source,
                value: None,
            });
    }

    pub fn insert_value(&mut self, expr_id: ExprId, value: Value) {
        self.entries
            .entry(expr_id)
            .and_modify(|entry| entry.value = Some(value.clone()))
            .or_insert(ExprResolutionEntry {
                source: Expr::unit(),
                value: Some(value),
            });
    }
}

impl ExprResolution for ExprResolutionTable {
    fn source_expr(&self, expr_id: ExprId) -> Option<&Expr> {
        self.entries.get(&expr_id).map(|entry| &entry.source)
    }

    fn resolved_value(&self, expr_id: ExprId) -> Option<&Value> {
        self.entries
            .get(&expr_id)
            .and_then(|entry| entry.value.as_ref())
    }
}
