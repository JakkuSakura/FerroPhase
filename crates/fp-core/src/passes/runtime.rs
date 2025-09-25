//! Runtime pass trait for language-specific semantics
//!
//! This module defines the RuntimePass trait that allows different language
//! runtimes to provide their own value semantics on top of the core AST.

use crate::ast::{Expr, RuntimeValue, Value};
use crate::context::SharedScopedContext;
use crate::id::Ident;
use crate::{bail, Result};

/// Trait for implementing language-specific runtime semantics
pub trait RuntimePass: Send + Sync {
    /// Name of this runtime pass (e.g., "rust", "javascript", "python")
    fn name(&self) -> &str;

    /// Convert Value to RuntimeValue with language-specific semantics
    fn create_runtime_value(&self, literal: Value) -> RuntimeValue;

    /// Handle assignment with language-specific ownership rules
    fn assign(&self, target: &Expr, value: RuntimeValue, ctx: &SharedScopedContext) -> Result<()>;

    /// Handle method calls with language-specific dispatch
    fn call_method(
        &self,
        obj: RuntimeValue,
        method: &str,
        args: Vec<RuntimeValue>,
    ) -> Result<RuntimeValue>;

    /// Handle field access with language-specific rules
    fn access_field(&self, obj: RuntimeValue, field: &str) -> Result<RuntimeValue>;

    /// Handle field assignment with language-specific rules
    fn assign_field(&self, obj: &mut RuntimeValue, field: &str, value: RuntimeValue) -> Result<()> {
        // Default implementation - try to mutate the object
        let field_name = field.to_string();
        let field_value = value.get_value();
        obj.try_mutate(move |ast_value| match ast_value {
            Value::Struct(ref mut s) => {
                if let Some(struct_field) = s
                    .structural
                    .fields
                    .iter_mut()
                    .find(|f| f.name.name == field_name)
                {
                    struct_field.value = field_value;
                    Ok(())
                } else {
                    bail!("Field '{}' not found", field_name)
                }
            }
            _ => bail!("Cannot assign field to non-struct value"),
        })
    }

    /// Convert between different runtime values (e.g., to_shared, to_gc)
    fn convert_value(&self, value: RuntimeValue, target_type: &str) -> Result<RuntimeValue> {
        match target_type {
            "shared" => Ok(value.to_shared()),
            "shared_atomic" => Ok(value.to_shared_atomic()),
            "owned" => Ok(RuntimeValue::owned(value.get_value())),
            "literal" => Ok(RuntimeValue::literal(value.get_value())),
            _ => bail!("Unknown conversion target: {}", target_type),
        }
    }

    /// Check if an operation is valid in this runtime
    fn validate_operation(&self, _op: &str, _operands: &[RuntimeValue]) -> Result<()> {
        // Default: allow all operations
        Ok(())
    }

    /// Cleanup when a value goes out of scope
    fn cleanup_value(&self, _value: RuntimeValue) -> Result<()> {
        // Default: no cleanup needed
        Ok(())
    }
}

/// Helper trait for extracting information from expressions
pub trait RuntimePassHelper {
    /// Extract variable name from an expression (for borrowing)
    fn get_variable_name(&self, expr: &Expr) -> Result<String> {
        match expr {
            Expr::Locator(locator) => Ok(locator.to_string()),
            _ => bail!("Cannot extract variable name from expression"),
        }
    }

    /// Check if expression represents a mutable reference
    fn is_mutable_reference(&self, _expr: &Expr) -> bool {
        // Default: assume non-mutable
        false
    }
}

// Blanket implementation for all RuntimePass implementations
impl<T: RuntimePass> RuntimePassHelper for T {}

/// Runtime pass error types
#[derive(Debug, Clone)]
pub enum RuntimePassError {
    InvalidAssignment(String),
    MethodNotFound(String),
    FieldNotFound(String),
    OwnershipViolation(String),
    ConversionFailed(String),
}

impl std::fmt::Display for RuntimePassError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimePassError::InvalidAssignment(msg) => write!(f, "Invalid assignment: {}", msg),
            RuntimePassError::MethodNotFound(method) => write!(f, "Method '{}' not found", method),
            RuntimePassError::FieldNotFound(field) => write!(f, "Field '{}' not found", field),
            RuntimePassError::OwnershipViolation(msg) => write!(f, "Ownership violation: {}", msg),
            RuntimePassError::ConversionFailed(msg) => write!(f, "Conversion failed: {}", msg),
        }
    }
}

impl std::error::Error for RuntimePassError {}

/// Default literal-only runtime pass (no ownership semantics)
#[derive(Debug, Clone)]
pub struct LiteralRuntimePass;

impl RuntimePass for LiteralRuntimePass {
    fn name(&self) -> &str {
        "literal"
    }

    fn create_runtime_value(&self, literal: Value) -> RuntimeValue {
        RuntimeValue::literal(literal)
    }

    fn assign(
        &self,
        _target: &Expr,
        _value: RuntimeValue,
        _ctx: &SharedScopedContext,
    ) -> Result<()> {
        // Literal runtime doesn't support assignment
        bail!("Assignment not supported in literal runtime")
    }

    fn call_method(
        &self,
        obj: RuntimeValue,
        method: &str,
        _args: Vec<RuntimeValue>,
    ) -> Result<RuntimeValue> {
        // Basic method support for literals
        match method {
            "clone" => Ok(RuntimeValue::literal(obj.get_value())),
            "to_string" => Ok(RuntimeValue::literal(Value::string(format!(
                "{}",
                obj.get_value()
            )))),
            _ => bail!("Method '{}' not supported in literal runtime", method),
        }
    }

    fn access_field(&self, obj: RuntimeValue, field: &str) -> Result<RuntimeValue> {
        let value = obj.get_value();
        match value {
            Value::Struct(s) => {
                if let Some(field_value) = s.structural.get_field(&Ident::new(field)) {
                    Ok(RuntimeValue::literal(field_value.value.clone()))
                } else {
                    bail!("Field '{}' not found", field)
                }
            }
            _ => bail!("Cannot access field on non-struct value"),
        }
    }
}

impl Default for LiteralRuntimePass {
    fn default() -> Self {
        LiteralRuntimePass
    }
}
