//! Rust-style runtime pass with ownership semantics
//! 
//! This runtime pass implements Rust's ownership model:
//! - Values are owned by default
//! - References can be borrowed (immutable or mutable)  
//! - Move semantics prevent use-after-move
//! - Borrow checker prevents data races

use crate::ast::{AstValue, AstExpr, RuntimeValue};
use crate::context::SharedScopedContext;
use crate::passes::{RuntimePass, RuntimePassHelper};
use crate::id::{Ident, Locator};
use crate::{Result, bail};

/// Rust-style runtime pass implementing ownership semantics
#[derive(Debug, Clone)]
pub struct RustRuntimePass {
    /// Track moved values to prevent use-after-move
    moved_values: std::collections::HashSet<String>,
    /// Track borrowed values
    borrowed_values: std::collections::HashMap<String, BorrowInfo>,
}

#[derive(Debug, Clone)]
struct BorrowInfo {
    is_mutable: bool,
    source: String,
}

impl RustRuntimePass {
    /// Create a new Rust runtime pass
    pub fn new() -> Self {
        Self {
            moved_values: std::collections::HashSet::new(),
            borrowed_values: std::collections::HashMap::new(),
        }
    }
    
    /// Check if a variable has been moved
    fn is_moved(&self, var_name: &str) -> bool {
        self.moved_values.contains(var_name)
    }
    
    /// Mark a variable as moved
    fn mark_moved(&mut self, var_name: String) {
        self.moved_values.insert(var_name);
    }
    
    /// Check if a variable is currently borrowed
    fn is_borrowed(&self, var_name: &str) -> bool {
        self.borrowed_values.contains_key(var_name)
    }
    
    /// Add a borrow
    fn add_borrow(&mut self, var_name: String, is_mutable: bool, source: String) {
        self.borrowed_values.insert(var_name, BorrowInfo { is_mutable, source });
    }
    
    /// Remove a borrow
    fn remove_borrow(&mut self, var_name: &str) {
        self.borrowed_values.remove(var_name);
    }
}

impl RuntimePass for RustRuntimePass {
    fn name(&self) -> &str {
        "rust"
    }
    
    fn create_runtime_value(&self, literal: AstValue) -> RuntimeValue {
        // Rust defaults to owned values
        RuntimeValue::owned(literal)
    }
    
    fn assign(&self, target: &AstExpr, value: RuntimeValue, ctx: &SharedScopedContext) -> Result<()> {
        match target {
            AstExpr::Locator(Locator::Ident(ident)) => {
                let var_name = &ident.name;
                
                // Check if target is already borrowed
                if self.is_borrowed(var_name) {
                    bail!("Cannot assign to borrowed variable '{}'", var_name);
                }
                
                // Move assignment - the old value is invalidated
                // Store the new runtime value in context
                ctx.insert_runtime_value(ident.clone(), value);
                Ok(())
            },
            AstExpr::Select(select) => {
                // Field assignment requires mutable access to the object
                let obj_name = self.get_variable_name(&select.obj)?;
                
                if self.is_moved(&obj_name) {
                    bail!("Use of moved value '{}'", obj_name);
                }
                
                if let Some(mut obj) = ctx.get_runtime_value_mut(&obj_name) {
                    self.assign_field(&mut obj, &select.field.name, value)?;
                    Ok(())
                } else {
                    bail!("Variable '{}' not found", obj_name);
                }
            },
            _ => bail!("Unsupported assignment target"),
        }
    }
    
    fn call_method(&self, obj: RuntimeValue, method: &str, args: Vec<RuntimeValue>) -> Result<RuntimeValue> {
        match method {
            "clone" => {
                // Rust clone creates a new owned value
                Ok(RuntimeValue::owned(obj.get_value()))
            },
            "to_shared" => {
                // Convert to shared ownership (Rc<RefCell<T>>)
                Ok(obj.to_shared())
            },
            "to_string" => {
                // Convert to string representation
                Ok(RuntimeValue::owned(AstValue::string(format!("{}", obj.get_value()))))
            },
            "len" => {
                // Get length for collections
                match obj.get_value() {
                    AstValue::String(s) => {
                        Ok(RuntimeValue::owned(AstValue::int(s.value.len() as i64)))
                    },
                    AstValue::List(l) => {
                        Ok(RuntimeValue::owned(AstValue::int(l.values.len() as i64)))
                    },
                    _ => bail!("Method 'len' not available for this type"),
                }
            },
            "push" => {
                // Push to collections (requires mutable self)
                if args.len() != 1 {
                    bail!("Method 'push' expects 1 argument");
                }
                
                if !obj.can_mutate() {
                    bail!("Cannot mutate immutable value");
                }
                
                let mut obj = obj;
                let arg_value = args[0].get_value();
                obj.try_mutate(move |ast_value| {
                    match ast_value {
                        AstValue::List(ref mut list) => {
                            list.values.push(arg_value);
                            Ok(())
                        },
                        _ => bail!("Method 'push' not available for this type"),
                    }
                })?;
                
                Ok(obj)
            },
            _ => bail!("Method '{}' not found", method),
        }
    }
    
    fn access_field(&self, obj: RuntimeValue, field: &str) -> Result<RuntimeValue> {
        let value = obj.get_value();
        match value {
            AstValue::Struct(s) => {
                if let Some(field_value) = s.structural.get_field(&Ident::new(field)) {
                    // In Rust, field access creates a borrowed reference
                    Ok(RuntimeValue::borrowed(
                        field_value.value.clone(),
                        format!("field_{}", field)
                    ))
                } else {
                    bail!("Field '{}' not found", field);
                }
            },
            _ => bail!("Cannot access field on non-struct value"),
        }
    }
    
    fn assign_field(&self, obj: &mut RuntimeValue, field: &str, value: RuntimeValue) -> Result<()> {
        // Requires mutable access to the object
        if !obj.can_mutate() {
            bail!("Cannot mutate field of immutable value");
        }
        
        let field_name = field.to_string();
        let field_value = value.get_value();
        obj.try_mutate(move |ast_value| {
            match ast_value {
                AstValue::Struct(ref mut s) => {
                    if let Some(struct_field) = s.structural.fields.iter_mut().find(|f| f.name.name == field_name) {
                        struct_field.value = field_value;
                        Ok(())
                    } else {
                        bail!("Field '{}' not found", field_name)
                    }
                },
                _ => bail!("Cannot assign field to non-struct value"),
            }
        })
    }
    
    fn convert_value(&self, value: RuntimeValue, target_type: &str) -> Result<RuntimeValue> {
        match target_type {
            "move" => {
                // Explicit move - take ownership
                Ok(RuntimeValue::owned(value.take_ownership()?))
            },
            "clone" => {
                // Explicit clone - create new owned value
                Ok(RuntimeValue::owned(value.get_value()))
            },
            "shared" => {
                // Convert to Rc<RefCell<T>>
                Ok(value.to_shared())
            },
            "arc" => {
                // Convert to Arc<RwLock<T>>
                Ok(value.to_shared_atomic())
            },
            _ => {
                // Delegate to default implementation
                RuntimePass::convert_value(self, value, target_type)
            }
        }
    }
    
    fn validate_operation(&self, op: &str, operands: &[RuntimeValue]) -> Result<()> {
        match op {
            "move" => {
                if operands.len() != 1 {
                    bail!("Move operation expects exactly 1 operand");
                }
                // Check if operand can be moved
                if operands[0].is_borrowed() {
                    bail!("Cannot move borrowed value");
                }
                Ok(())
            },
            "borrow" | "borrow_mut" => {
                if operands.len() != 1 {
                    bail!("Borrow operation expects exactly 1 operand");
                }
                // Additional borrow checking would go here
                Ok(())
            },
            _ => Ok(()), // Allow other operations
        }
    }
}

impl Default for RustRuntimePass {
    fn default() -> Self {
        Self::new()
    }
}

// Helper functions for Rust-specific operations
impl RustRuntimePass {
    /// Create a borrowed reference to a value
    pub fn borrow_value(&mut self, value: RuntimeValue, var_name: String) -> Result<RuntimeValue> {
        // Check borrow rules
        if self.is_moved(&var_name) {
            bail!("Cannot borrow moved value '{}'", var_name);
        }
        
        // Add borrow tracking
        self.add_borrow(var_name.clone(), false, var_name.clone());
        
        Ok(RuntimeValue::borrowed(value.get_value(), var_name))
    }
    
    /// Create a mutable borrowed reference to a value
    pub fn borrow_value_mut(&mut self, value: RuntimeValue, var_name: String) -> Result<RuntimeValue> {
        // Check borrow rules - no existing borrows allowed for mutable borrow
        if self.is_borrowed(&var_name) {
            bail!("Cannot create mutable borrow: '{}' is already borrowed", var_name);
        }
        
        if self.is_moved(&var_name) {
            bail!("Cannot borrow moved value '{}'", var_name);
        }
        
        // Add mutable borrow tracking
        self.add_borrow(var_name.clone(), true, var_name.clone());
        
        Ok(RuntimeValue::borrowed_mut(value.get_value(), var_name))
    }
    
    /// Move a value (consuming the original)
    pub fn move_value(&mut self, value: RuntimeValue, var_name: String) -> Result<RuntimeValue> {
        // Check if already moved
        if self.is_moved(&var_name) {
            bail!("Use of moved value '{}'", var_name);
        }
        
        // Mark as moved
        self.mark_moved(var_name);
        
        Ok(RuntimeValue::owned(value.take_ownership()?))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::AstValue;
    
    #[test]
    fn test_rust_runtime_owned_values() {
        let runtime = RustRuntimePass::new();
        let literal = AstValue::int(42);
        let runtime_value = runtime.create_runtime_value(literal);
        
        assert!(runtime_value.is_owned());
        assert_eq!(runtime_value.get_value(), AstValue::int(42));
    }
    
    #[test]
    fn test_rust_runtime_clone_method() {
        let runtime = RustRuntimePass::new();
        let value = RuntimeValue::owned(AstValue::string("hello".to_string()));
        
        let cloned = runtime.call_method(value, "clone", vec![]).unwrap();
        assert!(cloned.is_owned());
        assert_eq!(cloned.get_value(), AstValue::string("hello".to_string()));
    }
}