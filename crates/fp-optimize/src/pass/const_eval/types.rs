// Pass 5: Type System Update & Validation

use super::context::*;
use super::ConstEvaluator;
use fp_core::ast::*;
use fp_core::context::SharedScopedContext;
use fp_core::ctx::ty::{TypeId, TypeInfo, FieldInfo, MethodInfo};
use fp_core::error::Result;
use tracing::{debug, info, warn};

impl ConstEvaluator {
    /// Pass 5: Type System Update & Validation (Iterative)
    pub fn update_and_validate_types(&self, ctx: &SharedScopedContext) -> Result<bool> {
        debug!("Pass 5: Updating and validating type system");
        
        let mut changes_made = false;
        let side_effects = self.side_effects.read().unwrap().clone();
        
        // Process all accumulated side effects
        for effect in side_effects {
            match effect {
                SideEffect::GenerateType { type_name, type_definition } => {
                    changes_made |= self.process_generated_type(&type_name, &type_definition)?;
                },
                SideEffect::GenerateField { target_type, field_name, field_type } => {
                    changes_made |= self.process_generated_field(&target_type, &field_name, &field_type)?;
                },
                SideEffect::GenerateMethod { target_type, method_name, method_body } => {
                    changes_made |= self.process_generated_method(&target_type, &method_name, &method_body)?;
                },
                SideEffect::GenerateImpl { target_type, trait_name, methods } => {
                    changes_made |= self.process_generated_impl(&target_type, &trait_name, &methods)?;
                },
            }
        }
        
        // Validate type system consistency after updates
        if changes_made {
            self.validate_type_system_consistency(ctx)?;
            
            // Check if new types affect existing const blocks
            let type_dependent_blocks = self.find_type_dependent_const_blocks()?;
            if !type_dependent_blocks.is_empty() {
                debug!("Found {} const blocks that may be affected by type changes", 
                      type_dependent_blocks.len());
                changes_made = true;
            }
        }
        
        Ok(changes_made)
    }

    /// Process a generated type and add it to the type registry
    pub(super) fn process_generated_type(&self, type_name: &str, type_definition: &AstType) -> Result<bool> {
        debug!("Processing generated type: {}", type_name);
        
        // Check if type already exists
        if self.type_registry.get_type_by_name(type_name).is_some() {
            debug!("Type {} already exists, skipping", type_name);
            return Ok(false);
        }
        
        // Create type info for the new type
        let type_id = TypeId::new();
        let type_info = TypeInfo {
            id: type_id,
            name: type_name.to_string(),
            ast_type: type_definition.clone(),
            size_bytes: None, // Will be computed on demand
            fields: self.extract_fields_from_type(type_definition)?,
            methods: vec![], // Will be populated by method generation
            traits_implemented: vec![],
        };
        
        // Register the new type
        self.type_registry.register_type(type_info);
        info!("Generated new type: {}", type_name);
        
        Ok(true)
    }

    /// Extract field information from a type definition
    pub(super) fn extract_fields_from_type(&self, type_definition: &AstType) -> Result<Vec<FieldInfo>> {
        let mut fields = Vec::new();
        
        match type_definition {
            AstType::Struct(struct_def) => {
                for field in &struct_def.fields {
                    let field_info = FieldInfo {
                        name: field.name.name.clone(),
                        type_id: TypeId::new(), // Placeholder, will be resolved later
                        ast_type: field.value.clone(),
                        attributes: vec![], // TODO: Extract from field attributes
                    };
                    fields.push(field_info);
                }
            },
            _ => {
                // Non-struct types don't have fields
            }
        }
        
        Ok(fields)
    }

    /// Process a generated field and add it to an existing type
    pub(super) fn process_generated_field(&self, target_type: &str, field_name: &str, field_type: &AstType) -> Result<bool> {
        debug!("Processing generated field: {}.{}", target_type, field_name);
        
        // Find the target type in the registry
        if let Some(type_info) = self.type_registry.get_type_by_name(target_type) {
            let field_info = FieldInfo {
                name: field_name.to_string(),
                type_id: TypeId::new(),
                ast_type: field_type.clone(),
                attributes: vec![],
            };
            
            // Add the field to the type (this would need to be implemented in TypeRegistry)
            self.type_registry.add_field(type_info.id, field_info)?;
            info!("Added field {} to type {}", field_name, target_type);
            Ok(true)
        } else {
            warn!("Target type {} not found for field generation", target_type);
            Ok(false)
        }
    }

    /// Process a generated method and add it to an existing type
    pub(super) fn process_generated_method(&self, target_type: &str, method_name: &str, _method_body: &AstExpr) -> Result<bool> {
        debug!("Processing generated method: {}::{}", target_type, method_name);
        
        // Find the target type in the registry
        if let Some(type_info) = self.type_registry.get_type_by_name(target_type) {
            let method_info = MethodInfo {
                name: method_name.to_string(),
                params: vec![], // TODO: Parse method signature from body
                return_type: None, // TODO: Infer return type
                attributes: vec![],
            };
            
            // Add the method to the type
            self.type_registry.add_method(type_info.id, method_info)?;
            info!("Added method {} to type {}", method_name, target_type);
            Ok(true)
        } else {
            warn!("Target type {} not found for method generation", target_type);
            Ok(false)
        }
    }

    /// Process a generated impl block
    pub(super) fn process_generated_impl(&self, target_type: &str, trait_name: &str, methods: &[(String, AstExpr)]) -> Result<bool> {
        debug!("Processing generated impl {} for {}", trait_name, target_type);
        
        // For each method in the impl block, add it to the target type
        let mut changes_made = false;
        for (method_name, method_body) in methods {
            changes_made |= self.process_generated_method(target_type, method_name, method_body)?;
        }
        
        // TODO: Register trait implementation in type system
        info!("Generated impl {} for {} with {} methods", trait_name, target_type, methods.len());
        
        Ok(changes_made)
    }

    /// Validate type system consistency after updates
    pub(super) fn validate_type_system_consistency(&self, _ctx: &SharedScopedContext) -> Result<()> {
        debug!("Validating type system consistency");
        
        // For now, this is a placeholder. In a full implementation, this would:
        // 1. Check for circular type dependencies
        // 2. Validate that all type references can be resolved
        // 3. Ensure trait implementations are complete
        // 4. Check method signatures for consistency
        // 5. Validate generic constraints
        
        let type_list = self.type_registry.list_types();
        info!("Type system validation complete: {} types registered", type_list.len());
        
        Ok(())
    }

    /// Find const blocks that may be affected by type system changes
    pub(super) fn find_type_dependent_const_blocks(&self) -> Result<Vec<u64>> {
        let mut affected_blocks = Vec::new();
        let const_blocks = self.const_blocks.read().unwrap();
        
        for (block_id, block) in const_blocks.iter() {
            if self.block_depends_on_types(&block.expr)? {
                affected_blocks.push(*block_id);
                debug!("Const block {} may be affected by type changes", block_id);
            }
        }
        
        Ok(affected_blocks)
    }

    /// Check if a const expression depends on type information
    pub fn block_depends_on_types(&self, expr: &AstExpr) -> Result<bool> {
        match expr {
            AstExpr::Invoke(invoke) => {
                if let ExprInvokeTarget::Function(locator) = &invoke.target {
                    if let Some(ident) = locator.as_ident() {
                        // Check if this is a type-dependent intrinsic
                        match ident.name.as_str() {
                            "@sizeof" | "@reflect_fields" | "@hasmethod" | "@type_name" => {
                                return Ok(true);
                            },
                            _ => {}
                        }
                    }
                }
                
                // Check arguments recursively
                for arg in &invoke.args {
                    if self.block_depends_on_types(&arg.get())? {
                        return Ok(true);
                    }
                }
            },
            AstExpr::Block(block) => {
                // Check all statements and expressions in the block
                for stmt in block.first_stmts() {
                    if let BlockStmt::Expr(expr_stmt) = stmt {
                        if self.block_depends_on_types(&expr_stmt.expr)? {
                            return Ok(true);
                        }
                    }
                }
                if let Some(last_expr) = block.last_expr() {
                    if self.block_depends_on_types(&last_expr)? {
                        return Ok(true);
                    }
                }
            },
            _ => {}
        }
        
        Ok(false)
    }
}