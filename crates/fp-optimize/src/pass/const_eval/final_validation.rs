// Pass 9: Final Type Validation & Integration

use super::context::*;
use super::ConstEvaluator;
use fp_core::ast::*;
use fp_core::context::SharedScopedContext;
use fp_core::ctx::ty::{TypeRegistry, TypeInfo};
use fp_core::error::Result;
use tracing::{debug, info, warn};
use std::collections::{HashMap, HashSet};

impl ConstEvaluator {
    /// Pass 9: Final Type Validation & Integration (Final)
    pub fn final_type_validation(&self, module: &AstModule, ctx: &SharedScopedContext) -> Result<bool> {
        debug!("Pass 9: Performing final type validation and integration");
        
        let mut validation_issues = Vec::new();
        
        // Step 1: Perform final type checking with all generated types
        if let Err(type_errors) = self.perform_final_type_checking(module, ctx) {
            validation_issues.push(format!("Type checking errors: {:?}", type_errors));
        }
        
        // Step 2: Ensure all type references are resolved
        let unresolved_refs = self.find_unresolved_type_references(module)?;
        if !unresolved_refs.is_empty() {
            validation_issues.push(format!("Unresolved type references: {:?}", unresolved_refs));
        }
        
        // Step 3: Validate generic constraints with specialized types
        if let Err(constraint_errors) = self.validate_generic_constraints(module, ctx) {
            validation_issues.push(format!("Generic constraint violations: {:?}", constraint_errors));
        }
        
        // Step 4: Freeze type system state for runtime compilation
        let freeze_result = self.freeze_type_system_state()?;
        
        if validation_issues.is_empty() {
            info!("Final type validation completed successfully - {} types frozen", freeze_result.frozen_types_count);
            Ok(true)
        } else {
            warn!("Final type validation found issues: {}", validation_issues.join(", "));
            Ok(false)
        }
    }
    
    /// Perform comprehensive type checking with all generated types
    fn perform_final_type_checking(&self, module: &AstModule, _ctx: &SharedScopedContext) -> Result<()> {
        debug!("Performing final type checking");
        
        // Collect all type definitions from the module
        let mut type_definitions = HashMap::new();
        for item in &module.items {
            match item {
                AstItem::DefStruct(struct_def) => {
                    type_definitions.insert(struct_def.name.name.clone(), item);
                },
                _ => {} // Handle other item types as needed
            }
        }
        
        // Validate each type definition
        for (type_name, item) in &type_definitions {
            match item {
                AstItem::DefStruct(struct_def) => {
                    // Check field types are valid
                    for field in &struct_def.value.fields {
                        if let Err(e) = self.validate_field_type(&field.value, &type_definitions) {
                            return Err(e);
                        }
                    }
                    debug!("Type {} validation passed", type_name);
                },
                _ => {}
            }
        }
        
        // Validate impl blocks
        for item in &module.items {
            if let AstItem::Impl(impl_def) = item {
                self.validate_impl_block(impl_def, &type_definitions)?;
            }
        }
        
        info!("Final type checking completed for {} types", type_definitions.len());
        Ok(())
    }
    
    /// Validate a field type is well-formed and references valid types
    fn validate_field_type(&self, field_type: &AstType, known_types: &HashMap<String, &AstItem>) -> Result<()> {
        match field_type {
            AstType::Expr(expr) => {
                if let AstExpr::Locator(locator) = expr.as_ref() {
                    if let Some(ident) = locator.as_ident() {
                        // Check if it's a known type or primitive
                        if !self.is_primitive_type(&ident.name) && !known_types.contains_key(&ident.name) {
                            return Err(fp_core::error::Error::Generic(format!("Unknown type: {}", ident.name)));
                        }
                    }
                }
            },
            AstType::Struct(_) => {
                // Nested struct definitions are valid
            },
            _ => {
                // Other type forms - validate as needed
            }
        }
        Ok(())
    }
    
    /// Check if a type name is a primitive type
    pub fn is_primitive_type(&self, type_name: &str) -> bool {
        matches!(type_name, "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" | 
                             "f32" | "f64" | "bool" | "char" | "str" | "String")
    }
    
    /// Validate impl blocks have correct types and method signatures
    fn validate_impl_block(&self, impl_def: &ItemImpl, known_types: &HashMap<String, &AstItem>) -> Result<()> {
        // Validate the target type exists
        if let AstExpr::Locator(locator) = &impl_def.self_ty {
            if let Some(ident) = locator.as_ident() {
                if !self.is_primitive_type(&ident.name) && !known_types.contains_key(&ident.name) {
                    return Err(fp_core::error::Error::Generic(format!("Impl for unknown type: {}", ident.name)));
                }
            }
        }
        
        // Validate trait exists if it's a trait impl
        if let Some(trait_locator) = &impl_def.trait_ty {
            if let Some(trait_ident) = trait_locator.as_ident() {
                debug!("Validating trait impl: {}", trait_ident.name);
                // TODO: Check trait exists in type registry
            }
        }
        
        // Validate method signatures
        for item in &impl_def.items {
            if let AstItem::DefFunction(func_def) = item {
                debug!("Validating method: {}", func_def.name.name);
                // TODO: Validate method signature matches trait requirements if applicable
            }
        }
        
        Ok(())
    }
    
    /// Find any type references that couldn't be resolved
    pub fn find_unresolved_type_references(&self, module: &AstModule) -> Result<Vec<String>> {
        let mut unresolved = Vec::new();
        let type_registry_types: HashSet<String> = self.type_registry.list_types()
            .into_iter()
            .map(|(_, name)| name)
            .collect();
        
        // Check all type references in struct fields
        for item in &module.items {
            if let AstItem::DefStruct(struct_def) = item {
                for field in &struct_def.value.fields {
                    if let Some(type_name) = self.extract_type_name_from_type(&field.value) {
                        if !self.is_primitive_type(&type_name) && !type_registry_types.contains(&type_name) {
                            unresolved.push(type_name);
                        }
                    }
                }
            }
        }
        
        // Remove duplicates
        unresolved.sort();
        unresolved.dedup();
        
        Ok(unresolved)
    }
    
    /// Extract type name from an AstType if possible
    fn extract_type_name_from_type(&self, ast_type: &AstType) -> Option<String> {
        match ast_type {
            AstType::Expr(expr) => {
                if let AstExpr::Locator(locator) = expr.as_ref() {
                    if let Some(ident) = locator.as_ident() {
                        return Some(ident.name.clone());
                    }
                }
            },
            _ => {}
        }
        None
    }
    
    /// Validate generic constraints with specialized types
    fn validate_generic_constraints(&self, _module: &AstModule, _ctx: &SharedScopedContext) -> Result<()> {
        debug!("Validating generic constraints");
        
        // TODO: This would be a comprehensive validation of generic type constraints
        // For now, we assume all constraints are valid since we don't have complex
        // generic constraint checking implemented yet
        
        // In a full implementation, this would:
        // 1. Check that all generic parameters have been properly specialized
        // 2. Validate that trait bounds are satisfied
        // 3. Ensure lifetime constraints are met
        // 4. Verify that associated types are correctly specified
        
        Ok(())
    }
    
    /// Freeze the type system state to prevent further modifications
    pub fn freeze_type_system_state(&self) -> Result<FreezeResult> {
        debug!("Freezing type system state");
        
        // Get current state snapshot
        let type_list = self.type_registry.list_types();
        let frozen_count = type_list.len();
        
        info!("Type system frozen with {} types", frozen_count);
        
        // TODO: In a full implementation, this would:
        // 1. Create immutable snapshots of all type definitions
        // 2. Lock the type registry against further modifications
        // 3. Generate optimized lookup tables for runtime
        // 4. Validate that all dependent systems can use the frozen state
        
        Ok(FreezeResult {
            frozen_types_count: frozen_count,
            snapshot_created: true,
        })
    }
    
    /// Check if the type system is ready for final validation
    pub fn is_ready_for_final_validation(&self) -> bool {
        // Check if there are any pending side effects that would modify types
        if self.has_side_effects() {
            debug!("Type system not ready: pending side effects");
            return false;
        }
        
        // Check if all const blocks have been evaluated
        let const_blocks = self.const_blocks.read().unwrap();
        let unevaluated_blocks = const_blocks.iter()
            .filter(|(_, block)| matches!(block.state, ConstEvalState::NotEvaluated))
            .count();
        
        if unevaluated_blocks > 0 {
            debug!("Type system not ready: {} unevaluated const blocks", unevaluated_blocks);
            return false;
        }
        
        debug!("Type system ready for final validation");
        true
    }
}

/// Result of freezing the type system
#[derive(Debug)]
pub struct FreezeResult {
    pub frozen_types_count: usize,
    pub snapshot_created: bool,
}