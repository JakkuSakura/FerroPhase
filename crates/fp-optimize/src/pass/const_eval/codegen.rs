// Pass 8: Code Generation & AST Modification

use super::context::*;
use super::ConstEvaluator;
use fp_core::ast::*;
use fp_core::context::SharedScopedContext;
use fp_core::error::Result;
use fp_core::id::*;
use tracing::{debug, info, warn};

impl ConstEvaluator {
    /// Pass 8: Code Generation & AST Modification (Iterative)
    pub fn generate_and_modify_ast(&self, module: &mut AstModule, _ctx: &SharedScopedContext) -> Result<bool> {
        debug!("Pass 8: Generating code and modifying AST");
        
        let mut changes_made = false;
        let side_effects = self.side_effects.read().unwrap().clone();
        
        if side_effects.is_empty() {
            debug!("No side effects to process");
            return Ok(false);
        }
        
        // Apply all accumulated side effects to the AST
        for effect in &side_effects {
            match effect {
                SideEffect::GenerateType { type_name, type_definition } => {
                    changes_made |= self.generate_type_in_ast(module, type_name, type_definition)?;
                },
                SideEffect::GenerateField { target_type, field_name, field_type } => {
                    changes_made |= self.generate_field_in_ast(module, target_type, field_name, field_type)?;
                },
                SideEffect::GenerateMethod { target_type, method_name, method_body } => {
                    changes_made |= self.generate_method_in_ast(module, target_type, method_name, method_body)?;
                },
                SideEffect::GenerateImpl { target_type, trait_name, methods } => {
                    changes_made |= self.generate_impl_in_ast(module, target_type, trait_name, methods)?;
                },
            }
        }
        
        if changes_made {
            info!("Code generation completed - {} side effects processed", side_effects.len());
            
            // Clear processed side effects
            self.clear_side_effects();
        }
        
        Ok(changes_made)
    }
    
    /// Generate a new type definition in the AST
    fn generate_type_in_ast(&self, module: &mut AstModule, type_name: &str, type_definition: &AstType) -> Result<bool> {
        debug!("Generating type definition for: {}", type_name);
        
        // Check if type already exists in the module
        for item in &module.items {
            if let AstItem::DefStruct(struct_def) = item {
                if struct_def.name.name == type_name {
                    debug!("Type {} already exists in module, skipping", type_name);
                    return Ok(false);
                }
            }
        }
        
        // Create new struct definition
        match type_definition {
            AstType::Struct(struct_type) => {
                let struct_item = AstItem::DefStruct(ItemDefStruct {
                    visibility: Visibility::Public,
                    name: Ident::new(type_name.to_string()),
                    value: struct_type.clone(),
                });
                
                module.items.push(struct_item);
                info!("Generated struct type: {}", type_name);
                Ok(true)
            },
            _ => {
                warn!("Unsupported type definition for generation: {:?}", type_definition);
                Ok(false)
            }
        }
    }
    
    /// Generate a field in an existing struct
    fn generate_field_in_ast(&self, module: &mut AstModule, target_type: &str, field_name: &str, field_type: &AstType) -> Result<bool> {
        debug!("Generating field {}.{}", target_type, field_name);
        
        // Find the target struct in the module
        for item in &mut module.items {
            if let AstItem::DefStruct(ref mut struct_def) = item {
                if struct_def.name.name == target_type {
                    // Check if field already exists
                    for existing_field in &struct_def.value.fields {
                        if existing_field.name.name == field_name {
                            debug!("Field {}.{} already exists, skipping", target_type, field_name);
                            return Ok(false);
                        }
                    }
                    
                    // Add the new field
                    let new_field = StructuralField {
                        name: Ident::new(field_name.to_string()),
                        value: field_type.clone(),
                    };
                    
                    struct_def.value.fields.push(new_field);
                    info!("Generated field {}.{}", target_type, field_name);
                    return Ok(true);
                }
            }
        }
        
        warn!("Target type {} not found for field generation", target_type);
        Ok(false)
    }
    
    /// Generate a method implementation
    fn generate_method_in_ast(&self, module: &mut AstModule, target_type: &str, method_name: &str, method_body: &AstExpr) -> Result<bool> {
        debug!("Generating method {}::{}", target_type, method_name);
        
        // Look for an existing impl block for the target type
        for item in &mut module.items {
            if let AstItem::Impl(ref mut impl_def) = item {
                // Check if this impl is for our target type
                if let AstExpr::Locator(locator) = &impl_def.self_ty {
                    if let Some(ident) = locator.as_ident() {
                        if ident.name == target_type {
                            // Check if method already exists - look through ItemChunk
                            let mut method_exists = false;
                            for existing_item in &impl_def.items {
                                if let AstItem::DefFunction(func_def) = existing_item {
                                    if func_def.name.name == method_name {
                                        debug!("Method {}::{} already exists, skipping", target_type, method_name);
                                        method_exists = true;
                                        break;
                                    }
                                }
                            }
                            
                            if !method_exists {
                                // Add the new method
                                let method_def = ItemDefFunction::new_simple(
                                    Ident::new(method_name.to_string()),
                                    method_body.clone().into()
                                );
                                
                                impl_def.items.push(AstItem::DefFunction(method_def));
                                info!("Generated method {}::{}", target_type, method_name);
                                return Ok(true);
                            }
                        }
                    }
                }
            }
        }
        
        // No existing impl block found, create a new one
        let method_def = ItemDefFunction::new_simple(
            Ident::new(method_name.to_string()),
            method_body.clone().into()
        );
        
        let mut item_chunk: ItemChunk = vec![AstItem::DefFunction(method_def)];
        
        let impl_def = ItemImpl {
            trait_ty: None, // Regular impl block, not trait impl
            self_ty: AstExpr::Locator(Locator::Ident(Ident::new(target_type.to_string()))),
            items: item_chunk,
        };
        
        module.items.push(AstItem::Impl(impl_def));
        info!("Generated new impl block with method {}::{}", target_type, method_name);
        Ok(true)
    }
    
    /// Generate a trait implementation
    fn generate_impl_in_ast(&self, module: &mut AstModule, target_type: &str, trait_name: &str, methods: &[(String, AstExpr)]) -> Result<bool> {
        debug!("Generating impl {} for {} with {} methods", trait_name, target_type, methods.len());
        
        // Check if this trait impl already exists
        for item in &module.items {
            if let AstItem::Impl(impl_def) = item {
                if let Some(ref existing_trait) = impl_def.trait_ty {
                    if let Some(trait_ident) = existing_trait.as_ident() {
                        if trait_ident.name == trait_name {
                            // Check target type
                            if let AstExpr::Locator(type_locator) = &impl_def.self_ty {
                                if let Some(type_ident) = type_locator.as_ident() {
                                    if type_ident.name == target_type {
                                        debug!("Impl {} for {} already exists, skipping", trait_name, target_type);
                                        return Ok(false);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        
        // Create method definitions from the provided method bodies
        let mut item_chunk: ItemChunk = Vec::new();
        for (method_name, method_body) in methods {
            let method_def = ItemDefFunction::new_simple(
                Ident::new(method_name.clone()),
                method_body.clone().into()
            );
            item_chunk.push(AstItem::DefFunction(method_def));
        }
        
        // Create the trait impl
        let impl_def = ItemImpl {
            trait_ty: Some(Locator::Ident(Ident::new(trait_name.to_string()))),
            self_ty: AstExpr::Locator(Locator::Ident(Ident::new(target_type.to_string()))),
            items: item_chunk,
        };
        
        module.items.push(AstItem::Impl(impl_def));
        info!("Generated trait impl {} for {} with {} methods", trait_name, target_type, methods.len());
        Ok(true)
    }
    
    /// Maintain source location tracking for generated code
    pub fn track_generated_code(&self, item: &mut AstItem, original_location: Option<&str>) {
        // TODO: Add source tracking attributes to generated items
        // This would help with debugging and error reporting for generated code
        let _location_info = original_location.unwrap_or("@generated");
        
        // For now, just log the generated item - attributes would require AstAttribute construction
        match item {
            AstItem::DefStruct(struct_def) => {
                debug!("Tracking generated struct: {}", struct_def.name.name);
            },
            AstItem::Impl(impl_def) => {
                if let AstExpr::Locator(locator) = &impl_def.self_ty {
                    if let Some(ident) = locator.as_ident() {
                        debug!("Tracking generated impl for: {}", ident.name);
                    }
                }
            },
            _ => {} // Other items don't need tracking for now
        }
    }
}