// Side effect tracker - utility for collecting and applying side effects

use fp_core::ast::*;
use fp_core::error::Result;

/// Side effects that const evaluation can produce
#[derive(Debug, Clone)]
pub enum SideEffect {
    /// Add field to struct - corresponds to addfield! intrinsic
    GenerateField {
        target_type: String,
        field_name: String,
        field_type: AstType,
    },
    /// Add method to struct - corresponds to addmethod! intrinsic
    GenerateMethod {
        target_type: String,
        method_name: String,
        method_body: AstExpr,
    },
    /// Add trait implementation - corresponds to addimpl! intrinsic
    GenerateImpl {
        target_type: String,
        trait_name: String,
        methods: Vec<(String, AstExpr)>,
    },
    /// Create new struct type - corresponds to create_struct! intrinsic
    GenerateType {
        type_name: String,
        type_definition: AstType,
    },
    /// Create new struct builder for dynamic construction
    CreateStructBuilder {
        builder_name: String,
        struct_name: String,
    },
    /// Emit compile-time error - corresponds to compile_error! intrinsic
    CompileError {
        message: String,
    },
    /// Emit compile-time warning - corresponds to compile_warning! intrinsic
    CompileWarning {
        message: String,
    },
}

/// Utility for tracking and applying side effects
pub struct SideEffectTracker {
    side_effects: Vec<SideEffect>,
}

impl SideEffectTracker {
    pub fn new() -> Self {
        Self {
            side_effects: Vec::new(),
        }
    }

    /// Add a side effect to be processed later
    pub fn add_side_effect(&mut self, effect: SideEffect) {
        self.side_effects.push(effect);
    }

    /// Get all accumulated side effects
    pub fn get_side_effects(&self) -> Vec<SideEffect> {
        self.side_effects.clone()
    }

    /// Clear all side effects
    pub fn clear_side_effects(&mut self) {
        self.side_effects.clear();
    }

    /// Apply all accumulated side effects to the AST
    pub fn apply_side_effects(&mut self, ast: &mut AstNode) -> Result<bool> {
        let mut changes_made = false;

        for effect in &self.side_effects {
            match effect {
                SideEffect::GenerateField { target_type, field_name, field_type } => {
                    self.apply_field_generation(ast, target_type, field_name, field_type)?;
                    changes_made = true;
                },
                SideEffect::GenerateMethod { target_type, method_name, method_body } => {
                    self.apply_method_generation(ast, target_type, method_name, method_body)?;
                    changes_made = true;
                },
                SideEffect::GenerateImpl { target_type, trait_name, methods } => {
                    self.apply_impl_generation(ast, target_type, trait_name, methods)?;
                    changes_made = true;
                },
                SideEffect::GenerateType { type_name, type_definition } => {
                    self.apply_type_generation(ast, type_name, type_definition)?;
                    changes_made = true;
                },
                SideEffect::CreateStructBuilder { builder_name, struct_name } => {
                    self.apply_struct_builder_creation(ast, builder_name, struct_name)?;
                    changes_made = true;
                },
                SideEffect::CompileError { message } => {
                    return Err(fp_core::error::Error::Generic(format!("Compile error: {}", message)));
                },
                SideEffect::CompileWarning { message } => {
                    eprintln!("Warning: {}", message);
                },
            }
        }

        // Clear applied side effects
        self.clear_side_effects();
        Ok(changes_made)
    }

    /// Apply field generation side effect
    fn apply_field_generation(&self, _ast: &mut AstNode, _target_type: &str, _field_name: &str, _field_type: &AstType) -> Result<()> {
        // TODO: Implement field addition to struct definitions
        Ok(())
    }

    /// Apply method generation side effect
    fn apply_method_generation(&self, _ast: &mut AstNode, _target_type: &str, _method_name: &str, _method_body: &AstExpr) -> Result<()> {
        // TODO: Implement method addition to struct definitions
        Ok(())
    }

    /// Apply impl generation side effect
    fn apply_impl_generation(&self, _ast: &mut AstNode, _target_type: &str, _trait_name: &str, _methods: &[(String, AstExpr)]) -> Result<()> {
        // TODO: Implement trait implementation generation
        Ok(())
    }

    /// Apply type generation side effect
    fn apply_type_generation(&self, _ast: &mut AstNode, _type_name: &str, _type_definition: &AstType) -> Result<()> {
        // TODO: Implement new type generation
        Ok(())
    }

    /// Apply struct builder creation side effect
    fn apply_struct_builder_creation(&self, _ast: &mut AstNode, _builder_name: &str, _struct_name: &str) -> Result<()> {
        // TODO: Implement struct builder creation
        Ok(())
    }
}

impl SideEffect {
    /// Get a description of this side effect for debugging
    pub fn description(&self) -> String {
        match self {
            SideEffect::GenerateField { target_type, field_name, .. } => {
                format!("Add field {} to {}", field_name, target_type)
            },
            SideEffect::GenerateMethod { target_type, method_name, .. } => {
                format!("Add method {} to {}", method_name, target_type)
            },
            SideEffect::GenerateImpl { target_type, trait_name, methods } => {
                format!("Implement {} for {} with {} methods", trait_name, target_type, methods.len())
            },
            SideEffect::GenerateType { type_name, .. } => {
                format!("Generate type {}", type_name)
            },
            SideEffect::CreateStructBuilder { builder_name, struct_name } => {
                format!("Create struct builder {} for {}", builder_name, struct_name)
            },
            SideEffect::CompileError { message } => {
                format!("Compile error: {}", message)
            },
            SideEffect::CompileWarning { message } => {
                format!("Compile warning: {}", message)
            },
        }
    }
}