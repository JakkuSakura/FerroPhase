// Side effect tracker - utility for collecting and applying side effects

use eyre::eyre;
use fp_core::ast::*;
use fp_core::error::Result;
use fp_core::id::{Ident, Locator};

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
    CompileError { message: String },
    /// Emit compile-time warning - corresponds to compile_warning! intrinsic
    CompileWarning { message: String },
    /// Generate runtime function call - for functions like printf
    GenerateFunctionCall {
        function_name: String,
        args: Vec<AstValue>,
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
                SideEffect::GenerateField {
                    target_type,
                    field_name,
                    field_type,
                } => {
                    self.apply_field_generation(ast, target_type, field_name, field_type)?;
                    changes_made = true;
                }
                SideEffect::GenerateMethod {
                    target_type,
                    method_name,
                    method_body,
                } => {
                    self.apply_method_generation(ast, target_type, method_name, method_body)?;
                    changes_made = true;
                }
                SideEffect::GenerateImpl {
                    target_type,
                    trait_name,
                    methods,
                } => {
                    self.apply_impl_generation(ast, target_type, trait_name, methods)?;
                    changes_made = true;
                }
                SideEffect::GenerateType {
                    type_name,
                    type_definition,
                } => {
                    self.apply_type_generation(ast, type_name, type_definition)?;
                    changes_made = true;
                }
                SideEffect::CreateStructBuilder {
                    builder_name,
                    struct_name,
                } => {
                    if self.apply_struct_builder_creation(ast, builder_name, struct_name)? {
                        changes_made = true;
                    }
                }
                SideEffect::CompileError { message } => {
                    return Err(fp_core::error::Error::Generic(eyre!(
                        "Compile error: {}",
                        message
                    )));
                }
                SideEffect::CompileWarning { message } => {
                    tracing::warn!("Compile warning: {}", message);
                }
                SideEffect::GenerateFunctionCall {
                    function_name,
                    args,
                } => {
                    self.apply_function_call_generation(ast, function_name, args)?;
                    changes_made = true;
                }
            }
        }

        // Clear applied side effects
        self.clear_side_effects();
        Ok(changes_made)
    }

    /// Apply function call generation side effect
    /// For now this is a stub that preserves the intent without mutating the AST.
    /// A future implementation can inject an explicit call node into the AST.
    fn apply_function_call_generation(
        &self,
        _ast: &mut AstNode,
        _function_name: &str,
        _args: &Vec<AstValue>,
    ) -> Result<()> {
        // Intentionally a no-op for now. This ensures side effects are accounted for
        // without causing compilation failures until full lowering is implemented.
        Ok(())
    }

    /// Apply field generation side effect
    fn apply_field_generation(
        &self,
        ast: &mut AstNode,
        target_type: &str,
        field_name: &str,
        field_type: &AstType,
    ) -> Result<()> {
        if let Some(struct_def) = find_struct_mut(ast, target_type) {
            if struct_def
                .value
                .fields
                .iter()
                .any(|field| field.name.as_str() == field_name)
            {
                return Ok(());
            }

            struct_def.value.fields.push(StructuralField::new(
                Ident::new(field_name),
                field_type.clone(),
            ));
            Ok(())
        } else {
            Err(fp_core::error::Error::Generic(eyre!(
                "Unable to add field {} to unknown struct {}",
                field_name,
                target_type
            )))
        }
    }

    /// Apply method generation side effect
    fn apply_method_generation(
        &self,
        ast: &mut AstNode,
        target_type: &str,
        method_name: &str,
        method_body: &AstExpr,
    ) -> Result<()> {
        let function = create_method_function(method_name, method_body);

        if let Some(impl_block) = find_impl_mut(ast, target_type, None) {
            if method_exists(impl_block, method_name) {
                return Ok(());
            }
            impl_block.items.push(AstItem::DefFunction(function));
            Ok(())
        } else {
            let impl_item = ItemImpl::new_ident(
                Ident::new(target_type),
                vec![AstItem::DefFunction(function)],
            );
            push_item(ast, AstItem::Impl(impl_item))
        }
    }

    /// Apply impl generation side effect
    fn apply_impl_generation(
        &self,
        ast: &mut AstNode,
        target_type: &str,
        trait_name: &str,
        methods: &[(String, AstExpr)],
    ) -> Result<()> {
        let trait_locator = Locator::Ident(Ident::new(trait_name));

        if let Some(impl_block) = find_impl_mut(ast, target_type, Some(trait_name)) {
            for (name, body) in methods {
                if method_exists(impl_block, name) {
                    continue;
                }
                impl_block
                    .items
                    .push(AstItem::DefFunction(create_method_function(name, body)));
            }
            return Ok(());
        }

        let mut method_items = Vec::new();
        for (name, body) in methods {
            method_items.push(AstItem::DefFunction(create_method_function(name, body)));
        }

        let impl_item = ItemImpl::new(
            Some(trait_locator),
            AstExpr::ident(Ident::new(target_type)),
            method_items,
        );
        push_item(ast, AstItem::Impl(impl_item))
    }

    /// Apply type generation side effect
    fn apply_type_generation(
        &self,
        ast: &mut AstNode,
        type_name: &str,
        type_definition: &AstType,
    ) -> Result<()> {
        if find_struct_mut(ast, type_name).is_some() {
            return Ok(());
        }

        if let AstType::Struct(struct_ty) = type_definition {
            let mut struct_ty_clone = struct_ty.clone();
            struct_ty_clone.name = Ident::new(type_name);

            let struct_item = ItemDefStruct {
                visibility: Visibility::Public,
                name: Ident::new(type_name),
                value: struct_ty_clone,
            };

            push_item(ast, AstItem::DefStruct(struct_item))?;
        }

        Ok(())
    }

    /// Apply struct builder creation side effect
    fn apply_struct_builder_creation(
        &self,
        _ast: &mut AstNode,
        _builder_name: &str,
        _struct_name: &str,
    ) -> Result<bool> {
        // TODO: Implement struct builder creation
        Ok(false)
    }
}

impl SideEffect {
    /// Get a description of this side effect for debugging
    pub fn description(&self) -> String {
        match self {
            SideEffect::GenerateField {
                target_type,
                field_name,
                ..
            } => {
                format!("Add field {} to {}", field_name, target_type)
            }
            SideEffect::GenerateMethod {
                target_type,
                method_name,
                ..
            } => {
                format!("Add method {} to {}", method_name, target_type)
            }
            SideEffect::GenerateImpl {
                target_type,
                trait_name,
                methods,
            } => {
                format!(
                    "Implement {} for {} with {} methods",
                    trait_name,
                    target_type,
                    methods.len()
                )
            }
            SideEffect::GenerateType { type_name, .. } => {
                format!("Generate type {}", type_name)
            }
            SideEffect::CreateStructBuilder {
                builder_name,
                struct_name,
            } => {
                format!("Create struct builder {} for {}", builder_name, struct_name)
            }
            SideEffect::CompileError { message } => {
                format!("Compile error: {}", message)
            }
            SideEffect::CompileWarning { message } => {
                format!("Compile warning: {}", message)
            }
            SideEffect::GenerateFunctionCall {
                function_name,
                args,
            } => {
                format!(
                    "Generate function call {} with {} args",
                    function_name,
                    args.len()
                )
            }
        }
    }
}

fn find_struct_mut<'a>(node: &'a mut AstNode, target: &str) -> Option<&'a mut ItemDefStruct> {
    match node {
        AstNode::File(file) => find_struct_in_items_mut(&mut file.items, target),
        AstNode::Item(item) => match item {
            AstItem::DefStruct(def) if def.name.as_str() == target => Some(def),
            AstItem::Module(module) => find_struct_in_items_mut(&mut module.items, target),
            _ => None,
        },
        AstNode::Expr(_) => None,
    }
}

fn find_struct_in_items_mut<'a>(
    items: &'a mut [AstItem],
    target: &str,
) -> Option<&'a mut ItemDefStruct> {
    for item in items.iter_mut() {
        match item {
            AstItem::DefStruct(def) if def.name.as_str() == target => return Some(def),
            AstItem::Module(module) => {
                if let Some(found) = find_struct_in_items_mut(&mut module.items, target) {
                    return Some(found);
                }
            }
            _ => {}
        }
    }
    None
}

fn find_impl_mut<'a>(
    node: &'a mut AstNode,
    target: &str,
    trait_name: Option<&str>,
) -> Option<&'a mut ItemImpl> {
    match node {
        AstNode::File(file) => find_impl_in_items_mut(&mut file.items, target, trait_name),
        AstNode::Item(item) => match item {
            AstItem::Impl(item_impl) => {
                if impl_matches(item_impl, target, trait_name) {
                    Some(item_impl)
                } else {
                    None
                }
            }
            AstItem::Module(module) => {
                find_impl_in_items_mut(&mut module.items, target, trait_name)
            }
            _ => None,
        },
        AstNode::Expr(_) => None,
    }
}

fn find_impl_in_items_mut<'a>(
    items: &'a mut [AstItem],
    target: &str,
    trait_name: Option<&str>,
) -> Option<&'a mut ItemImpl> {
    for item in items.iter_mut() {
        match item {
            AstItem::Impl(item_impl) => {
                if impl_matches(item_impl, target, trait_name) {
                    return Some(item_impl);
                }
            }
            AstItem::Module(module) => {
                if let Some(found) = find_impl_in_items_mut(&mut module.items, target, trait_name) {
                    return Some(found);
                }
            }
            _ => {}
        }
    }
    None
}

fn impl_matches(impl_block: &ItemImpl, target: &str, trait_name: Option<&str>) -> bool {
    let self_matches = matches!(
        &impl_block.self_ty,
        AstExpr::Locator(Locator::Ident(ident)) if ident.as_str() == target
    );

    if !self_matches {
        return false;
    }

    match (trait_name, &impl_block.trait_ty) {
        (None, None) => true,
        (Some(name), Some(locator)) => locator
            .as_ident()
            .map(|ident| ident.as_str() == name)
            .unwrap_or(false),
        _ => false,
    }
}

fn method_exists(impl_block: &ItemImpl, method_name: &str) -> bool {
    impl_block.items.iter().any(|item| match item {
        AstItem::DefFunction(func) => func.name.as_str() == method_name,
        _ => false,
    })
}

fn create_method_function(method_name: &str, method_body: &AstExpr) -> ItemDefFunction {
    ItemDefFunction::new_simple(Ident::new(method_name), method_body.clone().into())
        .with_receiver(FunctionParamReceiver::Ref)
}

fn push_item(ast: &mut AstNode, item: AstItem) -> Result<()> {
    match ast {
        AstNode::File(file) => {
            file.items.push(item);
            Ok(())
        }
        AstNode::Item(AstItem::Module(module)) => {
            module.items.push(item);
            Ok(())
        }
        _ => Err(fp_core::error::Error::Generic(eyre!(
            "Unable to insert generated item into AST at this location"
        ))),
    }
}
