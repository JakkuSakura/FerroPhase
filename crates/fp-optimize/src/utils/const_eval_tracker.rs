// Const-eval tracker - collects compile-time mutations requested during evaluation

use eyre::eyre;
use fp_core::ast::*;
use fp_core::error::Result;
use fp_core::id::{Ident, Locator};

/// Const-eval operations produced by intrinsics during interpretation
#[derive(Debug, Clone)]
pub enum ConstEval {
    /// Add field to struct - corresponds to addfield! intrinsic
    GenerateField {
        target_type: String,
        field_name: String,
        field_type: Ty,
    },
    /// Add method to struct - corresponds to addmethod! intrinsic
    GenerateMethod {
        target_type: String,
        method_name: String,
        method_body: Expr,
    },
    /// Create new struct type - corresponds to create_struct! intrinsic
    GenerateType {
        type_name: String,
        type_definition: Ty,
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
        args: Vec<Value>,
    },
}

/// Utility for staging and applying const-eval mutations
pub struct ConstEvalTracker {
    pending: Vec<ConstEval>,
}

impl ConstEvalTracker {
    pub fn new() -> Self {
        Self {
            pending: Vec::new(),
        }
    }

    /// Record a const-eval operation produced during interpretation
    pub fn record(&mut self, op: ConstEval) {
        self.pending.push(op);
    }

    /// Inspect queued const-eval operations without consuming them
    pub fn pending(&self) -> Vec<ConstEval> {
        self.pending.clone()
    }

    /// Clear queued operations
    pub fn clear(&mut self) {
        self.pending.clear();
    }

    /// Apply all queued const-eval operations to the AST
    pub fn apply(&mut self, ast: &mut Node) -> Result<bool> {
        let mut changes_made = false;

        for op in &self.pending {
            match op {
                ConstEval::GenerateField {
                    target_type,
                    field_name,
                    field_type,
                } => {
                    self.apply_field_generation(ast, target_type, field_name, field_type)?;
                    changes_made = true;
                }
                ConstEval::GenerateMethod {
                    target_type,
                    method_name,
                    method_body,
                } => {
                    self.apply_method_generation(ast, target_type, method_name, method_body)?;
                    changes_made = true;
                }
                ConstEval::GenerateType {
                    type_name,
                    type_definition,
                } => {
                    self.apply_type_generation(ast, type_name, type_definition)?;
                    changes_made = true;
                }
                ConstEval::CreateStructBuilder {
                    builder_name,
                    struct_name,
                } => {
                    if self.apply_struct_builder_creation(ast, builder_name, struct_name)? {
                        changes_made = true;
                    }
                }
                ConstEval::CompileError { message } => {
                    return Err(fp_core::error::Error::Generic(eyre!(
                        "Compile error: {}",
                        message
                    )));
                }
                ConstEval::CompileWarning { message } => {
                    tracing::warn!("Compile warning: {}", message);
                }
                ConstEval::GenerateFunctionCall {
                    function_name,
                    args,
                } => {
                    self.apply_function_call_generation(ast, function_name, args)?;
                    changes_made = true;
                }
            }
        }

        // Clear applied operations before returning
        self.clear();
        Ok(changes_made)
    }

    /// Apply function call generation request
    /// For now this is a stub that preserves the intent without mutating the AST.
    /// A future implementation can inject an explicit call node into the AST.
    fn apply_function_call_generation(
        &self,
        _ast: &mut Node,
        _function_name: &str,
        _args: &Vec<Value>,
    ) -> Result<()> {
        // Intentionally a no-op for now. This ensures const-eval operations are accounted for
        // without causing compilation failures until full lowering is implemented.
        Ok(())
    }

    /// Apply field generation request
    fn apply_field_generation(
        &self,
        ast: &mut Node,
        target_type: &str,
        field_name: &str,
        field_type: &Ty,
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

    /// Apply method generation request
    fn apply_method_generation(
        &self,
        ast: &mut Node,
        target_type: &str,
        method_name: &str,
        method_body: &Expr,
    ) -> Result<()> {
        let function = create_method_function(method_name, method_body);

        if let Some(impl_block) = find_impl_mut(ast, target_type, None) {
            if method_exists(impl_block, method_name) {
                return Ok(());
            }
            impl_block.items.push(Item::DefFunction(function));
            Ok(())
        } else {
            let impl_item =
                ItemImpl::new_ident(Ident::new(target_type), vec![Item::DefFunction(function)]);
            push_item(ast, Item::Impl(impl_item))
        }
    }

    /// Apply type generation request
    fn apply_type_generation(
        &self,
        ast: &mut Node,
        type_name: &str,
        type_definition: &Ty,
    ) -> Result<()> {
        if find_struct_mut(ast, type_name).is_some() {
            return Ok(());
        }

        if let Ty::Struct(struct_ty) = type_definition {
            let mut struct_ty_clone = struct_ty.clone();
            struct_ty_clone.name = Ident::new(type_name);

            let struct_item = ItemDefStruct {
                visibility: Visibility::Public,
                name: Ident::new(type_name),
                value: struct_ty_clone,
            };

            push_item(ast, Item::DefStruct(struct_item))?;
        }

        Ok(())
    }

    /// Apply struct builder creation request
    fn apply_struct_builder_creation(
        &self,
        _ast: &mut Node,
        _builder_name: &str,
        _struct_name: &str,
    ) -> Result<bool> {
        // TODO: Implement struct builder creation
        Ok(false)
    }
}

impl ConstEval {
    /// Get a description of this const-eval operation for debugging
    pub fn description(&self) -> String {
        match self {
            ConstEval::GenerateField {
                target_type,
                field_name,
                ..
            } => {
                format!("Add field {} to {}", field_name, target_type)
            }
            ConstEval::GenerateMethod {
                target_type,
                method_name,
                ..
            } => {
                format!("Add method {} to {}", method_name, target_type)
            }
            ConstEval::GenerateType { type_name, .. } => {
                format!("Generate type {}", type_name)
            }
            ConstEval::CreateStructBuilder {
                builder_name,
                struct_name,
            } => {
                format!("Create struct builder {} for {}", builder_name, struct_name)
            }
            ConstEval::CompileError { message } => {
                format!("Compile error: {}", message)
            }
            ConstEval::CompileWarning { message } => {
                format!("Compile warning: {}", message)
            }
            ConstEval::GenerateFunctionCall {
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

fn find_struct_mut<'a>(node: &'a mut Node, target: &str) -> Option<&'a mut ItemDefStruct> {
    match node {
        Node::File(file) => find_struct_in_items_mut(&mut file.items, target),
        Node::Item(item) => match item {
            Item::DefStruct(def) if def.name.as_str() == target => Some(def),
            Item::Module(module) => find_struct_in_items_mut(&mut module.items, target),
            _ => None,
        },
        Node::Expr(_) => None,
    }
}

fn find_struct_in_items_mut<'a>(
    items: &'a mut [Item],
    target: &str,
) -> Option<&'a mut ItemDefStruct> {
    for item in items.iter_mut() {
        match item {
            Item::DefStruct(def) if def.name.as_str() == target => return Some(def),
            Item::Module(module) => {
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
    node: &'a mut Node,
    target: &str,
    trait_name: Option<&str>,
) -> Option<&'a mut ItemImpl> {
    match node {
        Node::File(file) => find_impl_in_items_mut(&mut file.items, target, trait_name),
        Node::Item(item) => match item {
            Item::Impl(item_impl) => {
                if impl_matches(item_impl, target, trait_name) {
                    Some(item_impl)
                } else {
                    None
                }
            }
            Item::Module(module) => find_impl_in_items_mut(&mut module.items, target, trait_name),
            _ => None,
        },
        Node::Expr(_) => None,
    }
}

fn find_impl_in_items_mut<'a>(
    items: &'a mut [Item],
    target: &str,
    trait_name: Option<&str>,
) -> Option<&'a mut ItemImpl> {
    for item in items.iter_mut() {
        match item {
            Item::Impl(item_impl) => {
                if impl_matches(item_impl, target, trait_name) {
                    return Some(item_impl);
                }
            }
            Item::Module(module) => {
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
        Expr::Locator(Locator::Ident(ident)) if ident.as_str() == target
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
        Item::DefFunction(func) => func.name.as_str() == method_name,
        _ => false,
    })
}

fn create_method_function(method_name: &str, method_body: &Expr) -> ItemDefFunction {
    ItemDefFunction::new_simple(Ident::new(method_name), method_body.clone().into())
        .with_receiver(FunctionParamReceiver::Ref)
}

fn push_item(ast: &mut Node, item: Item) -> Result<()> {
    match ast {
        Node::File(file) => {
            file.items.push(item);
            Ok(())
        }
        Node::Item(Item::Module(module)) => {
            module.items.push(item);
            Ok(())
        }
        _ => Err(fp_core::error::Error::Generic(eyre!(
            "Unable to insert generated item into AST at this location"
        ))),
    }
}
