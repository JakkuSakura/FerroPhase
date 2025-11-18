use super::context::LlvmContext;
use llvm_ir::function::{CallingConvention, Parameter};
use llvm_ir::module::{DLLStorageClass, Linkage, Visibility};
use llvm_ir::{Function, Name, Type};

impl LlvmContext {
    /// Create a function declaration
    pub fn declare_function(
        &mut self,
        name: &str,
        param_types: &[Type],
        return_type: Type,
    ) -> String {
        let function = Function {
            name: name.to_string(),
            parameters: param_types
                .iter()
                .enumerate()
                .map(|(i, ty)| Parameter {
                    name: Name::Name(Box::new(format!("arg{}", i))),
                    ty: self.module.types.get_for_type(ty),
                    attributes: vec![],
                })
                .collect(),
            is_var_arg: false,
            visibility: Visibility::Default,
            linkage: Linkage::External,
            calling_convention: CallingConvention::C,
            section: None,
            comdat: None,
            alignment: 0,
            garbage_collector_name: None,
            personality_function: None,
            basic_blocks: vec![],
            function_attributes: vec![],
            return_attributes: vec![],
            dll_storage_class: DLLStorageClass::Default,
            debugloc: None,
            return_type: self.module.types.get_for_type(&return_type),
        };

        // For now, append to functions vector
        self.module.functions.push(function);
        name.to_string()
    }

    /// Create a function definition with body
    pub fn define_function(
        &mut self,
        name: &str,
        param_types: &[Type],
        return_type: Type,
    ) -> String {
        let function_name = self.declare_function(name, param_types, return_type);
        self.current_function = Some(function_name.clone());
        function_name
    }

    /// Get a function by name
    pub fn get_function(&self, name: &str) -> Option<&Function> {
        self.module.functions.iter().find(|f| f.name == name)
    }

    /// Get the current function being built
    pub fn current_function(&self) -> Option<String> {
        self.current_function.clone()
    }
}

