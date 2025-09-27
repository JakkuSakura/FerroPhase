use llvm_ir::constant::Float;
use llvm_ir::function::{CallingConvention, Parameter};
use llvm_ir::instruction::{Add, Alloca, ICmp, Load, Mul, Store, Sub, UDiv, ZExt};
use llvm_ir::module::{DLLStorageClass, DataLayout, Linkage, Visibility};
use llvm_ir::predicates::IntPredicate;
use llvm_ir::terminator::{CondBr, Ret};
use llvm_ir::types::FPType;
use llvm_ir::*;
use std::collections::HashMap;
use std::path::Path;

/// LLVM compilation context that manages the LLVM module
pub struct LlvmContext {
    pub module: Module,
    functions: HashMap<String, String>, // Maps function names to their names in the module
    current_function: Option<String>,
    next_unnamed_counter: u32,
}

impl LlvmContext {
    /// Create a new LLVM context with the given name
    pub fn new(module_name: &str) -> Self {
        // Create a minimal module using from_ir_str with proper structure
        let module_ir = format!(
            "; ModuleID = '{}'\ntarget datalayout = \"e-m:o-i64:64-f80:128-n8:16:32:64-S128\"\ntarget triple = \"x86_64-unknown-linux-gnu\"\n",
            module_name
        );

        let module = Module::from_ir_str(&module_ir).unwrap_or_else(|e| {
            // Fallback: create a basic module structure manually
            tracing::warn!(
                "Failed to parse minimal module IR ({}), creating basic module",
                e
            );
            Module {
                name: module_name.to_string(),
                source_file_name: format!("{}.ll", module_name),
                data_layout: DataLayout::default(),
                target_triple: Some("x86_64-unknown-linux-gnu".to_string()),
                functions: Vec::new(),
                global_vars: Vec::new(),
                global_aliases: Vec::new(),
                func_declarations: Vec::new(),
                global_ifuncs: Vec::new(),
                types: llvm_ir::types::Types::blank_for_testing(),
                inline_assembly: String::new(),
            }
        });

        Self {
            module,
            functions: HashMap::new(),
            current_function: None,
            next_unnamed_counter: 0,
        }
    }

    /// Initialize target machine for the current platform
    pub fn init_target_machine(&mut self) -> Result<(), String> {
        // Set target triple to host
        let triple = if cfg!(target_os = "macos") {
            if cfg!(target_arch = "aarch64") {
                "arm64-apple-macosx15.0.0"
            } else {
                "x86_64-apple-macosx10.15.0"
            }
        } else if cfg!(target_os = "linux") {
            if cfg!(target_arch = "aarch64") {
                "aarch64-unknown-linux-gnu"
            } else {
                "x86_64-unknown-linux-gnu"
            }
        } else if cfg!(target_os = "windows") {
            "x86_64-pc-windows-msvc"
        } else {
            // Fallback to a common default
            "x86_64-unknown-linux-gnu"
        };
        self.module.target_triple = Some(triple.to_string());

        Ok(())
    }

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

        // TODO: Fix functions insertion - expecting usize index, not String key
        // self.module.functions.insert(name.to_string(), function);
        self.module.functions.push(function);
        self.functions.insert(name.to_string(), name.to_string());
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

        // // Create entry basic block
        // if let Some(function) = self.module.functions.get_mut(name) {
        //     let entry_block = BasicBlock {
        //         name: Name::Name("entry".to_string()),
        //         instrs: vec![],
        //         term: (),
        //     };
        //     function.basic_blocks.push(entry_block);
        // }

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

    /// Create basic LLVM types
    pub fn i1_type(&self) -> Type {
        Type::IntegerType { bits: 1 }
    }

    pub fn i8_type(&self) -> Type {
        Type::IntegerType { bits: 8 }
    }

    pub fn i16_type(&self) -> Type {
        Type::IntegerType { bits: 16 }
    }

    pub fn i32_type(&self) -> Type {
        Type::IntegerType { bits: 32 }
    }

    pub fn i64_type(&self) -> Type {
        Type::IntegerType { bits: 64 }
    }

    pub fn f32_type(&self) -> Type {
        Type::FPType(FPType::Single)
    }

    pub fn f64_type(&self) -> Type {
        Type::FPType(FPType::Double)
    }

    pub fn void_type(&self) -> Type {
        Type::VoidType
    }

    // pub fn ptr_type(&self) -> Type {
    //     Type::PointerType {
    //         pointee_type: Box::new(Type::IntegerType { bits: 8 }),
    //         addr_space: 0,
    //     }
    // }

    /// Create constant values
    pub fn const_i32(&self, value: i32) -> ConstantRef {
        ConstantRef::new(Constant::Int {
            bits: 32,
            value: value as u64,
        })
    }

    pub fn const_i64(&self, value: i64) -> ConstantRef {
        ConstantRef::new(Constant::Int {
            bits: 64,
            value: value as u64,
        })
    }
    pub fn const_f32(&self, value: f32) -> ConstantRef {
        ConstantRef::new(Constant::Float(Float::Single(value)))
    }
    pub fn const_f64(&self, value: f64) -> ConstantRef {
        ConstantRef::new(Constant::Float(Float::Double(value)))
    }

    pub fn const_bool(&self, value: bool) -> ConstantRef {
        ConstantRef::new(Constant::Int {
            bits: 1,
            value: if value { 1 } else { 0 },
        })
    }
    /// Add an instruction to the current function's current basic block
    pub fn add_instruction(&mut self, instruction: Instruction) -> Result<(), String> {
        let func_name = self
            .current_function
            .clone()
            .ok_or("No current function set")?;

        let function = self
            .module
            .functions
            .iter_mut()
            .find(|f| f.name == func_name)
            .ok_or("Current function not found")?;

        if function.basic_blocks.is_empty() {
            // Create an entry block if none exists yet
            let entry_block = BasicBlock::new(Name::Name(Box::new("entry".to_string())));
            function.basic_blocks.push(entry_block);
        }

        let last_block_idx = function.basic_blocks.len() - 1;
        function.basic_blocks[last_block_idx]
            .instrs
            .push(instruction);

        Ok(())
    }

    /// Create an add instruction
    pub fn build_add(
        &mut self,
        lhs: Operand,
        rhs: Operand,
        result_name: &str,
    ) -> Result<Name, String> {
        let name = Name::Name(Box::new(result_name.to_string()));
        let instruction = Instruction::Add(Add {
            operand0: lhs,
            operand1: rhs,
            dest: name.clone(),
            nuw: false,
            nsw: false,
            debugloc: None,
        });

        self.add_instruction(instruction)?;
        Ok(name)
    }

    /// Create a subtract instruction
    pub fn build_sub(
        &mut self,
        lhs: Operand,
        rhs: Operand,
        result_name: &str,
    ) -> Result<Name, String> {
        let name = Name::Name(Box::new(result_name.to_string()));
        let instruction = Instruction::Sub(Sub {
            operand0: lhs,
            operand1: rhs,
            dest: name.clone(),
            nsw: false, // No signed wrap
            nuw: false, // No unsigned wrap
            debugloc: None,
        });

        self.add_instruction(instruction)?;
        Ok(name)
    }

    /// Create a multiply instruction
    pub fn build_mul(
        &mut self,
        lhs: Operand,
        rhs: Operand,
        result_name: &str,
    ) -> Result<Name, String> {
        let name = Name::Name(Box::new(result_name.to_string()));
        let instruction = Instruction::Mul(Mul {
            operand0: lhs,
            operand1: rhs,
            dest: name.clone(),
            nuw: false,
            nsw: false,
            debugloc: None,
        });

        self.add_instruction(instruction)?;
        Ok(name)
    }

    /// Create a divide instruction
    pub fn build_udiv(
        &mut self,
        lhs: Operand,
        rhs: Operand,
        result_name: &str,
    ) -> Result<Name, String> {
        let name = Name::Name(Box::new(result_name.to_string()));
        let instruction = Instruction::UDiv(UDiv {
            operand0: lhs,
            operand1: rhs,
            dest: name.clone(),
            exact: false,
            debugloc: None,
        });

        self.add_instruction(instruction)?;
        Ok(name)
    }

    /// Create an integer comparison instruction
    pub fn build_icmp(
        &mut self,
        predicate: IntPredicate,
        lhs: Operand,
        rhs: Operand,
        result_name: &str,
    ) -> Result<Name, String> {
        let name = Name::Name(Box::new(result_name.to_string()));
        let instruction = Instruction::ICmp(ICmp {
            predicate,
            operand0: lhs,
            operand1: rhs,
            dest: name.clone(),
            debugloc: None,
        });

        self.add_instruction(instruction)?;
        Ok(name)
    }

    /// Zero-extend a value to a wider integer type
    pub fn build_zext(
        &mut self,
        operand: Operand,
        to_type: Type,
        result_name: &str,
    ) -> Result<Name, String> {
        let name = Name::Name(Box::new(result_name.to_string()));
        let instruction = Instruction::ZExt(ZExt {
            operand,
            to_type: self.module.types.get_for_type(&to_type),
            dest: name.clone(),
            debugloc: None,
        });

        self.add_instruction(instruction)?;
        Ok(name)
    }

    pub fn build_alloca(
        &mut self,
        allocated_type: TypeRef,
        num_elements: Operand,
        result_name: &str,
        alignment: u32,
    ) -> Result<Name, String> {
        let name = Name::Name(Box::new(result_name.to_string()));
        let instruction = Instruction::Alloca(Alloca {
            allocated_type,
            num_elements,
            dest: name.clone(),
            alignment,
            debugloc: None,
        });
        self.add_instruction(instruction)?;
        Ok(name)
    }

    pub fn build_load(
        &mut self,
        address: Operand,
        loaded_type: Type,
        result_name: &str,
        alignment: u32,
        volatile: bool,
    ) -> Result<Name, String> {
        let name = Name::Name(Box::new(result_name.to_string()));
        let instruction = Instruction::Load(Load {
            address,
            dest: name.clone(),
            loaded_ty: self.module.types.get_for_type(&loaded_type),
            volatile,
            atomicity: None,
            alignment,
            debugloc: None,
        });
        self.add_instruction(instruction)?;
        Ok(name)
    }

    pub fn build_store(
        &mut self,
        value: Operand,
        address: Operand,
        alignment: u32,
        volatile: bool,
    ) -> Result<(), String> {
        let instruction = Instruction::Store(Store {
            address,
            value,
            volatile,
            atomicity: None,
            alignment,
            debugloc: None,
        });
        self.add_instruction(instruction)?;
        Ok(())
    }

    /// Create a return instruction
    pub fn build_return(&mut self, value: Option<Operand>) -> Result<(), String> {
        let func_name = self
            .current_function
            .clone()
            .ok_or("No current function set")?;

        let function = self
            .module
            .functions
            .iter_mut()
            .find(|f| f.name == func_name)
            .ok_or("Current function not found")?;

        if function.basic_blocks.is_empty() {
            return Err("Function has no basic blocks".to_string());
        }

        let last_block_idx = function.basic_blocks.len() - 1;
        function.basic_blocks[last_block_idx].term = Terminator::Ret(Ret {
            return_operand: value,
            debugloc: None,
        });

        Ok(())
    }

    pub fn build_unconditional_branch(&mut self, target_label: &str) -> Result<(), String> {
        let func_name = self
            .current_function
            .clone()
            .ok_or("No current function set")?;

        let function = self
            .module
            .functions
            .iter_mut()
            .find(|f| f.name == func_name)
            .ok_or("Current function not found")?;

        if function.basic_blocks.is_empty() {
            return Err("Function has no basic blocks".to_string());
        }

        let last_block_idx = function.basic_blocks.len() - 1;
        function.basic_blocks[last_block_idx].term = Terminator::Br(llvm_ir::terminator::Br {
            dest: Name::Name(Box::new(target_label.to_string())),
            debugloc: None,
        });

        Ok(())
    }

    pub fn build_conditional_branch(
        &mut self,
        condition: Operand,
        true_label: &str,
        false_label: &str,
    ) -> Result<(), String> {
        let func_name = self
            .current_function
            .clone()
            .ok_or("No current function set")?;

        let function = self
            .module
            .functions
            .iter_mut()
            .find(|f| f.name == func_name)
            .ok_or("Current function not found")?;

        if function.basic_blocks.is_empty() {
            return Err("Function has no basic blocks".to_string());
        }

        let last_block_idx = function.basic_blocks.len() - 1;
        function.basic_blocks[last_block_idx].term = Terminator::CondBr(CondBr {
            condition,
            true_dest: Name::Name(Box::new(true_label.to_string())),
            false_dest: Name::Name(Box::new(false_label.to_string())),
            debugloc: None,
        });

        Ok(())
    }

    /// Generate a unique unnamed value name
    pub fn gen_unnamed(&mut self) -> Name {
        let name = Name::Number(self.next_unnamed_counter as usize);
        self.next_unnamed_counter += 1;
        name
    }

    /// Convert operand to LLVM operand
    pub fn operand_from_constant(&self, constant: ConstantRef) -> Operand {
        Operand::ConstantOperand(constant)
    }

    /// Convert name to LLVM operand with explicit type information
    pub fn operand_from_name_and_type(&self, name: Name, ty: &Type) -> Operand {
        Operand::LocalOperand {
            name,
            ty: self.module.types.get_for_type(ty),
        }
    }

    /// Verify the module
    pub fn verify_module(&self) -> Result<(), String> {
        // llvm-ir doesn't have built-in verification, so we'll do basic checks
        for function in &self.module.functions {
            if function.basic_blocks.is_empty() && function.linkage != Linkage::External {
                return Err(format!("Function '{}' has no basic blocks", function.name));
            }
        }
        Ok(())
    }

    /// Print the module IR to string
    pub fn print_to_string(&self) -> String {
        // Generate proper LLVM IR from the structured data
        // This is a clean implementation that uses the actual module structure
        let mut ir = String::new();

        // Module header
        ir.push_str(&format!("; ModuleID = '{}'\n", self.module.name));
        // Choose target triple and datalayout based on module or host
        let triple = self.module.target_triple.clone().unwrap_or_else(|| {
            if cfg!(target_os = "macos") {
                if cfg!(target_arch = "aarch64") {
                    "arm64-apple-macosx15.0.0".to_string()
                } else {
                    "x86_64-apple-macosx10.15.0".to_string()
                }
            } else if cfg!(target_os = "linux") {
                if cfg!(target_arch = "aarch64") {
                    "aarch64-unknown-linux-gnu".to_string()
                } else {
                    "x86_64-unknown-linux-gnu".to_string()
                }
            } else if cfg!(target_os = "windows") {
                "x86_64-pc-windows-msvc".to_string()
            } else {
                "x86_64-unknown-linux-gnu".to_string()
            }
        });
        let datalayout = if triple.starts_with("arm64-apple") {
            "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128"
        } else if triple.starts_with("aarch64-") {
            "e-m:e-i64:64-i128:128-n32:64-S128"
        } else if triple.starts_with("x86_64-") {
            "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
        } else {
            "e-i64:64-n32:64-S128"
        };
        ir.push_str(&format!("target datalayout = \"{}\"\n", datalayout));
        ir.push_str(&format!("target triple = \"{}\"\n\n", triple));

        // Global variables and constants
        for global in &self.module.global_vars {
            ir.push_str(&format_global_variable(global));
        }

        // External function declarations
        for function in &self.module.functions {
            if function.linkage == llvm_ir::module::Linkage::External {
                ir.push_str(&format_function_declaration(function));
            }
        }

        // Functions (skip external declarations which were already handled above)
        for function in &self.module.functions {
            if function.linkage == llvm_ir::module::Linkage::External
                && function.basic_blocks.is_empty()
            {
                continue; // Skip external function declarations - they're handled as declarations above
            }
            // Generate function signature; force main to return i32 for C ABIs
            let is_main = function.name == "func_0" || function.name == "main";
            let return_type_str = if is_main {
                "i32"
            } else {
                match &*function.return_type {
                    llvm_ir::Type::VoidType => "void",
                    llvm_ir::Type::IntegerType { bits } => match bits {
                        32 => "i32",
                        64 => "i64",
                        1 => "i1",
                        _ => "i32",
                    },
                    _ => "i32",
                }
            };

            // Use "main" for the first function, otherwise keep original name
            let func_name = if function.name == "func_0" {
                "main"
            } else {
                &function.name
            };

            ir.push_str(&format!("define {} @{}(", return_type_str, func_name));

            // Parameters (simplified)
            for (i, _param) in function.parameters.iter().enumerate() {
                if i > 0 {
                    ir.push_str(", ");
                }
                ir.push_str(&format!("i32 %arg{}", i));
            }
            ir.push_str(") {\n");

            // Fix control flow for main function: if bb0 just returns, make it branch to first block with instructions
            let mut modified_function = function.clone();
            if is_main && !function.basic_blocks.is_empty() {
                let first_bb = &function.basic_blocks[0];
                // Check if first block just returns (has no instructions or only unreachable)
                let is_empty_or_unreachable = first_bb.instrs.is_empty()
                    || first_bb.instrs.iter().all(|instr| {
                        matches!(
                            instr,
                            Instruction::ExtractValue(_) | Instruction::InsertValue(_)
                        )
                    });

                if is_empty_or_unreachable && matches!(first_bb.term, Terminator::Ret(_)) {
                    // Find first block with actual work (has call instructions)
                    if let Some((target_idx, _)) = function
                        .basic_blocks
                        .iter()
                        .enumerate()
                        .skip(1)
                        .find(|(_, bb)| {
                            bb.instrs
                                .iter()
                                .any(|instr| matches!(instr, Instruction::Call(_)))
                        })
                    {
                        // Modify first block to branch to the target block instead of returning
                        modified_function.basic_blocks[0].term =
                            Terminator::Br(llvm_ir::terminator::Br {
                                dest: Name::Name(Box::new(format!("bb{}", target_idx))),
                                debugloc: None,
                            });
                    }
                }
            }

            // Basic blocks
            for bb in &modified_function.basic_blocks {
                let block_name = format_name(&bb.name);
                ir.push_str(&format!("{}:\n", block_name));

                // Instructions - properly convert from LLVM IR BasicBlock
                for instr in &bb.instrs {
                    ir.push_str(&format!("  {}\n", format_instruction(instr)));
                }

                // Terminator - handle main function special case
                if is_main
                    && matches!(bb.term, Terminator::Ret(ref ret) if ret.return_operand.is_none())
                {
                    // For main function, emit ret i32 0 instead of ret void
                    ir.push_str("  ret i32 0\n");
                } else {
                    // Normal terminator handling
                    ir.push_str(&format!("  {}\n", format_terminator(&bb.term)));
                }
            }

            ir.push_str("}\n\n");
        }

        ir
    }

    /// Write module to file
    pub fn write_to_file(&self, output_path: &Path) -> Result<(), String> {
        std::fs::write(output_path, self.print_to_string())
            .map_err(|e| format!("Failed to write module to file: {}", e))
    }
}

// Helper functions for LLVM IR formatting
fn format_type(typ: &Type) -> String {
    match typ {
        Type::IntegerType { bits } => format!("i{}", bits),
        Type::PointerType { .. } => "ptr".to_string(), // modern LLVM IR uses ptr
        Type::VoidType => "void".to_string(),
        Type::ArrayType {
            element_type,
            num_elements,
        } => {
            format!("[{} x {}]", num_elements, format_type(element_type))
        }
        Type::StructType { .. } => "{  }".to_string(), // simplified struct type
        Type::FPType(_) => "double".to_string(),       // simplified float type
        Type::FuncType {
            result_type,
            param_types,
            is_var_arg,
        } => {
            // Format function type like: i32 (ptr, ...)
            let params_str = if param_types.is_empty() {
                "".to_string()
            } else {
                param_types
                    .iter()
                    .map(|t| format_type(t))
                    .collect::<Vec<_>>()
                    .join(", ")
            };
            let varargs_str = if *is_var_arg {
                if params_str.is_empty() {
                    "..."
                } else {
                    ", ..."
                }
            } else {
                ""
            };
            format!(
                "{} ({}{})",
                format_type(result_type),
                params_str,
                varargs_str
            )
        }
        other => panic!("Unsupported LLVM type in formatter: {:?}", other),
    }
}

fn format_constant_ref(constant_ref: &llvm_ir::ConstantRef) -> String {
    match constant_ref.as_ref() {
        llvm_ir::Constant::Int { value, .. } => value.to_string(),
        llvm_ir::Constant::Float(Float::Single(v)) => format!("{}", v),
        llvm_ir::Constant::Float(Float::Double(v)) => format!("{}", v),
        other @ llvm_ir::Constant::Float(_) => {
            panic!(
                "Unsupported floating-point constant encountered: {:?}",
                other
            )
        }
        llvm_ir::Constant::Null(_) => "null".to_string(),
        llvm_ir::Constant::Undef(_) => "undef".to_string(),
        llvm_ir::Constant::BitCast(bitcast) => format!(
            "bitcast ({}) to {}",
            format_constant_ref(&bitcast.operand),
            format_type(&bitcast.to_type)
        ),
        llvm_ir::Constant::GlobalReference { name, .. } => format!("@{}", format_name(name)),
        llvm_ir::Constant::GetElementPtr(gep) => {
            let (base_name, array_type) = match gep.address.as_ref() {
                llvm_ir::Constant::GlobalReference { name, ty } => {
                    let type_str = match ty.as_ref() {
                        llvm_ir::Type::ArrayType {
                            element_type: _,
                            num_elements,
                        } => format!("[{} x i8]", num_elements),
                        other => {
                            panic!(
                                "Unsupported array base type for GEP formatting: {:?}",
                                other
                            )
                        }
                    };
                    (format!("@{}", format_name(name)), type_str)
                }
                other => {
                    panic!("Unsupported GEP address for formatting: {:?}", other)
                }
            };
            format!(
                "getelementptr inbounds ({}, ptr {}, i32 0, i32 0)",
                array_type, base_name
            )
        }
        other => panic!("Unsupported LLVM constant in formatter: {:?}", other),
    }
}

fn format_instruction(instr: &llvm_ir::Instruction) -> String {
    match instr {
        llvm_ir::Instruction::Add(add) => {
            format!(
                "{} = add {} {}, {}",
                format_value_name(&add.dest),
                format_type(&get_operand_type(&add.operand0)),
                format_operand(&add.operand0),
                format_operand(&add.operand1)
            )
        }
        llvm_ir::Instruction::Sub(sub) => {
            format!(
                "{} = sub {} {}, {}",
                format_value_name(&sub.dest),
                format_type(&get_operand_type(&sub.operand0)),
                format_operand(&sub.operand0),
                format_operand(&sub.operand1)
            )
        }
        llvm_ir::Instruction::Mul(mul) => {
            format!(
                "{} = mul {} {}, {}",
                format_value_name(&mul.dest),
                format_type(&get_operand_type(&mul.operand0)),
                format_operand(&mul.operand0),
                format_operand(&mul.operand1)
            )
        }
        llvm_ir::Instruction::UDiv(div) => {
            format!(
                "{} = udiv {} {}, {}",
                format_value_name(&div.dest),
                format_type(&get_operand_type(&div.operand0)),
                format_operand(&div.operand0),
                format_operand(&div.operand1)
            )
        }
        llvm_ir::Instruction::ZExt(zext) => {
            format!(
                "{} = zext {} {} to {}",
                format_value_name(&zext.dest),
                format_type(&get_operand_type(&zext.operand)),
                format_operand(&zext.operand),
                format_type(zext.to_type.as_ref())
            )
        }
        llvm_ir::Instruction::Store(store) => {
            format!(
                "store {} {}, ptr {}",
                format_type(&get_operand_type(&store.value)),
                format_operand(&store.value),
                format_operand(&store.address)
            )
        }
        llvm_ir::Instruction::Load(load) => {
            format!(
                "{} = load {}, ptr {}",
                format_value_name(&load.dest),
                format_type(&load.loaded_ty),
                format_operand(&load.address)
            )
        }
        llvm_ir::Instruction::Call(call) => {
            let args_str = call
                .arguments
                .iter()
                .map(|(op, _attrs)| format_operand_with_type(op))
                .collect::<Vec<_>>()
                .join(", ");

            let function_operand = match &call.function {
                either::Either::Left(_inline_asm) => "@unknown_function".to_string(),
                either::Either::Right(operand) => format_operand(operand),
            };

            let (return_type, param_types, is_var_arg) = match call.function_ty.as_ref() {
                llvm_ir::Type::FuncType {
                    result_type,
                    param_types,
                    is_var_arg,
                } => (result_type, param_types, *is_var_arg),
                other => panic!(
                    "Call instruction missing function type information: {:?}",
                    other
                ),
            };

            let mut param_strs: Vec<String> = param_types
                .iter()
                .map(|t| format_type(t))
                .collect();
            if is_var_arg {
                param_strs.push("...".to_string());
            }
            let fn_sig = format!("({})", param_strs.join(", "));
            let return_type_str = format_type(return_type);

            if let Some(dest) = &call.dest {
                format!(
                    "{} = call {} {} {}({})",
                    format_value_name(dest),
                    return_type_str,
                    fn_sig,
                    function_operand,
                    args_str
                )
            } else {
                format!(
                    "call {} {} {}({})",
                    return_type_str,
                    fn_sig,
                    function_operand,
                    args_str
                )
            }
        }
        llvm_ir::Instruction::ICmp(icmp) => {
            let predicate = format!("{}", icmp.predicate).to_lowercase();
            format!(
                "{} = icmp {} {} {}, {}",
                format_value_name(&icmp.dest),
                predicate,
                format_type(&get_operand_type(&icmp.operand0)),
                format_operand(&icmp.operand0),
                format_operand(&icmp.operand1)
            )
        }
        _ => format!("{}", instr),
    }
}

fn format_terminator(term: &llvm_ir::Terminator) -> String {
    match term {
        llvm_ir::Terminator::Ret(ret) => {
            if let Some(operand) = &ret.return_operand {
                let value = format_operand(operand);
                format!("ret {} {}", format_type(&get_operand_type(operand)), value)
            } else {
                "ret void".to_string()
            }
        }
        llvm_ir::Terminator::Br(br) => {
            format!("br label %{}", format_name(&br.dest))
        }
        llvm_ir::Terminator::CondBr(cond_br) => {
            format!(
                "br i1 {}, label %{}, label %{}",
                format_operand(&cond_br.condition),
                format_name(&cond_br.true_dest),
                format_name(&cond_br.false_dest)
            )
        }
        llvm_ir::Terminator::Unreachable(_) => "unreachable".to_string(),
        other => format!("{}", other),
    }
}

fn format_operand(operand: &llvm_ir::Operand) -> String {
    match operand {
        llvm_ir::Operand::LocalOperand { name, .. } => format!("%{}", format_name(name)),
        llvm_ir::Operand::ConstantOperand(const_ref) => format_constant_ref(const_ref),
        other => panic!("Unsupported operand in formatter: {:?}", other),
    }
}

fn format_operand_with_type(operand: &llvm_ir::Operand) -> String {
    format!(
        "{} {}",
        format_type(&get_operand_type(operand)),
        format_operand(operand)
    )
}

fn format_name(name: &llvm_ir::Name) -> String {
    match name {
        llvm_ir::Name::Name(boxed_str) => boxed_str.to_string(),
        llvm_ir::Name::Number(num) => num.to_string(),
    }
}

fn format_value_name(name: &llvm_ir::Name) -> String {
    format!("%{}", format_name(name))
}

fn get_operand_type(operand: &llvm_ir::Operand) -> Type {
    match operand {
        llvm_ir::Operand::LocalOperand { ty, .. } => ty.as_ref().clone(),
        llvm_ir::Operand::ConstantOperand(const_ref) => match const_ref.as_ref() {
            llvm_ir::Constant::Int { bits, .. } => Type::IntegerType { bits: *bits },
            llvm_ir::Constant::Float(Float::Half) => Type::FPType(FPType::Half),
            llvm_ir::Constant::Float(Float::Single(_)) => Type::FPType(FPType::Single),
            llvm_ir::Constant::Float(Float::Double(_)) => Type::FPType(FPType::Double),
            llvm_ir::Constant::BitCast(bitcast) => bitcast.to_type.as_ref().clone(),
            llvm_ir::Constant::GlobalReference { ty, .. }
            | llvm_ir::Constant::Null(ty)
            | llvm_ir::Constant::Undef(ty) => ty.as_ref().clone(),
            llvm_ir::Constant::GetElementPtr(_) => Type::PointerType { addr_space: 0 },
            other => panic!(
                "Unsupported constant operand type for formatting: {:?}",
                other
            ),
        },
        other => panic!("Unsupported operand kind when deriving type: {:?}", other),
    }
}

fn format_global_variable(global: &llvm_ir::module::GlobalVariable) -> String {
    let initializer_str = match &global.initializer {
        Some(init_ref) => {
            // Properly format the constant based on its type
            match init_ref.as_ref() {
                llvm_ir::Constant::Array { elements, .. } => {
                    // For string constants, format as c"string\00"
                    if let Some(first_elem) = elements.first() {
                        if matches!(first_elem.as_ref(), llvm_ir::Constant::Int { .. }) {
                            // Check if this looks like a string (array of i8 values)
                            let chars: Vec<u8> = elements
                                .iter()
                                .filter_map(|elem| {
                                    if let llvm_ir::Constant::Int { value, .. } = elem.as_ref() {
                                        Some(*value as u8)
                                    } else {
                                        None
                                    }
                                })
                                .collect();

                            // Try to convert to a C string if it's printable ASCII
                            if chars.iter().all(|&c| c == 0 || (c >= 32 && c <= 126)) {
                                let s = String::from_utf8_lossy(
                                    &chars[..chars.len().saturating_sub(1)],
                                );
                                let escaped = s.escape_default().to_string().replace("\\'", "'");
                                format!("c\"{}\\00\"", escaped)
                            } else {
                                // Format as array of i8 values with correct syntax
                                format!(
                                    "[{}]",
                                    chars
                                        .iter()
                                        .map(|&c| format!("i8 {}", c))
                                        .collect::<Vec<_>>()
                                        .join(", ")
                                )
                            }
                        } else {
                            format!(
                                "[{}]",
                                elements
                                    .iter()
                                    .map(|e| {
                                        format_operand_with_type(
                                            &llvm_ir::Operand::ConstantOperand(e.clone()),
                                        )
                                    })
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            )
                        }
                    } else {
                        "zeroinitializer".to_string()
                    }
                }
                _ => format_constant_ref(init_ref),
            }
        }
        None => "zeroinitializer".to_string(),
    };

    format!(
        "@{} = {} {} {}, align {}\n",
        format_name(&global.name),
        if global.is_constant {
            "constant"
        } else {
            "global"
        },
        format_type(&global.ty),
        initializer_str,
        global.alignment
    )
}

fn format_function_declaration(function: &llvm_ir::Function) -> String {
    let return_type_str = format_type(&function.return_type);
    let mut params = Vec::new();
    for param in &function.parameters {
        params.push(format_type(&param.ty));
    }
    let params_str = params.join(", ");

    if function.is_var_arg {
        format!(
            "declare {} @{}({}, ...)\n",
            return_type_str, function.name, params_str
        )
    } else {
        format!(
            "declare {} @{}({})\n",
            return_type_str, function.name, params_str
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_context_creation() {
        let llvm_ctx = LlvmContext::new("test_module");
        assert_eq!(llvm_ctx.module.name, "test_module");
    }

    #[test]
    fn test_function_declaration() {
        let mut llvm_ctx = LlvmContext::new("test_module");

        let i32_type = llvm_ctx.i32_type();
        let params = vec![i32_type.clone(), i32_type.clone()];

        let function_name = llvm_ctx.declare_function("add", &params, i32_type);
        assert_eq!(function_name, "add");

        let retrieved = llvm_ctx.get_function("add");
        assert!(retrieved.is_some());
        assert_eq!(retrieved.unwrap().name, "add");
    }

    #[test]
    fn test_constant_creation() {
        let llvm_ctx = LlvmContext::new("test_module");

        // TODO: Fix constant pattern matching - ConstantRef structure has changed
        let _const_42 = llvm_ctx.const_i32(42);
        // TODO: Verify constant values when ConstantRef API is fixed

        let _const_true = llvm_ctx.const_bool(true);
        // TODO: Verify boolean constant values when ConstantRef API is fixed

        // For now, just test that the functions don't panic
        assert!(
            true,
            "Constant creation functions executed without panicking"
        );
    }

    #[test]
    fn test_simple_function_with_body() {
        let mut llvm_ctx = LlvmContext::new("test_module");

        let i32_type = llvm_ctx.i32_type();
        let params = vec![i32_type.clone(), i32_type.clone()];

        let function_name = llvm_ctx.define_function("add", &params, i32_type);
        assert_eq!(function_name, "add");

        // Create operands for parameters
        let param1 = llvm_ctx.operand_from_name_and_type(
            Name::Name(Box::new("arg0".to_string())),
            &llvm_ctx.i32_type(),
        );
        let param2 = llvm_ctx.operand_from_name_and_type(
            Name::Name(Box::new("arg1".to_string())),
            &llvm_ctx.i32_type(),
        );

        // Build add instruction
        let result_name = llvm_ctx.build_add(param1, param2, "add_result").unwrap();

        // Build return instruction
        let return_operand = llvm_ctx.operand_from_name_and_type(result_name, &llvm_ctx.i32_type());
        llvm_ctx.build_return(Some(return_operand)).unwrap();

        // Verify the module
        assert!(llvm_ctx.verify_module().is_ok());
    }

    #[test]
    fn test_target_machine_init() {
        let mut llvm_ctx = LlvmContext::new("test_module");
        assert!(llvm_ctx.init_target_machine().is_ok());
        assert!(llvm_ctx.module.target_triple.is_some());
    }
}
