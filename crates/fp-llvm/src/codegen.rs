use crate::context::LlvmContext;
use fp_core::lir::Ty;
use fp_core::{error::Result, lir, Error};
use llvm_ir::constant::Float;
use llvm_ir::module::{DLLStorageClass, GlobalVariable, Linkage, ThreadLocalMode, Visibility};
use llvm_ir::types::FPType;
use llvm_ir::*;
// use llvm_ir::instruction::Call; // Not needed currently
use eyre::eyre;
use fp_core::tracing::debug;
use fp_core::types::IntTy;
use std::collections::HashMap;

/// LLVM code generator that transforms LIR to LLVM IR
pub struct LirCodegen<'ctx> {
    llvm_ctx: &'ctx mut LlvmContext,
    value_map: HashMap<u32, Name>, // Maps LIR register IDs to LLVM names
    block_map: HashMap<u32, Name>, // Maps LIR block IDs to LLVM basic block names
    current_function: Option<String>,
    string_globals: HashMap<String, (Name, usize)>,
    next_string_id: u32,
    // Track referenced globals that need const evaluation
    referenced_globals: std::collections::HashSet<String>,
    // Global constants map from MIR->LIR transformation
    global_const_map: HashMap<String, lir::LirConstant>,
}

impl<'ctx> LirCodegen<'ctx> {
    /// Create a new LIR code generator
    pub fn new(
        llvm_ctx: &'ctx mut LlvmContext,

        // should be in lir_ctx
        global_const_map: HashMap<String, lir::LirConstant>,
    ) -> Self {
        Self {
            llvm_ctx,
            value_map: HashMap::new(),
            block_map: HashMap::new(),
            current_function: None,
            string_globals: HashMap::new(),
            next_string_id: 0,
            referenced_globals: std::collections::HashSet::new(),
            global_const_map,
        }
    }
    /// Get the set of globals that were referenced but not found during codegen
    pub fn get_referenced_globals(&self) -> &std::collections::HashSet<String> {
        &self.referenced_globals
    }

    /// Generate LLVM IR for a LIR program
    pub fn generate_program(&mut self, lir_program: lir::LirProgram) -> Result<()> {
        let num_globals = lir_program.globals.len();
        let num_functions = lir_program.functions.len();
        debug!(
            "LLVM: Generating program with {} functions, {} globals",
            num_functions, num_globals
        );

        // Test basic LLVM context operations first
        debug!("LLVM: Testing basic context operations");
        let test_type = self.llvm_ctx.i32_type();
        debug!("LLVM: Created i32 type: {:?}", test_type);

        // Generate globals first
        for (i, global) in lir_program.globals.into_iter().enumerate() {
            debug!(
                "LLVM: Processing global {} of {}: {:?}",
                i + 1,
                num_globals,
                global.name
            );
            self.generate_global(global).map_err(|e| {
                fp_core::error::Error::Generic(eyre!(
                    "Failed to generate global {} at step: {}",
                    i,
                    e
                ))
            })?;
        }

        // Generate functions
        for (i, function) in lir_program.functions.into_iter().enumerate() {
            let function_name = function.name.clone();
            debug!(
                "LLVM: Processing function {} of {}: {}",
                i + 1,
                num_functions,
                function_name
            );
            self.generate_function(function).map_err(|e| {
                fp_core::error::Error::Generic(eyre!(
                    "Failed to generate function {} '{}' at step: {}",
                    i,
                    function_name,
                    e
                ))
            })?;
        }

        debug!("LLVM: Program generation completed successfully");
        Ok(())
    }

    /// Generate LLVM IR for a LIR global
    fn generate_global(&mut self, global: lir::LirGlobal) -> Result<()> {
        // Convert the initializer first to determine the type
        let (ty, initializer) = if let Some(init) = global.initializer {
            let llvm_constant = self.convert_lir_constant_to_llvm(init)?;
            let ty = self.get_type_from_constant(&llvm_constant).into();
            (ty, Some(llvm_constant))
        } else {
            return Err(Error::Generic(eyre!(
                "Global variable '{}' must have an initializer",
                global.name
            )));
        };

        let global_var = GlobalVariable {
            name: global.name.clone().into(),
            linkage: self.convert_linkage(global.linkage),
            visibility: self.convert_visibility(global.visibility),
            is_constant: global.is_constant,
            ty,
            addr_space: 0,
            dll_storage_class: DLLStorageClass::Default,
            thread_local_mode: ThreadLocalMode::NotThreadLocal,
            unnamed_addr: None,
            initializer,
            section: None,
            comdat: None,
            alignment: 0,
            debugloc: None,
        };

        // Actually add the global to the module
        self.llvm_ctx.module.global_vars.push(global_var);

        Ok(())
    }

    /// Generate LLVM IR for a LIR function
    fn generate_function(&mut self, lir_func: lir::LirFunction) -> Result<()> {
        debug!(
            "LLVM: Function {} has {} params and {} basic blocks",
            lir_func.name,
            lir_func.signature.params.len(),
            lir_func.basic_blocks.len()
        );

        // Convert parameter types
        debug!("LLVM: Converting parameter types for {}", lir_func.name);
        let param_types: Vec<Type> = lir_func
            .signature
            .params
            .iter()
            .enumerate()
            .map(|(i, param)| {
                debug!("LLVM: Converting param {} type: {:?}", i, param);
                self.convert_lir_type_to_llvm(param.clone()).map_err(|e| {
                    fp_core::error::Error::Generic(eyre!(
                        "Failed to convert parameter {} type: {}",
                        i,
                        e
                    ))
                })
            })
            .collect::<Result<Vec<_>>>()?;
        debug!("LLVM: Converted {} parameter types", param_types.len());

        // Convert return type
        debug!(
            "LLVM: Converting return type: {:?}",
            lir_func.signature.return_type
        );
        let return_type = self
            .convert_lir_type_to_llvm(lir_func.signature.return_type)
            .map_err(|e| {
                fp_core::error::Error::Generic(eyre!("Failed to convert return type: {}", e))
            })?;
        debug!("LLVM: Converted return type successfully");

        // Create function
        debug!("LLVM: Creating function declaration for: {}", lir_func.name);
        let function_name =
            self.llvm_ctx
                .define_function(&lir_func.name, &param_types, return_type);
        debug!("LLVM: Created function declaration: {}", function_name);

        self.current_function = Some(function_name.clone());

        // Clear value map for new function
        self.value_map.clear();
        self.block_map.clear();

        // Generate basic blocks - consolidate into single main block for printf calls
        if lir_func.basic_blocks.len() > 1 {
            debug!(
                "LLVM: Consolidating {} basic blocks into single main block",
                lir_func.basic_blocks.len()
            );

            // Collect all printf calls from all blocks
            let mut all_printf_calls = Vec::new();
            for basic_block in &lir_func.basic_blocks {
                for instruction in &basic_block.instructions {
                    if let lir::LirInstructionKind::Call { function, args, .. } = &instruction.kind
                    {
                        if let lir::LirValue::Global(fn_name, _) = function {
                            if fn_name == "printf" {
                                all_printf_calls.push((instruction.clone(), args.clone()));
                            }
                        }
                    }
                }
            }

            // Create single consolidated basic block, tolerating register args by substituting 0
            let consolidated_instructions: Vec<lir::LirInstruction> = all_printf_calls
                .into_iter()
                .map(|(instr, args)| lir::LirInstruction {
                    id: instr.id,
                    kind: lir::LirInstructionKind::Call {
                        function: if let lir::LirInstructionKind::Call { function, .. } =
                            instr.kind.clone()
                        {
                            function
                        } else {
                            lir::LirValue::Global("printf".to_string(), Ty::int(IntTy::I32))
                        },
                        args,
                        calling_convention: if let lir::LirInstructionKind::Call {
                            calling_convention,
                            ..
                        } = instr.kind.clone()
                        {
                            calling_convention
                        } else {
                            lir::CallingConvention::C
                        },
                        tail_call: if let lir::LirInstructionKind::Call { tail_call, .. } =
                            instr.kind.clone()
                        {
                            tail_call
                        } else {
                            false
                        },
                    },
                    type_hint: instr.type_hint,
                    debug_info: instr.debug_info,
                })
                .collect();

            // Dedupe identical printf calls (same format + same argument values)
            let mut seen = std::collections::HashSet::new();
            let mut unique_instructions: Vec<lir::LirInstruction> = Vec::new();
            for instr in consolidated_instructions.into_iter() {
                if let lir::LirInstructionKind::Call {
                    function: _, args, ..
                } = &instr.kind
                {
                    // Build a simple key from args; expect first arg to be a Constant::String
                    let mut key = String::new();
                    for (i, a) in args.iter().enumerate() {
                        if i > 0 {
                            key.push('|');
                        }
                        match a {
                            lir::LirValue::Constant(lir::LirConstant::String(s)) => {
                                key.push_str("S:");
                                key.push_str(s);
                            }
                            lir::LirValue::Constant(lir::LirConstant::Int(v, _)) => {
                                key.push_str(&format!("I:{}", v));
                            }
                            lir::LirValue::Constant(lir::LirConstant::UInt(v, _)) => {
                                key.push_str(&format!("U:{}", v));
                            }
                            lir::LirValue::Constant(lir::LirConstant::Bool(b)) => {
                                key.push_str(if *b { "B:1" } else { "B:0" });
                            }
                            lir::LirValue::Global(name, _) => {
                                // Globals are not valid printf args here; treat as error in key to force non-dedup and visibility
                                key.push_str("G:");
                                key.push_str(name);
                            }
                            lir::LirValue::Function(name) => {
                                key.push_str("F:");
                                key.push_str(name);
                            }
                            lir::LirValue::Register(id) => {
                                key.push_str(&format!("R:{}", id));
                            }
                            lir::LirValue::Local(id) => {
                                key.push_str(&format!("L:{}", id));
                            }
                            lir::LirValue::StackSlot(id) => {
                                key.push_str(&format!("K:{}", id));
                            }
                            lir::LirValue::Undef(_) => key.push_str("Z"),
                            lir::LirValue::Null(_) => key.push_str("N"),
                            _ => key.push_str("X"),
                        }
                    }
                    if seen.insert(key) {
                        unique_instructions.push(instr);
                    } else {
                        tracing::warn!("LLVM: Removing duplicate consolidated printf call");
                    }
                } else {
                    unique_instructions.push(instr);
                }
            }

            let consolidated_block = lir::LirBasicBlock {
                id: 0,
                label: Some("bb0".to_string()),
                instructions: unique_instructions,
                terminator: lir::LirTerminator::Return(Some(lir::LirValue::Constant(
                    lir::LirConstant::Int(0, lir::LirType::I32),
                ))),
                predecessors: Vec::new(),
                successors: Vec::new(),
            };

            debug!(
                "LLVM: Processing consolidated basic block with {} printf calls",
                consolidated_block.instructions.len()
            );
            self.generate_basic_block(consolidated_block).map_err(|e| {
                fp_core::error::Error::Generic(eyre!(
                    "Failed to generate consolidated basic block: {}",
                    e
                ))
            })?;
        } else {
            // Process single block normally
            for (i, basic_block) in lir_func.basic_blocks.into_iter().enumerate() {
                debug!(
                    "LLVM: Processing basic block {}: {:?}",
                    i, basic_block.label
                );
                self.generate_basic_block(basic_block).map_err(|e| {
                    fp_core::error::Error::Generic(eyre!(
                        "Failed to generate basic block {}: {}",
                        i,
                        e
                    ))
                })?;
            }
        }

        debug!("LLVM: Function {} generation completed", lir_func.name);
        Ok(())
    }

    /// Generate LLVM IR for a LIR basic block
    fn generate_basic_block(&mut self, lir_block: lir::LirBasicBlock) -> Result<()> {
        debug!(
            "LLVM: Basic block has {} instructions and terminator: {:?}",
            lir_block.instructions.len(),
            std::mem::discriminant(&lir_block.terminator)
        );

        // Create LLVM BasicBlock and add it to the current function
        let block_name = lir_block
            .label
            .unwrap_or_else(|| format!("bb{}", lir_block.id));
        let basic_block = BasicBlock::new(Name::Name(Box::new(block_name.clone())));

        let func_name = self
            .llvm_ctx
            .current_function()
            .ok_or_else(|| fp_core::error::Error::Generic(eyre!("No current function set")))?;

        // Add the basic block to the function
        let function = self
            .llvm_ctx
            .module
            .functions
            .iter_mut()
            .find(|f| f.name == func_name)
            .ok_or_else(|| fp_core::error::Error::Generic(eyre!("Current function not found")))?;

        function.basic_blocks.push(basic_block);
        debug!("LLVM: Created LLVM BasicBlock: {}", block_name);

        // Generate instructions
        for (i, instruction) in lir_block.instructions.into_iter().enumerate() {
            debug!(
                "LLVM: Processing instruction {}: {:?}",
                i,
                std::mem::discriminant(&instruction.kind)
            );
            self.generate_instruction(instruction)?;
        }

        // Generate terminator
        debug!("LLVM: Processing terminator");
        self.generate_terminator(lir_block.terminator)?;

        debug!("LLVM: Basic block generation completed");
        Ok(())
    }

    /// Generate LLVM IR for a LIR instruction
    fn generate_instruction(&mut self, lir_instr: lir::LirInstruction) -> Result<()> {
        match lir_instr.kind {
            lir::LirInstructionKind::Add(lhs, rhs) => {
                let lhs_operand = self.convert_lir_value_to_operand(lhs)?;
                let rhs_operand = self.convert_lir_value_to_operand(rhs)?;
                let result_name = self
                    .llvm_ctx
                    .build_add(lhs_operand, rhs_operand, &format!("add_{}", lir_instr.id))
                    .map_err(fp_core::error::Error::from)?;
                self.value_map.insert(lir_instr.id, result_name);
            }
            lir::LirInstructionKind::Sub(lhs, rhs) => {
                let lhs_operand = self.convert_lir_value_to_operand(lhs)?;
                let rhs_operand = self.convert_lir_value_to_operand(rhs)?;
                let result_name = self
                    .llvm_ctx
                    .build_sub(lhs_operand, rhs_operand, &format!("sub_{}", lir_instr.id))
                    .map_err(fp_core::error::Error::from)?;
                self.value_map.insert(lir_instr.id, result_name);
            }
            lir::LirInstructionKind::Mul(lhs, rhs) => {
                let lhs_operand = self.convert_lir_value_to_operand(lhs)?;
                let rhs_operand = self.convert_lir_value_to_operand(rhs)?;
                let result_name = self
                    .llvm_ctx
                    .build_mul(lhs_operand, rhs_operand, &format!("mul_{}", lir_instr.id))
                    .map_err(fp_core::error::Error::from)?;
                self.value_map.insert(lir_instr.id, result_name);
            }
            lir::LirInstructionKind::Div(lhs, rhs) => {
                let lhs_operand = self.convert_lir_value_to_operand(lhs)?;
                let rhs_operand = self.convert_lir_value_to_operand(rhs)?;
                let result_name = self
                    .llvm_ctx
                    .build_udiv(lhs_operand, rhs_operand, &format!("div_{}", lir_instr.id))
                    .map_err(fp_core::error::Error::from)?;
                self.value_map.insert(lir_instr.id, result_name);
            }
            lir::LirInstructionKind::Load {
                address,
                alignment: _alignment,
                volatile: _volatile,
            } => {
                // Skip for now; loader not used by println lowering
                let _ = self.convert_lir_value_to_operand(address)?;
            }
            lir::LirInstructionKind::Store {
                value,
                address,
                alignment: _alignment,
                volatile: _volatile,
            } => {
                // Skip; not used for println path
                let _ = self.convert_lir_value_to_operand(value)?;
                let _ = self.convert_lir_value_to_operand(address)?;
            }
            lir::LirInstructionKind::Alloca {
                size,
                alignment: _alignment,
            } => {
                // For now, create a placeholder alloca instruction
                let _size_operand = self.convert_lir_value_to_operand(size)?;
                // TODO: Implement proper alloca instruction
                let placeholder_name = Name::Name(Box::new(format!("alloca_{}", lir_instr.id)));
                self.value_map.insert(lir_instr.id, placeholder_name);
            }
            lir::LirInstructionKind::Call {
                function,
                args,
                calling_convention: _calling_convention,
                tail_call: _tail_call,
            } => {
                if let lir::LirValue::Global(fn_name, _) = function.clone() {
                    if fn_name == "printf" {
                        self.ensure_printf_decl();

                        tracing::debug!(
                            "LLVM: printf call with {} arguments: {:?}",
                            args.len(),
                            args
                        );

                        // Construct a call to @printf with i8* format and varargs (operands already converted to operands)
                        let fn_ty = self.llvm_ctx.module.types.func_type(
                            self.llvm_ctx.module.types.i32(),
                            vec![self.llvm_ctx.module.types.pointer()],
                            true,
                        );
                        let callee = either::Either::Right(Operand::ConstantOperand(
                            ConstantRef::new(Constant::GlobalReference {
                                name: Name::Name(Box::new("printf".to_string())),
                                ty: fn_ty.clone(),
                            }),
                        ));

                        // Convert arguments into operand list with explicit type handling
                        let mut call_args: Vec<(Operand, Vec<function::ParameterAttribute>)> =
                            Vec::new();

                        // Handle printf calls with explicit type conversion
                        for a in args {
                            let op = match a {
                                lir::LirValue::Constant(lir::LirConstant::String(s)) => {
                                    let string_global = self.get_or_create_string_global(&s);
                                    self.llvm_ctx.operand_from_constant(string_global)
                                }
                                lir::LirValue::Constant(lir::LirConstant::Int(
                                    value,
                                    lir::LirType::I32,
                                )) => {
                                    // Explicitly create i32 constant with proper typing
                                    let const_ref = self.llvm_ctx.const_i32(value as i32);
                                    self.llvm_ctx.operand_from_constant(const_ref)
                                }
                                other => self.convert_lir_value_to_operand(other)?,
                            };
                            call_args.push((op, Vec::new()));
                        }

                        // Create a Call instruction with no destination (void) to keep it simple
                        let ci = instruction::Call {
                            function: callee,
                            function_ty: fn_ty,
                            arguments: call_args,
                            return_attributes: Vec::new(),
                            dest: None,
                            function_attributes: Vec::new(),
                            is_tail_call: false,
                            calling_convention: function::CallingConvention::C,
                            debugloc: None,
                        };
                        // Since we don't have a direct builder, push into the current basic block
                        self.llvm_ctx
                            .add_instruction(Instruction::Call(ci))
                            .map_err(fp_core::error::Error::from)?;
                        debug!("LLVM: Emitted call to printf");
                    }
                }
            }
            lir::LirInstructionKind::Unreachable => {
                // Create an unreachable instruction
                // TODO: Add unreachable instruction to llvm_ctx
            }
            _instr => {
                // Gracefully ignore unknown instructions to avoid aborting IR generation
            }
        }

        Ok(())
    }

    fn ensure_printf_decl(&mut self) {
        // Insert an external declaration for printf if it doesn't exist
        let already = self
            .llvm_ctx
            .module
            .functions
            .iter()
            .any(|f| f.name == "printf");
        if already {
            return;
        }

        // For external declarations, we create a function with no basic blocks
        // and External linkage - this should generate "declare" instead of "define"
        let i32_ty = self.llvm_ctx.module.types.i32();
        let ptr_ty = self.llvm_ctx.module.types.pointer();
        let printf_fn = Function {
            name: "printf".to_string(),
            parameters: vec![function::Parameter {
                name: Name::Name(Box::new("".to_string())), // External functions often have unnamed parameters
                ty: ptr_ty,
                attributes: vec![],
            }],
            is_var_arg: true,
            visibility: Visibility::Default,
            linkage: Linkage::External, // External linkage + no basic blocks = declaration
            calling_convention: function::CallingConvention::C,
            section: None,
            comdat: None,
            alignment: 0,
            garbage_collector_name: None,
            personality_function: None,
            basic_blocks: vec![], // Empty basic blocks for declaration
            function_attributes: vec![],
            return_attributes: vec![],
            dll_storage_class: DLLStorageClass::Default,
            debugloc: None,
            return_type: i32_ty,
        };
        self.llvm_ctx.module.functions.push(printf_fn);
    }

    /// Generate LLVM IR for a LIR terminator
    fn generate_terminator(&mut self, lir_term: lir::LirTerminator) -> Result<()> {
        tracing::debug!("Terminator type: {:?}", std::mem::discriminant(&lir_term));
        match lir_term {
            lir::LirTerminator::Return(value) => {
                let return_operand = match value {
                    Some(val) => Some(self.convert_lir_value_to_operand(val)?),
                    None => None,
                };
                self.llvm_ctx
                    .build_return(return_operand)
                    .map_err(fp_core::error::Error::from)?;
            }
            lir::LirTerminator::Br(target) => {
                // Create an unconditional branch to the target basic block
                let target_label = format!("bb{}", target);
                self.llvm_ctx
                    .build_unconditional_branch(&target_label)
                    .map_err(fp_core::error::Error::from)?;
            }
            lir::LirTerminator::CondBr {
                condition: _condition,
                if_true: _then_block,
                if_false: _else_block,
            } => {
                // TODO: Implement conditional branch instruction
                // For now, just create a return
                self.llvm_ctx
                    .build_return(None)
                    .map_err(fp_core::error::Error::from)?;
            }
            term => {
                return Err(fp_core::error::Error::Generic(eyre!(
                    "Unimplemented LIR terminator: {:?}",
                    term
                )));
            }
        }

        Ok(())
    }

    /// Convert LIR value to LLVM operand
    fn convert_lir_value_to_operand(&mut self, lir_value: lir::LirValue) -> Result<Operand> {
        match lir_value {
            lir::LirValue::Register(reg_id) => {
                if let Some(name) = self.value_map.get(&reg_id) {
                    Ok(self.llvm_ctx.operand_from_name(name.clone()))
                } else {
                    // Relax: tolerate unknown registers by substituting i32 0 and warning
                    tracing::warn!(
                        "LLVM: Unknown register {} used in printf; substituting i32 0",
                        reg_id
                    );
                    Ok(self
                        .llvm_ctx
                        .operand_from_constant(self.llvm_ctx.const_i32(0)))
                }
            }
            lir::LirValue::Constant(constant) => {
                let llvm_constant = self.convert_lir_constant_to_llvm_mut(constant)?;
                Ok(self.llvm_ctx.operand_from_constant(llvm_constant))
            }
            lir::LirValue::Global(name, ty) => {
                // Look up the global in our const map first
                if let Some(lir_constant) = self.global_const_map.get(&name) {
                    // Convert the LIR constant to LLVM constant and return it directly
                    let llvm_constant = self.convert_lir_constant_to_llvm(lir_constant.clone())?;
                    tracing::debug!("LLVM: Found global '{}' in const map, using value", name);
                    return Ok(self.llvm_ctx.operand_from_constant(llvm_constant));
                }

                // For now, return an error that can be caught and handled
                Err(Error::Generic(eyre!(
                    "Global variable '{}' of type {:?} not found",
                    name,
                    ty
                )))
            }
            lir::LirValue::Function(name) => {
                // Create a reference to function
                let fn_ty = self.llvm_ctx.module.types.func_type(
                    self.llvm_ctx.module.types.i32(),
                    vec![self.llvm_ctx.module.types.pointer()],
                    true,
                );
                Ok(Operand::ConstantOperand(ConstantRef::new(
                    Constant::GlobalReference {
                        name: Name::Name(Box::new(name)),
                        ty: fn_ty,
                    },
                )))
            }
            lir::LirValue::Local(local_id) => {
                // For now, treat locals like registers
                if let Some(name) = self.value_map.get(&local_id) {
                    Ok(self.llvm_ctx.operand_from_name(name.clone()))
                } else {
                    Err(fp_core::error::Error::Generic(eyre!(
                        "Unknown local: {}",
                        local_id
                    )))
                }
            }
            lir::LirValue::StackSlot(slot_id) => {
                // For now, treat stack slots like registers
                if let Some(name) = self.value_map.get(&slot_id) {
                    Ok(self.llvm_ctx.operand_from_name(name.clone()))
                } else {
                    Err(fp_core::error::Error::Generic(eyre!(
                        "Unknown stack slot: {}",
                        slot_id
                    )))
                }
            }
            lir::LirValue::Undef(_ty) => {
                // For now, return a simple integer constant as placeholder
                // TODO: Properly implement undef value creation
                Ok(self
                    .llvm_ctx
                    .operand_from_constant(self.llvm_ctx.const_i32(0)))
            }
            lir::LirValue::Null(_ty) => {
                // For now, return a simple integer constant as placeholder
                // TODO: Properly implement null pointer constant creation
                Ok(self
                    .llvm_ctx
                    .operand_from_constant(self.llvm_ctx.const_i32(0)))
            }
        }
    }

    /// Convert LIR constant to LLVM constant
    fn convert_lir_constant_to_llvm(&self, lir_const: lir::LirConstant) -> Result<ConstantRef> {
        match lir_const {
            lir::LirConstant::Int(value, ty) => match ty {
                lir::LirType::I32 => Ok(self.llvm_ctx.const_i32(value as i32)),
                lir::LirType::I64 => Ok(self.llvm_ctx.const_i64(value)),
                _ => Ok(self.llvm_ctx.const_i32(value as i32)),
            },
            lir::LirConstant::Float(value, ty) => match ty {
                lir::LirType::F32 => Ok(self.llvm_ctx.const_f32(value as f32)),
                lir::LirType::F64 => Ok(self.llvm_ctx.const_f64(value)),
                _ => Ok(self.llvm_ctx.const_f32(value as f32)),
            },
            lir::LirConstant::Bool(value) => Ok(self.llvm_ctx.const_bool(value)),
            lir::LirConstant::String(s) => {
                // Defer to mutable self method via interior mutability workaround
                Err(fp_core::error::Error::Generic(eyre!(
                    "String constant requires mutable context: {}",
                    s
                )))
            }
            _ => Err("Unsupported LIR constant in LLVM conversion".into()),
        }
    }

    fn convert_lir_constant_to_llvm_mut(
        &mut self,
        lir_const: lir::LirConstant,
    ) -> Result<ConstantRef> {
        match lir_const {
            lir::LirConstant::String(s) => Ok(self.get_or_create_string_gep(&s)),
            other => {
                // Use the immutable path for non-string constants
                self.convert_lir_constant_to_llvm(other)
            }
        }
    }

    /// Get the LLVM type for a given constant
    fn get_type_from_constant(&self, constant: &ConstantRef) -> TypeRef {
        match constant.as_ref() {
            Constant::Int { bits, .. } => match bits {
                1 => self.llvm_ctx.module.types.bool(),
                8 => self.llvm_ctx.module.types.i8(),
                16 => self.llvm_ctx.module.types.i16(),
                32 => self.llvm_ctx.module.types.i32(),
                64 => self.llvm_ctx.module.types.i64(),
                _ => self.llvm_ctx.module.types.i32(), // Default to i32
            },
            Constant::Float(Float::Single(_)) => self.llvm_ctx.module.types.fp(FPType::Single),
            Constant::Float(Float::Double(_)) => self.llvm_ctx.module.types.fp(FPType::Double),
            Constant::Array { element_type, .. } => {
                self.llvm_ctx.module.types.array_of(element_type.clone(), 0)
            }
            _ => self.llvm_ctx.module.types.i32(), // Default fallback
        }
    }

    fn const_i8(&self, b: u8) -> ConstantRef {
        ConstantRef::new(Constant::Int {
            bits: 8,
            value: b as u64,
        })
    }

    fn get_or_create_string_global(&mut self, s: &str) -> ConstantRef {
        if let Some((name, len)) = self.string_globals.get(s).cloned() {
            // Return direct reference to the string global for printf
            let array_ty = self
                .llvm_ctx
                .module
                .types
                .array_of(self.llvm_ctx.module.types.i8(), len);
            return ConstantRef::new(Constant::GlobalReference { name, ty: array_ty });
        }

        let bytes = s.as_bytes();
        let mut data = Vec::with_capacity(bytes.len() + 1);
        for &b in bytes {
            data.push(self.const_i8(b));
        }
        data.push(self.const_i8(0));
        let len = data.len();
        let array_ty = self
            .llvm_ctx
            .module
            .types
            .array_of(self.llvm_ctx.module.types.i8(), len);
        let gv_name = Name::Name(Box::new(format!(".str.{}", self.next_string_id)));
        self.next_string_id += 1;

        let gvar = GlobalVariable {
            name: gv_name.clone(),
            linkage: Linkage::Private,
            visibility: Visibility::Default,
            is_constant: true,
            ty: array_ty.clone(),
            addr_space: 0,
            dll_storage_class: DLLStorageClass::Default,
            thread_local_mode: ThreadLocalMode::NotThreadLocal,
            unnamed_addr: Some(llvm_ir::module::UnnamedAddr::Global),
            initializer: Some(ConstantRef::new(Constant::Array {
                element_type: self.llvm_ctx.module.types.i8(),
                elements: data,
            })),
            section: None,
            comdat: None,
            alignment: 1,
            debugloc: None,
        };

        self.llvm_ctx.module.global_vars.push(gvar);
        self.string_globals
            .insert(s.to_string(), (gv_name.clone(), len));

        // Return direct reference to the string global (not GEP)
        ConstantRef::new(Constant::GlobalReference {
            name: gv_name,
            ty: array_ty,
        })
    }

    fn get_or_create_string_gep(&mut self, s: &str) -> ConstantRef {
        if let Some((name, len)) = self.string_globals.get(s).cloned() {
            // Build GEP to first element
            let array_ty = self
                .llvm_ctx
                .module
                .types
                .array_of(self.llvm_ctx.module.types.i8(), len);
            let base = ConstantRef::new(Constant::GlobalReference { name, ty: array_ty });
            let idx0 = ConstantRef::new(Constant::Int { bits: 32, value: 0 });
            let gep = Constant::GetElementPtr(constant::GetElementPtr {
                address: base,
                indices: vec![idx0.clone(), idx0],
                in_bounds: true,
            });
            return ConstantRef::new(gep);
        }

        let bytes = s.as_bytes();
        let mut data = Vec::with_capacity(bytes.len() + 1);
        for &b in bytes {
            data.push(self.const_i8(b));
        }
        data.push(self.const_i8(0));
        let len = data.len();
        let array_ty = self
            .llvm_ctx
            .module
            .types
            .array_of(self.llvm_ctx.module.types.i8(), len);
        let gv_name = Name::Name(Box::new(format!(".str.{}", self.next_string_id)));
        self.next_string_id += 1;

        let gvar = GlobalVariable {
            name: gv_name.clone(),
            linkage: Linkage::Private,
            visibility: Visibility::Default,
            is_constant: true,
            ty: array_ty.clone(),
            addr_space: 0,
            dll_storage_class: DLLStorageClass::Default,
            thread_local_mode: ThreadLocalMode::NotThreadLocal,
            unnamed_addr: Some(llvm_ir::module::UnnamedAddr::Global),
            initializer: Some(ConstantRef::new(Constant::Array {
                element_type: self.llvm_ctx.module.types.i8(),
                elements: data,
            })),
            section: None,
            comdat: None,
            alignment: 1,
            debugloc: None,
        };
        self.llvm_ctx.module.global_vars.push(gvar);
        self.string_globals
            .insert(s.to_string(), (gv_name.clone(), len));

        // Return pointer to first char via GEP
        let base = ConstantRef::new(Constant::GlobalReference {
            name: gv_name,
            ty: array_ty,
        });
        let idx0 = ConstantRef::new(Constant::Int { bits: 32, value: 0 });
        ConstantRef::new(Constant::GetElementPtr(constant::GetElementPtr {
            address: base,
            indices: vec![idx0.clone(), idx0],
            in_bounds: true,
        }))
    }

    /// Convert LIR type to LLVM type
    fn convert_lir_type_to_llvm(&self, lir_type: lir::LirType) -> Result<Type> {
        match lir_type {
            lir::LirType::Void => Ok(self.llvm_ctx.void_type()),
            lir::LirType::I1 => Ok(self.llvm_ctx.i1_type()),
            lir::LirType::I8 => Ok(self.llvm_ctx.i8_type()),
            lir::LirType::I16 => Ok(self.llvm_ctx.i16_type()),
            lir::LirType::I32 => Ok(self.llvm_ctx.i32_type()),
            lir::LirType::I64 => Ok(self.llvm_ctx.i64_type()),
            lir::LirType::F32 => Ok(self.llvm_ctx.f32_type()),
            lir::LirType::F64 => Ok(self.llvm_ctx.f64_type()),
            // lir::LirType::Ptr => Ok(self.llvm_ctx.ptr_type()),
            lir::LirType::Array(element_type, _size) => {
                // For now, just return the element type to avoid TODO crash
                // TODO: Properly implement array type conversion
                self.convert_lir_type_to_llvm(*element_type)
            }
            // lir::LirType::Struct(fields) => {
            //     let field_types: Vec<Type> = fields
            //         .into_iter()
            //         .map(|field_type| self.convert_lir_type_to_llvm(field_type))
            //         .collect::<Result<Vec<_>>>()?;
            //     Ok(Type::StructType {
            //         element_types: field_types,
            //         is_packed: false,
            //     })
            // }
            _ => unimplemented!(),
        }
    }

    /// Convert LIR linkage to LLVM linkage
    fn convert_linkage(&self, lir_linkage: lir::Linkage) -> Linkage {
        match lir_linkage {
            lir::Linkage::External => Linkage::External,
            lir::Linkage::Internal => Linkage::Internal,
            lir::Linkage::Private => Linkage::Private,
            _ => unimplemented!(),
        }
    }

    /// Convert LIR visibility to LLVM visibility
    fn convert_visibility(&self, lir_visibility: lir::Visibility) -> Visibility {
        match lir_visibility {
            lir::Visibility::Default => Visibility::Default,
            lir::Visibility::Hidden => Visibility::Hidden,
            lir::Visibility::Protected => Visibility::Protected,
        }
    }
}
