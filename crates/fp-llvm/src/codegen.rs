use crate::context::LlvmContext;
use crate::stdlib::CStdLib;
use fp_core::diagnostics::report_error;
use fp_core::intrinsics::{
    self, BackendFlavor, CallAbi, CallArgStrategy, ResolvedCall, ResolvedIntrinsic,
};
use fp_core::{error::Result, lir};
use llvm_ir::constant::Float;
use llvm_ir::module::{DLLStorageClass, GlobalVariable, Linkage, ThreadLocalMode, Visibility};
use llvm_ir::predicates::IntPredicate;
use llvm_ir::types::FPType;
use llvm_ir::*;
// use llvm_ir::instruction::Call; // Not needed currently
use fp_core::tracing::debug;
use std::collections::HashMap;

const LOG_AREA: &str = "[lir→llvm]";

/// LLVM code generator that transforms LIR to LLVM IR
pub struct LirCodegen<'ctx> {
    llvm_ctx: &'ctx mut LlvmContext,
    value_map: HashMap<u32, (Name, lir::LirType)>, // Maps LIR register IDs to LLVM names and types
    block_map: HashMap<u32, Name>,                 // Maps LIR block IDs to LLVM basic block names
    current_function: Option<String>,
    string_globals: HashMap<String, (Name, usize)>,
    next_string_id: u32,
    // Track referenced globals that need const evaluation
    referenced_globals: std::collections::HashSet<String>,
    // Global constants map from MIR->LIR transformation
    global_const_map: HashMap<String, lir::LirConstant>,
    constant_results: HashMap<u32, lir::LirConstant>,
    current_return_type: Option<lir::LirType>,
    function_signatures: HashMap<String, lir::LirFunctionSignature>,
    symbol_names: HashMap<String, String>,
    defined_functions: std::collections::HashSet<String>,
    backend_flavor: BackendFlavor,
    argument_operands: HashMap<u32, Operand>,
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
            constant_results: HashMap::new(),
            current_return_type: None,
            function_signatures: HashMap::new(),
            symbol_names: HashMap::new(),
            defined_functions: std::collections::HashSet::new(),
            backend_flavor: BackendFlavor::Llvm,
            argument_operands: HashMap::new(),
        }
    }

    fn record_result(&mut self, instr_id: u32, ty_hint: Option<lir::LirType>, name: Name) {
        let ty = ty_hint.unwrap_or(lir::LirType::I32);
        self.value_map.insert(instr_id, (name, ty));
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

        let lir::LirProgram {
            functions,
            globals,
            type_definitions: _type_definitions,
        } = lir_program;

        self.function_signatures.clear();
        self.defined_functions.clear();
        for function in &functions {
            self.function_signatures
                .insert(function.name.clone(), function.signature.clone());
        }
        tracing::debug!(
            "Collected function signatures: {:?}",
            self.function_signatures.keys().cloned().collect::<Vec<_>>()
        );

        // Generate globals first
        for (i, global) in globals.into_iter().enumerate() {
            debug!(
                "LLVM: Processing global {} of {}: {:?}",
                i + 1,
                num_globals,
                global.name
            );
            self.generate_global(global).map_err(|e| {
                report_error(format!(
                    "{} Failed to generate global {} at step: {}",
                    LOG_AREA, i, e
                ))
            })?;
        }

        // Generate functions
        for (i, function) in functions.into_iter().enumerate() {
            let function_name = function.name.clone();
            debug!(
                "LLVM: Processing function {} of {}: {}",
                i + 1,
                num_functions,
                function_name
            );
            self.generate_function(function).map_err(|e| {
                report_error(format!(
                    "{} Failed to generate function {} '{}' at step: {}",
                    LOG_AREA, i, function_name, e
                ))
            })?;
        }

        debug!("LLVM: Program generation completed successfully");

        // Deduplicate function declarations, keeping the last occurrence (the full definition).
        if !self.llvm_ctx.module.functions.is_empty() {
            let mut seen = std::collections::HashSet::new();
            let mut deduped = Vec::with_capacity(self.llvm_ctx.module.functions.len());
            for function in self.llvm_ctx.module.functions.drain(..).rev() {
                if seen.insert(function.name.clone()) {
                    deduped.push(function);
                }
            }
            deduped.reverse();
            self.llvm_ctx.module.functions = deduped;
        }

        if !self.llvm_ctx.module.func_declarations.is_empty() {
            let defined: std::collections::HashSet<String> = self
                .llvm_ctx
                .module
                .functions
                .iter()
                .map(|f| f.name.clone())
                .collect();

            if !defined.is_empty() {
                self.llvm_ctx
                    .module
                    .func_declarations
                    .retain(|decl| !defined.contains(&decl.name));
            }
        }

        Ok(())
    }

    /// Generate LLVM IR for a LIR global
    fn generate_global(&mut self, global: lir::LirGlobal) -> Result<()> {
        // Convert the initializer first to determine the type
        let (ty, initializer) = if let Some(init) = global.initializer {
            let llvm_constant = self.convert_lir_constant_to_llvm(init)?;
            let ty = self.get_type_from_constant(&llvm_constant)?.into();
            (ty, Some(llvm_constant))
        } else {
            return Err(report_error(format!(
                "{} Global variable '{}' must have an initializer",
                LOG_AREA, global.name
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
                    report_error(format!(
                        "{} Failed to convert parameter {} type: {}",
                        LOG_AREA, i, e
                    ))
                })
            })
            .collect::<Result<Vec<_>>>()?;
        debug!(
            "LLVM: Converted parameter types for {}: {:?}",
            lir_func.name, param_types
        );

        // Convert return type
        debug!(
            "LLVM: Converting return type: {:?}",
            lir_func.signature.return_type
        );
        let mut return_lir_type = lir_func.signature.return_type.clone();
        if lir_func.name == "main" && matches!(return_lir_type, lir::LirType::Void) {
            return_lir_type = lir::LirType::I32;
        }
        let return_type = self
            .convert_lir_type_to_llvm(return_lir_type.clone())
            .map_err(|e| {
                report_error(format!("{} Failed to convert return type: {}", LOG_AREA, e))
            })?;
        debug!("LLVM: Converted return type successfully");

        // Create function
        debug!("LLVM: Creating function declaration for: {}", lir_func.name);
        let llvm_name = self.llvm_symbol_for(&lir_func.name);
        self.defined_functions.insert(llvm_name.clone());
        let function_name = self
            .llvm_ctx
            .define_function(&llvm_name, &param_types, return_type);
        debug!("LLVM: Created function declaration: {}", function_name);

        self.current_function = Some(function_name.clone());
        self.current_return_type = Some(return_lir_type);
        self.argument_operands.clear();
        self.populate_argument_operands(&function_name, &lir_func.locals)?;

        let function_linkage = self.convert_linkage(lir_func.linkage);
        if let Some(function_record) = self
            .llvm_ctx
            .module
            .functions
            .iter_mut()
            .find(|f| f.name == function_name)
        {
            function_record.linkage = function_linkage;
        }

        // Clear value map for new function
        self.value_map.clear();
        self.block_map.clear();
        self.constant_results.clear();

        for (i, basic_block) in lir_func.basic_blocks.into_iter().enumerate() {
            debug!(
                "LLVM: Processing basic block {}: {:?}",
                i, basic_block.label
            );
            self.generate_basic_block(basic_block).map_err(|e| {
                report_error(format!(
                    "{} Failed to generate basic block {}: {}",
                    LOG_AREA, i, e
                ))
            })?;
        }

        debug!("LLVM: Function {} generation completed", lir_func.name);
        self.current_return_type = None;
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
        self.block_map
            .insert(lir_block.id, Name::Name(Box::new(block_name.clone())));

        let func_name = self
            .llvm_ctx
            .current_function()
            .ok_or_else(|| report_error(format!("{} No current function set", LOG_AREA)))?;

        // Add the basic block to the function
        let function = self
            .llvm_ctx
            .module
            .functions
            .iter_mut()
            .find(|f| f.name == func_name)
            .ok_or_else(|| report_error(format!("{} Current function not found", LOG_AREA)))?;

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
        let instr_id = lir_instr.id;
        let ty_hint = lir_instr.type_hint.clone();
        match lir_instr.kind {
            lir::LirInstructionKind::Add(lhs, rhs) => {
                let lhs_operand = self.convert_lir_value_to_operand(lhs)?;
                let rhs_operand = self.convert_lir_value_to_operand(rhs)?;
                let result_name = self
                    .llvm_ctx
                    .build_add(lhs_operand, rhs_operand, &format!("add_{}", lir_instr.id))
                    .map_err(fp_core::error::Error::from)?;
                self.record_result(instr_id, ty_hint.clone(), result_name);
            }
            lir::LirInstructionKind::Eq(lhs, rhs) => {
                self.lower_int_cmp(IntPredicate::EQ, lhs, rhs, instr_id, ty_hint.clone())?;
            }
            lir::LirInstructionKind::Ne(lhs, rhs) => {
                self.lower_int_cmp(IntPredicate::NE, lhs, rhs, instr_id, ty_hint.clone())?;
            }
            lir::LirInstructionKind::Lt(lhs, rhs) => {
                self.lower_int_cmp(IntPredicate::SLT, lhs, rhs, instr_id, ty_hint.clone())?;
            }
            lir::LirInstructionKind::Le(lhs, rhs) => {
                self.lower_int_cmp(IntPredicate::SLE, lhs, rhs, instr_id, ty_hint.clone())?;
            }
            lir::LirInstructionKind::Gt(lhs, rhs) => {
                self.lower_int_cmp(IntPredicate::SGT, lhs, rhs, instr_id, ty_hint.clone())?;
            }
            lir::LirInstructionKind::Ge(lhs, rhs) => {
                self.lower_int_cmp(IntPredicate::SGE, lhs, rhs, instr_id, ty_hint.clone())?;
            }
            lir::LirInstructionKind::Sub(lhs, rhs) => {
                let lhs_operand = self.convert_lir_value_to_operand(lhs)?;
                let rhs_operand = self.convert_lir_value_to_operand(rhs)?;
                let result_name = self
                    .llvm_ctx
                    .build_sub(lhs_operand, rhs_operand, &format!("sub_{}", lir_instr.id))
                    .map_err(fp_core::error::Error::from)?;
                self.record_result(instr_id, ty_hint.clone(), result_name);
            }
            lir::LirInstructionKind::Mul(lhs, rhs) => {
                let lhs_operand = self.convert_lir_value_to_operand(lhs)?;
                let rhs_operand = self.convert_lir_value_to_operand(rhs)?;
                let result_name = self
                    .llvm_ctx
                    .build_mul(lhs_operand, rhs_operand, &format!("mul_{}", lir_instr.id))
                    .map_err(fp_core::error::Error::from)?;
                self.record_result(instr_id, ty_hint.clone(), result_name);
            }
            lir::LirInstructionKind::Div(lhs, rhs) => {
                let lhs_operand = self.convert_lir_value_to_operand(lhs)?;
                let rhs_operand = self.convert_lir_value_to_operand(rhs)?;
                let result_name = self
                    .llvm_ctx
                    .build_udiv(lhs_operand, rhs_operand, &format!("div_{}", lir_instr.id))
                    .map_err(fp_core::error::Error::from)?;
                self.record_result(instr_id, ty_hint.clone(), result_name);
            }
            lir::LirInstructionKind::Load {
                address,
                alignment,
                volatile,
            } => {
                let address_operand = self.convert_lir_value_to_operand(address)?;
                let loaded_lir_type = ty_hint.clone().unwrap_or(lir::LirType::I32);
                let loaded_llvm_type = self.convert_lir_type_to_llvm(loaded_lir_type.clone())?;
                let result_name = self
                    .llvm_ctx
                    .build_load(
                        address_operand,
                        loaded_llvm_type,
                        &format!("load_{}", instr_id),
                        alignment.unwrap_or(0),
                        volatile,
                    )
                    .map_err(fp_core::error::Error::from)?;
                self.record_result(instr_id, Some(loaded_lir_type), result_name);
            }
            lir::LirInstructionKind::Store {
                value,
                address,
                alignment,
                volatile,
            } => {
                let value_operand = self.convert_lir_value_to_operand(value)?;
                let address_operand = self.convert_lir_value_to_operand(address)?;
                self.llvm_ctx
                    .build_store(
                        value_operand,
                        address_operand,
                        alignment.unwrap_or(0),
                        volatile,
                    )
                    .map_err(fp_core::error::Error::from)?;
            }
            lir::LirInstructionKind::Alloca { size, alignment } => {
                let size_operand = self.convert_lir_value_to_operand(size)?;
                let element_lir_type = match ty_hint.clone() {
                    Some(lir::LirType::Ptr(inner)) => *inner,
                    _ => lir::LirType::I8,
                };
                let llvm_element_type = self.convert_lir_type_to_llvm(element_lir_type.clone())?;
                let allocated_type = self.llvm_ctx.module.types.get_for_type(&llvm_element_type);
                let result_name = self
                    .llvm_ctx
                    .build_alloca(
                        allocated_type,
                        size_operand,
                        &format!("alloca_{}", instr_id),
                        alignment,
                    )
                    .map_err(fp_core::error::Error::from)?;
                self.record_result(instr_id, ty_hint.clone(), result_name);
            }
            lir::LirInstructionKind::Bitcast(value, target_ty) => {
                let operand = self.convert_lir_value_to_operand(value)?;
                let llvm_target_ty = self.convert_lir_type_to_llvm(target_ty.clone())?;
                let result_name = self
                    .llvm_ctx
                    .build_bitcast(operand, llvm_target_ty, &format!("bitcast_{}", instr_id))
                    .map_err(fp_core::error::Error::from)?;
                self.record_result(instr_id, Some(target_ty), result_name);
            }
            lir::LirInstructionKind::ZExt(value, target_ty) => {
                let operand = self.convert_lir_value_to_operand(value)?;
                let llvm_target_ty = self.convert_lir_type_to_llvm(target_ty.clone())?;
                let result_name = self
                    .llvm_ctx
                    .build_zext(operand, llvm_target_ty, &format!("zext_{}", instr_id))
                    .map_err(fp_core::error::Error::from)?;
                self.record_result(instr_id, Some(target_ty), result_name);
            }
            lir::LirInstructionKind::GetElementPtr {
                ptr,
                indices,
                inbounds,
            } => {
                let base_operand = self.convert_lir_value_to_operand(ptr)?;
                let mut llvm_indices = Vec::with_capacity(indices.len());
                for index in indices {
                    llvm_indices.push(self.convert_lir_value_to_operand(index)?);
                }
                let element_lir_type = ty_hint
                    .clone()
                    .and_then(|ty| match ty {
                        lir::LirType::Ptr(inner) => Some(*inner),
                        _ => None,
                    })
                    .unwrap_or(lir::LirType::I8);
                let element_type = self.convert_lir_type_to_llvm(element_lir_type.clone())?;
                let result_name = self
                    .llvm_ctx
                    .build_gep(
                        base_operand,
                        element_type,
                        llvm_indices,
                        inbounds,
                        &format!("gep_{}", instr_id),
                    )
                    .map_err(fp_core::error::Error::from)?;
                let result_ty = ty_hint
                    .clone()
                    .or_else(|| Some(lir::LirType::Ptr(Box::new(lir::LirType::I8))));
                self.record_result(instr_id, result_ty, result_name);
            }
            lir::LirInstructionKind::Call {
                function,
                args,
                calling_convention,
                tail_call,
            } => {
                let call_instr = self.lower_call_instruction(
                    instr_id,
                    ty_hint.clone(),
                    function,
                    args,
                    calling_convention,
                    tail_call,
                )?;

                self.llvm_ctx
                    .add_instruction(Instruction::Call(call_instr))
                    .map_err(fp_core::error::Error::from)?;
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

    fn lower_call_instruction(
        &mut self,
        instr_id: u32,
        ty_hint: Option<lir::LirType>,
        function: lir::LirValue,
        args: Vec<lir::LirValue>,
        calling_convention: lir::CallingConvention,
        tail_call: bool,
    ) -> Result<instruction::Call> {
        let function_name = match &function {
            lir::LirValue::Function(name) => Some(name.clone()),
            lir::LirValue::Global(name, _) => Some(name.clone()),
            _ => None,
        };

        if let Some(name) = &function_name {
            debug!("[fp-llvm] lowering call to {}", name);
        } else {
            debug!("[fp-llvm] lowering call to <expr>");
        }

        if let Some(logical_name) = function_name.as_ref() {
            if let Some(resolved) = self.resolve_intrinsic(logical_name) {
                if let ResolvedIntrinsic::Call(call) = resolved {
                    return self.lower_intrinsic_call(
                        instr_id,
                        ty_hint,
                        call.clone(),
                        args,
                        tail_call,
                    );
                }
            }

            let runtime_target = self.map_std_function_to_runtime(logical_name);
            let append_newline = matches!(
                logical_name.as_str(),
                "println" | "println!" | "std::io::println" | "print" | "std::io::print"
            );
            if CStdLib::is_stdlib_function(&runtime_target) {
                return self.lower_runtime_call(
                    instr_id,
                    ty_hint,
                    runtime_target,
                    append_newline,
                    args,
                    calling_convention,
                    tail_call,
                );
            }
        }

        self.lower_user_call(
            instr_id,
            ty_hint,
            function,
            args,
            calling_convention,
            tail_call,
        )
    }

    fn lower_runtime_call(
        &mut self,
        instr_id: u32,
        ty_hint: Option<lir::LirType>,
        runtime_name: String,
        mut append_newline: bool,
        args: Vec<lir::LirValue>,
        calling_convention: lir::CallingConvention,
        tail_call: bool,
    ) -> Result<instruction::Call> {
        let runtime_decl = CStdLib::get_function_decl(&runtime_name, &self.llvm_ctx.module.types)
            .ok_or_else(|| {
            report_error(format!(
                "{} Unknown runtime function '{}'",
                LOG_AREA, runtime_name
            ))
        })?;

        let return_type = runtime_decl.return_type.clone();
        let param_type_refs: Vec<TypeRef> = runtime_decl
            .parameters
            .iter()
            .map(|p| p.ty.clone())
            .collect();
        let is_var_arg = runtime_decl.is_var_arg;

        if !self
            .llvm_ctx
            .module
            .functions
            .iter()
            .any(|f| f.name == runtime_name)
        {
            self.llvm_ctx.module.functions.push(runtime_decl);
        }

        let fn_ty = self.llvm_ctx.module.types.func_type(
            return_type.clone(),
            param_type_refs.clone(),
            is_var_arg,
        );

        let mut call_args: Vec<(Operand, Vec<function::ParameterAttribute>)> = Vec::new();
        for (index, arg) in args.into_iter().enumerate() {
            let operand = match arg {
                lir::LirValue::Constant(lir::LirConstant::String(mut s)) => {
                    if append_newline && index == 0 && !s.ends_with('\n') {
                        s.push('\n');
                    }
                    append_newline = false; // ensure only first string gets newline
                    let const_ref =
                        self.convert_lir_constant_to_llvm_mut(lir::LirConstant::String(s))?;
                    self.llvm_ctx.operand_from_constant(const_ref)
                }
                other => self.convert_lir_value_to_operand(other)?,
            };
            call_args.push((operand, Vec::new()));
        }

        let dest = if let Some(ref hint) = ty_hint {
            let name = Name::Name(Box::new(format!("call_{}", instr_id)));
            self.record_result(instr_id, Some(hint.clone()), name.clone());
            Some(name)
        } else {
            None
        };

        let callee = either::Either::Right(Operand::ConstantOperand(ConstantRef::new(
            Constant::GlobalReference {
                name: Name::Name(Box::new(runtime_name.clone())),
                ty: fn_ty.clone(),
            },
        )));

        let llvm_calling_convention = self.convert_calling_convention(&calling_convention);

        Ok(instruction::Call {
            function: callee,
            function_ty: fn_ty,
            arguments: call_args,
            return_attributes: Vec::new(),
            dest,
            function_attributes: Vec::new(),
            is_tail_call: tail_call,
            calling_convention: llvm_calling_convention,
            debugloc: None,
        })
    }

    fn lower_intrinsic_call(
        &mut self,
        instr_id: u32,
        ty_hint: Option<lir::LirType>,
        resolved: ResolvedCall,
        args: Vec<lir::LirValue>,
        tail_call: bool,
    ) -> Result<instruction::Call> {
        match resolved.arg_strategy {
            CallArgStrategy::Passthrough => {}
        }

        let calling_convention =
            self.calling_convention_for(resolved.abi, lir::CallingConvention::C);
        self.lower_runtime_call(
            instr_id,
            ty_hint,
            resolved.callee.to_string(),
            resolved.append_newline_to_first_string,
            args,
            calling_convention,
            tail_call,
        )
    }

    fn resolve_intrinsic(&self, logical_name: &str) -> Option<ResolvedIntrinsic> {
        let kind = intrinsics::identify_symbol(logical_name)?;
        intrinsics::default_resolver()
            .resolve(&(kind, self.backend_flavor))
            .cloned()
    }

    fn calling_convention_for(
        &self,
        abi: CallAbi,
        _fallback: lir::CallingConvention,
    ) -> lir::CallingConvention {
        match abi {
            CallAbi::Default | CallAbi::CVariadic => lir::CallingConvention::C,
        }
    }

    fn populate_argument_operands(
        &mut self,
        function_name: &str,
        locals: &[lir::LirLocal],
    ) -> Result<()> {
        let func = self
            .llvm_ctx
            .module
            .functions
            .iter()
            .find(|f| f.name == function_name)
            .ok_or_else(|| {
                report_error(format!(
                    "{} Unable to find LLVM function record for '{}'",
                    LOG_AREA, function_name
                ))
            })?
            .clone();

        let mut param_iter = func.parameters.into_iter();
        for local in locals {
            if local.is_argument {
                let param = param_iter.next().ok_or_else(|| {
                    report_error(format!(
                        "{} Not enough LLVM parameters to map local argument {} in '{}'",
                        LOG_AREA, local.id, function_name
                    ))
                })?;

                let operand = Operand::LocalOperand {
                    name: param.name,
                    ty: param.ty,
                };
                self.argument_operands.insert(local.id, operand);
            }
        }

        Ok(())
    }

    fn lower_user_call(
        &mut self,
        instr_id: u32,
        ty_hint: Option<lir::LirType>,
        function: lir::LirValue,
        args: Vec<lir::LirValue>,
        calling_convention: lir::CallingConvention,
        tail_call: bool,
    ) -> Result<instruction::Call> {
        let (function_name, callee_operand) = match &function {
            lir::LirValue::Function(name) => (name.clone(), None),
            lir::LirValue::Global(name, _) => (name.clone(), None),
            other => {
                return Err(report_error(format!(
                    "{} Unsupported call target in LLVM lowering: {:?}",
                    LOG_AREA, other
                )));
            }
        };

        let llvm_symbol = self.llvm_symbol_for(&function_name);

        let signature = self.function_signatures.get(&function_name).cloned();

        let return_lir_type = if let Some(sig) = &signature {
            sig.return_type.clone()
        } else if let Some(hint) = &ty_hint {
            hint.clone()
        } else {
            lir::LirType::Void
        };

        let return_type_ref = self.to_type_ref(return_lir_type.clone())?;

        let param_lir_types = if let Some(sig) = signature.clone() {
            sig.params
        } else {
            let mut inferred = Vec::with_capacity(args.len());
            for (index, arg) in args.iter().enumerate() {
                let ty = self.lir_value_type(arg).ok_or_else(|| {
                    report_error(format!(
                        "{} Unable to determine type for argument {} in call to '{}'",
                        LOG_AREA, index, function_name
                    ))
                })?;
                inferred.push(ty);
            }
            inferred
        };

        let mut param_type_refs = Vec::with_capacity(param_lir_types.len());
        for lir_ty in param_lir_types.iter() {
            param_type_refs.push(self.to_type_ref(lir_ty.clone())?);
        }

        let llvm_calling_convention = self.convert_calling_convention(&calling_convention);

        let is_defined = self.defined_functions.contains(&llvm_symbol);
        let current_symbol = self.current_function.clone();
        let is_current_function = current_symbol.as_deref() == Some(&llvm_symbol);

        let needs_stub = !is_defined
            && !is_current_function
            && !self
                .llvm_ctx
                .module
                .functions
                .iter()
                .any(|f| f.name == llvm_symbol);

        if needs_stub {
            tracing::debug!("Declaring stub for {}", llvm_symbol);
            self.declare_function_stub(
                &llvm_symbol,
                &param_type_refs,
                return_type_ref.clone(),
                signature.as_ref().map(|s| s.is_variadic).unwrap_or(false),
                llvm_calling_convention,
            );
        }

        let fn_ty = self.llvm_ctx.module.types.func_type(
            return_type_ref.clone(),
            param_type_refs.clone(),
            signature.as_ref().map(|s| s.is_variadic).unwrap_or(false),
        );

        let callee = either::Either::Right(match callee_operand {
            Some(op) => op,
            None => Operand::ConstantOperand(ConstantRef::new(Constant::GlobalReference {
                name: Name::Name(Box::new(llvm_symbol.clone())),
                ty: fn_ty.clone(),
            })),
        });

        let mut call_args: Vec<(Operand, Vec<function::ParameterAttribute>)> = Vec::new();
        for arg in args.into_iter() {
            let operand = self.convert_lir_value_to_operand(arg)?;
            call_args.push((operand, Vec::new()));
        }

        let dest = if let Some(ref hint) = ty_hint {
            if !matches!(hint, lir::LirType::Void) {
                let name = Name::Name(Box::new(format!("call_{}", instr_id)));
                self.record_result(instr_id, Some(hint.clone()), name.clone());
                Some(name)
            } else {
                None
            }
        } else {
            None
        };

        Ok(instruction::Call {
            function: callee,
            function_ty: fn_ty,
            arguments: call_args,
            return_attributes: Vec::new(),
            dest,
            function_attributes: Vec::new(),
            is_tail_call: tail_call,
            calling_convention: llvm_calling_convention,
            debugloc: None,
        })
    }

    fn lower_int_cmp(
        &mut self,
        predicate: IntPredicate,
        lhs: lir::LirValue,
        rhs: lir::LirValue,
        instr_id: u32,
        ty_hint: Option<lir::LirType>,
    ) -> Result<()> {
        let hint = ty_hint.clone();
        let (lhs_aligned, rhs_aligned) = self.align_cmp_operands(lhs, rhs);

        if let (lir::LirValue::Constant(ref lhs_const), lir::LirValue::Constant(ref rhs_const)) =
            (&lhs_aligned, &rhs_aligned)
        {
            if let Some(result) = Self::fold_int_cmp(predicate, lhs_const, rhs_const) {
                let target_ty = hint.clone().unwrap_or(lir::LirType::I1);
                let constant = match target_ty {
                    lir::LirType::I1 => lir::LirConstant::Bool(result),
                    lir::LirType::I8 => lir::LirConstant::Int(result as i64, lir::LirType::I8),
                    lir::LirType::I16 => lir::LirConstant::Int(result as i64, lir::LirType::I16),
                    lir::LirType::I32 => lir::LirConstant::Int(result as i64, lir::LirType::I32),
                    lir::LirType::I64 => lir::LirConstant::Int(result as i64, lir::LirType::I64),
                    lir::LirType::I128 => lir::LirConstant::Int(result as i64, lir::LirType::I128),
                    _ => lir::LirConstant::Bool(result),
                };
                self.constant_results.insert(instr_id, constant);
                return Ok(());
            }
        }

        let lhs_operand = self.convert_lir_value_to_operand(lhs_aligned)?;
        let rhs_operand = self.convert_lir_value_to_operand(rhs_aligned)?;
        let cmp_name = self
            .llvm_ctx
            .build_icmp(
                predicate,
                lhs_operand,
                rhs_operand,
                &format!("icmp_{}", instr_id),
            )
            .map_err(fp_core::error::Error::from)?;

        let bool_type = self.llvm_ctx.module.types.bool();
        let cmp_operand = Operand::LocalOperand {
            name: cmp_name.clone(),
            ty: bool_type,
        };

        match hint.unwrap_or(lir::LirType::I1) {
            lir::LirType::I1 => {
                self.record_result(instr_id, Some(lir::LirType::I1), cmp_name);
            }
            wider_ty => {
                let llvm_target_ty = self.convert_lir_type_to_llvm(wider_ty.clone())?;
                let zext_name = self
                    .llvm_ctx
                    .build_zext(cmp_operand, llvm_target_ty, &format!("zext_{}", instr_id))
                    .map_err(fp_core::error::Error::from)?;
                self.record_result(instr_id, Some(wider_ty), zext_name);
            }
        }

        Ok(())
    }

    fn cast_condition_to_bool(
        &mut self,
        operand: Operand,
        lir_ty: Option<lir::LirType>,
        hint_name: &str,
    ) -> Result<Operand> {
        if matches!(lir_ty, Some(lir::LirType::I1)) {
            return Ok(operand);
        }

        if let Operand::ConstantOperand(constant) = &operand {
            let truthy = match constant.as_ref() {
                llvm_ir::constant::Constant::Int { value, .. } => *value != 0,
                llvm_ir::constant::Constant::Float(f) => match f {
                    Float::Single(v) => *v != 0.0,
                    Float::Double(v) => *v != 0.0,
                    _ => true,
                },
                llvm_ir::constant::Constant::Null(_) => false,
                _ => true,
            };
            let bool_constant = self.llvm_ctx.const_bool(truthy);
            return Ok(self.llvm_ctx.operand_from_constant(bool_constant));
        }

        let zero_operand = match lir_ty.clone().unwrap_or(lir::LirType::I32) {
            lir::LirType::I64 => self
                .llvm_ctx
                .operand_from_constant(self.llvm_ctx.const_i64(0)),
            lir::LirType::I1 => self
                .llvm_ctx
                .operand_from_constant(self.llvm_ctx.const_bool(false)),
            _ => self
                .llvm_ctx
                .operand_from_constant(self.llvm_ctx.const_i32(0)),
        };

        let cmp_name = self
            .llvm_ctx
            .build_icmp(
                IntPredicate::NE,
                operand,
                zero_operand,
                &format!("cond_bool_{}", hint_name),
            )
            .map_err(fp_core::error::Error::from)?;

        Ok(Operand::LocalOperand {
            name: cmp_name,
            ty: self.llvm_ctx.module.types.bool(),
        })
    }

    fn align_cmp_operands(
        &self,
        lhs: lir::LirValue,
        rhs: lir::LirValue,
    ) -> (lir::LirValue, lir::LirValue) {
        let lhs_ty = self.lir_value_type(&lhs);
        let rhs_ty = self.lir_value_type(&rhs);

        if let (Some(ref lt), Some(ref rt)) = (&lhs_ty, &rhs_ty) {
            if Self::is_integer_type(lt) && Self::is_integer_type(rt) && lt != rt {
                let target = Self::max_int_type(lt.clone(), rt.clone());
                let lhs_conv = Self::convert_constant_to_type(lhs, &target);
                let rhs_conv = Self::convert_constant_to_type(rhs, &target);
                return (lhs_conv, rhs_conv);
            }
        }

        (lhs, rhs)
    }

    fn convert_constant_to_type(value: lir::LirValue, target: &lir::LirType) -> lir::LirValue {
        match value {
            lir::LirValue::Constant(lir::LirConstant::Int(v, _)) => {
                lir::LirValue::Constant(lir::LirConstant::Int(v, target.clone()))
            }
            lir::LirValue::Constant(lir::LirConstant::UInt(v, _)) => {
                lir::LirValue::Constant(lir::LirConstant::UInt(v, target.clone()))
            }
            other => other,
        }
    }

    fn is_integer_type(ty: &lir::LirType) -> bool {
        matches!(
            ty,
            lir::LirType::I1
                | lir::LirType::I8
                | lir::LirType::I16
                | lir::LirType::I32
                | lir::LirType::I64
                | lir::LirType::I128
        )
    }

    fn max_int_type(lhs: lir::LirType, rhs: lir::LirType) -> lir::LirType {
        let lhs_rank = Self::int_type_rank(&lhs);
        let rhs_rank = Self::int_type_rank(&rhs);
        if lhs_rank >= rhs_rank {
            lhs
        } else {
            rhs
        }
    }

    fn int_type_rank(ty: &lir::LirType) -> u32 {
        match ty {
            lir::LirType::I1 => 1,
            lir::LirType::I8 => 8,
            lir::LirType::I16 => 16,
            lir::LirType::I32 => 32,
            lir::LirType::I64 => 64,
            lir::LirType::I128 => 128,
            _ => 32,
        }
    }

    fn lir_value_type(&self, value: &lir::LirValue) -> Option<lir::LirType> {
        match value {
            lir::LirValue::Constant(lir::LirConstant::Int(_, ty))
            | lir::LirValue::Constant(lir::LirConstant::UInt(_, ty))
            | lir::LirValue::Constant(lir::LirConstant::Float(_, ty))
            | lir::LirValue::Constant(lir::LirConstant::Struct(_, ty))
            | lir::LirValue::Constant(lir::LirConstant::Array(_, ty))
            | lir::LirValue::Constant(lir::LirConstant::Null(ty))
            | lir::LirValue::Constant(lir::LirConstant::Undef(ty)) => Some(ty.clone()),
            lir::LirValue::Constant(lir::LirConstant::String(_)) => {
                Some(lir::LirType::Ptr(Box::new(lir::LirType::I8)))
            }
            lir::LirValue::Constant(lir::LirConstant::Bool(_)) => Some(lir::LirType::I1),
            lir::LirValue::Register(reg_id) => self.value_map.get(reg_id).map(|(_, ty)| ty.clone()),
            lir::LirValue::Global(_, ty) => Some(ty.clone()),
            lir::LirValue::Function(_) => Some(lir::LirType::Ptr(Box::new(lir::LirType::Void))),
            _ => None,
        }
    }

    fn to_type_ref(&self, lir_type: lir::LirType) -> Result<TypeRef> {
        let llvm_type = self.convert_lir_type_to_llvm(lir_type)?;
        Ok(self.llvm_ctx.module.types.get_for_type(&llvm_type))
    }

    fn llvm_symbol_for(&mut self, original: &str) -> String {
        if let Some(existing) = self.symbol_names.get(original) {
            return existing.clone();
        }

        let sanitized = Self::sanitize_symbol(original);
        self.symbol_names
            .insert(original.to_string(), sanitized.clone());
        sanitized
    }

    fn sanitize_symbol(name: &str) -> String {
        let mut result = String::with_capacity(name.len());
        for ch in name.chars() {
            match ch {
                'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '.' | '$' => result.push(ch),
                _ => result.push('_'),
            }
        }

        if result.is_empty() {
            return "_sym".to_string();
        }

        let first = result.chars().next().unwrap();
        if !matches!(first, 'a'..='z' | 'A'..='Z' | '_' | '$') {
            result.insert(0, '_');
        }

        result
    }

    fn function_type_from_signature(
        &mut self,
        signature: lir::LirFunctionSignature,
    ) -> Result<TypeRef> {
        let lir::LirFunctionSignature {
            params,
            return_type,
            is_variadic,
        } = signature;

        let return_type = self.to_type_ref(return_type)?;
        let mut param_types = Vec::with_capacity(params.len());
        for param in params {
            param_types.push(self.to_type_ref(param)?);
        }

        Ok(self
            .llvm_ctx
            .module
            .types
            .func_type(return_type, param_types, is_variadic))
    }

    fn declare_function_stub(
        &mut self,
        name: &str,
        param_types: &[TypeRef],
        return_type: TypeRef,
        is_variadic: bool,
        calling_convention: function::CallingConvention,
    ) {
        let parameters = param_types
            .iter()
            .enumerate()
            .map(|(index, ty)| function::Parameter {
                name: Name::Name(Box::new(format!("arg{}", index))),
                ty: ty.clone(),
                attributes: Vec::new(),
            })
            .collect();

        let declaration = function::FunctionDeclaration {
            name: name.to_string(),
            parameters,
            is_var_arg: is_variadic,
            return_type,
            return_attributes: Vec::new(),
            linkage: Linkage::External,
            visibility: Visibility::Default,
            dll_storage_class: DLLStorageClass::Default,
            calling_convention,
            alignment: 0,
            garbage_collector_name: None,
            debugloc: None,
        };

        self.llvm_ctx.module.func_declarations.push(declaration);
    }

    fn convert_calling_convention(
        &self,
        cc: &lir::CallingConvention,
    ) -> function::CallingConvention {
        use function::CallingConvention as LlvmCc;
        use lir::CallingConvention as LirCc;

        match cc {
            &LirCc::C => LlvmCc::C,
            &LirCc::Fast => LlvmCc::Fast,
            &LirCc::Cold => LlvmCc::Cold,
            &LirCc::WebKitJS => LlvmCc::WebKit_JS,
            &LirCc::AnyReg => LlvmCc::AnyReg,
            &LirCc::PreserveMost => LlvmCc::PreserveMost,
            &LirCc::PreserveAll => LlvmCc::PreserveAll,
            &LirCc::Swift => LlvmCc::Swift,
            &LirCc::CxxFastTLS => LlvmCc::CXX_FastTLS,
            &LirCc::X86StdCall => LlvmCc::X86_StdCall,
            &LirCc::X86FastCall => LlvmCc::X86_FastCall,
            &LirCc::X86ThisCall => LlvmCc::X86_ThisCall,
            &LirCc::X86VectorCall => LlvmCc::X86_VectorCall,
            &LirCc::Win64 => LlvmCc::Win64,
            &LirCc::X86_64SysV => LlvmCc::X86_64_SysV,
            &LirCc::AAPCS => LlvmCc::ARM_APCS,
            &LirCc::AAPCSVfp => LlvmCc::ARM_AAPCS_VFP,
        }
    }

    fn fold_int_cmp(
        predicate: IntPredicate,
        lhs: &lir::LirConstant,
        rhs: &lir::LirConstant,
    ) -> Option<bool> {
        use IntPredicate::*;
        match (lhs, rhs) {
            (lir::LirConstant::Int(a, _), lir::LirConstant::Int(b, _)) => {
                let (a, b) = (*a, *b);
                Some(match predicate {
                    EQ => a == b,
                    NE => a != b,
                    SLT => a < b,
                    SLE => a <= b,
                    SGT => a > b,
                    SGE => a >= b,
                    _ => return None,
                })
            }
            (lir::LirConstant::UInt(a, _), lir::LirConstant::UInt(b, _)) => {
                let (a, b) = (*a, *b);
                Some(match predicate {
                    EQ => a == b,
                    NE => a != b,
                    ULT => a < b,
                    ULE => a <= b,
                    UGT => a > b,
                    UGE => a >= b,
                    _ => return None,
                })
            }
            (lir::LirConstant::Bool(a), lir::LirConstant::Bool(b)) => Some(match predicate {
                EQ => a == b,
                NE => a != b,
                _ => return None,
            }),
            _ => None,
        }
    }

    fn zero_operand_for_type(&self, ty: &lir::LirType) -> Result<Operand> {
        let operand = match ty {
            lir::LirType::I1 => self
                .llvm_ctx
                .operand_from_constant(self.llvm_ctx.const_bool(false)),
            lir::LirType::I8 | lir::LirType::I16 | lir::LirType::I32 => self
                .llvm_ctx
                .operand_from_constant(self.llvm_ctx.const_i32(0)),
            lir::LirType::I64 | lir::LirType::I128 => self
                .llvm_ctx
                .operand_from_constant(self.llvm_ctx.const_i64(0)),
            _ => self
                .llvm_ctx
                .operand_from_constant(self.llvm_ctx.const_i32(0)),
        };
        Ok(operand)
    }

    /// Map std library functions to their runtime implementations
    fn map_std_function_to_runtime(&self, fn_name: &str) -> String {
        match fn_name {
            // I/O functions
            "println" | "println!" | "std::io::println" => "printf".to_string(),
            "print" | "print!" | "std::io::print" => "printf".to_string(),
            "eprint" | "eprint!" | "std::io::eprint" => "fprintf".to_string(),
            "eprintln" | "eprintln!" | "std::io::eprintln" => "fprintf".to_string(),

            // Memory management functions
            "std::alloc::alloc" => "malloc".to_string(),
            "std::alloc::dealloc" => "free".to_string(),
            "std::alloc::realloc" => "realloc".to_string(),

            // Math functions (map to libm)
            "std::f64::sin" => "sin".to_string(),
            "std::f64::cos" => "cos".to_string(),
            "std::f64::tan" => "tan".to_string(),
            "std::f64::sqrt" => "sqrt".to_string(),
            "std::f64::pow" => "pow".to_string(),
            "std::f32::sin" => "sinf".to_string(),
            "std::f32::cos" => "cosf".to_string(),
            "std::f32::tan" => "tanf".to_string(),
            "std::f32::sqrt" => "sqrtf".to_string(),
            "std::f32::pow" => "powf".to_string(),

            // String functions
            "std::str::len" => "strlen".to_string(),
            "std::str::cmp" => "strcmp".to_string(),

            // Process functions
            "std::process::exit" => "exit".to_string(),
            "std::process::abort" => "abort".to_string(),

            // For unknown functions, return as-is
            _ => fn_name.to_string(),
        }
    }

    /// Generate LLVM IR for a LIR terminator
    fn generate_terminator(&mut self, lir_term: lir::LirTerminator) -> Result<()> {
        tracing::debug!("Terminator type: {:?}", std::mem::discriminant(&lir_term));
        match lir_term {
            lir::LirTerminator::Return(value) => {
                let mut return_operand = match value {
                    Some(val) => Some(self.convert_lir_value_to_operand(val)?),
                    None => None,
                };

                if return_operand.is_none() {
                    if let Some(ref lir_ty) = self.current_return_type {
                        if !matches!(lir_ty, lir::LirType::Void) {
                            let zero = self.zero_operand_for_type(lir_ty)?;
                            return_operand = Some(zero);
                        }
                    }
                }

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
                condition,
                if_true,
                if_false,
            } => {
                let condition_clone = condition.clone();
                let lir_type = match condition_clone {
                    lir::LirValue::Register(reg_id) => {
                        self.value_map.get(&reg_id).map(|(_, ty)| ty.clone())
                    }
                    _ => None,
                };
                let condition_operand = self.convert_lir_value_to_operand(condition)?;
                let bool_operand = self.cast_condition_to_bool(
                    condition_operand,
                    lir_type,
                    &format!("{}_{}", if_true, if_false),
                )?;

                let true_label = format!("bb{}", if_true);
                let false_label = format!("bb{}", if_false);
                self.llvm_ctx
                    .build_conditional_branch(bool_operand, &true_label, &false_label)
                    .map_err(fp_core::error::Error::from)?;
            }
            term => {
                return Err(report_error(format!(
                    "{} Unimplemented LIR terminator: {:?}",
                    LOG_AREA, term
                )));
            }
        }

        Ok(())
    }

    /// Convert LIR value to LLVM operand
    fn convert_lir_value_to_operand(&mut self, lir_value: lir::LirValue) -> Result<Operand> {
        match lir_value {
            lir::LirValue::Register(reg_id) => {
                if let Some(constant) = self.constant_results.get(&reg_id) {
                    let llvm_constant = self.convert_lir_constant_to_llvm_mut(constant.clone())?;
                    return Ok(self.llvm_ctx.operand_from_constant(llvm_constant));
                }
                if let Some((name, lir_ty)) = self.value_map.get(&reg_id) {
                    let llvm_ty = self.convert_lir_type_to_llvm(lir_ty.clone())?;
                    Ok(self
                        .llvm_ctx
                        .operand_from_name_and_type(name.clone(), &llvm_ty))
                } else {
                    Err(report_error(format!(
                        "{} Unknown register {} encountered during codegen",
                        LOG_AREA, reg_id
                    )))
                }
            }
            lir::LirValue::Constant(constant) => {
                let llvm_constant = self.convert_lir_constant_to_llvm_mut(constant)?;
                Ok(self.llvm_ctx.operand_from_constant(llvm_constant))
            }
            lir::LirValue::Global(name, ty) => {
                if let Some(lir_constant) = self.global_const_map.get(&name) {
                    let llvm_constant = self.convert_lir_constant_to_llvm(lir_constant.clone())?;
                    tracing::debug!("LLVM: Found global '{}' in const map, using value", name);
                    return Ok(self.llvm_ctx.operand_from_constant(llvm_constant));
                }

                Err(report_error(format!(
                    "{} Global variable '{}' of type {:?} not found",
                    LOG_AREA, name, ty
                )))
            }
            lir::LirValue::Function(name) => {
                let llvm_name = self.llvm_symbol_for(&name);

                let fn_ty = if let Some(signature) = self.function_signatures.get(&name) {
                    self.function_type_from_signature(signature.clone())?
                } else if let Some(existing) = self.llvm_ctx.get_function(&llvm_name) {
                    self.llvm_ctx.module.types.func_type(
                        existing.return_type.clone(),
                        existing.parameters.iter().map(|p| p.ty.clone()).collect(),
                        existing.is_var_arg,
                    )
                } else {
                    return Err(report_error(format!(
                        "{} Unknown function reference '{}' encountered during codegen",
                        LOG_AREA, name
                    )));
                };

                Ok(Operand::ConstantOperand(ConstantRef::new(
                    Constant::GlobalReference {
                        name: Name::Name(Box::new(llvm_name)),
                        ty: fn_ty,
                    },
                )))
            }
            lir::LirValue::Local(local_id) => {
                if let Some(op) = self.argument_operands.get(&local_id) {
                    return Ok(op.clone());
                }

                Err(report_error(format!(
                    "{} Unknown local: {}",
                    LOG_AREA, local_id
                )))
            }
            lir::LirValue::StackSlot(slot_id) => {
                // For now, treat stack slots like registers
                if let Some((name, lir_ty)) = self.value_map.get(&slot_id) {
                    let llvm_ty = self.convert_lir_type_to_llvm(lir_ty.clone())?;
                    Ok(self
                        .llvm_ctx
                        .operand_from_name_and_type(name.clone(), &llvm_ty))
                } else {
                    Err(report_error(format!(
                        "{} Unknown stack slot: {}",
                        LOG_AREA, slot_id
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
                other => Err(report_error(format!(
                    "{} Unsupported floating-point type {:?} for LLVM constant",
                    LOG_AREA, other
                ))),
            },
            lir::LirConstant::Bool(value) => Ok(self.llvm_ctx.const_bool(value)),
            lir::LirConstant::String(s) => {
                // Defer to mutable self method via interior mutability workaround
                Err(report_error(format!(
                    "{} String constant requires mutable context: {}",
                    LOG_AREA, s
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
            lir::LirConstant::String(s) => Ok(self.get_or_create_string_ptr(&s)),
            other => {
                // Use the immutable path for non-string constants
                self.convert_lir_constant_to_llvm(other)
            }
        }
    }

    /// Get the LLVM type for a given constant
    fn get_type_from_constant(&self, constant: &ConstantRef) -> Result<TypeRef> {
        let ty = match constant.as_ref() {
            Constant::Int { bits, .. } => match bits {
                1 => self.llvm_ctx.module.types.bool(),
                8 => self.llvm_ctx.module.types.i8(),
                16 => self.llvm_ctx.module.types.i16(),
                32 => self.llvm_ctx.module.types.i32(),
                64 => self.llvm_ctx.module.types.i64(),
                other => {
                    return Err(report_error(format!(
                        "{} Unsupported integer bit width {} for LLVM constant",
                        LOG_AREA, other
                    )))
                }
            },
            Constant::Float(Float::Single(_)) => self.llvm_ctx.module.types.fp(FPType::Single),
            Constant::Float(Float::Double(_)) => self.llvm_ctx.module.types.fp(FPType::Double),
            Constant::Array {
                element_type,
                elements,
            } => self
                .llvm_ctx
                .module
                .types
                .array_of(element_type.clone(), elements.len()),
            other => {
                return Err(report_error(format!(
                    "{} Unsupported LLVM constant for type inference: {:?}",
                    LOG_AREA, other
                )))
            }
        };

        Ok(ty)
    }

    fn const_i8(&self, b: u8) -> ConstantRef {
        ConstantRef::new(Constant::Int {
            bits: 8,
            value: b as u64,
        })
    }

    fn get_or_create_string_ptr(&mut self, s: &str) -> ConstantRef {
        if let Some((name, _len)) = self.string_globals.get(s).cloned() {
            let ptr_ty = self.llvm_ctx.module.types.pointer();
            return ConstantRef::new(Constant::GlobalReference { name, ty: ptr_ty });
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

        let ptr_ty = self.llvm_ctx.module.types.pointer();
        ConstantRef::new(Constant::GlobalReference {
            name: gv_name,
            ty: ptr_ty,
        })
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
            lir::LirType::Ptr(_) => Ok(Type::PointerType { addr_space: 0 }),
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
