use crate::context::LlvmContext;
use crate::intrinsics::CRuntimeIntrinsics;
use fp_core::diagnostics::report_error_with_context;
use fp_core::{
    error::{Error, Result},
    lir,
};
use llvm_ir::constant::Float;
use llvm_ir::instruction::{And, LShr, Or, SRem, Shl, Xor};
use llvm_ir::module::{DLLStorageClass, GlobalVariable, Linkage, ThreadLocalMode, Visibility};
use llvm_ir::predicates::IntPredicate;
use llvm_ir::types::{FPType, Typed};
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
    symbol_prefix: String,
    // Track referenced globals that need const evaluation
    referenced_globals: std::collections::HashSet<String>,
    // Global constants map from MIR->LIR transformation
    global_const_map: HashMap<String, lir::LirConstant>,
    constant_results: HashMap<u32, lir::LirConstant>,
    current_return_type: Option<lir::LirType>,
    function_signatures: HashMap<String, lir::LirFunctionSignature>,
    symbol_names: HashMap<String, String>,
    defined_functions: std::collections::HashSet<String>,
    argument_operands: HashMap<u32, Operand>,
    allow_unresolved_globals: bool,
}

impl<'ctx> LirCodegen<'ctx> {
    fn attach_context(&self, err: Error, context: impl Into<String>) -> Error {
        match err {
            Error::Diagnostic(diag) => {
                let diag = if diag.source_context.is_none() {
                    diag.with_source_context(LOG_AREA.to_string())
                } else {
                    diag
                };
                let diag = diag.with_suggestion(context.into());
                Error::diagnostic(diag)
            }
            other => other,
        }
    }

    /// Create a new LIR code generator
    pub fn new(
        llvm_ctx: &'ctx mut LlvmContext,

        // should be in lir_ctx
        global_const_map: HashMap<String, lir::LirConstant>,
        allow_unresolved_globals: bool,
    ) -> Self {
        let prefix = Self::sanitize_symbol_component(&llvm_ctx.module.name);
        Self {
            llvm_ctx,
            value_map: HashMap::new(),
            block_map: HashMap::new(),
            current_function: None,
            string_globals: HashMap::new(),
            next_string_id: 0,
            symbol_prefix: prefix,
            referenced_globals: std::collections::HashSet::new(),
            global_const_map,
            constant_results: HashMap::new(),
            current_return_type: None,
            function_signatures: HashMap::new(),
            symbol_names: HashMap::new(),
            defined_functions: std::collections::HashSet::new(),
            argument_operands: HashMap::new(),
            allow_unresolved_globals,
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
            self.function_signatures.insert(
                String::from(function.name.clone()),
                function.signature.clone(),
            );
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
            let global_name = global.name.clone();
            self.generate_global(global).map_err(|err| {
                self.attach_context(
                    err,
                    format!("while generating global '{}' (index {})", global_name, i),
                )
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
            self.generate_function(function).map_err(|err| {
                self.attach_context(
                    err,
                    format!(
                        "while generating function '{}' (index {})",
                        function_name, i
                    ),
                )
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
            return Err(report_error_with_context(
                LOG_AREA,
                format!("Global variable '{}' must have an initializer", global.name),
            ));
        };

        let global_var = GlobalVariable {
            name: llvm_ir::Name::Name(Box::new(String::from(global.name.clone()))),
            linkage: self.convert_linkage(global.linkage),
            visibility: self.convert_visibility(global.visibility),
            is_constant: global.is_constant,
            ty,
            addr_space: 0,
            dll_storage_class: DLLStorageClass::Default,
            thread_local_mode: ThreadLocalMode::NotThreadLocal,
            unnamed_addr: None,
            initializer,
            section: global.section.clone(),
            comdat: None,
            alignment: global.alignment.unwrap_or(0),
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
        let func_name_for_params = lir_func.name.clone();
        let param_types: Vec<Type> = lir_func
            .signature
            .params
            .iter()
            .enumerate()
            .map(|(i, param)| {
                debug!("LLVM: Converting param {} type: {:?}", i, param);
                let context = format!(
                    "while converting parameter {} of function '{}'",
                    i, func_name_for_params
                );
                self.convert_lir_type_to_llvm(param.clone())
                    .map_err(|err| self.attach_context(err, context))
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
        if lir_func.name.as_str() == "main" && matches!(return_lir_type, lir::LirType::Void) {
            return_lir_type = lir::LirType::I32;
        }
        let func_name_for_return = String::from(lir_func.name.clone());
        let return_type = self
            .convert_lir_type_to_llvm(return_lir_type.clone())
            .map_err(|err| {
                self.attach_context(
                    err,
                    format!(
                        "while converting return type of function '{}'",
                        func_name_for_return
                    ),
                )
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
        self.argument_operands.clear();

        // Populate argument operands after clearing maps
        self.populate_argument_operands(&function_name, &lir_func.locals)?;

        for (i, basic_block) in lir_func.basic_blocks.into_iter().enumerate() {
            debug!(
                "LLVM: Processing basic block {}: {:?}",
                i, basic_block.label
            );
            let block_id = basic_block.id;
            let block_label = basic_block.label.clone();
            self.generate_basic_block(basic_block).map_err(|err| {
                self.attach_context(
                    err,
                    format!(
                        "while generating basic block {} ({:?}) of function '{}'",
                        block_id, block_label, function_name
                    ),
                )
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
            .map(|n| String::from(n))
            .unwrap_or_else(|| format!("bb{}", lir_block.id));
        let basic_block = BasicBlock::new(Name::Name(Box::new(block_name.clone())));
        self.block_map
            .insert(lir_block.id, Name::Name(Box::new(block_name.clone())));

        let func_name = self.llvm_ctx.current_function().ok_or_else(|| {
            report_error_with_context(LOG_AREA, "No current function set".to_string())
        })?;

        // Add the basic block to the function
        let function = self
            .llvm_ctx
            .module
            .functions
            .iter_mut()
            .find(|f| f.name == func_name)
            .ok_or_else(|| {
                report_error_with_context(LOG_AREA, "Current function not found".to_string())
            })?;

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
                let result_ty = self.infer_binary_result_type(ty_hint.clone(), &lhs, &rhs);
                let llvm_result_ty = self.convert_lir_type_to_llvm(result_ty.clone())?;
                let mut lhs_operand = self.convert_lir_value_to_operand(lhs.clone())?;
                let mut rhs_operand = self.convert_lir_value_to_operand(rhs.clone())?;

                let result_name = if matches!(llvm_result_ty, Type::FPType(_)) {
                    lhs_operand = self.coerce_float_operand(
                        lhs_operand,
                        &llvm_result_ty,
                        &format!("fadd_{}_lhs", instr_id),
                    )?;
                    rhs_operand = self.coerce_float_operand(
                        rhs_operand,
                        &llvm_result_ty,
                        &format!("fadd_{}_rhs", instr_id),
                    )?;
                    self.llvm_ctx
                        .build_fadd(lhs_operand, rhs_operand, &format!("fadd_{}", lir_instr.id))
                        .map_err(fp_core::error::Error::from)?
                } else {
                    lhs_operand = self.coerce_integer_operand(
                        lhs_operand,
                        &llvm_result_ty,
                        false,
                        &format!("add_{}_lhs", instr_id),
                    )?;
                    rhs_operand = self.coerce_integer_operand(
                        rhs_operand,
                        &llvm_result_ty,
                        false,
                        &format!("add_{}_rhs", instr_id),
                    )?;
                    self.llvm_ctx
                        .build_add(lhs_operand, rhs_operand, &format!("add_{}", lir_instr.id))
                        .map_err(fp_core::error::Error::from)?
                };

                self.record_result(instr_id, Some(result_ty), result_name);
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
                let result_ty = self.infer_binary_result_type(ty_hint.clone(), &lhs, &rhs);
                let llvm_result_ty = self.convert_lir_type_to_llvm(result_ty.clone())?;
                let mut lhs_operand = self.convert_lir_value_to_operand(lhs.clone())?;
                let mut rhs_operand = self.convert_lir_value_to_operand(rhs.clone())?;
                let result_name = if matches!(llvm_result_ty, Type::FPType(_)) {
                    lhs_operand = self.coerce_float_operand(
                        lhs_operand,
                        &llvm_result_ty,
                        &format!("fsub_{}_lhs", instr_id),
                    )?;
                    rhs_operand = self.coerce_float_operand(
                        rhs_operand,
                        &llvm_result_ty,
                        &format!("fsub_{}_rhs", instr_id),
                    )?;
                    self.llvm_ctx
                        .build_fsub(lhs_operand, rhs_operand, &format!("fsub_{}", lir_instr.id))
                        .map_err(fp_core::error::Error::from)?
                } else {
                    lhs_operand = self.coerce_integer_operand(
                        lhs_operand,
                        &llvm_result_ty,
                        false,
                        &format!("sub_{}_lhs", instr_id),
                    )?;
                    rhs_operand = self.coerce_integer_operand(
                        rhs_operand,
                        &llvm_result_ty,
                        false,
                        &format!("sub_{}_rhs", instr_id),
                    )?;
                    self.llvm_ctx
                        .build_sub(lhs_operand, rhs_operand, &format!("sub_{}", lir_instr.id))
                        .map_err(fp_core::error::Error::from)?
                };
                self.record_result(instr_id, Some(result_ty), result_name);
            }
            lir::LirInstructionKind::Mul(lhs, rhs) => {
                let result_ty = self.infer_binary_result_type(ty_hint.clone(), &lhs, &rhs);
                let llvm_result_ty = self.convert_lir_type_to_llvm(result_ty.clone())?;
                let mut lhs_operand = self.convert_lir_value_to_operand(lhs.clone())?;
                let mut rhs_operand = self.convert_lir_value_to_operand(rhs.clone())?;
                let result_name = if matches!(llvm_result_ty, Type::FPType(_)) {
                    lhs_operand = self.coerce_float_operand(
                        lhs_operand,
                        &llvm_result_ty,
                        &format!("fmul_{}_lhs", instr_id),
                    )?;
                    rhs_operand = self.coerce_float_operand(
                        rhs_operand,
                        &llvm_result_ty,
                        &format!("fmul_{}_rhs", instr_id),
                    )?;
                    self.llvm_ctx
                        .build_fmul(lhs_operand, rhs_operand, &format!("fmul_{}", lir_instr.id))
                        .map_err(fp_core::error::Error::from)?
                } else {
                    lhs_operand = self.coerce_integer_operand(
                        lhs_operand,
                        &llvm_result_ty,
                        false,
                        &format!("mul_{}_lhs", instr_id),
                    )?;
                    rhs_operand = self.coerce_integer_operand(
                        rhs_operand,
                        &llvm_result_ty,
                        false,
                        &format!("mul_{}_rhs", instr_id),
                    )?;
                    self.llvm_ctx
                        .build_mul(lhs_operand, rhs_operand, &format!("mul_{}", lir_instr.id))
                        .map_err(fp_core::error::Error::from)?
                };
                self.record_result(instr_id, Some(result_ty), result_name);
            }
            lir::LirInstructionKind::Div(lhs, rhs) => {
                let result_ty = self.infer_binary_result_type(ty_hint.clone(), &lhs, &rhs);
                let llvm_result_ty = self.convert_lir_type_to_llvm(result_ty.clone())?;
                let mut lhs_operand = self.convert_lir_value_to_operand(lhs.clone())?;
                let mut rhs_operand = self.convert_lir_value_to_operand(rhs.clone())?;
                let result_name = if matches!(llvm_result_ty, Type::FPType(_)) {
                    lhs_operand = self.coerce_float_operand(
                        lhs_operand,
                        &llvm_result_ty,
                        &format!("fdiv_{}_lhs", instr_id),
                    )?;
                    rhs_operand = self.coerce_float_operand(
                        rhs_operand,
                        &llvm_result_ty,
                        &format!("fdiv_{}_rhs", instr_id),
                    )?;
                    self.llvm_ctx
                        .build_fdiv(lhs_operand, rhs_operand, &format!("fdiv_{}", lir_instr.id))
                        .map_err(fp_core::error::Error::from)?
                } else {
                    lhs_operand = self.coerce_integer_operand(
                        lhs_operand,
                        &llvm_result_ty,
                        false,
                        &format!("div_{}_lhs", instr_id),
                    )?;
                    rhs_operand = self.coerce_integer_operand(
                        rhs_operand,
                        &llvm_result_ty,
                        false,
                        &format!("div_{}_rhs", instr_id),
                    )?;
                    self.llvm_ctx
                        .build_udiv(lhs_operand, rhs_operand, &format!("div_{}", lir_instr.id))
                        .map_err(fp_core::error::Error::from)?
                };
                self.record_result(instr_id, Some(result_ty), result_name);
            }
            lir::LirInstructionKind::Rem(lhs, rhs) => {
                let result_ty = self.infer_binary_result_type(ty_hint.clone(), &lhs, &rhs);
                let llvm_result_ty = self.convert_lir_type_to_llvm(result_ty.clone())?;
                let mut lhs_operand = self.convert_lir_value_to_operand(lhs.clone())?;
                let mut rhs_operand = self.convert_lir_value_to_operand(rhs.clone())?;
                let result_name = if matches!(llvm_result_ty, Type::FPType(_)) {
                    lhs_operand = self.coerce_float_operand(
                        lhs_operand,
                        &llvm_result_ty,
                        &format!("frem_{}_lhs", instr_id),
                    )?;
                    rhs_operand = self.coerce_float_operand(
                        rhs_operand,
                        &llvm_result_ty,
                        &format!("frem_{}_rhs", instr_id),
                    )?;
                    self.llvm_ctx
                        .build_frem(lhs_operand, rhs_operand, &format!("frem_{}", instr_id))
                        .map_err(fp_core::error::Error::from)?
                } else {
                    lhs_operand = self.coerce_integer_operand(
                        lhs_operand,
                        &llvm_result_ty,
                        false,
                        &format!("rem_{}_lhs", instr_id),
                    )?;
                    rhs_operand = self.coerce_integer_operand(
                        rhs_operand,
                        &llvm_result_ty,
                        false,
                        &format!("rem_{}_rhs", instr_id),
                    )?;
                    let result_name = Name::Name(Box::new(format!("srem_{}", instr_id)));
                    let instruction = Instruction::SRem(SRem {
                        operand0: lhs_operand,
                        operand1: rhs_operand,
                        dest: result_name.clone(),
                        debugloc: None,
                    });
                    self.llvm_ctx
                        .add_instruction(instruction)
                        .map_err(fp_core::error::Error::from)?;
                    result_name
                };
                self.record_result(instr_id, Some(result_ty), result_name);
            }
            lir::LirInstructionKind::And(lhs, rhs) => {
                let result_ty = self.infer_binary_result_type(ty_hint.clone(), &lhs, &rhs);
                let llvm_result_ty = self.convert_lir_type_to_llvm(result_ty.clone())?;
                let mut lhs_operand = self.convert_lir_value_to_operand(lhs.clone())?;
                let mut rhs_operand = self.convert_lir_value_to_operand(rhs.clone())?;
                if matches!(llvm_result_ty, Type::IntegerType { .. }) {
                    lhs_operand = self.coerce_integer_operand(
                        lhs_operand,
                        &llvm_result_ty,
                        false,
                        &format!("and_{}_lhs", instr_id),
                    )?;
                    rhs_operand = self.coerce_integer_operand(
                        rhs_operand,
                        &llvm_result_ty,
                        false,
                        &format!("and_{}_rhs", instr_id),
                    )?;
                }
                let result_name = Name::Name(Box::new(format!("and_{}", instr_id)));
                let instruction = Instruction::And(And {
                    operand0: lhs_operand,
                    operand1: rhs_operand,
                    dest: result_name.clone(),
                    debugloc: None,
                });
                self.llvm_ctx
                    .add_instruction(instruction)
                    .map_err(fp_core::error::Error::from)?;
                self.record_result(instr_id, Some(result_ty), result_name);
            }
            lir::LirInstructionKind::Or(lhs, rhs) => {
                let result_ty = self.infer_binary_result_type(ty_hint.clone(), &lhs, &rhs);
                let llvm_result_ty = self.convert_lir_type_to_llvm(result_ty.clone())?;
                let mut lhs_operand = self.convert_lir_value_to_operand(lhs.clone())?;
                let mut rhs_operand = self.convert_lir_value_to_operand(rhs.clone())?;
                if matches!(llvm_result_ty, Type::IntegerType { .. }) {
                    lhs_operand = self.coerce_integer_operand(
                        lhs_operand,
                        &llvm_result_ty,
                        false,
                        &format!("or_{}_lhs", instr_id),
                    )?;
                    rhs_operand = self.coerce_integer_operand(
                        rhs_operand,
                        &llvm_result_ty,
                        false,
                        &format!("or_{}_rhs", instr_id),
                    )?;
                }
                let result_name = Name::Name(Box::new(format!("or_{}", instr_id)));
                let instruction = Instruction::Or(Or {
                    operand0: lhs_operand,
                    operand1: rhs_operand,
                    dest: result_name.clone(),
                    debugloc: None,
                });
                self.llvm_ctx
                    .add_instruction(instruction)
                    .map_err(fp_core::error::Error::from)?;
                self.record_result(instr_id, Some(result_ty), result_name);
            }
            lir::LirInstructionKind::Xor(lhs, rhs) => {
                let result_ty = self.infer_binary_result_type(ty_hint.clone(), &lhs, &rhs);
                let llvm_result_ty = self.convert_lir_type_to_llvm(result_ty.clone())?;
                let mut lhs_operand = self.convert_lir_value_to_operand(lhs.clone())?;
                let mut rhs_operand = self.convert_lir_value_to_operand(rhs.clone())?;
                if matches!(llvm_result_ty, Type::IntegerType { .. }) {
                    lhs_operand = self.coerce_integer_operand(
                        lhs_operand,
                        &llvm_result_ty,
                        false,
                        &format!("xor_{}_lhs", instr_id),
                    )?;
                    rhs_operand = self.coerce_integer_operand(
                        rhs_operand,
                        &llvm_result_ty,
                        false,
                        &format!("xor_{}_rhs", instr_id),
                    )?;
                }
                let result_name = Name::Name(Box::new(format!("xor_{}", instr_id)));
                let instruction = Instruction::Xor(Xor {
                    operand0: lhs_operand,
                    operand1: rhs_operand,
                    dest: result_name.clone(),
                    debugloc: None,
                });
                self.llvm_ctx
                    .add_instruction(instruction)
                    .map_err(fp_core::error::Error::from)?;
                self.record_result(instr_id, Some(result_ty), result_name);
            }
            lir::LirInstructionKind::Shl(lhs, rhs) => {
                let result_ty = self.infer_binary_result_type(ty_hint.clone(), &lhs, &rhs);
                let llvm_result_ty = self.convert_lir_type_to_llvm(result_ty.clone())?;
                let mut lhs_operand = self.convert_lir_value_to_operand(lhs.clone())?;
                let mut rhs_operand = self.convert_lir_value_to_operand(rhs.clone())?;
                if matches!(llvm_result_ty, Type::IntegerType { .. }) {
                    lhs_operand = self.coerce_integer_operand(
                        lhs_operand,
                        &llvm_result_ty,
                        false,
                        &format!("shl_{}_lhs", instr_id),
                    )?;
                    rhs_operand = self.coerce_integer_operand(
                        rhs_operand,
                        &llvm_result_ty,
                        false,
                        &format!("shl_{}_rhs", instr_id),
                    )?;
                }
                let result_name = Name::Name(Box::new(format!("shl_{}", instr_id)));
                let instruction = Instruction::Shl(Shl {
                    operand0: lhs_operand,
                    operand1: rhs_operand,
                    dest: result_name.clone(),
                    nuw: false,
                    nsw: false,
                    debugloc: None,
                });
                self.llvm_ctx
                    .add_instruction(instruction)
                    .map_err(fp_core::error::Error::from)?;
                self.record_result(instr_id, Some(result_ty), result_name);
            }
            lir::LirInstructionKind::Shr(lhs, rhs) => {
                let result_ty = self.infer_binary_result_type(ty_hint.clone(), &lhs, &rhs);
                let llvm_result_ty = self.convert_lir_type_to_llvm(result_ty.clone())?;
                let mut lhs_operand = self.convert_lir_value_to_operand(lhs.clone())?;
                let mut rhs_operand = self.convert_lir_value_to_operand(rhs.clone())?;
                if matches!(llvm_result_ty, Type::IntegerType { .. }) {
                    lhs_operand = self.coerce_integer_operand(
                        lhs_operand,
                        &llvm_result_ty,
                        false,
                        &format!("lshr_{}_lhs", instr_id),
                    )?;
                    rhs_operand = self.coerce_integer_operand(
                        rhs_operand,
                        &llvm_result_ty,
                        false,
                        &format!("lshr_{}_rhs", instr_id),
                    )?;
                }
                let result_name = Name::Name(Box::new(format!("lshr_{}", instr_id)));
                let instruction = Instruction::LShr(LShr {
                    operand0: lhs_operand,
                    operand1: rhs_operand,
                    dest: result_name.clone(),
                    exact: false,
                    debugloc: None,
                });
                self.llvm_ctx
                    .add_instruction(instruction)
                    .map_err(fp_core::error::Error::from)?;
                self.record_result(instr_id, Some(result_ty), result_name);
            }
            lir::LirInstructionKind::Not(value) => {
                let operand = self.convert_lir_value_to_operand(value.clone())?;
                let result_ty = ty_hint
                    .clone()
                    .or_else(|| self.lir_type_from_value(&value))
                    .unwrap_or(lir::LirType::I1);

                let invert_const = match &result_ty {
                    lir::LirType::I1 => lir::LirConstant::Bool(true),
                    lir::LirType::I8 => lir::LirConstant::Int(-1, lir::LirType::I8),
                    lir::LirType::I16 => lir::LirConstant::Int(-1, lir::LirType::I16),
                    lir::LirType::I32 => lir::LirConstant::Int(-1, lir::LirType::I32),
                    lir::LirType::I64 => lir::LirConstant::Int(-1, lir::LirType::I64),
                    lir::LirType::I128 => lir::LirConstant::Int(-1, lir::LirType::I128),
                    _ => lir::LirConstant::Int(-1, lir::LirType::I64),
                };
                let invert_const_ref = self.convert_lir_constant_to_llvm_mut(invert_const)?;
                let const_operand = self.llvm_ctx.operand_from_constant(invert_const_ref);
                let result_name = Name::Name(Box::new(format!("not_{}", instr_id)));
                let instruction = Instruction::Xor(Xor {
                    operand0: operand,
                    operand1: const_operand,
                    dest: result_name.clone(),
                    debugloc: None,
                });
                self.llvm_ctx
                    .add_instruction(instruction)
                    .map_err(fp_core::error::Error::from)?;
                self.record_result(instr_id, Some(result_ty), result_name);
            }
            lir::LirInstructionKind::Load {
                address,
                alignment,
                volatile,
            } => {
                let address_operand = self.convert_lir_value_to_operand(address)?;
                let mut loaded_lir_type = ty_hint.clone().unwrap_or(lir::LirType::I32);
                if matches!(loaded_lir_type, lir::LirType::Void) {
                    loaded_lir_type = lir::LirType::Ptr(Box::new(lir::LirType::I8));
                }
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
            lir::LirInstructionKind::InsertValue {
                aggregate,
                element,
                indices,
            } => {
                let aggregate_operand = self.convert_lir_value_to_operand(aggregate.clone())?;
                let mut element_operand = self.convert_lir_value_to_operand(element.clone())?;

                // If we have a type hint for the aggregate, try to coerce element to the field type
                if let Some(lir_ty) = ty_hint.clone() {
                    if let Ok(llvm_agg_ty) = self.convert_lir_type_to_llvm(lir_ty.clone()) {
                        if let (Some(&field_index), Type::StructType { element_types, .. }) =
                            (indices.get(0), &llvm_agg_ty)
                        {
                            if let Some(expected_ty_ref) = element_types.get(field_index as usize) {
                                // Determine current operand type
                                let current_ty = match &element_operand {
                                    Operand::LocalOperand { ty, .. } => Some(ty.clone()),
                                    Operand::ConstantOperand(c) => {
                                        Some(c.get_type(&self.llvm_ctx.module.types))
                                    }
                                    _ => None,
                                };
                                if let Some(cur) = current_ty {
                                    if &cur != expected_ty_ref {
                                        // Try a few coercions
                                        let expected = expected_ty_ref.as_ref().clone();
                                        let coerced_name = match (cur.as_ref(), &expected) {
                                            (
                                                Type::IntegerType { .. },
                                                Type::PointerType { .. },
                                            ) => self.llvm_ctx.build_inttoptr(
                                                element_operand.clone(),
                                                expected.clone(),
                                                &format!("itop_{}", instr_id),
                                            )?,
                                            (
                                                Type::PointerType { .. },
                                                Type::IntegerType { .. },
                                            ) => self.llvm_ctx.build_ptrtoint(
                                                element_operand.clone(),
                                                expected.clone(),
                                                &format!("ptoi_{}", instr_id),
                                            )?,
                                            (_, _) => self.llvm_ctx.build_bitcast(
                                                element_operand.clone(),
                                                expected.clone(),
                                                &format!("bitcast_{}", instr_id),
                                            )?,
                                        };
                                        element_operand = self.llvm_ctx.operand_from_name_and_type(
                                            coerced_name,
                                            expected_ty_ref.as_ref(),
                                        );
                                    }
                                }
                            }
                        }
                    }
                }
                let result_name = self
                    .llvm_ctx
                    .build_insert_value(
                        aggregate_operand,
                        element_operand,
                        indices.clone(),
                        &format!("insertvalue_{}", instr_id),
                    )
                    .map_err(fp_core::error::Error::from)?;
                self.record_result(instr_id, ty_hint.clone(), result_name);
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
            lir::LirInstructionKind::SExt(value, target_ty) => {
                let operand = self.convert_lir_value_to_operand(value)?;
                let llvm_target_ty = self.convert_lir_type_to_llvm(target_ty.clone())?;
                let result_name = self
                    .llvm_ctx
                    .build_sext(operand, llvm_target_ty, &format!("sext_{}", instr_id))
                    .map_err(fp_core::error::Error::from)?;
                self.record_result(instr_id, Some(target_ty), result_name);
            }
            lir::LirInstructionKind::SextOrTrunc(value, target_ty) => {
                if let lir::LirValue::Constant(_) = value {
                    if let lir::LirValue::Constant(constant) =
                        Self::convert_constant_to_type(value.clone(), &target_ty)
                    {
                        self.constant_results.insert(instr_id, constant);
                        return Ok(());
                    }
                }

                let operand = self.convert_lir_value_to_operand(value.clone())?;
                let llvm_target_ty = self.convert_lir_type_to_llvm(target_ty.clone())?;
                let source_ty = self
                    .lir_value_type(&value)
                    .or_else(|| self.lir_value_type(&lir::LirValue::Register(instr_id)))
                    .unwrap_or(target_ty.clone());

                let src_bits = Self::int_type_bits(&source_ty);
                let dst_bits = Self::int_type_bits(&target_ty);

                let result_name = match (src_bits, dst_bits, operand.clone()) {
                    (Some(src), Some(dst), _) if dst > src => self
                        .llvm_ctx
                        .build_sext(
                            operand,
                            llvm_target_ty.clone(),
                            &format!("sext_trunc_{}", instr_id),
                        )
                        .map_err(fp_core::error::Error::from)?,
                    (Some(src), Some(dst), _) if dst < src => self
                        .llvm_ctx
                        .build_trunc(
                            operand,
                            llvm_target_ty.clone(),
                            &format!("sext_trunc_{}", instr_id),
                        )
                        .map_err(fp_core::error::Error::from)?,
                    _ => {
                        if let Operand::LocalOperand { name, .. } = operand {
                            self.record_result(instr_id, Some(target_ty), name.clone());
                            return Ok(());
                        } else {
                            self.llvm_ctx
                                .build_bitcast(
                                    operand,
                                    llvm_target_ty.clone(),
                                    &format!("sext_trunc_{}", instr_id),
                                )
                                .map_err(fp_core::error::Error::from)?
                        }
                    }
                };

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

        self.lower_user_call(
            instr_id,
            ty_hint,
            function,
            args,
            calling_convention,
            tail_call,
        )
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
                report_error_with_context(
                    LOG_AREA,
                    format!(
                        "Unable to find LLVM function record for '{}'",
                        function_name
                    ),
                )
            })?
            .clone();

        let mut param_iter = func.parameters.into_iter();
        for local in locals {
            if local.is_argument {
                let param = param_iter.next().ok_or_else(|| {
                    report_error_with_context(
                        LOG_AREA,
                        format!(
                            "Not enough LLVM parameters to map local argument {} in '{}'",
                            local.id, function_name
                        ),
                    )
                })?;

                let operand = Operand::LocalOperand {
                    name: param.name.clone(),
                    ty: param.ty.clone(),
                };
                self.argument_operands.insert(local.id, operand);

                // Also add to value_map so lir_value_type can find it
                self.value_map
                    .insert(local.id, (param.name, local.ty.clone()));
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
            lir::LirValue::Local(local_id) | lir::LirValue::Register(local_id) => {
                // Indirect call through function pointer
                let operand = self.convert_lir_value_to_operand(function.clone())?;
                (format!("indirect_call_{}", local_id), Some(operand))
            }
            other => {
                return Err(report_error_with_context(
                    LOG_AREA,
                    format!("Unsupported call target in LLVM lowering: {:?}", other),
                ));
            }
        };

        let llvm_symbol = self.llvm_symbol_for(&function_name);

        let signature = if callee_operand.is_some() {
            // For indirect calls, try to extract signature from function pointer type
            if let Some(fn_ptr_ty) = self.lir_value_type(&function) {
                // Function pointers in LIR are represented as Ptr(Function(...))
                let inner_ty = match &fn_ptr_ty {
                    lir::Ty::Ptr(inner) => inner.as_ref(),
                    other => other,
                };

                if let lir::Ty::Function {
                    return_type,
                    param_types,
                    is_variadic,
                } = inner_ty
                {
                    Some(lir::LirFunctionSignature {
                        params: param_types.clone(),
                        return_type: (**return_type).clone(),
                        is_variadic: *is_variadic,
                    })
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            self.function_signatures.get(&function_name).cloned()
        };

        let return_lir_type = if let Some(sig) = &signature {
            sig.return_type.clone()
        } else if let Some(hint) = &ty_hint {
            hint.clone()
        } else {
            lir::LirType::Void
        };

        let mut return_type_ref = self.to_type_ref(return_lir_type.clone())?;

        let param_lir_types = if let Some(sig) = signature.clone() {
            sig.params
        } else {
            let mut inferred = Vec::with_capacity(args.len());
            for (index, arg) in args.iter().enumerate() {
                let ty = self.lir_value_type(arg).ok_or_else(|| {
                    report_error_with_context(
                        LOG_AREA,
                        format!(
                            "Unable to determine type for argument {} in call to '{}'",
                            index, function_name
                        ),
                    )
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

        let mut is_variadic = signature.as_ref().map(|s| s.is_variadic).unwrap_or(false);

        let is_defined = self.defined_functions.contains(&llvm_symbol);
        let current_symbol = self.current_function.clone();
        let is_current_function = current_symbol.as_deref() == Some(&llvm_symbol);
        let is_runtime_intrinsic = CRuntimeIntrinsics::is_runtime_intrinsic(&function_name);

        let needs_stub = callee_operand.is_none()
            && !is_defined
            && !is_current_function
            && !self
                .llvm_ctx
                .module
                .functions
                .iter()
                .any(|f| f.name == llvm_symbol)
            && !is_runtime_intrinsic;

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

        if is_runtime_intrinsic {
            if let Some(runtime_decl) =
                CRuntimeIntrinsics::get_intrinsic_decl(&function_name, &self.llvm_ctx.module.types)
            {
                let already_present = self
                    .llvm_ctx
                    .module
                    .functions
                    .iter()
                    .any(|f| f.name == runtime_decl.name);

                if !already_present {
                    self.llvm_ctx.module.functions.push(runtime_decl.clone());
                }

                return_type_ref = runtime_decl.return_type.clone();
                param_type_refs = runtime_decl
                    .parameters
                    .iter()
                    .map(|param| param.ty.clone())
                    .collect();
                is_variadic = runtime_decl.is_var_arg;
            }
        }

        let fn_ty = self.llvm_ctx.module.types.func_type(
            return_type_ref.clone(),
            param_type_refs.clone(),
            is_variadic,
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

    fn int_type_bits(ty: &lir::LirType) -> Option<u32> {
        match ty {
            lir::LirType::I1 => Some(1),
            lir::LirType::I8 => Some(8),
            lir::LirType::I16 => Some(16),
            lir::LirType::I32 => Some(32),
            lir::LirType::I64 => Some(64),
            lir::LirType::I128 => Some(128),
            _ => None,
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
            lir::LirValue::Local(local_id) => {
                self.value_map.get(local_id).map(|(_, ty)| ty.clone())
            }
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

        let is_valid_start = result
            .chars()
            .next()
            .map(|c| matches!(c, 'a'..='z' | 'A'..='Z' | '_' | '$'))
            .unwrap_or(false);
        if !is_valid_start {
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
                return Err(report_error_with_context(
                    LOG_AREA,
                    format!("Unimplemented LIR terminator: {:?}", term),
                ));
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
                    Err(report_error_with_context(
                        LOG_AREA,
                        format!("Unknown register {} encountered during codegen", reg_id),
                    ))
                }
            }
            lir::LirValue::Constant(constant) => {
                let llvm_constant = self.convert_lir_constant_to_llvm_mut(constant)?;
                Ok(self.llvm_ctx.operand_from_constant(llvm_constant))
            }
            lir::LirValue::Global(name, ty) => {
                if self.function_signatures.contains_key(&name) {
                    return self.convert_lir_value_to_operand(lir::LirValue::Function(name));
                }

                if let Some(lir_constant) = self.global_const_map.get(&name) {
                    let llvm_constant = self.convert_lir_constant_to_llvm(lir_constant.clone())?;
                    tracing::debug!("LLVM: Found global '{}' in const map, using value", name);
                    return Ok(self.llvm_ctx.operand_from_constant(llvm_constant));
                }

                if self.allow_unresolved_globals {
                    tracing::warn!(
                        "LLVM: synthesizing placeholder null for unresolved global '{}'",
                        name
                    );
                    let llvm_ty = self
                        .convert_lir_type_to_llvm(ty.clone())
                        .unwrap_or(Type::PointerType { addr_space: 0 });
                    let ty_ref = self.llvm_ctx.module.types.get_for_type(&llvm_ty);
                    let null_constant = ConstantRef::new(Constant::Null(ty_ref));
                    return Ok(self.llvm_ctx.operand_from_constant(null_constant));
                }

                Err(report_error_with_context(
                    LOG_AREA,
                    format!("Global variable '{}' of type {:?} not found", name, ty),
                ))
            }
            lir::LirValue::Function(name) => {
                let llvm_name = self.llvm_symbol_for(&name);

                let _fn_ty = if let Some(signature) = self.function_signatures.get(&name) {
                    self.function_type_from_signature(signature.clone())?
                } else if let Some(existing) = self.llvm_ctx.get_function(&llvm_name) {
                    self.llvm_ctx.module.types.func_type(
                        existing.return_type.clone(),
                        existing.parameters.iter().map(|p| p.ty.clone()).collect(),
                        existing.is_var_arg,
                    )
                } else if let Some(runtime_decl) =
                    CRuntimeIntrinsics::get_intrinsic_decl(&name, &self.llvm_ctx.module.types)
                {
                    let already_present = self
                        .llvm_ctx
                        .module
                        .functions
                        .iter()
                        .any(|f| f.name == runtime_decl.name);

                    if !already_present {
                        self.llvm_ctx.module.functions.push(runtime_decl.clone());
                    }

                    self.llvm_ctx.module.types.func_type(
                        runtime_decl.return_type.clone(),
                        runtime_decl
                            .parameters
                            .iter()
                            .map(|p| p.ty.clone())
                            .collect(),
                        runtime_decl.is_var_arg,
                    )
                } else {
                    return Err(report_error_with_context(
                        LOG_AREA,
                        format!(
                            "Unknown function reference '{}' encountered during codegen",
                            name
                        ),
                    ));
                };

                let ptr_ty = Type::PointerType { addr_space: 0 };
                let ptr_ty_ref = self.llvm_ctx.module.types.get_for_type(&ptr_ty);

                Ok(Operand::ConstantOperand(ConstantRef::new(
                    Constant::GlobalReference {
                        name: Name::Name(Box::new(llvm_name)),
                        ty: ptr_ty_ref,
                    },
                )))
            }
            lir::LirValue::Local(local_id) => {
                if let Some(op) = self.argument_operands.get(&local_id) {
                    return Ok(op.clone());
                }

                Err(report_error_with_context(
                    LOG_AREA,
                    format!("Unknown local: {}", local_id),
                ))
            }
            lir::LirValue::StackSlot(slot_id) => {
                // For now, treat stack slots like registers
                if let Some((name, lir_ty)) = self.value_map.get(&slot_id) {
                    let llvm_ty = self.convert_lir_type_to_llvm(lir_ty.clone())?;
                    Ok(self
                        .llvm_ctx
                        .operand_from_name_and_type(name.clone(), &llvm_ty))
                } else {
                    Err(report_error_with_context(
                        LOG_AREA,
                        format!("Unknown stack slot: {}", slot_id),
                    ))
                }
            }
            lir::LirValue::Undef(ty) => {
                let llvm_ty = self.convert_lir_type_to_llvm(ty.clone())?;
                let ty_ref = self.llvm_ctx.module.types.get_for_type(&llvm_ty);
                Ok(self
                    .llvm_ctx
                    .operand_from_constant(ConstantRef::new(Constant::Undef(ty_ref))))
            }
            lir::LirValue::Null(ty) => {
                let llvm_ty = self.convert_lir_type_to_llvm(ty.clone())?;
                let constant = match &llvm_ty {
                    Type::PointerType { .. }
                    | Type::StructType { .. }
                    | Type::ArrayType { .. }
                    | Type::VectorType { .. } => {
                        let ty_ref = self.llvm_ctx.module.types.get_for_type(&llvm_ty);
                        ConstantRef::new(Constant::Null(ty_ref))
                    }
                    Type::IntegerType { bits } => ConstantRef::new(Constant::Int {
                        bits: *bits,
                        value: 0,
                    }),
                    Type::FPType(fp_ty) => {
                        let float_const = match fp_ty {
                            FPType::Half => Float::Half,
                            FPType::Single => Float::Single(0.0),
                            FPType::Double => Float::Double(0.0),
                            FPType::FP128 => Float::Quadruple,
                            FPType::X86_FP80 => Float::X86_FP80,
                            FPType::PPC_FP128 => Float::PPC_FP128,
                            _ => Float::Double(0.0),
                        };
                        ConstantRef::new(Constant::Float(float_const))
                    }
                    _ => {
                        let ty_ref = self.llvm_ctx.module.types.get_for_type(&llvm_ty);
                        ConstantRef::new(Constant::Null(ty_ref))
                    }
                };
                Ok(self.llvm_ctx.operand_from_constant(constant))
            }
        }
    }

    /// Convert LIR constant to LLVM constant
    fn convert_lir_constant_to_llvm(&mut self, lir_const: lir::LirConstant) -> Result<ConstantRef> {
        match lir_const {
            lir::LirConstant::Int(value, ty) => {
                let bits = self.int_bit_width(&ty).unwrap_or(32);
                let encoded = if bits == 64 {
                    value as u64
                } else if bits >= 64 {
                    value as u64
                } else if bits == 0 {
                    0
                } else {
                    let mask = (1u64 << bits) - 1;
                    ((value as i128) & mask as i128) as u64
                };
                Ok(ConstantRef::new(Constant::Int {
                    bits,
                    value: encoded,
                }))
            }
            lir::LirConstant::UInt(value, ty) => {
                let bits = self.int_bit_width(&ty).unwrap_or(32);
                let encoded = if bits >= 64 {
                    value
                } else if bits == 0 {
                    0
                } else {
                    value & ((1u64 << bits) - 1)
                };
                Ok(ConstantRef::new(Constant::Int {
                    bits,
                    value: encoded,
                }))
            }
            lir::LirConstant::Float(value, ty) => match ty {
                lir::LirType::F32 => Ok(self.llvm_ctx.const_f32(value as f32)),
                lir::LirType::F64 => Ok(self.llvm_ctx.const_f64(value)),
                other => Err(report_error_with_context(
                    LOG_AREA,
                    format!(
                        "Unsupported floating-point type {:?} for LLVM constant",
                        other
                    ),
                )),
            },
            lir::LirConstant::Bool(value) => Ok(self.llvm_ctx.const_bool(value)),
            lir::LirConstant::Struct(values, ty) => {
                let is_packed = match &ty {
                    lir::LirType::Struct { packed, .. } => *packed,
                    _ => false,
                };
                let name = match &ty {
                    lir::LirType::Struct { name, .. } => name.clone(),
                    _ => None,
                };
                let mut llvm_values = Vec::with_capacity(values.len());
                for value in values {
                    llvm_values.push(self.convert_lir_constant_to_llvm(value)?);
                }
                Ok(ConstantRef::new(Constant::Struct {
                    name,
                    values: llvm_values,
                    is_packed,
                }))
            }
            lir::LirConstant::Array(elements, elem_ty) => {
                let llvm_elem_ty = self.convert_lir_type_to_llvm(elem_ty.clone())?;
                let elem_ref = self.llvm_ctx.module.types.get_for_type(&llvm_elem_ty);
                let mut llvm_elements = Vec::with_capacity(elements.len());
                for element in elements {
                    llvm_elements.push(self.convert_lir_constant_to_llvm(element)?);
                }
                Ok(ConstantRef::new(Constant::Array {
                    element_type: elem_ref,
                    elements: llvm_elements,
                }))
            }
            lir::LirConstant::Null(ty) => {
                let llvm_ty = self.convert_lir_type_to_llvm(ty.clone())?;
                let ty_ref = self.llvm_ctx.module.types.get_for_type(&llvm_ty);
                Ok(ConstantRef::new(Constant::Null(ty_ref)))
            }
            lir::LirConstant::Undef(ty) => {
                let llvm_ty = self.convert_lir_type_to_llvm(ty.clone())?;
                let ty_ref = self.llvm_ctx.module.types.get_for_type(&llvm_ty);
                Ok(ConstantRef::new(Constant::Undef(ty_ref)))
            }
            lir::LirConstant::String(s) => Ok(self.get_or_create_string_ptr(&s)),
        }
    }

    fn int_bit_width(&self, ty: &lir::LirType) -> Option<u32> {
        match ty {
            lir::LirType::I1 => Some(1),
            lir::LirType::I8 => Some(8),
            lir::LirType::I16 => Some(16),
            lir::LirType::I32 => Some(32),
            lir::LirType::I64 => Some(64),
            lir::LirType::I128 => Some(128),
            _ => None,
        }
    }

    // convert_lir_constant_to_llvm_mut is now identical to convert_lir_constant_to_llvm
    // and retained for source compatibility.
    fn convert_lir_constant_to_llvm_mut(
        &mut self,
        lir_const: lir::LirConstant,
    ) -> Result<ConstantRef> {
        self.convert_lir_constant_to_llvm(lir_const)
    }

    fn infer_binary_result_type(
        &self,
        ty_hint: Option<lir::LirType>,
        lhs: &lir::LirValue,
        rhs: &lir::LirValue,
    ) -> lir::LirType {
        ty_hint
            .or_else(|| self.lir_type_from_value(lhs))
            .or_else(|| self.lir_type_from_value(rhs))
            .unwrap_or(lir::LirType::I32)
    }

    fn coerce_integer_operand(
        &mut self,
        operand: Operand,
        target_type: &Type,
        signed: bool,
        tag: &str,
    ) -> Result<Operand> {
        let target_bits = match target_type {
            Type::IntegerType { bits } => *bits,
            _ => return Ok(operand),
        };

        let operand_ty_ref = operand.get_type(&self.llvm_ctx.module.types);
        let operand_ty = operand_ty_ref.as_ref();
        let operand_bits = match operand_ty {
            Type::IntegerType { bits } => *bits,
            _ => return Ok(operand),
        };

        if operand_bits == target_bits {
            return Ok(operand);
        }

        let target_type_clone = Type::IntegerType { bits: target_bits };
        let name = if operand_bits < target_bits {
            if signed {
                self.llvm_ctx
                    .build_sext(operand, target_type_clone.clone(), &format!("{}_sext", tag))
                    .map_err(fp_core::error::Error::from)?
            } else {
                self.llvm_ctx
                    .build_zext(operand, target_type_clone.clone(), &format!("{}_zext", tag))
                    .map_err(fp_core::error::Error::from)?
            }
        } else {
            self.llvm_ctx
                .build_trunc(
                    operand,
                    target_type_clone.clone(),
                    &format!("{}_trunc", tag),
                )
                .map_err(fp_core::error::Error::from)?
        };

        Ok(Operand::LocalOperand {
            name,
            ty: self.llvm_ctx.module.types.get_for_type(&target_type_clone),
        })
    }

    fn coerce_float_operand(
        &mut self,
        operand: Operand,
        target_type: &Type,
        _tag: &str,
    ) -> Result<Operand> {
        let Type::FPType(_) = target_type else {
            return Ok(operand);
        };

        let target_ref = self.llvm_ctx.module.types.get_for_type(target_type);
        let operand_ty_ref = operand.get_type(&self.llvm_ctx.module.types);

        if operand_ty_ref == target_ref {
            return Ok(operand);
        }

        match operand {
            Operand::LocalOperand { name, .. } => Ok(Operand::LocalOperand {
                name,
                ty: target_ref,
            }),
            // Constants already carry their own type metadata; leave unchanged for now.
            _ => Ok(operand),
        }
    }

    fn lir_type_from_value(&self, value: &lir::LirValue) -> Option<lir::LirType> {
        match value {
            lir::LirValue::Register(id) => self.value_map.get(id).map(|(_, ty)| ty.clone()),
            lir::LirValue::Constant(c) => Some(Self::lir_type_from_constant(c)),
            lir::LirValue::Global(_, ty) => Some(ty.clone()),
            lir::LirValue::Undef(ty) => Some(ty.clone()),
            lir::LirValue::Null(ty) => Some(ty.clone()),
            _ => None,
        }
    }

    fn lir_type_from_constant(constant: &lir::LirConstant) -> lir::LirType {
        match constant {
            lir::LirConstant::Int(_, ty) => ty.clone(),
            lir::LirConstant::UInt(_, ty) => ty.clone(),
            lir::LirConstant::Float(_, ty) => ty.clone(),
            lir::LirConstant::Bool(_) => lir::LirType::I1,
            lir::LirConstant::String(_) => lir::LirType::Ptr(Box::new(lir::LirType::I8)),
            lir::LirConstant::Array(elements, elem_ty) => {
                lir::LirType::Array(Box::new(elem_ty.clone()), elements.len() as u64)
            }
            lir::LirConstant::Struct(_, ty) => ty.clone(),
            lir::LirConstant::Null(ty) => ty.clone(),
            lir::LirConstant::Undef(ty) => ty.clone(),
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
                    return Err(report_error_with_context(
                        LOG_AREA,
                        format!("Unsupported integer bit width {} for LLVM constant", other),
                    ))
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
            Constant::GlobalReference { ty, .. } => ty.clone(),
            other => {
                return Err(report_error_with_context(
                    LOG_AREA,
                    format!("Unsupported LLVM constant for type inference: {:?}", other),
                ))
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
        let gv_name = Name::Name(Box::new(format!(
            ".str.{}.{}",
            self.symbol_prefix, self.next_string_id
        )));
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

    fn sanitize_symbol_component(input: &str) -> String {
        let mut sanitized = String::with_capacity(input.len());
        for ch in input.chars() {
            if ch.is_ascii_alphanumeric() {
                sanitized.push(ch);
            } else {
                sanitized.push('_');
            }
        }
        if sanitized.is_empty() {
            sanitized.push_str("module");
        }
        sanitized
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
            lir::LirType::Struct { fields, packed, .. } => {
                let mut element_types = Vec::with_capacity(fields.len());
                for field in fields {
                    let field_ty = self.convert_lir_type_to_llvm(field)?;
                    let field_ref = self.llvm_ctx.module.types.get_for_type(&field_ty);
                    element_types.push(field_ref);
                }
                Ok(Type::StructType {
                    element_types,
                    is_packed: packed,
                })
            }
            lir::LirType::Function {
                return_type,
                param_types,
                is_variadic,
            } => {
                let result_type = self.convert_lir_type_to_llvm(*return_type)?;
                let result_ref = self.llvm_ctx.module.types.get_for_type(&result_type);
                let mut params = Vec::with_capacity(param_types.len());
                for param in param_types {
                    let param_ty = self.convert_lir_type_to_llvm(param)?;
                    params.push(self.llvm_ctx.module.types.get_for_type(&param_ty));
                }
                Ok(Type::FuncType {
                    result_type: result_ref,
                    param_types: params,
                    is_var_arg: is_variadic,
                })
            }
            lir::LirType::Error => {
                // Error placeholder type - use i64 as a safe default
                Ok(self.llvm_ctx.i64_type())
            }
            other => {
                return Err(report_error_with_context(
                    LOG_AREA,
                    format!("unsupported LIR type in LLVM lowering: {:?}", other),
                ))
            }
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
