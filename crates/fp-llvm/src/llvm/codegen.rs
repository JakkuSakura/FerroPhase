use crate::context::LlvmContext;
use crate::intrinsics::{CRuntimeIntrinsics, IntrinsicSignature};
use fp_core::diagnostics::report_error_with_context;
use fp_core::{
    error::{Error, Result},
    lir,
};
use fp_core::tracing::debug;
use inkwell::builder::BuilderError;
use inkwell::llvm_sys::core::LLVMConstArray2;
use inkwell::llvm_sys::LLVMCallConv;
use inkwell::types::{AsTypeRef, BasicType, BasicTypeEnum, FloatType, FunctionType, IntType};
use inkwell::values::{
    AggregateValueEnum, AsValueRef, BasicMetadataValueEnum, BasicValue, BasicValueEnum,
    FloatValue, FunctionValue, IntValue, PointerValue, ValueKind,
};
use inkwell::{AddressSpace, FloatPredicate, GlobalVisibility, IntPredicate};
use std::collections::{HashMap, HashSet};

const LOG_AREA: &str = "[lir→llvm]";

/// LLVM code generator that transforms LIR to LLVM IR.
pub struct LirCodegen<'a> {
    llvm_ctx: &'a mut LlvmContext,
    register_map: HashMap<u32, (BasicValueEnum<'static>, lir::LirType)>,
    local_map: HashMap<u32, (BasicValueEnum<'static>, lir::LirType)>,
    stack_slot_map: HashMap<u32, (PointerValue<'static>, lir::LirType)>,
    block_map: HashMap<u32, inkwell::basic_block::BasicBlock<'static>>,
    current_function: Option<FunctionValue<'static>>,
    string_globals: HashMap<String, PointerValue<'static>>,
    next_string_id: u32,
    symbol_prefix: String,
    referenced_globals: HashSet<String>,
    global_const_map: HashMap<String, lir::LirConstant>,
    constant_results: HashMap<u32, lir::LirConstant>,
    current_return_type: Option<lir::LirType>,
    function_signatures: HashMap<String, lir::LirFunctionSignature>,
    symbol_names: HashMap<String, String>,
    defined_functions: HashSet<String>,
    argument_operands: HashMap<u32, BasicValueEnum<'static>>,
    allow_unresolved_globals: bool,
}

enum Callee {
    Direct(FunctionValue<'static>),
    Indirect(PointerValue<'static>, FunctionType<'static>),
}

impl<'a> LirCodegen<'a> {
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

    /// Create a new LIR code generator.
    pub fn new(
        llvm_ctx: &'a mut LlvmContext,
        global_const_map: HashMap<String, lir::LirConstant>,
        allow_unresolved_globals: bool,
    ) -> Self {
        let prefix = Self::sanitize_symbol_component(&llvm_ctx.module.get_name().to_str().unwrap_or("module"));
        Self {
            llvm_ctx,
            register_map: HashMap::new(),
            local_map: HashMap::new(),
            stack_slot_map: HashMap::new(),
            block_map: HashMap::new(),
            current_function: None,
            string_globals: HashMap::new(),
            next_string_id: 0,
            symbol_prefix: prefix,
            referenced_globals: HashSet::new(),
            global_const_map,
            constant_results: HashMap::new(),
            current_return_type: None,
            function_signatures: HashMap::new(),
            symbol_names: HashMap::new(),
            defined_functions: HashSet::new(),
            argument_operands: HashMap::new(),
            allow_unresolved_globals,
        }
    }

    fn record_result(&mut self, instr_id: u32, ty_hint: Option<lir::LirType>, value: BasicValueEnum<'static>) {
        let ty = ty_hint.unwrap_or(lir::LirType::I32);
        self.register_map.insert(instr_id, (value, ty));
    }

    /// Get the set of globals that were referenced but not found during codegen.
    pub fn get_referenced_globals(&self) -> &HashSet<String> {
        &self.referenced_globals
    }

    /// Generate LLVM IR for a LIR program.
    pub fn generate_program(&mut self, lir_program: lir::LirProgram) -> Result<()> {
        let num_globals = lir_program.globals.len();
        let num_functions = lir_program.functions.len();
        debug!(
            "LLVM: Generating program with {} functions, {} globals",
            num_functions, num_globals
        );

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

        // Generate globals first.
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

        // Generate functions.
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
        Ok(())
    }

    fn generate_global(&mut self, global: lir::LirGlobal) -> Result<()> {
        let llvm_name = self.llvm_symbol_for(&global.name);
        let global_ty = self.llvm_basic_type(&global.ty)?;
        let gvar = self
            .llvm_ctx
            .module
            .add_global(global_ty, Some(AddressSpace::default()), &llvm_name);

        let linkage = self.convert_linkage(global.linkage);
        gvar.set_linkage(linkage);
        let visibility = if matches!(linkage, inkwell::module::Linkage::Internal) {
            GlobalVisibility::Default
        } else {
            self.convert_visibility(global.visibility)
        };
        gvar.set_visibility(visibility);
        gvar.set_constant(global.is_constant);
        gvar.set_alignment(global.alignment.unwrap_or(0));
        gvar.set_section(global.section.as_deref());

        if let Some(init) = global.initializer {
            let value = self.convert_lir_constant_to_value(init)?;
            gvar.set_initializer(&value);
        }

        Ok(())
    }

    fn generate_function(&mut self, lir_func: lir::LirFunction) -> Result<()> {
        let llvm_name = self.llvm_symbol_for(&lir_func.name);
        let mut signature = lir_func.signature.clone();
        let is_main = llvm_name == "main";
        let linkage = self.convert_linkage(lir_func.linkage.clone());
        if is_main && matches!(signature.return_type, lir::LirType::Void) {
            signature.return_type = lir::LirType::I32;
        }
        let fn_type = self.function_type_from_signature(signature.clone())?;
        // Reuse any pre-declared function to avoid LLVM auto-renaming (e.g. `foo` -> `foo.1`).
        // Calls can emit a declaration before we define the body; if we re-add here, the linker
        // will look for the unsuffixed symbol and fail.
        let function = if let Some(existing) = self.llvm_ctx.module.get_function(&llvm_name) {
            existing
        } else {
            self.llvm_ctx.module.add_function(
                &llvm_name,
                fn_type,
                Some(if is_main {
                    inkwell::module::Linkage::External
                } else {
                    linkage
                }),
            )
        };

        function.set_call_conventions(self.convert_calling_convention(&lir_func.calling_convention));
        if is_main {
            function.set_linkage(inkwell::module::Linkage::External);
        } else {
            function.set_linkage(linkage);
        }
        self.defined_functions.insert(llvm_name.clone());
        self.current_function = Some(function);
        self.llvm_ctx.current_function = Some(function);
        self.current_return_type = Some(signature.return_type);

        self.register_map.clear();
        self.local_map.clear();
        self.stack_slot_map.clear();
        self.block_map.clear();
        self.constant_results.clear();
        self.argument_operands.clear();

        self.populate_argument_operands(function, &lir_func.locals)?;

        for block in &lir_func.basic_blocks {
            let block_name = block
                .label
                .as_ref()
                .map(|n| n.to_string())
                .unwrap_or_else(|| format!("bb{}", block.id));
            let bb = self
                .llvm_ctx
                .context
                .append_basic_block(function, &block_name);
            self.block_map.insert(block.id, bb);
        }

        for block in lir_func.basic_blocks.into_iter() {
            self.generate_basic_block(block)?;
        }

        self.current_return_type = None;
        Ok(())
    }

    fn generate_basic_block(&mut self, lir_block: lir::LirBasicBlock) -> Result<()> {
        let bb = self.block_map.get(&lir_block.id).ok_or_else(|| {
            report_error_with_context(
                LOG_AREA,
                format!("Missing basic block for id {}", lir_block.id),
            )
        })?;

        self.llvm_ctx.builder.position_at_end(*bb);

        for instruction in lir_block.instructions.into_iter() {
            self.generate_instruction(instruction)?;
        }

        self.generate_terminator(lir_block.terminator)?;
        Ok(())
    }

    fn generate_instruction(&mut self, lir_instr: lir::LirInstruction) -> Result<()> {
        let instr_id = lir_instr.id;
        let ty_hint = lir_instr.type_hint.clone();

        match lir_instr.kind {
            lir::LirInstructionKind::Add(lhs, rhs) => {
                let result = self.lower_binary_op(
                    instr_id,
                    ty_hint.clone(),
                    lhs,
                    rhs,
                    |builder, l, r, name| builder.build_int_add(l, r, name),
                    |builder, l, r, name| builder.build_float_add(l, r, name),
                )?;
                self.record_result(instr_id, ty_hint, result);
            }
            lir::LirInstructionKind::Sub(lhs, rhs) => {
                let result = self.lower_binary_op(
                    instr_id,
                    ty_hint.clone(),
                    lhs,
                    rhs,
                    |builder, l, r, name| builder.build_int_sub(l, r, name),
                    |builder, l, r, name| builder.build_float_sub(l, r, name),
                )?;
                self.record_result(instr_id, ty_hint, result);
            }
            lir::LirInstructionKind::Mul(lhs, rhs) => {
                let result = self.lower_binary_op(
                    instr_id,
                    ty_hint.clone(),
                    lhs,
                    rhs,
                    |builder, l, r, name| builder.build_int_mul(l, r, name),
                    |builder, l, r, name| builder.build_float_mul(l, r, name),
                )?;
                self.record_result(instr_id, ty_hint, result);
            }
            lir::LirInstructionKind::Div(lhs, rhs) => {
                let result = self.lower_binary_op(
                    instr_id,
                    ty_hint.clone(),
                    lhs,
                    rhs,
                    |builder, l, r, name| builder.build_int_unsigned_div(l, r, name),
                    |builder, l, r, name| builder.build_float_div(l, r, name),
                )?;
                self.record_result(instr_id, ty_hint, result);
            }
            lir::LirInstructionKind::Rem(lhs, rhs) => {
                let result = self.lower_binary_op(
                    instr_id,
                    ty_hint.clone(),
                    lhs,
                    rhs,
                    |builder, l, r, name| builder.build_int_signed_rem(l, r, name),
                    |builder, l, r, name| builder.build_float_rem(l, r, name),
                )?;
                self.record_result(instr_id, ty_hint, result);
            }
            lir::LirInstructionKind::And(lhs, rhs) => {
                let result = self.lower_int_binary_op(instr_id, lhs, rhs, |builder, l, r, name| {
                    builder.build_and(l, r, name)
                })?;
                self.record_result(instr_id, ty_hint, result);
            }
            lir::LirInstructionKind::Or(lhs, rhs) => {
                let result = self.lower_int_binary_op(instr_id, lhs, rhs, |builder, l, r, name| {
                    builder.build_or(l, r, name)
                })?;
                self.record_result(instr_id, ty_hint, result);
            }
            lir::LirInstructionKind::Xor(lhs, rhs) => {
                let result = self.lower_int_binary_op(instr_id, lhs, rhs, |builder, l, r, name| {
                    builder.build_xor(l, r, name)
                })?;
                self.record_result(instr_id, ty_hint, result);
            }
            lir::LirInstructionKind::Shl(lhs, rhs) => {
                let result = self.lower_int_binary_op(instr_id, lhs, rhs, |builder, l, r, name| {
                    builder.build_left_shift(l, r, name)
                })?;
                self.record_result(instr_id, ty_hint, result);
            }
            lir::LirInstructionKind::Shr(lhs, rhs) => {
                let result = self.lower_int_binary_op(instr_id, lhs, rhs, |builder, l, r, name| {
                    builder.build_right_shift(l, r, false, name)
                })?;
                self.record_result(instr_id, ty_hint, result);
            }
            lir::LirInstructionKind::Not(value) => {
                let operand = self.convert_lir_value_to_basic_value(value)?;
                let int_value = self.coerce_to_int(operand, self.default_int_type())?;
                let mask = self.default_int_type().const_all_ones();
                let result = self
                    .llvm_ctx
                    .builder
                    .build_xor(int_value, mask, &format!("not_{}", instr_id))
                    .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
                self.record_result(instr_id, ty_hint, result.into());
            }
            lir::LirInstructionKind::Eq(lhs, rhs) => {
                self.lower_cmp(instr_id, lhs, rhs, IntPredicate::EQ, FloatPredicate::OEQ)?;
            }
            lir::LirInstructionKind::Ne(lhs, rhs) => {
                self.lower_cmp(instr_id, lhs, rhs, IntPredicate::NE, FloatPredicate::ONE)?;
            }
            lir::LirInstructionKind::Lt(lhs, rhs) => {
                self.lower_cmp(instr_id, lhs, rhs, IntPredicate::SLT, FloatPredicate::OLT)?;
            }
            lir::LirInstructionKind::Le(lhs, rhs) => {
                self.lower_cmp(instr_id, lhs, rhs, IntPredicate::SLE, FloatPredicate::OLE)?;
            }
            lir::LirInstructionKind::Gt(lhs, rhs) => {
                self.lower_cmp(instr_id, lhs, rhs, IntPredicate::SGT, FloatPredicate::OGT)?;
            }
            lir::LirInstructionKind::Ge(lhs, rhs) => {
                self.lower_cmp(instr_id, lhs, rhs, IntPredicate::SGE, FloatPredicate::OGE)?;
            }
            lir::LirInstructionKind::Load {
                address,
                alignment,
                volatile,
            } => {
                let address_value = self.convert_lir_value_to_basic_value(address)?;
                let ptr_value = self.coerce_to_pointer(address_value)?;
                let loaded_lir_type = ty_hint.clone().unwrap_or(lir::LirType::I32);
                let pointee_ty = self.llvm_basic_type(&loaded_lir_type)?;
                let load = self
                    .llvm_ctx
                    .builder
                    .build_load(pointee_ty, ptr_value, &format!("load_{}", instr_id))
                    .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
                if volatile {
                    tracing::debug!("volatile load requested for load_{}", instr_id);
                }
                if alignment.is_some() {
                    tracing::debug!("alignment requested for load_{}", instr_id);
                }
                self.record_result(instr_id, Some(loaded_lir_type), load);
            }
            lir::LirInstructionKind::Store {
                value,
                address,
                alignment,
                volatile,
            } => {
                let value = self.convert_lir_value_to_basic_value(value)?;
                let address_value = self.convert_lir_value_to_basic_value(address)?;
                let ptr_value = self.coerce_to_pointer(address_value)?;
                let store = self
                    .llvm_ctx
                    .builder
                    .build_store(ptr_value, value)
                    .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
                let _ = store.set_volatile(volatile);
                if let Some(align) = alignment {
                    store.set_alignment(align).map_err(|e| fp_core::error::Error::from(e.to_string()))?;
                }
            }
            lir::LirInstructionKind::Alloca { size, alignment: _alignment } => {
                let element_lir_type = match ty_hint.clone() {
                    Some(lir::LirType::Ptr(inner)) => *inner,
                    _ => lir::LirType::I8,
                };
                let llvm_element_type = self.llvm_basic_type(&element_lir_type)?;
                let saved_block = self.llvm_ctx.builder.get_insert_block();
                let entry_block = self
                    .current_function
                    .and_then(|func| func.get_first_basic_block())
                    .ok_or_else(|| report_error_with_context(LOG_AREA, "missing entry block"))?;
                if let Some(first_inst) = entry_block.get_first_instruction() {
                    self.llvm_ctx.builder.position_at(entry_block, &first_inst);
                } else {
                    self.llvm_ctx.builder.position_at_end(entry_block);
                }
                let alloca = self
                    .llvm_ctx
                    .builder
                    .build_alloca(llvm_element_type, &format!("alloca_{}", instr_id))
                    .map_err(|e| fp_core::error::Error::from(e.to_string()))?;

                if let Some(size_value) = self.try_coerce_to_int_value(size.clone())? {
                    let count = size_value;
                    let array_alloca = self
                        .llvm_ctx
                        .builder
                        .build_array_alloca(llvm_element_type, count, &format!("alloca_count_{}", instr_id))
                        .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
                    if let Some(block) = saved_block {
                        self.llvm_ctx.builder.position_at_end(block);
                    }
                    self.record_result(instr_id, ty_hint, array_alloca.into());
                } else {
                    if let Some(block) = saved_block {
                        self.llvm_ctx.builder.position_at_end(block);
                    }
                    self.record_result(instr_id, ty_hint, alloca.into());
                }
            }
            lir::LirInstructionKind::Bitcast(value, target_ty) => {
                let operand = self.convert_lir_value_to_basic_value(value)?;
                let target = self.llvm_basic_type(&target_ty)?;
                let result = self
                    .llvm_ctx
                    .builder
                    .build_bit_cast(operand, target, &format!("bitcast_{}", instr_id))
                    .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
                self.record_result(instr_id, Some(target_ty), result);
            }
            lir::LirInstructionKind::IntToPtr(value) => {
                let operand = self.convert_lir_value_to_basic_value(value)?;
                let target_ty = ty_hint
                    .clone()
                    .unwrap_or(lir::LirType::Ptr(Box::new(lir::LirType::I8)));
                let ptr_ty = self.llvm_ctx.ptr_type();
                let int_value = self.coerce_to_int(operand, self.default_int_type())?;
                let result = self
                    .llvm_ctx
                    .builder
                    .build_int_to_ptr(int_value, ptr_ty, &format!("inttoptr_{}", instr_id))
                    .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
                self.record_result(instr_id, Some(target_ty), result.into());
            }
            lir::LirInstructionKind::PtrToInt(value) => {
                let operand = self.convert_lir_value_to_basic_value(value)?;
                let target_ty = ty_hint.clone().unwrap_or(lir::LirType::I64);
                let int_ty = self.llvm_int_type(&target_ty)?;
                let ptr_value = self.coerce_to_pointer(operand)?;
                let result = self
                    .llvm_ctx
                    .builder
                    .build_ptr_to_int(ptr_value, int_ty, &format!("ptrtoint_{}", instr_id))
                    .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
                self.record_result(instr_id, Some(target_ty), result.into());
            }
            lir::LirInstructionKind::FPToSI(value, target_ty) => {
                let operand = self.convert_lir_value_to_basic_value(value)?;
                let float_val =
                    self.coerce_to_float(operand, self.llvm_float_type(&lir::LirType::F64)?)?;
                let int_ty = self.llvm_int_type(&target_ty)?;
                let result = self
                    .llvm_ctx
                    .builder
                    .build_float_to_signed_int(float_val, int_ty, &format!("fptosi_{}", instr_id))
                    .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
                self.record_result(instr_id, Some(target_ty), result.into());
            }
            lir::LirInstructionKind::FPToUI(value, target_ty) => {
                let operand = self.convert_lir_value_to_basic_value(value)?;
                let float_val =
                    self.coerce_to_float(operand, self.llvm_float_type(&lir::LirType::F64)?)?;
                let int_ty = self.llvm_int_type(&target_ty)?;
                let result = self
                    .llvm_ctx
                    .builder
                    .build_float_to_unsigned_int(float_val, int_ty, &format!("fptoui_{}", instr_id))
                    .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
                self.record_result(instr_id, Some(target_ty), result.into());
            }
            lir::LirInstructionKind::SIToFP(value, target_ty) => {
                let operand = self.convert_lir_value_to_basic_value(value)?;
                let int_val = self.coerce_to_int_signed(operand, self.default_int_type())?;
                let float_ty = self.llvm_float_type(&target_ty)?;
                let result = self
                    .llvm_ctx
                    .builder
                    .build_signed_int_to_float(int_val, float_ty, &format!("sitofp_{}", instr_id))
                    .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
                self.record_result(instr_id, Some(target_ty), result.into());
            }
            lir::LirInstructionKind::UIToFP(value, target_ty) => {
                let operand = self.convert_lir_value_to_basic_value(value)?;
                let int_val = self.coerce_to_int(operand, self.default_int_type())?;
                let float_ty = self.llvm_float_type(&target_ty)?;
                let result = self
                    .llvm_ctx
                    .builder
                    .build_unsigned_int_to_float(int_val, float_ty, &format!("uitofp_{}", instr_id))
                    .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
                self.record_result(instr_id, Some(target_ty), result.into());
            }
            lir::LirInstructionKind::FPExt(value, target_ty)
            | lir::LirInstructionKind::FPTrunc(value, target_ty) => {
                let operand = self.convert_lir_value_to_basic_value(value)?;
                let float_ty = self.llvm_float_type(&target_ty)?;
                let float_val = self.coerce_to_float(operand, float_ty)?;
                self.record_result(instr_id, Some(target_ty), float_val.into());
            }
            lir::LirInstructionKind::ZExt(value, target_ty) => {
                let operand = self.convert_lir_value_to_basic_value(value)?;
                let int_target = self.llvm_int_type(&target_ty)?;
                let int_value = self.coerce_to_int(operand, int_target)?;
                self.record_result(instr_id, Some(target_ty), int_value.into());
            }
            lir::LirInstructionKind::SExt(value, target_ty) => {
                let operand = self.convert_lir_value_to_basic_value(value)?;
                let int_target = self.llvm_int_type(&target_ty)?;
                let int_value = self.coerce_to_int_signed(operand, int_target)?;
                self.record_result(instr_id, Some(target_ty), int_value.into());
            }
            lir::LirInstructionKind::Trunc(value, target_ty) => {
                let operand = self.convert_lir_value_to_basic_value(value)?;
                let int_target = self.llvm_int_type(&target_ty)?;
                let int_value = self.coerce_to_int(operand, int_target)?;
                self.record_result(instr_id, Some(target_ty), int_value.into());
            }
            lir::LirInstructionKind::SextOrTrunc(value, target_ty) => {
                let operand = self.convert_lir_value_to_basic_value(value)?;
                let int_target = self.llvm_int_type(&target_ty)?;
                let int_value = self.coerce_to_int_signed(operand, int_target)?;
                self.record_result(instr_id, Some(target_ty), int_value.into());
            }
            lir::LirInstructionKind::GetElementPtr {
                ptr,
                indices,
                inbounds,
            } => {
                let base = self.convert_lir_value_to_basic_value(ptr)?;
                let ptr_value = self.coerce_to_pointer(base)?;
                let element_ty = ty_hint
                    .clone()
                    .and_then(|ty| match ty {
                        lir::LirType::Ptr(inner) => Some(*inner),
                        _ => None,
                    })
                    .unwrap_or(lir::LirType::I8);
                let llvm_element_ty = self.llvm_basic_type(&element_ty)?;

                let mut llvm_indices = Vec::with_capacity(indices.len());
                for index in indices {
                    let value = self.convert_lir_value_to_basic_value(index)?;
                    let int_value = self.coerce_to_int(value, self.default_int_type())?;
                    llvm_indices.push(int_value);
                }

                let gep = unsafe {
                    if inbounds {
                        self.llvm_ctx
                            .builder
                            .build_in_bounds_gep(
                                llvm_element_ty,
                                ptr_value,
                                &llvm_indices,
                                &format!("gep_{}", instr_id),
                            )
                    } else {
                        self.llvm_ctx
                            .builder
                            .build_gep(
                                llvm_element_ty,
                                ptr_value,
                                &llvm_indices,
                                &format!("gep_{}", instr_id),
                            )
                    }
                }
                .map_err(|e| fp_core::error::Error::from(e.to_string()))?;

                self.record_result(
                    instr_id,
                    ty_hint.or_else(|| Some(lir::LirType::Ptr(Box::new(lir::LirType::I8)))),
                    gep.into(),
                );
            }
            lir::LirInstructionKind::InsertValue {
                aggregate,
                element,
                indices,
            } => {
                let aggregate_value = self.convert_lir_value_to_basic_value(aggregate)?;
                let element_value = self.convert_lir_value_to_basic_value(element)?;

                let index = indices
                    .get(0)
                    .copied()
                    .ok_or_else(|| report_error_with_context(LOG_AREA, "InsertValue missing index"))?;

                let result = match aggregate_value {
                    BasicValueEnum::ArrayValue(array) => self
                        .llvm_ctx
                        .builder
                        .build_insert_value(array, element_value, index, &format!("insertvalue_{}", instr_id))
                        .map_err(|e| fp_core::error::Error::from(e.to_string()))?,
                    BasicValueEnum::StructValue(strct) => self
                        .llvm_ctx
                        .builder
                        .build_insert_value(strct, element_value, index, &format!("insertvalue_{}", instr_id))
                        .map_err(|e| fp_core::error::Error::from(e.to_string()))?,
                    _ => {
                        return Err(report_error_with_context(
                            LOG_AREA,
                            "InsertValue requires aggregate operand",
                        ))
                    }
                };

                let result_value = match result {
                    AggregateValueEnum::ArrayValue(value) => value.into(),
                    AggregateValueEnum::StructValue(value) => value.into(),
                };

                self.record_result(instr_id, ty_hint, result_value);
            }
            lir::LirInstructionKind::Call {
                function,
                args,
                calling_convention,
                tail_call,
            } => {
                let call_site = self.lower_call_instruction(
                    instr_id,
                    ty_hint.clone(),
                    function,
                    args,
                    calling_convention,
                    tail_call,
                )?;

                if let ValueKind::Basic(result) = call_site.try_as_basic_value() {
                    if let Some(hint) = ty_hint {
                        self.record_result(instr_id, Some(hint), result);
                    }
                }
            }
            lir::LirInstructionKind::IntrinsicCall { kind, format, args } => {
                let mut call_args = Vec::with_capacity(args.len() + 1);
                call_args.push(lir::LirValue::Constant(lir::LirConstant::String(
                    format.clone(),
                )));
                call_args.extend(args.into_iter());

                let call_site = match kind {
                    lir::LirIntrinsicKind::Print | lir::LirIntrinsicKind::Println => {
                        self.lower_call_instruction(
                            instr_id,
                            ty_hint.clone(),
                            lir::LirValue::Function("printf".to_string()),
                            call_args,
                            lir::CallingConvention::C,
                            false,
                        )?
                    }
                };

                if let ValueKind::Basic(result) = call_site.try_as_basic_value() {
                    if let Some(hint) = ty_hint {
                        self.record_result(instr_id, Some(hint), result);
                    }
                }
            }
            lir::LirInstructionKind::Unreachable => {
                self.llvm_ctx
                    .builder
                    .build_unreachable()
                    .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
            }
            _ => {
                // Unimplemented instruction kinds are currently ignored to keep codegen resilient.
            }
        }

        Ok(())
    }

    fn generate_terminator(&mut self, lir_term: lir::LirTerminator) -> Result<()> {
        match lir_term {
            lir::LirTerminator::Return(value) => {
                let mut return_value = match value {
                    Some(val) => Some(self.convert_lir_value_to_basic_value(val)?),
                    None => None,
                };

                if return_value.is_none() {
                    if let Some(ref lir_ty) = self.current_return_type {
                        if !matches!(lir_ty, lir::LirType::Void) {
                            return_value = Some(self.zero_value_for_type(lir_ty)?);
                        }
                    }
                }

                let return_ref = return_value
                    .as_ref()
                    .map(|value| value as &dyn BasicValue);
                self.llvm_ctx
                    .builder
                    .build_return(return_ref)
                    .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
            }
            lir::LirTerminator::Br(target) => {
                let target_bb = self.block_map.get(&target).ok_or_else(|| {
                    report_error_with_context(LOG_AREA, format!("Unknown branch target {}", target))
                })?;
                self.llvm_ctx
                    .builder
                    .build_unconditional_branch(*target_bb)
                    .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
            }
            lir::LirTerminator::CondBr {
                condition,
                if_true,
                if_false,
            } => {
                let condition_value = self.convert_lir_value_to_basic_value(condition)?;
                let bool_value = self.cast_condition_to_bool(condition_value)?;

                let true_bb = self.block_map.get(&if_true).ok_or_else(|| {
                    report_error_with_context(LOG_AREA, format!("Unknown branch target {}", if_true))
                })?;
                let false_bb = self.block_map.get(&if_false).ok_or_else(|| {
                    report_error_with_context(LOG_AREA, format!("Unknown branch target {}", if_false))
                })?;

                self.llvm_ctx
                    .builder
                    .build_conditional_branch(bool_value, *true_bb, *false_bb)
                    .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
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

    fn lower_call_instruction(
        &mut self,
        instr_id: u32,
        ty_hint: Option<lir::LirType>,
        function: lir::LirValue,
        args: Vec<lir::LirValue>,
        calling_convention: lir::CallingConvention,
        tail_call: bool,
    ) -> Result<inkwell::values::CallSiteValue<'static>> {
        let callee = self.resolve_callee(&function)?;

        let mut call_args: Vec<BasicMetadataValueEnum> = Vec::with_capacity(args.len());
        for arg in args {
            let value = self.convert_lir_value_to_basic_value(arg)?;
            call_args.push(value.into());
        }

        let call = match callee {
            Callee::Direct(func) => self
                .llvm_ctx
                .builder
                .build_call(func, &call_args, &format!("call_{}", instr_id))
                .map_err(|e| fp_core::error::Error::from(e.to_string()))?,
            Callee::Indirect(ptr, fn_ty) => self
                .llvm_ctx
                .builder
                .build_indirect_call(fn_ty, ptr, &call_args, &format!("call_{}", instr_id))
                .map_err(|e| fp_core::error::Error::from(e.to_string()))?,
        };

        call.set_tail_call(tail_call);
        call.set_call_convention(self.convert_calling_convention(&calling_convention));

        if let Some(hint) = ty_hint {
            if matches!(hint, lir::LirType::Void) {
                call.set_tail_call(tail_call);
            }
        }

        Ok(call)
    }

    fn resolve_callee(&mut self, function: &lir::LirValue) -> Result<Callee> {
        match function {
            lir::LirValue::Function(name) | lir::LirValue::Global(name, _) => {
                let llvm_name = self.llvm_symbol_for(name);
                if let Some(func) = self.llvm_ctx.module.get_function(&llvm_name) {
                    return Ok(Callee::Direct(func));
                }

                if let Some(signature) = self.function_signatures.get(name).cloned() {
                    let fn_type = self.function_type_from_signature(signature)?;
                    let func = self
                        .llvm_ctx
                        .module
                        .add_function(&llvm_name, fn_type, Some(inkwell::module::Linkage::External));
                    return Ok(Callee::Direct(func));
                }

                if let Some(signature) = CRuntimeIntrinsics::get_intrinsic_signature(name) {
                    let IntrinsicSignature {
                        name,
                        params,
                        return_type,
                        is_variadic,
                    } = signature;
                    let fn_type = self.function_type_from_signature(lir::LirFunctionSignature {
                        params,
                        return_type,
                        is_variadic,
                    })?;
                    let func = self
                        .llvm_ctx
                        .module
                        .add_function(name, fn_type, Some(inkwell::module::Linkage::External));
                    return Ok(Callee::Direct(func));
                }

                Err(report_error_with_context(
                    LOG_AREA,
                    format!("Unknown function reference '{}' encountered during codegen", name),
                ))
            }
            lir::LirValue::Local(local_id) | lir::LirValue::Register(local_id) => {
                let (value, lir_ty) = self
                    .register_map
                    .get(local_id)
                    .map(|(val, ty)| (*val, ty.clone()))
                    .or_else(|| self.local_map.get(local_id).map(|(val, ty)| (*val, ty.clone())))
                    .ok_or_else(|| {
                        report_error_with_context(
                            LOG_AREA,
                            format!("Unknown call target register/local {}", local_id),
                        )
                    })?;

                let ptr = self.coerce_to_pointer(value)?;
                let fn_ty = self
                    .function_type_from_lir_type(&lir_ty)
                    .ok_or_else(|| {
                        report_error_with_context(
                            LOG_AREA,
                            format!(
                                "Value {} is not a callable function pointer (type={:?})",
                                local_id, lir_ty
                            ),
                        )
                    })?;
                Ok(Callee::Indirect(ptr, fn_ty))
            }
            other => Err(report_error_with_context(
                LOG_AREA,
                format!("Unsupported call target in LLVM lowering: {:?}", other),
            )),
        }
    }

    fn populate_argument_operands(
        &mut self,
        function: FunctionValue<'static>,
        locals: &[lir::LirLocal],
    ) -> Result<()> {
        let params = function.get_params();
        let mut param_iter = params.into_iter();

        for local in locals {
            if local.is_argument {
                let param = param_iter.next().ok_or_else(|| {
                    report_error_with_context(
                        LOG_AREA,
                        format!(
                            "Not enough LLVM parameters to map local argument {} in '{}'",
                            local.id,
                            function.get_name().to_str().unwrap_or("<fn>")
                        ),
                    )
                })?;

                self.argument_operands.insert(local.id, param);
                self.local_map.insert(local.id, (param, local.ty.clone()));
            }
        }

        Ok(())
    }

    fn lower_binary_op<FI, FF>(
        &mut self,
        instr_id: u32,
        ty_hint: Option<lir::LirType>,
        lhs: lir::LirValue,
        rhs: lir::LirValue,
        int_op: FI,
        float_op: FF,
    ) -> Result<BasicValueEnum<'static>>
    where
        FI: FnOnce(
            &inkwell::builder::Builder<'static>,
            IntValue<'static>,
            IntValue<'static>,
            &str,
        ) -> std::result::Result<IntValue<'static>, BuilderError>,
        FF: FnOnce(
            &inkwell::builder::Builder<'static>,
            FloatValue<'static>,
            FloatValue<'static>,
            &str,
        ) -> std::result::Result<FloatValue<'static>, BuilderError>,
    {
        let result_ty = self.infer_binary_result_type(ty_hint.clone(), &lhs, &rhs);
        let lhs_value = self.convert_lir_value_to_basic_value(lhs)?;
        let rhs_value = self.convert_lir_value_to_basic_value(rhs)?;

        if result_ty.is_float() {
            let float_ty = self.llvm_float_type(&result_ty)?;
            let lhs_float = self.coerce_to_float(lhs_value, float_ty)?;
            let rhs_float = self.coerce_to_float(rhs_value, float_ty)?;
            let result = float_op(&self.llvm_ctx.builder, lhs_float, rhs_float, &format!("fop_{}", instr_id))
                .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
            Ok(result.into())
        } else {
            let int_ty = self.default_int_type();
            let lhs_int = self.coerce_to_int(lhs_value, int_ty)?;
            let rhs_int = self.coerce_to_int(rhs_value, int_ty)?;
            let result = int_op(&self.llvm_ctx.builder, lhs_int, rhs_int, &format!("iop_{}", instr_id))
                .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
            Ok(result.into())
        }
    }

    fn lower_int_binary_op<F>(
        &mut self,
        instr_id: u32,
        lhs: lir::LirValue,
        rhs: lir::LirValue,
        op: F,
    ) -> Result<BasicValueEnum<'static>>
    where
        F: FnOnce(
            &inkwell::builder::Builder<'static>,
            IntValue<'static>,
            IntValue<'static>,
            &str,
        ) -> std::result::Result<IntValue<'static>, BuilderError>,
    {
        let lhs_value = self.convert_lir_value_to_basic_value(lhs)?;
        let rhs_value = self.convert_lir_value_to_basic_value(rhs)?;
        let int_ty = self.default_int_type();
        let lhs_int = self.coerce_to_int(lhs_value, int_ty)?;
        let rhs_int = self.coerce_to_int(rhs_value, int_ty)?;
        let result = op(&self.llvm_ctx.builder, lhs_int, rhs_int, &format!("iop_{}", instr_id))
            .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
        Ok(result.into())
    }

    fn lower_cmp(
        &mut self,
        instr_id: u32,
        lhs: lir::LirValue,
        rhs: lir::LirValue,
        int_pred: IntPredicate,
        float_pred: FloatPredicate,
    ) -> Result<()> {
        let lhs_value = self.convert_lir_value_to_basic_value(lhs)?;
        let rhs_value = self.convert_lir_value_to_basic_value(rhs)?;

        let is_float = matches!(lhs_value, BasicValueEnum::FloatValue(_))
            || matches!(rhs_value, BasicValueEnum::FloatValue(_));

        let cmp_value = if is_float {
            let float_ty = self.llvm_float_type(&lir::LirType::F64)?;
            let lhs_float = self.coerce_to_float(lhs_value, float_ty)?;
            let rhs_float = self.coerce_to_float(rhs_value, float_ty)?;
            self.llvm_ctx
                .builder
                .build_float_compare(float_pred, lhs_float, rhs_float, &format!("fcmp_{}", instr_id))
                .map_err(|e| fp_core::error::Error::from(e.to_string()))?
        } else {
            let int_ty = self.default_int_type();
            let lhs_int = self.coerce_to_int(lhs_value, int_ty)?;
            let rhs_int = self.coerce_to_int(rhs_value, int_ty)?;
            self.llvm_ctx
                .builder
                .build_int_compare(int_pred, lhs_int, rhs_int, &format!("icmp_{}", instr_id))
                .map_err(|e| fp_core::error::Error::from(e.to_string()))?
        };

        self.record_result(instr_id, Some(lir::LirType::I1), cmp_value.into());
        Ok(())
    }

    fn cast_condition_to_bool(
        &mut self,
        value: BasicValueEnum<'static>,
    ) -> Result<IntValue<'static>> {
        match value {
            BasicValueEnum::IntValue(int_val) => {
                if int_val.get_type().get_bit_width() == 1 {
                    return Ok(int_val);
                }
                let zero = int_val.get_type().const_zero();
                self.llvm_ctx
                    .builder
                    .build_int_compare(IntPredicate::NE, int_val, zero, "cond_bool")
                    .map_err(|e| fp_core::error::Error::from(e.to_string()))
            }
            BasicValueEnum::FloatValue(float_val) => {
                let zero = float_val.get_type().const_float(0.0);
                self.llvm_ctx
                    .builder
                    .build_float_compare(FloatPredicate::ONE, float_val, zero, "cond_bool")
                    .map_err(|e| fp_core::error::Error::from(e.to_string()))
            }
            BasicValueEnum::PointerValue(ptr_val) => {
                self.llvm_ctx
                    .builder
                    .build_is_not_null(ptr_val, "cond_bool")
                    .map_err(|e| fp_core::error::Error::from(e.to_string()))
            }
            _ => Err(report_error_with_context(
                LOG_AREA,
                "Unsupported condition type for branch",
            )),
        }
    }

    fn convert_lir_value_to_basic_value(
        &mut self,
        lir_value: lir::LirValue,
    ) -> Result<BasicValueEnum<'static>> {
        match lir_value {
            lir::LirValue::Register(reg_id) => {
                if let Some(constant) = self.constant_results.get(&reg_id) {
                    return self.convert_lir_constant_to_value(constant.clone());
                }
                self.register_map
                    .get(&reg_id)
                    .map(|(value, _)| *value)
                    .ok_or_else(|| {
                        report_error_with_context(
                            LOG_AREA,
                            format!("Unknown register {} encountered during codegen", reg_id),
                        )
                    })
            }
            lir::LirValue::Constant(constant) => self.convert_lir_constant_to_value(constant),
            lir::LirValue::Global(name, ty) => {
                let llvm_name = self.llvm_symbol_for(&name);
                if let Some(signature) = self.function_signatures.get(&name) {
                    let fn_type = self.function_type_from_signature(signature.clone())?;
                    let fn_value = self
                        .llvm_ctx
                        .module
                        .get_function(&llvm_name)
                        .unwrap_or_else(|| {
                            self.llvm_ctx
                                .module
                                .add_function(
                                    &llvm_name,
                                    fn_type,
                                    Some(inkwell::module::Linkage::External),
                                )
                        });
                    return Ok(fn_value.as_global_value().as_pointer_value().into());
                }

                if let Some(lir_constant) = self.global_const_map.get(&name) {
                    let llvm_constant = self.convert_lir_constant_to_value(lir_constant.clone())?;
                    tracing::debug!("LLVM: Found global '{}' in const map, using value", name);
                    return Ok(llvm_constant);
                }

                if let Some(global) = self.llvm_ctx.module.get_global(&llvm_name) {
                    return Ok(global.as_pointer_value().into());
                }

                self.referenced_globals.insert(name.clone());

                if self.allow_unresolved_globals {
                    tracing::warn!(
                        "LLVM: synthesizing placeholder null for unresolved global '{}'",
                        name
                    );
                    let ptr = self.llvm_ctx.ptr_type().const_null();
                    return Ok(ptr.into());
                }

                Err(report_error_with_context(
                    LOG_AREA,
                    format!("Global variable '{}' of type {:?} not found", name, ty),
                ))
            }
            lir::LirValue::Function(name) => {
                let llvm_name = self.llvm_symbol_for(&name);
                let function = self
                    .llvm_ctx
                    .module
                    .get_function(&llvm_name)
                    .or_else(|| {
                        self.function_signatures.get(&name).and_then(|sig| {
                            self.function_type_from_signature(sig.clone()).ok().map(|fn_type| {
                                self.llvm_ctx.module.add_function(
                                    &llvm_name,
                                    fn_type,
                                    Some(inkwell::module::Linkage::External),
                                )
                            })
                        })
                    })
                    .or_else(|| {
                        CRuntimeIntrinsics::get_intrinsic_signature(&name).and_then(|sig| {
                            let IntrinsicSignature {
                                name,
                                params,
                                return_type,
                                is_variadic,
                            } = sig;
                            let signature = lir::LirFunctionSignature {
                                params,
                                return_type,
                                is_variadic,
                            };
                            self.function_type_from_signature(signature)
                                .ok()
                                .map(|fn_type| {
                                    self.llvm_ctx.module.add_function(
                                        name,
                                        fn_type,
                                        Some(inkwell::module::Linkage::External),
                                    )
                                })
                        })
                    })
                    .ok_or_else(|| {
                        report_error_with_context(
                            LOG_AREA,
                            format!(
                                "Unknown function reference '{}' encountered during codegen",
                                name
                            ),
                        )
                    })?;

                Ok(function.as_global_value().as_pointer_value().into())
            }
            lir::LirValue::Local(local_id) => self.argument_operands.get(&local_id).copied().ok_or_else(|| {
                report_error_with_context(LOG_AREA, format!("Unknown local: {}", local_id))
            }),
            lir::LirValue::StackSlot(slot_id) => self
                .stack_slot_map
                .get(&slot_id)
                .map(|(ptr, _)| (*ptr).into())
                .ok_or_else(|| {
                    report_error_with_context(LOG_AREA, format!("Unknown stack slot: {}", slot_id))
                }),
            lir::LirValue::Undef(ty) => {
                let llvm_ty = self.llvm_basic_type(&ty)?;
                Ok(self.undef_value_for_type(llvm_ty))
            }
            lir::LirValue::Null(ty) => {
                let llvm_ty = self.llvm_basic_type(&ty)?;
                Ok(llvm_ty.const_zero())
            }
        }
    }

    fn convert_lir_constant_to_value(&mut self, lir_const: lir::LirConstant) -> Result<BasicValueEnum<'static>> {
        match lir_const {
            lir::LirConstant::Int(value, ty) => {
                let int_ty = self.llvm_int_type(&ty)?;
                Ok(int_ty.const_int(value as u64, true).into())
            }
            lir::LirConstant::UInt(value, ty) => {
                let int_ty = self.llvm_int_type(&ty)?;
                Ok(int_ty.const_int(value, false).into())
            }
            lir::LirConstant::Float(value, ty) => {
                let float_ty = self.llvm_float_type(&ty)?;
                Ok(float_ty.const_float(value).into())
            }
            lir::LirConstant::Bool(value) => Ok(self.llvm_ctx.const_bool(value).into()),
            lir::LirConstant::Struct(values, ty) => {
                let struct_ty = match self.llvm_basic_type(&ty)? {
                    BasicTypeEnum::StructType(strct) => strct,
                    _ => {
                        return Err(report_error_with_context(
                            LOG_AREA,
                            "Expected struct type for struct constant",
                        ))
                    }
                };
                let mut llvm_values = Vec::with_capacity(values.len());
                for value in values {
                    llvm_values.push(self.convert_lir_constant_to_value(value)?);
                }
                Ok(struct_ty.const_named_struct(&llvm_values).into())
            }
            lir::LirConstant::Array(elements, elem_ty) => {
                let elem_ty = self.llvm_basic_type(&elem_ty)?;
                let mut llvm_values = Vec::with_capacity(elements.len());
                for element in elements {
                    llvm_values.push(self.convert_lir_constant_to_value(element)?);
                }

                let array_value = unsafe {
                    let mut raw_values: Vec<_> = llvm_values
                        .iter()
                        .map(|v| v.as_value_ref())
                        .collect();
                    let value_ref = LLVMConstArray2(
                        elem_ty.as_type_ref(),
                        raw_values.as_mut_ptr(),
                        raw_values.len() as u64,
                    );
                    inkwell::values::ArrayValue::new(value_ref)
                };
                Ok(array_value.into())
            }
            lir::LirConstant::Null(ty) => {
                let llvm_ty = self.llvm_basic_type(&ty)?;
                Ok(llvm_ty.const_zero())
            }
            lir::LirConstant::Undef(ty) => {
                let llvm_ty = self.llvm_basic_type(&ty)?;
                Ok(self.undef_value_for_type(llvm_ty))
            }
            lir::LirConstant::String(value) => {
                let ptr = self.get_or_create_string_ptr(&value)?;
                Ok(ptr.into())
            }
        }
    }

    fn get_or_create_string_ptr(&mut self, value: &str) -> Result<PointerValue<'static>> {
        if let Some(ptr) = self.string_globals.get(value) {
            return Ok(*ptr);
        }

        let name = format!(".str.{}.{}", self.symbol_prefix, self.next_string_id);
        self.next_string_id += 1;

        let global = self
            .llvm_ctx
            .builder
            .build_global_string_ptr(value, &name)
            .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
        let ptr = global.as_pointer_value();
        self.string_globals.insert(value.to_string(), ptr);
        Ok(ptr)
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

    fn lir_type_from_value(&self, value: &lir::LirValue) -> Option<lir::LirType> {
        match value {
            lir::LirValue::Register(id) => self.register_map.get(id).map(|(_, ty)| ty.clone()),
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

    fn llvm_basic_type(&self, lir_type: &lir::LirType) -> Result<BasicTypeEnum<'static>> {
        match lir_type {
            lir::LirType::I1 => Ok(self.llvm_ctx.i1_type().into()),
            lir::LirType::I8 => Ok(self.llvm_ctx.i8_type().into()),
            lir::LirType::I16 => Ok(self.llvm_ctx.i16_type().into()),
            lir::LirType::I32 => Ok(self.llvm_ctx.i32_type().into()),
            lir::LirType::I64 => Ok(self.llvm_ctx.i64_type().into()),
            lir::LirType::I128 => Ok(self.llvm_ctx.i128_type().into()),
            lir::LirType::F32 => Ok(self.llvm_ctx.f32_type().into()),
            lir::LirType::F64 => Ok(self.llvm_ctx.f64_type().into()),
            lir::LirType::Ptr(_) => Ok(self.llvm_ctx.ptr_type().into()),
            lir::LirType::Array(element_type, size) => {
                let element = self.llvm_basic_type(element_type)?;
                Ok(element.array_type(*size as u32).into())
            }
            lir::LirType::Struct { fields, packed, .. } => {
                let mut element_types = Vec::with_capacity(fields.len());
                for field in fields {
                    element_types.push(self.llvm_basic_type(field)?);
                }
                Ok(self.llvm_ctx.context.struct_type(&element_types, *packed).into())
            }
            lir::LirType::Function { .. } => Ok(self.llvm_ctx.ptr_type().into()),
            lir::LirType::Void => Err(report_error_with_context(
                LOG_AREA,
                "Void type cannot be used as a basic type",
            )),
            lir::LirType::Error => Ok(self.llvm_ctx.i64_type().into()),
            other => Err(report_error_with_context(
                LOG_AREA,
                format!("unsupported LIR type in LLVM lowering: {:?}", other),
            )),
        }
    }

    fn llvm_int_type(&self, lir_type: &lir::LirType) -> Result<IntType<'static>> {
        match lir_type {
            lir::LirType::I1 => Ok(self.llvm_ctx.i1_type()),
            lir::LirType::I8 => Ok(self.llvm_ctx.i8_type()),
            lir::LirType::I16 => Ok(self.llvm_ctx.i16_type()),
            lir::LirType::I32 => Ok(self.llvm_ctx.i32_type()),
            lir::LirType::I64 => Ok(self.llvm_ctx.i64_type()),
            lir::LirType::I128 => Ok(self.llvm_ctx.i128_type()),
            _ => Ok(self.llvm_ctx.i64_type()),
        }
    }

    fn llvm_float_type(&self, lir_type: &lir::LirType) -> Result<FloatType<'static>> {
        match lir_type {
            lir::LirType::F32 => Ok(self.llvm_ctx.f32_type()),
            lir::LirType::F64 => Ok(self.llvm_ctx.f64_type()),
            _ => Ok(self.llvm_ctx.f64_type()),
        }
    }

    fn function_type_from_signature(
        &self,
        signature: lir::LirFunctionSignature,
    ) -> Result<inkwell::types::FunctionType<'static>> {
        let mut param_types = Vec::with_capacity(signature.params.len());
        for param in signature.params {
            param_types.push(self.llvm_basic_type(&param)?.into());
        }

        match signature.return_type {
            lir::LirType::Void => Ok(self
                .llvm_ctx
                .void_type()
                .fn_type(&param_types, signature.is_variadic)),
            _ => Ok(self
                .llvm_basic_type(&signature.return_type)?
                .fn_type(&param_types, signature.is_variadic)),
        }
    }

    fn function_type_from_lir_type(&self, ty: &lir::LirType) -> Option<FunctionType<'static>> {
        match ty {
            lir::LirType::Function {
                return_type,
                param_types,
                is_variadic,
            } => self
                .function_type_from_signature(lir::LirFunctionSignature {
                    params: param_types.clone(),
                    return_type: (**return_type).clone(),
                    is_variadic: *is_variadic,
                })
                .ok(),
            lir::LirType::Ptr(inner) => self.function_type_from_lir_type(inner),
            _ => None,
        }
    }

    fn default_int_type(&self) -> IntType<'static> {
        self.llvm_ctx.i64_type()
    }

    fn coerce_to_int(
        &self,
        value: BasicValueEnum<'static>,
        target: IntType<'static>,
    ) -> Result<IntValue<'static>> {
        match value {
            BasicValueEnum::IntValue(int_val) => {
                let src_bits = int_val.get_type().get_bit_width();
                let dst_bits = target.get_bit_width();
                if src_bits == dst_bits {
                    Ok(int_val)
                } else if src_bits < dst_bits {
                    self.llvm_ctx
                        .builder
                        .build_int_z_extend(int_val, target, "zext")
                        .map_err(|e| fp_core::error::Error::from(e.to_string()))
                } else {
                    self.llvm_ctx
                        .builder
                        .build_int_truncate(int_val, target, "trunc")
                        .map_err(|e| fp_core::error::Error::from(e.to_string()))
                }
            }
            BasicValueEnum::PointerValue(ptr_val) => self
                .llvm_ctx
                .builder
                .build_ptr_to_int(ptr_val, target, "ptrtoint")
                .map_err(|e| fp_core::error::Error::from(e.to_string())),
            BasicValueEnum::FloatValue(float_val) => self
                .llvm_ctx
                .builder
                .build_float_to_unsigned_int(float_val, target, "fptoui")
                .map_err(|e| fp_core::error::Error::from(e.to_string())),
            _ => Err(report_error_with_context(
                LOG_AREA,
                "Unable to coerce value to integer",
            )),
        }
    }

    fn coerce_to_int_signed(
        &self,
        value: BasicValueEnum<'static>,
        target: IntType<'static>,
    ) -> Result<IntValue<'static>> {
        match value {
            BasicValueEnum::IntValue(int_val) => {
                let src_bits = int_val.get_type().get_bit_width();
                let dst_bits = target.get_bit_width();
                if src_bits == dst_bits {
                    Ok(int_val)
                } else if src_bits < dst_bits {
                    self.llvm_ctx
                        .builder
                        .build_int_s_extend(int_val, target, "sext")
                        .map_err(|e| fp_core::error::Error::from(e.to_string()))
                } else {
                    self.llvm_ctx
                        .builder
                        .build_int_truncate(int_val, target, "trunc")
                        .map_err(|e| fp_core::error::Error::from(e.to_string()))
                }
            }
            _ => self.coerce_to_int(value, target),
        }
    }

    fn coerce_to_float(
        &self,
        value: BasicValueEnum<'static>,
        target: FloatType<'static>,
    ) -> Result<FloatValue<'static>> {
        match value {
            BasicValueEnum::FloatValue(float_val) => {
                let src_bits = float_val.get_type().get_bit_width();
                let dst_bits = target.get_bit_width();
                if src_bits == dst_bits {
                    Ok(float_val)
                } else if src_bits < dst_bits {
                    self.llvm_ctx
                        .builder
                        .build_float_ext(float_val, target, "fpext")
                        .map_err(|e| fp_core::error::Error::from(e.to_string()))
                } else {
                    self.llvm_ctx
                        .builder
                        .build_float_trunc(float_val, target, "fptrunc")
                        .map_err(|e| fp_core::error::Error::from(e.to_string()))
                }
            }
            BasicValueEnum::IntValue(int_val) => self
                .llvm_ctx
                .builder
                .build_unsigned_int_to_float(int_val, target, "uitofp")
                .map_err(|e| fp_core::error::Error::from(e.to_string())),
            _ => Err(report_error_with_context(
                LOG_AREA,
                "Unable to coerce value to float",
            )),
        }
    }

    fn coerce_to_pointer(&self, value: BasicValueEnum<'static>) -> Result<PointerValue<'static>> {
        match value {
            BasicValueEnum::PointerValue(ptr) => Ok(ptr),
            BasicValueEnum::IntValue(int_val) => self
                .llvm_ctx
                .builder
                .build_int_to_ptr(int_val, self.llvm_ctx.ptr_type(), "inttoptr")
                .map_err(|e| fp_core::error::Error::from(e.to_string())),
            _ => Err(report_error_with_context(
                LOG_AREA,
                "Unable to coerce value to pointer",
            )),
        }
    }

    fn try_coerce_to_int_value(
        &mut self,
        value: lir::LirValue,
    ) -> Result<Option<IntValue<'static>>> {
        let value = self.convert_lir_value_to_basic_value(value)?;
        match value {
            BasicValueEnum::IntValue(int_val) => Ok(Some(int_val)),
            _ => Ok(None),
        }
    }

    fn zero_value_for_type(&self, ty: &lir::LirType) -> Result<BasicValueEnum<'static>> {
        match ty {
            lir::LirType::Void => Err(report_error_with_context(
                LOG_AREA,
                "Cannot build zero value for void type",
            )),
            _ => Ok(self.llvm_basic_type(ty)?.const_zero()),
        }
    }

    fn undef_value_for_type(&self, ty: BasicTypeEnum<'static>) -> BasicValueEnum<'static> {
        match ty {
            BasicTypeEnum::ArrayType(array_ty) => array_ty.get_undef().into(),
            BasicTypeEnum::FloatType(float_ty) => float_ty.get_undef().into(),
            BasicTypeEnum::IntType(int_ty) => int_ty.get_undef().into(),
            BasicTypeEnum::PointerType(ptr_ty) => ptr_ty.get_undef().into(),
            BasicTypeEnum::StructType(struct_ty) => struct_ty.get_undef().into(),
            BasicTypeEnum::VectorType(vec_ty) => vec_ty.get_undef().into(),
            BasicTypeEnum::ScalableVectorType(vec_ty) => vec_ty.get_undef().into(),
        }
    }

    fn convert_linkage(&self, lir_linkage: lir::Linkage) -> inkwell::module::Linkage {
        match lir_linkage {
            lir::Linkage::External => inkwell::module::Linkage::External,
            lir::Linkage::Internal => inkwell::module::Linkage::Internal,
            lir::Linkage::Private => inkwell::module::Linkage::Private,
            _ => inkwell::module::Linkage::External,
        }
    }

    fn convert_visibility(&self, lir_visibility: lir::Visibility) -> GlobalVisibility {
        match lir_visibility {
            lir::Visibility::Default => GlobalVisibility::Default,
            lir::Visibility::Hidden => GlobalVisibility::Hidden,
            lir::Visibility::Protected => GlobalVisibility::Protected,
        }
    }

    fn convert_calling_convention(&self, cc: &lir::CallingConvention) -> u32 {
        match cc {
            lir::CallingConvention::C => LLVMCallConv::LLVMCCallConv as u32,
            lir::CallingConvention::Fast => LLVMCallConv::LLVMFastCallConv as u32,
            lir::CallingConvention::Cold => LLVMCallConv::LLVMColdCallConv as u32,
            lir::CallingConvention::AnyReg => LLVMCallConv::LLVMAnyRegCallConv as u32,
            lir::CallingConvention::PreserveMost => LLVMCallConv::LLVMPreserveMostCallConv as u32,
            lir::CallingConvention::PreserveAll => LLVMCallConv::LLVMPreserveAllCallConv as u32,
            lir::CallingConvention::Swift => LLVMCallConv::LLVMSwiftCallConv as u32,
            lir::CallingConvention::CxxFastTLS => LLVMCallConv::LLVMCXXFASTTLSCallConv as u32,
            lir::CallingConvention::X86StdCall => LLVMCallConv::LLVMX86StdcallCallConv as u32,
            lir::CallingConvention::X86FastCall => LLVMCallConv::LLVMX86FastcallCallConv as u32,
            lir::CallingConvention::X86ThisCall => LLVMCallConv::LLVMX86ThisCallCallConv as u32,
            lir::CallingConvention::X86VectorCall => LLVMCallConv::LLVMX86VectorCallCallConv as u32,
            lir::CallingConvention::Win64 => LLVMCallConv::LLVMWin64CallConv as u32,
            lir::CallingConvention::X86_64SysV => LLVMCallConv::LLVMX8664SysVCallConv as u32,
            lir::CallingConvention::AAPCS => LLVMCallConv::LLVMARMAAPCSCallConv as u32,
            lir::CallingConvention::AAPCSVfp => LLVMCallConv::LLVMARMAAPCSVFPCallConv as u32,
            lir::CallingConvention::WebKitJS => LLVMCallConv::LLVMCCallConv as u32,
        }
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
}
