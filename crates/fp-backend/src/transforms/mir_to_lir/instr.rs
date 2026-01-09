// MIR→LIR lowering implementation (moved from mod.rs)
// This file currently contains the full original implementation and will be
// gradually split into layout and abi submodules.

// BEGIN ORIGINAL CONTENT
use crate::transformations::IrTransform;
use fp_core::error::Result;
use fp_core::intrinsics::IntrinsicCallKind;
use fp_core::mir::ty::{
    ConstKind, ConstValue, FloatTy, IntTy, Scalar, Ty, TyKind, TypeAndMut, UintTy,
};
use fp_core::{lir, mir};
use std::collections::{HashMap, HashSet, VecDeque};

// Internal submodules; items are used via inherent methods

/// Generator for transforming MIR to LIR (Low-level IR)
pub struct LirGenerator {
    next_lir_id: lir::LirId,
    next_label: u32,
    register_map: HashMap<mir::LocalId, lir::LirValue>,
    current_function: Option<lir::LirFunction>,
    pub(crate) const_values: HashMap<mir::LocalId, lir::LirConstant>,
    extra_globals: Vec<lir::LirGlobal>,
    const_global_counter: u64,
    local_types: Vec<Ty>,
    current_return_type: Option<lir::LirType>,
    return_local: Option<mir::LocalId>,
    mutable_locals: HashSet<mir::LocalId>,
    local_storage: HashMap<mir::LocalId, LocalStorage>,
    entry_allocas: Vec<lir::LirInstruction>,
    queued_instructions: Vec<lir::LirInstruction>,
    name_counters: HashMap<String, usize>,
    struct_layouts: HashMap<String, Vec<Option<lir::LirType>>>,
    function_symbol_map: HashMap<String, String>,
    function_signatures: HashMap<String, lir::LirFunctionSignature>,
    runtime_symbol_map: fn(&str) -> Option<lir::RuntimeSymbol>,
}

#[derive(Clone)]
struct LocalStorage {
    ptr_value: lir::LirValue,
    element_type: lir::LirType,
    alignment: u32,
}

#[derive(Clone)]
struct PlaceAddress {
    ptr: lir::LirValue,
    ty: Ty,
    lir_ty: lir::LirType,
    alignment: u32,
}

#[derive(Clone)]
enum PlaceAccess {
    Address(PlaceAddress),
    Value {
        value: lir::LirValue,
        ty: Ty,
        lir_ty: lir::LirType,
    },
}

impl LirGenerator {
    /// Create a new LIR generator
    pub fn new() -> Self {
        Self::new_with_runtime_symbol_map(|_| None)
    }

    /// Create a new LIR generator with a backend-specific runtime symbol mapper.
    pub fn new_with_runtime_symbol_map(
        runtime_symbol_map: fn(&str) -> Option<lir::RuntimeSymbol>,
    ) -> Self {
        Self {
            next_lir_id: 0,
            next_label: 0,
            register_map: HashMap::new(),
            current_function: None,
            const_values: HashMap::new(),
            extra_globals: Vec::new(),
            const_global_counter: 0,
            local_types: Vec::new(),
            current_return_type: None,
            return_local: None,
            mutable_locals: HashSet::new(),
            local_storage: HashMap::new(),
            entry_allocas: Vec::new(),
            queued_instructions: Vec::new(),
            name_counters: HashMap::new(),
            struct_layouts: HashMap::new(),
            function_symbol_map: HashMap::new(),
            function_signatures: HashMap::new(),
            runtime_symbol_map,
        }
    }

    /// Transform a MIR program to LIR
    pub fn transform(&mut self, mir_program: mir::Program) -> Result<lir::LirProgram> {
        let mut lir_program = lir::LirProgram::new();

        self.predeclare_function_signatures(&mir_program);

        for mir_item in mir_program.items {
            match mir_item.kind {
                mir::ItemKind::Function(mir_func) => {
                    let lir_func =
                        self.transform_function_with_bodies(mir_func, &mir_program.bodies)?;
                    lir_program.functions.push(lir_func);
                }
                mir::ItemKind::Static(mir_static) => {
                    let lir_static = self.transform_static(mir_static)?;
                    lir_program.globals.push(lir_static);
                }
            }
        }

        if !self.extra_globals.is_empty() {
            lir_program.globals.extend(self.extra_globals.drain(..));
        }

        Ok(lir_program)
    }

    fn predeclare_function_signatures(&mut self, program: &mir::Program) {
        for item in &program.items {
            if let mir::ItemKind::Function(func) = &item.kind {
                let name = self.mangle_function_name(func);
                let signature = lir::LirFunctionSignature {
                    params: func
                        .sig
                        .inputs
                        .iter()
                        .map(|ty| self.lir_type_from_ty(ty))
                        .collect(),
                    return_type: self.lir_type_from_ty(&func.sig.output),
                    is_variadic: false,
                };
                self.function_signatures.entry(name).or_insert(signature);
            }
        }
    }

    /// Transform a MIR function to LIR
    fn transform_function_with_bodies(
        &mut self,
        mir_func: mir::Function,
        bodies: &std::collections::HashMap<mir::BodyId, mir::Body>,
    ) -> Result<lir::LirFunction> {
        // Reset generator state for new function
        self.reset_for_new_function();

        let function_name = self.mangle_function_name(&mir_func);
        let param_types: Vec<lir::LirType> = mir_func
            .sig
            .inputs
            .iter()
            .map(|ty| self.lir_type_from_ty(ty))
            .collect();
        let return_type = self.lir_type_from_ty(&mir_func.sig.output);
        self.current_return_type = Some(return_type.clone());

        let signature = lir::LirFunctionSignature {
            params: param_types.clone(),
            return_type: return_type.clone(),
            is_variadic: false,
        };
        self.function_signatures
            .insert(function_name.clone(), signature.clone());

        let mut lir_func = lir::LirFunction {
            name: lir::Name::new(function_name),
            signature,
            basic_blocks: Vec::new(),
            locals: Vec::new(),
            stack_slots: Vec::new(),
            calling_convention: lir::CallingConvention::C,
            linkage: lir::Linkage::Internal,
        };

        // Transform MIR body if present
        if let Some(mir_body) = bodies.get(&mir_func.body_id) {
            // First pass: analyze const values
            self.analyze_const_values(mir_body)?;
            self.local_types = mir_body.locals.iter().map(|decl| decl.ty.clone()).collect();
            self.return_local = Some(mir_body.return_local);
            self.mutable_locals = self.compute_mutable_locals(mir_body);
            if let Some(ret_local) = self.return_local {
                self.mutable_locals.insert(ret_local);
            }
            self.initialize_local_storage(mir_body);
            lir_func.locals = self.build_lir_locals(mir_body);
            self.seed_argument_registers(mir_body);

            let block_order = self.compute_block_order(mir_body);
            for &bb_idx in &block_order {
                let bb = &mir_body.basic_blocks[bb_idx];
                let lir_block = self.transform_basic_block(bb_idx as u32, bb)?;
                lir_func.basic_blocks.push(lir_block);
            }
            // Ensure at least one block exists
            if lir_func.basic_blocks.is_empty() {
                lir_func.basic_blocks.push(lir::LirBasicBlock {
                    id: 0,
                    label: Some(lir::Name::new("entry")),
                    instructions: Vec::new(),
                    terminator: lir::LirTerminator::Return(None),
                    predecessors: Vec::new(),
                    successors: Vec::new(),
                });
            }
        } else {
            // Fallback: create a minimal function with a return
            lir_func.basic_blocks.push(lir::LirBasicBlock {
                id: 0,
                label: Some(lir::Name::new("entry")),
                instructions: Vec::new(),
                terminator: lir::LirTerminator::Return(None),
                predecessors: Vec::new(),
                successors: Vec::new(),
            });
        }

        self.populate_block_edges(&mut lir_func.basic_blocks);
        self.function_signatures.insert(
            String::from(lir_func.name.clone()),
            lir_func.signature.clone(),
        );

        self.current_function = Some(lir_func.clone());
        Ok(lir_func)
    }

    fn mangle_function_name(&mut self, mir_func: &mir::Function) -> String {
        let base = if !mir_func.path.is_empty() {
            mir_func
                .path
                .iter()
                .map(|s| s.as_str())
                .collect::<Vec<_>>()
                .join("::")
        } else if !mir_func.name.as_str().is_empty() {
            String::from(mir_func.name.clone())
        } else {
            "anonymous_fn".to_string()
        };

        if let Some(existing) = self.function_symbol_map.get(&base) {
            return existing.clone();
        }

        let sanitized = Self::sanitize_symbol(&base);
        let entry = self
            .name_counters
            .entry(sanitized.clone())
            .or_insert(0_usize);
        let suffix = *entry;
        *entry += 1;

        let final_name = if suffix == 0 {
            sanitized
        } else {
            format!("{sanitized}__{suffix}")
        };

        self.function_symbol_map
            .insert(base.clone(), final_name.clone());
        if !mir_func.name.as_str().is_empty() {
            self.function_symbol_map
                .entry(String::from(mir_func.name.clone()))
                .or_insert(final_name.clone());
        }

        final_name
    }

    fn sanitize_symbol(name: &str) -> String {
        let mut result = String::with_capacity(name.len());
        for ch in name.chars() {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                result.push(ch);
            } else {
                result.push('_');
            }
        }

        if result.is_empty() {
            return "anonymous_fn".to_string();
        }

        if matches!(result.chars().next(), Some(c) if c.is_ascii_digit()) {
            let mut prefixed = String::with_capacity(result.len() + 1);
            prefixed.push('_');
            prefixed.push_str(&result);
            prefixed
        } else {
            result
        }
    }

    /// Transform a MIR static to LIR global
    fn transform_static(&mut self, mir_static: mir::Static) -> Result<lir::LirGlobal> {
        let name = lir::Name::new(format!("global_{}", self.next_id()));
        let lir_ty = self.lir_type_from_ty(&mir_static.ty);
        let initializer = self.convert_static_initializer(&mir_static.init, &mir_static.ty)?;
        let alignment = Self::alignment_for_lir_type(&lir_ty).max(1);

        Ok(lir::LirGlobal {
            name,
            ty: lir_ty,
            initializer: Some(initializer),
            linkage: lir::Linkage::Internal,
            visibility: lir::Visibility::Hidden,
            is_constant: matches!(mir_static.mutability, mir::Mutability::Not),
            alignment: Some(alignment),
            section: None,
        })
    }

    fn convert_static_initializer(
        &mut self,
        init: &mir::Operand,
        ty: &Ty,
    ) -> Result<lir::LirConstant> {
        match init {
            mir::Operand::Constant(constant) => self.constant_to_lir_constant(constant, ty),
            other => {
                fp_core::diagnostics::report_warning_with_context(
                    "mir→lir",
                    format!(
                        "unsupported static initializer operand {:?}; lowering to undef",
                        other
                    ),
                );
                Ok(lir::LirConstant::Undef(self.lir_type_from_ty(ty)))
            }
        }
    }

    fn constant_to_lir_constant(
        &mut self,
        constant: &mir::Constant,
        ty_hint: &Ty,
    ) -> Result<lir::LirConstant> {
        let target_ty = self.lir_type_from_ty(ty_hint);
        let lir_constant = match &constant.literal {
            mir::ConstantKind::Bool(value) => lir::LirConstant::Bool(*value),
            mir::ConstantKind::Int(value) => match &ty_hint.kind {
                TyKind::Uint(_) => lir::LirConstant::UInt(*value as u64, target_ty.clone()),
                TyKind::Bool => lir::LirConstant::Bool(*value != 0),
                _ => lir::LirConstant::Int(*value, target_ty.clone()),
            },
            mir::ConstantKind::UInt(value) => lir::LirConstant::UInt(*value, target_ty.clone()),
            mir::ConstantKind::Float(value) => lir::LirConstant::Float(*value, target_ty.clone()),
            mir::ConstantKind::Str(value) => {
                if let TyKind::Slice(elem_ty) = &ty_hint.kind {
                    let elem_lir_ty = self.lir_type_from_ty(elem_ty);
                    let slice_ty = self.slice_lir_type(&elem_lir_ty);
                    let ptr_const = lir::LirConstant::String(value.clone());
                    let len_const = lir::LirConstant::UInt(value.len() as u64, lir::LirType::I64);
                    lir::LirConstant::Struct(vec![ptr_const, len_const], slice_ty)
                } else {
                    lir::LirConstant::String(value.clone())
                }
            }
            mir::ConstantKind::Null => lir::LirConstant::Null(target_ty.clone()),
            mir::ConstantKind::Val(value, value_ty) => {
                self.const_value_to_lir_constant(value, value_ty)?
            }
            mir::ConstantKind::Fn(name, _ty) => lir::LirConstant::FunctionRef(
                lir::Name::new(name.as_str().to_string()),
                target_ty.clone(),
            ),
            mir::ConstantKind::Global(name, _ty) => lir::LirConstant::GlobalRef(
                lir::Name::new(name.as_str().to_string()),
                target_ty.clone(),
                Vec::new(),
            ),
            mir::ConstantKind::Ty(_) => {
                fp_core::diagnostics::report_warning_with_context(
                    "mir→lir",
                    "type-only constant in static initializer lowered to undef".to_string(),
                );
                lir::LirConstant::Undef(target_ty.clone())
            }
        };

        Ok(self.cast_constant_to_lir_type(lir_constant, &target_ty))
    }

    fn const_value_to_lir_constant(
        &mut self,
        value: &mir::ConstValue,
        ty: &Ty,
    ) -> Result<lir::LirConstant> {
        match value {
            mir::ConstValue::Unit => {
                let lir_ty = self.lir_type_from_ty(ty);
                Ok(lir::LirConstant::Undef(lir_ty))
            }
            mir::ConstValue::Bool(value) => Ok(lir::LirConstant::Bool(*value)),
            mir::ConstValue::Int(value) => {
                Ok(lir::LirConstant::Int(*value, self.lir_type_from_ty(ty)))
            }
            mir::ConstValue::UInt(value) => {
                Ok(lir::LirConstant::UInt(*value, self.lir_type_from_ty(ty)))
            }
            mir::ConstValue::Float(value) => {
                Ok(lir::LirConstant::Float(*value, self.lir_type_from_ty(ty)))
            }
            mir::ConstValue::Str(value) => {
                if let TyKind::Slice(elem_ty) = &ty.kind {
                    let elem_lir_ty = self.lir_type_from_ty(elem_ty);
                    let slice_ty = self.slice_lir_type(&elem_lir_ty);
                    let ptr_const = lir::LirConstant::String(value.clone());
                    let len_const = lir::LirConstant::UInt(value.len() as u64, lir::LirType::I64);
                    return Ok(lir::LirConstant::Struct(
                        vec![ptr_const, len_const],
                        slice_ty,
                    ));
                }
                Ok(lir::LirConstant::String(value.clone()))
            }
            mir::ConstValue::Null => Ok(lir::LirConstant::Null(self.lir_type_from_ty(ty))),
            mir::ConstValue::Fn(name) => Ok(lir::LirConstant::FunctionRef(
                lir::Name::new(name.as_str().to_string()),
                self.lir_type_from_ty(ty),
            )),
            mir::ConstValue::Tuple(elements) => {
                let element_types = match &ty.kind {
                    TyKind::Tuple(items) => items.clone(),
                    _ => {
                        return Ok(lir::LirConstant::Undef(self.lir_type_from_ty(ty)));
                    }
                };
                let mut lowered = Vec::with_capacity(elements.len());
                for (elem, elem_ty) in elements.iter().zip(element_types.iter()) {
                    lowered.push(self.const_value_to_lir_constant(elem, elem_ty)?);
                }
                let lir_ty = self.lir_type_from_ty(ty);
                Ok(lir::LirConstant::Struct(lowered, lir_ty))
            }
            mir::ConstValue::Array(elements) => {
                let elem_ty = match &ty.kind {
                    TyKind::Array(inner, _) => inner.as_ref(),
                    _ => {
                        return Ok(lir::LirConstant::Undef(self.lir_type_from_ty(ty)));
                    }
                };
                let lir_elem_ty = self.lir_type_from_ty(elem_ty);
                let mut lowered = Vec::with_capacity(elements.len());
                for element in elements {
                    lowered.push(self.const_value_to_lir_constant(element, elem_ty)?);
                }
                Ok(lir::LirConstant::Array(lowered, lir_elem_ty))
            }
            mir::ConstValue::Struct(fields) => {
                let lir_ty = self.lir_type_from_ty(ty);
                let lir::LirType::Struct {
                    fields: lir_fields, ..
                } = &lir_ty
                else {
                    return Err(fp_core::error::Error::from(
                        "struct constant requires a struct layout in LIR",
                    ));
                };
                if lir_fields.len() != fields.len() {
                    return Err(fp_core::error::Error::from(format!(
                        "struct constant field count mismatch: expected {}, got {}",
                        lir_fields.len(),
                        fields.len()
                    )));
                }
                let mut lowered = Vec::with_capacity(fields.len());
                for (idx, field) in fields.iter().enumerate() {
                    let field_lir_ty = lir_fields
                        .get(idx)
                        .ok_or_else(|| {
                            fp_core::error::Error::from("struct constant field type missing")
                        })?
                        .clone();
                    lowered.push(
                        self.const_value_to_lir_constant_with_lir_type(field, &field_lir_ty)?,
                    );
                }
                Ok(lir::LirConstant::Struct(lowered, lir_ty))
            }
            mir::ConstValue::List { elements, elem_ty } => {
                let elem_lir_ty = self.lir_type_from_ty(elem_ty);
                let mut lowered = Vec::with_capacity(elements.len());
                for element in elements {
                    lowered.push(self.const_value_to_lir_constant(element, elem_ty)?);
                }
                let data_global = self.allocate_const_array_global(elem_lir_ty.clone(), lowered);
                let ptr_ty = lir::LirType::Ptr(Box::new(elem_lir_ty.clone()));
                let ptr_const =
                    lir::LirConstant::GlobalRef(data_global.name.clone(), ptr_ty, vec![0, 0]);
                let slice_ty = self.slice_lir_type(&elem_lir_ty);
                let len_const = lir::LirConstant::UInt(elements.len() as u64, lir::LirType::I64);
                Ok(lir::LirConstant::Struct(
                    vec![ptr_const, len_const],
                    slice_ty,
                ))
            }
            mir::ConstValue::Map {
                entries,
                key_ty,
                value_ty,
            } => {
                let key_lir_ty = self.lir_type_from_ty(key_ty);
                let value_lir_ty = self.lir_type_from_ty(value_ty);
                let entry_lir_ty = lir::LirType::Struct {
                    fields: vec![key_lir_ty.clone(), value_lir_ty.clone()],
                    packed: false,
                    name: Some("__map_entry".to_string()),
                };
                let mut lowered_entries = Vec::with_capacity(entries.len());
                for (key, value) in entries {
                    let key_val = self.const_value_to_lir_constant(key, key_ty)?;
                    let value_val = self.const_value_to_lir_constant(value, value_ty)?;
                    lowered_entries.push(lir::LirConstant::Struct(
                        vec![key_val, value_val],
                        entry_lir_ty.clone(),
                    ));
                }
                let data_global =
                    self.allocate_const_array_global(entry_lir_ty.clone(), lowered_entries);
                let ptr_ty = lir::LirType::Ptr(Box::new(entry_lir_ty.clone()));
                let ptr_const =
                    lir::LirConstant::GlobalRef(data_global.name.clone(), ptr_ty, vec![0, 0]);
                let slice_ty = self.slice_lir_type(&entry_lir_ty);
                let len_const = lir::LirConstant::UInt(entries.len() as u64, lir::LirType::I64);
                Ok(lir::LirConstant::Struct(
                    vec![ptr_const, len_const],
                    slice_ty,
                ))
            }
        }
    }

    fn const_value_to_lir_constant_with_lir_type(
        &mut self,
        value: &mir::ConstValue,
        lir_ty: &lir::LirType,
    ) -> Result<lir::LirConstant> {
        match value {
            mir::ConstValue::Unit => Ok(lir::LirConstant::Undef(lir_ty.clone())),
            mir::ConstValue::Bool(value) => Ok(lir::LirConstant::Bool(*value)),
            mir::ConstValue::Int(value) => Ok(lir::LirConstant::Int(*value, lir_ty.clone())),
            mir::ConstValue::UInt(value) => Ok(lir::LirConstant::UInt(*value, lir_ty.clone())),
            mir::ConstValue::Float(value) => Ok(lir::LirConstant::Float(*value, lir_ty.clone())),
            mir::ConstValue::Str(value) => {
                if let lir::LirType::Struct { fields, .. } = lir_ty {
                    if fields.len() == 2
                        && matches!(&fields[0], lir::LirType::Ptr(inner) if **inner == lir::LirType::I8)
                        && fields[1] == lir::LirType::I64
                    {
                        let ptr_const = lir::LirConstant::String(value.clone());
                        let len_const =
                            lir::LirConstant::UInt(value.len() as u64, lir::LirType::I64);
                        return Ok(lir::LirConstant::Struct(
                            vec![ptr_const, len_const],
                            lir_ty.clone(),
                        ));
                    }
                }
                Ok(lir::LirConstant::String(value.clone()))
            }
            mir::ConstValue::Null => Ok(lir::LirConstant::Null(lir_ty.clone())),
            mir::ConstValue::Fn(name) => Ok(lir::LirConstant::FunctionRef(
                lir::Name::new(name.as_str().to_string()),
                lir_ty.clone(),
            )),
            mir::ConstValue::Array(elements) => {
                let lir::LirType::Array(elem_ty, _len) = lir_ty else {
                    return Err(fp_core::error::Error::from(
                        "array constant requires an array type in LIR",
                    ));
                };
                let mut lowered = Vec::with_capacity(elements.len());
                for element in elements {
                    lowered.push(
                        self.const_value_to_lir_constant_with_lir_type(element, elem_ty.as_ref())?,
                    );
                }
                Ok(lir::LirConstant::Array(lowered, lir_ty.clone()))
            }
            mir::ConstValue::Tuple(elements) | mir::ConstValue::Struct(elements) => {
                let lir::LirType::Struct { fields, .. } = lir_ty else {
                    return Err(fp_core::error::Error::from(
                        "tuple/struct constant requires a struct type in LIR",
                    ));
                };
                if fields.len() != elements.len() {
                    return Err(fp_core::error::Error::from(format!(
                        "tuple/struct constant field count mismatch: expected {}, got {}",
                        fields.len(),
                        elements.len()
                    )));
                }
                let mut lowered = Vec::with_capacity(elements.len());
                for (idx, element) in elements.iter().enumerate() {
                    let field_ty = fields
                        .get(idx)
                        .ok_or_else(|| {
                            fp_core::error::Error::from("struct constant field type missing")
                        })?
                        .clone();
                    lowered
                        .push(self.const_value_to_lir_constant_with_lir_type(element, &field_ty)?);
                }
                Ok(lir::LirConstant::Struct(lowered, lir_ty.clone()))
            }
            mir::ConstValue::List { .. } | mir::ConstValue::Map { .. } => Err(
                fp_core::error::Error::from("container constants require MIR type information"),
            ),
        }
    }

    fn allocate_const_array_global(
        &mut self,
        elem_ty: lir::LirType,
        elements: Vec<lir::LirConstant>,
    ) -> lir::LirGlobal {
        let name = lir::Name::new(format!("__const_data_{}", self.const_global_counter));
        self.const_global_counter += 1;
        let array_ty = lir::LirType::Array(Box::new(elem_ty), elements.len() as u64);
        let initializer = lir::LirConstant::Array(
            elements,
            match &array_ty {
                lir::LirType::Array(elem, _) => *elem.clone(),
                _ => lir::LirType::I8,
            },
        );
        let align = Self::alignment_for_lir_type(&array_ty);
        let global = lir::LirGlobal {
            name,
            ty: array_ty,
            initializer: Some(initializer),
            linkage: lir::Linkage::Internal,
            visibility: lir::Visibility::Hidden,
            is_constant: true,
            alignment: Some(align),
            section: None,
        };
        self.extra_globals.push(global.clone());
        global
    }

    /// Transform a basic block
    fn transform_basic_block(
        &mut self,
        bb_id: u32,
        bb_data: &mir::BasicBlockData,
    ) -> Result<lir::LirBasicBlock> {
        let mut lir_block = lir::LirBasicBlock {
            id: bb_id,
            label: Some(lir::Name::new(format!("bb{}", bb_id))),
            instructions: Vec::new(),
            terminator: lir::LirTerminator::Return(None),
            predecessors: Vec::new(),
            successors: Vec::new(),
        };

        if bb_data.is_cleanup {
            let landingpad_ty = lir::LirType::Struct {
                fields: vec![
                    lir::LirType::Ptr(Box::new(lir::LirType::I8)),
                    lir::LirType::I32,
                ],
                packed: false,
                name: None,
            };
            lir_block.instructions.push(lir::LirInstruction {
                id: self.next_id(),
                kind: lir::LirInstructionKind::LandingPad {
                    result_type: landingpad_ty.clone(),
                    personality: None,
                    cleanup: false,
                    clauses: vec![lir::LandingPadClause::Catch(lir::LirValue::Constant(
                        lir::LirConstant::Null(lir::LirType::Ptr(Box::new(lir::LirType::I8))),
                    ))],
                },
                type_hint: Some(landingpad_ty),
                debug_info: None,
            });
        }

        // Transform all MIR statements into LIR instructions
        for stmt in &bb_data.statements {
            if bb_id == 0 && !self.entry_allocas.is_empty() {
                lir_block.instructions.extend(self.entry_allocas.clone());
                self.entry_allocas.clear();
            }
            let lir_insts = self.transform_statement(stmt)?;
            for inst in lir_insts {
                lir_block.instructions.push(inst);
            }
        }

        if bb_id == 0 && !self.entry_allocas.is_empty() {
            lir_block.instructions.extend(self.entry_allocas.clone());
            self.entry_allocas.clear();
        }

        // Transform the terminator
        let terminator = if let Some(terminator) = &bb_data.terminator {
            self.transform_terminator(terminator, &mut lir_block)?
        } else {
            // Some MIR producers omit an explicit return on the final block when the
            // value has already been written to the designated return local. In
            // that case, synthesize a return terminator and let `prepare_return_value`
            // materialize the value (loading from the return slot if needed).
            lir::LirTerminator::Return(self.prepare_return_value(&mut lir_block))
        };

        lir_block.terminator = terminator;
        Ok(lir_block)
    }

    /// Transform a MIR statement to LIR instructions
    fn transform_statement(&mut self, stmt: &mir::Statement) -> Result<Vec<lir::LirInstruction>> {
        match &stmt.kind {
            mir::StatementKind::Assign(place, rvalue) => self.transform_assign(place, rvalue),
            mir::StatementKind::IntrinsicCall { kind, format, args } => {
                let lir_kind = match kind {
                    IntrinsicCallKind::Print => lir::LirIntrinsicKind::Print,
                    IntrinsicCallKind::Println => lir::LirIntrinsicKind::Println,
                    IntrinsicCallKind::Format => {
                        return Err(fp_core::error::Error::from(
                            "format intrinsic must be assigned to a place".to_string(),
                        ))
                    }
                    _ => {
                        return Err(fp_core::error::Error::from(format!(
                            "unsupported MIR intrinsic in LIR lowering: {:?}",
                            kind
                        )))
                    }
                };
                let mut instructions = Vec::new();
                let mut lir_args = Vec::with_capacity(args.len());
                for arg in args {
                    let value = self.transform_operand(arg)?;
                    instructions.extend(self.take_queued_instructions());
                    let adjusted = match arg {
                        mir::Operand::Move(place) | mir::Operand::Copy(place) => {
                            let slice_elem =
                                self.lookup_place_type(place).and_then(|ty| match ty.kind {
                                    TyKind::Slice(elem) => Some(*elem),
                                    _ => None,
                                });
                            if let Some(elem_ty) = slice_elem {
                                let elem_lir = self.lir_type_from_ty(&elem_ty);
                                let ptr_ty = lir::LirType::Ptr(Box::new(elem_lir));
                                self.extract_slice_field(value, 0, ptr_ty, &mut instructions)
                            } else {
                                value
                            }
                        }
                        _ => value,
                    };
                    lir_args.push(adjusted);
                }
                instructions.push(lir::LirInstruction {
                    id: self.next_id(),
                    kind: lir::LirInstructionKind::IntrinsicCall {
                        kind: lir_kind,
                        format: format.clone(),
                        args: lir_args,
                    },
                    type_hint: None,
                    debug_info: None,
                });
                Ok(instructions)
            }
            mir::StatementKind::StorageLive(_) => Ok(Vec::new()),
            mir::StatementKind::StorageDead(_) => Ok(Vec::new()),
            _ => Ok(vec![lir::LirInstruction {
                id: self.next_id(),
                kind: lir::LirInstructionKind::Unreachable,
                type_hint: None,
                debug_info: None,
            }]),
        }
    }

    /// Transform an assignment
    #[allow(unused_assignments)]
    fn transform_assign(
        &mut self,
        place: &mir::Place,
        rvalue: &mir::Rvalue,
    ) -> Result<Vec<lir::LirInstruction>> {
        let mut instructions = Vec::new();
        let target_access = self.resolve_place(place)?;
        instructions.extend(self.take_queued_instructions());
        let assign_whole_place = place.projection.is_empty();
        let place_ty = self.lookup_place_type(place);
        let destination_lir_ty = place_ty.as_ref().map(|ty| self.lir_type_from_ty(ty));
        let mut result_value: Option<lir::LirValue> = None;

        match rvalue {
            mir::Rvalue::Use(operand) => match operand {
                mir::Operand::Move(op_place) | mir::Operand::Copy(op_place) => {
                    let operand_access = self.resolve_place(op_place)?;
                    instructions.extend(self.take_queued_instructions());
                    let value = match operand_access {
                        PlaceAccess::Address(addr) => {
                            let load_id = self.next_id();
                            instructions.push(lir::LirInstruction {
                                id: load_id,
                                kind: lir::LirInstructionKind::Load {
                                    address: addr.ptr,
                                    alignment: Some(addr.alignment),
                                    volatile: false,
                                },
                                type_hint: Some(addr.lir_ty.clone()),
                                debug_info: None,
                            });
                            lir::LirValue::Register(load_id)
                        }
                        PlaceAccess::Value { value, lir_ty, .. } => {
                            let expects_pointer =
                                matches!(destination_lir_ty, Some(lir::LirType::Ptr(_)));
                            if !expects_pointer && matches!(lir_ty, lir::LirType::Ptr(_)) {
                                let load_ty = destination_lir_ty.clone().expect(
                                    "destination LIR type must be known for load operation",
                                );
                                let load_id = self.next_id();
                                instructions.push(lir::LirInstruction {
                                    id: load_id,
                                    kind: lir::LirInstructionKind::Load {
                                        address: value.clone(),
                                        alignment: Some(Self::alignment_for_lir_type(&load_ty)),
                                        volatile: false,
                                    },
                                    type_hint: Some(load_ty.clone()),
                                    debug_info: None,
                                });
                                lir::LirValue::Register(load_id)
                            } else {
                                value
                            }
                        }
                    };
                    result_value = Some(value);
                }
                mir::Operand::Constant(_) => {
                    let value = self.transform_operand(operand)?;
                    instructions.extend(self.take_queued_instructions());
                    result_value = Some(value);
                }
            },
            mir::Rvalue::IntrinsicCall { kind, format, args } => {
                let lir_kind = match kind {
                    IntrinsicCallKind::Format => lir::LirIntrinsicKind::Format,
                    IntrinsicCallKind::Print | IntrinsicCallKind::Println => {
                        return Err(fp_core::error::Error::from(
                            "print/println must be emitted as statements".to_string(),
                        ))
                    }
                    _ => {
                        return Err(fp_core::error::Error::from(format!(
                            "unsupported intrinsic in assignment: {:?}",
                            kind
                        )))
                    }
                };
                let mut lir_args = Vec::with_capacity(args.len());
                for arg in args {
                    let value = self.transform_operand(arg)?;
                    instructions.extend(self.take_queued_instructions());
                    lir_args.push(value);
                }

                let instr_id = self.next_id();
                instructions.push(lir::LirInstruction {
                    id: instr_id,
                    kind: lir::LirInstructionKind::IntrinsicCall {
                        kind: lir_kind,
                        format: format.clone(),
                        args: lir_args,
                    },
                    type_hint: destination_lir_ty.clone(),
                    debug_info: None,
                });
                result_value = Some(lir::LirValue::Register(instr_id));
            }
            mir::Rvalue::BinaryOp(bin_op, lhs, rhs) => {
                let lhs_value = self.transform_operand(lhs)?;
                instructions.extend(self.take_queued_instructions());
                let rhs_value = self.transform_operand(rhs)?;
                instructions.extend(self.take_queued_instructions());

                let instr_id = self.next_id();
                let lir_kind =
                    self.lower_binary_op(bin_op.clone(), lhs_value.clone(), rhs_value.clone());
                let type_hint = destination_lir_ty
                    .clone()
                    .or_else(|| Some(lir::LirType::I32));

                instructions.push(lir::LirInstruction {
                    id: instr_id,
                    kind: lir_kind,
                    type_hint,
                    debug_info: None,
                });

                result_value = Some(lir::LirValue::Register(instr_id));
            }
            mir::Rvalue::Repeat(operand, len) => {
                let elem_ty = match place_ty.as_ref().map(|ty| &ty.kind) {
                    Some(TyKind::Array(elem, _)) => *elem.clone(),
                    other => {
                        return Err(fp_core::error::Error::from(format!(
                            "MIR→LIR: repeat expects array destination, found {:?}",
                            other
                        )));
                    }
                };
                let mut fields = Vec::with_capacity(*len as usize);
                for _ in 0..*len {
                    fields.push(operand.clone());
                }
                let (mut aggregate_insts, aggregate_value) =
                    self.handle_aggregate(place, &mir::AggregateKind::Array(elem_ty), &fields)?;
                instructions.append(&mut aggregate_insts);
                result_value = aggregate_value;
            }
            mir::Rvalue::UnaryOp(un_op, operand) => {
                let operand_value = self.transform_operand(operand)?;
                instructions.extend(self.take_queued_instructions());

                let result_ty = destination_lir_ty
                    .clone()
                    .expect("destination LIR type must be known for unary operation");

                let instr_id = self.next_id();
                let lir_kind =
                    self.lower_unary_op(un_op.clone(), operand_value.clone(), &result_ty)?;

                instructions.push(lir::LirInstruction {
                    id: instr_id,
                    kind: lir_kind,
                    type_hint: Some(result_ty.clone()),
                    debug_info: None,
                });

                result_value = Some(lir::LirValue::Register(instr_id));
            }
            mir::Rvalue::Aggregate(kind, fields) => {
                let (mut aggregate_insts, aggregate_value) =
                    self.handle_aggregate(place, kind, fields)?;
                instructions.append(&mut aggregate_insts);
                result_value = aggregate_value;
            }
            mir::Rvalue::ContainerLiteral { kind, elements } => {
                let elem_lir_ty = self.container_element_lir_type(kind);
                let len = self.container_len(kind);
                let align = Self::alignment_for_lir_type(&elem_lir_ty);
                let ptr_ty = lir::LirType::Ptr(Box::new(elem_lir_ty.clone()));
                let size_value =
                    lir::LirValue::Constant(lir::LirConstant::Int(len as i64, lir::LirType::I32));

                let alloca_id = self.next_id();
                instructions.push(lir::LirInstruction {
                    id: alloca_id,
                    kind: lir::LirInstructionKind::Alloca {
                        size: size_value,
                        alignment: align,
                    },
                    type_hint: Some(ptr_ty.clone()),
                    debug_info: None,
                });
                let array_ptr = lir::LirValue::Register(alloca_id);

                for (idx, operand) in elements.iter().enumerate() {
                    let value = self.transform_operand(operand)?;
                    instructions.extend(self.take_queued_instructions());
                    let coerced = self.coerce_aggregate_value_with_source(
                        value,
                        self.type_of_operand(operand).as_ref(),
                        &elem_lir_ty,
                        &mut instructions,
                    );
                    let index_value = lir::LirValue::Constant(lir::LirConstant::UInt(
                        idx as u64,
                        lir::LirType::I64,
                    ));
                    let elem_ptr = self.element_ptr_at(
                        array_ptr.clone(),
                        &elem_lir_ty,
                        index_value,
                        &mut instructions,
                    );
                    instructions.push(lir::LirInstruction {
                        id: self.next_id(),
                        kind: lir::LirInstructionKind::Store {
                            value: coerced,
                            address: elem_ptr,
                            alignment: Some(align),
                            volatile: false,
                        },
                        type_hint: None,
                        debug_info: None,
                    });
                }

                result_value =
                    Some(self.build_slice_value(array_ptr, len, &elem_lir_ty, &mut instructions));
            }
            mir::Rvalue::ContainerMapLiteral { kind, entries } => {
                let entry_lir_ty = self.container_element_lir_type(kind);
                let len = self.container_len(kind);
                let align = Self::alignment_for_lir_type(&entry_lir_ty);
                let ptr_ty = lir::LirType::Ptr(Box::new(entry_lir_ty.clone()));
                let size_value =
                    lir::LirValue::Constant(lir::LirConstant::Int(len as i64, lir::LirType::I32));

                let alloca_id = self.next_id();
                instructions.push(lir::LirInstruction {
                    id: alloca_id,
                    kind: lir::LirInstructionKind::Alloca {
                        size: size_value,
                        alignment: align,
                    },
                    type_hint: Some(ptr_ty.clone()),
                    debug_info: None,
                });
                let array_ptr = lir::LirValue::Register(alloca_id);

                let entry_fields = match &entry_lir_ty {
                    lir::LirType::Struct { fields, .. } => fields.clone(),
                    _ => vec![lir::LirType::I64, lir::LirType::I64],
                };
                let key_ty = entry_fields.get(0).cloned().unwrap_or(lir::LirType::I64);
                let value_ty = entry_fields.get(1).cloned().unwrap_or(lir::LirType::I64);

                for (idx, (key_op, value_op)) in entries.iter().enumerate() {
                    let key_val = self.transform_operand(key_op)?;
                    instructions.extend(self.take_queued_instructions());
                    let value_val = self.transform_operand(value_op)?;
                    instructions.extend(self.take_queued_instructions());

                    let key_val = self.coerce_aggregate_value_with_source(
                        key_val,
                        self.type_of_operand(key_op).as_ref(),
                        &key_ty,
                        &mut instructions,
                    );
                    let value_val = self.coerce_aggregate_value_with_source(
                        value_val,
                        self.type_of_operand(value_op).as_ref(),
                        &value_ty,
                        &mut instructions,
                    );

                    let mut entry_value =
                        lir::LirValue::Constant(lir::LirConstant::Undef(entry_lir_ty.clone()));
                    let key_insert = self.next_id();
                    instructions.push(lir::LirInstruction {
                        id: key_insert,
                        kind: lir::LirInstructionKind::InsertValue {
                            aggregate: entry_value,
                            element: key_val,
                            indices: vec![0],
                        },
                        type_hint: Some(entry_lir_ty.clone()),
                        debug_info: None,
                    });
                    entry_value = lir::LirValue::Register(key_insert);
                    let value_insert = self.next_id();
                    instructions.push(lir::LirInstruction {
                        id: value_insert,
                        kind: lir::LirInstructionKind::InsertValue {
                            aggregate: entry_value,
                            element: value_val,
                            indices: vec![1],
                        },
                        type_hint: Some(entry_lir_ty.clone()),
                        debug_info: None,
                    });
                    let entry_value = lir::LirValue::Register(value_insert);

                    let index_value = lir::LirValue::Constant(lir::LirConstant::UInt(
                        idx as u64,
                        lir::LirType::I64,
                    ));
                    let entry_ptr = self.element_ptr_at(
                        array_ptr.clone(),
                        &entry_lir_ty,
                        index_value,
                        &mut instructions,
                    );
                    instructions.push(lir::LirInstruction {
                        id: self.next_id(),
                        kind: lir::LirInstructionKind::Store {
                            value: entry_value,
                            address: entry_ptr,
                            alignment: Some(align),
                            volatile: false,
                        },
                        type_hint: None,
                        debug_info: None,
                    });
                }

                result_value =
                    Some(self.build_slice_value(array_ptr, len, &entry_lir_ty, &mut instructions));
            }
            mir::Rvalue::ContainerLen { kind, .. } => {
                let len = self.container_len(kind);
                result_value = Some(lir::LirValue::Constant(lir::LirConstant::UInt(
                    len,
                    lir::LirType::I64,
                )));
            }
            mir::Rvalue::ContainerGet {
                kind,
                container,
                key,
            } => {
                let elem_lir_ty = self.container_element_lir_type(kind);
                let slice_value = self.transform_operand(container)?;
                instructions.extend(self.take_queued_instructions());
                let slice_ptr_ty = lir::LirType::Ptr(Box::new(elem_lir_ty.clone()));
                let slice_ptr = self.extract_slice_field(
                    slice_value,
                    0,
                    slice_ptr_ty.clone(),
                    &mut instructions,
                );

                match kind {
                    mir::ContainerKind::List { .. } => {
                        let idx_value = self.transform_operand(key)?;
                        instructions.extend(self.take_queued_instructions());
                        let elem_ptr = self.element_ptr_at(
                            slice_ptr,
                            &elem_lir_ty,
                            idx_value,
                            &mut instructions,
                        );
                        let load_id = self.next_id();
                        instructions.push(lir::LirInstruction {
                            id: load_id,
                            kind: lir::LirInstructionKind::Load {
                                address: elem_ptr,
                                alignment: Some(Self::alignment_for_lir_type(&elem_lir_ty)),
                                volatile: false,
                            },
                            type_hint: Some(elem_lir_ty.clone()),
                            debug_info: None,
                        });
                        result_value = Some(lir::LirValue::Register(load_id));
                    }
                    mir::ContainerKind::Map {
                        key_ty,
                        value_ty,
                        len,
                    } => {
                        let query_value = self.transform_operand(key)?;
                        instructions.extend(self.take_queued_instructions());
                        let key_lir_ty = self.lir_type_from_ty(key_ty);
                        let value_lir_ty = self.lir_type_from_ty(value_ty);
                        let query_value = self.coerce_aggregate_value_with_source(
                            query_value,
                            self.type_of_operand(key).as_ref(),
                            &key_lir_ty,
                            &mut instructions,
                        );

                        let mut current =
                            lir::LirValue::Constant(lir::LirConstant::Undef(value_lir_ty.clone()));

                        for idx in 0..*len {
                            let index_value = lir::LirValue::Constant(lir::LirConstant::UInt(
                                idx,
                                lir::LirType::I64,
                            ));
                            let entry_ptr = self.element_ptr_at(
                                slice_ptr.clone(),
                                &elem_lir_ty,
                                index_value,
                                &mut instructions,
                            );
                            let load_id = self.next_id();
                            instructions.push(lir::LirInstruction {
                                id: load_id,
                                kind: lir::LirInstructionKind::Load {
                                    address: entry_ptr,
                                    alignment: Some(Self::alignment_for_lir_type(&elem_lir_ty)),
                                    volatile: false,
                                },
                                type_hint: Some(elem_lir_ty.clone()),
                                debug_info: None,
                            });
                            let entry_value = lir::LirValue::Register(load_id);

                            let key_id = self.next_id();
                            instructions.push(lir::LirInstruction {
                                id: key_id,
                                kind: lir::LirInstructionKind::ExtractValue {
                                    aggregate: entry_value.clone(),
                                    indices: vec![0],
                                },
                                type_hint: Some(key_lir_ty.clone()),
                                debug_info: None,
                            });
                            let entry_key = lir::LirValue::Register(key_id);

                            let value_id = self.next_id();
                            instructions.push(lir::LirInstruction {
                                id: value_id,
                                kind: lir::LirInstructionKind::ExtractValue {
                                    aggregate: entry_value,
                                    indices: vec![1],
                                },
                                type_hint: Some(value_lir_ty.clone()),
                                debug_info: None,
                            });
                            let entry_value = lir::LirValue::Register(value_id);

                            let cmp_id = self.next_id();
                            instructions.push(lir::LirInstruction {
                                id: cmp_id,
                                kind: lir::LirInstructionKind::Eq(entry_key, query_value.clone()),
                                type_hint: Some(lir::LirType::I1),
                                debug_info: None,
                            });
                            let cmp_val = lir::LirValue::Register(cmp_id);

                            let select_id = self.next_id();
                            instructions.push(lir::LirInstruction {
                                id: select_id,
                                kind: lir::LirInstructionKind::Select {
                                    condition: cmp_val,
                                    if_true: entry_value,
                                    if_false: current,
                                },
                                type_hint: Some(value_lir_ty.clone()),
                                debug_info: None,
                            });
                            current = lir::LirValue::Register(select_id);
                        }

                        result_value = Some(current);
                    }
                }
            }
            mir::Rvalue::Ref(_, _, borrowed_place) => {
                let borrowed_access = self.resolve_place(borrowed_place)?;
                instructions.extend(self.take_queued_instructions());
                let pointer = match borrowed_access {
                    PlaceAccess::Address(addr) => addr.ptr,
                    PlaceAccess::Value { value, lir_ty, .. } => {
                        if matches!(lir_ty, lir::LirType::Ptr(_)) {
                            value
                        } else {
                            let alloca_id = self.next_id();
                            let pointer_type = lir::LirType::Ptr(Box::new(lir_ty.clone()));
                            let size_value = lir::LirValue::Constant(lir::LirConstant::Int(
                                1,
                                lir::LirType::I32,
                            ));
                            let alignment = Self::alignment_for_lir_type(&lir_ty);
                            instructions.push(lir::LirInstruction {
                                id: alloca_id,
                                kind: lir::LirInstructionKind::Alloca {
                                    size: size_value,
                                    alignment,
                                },
                                type_hint: Some(pointer_type),
                                debug_info: None,
                            });
                            let ptr_value = lir::LirValue::Register(alloca_id);
                            instructions.push(lir::LirInstruction {
                                id: self.next_id(),
                                kind: lir::LirInstructionKind::Store {
                                    value,
                                    address: ptr_value.clone(),
                                    alignment: Some(alignment),
                                    volatile: false,
                                },
                                type_hint: None,
                                debug_info: None,
                            });
                            ptr_value
                        }
                    }
                };
                result_value = Some(pointer);
            }
            mir::Rvalue::AddressOf(_, borrowed_place) => {
                let borrowed_access = self.resolve_place(borrowed_place)?;
                instructions.extend(self.take_queued_instructions());
                let pointer = match borrowed_access {
                    PlaceAccess::Address(addr) => addr.ptr,
                    PlaceAccess::Value { value, lir_ty, .. } => {
                        if matches!(lir_ty, lir::LirType::Ptr(_)) {
                            value
                        } else {
                            let alloca_id = self.next_id();
                            let pointer_type = lir::LirType::Ptr(Box::new(lir_ty.clone()));
                            let size_value = lir::LirValue::Constant(lir::LirConstant::Int(
                                1,
                                lir::LirType::I32,
                            ));
                            let alignment = Self::alignment_for_lir_type(&lir_ty);
                            instructions.push(lir::LirInstruction {
                                id: alloca_id,
                                kind: lir::LirInstructionKind::Alloca {
                                    size: size_value,
                                    alignment,
                                },
                                type_hint: Some(pointer_type),
                                debug_info: None,
                            });
                            let ptr_value = lir::LirValue::Register(alloca_id);
                            instructions.push(lir::LirInstruction {
                                id: self.next_id(),
                                kind: lir::LirInstructionKind::Store {
                                    value,
                                    address: ptr_value.clone(),
                                    alignment: Some(alignment),
                                    volatile: false,
                                },
                                type_hint: None,
                                debug_info: None,
                            });
                            ptr_value
                        }
                    }
                };
                result_value = Some(pointer);
            }
            mir::Rvalue::Len(place) => {
                let place_ty = self.lookup_place_type(place).ok_or_else(|| {
                    crate::error::optimization_error(
                        "MIR→LIR: missing type information for len() operand",
                    )
                })?;
                match &place_ty.kind {
                    TyKind::Array(_, len) => {
                        let len_value = self.array_length_from_const(len) as i64;
                        result_value = Some(lir::LirValue::Constant(lir::LirConstant::UInt(
                            len_value as u64,
                            lir::LirType::I64,
                        )));
                    }
                    TyKind::Slice(_) => {
                        let access = self.resolve_place(place)?;
                        instructions.extend(self.take_queued_instructions());
                        let slice_value = match access {
                            PlaceAccess::Address(addr) => {
                                let load_id = self.next_id();
                                instructions.push(lir::LirInstruction {
                                    id: load_id,
                                    kind: lir::LirInstructionKind::Load {
                                        address: addr.ptr,
                                        alignment: Some(addr.alignment),
                                        volatile: false,
                                    },
                                    type_hint: Some(addr.lir_ty),
                                    debug_info: None,
                                });
                                lir::LirValue::Register(load_id)
                            }
                            PlaceAccess::Value { value, .. } => value,
                        };

                        let extract_id = self.next_id();
                        instructions.push(lir::LirInstruction {
                            id: extract_id,
                            kind: lir::LirInstructionKind::ExtractValue {
                                aggregate: slice_value,
                                indices: vec![1],
                            },
                            type_hint: Some(lir::LirType::I64),
                            debug_info: None,
                        });
                        result_value = Some(lir::LirValue::Register(extract_id));
                    }
                    _ => {
                        return Err(crate::error::optimization_error(
                            "MIR→LIR: len() expects array or slice operand",
                        ));
                    }
                }
            }
            mir::Rvalue::Cast(cast_kind, operand, _ty) => {
                let operand_value = self.transform_operand(operand)?;
                instructions.extend(self.take_queued_instructions());
                let source_ty = self.type_of_operand(operand);
                let target_ty = destination_lir_ty
                    .clone()
                    .expect("destination LIR type must be known for cast operation");

                if matches!(target_ty, lir::LirType::Void) {
                    result_value = Some(lir::LirValue::Undef(target_ty));
                    return Ok(instructions);
                }

                if let Some(const_value) = self.cast_constant_value(&operand_value, &target_ty) {
                    result_value = Some(const_value);
                    return Ok(instructions);
                }

                if let Some(src_ty) = source_ty.clone() {
                    let target_is_ptr = matches!(target_ty, lir::LirType::Ptr(_));
                    if self.is_float_type(&src_ty) && target_is_ptr {
                        let int_ty = lir::LirType::I64;
                        let fp_to_int_id = self.next_id();
                        instructions.push(lir::LirInstruction {
                            id: fp_to_int_id,
                            kind: lir::LirInstructionKind::FPToSI(
                                operand_value.clone(),
                                int_ty.clone(),
                            ),
                            type_hint: Some(int_ty.clone()),
                            debug_info: None,
                        });
                        let ptr_id = self.next_id();
                        instructions.push(lir::LirInstruction {
                            id: ptr_id,
                            kind: lir::LirInstructionKind::IntToPtr(lir::LirValue::Register(
                                fp_to_int_id,
                            )),
                            type_hint: Some(target_ty.clone()),
                            debug_info: None,
                        });
                        result_value = Some(lir::LirValue::Register(ptr_id));
                        return Ok(instructions);
                    }
                    if matches!(src_ty, lir::LirType::Ptr(_)) && self.is_float_type(&target_ty) {
                        let int_ty = lir::LirType::I64;
                        let ptr_to_int_id = self.next_id();
                        instructions.push(lir::LirInstruction {
                            id: ptr_to_int_id,
                            kind: lir::LirInstructionKind::PtrToInt(operand_value.clone()),
                            type_hint: Some(int_ty.clone()),
                            debug_info: None,
                        });
                        let fp_id = self.next_id();
                        instructions.push(lir::LirInstruction {
                            id: fp_id,
                            kind: lir::LirInstructionKind::SIToFP(
                                lir::LirValue::Register(ptr_to_int_id),
                                target_ty.clone(),
                            ),
                            type_hint: Some(target_ty.clone()),
                            debug_info: None,
                        });
                        result_value = Some(lir::LirValue::Register(fp_id));
                        return Ok(instructions);
                    }
                }

                let instr_id = self.next_id();
                let instr_kind = self.lower_cast(
                    cast_kind.clone(),
                    operand_value.clone(),
                    source_ty,
                    target_ty.clone(),
                );

                instructions.push(lir::LirInstruction {
                    id: instr_id,
                    kind: instr_kind,
                    type_hint: Some(target_ty.clone()),
                    debug_info: None,
                });

                result_value = Some(lir::LirValue::Register(instr_id));
            }
            _ => {
                instructions.push(lir::LirInstruction {
                    id: self.next_id(),
                    kind: lir::LirInstructionKind::Unreachable,
                    type_hint: None,
                    debug_info: None,
                });
                return Ok(instructions);
            }
        }

        if let Some(value) = result_value.clone() {
            let (_target_lir_ty, target_is_zst) = match &target_access {
                PlaceAccess::Address(addr) => (addr.lir_ty.clone(), Self::is_zero_sized(&addr.ty)),
                PlaceAccess::Value { ty, lir_ty, .. } => (lir_ty.clone(), Self::is_zero_sized(ty)),
            };

            if !target_is_zst {
                // No additional handling needed for zero-sized constants; they will be
                // materialised as undef where appropriate when consumed.
            }

            if let PlaceAccess::Address(addr) = &target_access {
                if matches!(value, lir::LirValue::Function(_)) {
                    self.local_storage.remove(&place.local);
                } else if !target_is_zst {
                    let store_id = self.next_id();
                    instructions.push(lir::LirInstruction {
                        id: store_id,
                        kind: lir::LirInstructionKind::Store {
                            value: value.clone(),
                            address: addr.ptr.clone(),
                            alignment: Some(addr.alignment),
                            volatile: false,
                        },
                        type_hint: None,
                        debug_info: None,
                    });
                }
            }

            let mut should_update_register_map = assign_whole_place;
            if matches!(target_access, PlaceAccess::Address(_))
                && self.local_storage.contains_key(&place.local)
            {
                should_update_register_map = false;
            }
            if let Some(return_local) = self.return_local {
                if place.local == return_local {
                    should_update_register_map = true;
                }
            }

            if matches!(value, lir::LirValue::Function(_)) {
                should_update_register_map = true;
            }

            if should_update_register_map {
                self.register_map.insert(place.local, value);
            }
        }

        Ok(instructions)
    }

    /// Transform a MIR terminator to LIR terminator
    fn transform_terminator(
        &mut self,
        terminator: &mir::Terminator,
        block: &mut lir::LirBasicBlock,
    ) -> Result<lir::LirTerminator> {
        match &terminator.kind {
            mir::TerminatorKind::Return => {
                Ok(lir::LirTerminator::Return(self.prepare_return_value(block)))
            }
            mir::TerminatorKind::Goto { target } => Ok(lir::LirTerminator::Br(*target)),
            mir::TerminatorKind::Unreachable => Ok(lir::LirTerminator::Unreachable),
            mir::TerminatorKind::Call {
                func,
                args,
                destination,
                cleanup,
                ..
            } => self.transform_call_terminator(func, args, destination, cleanup, block),
            mir::TerminatorKind::SwitchInt {
                discr,
                switch_ty: _,
                targets,
            } => {
                let discr_value = self.transform_operand(discr)?;
                block.instructions.extend(self.take_queued_instructions());
                if targets.values.len() == 1 {
                    let true_target = targets.targets[0];
                    let false_target = targets.otherwise;
                    Ok(lir::LirTerminator::CondBr {
                        condition: discr_value,
                        if_true: true_target,
                        if_false: false_target,
                    })
                } else {
                    let cases = targets
                        .values
                        .iter()
                        .zip(targets.targets.iter())
                        .map(|(value, target)| (*value as u64, *target))
                        .collect();
                    Ok(lir::LirTerminator::Switch {
                        value: discr_value,
                        default: targets.otherwise,
                        cases,
                    })
                }
            }
            _ => Ok(lir::LirTerminator::Return(None)),
        }
    }

    /// Transform a MIR operand to LIR value
    fn transform_operand(&mut self, operand: &mir::Operand) -> Result<lir::LirValue> {
        match operand {
            mir::Operand::Move(place) | mir::Operand::Copy(place) => {
                let access = self.resolve_place(place)?;
                match access {
                    PlaceAccess::Address(addr) => {
                        let load_id = self.next_id();
                        self.queued_instructions.push(lir::LirInstruction {
                            id: load_id,
                            kind: lir::LirInstructionKind::Load {
                                address: addr.ptr.clone(),
                                alignment: Some(addr.alignment),
                                volatile: false,
                            },
                            type_hint: Some(addr.lir_ty.clone()),
                            debug_info: None,
                        });
                        Ok(lir::LirValue::Register(load_id))
                    }
                    PlaceAccess::Value { value, .. } => Ok(value),
                }
            }
            mir::Operand::Constant(constant) => match &constant.literal {
                mir::ConstantKind::Fn(name, _ty) => {
                    let function_name = self
                        .function_symbol_map
                        .get(&String::from(name.clone()))
                        .cloned()
                        .unwrap_or_else(|| String::from(name.clone()));
                    Ok(lir::LirValue::Function(function_name))
                }
                mir::ConstantKind::Global(name, ty) => {
                    let mapped_name = self
                        .function_symbol_map
                        .get(&String::from(name.clone()))
                        .cloned()
                        .unwrap_or_else(|| String::from(name.clone()));
                    if self.function_signatures.contains_key(&mapped_name) {
                        return Ok(lir::LirValue::Function(mapped_name));
                    }
                    if let Some(runtime_target) = (self.runtime_symbol_map)(&mapped_name) {
                        return Ok(lir::LirValue::Function(runtime_target.as_str().to_string()));
                    }
                    Ok(lir::LirValue::Global(
                        mapped_name,
                        self.lir_type_from_ty(ty),
                    ))
                }
                mir::ConstantKind::Str(s) => {
                    Ok(lir::LirValue::Constant(lir::LirConstant::String(s.clone())))
                }
                mir::ConstantKind::Int(value) => Ok(lir::LirValue::Constant(
                    lir::LirConstant::Int(*value, lir::LirType::I64),
                )),
                mir::ConstantKind::UInt(value) => Ok(lir::LirValue::Constant(
                    lir::LirConstant::UInt(*value, lir::LirType::I64),
                )),
                mir::ConstantKind::Float(value) => Ok(lir::LirValue::Constant(
                    lir::LirConstant::Float(*value, lir::LirType::F64),
                )),
                mir::ConstantKind::Bool(b) => {
                    Ok(lir::LirValue::Constant(lir::LirConstant::Bool(*b)))
                }
                mir::ConstantKind::Null => Ok(lir::LirValue::Null(lir::LirType::Ptr(Box::new(
                    lir::LirType::I8,
                )))),
                mir::ConstantKind::Val(_cv, _ty) => {
                    return Err(crate::error::optimization_error(
                        "Unsupported complex constant in MIR operand",
                    ));
                }
                _ => {
                    return Err(crate::error::optimization_error(
                        "Unsupported constant kind for MIR→LIR",
                    ))
                }
            },
        }
    }

    /// Helper methods
    fn reset_for_new_function(&mut self) {
        self.next_label = 0;
        self.register_map.clear();
        self.current_function = None;
        self.const_values.clear();
        self.local_types.clear();
        self.current_return_type = None;
        self.return_local = None;
        self.mutable_locals.clear();
        self.local_storage.clear();
        self.entry_allocas.clear();
        self.queued_instructions.clear();
        self.struct_layouts.clear();
    }

    fn compute_mutable_locals(&self, mir_body: &mir::Body) -> HashSet<mir::LocalId> {
        let mut assignment_counts: HashMap<mir::LocalId, usize> = HashMap::new();
        for basic_block in &mir_body.basic_blocks {
            for stmt in &basic_block.statements {
                if let mir::StatementKind::Assign(place, _) = &stmt.kind {
                    *assignment_counts.entry(place.local).or_insert(0) += 1;
                }
            }
        }

        assignment_counts
            .into_iter()
            .filter_map(|(local, count)| if count > 1 { Some(local) } else { None })
            .collect()
    }

    fn initialize_local_storage(&mut self, mir_body: &mir::Body) {
        self.entry_allocas.clear();
        self.local_storage.clear();

        let locals: Vec<_> = self.mutable_locals.clone().into_iter().collect();
        for local in locals {
            let local_index = local as usize;
            if local_index >= self.local_types.len() {
                continue;
            }

            let ty = &self.local_types[local_index];
            if Self::is_zero_sized(ty) {
                continue;
            }

            let lir_ty = self.lir_type_from_ty(ty);
            let alignment = Self::alignment_for_lir_type(&lir_ty);
            if alignment == 0 {
                continue;
            }

            let alloca_id = self.next_id();
            let pointer_type = lir::LirType::Ptr(Box::new(lir_ty.clone()));
            let size_value = lir::LirValue::Constant(lir::LirConstant::Int(1, lir::LirType::I32));
            self.entry_allocas.push(lir::LirInstruction {
                id: alloca_id,
                kind: lir::LirInstructionKind::Alloca {
                    size: size_value,
                    alignment,
                },
                type_hint: Some(pointer_type.clone()),
                debug_info: None,
            });

            self.local_storage.insert(
                local,
                LocalStorage {
                    ptr_value: lir::LirValue::Register(alloca_id),
                    element_type: lir_ty,
                    alignment,
                },
            );

            if local > 0 && (local as usize) <= mir_body.arg_count {
                let store_id = self.next_id();
                self.entry_allocas.push(lir::LirInstruction {
                    id: store_id,
                    kind: lir::LirInstructionKind::Store {
                        value: lir::LirValue::Local(local),
                        address: lir::LirValue::Register(alloca_id),
                        alignment: Some(alignment),
                        volatile: false,
                    },
                    type_hint: None,
                    debug_info: None,
                });
            }
        }

        // Ensure entry allocas appear once at the top of the entry block
        if self.entry_allocas.is_empty() {
            return;
        }
    }

    fn get_or_create_register_for_place(&mut self, place: &mir::Place) -> Result<lir::LirValue> {
        if let Some(storage) = self.local_storage.get(&place.local) {
            return Ok(storage.ptr_value.clone());
        }
        let existing_reg = self.register_map.get(&place.local).cloned();

        if let Some(place_ty) = self.lookup_place_type(place) {
            if Self::is_zero_sized(&place_ty) {
                // Use a dedicated empty-struct constant for zero-sized values to avoid
                // creating "struct ptr i8 { }" constants when the place type lowers to Ptr(I8).
                let empty_ty = lir::LirType::Struct {
                    fields: Vec::new(),
                    packed: false,
                    name: None,
                };
                let value = lir::LirValue::Constant(lir::LirConstant::Struct(Vec::new(), empty_ty));
                self.register_map.insert(place.local, value.clone());
                return Ok(value);
            }

            let lir_ty = self.lir_type_from_ty(&place_ty);
            let alignment = Self::alignment_for_lir_type(&lir_ty);
            if alignment > 0 {
                let pointer_type = lir::LirType::Ptr(Box::new(lir_ty.clone()));
                let size_value =
                    lir::LirValue::Constant(lir::LirConstant::Int(1, lir::LirType::I32));
                let alloca_id = self.next_id();
                self.queued_instructions.push(lir::LirInstruction {
                    id: alloca_id,
                    kind: lir::LirInstructionKind::Alloca {
                        size: size_value,
                        alignment,
                    },
                    type_hint: Some(pointer_type.clone()),
                    debug_info: None,
                });

                let ptr_value = lir::LirValue::Register(alloca_id);
                self.local_storage.insert(
                    place.local,
                    LocalStorage {
                        ptr_value: ptr_value.clone(),
                        element_type: lir_ty,
                        alignment,
                    },
                );

                if let Some(existing) = existing_reg {
                    let store_id = self.next_id();
                    self.queued_instructions.push(lir::LirInstruction {
                        id: store_id,
                        kind: lir::LirInstructionKind::Store {
                            value: existing,
                            address: ptr_value.clone(),
                            alignment: Some(alignment),
                            volatile: false,
                        },
                        type_hint: None,
                        debug_info: None,
                    });
                }

                return Ok(ptr_value);
            }
        }
        Err(crate::error::optimization_error(format!(
            "MIR→LIR: missing value for local {} (place={:?}); cannot lower MIR",
            place.local, place
        )))
    }

    fn resolve_place(&mut self, place: &mir::Place) -> Result<PlaceAccess> {
        if place.projection.is_empty() {
            let ty = self
                .local_types
                .get(place.local as usize)
                .cloned()
                .ok_or_else(|| {
                    crate::error::optimization_error(format!(
                        "MIR→LIR: no type information for local {}",
                        place.local
                    ))
                })?;

            if let Some(storage) = self.local_storage.get(&place.local).cloned() {
                return Ok(PlaceAccess::Address(PlaceAddress {
                    ptr: storage.ptr_value,
                    ty,
                    lir_ty: storage.element_type,
                    alignment: storage.alignment,
                }));
            }

            if let Some(value) = self.register_map.get(&place.local).cloned() {
                let lir_ty = self.lir_type_from_ty(&ty);
                return Ok(PlaceAccess::Value { value, ty, lir_ty });
            }

            if let Ok(value) = self.get_or_create_register_for_place(place) {
                if let Some(storage) = self.local_storage.get(&place.local).cloned() {
                    return Ok(PlaceAccess::Address(PlaceAddress {
                        ptr: storage.ptr_value,
                        ty,
                        lir_ty: storage.element_type,
                        alignment: storage.alignment,
                    }));
                }
                let lir_ty = self.lir_type_from_ty(&ty);
                return Ok(PlaceAccess::Value { value, ty, lir_ty });
            }

            let lir_ty = self.lir_type_from_ty(&ty);
            let placeholder = lir::LirValue::Constant(lir::LirConstant::Undef(lir_ty.clone()));
            return Ok(PlaceAccess::Value {
                value: placeholder,
                ty,
                lir_ty,
            });
        }

        let mut base_place = place.clone();
        let last_projection = base_place
            .projection
            .pop()
            .expect("projection should be non-empty here");
        let base_access = self.resolve_place(&base_place)?;

        match last_projection {
            mir::PlaceElem::Deref => self.apply_deref_projection(&base_place, base_access),
            mir::PlaceElem::Field(idx, field_ty) => {
                self.apply_field_projection(&base_place, base_access, place.local, idx, &field_ty)
            }
            mir::PlaceElem::Index(index_local) => {
                self.apply_index_projection(&base_place, base_access, index_local)
            }
            mir::PlaceElem::ConstantIndex { .. }
            | mir::PlaceElem::Subslice { .. }
            | mir::PlaceElem::Downcast(_, _) => Err(crate::error::optimization_error(
                "MIR→LIR: unsupported place projection for lowering",
            )),
        }
    }

    fn apply_deref_projection(
        &mut self,
        base_place: &mir::Place,
        access: PlaceAccess,
    ) -> Result<PlaceAccess> {
        let base_ty = self.lookup_place_type(base_place).ok_or_else(|| {
            crate::error::optimization_error("MIR→LIR: missing type for deref projection")
        })?;

        let (inner_ty, pointer_lir_ty) = match base_ty.kind {
            TyKind::Ref(_, inner, _) => {
                let pointee = (*inner).clone();
                let lir = self.lir_type_from_ty(&pointee);
                (pointee, lir::LirType::Ptr(Box::new(lir.clone())))
            }
            TyKind::RawPtr(TypeAndMut { ty: inner, .. }) => {
                let pointee = (*inner).clone();
                let lir = self.lir_type_from_ty(&pointee);
                (pointee, lir::LirType::Ptr(Box::new(lir.clone())))
            }
            _ => {
                return Err(crate::error::optimization_error(
                    "MIR→LIR: cannot dereference non-pointer place",
                ));
            }
        };

        let pointer_value = match access {
            PlaceAccess::Address(addr) => {
                let load_id = self.next_id();
                self.queued_instructions.push(lir::LirInstruction {
                    id: load_id,
                    kind: lir::LirInstructionKind::Load {
                        address: addr.ptr,
                        alignment: Some(addr.alignment),
                        volatile: false,
                    },
                    type_hint: Some(pointer_lir_ty.clone()),
                    debug_info: None,
                });
                lir::LirValue::Register(load_id)
            }
            PlaceAccess::Value { value, .. } => value,
        };

        let pointee_lir_ty = self.lir_type_from_ty(&inner_ty);

        let alignment = Self::alignment_for_lir_type(&pointee_lir_ty);
        Ok(PlaceAccess::Address(PlaceAddress {
            ptr: pointer_value,
            ty: inner_ty,
            lir_ty: pointee_lir_ty,
            alignment,
        }))
    }

    fn apply_field_projection(
        &mut self,
        _base_place: &mir::Place,
        access: PlaceAccess,
        _local: mir::LocalId,
        field_index: usize,
        field_ty: &Ty,
    ) -> Result<PlaceAccess> {
        let base_addr = match access {
            PlaceAccess::Address(addr) => addr,
            PlaceAccess::Value { value, ty, lir_ty } => {
                let alignment = Self::alignment_for_lir_type(&lir_ty).max(1);
                let pointer_type = lir::LirType::Ptr(Box::new(lir_ty.clone()));
                let size_value =
                    lir::LirValue::Constant(lir::LirConstant::Int(1, lir::LirType::I32));
                let alloca_id = self.next_id();
                self.queued_instructions.push(lir::LirInstruction {
                    id: alloca_id,
                    kind: lir::LirInstructionKind::Alloca {
                        size: size_value,
                        alignment,
                    },
                    type_hint: Some(pointer_type.clone()),
                    debug_info: None,
                });
                let ptr_value = lir::LirValue::Register(alloca_id);

                let store_id = self.next_id();
                self.queued_instructions.push(lir::LirInstruction {
                    id: store_id,
                    kind: lir::LirInstructionKind::Store {
                        value,
                        address: ptr_value.clone(),
                        alignment: Some(alignment),
                        volatile: false,
                    },
                    type_hint: None,
                    debug_info: None,
                });

                PlaceAddress {
                    ptr: ptr_value,
                    ty,
                    lir_ty,
                    alignment,
                }
            }
        };

        let field_lir_ty = self.lir_type_from_ty(field_ty);

        let mut offset = 0u64;
        if let TyKind::Tuple(elements) = &base_addr.ty.kind {
            for elem_ty in elements.iter().take(field_index) {
                let elem_lir_ty = self.lir_type_from_ty(elem_ty);
                offset = offset.saturating_add(Self::size_of_lir_type(&elem_lir_ty));
            }
        }

        let desired_ptr_ty = lir::LirType::Ptr(Box::new(field_lir_ty.clone()));
        let target_ptr = if offset == 0 {
            let cast_id = self.next_id();
            self.queued_instructions.push(lir::LirInstruction {
                id: cast_id,
                kind: lir::LirInstructionKind::Bitcast(
                    base_addr.ptr.clone(),
                    desired_ptr_ty.clone(),
                ),
                type_hint: Some(desired_ptr_ty.clone()),
                debug_info: None,
            });
            lir::LirValue::Register(cast_id)
        } else {
            let i8_ptr_ty = lir::LirType::Ptr(Box::new(lir::LirType::I8));
            let base_i8_ptr_id = self.next_id();
            self.queued_instructions.push(lir::LirInstruction {
                id: base_i8_ptr_id,
                kind: lir::LirInstructionKind::Bitcast(base_addr.ptr.clone(), i8_ptr_ty.clone()),
                type_hint: Some(i8_ptr_ty.clone()),
                debug_info: None,
            });
            let base_i8_ptr = lir::LirValue::Register(base_i8_ptr_id);

            let offset_value =
                lir::LirValue::Constant(lir::LirConstant::Int(offset as i64, lir::LirType::I64));

            let gep_id = self.next_id();
            self.queued_instructions.push(lir::LirInstruction {
                id: gep_id,
                kind: lir::LirInstructionKind::GetElementPtr {
                    ptr: base_i8_ptr,
                    indices: vec![offset_value],
                    inbounds: true,
                },
                type_hint: Some(i8_ptr_ty.clone()),
                debug_info: None,
            });

            let cast_id = self.next_id();
            self.queued_instructions.push(lir::LirInstruction {
                id: cast_id,
                kind: lir::LirInstructionKind::Bitcast(
                    lir::LirValue::Register(gep_id),
                    desired_ptr_ty.clone(),
                ),
                type_hint: Some(desired_ptr_ty.clone()),
                debug_info: None,
            });
            lir::LirValue::Register(cast_id)
        };

        let alignment = Self::alignment_for_lir_type(&field_lir_ty);
        Ok(PlaceAccess::Address(PlaceAddress {
            ptr: target_ptr,
            ty: field_ty.clone(),
            lir_ty: field_lir_ty,
            alignment,
        }))
    }

    fn apply_index_projection(
        &mut self,
        base_place: &mir::Place,
        access: PlaceAccess,
        index_local: mir::LocalId,
    ) -> Result<PlaceAccess> {
        let base_ty = self.lookup_place_type(base_place).ok_or_else(|| {
            crate::error::optimization_error("MIR→LIR: missing type for index projection")
        })?;

        let element_ty = match &base_ty.kind {
            TyKind::Array(elem, _) => *elem.clone(),
            TyKind::Slice(elem) => *elem.clone(),
            _ => {
                return Err(crate::error::optimization_error(
                    "MIR→LIR: index projection requires array or slice type",
                ))
            }
        };

        let element_lir_ty = self.lir_type_from_ty(&element_ty);
        let element_alignment = Self::alignment_for_lir_type(&element_lir_ty);

        let slice_ptr_ty = lir::LirType::Ptr(Box::new(element_lir_ty.clone()));
        let base_ptr = match access {
            PlaceAccess::Address(addr) => match base_ty.kind {
                TyKind::Slice(_) => {
                    let load_id = self.next_id();
                    self.queued_instructions.push(lir::LirInstruction {
                        id: load_id,
                        kind: lir::LirInstructionKind::Load {
                            address: addr.ptr.clone(),
                            alignment: Some(addr.alignment),
                            volatile: false,
                        },
                        type_hint: Some(addr.lir_ty.clone()),
                        debug_info: None,
                    });
                    let extract_id = self.next_id();
                    self.queued_instructions.push(lir::LirInstruction {
                        id: extract_id,
                        kind: lir::LirInstructionKind::ExtractValue {
                            aggregate: lir::LirValue::Register(load_id),
                            indices: vec![0],
                        },
                        type_hint: Some(slice_ptr_ty.clone()),
                        debug_info: None,
                    });
                    lir::LirValue::Register(extract_id)
                }
                _ => addr.ptr,
            },
            PlaceAccess::Value { value, lir_ty, .. } => match base_ty.kind {
                TyKind::Slice(_) => {
                    let extract_id = self.next_id();
                    self.queued_instructions.push(lir::LirInstruction {
                        id: extract_id,
                        kind: lir::LirInstructionKind::ExtractValue {
                            aggregate: value,
                            indices: vec![0],
                        },
                        type_hint: Some(slice_ptr_ty.clone()),
                        debug_info: None,
                    });
                    lir::LirValue::Register(extract_id)
                }
                _ => {
                    let alignment = Self::alignment_for_lir_type(&lir_ty).max(1);
                    let pointer_type = lir::LirType::Ptr(Box::new(lir_ty.clone()));
                    let size_value =
                        lir::LirValue::Constant(lir::LirConstant::Int(1, lir::LirType::I32));
                    let alloca_id = self.next_id();
                    self.queued_instructions.push(lir::LirInstruction {
                        id: alloca_id,
                        kind: lir::LirInstructionKind::Alloca {
                            size: size_value,
                            alignment,
                        },
                        type_hint: Some(pointer_type.clone()),
                        debug_info: None,
                    });
                    let ptr_value = lir::LirValue::Register(alloca_id);

                    let store_id = self.next_id();
                    self.queued_instructions.push(lir::LirInstruction {
                        id: store_id,
                        kind: lir::LirInstructionKind::Store {
                            value,
                            address: ptr_value.clone(),
                            alignment: Some(alignment),
                            volatile: false,
                        },
                        type_hint: None,
                        debug_info: None,
                    });

                    ptr_value
                }
            },
        };

        let index_place = mir::Place::from_local(index_local);
        let index_operand = mir::Operand::Copy(index_place);
        let mut index_value = self.transform_operand(&index_operand)?;
        let index_lir_ty = self
            .type_of_operand(&index_operand)
            .unwrap_or(lir::LirType::I64);
        if index_lir_ty != lir::LirType::I64 {
            let cast_id = self.next_id();
            self.queued_instructions.push(lir::LirInstruction {
                id: cast_id,
                kind: lir::LirInstructionKind::SextOrTrunc(index_value.clone(), lir::LirType::I64),
                type_hint: Some(lir::LirType::I64),
                debug_info: None,
            });
            index_value = lir::LirValue::Register(cast_id);
        }

        let element_size = Self::size_of_lir_type(&element_lir_ty);
        let offset_value = if element_size == 1 {
            index_value
        } else {
            let scale = lir::LirValue::Constant(lir::LirConstant::Int(
                element_size as i64,
                lir::LirType::I64,
            ));
            let mul_id = self.next_id();
            self.queued_instructions.push(lir::LirInstruction {
                id: mul_id,
                kind: lir::LirInstructionKind::Mul(index_value, scale),
                type_hint: Some(lir::LirType::I64),
                debug_info: None,
            });
            lir::LirValue::Register(mul_id)
        };

        let i8_ptr_ty = lir::LirType::Ptr(Box::new(lir::LirType::I8));
        let base_i8_ptr_id = self.next_id();
        self.queued_instructions.push(lir::LirInstruction {
            id: base_i8_ptr_id,
            kind: lir::LirInstructionKind::Bitcast(base_ptr.clone(), i8_ptr_ty.clone()),
            type_hint: Some(i8_ptr_ty.clone()),
            debug_info: None,
        });
        let base_i8_ptr = lir::LirValue::Register(base_i8_ptr_id);

        let gep_id = self.next_id();
        self.queued_instructions.push(lir::LirInstruction {
            id: gep_id,
            kind: lir::LirInstructionKind::GetElementPtr {
                ptr: base_i8_ptr,
                indices: vec![offset_value],
                inbounds: true,
            },
            type_hint: Some(i8_ptr_ty.clone()),
            debug_info: None,
        });

        let target_ptr_ty = lir::LirType::Ptr(Box::new(element_lir_ty.clone()));
        let cast_id = self.next_id();
        self.queued_instructions.push(lir::LirInstruction {
            id: cast_id,
            kind: lir::LirInstructionKind::Bitcast(
                lir::LirValue::Register(gep_id),
                target_ptr_ty.clone(),
            ),
            type_hint: Some(target_ptr_ty.clone()),
            debug_info: None,
        });

        Ok(PlaceAccess::Address(PlaceAddress {
            ptr: lir::LirValue::Register(cast_id),
            ty: element_ty.clone(),
            lir_ty: element_lir_ty,
            alignment: element_alignment,
        }))
    }

    fn slice_lir_type(&self, elem_lir: &lir::LirType) -> lir::LirType {
        lir::LirType::Struct {
            fields: vec![
                lir::LirType::Ptr(Box::new(elem_lir.clone())),
                lir::LirType::I64,
            ],
            packed: false,
            name: Some("__slice".to_string()),
        }
    }

    fn container_element_lir_type(&self, kind: &mir::ContainerKind) -> lir::LirType {
        match kind {
            mir::ContainerKind::List { elem_ty, .. } => self.lir_type_from_ty(elem_ty),
            mir::ContainerKind::Map {
                key_ty, value_ty, ..
            } => {
                let key_lir = self.lir_type_from_ty(key_ty);
                let value_lir = self.lir_type_from_ty(value_ty);
                lir::LirType::Struct {
                    fields: vec![key_lir, value_lir],
                    packed: false,
                    name: Some("__map_entry".to_string()),
                }
            }
        }
    }

    fn container_len(&self, kind: &mir::ContainerKind) -> u64 {
        match kind {
            mir::ContainerKind::List { len, .. } => *len,
            mir::ContainerKind::Map { len, .. } => *len,
        }
    }

    fn ensure_i64_value(
        &mut self,
        value: lir::LirValue,
        instructions: &mut Vec<lir::LirInstruction>,
    ) -> lir::LirValue {
        let current_ty = self
            .infer_lir_value_type(&value)
            .unwrap_or(lir::LirType::I64);
        if current_ty == lir::LirType::I64 {
            return value;
        }
        let cast_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: cast_id,
            kind: lir::LirInstructionKind::SextOrTrunc(value, lir::LirType::I64),
            type_hint: Some(lir::LirType::I64),
            debug_info: None,
        });
        lir::LirValue::Register(cast_id)
    }

    fn element_ptr_at(
        &mut self,
        base_ptr: lir::LirValue,
        element_ty: &lir::LirType,
        index_value: lir::LirValue,
        instructions: &mut Vec<lir::LirInstruction>,
    ) -> lir::LirValue {
        let index_i64 = self.ensure_i64_value(index_value, instructions);
        let element_size = Self::size_of_lir_type(element_ty);
        let offset_value = if element_size == 1 {
            index_i64
        } else {
            let scale = lir::LirValue::Constant(lir::LirConstant::Int(
                element_size as i64,
                lir::LirType::I64,
            ));
            let mul_id = self.next_id();
            instructions.push(lir::LirInstruction {
                id: mul_id,
                kind: lir::LirInstructionKind::Mul(index_i64, scale),
                type_hint: Some(lir::LirType::I64),
                debug_info: None,
            });
            lir::LirValue::Register(mul_id)
        };

        let i8_ptr_ty = lir::LirType::Ptr(Box::new(lir::LirType::I8));
        let base_i8_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: base_i8_id,
            kind: lir::LirInstructionKind::Bitcast(base_ptr, i8_ptr_ty.clone()),
            type_hint: Some(i8_ptr_ty.clone()),
            debug_info: None,
        });
        let base_i8 = lir::LirValue::Register(base_i8_id);

        let gep_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: gep_id,
            kind: lir::LirInstructionKind::GetElementPtr {
                ptr: base_i8,
                indices: vec![offset_value],
                inbounds: true,
            },
            type_hint: Some(i8_ptr_ty.clone()),
            debug_info: None,
        });

        let elem_ptr_ty = lir::LirType::Ptr(Box::new(element_ty.clone()));
        let cast_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: cast_id,
            kind: lir::LirInstructionKind::Bitcast(
                lir::LirValue::Register(gep_id),
                elem_ptr_ty.clone(),
            ),
            type_hint: Some(elem_ptr_ty),
            debug_info: None,
        });
        lir::LirValue::Register(cast_id)
    }

    fn build_slice_value(
        &mut self,
        ptr: lir::LirValue,
        len: u64,
        elem_lir: &lir::LirType,
        instructions: &mut Vec<lir::LirInstruction>,
    ) -> lir::LirValue {
        let slice_ty = self.slice_lir_type(elem_lir);
        let mut current = lir::LirValue::Constant(lir::LirConstant::Undef(slice_ty.clone()));
        let ptr_insert = self.next_id();
        instructions.push(lir::LirInstruction {
            id: ptr_insert,
            kind: lir::LirInstructionKind::InsertValue {
                aggregate: current,
                element: ptr,
                indices: vec![0],
            },
            type_hint: Some(slice_ty.clone()),
            debug_info: None,
        });
        current = lir::LirValue::Register(ptr_insert);

        let len_value = lir::LirValue::Constant(lir::LirConstant::UInt(len, lir::LirType::I64));
        let len_insert = self.next_id();
        instructions.push(lir::LirInstruction {
            id: len_insert,
            kind: lir::LirInstructionKind::InsertValue {
                aggregate: current,
                element: len_value,
                indices: vec![1],
            },
            type_hint: Some(slice_ty),
            debug_info: None,
        });
        lir::LirValue::Register(len_insert)
    }

    fn slice_element_type(expected: &lir::LirType) -> Option<lir::LirType> {
        let lir::LirType::Struct { fields, name, .. } = expected else {
            return None;
        };
        if name.as_deref() != Some("__slice") || fields.len() != 2 {
            return None;
        }
        if !matches!(fields[1], lir::LirType::I64) {
            return None;
        }
        let lir::LirType::Ptr(elem) = &fields[0] else {
            return None;
        };
        Some((**elem).clone())
    }

    fn extract_slice_field(
        &mut self,
        value: lir::LirValue,
        field_index: u32,
        field_ty: lir::LirType,
        instructions: &mut Vec<lir::LirInstruction>,
    ) -> lir::LirValue {
        let extract_id = self.next_id();
        instructions.push(lir::LirInstruction {
            id: extract_id,
            kind: lir::LirInstructionKind::ExtractValue {
                aggregate: value,
                indices: vec![field_index],
            },
            type_hint: Some(field_ty),
            debug_info: None,
        });
        lir::LirValue::Register(extract_id)
    }

    fn size_of_lir_type(ty: &lir::LirType) -> u64 {
        match ty {
            lir::LirType::I1 | lir::LirType::I8 => 1,
            lir::LirType::I16 => 2,
            lir::LirType::I32 | lir::LirType::F32 => 4,
            lir::LirType::I64 | lir::LirType::F64 => 8,
            lir::LirType::I128 => 16,
            lir::LirType::Ptr(_) => 8,
            lir::LirType::Array(element_ty, len) => {
                Self::size_of_lir_type(element_ty) * (*len as u64)
            }
            lir::LirType::Struct { fields, .. } => fields
                .iter()
                .map(|field| Self::size_of_lir_type(field))
                .sum(),
            _ => 8,
        }
    }

    fn alignment_for_lir_type(ty: &lir::LirType) -> u32 {
        match ty {
            lir::LirType::I1 => 1,
            lir::LirType::I8 => 1,
            lir::LirType::I16 => 2,
            lir::LirType::I32 => 4,
            lir::LirType::I64 => 8,
            lir::LirType::I128 => 16,
            lir::LirType::F32 => 4,
            lir::LirType::F64 => 8,
            lir::LirType::Ptr(_) => 8,
            lir::LirType::Array(element_type, _) => Self::alignment_for_lir_type(element_type),
            lir::LirType::Struct { fields, .. } => fields
                .iter()
                .map(Self::alignment_for_lir_type)
                .max()
                .expect("struct must have at least one field to compute alignment"),
            _ => 8,
        }
    }

    fn emit_load_from_address(
        &mut self,
        addr: PlaceAddress,
        block: &mut lir::LirBasicBlock,
    ) -> lir::LirValue {
        if Self::is_zero_sized(&addr.ty) {
            return lir::LirValue::Constant(lir::LirConstant::Undef(addr.lir_ty));
        }
        let load_id = self.next_id();
        block.instructions.push(lir::LirInstruction {
            id: load_id,
            kind: lir::LirInstructionKind::Load {
                address: addr.ptr,
                alignment: Some(addr.alignment),
                volatile: false,
            },
            type_hint: Some(addr.lir_ty.clone()),
            debug_info: None,
        });
        lir::LirValue::Register(load_id)
    }

    fn materialize_pointer_from_value(
        &mut self,
        value: lir::LirValue,
        value_ty: lir::LirType,
        block: &mut lir::LirBasicBlock,
    ) -> lir::LirValue {
        let alloca_id = self.next_id();
        let pointer_type = lir::LirType::Ptr(Box::new(value_ty.clone()));
        let size_value = lir::LirValue::Constant(lir::LirConstant::Int(1, lir::LirType::I32));
        let alignment = Self::alignment_for_lir_type(&value_ty);
        block.instructions.push(lir::LirInstruction {
            id: alloca_id,
            kind: lir::LirInstructionKind::Alloca {
                size: size_value,
                alignment,
            },
            type_hint: Some(pointer_type.clone()),
            debug_info: None,
        });

        let ptr_value = lir::LirValue::Register(alloca_id);
        let store_id = self.next_id();
        block.instructions.push(lir::LirInstruction {
            id: store_id,
            kind: lir::LirInstructionKind::Store {
                value,
                address: ptr_value.clone(),
                alignment: Some(alignment),
                volatile: false,
            },
            type_hint: None,
            debug_info: None,
        });

        ptr_value
    }

    fn adjust_call_argument(
        &mut self,
        value: lir::LirValue,
        source_ty: Option<&Ty>,
        source_lir_ty: &lir::LirType,
        expected_ty: Option<&lir::LirType>,
        block: &mut lir::LirBasicBlock,
    ) -> lir::LirValue {
        if let Some(expected) = expected_ty {
            if let (Some(elem_lir_ty), lir::LirType::Array(_, len)) =
                (Self::slice_element_type(expected), source_lir_ty)
            {
                return self.build_slice_from_array_value(value, elem_lir_ty, *len, block);
            }
            if matches!(expected, lir::LirType::Ptr(_)) {
                if let lir::LirValue::Constant(constant) = &value {
                    match constant {
                        lir::LirConstant::UInt(v, _) if *v == 0 => {
                            return lir::LirValue::Constant(lir::LirConstant::Null(
                                expected.clone(),
                            ));
                        }
                        lir::LirConstant::Int(v, _) if *v == 0 => {
                            return lir::LirValue::Constant(lir::LirConstant::Null(
                                expected.clone(),
                            ));
                        }
                        _ => {}
                    }
                }
            }

            if matches!(source_lir_ty, lir::LirType::Void) {
                return if matches!(expected, lir::LirType::Ptr(_)) {
                    lir::LirValue::Constant(lir::LirConstant::Null(expected.clone()))
                } else {
                    lir::LirValue::Constant(lir::LirConstant::Undef(expected.clone()))
                };
            }

            if source_lir_ty == expected {
                return value;
            }
            return self.cast_value_to_type(value, source_lir_ty.clone(), expected.clone(), block);
        }

        self.promote_vararg_argument(value, source_ty, source_lir_ty, block)
    }

    fn build_slice_from_array_ptr(
        &mut self,
        array_ptr: lir::LirValue,
        elem_lir_ty: lir::LirType,
        len: u64,
        block: &mut lir::LirBasicBlock,
    ) -> lir::LirValue {
        let slice_ty = self.slice_lir_type(&elem_lir_ty);
        let zero = lir::LirValue::Constant(lir::LirConstant::Int(0, lir::LirType::I64));

        let gep_id = self.next_id();
        block.instructions.push(lir::LirInstruction {
            id: gep_id,
            kind: lir::LirInstructionKind::GetElementPtr {
                ptr: array_ptr,
                indices: vec![zero.clone(), zero],
                inbounds: true,
            },
            type_hint: Some(lir::LirType::Ptr(Box::new(lir::LirType::Array(
                Box::new(elem_lir_ty.clone()),
                len,
            )))),
            debug_info: None,
        });
        let elem_ptr = lir::LirValue::Register(gep_id);

        let insert_ptr_id = self.next_id();
        block.instructions.push(lir::LirInstruction {
            id: insert_ptr_id,
            kind: lir::LirInstructionKind::InsertValue {
                aggregate: lir::LirValue::Constant(lir::LirConstant::Undef(slice_ty.clone())),
                element: elem_ptr,
                indices: vec![0],
            },
            type_hint: Some(slice_ty.clone()),
            debug_info: None,
        });

        let len_value = lir::LirValue::Constant(lir::LirConstant::UInt(len, lir::LirType::I64));
        let insert_len_id = self.next_id();
        block.instructions.push(lir::LirInstruction {
            id: insert_len_id,
            kind: lir::LirInstructionKind::InsertValue {
                aggregate: lir::LirValue::Register(insert_ptr_id),
                element: len_value,
                indices: vec![1],
            },
            type_hint: Some(slice_ty.clone()),
            debug_info: None,
        });

        lir::LirValue::Register(insert_len_id)
    }

    fn build_slice_from_array_value(
        &mut self,
        value: lir::LirValue,
        elem_lir_ty: lir::LirType,
        len: u64,
        block: &mut lir::LirBasicBlock,
    ) -> lir::LirValue {
        let array_ty = lir::LirType::Array(Box::new(elem_lir_ty.clone()), len);
        let array_ptr = self.materialize_pointer_from_value(value, array_ty, block);
        self.build_slice_from_array_ptr(array_ptr, elem_lir_ty, len, block)
    }

    fn promote_vararg_argument(
        &mut self,
        value: lir::LirValue,
        source_ty: Option<&Ty>,
        source_lir_ty: &lir::LirType,
        block: &mut lir::LirBasicBlock,
    ) -> lir::LirValue {
        match source_lir_ty {
            lir::LirType::I1 | lir::LirType::I8 | lir::LirType::I16 => {
                let signed = matches!(source_ty.map(|ty| &ty.kind), Some(TyKind::Int(_)));
                self.extend_integer_value(
                    value,
                    source_lir_ty.clone(),
                    lir::LirType::I32,
                    signed,
                    block,
                )
            }
            lir::LirType::F32 => self.extend_float_value(value, lir::LirType::F64, block),
            _ => value,
        }
    }

    fn cast_value_to_type(
        &mut self,
        value: lir::LirValue,
        from_ty: lir::LirType,
        target_ty: lir::LirType,
        block: &mut lir::LirBasicBlock,
    ) -> lir::LirValue {
        if matches!(target_ty, lir::LirType::Void) {
            return lir::LirValue::Undef(target_ty);
        }
        if from_ty == target_ty {
            return value;
        }
        if let Some(const_value) = self.cast_constant_value(&value, &target_ty) {
            return const_value;
        }
        if self.is_float_type(&from_ty) && matches!(target_ty, lir::LirType::Ptr(_)) {
            let int_ty = lir::LirType::I64;
            let fp_to_int_id = self.next_id();
            block.instructions.push(lir::LirInstruction {
                id: fp_to_int_id,
                kind: lir::LirInstructionKind::FPToSI(value.clone(), int_ty.clone()),
                type_hint: Some(int_ty.clone()),
                debug_info: None,
            });
            let ptr_id = self.next_id();
            block.instructions.push(lir::LirInstruction {
                id: ptr_id,
                kind: lir::LirInstructionKind::IntToPtr(lir::LirValue::Register(fp_to_int_id)),
                type_hint: Some(target_ty.clone()),
                debug_info: None,
            });
            return lir::LirValue::Register(ptr_id);
        }
        if matches!(from_ty, lir::LirType::Ptr(_)) && self.is_float_type(&target_ty) {
            let int_ty = lir::LirType::I64;
            let ptr_to_int_id = self.next_id();
            block.instructions.push(lir::LirInstruction {
                id: ptr_to_int_id,
                kind: lir::LirInstructionKind::PtrToInt(value.clone()),
                type_hint: Some(int_ty.clone()),
                debug_info: None,
            });
            let fp_id = self.next_id();
            block.instructions.push(lir::LirInstruction {
                id: fp_id,
                kind: lir::LirInstructionKind::SIToFP(
                    lir::LirValue::Register(ptr_to_int_id),
                    target_ty.clone(),
                ),
                type_hint: Some(target_ty.clone()),
                debug_info: None,
            });
            return lir::LirValue::Register(fp_id);
        }
        let id = self.next_id();
        let kind = if matches!(from_ty, lir::LirType::Ptr(_)) && self.is_integral_type(&target_ty) {
            lir::LirInstructionKind::PtrToInt(value.clone())
        } else if self.is_integral_type(&from_ty) && matches!(target_ty, lir::LirType::Ptr(_)) {
            lir::LirInstructionKind::IntToPtr(value.clone())
        } else if self.is_integral_type(&from_ty) && self.is_integral_type(&target_ty) {
            let src_w = self.type_bit_width(&from_ty);
            let dst_w = self.type_bit_width(&target_ty);
            if src_w == dst_w {
                lir::LirInstructionKind::Bitcast(value.clone(), target_ty.clone())
            } else {
                lir::LirInstructionKind::SextOrTrunc(value.clone(), target_ty.clone())
            }
        } else if self.is_float_type(&from_ty) && self.is_float_type(&target_ty) {
            let src_w = self.type_bit_width(&from_ty);
            let dst_w = self.type_bit_width(&target_ty);
            match (src_w, dst_w) {
                (Some(s), Some(d)) if d > s => {
                    lir::LirInstructionKind::FPExt(value.clone(), target_ty.clone())
                }
                (Some(s), Some(d)) if d < s => {
                    lir::LirInstructionKind::FPTrunc(value.clone(), target_ty.clone())
                }
                _ => lir::LirInstructionKind::Bitcast(value.clone(), target_ty.clone()),
            }
        } else if self.is_float_type(&from_ty) && self.is_integral_type(&target_ty) {
            lir::LirInstructionKind::FPToSI(value.clone(), target_ty.clone())
        } else if self.is_integral_type(&from_ty) && self.is_float_type(&target_ty) {
            lir::LirInstructionKind::SIToFP(value.clone(), target_ty.clone())
        } else {
            lir::LirInstructionKind::Bitcast(value.clone(), target_ty.clone())
        };
        block.instructions.push(lir::LirInstruction {
            id,
            kind,
            type_hint: Some(target_ty),
            debug_info: None,
        });
        lir::LirValue::Register(id)
    }

    fn cast_constant_value(
        &self,
        value: &lir::LirValue,
        target_ty: &lir::LirType,
    ) -> Option<lir::LirValue> {
        match value {
            lir::LirValue::Constant(constant) => {
                if matches!(target_ty, lir::LirType::Ptr(_)) {
                    match constant {
                        lir::LirConstant::Int(val, _) if *val == 0 => {
                            return Some(lir::LirValue::Null(target_ty.clone()));
                        }
                        lir::LirConstant::UInt(val, _) if *val == 0 => {
                            return Some(lir::LirValue::Null(target_ty.clone()));
                        }
                        _ => {}
                    }
                }
                self.cast_constant_literal(constant, target_ty)
                    .map(lir::LirValue::Constant)
            }
            lir::LirValue::Null(_) => match target_ty {
                lir::LirType::Ptr(_) => Some(lir::LirValue::Null(target_ty.clone())),
                ty if self.is_integral_type(ty) => Some(lir::LirValue::Constant(
                    lir::LirConstant::Int(0, ty.clone()),
                )),
                ty if self.is_float_type(ty) => Some(lir::LirValue::Constant(
                    lir::LirConstant::Float(0.0, ty.clone()),
                )),
                _ => None,
            },
            _ => None,
        }
    }

    fn cast_constant_literal(
        &self,
        constant: &lir::LirConstant,
        target_ty: &lir::LirType,
    ) -> Option<lir::LirConstant> {
        match (constant, target_ty) {
            (lir::LirConstant::Int(value, _), ty) if self.is_integral_type(ty) => {
                Some(lir::LirConstant::Int(*value, ty.clone()))
            }
            (lir::LirConstant::UInt(value, _), ty) if self.is_integral_type(ty) => {
                Some(lir::LirConstant::UInt(*value, ty.clone()))
            }
            (lir::LirConstant::Bool(value), ty) if self.is_integral_type(ty) => Some(
                lir::LirConstant::Int(if *value { 1 } else { 0 }, ty.clone()),
            ),
            (lir::LirConstant::Float(value, _), ty) if self.is_integral_type(ty) => {
                Some(lir::LirConstant::Int(*value as i64, ty.clone()))
            }
            (lir::LirConstant::Int(value, _), ty) if self.is_float_type(ty) => {
                Some(lir::LirConstant::Float(*value as f64, ty.clone()))
            }
            (lir::LirConstant::UInt(value, _), ty) if self.is_float_type(ty) => {
                Some(lir::LirConstant::Float(*value as f64, ty.clone()))
            }
            (lir::LirConstant::Bool(value), ty) if self.is_float_type(ty) => Some(
                lir::LirConstant::Float(if *value { 1.0 } else { 0.0 }, ty.clone()),
            ),
            (lir::LirConstant::Float(value, _), ty) if self.is_float_type(ty) => {
                Some(lir::LirConstant::Float(*value, ty.clone()))
            }
            _ => None,
        }
    }

    fn extend_integer_value(
        &mut self,
        value: lir::LirValue,
        from_ty: lir::LirType,
        target_ty: lir::LirType,
        signed: bool,
        block: &mut lir::LirBasicBlock,
    ) -> lir::LirValue {
        if from_ty == target_ty {
            return value;
        }
        let id = self.next_id();
        let kind = if signed {
            lir::LirInstructionKind::SExt(value.clone(), target_ty.clone())
        } else {
            lir::LirInstructionKind::ZExt(value.clone(), target_ty.clone())
        };
        block.instructions.push(lir::LirInstruction {
            id,
            kind,
            type_hint: Some(target_ty),
            debug_info: None,
        });
        lir::LirValue::Register(id)
    }

    fn extend_float_value(
        &mut self,
        value: lir::LirValue,
        target_ty: lir::LirType,
        block: &mut lir::LirBasicBlock,
    ) -> lir::LirValue {
        let id = self.next_id();
        block.instructions.push(lir::LirInstruction {
            id,
            kind: lir::LirInstructionKind::FPExt(value.clone(), target_ty.clone()),
            type_hint: Some(target_ty),
            debug_info: None,
        });
        lir::LirValue::Register(id)
    }

    fn infer_lir_value_type(&self, value: &lir::LirValue) -> Option<lir::LirType> {
        match value {
            lir::LirValue::Constant(constant) => match constant {
                lir::LirConstant::Int(_, ty)
                | lir::LirConstant::UInt(_, ty)
                | lir::LirConstant::Float(_, ty)
                | lir::LirConstant::Array(_, ty)
                | lir::LirConstant::Struct(_, ty)
                | lir::LirConstant::GlobalRef(_, ty, _)
                | lir::LirConstant::FunctionRef(_, ty)
                | lir::LirConstant::Null(ty)
                | lir::LirConstant::Undef(ty) => Some(ty.clone()),
                lir::LirConstant::Bool(_) => Some(lir::LirType::I1),
                lir::LirConstant::String(_) => Some(lir::LirType::Ptr(Box::new(lir::LirType::I8))),
            },
            _ => None,
        }
    }

    fn take_queued_instructions(&mut self) -> Vec<lir::LirInstruction> {
        std::mem::take(&mut self.queued_instructions)
    }

    fn next_id(&mut self) -> lir::LirId {
        let id = self.next_lir_id;
        self.next_lir_id += 1;
        id
    }

    fn handle_aggregate(
        &mut self,
        place: &mir::Place,
        kind: &mir::AggregateKind,
        fields: &[mir::Operand],
    ) -> Result<(Vec<lir::LirInstruction>, Option<lir::LirValue>)> {
        let mut instructions = Vec::new();
        let mut raw_values = Vec::with_capacity(fields.len());
        let mut constants = Vec::with_capacity(fields.len());
        // Track operand types so we can coerce register values into aggregate field types.
        // Without this, registers have no local type info and we can emit invalid insertvalue
        // operands (e.g. inserting i64 into a ptr field).
        let mut operand_types = Vec::with_capacity(fields.len());
        let mut all_constants = true;

        for operand in fields {
            let value = self.transform_operand(operand)?;
            instructions.extend(self.take_queued_instructions());
            operand_types.push(self.type_of_operand(operand));
            let is_constant = matches!(value, lir::LirValue::Constant(_));
            if let lir::LirValue::Constant(ref constant) = value {
                constants.push(constant.clone());
            }
            if !is_constant {
                all_constants = false;
            }
            raw_values.push(value);
        }

        let place_ty = self.lookup_place_type(place);
        let aggregate_ty = place_ty.as_ref().map(|ty| self.lir_type_from_ty(ty));
        let mut expected_field_tys = self.expected_aggregate_element_types(
            place_ty.as_ref(),
            aggregate_ty.as_ref(),
            raw_values.len(),
        );

        // If we could not infer field types from place/aggregate, derive them from operands.
        // This avoids falling back to Ptr(I8) for non-constant array elements (e.g. `-1`),
        // which can corrupt aggregate layouts and cause invalid insertvalue operands.
        if expected_field_tys.is_empty()
            || (matches!(aggregate_ty, Some(lir::LirType::Ptr(_)))
                && raw_values.len() == expected_field_tys.len()
                && expected_field_tys
                    .iter()
                    .all(|t| matches!(t, lir::LirType::Ptr(_))))
        {
            expected_field_tys = operand_types
                .iter()
                .zip(raw_values.iter())
                .map(|(operand_ty, value)| {
                    operand_ty
                        .clone()
                        .or_else(|| self.infer_lir_value_type(value))
                        .unwrap_or(lir::LirType::Ptr(Box::new(lir::LirType::I8)))
                })
                .collect();
        }

        for (idx, ty) in expected_field_tys.iter_mut().enumerate() {
            if matches!(ty, lir::LirType::Void) {
                if let Some(operand) = fields.get(idx) {
                    if let Some(operand_ty) = self.type_of_operand(operand) {
                        *ty = operand_ty;
                    }
                }
            }
        }

        if fields.is_empty() {
            if let Some(lir_ty) = aggregate_ty {
                let value = match lir_ty {
                    lir::LirType::Struct { .. } => {
                        lir::LirValue::Constant(lir::LirConstant::Struct(Vec::new(), lir_ty))
                    }
                    lir::LirType::Array(elem, _len) => {
                        lir::LirValue::Constant(lir::LirConstant::Array(Vec::new(), *elem))
                    }
                    _ => {
                        // Non-aggregate zero-field values should not emit struct constants.
                        return Ok((instructions, None));
                    }
                };
                return Ok((instructions, Some(value)));
            }
            return Ok((instructions, None));
        }

        if all_constants {
            let adjusted_consts =
                self.adjust_constants_for_aggregate(constants, &expected_field_tys);
            if let Some(place_ty) = place_ty.as_ref() {
                if let Some(constant) =
                    self.constant_from_aggregate(kind, adjusted_consts, place_ty)
                {
                    return Ok((instructions, Some(lir::LirValue::Constant(constant))));
                }
            }
        }

        // Choose an aggregate type suitable for InsertValue construction.
        // Prefer a real struct/array type; otherwise synthesize a struct from expected fields.
        let agg_construction_ty: Option<lir::LirType> =
            if matches!(kind, mir::AggregateKind::Array(_)) {
                match aggregate_ty.clone() {
                    Some(lir::LirType::Array(elem, _n)) => {
                        Some(lir::LirType::Array(elem, raw_values.len() as u64))
                    }
                    _ => {
                        let elem_ty = expected_field_tys
                            .get(0)
                            .cloned()
                            .unwrap_or(lir::LirType::I64);
                        Some(lir::LirType::Array(
                            Box::new(elem_ty),
                            raw_values.len() as u64,
                        ))
                    }
                }
            } else {
                match aggregate_ty.clone() {
                    Some(lir::LirType::Struct {
                        fields,
                        packed,
                        name,
                    }) => {
                        if fields.len() == raw_values.len() {
                            Some(lir::LirType::Struct {
                                fields,
                                packed,
                                name,
                            })
                        } else {
                            Some(lir::LirType::Struct {
                                fields: expected_field_tys.clone(),
                                packed: false,
                                name: None,
                            })
                        }
                    }
                    Some(lir::LirType::Array(elem, _n)) => {
                        Some(lir::LirType::Array(elem, raw_values.len() as u64))
                    }
                    Some(_other) => {
                        // Not an aggregate; synthesize a struct if multiple fields; if single field, just return it below
                        if raw_values.len() > 1 {
                            Some(lir::LirType::Struct {
                                fields: expected_field_tys.clone(),
                                packed: false,
                                name: None,
                            })
                        } else {
                            None
                        }
                    }
                    None => {
                        if raw_values.len() > 1 {
                            Some(lir::LirType::Struct {
                                fields: expected_field_tys.clone(),
                                packed: false,
                                name: None,
                            })
                        } else {
                            None
                        }
                    }
                }
            };

        if let Some(agg_ty) = agg_construction_ty {
            let mut current_value =
                lir::LirValue::Constant(lir::LirConstant::Undef(agg_ty.clone()));

            for (index, value) in raw_values.into_iter().enumerate() {
                let mut element = value;
                if let Some(field_ty) = expected_field_tys.get(index) {
                    let source_ty = operand_types.get(index).and_then(|ty| ty.clone());
                    element = self.coerce_aggregate_value_with_source(
                        element,
                        source_ty.as_ref(),
                        field_ty,
                        &mut instructions,
                    );
                }
                let instr_id = self.next_id();
                instructions.push(lir::LirInstruction {
                    id: instr_id,
                    kind: lir::LirInstructionKind::InsertValue {
                        aggregate: current_value.clone(),
                        element,
                        indices: vec![index as u32],
                    },
                    type_hint: Some(agg_ty.clone()),
                    debug_info: None,
                });
                current_value = lir::LirValue::Register(instr_id);
            }

            return Ok((instructions, Some(current_value)));
        }

        // If we couldn't build an aggregate and there is exactly one element, return it directly
        if raw_values.len() == 1 {
            return Ok((instructions, raw_values.into_iter().next()));
        }

        Ok((instructions, None))
    }

    fn expected_aggregate_element_types(
        &self,
        place_ty: Option<&Ty>,
        aggregate_ty: Option<&lir::LirType>,
        element_count: usize,
    ) -> Vec<lir::LirType> {
        if let Some(ty) = place_ty {
            match &ty.kind {
                TyKind::Tuple(elements) => {
                    if elements.len() == element_count {
                        return elements
                            .iter()
                            .map(|elem| self.lir_type_from_ty(elem))
                            .collect();
                    }
                }
                TyKind::Array(element_ty, _) => {
                    let lir_elem_ty = self.lir_type_from_ty(element_ty);
                    return (0..element_count).map(|_| lir_elem_ty.clone()).collect();
                }
                _ => {}
            }
        }

        if let Some(lir::LirType::Struct { fields, .. }) = aggregate_ty {
            if fields.len() == element_count {
                return fields.clone();
            }
        }

        if let Some(lir::LirType::Array(element_ty, _)) = aggregate_ty {
            let elem_ty = *element_ty.clone();
            return (0..element_count).map(|_| elem_ty.clone()).collect();
        }

        aggregate_ty
            .cloned()
            .map(|ty| (0..element_count).map(|_| ty.clone()).collect())
            .unwrap_or_default()
    }

    fn adjust_constants_for_aggregate(
        &self,
        constants: Vec<lir::LirConstant>,
        expected_field_tys: &[lir::LirType],
    ) -> Vec<lir::LirConstant> {
        constants
            .into_iter()
            .enumerate()
            .map(|(index, constant)| {
                if let Some(field_ty) = expected_field_tys.get(index) {
                    self.cast_constant_to_lir_type(constant, field_ty)
                } else {
                    constant
                }
            })
            .collect()
    }

    fn coerce_aggregate_value_with_source(
        &mut self,
        value: lir::LirValue,
        source_ty: Option<&lir::LirType>,
        target_ty: &lir::LirType,
        instructions: &mut Vec<lir::LirInstruction>,
    ) -> lir::LirValue {
        match value {
            lir::LirValue::Constant(constant) => {
                lir::LirValue::Constant(self.cast_constant_to_lir_type(constant, target_ty))
            }
            other => self.cast_runtime_value_to_lir_type_with_source(
                other,
                source_ty,
                target_ty.clone(),
                instructions,
            ),
        }
    }

    fn cast_runtime_value_to_lir_type_with_source(
        &mut self,
        value: lir::LirValue,
        source_ty: Option<&lir::LirType>,
        target_ty: lir::LirType,
        instructions: &mut Vec<lir::LirInstruction>,
    ) -> lir::LirValue {
        let current_ty = source_ty
            .cloned()
            .or_else(|| self.infer_lir_value_type(&value));
        if let Some(current_ty) = current_ty {
            if current_ty == target_ty {
                return value;
            }

            if self.is_integral_type(&current_ty) && self.is_integral_type(&target_ty) {
                let current_bits = self.type_bit_width(&current_ty).unwrap_or(64);
                let target_bits = self.type_bit_width(&target_ty).unwrap_or(64);
                let instr_id = self.next_id();
                let kind = if target_bits < current_bits {
                    lir::LirInstructionKind::Trunc(value.clone(), target_ty.clone())
                } else if target_bits > current_bits {
                    lir::LirInstructionKind::ZExt(value.clone(), target_ty.clone())
                } else {
                    lir::LirInstructionKind::Bitcast(value.clone(), target_ty.clone())
                };
                instructions.push(lir::LirInstruction {
                    id: instr_id,
                    kind,
                    type_hint: Some(target_ty.clone()),
                    debug_info: None,
                });
                return lir::LirValue::Register(instr_id);
            }

            if self.is_float_type(&current_ty) && self.is_float_type(&target_ty) {
                let current_bits = self.type_bit_width(&current_ty).unwrap_or(64);
                let target_bits = self.type_bit_width(&target_ty).unwrap_or(64);
                let instr_id = self.next_id();
                let kind = if target_bits > current_bits {
                    lir::LirInstructionKind::FPExt(value.clone(), target_ty.clone())
                } else if target_bits < current_bits {
                    lir::LirInstructionKind::FPTrunc(value.clone(), target_ty.clone())
                } else {
                    lir::LirInstructionKind::Bitcast(value.clone(), target_ty.clone())
                };
                instructions.push(lir::LirInstruction {
                    id: instr_id,
                    kind,
                    type_hint: Some(target_ty.clone()),
                    debug_info: None,
                });
                return lir::LirValue::Register(instr_id);
            }

            // Pointer/integer interop for bootstrap tolerance
            let current_is_int = self.is_integral_type(&current_ty);
            let target_is_int = self.is_integral_type(&target_ty);
            let current_is_ptr = matches!(&current_ty, lir::LirType::Ptr(_));
            let target_is_ptr = matches!(&target_ty, lir::LirType::Ptr(_));
            if current_is_int && target_is_ptr {
                let instr_id = self.next_id();
                instructions.push(lir::LirInstruction {
                    id: instr_id,
                    kind: lir::LirInstructionKind::IntToPtr(value.clone()),
                    type_hint: Some(target_ty.clone()),
                    debug_info: None,
                });
                return lir::LirValue::Register(instr_id);
            }
            if current_is_ptr && target_is_int {
                let instr_id = self.next_id();
                instructions.push(lir::LirInstruction {
                    id: instr_id,
                    kind: lir::LirInstructionKind::PtrToInt(value.clone()),
                    type_hint: Some(target_ty.clone()),
                    debug_info: None,
                });
                return lir::LirValue::Register(instr_id);
            }
        }

        value
    }

    fn cast_constant_to_lir_type(
        &self,
        constant: lir::LirConstant,
        target_ty: &lir::LirType,
    ) -> lir::LirConstant {
        match constant {
            lir::LirConstant::String(value)
                if matches!(target_ty, lir::LirType::Struct { fields, .. } if fields.len() == 2
                    && matches!(&fields[0], lir::LirType::Ptr(inner) if **inner == lir::LirType::I8)
                    && fields[1] == lir::LirType::I64) =>
            {
                lir::LirConstant::Struct(
                    vec![
                        lir::LirConstant::String(value.clone()),
                        lir::LirConstant::UInt(value.len() as u64, lir::LirType::I64),
                    ],
                    target_ty.clone(),
                )
            }
            lir::LirConstant::Int(0, _) if matches!(target_ty, lir::LirType::Ptr(_)) => {
                lir::LirConstant::Null(target_ty.clone())
            }
            lir::LirConstant::UInt(0, _) if matches!(target_ty, lir::LirType::Ptr(_)) => {
                lir::LirConstant::Null(target_ty.clone())
            }
            lir::LirConstant::Int(value, _) if self.is_integral_type(target_ty) => {
                let bits = self.type_bit_width(target_ty).unwrap_or(64);
                let adjusted = if bits >= 64 {
                    value
                } else {
                    let shift = 64 - bits;
                    (value << shift) >> shift
                };
                lir::LirConstant::Int(adjusted, target_ty.clone())
            }
            lir::LirConstant::UInt(value, _) if self.is_integral_type(target_ty) => {
                let bits = self.type_bit_width(target_ty).unwrap_or(64);
                let mask = if bits >= 64 {
                    u64::MAX
                } else if bits == 0 {
                    0
                } else {
                    (1u64 << bits) - 1
                };
                let adjusted = value & mask;
                lir::LirConstant::UInt(adjusted, target_ty.clone())
            }
            lir::LirConstant::Float(value, _) if self.is_float_type(target_ty) => match target_ty {
                lir::LirType::F32 => {
                    lir::LirConstant::Float((value as f32) as f64, target_ty.clone())
                }
                lir::LirType::F64 => lir::LirConstant::Float(value, target_ty.clone()),
                _ => lir::LirConstant::Float(value, target_ty.clone()),
            },
            lir::LirConstant::GlobalRef(name, _, indices)
                if matches!(target_ty, lir::LirType::Ptr(_)) =>
            {
                lir::LirConstant::GlobalRef(name, target_ty.clone(), indices)
            }
            lir::LirConstant::Struct(fields, _) => {
                if let lir::LirType::Struct {
                    fields: target_fields,
                    ..
                } = target_ty
                {
                    let adjusted = fields
                        .into_iter()
                        .enumerate()
                        .map(|(idx, field_const)| {
                            let field_ty =
                                target_fields.get(idx).cloned().unwrap_or(lir::LirType::I64);
                            self.cast_constant_to_lir_type(field_const, &field_ty)
                        })
                        .collect();
                    lir::LirConstant::Struct(adjusted, target_ty.clone())
                } else if fields.len() == 1 {
                    let field = fields.into_iter().next().expect("len checked");
                    self.cast_constant_to_lir_type(field, target_ty)
                } else {
                    lir::LirConstant::Struct(fields, target_ty.clone())
                }
            }
            lir::LirConstant::Array(elements, _) => {
                if let lir::LirType::Array(element_ty, _) = target_ty {
                    let adjusted = elements
                        .into_iter()
                        .map(|elem| self.cast_constant_to_lir_type(elem, element_ty))
                        .collect();
                    lir::LirConstant::Array(adjusted, (*element_ty.as_ref()).clone())
                } else {
                    lir::LirConstant::Array(elements, target_ty.clone())
                }
            }
            lir::LirConstant::Null(_) => lir::LirConstant::Null(target_ty.clone()),
            lir::LirConstant::Undef(_) => lir::LirConstant::Undef(target_ty.clone()),
            other => other,
        }
    }

    fn lower_call_argument(
        &mut self,
        operand: &mir::Operand,
        expected_ty: Option<&lir::LirType>,
        block: &mut lir::LirBasicBlock,
    ) -> Result<lir::LirValue> {
        let expects_pointer = matches!(expected_ty, Some(lir::LirType::Ptr(_)));
        match operand {
            mir::Operand::Move(place) | mir::Operand::Copy(place) => {
                let access = self.resolve_place(place)?;
                block.instructions.extend(self.take_queued_instructions());
                match access {
                    PlaceAccess::Address(addr) => {
                        if let Some(expected) = expected_ty {
                            if let (Some(elem_lir_ty), TyKind::Array(_, len)) =
                                (Self::slice_element_type(expected), &addr.ty.kind)
                            {
                                let length = self.array_length_from_const(len);
                                return Ok(self.build_slice_from_array_ptr(
                                    addr.ptr,
                                    elem_lir_ty,
                                    length,
                                    block,
                                ));
                            }
                        }
                        if expects_pointer {
                            if matches!(addr.lir_ty, lir::LirType::Ptr(_)) {
                                Ok(self.emit_load_from_address(addr.clone(), block))
                            } else {
                                Ok(addr.ptr)
                            }
                        } else {
                            let loaded = self.emit_load_from_address(addr.clone(), block);
                            Ok(self.adjust_call_argument(
                                loaded,
                                Some(&addr.ty),
                                &addr.lir_ty,
                                expected_ty,
                                block,
                            ))
                        }
                    }
                    PlaceAccess::Value { value, ty, lir_ty } => {
                        if expects_pointer {
                            if matches!(lir_ty, lir::LirType::Ptr(_)) {
                                Ok(value)
                            } else {
                                Ok(self.materialize_pointer_from_value(value, lir_ty, block))
                            }
                        } else {
                            Ok(self.adjust_call_argument(
                                value,
                                Some(&ty),
                                &lir_ty,
                                expected_ty,
                                block,
                            ))
                        }
                    }
                }
            }
            _ => {
                let value = self.transform_operand(operand)?;
                block.instructions.extend(self.take_queued_instructions());
                let value_ty = self.infer_lir_value_type(&value);
                let adjusted = if let Some(lir_ty) = value_ty {
                    self.adjust_call_argument(value, None, &lir_ty, expected_ty, block)
                } else {
                    value
                };
                Ok(adjusted)
            }
        }
    }

    fn build_lir_locals(&self, mir_body: &mir::Body) -> Vec<lir::LirLocal> {
        let arg_count = mir_body.arg_count;
        mir_body
            .locals
            .iter()
            .enumerate()
            .map(|(idx, decl)| lir::LirLocal {
                id: idx as u32,
                ty: self.lir_type_from_ty(&decl.ty),
                name: None,
                is_argument: idx > 0 && idx <= arg_count,
            })
            .collect()
    }

    fn seed_argument_registers(&mut self, mir_body: &mir::Body) {
        for arg_index in 0..mir_body.arg_count {
            let local_id = (arg_index as mir::LocalId) + 1;
            self.register_map
                .entry(local_id)
                .or_insert_with(|| lir::LirValue::Local(local_id));
        }
    }

    fn populate_block_edges(&self, blocks: &mut Vec<lir::LirBasicBlock>) {
        let mut predecessors: HashMap<lir::BasicBlockId, Vec<lir::BasicBlockId>> = HashMap::new();

        for block in blocks.iter_mut() {
            let successors = Self::successors_from_terminator(&block.terminator);
            block.successors = successors.clone();
            for succ in successors {
                predecessors.entry(succ).or_default().push(block.id);
            }
        }

        for block in blocks.iter_mut() {
            if let Some(preds) = predecessors.remove(&block.id) {
                block.predecessors = preds;
            }
        }
    }

    fn successors_from_terminator(terminator: &lir::LirTerminator) -> Vec<lir::BasicBlockId> {
        match terminator {
            lir::LirTerminator::Br(target) => vec![*target],
            lir::LirTerminator::CondBr {
                if_true, if_false, ..
            } => vec![*if_true, *if_false],
            lir::LirTerminator::Switch { default, cases, .. } => {
                let mut targets: Vec<lir::BasicBlockId> = cases.iter().map(|(_, bb)| *bb).collect();
                targets.push(*default);
                targets.sort_unstable();
                targets.dedup();
                targets
            }
            _ => Vec::new(),
        }
    }

    fn transform_call_terminator(
        &mut self,
        func: &mir::Operand,
        args: &[mir::Operand],
        destination: &Option<(mir::Place, mir::BasicBlockId)>,
        cleanup: &Option<mir::BasicBlockId>,
        block: &mut lir::LirBasicBlock,
    ) -> Result<lir::LirTerminator> {
        let mut function_value = self.transform_operand(func)?;
        block.instructions.extend(self.take_queued_instructions());

        function_value = self.normalize_callee_value(func, function_value);
        let callee_name = match &function_value {
            lir::LirValue::Function(name) => Some(name.clone()),
            _ => None,
        };
        let expected_params = callee_name
            .as_ref()
            .and_then(|name| self.function_signatures.get(name))
            .map(|sig| sig.params.clone());
        let signature_return = callee_name
            .as_ref()
            .and_then(|name| self.function_signatures.get(name))
            .map(|sig| sig.return_type.clone());

        let mut lowered_args = Vec::with_capacity(args.len());
        for (idx, arg) in args.iter().enumerate() {
            let expected_ty = expected_params.as_ref().and_then(|params| params.get(idx));
            let value = self.lower_call_argument(arg, expected_ty, block)?;
            lowered_args.push(value);
        }

        let call_id = self.next_id();
        let mut result_type = destination
            .as_ref()
            .and_then(|(place, _)| self.lookup_place_type(place))
            .map(|ty| self.lir_type_from_ty(&ty));
        if let Some(sig_ty) = signature_return.clone() {
            result_type = Some(sig_ty);
        }

        if cleanup.is_some() {
            let Some((_, dest_bb)) = destination.as_ref() else {
                return Err(fp_core::error::Error::from(
                    "invoke lowering requires a destination basic block",
                ));
            };
            let unwind_bb = cleanup.expect("invoke lowering requires a cleanup block");
            return Ok(lir::LirTerminator::Invoke {
                function: function_value,
                args: lowered_args,
                normal_dest: *dest_bb,
                unwind_dest: unwind_bb,
                calling_convention: lir::CallingConvention::C,
            });
        }

        block.instructions.push(lir::LirInstruction {
            id: call_id,
            kind: lir::LirInstructionKind::Call {
                function: function_value,
                args: lowered_args,
                calling_convention: lir::CallingConvention::C,
                tail_call: false,
            },
            type_hint: result_type
                .clone()
                .filter(|ty| !matches!(ty, lir::LirType::Void)),
            debug_info: None,
        });

        if let Some((dest_place, dest_bb)) = destination.as_ref() {
            match result_type {
                Some(ref ty) if !matches!(ty, lir::LirType::Void) => {
                    self.register_map
                        .insert(dest_place.local, lir::LirValue::Register(call_id));

                    if let Some(storage) = self.local_storage.get(&dest_place.local) {
                        let ptr = storage.ptr_value.clone();
                        let alignment = storage.alignment;
                        block.instructions.push(lir::LirInstruction {
                            id: self.next_id(),
                            kind: lir::LirInstructionKind::Store {
                                value: lir::LirValue::Register(call_id),
                                address: ptr,
                                alignment: Some(alignment),
                                volatile: false,
                            },
                            type_hint: Some(ty.clone()),
                            debug_info: None,
                        });
                    }
                }
                Some(ref ty) => {
                    self.register_map.insert(
                        dest_place.local,
                        lir::LirValue::Constant(lir::LirConstant::Undef(ty.clone())),
                    );
                }
                None => {
                    self.register_map.insert(
                        dest_place.local,
                        lir::LirValue::Constant(lir::LirConstant::Undef(lir::LirType::Void)),
                    );
                }
            }
            return Ok(lir::LirTerminator::Br(*dest_bb));
        }

        Ok(lir::LirTerminator::Return(None))
    }

    fn normalize_callee_value(
        &mut self,
        func_operand: &mir::Operand,
        value: lir::LirValue,
    ) -> lir::LirValue {
        match value {
            lir::LirValue::Register(id) => {
                if let Some(place) = Self::operand_place(func_operand) {
                    if let Some(lir::LirValue::Function(name)) = self.register_map.get(&place.local)
                    {
                        return lir::LirValue::Function(self.resolve_function_symbol(name));
                    }
                }
                lir::LirValue::Register(id)
            }
            lir::LirValue::Global(name, _) => {
                lir::LirValue::Function(self.resolve_function_symbol(&name))
            }
            lir::LirValue::Function(name) => {
                lir::LirValue::Function(self.resolve_function_symbol(&name))
            }
            other => other,
        }
    }

    fn resolve_function_symbol(&self, name: &str) -> String {
        let logical = self
            .function_symbol_map
            .get(name)
            .cloned()
            .unwrap_or_else(|| name.to_string());

        if self.function_signatures.contains_key(&logical) {
            return logical;
        }

        if let Some(mapped) = (self.runtime_symbol_map)(&logical) {
            return mapped.as_str().to_string();
        }

        logical
    }

    fn operand_place(operand: &mir::Operand) -> Option<&mir::Place> {
        match operand {
            mir::Operand::Move(place) | mir::Operand::Copy(place) => Some(place),
            _ => None,
        }
    }

    fn prepare_return_value(&mut self, block: &mut lir::LirBasicBlock) -> Option<lir::LirValue> {
        let return_ty = self.current_return_type.clone()?;
        if matches!(return_ty, lir::LirType::Void) {
            return None;
        }

        if let Some(local) = self.return_local {
            if let Some(storage) = self.local_storage.get(&local) {
                let ptr_value = storage.ptr_value.clone();
                let element_ty = storage.element_type.clone();
                let alignment = storage.alignment;

                let load_id = self.next_id();
                block.instructions.push(lir::LirInstruction {
                    id: load_id,
                    kind: lir::LirInstructionKind::Load {
                        address: ptr_value,
                        alignment: Some(alignment),
                        volatile: false,
                    },
                    type_hint: Some(element_ty.clone()),
                    debug_info: None,
                });

                if element_ty == return_ty {
                    return Some(lir::LirValue::Register(load_id));
                } else if let Some(zero) = self.zero_value_for_lir_type(&return_ty) {
                    return Some(zero);
                } else {
                    return Some(lir::LirValue::Constant(lir::LirConstant::Undef(return_ty)));
                }
            }

            if let Some(value) = self.register_map.get(&local) {
                if let Some(current_ty) = self.infer_lir_value_type(value) {
                    if current_ty == return_ty {
                        return Some(value.clone());
                    } else if let Some(zero) = self.zero_value_for_lir_type(&return_ty) {
                        return Some(zero);
                    }
                }
                return Some(lir::LirValue::Constant(lir::LirConstant::Undef(return_ty)));
            }
        }

        Some(lir::LirValue::Constant(lir::LirConstant::Undef(return_ty)))
    }

    fn compute_block_order(&self, mir_body: &mir::Body) -> Vec<usize> {
        let mut order = Vec::new();
        let block_count = mir_body.basic_blocks.len();
        if block_count == 0 {
            return order;
        }

        let mut visited = vec![false; block_count];
        let mut queue = VecDeque::new();
        queue.push_back(0usize);
        visited[0] = true;

        while let Some(bb_idx) = queue.pop_front() {
            order.push(bb_idx);
            let successors = Self::mir_successors(&mir_body.basic_blocks[bb_idx]);
            for succ in successors {
                let succ_idx = succ as usize;
                if succ_idx < block_count && !visited[succ_idx] {
                    visited[succ_idx] = true;
                    queue.push_back(succ_idx);
                }
            }
        }

        // Append any unreachable blocks deterministically to maintain coverage
        for idx in 0..block_count {
            if !visited[idx] {
                order.push(idx);
            }
        }

        order
    }

    fn mir_successors(bb: &mir::BasicBlockData) -> Vec<mir::BasicBlockId> {
        let mut successors = Vec::new();
        if let Some(terminator) = &bb.terminator {
            match &terminator.kind {
                mir::TerminatorKind::Goto { target } => successors.push(*target),
                mir::TerminatorKind::SwitchInt { targets, .. } => {
                    successors.extend(targets.targets.iter().copied());
                    successors.push(targets.otherwise);
                }
                mir::TerminatorKind::Call {
                    destination,
                    cleanup,
                    ..
                } => {
                    if let Some((_, dest_bb)) = destination {
                        successors.push(*dest_bb);
                    }
                    if let Some(cleanup_bb) = cleanup {
                        successors.push(*cleanup_bb);
                    }
                }
                mir::TerminatorKind::Drop { target, unwind, .. }
                | mir::TerminatorKind::DropAndReplace { target, unwind, .. } => {
                    successors.push(*target);
                    if let Some(unwind_bb) = unwind {
                        successors.push(*unwind_bb);
                    }
                }
                mir::TerminatorKind::Assert {
                    target, cleanup, ..
                } => {
                    successors.push(*target);
                    if let Some(cleanup_bb) = cleanup {
                        successors.push(*cleanup_bb);
                    }
                }
                mir::TerminatorKind::Yield { resume, drop, .. } => {
                    successors.push(*resume);
                    if let Some(drop_bb) = drop {
                        successors.push(*drop_bb);
                    }
                }
                mir::TerminatorKind::FalseEdge {
                    real_target,
                    imaginary_target,
                } => {
                    successors.push(*real_target);
                    successors.push(*imaginary_target);
                }
                mir::TerminatorKind::FalseUnwind {
                    real_target,
                    unwind,
                } => {
                    successors.push(*real_target);
                    if let Some(unwind_bb) = unwind {
                        successors.push(*unwind_bb);
                    }
                }
                mir::TerminatorKind::InlineAsm {
                    destination,
                    cleanup,
                    ..
                } => {
                    if let Some(dest_bb) = destination {
                        successors.push(*dest_bb);
                    }
                    if let Some(cleanup_bb) = cleanup {
                        successors.push(*cleanup_bb);
                    }
                }
                _ => {}
            }
        }

        successors.sort_unstable();
        successors.dedup();
        successors
    }

    fn constant_from_aggregate(
        &self,
        kind: &mir::AggregateKind,
        constants: Vec<lir::LirConstant>,
        place_ty: &Ty,
    ) -> Option<lir::LirConstant> {
        match kind {
            mir::AggregateKind::Tuple => {
                // Only emit struct constants when the place itself is a tuple.
                // If the place type is non-aggregate, returning a struct constant
                // produces invalid LLVM IR like "struct i64 { i64 1 }".
                if !matches!(place_ty.kind, TyKind::Tuple(_)) {
                    return None;
                }
                let lir_ty = self.lir_type_from_ty(place_ty);
                Some(lir::LirConstant::Struct(constants, lir_ty))
            }
            mir::AggregateKind::Array(_elem_ty) => {
                if let TyKind::Array(inner_ty, len) = &place_ty.kind {
                    let lir_elem_ty = self.lir_type_from_ty(inner_ty);
                    let expected = self.array_length_from_const(len);
                    if expected != 0 && expected != constants.len() as u64 {
                        tracing::warn!(
                            "MIR→LIR: array constant length {} differs from {} elements",
                            expected,
                            constants.len()
                        );
                    }
                    Some(lir::LirConstant::Array(constants, lir_elem_ty))
                } else {
                    Some(lir::LirConstant::Array(constants, lir::LirType::I64))
                }
            }
            _ => None,
        }
    }

    fn lookup_place_type(&self, place: &mir::Place) -> Option<Ty> {
        let mut ty = self.local_types.get(place.local as usize)?.clone();
        for elem in &place.projection {
            match elem {
                mir::PlaceElem::Deref => match ty.kind {
                    TyKind::Ref(_, inner, _) => {
                        ty = (*inner).clone();
                    }
                    TyKind::RawPtr(TypeAndMut { ty: inner, .. }) => {
                        ty = (*inner).clone();
                    }
                    _ => {
                        return None;
                    }
                },
                mir::PlaceElem::Field(_, field_ty) => {
                    ty = field_ty.clone();
                }
                mir::PlaceElem::Index(_) => match &ty.kind {
                    TyKind::Array(elem, _) => {
                        ty = *elem.clone();
                    }
                    TyKind::Slice(elem) => {
                        ty = *elem.clone();
                    }
                    _ => {
                        return None;
                    }
                },
                mir::PlaceElem::ConstantIndex { .. }
                | mir::PlaceElem::Subslice { .. }
                | mir::PlaceElem::Downcast(_, _) => {
                    return None;
                }
            }
        }
        Some(ty)
    }

    fn is_zero_sized(ty: &Ty) -> bool {
        matches!(ty.kind, TyKind::Tuple(ref elements) if elements.is_empty())
    }

    fn lir_type_from_ty(&self, ty: &Ty) -> lir::LirType {
        match &ty.kind {
            TyKind::Bool => lir::LirType::I1,
            TyKind::Char => lir::LirType::I32,
            TyKind::Int(int_ty) => match int_ty {
                IntTy::I8 => lir::LirType::I8,
                IntTy::I16 => lir::LirType::I16,
                IntTy::I32 => lir::LirType::I32,
                IntTy::I64 => lir::LirType::I64,
                IntTy::I128 => lir::LirType::I128,
                IntTy::Isize => lir::LirType::I64,
            },
            TyKind::Uint(uint_ty) => match uint_ty {
                UintTy::U8 => lir::LirType::I8,
                UintTy::U16 => lir::LirType::I16,
                UintTy::U32 => lir::LirType::I32,
                UintTy::U64 => lir::LirType::I64,
                UintTy::U128 => lir::LirType::I128,
                UintTy::Usize => lir::LirType::I64,
            },
            TyKind::Float(float_ty) => match float_ty {
                FloatTy::F32 => lir::LirType::F32,
                FloatTy::F64 => lir::LirType::F64,
            },
            TyKind::Tuple(elements) if elements.is_empty() => lir::LirType::Void,
            TyKind::Tuple(elements) => lir::LirType::Struct {
                fields: elements
                    .iter()
                    .map(|elem| self.lir_type_from_ty(elem))
                    .collect(),
                packed: false,
                name: None,
            },
            TyKind::Array(element_ty, len) => lir::LirType::Array(
                Box::new(self.lir_type_from_ty(element_ty)),
                self.array_length_from_const(len),
            ),
            TyKind::Slice(element_ty) => {
                let elem_lir = self.lir_type_from_ty(element_ty);
                self.slice_lir_type(&elem_lir)
            }
            TyKind::Ref(_, inner, _) => lir::LirType::Ptr(Box::new(self.lir_type_from_ty(inner))),
            TyKind::RawPtr(TypeAndMut { ty: inner, .. }) => {
                lir::LirType::Ptr(Box::new(self.lir_type_from_ty(inner)))
            }
            TyKind::Adt(_, _)
            | TyKind::FnDef(_, _)
            | TyKind::Dynamic(_, _)
            | TyKind::Closure(_, _)
            | TyKind::Generator(_, _, _)
            | TyKind::GeneratorWitness(_)
            | TyKind::Projection(_)
            | TyKind::Opaque(_, _)
            | TyKind::Param(_)
            | TyKind::Bound(_, _)
            | TyKind::Placeholder(_)
            | TyKind::Infer(_)
            | TyKind::Error(_) => lir::LirType::Ptr(Box::new(lir::LirType::I8)),
            TyKind::Never => lir::LirType::Void,
            TyKind::FnPtr(poly_fn_sig) => {
                let fn_sig = &poly_fn_sig.binder.value;
                lir::LirType::Ptr(Box::new(lir::LirType::Function {
                    return_type: Box::new(self.lir_type_from_ty(&fn_sig.output)),
                    param_types: fn_sig
                        .inputs
                        .iter()
                        .map(|ty| self.lir_type_from_ty(ty))
                        .collect(),
                    is_variadic: fn_sig.c_variadic,
                }))
            }
        }
    }

    fn lower_binary_op(
        &self,
        bin_op: mir::BinOp,
        lhs: lir::LirValue,
        rhs: lir::LirValue,
    ) -> lir::LirInstructionKind {
        match bin_op {
            mir::BinOp::Add => lir::LirInstructionKind::Add(lhs, rhs),
            mir::BinOp::Sub => lir::LirInstructionKind::Sub(lhs, rhs),
            mir::BinOp::Mul => lir::LirInstructionKind::Mul(lhs, rhs),
            mir::BinOp::Div => lir::LirInstructionKind::Div(lhs, rhs),
            mir::BinOp::Rem => lir::LirInstructionKind::Rem(lhs, rhs),
            mir::BinOp::And => lir::LirInstructionKind::And(lhs, rhs),
            mir::BinOp::Or => lir::LirInstructionKind::Or(lhs, rhs),
            mir::BinOp::BitAnd => lir::LirInstructionKind::And(lhs, rhs),
            mir::BinOp::BitOr => lir::LirInstructionKind::Or(lhs, rhs),
            mir::BinOp::BitXor => lir::LirInstructionKind::Xor(lhs, rhs),
            mir::BinOp::Shl => lir::LirInstructionKind::Shl(lhs, rhs),
            mir::BinOp::Shr => lir::LirInstructionKind::Shr(lhs, rhs),
            mir::BinOp::Eq => lir::LirInstructionKind::Eq(lhs, rhs),
            mir::BinOp::Ne => lir::LirInstructionKind::Ne(lhs, rhs),
            mir::BinOp::Lt => lir::LirInstructionKind::Lt(lhs, rhs),
            mir::BinOp::Le => lir::LirInstructionKind::Le(lhs, rhs),
            mir::BinOp::Gt => lir::LirInstructionKind::Gt(lhs, rhs),
            mir::BinOp::Ge => lir::LirInstructionKind::Ge(lhs, rhs),
            _ => lir::LirInstructionKind::Unreachable,
        }
    }

    fn lower_unary_op(
        &self,
        op: mir::UnOp,
        operand: lir::LirValue,
        result_ty: &lir::LirType,
    ) -> Result<lir::LirInstructionKind> {
        match op {
            mir::UnOp::Not => Ok(lir::LirInstructionKind::Not(operand)),
            mir::UnOp::Neg => {
                let zero = self.zero_value_for_lir_type(result_ty).ok_or_else(|| {
                    crate::error::optimization_error(format!(
                        "MIR→LIR: unsupported type {:?} for unary negation",
                        result_ty
                    ))
                })?;
                Ok(lir::LirInstructionKind::Sub(zero, operand))
            }
        }
    }

    fn lower_cast(
        &self,
        cast_kind: mir::CastKind,
        source: lir::LirValue,
        source_ty: Option<lir::LirType>,
        target_ty: lir::LirType,
    ) -> lir::LirInstructionKind {
        let source_ty = source_ty.or_else(|| self.infer_lir_type_from_value(&source));
        match cast_kind {
            mir::CastKind::Misc => {
                if let Some(src_ty) = source_ty {
                    if matches!(src_ty, lir::LirType::Ptr(_)) && self.is_integral_type(&target_ty) {
                        return lir::LirInstructionKind::PtrToInt(source);
                    }
                    if self.is_integral_type(&src_ty) && matches!(target_ty, lir::LirType::Ptr(_)) {
                        return lir::LirInstructionKind::IntToPtr(source);
                    }
                    if self.is_integral_type(&src_ty) && self.is_integral_type(&target_ty) {
                        let src_w = self.type_bit_width(&src_ty);
                        let dst_w = self.type_bit_width(&target_ty);
                        if src_w == dst_w {
                            lir::LirInstructionKind::Bitcast(source, target_ty)
                        } else {
                            lir::LirInstructionKind::SextOrTrunc(source, target_ty)
                        }
                    } else if self.is_float_type(&src_ty) && self.is_float_type(&target_ty) {
                        let src_w = self.type_bit_width(&src_ty);
                        let dst_w = self.type_bit_width(&target_ty);
                        match (src_w, dst_w) {
                            (Some(s), Some(d)) if d > s => {
                                lir::LirInstructionKind::FPExt(source, target_ty)
                            }
                            (Some(s), Some(d)) if d < s => {
                                lir::LirInstructionKind::FPTrunc(source, target_ty)
                            }
                            _ => lir::LirInstructionKind::Bitcast(source, target_ty),
                        }
                    } else if self.is_float_type(&src_ty) && self.is_integral_type(&target_ty) {
                        lir::LirInstructionKind::FPToSI(source, target_ty)
                    } else if self.is_integral_type(&src_ty) && self.is_float_type(&target_ty) {
                        lir::LirInstructionKind::SIToFP(source, target_ty)
                    } else {
                        lir::LirInstructionKind::Bitcast(source, target_ty)
                    }
                } else {
                    lir::LirInstructionKind::Bitcast(source, target_ty)
                }
            }
            mir::CastKind::Pointer(pointer_cast) => match pointer_cast {
                mir::PointerCast::ReifyFnPointer
                | mir::PointerCast::UnsafeFnPointer
                | mir::PointerCast::ClosureFnPointer
                | mir::PointerCast::MutToConstPointer
                | mir::PointerCast::ArrayToPointer
                | mir::PointerCast::Unsize => {
                    if let Some(src_ty) = source_ty {
                        if matches!(src_ty, lir::LirType::Ptr(_))
                            && self.is_integral_type(&target_ty)
                        {
                            lir::LirInstructionKind::PtrToInt(source)
                        } else if self.is_integral_type(&src_ty)
                            && matches!(target_ty, lir::LirType::Ptr(_))
                        {
                            lir::LirInstructionKind::IntToPtr(source)
                        } else {
                            lir::LirInstructionKind::Bitcast(source, target_ty)
                        }
                    } else {
                        lir::LirInstructionKind::Bitcast(source, target_ty)
                    }
                }
            },
        }
    }

    fn infer_lir_type_from_value(&self, value: &lir::LirValue) -> Option<lir::LirType> {
        match value {
            lir::LirValue::Constant(lir::LirConstant::Int(_, ty))
            | lir::LirValue::Constant(lir::LirConstant::UInt(_, ty))
            | lir::LirValue::Constant(lir::LirConstant::Float(_, ty))
            | lir::LirValue::Constant(lir::LirConstant::Struct(_, ty))
            | lir::LirValue::Constant(lir::LirConstant::Array(_, ty))
            | lir::LirValue::Constant(lir::LirConstant::Null(ty))
            | lir::LirValue::Constant(lir::LirConstant::Undef(ty)) => Some(ty.clone()),
            lir::LirValue::Constant(lir::LirConstant::Bool(_)) => Some(lir::LirType::I1),
            lir::LirValue::Constant(lir::LirConstant::String(_)) => {
                Some(lir::LirType::Ptr(Box::new(lir::LirType::I8)))
            }
            lir::LirValue::Null(ty) | lir::LirValue::Undef(ty) => Some(ty.clone()),
            _ => None,
        }
    }

    fn array_length_from_const(&self, len: &ConstKind) -> u64 {
        match len {
            ConstKind::Value(ConstValue::Scalar(Scalar::Int(int))) => int.data as u64,
            other => {
                tracing::warn!(
                    "MIR→LIR: array length {:?} not evaluated; defaulting to 0",
                    other
                );
                0
            }
        }
    }

    fn zero_value_for_lir_type(&self, ty: &lir::LirType) -> Option<lir::LirValue> {
        match ty {
            lir::LirType::I1
            | lir::LirType::I8
            | lir::LirType::I16
            | lir::LirType::I32
            | lir::LirType::I64
            | lir::LirType::I128 => Some(lir::LirValue::Constant(lir::LirConstant::Int(
                0,
                ty.clone(),
            ))),
            lir::LirType::F32 | lir::LirType::F64 => Some(lir::LirValue::Constant(
                lir::LirConstant::Float(0.0, ty.clone()),
            )),
            lir::LirType::Ptr(_) => Some(lir::LirValue::Null(ty.clone())),
            _ => None,
        }
    }

    fn type_of_operand(&self, operand: &mir::Operand) -> Option<lir::LirType> {
        match operand {
            mir::Operand::Move(place) | mir::Operand::Copy(place) => self
                .lookup_place_type(place)
                .map(|ty| self.lir_type_from_ty(&ty)),
            mir::Operand::Constant(constant) => match &constant.literal {
                mir::ConstantKind::Bool(_) => Some(lir::LirType::I1),
                mir::ConstantKind::Int(_) | mir::ConstantKind::UInt(_) => Some(lir::LirType::I64),
                mir::ConstantKind::Float(_) => Some(lir::LirType::F64),
                mir::ConstantKind::Fn(_, _) | mir::ConstantKind::Global(_, _) => {
                    Some(lir::LirType::Ptr(Box::new(lir::LirType::I8)))
                }
                mir::ConstantKind::Null => Some(lir::LirType::Ptr(Box::new(lir::LirType::I8))),
                _ => None,
            },
        }
    }

    fn is_integral_type(&self, ty: &lir::LirType) -> bool {
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

    fn is_float_type(&self, ty: &lir::LirType) -> bool {
        matches!(ty, lir::LirType::F32 | lir::LirType::F64)
    }

    fn type_bit_width(&self, ty: &lir::LirType) -> Option<u32> {
        match ty {
            lir::LirType::I1 => Some(1),
            lir::LirType::I8 => Some(8),
            lir::LirType::I16 => Some(16),
            lir::LirType::I32 => Some(32),
            lir::LirType::I64 => Some(64),
            lir::LirType::I128 => Some(128),
            lir::LirType::F32 => Some(32),
            lir::LirType::F64 => Some(64),
            _ => None,
        }
    }
}

impl IrTransform<mir::Program, lir::LirProgram> for LirGenerator {
    fn transform(&mut self, source: mir::Program) -> Result<lir::LirProgram> {
        LirGenerator::transform(self, source)
    }
}

impl Default for LirGenerator {
    fn default() -> Self {
        Self::new()
    }
}

// END ORIGINAL CONTENT
