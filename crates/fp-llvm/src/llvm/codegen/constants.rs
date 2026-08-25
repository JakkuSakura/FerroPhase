use super::*;

impl<'a> LirCodegen<'a> {
    pub(super) fn convert_lir_value_to_basic_value(
        &mut self,
        lir_value: lir::LirValue,
    ) -> Result<BasicValueEnum<'static>> {
        let value_ty = lir_value.ty.clone();
        match lir_value.kind {
            lir::LirValueKind::Register(reg_id) => {
                if let Some(constant) = self.constant_results.get(&reg_id) {
                    return self.convert_lir_constant_to_value(constant.clone());
                }
                self.register_map
                    .get(&reg_id)
                    .map(|(value, _)| *value)
                    .ok_or_else(|| {
                        DiagnosticManager::report_error_with_context(
                            LOG_AREA,
                            format!("Unknown register {} encountered during codegen", reg_id),
                        )
                    })
            }
            lir::LirValueKind::Constant(constant) => {
                self.convert_lir_constant_to_value(lir::LirConstant {
                    ty: value_ty,
                    kind: constant,
                })
            }
            lir::LirValueKind::Global(name) => {
                let llvm_name = self.llvm_symbol_for(name.as_str());
                if let Some(signature) = self.function_signatures.get(name.as_str()) {
                    let fn_type = self.function_type_from_signature(signature.clone())?;
                    let fn_value = self
                        .llvm_ctx
                        .module
                        .get_function(&llvm_name)
                        .unwrap_or_else(|| {
                            self.llvm_ctx.module.add_function(
                                &llvm_name,
                                fn_type,
                                Some(inkwell::module::Linkage::External),
                            )
                        });
                    return Ok(fn_value.as_global_value().as_pointer_value().into());
                }

                if let Some(lir_constant) = self.global_const_map.get(name.as_str()) {
                    let llvm_constant = self.convert_lir_constant_to_value(lir_constant.clone())?;
                    tracing::debug!("LLVM: Found global '{}' in const map, using value", name);
                    return Ok(llvm_constant);
                }

                if let Some(global) = self.llvm_ctx.module.get_global(&llvm_name) {
                    return Ok(global.as_pointer_value().into());
                }

                self.referenced_globals.insert(name.as_str().to_owned());

                if self.allow_unresolved_globals {
                    tracing::warn!(
                        "LLVM: synthesizing placeholder null for unresolved global '{}'",
                        name
                    );
                    let ptr = self.llvm_ctx.ptr_type().const_null();
                    return Ok(ptr.into());
                }

                Err(DiagnosticManager::report_error_with_context(
                    LOG_AREA,
                    format!(
                        "Global variable '{}' of type {:?} not found",
                        name, value_ty
                    ),
                ))
            }
            lir::LirValueKind::Function(lir::LirFunctionRef::Name(name)) => {
                let llvm_name = self.llvm_symbol_for(name.as_str());
                let function = self
                    .llvm_ctx
                    .module
                    .get_function(&llvm_name)
                    .or_else(|| {
                        self.function_signatures.get(name.as_str()).and_then(|sig| {
                            self.function_type_from_signature(sig.clone())
                                .ok()
                                .map(|fn_type| {
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
                        DiagnosticManager::report_error_with_context(
                            LOG_AREA,
                            format!(
                                "Unknown function reference '{}' encountered during codegen",
                                name
                            ),
                        )
                    })?;

                Ok(function.as_global_value().as_pointer_value().into())
            }
            lir::LirValueKind::Function(lir::LirFunctionRef::Package { package_id, name }) => {
                Err(DiagnosticManager::report_error_with_context(
                    LOG_AREA,
                    format!(
                        "Package-qualified function `{:?}::{}` is not supported by LLVM lowering",
                        package_id, name
                    ),
                ))
            }
            lir::LirValueKind::Function(lir::LirFunctionRef::Definition(def_id)) => {
                Err(DiagnosticManager::report_error_with_context(
                    LOG_AREA,
                    format!("Function definition `{def_id}` is not supported by LLVM lowering"),
                ))
            }
            lir::LirValueKind::Local(local_id) => self
                .argument_operands
                .get(&local_id)
                .copied()
                .ok_or_else(|| {
                    DiagnosticManager::report_error_with_context(
                        LOG_AREA,
                        format!("Unknown local: {}", local_id),
                    )
                }),
            lir::LirValueKind::StackSlot(slot_id) => self
                .stack_slot_map
                .get(&slot_id)
                .map(|(ptr, _)| (*ptr).into())
                .ok_or_else(|| {
                    DiagnosticManager::report_error_with_context(
                        LOG_AREA,
                        format!("Unknown stack slot: {}", slot_id),
                    )
                }),
        }
    }

    pub(super) fn convert_lir_constant_to_value(
        &mut self,
        lir_const: lir::LirConstant,
    ) -> Result<BasicValueEnum<'static>> {
        let ty = lir_const.ty.clone();
        match lir_const.kind {
            lir::LirConstantKind::Data(lir::LirConstantData::Integer(integer)) => {
                let int_ty = self.llvm_int_type(&ty)?;
                let value = match integer {
                    lir::LirInteger::I1(value) => u64::from(value),
                    lir::LirInteger::I8(value) => u64::from(value),
                    lir::LirInteger::I16(value) => u64::from(value),
                    lir::LirInteger::I32(value) => u64::from(value),
                    lir::LirInteger::I64(value) => value,
                    lir::LirInteger::I128(value) => value as u64,
                    lir::LirInteger::Arbitrary(_) => {
                        return Err(Error::from(
                            "LLVM arbitrary integer constant lowering is unsupported",
                        ));
                    }
                };
                Ok(int_ty.const_int(value, false).into())
            }
            lir::LirConstantKind::Data(lir::LirConstantData::Float(float)) => {
                let float_ty = self.llvm_float_type(&ty)?;
                let value = match float {
                    lir::LirFloat::F32(value) => f32::from_bits(value) as f64,
                    lir::LirFloat::F64(value) => f64::from_bits(value),
                };
                Ok(float_ty.const_float(value).into())
            }
            lir::LirConstantKind::Data(lir::LirConstantData::Bytes(bytes)) => {
                let values = bytes
                    .into_iter()
                    .map(|byte| self.llvm_ctx.i8_type().const_int(u64::from(byte), false))
                    .collect::<Vec<_>>();
                Ok(self.llvm_ctx.i8_type().const_array(&values).into())
            }
            lir::LirConstantKind::Aggregate(aggregate) => {
                let values = match aggregate {
                    lir::LirConstantAggregate::Array(values)
                    | lir::LirConstantAggregate::Vector(values)
                    | lir::LirConstantAggregate::Struct(values) => values,
                };
                let converted = values
                    .into_iter()
                    .map(|value| self.convert_lir_constant_to_value(value))
                    .collect::<Result<Vec<_>>>()?;
                match self.llvm_basic_type(&ty)? {
                    BasicTypeEnum::StructType(struct_ty) => {
                        Ok(struct_ty.const_named_struct(&converted).into())
                    }
                    BasicTypeEnum::ArrayType(array_ty) => {
                        let mut raw_values = converted
                            .iter()
                            .map(|value| value.as_value_ref())
                            .collect::<Vec<_>>();
                        let value_ref = unsafe {
                            LLVMConstArray2(
                                array_ty.get_element_type().as_type_ref(),
                                raw_values.as_mut_ptr(),
                                raw_values.len() as u64,
                            )
                        };
                        Ok(unsafe { inkwell::values::ArrayValue::new(value_ref) }.into())
                    }
                    _ => Err(Error::from("aggregate constant type mismatch")),
                }
            }
            lir::LirConstantKind::GlobalAddress { global } => {
                let llvm_name = self.llvm_symbol_for(global.as_str());
                let global =
                    self.llvm_ctx.module.get_global(&llvm_name).ok_or_else(|| {
                        Error::from(format!("unknown global constant `{global}`"))
                    })?;
                Ok(global.as_pointer_value().into())
            }
            lir::LirConstantKind::FunctionAddress(function) => {
                let lir::LirFunctionRef::Name(name) = function else {
                    return Err(Error::from("non-name function constant is unsupported"));
                };
                let llvm_name = self.llvm_symbol_for(name.as_str());
                let function = self
                    .llvm_ctx
                    .module
                    .get_function(&llvm_name)
                    .ok_or_else(|| Error::from(format!("unknown function constant `{name}`")))?;
                Ok(function.as_global_value().as_pointer_value().into())
            }
            lir::LirConstantKind::Null => Ok(self.llvm_basic_type(&ty)?.const_zero()),
            lir::LirConstantKind::Undef => {
                Ok(self.undef_value_for_type(self.llvm_basic_type(&ty)?))
            }
            lir::LirConstantKind::Poison | lir::LirConstantKind::Expr(_) => {
                Err(Error::from("unsupported LLVM constant expression"))
            }
        }
    }

    /*
    pub(super) fn convert_lir_constant_to_value_legacy(
        &mut self,
        lir_const: lir::LirConstant,
    ) -> Result<BasicValueEnum<'static>> {
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
            lir::LirConstant::Bytes(bytes) => {
                let values = bytes
                    .into_iter()
                    .map(|byte| self.llvm_ctx.i8_type().const_int(byte as u64, false))
                    .collect::<Vec<_>>();
                Ok(self.llvm_ctx.i8_type().const_array(&values).into())
            }
            lir::LirConstant::Struct(values, ty) => {
                let struct_ty = match self.llvm_basic_type(&ty)? {
                    BasicTypeEnum::StructType(strct) => strct,
                    _ => {
                        return Err(DiagnosticManager::report_error_with_context(
                            LOG_AREA,
                            "Expected struct type for struct constant",
                        ))
                    }
                };
                if struct_ty.count_fields() == 1 && values.len() != 1 {
                    let field_ty = struct_ty.get_field_types()[0];
                    if let BasicTypeEnum::StructType(inner_ty) = field_ty {
                        if inner_ty.count_fields() as usize == values.len() {
                            let mut inner_values = Vec::with_capacity(values.len());
                            for value in values {
                                inner_values.push(self.convert_lir_constant_to_value(value)?);
                            }
                            let inner = inner_ty.const_named_struct(&inner_values);
                            let outer = struct_ty.const_named_struct(&[inner.into()]);
                            return Ok(outer.into());
                        }
                    }
                }

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
                    let mut raw_values: Vec<_> =
                        llvm_values.iter().map(|v| v.as_value_ref()).collect();
                    let value_ref = LLVMConstArray2(
                        elem_ty.as_type_ref(),
                        raw_values.as_mut_ptr(),
                        raw_values.len() as u64,
                    );
                    inkwell::values::ArrayValue::new(value_ref)
                };
                Ok(array_value.into())
            }
            lir::LirConstant::GlobalRef(name, ty, indices) => {
                let llvm_name = self.llvm_symbol_for(&name);
                let global = self.llvm_ctx.module.get_global(&llvm_name).ok_or_else(|| {
                    DiagnosticManager::report_error_with_context(
                        LOG_AREA,
                        format!("Unknown global referenced in constant: {}", name),
                    )
                })?;
                let mut ptr = global.as_pointer_value();
                if !indices.is_empty() {
                    let mut idx_values: Vec<_> = Vec::with_capacity(indices.len());
                    for idx in indices {
                        let idx_val = self.llvm_ctx.i32_type().const_int(idx as u64, false);
                        idx_values.push(idx_val);
                    }
                    let elem_ty = match global.get_value_type() {
                        AnyTypeEnum::ArrayType(ty) => ty.as_basic_type_enum(),
                        AnyTypeEnum::FloatType(ty) => ty.as_basic_type_enum(),
                        AnyTypeEnum::IntType(ty) => ty.as_basic_type_enum(),
                        AnyTypeEnum::PointerType(ty) => ty.as_basic_type_enum(),
                        AnyTypeEnum::StructType(ty) => ty.as_basic_type_enum(),
                        AnyTypeEnum::VectorType(ty) => ty.as_basic_type_enum(),
                        AnyTypeEnum::ScalableVectorType(ty) => ty.as_basic_type_enum(),
                        AnyTypeEnum::FunctionType(_) | AnyTypeEnum::VoidType(_) => {
                            return Err(DiagnosticManager::report_error_with_context(
                                LOG_AREA,
                                "global reference GEP expects a basic value type".to_string(),
                            ))
                        }
                    };
                    let gep = unsafe { ptr.const_gep(elem_ty, &idx_values) };
                    ptr = gep;
                }
                let target_ptr_ty = match self.llvm_basic_type(&ty)? {
                    BasicTypeEnum::PointerType(ptr_ty) => ptr_ty,
                    _other => self
                        .llvm_ctx
                        .context
                        .ptr_type(inkwell::AddressSpace::default()),
                };
                Ok(ptr.const_cast(target_ptr_ty).into())
            }
            lir::LirConstant::FunctionRef(name, ty) => {
                let llvm_name = self.llvm_symbol_for(&name);
                let func = self
                    .llvm_ctx
                    .module
                    .get_function(&llvm_name)
                    .ok_or_else(|| {
                        DiagnosticManager::report_error_with_context(
                            LOG_AREA,
                            format!("Unknown function referenced in constant: {}", name),
                        )
                    })?;
                let ptr = func.as_global_value().as_pointer_value();
                let target_ptr_ty = match self.llvm_basic_type(&ty)? {
                    BasicTypeEnum::PointerType(ptr_ty) => ptr_ty,
                    _other => self
                        .llvm_ctx
                        .context
                        .ptr_type(inkwell::AddressSpace::default()),
                };
                Ok(ptr.const_cast(target_ptr_ty).into())
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
    */

    pub(super) fn convert_global_bytes_to_typed_value(
        &mut self,
        bytes: &[u8],
        relocations: &[lir::LirGlobalRelocation],
        ty: &lir::LirType,
        base: usize,
    ) -> Result<BasicValueEnum<'static>> {
        match ty {
            lir::LirType::Integer(width) => {
                let int_ty = self.llvm_ctx.context.custom_width_int_type(*width);
                Ok(int_ty
                    .const_int(
                        Self::read_le_u128(bytes, base, (*width).div_ceil(8) as usize)? as u64,
                        false,
                    )
                    .into())
            }
            lir::LirType::I1 => Ok(self
                .llvm_ctx
                .i1_type()
                .const_int((bytes.get(base).copied().unwrap_or(0) & 1) as u64, false)
                .into()),
            lir::LirType::I8 => Ok(self
                .llvm_ctx
                .i8_type()
                .const_int(bytes.get(base).copied().unwrap_or(0) as u64, false)
                .into()),
            lir::LirType::I16 => Ok(self
                .llvm_ctx
                .i16_type()
                .const_int(Self::read_le_u128(bytes, base, 2)? as u64, false)
                .into()),
            lir::LirType::I32 => Ok(self
                .llvm_ctx
                .i32_type()
                .const_int(Self::read_le_u128(bytes, base, 4)? as u64, false)
                .into()),
            lir::LirType::I64 => Ok(self
                .llvm_ctx
                .i64_type()
                .const_int(Self::read_le_u128(bytes, base, 8)? as u64, false)
                .into()),
            lir::LirType::I128 => Ok(self
                .llvm_ctx
                .i128_type()
                .const_int_arbitrary_precision(&[
                    Self::read_le_u128(bytes, base, 8)? as u64,
                    Self::read_le_u128(bytes, base + 8, 8)? as u64,
                ])
                .into()),
            lir::LirType::F32 => {
                let bits = Self::read_le_u128(bytes, base, 4)? as u32;
                Ok(self
                    .llvm_ctx
                    .f32_type()
                    .const_float(f32::from_bits(bits) as f64)
                    .into())
            }
            lir::LirType::F64 => {
                let bits = Self::read_le_u128(bytes, base, 8)? as u64;
                Ok(self
                    .llvm_ctx
                    .f64_type()
                    .const_float(f64::from_bits(bits))
                    .into())
            }
            lir::LirType::Ptr(_) | lir::LirType::Function { .. } => {
                self.convert_global_pointer_bytes(bytes, relocations, ty, base)
            }
            lir::LirType::Array(element_ty, size) => {
                let llvm_elem_ty = self.llvm_basic_type(element_ty)?;
                let elem_size = self
                    .data_layout
                    .as_ref()
                    .ok_or_else(|| Error::from("LLVM data layout is not initialized"))?
                    .size_of(element_ty)
                    .map_err(|error| Error::from(error.to_string()))?
                    as usize;
                let mut values = Vec::with_capacity(*size as usize);
                for index in 0..(*size as usize) {
                    values.push(self.convert_global_bytes_to_typed_value(
                        bytes,
                        relocations,
                        element_ty,
                        base + index * elem_size,
                    )?);
                }
                let array_value = unsafe {
                    let mut raw_values: Vec<_> =
                        values.iter().map(|value| value.as_value_ref()).collect();
                    let value_ref = LLVMConstArray2(
                        llvm_elem_ty.as_type_ref(),
                        raw_values.as_mut_ptr(),
                        raw_values.len() as u64,
                    );
                    inkwell::values::ArrayValue::new(value_ref)
                };
                Ok(array_value.into())
            }
            lir::LirType::Struct { fields, packed, .. } => {
                let struct_ty = self.llvm_ctx.context.struct_type(
                    &fields
                        .iter()
                        .map(|field| self.llvm_basic_type(field))
                        .collect::<Result<Vec<_>>>()?,
                    *packed,
                );
                let layout = self
                    .data_layout
                    .as_ref()
                    .ok_or_else(|| Error::from("LLVM data layout is not initialized"))?
                    .struct_layout(ty)
                    .map_err(|error| Error::from(error.to_string()))?
                    .ok_or_else(|| {
                        DiagnosticManager::report_error_with_context(
                            LOG_AREA,
                            "missing LIR struct layout",
                        )
                    })?;
                let mut values = Vec::with_capacity(fields.len());
                for (index, field_ty) in fields.iter().enumerate() {
                    values.push(self.convert_global_bytes_to_typed_value(
                        bytes,
                        relocations,
                        field_ty,
                        base + layout.field_offsets[index] as usize,
                    )?);
                }
                Ok(struct_ty.const_named_struct(&values).into())
            }
            lir::LirType::Void
            | lir::LirType::Label
            | lir::LirType::Token
            | lir::LirType::Metadata => {
                let llvm_ty = self.llvm_basic_type(ty)?;
                Ok(llvm_ty.const_zero())
            }
            lir::LirType::Error => Ok(self.llvm_ctx.i64_type().const_zero().into()),
            lir::LirType::Vector(..) => Err(DiagnosticManager::report_error_with_context(
                LOG_AREA,
                "vector-typed global initializers are not yet supported by fp-llvm",
            )),
        }
    }

    pub(super) fn convert_global_pointer_bytes(
        &mut self,
        bytes: &[u8],
        relocations: &[lir::LirGlobalRelocation],
        ty: &lir::LirType,
        base: usize,
    ) -> Result<BasicValueEnum<'static>> {
        let target_ptr_ty = match self.llvm_basic_type(ty)? {
            BasicTypeEnum::PointerType(ptr_ty) => ptr_ty,
            _ => {
                return Err(DiagnosticManager::report_error_with_context(
                    LOG_AREA,
                    "expected pointer type for global relocation decoding",
                ));
            }
        };

        if let Some(reloc) = relocations
            .iter()
            .find(|reloc| reloc.offset as usize == base)
        {
            if reloc.addend != 0 {
                return Err(DiagnosticManager::report_error_with_context(
                    LOG_AREA,
                    "non-zero global relocation addends are not yet supported by fp-llvm",
                ));
            }
            let ptr = match &reloc.target {
                lir::LirRelocationTarget::Global(name) => {
                    let llvm_name = self.llvm_symbol_for(name);
                    let global = self.llvm_ctx.module.get_global(&llvm_name).ok_or_else(|| {
                        DiagnosticManager::report_error_with_context(
                            LOG_AREA,
                            format!("Unknown global referenced in relocation: {}", name),
                        )
                    })?;
                    global.as_pointer_value()
                }
                lir::LirRelocationTarget::Function(name) => {
                    let llvm_name = self.llvm_symbol_for(name);
                    let function =
                        self.llvm_ctx
                            .module
                            .get_function(&llvm_name)
                            .ok_or_else(|| {
                                DiagnosticManager::report_error_with_context(
                                    LOG_AREA,
                                    format!("Unknown function referenced in relocation: {}", name),
                                )
                            })?;
                    function.as_global_value().as_pointer_value()
                }
            };
            return Ok(ptr.const_cast(target_ptr_ty).into());
        }

        let raw = Self::read_le_u128(
            bytes,
            base,
            self.data_layout
                .as_ref()
                .ok_or_else(|| Error::from("LLVM data layout is not initialized"))?
                .size_of(ty)
                .map_err(|error| Error::from(error.to_string()))? as usize,
        )? as u64;
        if raw == 0 {
            return Ok(target_ptr_ty.const_null().into());
        }

        Err(DiagnosticManager::report_error_with_context(
            LOG_AREA,
            "non-null raw pointer bytes without relocation are not supported by fp-llvm",
        ))
    }

    pub(super) fn read_le_u128(bytes: &[u8], offset: usize, size: usize) -> Result<u128> {
        if size > 16 || offset.saturating_add(size) > bytes.len() {
            return Err(DiagnosticManager::report_error_with_context(
                LOG_AREA,
                "global byte initializer read out of bounds",
            ));
        }
        let mut value = 0u128;
        for (index, byte) in bytes[offset..offset + size].iter().enumerate() {
            value |= (*byte as u128) << (index * 8);
        }
        Ok(value)
    }
}
