use fp_core::ast::{
    DecimalType, EnumTypeVariant, ExprKind, FunctionSignature, Name, ReprFlags, ReprInt,
    ReprOptions, Ty, TypeArray, TypeEnum, TypeInt, TypePrimitive, TypeReference, TypeStruct,
    TypeStructural, TypeTuple, Value, ValueChar, ValueEscaped, ValueList, ValuePointer,
    ValueStruct, ValueStructural, ValueTuple,
};
use fp_core::error::{Error, Result};
use std::ffi::{CString, c_void};

// Re-export FFI types from the shared crate.
pub use fp_core::ast::FfiSliceRef;
pub use fp_ffi::{FfiSignature, FfiType, FfiValue};

fn ffi_err(e: impl std::error::Error) -> Error {
    Error::from(format!("ffi: {e}"))
}

#[derive(Debug, Clone)]
struct CAbiLayout {
    size: usize,
    align: usize,
    field_offsets: Vec<usize>,
}

struct EnumTagLayout {
    primitive: TypePrimitive,
    layout: CAbiLayout,
}

#[derive(Debug)]
pub struct FfiRuntime {
    inner: fp_ffi::FfiRuntime,
}

impl FfiRuntime {
    pub fn new() -> Result<Self> {
        Ok(Self {
            inner: fp_ffi::FfiRuntime::new().map_err(ffi_err)?,
        })
    }

    /// Low-level call: convert FfiValue args to raw u64 and delegate.
    pub fn call(
        &mut self,
        name: &str,
        sig: &FfiSignature,
        args: &[FfiValue],
    ) -> Result<Option<FfiValue>> {
        let raws: Vec<u64> = args.iter().map(|v| v.as_u64()).collect();
        let ret = self.inner.call(name, sig, &raws).map_err(ffi_err)?;
        Ok(ret.map(|r| match sig.ret {
            FfiType::I64 => FfiValue::I64(r as i64),
            FfiType::U64 => FfiValue::U64(r),
            FfiType::Ptr => FfiValue::Ptr(r as *mut c_void),
            FfiType::Void => unreachable!(),
        }))
    }

    pub fn call_fp(
        &mut self,
        name: &str,
        sig: &FunctionSignature,
        args: &[Value],
    ) -> Result<Value> {
        if !sig.abi.is_c() {
            return Err(Error::from(format!(
                "unsupported ABI for extern call: {:?}",
                sig.abi
            )));
        }
        if sig.params.len() != args.len() {
            return Err(Error::from(format!(
                "extern call '{}' expects {} args, got {}",
                name,
                sig.params.len(),
                args.len()
            )));
        }

        let (arg_types, arg_values, _cstrings, _escapes) = build_args(sig, args)?;
        let ret_ty = ffi_type_for_return(sig.ret_ty.as_ref())?;
        let signature = FfiSignature {
            args: arg_types,
            ret: ret_ty,
        };
        let result = self.call(name, &signature, &arg_values)?;
        convert_return(sig.ret_ty.as_ref(), result)
    }
}

fn build_args(
    sig: &FunctionSignature,
    args: &[Value],
) -> Result<(Vec<FfiType>, Vec<FfiValue>, Vec<CString>, Vec<ValueEscaped>)> {
    let mut arg_types = Vec::with_capacity(sig.params.len());
    let mut arg_values = Vec::with_capacity(sig.params.len());
    let mut cstrings = Vec::new();
    let mut escapes = Vec::new();

    for (param, value) in sig.params.iter().zip(args.iter()) {
        let ty = &param.ty;
        let ffi_ty = ffi_type_for_arg(ty)?;
        arg_types.push(ffi_ty);
        push_arg_value(ty, value, &mut arg_values, &mut cstrings, &mut escapes)?;
    }

    Ok((arg_types, arg_values, cstrings, escapes))
}

fn convert_return(ret_ty: Option<&Ty>, value: Option<FfiValue>) -> Result<Value> {
    let Some(ret_ty) = ret_ty else {
        return Ok(Value::unit());
    };
    if matches!(ret_ty, Ty::Unit(_)) {
        return Ok(Value::unit());
    }
    let ret_ty = resolve_ffi_ty(ret_ty).unwrap_or_else(|| ret_ty.clone());
    match (&ret_ty, value) {
        (Ty::Primitive(TypePrimitive::Bool), Some(FfiValue::U64(v))) => Ok(Value::bool(v != 0)),
        (Ty::Primitive(TypePrimitive::Bool), Some(FfiValue::I64(v))) => Ok(Value::bool(v != 0)),
        (Ty::Primitive(TypePrimitive::Char), Some(FfiValue::U64(v))) => {
            Ok(Value::Char(ValueChar::new(v as u8 as char)))
        }
        (Ty::Primitive(TypePrimitive::Char), Some(FfiValue::I64(v))) => {
            Ok(Value::Char(ValueChar::new(v as u8 as char)))
        }
        (Ty::Primitive(TypePrimitive::Int(_)), Some(FfiValue::I64(v))) => Ok(Value::int(v)),
        (Ty::Primitive(TypePrimitive::Int(_)), Some(FfiValue::U64(v))) => Ok(Value::int(v as i64)),
        (Ty::Primitive(TypePrimitive::Decimal(_)), _) => Err(Error::from(
            "unsupported extern decimal return type without libffi",
        )),
        (Ty::Primitive(TypePrimitive::String), _) => Err(Error::from(
            "unsupported extern string return type; use &CStr",
        )),
        (Ty::Reference(_), Some(FfiValue::Ptr(ptr))) => {
            Ok(Value::Pointer(ValuePointer::new(ptr as i64)))
        }
        _ => Err(Error::from("unsupported extern return type")),
    }
}

fn ffi_type_for_arg(ty: &Ty) -> Result<FfiType> {
    if is_cstr_reference(ty) {
        return Ok(FfiType::Ptr);
    }
    let resolved = resolve_ffi_ty(ty).unwrap_or_else(|| ty.clone());
    match &resolved {
        Ty::Primitive(TypePrimitive::Bool) => Ok(FfiType::U64),
        Ty::Primitive(TypePrimitive::Char) => Ok(FfiType::U64),
        Ty::Primitive(TypePrimitive::Int(int_ty)) => Ok(match int_ty {
            TypeInt::I8 | TypeInt::I16 | TypeInt::I32 | TypeInt::I64 | TypeInt::BigInt => {
                FfiType::I64
            }
            TypeInt::U8 | TypeInt::U16 | TypeInt::U32 | TypeInt::U64 => FfiType::U64,
            TypeInt::I128 | TypeInt::U128 => {
                return Err(Error::from(
                    "unsupported extern 128-bit integer arg without libffi",
                ));
            }
        }),
        Ty::Primitive(TypePrimitive::Decimal(_)) => {
            Err(Error::from("unsupported extern decimal arg without libffi"))
        }
        Ty::Primitive(TypePrimitive::String) => {
            Err(Error::from("unsupported extern string arg type; use &CStr"))
        }
        Ty::Reference(TypeReference { ty, .. }) => {
            if resolves_to_string(ty.as_ref()) {
                Err(Error::from("unsupported extern &str arg type; use &CStr"))
            } else {
                Ok(FfiType::Ptr)
            }
        }
        Ty::Unit(_) => Err(Error::from("unsupported extern unit arg")),
        _ => Err(Error::from("unsupported extern arg type")),
    }
}

fn ffi_type_for_return(ty: Option<&Ty>) -> Result<FfiType> {
    match ty {
        None => Ok(FfiType::Void),
        Some(Ty::Unit(_)) => Ok(FfiType::Void),
        Some(other) => {
            if is_cstr_reference(other) {
                return Ok(FfiType::Ptr);
            }
            ffi_type_for_arg(other)
        }
    }
}

fn push_arg_value(
    ty: &Ty,
    value: &Value,
    args: &mut Vec<FfiValue>,
    cstrings: &mut Vec<CString>,
    escapes: &mut Vec<ValueEscaped>,
) -> Result<()> {
    if is_cstr_reference(ty) {
        match value {
            Value::String(s) => {
                let cstr = CString::new(s.value.clone())
                    .map_err(|e| Error::from(format!("string contains interior NUL: {e}")))?;
                let ptr = cstr.as_ptr() as *mut c_void;
                cstrings.push(cstr);
                args.push(FfiValue::Ptr(ptr));
            }
            // A `c"..."` literal (`ast/expr.rs::parse_string`) evaluates
            // to a `Value::Bytes` in the interpreter, same as a plain
            // string does today — reuse the identical `CString::new` path.
            Value::Bytes(b) => {
                let cstr = CString::new(b.value.to_vec())
                    .map_err(|e| Error::from(format!("string contains interior NUL: {e}")))?;
                let ptr = cstr.as_ptr() as *mut c_void;
                cstrings.push(cstr);
                args.push(FfiValue::Ptr(ptr));
            }
            Value::Pointer(ptr) => args.push(FfiValue::Ptr(ptr.value as *mut c_void)),
            Value::Null(_) => args.push(FfiValue::Ptr(std::ptr::null_mut())),
            _ => return Err(Error::from("expected CStr argument")),
        }
        return Ok(());
    }

    let resolved = resolve_ffi_ty(ty).unwrap_or_else(|| ty.clone());
    match &resolved {
        Ty::Primitive(TypePrimitive::Bool) => match value {
            Value::Bool(v) => args.push(FfiValue::U64(if v.value { 1 } else { 0 })),
            _ => return Err(Error::from("expected bool argument")),
        },
        Ty::Primitive(TypePrimitive::Char) => match value {
            Value::Char(v) => args.push(FfiValue::U64(v.value as u64)),
            _ => return Err(Error::from("expected char argument")),
        },
        Ty::Primitive(TypePrimitive::Int(int_ty)) => match value {
            Value::Int(v) => push_int_arg(*int_ty, v.value, args)?,
            Value::Bool(v) => push_int_arg(*int_ty, if v.value { 1 } else { 0 }, args)?,
            Value::Char(v) => push_int_arg(*int_ty, v.value as i64, args)?,
            _ => return Err(Error::from("expected integer argument")),
        },
        Ty::Primitive(TypePrimitive::Decimal(_)) => {
            return Err(Error::from("unsupported extern decimal arg without libffi"));
        }
        Ty::Primitive(TypePrimitive::String) => {
            return Err(Error::from("unsupported extern string arg type; use &CStr"));
        }
        Ty::Reference(TypeReference { ty, .. }) => {
            if resolves_to_string(ty.as_ref()) {
                return Err(Error::from("unsupported extern &str arg type; use &CStr"));
            }
            if let Value::Pointer(ptr) = value {
                args.push(FfiValue::Ptr(ptr.value as *mut c_void));
                return Ok(());
            }
            if let Value::Null(_) = value {
                args.push(FfiValue::Ptr(std::ptr::null_mut()));
                return Ok(());
            }
            if let Value::Escaped(escaped) = value {
                args.push(FfiValue::Ptr(escaped.ptr.value as *mut c_void));
                return Ok(());
            }
            if let Value::FfiSliceRef(slice_ref) = value {
                let elem_ty = ty.as_ref();
                let elem_layout = c_abi_layout(elem_ty)?;
                let buf_size = elem_layout
                    .size
                    .checked_mul(slice_ref.values.len())
                    .ok_or_else(|| Error::from("ffi slice buffer size overflow"))?;
                let mut escaped = ValueEscaped::new(buf_size as i64, elem_layout.align as i64);
                {
                    let buf = unsafe { escaped.as_slice_mut() };
                    for (idx, value) in slice_ref.values.iter().enumerate() {
                        let offset = idx
                            .checked_mul(elem_layout.size)
                            .ok_or_else(|| Error::from("ffi slice offset overflow"))?;
                        write_c_abi_value(elem_ty, value, buf, offset)?;
                    }
                }
                let offset = slice_ref
                    .index
                    .checked_mul(elem_layout.size)
                    .ok_or_else(|| Error::from("ffi slice index overflow"))?;
                let ptr = unsafe { escaped.as_ptr().add(offset) } as *mut c_void;
                args.push(FfiValue::Ptr(ptr));
                escapes.push(escaped);
                return Ok(());
            }

            let layout = c_abi_layout(ty.as_ref())?;
            let mut escaped = ValueEscaped::new(layout.size as i64, layout.align as i64);
            {
                let buf = unsafe { escaped.as_slice_mut() };
                write_c_abi_value(ty.as_ref(), value, buf, 0)?;
            }
            args.push(FfiValue::Ptr(escaped.ptr.value as *mut c_void));
            escapes.push(escaped);
        }
        Ty::Unit(_) => return Err(Error::from("unit cannot be passed as extern arg")),
        _ => return Err(Error::from("unsupported extern argument type")),
    }

    Ok(())
}

fn push_int_arg(int_ty: TypeInt, value: i64, args: &mut Vec<FfiValue>) -> Result<()> {
    match int_ty {
        TypeInt::I8 | TypeInt::I16 | TypeInt::I32 | TypeInt::I64 | TypeInt::BigInt => {
            args.push(FfiValue::I64(value));
        }
        TypeInt::U8 | TypeInt::U16 | TypeInt::U32 | TypeInt::U64 => {
            args.push(FfiValue::U64(value as u64));
        }
        TypeInt::I128 | TypeInt::U128 => {
            return Err(Error::from(
                "unsupported extern 128-bit integer arg without libffi",
            ));
        }
    }
    Ok(())
}

fn c_abi_layout(ty: &Ty) -> Result<CAbiLayout> {
    let resolved = resolve_ffi_ty(ty).unwrap_or_else(|| ty.clone());
    match &resolved {
        Ty::Primitive(primitive) => c_abi_layout_for_primitive(*primitive),
        Ty::Tuple(TypeTuple { types }) => c_abi_layout_for_fields(types),
        Ty::Struct(TypeStruct { repr, fields, .. }) => c_abi_layout_for_struct(fields, repr),
        Ty::Enum(TypeEnum { repr, variants, .. }) => c_abi_layout_for_enum(variants, repr),
        Ty::Structural(TypeStructural { .. }) => Err(Error::from(
            "anonymous structural types are not allowed in C ABI; declare a #[repr(C)] struct",
        )),
        Ty::Array(TypeArray { elem, len }) => {
            let elem_layout = c_abi_layout(elem)?;
            let count = array_len_from_expr(len)?;
            let size = elem_layout
                .size
                .checked_mul(count)
                .ok_or_else(|| Error::from("array size overflow"))?;
            Ok(CAbiLayout {
                size,
                align: elem_layout.align,
                field_offsets: Vec::new(),
            })
        }
        Ty::Reference(_) => Ok(CAbiLayout {
            size: std::mem::size_of::<usize>(),
            align: std::mem::align_of::<usize>(),
            field_offsets: Vec::new(),
        }),
        _ => Err(Error::from("unsupported C ABI layout for type")),
    }
}

fn c_abi_layout_for_fields(fields: &[Ty]) -> Result<CAbiLayout> {
    c_abi_layout_for_fields_with_repr(fields, &ReprOptions::default())
}

fn c_abi_layout_for_struct(
    fields: &[fp_core::ast::StructuralField],
    repr: &ReprOptions,
) -> Result<CAbiLayout> {
    validate_struct_repr_for_c_abi(repr)?;
    if repr.flags.contains(ReprFlags::IS_TRANSPARENT) {
        let transparent = transparent_field_ty(fields)?;
        return c_abi_layout(transparent);
    }
    if !repr.is_c() {
        return Err(Error::from(
            "C ABI struct arguments require explicit #[repr(C)] or #[repr(transparent)]",
        ));
    }
    let field_tys: Vec<Ty> = fields.iter().map(|field| field.value.clone()).collect();
    c_abi_layout_for_fields_with_repr(&field_tys, repr)
}

fn c_abi_layout_for_fields_with_repr(fields: &[Ty], repr: &ReprOptions) -> Result<CAbiLayout> {
    let mut offsets = Vec::with_capacity(fields.len());
    let mut offset = 0usize;
    let pack = repr
        .pack
        .map(|value| parse_repr_align(value, "repr(packed)"))
        .transpose()?;
    let mut max_align = 1usize;
    for field in fields {
        let layout = c_abi_layout(field)?;
        let field_align = pack
            .map(|pack| layout.align.min(pack))
            .unwrap_or(layout.align);
        max_align = max_align.max(field_align);
        offset = align_to(offset, field_align);
        offsets.push(offset);
        offset = offset
            .checked_add(layout.size)
            .ok_or_else(|| Error::from("struct size overflow"))?;
    }
    let explicit_align = repr
        .align
        .map(|value| parse_repr_align(value, "repr(align)"))
        .transpose()?;
    let struct_align = explicit_align
        .map(|align| max_align.max(align))
        .unwrap_or(max_align);
    let size = align_to(offset, struct_align);
    Ok(CAbiLayout {
        size,
        align: struct_align,
        field_offsets: offsets,
    })
}

fn c_abi_layout_for_enum(variants: &[EnumTypeVariant], repr: &ReprOptions) -> Result<CAbiLayout> {
    validate_enum_repr_for_c_abi(variants, repr)?;
    let tag = enum_tag_layout(repr)?;
    if variants.is_empty() {
        return Ok(tag.layout);
    }

    let mut max_size = 0usize;
    let mut max_align = tag.layout.align;
    for variant in variants {
        let payload_fields = enum_variant_payload_fields(&variant.value);
        let mut fields = Vec::with_capacity(payload_fields.len() + 1);
        fields.push(Ty::Primitive(tag.primitive));
        fields.extend_from_slice(&payload_fields);
        let variant_layout = c_abi_layout_for_fields(&fields)?;
        max_size = max_size.max(variant_layout.size);
        max_align = max_align.max(variant_layout.align);
    }
    Ok(CAbiLayout {
        size: align_to(max_size, max_align),
        align: max_align,
        field_offsets: Vec::new(),
    })
}

fn parse_repr_align(value: u64, context: &str) -> Result<usize> {
    let align =
        usize::try_from(value).map_err(|e| Error::from(format!("{context} is too large: {e}")))?;
    if align == 0 || !align.is_power_of_two() {
        return Err(Error::from(format!(
            "{context} requires a non-zero power-of-two alignment"
        )));
    }
    Ok(align)
}

fn align_to(value: usize, alignment: usize) -> usize {
    if alignment <= 1 {
        return value;
    }
    let rem = value % alignment;
    if rem == 0 {
        value
    } else {
        value + (alignment - rem)
    }
}

fn c_abi_layout_for_primitive(primitive: TypePrimitive) -> Result<CAbiLayout> {
    let (size, align) = match primitive {
        TypePrimitive::Bool => (1, 1),
        TypePrimitive::Char => (4, 4),
        TypePrimitive::Int(int_ty) => match int_ty {
            TypeInt::I8 => (1, 1),
            TypeInt::U8 => (1, 1),
            TypeInt::I16 => (2, 2),
            TypeInt::U16 => (2, 2),
            TypeInt::I32 => (4, 4),
            TypeInt::U32 => (4, 4),
            TypeInt::I64 => (8, 8),
            TypeInt::U64 => (8, 8),
            TypeInt::I128 | TypeInt::U128 | TypeInt::BigInt => (16, 16),
        },
        TypePrimitive::Decimal(decimal_ty) => match decimal_ty {
            DecimalType::F32 => (4, 4),
            DecimalType::F64 => (8, 8),
            _ => (8, 8),
        },
        TypePrimitive::String => (std::mem::size_of::<usize>(), std::mem::align_of::<usize>()),
        TypePrimitive::List => {
            return Err(Error::from("unsupported C ABI primitive list type"));
        }
    };
    Ok(CAbiLayout {
        size,
        align,
        field_offsets: Vec::new(),
    })
}

fn validate_struct_repr_for_c_abi(repr: &ReprOptions) -> Result<()> {
    if repr.int.is_some() {
        return Err(Error::from(
            "primitive integer repr is only supported on enums for C ABI",
        ));
    }
    if repr.flags.contains(ReprFlags::IS_SIMD) {
        return Err(Error::from("repr(simd) is not supported for C ABI"));
    }
    if repr.flags.contains(ReprFlags::IS_LINEAR) {
        return Err(Error::from("repr(linear) is not supported for C ABI"));
    }
    if repr.flags.contains(ReprFlags::IS_TRANSPARENT) {
        if repr.is_c() {
            return Err(Error::from(
                "repr(transparent) cannot be combined with repr(C) for C ABI",
            ));
        }
        if repr.flags.contains(ReprFlags::IS_PACKED) || repr.pack.is_some() {
            return Err(Error::from(
                "repr(transparent) cannot be combined with repr(packed)",
            ));
        }
    }
    Ok(())
}

fn validate_enum_repr_for_c_abi(variants: &[EnumTypeVariant], repr: &ReprOptions) -> Result<()> {
    if repr.flags.contains(ReprFlags::IS_TRANSPARENT) {
        return Err(Error::from(
            "repr(transparent) is not supported on enums for C ABI",
        ));
    }
    if repr.flags.contains(ReprFlags::IS_PACKED) || repr.pack.is_some() {
        return Err(Error::from(
            "repr(packed) is not supported on enums for C ABI",
        ));
    }
    if repr.flags.contains(ReprFlags::IS_SIMD) {
        return Err(Error::from(
            "repr(simd) is not supported on enums for C ABI",
        ));
    }
    if repr.flags.contains(ReprFlags::IS_LINEAR) {
        return Err(Error::from(
            "repr(linear) is not supported on enums for C ABI",
        ));
    }
    if repr.int.is_some() && variants.is_empty() {
        return Err(Error::from(
            "primitive integer repr is not supported on zero-variant enums",
        ));
    }
    if !repr.is_c() && repr.int.is_none() {
        return Err(Error::from(
            "C ABI enum arguments require explicit #[repr(C)] or primitive integer repr",
        ));
    }
    Ok(())
}

fn transparent_field_ty<'a>(fields: &'a [fp_core::ast::StructuralField]) -> Result<&'a Ty> {
    let mut non_zero: Option<&Ty> = None;
    for field in fields {
        let layout = c_abi_layout(&field.value)?;
        if layout.size == 0 {
            continue;
        }
        if non_zero.is_some() {
            return Err(Error::from(
                "repr(transparent) requires exactly one non-zero-sized field",
            ));
        }
        non_zero = Some(&field.value);
    }
    non_zero
        .ok_or_else(|| Error::from("repr(transparent) requires exactly one non-zero-sized field"))
}

fn enum_tag_layout(repr: &ReprOptions) -> Result<EnumTagLayout> {
    let primitive = match repr.int {
        Some(int) => repr_int_primitive(int),
        None if repr.is_c() => TypePrimitive::Int(TypeInt::I32),
        None => {
            return Err(Error::from(
                "missing repr(C) or primitive integer repr for C ABI enum",
            ));
        }
    };
    let layout = c_abi_layout_for_primitive(primitive)?;
    Ok(EnumTagLayout { primitive, layout })
}

fn repr_int_primitive(repr: ReprInt) -> TypePrimitive {
    match repr {
        ReprInt::I8 => TypePrimitive::Int(TypeInt::I8),
        ReprInt::I16 => TypePrimitive::Int(TypeInt::I16),
        ReprInt::I32 => TypePrimitive::Int(TypeInt::I32),
        ReprInt::I64 => TypePrimitive::Int(TypeInt::I64),
        ReprInt::I128 => TypePrimitive::Int(TypeInt::BigInt),
        ReprInt::U8 => TypePrimitive::Int(TypeInt::U8),
        ReprInt::U16 => TypePrimitive::Int(TypeInt::U16),
        ReprInt::U32 => TypePrimitive::Int(TypeInt::U32),
        ReprInt::U64 => TypePrimitive::Int(TypeInt::U64),
        ReprInt::U128 => TypePrimitive::Int(TypeInt::U64),
        ReprInt::Isize => TypePrimitive::Int(TypeInt::I64),
        ReprInt::Usize => TypePrimitive::Int(TypeInt::U64),
    }
}

fn enum_variant_payload_fields(variant_ty: &Ty) -> Vec<Ty> {
    match variant_ty {
        Ty::Unit(_) => Vec::new(),
        Ty::Tuple(tuple) => tuple.types.clone(),
        Ty::Struct(struct_ty) => struct_ty
            .fields
            .iter()
            .map(|field| field.value.clone())
            .collect(),
        Ty::Structural(structural) => structural
            .fields
            .iter()
            .map(|field| field.value.clone())
            .collect(),
        other => vec![other.clone()],
    }
}

fn array_len_from_expr(expr: &fp_core::ast::Expr) -> Result<usize> {
    if let ExprKind::Value(value) = expr.kind() {
        match &**value {
            Value::Int(v) if v.value >= 0 => {
                return usize::try_from(v.value)
                    .map_err(|e| Error::from(format!("array length out of range: {e}")));
            }
            _ => {}
        }
    }
    Err(Error::from(
        "array length must be a non-negative integer literal",
    ))
}

fn write_c_abi_value(ty: &Ty, value: &Value, buf: &mut [u8], base: usize) -> Result<()> {
    let resolved = resolve_ffi_ty(ty).unwrap_or_else(|| ty.clone());
    match &resolved {
        Ty::Primitive(primitive) => write_primitive_value(*primitive, value, buf, base),
        Ty::Tuple(TypeTuple { types }) => {
            let layout = c_abi_layout_for_fields(types)?;
            let tuple = match value {
                Value::Tuple(ValueTuple { values }) => values,
                _ => return Err(Error::from("expected tuple value for C ABI tuple")),
            };
            if tuple.len() != types.len() {
                return Err(Error::from("tuple length mismatch for C ABI tuple"));
            }
            for ((field_ty, field_value), offset) in types
                .iter()
                .zip(tuple.iter())
                .zip(layout.field_offsets.iter())
            {
                write_c_abi_value(field_ty, field_value, buf, base + *offset)?;
            }
            Ok(())
        }
        Ty::Struct(TypeStruct { repr, fields, .. }) => {
            validate_struct_repr_for_c_abi(repr)?;
            if repr.flags.contains(ReprFlags::IS_TRANSPARENT) {
                let inner = transparent_field_ty(fields)?;
                return write_transparent_struct_value(fields, inner, value, buf, base);
            }
            write_struct_value_with_repr(fields, repr, value, buf, base)
        }
        Ty::Enum(TypeEnum { repr, variants, .. }) => {
            write_enum_value(variants, repr, value, buf, base)
        }
        Ty::Structural(TypeStructural { .. }) => Err(Error::from(
            "anonymous structural types are not allowed in C ABI; declare a #[repr(C)] struct",
        )),
        Ty::Array(TypeArray { elem, len }) => {
            let count = array_len_from_expr(len)?;
            let elem_layout = c_abi_layout(elem)?;
            let list = match value {
                Value::List(ValueList { values }) => values,
                _ => return Err(Error::from("expected list value for C ABI array")),
            };
            if list.len() != count {
                return Err(Error::from("array length mismatch for C ABI array"));
            }
            for (idx, item) in list.iter().enumerate() {
                let offset = idx
                    .checked_mul(elem_layout.size)
                    .ok_or_else(|| Error::from("array element offset overflow"))?;
                write_c_abi_value(elem, item, buf, base + offset)?;
            }
            Ok(())
        }
        _ => Err(Error::from("unsupported C ABI value type")),
    }
}

fn write_struct_value_with_repr(
    fields: &[fp_core::ast::StructuralField],
    repr: &ReprOptions,
    value: &Value,
    buf: &mut [u8],
    base: usize,
) -> Result<()> {
    let field_tys: Vec<Ty> = fields.iter().map(|field| field.value.clone()).collect();
    let layout = c_abi_layout_for_fields_with_repr(&field_tys, repr)?;
    match value {
        Value::Struct(ValueStruct { structural, .. }) => {
            for ((field_ty, field_value), offset) in field_tys
                .iter()
                .zip(structural.fields.iter())
                .zip(layout.field_offsets.iter())
            {
                write_c_abi_value(field_ty, &field_value.value, buf, base + *offset)?;
            }
            Ok(())
        }
        Value::Structural(ValueStructural { fields: values }) => {
            for ((field_ty, field_value), offset) in field_tys
                .iter()
                .zip(values.iter())
                .zip(layout.field_offsets.iter())
            {
                write_c_abi_value(field_ty, &field_value.value, buf, base + *offset)?;
            }
            Ok(())
        }
        _ => Err(Error::from("expected struct value for C ABI struct")),
    }
}

fn write_transparent_struct_value(
    fields: &[fp_core::ast::StructuralField],
    inner_ty: &Ty,
    value: &Value,
    buf: &mut [u8],
    base: usize,
) -> Result<()> {
    match value {
        Value::Struct(ValueStruct { structural, .. }) => {
            let mut selected: Option<&Value> = None;
            for (field_def, field_value) in fields.iter().zip(structural.fields.iter()) {
                let layout = c_abi_layout(&field_def.value)?;
                if layout.size == 0 {
                    continue;
                }
                selected = Some(&field_value.value);
                break;
            }
            let Some(selected) = selected else {
                return Err(Error::from(
                    "repr(transparent) requires exactly one non-zero-sized field",
                ));
            };
            write_c_abi_value(inner_ty, selected, buf, base)
        }
        Value::Structural(ValueStructural { fields: values }) => {
            let mut selected: Option<&Value> = None;
            for (field_def, field_value) in fields.iter().zip(values.iter()) {
                let layout = c_abi_layout(&field_def.value)?;
                if layout.size == 0 {
                    continue;
                }
                selected = Some(&field_value.value);
                break;
            }
            let Some(selected) = selected else {
                return Err(Error::from(
                    "repr(transparent) requires exactly one non-zero-sized field",
                ));
            };
            write_c_abi_value(inner_ty, selected, buf, base)
        }
        _ => Err(Error::from(
            "expected struct value for repr(transparent) struct",
        )),
    }
}

fn write_enum_value(
    variants: &[EnumTypeVariant],
    repr: &ReprOptions,
    value: &Value,
    buf: &mut [u8],
    base: usize,
) -> Result<()> {
    validate_enum_repr_for_c_abi(variants, repr)?;
    if variants
        .iter()
        .any(|variant| !matches!(variant.value, Ty::Unit(_)))
    {
        return Err(Error::from(
            "native C ABI enum values with payloads are not yet supported",
        ));
    }
    let tag = enum_tag_layout(repr)?;
    write_primitive_value(tag.primitive, value, buf, base)
}

fn write_primitive_value(
    primitive: TypePrimitive,
    value: &Value,
    buf: &mut [u8],
    base: usize,
) -> Result<()> {
    match primitive {
        TypePrimitive::Bool => match value {
            Value::Bool(v) => {
                buf[base] = if v.value { 1 } else { 0 };
                Ok(())
            }
            _ => Err(Error::from("expected bool value")),
        },
        TypePrimitive::Char => match value {
            Value::Char(v) => {
                buf[base] = v.value as u8;
                Ok(())
            }
            _ => Err(Error::from("expected char value")),
        },
        TypePrimitive::Int(int_ty) => match value {
            Value::Int(v) => write_int_value(int_ty, v.value, buf, base),
            _ => Err(Error::from("expected int value")),
        },
        TypePrimitive::Decimal(decimal_ty) => match value {
            Value::Decimal(v) => write_decimal_value(decimal_ty, v.value, buf, base),
            _ => Err(Error::from("expected decimal value")),
        },
        TypePrimitive::String => Err(Error::from("expected string pointer value")),
        TypePrimitive::List => Err(Error::from("unsupported list primitive value")),
    }
}

fn write_int_value(int_ty: TypeInt, value: i64, buf: &mut [u8], base: usize) -> Result<()> {
    match int_ty {
        TypeInt::I8 => buf[base..base + 1].copy_from_slice(&(value as i8).to_ne_bytes()),
        TypeInt::U8 => buf[base..base + 1].copy_from_slice(&(value as u8).to_ne_bytes()),
        TypeInt::I16 => buf[base..base + 2].copy_from_slice(&(value as i16).to_ne_bytes()),
        TypeInt::U16 => buf[base..base + 2].copy_from_slice(&(value as u16).to_ne_bytes()),
        TypeInt::I32 => buf[base..base + 4].copy_from_slice(&(value as i32).to_ne_bytes()),
        TypeInt::U32 => buf[base..base + 4].copy_from_slice(&(value as u32).to_ne_bytes()),
        TypeInt::I64 => buf[base..base + 8].copy_from_slice(&(value as i64).to_ne_bytes()),
        TypeInt::U64 => buf[base..base + 8].copy_from_slice(&(value as u64).to_ne_bytes()),
        TypeInt::I128 | TypeInt::U128 | TypeInt::BigInt => {
            return Err(Error::from("unsupported 128-bit integer in C ABI layout"));
        }
    };
    Ok(())
}

fn write_decimal_value(
    decimal_ty: DecimalType,
    value: f64,
    buf: &mut [u8],
    base: usize,
) -> Result<()> {
    match decimal_ty {
        DecimalType::F32 => {
            let val = value as f32;
            buf[base..base + 4].copy_from_slice(&val.to_ne_bytes());
        }
        DecimalType::F64 => {
            buf[base..base + 8].copy_from_slice(&value.to_ne_bytes());
        }
        _ => {
            return Err(Error::from("unsupported decimal type in C ABI layout"));
        }
    }
    Ok(())
}

fn resolve_ffi_ty(ty: &Ty) -> Option<Ty> {
    match ty {
        Ty::Expr(expr) => match expr.kind() {
            ExprKind::Name(name) => match name {
                Name { path, .. } => match path.last().as_str() {
                    "i128" => Some(Ty::Primitive(TypePrimitive::Int(TypeInt::I128))),
                    "u128" => Some(Ty::Primitive(TypePrimitive::Int(TypeInt::U128))),
                    "i64" => Some(Ty::Primitive(TypePrimitive::Int(TypeInt::I64))),
                    "u64" => Some(Ty::Primitive(TypePrimitive::Int(TypeInt::U64))),
                    "i32" => Some(Ty::Primitive(TypePrimitive::Int(TypeInt::I32))),
                    "u32" => Some(Ty::Primitive(TypePrimitive::Int(TypeInt::U32))),
                    "i16" => Some(Ty::Primitive(TypePrimitive::Int(TypeInt::I16))),
                    "u16" => Some(Ty::Primitive(TypePrimitive::Int(TypeInt::U16))),
                    "i8" => Some(Ty::Primitive(TypePrimitive::Int(TypeInt::I8))),
                    "u8" => Some(Ty::Primitive(TypePrimitive::Int(TypeInt::U8))),
                    "isize" => Some(Ty::Primitive(TypePrimitive::Int(TypeInt::I64))),
                    "usize" => Some(Ty::Primitive(TypePrimitive::Int(TypeInt::U64))),
                    "bool" => Some(Ty::Primitive(TypePrimitive::Bool)),
                    "char" => Some(Ty::Primitive(TypePrimitive::Char)),
                    "f32" => Some(Ty::Primitive(TypePrimitive::Decimal(DecimalType::F32))),
                    "f64" => Some(Ty::Primitive(TypePrimitive::Decimal(DecimalType::F64))),
                    _ => None,
                },
                _ => None,
            },
            _ => None,
        },
        _ => None,
    }
}

fn resolves_to_string(ty: &Ty) -> bool {
    match ty {
        Ty::Expr(expr) => match expr.kind() {
            ExprKind::Name(name) => match name {
                Name { path, .. } => path
                    .segments
                    .last()
                    .is_some_and(|seg| seg.as_str() == "str"),
            },
            _ => false,
        },
        Ty::Primitive(TypePrimitive::String) => true,
        _ => false,
    }
}

fn is_cstr_reference(ty: &Ty) -> bool {
    match ty {
        Ty::Reference(TypeReference { ty, .. }) => match ty.as_ref() {
            Ty::Expr(expr) => match expr.kind() {
                ExprKind::Name(name) => match name {
                    Name { path: path, .. } => path
                        .segments
                        .iter()
                        .map(|seg| seg.as_str())
                        .eq(["std", "ffi", "CStr"].into_iter()),
                    _ => false,
                },
                _ => false,
            },
            _ => false,
        },
        _ => false,
    }
}
