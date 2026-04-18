use super::{RuntimeEnum, RuntimeRef, RuntimeRefTarget};
use std::collections::HashMap;
use std::ffi::{c_char, c_void, CStr, CString};
use std::sync::MutexGuard;

use fp_core::ast::{
    DecimalType, EnumTypeVariant, ExprKind, FunctionSignature, Name, ReprFlags, ReprInt,
    ReprOptions, StructuralField, Ty, TypeArray, TypeEnum, TypeInt, TypePrimitive, TypeReference,
    TypeStruct, TypeStructural, TypeTuple, Value, ValueChar, ValueEscaped, ValueList, ValuePointer,
    ValueStruct, ValueStructural, ValueTuple,
};
use fp_core::error::{Error, Result};
use libffi::middle::{Arg, Cif, CodePtr, Type};
use libloading::Library;

#[derive(Debug)]
pub struct FfiRuntime {
    lib: Library,
    symbols: HashMap<String, *const c_void>,
}

#[derive(Debug)]
enum FfiArgValue {
    I8(i8),
    U8(u8),
    I16(i16),
    U16(u16),
    I32(i32),
    U32(u32),
    I64(i64),
    U64(u64),
    F32(f32),
    F64(f64),
    Ptr(*mut c_void),
}

#[allow(dead_code)]
enum FfiArgBacking<'a> {
    CString(CString),
    Escaped(ValueEscaped),
    RuntimeRef(MutexGuard<'a, Value>),
}

impl FfiRuntime {
    pub fn new() -> Result<Self> {
        let lib = load_libc()?;
        Ok(Self {
            lib,
            symbols: HashMap::new(),
        })
    }

    pub fn call(&mut self, name: &str, sig: &FunctionSignature, args: &[Value]) -> Result<Value> {
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

        let func_ptr = self.resolve_symbol(name)?;
        let (arg_types, arg_values, _backings) = self.build_args(sig, args)?;
        let ret_ty = ffi_type_for_return(sig.ret_ty.as_ref())?;

        let cif = Cif::new(arg_types, ret_ty.clone());
        let ffi_args: Vec<Arg<'_>> = arg_values
            .iter()
            .map(|value| match value {
                FfiArgValue::I8(v) => Arg::new(v),
                FfiArgValue::U8(v) => Arg::new(v),
                FfiArgValue::I16(v) => Arg::new(v),
                FfiArgValue::U16(v) => Arg::new(v),
                FfiArgValue::I32(v) => Arg::new(v),
                FfiArgValue::U32(v) => Arg::new(v),
                FfiArgValue::I64(v) => Arg::new(v),
                FfiArgValue::U64(v) => Arg::new(v),
                FfiArgValue::F32(v) => Arg::new(v),
                FfiArgValue::F64(v) => Arg::new(v),
                FfiArgValue::Ptr(v) => Arg::new(v),
            })
            .collect();

        let code = CodePtr::from_ptr(func_ptr as *mut c_void);
        unsafe { self.invoke(&cif, code, sig.ret_ty.as_ref(), &ffi_args) }
    }

    fn resolve_symbol(&mut self, name: &str) -> Result<*const c_void> {
        if let Some(ptr) = self.symbols.get(name).copied() {
            return Ok(ptr);
        }
        let symbol: libloading::Symbol<*const c_void> = unsafe {
            self.lib
                .get(name.as_bytes())
                .map_err(|e| Error::from(e.to_string()))?
        };
        let ptr = *symbol;
        self.symbols.insert(name.to_string(), ptr);
        Ok(ptr)
    }

    fn build_args<'a>(
        &self,
        sig: &FunctionSignature,
        args: &'a [Value],
    ) -> Result<(Vec<Type>, Vec<FfiArgValue>, Vec<FfiArgBacking<'a>>)> {
        let mut arg_types = Vec::with_capacity(sig.params.len());
        let mut arg_values = Vec::with_capacity(sig.params.len());
        let mut backings = Vec::new();

        for (param, value) in sig.params.iter().zip(args.iter()) {
            let ty = &param.ty;
            let ffi_ty = ffi_type_for_arg(ty)?;
            arg_types.push(ffi_ty);
            push_arg_value(ty, value, &mut arg_values, &mut backings)?;
        }

        Ok((arg_types, arg_values, backings))
    }

    unsafe fn invoke(
        &self,
        cif: &Cif,
        code: CodePtr,
        ret_ty: Option<&Ty>,
        args: &[Arg<'_>],
    ) -> Result<Value> {
        let ret_ty = match ret_ty {
            Some(ty) => resolve_ffi_ty(ty).unwrap_or_else(|| ty.clone()),
            None => return Ok(Value::unit()),
        };
        match &ret_ty {
            Ty::Unit(_) => {
                let _: () = cif.call(code, args);
                Ok(Value::unit())
            }
            Ty::Primitive(TypePrimitive::Bool) => {
                let result: u8 = cif.call(code, args);
                Ok(Value::bool(result != 0))
            }
            Ty::Primitive(TypePrimitive::Char) => {
                let result: u8 = cif.call(code, args);
                Ok(Value::Char(ValueChar::new(result as char)))
            }
            Ty::Primitive(TypePrimitive::Int(int_ty)) => match int_ty {
                TypeInt::I8 => Ok(Value::int(cif.call::<i8>(code, args) as i64)),
                TypeInt::U8 => Ok(Value::int(cif.call::<u8>(code, args) as i64)),
                TypeInt::I16 => Ok(Value::int(cif.call::<i16>(code, args) as i64)),
                TypeInt::U16 => Ok(Value::int(cif.call::<u16>(code, args) as i64)),
                TypeInt::I32 => Ok(Value::int(cif.call::<i32>(code, args) as i64)),
                TypeInt::U32 => Ok(Value::int(cif.call::<u32>(code, args) as i64)),
                TypeInt::I64 => Ok(Value::int(cif.call::<i64>(code, args))),
                TypeInt::U64 => Ok(Value::int(cif.call::<u64>(code, args) as i64)),
                TypeInt::I128 | TypeInt::U128 | TypeInt::BigInt => {
                    Err(Error::from("unsupported extern bigint return"))
                }
            },
            Ty::Primitive(TypePrimitive::Decimal(decimal_ty)) => match decimal_ty {
                DecimalType::F64 => Ok(Value::decimal(cif.call::<f64>(code, args))),
                DecimalType::F32 => {
                    let result: f32 = cif.call(code, args);
                    Ok(Value::decimal(result as f64))
                }
                _ => Err(Error::from("unsupported extern decimal return")),
            },
            Ty::Primitive(TypePrimitive::String) => Err(Error::from(
                "unsupported extern string return type; use &CStr",
            )),
            Ty::Reference(TypeReference { ty, .. }) => {
                if resolves_to_string(ty.as_ref()) {
                    return Err(Error::from(
                        "unsupported extern &str return type; use &CStr",
                    ));
                }
                let ptr: *mut c_void = cif.call(code, args);
                if is_cstr_reference(&ret_ty) {
                    if ptr.is_null() {
                        return Ok(Value::string(String::new()));
                    }
                    let text = unsafe { CStr::from_ptr(ptr as *const c_char) }
                        .to_string_lossy()
                        .into_owned();
                    return Ok(Value::string(text));
                }
                Ok(Value::Pointer(ValuePointer::new(ptr as i64)))
            }
            _ => Err(Error::from("unsupported extern return type")),
        }
    }
}

fn ffi_type_for_arg(ty: &Ty) -> Result<Type> {
    if is_cstr_reference(ty) {
        return Ok(Type::pointer());
    }
    let resolved = resolve_ffi_ty(ty).unwrap_or_else(|| ty.clone());
    match &resolved {
        Ty::Primitive(TypePrimitive::Bool) => Ok(Type::u8()),
        Ty::Primitive(TypePrimitive::Char) => Ok(Type::u8()),
        Ty::Primitive(TypePrimitive::Int(int_ty)) => Ok(match int_ty {
            TypeInt::I8 => Type::i8(),
            TypeInt::U8 => Type::u8(),
            TypeInt::I16 => Type::i16(),
            TypeInt::U16 => Type::u16(),
            TypeInt::I32 => Type::i32(),
            TypeInt::U32 => Type::u32(),
            TypeInt::I64 => Type::i64(),
            TypeInt::U64 => Type::u64(),
            TypeInt::I128 | TypeInt::U128 | TypeInt::BigInt => {
                return Err(Error::from("unsupported extern bigint arg"));
            }
        }),
        Ty::Primitive(TypePrimitive::Decimal(decimal_ty)) => Ok(match decimal_ty {
            DecimalType::F64 => Type::f64(),
            DecimalType::F32 => Type::f32(),
            _ => return Err(Error::from("unsupported extern decimal arg")),
        }),
        Ty::Primitive(TypePrimitive::String) => {
            Err(Error::from("unsupported extern string arg type; use &CStr"))
        }
        Ty::Reference(TypeReference { ty, .. }) => {
            if resolves_to_string(ty.as_ref()) {
                Err(Error::from("unsupported extern &str arg type; use &CStr"))
            } else {
                Ok(Type::pointer())
            }
        }
        Ty::Unit(_) => Err(Error::from("unsupported extern unit arg")),
        _ => Err(Error::from("unsupported extern arg type")),
    }
}

fn ffi_type_for_return(ty: Option<&Ty>) -> Result<Type> {
    match ty {
        None => Ok(Type::void()),
        Some(Ty::Unit(_)) => Ok(Type::void()),
        Some(other) => {
            if is_cstr_reference(other) {
                Ok(Type::pointer())
            } else {
                ffi_type_for_arg(other)
            }
        }
    }
}

pub(crate) fn validate_ffi_signature(sig: &FunctionSignature) -> Result<()> {
    for param in &sig.params {
        let _ = ffi_type_for_arg(&param.ty)?;
    }
    let _ = ffi_type_for_return(sig.ret_ty.as_ref())?;
    Ok(())
}

pub(crate) fn validate_ffi_signature_with_receiver(
    sig: &FunctionSignature,
    receiver_ty: &Ty,
) -> Result<()> {
    let _ = ffi_type_for_arg(receiver_ty)?;
    for param in &sig.params {
        let _ = ffi_type_for_arg(&param.ty)?;
    }
    let _ = ffi_type_for_return(sig.ret_ty.as_ref())?;
    Ok(())
}

fn push_arg_value<'a>(
    ty: &Ty,
    value: &'a Value,
    args: &mut Vec<FfiArgValue>,
    backings: &mut Vec<FfiArgBacking<'a>>,
) -> Result<()> {
    if is_cstr_reference(ty) {
        match value {
            Value::String(s) => {
                let cstr = CString::new(s.value.clone())
                    .map_err(|_| Error::from("string contains interior NUL"))?;
                let ptr = cstr.as_ptr() as *mut c_void;
                backings.push(FfiArgBacking::CString(cstr));
                args.push(FfiArgValue::Ptr(ptr));
            }
            Value::Pointer(ptr) => args.push(FfiArgValue::Ptr(ptr.value as *mut c_void)),
            Value::Null(_) => args.push(FfiArgValue::Ptr(std::ptr::null_mut())),
            _ => return Err(Error::from("expected CStr argument")),
        }
        return Ok(());
    }

    let resolved = resolve_ffi_ty(ty).unwrap_or_else(|| ty.clone());
    match &resolved {
        Ty::Primitive(TypePrimitive::Bool) => match value {
            Value::Bool(v) => args.push(FfiArgValue::U8(if v.value { 1 } else { 0 })),
            _ => return Err(Error::from("expected bool argument")),
        },
        Ty::Primitive(TypePrimitive::Char) => match value {
            Value::Char(v) => args.push(FfiArgValue::U8(v.value as u8)),
            _ => return Err(Error::from("expected char argument")),
        },
        Ty::Primitive(TypePrimitive::Int(int_ty)) => match value {
            Value::Int(v) => push_int_arg(*int_ty, v.value, args)?,
            Value::Bool(v) => push_int_arg(*int_ty, if v.value { 1 } else { 0 }, args)?,
            Value::Char(v) => push_int_arg(*int_ty, v.value as i64, args)?,
            _ => return Err(Error::from("expected integer argument")),
        },
        Ty::Primitive(TypePrimitive::Decimal(decimal_ty)) => match value {
            Value::Decimal(v) => push_float_arg(*decimal_ty, v.value, args)?,
            _ => return Err(Error::from("expected float argument")),
        },
        Ty::Primitive(TypePrimitive::String) => {
            return Err(Error::from("unsupported extern string arg type; use &CStr"));
        }
        Ty::Reference(TypeReference { ty, mutability, .. }) => {
            if resolves_to_string(ty.as_ref()) {
                return Err(Error::from("unsupported extern &str arg type; use &CStr"));
            }
            if let Value::Pointer(ptr) = value {
                args.push(FfiArgValue::Ptr(ptr.value as *mut c_void));
                return Ok(());
            }
            if let Value::Null(_) = value {
                args.push(FfiArgValue::Ptr(std::ptr::null_mut()));
                return Ok(());
            }
            if let Value::Escaped(escaped) = value {
                args.push(FfiArgValue::Ptr(escaped.ptr.value as *mut c_void));
                return Ok(());
            }
            if let Value::Any(any) = value {
                if let Some(runtime_ref) = any.downcast_ref::<RuntimeRef>() {
                    if push_runtime_ref_arg(ty.as_ref(), runtime_ref, args, backings)? {
                        return Ok(());
                    }
                    if mutability.unwrap_or(false) {
                        return Err(Error::from(
                            "unsupported mutable extern reference argument type; direct addressable storage is unavailable",
                        ));
                    }
                }
                if let Some(slice_ref) = any.downcast_ref::<fp_native::ffi::FfiSliceRef>() {
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
                    args.push(FfiArgValue::Ptr(ptr));
                    backings.push(FfiArgBacking::Escaped(escaped));
                    return Ok(());
                }
            }
            let layout = c_abi_layout(ty.as_ref())?;
            let mut escaped = ValueEscaped::new(layout.size as i64, layout.align as i64);
            {
                let buf = unsafe { escaped.as_slice_mut() };
                write_c_abi_value(ty.as_ref(), value, buf, 0)?;
            }
            args.push(FfiArgValue::Ptr(escaped.ptr.value as *mut c_void));
            backings.push(FfiArgBacking::Escaped(escaped));
        }
        Ty::Unit(_) => return Err(Error::from("unit cannot be passed as extern arg")),
        _ => return Err(Error::from("unsupported extern argument type")),
    }

    Ok(())
}

fn push_runtime_ref_arg<'a>(
    ty: &Ty,
    runtime_ref: &'a RuntimeRef,
    args: &mut Vec<FfiArgValue>,
    backings: &mut Vec<FfiArgBacking<'a>>,
) -> Result<bool> {
    let mut guard = match runtime_ref.shared().lock() {
        Ok(guard) => guard,
        Err(err) => err.into_inner(),
    };
    let Some(ptr) = runtime_ref_ptr(ty, runtime_ref, &mut guard)? else {
        return Ok(false);
    };
    args.push(FfiArgValue::Ptr(ptr));
    backings.push(FfiArgBacking::RuntimeRef(guard));
    Ok(true)
}

fn runtime_ref_ptr(
    ty: &Ty,
    runtime_ref: &RuntimeRef,
    root: &mut Value,
) -> Result<Option<*mut c_void>> {
    if let Some(bytes_ptr) = runtime_ref_bytes_ptr(ty, &runtime_ref.target, root)? {
        return Ok(Some(bytes_ptr));
    }
    let Some(target) = runtime_ref_target_value_mut(&runtime_ref.target, root)? else {
        return Ok(None);
    };
    Ok(direct_scalar_ptr(ty, target))
}

fn runtime_ref_bytes_ptr(
    ty: &Ty,
    target: &RuntimeRefTarget,
    root: &mut Value,
) -> Result<Option<*mut c_void>> {
    if !is_direct_byte_ty(ty) {
        return Ok(None);
    }

    match target {
        RuntimeRefTarget::Whole(_) => match root {
            Value::Bytes(bytes) => Ok(Some(bytes.value.as_mut_ptr().cast())),
            _ => Ok(None),
        },
        RuntimeRefTarget::Slice { start, end, .. } => match root {
            Value::Bytes(bytes) => {
                if *start > *end || *end > bytes.value.len() {
                    return Err(Error::from("range slice is out of bounds"));
                }
                Ok(Some(unsafe { bytes.value.as_mut_ptr().add(*start) }.cast()))
            }
            _ => Ok(None),
        },
        _ => Ok(None),
    }
}

fn runtime_ref_target_value_mut<'a>(
    target: &RuntimeRefTarget,
    root: &'a mut Value,
) -> Result<Option<&'a mut Value>> {
    match target {
        RuntimeRefTarget::Whole(_) => Ok(Some(root)),
        RuntimeRefTarget::Field { field, .. } => match root {
            Value::Struct(struct_value) => Ok(struct_value
                .structural
                .fields
                .iter_mut()
                .find(|existing| existing.name.as_str() == field.as_str())
                .map(|existing| &mut existing.value)),
            Value::Structural(structural) => Ok(structural
                .fields
                .iter_mut()
                .find(|existing| existing.name.as_str() == field.as_str())
                .map(|existing| &mut existing.value)),
            _ => Err(Error::from("field reference requires struct value")),
        },
        RuntimeRefTarget::Index { index, .. } => match root {
            Value::List(list) => Ok(list.values.get_mut(*index)),
            Value::Tuple(tuple) => Ok(tuple.values.get_mut(*index)),
            _ => Err(Error::from("index reference requires list or tuple")),
        },
        RuntimeRefTarget::Slice { .. } => Ok(None),
    }
}

fn direct_scalar_ptr(ty: &Ty, value: &mut Value) -> Option<*mut c_void> {
    let resolved = resolve_ffi_ty(ty).unwrap_or_else(|| ty.clone());
    match resolved {
        Ty::Primitive(TypePrimitive::Bool) => match value {
            Value::Bool(v) => Some((&mut v.value as *mut bool).cast()),
            _ => None,
        },
        Ty::Primitive(TypePrimitive::Int(TypeInt::I64))
        | Ty::Primitive(TypePrimitive::Int(TypeInt::U64)) => match value {
            Value::Int(v) => Some((&mut v.value as *mut i64).cast()),
            _ => None,
        },
        Ty::Primitive(TypePrimitive::Decimal(DecimalType::F64)) => match value {
            Value::Decimal(v) => Some((&mut v.value as *mut f64).cast()),
            _ => None,
        },
        _ => None,
    }
}

fn is_direct_byte_ty(ty: &Ty) -> bool {
    matches!(
        resolve_ffi_ty(ty).unwrap_or_else(|| ty.clone()),
        Ty::Primitive(TypePrimitive::Int(TypeInt::I8))
            | Ty::Primitive(TypePrimitive::Int(TypeInt::U8))
    )
}

fn push_int_arg(int_ty: TypeInt, value: i64, args: &mut Vec<FfiArgValue>) -> Result<()> {
    match int_ty {
        TypeInt::I8 => args.push(FfiArgValue::I8(value as i8)),
        TypeInt::U8 => args.push(FfiArgValue::U8(value as u8)),
        TypeInt::I16 => args.push(FfiArgValue::I16(value as i16)),
        TypeInt::U16 => args.push(FfiArgValue::U16(value as u16)),
        TypeInt::I32 => args.push(FfiArgValue::I32(value as i32)),
        TypeInt::U32 => args.push(FfiArgValue::U32(value as u32)),
        TypeInt::I64 => args.push(FfiArgValue::I64(value)),
        TypeInt::U64 => args.push(FfiArgValue::U64(value as u64)),
        TypeInt::I128 | TypeInt::U128 | TypeInt::BigInt => {
            return Err(Error::from("unsupported extern bigint arg"));
        }
    }
    Ok(())
}

fn push_float_arg(decimal_ty: DecimalType, value: f64, args: &mut Vec<FfiArgValue>) -> Result<()> {
    match decimal_ty {
        DecimalType::F64 => args.push(FfiArgValue::F64(value)),
        DecimalType::F32 => args.push(FfiArgValue::F32(value as f32)),
        _ => return Err(Error::from("unsupported extern decimal arg")),
    }
    Ok(())
}

fn resolve_ffi_ty(ty: &Ty) -> Option<Ty> {
    let Ty::Expr(expr) = ty else {
        return None;
    };
    let ExprKind::Name(locator) = expr.kind() else {
        return None;
    };
    let name = match locator {
        Name::Ident(ident) => ident.as_str(),
        Name::Path(path) => path.segments.last().map(|seg| seg.as_str())?,
        Name::ParameterPath(path) => path.last().map(|seg| seg.ident.as_str())?,
    };

    let primitive = match name {
        "i128" => TypePrimitive::Int(TypeInt::I128),
        "u128" => TypePrimitive::Int(TypeInt::U128),
        "i64" => TypePrimitive::Int(TypeInt::I64),
        "u64" => TypePrimitive::Int(TypeInt::U64),
        "i32" => TypePrimitive::Int(TypeInt::I32),
        "u32" => TypePrimitive::Int(TypeInt::U32),
        "i16" => TypePrimitive::Int(TypeInt::I16),
        "u16" => TypePrimitive::Int(TypeInt::U16),
        "i8" => TypePrimitive::Int(TypeInt::I8),
        "u8" => TypePrimitive::Int(TypeInt::U8),
        "isize" => TypePrimitive::Int(TypeInt::I64),
        "usize" => TypePrimitive::Int(TypeInt::U64),
        "bool" => TypePrimitive::Bool,
        "char" => TypePrimitive::Char,
        "str" | "String" | "string" => TypePrimitive::String,
        _ => return None,
    };

    Some(Ty::Primitive(primitive))
}

fn resolves_to_string(ty: &Ty) -> bool {
    match resolve_ffi_ty(ty) {
        Some(Ty::Primitive(TypePrimitive::String)) => true,
        _ => matches!(ty, Ty::Primitive(TypePrimitive::String)),
    }
}

fn is_cstr_reference(ty: &Ty) -> bool {
    let Ty::Reference(TypeReference { ty, .. }) = ty else {
        return false;
    };
    let Some(name) = locator_name(ty.as_ref()) else {
        return false;
    };
    name == "CStr"
}

fn locator_name(ty: &Ty) -> Option<&str> {
    let Ty::Expr(expr) = ty else {
        return None;
    };
    let ExprKind::Name(locator) = expr.kind() else {
        return None;
    };
    match locator {
        Name::Ident(ident) => Some(ident.as_str()),
        Name::Path(path) => path.segments.last().map(|seg| seg.as_str()),
        Name::ParameterPath(path) => path.last().map(|seg| seg.ident.as_str()),
    }
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

fn c_abi_layout_for_struct(fields: &[StructuralField], repr: &ReprOptions) -> Result<CAbiLayout> {
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
        usize::try_from(value).map_err(|_| Error::from(format!("{context} is too large")))?;
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

fn transparent_field_ty<'a>(fields: &'a [StructuralField]) -> Result<&'a Ty> {
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
        ReprInt::I128 => TypePrimitive::Int(TypeInt::I128),
        ReprInt::U8 => TypePrimitive::Int(TypeInt::U8),
        ReprInt::U16 => TypePrimitive::Int(TypeInt::U16),
        ReprInt::U32 => TypePrimitive::Int(TypeInt::U32),
        ReprInt::U64 => TypePrimitive::Int(TypeInt::U64),
        ReprInt::U128 => TypePrimitive::Int(TypeInt::U128),
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
                    .map_err(|_| Error::from("array length out of range"));
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
    fields: &[StructuralField],
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
    fields: &[StructuralField],
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
    let tag = enum_tag_layout(repr)?;
    match value {
        Value::Any(any) => {
            let Some(enum_value) = any.downcast_ref::<RuntimeEnum>() else {
                return Err(Error::from("expected runtime enum value for C ABI enum"));
            };
            let discriminant = enum_value
                .discriminant
                .ok_or_else(|| Error::from("runtime enum is missing a discriminant"))?;
            let variant = variants
                .iter()
                .find(|variant| variant.name.as_str() == enum_value.variant_name.as_str())
                .ok_or_else(|| Error::from("enum variant does not match enum type"))?;

            for byte in buf
                .iter_mut()
                .skip(base)
                .take(c_abi_layout_for_enum(variants, repr)?.size)
            {
                *byte = 0;
            }
            write_primitive_value(tag.primitive, &Value::int(discriminant), buf, base)?;

            let payload_fields = enum_variant_payload_fields(&variant.value);
            if payload_fields.is_empty() {
                return Ok(());
            }
            let mut variant_fields = Vec::with_capacity(payload_fields.len() + 1);
            variant_fields.push(Ty::Primitive(tag.primitive));
            variant_fields.extend_from_slice(&payload_fields);
            let layout = c_abi_layout_for_fields(&variant_fields)?;
            let payload_values = enum_payload_values(&variant.value, enum_value.payload.as_ref())?;
            for ((field_ty, field_value), offset) in payload_fields
                .iter()
                .zip(payload_values.iter())
                .zip(layout.field_offsets.iter().skip(1))
            {
                write_c_abi_value(field_ty, field_value, buf, base + *offset)?;
            }
            Ok(())
        }
        other => {
            if variants
                .iter()
                .all(|variant| matches!(variant.value, Ty::Unit(_)))
            {
                write_primitive_value(tag.primitive, other, buf, base)
            } else {
                Err(Error::from(
                    "C ABI enum values with payloads require an interpreter runtime enum value",
                ))
            }
        }
    }
}

fn enum_payload_values<'a>(variant_ty: &Ty, payload: Option<&'a Value>) -> Result<Vec<&'a Value>> {
    match variant_ty {
        Ty::Unit(_) => Ok(Vec::new()),
        Ty::Tuple(tuple) => {
            let Some(Value::Tuple(value_tuple)) = payload else {
                return Err(Error::from("expected tuple payload for enum variant"));
            };
            if value_tuple.values.len() != tuple.types.len() {
                return Err(Error::from("enum tuple payload length mismatch"));
            }
            Ok(value_tuple.values.iter().collect())
        }
        Ty::Struct(struct_ty) => {
            let Some(Value::Struct(value_struct)) = payload else {
                return Err(Error::from("expected struct payload for enum variant"));
            };
            if value_struct.structural.fields.len() != struct_ty.fields.len() {
                return Err(Error::from("enum struct payload field mismatch"));
            }
            Ok(value_struct
                .structural
                .fields
                .iter()
                .map(|field| &field.value)
                .collect())
        }
        Ty::Structural(structural) => {
            let Some(Value::Structural(value_structural)) = payload else {
                return Err(Error::from("expected structural payload for enum variant"));
            };
            if value_structural.fields.len() != structural.fields.len() {
                return Err(Error::from("enum structural payload field mismatch"));
            }
            Ok(value_structural
                .fields
                .iter()
                .map(|field| &field.value)
                .collect())
        }
        _ => payload
            .map(|value| vec![value])
            .ok_or_else(|| Error::from("expected payload for enum variant")),
    }
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

fn load_libc() -> Result<Library> {
    let candidates: &[&str] = if cfg!(target_os = "linux") {
        &["libc.so.6", "libc.so"]
    } else if cfg!(target_os = "macos") {
        &["/usr/lib/libSystem.B.dylib"]
    } else if cfg!(target_os = "windows") {
        &["msvcrt.dll", "ucrtbase.dll"]
    } else {
        &[]
    };

    let mut last_err = None;
    for name in candidates {
        match unsafe { Library::new(*name) } {
            Ok(lib) => return Ok(lib),
            Err(err) => last_err = Some(err),
        }
    }

    Err(Error::from(format!("failed to load libc: {:?}", last_err)))
}
