use std::collections::HashMap;
use std::ffi::{c_void, CString};

use fp_core::ast::{
    Abi, DecimalType, ExprKind, FunctionSignature, Name, Ty, TypeArray, TypeInt, TypePrimitive,
    TypeReference, TypeStruct, TypeStructural, TypeTuple, Value, ValueChar, ValueEscaped,
    ValueList, ValuePointer, ValueStruct, ValueStructural, ValueTuple,
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

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct FfiSliceRef {
    pub values: Vec<Value>,
    pub index: usize,
}

#[derive(Debug, Clone)]
struct CAbiLayout {
    size: usize,
    align: usize,
    field_offsets: Vec<usize>,
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
        if !matches!(sig.abi, Abi::C) {
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
        let (arg_types, arg_values, _cstrings, _escapes) = self.build_args(sig, args)?;
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

    fn build_args(
        &self,
        sig: &FunctionSignature,
        args: &[Value],
    ) -> Result<(Vec<Type>, Vec<FfiArgValue>, Vec<CString>, Vec<ValueEscaped>)> {
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
                TypeInt::BigInt => Err(Error::from("unsupported extern bigint return")),
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
            TypeInt::BigInt => return Err(Error::from("unsupported extern bigint arg")),
        }),
        Ty::Primitive(TypePrimitive::Decimal(decimal_ty)) => Ok(match decimal_ty {
            DecimalType::F64 => Type::f64(),
            DecimalType::F32 => Type::f32(),
            _ => return Err(Error::from("unsupported extern decimal arg")),
        }),
        Ty::Primitive(TypePrimitive::String) => Err(Error::from(
            "unsupported extern string arg type; use &CStr",
        )),
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

fn push_arg_value(
    ty: &Ty,
    value: &Value,
    args: &mut Vec<FfiArgValue>,
    cstrings: &mut Vec<CString>,
    escapes: &mut Vec<ValueEscaped>,
) -> Result<()> {
    if is_cstr_reference(ty) {
        match value {
            Value::String(s) => {
                let cstr = CString::new(s.value.clone())
                    .map_err(|_| Error::from("string contains interior NUL"))?;
                let ptr = cstr.as_ptr() as *mut c_void;
                cstrings.push(cstr);
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
            return Err(Error::from(
                "unsupported extern string arg type; use &CStr",
            ));
        }
        Ty::Reference(TypeReference { ty, .. }) => {
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
                if let Some(slice_ref) = any.downcast_ref::<FfiSliceRef>() {
                    let elem_ty = ty.as_ref();
                    let elem_layout = c_abi_layout(elem_ty)?;
                    let buf_size = elem_layout
                        .size
                        .checked_mul(slice_ref.values.len())
                        .ok_or_else(|| Error::from("ffi slice buffer size overflow"))?;
                    let mut escaped =
                        ValueEscaped::new(buf_size as i64, elem_layout.align as i64);
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
                    escapes.push(escaped);
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
            escapes.push(escaped);
        }
        Ty::Unit(_) => return Err(Error::from("unit cannot be passed as extern arg")),
        _ => return Err(Error::from("unsupported extern argument type")),
    }

    Ok(())
}

fn c_abi_layout(ty: &Ty) -> Result<CAbiLayout> {
    let resolved = resolve_ffi_ty(ty).unwrap_or_else(|| ty.clone());
    match &resolved {
        Ty::Primitive(primitive) => c_abi_layout_for_primitive(*primitive),
        Ty::Tuple(TypeTuple { types }) => c_abi_layout_for_fields(types),
        Ty::Struct(TypeStruct { fields, .. }) => {
            let field_tys: Vec<Ty> = fields.iter().map(|field| field.value.clone()).collect();
            c_abi_layout_for_fields(&field_tys)
        }
        Ty::Structural(TypeStructural { fields }) => {
            let field_tys: Vec<Ty> = fields.iter().map(|field| field.value.clone()).collect();
            c_abi_layout_for_fields(&field_tys)
        }
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
    let mut offsets = Vec::with_capacity(fields.len());
    let mut offset = 0usize;
    let mut max_align = 1usize;
    for field in fields {
        let layout = c_abi_layout(field)?;
        max_align = max_align.max(layout.align);
        offset = align_to(offset, layout.align);
        offsets.push(offset);
        offset = offset
            .checked_add(layout.size)
            .ok_or_else(|| Error::from("struct size overflow"))?;
    }
    let size = align_to(offset, max_align);
    Ok(CAbiLayout {
        size,
        align: max_align,
        field_offsets: offsets,
    })
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
            TypeInt::BigInt => (8, 8),
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
    Err(Error::from("array length must be a non-negative integer literal"))
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
        Ty::Struct(TypeStruct { fields, .. }) => write_struct_value(fields, value, buf, base),
        Ty::Structural(TypeStructural { fields }) => write_struct_value(fields, value, buf, base),
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

fn write_struct_value(
    fields: &[fp_core::ast::StructuralField],
    value: &Value,
    buf: &mut [u8],
    base: usize,
) -> Result<()> {
    let field_tys: Vec<Ty> = fields.iter().map(|field| field.value.clone()).collect();
    let layout = c_abi_layout_for_fields(&field_tys)?;
    let struct_fields = match value {
        Value::Struct(ValueStruct { structural, .. }) => &structural.fields,
        Value::Structural(ValueStructural { fields }) => fields,
        _ => return Err(Error::from("expected struct value for C ABI struct")),
    };
    if struct_fields.len() != fields.len() {
        return Err(Error::from("struct field count mismatch for C ABI struct"));
    }
    for (field_def, offset) in fields.iter().zip(layout.field_offsets.iter()) {
        let Some(field_value) = struct_fields
            .iter()
            .find(|field| field.name == field_def.name)
        else {
            return Err(Error::from("missing field value for C ABI struct"));
        };
        write_c_abi_value(&field_def.value, &field_value.value, buf, base + *offset)?;
    }
    Ok(())
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
            Value::Char(ValueChar { value }) => {
                let bytes = (*value as u32).to_ne_bytes();
                buf[base..base + 4].copy_from_slice(&bytes);
                Ok(())
            }
            _ => Err(Error::from("expected char value")),
        },
        TypePrimitive::Int(int_ty) => match value {
            Value::Int(v) => write_int_value(int_ty, v.value, buf, base),
            Value::Bool(v) => write_int_value(int_ty, if v.value { 1 } else { 0 }, buf, base),
            Value::Char(v) => write_int_value(int_ty, v.value as i64, buf, base),
            _ => Err(Error::from("expected integer value")),
        },
        TypePrimitive::Decimal(decimal_ty) => match value {
            Value::Decimal(v) => match decimal_ty {
                DecimalType::F32 => {
                    let bytes = (v.value as f32).to_ne_bytes();
                    buf[base..base + 4].copy_from_slice(&bytes);
                    Ok(())
                }
                DecimalType::F64 => {
                    let bytes = (v.value as f64).to_ne_bytes();
                    buf[base..base + 8].copy_from_slice(&bytes);
                    Ok(())
                }
                _ => Err(Error::from("unsupported decimal value for C ABI")),
            },
            _ => Err(Error::from("expected decimal value")),
        },
        TypePrimitive::String => Err(Error::from("unsupported C ABI string value")),
        TypePrimitive::List => Err(Error::from("unsupported C ABI list value")),
    }
}

fn write_int_value(int_ty: TypeInt, value: i64, buf: &mut [u8], base: usize) -> Result<()> {
    match int_ty {
        TypeInt::I8 => buf[base] = value as i8 as u8,
        TypeInt::U8 => buf[base] = value as u8,
        TypeInt::I16 => {
            let bytes = (value as i16).to_ne_bytes();
            buf[base..base + 2].copy_from_slice(&bytes);
        }
        TypeInt::U16 => {
            let bytes = (value as u16).to_ne_bytes();
            buf[base..base + 2].copy_from_slice(&bytes);
        }
        TypeInt::I32 => {
            let bytes = (value as i32).to_ne_bytes();
            buf[base..base + 4].copy_from_slice(&bytes);
        }
        TypeInt::U32 => {
            let bytes = (value as u32).to_ne_bytes();
            buf[base..base + 4].copy_from_slice(&bytes);
        }
        TypeInt::I64 => {
            let bytes = (value as i64).to_ne_bytes();
            buf[base..base + 8].copy_from_slice(&bytes);
        }
        TypeInt::U64 => {
            let bytes = (value as u64).to_ne_bytes();
            buf[base..base + 8].copy_from_slice(&bytes);
        }
        TypeInt::BigInt => {
            let bytes = (value as i64).to_ne_bytes();
            buf[base..base + 8].copy_from_slice(&bytes);
        }
    }
    Ok(())
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
        TypeInt::BigInt => return Err(Error::from("unsupported extern bigint arg")),
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
        match unsafe { Library::new(name) } {
            Ok(lib) => return Ok(lib),
            Err(err) => last_err = Some(err),
        }
    }

    Err(Error::from(format!(
        "failed to load libc: {:?}",
        last_err
    )))
}
