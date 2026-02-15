use fp_core::ast::{
    Abi, DecimalType, ExprKind, FunctionSignature, Name, Ty, TypeArray, TypeInt, TypePrimitive,
    TypeReference, TypeStruct, TypeStructural, TypeTuple, Value, ValueChar, ValueEscaped,
    ValueList, ValuePointer, ValueStruct, ValueStructural, ValueTuple,
};
use fp_core::error::{Error, Result};
use std::collections::HashMap;
use std::ffi::{CStr, CString, c_char, c_void};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FfiType {
    I64,
    U64,
    Ptr,
    Void,
}

#[derive(Debug, Clone, Copy)]
pub enum FfiValue {
    I64(i64),
    U64(u64),
    Ptr(*mut c_void),
}

impl FfiValue {
    fn as_u64(self) -> u64 {
        match self {
            FfiValue::I64(v) => v as u64,
            FfiValue::U64(v) => v,
            FfiValue::Ptr(ptr) => ptr as u64,
        }
    }
}

#[derive(Debug, Clone)]
pub struct FfiSignature {
    pub args: Vec<FfiType>,
    pub ret: FfiType,
}

#[derive(Debug)]
pub struct FfiRuntime {
    lib: DynamicLibrary,
    symbols: HashMap<String, *const c_void>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FfiSliceRef {
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
        Ok(Self {
            lib: DynamicLibrary::open_default()?,
            symbols: HashMap::new(),
        })
    }

    pub fn call(
        &mut self,
        name: &str,
        sig: &FfiSignature,
        args: &[FfiValue],
    ) -> Result<Option<FfiValue>> {
        if sig.args.len() != args.len() {
            return Err(Error::from(format!(
                "ffi call '{name}' expects {} args, got {}",
                sig.args.len(),
                args.len()
            )));
        }
        if sig.args.iter().any(|ty| matches!(ty, FfiType::Void)) {
            return Err(Error::from("ffi arguments cannot be void"));
        }
        let fn_ptr = self.resolve_symbol(name)?;
        let mut raw_args = Vec::with_capacity(args.len());
        for value in args {
            raw_args.push(value.as_u64());
        }
        let ret = match sig.ret {
            FfiType::Void => {
                unsafe { call_void(fn_ptr, &raw_args)? };
                None
            }
            FfiType::I64 => Some(FfiValue::I64(unsafe { call_i64(fn_ptr, &raw_args)? })),
            FfiType::U64 => Some(FfiValue::U64(unsafe { call_u64(fn_ptr, &raw_args)? })),
            FfiType::Ptr => {
                let ptr = unsafe { call_ptr(fn_ptr, &raw_args)? };
                Some(FfiValue::Ptr(ptr))
            }
        };
        Ok(ret)
    }

    pub fn call_fp(
        &mut self,
        name: &str,
        sig: &FunctionSignature,
        args: &[Value],
    ) -> Result<Value> {
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

        let (arg_types, arg_values, _cstrings, _escapes) = build_args(sig, args)?;
        let ret_ty = ffi_type_for_return(sig.ret_ty.as_ref())?;
        let signature = FfiSignature {
            args: arg_types,
            ret: ret_ty,
        };
        let result = self.call(name, &signature, &arg_values)?;
        convert_return(sig.ret_ty.as_ref(), result)
    }

    fn resolve_symbol(&mut self, name: &str) -> Result<*const c_void> {
        if let Some(ptr) = self.symbols.get(name).copied() {
            return Ok(ptr);
        }
        let symbol = self.lib.symbol(name)?;
        self.symbols.insert(name.to_string(), symbol);
        Ok(symbol)
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
                    .map_err(|_| Error::from("string contains interior NUL"))?;
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
            if let Value::Any(any) = value {
                if let Some(slice_ref) = any.downcast_ref::<FfiSliceRef>() {
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
        TypeInt::I64 | TypeInt::BigInt => {
            buf[base..base + 8].copy_from_slice(&(value as i64).to_ne_bytes())
        }
        TypeInt::U64 => buf[base..base + 8].copy_from_slice(&(value as u64).to_ne_bytes()),
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
            ExprKind::Name(locator) => match locator {
                Name::Ident(ident) => match ident.as_str() {
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
            ExprKind::Name(locator) => match locator {
                Name::Path(path) => path.segments.last().map(|seg| seg.as_str()) == Some("str"),
                Name::Ident(ident) => ident.as_str() == "str",
                _ => false,
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
                ExprKind::Name(locator) => match locator {
                    Name::Path(path) => path
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

unsafe fn call_void(fn_ptr: *const c_void, args: &[u64]) -> Result<()> {
    match args.len() {
        0 => {
            let func: extern "C" fn() = unsafe { std::mem::transmute(fn_ptr) };
            func();
        }
        1 => {
            let func: extern "C" fn(u64) = unsafe { std::mem::transmute(fn_ptr) };
            func(args[0]);
        }
        2 => {
            let func: extern "C" fn(u64, u64) = unsafe { std::mem::transmute(fn_ptr) };
            func(args[0], args[1]);
        }
        3 => {
            let func: extern "C" fn(u64, u64, u64) = unsafe { std::mem::transmute(fn_ptr) };
            func(args[0], args[1], args[2]);
        }
        4 => {
            let func: extern "C" fn(u64, u64, u64, u64) = unsafe { std::mem::transmute(fn_ptr) };
            func(args[0], args[1], args[2], args[3]);
        }
        5 => {
            let func: extern "C" fn(u64, u64, u64, u64, u64) =
                unsafe { std::mem::transmute(fn_ptr) };
            func(args[0], args[1], args[2], args[3], args[4]);
        }
        6 => {
            let func: extern "C" fn(u64, u64, u64, u64, u64, u64) =
                unsafe { std::mem::transmute(fn_ptr) };
            func(args[0], args[1], args[2], args[3], args[4], args[5]);
        }
        _ => return Err(Error::from("ffi supports up to 6 arguments")),
    }
    Ok(())
}

unsafe fn call_i64(fn_ptr: *const c_void, args: &[u64]) -> Result<i64> {
    let value = match args.len() {
        0 => {
            let func: extern "C" fn() -> i64 = unsafe { std::mem::transmute(fn_ptr) };
            func()
        }
        1 => {
            let func: extern "C" fn(u64) -> i64 = unsafe { std::mem::transmute(fn_ptr) };
            func(args[0])
        }
        2 => {
            let func: extern "C" fn(u64, u64) -> i64 = unsafe { std::mem::transmute(fn_ptr) };
            func(args[0], args[1])
        }
        3 => {
            let func: extern "C" fn(u64, u64, u64) -> i64 = unsafe { std::mem::transmute(fn_ptr) };
            func(args[0], args[1], args[2])
        }
        4 => {
            let func: extern "C" fn(u64, u64, u64, u64) -> i64 =
                unsafe { std::mem::transmute(fn_ptr) };
            func(args[0], args[1], args[2], args[3])
        }
        5 => {
            let func: extern "C" fn(u64, u64, u64, u64, u64) -> i64 =
                unsafe { std::mem::transmute(fn_ptr) };
            func(args[0], args[1], args[2], args[3], args[4])
        }
        6 => {
            let func: extern "C" fn(u64, u64, u64, u64, u64, u64) -> i64 =
                unsafe { std::mem::transmute(fn_ptr) };
            func(args[0], args[1], args[2], args[3], args[4], args[5])
        }
        _ => return Err(Error::from("ffi supports up to 6 arguments")),
    };
    Ok(value)
}

unsafe fn call_u64(fn_ptr: *const c_void, args: &[u64]) -> Result<u64> {
    let value = match args.len() {
        0 => {
            let func: extern "C" fn() -> u64 = unsafe { std::mem::transmute(fn_ptr) };
            func()
        }
        1 => {
            let func: extern "C" fn(u64) -> u64 = unsafe { std::mem::transmute(fn_ptr) };
            func(args[0])
        }
        2 => {
            let func: extern "C" fn(u64, u64) -> u64 = unsafe { std::mem::transmute(fn_ptr) };
            func(args[0], args[1])
        }
        3 => {
            let func: extern "C" fn(u64, u64, u64) -> u64 = unsafe { std::mem::transmute(fn_ptr) };
            func(args[0], args[1], args[2])
        }
        4 => {
            let func: extern "C" fn(u64, u64, u64, u64) -> u64 =
                unsafe { std::mem::transmute(fn_ptr) };
            func(args[0], args[1], args[2], args[3])
        }
        5 => {
            let func: extern "C" fn(u64, u64, u64, u64, u64) -> u64 =
                unsafe { std::mem::transmute(fn_ptr) };
            func(args[0], args[1], args[2], args[3], args[4])
        }
        6 => {
            let func: extern "C" fn(u64, u64, u64, u64, u64, u64) -> u64 =
                unsafe { std::mem::transmute(fn_ptr) };
            func(args[0], args[1], args[2], args[3], args[4], args[5])
        }
        _ => return Err(Error::from("ffi supports up to 6 arguments")),
    };
    Ok(value)
}

unsafe fn call_ptr(fn_ptr: *const c_void, args: &[u64]) -> Result<*mut c_void> {
    let value = match args.len() {
        0 => {
            let func: extern "C" fn() -> *mut c_void = unsafe { std::mem::transmute(fn_ptr) };
            func()
        }
        1 => {
            let func: extern "C" fn(u64) -> *mut c_void = unsafe { std::mem::transmute(fn_ptr) };
            func(args[0])
        }
        2 => {
            let func: extern "C" fn(u64, u64) -> *mut c_void =
                unsafe { std::mem::transmute(fn_ptr) };
            func(args[0], args[1])
        }
        3 => {
            let func: extern "C" fn(u64, u64, u64) -> *mut c_void =
                unsafe { std::mem::transmute(fn_ptr) };
            func(args[0], args[1], args[2])
        }
        4 => {
            let func: extern "C" fn(u64, u64, u64, u64) -> *mut c_void =
                unsafe { std::mem::transmute(fn_ptr) };
            func(args[0], args[1], args[2], args[3])
        }
        5 => {
            let func: extern "C" fn(u64, u64, u64, u64, u64) -> *mut c_void =
                unsafe { std::mem::transmute(fn_ptr) };
            func(args[0], args[1], args[2], args[3], args[4])
        }
        6 => {
            let func: extern "C" fn(u64, u64, u64, u64, u64, u64) -> *mut c_void =
                unsafe { std::mem::transmute(fn_ptr) };
            func(args[0], args[1], args[2], args[3], args[4], args[5])
        }
        _ => return Err(Error::from("ffi supports up to 6 arguments")),
    };
    Ok(value)
}

#[derive(Debug)]
pub struct DynamicLibrary {
    handle: *mut c_void,
}

impl DynamicLibrary {
    pub fn open_default() -> Result<Self> {
        let mut errors = Vec::new();
        for lib in default_libs() {
            match Self::open(Some(lib)) {
                Ok(lib) => return Ok(lib),
                Err(err) => errors.push(err.to_string()),
            }
        }
        Self::open(None).map_err(|err| {
            let mut message = String::from("failed to open default ffi library");
            for detail in errors {
                message.push_str(&format!("; {detail}"));
            }
            Error::from(format!("{message}; {err}"))
        })
    }

    fn open(path: Option<&str>) -> Result<Self> {
        let handle = unsafe { dlopen(path)? };
        if handle.is_null() {
            return Err(Error::from("dlopen returned null handle"));
        }
        Ok(Self { handle })
    }

    pub fn symbol(&self, name: &str) -> Result<*const c_void> {
        let symbol = unsafe { dlsym(self.handle, name)? };
        if symbol.is_null() {
            return Err(Error::from(format!("symbol '{name}' not found")));
        }
        Ok(symbol)
    }
}

impl Drop for DynamicLibrary {
    fn drop(&mut self) {
        unsafe {
            dlclose(self.handle);
        }
    }
}

#[cfg(any(target_os = "linux", target_os = "android", target_os = "freebsd"))]
fn default_libs() -> &'static [&'static str] {
    &["libc.so.6", "libc.so"]
}

#[cfg(target_os = "macos")]
fn default_libs() -> &'static [&'static str] {
    &["libSystem.B.dylib"]
}

#[cfg(target_os = "windows")]
fn default_libs() -> &'static [&'static str] {
    &["msvcrt.dll", "ucrtbase.dll"]
}

#[cfg(unix)]
unsafe fn dlopen(path: Option<&str>) -> Result<*mut c_void> {
    let c_path = match path {
        Some(path) => Some(CString::new(path).map_err(|err| Error::from(err.to_string()))?),
        None => None,
    };
    let handle = unsafe {
        dlopen_raw(
            c_path
                .as_ref()
                .map(|s| s.as_ptr())
                .unwrap_or(std::ptr::null()),
            RTLD_NOW,
        )
    };
    if handle.is_null() {
        let err = unsafe { dlerror_string() };
        return Err(Error::from(err));
    }
    Ok(handle)
}

#[cfg(unix)]
unsafe fn dlsym(handle: *mut c_void, name: &str) -> Result<*const c_void> {
    let c_name = CString::new(name).map_err(|err| Error::from(err.to_string()))?;
    let symbol = unsafe { dlsym_raw(handle, c_name.as_ptr()) };
    if symbol.is_null() {
        let err = unsafe { dlerror_string() };
        return Err(Error::from(err));
    }
    Ok(symbol)
}

#[cfg(unix)]
unsafe fn dlclose(handle: *mut c_void) {
    unsafe {
        dlclose_raw(handle);
    }
}

#[cfg(unix)]
unsafe fn dlerror_string() -> String {
    let err = unsafe { dlerror_raw() };
    if err.is_null() {
        "unknown dlerror".to_string()
    } else {
        unsafe { CStr::from_ptr(err).to_string_lossy().into_owned() }
    }
}

#[cfg(unix)]
const RTLD_NOW: i32 = 2;

#[cfg(all(unix, not(target_os = "macos")))]
#[link(name = "dl")]
unsafe extern "C" {
    #[link_name = "dlopen"]
    fn dlopen_raw(filename: *const c_char, flags: i32) -> *mut c_void;
    #[link_name = "dlsym"]
    fn dlsym_raw(handle: *mut c_void, symbol: *const c_char) -> *const c_void;
    #[link_name = "dlclose"]
    fn dlclose_raw(handle: *mut c_void) -> i32;
    #[link_name = "dlerror"]
    fn dlerror_raw() -> *const c_char;
}

#[cfg(target_os = "macos")]
#[link(name = "System")]
unsafe extern "C" {
    #[link_name = "dlopen"]
    fn dlopen_raw(filename: *const c_char, flags: i32) -> *mut c_void;
    #[link_name = "dlsym"]
    fn dlsym_raw(handle: *mut c_void, symbol: *const c_char) -> *const c_void;
    #[link_name = "dlclose"]
    fn dlclose_raw(handle: *mut c_void) -> i32;
    #[link_name = "dlerror"]
    fn dlerror_raw() -> *const c_char;
}

#[cfg(windows)]
use std::ffi::OsStr;
#[cfg(windows)]
use std::os::windows::ffi::OsStrExt;

#[cfg(windows)]
unsafe fn dlopen(path: Option<&str>) -> Result<*mut c_void> {
    let path = path.ok_or_else(|| Error::from("ffi requires explicit library path on Windows"))?;
    let wide: Vec<u16> = OsStr::new(path)
        .encode_wide()
        .chain(std::iter::once(0))
        .collect();
    let handle = unsafe { LoadLibraryW(wide.as_ptr()) };
    if handle.is_null() {
        return Err(Error::from("LoadLibraryW failed"));
    }
    Ok(handle as *mut c_void)
}

#[cfg(windows)]
unsafe fn dlsym(handle: *mut c_void, name: &str) -> Result<*const c_void> {
    let c_name = CString::new(name).map_err(|err| Error::from(err.to_string()))?;
    let symbol = unsafe { GetProcAddress(handle as HMODULE, c_name.as_ptr()) };
    if symbol.is_null() {
        return Err(Error::from("GetProcAddress failed"));
    }
    Ok(symbol as *const c_void)
}

#[cfg(windows)]
unsafe fn dlclose(handle: *mut c_void) {
    unsafe {
        FreeLibrary(handle as HMODULE);
    }
}

#[cfg(windows)]
type HMODULE = *mut c_void;

#[cfg(windows)]
#[link(name = "Kernel32")]
unsafe extern "system" {
    fn LoadLibraryW(lpFileName: *const u16) -> HMODULE;
    fn GetProcAddress(hModule: HMODULE, lpProcName: *const c_char) -> *const c_void;
    fn FreeLibrary(hModule: HMODULE) -> i32;
}
