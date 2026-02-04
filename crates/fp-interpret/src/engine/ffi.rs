use std::collections::HashMap;
use std::ffi::{c_void, CString};

use fp_core::ast::{
    Abi, DecimalType, ExprKind, FunctionSignature, Name, Ty, TypeInt, TypePrimitive,
    TypeReference, Value, ValueChar, ValuePointer,
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
        let (arg_types, arg_values, _cstrings) = self.build_args(sig, args)?;
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
    ) -> Result<(Vec<Type>, Vec<FfiArgValue>, Vec<CString>)> {
        let mut arg_types = Vec::with_capacity(sig.params.len());
        let mut arg_values = Vec::with_capacity(sig.params.len());
        let mut cstrings = Vec::new();

        for (param, value) in sig.params.iter().zip(args.iter()) {
            let ty = &param.ty;
            let ffi_ty = ffi_type_for_arg(ty)?;
            arg_types.push(ffi_ty);
            push_arg_value(ty, value, &mut arg_values, &mut cstrings)?;
        }

        Ok((arg_types, arg_values, cstrings))
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
            match value {
                Value::Pointer(ptr) => args.push(FfiArgValue::Ptr(ptr.value as *mut c_void)),
                Value::Null(_) => args.push(FfiArgValue::Ptr(std::ptr::null_mut())),
                _ => return Err(Error::from("expected pointer argument")),
            }
        }
        Ty::Unit(_) => return Err(Error::from("unit cannot be passed as extern arg")),
        _ => return Err(Error::from("unsupported extern argument type")),
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
