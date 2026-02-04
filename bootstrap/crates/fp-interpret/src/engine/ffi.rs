use std::ffi::{c_void, CString};

use fp_core::ast::{
    Abi, DecimalType, ExprKind, FunctionSignature, Name, Ty, TypeInt, TypePrimitive,
    TypeReference, Value, ValueChar, ValuePointer,
};
use fp_core::error::{Error, Result};
use fp_native::ffi::{FfiRuntime as NativeFfiRuntime, FfiSignature, FfiType, FfiValue};

#[derive(Debug)]
pub struct FfiRuntime {
    inner: NativeFfiRuntime,
}

impl FfiRuntime {
    pub fn new() -> Result<Self> {
        Ok(Self {
            inner: NativeFfiRuntime::new()?,
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

        let mut arg_types = Vec::with_capacity(sig.params.len());
        let mut arg_values = Vec::with_capacity(sig.params.len());
        let mut cstrings = Vec::new();

        for (param, value) in sig.params.iter().zip(args.iter()) {
            let ty = &param.ty;
            let ffi_ty = ffi_type_for_arg(ty)?;
            arg_types.push(ffi_ty);
            arg_values.push(ffi_value_for_arg(ty, value, &mut cstrings)?);
        }

        let ret_ty = ffi_type_for_return(sig.ret_ty.as_ref())?;
        let signature = FfiSignature {
            args: arg_types,
            ret: ret_ty,
        };

        let result = self.inner.call(name, &signature, &arg_values)?;
        drop(cstrings);
        Ok(convert_return(sig.ret_ty.as_ref(), result))
    }
}

fn convert_return(ret_ty: Option<&Ty>, value: Option<FfiValue>) -> Value {
    match (ret_ty, value) {
        (None, _) => Value::unit(),
        (Some(Ty::Unit(_)), _) => Value::unit(),
        (Some(Ty::Primitive(TypePrimitive::Bool)), Some(FfiValue::U64(v))) => {
            Value::bool(v != 0)
        }
        (Some(Ty::Primitive(TypePrimitive::Char)), Some(FfiValue::U64(v))) => {
            Value::Char(ValueChar::new((v as u8) as char))
        }
        (Some(Ty::Primitive(TypePrimitive::Int(_))), Some(FfiValue::I64(v))) => Value::int(v),
        (Some(Ty::Primitive(TypePrimitive::Int(_))), Some(FfiValue::U64(v))) => {
            Value::int(v as i64)
        }
        (Some(Ty::Primitive(TypePrimitive::Decimal(_))), Some(FfiValue::I64(v))) => {
            Value::decimal(v as f64)
        }
        (Some(Ty::Primitive(TypePrimitive::Decimal(_))), Some(FfiValue::U64(v))) => {
            Value::decimal(v as f64)
        }
        (Some(Ty::Reference(_)), Some(FfiValue::Ptr(ptr))) => {
            Value::Pointer(ValuePointer::new(ptr as i64))
        }
        (Some(Ty::Primitive(TypePrimitive::String)), _) => {
            Value::undefined()
        }
        _ => Value::undefined(),
    }
}

fn ffi_type_for_arg(ty: &Ty) -> Result<FfiType> {
    if is_cstr_reference(ty) {
        return Ok(FfiType::Ptr);
    }
    let resolved = resolve_ffi_ty(ty).unwrap_or_else(|| ty.clone());
    match &resolved {
        Ty::Primitive(TypePrimitive::Bool)
        | Ty::Primitive(TypePrimitive::Char)
        | Ty::Primitive(TypePrimitive::Int(_)) => Ok(FfiType::I64),
        Ty::Primitive(TypePrimitive::Decimal(_)) => Err(Error::from(
            "unsupported extern decimal arg without libffi",
        )),
        Ty::Primitive(TypePrimitive::String) => Err(Error::from(
            "unsupported extern string arg type; use &CStr",
        )),
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
        Some(other) => match ffi_type_for_arg(other) {
            Ok(FfiType::Ptr) => Ok(FfiType::Ptr),
            Ok(FfiType::I64) => Ok(FfiType::I64),
            Ok(FfiType::U64) => Ok(FfiType::U64),
            Ok(FfiType::Void) => Ok(FfiType::Void),
            Err(err) => Err(err),
        },
    }
}

fn ffi_value_for_arg(ty: &Ty, value: &Value, cstrings: &mut Vec<CString>) -> Result<FfiValue> {
    if is_cstr_reference(ty) {
        let Value::String(text) = value else {
            return Err(Error::from("expected string for &CStr argument"));
        };
        let cstr =
            CString::new(text.value.clone()).map_err(|err| Error::from(err.to_string()))?;
        let ptr = cstr.as_ptr() as *mut c_void;
        cstrings.push(cstr);
        return Ok(FfiValue::Ptr(ptr));
    }
    let resolved = resolve_ffi_ty(ty).unwrap_or_else(|| ty.clone());
    match &resolved {
        Ty::Primitive(TypePrimitive::Bool) => Ok(FfiValue::U64(match value {
            Value::Bool(b) => b.value as u64,
            other => {
                return Err(Error::from(format!(
                    "expected bool argument, found {other}"
                )))
            }
        })),
        Ty::Primitive(TypePrimitive::Char) => Ok(FfiValue::U64(match value {
            Value::Char(ch) => ch.value as u64,
            other => {
                return Err(Error::from(format!(
                    "expected char argument, found {other}"
                )))
            }
        })),
        Ty::Primitive(TypePrimitive::Int(_)) => Ok(FfiValue::I64(match value {
            Value::Int(i) => i.value,
            other => {
                return Err(Error::from(format!(
                    "expected int argument, found {other}"
                )))
            }
        })),
        Ty::Reference(TypeReference { ty, .. }) => {
            if resolves_to_string(ty.as_ref()) {
                Err(Error::from("unsupported extern &str arg type; use &CStr"))
            } else {
                match value {
                    Value::Pointer(ptr) => Ok(FfiValue::Ptr(ptr.value as *mut c_void)),
                    Value::Bytes(bytes) => Ok(FfiValue::Ptr(bytes.value.as_ptr() as *mut c_void)),
                    other => Err(Error::from(format!(
                        "expected pointer argument, found {other}"
                    ))),
                }
            }
        }
        _ => Err(Error::from("unsupported extern argument value")),
    }
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
