use super::*;

/// Converts Rust primitive spellings that arrive as unresolved AST names into
/// the shared AST primitive forms. The framework applies this hook to the
/// owned type tree; this function only maps the current node and never walks
/// or mutates a caller-owned type.
pub(super) fn materialize_rust_alias(ty: Ty) -> Ty {
    let name = match &ty {
        Ty::Expr(expr) => match expr.kind() {
            ExprKind::Name(Name::Ident(name)) => Some(name.as_str()),
            ExprKind::Name(Name::Path(path)) => Some(path.last().as_str()),
            ExprKind::Name(Name::ParameterPath(path)) => {
                path.last().map(|segment| segment.ident.as_str())
            }
            _ => None,
        },
        _ => None,
    };
    let Some(name) = name else { return ty };
    if let Some(inner) = name
        .strip_prefix("to_vec_in<")
        .and_then(|value| value.strip_suffix('>'))
    {
        let element = if inner == "str" {
            Ty::Primitive(TypePrimitive::String)
        } else {
            Ty::ident(Ident::new(inner))
        };
        return parameterized("MutableList", element);
    }
    if name.starts_with("Split<") {
        return parameterized("List", Ty::Primitive(TypePrimitive::String));
    }
    match name {
        "bool" => Ty::Primitive(TypePrimitive::Bool),
        "i8" => Ty::Primitive(TypePrimitive::Int(TypeInt::I8)),
        "u8" => Ty::Primitive(TypePrimitive::Int(TypeInt::U8)),
        "i16" => Ty::Primitive(TypePrimitive::Int(TypeInt::I16)),
        "u16" => Ty::Primitive(TypePrimitive::Int(TypeInt::U16)),
        "i32" => Ty::Primitive(TypePrimitive::Int(TypeInt::I32)),
        "u32" => Ty::Primitive(TypePrimitive::Int(TypeInt::U32)),
        "i64" | "isize" => Ty::Primitive(TypePrimitive::Int(TypeInt::I64)),
        "u64" | "usize" => Ty::Primitive(TypePrimitive::Int(TypeInt::U64)),
        "i128" => Ty::Primitive(TypePrimitive::Int(TypeInt::I128)),
        "u128" => Ty::Primitive(TypePrimitive::Int(TypeInt::U128)),
        "f16" | "f32" => Ty::Primitive(TypePrimitive::Decimal(fp_core::ast::DecimalType::F32)),
        "f64" | "f128" => Ty::Primitive(TypePrimitive::Decimal(fp_core::ast::DecimalType::F64)),
        "str" => Ty::Primitive(TypePrimitive::String),
        _ => ty,
    }
}

pub(super) fn materialize_aliases(mut ty: Ty) -> Ty {
    match &mut ty {
        Ty::Reference(r) => r.ty = Box::new(materialize_aliases(*r.ty.clone())),
        Ty::RawPtr(p) => p.ty = Box::new(materialize_aliases(*p.ty.clone())),
        Ty::Vec(v) => v.ty = Box::new(materialize_aliases(*v.ty.clone())),
        Ty::Slice(s) => s.elem = Box::new(materialize_aliases(*s.elem.clone())),
        Ty::Array(a) => a.elem = Box::new(materialize_aliases(*a.elem.clone())),
        Ty::Tuple(t) => t.types = t.types.iter().cloned().map(materialize_aliases).collect(),
        Ty::Expr(expr) => {
            let Some(name) = (match expr.kind() {
                ExprKind::Name(name) => Some(name),
                _ => None,
            }) else {
                return ty;
            };
            if let Name::Ident(id) = name {
                if let Some(inner) = id
                    .as_str()
                    .strip_prefix("to_vec_in<")
                    .and_then(|value| value.strip_suffix('>'))
                {
                    let element = if inner == "str" {
                        Ty::ident(Ident::new("String"))
                    } else {
                        Ty::ident(Ident::new(inner))
                    };
                    return parameterized("MutableList", element);
                }
            }
            let (last, args) = match name {
                Name::Ident(id) => (id.as_str().to_owned(), Vec::new()),
                Name::Path(path) => (path.last().as_str().to_owned(), Vec::new()),
                Name::ParameterPath(path) => path
                    .last()
                    .map(|s| (s.ident.as_str().to_owned(), s.args.clone()))
                    .unwrap_or_default(),
            };
            let replacement = match last.as_str() {
                // `Option` and `Result` already use their Kotlin target names
                // after JVM-name materialization. Option needs an explicit
                // target AST marker because Kotlin expresses it with `?`.
                "Option" => Some(parameterized(
                    "Nullable",
                    args.into_iter().next().unwrap_or(Ty::ANY),
                )),
                "Vec" | "to_vec" | "to_vec_in" | "slice_to_vec" | "slice_to_vec_in" => {
                    let element = args.into_iter().next().unwrap_or(Ty::ANY);
                    Some(if is_u8_type(&element) {
                        Ty::ident(Ident::new("ByteArray"))
                    } else {
                        parameterized("MutableList", element)
                    })
                }
                _ => None,
            };
            if let Some(replacement) = replacement {
                return materialize_aliases(replacement);
            }
        }
        _ => {}
    }
    ty
}

pub(super) fn parameterized(name: &str, arg: Ty) -> Ty {
    Ty::Expr(Box::new(Expr::name(Name::parameter_path(
        fp_core::ast::ParameterPath::new(
            fp_core::ast::path::PathPrefix::Plain,
            vec![fp_core::ast::ParameterPathSegment::new(
                Ident::new(name),
                vec![arg],
            )],
        ),
    ))))
}

fn is_u8_type(ty: &Ty) -> bool {
    matches!(
        ty,
        Ty::Primitive(fp_core::ast::TypePrimitive::Int(fp_core::ast::TypeInt::U8))
    )
}

/// Rewrite JVM-backed source types as real AST paths.  This is deliberately
/// type-shaped: no Kotlin source text is manufactured here.
pub(super) fn materialize_jvm_type(mut ty: Ty) -> Ty {
    match &mut ty {
        Ty::Reference(reference) => {
            reference.ty = Box::new(materialize_jvm_type(*reference.ty.clone()))
        }
        Ty::Vec(vector) => vector.ty = Box::new(materialize_jvm_type(*vector.ty.clone())),
        Ty::Slice(slice) => slice.elem = Box::new(materialize_jvm_type(*slice.elem.clone())),
        Ty::Expr(expr) => {
            if let ExprKind::Name(name) = expr.kind_mut() {
                *name = materialize_jvm_name(name.clone());
            }
        }
        Ty::Array(array) => array.elem = Box::new(materialize_jvm_type(*array.elem.clone())),
        Ty::Tuple(tuple) => {
            tuple.types = tuple
                .types
                .iter()
                .cloned()
                .map(materialize_jvm_type)
                .collect()
        }
        Ty::Function(function) => {
            function.params = function
                .params
                .iter()
                .cloned()
                .map(materialize_jvm_type)
                .collect();
            if let Some(ret) = &function.ret_ty {
                function.ret_ty = Some(Box::new(materialize_jvm_type((**ret).clone())));
            }
        }
        _ => {}
    }
    ty
}

pub(super) fn materialize_jvm_name(mut name: Name) -> Name {
    let last = match &mut name {
        Name::Ident(ident) => ident.as_str().to_owned(),
        Name::Path(path) => path.last().as_str().to_owned(),
        Name::ParameterPath(path) => {
            let last = path
                .last()
                .map(|s| s.ident.as_str())
                .unwrap_or("")
                .to_owned();
            for segment in &mut path.segments {
                for arg in &mut segment.args {
                    *arg = materialize_jvm_type(arg.clone());
                }
                if segment.ident.as_str() == "Result" && segment.args.len() > 1 {
                    segment.args.truncate(1);
                }
            }
            last
        }
    };
    let target: &[&str] = match last.as_str() {
        "Command" | "Child" | "Output" | "DirEntry" | "FileType" | "ExitStatus" | "Stdio" => {
            &["RustKotlinRuntime", last.as_str()]
        }
        "Path" | "PathBuf" => &["java", "nio", "file", "Path"],
        "OsStr" | "OsString" => &["String"],
        "JsonNode" => &["com", "fasterxml", "jackson", "databind", "JsonNode"],
        "str" => &["String"],
        "Option" => return name,
        "Result" => return name,
        "Vec" | "to_vec" | "to_vec_in" | "slice_to_vec" | "slice_to_vec_in" => return name,
        "Error" if is_std_io_error(&name) => &["java", "io", "IOException"],
        "Error" => &["Throwable"],
        _ => return name,
    };
    name = Name::path(Path::plain(
        target.iter().map(|part| Ident::new(*part)).collect(),
    ));
    name
}

pub(super) fn is_std_io_error(name: &Name) -> bool {
    let segments: Vec<&str> = match name {
        Name::Path(path) => path.segments.iter().map(Ident::as_str).collect(),
        Name::ParameterPath(path) => path.segments.iter().map(|s| s.ident.as_str()).collect(),
        Name::Ident(_) => return false,
    };
    segments.ends_with(&["std", "io", "Error"])
}
