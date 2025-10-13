use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::Path;

use fp_core::ast::{
    DecimalType, EnumTypeVariant, Expr, Ident, StructuralField, Ty, TypeAny, TypeArray, TypeEnum,
    TypeInt, TypePrimitive, TypeStruct, TypeTuple, TypeUnit, TypeVec, Value,
};
use wit_parser::{
    Function, Interface, Package, Resolve, Type, TypeDef, TypeDefKind, TypeId, Variant,
};

use crate::error::{WitError, WitResult};
use crate::model::{WitDocument, WitFunction, WitInterface, WitPackage, WitParameter, WitType};

/// Parse a WIT document from a string.
pub fn parse_str(path: impl AsRef<Path>, contents: &str) -> WitResult<WitDocument> {
    let mut resolve = Resolve::default();
    let prepared = normalize_package_names(contents);
    resolve
        .push_str(path.as_ref(), prepared.as_ref())
        .map_err(WitError::Parse)?;
    build_document(&resolve)
}

/// Parse a WIT document from disk.
pub fn parse_file(path: impl AsRef<Path>) -> WitResult<WitDocument> {
    let path_ref = path.as_ref();
    let source =
        fs::read_to_string(path_ref).map_err(|err| WitError::Io(path_ref.to_path_buf(), err))?;
    parse_str(path_ref, &source)
}

fn build_document(resolve: &Resolve) -> WitResult<WitDocument> {
    let mut packages = Vec::new();
    let mut lowering = Lowering::new(resolve);

    for (_, package) in resolve.packages.iter() {
        packages.push(convert_package(resolve, package, &mut lowering)?);
    }

    Ok(WitDocument { packages })
}

fn convert_package(
    resolve: &Resolve,
    package: &Package,
    lowering: &mut Lowering<'_>,
) -> WitResult<WitPackage> {
    let mut interfaces = Vec::new();

    for (interface_name, interface_id) in &package.interfaces {
        let interface = &resolve.interfaces[*interface_id];
        interfaces.push(convert_interface(interface_name, interface, lowering)?);
    }

    Ok(WitPackage {
        name: package.name.to_string(),
        interfaces,
    })
}

fn convert_interface(
    declared_name: &str,
    interface: &Interface,
    lowering: &mut Lowering<'_>,
) -> WitResult<WitInterface> {
    let interface_name = interface
        .name
        .as_ref()
        .map(|name| make_ident(name))
        .unwrap_or_else(|| make_ident(declared_name));

    let mut types = Vec::new();
    for (type_name, type_id) in &interface.types {
        let ty = lowering.lower_type_id(*type_id)?;
        types.push(WitType {
            name: make_ident(type_name),
            ty,
        });
    }

    let mut functions = Vec::new();
    for (export_name, function) in &interface.functions {
        functions.push(convert_function(export_name, function, lowering)?);
    }

    Ok(WitInterface {
        name: interface_name,
        functions,
        types,
    })
}

fn convert_function(
    export_name: &str,
    function: &Function,
    lowering: &mut Lowering<'_>,
) -> WitResult<WitFunction> {
    let name = if function.name.is_empty() {
        make_ident(export_name)
    } else {
        make_ident(&function.name)
    };

    let mut params = Vec::with_capacity(function.params.len());
    for (param_name, ty) in &function.params {
        let ty = lowering.lower_type(ty)?;
        params.push(WitParameter {
            name: Some(make_ident(param_name)),
            ty,
        });
    }

    let mut results = Vec::new();
    if let Some(result_ty) = &function.result {
        let ty = lowering.lower_type(result_ty)?;
        results.push(WitParameter { name: None, ty });
    }

    Ok(WitFunction {
        name,
        params,
        results,
    })
}

struct Lowering<'a> {
    resolve: &'a Resolve,
    cache: HashMap<TypeId, Ty>,
    visiting: HashSet<TypeId>,
}

impl<'a> Lowering<'a> {
    fn new(resolve: &'a Resolve) -> Self {
        Self {
            resolve,
            cache: HashMap::new(),
            visiting: HashSet::new(),
        }
    }

    fn lower_type(&mut self, ty: &Type) -> WitResult<Ty> {
        match ty {
            Type::Bool => Ok(Ty::Primitive(TypePrimitive::Bool)),
            Type::U8 => Ok(Ty::Primitive(TypePrimitive::Int(TypeInt::U8))),
            Type::U16 => Ok(Ty::Primitive(TypePrimitive::Int(TypeInt::U16))),
            Type::U32 => Ok(Ty::Primitive(TypePrimitive::Int(TypeInt::U32))),
            Type::U64 => Ok(Ty::Primitive(TypePrimitive::Int(TypeInt::U64))),
            Type::S8 => Ok(Ty::Primitive(TypePrimitive::Int(TypeInt::I8))),
            Type::S16 => Ok(Ty::Primitive(TypePrimitive::Int(TypeInt::I16))),
            Type::S32 => Ok(Ty::Primitive(TypePrimitive::Int(TypeInt::I32))),
            Type::S64 => Ok(Ty::Primitive(TypePrimitive::Int(TypeInt::I64))),
            Type::F32 => Ok(Ty::Primitive(TypePrimitive::Decimal(DecimalType::F32))),
            Type::F64 => Ok(Ty::Primitive(TypePrimitive::Decimal(DecimalType::F64))),
            Type::Char => Ok(Ty::Primitive(TypePrimitive::Char)),
            Type::String => Ok(Ty::Primitive(TypePrimitive::String)),
            Type::ErrorContext => Ok(Ty::Vec(TypeVec {
                ty: Box::new(Ty::Primitive(TypePrimitive::String)),
            })),
            Type::Id(id) => self.lower_type_id(*id),
        }
    }

    fn lower_type_id(&mut self, type_id: TypeId) -> WitResult<Ty> {
        if let Some(cached) = self.cache.get(&type_id) {
            return Ok(cached.clone());
        }
        if !self.visiting.insert(type_id) {
            return Ok(Ty::Any(TypeAny));
        }

        let lowered = self.lower_type_def(type_id)?;
        self.visiting.remove(&type_id);
        self.cache.insert(type_id, lowered.clone());
        Ok(lowered)
    }

    fn lower_type_def(&mut self, type_id: TypeId) -> WitResult<Ty> {
        let typedef = &self.resolve.types[type_id];
        match &typedef.kind {
            TypeDefKind::Record(record) => Ok(Ty::Struct(TypeStruct {
                name: type_name_or_fallback(typedef, type_id),
                fields: self.lower_record_fields(record)?,
            })),
            TypeDefKind::Tuple(tuple) => Ok(Ty::Tuple(TypeTuple {
                types: tuple
                    .types
                    .iter()
                    .map(|ty| self.lower_type(ty))
                    .collect::<WitResult<_>>()?,
            })),
            TypeDefKind::Flags(flags) => Ok(Ty::Struct(TypeStruct {
                name: type_name_or_fallback(typedef, type_id),
                fields: flags
                    .flags
                    .iter()
                    .map(|flag| {
                        Ok(StructuralField {
                            name: make_ident(&flag.name),
                            value: Ty::Primitive(TypePrimitive::Bool),
                        })
                    })
                    .collect::<WitResult<_>>()?,
            })),
            TypeDefKind::Variant(variant) => self.lower_variant(typedef, type_id, variant),
            TypeDefKind::Enum(enum_) => Ok(Ty::Enum(TypeEnum {
                name: type_name_or_fallback(typedef, type_id),
                variants: enum_
                    .cases
                    .iter()
                    .map(|case| EnumTypeVariant {
                        name: make_ident(&case.name),
                        value: Ty::Unit(TypeUnit),
                        discriminant: None,
                    })
                    .collect(),
            })),
            TypeDefKind::Option(inner) => Ok(Ty::Enum(TypeEnum {
                name: type_name_or_fallback(typedef, type_id),
                variants: vec![
                    EnumTypeVariant {
                        name: make_ident("none"),
                        value: Ty::Unit(TypeUnit),
                        discriminant: None,
                    },
                    EnumTypeVariant {
                        name: make_ident("some"),
                        value: self.lower_type(inner)?,
                        discriminant: None,
                    },
                ],
            })),
            TypeDefKind::Result(result) => {
                let ok_ty = result
                    .ok
                    .as_ref()
                    .map(|ty| self.lower_type(ty))
                    .transpose()?;
                let err_ty = result
                    .err
                    .as_ref()
                    .map(|ty| self.lower_type(ty))
                    .transpose()?;

                Ok(Ty::Enum(TypeEnum {
                    name: type_name_or_fallback(typedef, type_id),
                    variants: vec![
                        EnumTypeVariant {
                            name: make_ident("ok"),
                            value: ok_ty.unwrap_or(Ty::Unit(TypeUnit)),
                            discriminant: None,
                        },
                        EnumTypeVariant {
                            name: make_ident("err"),
                            value: err_ty.unwrap_or(Ty::Unit(TypeUnit)),
                            discriminant: None,
                        },
                    ],
                }))
            }
            TypeDefKind::List(inner) => Ok(Ty::Vec(TypeVec {
                ty: Box::new(self.lower_type(inner)?),
            })),
            TypeDefKind::FixedSizeList(inner, len) => Ok(Ty::Array(TypeArray {
                elem: Box::new(self.lower_type(inner)?),
                len: Box::new(Expr::value(Value::int(*len as i64))),
            })),
            TypeDefKind::Type(inner) => self.lower_type(inner),
            TypeDefKind::Future(_) | TypeDefKind::Stream(_) => Ok(Ty::Any(TypeAny)),
            TypeDefKind::Handle(_) | TypeDefKind::Resource => Ok(Ty::Any(TypeAny)),
            TypeDefKind::Unknown => Err(WitError::UnsupportedFeature(
                "unknown imported type".to_string(),
            )),
        }
    }

    fn lower_record_fields(
        &mut self,
        record: &wit_parser::Record,
    ) -> WitResult<Vec<StructuralField>> {
        record
            .fields
            .iter()
            .map(|field| {
                Ok(StructuralField {
                    name: make_ident(&field.name),
                    value: self.lower_type(&field.ty)?,
                })
            })
            .collect()
    }

    fn lower_variant(
        &mut self,
        typedef: &TypeDef,
        type_id: TypeId,
        variant: &Variant,
    ) -> WitResult<Ty> {
        let variants = variant
            .cases
            .iter()
            .map(|case| {
                let value = match &case.ty {
                    Some(ty) => self.lower_type(ty)?,
                    None => Ty::Unit(TypeUnit),
                };
                Ok(EnumTypeVariant {
                    name: make_ident(&case.name),
                    value,
                    discriminant: None,
                })
            })
            .collect::<WitResult<_>>()?;

        Ok(Ty::Enum(TypeEnum {
            name: type_name_or_fallback(typedef, type_id),
            variants,
        }))
    }
}

fn type_name_or_fallback(typedef: &TypeDef, type_id: TypeId) -> Ident {
    typedef
        .name
        .as_ref()
        .map(|name| make_ident(name))
        .unwrap_or_else(|| make_ident(&format!("__wit_type_{}", type_id.index())))
}

fn make_ident(raw: &str) -> Ident {
    Ident::new(sanitize_identifier(raw))
}

pub(crate) fn sanitize_identifier(raw: &str) -> String {
    let mut result = String::with_capacity(raw.len());
    for ch in raw.chars() {
        if matches!(ch, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_') {
            result.push(ch);
        } else {
            result.push('_');
        }
    }
    if result.is_empty() {
        result.push('_');
    }
    if result.chars().next().unwrap().is_ascii_digit() {
        result.insert(0, '_');
    }
    result
}

fn sanitize_package_name(raw: &str) -> String {
    let mut result = String::with_capacity(raw.len());
    let mut last_dash = false;

    for ch in raw.chars() {
        let mapped = match ch {
            'a'..='z' | 'A'..='Z' | '0'..='9' | '-' => ch,
            _ => '-',
        };

        if mapped == '-' {
            if last_dash {
                continue;
            }
            last_dash = true;
        } else {
            last_dash = false;
        }

        result.push(mapped);
    }

    let trimmed = result.trim_matches('-');
    let mut cleaned = if trimmed.is_empty() {
        "pkg".to_string()
    } else {
        trimmed.to_string()
    };

    if !cleaned
        .chars()
        .next()
        .map(|c| c.is_ascii_alphabetic())
        .unwrap_or(false)
    {
        cleaned.insert_str(0, "pkg-");
    }

    cleaned
}

fn normalize_package_names(source: &str) -> Cow<'_, str> {
    let mut changed = false;
    let mut output = String::with_capacity(source.len());

    for segment in source.split_inclusive('\n') {
        let (line, newline) = segment
            .strip_suffix('\n')
            .map(|line| (line, "\n"))
            .unwrap_or((segment, ""));

        let (processed, line_changed) = normalize_package_line(line);
        if line_changed {
            changed = true;
        }
        output.push_str(&processed);
        output.push_str(newline);
    }

    if changed {
        Cow::Owned(output)
    } else {
        Cow::Borrowed(source)
    }
}

fn normalize_package_line(line: &str) -> (String, bool) {
    let trimmed = line.trim_start();
    if !trimmed.starts_with("package ") {
        return (line.to_string(), false);
    }

    let indent_len = line.len() - trimmed.len();
    let indent = &line[..indent_len];
    let rest = &trimmed["package ".len()..];

    let colon_idx = match rest.find(':') {
        Some(idx) => idx,
        None => return (line.to_string(), false),
    };

    let (namespace_part, tail) = rest.split_at(colon_idx + 1);
    let end_idx = tail
        .find(|c: char| matches!(c, ';' | '@' | ' ' | '\t'))
        .unwrap_or_else(|| tail.len());
    let (pkg_name, remainder) = tail.split_at(end_idx);

    let sanitized = sanitize_package_name(pkg_name);
    if sanitized == pkg_name {
        return (line.to_string(), false);
    }

    let mut rebuilt = String::with_capacity(line.len());
    rebuilt.push_str(indent);
    rebuilt.push_str("package ");
    rebuilt.push_str(namespace_part);
    rebuilt.push_str(&sanitized);
    rebuilt.push_str(remainder);
    (rebuilt, true)
}

#[cfg(test)]
mod tests {
    use super::*;
    use fp_core::ast::TypeEnum;

    const SIMPLE_WIT: &str = r#"
package test:math;

interface geometry {
    record point { x: f32, y: f32 }
    distance: func(a: point, b: point) -> f64;
    colours: func(names: list<string>) -> result<u32, string>;
}
"#;

    #[test]
    fn parses_basic_interface() {
        let doc = parse_str("simple.wit", SIMPLE_WIT).expect("parse simple WIT");
        assert_eq!(doc.packages.len(), 1);
        let package = &doc.packages[0];
        assert_eq!(package.name, "test:math");
        assert_eq!(package.interfaces.len(), 1);
        let interface = &package.interfaces[0];
        assert_eq!(interface.name, Ident::new("geometry"));
        assert_eq!(interface.types.len(), 1);
        let point_ty = &interface.types[0];
        assert_eq!(point_ty.name, Ident::new("point"));

        match &point_ty.ty {
            Ty::Struct(TypeStruct { fields, .. }) => {
                assert_eq!(fields.len(), 2);
                assert_eq!(fields[0].name, Ident::new("x"));
            }
            other => panic!("expected struct type, found {other:?}"),
        }

        assert_eq!(interface.functions.len(), 2);
        let distance = &interface.functions[0];
        assert_eq!(distance.params.len(), 2);
        assert_eq!(distance.results.len(), 1);

        let colours = &interface.functions[1];
        assert_eq!(colours.results.len(), 1);
        match &colours.results[0].ty {
            Ty::Enum(TypeEnum { variants, .. }) => {
                assert_eq!(variants.len(), 2);
                assert_eq!(variants[0].name, Ident::new("ok"));
                assert_eq!(variants[1].name, Ident::new("err"));
            }
            other => panic!("expected enum type, found {other:?}"),
        }
    }

    #[test]
    fn normalizes_package_names_with_invalid_characters() {
        let src = r#"
package ferrophase:examples/wit;

interface demo {}
"#;
        let doc = parse_str("invalid_package.wit", src).expect("parse WIT with slash in package");
        assert_eq!(doc.packages.len(), 1);
        assert_eq!(doc.packages[0].name, "ferrophase:examples-wit");
    }
}
