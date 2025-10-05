use eyre::Context;
use fp_core::error::Result;
use itertools::Itertools;
use quote::ToTokens;
use syn::parse::ParseStream;
use syn::{parse_quote, FieldsNamed, Token};

use fp_core::ast::{
    DecimalType, Expr, ExprBinOp, ExprKind, StructuralField, Ty, TypeAny, TypeArray, TypeBounds,
    TypeFunction, TypeInt, TypePrimitive, TypeReference, TypeSlice, TypeStruct, TypeStructural,
};
use fp_core::id::{Ident, Path};
use fp_core::ops::BinOpKind;

use crate::parser;
use crate::parser::item;
use crate::parser::item::parse_impl_trait_with;
use crate::parser::{parse_path, RustParser};

impl RustParser {
    pub(super) fn parse_type_internal(&self, ty_syn: syn::Type) -> Result<Ty> {
        let ty = match ty_syn {
            syn::Type::BareFn(f) => Ty::Function(
                TypeFunction {
                    params: f
                        .inputs
                        .into_iter()
                        .map(|arg| self.parse_type_internal(arg.ty))
                        .try_collect()?,
                    generics_params: vec![],
                    ret_ty: match f.output {
                        syn::ReturnType::Default => None,
                        syn::ReturnType::Type(_, ty) => Some(self.parse_type_internal(*ty)?.into()),
                    },
                }
                .into(),
            )
            .into(),
            syn::Type::Path(p) => {
                let s = p.path.to_token_stream().to_string();
                fn int(ty: TypeInt) -> Ty {
                    Ty::Primitive(TypePrimitive::Int(ty))
                }
                fn float(ty: DecimalType) -> Ty {
                    Ty::Primitive(TypePrimitive::Decimal(ty))
                }

                match s.as_str() {
                    "i64" => int(TypeInt::I64),
                    "i32" => int(TypeInt::I32),
                    "i16" => int(TypeInt::I16),
                    "i8" => int(TypeInt::I8),
                    "u64" => int(TypeInt::U64),
                    "u32" => int(TypeInt::U32),
                    "u16" => int(TypeInt::U16),
                    "u8" => int(TypeInt::U8),
                    "usize" => int(TypeInt::U64),
                    "f64" => float(DecimalType::F64),
                    "f32" => float(DecimalType::F32),
                    _ => Ty::locator(self.parse_locator(p.path)?),
                }
            }
            syn::Type::ImplTrait(im) => Ty::ImplTraits(parse_impl_trait_with(self, im)?),
            syn::Type::Tuple(t) if t.elems.is_empty() => Ty::unit().into(),
            syn::Type::Slice(s) => self.parse_type_slice_internal(s)?,
            syn::Type::Array(a) => self.parse_type_array_internal(a)?,
            syn::Type::Macro(m) if m.mac.path == parse_quote!(ty_syn) => {
                Ty::expr(parse_custom_type_expr(m)?)
            }
            syn::Type::Reference(r) => Ty::Reference(self.parse_type_reference_internal(r)?.into()),
            syn::Type::Infer(_) => Ty::Any(TypeAny),
            other => {
                return self.error(format!("Type not supported {:?}", other), Ty::Any(TypeAny));
            }
        };
        Ok(ty)
    }

    fn parse_type_slice_internal(&self, s: syn::TypeSlice) -> Result<Ty> {
        Ok(Ty::Slice(TypeSlice {
            elem: self.parse_type_internal(*s.elem)?.into(),
        }))
    }

    fn parse_type_array_internal(&self, a: syn::TypeArray) -> Result<Ty> {
        let syn::TypeArray { elem, len, .. } = a;
        Ok(Ty::Array(TypeArray {
            elem: self.parse_type_internal(*elem)?.into(),
            len: self.parse_expr(len)?.into(),
        }))
    }

    fn parse_type_reference_internal(&self, r: syn::TypeReference) -> Result<TypeReference> {
        Ok(TypeReference {
            ty: Box::new(self.parse_type_internal(*r.elem)?),
            mutability: r.mutability.map(|_| true),
            lifetime: r.lifetime.map(|lt| parser::parse_ident(lt.ident)),
        })
    }

    pub(super) fn parse_type_param_bound_internal(
        &self,
        bound: syn::TypeParamBound,
    ) -> Result<Expr> {
        match bound {
            syn::TypeParamBound::Trait(t) => {
                let path = parse_path(t.path)?;
                Ok(Expr::path(path))
            }
            other => self.error(
                format!("Does not support lifetimes {:?}", other),
                Expr::unit(),
            ),
        }
    }

    pub(super) fn parse_type_param_bounds_internal(
        &self,
        bounds: Vec<syn::TypeParamBound>,
    ) -> Result<TypeBounds> {
        Ok(TypeBounds {
            bounds: bounds
                .into_iter()
                .map(|bound| self.parse_type_param_bound_internal(bound))
                .try_collect()?,
        })
    }
}

pub fn parse_type(t: syn::Type) -> Result<Ty> {
    RustParser::new().parse_type_internal(t)
}

#[allow(dead_code)]
pub fn parse_type_param_bound(b: syn::TypeParamBound) -> Result<Expr> {
    RustParser::new().parse_type_param_bound_internal(b)
}

#[allow(dead_code)]
pub fn parse_type_param_bounds(bs: Vec<syn::TypeParamBound>) -> Result<TypeBounds> {
    RustParser::new().parse_type_param_bounds_internal(bs)
}

pub fn parse_member(mem: syn::Member) -> Result<Ident> {
    Ok(match mem {
        syn::Member::Named(n) => parser::parse_ident(n),
        syn::Member::Unnamed(n) => Ident::new(n.index.to_string()),
    })
}

pub fn parse_struct_field(i: usize, f: syn::Field) -> Result<StructuralField> {
    let parser = RustParser::new();
    Ok(StructuralField {
        name: f
            .ident
            .map(parser::parse_ident)
            .unwrap_or(Ident::new(format!("{}", i))),
        value: parser.parse_type(f.ty)?.into(),
    })
}

struct StructuralTypeParser(TypeStructural);
impl syn::parse::Parse for StructuralTypeParser {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        input.parse::<Token![struct]>()?;

        let fields: FieldsNamed = input.parse()?;

        Ok(StructuralTypeParser(TypeStructural {
            fields: fields
                .named
                .into_iter()
                .enumerate()
                .map(|(i, f)| parse_struct_field(i, f))
                .try_collect()
                .map_err(|err| input.error(err.to_string()))?,
        }))
    }
}

enum TypeValueParser {
    Structural(TypeStructural),
    Struct(TypeStruct),
    Path(Path),
}

impl Into<Ty> for TypeValueParser {
    fn into(self) -> Ty {
        match self {
            TypeValueParser::Structural(s) => Ty::Structural(s),
            TypeValueParser::Struct(s) => Ty::Struct(s),
            TypeValueParser::Path(p) => Ty::path(p),
        }
    }
}

impl syn::parse::Parse for TypeValueParser {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Token![struct]) {
            if input.peek2(syn::Ident) {
                let s: syn::ItemStruct = input.parse()?;
                Ok(TypeValueParser::Struct(
                    item::parse_type_struct(s).map_err(|err| input.error(err.to_string()))?,
                ))
            } else {
                Ok(TypeValueParser::Structural(
                    input.parse::<StructuralTypeParser>()?.0,
                ))
            }
        } else {
            let path = input.parse::<syn::Path>()?;
            Ok(TypeValueParser::Path(
                parse_path(path).map_err(|err| input.error(err.to_string()))?,
            ))
        }
    }
}

enum TypeExprParser {
    Add { left: Expr, right: Expr },
    Value(Ty),
}

impl syn::parse::Parse for TypeExprParser {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut lhs = TypeExprParser::Value(input.parse::<TypeValueParser>()?.into());
        loop {
            if input.is_empty() {
                break;
            }
            if input.peek(Token![+]) {
                input.parse::<Token![+]>()?;
                let rhs: Ty = input.parse::<TypeValueParser>()?.into();
                lhs = TypeExprParser::Add {
                    left: lhs.into(),
                    right: Expr::value(rhs.into()),
                };
            } else {
                return Err(input.error("Expected + or -"));
            }
        }
        Ok(lhs)
    }
}

impl From<TypeExprParser> for Expr {
    fn from(parser: TypeExprParser) -> Self {
        match parser {
            TypeExprParser::Add { left, right } => ExprKind::BinOp(ExprBinOp {
                lhs: left.into(),
                rhs: right.into(),
                kind: BinOpKind::Add,
            })
            .into(),
            TypeExprParser::Value(v) => Expr::value(v.into()),
        }
    }
}

fn parse_custom_type_expr(m: syn::TypeMacro) -> Result<Expr> {
    let t: TypeExprParser = m.mac.parse_body().with_context(|| format!("{:?}", m))?;
    Ok(Expr::from(t))
}
