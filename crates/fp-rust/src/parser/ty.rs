use eyre::Context;
use fp_core::bail;
use fp_core::error::Result;
use itertools::Itertools;
use quote::ToTokens;
use syn::parse::ParseStream;
use syn::{parse_quote, FieldsNamed, Token};

use fp_core::ast::{
    DecimalType, Expr, ExprBinOp, StructuralField, Ty, TypeBounds, TypeFunction, TypeInt,
    TypePrimitive, TypeReference, TypeSlice, TypeStruct, TypeStructural,
};
use fp_core::id::{Ident, Path};
use fp_core::ops::BinOpKind;

use crate::parser;
use crate::parser::item::parse_impl_trait;
use crate::parser::{item, parse_path};

pub fn parse_type(t: syn::Type) -> Result<Ty> {
    let t = match t {
        syn::Type::BareFn(f) => Ty::Function(
            TypeFunction {
                params: f
                    .inputs
                    .into_iter()
                    .map(|x| x.ty)
                    .map(parse_type)
                    .try_collect()?,
                generics_params: vec![],
                ret_ty: match f.output {
                    syn::ReturnType::Default => None,
                    syn::ReturnType::Type(_, t) => Some(parse_type(*t)?.into()),
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
                "f64" => float(DecimalType::F64),
                "f32" => float(DecimalType::F32),
                _ => Ty::locator(parser::parse_locator(p.path)?),
            }
        }
        syn::Type::ImplTrait(im) => Ty::ImplTraits(parse_impl_trait(im)?),
        syn::Type::Tuple(t) if t.elems.is_empty() => Ty::unit().into(),
        syn::Type::Slice(s) => parse_type_slice(s)?,
        // types like t!{ }
        syn::Type::Macro(m) if m.mac.path == parse_quote!(t) => {
            Ty::expr(parse_custom_type_expr(m)?)
        }
        syn::Type::Reference(r) => Ty::Reference(parse_type_reference(r)?.into()),
        t => bail!("Type not supported {:?}", t),
    };
    Ok(t)
}
fn parse_type_slice(s: syn::TypeSlice) -> Result<Ty> {
    Ok(Ty::Slice(TypeSlice {
        elem: parse_type(*s.elem)?.into(),
    }))
}
fn parse_type_reference(r: syn::TypeReference) -> Result<TypeReference> {
    Ok(TypeReference {
        ty: Box::new(parse_type(*r.elem)?),
        mutability: r.mutability.map(|_| true),
        lifetime: r.lifetime.map(|x| parser::parse_ident(x.ident)),
    })
}

pub fn parse_type_param_bound(b: syn::TypeParamBound) -> Result<Expr> {
    match b {
        syn::TypeParamBound::Trait(t) => {
            let path = parse_path(t.path)?;
            Ok(Expr::path(path))
        }
        _ => bail!("Does not support lifetimes {:?}", b),
    }
}

pub fn parse_type_param_bounds(bs: Vec<syn::TypeParamBound>) -> Result<TypeBounds> {
    Ok(TypeBounds {
        bounds: bs.into_iter().map(parse_type_param_bound).try_collect()?,
    })
}

pub fn parse_member(mem: syn::Member) -> Result<Ident> {
    Ok(match mem {
        syn::Member::Named(n) => parser::parse_ident(n),
        syn::Member::Unnamed(_) => bail!("Does not support unnamed field yet {:?}", mem),
    })
}

pub fn parse_struct_field(i: usize, f: syn::Field) -> Result<StructuralField> {
    Ok(StructuralField {
        name: f
            .ident
            .map(parser::parse_ident)
            .unwrap_or(Ident::new(format!("{}", i))),

        value: parse_type(f.ty)?.into(),
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
    // Ident(Ident),
}
impl Into<Ty> for TypeValueParser {
    fn into(self) -> Ty {
        match self {
            // TypeExprParser::Add(o) => TypeExpr::Op(TypeOp::Add(AddOp {
            //     lhs: o.lhs.into(),
            //     rhs: o.rhs.into(),
            // })),
            // TypeExprParser::Sub(o) => TypeExpr::Op(TypeOp::Sub(SubOp {
            //     lhs: o.lhs.into(),
            //     rhs: o.rhs.into(),
            // })),
            TypeValueParser::Structural(s) => Ty::Structural(s),
            TypeValueParser::Struct(s) => Ty::Struct(s),
            TypeValueParser::Path(p) => Ty::path(p),
            // TypeValueParser::Ident(i) => TypeValue::ident(i),
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
            // } else if input.peek(Token![-]) {
            //     input.parse::<Token![-]>()?;
            //     let rhs: TypeValue = input.parse::<TypeValueParser>()?.into();
            //     lhs = TypeExprParser::Sub {
            //         left: lhs.into(),
            //         right: Expr::value(rhs.into()),
            //     };
            } else {
                return Err(input.error("Expected + or -"));
            }
        }
        Ok(lhs)
    }
}
impl Into<Expr> for TypeExprParser {
    fn into(self) -> Expr {
        match self {
            TypeExprParser::Add { left, right } => Expr::BinOp(ExprBinOp {
                lhs: left.into(),
                rhs: right.into(),
                kind: BinOpKind::Add,
            }),
            // TypeExprParser::Sub { .. } => {
            //     unreachable!()
            // }
            TypeExprParser::Value(v) => Expr::value(v.into()),
        }
    }
}
fn parse_custom_type_expr(m: syn::TypeMacro) -> Result<Expr> {
    let t: TypeExprParser = m.mac.parse_body().with_context(|| format!("{:?}", m))?;
    Ok(t.into())
}
