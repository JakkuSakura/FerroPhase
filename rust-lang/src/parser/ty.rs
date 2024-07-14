use eyre::bail;
use itertools::Itertools;
use lang_core::ast::{
    AstExpr, AstType, DecimalType, StructuralField, TypeBounds, TypeFunction, TypeInt,
    TypePrimitive, TypeReference,
};
use lang_core::id::Ident;
use quote::ToTokens;
use syn::parse_quote;

use crate::parser;
use crate::parser::item;

pub fn parse_type(t: syn::Type) -> eyre::Result<AstType> {
    let t = match t {
        syn::Type::BareFn(f) => AstType::Function(
            TypeFunction {
                params: f
                    .inputs
                    .into_iter()
                    .map(|x| x.ty)
                    .map(parse_type)
                    .try_collect()?,
                generics_params: vec![],
                ret_ty: item::parse_return_type(f.output)?.into(),
            }
            .into(),
        )
        .into(),
        syn::Type::Path(p) => {
            let s = p.path.to_token_stream().to_string();
            fn int(ty: TypeInt) -> AstType {
                AstType::Primitive(TypePrimitive::Int(ty))
            }
            fn float(ty: DecimalType) -> AstType {
                AstType::Primitive(TypePrimitive::Decimal(ty))
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
                _ => AstType::locator(parser::parse_locator(p.path)?),
            }
        }
        syn::Type::ImplTrait(im) => AstType::ImplTraits(parser::parse_impl_trait(im)?),
        syn::Type::Tuple(t) if t.elems.is_empty() => AstType::unit().into(),
        // types like t!{ }
        syn::Type::Macro(m) if m.mac.path == parse_quote!(t) => {
            AstType::expr(parser::parse_custom_type_expr(m)?)
        }
        syn::Type::Reference(r) => AstType::Reference(parse_type_reference(r)?.into()),
        t => bail!("Type not supported {:?}", t),
    };
    Ok(t)
}

fn parse_type_reference(r: syn::TypeReference) -> eyre::Result<TypeReference> {
    Ok(TypeReference {
        ty: Box::new(parse_type(*r.elem)?),
        mutability: r.mutability.map(|_| true),
        lifetime: r.lifetime.map(|x| parser::parse_ident(x.ident)),
    })
}

pub fn parse_type_param_bound(b: syn::TypeParamBound) -> eyre::Result<AstExpr> {
    match b {
        syn::TypeParamBound::Trait(t) => {
            let path = parser::parse_path(t.path)?;
            Ok(AstExpr::path(path))
        }
        _ => bail!("Does not support lifetimes {:?}", b),
    }
}

pub fn parse_type_param_bounds(bs: Vec<syn::TypeParamBound>) -> eyre::Result<TypeBounds> {
    Ok(TypeBounds {
        bounds: bs.into_iter().map(parse_type_param_bound).try_collect()?,
    })
}

pub fn parse_member(mem: syn::Member) -> eyre::Result<Ident> {
    Ok(match mem {
        syn::Member::Named(n) => parser::parse_ident(n),
        syn::Member::Unnamed(_) => bail!("Does not support unnamed field yet {:?}", mem),
    })
}

pub fn parse_struct_field(i: usize, f: syn::Field) -> eyre::Result<StructuralField> {
    Ok(StructuralField {
        name: f
            .ident
            .map(parser::parse_ident)
            .unwrap_or(Ident::new(format!("{}", i))),

        value: parse_type(f.ty)?.into(),
    })
}
