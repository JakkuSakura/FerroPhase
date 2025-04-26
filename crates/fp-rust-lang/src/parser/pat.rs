use crate::parser::{parse_ident, parse_locator, ty};
use eyre::bail;
use itertools::Itertools;
use fp_core::pat::{
    Pattern, PatternIdent, PatternTuple, PatternTupleStruct, PatternType, PatternWildcard,
};
use quote::ToTokens;

pub fn parse_pat_ident(i: syn::PatIdent) -> eyre::Result<PatternIdent> {
    Ok(PatternIdent {
        ident: parse_ident(i.ident),
        mutability: Some(i.mutability.is_some()),
    })
}
pub fn parse_pat(p: syn::Pat) -> eyre::Result<Pattern> {
    Ok(match p {
        syn::Pat::Ident(ident) => parse_pat_ident(ident)?.into(),
        syn::Pat::Wild(_) => Pattern::Wildcard(PatternWildcard {}),
        syn::Pat::TupleStruct(t) => Pattern::TupleStruct(PatternTupleStruct {
            name: parse_locator(t.path)?,
            patterns: t.elems.into_iter().map(parse_pat).try_collect()?,
        }),
        syn::Pat::Tuple(t) => Pattern::Tuple(PatternTuple {
            patterns: t.elems.into_iter().map(parse_pat).try_collect()?,
        }),
        syn::Pat::Type(p) => Pattern::Type(PatternType {
            pat: parse_pat(*p.pat)?.into(),
            ty: ty::parse_type(*p.ty)?,
        }),
        _ => bail!("Pattern not supported {}: {:?}", p.to_token_stream(), p),
    })
}
