use crate::parser::{parse_ident, parse_locator, ty};
use fp_core::bail;
use fp_core::error::Result;
use fp_core::pat::{
    Pattern, PatternIdent, PatternKind, PatternTuple, PatternTupleStruct, PatternType,
    PatternWildcard,
};
use itertools::Itertools;
use quote::ToTokens;

pub fn parse_pat_ident(i: syn::PatIdent) -> Result<PatternIdent> {
    let mut ident = PatternIdent::new(parse_ident(i.ident));
    if i.mutability.is_some() {
        ident.mutability = Some(true);
    }
    Ok(ident)
}
pub fn parse_pat(p: syn::Pat) -> Result<Pattern> {
    Ok(match p {
        syn::Pat::Ident(ident) => Pattern::from(PatternKind::Ident(parse_pat_ident(ident)?)),
        syn::Pat::Wild(_) => Pattern::from(PatternKind::Wildcard(PatternWildcard {})),
        syn::Pat::TupleStruct(t) => Pattern::from(PatternKind::TupleStruct(PatternTupleStruct {
            name: parse_locator(t.path)?,
            patterns: t.elems.into_iter().map(parse_pat).try_collect()?,
        })),
        syn::Pat::Tuple(t) => Pattern::from(PatternKind::Tuple(PatternTuple {
            patterns: t.elems.into_iter().map(parse_pat).try_collect()?,
        })),
        syn::Pat::Type(p) => Pattern::from(PatternKind::Type(PatternType {
            pat: parse_pat(*p.pat)?.into(),
            ty: ty::parse_type(*p.ty)?,
        })),
        _ => bail!("Pattern not supported {}: {:?}", p.to_token_stream(), p),
    })
}
