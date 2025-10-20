use super::RustParser;
use crate::parser::parse_ident;
use fp_core::ast::{
    Expr, Pattern, PatternIdent, PatternKind, PatternTuple, PatternTupleStruct, PatternType,
    PatternVariant, PatternWildcard,
};
use fp_core::error::Result;
use itertools::Itertools;
use quote::ToTokens;

impl RustParser {
    pub(super) fn parse_pat_ident(&self, ident: syn::PatIdent) -> Result<PatternIdent> {
        let mut pattern_ident = PatternIdent::new(parse_ident(ident.ident));
        if ident.mutability.is_some() {
            pattern_ident.mutability = Some(true);
        }
        Ok(pattern_ident)
    }

    pub(super) fn parse_pat(&self, pat: syn::Pat) -> Result<Pattern> {
        Ok(match pat {
            syn::Pat::Ident(ident) => {
                Pattern::from(PatternKind::Ident(self.parse_pat_ident(ident)?))
            }
            syn::Pat::Wild(_) => Pattern::from(PatternKind::Wildcard(PatternWildcard {})),
            syn::Pat::TupleStruct(t) => {
                Pattern::from(PatternKind::TupleStruct(PatternTupleStruct {
                    name: self.parse_locator(t.path)?,
                    patterns: t
                        .elems
                        .into_iter()
                        .map(|p| self.parse_pat(p))
                        .try_collect()?,
                }))
            }
            syn::Pat::Tuple(t) => Pattern::from(PatternKind::Tuple(PatternTuple {
                patterns: t
                    .elems
                    .into_iter()
                    .map(|p| self.parse_pat(p))
                    .try_collect()?,
            })),
            syn::Pat::Type(p) => Pattern::from(PatternKind::Type(PatternType {
                pat: self.parse_pat(*p.pat)?.into(),
                ty: self.parse_type(*p.ty)?,
            })),
            syn::Pat::Path(path) => {
                let locator = self.parse_locator(path.path)?;
                Pattern::from(PatternKind::Variant(PatternVariant {
                    name: Expr::locator(locator),
                    pattern: None,
                }))
            }
            syn::Pat::Lit(lit) => {
                let expr = self.expr_parser().parse_expr(syn::Expr::Lit(lit))?;
                Pattern::from(PatternKind::Variant(PatternVariant {
                    name: expr,
                    pattern: None,
                }))
            }
            syn::Pat::Paren(paren) => return self.parse_pat(*paren.pat),
            other => {
                return self.error(
                    format!(
                        "Pattern not supported {}: {:?}",
                        other.to_token_stream(),
                        other
                    ),
                    Pattern::from(PatternKind::Wildcard(PatternWildcard {})),
                );
            }
        })
    }
}
