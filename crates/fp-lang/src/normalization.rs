use fp_core::ast::{Expr, ExprKind, Value};
use fp_core::error::Result;
use fp_core::intrinsics::{IntrinsicNormalizer, NormalizeOutcome};
use fp_rust::normalization::RustIntrinsicNormalizer;

use crate::ast::expr::lower_type_from_cst;
use crate::cst::parse_type_lexemes_prefix_to_cst;
use crate::lexer::lexeme::LexemeKind;
use crate::lexer::tokenizer::lex_lexemes;

/// FerroPhase intrinsic normalizer that adds `t!` macro lowering for type expressions,
/// delegating all other macros to the Rust normalizer.
#[derive(Debug, Default, Clone, Copy)]
pub struct FerroIntrinsicNormalizer;

impl IntrinsicNormalizer for FerroIntrinsicNormalizer {
    fn normalize_macro(&self, expr: Expr) -> Result<NormalizeOutcome<Expr>> {
        let (ty_slot, kind) = expr.into_parts();
        let ExprKind::Macro(macro_expr) = kind else {
            return Ok(NormalizeOutcome::Ignored(Expr::from_parts(ty_slot, kind)));
        };

        if let Some(name) = macro_expr.invocation.path.segments.last() {
            if name.as_str() == "t" {
                let ty = parse_type_macro_tokens(&macro_expr.invocation.tokens)?;
                let replacement = Expr::value(Value::Type(ty)).with_ty_slot(ty_slot);
                return Ok(NormalizeOutcome::Normalized(replacement));
            }
        }

        let fallback = Expr::from_parts(ty_slot, ExprKind::Macro(macro_expr));
        RustIntrinsicNormalizer::default().normalize_macro(fallback)
    }
}

fn parse_type_macro_tokens(tokens: &str) -> Result<fp_core::ast::Ty> {
    let lexemes = lex_lexemes(tokens).map_err(|err| fp_core::error::Error::from(err.to_string()))?;
    let (ty_cst, consumed) =
        parse_type_lexemes_prefix_to_cst(&lexemes, 0, &[])
            .map_err(|err| fp_core::error::Error::from(err.to_string()))?;

    if lexemes[consumed..]
        .iter()
        .any(|lex| lex.kind == LexemeKind::Token)
    {
        return Err(fp_core::error::Error::from(
            "t! macro tokens contain trailing input",
        ));
    }

    lower_type_from_cst(&ty_cst).map_err(|err| fp_core::error::Error::from(err.to_string()))
}
