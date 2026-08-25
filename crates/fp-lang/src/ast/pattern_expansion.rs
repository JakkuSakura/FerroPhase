use super::*;
use fp_core::ast::{PatternBind, PatternRef};

pub(super) fn parse_pattern_alternatives(input: &mut &[Token]) -> ModalResult<Pattern> {
    let mut alternatives = vec![parse_general_pattern(input)?];
    while skip_symbol(input, "|").is_ok() {
        alternatives.push(parse_general_pattern(input)?);
    }
    if alternatives.len() == 1 {
        return Ok(alternatives.into_iter().next().unwrap());
    }
    Ok(Pattern::new(PatternKind::Or(PatternOr {
        patterns: alternatives,
    })))
}

/// Recursively expands every `PatternKind::Or` node in `pat`, at any
/// nesting depth, into the cartesian product of concrete, `Or`-free
/// patterns — e.g. `(Some(1) | Some(2), y)` becomes `(Some(1), y)` and
/// `(Some(2), y)`. A pattern containing no `Or` anywhere returns itself,
/// unchanged, as the sole element.
pub(super) fn expand_pattern_alternatives(pat: &Pattern) -> Vec<Pattern> {
    match pat.kind() {
        PatternKind::Or(or_pat) => or_pat
            .patterns
            .iter()
            .flat_map(expand_pattern_alternatives)
            .collect(),
        PatternKind::Tuple(tuple) => cartesian_patterns(&tuple.patterns)
            .into_iter()
            .map(|patterns| Pattern::new(PatternKind::Tuple(PatternTuple { patterns })))
            .collect(),
        PatternKind::TupleStruct(tuple_struct) => cartesian_patterns(&tuple_struct.patterns)
            .into_iter()
            .map(|patterns| {
                Pattern::new(PatternKind::TupleStruct(PatternTupleStruct {
                    name: tuple_struct.name.clone(),
                    patterns,
                }))
            })
            .collect(),
        PatternKind::Struct(struct_pat) => cartesian_struct_fields(&struct_pat.fields)
            .into_iter()
            .map(|fields| {
                Pattern::new(PatternKind::Struct(PatternStruct {
                    name: struct_pat.name.clone(),
                    fields,
                    has_rest: struct_pat.has_rest,
                }))
            })
            .collect(),
        PatternKind::Structural(structural) => cartesian_struct_fields(&structural.fields)
            .into_iter()
            .map(|fields| {
                Pattern::new(PatternKind::Structural(PatternStructural {
                    fields,
                    has_rest: structural.has_rest,
                }))
            })
            .collect(),
        PatternKind::Box(box_pat) => expand_pattern_alternatives(&box_pat.pattern)
            .into_iter()
            .map(|inner| {
                Pattern::new(PatternKind::Box(PatternBox {
                    pattern: Box::new(inner),
                }))
            })
            .collect(),
        PatternKind::Ref(reference) => expand_pattern_alternatives(&reference.pattern)
            .into_iter()
            .map(|inner| {
                Pattern::new(PatternKind::Ref(PatternRef {
                    mutability: reference.mutability,
                    pattern: Box::new(inner),
                }))
            })
            .collect(),
        PatternKind::Bind(bind) => expand_pattern_alternatives(&bind.pattern)
            .into_iter()
            .map(|inner| {
                Pattern::new(PatternKind::Bind(PatternBind {
                    ident: bind.ident.clone(),
                    pattern: Box::new(inner),
                }))
            })
            .collect(),
        PatternKind::Type(pattern_type) => expand_pattern_alternatives(&pattern_type.pat)
            .into_iter()
            .map(|inner| {
                Pattern::new(PatternKind::Type(PatternType {
                    pat: Box::new(inner),
                    ty: pattern_type.ty.clone(),
                }))
            })
            .collect(),
        PatternKind::Variant(variant) => match &variant.pattern {
            Some(nested) => expand_pattern_alternatives(nested)
                .into_iter()
                .map(|inner| {
                    Pattern::new(PatternKind::Variant(PatternVariant {
                        name: variant.name.clone(),
                        pattern: Some(Box::new(inner)),
                    }))
                })
                .collect(),
            None => vec![pat.clone()],
        },
        PatternKind::Ident(_)
        | PatternKind::Quote(_)
        | PatternKind::QuotePlural(_)
        | PatternKind::Wildcard(_) => vec![pat.clone()],
    }
}

/// Cartesian product of each pattern's own expansion — `patterns[i]`'s
/// alternatives are independent of every other element's.
pub(super) fn cartesian_patterns(patterns: &[Pattern]) -> Vec<Vec<Pattern>> {
    patterns.iter().fold(vec![Vec::new()], |acc, pat| {
        let alts = expand_pattern_alternatives(pat);
        acc.into_iter()
            .flat_map(|prefix| {
                alts.iter().map(move |alt| {
                    let mut next = prefix.clone();
                    next.push(alt.clone());
                    next
                })
            })
            .collect()
    })
}

/// Same idea as `cartesian_patterns`, but for struct/structural pattern
/// fields, whose `Or`-bearing part (if any) lives in `field.rename`.
fn cartesian_struct_fields(
    fields: &[fp_core::ast::PatternStructField],
) -> Vec<Vec<fp_core::ast::PatternStructField>> {
    fields.iter().fold(vec![Vec::new()], |acc, field| {
        let alts: Vec<Option<Box<Pattern>>> = match &field.rename {
            Some(rename) => expand_pattern_alternatives(rename)
                .into_iter()
                .map(|p| Some(Box::new(p)))
                .collect(),
            None => vec![None],
        };
        acc.into_iter()
            .flat_map(|prefix| {
                alts.iter().map(move |rename| {
                    let mut next = prefix.clone();
                    next.push(fp_core::ast::PatternStructField {
                        name: field.name.clone(),
                        rename: rename.clone(),
                    });
                    next
                })
            })
            .collect()
    })
}
