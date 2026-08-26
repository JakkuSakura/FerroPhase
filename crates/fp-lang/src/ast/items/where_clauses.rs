use super::super::*;

pub(super) fn parse_where_clause_predicates(
    input: &mut &[Token],
) -> ModalResult<Vec<(Vec<Ident>, TypeBounds)>> {
    let mut predicates = Vec::new();
    loop {
        if input.is_empty() || matches!(peek_symbol(*input), Some("{") | Some(";")) {
            break;
        }
        let mut probe = *input;
        let parsed = (|| -> ModalResult<(Vec<Ident>, TypeBounds)> {
            skip_hrtb_for_lifetimes_in_predicate(&mut probe);
            let mut path = vec![ident_like(&mut probe)?];
            while skip_symbol(&mut probe, "::").is_ok() {
                path.push(ident_like(&mut probe)?);
            }
            skip_symbol(&mut probe, ":")?;
            let bounds = parse_type_bounds(&mut probe)?;
            Ok((path, bounds))
        })();
        match parsed {
            Ok(predicate) => {
                predicates.push(predicate);
                *input = probe;
            }
            Err(_) => skip_one_where_predicate(input)?,
        }
        if skip_symbol(input, ",").is_err() {
            break;
        }
    }
    Ok(predicates)
}

fn skip_hrtb_for_lifetimes_in_predicate(input: &mut &[Token]) {
    let mut probe = *input;
    if skip_keyword(&mut probe, Keyword::For).is_err() || skip_symbol(&mut probe, "<").is_err() {
        return;
    }
    loop {
        if ident_like(&mut probe).is_err() {
            return;
        }
        if skip_symbol(&mut probe, ",").is_ok() {
            continue;
        }
        break;
    }
    if skip_symbol(&mut probe, ">").is_ok() {
        *input = probe;
    }
}

fn skip_one_where_predicate(input: &mut &[Token]) -> ModalResult<()> {
    let mut depth: i32 = 0;
    while !input.is_empty() {
        if depth == 0 && matches!(peek_symbol(input), Some(",") | Some("{") | Some(";")) {
            return Ok(());
        }
        match peek_symbol(input) {
            Some("(" | "[" | "{" | "<") => depth += 1,
            Some(")" | "]" | "}" | ">") => depth -= 1,
            Some("<<") => depth += 2,
            Some(">>") => depth -= 2,
            _ => {}
        }
        *input = &input[1..];
    }
    Err(ErrMode::Cut(ContextError::new()))
}

pub(super) fn parse_where_clause_and_merge(
    input: &mut &[Token],
    generics_params: &mut [fp_core::ast::GenericParam],
) -> ModalResult<()> {
    let predicates = parse_where_clause_predicates(input)?;
    for (path, bounds) in predicates {
        let Some(name) = path.first() else {
            continue;
        };
        if let Some(param) = generics_params
            .iter_mut()
            .find(|param| param.name.as_str() == name.as_str())
        {
            if let [_, projection] = path.as_slice() {
                param.projection_bounds.push((projection.clone(), bounds));
            } else if path.len() == 1 {
                param.bounds.bounds.extend(bounds.bounds);
            }
        }
    }
    Ok(())
}

pub(super) fn skip_where_clause(input: &mut &[Token]) -> ModalResult<()> {
    let mut depth: i32 = 0;
    while !input.is_empty() {
        if depth == 0 && matches!(peek_symbol(input), Some("{") | Some(";")) {
            return Ok(());
        }
        match peek_symbol(input) {
            Some("(" | "[" | "{" | "<") => depth += 1,
            Some(")" | "]" | "}" | ">") => depth -= 1,
            Some("<<") => depth += 2,
            Some(">>") => depth -= 2,
            _ => {}
        }
        *input = &input[1..];
    }
    Err(ErrMode::Cut(ContextError::new()))
}
