use anyhow::{bail, Result};

pub fn extract_select_projection(statement: &str) -> Result<&str> {
    let trimmed = statement.trim().trim_end_matches(';').trim();
    if trimmed.is_empty() {
        return Ok("");
    }

    let lower = trimmed.to_ascii_lowercase();
    if !lower.starts_with("select") {
        bail!("unsupported SQL statement: {trimmed}");
    }

    let select_len = "select".len();
    let from_idx = find_keyword_outside_quotes(trimmed, "from", select_len);
    let selection = if let Some(idx) = from_idx {
        if idx < select_len || !trimmed.is_char_boundary(idx) {
            bail!("invalid SQL statement: {trimmed}");
        }
        let (_, tail) = trimmed.split_at(select_len);
        let offset = idx - select_len;
        if offset > tail.len() || !tail.is_char_boundary(offset) {
            bail!("invalid SQL statement: {trimmed}");
        }
        let (selection, _) = tail.split_at(offset);
        selection
    } else {
        let (_, tail) = trimmed.split_at(select_len);
        tail
    };

    Ok(selection)
}

fn find_keyword_outside_quotes(input: &str, keyword: &str, start: usize) -> Option<usize> {
    let mut in_single = false;
    let mut in_double = false;
    let mut in_backtick = false;
    let mut word_start: Option<usize> = None;

    for (idx, ch) in input.char_indices().skip(start) {
        let mut check_word = |word_start: &mut Option<usize>| {
            if let Some(start_idx) = word_start.take() {
                let word = &input[start_idx..idx];
                if word.eq_ignore_ascii_case(keyword) {
                    return Some(start_idx);
                }
            }
            None
        };

        match ch {
            '\'' if !in_double && !in_backtick => {
                if let Some(found) = check_word(&mut word_start) {
                    return Some(found);
                }
                in_single = !in_single;
                continue;
            }
            '"' if !in_single && !in_backtick => {
                if let Some(found) = check_word(&mut word_start) {
                    return Some(found);
                }
                in_double = !in_double;
                continue;
            }
            '`' if !in_single && !in_double => {
                if let Some(found) = check_word(&mut word_start) {
                    return Some(found);
                }
                in_backtick = !in_backtick;
                continue;
            }
            _ => {}
        }

        if in_single || in_double || in_backtick {
            continue;
        }

        if ch.is_ascii_alphanumeric() || ch == '_' {
            if word_start.is_none() {
                word_start = Some(idx);
            }
        } else if let Some(start_idx) = word_start.take() {
            let word = &input[start_idx..idx];
            if word.eq_ignore_ascii_case(keyword) {
                return Some(start_idx);
            }
        }
    }

    if let Some(start_idx) = word_start.take() {
        let word = &input[start_idx..];
        if word.eq_ignore_ascii_case(keyword) {
            return Some(start_idx);
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::extract_select_projection;

    #[test]
    fn extracts_projection_before_from() {
        let projection = extract_select_projection("SELECT a, b FROM t").expect("projection");
        assert_eq!(projection.trim(), "a, b");
    }

    #[test]
    fn extracts_projection_without_from() {
        let projection = extract_select_projection("SELECT a, b").expect("projection");
        assert_eq!(projection.trim(), "a, b");
    }

    #[test]
    fn detects_from_followed_by_quote() {
        let projection = extract_select_projection("SELECT a FROM'table'")
            .expect("projection");
        assert_eq!(projection.trim(), "a");
    }
}
