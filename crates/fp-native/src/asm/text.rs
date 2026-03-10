use fp_core::error::{Error, Result};

pub(crate) fn strip_comment(line: &str) -> &str {
    let mut end = line.len();
    if let Some(index) = line.find(';') {
        end = end.min(index);
    }
    let trimmed = line.trim_start();
    if trimmed.starts_with('#') {
        end = end.min(line.len() - trimmed.len());
    }
    line[..end].trim()
}

pub(crate) fn split_mnemonic_operands(line: &str) -> (&str, Option<&str>) {
    let trimmed = line.trim();
    match trimmed.split_once(char::is_whitespace) {
        Some((mnemonic, rest)) => (mnemonic, Some(rest.trim())),
        None => (trimmed, None),
    }
}

pub(crate) fn split_operands(input: &str) -> Vec<&str> {
    let mut parts = Vec::new();
    let mut start = 0usize;
    let mut depth = 0i32;
    for (index, ch) in input.char_indices() {
        match ch {
            '[' => depth += 1,
            ']' => depth -= 1,
            ',' if depth == 0 => {
                parts.push(input[start..index].trim());
                start = index + 1;
            }
            _ => {}
        }
    }
    let tail = input[start..].trim();
    if !tail.is_empty() {
        parts.push(tail);
    }
    parts
}

pub(crate) fn parse_block_id(token: &str) -> Result<u32> {
    let suffix = token
        .strip_prefix("bb")
        .ok_or_else(|| Error::from(format!("invalid block label: {token}")))?;
    suffix
        .parse::<u32>()
        .map_err(|_| Error::from(format!("invalid block id: {token}")))
}

pub(crate) fn render_signed_offset(value: i64) -> String {
    if value < 0 {
        format!(" - {}", value.unsigned_abs())
    } else if value > 0 {
        format!(" + {value}")
    } else {
        String::new()
    }
}

pub(crate) fn parse_i128(token: &str) -> Result<i128> {
    token
        .trim()
        .trim_start_matches('#')
        .parse::<i128>()
        .map_err(|_| Error::from(format!("invalid immediate: {token}")))
}

pub(crate) fn parse_u16(token: &str, what: &str) -> Result<u16> {
    token
        .parse::<u16>()
        .map_err(|_| Error::from(format!("invalid {what}: {token}")))
}
