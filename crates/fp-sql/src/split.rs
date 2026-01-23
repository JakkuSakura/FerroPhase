use fp_core::query::split_sql_statements;

pub fn split_statements(sql: &str) -> Vec<String> {
    split_sql_statements(sql)
}

pub fn strip_leading_sql_comments(sql: &str) -> &str {
    let mut remaining = sql.trim_start();
    loop {
        let trimmed = remaining.trim_start();
        if trimmed.starts_with("--") {
            if let Some(idx) = trimmed.find('\n') {
                remaining = &trimmed[idx + 1..];
                continue;
            } else {
                return "";
            }
        }
        if trimmed.starts_with("/*") {
            if let Some(idx) = trimmed.find("*/") {
                remaining = &trimmed[idx + 2..];
                continue;
            } else {
                return "";
            }
        }
        return trimmed;
    }
}

pub fn replace_engine_case_insensitive(sql: &str, engine: &str) -> String {
    let lower = sql.to_ascii_lowercase();
    let Some(idx) = find_keyword(&lower, "engine") else {
        return sql.to_string();
    };
    let mut cursor = idx + "engine".len();
    let bytes = sql.as_bytes();
    while cursor < bytes.len() && bytes[cursor].is_ascii_whitespace() {
        cursor += 1;
    }
    if cursor < bytes.len() && bytes[cursor] == b'=' {
        cursor += 1;
    }
    while cursor < bytes.len() && bytes[cursor].is_ascii_whitespace() {
        cursor += 1;
    }
    let start = cursor;
    while cursor < bytes.len()
        && (bytes[cursor].is_ascii_alphanumeric() || bytes[cursor] == b'_')
    {
        cursor += 1;
    }

    let mut output = String::new();
    output.push_str(sql[..start].trim_end());
    output.push(' ');
    output.push_str(engine);
    output.push(' ');
    output.push_str(sql[cursor..].trim_start());
    output.trim().to_string()
}

pub fn ensure_engine_clause(sql: &str, engine: &str) -> String {
    let lower = sql.to_ascii_lowercase();
    if find_keyword(&lower, "engine").is_some() {
        return sql.to_string();
    }

    if let Some(order_idx) = find_keyword(&lower, "order by") {
        let (head, tail) = sql.split_at(order_idx);
        let mut output = head.trim_end().to_string();
        output.push_str(" ENGINE = ");
        output.push_str(engine);
        output.push(' ');
        output.push_str(tail.trim_start());
        return output;
    }

    let mut output = sql.trim_end().to_string();
    output.push_str(" ENGINE = ");
    output.push_str(engine);
    output
}

fn find_keyword(haystack_lower: &str, keyword: &str) -> Option<usize> {
    let target = keyword.to_ascii_lowercase();
    let mut start = 0;
    while let Some(idx) = haystack_lower[start..].find(&target) {
        let absolute = start + idx;
        if is_word_boundary(haystack_lower, absolute, target.len()) {
            return Some(absolute);
        }
        start = absolute + target.len();
    }
    None
}

fn is_word_boundary(text: &str, start: usize, len: usize) -> bool {
    let bytes = text.as_bytes();
    let before = start.checked_sub(1).and_then(|idx| bytes.get(idx));
    let after = bytes.get(start + len);

    let is_boundary = |b: &u8| !b.is_ascii_alphanumeric() && *b != b'_';

    let before_ok = before.map(is_boundary).unwrap_or(true);
    let after_ok = after.map(is_boundary).unwrap_or(true);
    before_ok && after_ok
}
