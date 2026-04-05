use std::option::Option;

pub struct ParsedLine {
    name: str,
    ok: bool,
}

pub fn find_substring(text: &str, needle: &str) -> i64 {
    if needle == "" {
        return 0;
    }
    if needle.len() > text.len() {
        return -1;
    }
    let mut idx = 0;
    let last = text.len() - needle.len();
    while idx <= last {
        if text[idx..idx + needle.len()] == needle {
            return idx;
        }
        idx = idx + 1;
    }
    -1
}

pub fn find_char(text: &str, ch: &str, start: i64) -> i64 {
    let mut idx = start;
    while idx < text.len() {
        if text[idx..idx + 1] == ch {
            return idx;
        }
        idx = idx + 1;
    }
    -1
}

pub fn trim_leading(text: &str) -> str {
    let mut idx = 0;
    while idx < text.len() {
        let ch = text[idx..idx + 1];
        if ch == " " || ch == "\t" || ch == ":" {
            idx = idx + 1;
        } else {
            break;
        }
    }
    text[idx..text.len()]
}

pub fn extract_test_name(line: &str) -> Option<str> {
    let fn_idx = find_substring(line, "fn ");
    if fn_idx < 0 {
        return Option::None;
    }
    let name_start = fn_idx + "fn ".len();
    if name_start >= line.len() {
        return Option::None;
    }
    let open_idx = find_char(line, "(", name_start);
    if open_idx < 0 {
        return Option::None;
    }
    let name = line[name_start..open_idx];
    let name = trim_leading(name);
    if !name.starts_with("test_") {
        return Option::None;
    }
    Option::Some(name)
}

pub fn parse_test_output(output: &str) -> Vec<ParsedLine> {
    let lines = output.split("\n");
    let mut idx = 0;
    let mut parsed: Vec<ParsedLine> = Vec::new();
    while idx < lines.len() {
        let line = lines[idx];
        let marker_idx = find_substring(line, " ... ");
        if marker_idx >= 0 {
            let name = trim_leading(line[0..marker_idx]);
            let status = line[marker_idx + " ... ".len()..line.len()];
            let ok = status.starts_with("ok");
            let failed = status.starts_with("FAILED");
            if ok || failed {
                parsed.push(ParsedLine {
                    name,
                    ok,
                });
            }
        }
        idx = idx + 1;
    }
    parsed
}
