use std::fs;
use std::path::Path;
use std::option::Option;

pub enum Expectation {
    Normal,
    Skip,
    Xfail,
}

pub struct NamedExpectation {
    name: str,
    expectation: Expectation,
    reason: str,
}

pub struct MarkerSet {
    file_expectation: Expectation,
    file_reason: str,
    named: Vec<NamedExpectation>,
}

fn find_substring(text: &str, needle: &str) -> i64 {
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

fn find_char(text: &str, ch: &str, start: i64) -> i64 {
    let mut idx = start;
    while idx < text.len() {
        if text[idx..idx + 1] == ch {
            return idx;
        }
        idx = idx + 1;
    }
    -1
}

fn trim_leading(text: &str) -> str {
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

fn extract_test_name(line: &str) -> Option<str> {
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

fn is_marker(line: &str, marker: &str) -> bool {
    line.contains(marker)
}

fn marker_reason(line: &str, marker: &str) -> str {
    let idx = find_substring(line, marker);
    if idx < 0 {
        return "";
    }
    let start = idx + marker.len();
    if start >= line.len() {
        return "";
    }
    trim_leading(line[start..line.len()])
}

pub fn parse_markers(path: &str) -> MarkerSet {
    let content = fs::read_to_string(&Path::new(path));
    let lines = content.split("\n");
    let mut idx = 0;
    let mut file_expectation = Expectation::Normal;
    let mut file_reason = "";
    let mut named: Vec<NamedExpectation> = Vec::new();
    let mut next_expectation = Expectation::Normal;
    let mut next_reason = "";

    while idx < lines.len() {
        let line = lines[idx];
        if is_marker(line, "fptest: skip-next") {
            next_expectation = Expectation::Skip;
            next_reason = marker_reason(line, "fptest: skip-next");
        } else if is_marker(line, "fptest: xfail-next") {
            next_expectation = Expectation::Xfail;
            next_reason = marker_reason(line, "fptest: xfail-next");
        } else if is_marker(line, "fptest: skip") {
            file_expectation = Expectation::Skip;
            file_reason = marker_reason(line, "fptest: skip");
        } else if is_marker(line, "fptest: xfail") {
            file_expectation = Expectation::Xfail;
            file_reason = marker_reason(line, "fptest: xfail");
        }

        match extract_test_name(line) {
            Option::Some(name) => {
                match next_expectation {
                    Expectation::Normal => {}
                    Expectation::Skip => {
                        named.push(NamedExpectation {
                            name,
                            expectation: Expectation::Skip,
                            reason: next_reason,
                        });
                        next_expectation = Expectation::Normal;
                        next_reason = "";
                    }
                    Expectation::Xfail => {
                        named.push(NamedExpectation {
                            name,
                            expectation: Expectation::Xfail,
                            reason: next_reason,
                        });
                        next_expectation = Expectation::Normal;
                        next_reason = "";
                    }
                }
            }
            Option::None => {}
        }

        idx = idx + 1;
    }

    MarkerSet {
        file_expectation,
        file_reason,
        named,
    }
}

pub fn find_named_expectation(markers: &MarkerSet, name: &str) -> Expectation {
    let mut idx = 0;
    while idx < markers.named.len() {
        let entry = markers.named[idx];
        if entry.name == name {
            return entry.expectation;
        }
        idx = idx + 1;
    }
    Expectation::Normal
}

pub fn find_named_reason(markers: &MarkerSet, name: &str) -> str {
    let mut idx = 0;
    while idx < markers.named.len() {
        let entry = markers.named[idx];
        if entry.name == name {
            return entry.reason;
        }
        idx = idx + 1;
    }
    ""
}

pub fn file_expectation(markers: &MarkerSet) -> Expectation {
    markers.file_expectation
}

pub fn file_reason(markers: &MarkerSet) -> str {
    markers.file_reason
}
