#!/usr/bin/env fp interpret
//! Generate `crates/fp-lang/src/std/libc/generated.fp` from bindgen output.
//!
//! This FerroPhase script is intended to become the source of truth for libc binding
//! generation. The existing `scripts/generate_libc_bindings.py` is transitional and
//! should eventually be transpiled from this file.

use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process;

type BindingSet = {
    type_aliases: Vec<str>,
    consts: Vec<str>,
    externs: Vec<str>,
};

const DEFAULT_OUTPUT: &str = "crates/fp-lang/src/std/libc/generated.fp";
const OUTPUT_ENV: &str = "FP_LIBC_BINDINGS_OUTPUT";
const REPO_ROOT_ENV: &str = "FP_REPO_ROOT";

fn main() {
    let output_path = output_path();
    let bindgen_rs = run_bindgen();
    let bindings = parse_bindgen_output(&bindgen_rs);
    let content = render_output(&bindings);
    let changed = write_if_changed(&output_path, &content);
    let action = if changed { "updated" } else { "unchanged" };
    println!("{} {} via bindgen", output_path.as_str(), action);
}

fn output_path() -> PathBuf {
    if env::exists(OUTPUT_ENV) {
        return PathBuf::from(env::var(OUTPUT_ENV));
    }
    repo_root().join(&Path::new(DEFAULT_OUTPUT))
}

fn repo_root() -> PathBuf {
    if env::exists(REPO_ROOT_ENV) {
        return PathBuf::from(env::var(REPO_ROOT_ENV));
    }
    PathBuf::from(env::current_dir())
}

fn build_wrapper_header() -> str {
    let mut lines = Vec::new();
    lines.push("#include <sys/types.h>");
    lines.push("#include <sys/wait.h>");
    lines.push("#include <unistd.h>");
    lines.push("#include <fcntl.h>");
    lines.push("");
    join_strings(&lines, "\n")
}

fn run_bindgen() -> str {
    if !process::ok("command -v bindgen >/dev/null 2>&1") {
        panic("bindgen was not found in PATH. Install it with `cargo install bindgen-cli`.");
    }

    let temp_dir = PathBuf::from(env::temp_dir()).join(&Path::new("fp-libc-bindings"));
    fs::create_dir_all(temp_dir.as_path());

    let wrapper_path = temp_dir.join(&Path::new("wrapper.h"));
    fs::write_string(wrapper_path.as_path(), &build_wrapper_header());

    let command =
        "bindgen " + shell_quote(wrapper_path.as_str()) + " --no-layout-tests --no-doc-comments";
    process::output(command)
}

fn parse_bindgen_output(bindgen_rs: &str) -> BindingSet {
    let mut type_aliases = canonical_type_aliases();
    let mut consts = Vec::new();
    let mut externs = Vec::new();

    let lines = split_lines(bindgen_rs);
    let mut idx = 0;
    let mut in_extern_block = false;
    let mut extern_stmt = "";

    while idx < lines.len() {
        let line = trim(lines[idx]);
        idx = idx + 1;

        if line == "" {
            continue;
        }

        if starts_with(line, "pub type ") {
            let alias = parse_type_alias_line(line);
            if alias != "" {
                push_unique(&mut type_aliases, alias);
            }
            continue;
        }

        if starts_with(line, "pub const ") {
            continue;
        }

        if starts_with(line, "unsafe extern \"C\"") {
            in_extern_block = true;
            extern_stmt = "";
            continue;
        }

        if !in_extern_block {
            continue;
        }

        if line == "}" {
            in_extern_block = false;
            extern_stmt = "";
            continue;
        }

        if extern_stmt == "" {
            extern_stmt = line;
        } else {
            extern_stmt = extern_stmt + " " + line;
        }

        if ends_with(line, ";") {
            let signature = parse_extern_line(&extern_stmt);
            if signature != "" {
                push_unique(&mut externs, signature);
            }
            extern_stmt = "";
        }
    }

    BindingSet {
        type_aliases,
        consts,
        externs,
    }
}

fn canonical_type_aliases() -> Vec<str> {
    let mut aliases = Vec::new();
    aliases.push("pub type c_char = i8;");
    aliases.push("pub type c_int = i32;");
    aliases.push("pub type pid_t = i32;");
    aliases.push("pub type size_t = usize;");
    aliases.push("pub type ssize_t = isize;");
    aliases
}

fn parse_type_alias_line(line: &str) -> str {
    let body = strip_prefix(line, "pub type ");
    let eq = find(body, "=");
    if eq < 0 {
        return "";
    }
    let name = trim(body[0..eq as usize]);
    let rhs = trim(strip_suffix(body[eq as usize + 1..], ";"));
    if name == "" || rhs == "" {
        return "";
    }
    "pub type " + name + " = " + normalize_rust_type(rhs) + ";"
}

fn parse_const_line(line: &str) -> str {
    let body = strip_prefix(line, "pub const ");
    let colon = find(body, ":");
    let eq = find(body, "=");
    if colon < 0 || eq < 0 || colon > eq {
        return "";
    }
    let name = trim(body[0..colon as usize]);
    let rust_ty = trim(body[colon as usize + 1..eq as usize]);
    let value = trim(strip_suffix(body[eq as usize + 1..], ";"));
    if starts_with(value, "b\"") {
        return "";
    }
    if name == "" || rust_ty == "" || value == "" {
        return "";
    }
    "pub const " + name + ": " + normalize_rust_type(rust_ty) + " = " + value + ";"
}

fn parse_extern_line(line: &str) -> str {
    let body = trim(strip_suffix(line, ";"));
    if !starts_with(body, "pub fn ") {
        return "";
    }

    let name_start = "pub fn ".len() as i64;
    let open_paren = find(body, "(");
    if open_paren < 0 || open_paren < name_start {
        return "";
    }
    let close_paren = find_matching_paren(body, open_paren);
    if close_paren < 0 {
        return "";
    }

    let name = trim(body[name_start as usize..open_paren as usize]);
    let raw_args = trim(body[open_paren as usize + 1..close_paren as usize]);
    let rest = trim(body[close_paren as usize + 1..]);

    if contains(raw_args, "...") {
        if name == "open" {
            return "pub extern \"C\" fn open(path: &std::ffi::CStr, oflag: c_int, mode: mode_t) -> c_int;";
        }
        return "";
    }

    let mut fp_args = Vec::new();
    if raw_args != "" {
        let args = split_args(raw_args);
        let mut idx = 0;
        while idx < args.len() {
            let fp_arg = parse_extern_arg(args[idx]);
            if fp_arg == "" {
                return "";
            }
            fp_args.push(fp_arg);
            idx = idx + 1;
        }
    }

    let mut signature = "pub extern \"C\" fn " + name + "(" + join_strings(&fp_args, ", ") + ")";
    if starts_with(rest, "->") {
        let raw_ret = trim(rest[2..]);
        if raw_ret != "" && raw_ret != "()" {
            signature = signature + " -> " + normalize_rust_type(raw_ret);
        }
    }
    signature + ";"
}

fn parse_extern_arg(raw_arg: &str) -> str {
    let colon = find(raw_arg, ":");
    if colon < 0 {
        return "";
    }
    let arg_name = trim(raw_arg[0..colon as usize]);
    let arg_ty = trim(raw_arg[colon as usize + 1..]);
    if arg_name == "" || arg_ty == "" {
        return "";
    }
    arg_name + ": " + normalize_function_arg_type(arg_ty)
}

fn normalize_function_arg_type(raw: &str) -> str {
    let normalized = normalize_rust_type(raw);
    if normalized == "*const c_char" {
        return "&std::ffi::CStr";
    }
    normalized
}

fn normalize_rust_type(raw: &str) -> str {
    let mut text = trim(raw);
    text = replace_all(text, "::std::os::raw::", "");
    text = replace_all(text, "::std::ffi::", "");
    text = replace_all(text, "std::os::raw::", "");
    text = replace_all(text, "std::ffi::", "");
    text = collapse_spaces(text);

    if contains(text, "extern \"C\" fn") {
        if starts_with(text, "::std::option::Option<") || starts_with(text, "std::option::Option<") {
            return "::std::option::Option<usize>";
        }
        return "usize";
    }

    if text == "!" { return "c_int"; }
    if text == "i8" { return "i8"; }
    if text == "i32" { return "i32"; }
    if text == "isize" { return "isize"; }
    if text == "u8" { return "u8"; }
    if text == "u32" { return "c_int"; }
    if text == "usize" { return "usize"; }
    if text == "c_char" { return "c_char"; }
    if text == "c_int" { return "c_int"; }
    if text == "c_void" { return "u8"; }
    if text == "pid_t" { return "pid_t"; }
    if text == "size_t" { return "size_t"; }
    if text == "ssize_t" { return "ssize_t"; }
    if text == "__int32_t" { return "c_int"; }
    if text == "__darwin_pid_t" { return "pid_t"; }

    if starts_with(text, "*const ") {
        return "*const " + normalize_rust_type(text["*const ".len()..]);
    }
    if starts_with(text, "*mut ") {
        return "*mut " + normalize_rust_type(text["*mut ".len()..]);
    }

    text
}

fn render_output(bindings: &BindingSet) -> str {
    let mut header = Vec::new();
    header.push("// @generated by scripts/generate_libc_bindings.fp");
    header.push("// Future source of truth: this .fp generator; scripts/generate_libc_bindings.py is transitional and should be transpiled from it.");
    header.push("// Source: bindgen over libc wrapper headers.");
    header.push("");
    let mut sections = Vec::new();
    sections.push(join_strings(&header, "\n"));
    sections.push(join_strings(&bindings.type_aliases, "\n"));
    sections.push("");
    sections.push(join_strings(&bindings.consts, "\n"));
    sections.push("");
    sections.push(join_strings(&bindings.externs, "\n"));
    sections.push("");
    join_strings(&sections, "\n")
}

fn write_if_changed(path: &PathBuf, content: &str) -> bool {
    if fs::exists(path.as_path()) {
        let current = fs::read_to_string(path.as_path());
        if current == content {
            return false;
        }
    }
    fs::write_string(path.as_path(), content);
    true
}

fn split_lines(text: &str) -> Vec<str> {
    let mut lines = Vec::new();
    let mut start = 0i64;
    let mut idx = 0i64;
    let text_len = text.len() as i64;
    while idx < text_len {
        let ch = text[idx as usize..idx as usize + 1];
        if ch == "\n" {
            lines.push(text[start as usize..idx as usize]);
            start = idx + 1;
        }
        idx = idx + 1;
    }
    lines.push(text[start as usize..text_len as usize]);
    lines
}

fn split_args(raw_args: &str) -> Vec<str> {
    let mut args = Vec::new();
    let mut current = "";
    let mut idx = 0i64;
    let mut depth = 0i64;
    let raw_len = raw_args.len() as i64;

    while idx < raw_len {
        let ch = raw_args[idx as usize..idx as usize + 1];
        if ch == "," && depth == 0 {
            args.push(trim(current));
            current = "";
            idx = idx + 1;
            continue;
        }
        if ch == "(" || ch == "<" || ch == "[" || ch == "{" {
            depth = depth + 1;
        } else if ch == ")" || ch == ">" || ch == "]" || ch == "}" {
            depth = depth - 1;
        }
        current = current + ch;
        idx = idx + 1;
    }

    if trim(current) != "" {
        args.push(trim(current));
    }
    args
}

fn join_strings(parts: &Vec<str>, separator: &str) -> str {
    let mut out = "";
    let mut idx = 0;
    while idx < parts.len() {
        if idx > 0 {
            out = out + separator;
        }
        out = out + parts[idx];
        idx = idx + 1;
    }
    out
}

fn push_unique(items: &mut Vec<str>, value: str) {
    let mut idx = 0;
    while idx < items.len() {
        if items[idx] == value {
            return;
        }
        idx = idx + 1;
    }
    items.push(value);
}

fn trim(text: &str) -> str {
    trim_end(trim_start(text))
}

fn trim_start(text: &str) -> str {
    let mut idx = 0i64;
    let text_len = text.len() as i64;
    while idx < text_len {
        let ch = text[idx as usize..idx as usize + 1];
        if ch == " " || ch == "\t" || ch == "\n" || ch == "\r" {
            idx = idx + 1;
            continue;
        }
        break;
    }
    text[idx as usize..text_len as usize]
}

fn trim_end(text: &str) -> str {
    let mut idx = text.len() as i64 - 1;
    while idx >= 0 {
        let ch = text[idx as usize..idx as usize + 1];
        if ch == " " || ch == "\t" || ch == "\n" || ch == "\r" {
            idx = idx - 1;
            continue;
        }
        break;
    }
    if idx < 0 {
        return "";
    }
    text[0..idx as usize + 1]
}

fn starts_with(text: &str, prefix: &str) -> bool {
    if prefix.len() > text.len() {
        return false;
    }
    let mut idx = 0i64;
    let prefix_len = prefix.len() as i64;
    while idx < prefix_len {
        if text[idx as usize..idx as usize + 1] != prefix[idx as usize..idx as usize + 1] {
            return false;
        }
        idx = idx + 1;
    }
    true
}

fn ends_with(text: &str, suffix: &str) -> bool {
    if suffix.len() > text.len() {
        return false;
    }
    let offset = text.len() - suffix.len();
    text[offset..text.len()] == suffix
}

fn contains(text: &str, needle: &str) -> bool {
    find(text, needle) >= 0
}

fn find(text: &str, needle: &str) -> i64 {
    if needle == "" {
        return 0;
    }
    let text_len = text.len() as i64;
    let needle_len = needle.len() as i64;
    if needle_len > text_len {
        return -1;
    }

    let mut idx = 0i64;
    while idx <= text_len - needle_len {
        if text[idx as usize..idx as usize + needle_len as usize] == needle {
            return idx;
        }
        idx = idx + 1;
    }
    -1
}

fn strip_prefix(text: &str, prefix: &str) -> str {
    if starts_with(text, prefix) {
        return text[prefix.len()..];
    }
    text
}

fn strip_suffix(text: &str, suffix: &str) -> str {
    if ends_with(text, suffix) {
        return text[0..text.len() - suffix.len()];
    }
    text
}

fn replace_all(text: &str, from: &str, to: &str) -> str {
    if from == "" {
        return text;
    }

    let mut out = "";
    let mut start = 0i64;
    let text_len = text.len() as i64;
    let from_len = from.len() as i64;

    while start < text_len {
        let slice = text[start as usize..text_len as usize];
        let pos = find(slice, from);
        if pos < 0 {
            out = out + slice;
            break;
        }
        let absolute = start + pos;
        out = out + text[start as usize..absolute as usize] + to;
        start = absolute + from_len;
    }

    out
}

fn collapse_spaces(text: &str) -> str {
    let mut out = "";
    let mut idx = 0i64;
    let mut prev_space = false;
    let text_len = text.len() as i64;
    while idx < text_len {
        let ch = text[idx as usize..idx as usize + 1];
        let is_space = ch == " " || ch == "\t" || ch == "\n" || ch == "\r";
        if is_space {
            if !prev_space {
                out = out + " ";
            }
            prev_space = true;
        } else {
            out = out + ch;
            prev_space = false;
        }
        idx = idx + 1;
    }
    trim(out)
}

fn find_matching_paren(text: &str, open_paren: i64) -> i64 {
    let mut idx = open_paren;
    let mut depth = 0i64;
    let text_len = text.len() as i64;
    while idx < text_len {
        let ch = text[idx as usize..idx as usize + 1];
        if ch == "(" {
            depth = depth + 1;
        } else if ch == ")" {
            depth = depth - 1;
            if depth == 0 {
                return idx;
            }
        }
        idx = idx + 1;
    }
    -1
}

fn shell_quote(text: &str) -> str {
    "'" + replace_all(text, "'", "'\"'\"'") + "'"
}
