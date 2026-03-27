#![feature(replace_binding)]
#![feature(exception)]
use std::assert;
use std::env;
use std::fs;
use std::json::Value;
use std::path::Path;
use std::yaml;
use std::shell::backend;
use std::collections::hash_map::HashMap;
use std::option::Option;

struct FixtureSummary {
    total: i64,
    passed: i64,
    failed: i64,
}

type JsonValue = Value;

const mut COMMAND_CALLS: Vec<str> = Vec::new();

fn reset_command_calls() {
    COMMAND_CALLS = Vec::new();
}

fn record_command(command: str) {
    COMMAND_CALLS.push(command);
}

fn take_command_calls() -> Vec<str> {
    let calls = COMMAND_CALLS;
    COMMAND_CALLS = Vec::new();
    calls
}

fn install_shell_hooks() {
    let handler = move |_host: str,
                        command: str,
                        _only_if: str,
                        _unless: str,
                        _creates: str,
                        _removes: str| {
        record_command(command);
        std::shell::backend::runtime_set_changed(true);
    };
    std::shell::backend::shell_run = handler;
    std::shell::backend::shell_run_local = handler;
    std::shell::backend::shell_run_ssh = handler;
    std::shell::backend::shell_run_docker = handler;
    std::shell::backend::shell_run_kubectl = handler;
    std::shell::backend::shell_run_winrm = handler;
    std::shell::backend::shell_run_chroot = handler;
}

fn main() {
    install_shell_hooks();
    let repo_root = env::current_dir();
    let fixture_root = join_path(&repo_root, "crates/fp-shell/tests/fixtures/operations");

    let summary = run_fixture_suite(&fixture_root);
    println(
        "fp-shell fixture cases: {} passed; {} failed; {} total",
        summary.passed,
        summary.failed,
        summary.total,
    );
    assert::eq_i64(summary.failed, 0);
}

fn run_fixture_suite(fixture_root: &str) -> FixtureSummary {
    let entries = fs::walk_dir(Path::new(fixture_root));
    let mut passed = 0;
    let mut failed = 0;
    let mut total = 0;
    let mut idx = 0;

    while idx < entries.len() {
        let path = entries[idx];
        if is_fixture_path(path) {
            total = total + 1;
            if run_case(path) {
                passed = passed + 1;
            } else {
                failed = failed + 1;
            }
        }
        idx = idx + 1;
    }

    FixtureSummary {
        total,
        passed,
        failed,
    }
}

fn is_fixture_path(path: &str) -> bool {
    path.replace(".json", "") != path
        || path.replace(".yaml", "") != path
        || path.replace(".yml", "") != path
}

fn run_case(path: &str) -> bool {
    let fixture = load_fixture(path);
    let family = case_family(path);
    let workspace = case_workspace(path);

    reset_workspace(&workspace);
    materialize_fixture_workspace(&workspace, fixture);
    reset_command_calls();

    let error = run_shell_case(path, family, fixture);
    let calls = take_command_calls();
    let rendered = calls.join("\n");
    let expects_failure = expects_exception(fixture);

    if expects_failure {
        if error == "" {
            println("{} ... FAILED", path);
            println("  expected interpreted failure but case succeeded");
            return false;
        }

        let message = expected_exception_message(fixture);
        if message != "" && !error.contains(&message) {
            println("{} ... FAILED", path);
            println("  missing exception fragment: {}", message);
            println("  error: {}", error);
            return false;
        }

        println("{} ... ok", path);
        return true;
    }

    if error != "" {
        println("{} ... FAILED", path);
        println("  interpret error: {}", error);
        return false;
    }

    let expected = expected_commands(fixture);
    let mut idx = 0;
    while idx < expected.len() {
        let fragment = expected[idx];
        if !rendered.contains(fragment) {
            println("{} ... FAILED", path);
            println("  missing command fragment: {}", fragment);
            return false;
        }
        idx = idx + 1;
    }

    println("{} ... ok", path);
    true
}

fn case_family(path: &str) -> str {
    let parent = Path::new(path).parent().unwrap();
    parent.file_name().unwrap()
}

fn run_shell_case(path: &str, family: &str, fixture: JsonValue) -> str {
    let op_path = operation_path(family);
    let op_name = operation_name(path);
    let args = json_args(std::json::find_object_field(fixture, "args"));
    let kwargs = json_kwargs(std::json::find_object_field(fixture, "kwargs"));
    let mut error = "";

    try {
        let operation = op_path[op_name];
        operation(*args, **kwargs);
    } catch err {
        error = err;
    }

    error
}

fn operation_name(path: &str) -> str {
    let name = Path::new(path).file_name().unwrap_or("");
    Path::new(name).stem().unwrap_or(name)
}

fn load_fixture(path: &str) -> JsonValue {
    let source = fs::read_to_string(Path::new(path));
    yaml::parse(&source)
}

fn operation_path(family: &str) -> str {
    let rendered = family.replace(".", "::");
    f"std::ops::{rendered}"
}

fn json_args(value: JsonValue) -> Vec<any> {
    match value {
        JsonValue::Null => Vec::new(),
        JsonValue::Array(values) => {
            let mut out = Vec::new();
            let mut idx = 0;
            while idx < values.len() {
                out.push(json_to_value(values[idx]));
                idx = idx + 1;
            }
            out
        }
        _ => panic("expected args array"),
    }
}

fn json_kwargs(value: JsonValue) -> any {
    match value {
        JsonValue::Null => HashMap::new(),
        JsonValue::Object(fields) => {
            let mut entries = Vec::new();
            let mut idx = 0;
            while idx < fields.len() {
                let field = fields[idx];
                entries.push((field.key, json_to_value(field.value)));
                idx = idx + 1;
            }
            HashMap::from(entries)
        }
        _ => panic("expected kwargs object"),
    }
}

fn json_to_value(value: JsonValue) -> any {
    match value {
        JsonValue::Null => null,
        JsonValue::Bool(flag) => {
            flag
        }
        JsonValue::Number(number) => {
            match number.as_i64() {
                Option::Some(value) => value,
                Option::None => {
                    match number.as_u64() {
                        Option::Some(value) => value,
                        Option::None => {
                            match number.as_f64() {
                                Option::Some(value) => value,
                                Option::None => panic("unsupported json number"),
                            }
                        }
                    }
                }
            }
        }
        JsonValue::String(text) => text,
        JsonValue::Array(values) => {
            if array_is_string_list(values) {
                return join_string_list(values);
            }
            let mut rendered = Vec::new();
            let mut idx = 0;
            while idx < values.len() {
                rendered.push(json_to_value(values[idx]));
                idx = idx + 1;
            }
            rendered
        }
        JsonValue::Object(fields) => {
            let mut entries = Vec::new();
            let mut idx = 0;
            while idx < fields.len() {
                let field = fields[idx];
                entries.push((field.key, json_to_value(field.value)));
                idx = idx + 1;
            }
            HashMap::from(entries)
        }
    }
}

fn render_value(value: JsonValue) -> str {
    match value {
        JsonValue::Null => "null",
        JsonValue::Bool(flag) => {
            if flag {
                "true"
            } else {
                "false"
            }
        }
        JsonValue::Number(number) => number.to_string(),
        JsonValue::String(text) => fp_quote(text),
        JsonValue::Array(values) => {
            if array_is_string_list(values) {
                return fp_quote(join_string_list(values));
            }
            let mut rendered = Vec::new();
            let mut idx = 0;
            while idx < values.len() {
                rendered.push(render_value(values[idx]));
                idx = idx + 1;
            }
            let body = rendered.join(", ");
            f"[{body}]"
        }
        JsonValue::Object(fields) => {
            let mut rendered = Vec::new();
            let mut idx = 0;
            while idx < fields.len() {
                let field = fields[idx];
                rendered.push(f"{fp_quote(field.key)}: {render_value(field.value)}");
                idx = idx + 1;
            }
            let body = rendered.join(", ");
            f"{{{body}}}"
        }
    }
}

fn array_is_string_list(values: Vec<JsonValue>) -> bool {
    let mut idx = 0;
    while idx < values.len() {
        match values[idx] {
            JsonValue::String(_) => {}
            _ => return false,
        }
        idx = idx + 1;
    }
    true
}

fn join_string_list(values: Vec<JsonValue>) -> str {
    let mut out = Vec::new();
    let mut idx = 0;
    while idx < values.len() {
        match values[idx] {
            JsonValue::String(text) => out.push(text),
            _ => {}
        }
        idx = idx + 1;
    }
    out.join(" ")
}

fn case_workspace(path: &str) -> str {
    let slug = path
        .replace("/", "__")
        .replace(" ", "_")
        .replace(".", "_");
    join_path("/tmp/fp-shell-fixtures", &slug)
}

fn reset_workspace(workspace: &str) {
    if fs::exists(Path::new(workspace)) {
        fs::remove_dir_all(Path::new(workspace));
    }
    fs::create_dir_all(Path::new(workspace));
}

fn materialize_fixture_workspace(workspace: &str, fixture: JsonValue) {
    materialize_local_files(workspace, std::json::find_object_field(fixture, "local_files"));
    materialize_directories(workspace, std::json::find_object_field(fixture, "directories"));
}

fn materialize_local_files(workspace: &str, value: JsonValue) {
    match value {
        JsonValue::Null => {}
        JsonValue::Object(_) => {
            materialize_file_map(workspace, std::json::find_object_field(value, "files"));
            materialize_dir_map(workspace, std::json::find_object_field(value, "dirs"));
        }
        _ => panic("expected local_files object"),
    }
}

fn materialize_directories(workspace: &str, value: JsonValue) {
    match value {
        JsonValue::Null => {}
        JsonValue::Object(fields) => {
            let mut idx = 0;
            while idx < fields.len() {
                let dir_path = join_path(workspace, fields[idx].key);
                fs::create_dir_all(Path::new(&dir_path));
                idx = idx + 1;
            }
        }
        _ => panic("expected directories object"),
    }
}

fn materialize_file_map(workspace: &str, value: JsonValue) {
    match value {
        JsonValue::Null => {}
        JsonValue::Object(fields) => {
            let mut idx = 0;
            while idx < fields.len() {
                let field = fields[idx];
                let file_path = join_path(workspace, field.key);
                match field.value {
                    JsonValue::Null => fs::write_string(Path::new(&file_path), ""),
                    JsonValue::String(content) => fs::write_string(Path::new(&file_path), content),
                    _ => panic("expected local file content to be string or null"),
                }
                idx = idx + 1;
            }
        }
        _ => panic("expected files object"),
    }
}

fn materialize_dir_map(workspace: &str, value: JsonValue) {
    match value {
        JsonValue::Null => {}
        JsonValue::Object(fields) => {
            let mut idx = 0;
            while idx < fields.len() {
                let dir_path = join_path(workspace, fields[idx].key);
                fs::create_dir_all(Path::new(&dir_path));
                idx = idx + 1;
            }
        }
        _ => panic("expected dirs object"),
    }
}

fn join_path(base: &str, child: &str) -> str {
    f"{base}/{child}"
}

fn expects_exception(fixture: JsonValue) -> bool {
    !std::json::is_null(std::json::find_object_field(fixture, "exception"))
}

fn expected_exception_message(fixture: JsonValue) -> str {
    let exception = std::json::find_object_field(fixture, "exception");
    if std::json::is_null(exception) {
        return "";
    }

    let message = std::json::find_object_field(exception, "message");
    if std::json::is_null(message) {
        return "";
    }

    std::json::get_string(message)
}

fn expected_commands(fixture: JsonValue) -> Vec<&str> {
    let commands = std::json::find_object_field(fixture, "commands");
    match commands {
        JsonValue::Null => Vec::new(),
        JsonValue::Array(values) => {
            let mut out = Vec::new();
            let mut idx = 0;
            while idx < values.len() {
                let fragment = command_fragment(values[idx]);
                if fragment != "" {
                    out.push(fragment);
                }
                idx = idx + 1;
            }
            out
        }
        _ => panic("expected commands array"),
    }
}

fn command_fragment(value: JsonValue) -> str {
    match value {
        JsonValue::Null => "",
        JsonValue::String(text) => text,
        JsonValue::Object(_) => {
            let masked = std::json::find_object_field(value, "masked");
            if !std::json::is_null(masked) {
                return std::json::get_string(masked);
            }
            std::json::get_string(std::json::find_object_field(value, "raw"))
        }
        JsonValue::Array(values) => {
            let mut rendered = Vec::new();
            let mut idx = 0;
            while idx < values.len() {
                rendered.push(render_value(values[idx]));
                idx = idx + 1;
            }
            rendered.join(" ")
        }
        _ => panic("unsupported command fragment value"),
    }
}

fn fp_quote(value: &str) -> str {
    let escaped_slash = value.replace("\\", "\\\\");
    let escaped_quote = escaped_slash.replace("\"", "\\\"");
    f"\"{escaped_quote}\""
}
