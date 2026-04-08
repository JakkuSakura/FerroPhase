#![feature(replace-bindings)]
#![feature(exception)]
use std::assert;
use std::fs;
use std::json::Value;
use std::path::Path;
use std::yaml;
use std::collections::hash_map::HashMap;
use std::collections::hash_map::HashMapEntry;
use std::option::Option;

struct FixtureSummary {
    total: i64,
    passed: i64,
    failed: i64,
}

const mut CURRENT_FACTS: Value = Value::Null;

fn main() {
    let fixture_root = "crates/fp-shell/tests/fixtures/operations";

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
    install_fact_hooks(fixture);
    std::test::reset_command_mocks();

    let error = run_shell_case(path, family, fixture);
    let calls = std::test::take_command_calls();
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

fn run_shell_case(path: &str, family: &str, fixture: Value) -> str {
    let op_path = operation_path(family);
    let op_name = operation_name(path);
    let args = json_args(std::json::find_object_field(fixture, "args"));
    let kwargs = json_kwargs(std::json::find_object_field(fixture, "kwargs"));
    let mut error = "";

    try {
        let operation = op_path[op_name];
        operation(*args, **kwargs);
    } catch (err) {
        error = err;
    }

    error
}

fn operation_name(path: &str) -> str {
    let name = Path::new(path).file_name().unwrap_or("");
    Path::new(name).stem().unwrap_or(name)
}

fn load_fixture(path: &str) -> Value {
    let source = fs::read_to_string(Path::new(path));
    yaml::parse(&source)
}

fn operation_path(family: &str) -> str {
    let rendered = family.replace(".", "::");
    f"std::ops::{rendered}"
}

fn json_args(value: Value) -> Vec<any> {
    match value {
        Value::Null => Vec::new(),
        Value::Array(values) => {
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

fn json_kwargs(value: Value) -> any {
    match value {
        Value::Null => HashMap::new(),
        Value::Object(fields) => {
            let mut entries = Vec::new();
            let mut idx = 0;
            while idx < fields.len() {
                let field = fields[idx];
                entries.push(HashMapEntry {
                    key: field.key,
                    value: json_to_value_with_key(field.key, field.value),
                });
                idx = idx + 1;
            }
            HashMap::from(entries)
        }
        _ => panic("expected kwargs object"),
    }
}

fn json_to_value_with_key(key: &str, value: Value) -> any {
    if key == "content" {
        match value {
            Value::Array(values) => {
                if array_is_string_list(values) {
                    let mut out = Vec::new();
                    let mut idx = 0;
                    while idx < values.len() {
                        match values[idx] {
                            Value::String(text) => out.push(text),
                            _ => {}
                        }
                        idx = idx + 1;
                    }
                    return out.join("\n");
                }
                let mut rendered = Vec::new();
                let mut idx = 0;
                while idx < values.len() {
                    rendered.push(json_to_value(values[idx]));
                    idx = idx + 1;
                }
                return rendered;
            }
            _ => {}
        }
    }
    if key == "keyid" {
        match value {
            Value::String(text) => {
                let mut out = Vec::new();
                out.push(text);
                return out;
            }
            Value::Array(values) => {
                let mut out = Vec::new();
                let mut idx = 0;
                while idx < values.len() {
                    match values[idx] {
                        Value::String(text) => out.push(text),
                        _ => {}
                    }
                    idx = idx + 1;
                }
                return out;
            }
            _ => {}
        }
    }
    json_to_value(value)
}

fn json_to_value(value: Value) -> any {
    match value {
        Value::Null => null,
        Value::Bool(flag) => {
            flag
        }
        Value::Number(number) => {
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
        Value::String(text) => text,
        Value::Array(values) => {
            let mut rendered = Vec::new();
            let mut idx = 0;
            while idx < values.len() {
                rendered.push(json_to_value(values[idx]));
                idx = idx + 1;
            }
            rendered
        }
        Value::Object(fields) => {
            let mut entries = Vec::new();
            let mut idx = 0;
            while idx < fields.len() {
                let field = fields[idx];
                entries.push(HashMapEntry {
                    key: field.key,
                    value: json_to_value(field.value),
                });
                idx = idx + 1;
            }
            HashMap::from(entries)
        }
    }
}

fn json_to_fact_value(value: Value) -> any {
    match value {
        Value::Null => null,
        Value::Bool(flag) => flag,
        Value::Number(number) => {
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
        Value::String(text) => text,
        Value::Array(values) => {
            let mut rendered = Vec::new();
            let mut idx = 0;
            while idx < values.len() {
                rendered.push(json_to_fact_value(values[idx]));
                idx = idx + 1;
            }
            rendered
        }
        Value::Object(fields) => {
            let mut entries = Vec::new();
            let mut idx = 0;
            while idx < fields.len() {
                let field = fields[idx];
                entries.push(HashMapEntry {
                    key: field.key,
                    value: json_to_fact_value(field.value),
                });
                idx = idx + 1;
            }
            HashMap::from(entries)
        }
    }
}

fn render_value(value: Value) -> str {
    match value {
        Value::Null => "null",
        Value::Bool(flag) => {
            if flag {
                "true"
            } else {
                "false"
            }
        }
        Value::Number(number) => number.to_string(),
        Value::String(text) => fp_quote(text),
        Value::Array(values) => {
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
        Value::Object(fields) => {
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

fn array_is_string_list(values: Vec<Value>) -> bool {
    let mut idx = 0;
    while idx < values.len() {
        match values[idx] {
            Value::String(_) => {}
            _ => return false,
        }
        idx = idx + 1;
    }
    true
}

fn join_string_list(values: Vec<Value>) -> str {
    let mut out = Vec::new();
    let mut idx = 0;
    while idx < values.len() {
        match values[idx] {
            Value::String(text) => out.push(text),
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

fn materialize_fixture_workspace(workspace: &str, fixture: Value) {
    materialize_local_files(workspace, std::json::find_object_field(fixture, "local_files"));
    materialize_directories(workspace, std::json::find_object_field(fixture, "directories"));
}

fn materialize_local_files(workspace: &str, value: Value) {
    match value {
        Value::Null => {}
        Value::Object(_) => {
            materialize_file_map(workspace, std::json::find_object_field(value, "files"));
            materialize_dir_map(workspace, std::json::find_object_field(value, "dirs"));
        }
        _ => panic("expected local_files object"),
    }
}

fn materialize_directories(workspace: &str, value: Value) {
    match value {
        Value::Null => {}
        Value::Object(fields) => {
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

fn materialize_file_map(workspace: &str, value: Value) {
    match value {
        Value::Null => {}
        Value::Object(fields) => {
            let mut idx = 0;
            while idx < fields.len() {
                let field = fields[idx];
                let file_path = join_path(workspace, field.key);
                match field.value {
                    Value::Null => fs::write_string(Path::new(&file_path), ""),
                    Value::String(content) => fs::write_string(Path::new(&file_path), content),
                    Value::Object(_) => {
                        let content = std::json::find_object_field(field.value, "content");
                        match content {
                            Value::Null => fs::write_string(Path::new(&file_path), ""),
                            Value::String(text) => fs::write_string(Path::new(&file_path), text),
                            _ => panic("expected local file content to be string or null"),
                        }
                    }
                    _ => panic("expected local file content to be string or null"),
                }
                idx = idx + 1;
            }
        }
        _ => panic("expected files object"),
    }
}

fn materialize_dir_map(workspace: &str, value: Value) {
    match value {
        Value::Null => {}
        Value::Object(fields) => {
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

fn install_fact_hooks(fixture: Value) {
    CURRENT_FACTS = std::json::find_object_field(fixture, "facts");

    let apt_keys = || fact_lookup("apt.AptKeys", "");
    let apt_sources = || fact_lookup("apt.AptSources", "");
    let apt_simulate = |command: str| {
        fact_lookup("apt.SimulateOperationWillChange", f"command={command}")
    };
    let deb_packages = || fact_lookup("deb.DebPackages", "");
    let deb_package = |package: str| fact_lookup("deb.DebPackage", f"package={package}");
    let gpg_key = |src: str| fact_lookup("gpg.GpgKey", f"src={src}");

    std::facts::apt::keys = apt_keys;
    std::facts::apt::sources = apt_sources;
    std::facts::apt::simulate_operation_will_change = apt_simulate;
    std::facts::deb::packages = deb_packages;
    std::facts::deb::package = deb_package;
    std::facts::gpg::key = gpg_key;

    let files_block = |path: str, marker: str, begin: str, end: str| {
        fact_lookup(
            "files.Block",
            f"begin={begin}, end={end}, marker={marker}, path={path}",
        )
    };
    let files_directory = |path: str| fact_lookup("files.Directory", f"path={path}");
    let files_file = |path: str| fact_lookup("files.File", f"path={path}");
    let files_link = |path: str| fact_lookup("files.Link", f"path={path}");
    let files_find_in_file =
        |path: str, pattern: str, interpolate_variables: bool| {
            fact_lookup(
                "files.FindInFile",
                f"interpolate_variables={interpolate_variables}, path={path}, pattern={pattern}",
            )
        };

    let server_date = || fact_lookup("server.Date", "");
    let server_which = |command: str| fact_lookup("server.Which", f"command={command}");

    std::facts::files::block = files_block;
    std::facts::files::directory = files_directory;
    std::facts::files::file = files_file;
    std::facts::files::link = files_link;
    std::facts::files::find_in_file = files_find_in_file;
    std::facts::server::date = server_date;
    std::facts::server::which = server_which;
}

fn fact_lookup(name: &str, key: &str) -> any {
    let facts = CURRENT_FACTS;
    if std::json::is_null(facts) {
        return null;
    }
    let entry = std::json::find_object_field(facts, name);
    if std::json::is_null(entry) {
        return null;
    }
    match entry {
        Value::Object(_) => {
            if key == "" {
                return json_to_fact_value(entry);
            }
            let value = std::json::find_object_field(entry, key);
            if std::json::is_null(value) {
                return null;
            }
            if name == "files.Block" {
                return block_fact_value(value);
            }
            json_to_fact_value(value)
        }
        _ => {
            if name == "files.Block" {
                return block_fact_value(entry);
            }
            json_to_fact_value(entry)
        }
    }
}

fn block_fact_value(value: Value) -> any {
    match value {
        Value::Array(values) => {
            if array_is_string_list(values) {
                let mut out = Vec::new();
                let mut idx = 0;
                while idx < values.len() {
                    match values[idx] {
                        Value::String(text) => out.push(text),
                        _ => {}
                    }
                    idx = idx + 1;
                }
                return out.join("\n");
            }
            json_to_fact_value(value)
        }
        _ => json_to_fact_value(value),
    }
}

fn expects_exception(fixture: Value) -> bool {
    !std::json::is_null(std::json::find_object_field(fixture, "exception"))
}

fn expected_exception_message(fixture: Value) -> str {
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

fn expected_commands(fixture: Value) -> Vec<&str> {
    let commands = std::json::find_object_field(fixture, "commands");
    match commands {
        Value::Null => Vec::new(),
        Value::Array(values) => {
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

fn command_fragment(value: Value) -> str {
    match value {
        Value::Null => "",
        Value::String(text) => text,
        Value::Object(_) => {
            let masked = std::json::find_object_field(value, "masked");
            if !std::json::is_null(masked) {
                return std::json::get_string(masked);
            }
            std::json::get_string(std::json::find_object_field(value, "raw"))
        }
        Value::Array(values) => {
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
