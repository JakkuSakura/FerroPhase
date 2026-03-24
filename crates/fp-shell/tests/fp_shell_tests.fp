use std::assert;
use std::env;
use std::fs;
use std::json::{self, JsonValue};
use std::process::{Command, ProcessResult};
use std::yaml;

struct FixtureCase {
    path: str,
    family: str,
}

struct FixtureSummary {
    total: i64,
    passed: i64,
    failed: i64,
}

struct CompileCaseResult {
    process: ProcessResult,
    output_path: str,
}

fn main() {
    let repo_root = env::current_dir();
    let fp_bin = join_path(&repo_root, "target/debug/fp");
    let fp_shell_bin = join_path(&repo_root, "target/debug/fp-shell");
    let fixture_root = join_path(&repo_root, "crates/fp-shell/tests/fixtures/operations");

    require_binary(&fp_bin);
    require_binary(&fp_shell_bin);

    let summary = run_fixture_suite(&fixture_root, &fp_shell_bin);
    println(
        "fp-shell fixture cases: {} passed; {} failed; {} total",
        summary.passed,
        summary.failed,
        summary.total,
    );
    assert::eq_i64(summary.failed, 0);
}

fn require_binary(path: &str) {
    let result = Command::new(path).arg("--help").output_result();
    if !result.success() {
        panic(f"required binary is not runnable: {path}");
    }
}

fn run_fixture_suite(fixture_root: &str, fp_shell_bin: &str) -> FixtureSummary {
    let families = fs::read_dir(fixture_root);
    let mut passed = 0;
    let mut failed = 0;
    let mut total = 0;
    let mut family_idx = 0;

    while family_idx < families.len() {
        let family_dir = families[family_idx];
        let family = family_dir.replace(f"{fixture_root}/", "");
        let entries = fs::read_dir(family_dir);
        let mut entry_idx = 0;

        while entry_idx < entries.len() {
            let entry = entries[entry_idx];
            if is_fixture_path(entry) {
                total = total + 1;
                let case = FixtureCase {
                    path: entry,
                    family,
                };

                if run_case(case, fp_shell_bin) {
                    passed = passed + 1;
                } else {
                    failed = failed + 1;
                }
            }
            entry_idx = entry_idx + 1;
        }

        family_idx = family_idx + 1;
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

fn run_case(case: FixtureCase, fp_shell_bin: &str) -> bool {
    let fixture = load_fixture(case.path);
    let source = build_case_source(&case, fixture);
    let compile = compile_case(&case, fixture, &source, fp_shell_bin);
    let expects_failure = expects_exception(fixture);

    if expects_failure {
        if compile.process.success() {
            println("{} ... FAILED", case.path);
            println("  expected compile failure but compilation succeeded");
            return false;
        }

        let message = expected_exception_message(fixture);
        if message != "" && !compile.process.stderr().contains(&message) {
            println("{} ... FAILED", case.path);
            println("  missing exception fragment: {}", message);
            println("  stderr: {}", compile.process.stderr());
            return false;
        }

        println("{} ... ok", case.path);
        return true;
    }

    if !compile.process.success() {
        println("{} ... FAILED", case.path);
        println("  compile stderr: {}", compile.process.stderr());
        return false;
    }

    let rendered = fs::read_to_string(compile.output_path);
    let expected = expected_commands(fixture);
    let mut idx = 0;

    while idx < expected.len() {
        let fragment = expected[idx];
        if !rendered.contains(fragment) {
            println("{} ... FAILED", case.path);
            println("  missing command fragment: {}", fragment);
            return false;
        }
        idx = idx + 1;
    }

    println("{} ... ok", case.path);
    true
}

fn load_fixture(path: &str) -> JsonValue {
    let source = fs::read_to_string(path);
    yaml::parse(&source)
}

fn build_case_source(case: &FixtureCase, fixture: JsonValue) -> str {
    let operation = operation_path(case.family);
    let args = render_args(json::find_object_field(fixture, "args"));
    let kwargs = render_kwargs(json::find_object_field(fixture, "kwargs"));
    let call = render_call(&operation, &args, &kwargs);
    f"const fn main() {{\n    {call};\n}}\n"
}

fn operation_path(family: &str) -> str {
    let rendered = family.replace(".", "::");
    f"std::ops::{rendered}"
}

fn render_call(operation: &str, args: &str, kwargs: &str) -> str {
    if args == "" {
        if kwargs == "" {
            return f"{operation}()";
        }
        return f"{operation}({kwargs})";
    }

    if kwargs == "" {
        return f"{operation}({args})";
    }

    f"{operation}({args}, {kwargs})"
}

fn render_args(value: JsonValue) -> str {
    match value {
        JsonValue::Null => "",
        JsonValue::Array(values) => {
            let mut rendered = Vec::new();
            let mut idx = 0;
            while idx < values.len() {
                rendered.push(render_value(values[idx]));
                idx = idx + 1;
            }
            rendered.join(", ")
        }
        _ => panic("expected args array"),
    }
}

fn render_kwargs(value: JsonValue) -> str {
    match value {
        JsonValue::Null => "",
        JsonValue::Object(fields) => {
            let mut rendered = Vec::new();
            let mut idx = 0;
            while idx < fields.len() {
                let field = fields[idx];
                rendered.push(f"{field.key}={render_value(field.value)}");
                idx = idx + 1;
            }
            rendered.join(", ")
        }
        _ => panic("expected kwargs object"),
    }
}

fn render_value(value: JsonValue) -> str {
    match value {
        JsonValue::Null => "null",
        JsonValue::Bool(true) => "true",
        JsonValue::Bool(false) => "false",
        JsonValue::Number(number) => number,
        JsonValue::String(text) => fp_quote(text),
        JsonValue::Array(values) => {
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

fn compile_case(case: &FixtureCase, fixture: JsonValue, source: &str, fp_shell_bin: &str) -> CompileCaseResult {
    let workspace = case_workspace(&case);
    reset_workspace(&workspace);
    materialize_fixture_workspace(&workspace, fixture);

    let source_path = join_path(&workspace, "case.fp");
    let output_path = join_path(&workspace, "case.sh");
    fs::write_string(&source_path, source);

    let mut command = Command::new(fp_shell_bin)
        .arg("compile")
        .arg(&source_path)
        .arg("--output")
        .arg(&output_path)
        .current_dir(&workspace);

    let inventory = json::find_object_field(fixture, "inventory");
    if !json::is_null(inventory) {
        let inventory_path = join_path(&workspace, json::get_string(inventory));
        command = command.arg("--inventory").arg(&inventory_path);
    }

    CompileCaseResult {
        process: command.output_result(),
        output_path,
    }
}

fn case_workspace(case: &FixtureCase) -> str {
    let slug = case
        .path
        .replace("/", "__")
        .replace(" ", "_")
        .replace(".", "_");
    join_path("/tmp/fp-shell-fixtures", &slug)
}

fn reset_workspace(workspace: &str) {
    if fs::exists(workspace) {
        fs::remove_dir_all(workspace);
    }
    fs::create_dir_all(workspace);
}

fn materialize_fixture_workspace(workspace: &str, fixture: JsonValue) {
    materialize_local_files(workspace, json::find_object_field(fixture, "local_files"));
    materialize_directories(workspace, json::find_object_field(fixture, "directories"));
}

fn materialize_local_files(workspace: &str, value: JsonValue) {
    match value {
        JsonValue::Null => {}
        JsonValue::Object(_) => {
            materialize_file_map(workspace, json::find_object_field(value, "files"));
            materialize_dir_map(workspace, json::find_object_field(value, "dirs"));
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
                fs::create_dir_all(&dir_path);
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
                    JsonValue::Null => fs::write_string(&file_path, ""),
                    JsonValue::String(content) => fs::write_string(&file_path, content),
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
                fs::create_dir_all(&dir_path);
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
    !json::is_null(json::find_object_field(fixture, "exception"))
}

fn expected_exception_message(fixture: JsonValue) -> str {
    let exception = json::find_object_field(fixture, "exception");
    if json::is_null(exception) {
        return "";
    }

    let message = json::find_object_field(exception, "message");
    if json::is_null(message) {
        return "";
    }

    json::get_string(message)
}

fn expected_commands(fixture: JsonValue) -> Vec<&str> {
    let commands = json::find_object_field(fixture, "commands");
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
            let masked = json::find_object_field(value, "masked");
            if !json::is_null(masked) {
                return json::get_string(masked);
            }
            json::get_string(json::find_object_field(value, "raw"))
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
