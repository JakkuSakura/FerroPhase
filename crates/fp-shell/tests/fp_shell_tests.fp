use std::fs;
use std::json;
use std::process;
use std::yaml;

const FP_BIN: &str = "./target/debug/fp";
const FP_SHELL_BIN: &str = "./target/debug/fp-shell";
const FIXTURE_ROOT: &str = "crates/fp-shell/tests/fixtures/operations";
const TEMP_SOURCE: &str = "/tmp/fp-shell-fixture-case.fp";
const TEMP_OUTPUT: &str = "/tmp/fp-shell-fixture-case.sh";

struct FixtureCase {
    path: &str,
    family: &str,
}

struct FixtureSummary {
    total: i64,
    passed: i64,
    failed: i64,
}

fn fail() {
    process::run("exit 1")
}

fn main() {
    require_binary(FP_BIN);
    require_binary(FP_SHELL_BIN);

    let report = run_fixture_suite();
    print(
        f"fp-shell fixture cases: {report.passed} passed; {report.failed} failed; {report.total} total\n"
    );

    match report.failed == 0 {
        true => {}
        false => fail(),
    }
}

fn require_binary(path: &str) {
    match process::status(f"{path} --help >/dev/null") == 0 {
        true => {}
        false => fail(),
    }
}

fn run_fixture_suite() -> FixtureSummary {
    let mut cases = Vec::new();
    collect_cases(FIXTURE_ROOT, &mut cases);

    let mut passed = 0;
    let mut failed = 0;
    let mut idx = 0;

    while idx < cases.len() {
        let case = cases[idx];
        match run_case(case) {
            true => {
                passed = passed + 1;
                print(f"PASS {case.path}\n");
            }
            false => {
                failed = failed + 1;
            }
        }
        idx = idx + 1;
    }

    FixtureSummary {
        total: cases.len() as i64,
        passed,
        failed,
    }
}

fn collect_cases(path: &str, cases: &mut Vec<FixtureCase>) {
    let entries = fs::read_dir(path);
    let mut idx = 0;

    while idx < entries.len() {
        let entry = entries[idx];
        match fs::is_dir(entry) {
            true => collect_cases(entry, cases),
            false => {
                match is_fixture_path(entry) {
                    true => {
                        cases.push(FixtureCase {
                            path: entry,
                            family: family_from_path(entry),
                        });
                    }
                    false => {}
                }
            }
        }
        idx = idx + 1;
    }
}

fn is_fixture_path(path: &str) -> bool {
    path.ends_with(".json") || path.ends_with(".yaml") || path.ends_with(".yml")
}

fn run_case(case: FixtureCase) -> bool {
    let fixture = load_fixture(case.path);
    let source = build_case_source(case, fixture);
    let expected = expected_commands(fixture);

    fs::write_string(TEMP_SOURCE, source);

    match process::status(
        f"{FP_SHELL_BIN} compile {TEMP_SOURCE} --output {TEMP_OUTPUT} >/dev/null"
    ) == 0 {
        true => {}
        false => {
            print(f"FAIL {case.path} compile failed\n");
            return false;
        }
    }

    let rendered = fs::read_to_string(TEMP_OUTPUT);
    let mut idx = 0;
    while idx < expected.len() {
        let command = expected[idx];
        match rendered.contains(command) {
            true => {}
            false => {
                print(f"FAIL {case.path} missing command: {command}\n");
                return false;
            }
        }
        idx = idx + 1;
    }

    true
}

fn build_case_source(case: FixtureCase, fixture: json::JsonValue) -> str {
    let operation = operation_path(case.family);
    let args = render_args(json::find_object_field(fixture, "args"));
    let kwargs = render_kwargs(json::find_object_field(fixture, "kwargs"));

    let call = match args == "" {
        true => match kwargs == "" {
            true => f"{operation}()",
            false => f"{operation}({kwargs})",
        },
        false => match kwargs == "" {
            true => f"{operation}({args})",
            false => f"{operation}({args}, {kwargs})",
        },
    };

    f"const fn main() {{\n    {call};\n}}\n"
}

fn operation_path(family: &str) -> str {
    let pieces = family.split(".");
    let mut out = "std::ops";
    let mut idx = 0;

    while idx < pieces.len() {
        out = f"{out}::{pieces[idx]}";
        idx = idx + 1;
    }

    out
}

fn render_args(value: json::JsonValue) -> str {
    match value {
        json::JsonValue::Null => "",
        json::JsonValue::Array(values) => {
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

fn render_kwargs(value: json::JsonValue) -> str {
    match value {
        json::JsonValue::Null => "",
        json::JsonValue::Object(fields) => {
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

fn render_value(value: json::JsonValue) -> str {
    match value {
        json::JsonValue::Null => "null",
        json::JsonValue::Bool(flag) => {
            match flag {
                true => "true",
                false => "false",
            }
        }
        json::JsonValue::Number(number) => number,
        json::JsonValue::String(text) => fp_quote(text),
        json::JsonValue::Array(values) => render_array(values),
        json::JsonValue::Object(fields) => render_object(fields),
    }
}

fn render_array(values: Vec<json::JsonValue>) -> str {
    let mut rendered = Vec::new();
    let mut idx = 0;

    while idx < values.len() {
        rendered.push(render_value(values[idx]));
        idx = idx + 1;
    }

    f"[{rendered.join(", ")}]"
}

fn render_object(fields: Vec<json::JsonField>) -> str {
    let mut rendered = Vec::new();
    let mut idx = 0;

    while idx < fields.len() {
        let field = fields[idx];
        rendered.push(f"{fp_quote(field.key)}: {render_value(field.value)}");
        idx = idx + 1;
    }

    f"{{{rendered.join(", ")}}}"
}

fn expected_commands(fixture: json::JsonValue) -> Vec<&str> {
    match json::find_object_field(fixture, "commands") {
        json::JsonValue::Null => Vec::new(),
        json::JsonValue::Array(values) => {
            let mut commands = Vec::new();
            let mut idx = 0;

            while idx < values.len() {
                commands.push(json::get_string(values[idx]));
                idx = idx + 1;
            }

            commands
        }
        _ => panic("expected commands array"),
    }
}

fn load_fixture(path: &str) -> json::JsonValue {
    let source = fs::read_to_string(path);
    match path.ends_with(".json") {
        true => json::parse(source),
        false => yaml::parse(source),
    }
}

fn family_from_path(path: &str) -> &str {
    let parts = path.split("/");
    parts[parts.len() - 2]
}

fn fp_quote(value: &str) -> str {
    let escaped = value.replace("\\", "\\\\").replace("\"", "\\\"");
    f"\"{escaped}\""
}
