use std::fs;
use std::path::Path;
use std::process::Command;
use fptest::config::Config;
use fptest::discovery::TestFile;
use fptest::markers;
use fptest::parse;

pub struct FileRun {
    path: str,
    markers: markers::MarkerSet,
    status: i64,
    output: str,
    parsed: Vec<parse::ParsedLine>,
    skipped: bool,
    skip_reason: str,
}

impl FileRun {
    pub fn path(&self) -> &str {
        &self.path
    }

    pub fn markers(&self) -> &markers::MarkerSet {
        &self.markers
    }

    pub fn status(&self) -> i64 {
        self.status
    }

    pub fn output(&self) -> &str {
        &self.output
    }

    pub fn parsed(&self) -> &Vec<parse::ParsedLine> {
        &self.parsed
    }

    pub fn skipped(&self) -> bool {
        self.skipped
    }

    pub fn skip_reason(&self) -> &str {
        &self.skip_reason
    }
}

fn quote_shell_arg(value: &str) -> str {
    let escaped = value.replace("'", "'\"'\"'");
    f"'{escaped}'"
}

fn output_path(index: i64) -> str {
    f".fptest-output-{index}.log"
}

fn read_output(path: &str) -> str {
    let path_obj = Path::new(path);
    if fs::exists(path_obj) {
        fs::read_to_string(path_obj)
    } else {
        ""
    }
}

fn cleanup_output(path: &str) {
    let path_obj = Path::new(path);
    if fs::exists(path_obj) {
        fs::remove_file(path_obj);
    }
}

pub fn skipped_file(file: &TestFile, reason: str) -> FileRun {
    FileRun {
        path: file.path,
        markers: file.markers,
        status: 0,
        output: "",
        parsed: Vec::new(),
        skipped: true,
        skip_reason: reason,
    }
}

pub fn run_file(_config: &Config, file: &TestFile, index: i64) -> FileRun {
    let out_path = output_path(index);
    let command = f"fp interpret {quote_shell_arg(file.path)} > {quote_shell_arg(&out_path)} 2>&1";
    let result = Command::shell(command).output_result();
    let output = read_output(&out_path);
    cleanup_output(&out_path);
    let parsed = parse::parse_test_output(&output);

    FileRun {
        path: file.path,
        markers: file.markers,
        status: result.status(),
        output,
        parsed,
        skipped: false,
        skip_reason: "",
    }
}
