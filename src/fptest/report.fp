use std::io;
use fptest::config::Config;
use fptest::markers;
use fptest::markers::Expectation;
use fptest::runner::FileRun;

pub enum TestStatus {
    Passed,
    Failed,
    Skipped,
    Xfailed,
    Xpassed,
    Error,
}

pub struct TestResult {
    name: str,
    status: TestStatus,
    reason: str,
}

pub struct FileReport {
    path: str,
    tests: Vec<TestResult>,
    status: i64,
    parse_error: bool,
}

pub struct Summary {
    files: Vec<FileReport>,
    passed: i64,
    failed: i64,
    skipped: i64,
    xfailed: i64,
    xpassed: i64,
    errors: i64,
    total: i64,
    stopped: bool,
}

impl Summary {
    pub fn new() -> Summary {
        Summary {
            files: Vec::new(),
            passed: 0,
            failed: 0,
            skipped: 0,
            xfailed: 0,
            xpassed: 0,
            errors: 0,
            total: 0,
            stopped: false,
        }
    }

    pub fn stopped(&self) -> bool {
        self.stopped
    }

    pub fn should_fail(&self) -> bool {
        (self.failed + self.xpassed + self.errors) > 0
    }

    pub fn record(&mut self, file: FileRun, config: &Config) -> bool {
        let output = file.output();
        let report = build_file_report(&file);
        self.update_counts(&report);

        if !config.quiet() {
            print_file_report(&report);
        }

        if (config.verbose() || !config.capture()) && output != "" {
            println("  ---- output ----");
            io::write_stdout(output);
            if !output.ends_with("\n") {
                println("");
            }
        }

        self.files.push(report);

        let failures = self.failed + self.xpassed + self.errors;
        if config.fail_fast() && failures > 0 {
            self.stopped = true;
            return true;
        }
        if config.max_fail() > 0 && failures >= config.max_fail() {
            self.stopped = true;
            return true;
        }
        false
    }

    pub fn finalize(&self) {
        println(
            "fptest result: {} passed; {} failed; {} skipped; {} xfailed; {} xpassed; {} errors; {} total",
            self.passed,
            self.failed,
            self.skipped,
            self.xfailed,
            self.xpassed,
            self.errors,
            self.total,
        );
        if self.stopped {
            println("fptest stopped early due to failure threshold");
        }
    }

    fn update_counts(&mut self, report: &FileReport) {
        let mut idx = 0;
        while idx < report.tests.len() {
            let test = report.tests[idx];
            match test.status {
                TestStatus::Passed => self.passed = self.passed + 1,
                TestStatus::Failed => self.failed = self.failed + 1,
                TestStatus::Skipped => self.skipped = self.skipped + 1,
                TestStatus::Xfailed => self.xfailed = self.xfailed + 1,
                TestStatus::Xpassed => self.xpassed = self.xpassed + 1,
                TestStatus::Error => self.errors = self.errors + 1,
            }
            idx = idx + 1;
        }
        self.total = self.passed + self.failed + self.skipped + self.xfailed + self.xpassed + self.errors;
    }
}

fn build_file_report(file: &FileRun) -> FileReport {
    if file.skipped() {
        let mut tests: Vec<TestResult> = Vec::new();
        tests.push(TestResult {
            name: "fptest",
            status: TestStatus::Skipped,
            reason: f"{file.skip_reason()}",
        });
        return FileReport {
            path: f"{file.path()}",
            tests,
            status: 0,
            parse_error: false,
        };
    }

    let mut tests: Vec<TestResult> = Vec::new();
    let mut idx = 0;
    while idx < file.parsed().len() {
        let entry = file.parsed()[idx];
        let resolved = resolve_expectation(file.markers(), entry.name);
        let expectation = resolved.expectation;
        let reason = resolved.reason;
        let status = match expectation {
            Expectation::Skip => TestStatus::Skipped,
            Expectation::Xfail => match entry.ok {
                true => TestStatus::Xpassed,
                false => TestStatus::Xfailed,
            },
            Expectation::Normal => match entry.ok {
                true => TestStatus::Passed,
                false => TestStatus::Failed,
            },
        };
        tests.push(TestResult {
            name: entry.name,
            status,
            reason,
        });
        idx = idx + 1;
    }

    let parse_error = file.parsed().len() == 0;
    if (file.status() != 0 || parse_error) && !all_skipped(&tests) {
        let reason = match file.status() != 0 {
            true => f"exit status {file.status()}",
            false => "no test output",
        };
        tests.push(TestResult {
            name: "fptest",
            status: TestStatus::Error,
            reason,
        });
    }

    FileReport {
        path: f"{file.path()}",
        tests,
        status: file.status(),
        parse_error,
    }
}

struct ResolvedExpectation {
    expectation: Expectation,
    reason: str,
}

fn all_skipped(tests: &Vec<TestResult>) -> bool {
    if tests.len() == 0 {
        return false;
    }
    let mut idx = 0;
    while idx < tests.len() {
        let test = tests[idx];
        match test.status {
            TestStatus::Skipped => {}
            _ => return false,
        }
        idx = idx + 1;
    }
    true
}

fn resolve_expectation(markers_ref: &markers::MarkerSet, name: &str) -> ResolvedExpectation {
    let file_expectation = markers::file_expectation(markers_ref);
    if file_expectation == Expectation::Skip {
        return ResolvedExpectation {
            expectation: Expectation::Skip,
            reason: markers::file_reason(markers_ref),
        };
    }
    let named = markers::find_named_expectation(markers_ref, name);
    if named == Expectation::Skip {
        return ResolvedExpectation {
            expectation: named,
            reason: markers::find_named_reason(markers_ref, name),
        };
    }
    if file_expectation == Expectation::Xfail {
        return ResolvedExpectation {
            expectation: Expectation::Xfail,
            reason: markers::file_reason(markers_ref),
        };
    }
    match named {
        Expectation::Xfail => {
            return ResolvedExpectation {
                expectation: named,
                reason: markers::find_named_reason(markers_ref, name),
            };
        }
        _ => {}
    };
    ResolvedExpectation {
        expectation: Expectation::Normal,
        reason: "",
    }
}

fn print_file_report(file: &FileReport) {
    println(f"== {file.path} ==");
    let mut idx = 0;
    while idx < file.tests.len() {
        let test = file.tests[idx];
        println(render_test_line(&test));
        idx = idx + 1;
    }
}

fn render_test_line(test: &TestResult) -> str {
    let status = match test.status {
        TestStatus::Passed => "ok",
        TestStatus::Failed => "FAILED",
        TestStatus::Skipped => "SKIPPED",
        TestStatus::Xfailed => "XFAIL",
        TestStatus::Xpassed => "XPASS",
        TestStatus::Error => "ERROR",
    };
    if test.reason != "" {
        f"  {test.name} ... {status} ({test.reason})"
    } else {
        f"  {test.name} ... {status}"
    }
}
