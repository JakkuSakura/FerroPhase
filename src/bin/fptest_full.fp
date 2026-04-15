use crate::fptest::config;
use crate::fptest::discovery;
use crate::fptest::markers;
use crate::fptest::markers::Expectation;
use crate::fptest::report;
use crate::fptest::runner;

fn main() {
    let config = config::from_env();
    let files = discovery::discover(&config);
    if files.len() == 0 {
        println("fptest: no tests collected");
        return;
    }

    let mut summary = report::Summary::new();
    let mut idx = 0;
    while idx < files.len() {
        let file = files[idx];
        let file_expectation = markers::file_expectation(&file.markers);
        let file_result = match file_expectation {
            Expectation::Skip => runner::skipped_file(&file, markers::file_reason(&file.markers)),
            _ => runner::run_file(&config, &file, idx),
        };
        let stop = summary.record(file_result, &config);
        if stop {
            break;
        }
        idx = idx + 1;
    }

    summary.finalize();
    if summary.should_fail() {
        panic("fptest failed");
    }
}
