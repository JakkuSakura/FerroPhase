// Const-eval coverage for basic arithmetic and string operations.

const GREETING: &str = "Hello" + ", " + "tests";
const ANSWER: i64 = 6 * 7;

fn main() {
    println!("Running test harness for 02_const_eval.fp");
    let report = std::test::run_tests();
    println!("Summary: {} passed, {} failed, {} total", report.passed, report.failed, report.total);
}

#[test]
fn const_string_concat() {
    assert_eq!(GREETING, "Hello, tests");
}

#[test]
fn const_numeric() {
    assert_eq!(ANSWER, 42);
}
