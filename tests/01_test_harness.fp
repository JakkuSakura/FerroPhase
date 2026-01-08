// Minimal test harness for fp's #[test] registration and assert macros.

fn main() {
    println!("Running test harness for 01_test_harness.fp");
    let report = std::test::run_tests();
    println!("Summary: {} passed, {} failed, {} total", report.passed, report.failed, report.total);
}

#[test]
fn arithmetic_addition() {
    let result = 40 + 2;
    assert_eq!(result, 42);
}

#[test]
fn boolean_logic() {
    let flag = true && !false;
    assert!(flag);
}
