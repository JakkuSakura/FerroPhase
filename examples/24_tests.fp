// Demonstrates #[test] as a const-driven decorator that registers a test item.
// The std::test module is expected to provide the registration API via const eval.

fn main() {
    println!("📘 Tutorial: 24_tests.fp");
    println!("🧭 Focus: minimal test harness using assert! macros");
    println!("🧪 What to look for: both tests pass with no assertion failures");
    println!("✅ Expectation: test report shows 2 passed");
    println!("");

    println!("Running tests via std::test::run:");
    let tests = vec![
        std::test::TestCase { name: "adds_two_numbers", run: adds_two_numbers },
        std::test::TestCase { name: "string_concat", run: string_concat },
    ];
    let report = std::test::run(tests);
    println!("Summary: {} passed, {} failed, {} total", report.passed, report.failed, report.total);
}

#[test]
fn adds_two_numbers() {
    let left = 1 + 2;
    let right = 3;
    assert_eq!(left, right);
}

#[test]
fn string_concat() {
    let msg = "hello, " + "world";
    assert_eq!(msg, "hello, world");
}
