struct TestCase {
    name: str,
    run: fn(),
}

const mut REGISTRY: Vec<TestCase> = Vec::new();

const fn test(item: quote<item>) -> quote<item> {
    let name = item.name;
    REGISTRY.push(TestCase { name, run: item.value });
    item
}

struct TestReport {
    total: i64,
    passed: i64,
    failed: i64,
}

fn run_tests() -> TestReport {
    let tests: Vec<TestCase> = REGISTRY;
    let mut passed = 0;
    let mut failed = 0;
    let mut idx = 0;
    while idx < tests.len() {
        let test: TestCase = tests[idx];
        let ok = catch_unwind(test.run);
        if ok {
            passed = passed + 1;
            println("  {} ... ok", test.name);
        } else {
            failed = failed + 1;
            println("  {} ... FAILED", test.name);
        }
        idx = idx + 1;
    }
    let total = passed + failed;
    println(
        "test result: {} passed; {} failed; {} total",
        passed,
        failed,
        total
    );
    TestReport {
        total,
        passed,
        failed,
    }
}

fn run() -> TestReport {
    run_tests()
}
