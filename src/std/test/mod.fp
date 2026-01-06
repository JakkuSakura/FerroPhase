mod std {
    mod test {
        struct TestCase {
            name: str,
            run: fn(),
        }

        struct TestReport {
            total: i64,
            passed: i64,
            failed: i64,
        }

        fn run(tests: Vec<TestCase>) -> TestReport {
            let mut passed = 0;
            let mut failed = 0;
            let mut idx = 0;
            while idx < tests.len() {
                let test = tests[idx];
                let ok = catch_unwind(test.run);
                if ok {
                    passed = passed + 1;
                    println!("  {} ... ok", test.name);
                } else {
                    failed = failed + 1;
                    println!("  {} ... FAILED", test.name);
                }
                idx = idx + 1;
            }
            let total = passed + failed;
            println!(
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
    }
}
