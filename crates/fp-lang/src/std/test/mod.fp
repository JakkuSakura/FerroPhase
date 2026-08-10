
struct TestCase {
    name: str,
    run: fn(),
}

pub struct CommandMock {
    pattern: str,
    stdout: str,
    stderr: str,
    status: i64,
}

const mut REGISTRY: ::std::alloc::Vec<TestCase> = ::std::alloc::Vec::new();
// TODO: use quote<fn> instead
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
    let tests: ::std::alloc::Vec<TestCase> = REGISTRY;
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

pub fn reset_command_mocks() {
    ::std::intrinsics::test::command_mock_reset();
}

pub fn mock_command(
    pattern: &str,
    stdout: &str = "",
    stderr: &str = "",
    status: i64 = 0,
) {
    ::std::intrinsics::test::command_mock_push(pattern, stdout, stderr, status);
}

pub fn take_command_calls() -> ::std::alloc::Vec<&str> {
    ::std::intrinsics::test::command_mock_take_calls()
}

pub fn apply_command_mock(
    command: &str,
) -> ::std::option::Option<::std::intrinsics::test::CommandMockMatch> {
    ::std::intrinsics::test::command_mock_apply(command)
}
