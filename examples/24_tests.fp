// Demonstrates #[test] as a const-driven decorator that registers a test item.
// The std::test module is expected to provide the registration API via const eval.

fn main() {
    println!("📘 Tutorial: 24_tests.fp");
    println!("🧭 Focus: minimal test registration example plus manual execution");
    println!("🧪 What to look for: assert() should not trigger");
    println!("✅ Expectation: both tests run without printing assertion failure");
    println!("");

    println!("Running tests manually:");
    adds_two_numbers();
    println!("  adds_two_numbers ... ok");
    string_concat();
    println!("  string_concat ... ok");
}

#[test]
fn adds_two_numbers() {
    let left = 1 + 2;
    let right = 3;
    assert(left == right);
}

#[test]
fn string_concat() {
    let msg = "hello, " + "world";
    assert(msg == "hello, world");
}
