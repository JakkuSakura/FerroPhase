// Demonstrates #[test] as a const-driven decorator that registers a test item.
// The std::test module is expected to provide the registration API via const eval.

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
