// Minimal semantic fixture: closures and capture.
// Expected output:
// apply: 15
// add_offset: 17
// double: 14

fn apply(a: i64, b: i64, op: fn(i64, i64) -> i64) -> i64 {
    op(a, b)
}

fn add(a: i64, b: i64) -> i64 {
    a + b
}

fn main() {
    let applied = apply(7, 8, add);
    println!("apply: {}", applied);

    let offset = 10;
    let add_offset = |x| x + offset;
    println!("add_offset: {}", add_offset(7));

    let double = |x| x * 2;
    println!("double: {}", double(7));
}
