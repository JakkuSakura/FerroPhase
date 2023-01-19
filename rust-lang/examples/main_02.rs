use std::ops::Add;

fn do_op(a: i64, b: i64, c: f64, d: f64, op: fn(i64, i64) -> i64) {
    print(op(a, b));
    print(op(c, d));
}
fn add<T: Add>(a: T, b: T) -> T {
    a + b
}
fn print(i: i64) {
    println!("{}", i)
}
fn main() {
    do_op(1, 2, 3.0, 4.0, add);
}
