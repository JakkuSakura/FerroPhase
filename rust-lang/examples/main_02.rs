fn do_op(a: i64, b: i64, op: fn(i64, i64) -> i64) -> i64 {
    op(a, b)
}
fn add(a: i64, b: i64) -> i64 {
    a + b
}
fn print(i: i64) {
    println!("{}", i)
}
fn main() {
    print(do_op(1, 2, add));
    print(do_op(10, 20, add));
}
