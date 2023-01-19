fn inc(i: i64) -> i64 {
    i + 1
}
fn print(i: i64) {
    println!("{}", i)
}
fn main() {
    print(inc(1));
    print(inc(2));
}
