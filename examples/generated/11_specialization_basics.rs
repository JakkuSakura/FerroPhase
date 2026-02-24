pub fn add(a: i64, b: i64) -> i64 {
    a + b
}
pub fn double(x: i64) -> i64 {
    x * 2
}
pub fn compose(x: i64) -> i64 {
    double(add(x, 1))
}
pub fn main() -> () {
    println!("{}", add(2, 3));
    println!("{}", double(5));
    println!("{}", compose(10));
    const RESULT: i64 = 30;
    println!("const: {}", RESULT);
}
