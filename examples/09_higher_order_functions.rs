pub use std::fmt::Display;
pub use std::ops::Add;
pub fn apply_if(cond: bool, a: i64, b: i64, op: fn(i64, i64) -> i64) -> i64 {
    if cond {
        op(a, b)
    } else {
        0
    }
}
pub fn make_adder(n: i64) -> impl Fn(i64) -> i64 {
    move |x| x + n
}
pub fn main() -> () {
    println!("Generic operations:");
    apply__spec0(10, 20, add__spec0);
    apply__spec1(1.5, 2.5, add__spec1);
    println!("\nConditional:");
    println!("{}", apply_if(true, 5, 3, add__spec0));
    println!("{}", apply_if(false, 5, 3, add__spec0));
    println!("\nClosure factory:");
    let add_10 = make_adder(10);
    println!("add_10(5) = {}", add_10(5));
    let double = move |x| x * 2;
    println!("double(7) = {}", double(7));
}
pub fn apply__spec0(a: i64, b: i64, op: fn(i64, i64) -> i64) -> () {
    println!("{}", op(a, b));
}
pub fn add__spec0(a: i64, b: i64) -> i64 {
    a + b
}
pub fn apply__spec1(a: f64, b: f64, op: fn(f64, f64) -> f64) -> () {
    println!("{}", op(a, b));
}
pub fn add__spec1(a: f64, b: f64) -> f64 {
    a + b
}
