use std::fmt::Display;
use std::ops::Add;
fn add<T: Add>(a: T, b: T) -> T {
    a + b
}
fn print(i: impl Display) {
    println!("{}", i)
}
fn main() {
    let a = 1i64;
    let c = 3f64;
    let b = 2i64;
    let op = add;
    let d = 4f64;
    {
        let a = 1i64;
        let c = 3f64;
        let b = 2i64;
        let op = add;
        let d = 4f64;
        let i = op(a, b);
        print(i)
    };
    {
        let a = 1i64;
        let c = 3f64;
        let b = 2i64;
        let op = add;
        let d = 4f64;
        let i = op(c, d);
        print(i)
    };
}
pub fn do_op_2() {
    let a = 1i64;
    let c = 3f64;
    let b = 2i64;
    let op = add;
    let d = 4f64;
    {
        let a = 1i64;
        let c = 3f64;
        let b = 2i64;
        let op = add;
        let d = 4f64;
        let i = op(a, b);
        print(i)
    };
    {
        let a = 1i64;
        let c = 3f64;
        let b = 2i64;
        let op = add;
        let d = 4f64;
        let i = op(c, d);
        print(i)
    };
}
pub fn print2_0() {
    let a = 1i64;
    let c = 3f64;
    let b = 2i64;
    let op = add;
    let d = 4f64;
    let i = op(a, b);
    print(i)
}
pub fn print2_1() {
    let a = 1i64;
    let c = 3f64;
    let b = 2i64;
    let op = add;
    let d = 4f64;
    let i = op(c, d);
    print(i)
}

// stdout: 3i64
// stdout: 7f64
// result: ()
