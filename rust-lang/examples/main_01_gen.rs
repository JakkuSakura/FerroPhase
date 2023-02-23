fn print(i: i64) -> () {
    println!("{}", i)
}
fn main() -> () {
    (print((inc_0())));
}
pub fn inc_0() -> i64 {
    (((1i64 + 1i64) + 2i64) + 3i64)
}

// stdout: 7i64
// result: ()
