fn print(i: i64) -> () {
    println!("{}", i)
}
fn main() -> () {
    print(inc_0());
}
fn inc_0() -> i64 {
    1i64 + 1i64
}

// stdout: 2i64
// result: ()
