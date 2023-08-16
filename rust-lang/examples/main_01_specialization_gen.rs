fn print(i: i64) {
    println!("{}", i)
}
fn main() {
    print(inc_0())
}
pub fn inc_0() -> i64 {
    7i64
}

// stdout: 7i64
// result: ()
