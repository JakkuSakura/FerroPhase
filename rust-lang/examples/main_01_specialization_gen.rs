fn print(i: i64) {
    println!("{}", i)
}
fn main() {
    print(1 + 1 + 2 + 3);
}
pub fn inc_0() -> i64 {
    1 + 1 + 2 + 3
}

// stdout: 7
// result: ()
