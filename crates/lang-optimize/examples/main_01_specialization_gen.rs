fn inc(i: i64) -> i64 {
    i + 1 + 2 + 3
}
fn double(i: i64) -> i64 {
    i * 2
}
fn print(i: i64) {
    println!("{}", i)
}
fn main() {
    print(inc(1));
}

// result: ()
