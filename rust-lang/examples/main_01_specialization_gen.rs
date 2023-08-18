fn print(i: i64) {
    println!("{}", i)
}
fn main() {
    print({
        let i = 1i64;
        i + 1i64 + 2i64 + 3i64
    });
}

// stdout: 7i64
// result: ()
