// Minimal semantic fixture: functions and return values.
// Expected output:
// add: 42
// square: 49
// sum_to: 10

fn add(a: i64, b: i64) -> i64 {
    a + b
}

fn square(n: i64) -> i64 {
    n * n
}

fn sum_to(n: i64) -> i64 {
    let mut acc = 0;
    let mut i = 1;
    while i <= n {
        acc += i;
        i += 1;
    }
    acc
}

fn main() {
    println!("add: {}", add(40, 2));
    println!("square: {}", square(7));
    println!("sum_to: {}", sum_to(4));
}
