// Minimal semantic fixture: expressions.
// Expected output:
// exprs: a=7, b=3, c=true
// concat: fplang

fn main() {
    let a = 1 + 2 * 3;
    let b = (10 - 4) / 2;
    let c = a == 7 && b == 3;
    println!("exprs: a={}, b={}, c={}", a, b, c);

    let s = "fp";
    let t = "lang";
    println!("concat: {}{}", s, t);
}
