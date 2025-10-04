#!/usr/bin/env fp run
//! Function specialization and inlining

fn add(a: i64, b: i64) -> i64 {
    a + b
}

fn double(x: i64) -> i64 {
    x * 2
}

fn compose(x: i64) -> i64 {
    double(add(x, 1))
}

fn main() {
    // Simple calls (should inline)
    println!("{}", add(2, 3));
    println!("{}", double(5));

    // Composition (should inline entire chain)
    println!("{}", compose(10)); // (10 + 1) * 2 = 22

    // Const evaluation (compile-time)
    const RESULT: i64 = {
        let x = 5;
        let y = x + 10; // add inlined
        y * 2           // double inlined
    };
    println!("const: {}", RESULT);
}
