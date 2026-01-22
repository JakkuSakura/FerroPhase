#!/usr/bin/env fp run
//! Named arguments and call-site reordering for functions.

fn summarize(label: str, count: i64, active: bool) -> str {
    format!("label={} count={} active={}", label, count, active)
}

fn add(a: i64, b: i64) -> i64 {
    a + b
}

const fn main() {
    println!("Tutorial: 33_named_args.fp");
    println!("Focus: Named arguments in function calls");
    println!("Expectation: keyword arguments can be reordered");
    println!("");

    let first = summarize(label="alpha", count=3, active=true);
    println!("first: {}", first);

    let second = summarize(count=7, active=false, label="beta");
    println!("second: {}", second);

    let total = add(b=2, a=5);
    println!("add: {}", total);
}
