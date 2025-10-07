#!/usr/bin/env fp run
//! Generics: type parameters and monomorphization

struct Pair<T, U> {
    first: T,
    second: U,
}

impl<T, U> Pair<T, U> {
    fn new(first: T, second: U) -> Self {
        Self { first, second }
    }
}

enum Option<T> {
    Some(T),
    None,
}

impl<T> Option<T> {
    fn unwrap_or(self, default: T) -> T {
        match self {
            Option::Some(v) => v,
            Option::None => default,
        }
    }
}

fn max<T: PartialOrd>(a: T, b: T) -> T {
    if a > b { a } else { b }
}

fn main() {
    // Generic struct
    let pair = Pair::new(42, "hello");
    println!("({}, {})", pair.first, pair.second);

    // Generic function (monomorphized)
    println!("max(10, 20) = {}", max(10, 20));
    println!("max(3.5, 2.1) = {}", max(3.5, 2.1));

    // Generic enum
    let some: Option<i64> = Option::Some(100);
    let none: Option<i64> = Option::None;
    println!("{}", some.unwrap_or(0));
    println!("{}", none.unwrap_or(99));
}