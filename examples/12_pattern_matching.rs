#!/usr/bin/env fp run
//! Pattern matching: match expressions with guards and destructuring

enum Color {
    Red,
    Green,
    Rgb(u8, u8, u8),
}

enum Option<T> {
    Some(T),
    None,
}

fn describe(color: &Color) -> &'static str {
    match color {
        Color::Red => "red",
        Color::Green => "green",
        Color::Rgb(255, 0, 0) => "red rgb",
        Color::Rgb(r, g, b) => "custom rgb",
    }
}

fn classify(n: i64) -> &'static str {
    match n {
        0 => "zero",
        n if n < 0 => "negative",
        n if n % 2 == 0 => "even",
        _ => "odd",
    }
}

fn unwrap_or(opt: Option<i64>, default: i64) -> i64 {
    match opt {
        Option::Some(v) => v,
        Option::None => default,
    }
}

fn main() {
    // Enum patterns
    let red = Color::Red;
    let rgb = Color::Rgb(128, 64, 32);
    println!("{}", describe(&red));
    println!("{}", describe(&rgb));

    // Guards
    println!("{}", classify(-5));
    println!("{}", classify(0));
    println!("{}", classify(4));
    println!("{}", classify(7));

    // Option matching
    println!("{}", unwrap_or(Option::Some(42), 0));
    println!("{}", unwrap_or(Option::None, 99));

    // Const match
    const CODE: i64 = match 1 {
        0 => 0xFF0000,
        1 => 0x00FF00,
        _ => 0x000000,
    };
    println!("0x{:06X}", CODE);
}