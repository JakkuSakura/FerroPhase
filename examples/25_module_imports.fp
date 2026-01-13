#!/usr/bin/env fp run
//! Module definitions and `use` imports within a single file

mod helpers {
    pub const GREETING: &str = "Hello";

    pub fn greet(name: &str) {
        println!("{} {}!", GREETING, name);
    }

    pub mod math {
        pub fn add(a: i64, b: i64) -> i64 {
            a + b
        }
    }
}

mod modules;

use helpers;
use helpers::greet as say_hi;
use helpers::math::add;
use modules::helpers::greet_from_file as file_greet;
use modules::helpers::math::add as file_add;
use std::fmt::Display;

fn echo<T: Display>(value: T) {
    println!("echo: {}", value);
}

const fn main() {
    println!("📦 Example: 25_module_imports.fp");
    println!("🔧 Focus: inline modules + external file modules + std imports");
    println!("🔍 Expect: greetings + math output using imported names");
    println!("");

    helpers::greet("Ferro");
    say_hi("module imports");
    file_greet("external module");
    echo(2025);

    let sum = add(8, 34);
    println!("math.add(8, 34) = {}", sum);
    let file_sum = file_add(10, 32);
    println!("file_math.add(10, 32) = {}", file_sum);
}
