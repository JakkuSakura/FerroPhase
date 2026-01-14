#!/usr/bin/env fp run
//! Parse JSON into a value and print it back.

use std::json;

fn main() {
    println!("Tutorial: 29_json_parse.fp");
    println!("Focus: Parse JSON into a value and print it back");
    println!("What to look for: printed JSON matches input");
    println!("Expectation: output JSON matches");
    println!("");

    let input = "{\"name\":\"Ferro\",\"active\":true,\"count\":3,\"tags\":[\"fast\",\"safe\"],\"meta\":null}";
    let value = json::parse(input);

    print("input  = ");
    print(input);
    println!();
    print("parsed = ");
    json::print(value);
    println!();
}
