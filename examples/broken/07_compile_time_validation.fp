#!/usr/bin/env fp interpret
//! Compile-time validation using const expressions and introspection

fn main() {
    println!("📘 Tutorial: 07_compile_time_validation.fp");
    println!("🧭 Focus: Compile-time validation using const expressions and introspection");
    println!("🧪 What to look for: labeled outputs below");
    println!("✅ Expectation: outputs match labels");
    println!("");
    struct Data {
        a: i64,
        b: i64,
        c: [u8; 16],
    }

    struct Header {
        magic: i64,
        version: u8,
        flags: u8,
        pad: [u8; 6],
    }

    // Introspection macros
    const DATA_SIZE: u64 = sizeof!(Data);
    const DATA_FIELDS: i64 = field_count!(Data);
    const DATA_HAS_A: bool = hasfield!(Data, "a");
    const DATA_HAS_X: bool = hasfield!(Data, "x");
    const HEADER_SIZE: u64 = sizeof!(Header);
    const HEADER_FIELDS: i64 = field_count!(Header);
    const HEADER_HAS_VERSION: bool = hasfield!(Header, "version");

    println!("data: sizeof={}, fields={}", DATA_SIZE, DATA_FIELDS);
    println!("data: has_a={}, has_x={}", DATA_HAS_A, DATA_HAS_X);
    println!(
        "header: sizeof={}, fields={}, has_version={}",
        HEADER_SIZE, HEADER_FIELDS, HEADER_HAS_VERSION
    );

    // Type queries at compile time
    const DATA_TYPE_NAME: &str = type(Data).name;
    const DATA_FIELD_A_TYPE: &str = type(Data).field_type("a").name;
    const HEADER_FIELD_VERSION_TYPE: &str = type(Header).field_type("version").name;
    const HAS_TO_STRING: bool = hasmethod!(Data, "to_string");

    println!(
        "types: data='{}' a='{}' version='{}'",
        DATA_TYPE_NAME, DATA_FIELD_A_TYPE, HEADER_FIELD_VERSION_TYPE
    );
    println!("data has to_string: {}", HAS_TO_STRING);

    // Compile-time layout checks
    const MAX_SIZE: u64 = 64;
    const DATA_OK: bool = DATA_SIZE <= MAX_SIZE;
    const HEADER_OK: bool = HEADER_SIZE <= MAX_SIZE;
    const TOTAL_SIZE: u64 = DATA_SIZE + HEADER_SIZE;
    const TOTAL_OK: bool = TOTAL_SIZE <= 96;

    println!(
        "layout: data_ok={}, header_ok={}, total_ok={}, total_size={}",
        DATA_OK, HEADER_OK, TOTAL_OK, TOTAL_SIZE
    );

    // Emit compile-time warnings when validation fails.
    const _WARN: () = const {
        if !DATA_OK {
            compile_warning!("Data is larger than MAX_SIZE");
        }
        if !HEADER_OK {
            compile_warning!("Header is larger than MAX_SIZE");
        }
        if !TOTAL_OK {
            compile_warning!("Combined size exceeds limit");
        }
    };
}
