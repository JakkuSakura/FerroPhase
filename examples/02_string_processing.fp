#!/usr/bin/env fp run
//! Compile-time string operations and intrinsics (future feature showcase)

fn main() {
    // Const strings
    const NAME: &str = "FerroPhase";
    const VERSION: &str = "0.1.0";

    // String .len() intrinsic at compile time (TODO: implement in codegen)
    // const NAME_LEN: usize = NAME.len();
    // const VERSION_LEN: usize = VERSION.len();
    const NAME_LEN: usize = 10;  // Placeholder until .len() codegen is implemented
    const VERSION_LEN: usize = 5; // Placeholder until .len() codegen is implemented

    println!("name='{}' len={}", NAME, NAME_LEN);
    println!("version='{}' len={}", VERSION, VERSION_LEN);

    // String-based comparisons at compile time
    const IS_EMPTY: bool = NAME_LEN == 0;
    const IS_LONG: bool = NAME_LEN > 5;

    println!("empty={}, long={}", IS_EMPTY, IS_LONG);

    // Const string concatenation (future)
    // const BANNER: &str = NAME + " v" + VERSION;
    const BANNER: &str = "FerroPhase v0.1.0";
    println!("banner='{}'", BANNER);

    // Buffer sizing based on string length
    const BUFFER_SIZE: usize = if NAME_LEN > 8 { 256 } else { 128 };
    println!("buffer_size={}", BUFFER_SIZE);

    // Future intrinsics: .starts_with(), .ends_with(), .contains(), slicing
}
