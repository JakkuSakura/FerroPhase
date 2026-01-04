#!/usr/bin/env fp run
//! Compile-time string operations and intrinsics

fn main() {
    println!("📘 Tutorial: 02_string_processing.fp");
    println!("🧭 Focus: Compile-time string operations and intrinsics");
    println!("🧪 What to look for: labeled outputs below");
    println!("✅ Expectation: outputs match labels");
    println!("");
    // Const strings
    const NAME: &str = "FerroPhase";
    const VERSION: &str = "0.1.0";

    // String intrinsics at compile time
    const NAME_LEN: usize = NAME.len();
    const VERSION_LEN: usize = VERSION.len();
    const PREFIX_OK: bool = NAME.starts_with("Ferro");
    const SUFFIX_OK: bool = NAME.ends_with("Phase");
    const HAS_PHASE: bool = NAME.contains("Phase");
    const SHORT: &str = NAME[0..5];
    const TAIL: &str = NAME[5..];

    println!("name='{}' len={}", NAME, NAME_LEN);
    println!("version='{}' len={}", VERSION, VERSION_LEN);
    println!(
        "prefix_ok={}, suffix_ok={}, contains_phase={}",
        PREFIX_OK, SUFFIX_OK, HAS_PHASE
    );
    println!("slices: short='{}' tail='{}'", SHORT, TAIL);

    // Const word table
    const WORDS: [&str; 4] = ["alpha", "beta", "gamma", "delta"];
    const WORD_LENGTHS: [usize; 4] = [
        WORDS[0].len(),
        WORDS[1].len(),
        WORDS[2].len(),
        WORDS[3].len(),
    ];
    const TOTAL_WORD_LEN: usize =
        WORD_LENGTHS[0] + WORD_LENGTHS[1] + WORD_LENGTHS[2] + WORD_LENGTHS[3];

    println!("words:");
    for i in 0..4 {
        println!("  {} -> len={}", WORDS[i], WORD_LENGTHS[i]);
    }
    println!("total word length={}", TOTAL_WORD_LEN);

    // String-based comparisons at compile time
    const IS_EMPTY: bool = NAME_LEN == 0;
    const IS_LONG: bool = NAME_LEN > 5;
    const BANNER: &str = NAME + " v" + VERSION;

    println!("empty={}, long={}", IS_EMPTY, IS_LONG);
    println!("banner='{}'", BANNER);

    // Buffer sizing based on string length
    const BUFFER_SIZE: usize = if NAME_LEN > 8 { 256 } else { 128 };
    println!("buffer_size={}", BUFFER_SIZE);
}
