#!/usr/bin/env fp run
//! Placeholder string metrics until string intrinsics are available.

fn main() {
    const APP_LENGTH: i64 = 10;
    const VERSION_LENGTH: i64 = 5;
    const BANNER_EXTRA: i64 = 3;
    const BANNER_LENGTH: i64 = APP_LENGTH + VERSION_LENGTH + BANNER_EXTRA;
    const VERSION_HAS_SEPARATOR: bool = true;
    const RELEASE_CHANNEL_IS_BETA: bool = false;
    const METADATA_VALID: bool = BANNER_LENGTH >= 15;

    const BUFFER_SIZE: i64 = if BANNER_LENGTH > 20 { 1024 } else { 512 };
    const HASH_BASE: i64 = 31;
    const SIMPLE_HASH: i64 = APP_LENGTH * HASH_BASE + VERSION_LENGTH;
    const SUMMARY_SCORE: i64 = APP_LENGTH + VERSION_LENGTH + BANNER_LENGTH;

    println!("App length {}", APP_LENGTH);
    println!("Version length {}", VERSION_LENGTH);
    println!("Banner length {}", BANNER_LENGTH);
    println!("Separator flag {}", VERSION_HAS_SEPARATOR);
    println!("Beta channel {}", RELEASE_CHANNEL_IS_BETA);
    println!("Metadata valid {}", METADATA_VALID);
    println!("Buffer size {}", BUFFER_SIZE);
    println!("Hash marker {}", SIMPLE_HASH);
    println!("Summary score {}", SUMMARY_SCORE);
}
