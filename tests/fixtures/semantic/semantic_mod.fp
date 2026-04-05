// Module for 07_module_import.fp.
// Expected output:
// (none; this module defines helpers only)

pub const PREFIX: &str = "mod";

pub fn greet(name: &str) -> &'static str {
    if name.len() == 0 {
        "mod:anon"
    } else {
        "mod:hello"
    }
}

pub mod util;
