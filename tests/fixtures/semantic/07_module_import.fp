// Minimal semantic fixture: module import (file + submodule).
// Expected output:
// greet: mod:hello
// triple: 18

mod semantic_mod;

use semantic_mod::greet;
use semantic_mod::util::triple;

fn main() {
    println!("greet: {}", greet("fp"));
    println!("triple: {}", triple(6));
}
