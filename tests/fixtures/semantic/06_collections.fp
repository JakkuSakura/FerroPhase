// Minimal semantic fixture: collections (Vec, HashMap).
// Expected output:
// vec[1]: 20
// vec_len: 3
// map[beta]: 2
// map_len: 3

use std::collections::HashMap;

fn main() {
    let numbers: Vec<i64> = [10, 20, 30];
    println!("vec[1]: {}", numbers[1]);
    println!("vec_len: {}", numbers.len());

    let scores: HashMap<&'static str, i64> = HashMap::from([
        ("alpha", 1),
        ("beta", 2),
        ("gamma", 3),
    ]);
    println!("map[beta]: {}", scores["beta"]);
    println!("map_len: {}", scores.len());
}
