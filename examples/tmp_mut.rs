fn main() {
    let mut value: i64 = 1;
    println!("initial: {}", value);

    value = value + 41;
    println!("after first add: {}", value);

    value = value - 2;
    println!("after second step: {}", value);

    value = value * 3;
    println!("final: {}", value);
}
