pub fn main() -> () {
    print!("Hello");
    print!("World with newlines");
    println!("");
    print!("Number:", 42);
    print!("Boolean:", true, false);
    print!("Mixed:", 1, 2.5, "text".to_string(), true);
    println!("");
    print!("Namespace test", "still works".to_string());
    println!("");
    let value = 7;
    println!("value = {}", value);
    println!("math: {} + {} = {}", 2, 3, 5);
    print!(
        "This",
        "stays".to_string(),
        "on".to_string(),
        "one".to_string(),
        "line".to_string()
    );
    println!("");
    print!("Continuing without newline");
    print!(" - appended content");
    println!("");
    print!("Unit:", ());
    print!("Null:", ());
    println!("");
}
