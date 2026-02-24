pub fn main() -> () {
    print!("Hello");
    print!("World with newlines");
    println!("");
    print!("Number: {}", 42);
    print!("Boolean: {} {}", true, false);
    print!("Mixed: {} {} {} {}", 1, 2.5, "text", true);
    println!("");
    print!("Namespace test {}", "still works");
    println!("");
    let value = 7;
    println!("value = {}", value);
    println!("math: {} + {} = {}", 2, 3, 5);
    print!("This {} {} {} {}", "stays", "on", "one", "line");
    println!("");
    print!("Continuing without newline");
    print!(" - appended content");
    println!("");
    print!("Unit: {}", format_args!("{:?}", ()));
    print!("Null: {}", format_args!("{:?}", ()));
    println!("");
}
