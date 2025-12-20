pub enum Color {
    Red,
    Green,
    Rgb(u8, u8, u8),
}
pub enum Option<T> {
    Some(T),
    None,
}
pub fn describe(color: &Color) -> &str {
    match color {
        Color::Red => "red".to_string(),
        Color::Green => "green".to_string(),
        Color::Rgb(255, 0, 0) => "red rgb".to_string(),
        Color::Rgb(r, g, b) => "custom rgb".to_string(),
    }
}
pub fn classify(n: i64) -> &str {
    match n {
        0 => "zero".to_string(),
        n if n < 0 => "negative".to_string(),
        n if n % 2 == 0 => "even".to_string(),
        _ => "odd".to_string(),
    }
}
pub fn unwrap_or(opt: Option<i64>, default: i64) -> i64 {
    match opt {
        Option::Some(v) => v,
        Option::None => default,
    }
}
pub fn main() -> () {
    let red = Color::Red;
    let rgb = Color::Rgb(128, 64, 32);
    println!("{}", describe(&red));
    println!("{}", describe(&rgb));
    println!("{}", classify(-5));
    println!("{}", classify(0));
    println!("{}", classify(4));
    println!("{}", classify(7));
    println!("{}", unwrap_or(Option::Some(42), 0));
    println!("{}", unwrap_or(Option::None, 99));
    pub const CODE: i64 = 0;
    println!("0x{06X}", CODE);
}
