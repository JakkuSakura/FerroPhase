pub fn main() -> () {
    pub const TEMP: i64 = 25;
    pub const WEATHER: &str = "warm".to_string();
    println!("{}°C is {}", TEMP, WEATHER);
    pub const IS_SUNNY: bool = true;
    pub const IS_WARM: bool = true;
    pub const ACTIVITY: &str = "outdoor".to_string();
    println!("Suggested: {}", ACTIVITY);
    pub const SCORE: i64 = 85;
    pub const GRADE: &str = "B".to_string();
    println!("Score {} = grade {}", SCORE, GRADE);
    let value = 42;
    let category = if value > 50 {
        "high".to_string()
    } else if value > 25 {
        "medium".to_string()
    } else {
        "low".to_string()
    };
    println!("Value {} is {}", value, category);
}
