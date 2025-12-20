pub fn main() -> () {
    const TEMP: i64 = 25;
    const WEATHER: &str = "warm";
    println!("{}°C is {}", TEMP, WEATHER);
    const IS_SUNNY: bool = true;
    const IS_WARM: bool = true;
    const ACTIVITY: &str = "outdoor";
    println!("Suggested: {}", ACTIVITY);
    const SCORE: i64 = 85;
    const GRADE: &str = "B";
    println!("Score {} = grade {}", SCORE, GRADE);
    let value = 42;
    let category = if value > 50 {
        "high"
    } else if value > 25 {
        "medium"
    } else {
        "low"
    };
    println!("Value {} is {}", value, category);
}
