#[op = "time_now"]
fn now() -> f64 {
    std::intrinsics::time::now()
}

fn sleep(seconds: f64) -> () {
    std::time::sleep(seconds)
}
