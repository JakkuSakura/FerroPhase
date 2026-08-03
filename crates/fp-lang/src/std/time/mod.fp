#[op = "time_now"]
fn now() -> f64 {
    intrinsic_now()
}

#[intrinsic = "time_now"]
fn intrinsic_now() -> f64 { compile_error!("compiler intrinsic") }

fn sleep(seconds: f64) -> () {
    std::time::sleep(seconds)
}
