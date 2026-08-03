#[intrinsic = "time_now"]
pub fn now() -> f64 { compile_error!("compiler intrinsic") }
