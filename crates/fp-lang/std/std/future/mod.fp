// Async-friendly helpers.

fn sleep(seconds: f64) {
    let _ = seconds;
    compile_error!("compiler intrinsic")
}
