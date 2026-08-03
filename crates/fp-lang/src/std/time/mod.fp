#[op = "time_now"]
fn now() -> f64 {
    std::intrinsics::time::now()
}

fn sleep(seconds: f64) -> () {
    ::libc::usleep((seconds * 1000000.0) as ::libc::useconds_t);
}
