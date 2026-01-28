// Async-friendly helpers.

fn sleep(seconds: f64) -> std::task::Future<()> {
    std::task::Future {
        handle: async {
            std::time::sleep(seconds);
            ()
        },
    }
}
