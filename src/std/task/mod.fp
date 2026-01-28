// Task and Future placeholders for the runtime scheduler.

struct Future<T> {
    handle: any,
}

struct Task<T> {
    handle: any,
}

// Accept any Future-like value (async block, std::future::sleep, etc.).
fn spawn<T>(fut: any) -> Task<T> {
    Task { handle: std::task::spawn(fut) }
}

macro_rules! join {
    ($($future:expr),+ $(,)?) => { std::task::join($($future),+) };
}

macro_rules! select {
    ($($future:expr),+ $(,)?) => { std::task::select($($future),+) };
}
