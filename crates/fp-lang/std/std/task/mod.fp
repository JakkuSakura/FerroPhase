// Future placeholder helpers for the runtime scheduler.

pub struct Future<T> {
    pub handle: any,
}

pub fn spawn(fut: any) -> any {
    ::std::intrinsics::task::spawn(fut)
}

pub fn join(fut: any) -> any {
    ::std::intrinsics::task::join(fut)
}

pub fn select(fut: any) -> any {
    ::std::intrinsics::task::select(fut)
}

macro_rules! join {
    ($($future:expr),+ $(,)?) => { ::std::task::join($($future),+) };
}

macro_rules! select {
    ($($future:expr),+ $(,)?) => { ::std::task::select($($future),+) };
}
