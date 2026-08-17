#[intrinsic = "spawn"]
pub fn spawn(fut: any) -> any {
    compile_error!("spawn is a compiler intrinsic")
}

#[intrinsic = "join"]
pub fn join(fut: any) -> any {
    compile_error!("join is a compiler intrinsic")
}

#[intrinsic = "select"]
pub fn select(fut: any) -> any {
    compile_error!("select is a compiler intrinsic")
}
