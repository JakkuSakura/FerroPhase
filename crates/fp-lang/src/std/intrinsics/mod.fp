#[intrinsic = "create_struct"]
pub const fn create_struct(name: &str) -> type {
    compile_error!("create_struct is a compiler intrinsic")
}

#[intrinsic = "addfield"]
pub const fn addfield(ty: type, name: &str, field_ty: type) -> type {
    compile_error!("addfield is a compiler intrinsic")
}

#[intrinsic = "build_type"]
pub const fn build_type(ty: type) -> type<_> {
    compile_error!("build_type is a compiler intrinsic")
}
