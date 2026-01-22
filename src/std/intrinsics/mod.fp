#[lang = "create_struct"]
pub const fn create_struct(name: &str) -> type {
    compile_error!("create_struct is a compiler intrinsic")
}

#[lang = "addfield"]
pub const fn addfield(ty: type, name: &str, field_ty: type) -> type {
    compile_error!("addfield is a compiler intrinsic")
}
