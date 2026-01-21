pub mod bench;
pub mod collections;
pub mod json;
pub mod meta;
pub mod proc_macro;
pub mod test;
pub mod time;

#[lang = "create_struct"]
fn create_struct(name: &str) -> type {
    compile_error!("create_struct is a compiler intrinsic")
}

#[lang = "addfield"]
fn addfield(ty: type, name: &str, field_ty: type) -> type {
    compile_error!("addfield is a compiler intrinsic")
}
