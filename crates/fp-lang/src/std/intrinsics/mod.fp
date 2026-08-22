pub mod env;
pub mod fs;
pub mod io;
pub mod json;
pub mod path;
pub mod proc_macro;
pub mod task;
pub mod test;
pub mod time;
pub mod yaml;

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

#[intrinsic = "primitive_type"]
pub const fn primitive_type(name: &str) -> type<_> {
    compile_error!("primitive_type is a compiler intrinsic")
}

#[intrinsic = "catch_unwind"]
pub fn catch_unwind(f: fn()) -> bool {
    compile_error!("catch_unwind is a compiler intrinsic")
}

#[intrinsic = "catch_unwind_result"]
pub fn catch_unwind_result(f: fn()) -> bool {
    compile_error!("catch_unwind_result is a compiler intrinsic")
}

#[intrinsic = "print"]
pub fn print(text: &str) {
    compile_error!("print is a compiler intrinsic")
}

#[intrinsic = "println"]
pub fn println(text: &str) {
    compile_error!("println is a compiler intrinsic")
}

#[intrinsic = "field_type"]
pub fn field_type(
    ty: ::std::meta::TypeDescriptor,
    name: &str,
) -> ::std::meta::FieldTypeDescriptor {
    compile_error!("field_type is a compiler intrinsic")
}
