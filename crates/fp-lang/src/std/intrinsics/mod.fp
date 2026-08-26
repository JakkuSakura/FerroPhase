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

// TODO: implement this as a real std fn once type-level reflection/
// pattern matching is expressive enough in-language; it's a compiler
// intrinsic for now purely to bootstrap union-of-literal-type support
// (TypeScript-style template literal type union distribution). Returns a
// closure (not currying/general partial application): calling that
// closure with a reflected union type applies `f` to each member's
// literal string and rebuilds the union.
#[intrinsic = "unionify"]
pub const fn unionify(f: fn(&str) -> &str) -> fn(type) -> type<_> {
    compile_error!("unionify is a compiler intrinsic")
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
pub fn print(text: &str, *args: _, **argv: _) {
    compile_error!("print is a compiler intrinsic")
}

#[intrinsic = "println"]
pub fn println(text: &str, *args: _, **argv: _) {
    compile_error!("println is a compiler intrinsic")
}

#[intrinsic = "field_type"]
pub fn field_type(
    ty: ::std::meta::Type,
    name: &str,
) -> ::std::meta::FieldTypeDescriptor {
    compile_error!("field_type is a compiler intrinsic")
}
