pub struct TypeBuilder {
    ty: type,
}

impl TypeBuilder {
    pub const fn new(name: &str) -> TypeBuilder {
        TypeBuilder {
            ty: ::std::intrinsics::create_struct(name),
        }
    }

    pub const fn from(ty: type) -> TypeBuilder {
        TypeBuilder { ty }
    }

    pub const fn with_field(self, name: &str, field_ty: type) -> TypeBuilder {
        let ty = ::std::intrinsics::addfield(self.ty, name, field_ty);
        TypeBuilder { ty }
    }

    // Convert the builder's internal type handle into the completed,
    // concrete type value visible to callers.
    pub const fn build(self) -> type<_> {
        self.ty as _
    }
}

pub struct FieldTypeDescriptor {
    pub name: &str,
}

pub struct StructField {
    pub name: &str,
    pub ty: FieldTypeDescriptor,
}

pub struct Type {
    pub name: &str,
    pub size: usize,
    pub fields: ::alloc::Vec<StructField>,
    pub methods: ::alloc::Vec<&str>,
}

impl Type {
    pub fn field_type(self, name: &str) -> FieldTypeDescriptor {
        ::std::intrinsics::field_type(self, name)
    }
}

// A primitive type's own runtime reflection value — lets a bare `i64`
// (or any other primitive name below) be passed as an ordinary value
// argument (e.g. `TypeBuilder::with_field("id", i64)`), the same way a
// struct's name already doubles as its constructor in the value
// namespace. One macro invocation per name (mirroring how rustc's own
// libcore implements a trait for every integer type via one macro rather
// than N hand-written impls) — `fp-lang` has no `stringify!`, so each arm
// still has to spell its own name out as a string literal once, but the
// `pub const $name: type<_> = ...;` shape itself is written only once,
// here, kept in sync with `is_primitive_type_name` (`ast_to_hir/mod.rs`)
// by hand.
macro_rules! decl_primitive {
    (bool) => { pub const bool: type<_> = ::std::intrinsics::primitive_type("bool"); };
    (char) => { pub const char: type<_> = ::std::intrinsics::primitive_type("char"); };
    (str) => { pub const str: type<_> = ::std::intrinsics::primitive_type("str"); };
    (i8) => { pub const i8: type<_> = ::std::intrinsics::primitive_type("i8"); };
    (i16) => { pub const i16: type<_> = ::std::intrinsics::primitive_type("i16"); };
    (i32) => { pub const i32: type<_> = ::std::intrinsics::primitive_type("i32"); };
    (i64) => { pub const i64: type<_> = ::std::intrinsics::primitive_type("i64"); };
    (i128) => { pub const i128: type<_> = ::std::intrinsics::primitive_type("i128"); };
    (isize) => { pub const isize: type<_> = ::std::intrinsics::primitive_type("isize"); };
    (u8) => { pub const u8: type<_> = ::std::intrinsics::primitive_type("u8"); };
    (u16) => { pub const u16: type<_> = ::std::intrinsics::primitive_type("u16"); };
    (u32) => { pub const u32: type<_> = ::std::intrinsics::primitive_type("u32"); };
    (u64) => { pub const u64: type<_> = ::std::intrinsics::primitive_type("u64"); };
    (u128) => { pub const u128: type<_> = ::std::intrinsics::primitive_type("u128"); };
    (usize) => { pub const usize: type<_> = ::std::intrinsics::primitive_type("usize"); };
    (f32) => { pub const f32: type<_> = ::std::intrinsics::primitive_type("f32"); };
    (f64) => { pub const f64: type<_> = ::std::intrinsics::primitive_type("f64"); };
}

decl_primitive!{bool}
decl_primitive!{char}
decl_primitive!{str}
decl_primitive!{i8}
decl_primitive!{i16}
decl_primitive!{i32}
decl_primitive!{i64}
decl_primitive!{i128}
decl_primitive!{isize}
decl_primitive!{u8}
decl_primitive!{u16}
decl_primitive!{u32}
decl_primitive!{u64}
decl_primitive!{u128}
decl_primitive!{usize}
decl_primitive!{f32}
decl_primitive!{f64}
