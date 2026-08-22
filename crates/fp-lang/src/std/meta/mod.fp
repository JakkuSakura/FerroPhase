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

    pub const fn build(self) -> type {
        self.ty
    }
}

// Deliberately not `ty: TypeDescriptor` (which would need `TypeDescriptor`
// and `FieldDescriptor` to nest into each other by value) — `&[T]`/`Ref`
// type lowering resolves its pointee's own layout eagerly rather than
// treating a reference as the fixed-size pointer it actually is, so a
// real by-value cycle here trips the compiler's "recursive type has
// infinite size" check even though no such cycle exists at the machine
// level. A field's own type only ever needs a name here, so route it
// through this smaller, non-nesting descriptor instead.
pub struct FieldTypeDescriptor {
    pub name: &str,
}

pub struct FieldDescriptor {
    pub name: &str,
    pub ty: FieldTypeDescriptor,
}

pub struct TypeDescriptor {
    pub name: &str,
    pub size: i64,
    pub fields: &[FieldDescriptor],
}

impl TypeDescriptor {
    pub fn field_type(self, name: &str) -> FieldTypeDescriptor {
        ::std::intrinsics::field_type(self, name)
    }
}

// A primitive type's own runtime reflection value — lets a bare `i64`
// (or any other primitive name below) be passed as an ordinary value
// argument (e.g. `TypeBuilder::with_field("id", i64)`), the same way a
// struct's name already doubles as its constructor in the value
// namespace. One line of data per name, not compiler logic — mirrors how
// `is_primitive_type_name` (`ast_to_hir/mod.rs`) enumerates the same set
// for the *type*-position case, kept in sync with it by hand.
pub const bool: type<_> = ::std::intrinsics::primitive_type("bool");
pub const char: type<_> = ::std::intrinsics::primitive_type("char");
pub const str: type<_> = ::std::intrinsics::primitive_type("str");
pub const i8: type<_> = ::std::intrinsics::primitive_type("i8");
pub const i16: type<_> = ::std::intrinsics::primitive_type("i16");
pub const i32: type<_> = ::std::intrinsics::primitive_type("i32");
pub const i64: type<_> = ::std::intrinsics::primitive_type("i64");
pub const i128: type<_> = ::std::intrinsics::primitive_type("i128");
pub const isize: type<_> = ::std::intrinsics::primitive_type("isize");
pub const u8: type<_> = ::std::intrinsics::primitive_type("u8");
pub const u16: type<_> = ::std::intrinsics::primitive_type("u16");
pub const u32: type<_> = ::std::intrinsics::primitive_type("u32");
pub const u64: type<_> = ::std::intrinsics::primitive_type("u64");
pub const u128: type<_> = ::std::intrinsics::primitive_type("u128");
pub const usize: type<_> = ::std::intrinsics::primitive_type("usize");
pub const f32: type<_> = ::std::intrinsics::primitive_type("f32");
pub const f64: type<_> = ::std::intrinsics::primitive_type("f64");
