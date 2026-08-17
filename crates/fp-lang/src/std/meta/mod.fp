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
