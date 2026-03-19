pub struct TypeBuilder {
    ty: type,
}

impl TypeBuilder {
    pub const fn new(name: &str) -> TypeBuilder {
        TypeBuilder {
            ty: std::intrinsics::create_struct(name),
        }
    }

    pub const fn from(ty: type) -> TypeBuilder {
        TypeBuilder { ty }
    }

    pub const fn with_field(self, name: &str, field_ty: type) -> TypeBuilder {
        let ty = std::intrinsics::addfield(self.ty, name, field_ty);
        TypeBuilder { ty }
    }

    pub const fn build(self) -> type {
        self.ty
    }
}
