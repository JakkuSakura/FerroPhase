#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Ty {
    I1,
    I8,
    I16,
    I32,
    I64,
    I128,
    F32,
    F64,
    Ptr(Box<Ty>),
    Array(Box<Ty>, u64),
    Struct {
        fields: Vec<Ty>,
        packed: bool,
        name: Option<String>,
    },
    Function {
        return_type: Box<Ty>,
        param_types: Vec<Ty>,
        is_variadic: bool,
    },
    Vector(Box<Ty>, u32),
    Void,
    Label,
    Token,
    Metadata,
}
