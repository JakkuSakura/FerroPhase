use crate::ops::*;
use crate::tree::*;
use crate::value::*;

/// TypeExpr is an expression that returns a type
#[derive(Clone, Serialize, Deserialize, Debug)]
pub enum TypeExpr {
    Ident(Ident),
    Path(Path),
    NamedStruct(NamedStructTypeExpr),
    UnnamedStruct(UnnamedStructTypeExpr),
    Primitive(PrimitiveType),
    ConcreteType(TypeValue),
    FuncType(FuncTypeExpr),
    Op(TypeOp),
    Invoke(InvokeTypeExpr),
    RequireTrait(RequireTrait),
    RequireTraits(RequireTraits),
}

impl TypeExpr {
    pub fn unit() -> TypeExpr {
        TypeExpr::Primitive(PrimitiveType::Unit)
    }
}
#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct RequireTrait {
    pub name: Ident,
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct RequireTraits {
    pub traits: Vec<RequireTrait>,
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub enum TypeOp {
    Add(AddOp<TypeExpr>),
    Sub(SubOp<TypeExpr>),
}
pub type InvokeTypeExpr = Invoke<TypeExpr>;
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FuncTypeExpr {
    pub params: Vec<TypeExpr>,
    pub ret: Box<TypeExpr>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Generics {
    pub params: Vec<ParamExpr>,
    // TODO: restrains
    pub value: Tree,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FieldTypeExpr {
    pub name: Ident,
    pub ty: TypeExpr,
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NamedStructTypeExpr {
    pub name: Ident,
    pub fields: Vec<FieldTypeExpr>,
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UnnamedStructTypeExpr {
    pub fields: Vec<FieldTypeExpr>,
}
