use crate::ast::BExpr;
use crate::id::Path;
use crate::{common_enum, common_struct};

common_enum! {
    pub enum AstAttrStyle {
        Outer,
        Inner,
    }
}

common_enum! {
    pub enum AstAttrMeta {
        Path(Path),
        List(AstAttrMetaList),
        NameValue(AstAttrMetaNameValue),
    }
}
common_struct! {
    pub struct AstAttrMetaList {
        pub name: Path,
        pub items: Vec<AstAttrMeta>,
    }
}
common_struct! {
    pub struct AstAttrMetaNameValue {
        pub name: Path,
        pub value: BExpr,
    }
}
common_struct! {
    pub struct AstAttribute {
        pub style: AstAttrStyle,
        pub meta: AstAttrMeta,
    }
}
