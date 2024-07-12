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

pub trait AstAttributesExt {
    fn find_by_path(&self, path: &Path) -> Option<&AstAttrMeta>;
    fn find_by_name(&self, name: &str) -> Option<&AstAttrMeta>;
}
impl AstAttributesExt for Vec<AstAttribute> {
    fn find_by_path(&self, path: &Path) -> Option<&AstAttrMeta> {
        self.iter()
            .find(|x| match &x.meta {
                AstAttrMeta::Path(p) => p == path,
                _ => false,
            })
            .map(|x| &x.meta)
    }
    fn find_by_name(&self, name: &str) -> Option<&AstAttrMeta> {
        self.iter()
            .find(|x| match &x.meta {
                AstAttrMeta::NameValue(nv) => nv.name.last().as_str() == name,
                AstAttrMeta::Path(p) => p.last().as_str() == name,
                _ => false,
            })
            .map(|x| &x.meta)
    }
}
