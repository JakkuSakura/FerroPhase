use crate::ast::{BExpr, Path};
use crate::{common_enum, common_struct};

common_enum! {
    pub enum AttrStyle {
        Outer,
        Inner,
    }
}

common_enum! {
    pub enum AttrMeta {
        Path(Path),
        List(AttrMetaList),
        NameValue(AttrMetaNameValue),
    }
}
common_struct! {
    pub struct AttrMetaList {
        pub name: Path,
        pub items: Vec<AttrMeta>,
    }
}
common_struct! {
    pub struct AttrMetaNameValue {
        pub name: Path,
        pub value: BExpr,
    }
}
common_struct! {
    pub struct Attribute {
        pub style: AttrStyle,
        pub meta: AttrMeta,
    }
}

pub trait AttributesExt {
    fn find_by_path(&self, path: &Path) -> Option<&AttrMeta>;
    fn find_by_name(&self, name: &str) -> Option<&AttrMeta>;
}
impl AttributesExt for Vec<Attribute> {
    fn find_by_path(&self, path: &Path) -> Option<&AttrMeta> {
        self.iter()
            .find(|x| match &x.meta {
                AttrMeta::Path(p) => p == path,
                _ => false,
            })
            .map(|x| &x.meta)
    }
    fn find_by_name(&self, name: &str) -> Option<&AttrMeta> {
        self.iter()
            .find(|x| match &x.meta {
                AttrMeta::NameValue(nv) => nv.name.last().as_str() == name,
                AttrMeta::Path(p) => p.last().as_str() == name,
                _ => false,
            })
            .map(|x| &x.meta)
    }
}
