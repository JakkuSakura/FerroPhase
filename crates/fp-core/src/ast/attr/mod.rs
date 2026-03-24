use crate::ast::{BExpr, ExprKind, Path, ReprFlags, ReprInt, ReprOptions, Value};
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

pub fn attrs_repr(attrs: &[Attribute]) -> ReprOptions {
    let mut repr = ReprOptions::default();
    for attr in attrs {
        apply_repr_attr(attr, &mut repr);
    }
    repr
}

fn apply_repr_attr(attr: &Attribute, repr: &mut ReprOptions) {
    let AttrMeta::List(list) = &attr.meta else {
        return;
    };
    if list.name.last().as_str() != "repr" {
        return;
    }
    for item in &list.items {
        apply_repr_meta(item, repr);
    }
}

fn apply_repr_meta(meta: &AttrMeta, repr: &mut ReprOptions) {
    match meta {
        AttrMeta::Path(path) => match path.last().as_str() {
            "C" => repr.flags.insert(ReprFlags::IS_C),
            "simd" => repr.flags.insert(ReprFlags::IS_SIMD),
            "transparent" => repr.flags.insert(ReprFlags::IS_TRANSPARENT),
            "linear" => repr.flags.insert(ReprFlags::IS_LINEAR),
            "packed" => {
                repr.flags.insert(ReprFlags::IS_PACKED);
                if repr.pack.is_none() {
                    repr.pack = Some(1);
                }
            }
            other => {
                if let Some(int) = parse_repr_int(other) {
                    repr.int = Some(int);
                }
            }
        },
        AttrMeta::List(list) => match list.name.last().as_str() {
            "align" => {
                if let Some(value) = list.items.first().and_then(meta_usize) {
                    repr.align = Some(value);
                }
            }
            "packed" => {
                repr.flags.insert(ReprFlags::IS_PACKED);
                repr.pack = list.items.first().and_then(meta_usize).or(Some(1));
            }
            _ => {}
        },
        AttrMeta::NameValue(nv) => match nv.name.last().as_str() {
            "align" => repr.align = expr_usize(&nv.value),
            "packed" => {
                repr.flags.insert(ReprFlags::IS_PACKED);
                repr.pack = expr_usize(&nv.value).or(Some(1));
            }
            _ => {}
        },
    }
}

fn meta_usize(meta: &AttrMeta) -> Option<u64> {
    match meta {
        AttrMeta::NameValue(nv) => expr_usize(&nv.value),
        AttrMeta::Path(path) => path.last().as_str().parse().ok(),
        AttrMeta::List(_) => None,
    }
}

fn expr_usize(expr: &BExpr) -> Option<u64> {
    match expr.kind() {
        ExprKind::Value(value) => match value.as_ref() {
            Value::Int(int) if int.value >= 0 => u64::try_from(int.value).ok(),
            _ => None,
        },
        _ => None,
    }
}

fn parse_repr_int(name: &str) -> Option<ReprInt> {
    Some(match name {
        "i8" => ReprInt::I8,
        "i16" => ReprInt::I16,
        "i32" => ReprInt::I32,
        "i64" => ReprInt::I64,
        "i128" => ReprInt::I128,
        "u8" => ReprInt::U8,
        "u16" => ReprInt::U16,
        "u32" => ReprInt::U32,
        "u64" => ReprInt::U64,
        "u128" => ReprInt::U128,
        "isize" => ReprInt::Isize,
        "usize" => ReprInt::Usize,
        _ => return None,
    })
}
