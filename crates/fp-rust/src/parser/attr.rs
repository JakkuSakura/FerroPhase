use super::RustParser;
use crate::parser::parse_path;
use fp_core::ast::{AttrMeta, AttrMetaList, AttrMetaNameValue, AttrStyle, Attribute};
use fp_core::error::Result;

impl RustParser {
    fn parse_attr_style(&self, style: syn::AttrStyle) -> AttrStyle {
        match style {
            syn::AttrStyle::Outer => AttrStyle::Outer,
            syn::AttrStyle::Inner(_) => AttrStyle::Inner,
        }
    }

    fn parse_attr_meta_list(&self, list: syn::MetaList) -> Result<AttrMetaList> {
        let name = parse_path(list.path.clone())?;
        self.error(
            format!("AttrMetaList is not implemented: {:?}", list),
            AttrMetaList {
                name,
                items: Vec::new(),
            },
        )
    }

    fn parse_attr_meta_name_value(&self, nv: syn::MetaNameValue) -> Result<AttrMetaNameValue> {
        let name = parse_path(nv.path)?;
        let value = self.parse_expr(nv.value)?.into();
        Ok(AttrMetaNameValue { name, value })
    }

    fn parse_attr_meta(&self, meta: syn::Meta) -> Result<AttrMeta> {
        Ok(match meta {
            syn::Meta::Path(p) => AttrMeta::Path(parse_path(p)?),
            syn::Meta::List(l) => AttrMeta::List(self.parse_attr_meta_list(l)?),
            syn::Meta::NameValue(nv) => AttrMeta::NameValue(self.parse_attr_meta_name_value(nv)?),
        })
    }

    pub(super) fn parse_attr(&self, attr: syn::Attribute) -> Result<Attribute> {
        let style = self.parse_attr_style(attr.style);
        let meta = self.parse_attr_meta(attr.meta)?;
        Ok(Attribute { style, meta })
    }

    pub(super) fn parse_attrs(&self, attrs: Vec<syn::Attribute>) -> Result<Vec<Attribute>> {
        attrs
            .into_iter()
            .map(|attr| self.parse_attr(attr))
            .collect()
    }
}
