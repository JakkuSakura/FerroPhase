use crate::parser::expr::parse_expr;
use crate::parser::parse_path;
use fp_core::ast::{AttrMeta, AttrMetaList, AttrMetaNameValue, AttrStyle, Attribute};
use fp_core::bail;
use fp_core::error::Result;
fn parse_attr_style(s: syn::AttrStyle) -> Result<AttrStyle> {
    Ok(match s {
        syn::AttrStyle::Outer => AttrStyle::Outer,
        syn::AttrStyle::Inner(_) => AttrStyle::Inner,
    })
}
fn parse_attr_meta_list(l: syn::MetaList) -> Result<AttrMetaList> {
    bail!("AttrMetaList is not implemented: {:?}", l);
    // let name = parse_path(l.path)?;
    // let items = todo!();
    // Ok(AttrMetaList { name, items })
    // todo!()
}
fn parse_attr_meta_name_value(nv: syn::MetaNameValue) -> Result<AttrMetaNameValue> {
    let name = parse_path(nv.path)?;
    let value = parse_expr(nv.value)?.into();
    Ok(AttrMetaNameValue { name, value })
}
fn parse_attr_meta(m: syn::Meta) -> Result<AttrMeta> {
    Ok(match m {
        syn::Meta::Path(p) => AttrMeta::Path(parse_path(p)?),
        syn::Meta::List(l) => AttrMeta::List(parse_attr_meta_list(l)?),
        syn::Meta::NameValue(nv) => AttrMeta::NameValue(parse_attr_meta_name_value(nv)?),
    })
}
pub fn parse_attr(a: syn::Attribute) -> Result<Attribute> {
    let style = parse_attr_style(a.style)?;
    let meta = parse_attr_meta(a.meta)?;
    Ok(Attribute { style, meta })
}
pub fn parse_attrs(attrs: Vec<syn::Attribute>) -> Result<Vec<Attribute>> {
    attrs.into_iter().map(parse_attr).collect()
}
