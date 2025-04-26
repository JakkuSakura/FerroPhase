use crate::parser::expr::parse_expr;
use crate::parser::parse_path;
use eyre::bail;
use eyre::Result;
use fp_core::ast::{
    AstAttrMeta, AstAttrMetaList, AstAttrMetaNameValue, AstAttrStyle, AstAttribute,
};
fn parse_attr_style(s: syn::AttrStyle) -> Result<AstAttrStyle> {
    Ok(match s {
        syn::AttrStyle::Outer => AstAttrStyle::Outer,
        syn::AttrStyle::Inner(_) => AstAttrStyle::Inner,
    })
}
fn parse_attr_meta_list(l: syn::MetaList) -> Result<AstAttrMetaList> {
    bail!("AstAttrMetaList is not implemented: {:?}", l);
    // let name = parse_path(l.path)?;
    // let items = todo!();
    // Ok(AstAttrMetaList { name, items })
    // todo!()
}
fn parse_attr_meta_name_value(nv: syn::MetaNameValue) -> Result<AstAttrMetaNameValue> {
    let name = parse_path(nv.path)?;
    let value = parse_expr(nv.value)?.into();
    Ok(AstAttrMetaNameValue { name, value })
}
fn parse_attr_meta(m: syn::Meta) -> Result<AstAttrMeta> {
    Ok(match m {
        syn::Meta::Path(p) => AstAttrMeta::Path(parse_path(p)?),
        syn::Meta::List(l) => AstAttrMeta::List(parse_attr_meta_list(l)?),
        syn::Meta::NameValue(nv) => AstAttrMeta::NameValue(parse_attr_meta_name_value(nv)?),
    })
}
pub fn parse_attr(a: syn::Attribute) -> Result<AstAttribute> {
    let style = parse_attr_style(a.style)?;
    let meta = parse_attr_meta(a.meta)?;
    Ok(AstAttribute { style, meta })
}
pub fn parse_attrs(attrs: Vec<syn::Attribute>) -> Result<Vec<AstAttribute>> {
    attrs.into_iter().map(parse_attr).collect()
}
