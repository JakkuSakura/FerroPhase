#[macro_export]
macro_rules! shll_parse_item {
    ($($tt:tt)*) => {{
        let code: syn::Item = syn::parse_quote!($($tt)*);
        rust_lang::parser::RustParser::new().parse_item(code)?
    }};
}
#[macro_export]
macro_rules! shll_parse_items {
    ($($tt:tt)*) => {{
        let file: syn::File = syn::parse_quote!($($tt)*);
        rust_lang::parser::RustParser::new().parse_items(file.items)?
    }};
}
#[macro_export]
macro_rules! shll_parse_type {
    ($($tt:tt)*) => {{
        let code: syn::Type = syn::parse_quote!($($tt)*);
        rust_lang::parser::RustParser::new().parse_type(code)?
    }};
}
#[macro_export]
macro_rules! shll_parse_expr {
    ($($tt:tt)*) => {{
        let code: syn::Expr = syn::parse_quote!($($tt)*);
        rust_lang::parser::RustParser::new().parse_expr(code)?
    }};
}
#[macro_export]
macro_rules! shll_parse_value {
    ($($tt:tt)*) => {{
        let code: syn::Expr = syn::parse_quote!($($tt)*);
        rust_lang::parser::RustParser::new().parse_value(code)?
    }};
}
