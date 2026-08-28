use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{Data, DeriveInput, Fields, Type, parse_macro_input};

#[proc_macro_derive(Host)]
pub fn derive_host_layout(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match expand(&input) {
        Ok(tokens) => tokens.into(),
        Err(error) => error.into_compile_error().into(),
    }
}

#[proc_macro_derive(HostLayout)]
pub fn derive_host_layout_compat(input: TokenStream) -> TokenStream {
    derive_host_layout(input)
}

fn expand(input: &DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let name = &input.ident;
    let mut repr_c = false;
    let mut packed = false;
    for attr in &input.attrs {
        if !attr.path().is_ident("repr") {
            continue;
        }
        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("C") {
                repr_c = true;
            }
            if meta.path.is_ident("packed") {
                packed = true;
            }
            Ok(())
        })?;
    }
    if !repr_c {
        return Err(syn::Error::new_spanned(
            name,
            "HostLayout requires #[repr(C)]",
        ));
    }
    let Data::Struct(data) = &input.data else {
        return Err(syn::Error::new_spanned(
            name,
            "HostLayout supports structs only",
        ));
    };
    let Fields::Named(fields) = &data.fields else {
        return Err(syn::Error::new_spanned(
            name,
            "HostLayout requires named fields",
        ));
    };
    let field_names: Vec<_> = fields
        .named
        .iter()
        .map(|f| f.ident.as_ref().unwrap())
        .collect();
    let field_types: Vec<_> = fields
        .named
        .iter()
        .map(|f| lir_type(&f.ty))
        .collect::<syn::Result<_>>()?;
    let rust_types: Vec<_> = fields.named.iter().map(|f| &f.ty).collect();
    let descriptor = format_ident!("__{}_HOST_LAYOUT_FIELDS", name);
    let entries = field_names
        .iter()
        .zip(field_types.iter())
        .map(|(field, ty)| {
            let field_name = field.to_string();
            quote! { ::fp_core::HostFieldDescriptor { name: #field_name, ty: #ty } }
        });
    let getters = field_names
        .iter()
        .zip(rust_types.iter())
        .map(|(field, ty)| {
            let getter = format_ident!("get_{}", field);
            quote! {
                pub fn #getter(&self) -> &#ty { &self.#field }
            }
        });
    let setters = field_names
        .iter()
        .zip(rust_types.iter())
        .map(|(field, ty)| {
            let setter = format_ident!("set_{}", field);
            quote! {
                pub fn #setter(&mut self, value: #ty) { self.#field = value; }
            }
        });
    Ok(quote! {
        const #descriptor: &[::fp_core::HostFieldDescriptor] = &[#(#entries),*];
        impl ::fp_core::HostLayout for #name {
            const DESCRIPTOR: ::fp_core::HostLayoutDescriptor = ::fp_core::HostLayoutDescriptor::new(
                stringify!(#name), #descriptor, #packed,
            );
        }
        impl #name {
            pub fn as_raw(&self) -> *const Self {
                self as *const Self
            }

            pub fn as_raw_mut(&mut self) -> *mut Self {
                self as *mut Self
            }

            #(#getters)*
            #(#setters)*
        }
    })
}

fn lir_type(ty: &Type) -> syn::Result<proc_macro2::TokenStream> {
    match ty {
        Type::Path(path) if path.qself.is_none() && path.path.segments.len() == 1 => {
            let ident = path.path.segments.first().unwrap().ident.to_string();
            let token = match ident.as_str() {
                "bool" => quote!(::fp_core::lir::LirType::I1),
                "i8" | "u8" => quote!(::fp_core::lir::LirType::I8),
                "i16" | "u16" => quote!(::fp_core::lir::LirType::I16),
                "i32" | "u32" => quote!(::fp_core::lir::LirType::I32),
                "i64" | "u64" => quote!(::fp_core::lir::LirType::I64),
                "i128" | "u128" => quote!(::fp_core::lir::LirType::I128),
                "f32" => quote!(::fp_core::lir::LirType::F32),
                "f64" => quote!(::fp_core::lir::LirType::F64),
                _ => {
                    return Err(syn::Error::new_spanned(
                        ty,
                        "unsupported HostLayout field type",
                    ));
                }
            };
            Ok(token)
        }
        Type::Ptr(ptr) => {
            let inner = lir_type(&ptr.elem)?;
            Ok(quote!(::fp_core::lir::LirType::Ptr(Box::new(#inner))))
        }
        Type::Array(array) => {
            let inner = lir_type(&array.elem)?;
            let len = &array.len;
            Ok(quote!(::fp_core::lir::LirType::Array(Box::new(#inner), (#len) as u64)))
        }
        _ => Err(syn::Error::new_spanned(
            ty,
            "unsupported HostLayout field type",
        )),
    }
}
