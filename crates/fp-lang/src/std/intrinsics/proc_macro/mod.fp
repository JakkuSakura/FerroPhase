#[intrinsic = "proc_macro_token_stream_from_str"]
pub const fn token_stream_from_str(text: str) -> ::std::proc_macro::TokenStream {
    compile_error!("token_stream_from_str is a compiler intrinsic")
}

#[intrinsic = "proc_macro_token_stream_to_string"]
pub const fn token_stream_to_string(stream: ::std::proc_macro::TokenStream) -> str {
    compile_error!("token_stream_to_string is a compiler intrinsic")
}
