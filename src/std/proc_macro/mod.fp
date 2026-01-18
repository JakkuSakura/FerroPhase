pub enum Delimiter {
    Parenthesis,
    Brace,
    Bracket,
}

pub struct Group {
    delimiter: Delimiter,
    tokens: Vec<TokenTree>,
}

pub struct Ident {
    text: str,
}

pub struct Punct {
    text: str,
}

pub struct Literal {
    text: str,
}

pub enum TokenTree {
    Token(str),
    Group(Group),
}

pub struct TokenStream {}

pub const fn token_stream_from_str(text: str) -> TokenStream {
    std::proc_macro::token_stream_from_str(text)
}

pub const fn token_stream_to_string(stream: TokenStream) -> str {
    std::proc_macro::token_stream_to_string(stream)
}

impl TokenStream {
    pub fn from_str(text: str) -> TokenStream {
        std::proc_macro::token_stream_from_str(text)
    }

    pub fn to_string(self) -> str {
        std::proc_macro::token_stream_to_string(self)
    }
}
