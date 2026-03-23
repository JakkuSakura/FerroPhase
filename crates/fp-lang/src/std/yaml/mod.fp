extern "host" fn to_json_impl(input: str) -> str;

pub fn to_json(input: &str) -> str {
    to_json_impl(input)
}

pub fn parse(input: &str) -> std::json::JsonValue {
    std::json::parse(to_json_impl(input))
}
