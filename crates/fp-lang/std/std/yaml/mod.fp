#[op(func = "yaml_to_json")]
pub fn to_json(input: &str) -> str { ::std::intrinsics::yaml::to_json(input) }

pub fn parse(input: &str) -> ::std::json::Value {
    ::std::json::parse(to_json(input))
}
