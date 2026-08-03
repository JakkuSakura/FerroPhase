#[op = "yaml_to_json"]
pub fn to_json(input: &str) -> str { intrinsic_to_json(input) }

#[intrinsic = "yaml_to_json"]
fn intrinsic_to_json(input: &str) -> str { compile_error!("compiler intrinsic") }

pub fn parse(input: &str) -> std::json::Value {
    std::json::parse(to_json(input))
}
