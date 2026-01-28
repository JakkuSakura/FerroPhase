#[derive(Debug, Clone, Default)]
pub struct RegistryConfig {
    pub enabled: bool,
}

pub fn registry_disabled() -> crate::Result<()> {
    Err("registry resolution is not supported in bootstrap magnet".to_string().into())
}
