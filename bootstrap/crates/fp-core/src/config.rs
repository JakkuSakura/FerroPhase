use std::sync::OnceLock;

fn env_true(key: &str) -> Option<bool> {
    std::env::var(key).ok().map(|val| {
        let trimmed = val.trim();
        !trimmed.is_empty() && !matches!(trimmed, "0" | "false" | "FALSE" | "False")
    })
}

fn bool_from_env(key: &str) -> bool {
    env_true(key).unwrap_or(false)
}

pub fn lossy_mode() -> bool {
    static LOSSY: OnceLock<bool> = OnceLock::new();
    *LOSSY.get_or_init(|| bool_from_env("FERROPHASE_LOSSY"))
}
