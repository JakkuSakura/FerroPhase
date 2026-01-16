use cfg_expr::targets::{Family, TargetInfo, get_builtin_target_by_triple};
use cfg_expr::{Expression, Predicate};
use eyre::Result;
use target_lexicon::HOST;

pub struct TargetContext {
    triple: String,
    target: &'static TargetInfo,
}

impl TargetContext {
    pub fn from_env(target_override: Option<&str>) -> Result<Self> {
        let triple = target_override
            .map(|value| value.to_string())
            .or_else(|| std::env::var("MAGNET_TARGET").ok())
            .unwrap_or_else(|| HOST.to_string());
        Self::from_triple(&triple)
    }

    pub fn from_triple(triple: &str) -> Result<Self> {
        let target = get_builtin_target_by_triple(triple)
            .ok_or_else(|| eyre::eyre!("unknown target triple '{}'", triple))?;
        Ok(Self {
            triple: triple.to_string(),
            target,
        })
    }

    pub fn is_active(&self, spec: &str) -> Result<bool> {
        let trimmed = spec.trim();
        if let Some(expr) = strip_cfg_expression(trimmed) {
            return Ok(eval_cfg_expr(expr, self.target));
        }
        Ok(trimmed == self.triple)
    }
}

fn strip_cfg_expression(spec: &str) -> Option<&str> {
    let trimmed = spec.trim();
    let inner = trimmed.strip_prefix("cfg(")?;
    let inner = inner.strip_suffix(')')?;
    Some(inner.trim())
}

fn eval_cfg_expr(expr: &str, target: &TargetInfo) -> bool {
    let Ok(expression) = Expression::parse(expr) else {
        return false;
    };
    expression.eval(|pred| match pred {
        Predicate::Target(tp) => tp.matches(target),
        Predicate::Flag(flag) => flag_matches(flag, target),
        _ => false,
    })
}

fn flag_matches(flag: &str, target: &TargetInfo) -> bool {
    match flag {
        "unix" => target.families.contains(&Family::unix),
        "windows" => target.families.contains(&Family::windows),
        "wasm" => target.families.contains(&Family::wasm),
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cfg_windows_is_inactive_on_linux() -> Result<()> {
        let ctx = TargetContext::from_triple("x86_64-unknown-linux-gnu")?;
        assert!(!ctx.is_active("cfg(windows)")?);
        assert!(ctx.is_active("cfg(unix)")?);
        Ok(())
    }

    #[test]
    fn triple_match_requires_exact_match() -> Result<()> {
        let ctx = TargetContext::from_triple("x86_64-unknown-linux-gnu")?;
        assert!(ctx.is_active("x86_64-unknown-linux-gnu")?);
        assert!(!ctx.is_active("x86_64-unknown-linux-musl")?);
        Ok(())
    }
}
