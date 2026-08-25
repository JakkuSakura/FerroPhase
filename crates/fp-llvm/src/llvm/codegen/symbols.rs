use super::*;

impl<'a> LirCodegen<'a> {
    pub(super) fn llvm_symbol_for(&mut self, original: &str) -> String {
        if let Some(existing) = self.symbol_names.get(original) {
            return existing.clone();
        }

        let sanitized = Self::sanitize_symbol(original);
        self.symbol_names
            .insert(original.to_string(), sanitized.clone());
        sanitized
    }

    fn sanitize_symbol(name: &str) -> String {
        let mut result = String::with_capacity(name.len());
        for ch in name.chars() {
            match ch {
                'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '.' | '$' => result.push(ch),
                _ => result.push('_'),
            }
        }

        if result.is_empty() {
            return "_sym".to_string();
        }

        let is_valid_start = result
            .chars()
            .next()
            .map(|c| matches!(c, 'a'..='z' | 'A'..='Z' | '_' | '$'))
            .unwrap_or(false);
        if !is_valid_start {
            result.insert(0, '_');
        }

        result
    }
}
