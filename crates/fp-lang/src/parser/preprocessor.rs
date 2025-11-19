//! Preprocessor for FerroPhase sugar: quote/splice
//!
//! Design:
//! - Strategy-like pipeline of rewrite rules, each implementing `Rule`.
//! - Preprocessor applies rules sequentially to the whole source (string → string),
//!   keeping the implementation simple and easy to extend with more rules.

pub struct Preprocessor {
    rules: Vec<Box<dyn Rule + Send + Sync>>,    
}

impl Default for Preprocessor {
    fn default() -> Self {
        let mut rules: Vec<Box<dyn Rule + Send + Sync>> = Vec::new();
        rules.push(Box::new(QuoteRule));
        rules.push(Box::new(SpliceRule));
        Self { rules }
    }
}

impl Preprocessor {
    pub fn new(rules: Vec<Box<dyn Rule + Send + Sync>>) -> Self { Self { rules } }

    /// Apply all rules in sequence.
    pub fn apply(&self, source: &str) -> String {
        let mut cur = source.to_owned();
        for rule in &self.rules {
            cur = rule.rewrite(&cur);
        }
        cur
    }

    /// Apply rules repeatedly until no changes occur or max_passes reached.
    pub fn apply_until_stable(&self, source: &str, max_passes: usize) -> String {
        let mut out = source.to_owned();
        for _ in 0..max_passes {
            let next = self.apply(&out);
            if next == out { break; }
            out = next;
        }
        out
    }
}

/// A rewrite rule that maps input to output source.
pub trait Rule {
    fn rewrite(&self, input: &str) -> String;
}

/// Builder for composing preprocessors in a fluent manner.
pub struct PreprocessorBuilder {
    rules: Vec<Box<dyn Rule + Send + Sync>>,
}

impl PreprocessorBuilder {
    pub fn new() -> Self { Self { rules: Vec::new() } }
    pub fn with_default_rules() -> Self {
        Self::new().add_rule(Box::new(QuoteRule)).add_rule(Box::new(SpliceRule))
    }
    pub fn add_rule(mut self, rule: Box<dyn Rule + Send + Sync>) -> Self {
        self.rules.push(rule);
        self
    }
    pub fn build(self) -> Preprocessor { Preprocessor::new(self.rules) }
}

/// Utility: scan balanced block delimited by open/close. Returns end index of matching close.
fn scan_balanced(bytes: &[u8], start: usize, open: u8, close: u8) -> Option<usize> {
    let mut i = start;
    let mut depth: i32 = 0;
    while i < bytes.len() {
        match bytes[i] {
            b if b == open => depth += 1,
            b if b == close => { depth -= 1; if depth == 0 { return Some(i); } },
            _ => {}
        }
        i += 1;
    }
    None
}

fn is_ident_char(b: u8) -> bool { b.is_ascii_alphanumeric() || b == b'_' }
fn skip_ws(bytes: &[u8], mut idx: usize) -> usize { while idx < bytes.len() && bytes[idx].is_ascii_whitespace() { idx += 1; } idx }

/// Rewrite `quote [<kind>] { ... }` to `fp_quote!({ ... })`. Ignores kind tokens.
struct QuoteRule;
impl Rule for QuoteRule {
    fn rewrite(&self, input: &str) -> String {
        let bytes = input.as_bytes();
        let mut out = String::with_capacity(input.len() + 8);
        let mut i = 0;
        while i < bytes.len() {
            // Skip strings and comments verbatim
            if let Some(next) = skip_string_or_comment(input, i) {
                out.push_str(&input[i..next]);
                i = next;
                continue;
            }
            if bytes[i].is_ascii_alphabetic() || bytes[i] == b'_' {
                let ident_start = i; i += 1; while i < bytes.len() && is_ident_char(bytes[i]) { i += 1; }
                let ident = &input[ident_start..i];
                if ident == "quote" {
                    let mut j = skip_ws(bytes, i);
                    // Optional kind ident
                    if j < bytes.len() && (bytes[j].is_ascii_alphabetic() || bytes[j] == b'_') {
                        j += 1; while j < bytes.len() && is_ident_char(bytes[j]) { j += 1; }
                        j = skip_ws(bytes, j);
                    }
                    if j < bytes.len() && bytes[j] == b'{' {
                        if let Some(end) = scan_balanced(bytes, j, b'{', b'}') {
                            out.push_str("fp_quote!(");
                            out.push_str(&input[j..=end]);
                            out.push(')');
                            i = end + 1; // continue from after the block
                            continue;
                        }
                    }
                    // Fallback: emit the original ident
                    out.push_str(ident);
                    continue;
                }
                out.push_str(ident);
                continue;
            }
            out.push(bytes[i] as char); i += 1;
        }
        out
    }
}

/// Rewrite `splice ( ... )` to `fp_splice!( ... )`.
struct SpliceRule;
impl Rule for SpliceRule {
    fn rewrite(&self, input: &str) -> String {
        let bytes = input.as_bytes();
        let mut out = String::with_capacity(input.len() + 8);
        let mut i = 0;
        while i < bytes.len() {
            // Skip strings and comments verbatim
            if let Some(next) = skip_string_or_comment(input, i) {
                out.push_str(&input[i..next]);
                i = next;
                continue;
            }
            if bytes[i].is_ascii_alphabetic() || bytes[i] == b'_' {
                let ident_start = i; i += 1; while i < bytes.len() && is_ident_char(bytes[i]) { i += 1; }
                let ident = &input[ident_start..i];
                if ident == "splice" {
                    let j = skip_ws(bytes, i);
                    if j < bytes.len() && bytes[j] == b'(' {
                        if let Some(end) = scan_balanced(bytes, j, b'(', b')') {
                            out.push_str("fp_splice!(");
                            out.push_str(&input[j+1..end]);
                            out.push(')');
                            i = end + 1; continue;
                        }
                    }
                    out.push_str(ident); continue;
                }
                out.push_str(ident); continue;
            }
            out.push(bytes[i] as char); i += 1;
        }
        out
    }
}

/// Skip over strings and comments so keyword rewrites do not affect them.
/// Returns Some(next_index) if a skip occurred starting at `i`, otherwise None.
fn skip_string_or_comment(input: &str, i: usize) -> Option<usize> {
    let bytes = input.as_bytes();
    let n = bytes.len();
    if i >= n { return None; }

    // Line comment: // ... \n
    if bytes[i] == b'/' && i + 1 < n && bytes[i + 1] == b'/' {
        let mut j = i + 2;
        while j < n && bytes[j] != b'\n' { j += 1; }
        return Some(j);
    }

    // Block comment (nested): /* ... */
    if bytes[i] == b'/' && i + 1 < n && bytes[i + 1] == b'*' {
        let mut j = i + 2;
        let mut depth: i32 = 1;
        while j < n {
            if j + 1 < n && bytes[j] == b'/' && bytes[j + 1] == b'*' {
                depth += 1; j += 2; continue;
            }
            if j + 1 < n && bytes[j] == b'*' && bytes[j + 1] == b'/' {
                depth -= 1; j += 2; if depth == 0 { return Some(j); } else { continue; }
            }
            j += 1;
        }
        return Some(n);
    }

    // Raw string: r#*"..."#*
    // Also support byte-raw: br#*"..."#*
    // Handle prefix b? r #* "
    let (r_start, _consumed_b_prefix) = if bytes[i] == b'r' {
        (i, false)
    } else if bytes[i] == b'b' && i + 1 < n && bytes[i + 1] == b'r' {
        (i + 1, true)
    } else { (usize::MAX, false) };

    if r_start != usize::MAX {
        let mut j = r_start + 1;
        // Count optional #s
        let mut hashes = 0usize;
        while j < n && bytes[j] == b'#' { hashes += 1; j += 1; }
        if j < n && bytes[j] == b'"' {
            j += 1; // after opening quote
            // Find closing "#*
            loop {
                if j >= n { return Some(n); }
                if bytes[j] == b'"' {
                    // Check matching number of #
                    let mut k = j + 1;
                    let mut matched = true;
                    for _ in 0..hashes {
                        if k < n && bytes[k] == b'#' { k += 1; } else { matched = false; break; }
                    }
                    if matched {
                        return Some(k);
                    }
                }
                j += 1;
            }
        }
        // Not a raw string actually; fall through (but include the 'b' prefix if present)
        // no-op
    }

    // Byte string: b"..."
    if bytes[i] == b'b' && i + 1 < n && bytes[i + 1] == b'"' {
        let mut j = i + 2;
        while j < n {
            match bytes[j] {
                b'\\' => { j += 2; },
                b'"' => { return Some(j + 1); },
                _ => j += 1,
            }
        }
        return Some(n);
    }

    // Normal string: "..."
    if bytes[i] == b'"' {
        let mut j = i + 1;
        while j < n {
            match bytes[j] {
                b'\\' => { j += 2; },
                b'"' => { return Some(j + 1); },
                _ => j += 1,
            }
        }
        return Some(n);
    }

    // Char literal: '\'' ... '\'' (handle escapes)
    if bytes[i] == b'\'' {
        let mut j = i + 1;
        while j < n {
            match bytes[j] {
                b'\\' => { j += 2; },
                b'\'' => { return Some(j + 1); },
                _ => j += 1,
            }
        }
        return Some(n);
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn quote_without_kind_rewrites_to_macro() {
        let pre = Preprocessor::default();
        let src = "fn main() { let x = quote { 1 + 2 }; }";
        let out = pre.apply(src);
        assert!(out.contains("fp_quote!("));
        assert!(out.contains("{ 1 + 2 }"));
    }

    #[test]
    fn quote_stmt_alias_is_ignored() {
        let pre = Preprocessor::default();
        let src = "fn main() { let x = quote stmt { let y = 3; }; }";
        let out = pre.apply(src);
        assert!(out.contains("fp_quote!("));
        assert!(!out.contains("quote stmt"));
    }

    #[test]
    fn splice_paren_rewrites_to_macro() {
        let pre = Preprocessor::default();
        let src = "fn main() { splice ( some_token ) }";
        let out = pre.apply(src);
        assert!(out.contains("fp_splice!( some_token )"));
    }

    #[test]
    fn nested_quote_and_splice_rewrite_stably() {
        let pre = PreprocessorBuilder::with_default_rules().build();
        let src = "fn main() { let a = quote { splice ( x ) }; }";
        let once = pre.apply(&src);
        let stable = pre.apply_until_stable(&src, 4);
        assert_eq!(once, stable, "should stabilize within one pass");
        assert!(stable.contains("fp_quote!("));
        assert!(stable.contains("fp_splice!( x )"));
    }

    #[test]
    fn does_not_rewrite_inside_strings_or_comments() {
        let pre = Preprocessor::default();
        // inside string
        let src = r#"fn main() { let s = "quote { 1 + 2 }"; /* splice ( x ) */ }"#;
        let out = pre.apply(src);
        // Both quote/splice occurrences are inside string/comment ⇒ unchanged, no macros injected
        assert!(!out.contains("fp_quote!("));
        assert!(!out.contains("fp_splice!("));

        // raw string
        let src = r##"fn main() { let s = r#"splice ( x )"#; }"##;
        let out = pre.apply(src);
        assert!(!out.contains("fp_splice!("));
    }
}
