use std::env;

pub struct Config {
    root: str,
    pattern: str,
    keyword: str,
    fail_fast: bool,
    max_fail: i64,
    verbose: bool,
    quiet: bool,
    capture: bool,
}

impl Config {
    pub fn root(&self) -> &str {
        &self.root
    }

    pub fn pattern(&self) -> &str {
        &self.pattern
    }

    pub fn keyword(&self) -> &str {
        &self.keyword
    }

    pub fn fail_fast(&self) -> bool {
        self.fail_fast
    }

    pub fn max_fail(&self) -> i64 {
        self.max_fail
    }

    pub fn verbose(&self) -> bool {
        self.verbose
    }

    pub fn quiet(&self) -> bool {
        self.quiet
    }

    pub fn capture(&self) -> bool {
        self.capture
    }
}

fn env_str(name: &str, fallback: &str) -> str {
    if env::exists(name) {
        env::var(name)
    } else {
        fallback
    }
}

fn env_bool(name: &str, fallback: bool) -> bool {
    if !env::exists(name) {
        return fallback;
    }
    let value = env::var(name);
    value == "1"
        || value == "true"
        || value == "TRUE"
        || value == "yes"
        || value == "YES"
        || value == "on"
        || value == "ON"
}

fn digit_value(ch: &str) -> i64 {
    match ch {
        "0" => 0,
        "1" => 1,
        "2" => 2,
        "3" => 3,
        "4" => 4,
        "5" => 5,
        "6" => 6,
        "7" => 7,
        "8" => 8,
        "9" => 9,
        _ => -1,
    }
}

fn parse_i64(value: &str, fallback: i64) -> i64 {
    if value == "" {
        return fallback;
    }
    let mut idx = 0;
    let mut sign = 1;
    if value.starts_with("-") {
        sign = -1;
        idx = 1;
    }
    let mut number = 0;
    while idx < value.len() {
        let ch = value[idx..idx + 1];
        let digit = digit_value(ch);
        if digit < 0 {
            return fallback;
        }
        number = number * 10 + digit;
        idx = idx + 1;
    }
    number * sign
}

pub fn from_env() -> Config {
    let root = env_str("FPTEST_ROOT", "tests");
    let pattern = env_str("FPTEST_PATTERN", "");
    let keyword = env_str("FPTEST_K", "");
    let fail_fast = env_bool("FPTEST_FAIL_FAST", false);
    let verbose = env_bool("FPTEST_VERBOSE", false);
    let quiet = env_bool("FPTEST_QUIET", false);
    let capture = env_bool("FPTEST_CAPTURE", true);
    let max_fail = if env::exists("FPTEST_MAX_FAIL") {
        parse_i64(env::var("FPTEST_MAX_FAIL"), 0)
    } else {
        0
    };

    Config {
        root,
        pattern,
        keyword,
        fail_fast,
        max_fail,
        verbose,
        quiet,
        capture,
    }
}

pub fn default_config() -> Config {
    Config {
        root: "tests",
        pattern: "",
        keyword: "",
        fail_fast: false,
        max_fail: 0,
        verbose: false,
        quiet: false,
        capture: true,
    }
}
