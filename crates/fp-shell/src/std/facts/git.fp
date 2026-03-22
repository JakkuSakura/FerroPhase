pub const fn head(path: str) -> str {
    std::shell::process::output(f"git -C {path} rev-parse HEAD")
}

pub const fn branch(path: str) -> str {
    std::shell::process::output(f"git -C {path} rev-parse --abbrev-ref HEAD")
}

pub const fn origin_url(path: str) -> str {
    std::shell::process::output(f"git -C {path} remote get-url origin")
}

pub const fn working_tree_status(path: str) -> str {
    std::shell::process::output(f"git -C {path} status --short")
}
