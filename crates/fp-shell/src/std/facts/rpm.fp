pub const fn packages() -> str {
    std::shell::process::output("rpm --queryformat '%{NAME} %{VERSION}-%{RELEASE}\\n' -qa")
}

pub const fn package(package: str) -> str {
    std::shell::process::output(
        f"rpm --queryformat '%{{NAME}} %{{VERSION}}-%{{RELEASE}}\\n' -q {package} || ! test -e {package} || rpm --queryformat '%{{NAME}} %{{VERSION}}-%{{RELEASE}}\\n' -qp {package} 2>/dev/null"
    )
}

pub const fn provides(package: str) -> str {
    std::shell::process::output(
        f"repoquery --queryformat '%{{NAME}} %{{VERSION}}-%{{RELEASE}}\\n' --whatprovides {package} || true"
    )
}
