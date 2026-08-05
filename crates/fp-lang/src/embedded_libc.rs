use std::path::{Component, Path, PathBuf};

const MOD_SOURCE: &str = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/src/libc/mod.fp"));
const LINUX_SOURCE: &str = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/src/libc/linux.fp"));
const MACOS_SOURCE: &str = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/src/libc/macos.fp"));
const IOS_SOURCE: &str = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/src/libc/ios.fp"));
const FREEBSD_SOURCE: &str = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/src/libc/freebsd.fp"));
const WINDOWS_SOURCE: &str = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/src/libc/windows.fp"));

const VIRTUAL_ROOT: &str = "<fp-lang-libc>";

pub fn root_dir() -> PathBuf {
    PathBuf::from(VIRTUAL_ROOT)
}

pub fn read(path: &Path) -> Option<&'static str> {
    let relative = path.strip_prefix(Path::new(VIRTUAL_ROOT)).ok()?;
    let normalized = normalize_relative_path(relative)?;
    match normalized.as_str() {
        "mod.fp" => Some(MOD_SOURCE),
        "linux.fp" => Some(LINUX_SOURCE),
        "macos.fp" => Some(MACOS_SOURCE),
        "ios.fp" => Some(IOS_SOURCE),
        "freebsd.fp" => Some(FREEBSD_SOURCE),
        "windows.fp" => Some(WINDOWS_SOURCE),
        _ => None,
    }
}

pub fn module_paths() -> &'static [&'static str] {
    #[cfg(target_os = "linux")]
    {
        &["mod.fp", "linux.fp"]
    }
    #[cfg(target_os = "macos")]
    {
        &["mod.fp", "macos.fp"]
    }
    #[cfg(target_os = "ios")]
    {
        &["mod.fp", "ios.fp"]
    }
    #[cfg(target_os = "freebsd")]
    {
        &["mod.fp", "freebsd.fp"]
    }
    #[cfg(target_os = "windows")]
    {
        &["mod.fp", "windows.fp"]
    }
    #[cfg(not(any(
        target_os = "linux",
        target_os = "macos",
        target_os = "ios",
        target_os = "freebsd",
        target_os = "windows"
    )))]
    {
        &["mod.fp"]
    }
}

fn normalize_relative_path(path: &Path) -> Option<String> {
    let mut parts = Vec::new();
    for component in path.components() {
        match component {
            Component::Normal(part) => parts.push(part.to_str()?.to_owned()),
            Component::CurDir => {}
            Component::RootDir | Component::ParentDir | Component::Prefix(_) => return None,
        }
    }
    if parts.is_empty() {
        return None;
    }
    Some(parts.join("/"))
}
