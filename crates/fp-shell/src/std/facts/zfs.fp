pub const fn pools() -> str {
    std::shell::process::output("zpool get -H all")
}

pub const fn datasets() -> str {
    std::shell::process::output("zfs get -H all")
}

pub const fn filesystems() -> str {
    std::shell::process::output("zfs get -H all | grep '\\ttype\\tfilesystem\\t' || true")
}

pub const fn snapshots() -> str {
    std::shell::process::output("zfs get -H all | grep '\\ttype\\tsnapshot\\t' || true")
}

pub const fn volumes() -> str {
    std::shell::process::output("zfs get -H all | grep '\\ttype\\tvolume\\t' || true")
}
