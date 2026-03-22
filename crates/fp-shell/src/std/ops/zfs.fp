pub const fn dataset(dataset_name: str, context hosts: str = "localhost", present: bool = true, recursive: bool, sparse: bool, volume_size: str, properties: str, sudo: bool) -> bool {
    if present {
        if recursive {
            if sparse {
                if volume_size == "" {
                    std::ops::server::shell(f"zfs create -p -s {properties} {dataset_name}", hosts=hosts, sudo=sudo)
                } else {
                    std::ops::server::shell(f"zfs create -p -s -V {volume_size} {properties} {dataset_name}", hosts=hosts, sudo=sudo)
                }
            } else {
                if volume_size == "" {
                    std::ops::server::shell(f"zfs create -p {properties} {dataset_name}", hosts=hosts, sudo=sudo)
                } else {
                    std::ops::server::shell(f"zfs create -p -V {volume_size} {properties} {dataset_name}", hosts=hosts, sudo=sudo)
                }
            }
        } else {
            if sparse {
                if volume_size == "" {
                    std::ops::server::shell(f"zfs create -s {properties} {dataset_name}", hosts=hosts, sudo=sudo)
                } else {
                    std::ops::server::shell(f"zfs create -s -V {volume_size} {properties} {dataset_name}", hosts=hosts, sudo=sudo)
                }
            } else {
                if volume_size == "" {
                    std::ops::server::shell(f"zfs create {properties} {dataset_name}", hosts=hosts, sudo=sudo)
                } else {
                    std::ops::server::shell(f"zfs create -V {volume_size} {properties} {dataset_name}", hosts=hosts, sudo=sudo)
                }
            }
        }
    } else {
        if recursive {
            std::ops::server::shell(f"zfs destroy -r {dataset_name}", hosts=hosts, sudo=sudo)
        } else {
            std::ops::server::shell(f"zfs destroy {dataset_name}", hosts=hosts, sudo=sudo)
        }
    }
}

pub const fn snapshot(snapshot_name: str, context hosts: str = "localhost", present: bool = true, recursive: bool, properties: str, sudo: bool) -> bool {
    if present {
        if recursive {
            std::ops::server::shell(f"zfs snap -r {properties} {snapshot_name}", hosts=hosts, sudo=sudo)
        } else {
            std::ops::server::shell(f"zfs snap {properties} {snapshot_name}", hosts=hosts, sudo=sudo)
        }
    } else {
        std::ops::zfs::dataset(snapshot_name, hosts=hosts, present=false, recursive=recursive, properties=properties, sudo=sudo)
    }
}

pub const fn volume(volume_name: str, size: str, context hosts: str = "localhost", sparse: bool, present: bool = true, recursive: bool, properties: str, sudo: bool) -> bool {
    std::ops::zfs::dataset(volume_name, hosts=hosts, present=present, recursive=recursive, sparse=sparse, volume_size=size, properties=properties, sudo=sudo)
}

pub const fn filesystem(fs_name: str, context hosts: str = "localhost", present: bool = true, recursive: bool, properties: str, sudo: bool) -> bool {
    std::ops::zfs::dataset(fs_name, hosts=hosts, present=present, recursive=recursive, properties=properties, sudo=sudo)
}
