use std::collections::HashMap;
use std::hosts::{Host, Inventory};

const fn inventory() -> Inventory {
    Inventory {
        groups: HashMap::from([]),
        hosts: HashMap::from([
            ("builder", Host {
                transport: "chroot",
                chroot_directory: "/mnt/rootfs",
            }),
        ]),
    }
}
