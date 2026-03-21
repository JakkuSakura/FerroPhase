use std::collections::HashMap;
use std::hosts::{Host, Inventory};

const fn inventory() -> Inventory {
    Inventory {
        groups: HashMap::from([]),
        hosts: HashMap::from([
            ("web-1", Host {
                transport: "ssh",
                address: "10.0.0.21",
                user: "deploy",
                port: 2222,
            }),
        ]),
    }
}
