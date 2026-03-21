use std::collections::HashMap;
use std::hosts::{Host, Inventory};

const fn inventory() -> Inventory {
    Inventory {
        groups: HashMap::from([]),
        hosts: HashMap::from([
            ("win-1", Host {
                transport: "winrm",
                address: "10.0.0.44",
                user: "Administrator",
                password: "secret",
                port: 5986,
                scheme: "https",
            }),
        ]),
    }
}
