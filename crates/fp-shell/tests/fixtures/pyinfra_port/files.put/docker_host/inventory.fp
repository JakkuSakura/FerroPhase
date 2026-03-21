use std::collections::HashMap;
use std::hosts::{Host, Inventory};

const fn inventory() -> Inventory {
    Inventory {
        groups: HashMap::from([]),
        hosts: HashMap::from([
            ("app", Host {
                transport: "docker",
                container: "app-container",
            }),
        ]),
    }
}
