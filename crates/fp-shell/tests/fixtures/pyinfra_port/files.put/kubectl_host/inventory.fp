use std::collections::HashMap;
use std::hosts::{Host, Inventory};

const fn inventory() -> Inventory {
    Inventory {
        groups: HashMap::from([]),
        hosts: HashMap::from([
            ("api", Host {
                transport: "kubectl",
                pod: "api-pod",
                namespace: "prod",
                context: "staging-cluster",
            }),
        ]),
    }
}
