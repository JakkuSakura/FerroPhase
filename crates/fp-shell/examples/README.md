# `fp-shell` examples

Compile a Ferro shell program to Bash:

```bash
cargo run -p fp-shell -- compile crates/fp-shell/examples/transports.fp \
  --inventory crates/fp-shell/examples/transports.inventory.fp \
  --target bash
```

Compile the same program to PowerShell:

```bash
cargo run -p fp-shell -- compile crates/fp-shell/examples/transports.fp \
  --inventory crates/fp-shell/examples/transports.inventory.fp \
  --target powershell
```

Inventory is expressed as Ferro source under `std::hosts` by defining
`const fn inventory() -> Inventory`.

Minimal sketch:

```fp
use std::collections::HashMap;
use std::hosts::{Host, Inventory};

const fn inventory() -> Inventory {
    Inventory {
        groups: HashMap::from([("web", ["web-1", "web-2"])]),
        hosts: HashMap::from([
            ("web-1", Host { transport: "ssh", address: "10.0.0.11", user: "deploy" }),
            ("web-2", Host { transport: "ssh", address: "10.0.0.12", user: "deploy" }),
        ]),
    }
}
```

- `local` runs on the machine executing the generated script.
- `ssh` uses `ssh` and `scp` in Bash, and `ssh` and `scp` in PowerShell.
- `docker` uses `docker exec` and `docker cp`.
- `kubectl` uses `kubectl exec` and `kubectl cp`.
- `winrm` uses local `pwsh` remoting in Bash and `New-PSSession` in PowerShell.

Pyinfra-style helpers now live under:

- `std::facts::server`, `std::facts::files`, `std::facts::systemd`, `std::facts::git`, `std::facts::docker`
- `std::ops::systemd`, `std::ops::git`, `std::ops::packages`, `std::ops::server_utils`

Current limits:

- `rsync` uses native local `rsync` CLI. For remote hosts it resolves the endpoint from host
  credentials like `user`, `address`, and `port`, even if command transport is `winrm`.
- Hosts without an rsync-reachable endpoint, such as plain `docker` or `kubectl` entries without
  address/user data, cannot use `rsync`.
- WinRM requires local `pwsh` plus `password` in inventory for non-interactive execution.
