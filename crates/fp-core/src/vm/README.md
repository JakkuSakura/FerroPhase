# SHLL Virtual Machine
This is an VM implementation for SHLL. The main purpose is to support unsafe and pointer operations in a interpreted and safe manner.

```rust
use std::collections::HashMap;
use bytes::Bytes;

pub struct VmStorage {
    pub stack: Bytes,
    pub heap: Bytes,
    pub objects: HashMap<i64, Value>
}
```


