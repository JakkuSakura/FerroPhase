use fp_core::ast::{Abi, FunctionParamReceiver, FunctionSignature, Value};
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::sync::Mutex;

pub const JIT_ABI_VERSION: u16 = 1;

#[repr(C)]
pub struct FpJitArgs {
    pub args: *const Value,
    pub len: u32,
}

#[repr(C)]
pub struct FpJitContext {
    _private: (),
}

impl FpJitContext {
    pub fn new() -> Self {
        Self { _private: () }
    }
}

#[allow(improper_ctypes_definitions)]
pub type FpJitFn = unsafe extern "C" fn(*mut FpJitContext, FpJitArgs) -> Value;

#[derive(Clone, Copy, Debug)]
pub struct JitEntry {
    fn_ptr: FpJitFn,
}

impl JitEntry {
    pub fn new(fn_ptr: FpJitFn) -> Self {
        Self { fn_ptr }
    }

    pub unsafe fn call(&self, ctx: &mut FpJitContext, args: &[Value]) -> Result<Value, JitError> {
        let len = u32::try_from(args.len()).map_err(|_| JitError::new("too many arguments"))?;
        let args = FpJitArgs {
            args: args.as_ptr(),
            len,
        };
        Ok((self.fn_ptr)(ctx as *mut FpJitContext, args))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct JitKey {
    pub canonical_name: String,
    pub sig_hash: u64,
    pub abi: u16,
}

impl JitKey {
    pub fn new(canonical_name: impl Into<String>, sig: &FunctionSignature, abi: u16) -> Self {
        Self {
            canonical_name: canonical_name.into(),
            sig_hash: signature_hash(sig),
            abi,
        }
    }

    pub fn from_signature(canonical_name: impl Into<String>, sig: &FunctionSignature) -> Self {
        Self::new(canonical_name, sig, JIT_ABI_VERSION)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct JitError {
    pub message: String,
}

impl JitError {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct JitOptions {
    pub enabled: bool,
    pub hot_threshold: u32,
    pub abi_version: u16,
}

impl Default for JitOptions {
    fn default() -> Self {
        Self {
            enabled: false,
            hot_threshold: 32,
            abi_version: JIT_ABI_VERSION,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum JitCallOutcome {
    Hit(Value),
    Miss,
    Disabled,
    Fault(JitError),
}

#[derive(Default)]
pub struct JitRegistry {
    entries: HashMap<JitKey, JitEntry>,
}

impl JitRegistry {
    pub fn lookup(&self, key: &JitKey) -> Option<JitEntry> {
        self.entries.get(key).copied()
    }

    pub fn insert(&mut self, key: JitKey, entry: JitEntry) -> Option<JitEntry> {
        self.entries.insert(key, entry)
    }

    pub fn invalidate_exact(&mut self, key: &JitKey) -> Option<JitEntry> {
        self.entries.remove(key)
    }

    pub fn invalidate_prefix(&mut self, prefix: &str) -> usize {
        let before = self.entries.len();
        self.entries
            .retain(|key, _| !key.canonical_name.starts_with(prefix));
        before - self.entries.len()
    }
}

pub struct JitSession {
    options: JitOptions,
    registry: Mutex<JitRegistry>,
    hotness: Mutex<HashMap<JitKey, u32>>,
    pending: Mutex<Vec<JitKey>>,
}

impl JitSession {
    pub fn new(options: JitOptions) -> Self {
        Self {
            options,
            registry: Mutex::new(JitRegistry::default()),
            hotness: Mutex::new(HashMap::new()),
            pending: Mutex::new(Vec::new()),
        }
    }

    pub fn options(&self) -> &JitOptions {
        &self.options
    }

    pub fn is_enabled(&self) -> bool {
        self.options.enabled
    }

    pub fn lookup(&self, key: &JitKey) -> Option<JitEntry> {
        let registry = self.registry.lock().unwrap();
        registry.lookup(key)
    }

    pub fn register_entry(&self, key: JitKey, entry: JitEntry) {
        let mut registry = self.registry.lock().unwrap();
        registry.insert(key, entry);
    }

    pub fn try_call(&self, key: &JitKey, args: &[Value]) -> JitCallOutcome {
        if !self.is_enabled() {
            return JitCallOutcome::Disabled;
        }
        let entry = match self.lookup(key) {
            Some(entry) => entry,
            None => return JitCallOutcome::Miss,
        };
        let mut ctx = FpJitContext::new();
        match unsafe { entry.call(&mut ctx, args) } {
            Ok(value) => JitCallOutcome::Hit(value),
            Err(err) => JitCallOutcome::Fault(err),
        }
    }

    pub fn record_call(&self, key: JitKey) -> bool {
        if !self.is_enabled() {
            return false;
        }
        if self.lookup(&key).is_some() {
            return false;
        }
        let threshold = self.options.hot_threshold.max(1);
        let mut hotness = self.hotness.lock().unwrap();
        let count = hotness.entry(key.clone()).or_insert(0);
        *count += 1;
        if *count >= threshold {
            *count = 0;
            self.enqueue_compile(key);
            return true;
        }
        false
    }

    pub fn enqueue_compile(&self, key: JitKey) {
        let mut pending = self.pending.lock().unwrap();
        if pending.iter().any(|existing| existing == &key) {
            return;
        }
        pending.push(key);
    }

    pub fn take_pending(&self) -> Vec<JitKey> {
        let mut pending = self.pending.lock().unwrap();
        std::mem::take(&mut *pending)
    }
}

pub fn signature_hash(sig: &FunctionSignature) -> u64 {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    hash_signature(sig, &mut hasher);
    hasher.finish()
}

fn hash_signature(sig: &FunctionSignature, hasher: &mut impl Hasher) {
    match &sig.abi {
        Abi::Rust => 0u8.hash(hasher),
        Abi::Named(name) => {
            1u8.hash(hasher);
            name.hash(hasher);
        }
    }

    if let Some(receiver) = sig.receiver.as_ref() {
        receiver_tag(receiver).hash(hasher);
    } else {
        0u8.hash(hasher);
    }

    for param in &sig.params {
        param.ty.to_string().hash(hasher);
        param.is_const.hash(hasher);
        param.is_context.hash(hasher);
        param.as_tuple.hash(hasher);
        param.as_dict.hash(hasher);
        param.positional_only.hash(hasher);
        param.keyword_only.hash(hasher);
    }

    if let Some(ret_ty) = sig.ret_ty.as_ref() {
        ret_ty.to_string().hash(hasher);
    } else {
        0u8.hash(hasher);
    }
}

fn receiver_tag(receiver: &FunctionParamReceiver) -> u8 {
    match receiver {
        FunctionParamReceiver::Implicit => 0,
        FunctionParamReceiver::Value => 1,
        FunctionParamReceiver::MutValue => 2,
        FunctionParamReceiver::Ref => 3,
        FunctionParamReceiver::RefStatic => 4,
        FunctionParamReceiver::RefMut => 5,
        FunctionParamReceiver::RefMutStatic => 6,
    }
}
