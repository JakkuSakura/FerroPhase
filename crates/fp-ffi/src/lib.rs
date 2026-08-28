//! Lightweight FFI runtime for calling native C functions from
//! interpreted or JIT-compiled code.  Built on `dlopen`/`dlsym`.
//!
//! Shared by `fp-interpret` (comptime evaluation) and `fp-native`
//! (native codegen / JIT).

use std::collections::HashMap;
use std::ffi::{CStr, CString, c_char, c_void};

// ── error type ────────────────────────────────────────────────────

#[derive(Debug, thiserror::Error)]
pub enum FfiError {
    #[error("ffi: {0}")]
    Message(String),
    #[error("symbol '{0}' not found")]
    SymbolNotFound(String),
}

pub type FfiResult<T> = Result<T, FfiError>;

// ── FFI type system ───────────────────────────────────────────────

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FfiType {
    I64,
    U64,
    Ptr,
    Void,
}

#[derive(Debug, Clone, Copy)]
pub enum FfiValue {
    I64(i64),
    U64(u64),
    Ptr(*mut c_void),
}

impl FfiValue {
    pub fn as_u64(self) -> u64 {
        match self {
            FfiValue::I64(v) => v as u64,
            FfiValue::U64(v) => v,
            FfiValue::Ptr(ptr) => ptr as u64,
        }
    }
}

#[derive(Debug, Clone)]
pub struct FfiSignature {
    pub args: Vec<FfiType>,
    pub ret: FfiType,
}

// ── FFI runtime ───────────────────────────────────────────────────

#[derive(Debug)]
pub struct FfiRuntime {
    lib: DynamicLibrary,
    symbols: HashMap<String, *const c_void>,
}

impl FfiRuntime {
    pub fn new() -> FfiResult<Self> {
        Ok(Self {
            lib: DynamicLibrary::open_default()?,
            symbols: HashMap::new(),
        })
    }

    /// Call a C function by name.
    ///
    /// `sig` describes the argument types and return type.  All
    /// arguments are passed as `u64` (the native register width on
    /// 64-bit platforms).
    pub fn call(&mut self, name: &str, sig: &FfiSignature, args: &[u64]) -> FfiResult<Option<u64>> {
        if sig.args.len() != args.len() {
            return Err(FfiError::Message(format!(
                "ffi call '{name}' expects {} args, got {}",
                sig.args.len(),
                args.len()
            )));
        }
        if sig.args.iter().any(|ty| matches!(ty, FfiType::Void)) {
            return Err(FfiError::Message("ffi arguments cannot be void".into()));
        }

        let fn_ptr = self.resolve_symbol(name)?;
        self.call_address(fn_ptr, name, sig, args)
    }

    /// Call a function pointer supplied by the embedding host.
    pub fn call_address(
        &mut self,
        address: *const c_void,
        name: &str,
        sig: &FfiSignature,
        args: &[u64],
    ) -> FfiResult<Option<u64>> {
        if address.is_null() {
            return Err(FfiError::Message(format!(
                "ffi call '{name}' has a null address"
            )));
        }

        let ret = match sig.ret {
            FfiType::Void => {
                unsafe { call_void(address, args)? };
                None
            }
            FfiType::I64 => Some(unsafe { call_i64(address, args)? } as u64),
            FfiType::U64 => Some(unsafe { call_u64(address, args)? }),
            FfiType::Ptr => Some(unsafe { call_ptr(address, args)? } as u64),
        };
        Ok(ret)
    }

    fn resolve_symbol(&mut self, name: &str) -> FfiResult<*const c_void> {
        if let Some(&ptr) = self.symbols.get(name) {
            return Ok(ptr);
        }
        let ptr = self.lib.symbol(name)?;
        self.symbols.insert(name.to_string(), ptr);
        Ok(ptr)
    }
}

// ── dynamic library ───────────────────────────────────────────────

#[derive(Debug)]
pub struct DynamicLibrary {
    handle: *mut c_void,
}

impl DynamicLibrary {
    pub fn open_default() -> FfiResult<Self> {
        let mut errors = Vec::new();
        for lib in default_libs() {
            match Self::open(Some(lib)) {
                Ok(lib) => return Ok(lib),
                Err(err) => errors.push(err.to_string()),
            }
        }
        Self::open(None).map_err(|err| {
            let mut message = String::from("failed to open default ffi library");
            for detail in errors {
                message.push_str(&format!("; {detail}"));
            }
            FfiError::Message(format!("{message}; {err}"))
        })
    }

    fn open(path: Option<&str>) -> FfiResult<Self> {
        let handle = unsafe { dlopen(path)? };
        if handle.is_null() {
            return Err(FfiError::Message("dlopen returned null handle".into()));
        }
        Ok(Self { handle })
    }

    pub fn symbol(&self, name: &str) -> FfiResult<*const c_void> {
        let symbol = unsafe { dlsym(self.handle, name)? };
        if symbol.is_null() {
            return Err(FfiError::SymbolNotFound(name.into()));
        }
        Ok(symbol)
    }
}

impl Drop for DynamicLibrary {
    fn drop(&mut self) {
        unsafe {
            dlclose(self.handle);
        }
    }
}

// ── platform default libs ─────────────────────────────────────────

#[cfg(any(target_os = "linux", target_os = "android", target_os = "freebsd"))]
fn default_libs() -> &'static [&'static str] {
    &["libc.so.6", "libc.so"]
}

#[cfg(target_os = "macos")]
fn default_libs() -> &'static [&'static str] {
    &["libSystem.B.dylib"]
}

#[cfg(target_os = "windows")]
fn default_libs() -> &'static [&'static str] {
    &["msvcrt.dll", "ucrtbase.dll"]
}

// ── unix dlfcn wrappers ───────────────────────────────────────────

#[cfg(unix)]
unsafe fn dlopen(path: Option<&str>) -> FfiResult<*mut c_void> {
    let c_path = match path {
        Some(p) => Some(CString::new(p).map_err(|e| FfiError::Message(e.to_string()))?),
        None => None,
    };
    let handle = unsafe {
        dlopen_raw(
            c_path
                .as_ref()
                .map(|s| s.as_ptr())
                .unwrap_or(std::ptr::null()),
            RTLD_NOW,
        )
    };
    if handle.is_null() {
        let err = unsafe { dlerror_string() };
        return Err(FfiError::Message(err));
    }
    Ok(handle)
}

#[cfg(unix)]
unsafe fn dlsym(handle: *mut c_void, name: &str) -> FfiResult<*const c_void> {
    let c_name = CString::new(name).map_err(|e| FfiError::Message(e.to_string()))?;
    let symbol = unsafe { dlsym_raw(handle, c_name.as_ptr()) };
    if symbol.is_null() {
        let err = unsafe { dlerror_string() };
        return Err(FfiError::Message(err));
    }
    Ok(symbol)
}

#[cfg(unix)]
unsafe fn dlclose(handle: *mut c_void) {
    unsafe {
        dlclose_raw(handle);
    }
}

#[cfg(unix)]
unsafe fn dlerror_string() -> String {
    let err = unsafe { dlerror_raw() };
    if err.is_null() {
        "unknown dlerror".to_string()
    } else {
        unsafe { CStr::from_ptr(err).to_string_lossy().into_owned() }
    }
}

#[cfg(unix)]
const RTLD_NOW: i32 = 2;

#[cfg(all(unix, not(target_os = "macos")))]
#[link(name = "dl")]
unsafe extern "C" {
    #[link_name = "dlopen"]
    fn dlopen_raw(filename: *const c_char, flags: i32) -> *mut c_void;
    #[link_name = "dlsym"]
    fn dlsym_raw(handle: *mut c_void, symbol: *const c_char) -> *const c_void;
    #[link_name = "dlclose"]
    fn dlclose_raw(handle: *mut c_void) -> i32;
    #[link_name = "dlerror"]
    fn dlerror_raw() -> *const c_char;
}

#[cfg(target_os = "macos")]
#[link(name = "System")]
unsafe extern "C" {
    #[link_name = "dlopen"]
    fn dlopen_raw(filename: *const c_char, flags: i32) -> *mut c_void;
    #[link_name = "dlsym"]
    fn dlsym_raw(handle: *mut c_void, symbol: *const c_char) -> *const c_void;
    #[link_name = "dlclose"]
    fn dlclose_raw(handle: *mut c_void) -> i32;
    #[link_name = "dlerror"]
    fn dlerror_raw() -> *const c_char;
}

// ── windows stubs ─────────────────────────────────────────────────

#[cfg(windows)]
use std::ffi::OsStr;
#[cfg(windows)]
use std::os::windows::ffi::OsStrExt;

#[cfg(windows)]
unsafe fn dlopen(path: Option<&str>) -> FfiResult<*mut c_void> {
    use libc::{GetModuleHandleW, LoadLibraryW};
    let path = path
        .ok_or_else(|| FfiError::Message("ffi requires explicit library path on Windows".into()))?;
    let wide: Vec<u16> = OsStr::new(path)
        .encode_wide()
        .chain(std::iter::once(0))
        .collect();
    let handle = LoadLibraryW(wide.as_ptr()) as *mut c_void;
    if handle.is_null() {
        return Err(FfiError::Message("LoadLibraryW failed".into()));
    }
    Ok(handle)
}

#[cfg(windows)]
unsafe fn dlsym(handle: *mut c_void, name: &str) -> FfiResult<*const c_void> {
    use libc::GetProcAddress;
    let c_name = CString::new(name).map_err(|e| FfiError::Message(e.to_string()))?;
    let symbol = GetProcAddress(handle as _, c_name.as_ptr()) as *const c_void;
    if symbol.is_null() {
        return Err(FfiError::Message("GetProcAddress failed".into()));
    }
    Ok(symbol)
}

#[cfg(windows)]
unsafe fn dlclose(handle: *mut c_void) {
    use libc::FreeLibrary;
    FreeLibrary(handle as _);
}

// ── transmute call helpers ────────────────────────────────────────

unsafe fn call_void(fn_ptr: *const c_void, args: &[u64]) -> FfiResult<()> {
    unsafe {
        match args.len() {
            0 => {
                let f: extern "C" fn() = std::mem::transmute(fn_ptr);
                f();
            }
            1 => {
                let f: extern "C" fn(u64) = std::mem::transmute(fn_ptr);
                f(args[0]);
            }
            2 => {
                let f: extern "C" fn(u64, u64) = std::mem::transmute(fn_ptr);
                f(args[0], args[1]);
            }
            3 => {
                let f: extern "C" fn(u64, u64, u64) = std::mem::transmute(fn_ptr);
                f(args[0], args[1], args[2]);
            }
            4 => {
                let f: extern "C" fn(u64, u64, u64, u64) = std::mem::transmute(fn_ptr);
                f(args[0], args[1], args[2], args[3]);
            }
            5 => {
                let f: extern "C" fn(u64, u64, u64, u64, u64) = std::mem::transmute(fn_ptr);
                f(args[0], args[1], args[2], args[3], args[4]);
            }
            6 => {
                let f: extern "C" fn(u64, u64, u64, u64, u64, u64) = std::mem::transmute(fn_ptr);
                f(args[0], args[1], args[2], args[3], args[4], args[5]);
            }
            _ => return Err(FfiError::Message("ffi supports up to 6 arguments".into())),
        }
        Ok(())
    }
}

unsafe fn call_i64(fn_ptr: *const c_void, args: &[u64]) -> FfiResult<i64> {
    unsafe {
        Ok(match args.len() {
            0 => {
                let f: extern "C" fn() -> i64 = std::mem::transmute(fn_ptr);
                f()
            }
            1 => {
                let f: extern "C" fn(u64) -> i64 = std::mem::transmute(fn_ptr);
                f(args[0])
            }
            2 => {
                let f: extern "C" fn(u64, u64) -> i64 = std::mem::transmute(fn_ptr);
                f(args[0], args[1])
            }
            3 => {
                let f: extern "C" fn(u64, u64, u64) -> i64 = std::mem::transmute(fn_ptr);
                f(args[0], args[1], args[2])
            }
            4 => {
                let f: extern "C" fn(u64, u64, u64, u64) -> i64 = std::mem::transmute(fn_ptr);
                f(args[0], args[1], args[2], args[3])
            }
            5 => {
                let f: extern "C" fn(u64, u64, u64, u64, u64) -> i64 = std::mem::transmute(fn_ptr);
                f(args[0], args[1], args[2], args[3], args[4])
            }
            6 => {
                let f: extern "C" fn(u64, u64, u64, u64, u64, u64) -> i64 =
                    std::mem::transmute(fn_ptr);
                f(args[0], args[1], args[2], args[3], args[4], args[5])
            }
            _ => return Err(FfiError::Message("ffi supports up to 6 arguments".into())),
        })
    }
}

unsafe fn call_u64(fn_ptr: *const c_void, args: &[u64]) -> FfiResult<u64> {
    unsafe {
        Ok(match args.len() {
            0 => {
                let f: extern "C" fn() -> u64 = std::mem::transmute(fn_ptr);
                f()
            }
            1 => {
                let f: extern "C" fn(u64) -> u64 = std::mem::transmute(fn_ptr);
                f(args[0])
            }
            2 => {
                let f: extern "C" fn(u64, u64) -> u64 = std::mem::transmute(fn_ptr);
                f(args[0], args[1])
            }
            3 => {
                let f: extern "C" fn(u64, u64, u64) -> u64 = std::mem::transmute(fn_ptr);
                f(args[0], args[1], args[2])
            }
            4 => {
                let f: extern "C" fn(u64, u64, u64, u64) -> u64 = std::mem::transmute(fn_ptr);
                f(args[0], args[1], args[2], args[3])
            }
            5 => {
                let f: extern "C" fn(u64, u64, u64, u64, u64) -> u64 = std::mem::transmute(fn_ptr);
                f(args[0], args[1], args[2], args[3], args[4])
            }
            6 => {
                let f: extern "C" fn(u64, u64, u64, u64, u64, u64) -> u64 =
                    std::mem::transmute(fn_ptr);
                f(args[0], args[1], args[2], args[3], args[4], args[5])
            }
            _ => return Err(FfiError::Message("ffi supports up to 6 arguments".into())),
        })
    }
}

unsafe fn call_ptr(fn_ptr: *const c_void, args: &[u64]) -> FfiResult<*mut c_void> {
    unsafe {
        Ok(match args.len() {
            0 => {
                let f: extern "C" fn() -> *mut c_void = std::mem::transmute(fn_ptr);
                f()
            }
            1 => {
                let f: extern "C" fn(u64) -> *mut c_void = std::mem::transmute(fn_ptr);
                f(args[0])
            }
            2 => {
                let f: extern "C" fn(u64, u64) -> *mut c_void = std::mem::transmute(fn_ptr);
                f(args[0], args[1])
            }
            3 => {
                let f: extern "C" fn(u64, u64, u64) -> *mut c_void = std::mem::transmute(fn_ptr);
                f(args[0], args[1], args[2])
            }
            4 => {
                let f: extern "C" fn(u64, u64, u64, u64) -> *mut c_void =
                    std::mem::transmute(fn_ptr);
                f(args[0], args[1], args[2], args[3])
            }
            5 => {
                let f: extern "C" fn(u64, u64, u64, u64, u64) -> *mut c_void =
                    std::mem::transmute(fn_ptr);
                f(args[0], args[1], args[2], args[3], args[4])
            }
            6 => {
                let f: extern "C" fn(u64, u64, u64, u64, u64, u64) -> *mut c_void =
                    std::mem::transmute(fn_ptr);
                f(args[0], args[1], args[2], args[3], args[4], args[5])
            }
            _ => return Err(FfiError::Message("ffi supports up to 6 arguments".into())),
        })
    }
}
