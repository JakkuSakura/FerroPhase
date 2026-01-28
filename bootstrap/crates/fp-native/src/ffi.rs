use fp_core::error::{Error, Result};
use std::collections::HashMap;
use std::ffi::{c_char, c_void, CStr, CString};

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
    fn as_u64(self) -> u64 {
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

#[derive(Debug)]
pub struct FfiRuntime {
    lib: DynamicLibrary,
    symbols: HashMap<String, *const c_void>,
}

impl FfiRuntime {
    pub fn new() -> Result<Self> {
        Ok(Self {
            lib: DynamicLibrary::open_default()?,
            symbols: HashMap::new(),
        })
    }

    pub fn call(&mut self, name: &str, sig: &FfiSignature, args: &[FfiValue]) -> Result<Option<FfiValue>> {
        if sig.args.len() != args.len() {
            return Err(Error::from(format!(
                "ffi call '{name}' expects {} args, got {}",
                sig.args.len(),
                args.len()
            )));
        }
        if sig.args.iter().any(|ty| matches!(ty, FfiType::Void)) {
            return Err(Error::from("ffi arguments cannot be void"));
        }
        let fn_ptr = self.resolve_symbol(name)?;
        let mut raw_args = Vec::with_capacity(args.len());
        for value in args {
            raw_args.push(value.as_u64());
        }
        let ret = match sig.ret {
            FfiType::Void => {
                unsafe { call_void(fn_ptr, &raw_args)? };
                None
            }
            FfiType::I64 => Some(FfiValue::I64(unsafe { call_i64(fn_ptr, &raw_args)? })),
            FfiType::U64 => Some(FfiValue::U64(unsafe { call_u64(fn_ptr, &raw_args)? })),
            FfiType::Ptr => {
                let ptr = unsafe { call_ptr(fn_ptr, &raw_args)? };
                Some(FfiValue::Ptr(ptr))
            }
        };
        Ok(ret)
    }

    fn resolve_symbol(&mut self, name: &str) -> Result<*const c_void> {
        if let Some(ptr) = self.symbols.get(name).copied() {
            return Ok(ptr);
        }
        let symbol = self.lib.symbol(name)?;
        self.symbols.insert(name.to_string(), symbol);
        Ok(symbol)
    }
}

unsafe fn call_void(fn_ptr: *const c_void, args: &[u64]) -> Result<()> {
    match args.len() {
        0 => {
            let func: extern "C" fn() = std::mem::transmute(fn_ptr);
            func();
        }
        1 => {
            let func: extern "C" fn(u64) = std::mem::transmute(fn_ptr);
            func(args[0]);
        }
        2 => {
            let func: extern "C" fn(u64, u64) = std::mem::transmute(fn_ptr);
            func(args[0], args[1]);
        }
        3 => {
            let func: extern "C" fn(u64, u64, u64) = std::mem::transmute(fn_ptr);
            func(args[0], args[1], args[2]);
        }
        4 => {
            let func: extern "C" fn(u64, u64, u64, u64) = std::mem::transmute(fn_ptr);
            func(args[0], args[1], args[2], args[3]);
        }
        5 => {
            let func: extern "C" fn(u64, u64, u64, u64, u64) = std::mem::transmute(fn_ptr);
            func(args[0], args[1], args[2], args[3], args[4]);
        }
        6 => {
            let func: extern "C" fn(u64, u64, u64, u64, u64, u64) =
                std::mem::transmute(fn_ptr);
            func(args[0], args[1], args[2], args[3], args[4], args[5]);
        }
        _ => return Err(Error::from("ffi supports up to 6 arguments")),
    }
    Ok(())
}

unsafe fn call_i64(fn_ptr: *const c_void, args: &[u64]) -> Result<i64> {
    let value = match args.len() {
        0 => {
            let func: extern "C" fn() -> i64 = std::mem::transmute(fn_ptr);
            func()
        }
        1 => {
            let func: extern "C" fn(u64) -> i64 = std::mem::transmute(fn_ptr);
            func(args[0])
        }
        2 => {
            let func: extern "C" fn(u64, u64) -> i64 = std::mem::transmute(fn_ptr);
            func(args[0], args[1])
        }
        3 => {
            let func: extern "C" fn(u64, u64, u64) -> i64 = std::mem::transmute(fn_ptr);
            func(args[0], args[1], args[2])
        }
        4 => {
            let func: extern "C" fn(u64, u64, u64, u64) -> i64 = std::mem::transmute(fn_ptr);
            func(args[0], args[1], args[2], args[3])
        }
        5 => {
            let func: extern "C" fn(u64, u64, u64, u64, u64) -> i64 =
                std::mem::transmute(fn_ptr);
            func(args[0], args[1], args[2], args[3], args[4])
        }
        6 => {
            let func: extern "C" fn(u64, u64, u64, u64, u64, u64) -> i64 =
                std::mem::transmute(fn_ptr);
            func(args[0], args[1], args[2], args[3], args[4], args[5])
        }
        _ => return Err(Error::from("ffi supports up to 6 arguments")),
    };
    Ok(value)
}

unsafe fn call_u64(fn_ptr: *const c_void, args: &[u64]) -> Result<u64> {
    let value = match args.len() {
        0 => {
            let func: extern "C" fn() -> u64 = std::mem::transmute(fn_ptr);
            func()
        }
        1 => {
            let func: extern "C" fn(u64) -> u64 = std::mem::transmute(fn_ptr);
            func(args[0])
        }
        2 => {
            let func: extern "C" fn(u64, u64) -> u64 = std::mem::transmute(fn_ptr);
            func(args[0], args[1])
        }
        3 => {
            let func: extern "C" fn(u64, u64, u64) -> u64 = std::mem::transmute(fn_ptr);
            func(args[0], args[1], args[2])
        }
        4 => {
            let func: extern "C" fn(u64, u64, u64, u64) -> u64 = std::mem::transmute(fn_ptr);
            func(args[0], args[1], args[2], args[3])
        }
        5 => {
            let func: extern "C" fn(u64, u64, u64, u64, u64) -> u64 =
                std::mem::transmute(fn_ptr);
            func(args[0], args[1], args[2], args[3], args[4])
        }
        6 => {
            let func: extern "C" fn(u64, u64, u64, u64, u64, u64) -> u64 =
                std::mem::transmute(fn_ptr);
            func(args[0], args[1], args[2], args[3], args[4], args[5])
        }
        _ => return Err(Error::from("ffi supports up to 6 arguments")),
    };
    Ok(value)
}

unsafe fn call_ptr(fn_ptr: *const c_void, args: &[u64]) -> Result<*mut c_void> {
    let value = match args.len() {
        0 => {
            let func: extern "C" fn() -> *mut c_void = std::mem::transmute(fn_ptr);
            func()
        }
        1 => {
            let func: extern "C" fn(u64) -> *mut c_void = std::mem::transmute(fn_ptr);
            func(args[0])
        }
        2 => {
            let func: extern "C" fn(u64, u64) -> *mut c_void = std::mem::transmute(fn_ptr);
            func(args[0], args[1])
        }
        3 => {
            let func: extern "C" fn(u64, u64, u64) -> *mut c_void = std::mem::transmute(fn_ptr);
            func(args[0], args[1], args[2])
        }
        4 => {
            let func: extern "C" fn(u64, u64, u64, u64) -> *mut c_void =
                std::mem::transmute(fn_ptr);
            func(args[0], args[1], args[2], args[3])
        }
        5 => {
            let func: extern "C" fn(u64, u64, u64, u64, u64) -> *mut c_void =
                std::mem::transmute(fn_ptr);
            func(args[0], args[1], args[2], args[3], args[4])
        }
        6 => {
            let func: extern "C" fn(u64, u64, u64, u64, u64, u64) -> *mut c_void =
                std::mem::transmute(fn_ptr);
            func(args[0], args[1], args[2], args[3], args[4], args[5])
        }
        _ => return Err(Error::from("ffi supports up to 6 arguments")),
    };
    Ok(value)
}

#[derive(Debug)]
struct DynamicLibrary {
    handle: *mut c_void,
}

impl DynamicLibrary {
    fn open_default() -> Result<Self> {
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
            Error::from(format!("{message}; {err}"))
        })
    }

    fn open(path: Option<&str>) -> Result<Self> {
        let handle = unsafe { dlopen(path)? };
        if handle.is_null() {
            return Err(Error::from("dlopen returned null handle"));
        }
        Ok(Self { handle })
    }

    fn symbol(&self, name: &str) -> Result<*const c_void> {
        let symbol = unsafe { dlsym(self.handle, name)? };
        if symbol.is_null() {
            return Err(Error::from(format!("symbol '{name}' not found")));
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

#[cfg(unix)]
unsafe fn dlopen(path: Option<&str>) -> Result<*mut c_void> {
    let c_path = match path {
        Some(path) => Some(CString::new(path).map_err(|err| Error::from(err.to_string()))?),
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
        return Err(Error::from(err));
    }
    Ok(handle)
}

#[cfg(unix)]
unsafe fn dlsym(handle: *mut c_void, name: &str) -> Result<*const c_void> {
    let c_name = CString::new(name).map_err(|err| Error::from(err.to_string()))?;
    let symbol = unsafe { dlsym_raw(handle, c_name.as_ptr()) };
    if symbol.is_null() {
        let err = unsafe { dlerror_string() };
        return Err(Error::from(err));
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
extern "C" {
    fn dlopen_raw(filename: *const c_char, flags: i32) -> *mut c_void;
    fn dlsym_raw(handle: *mut c_void, symbol: *const c_char) -> *const c_void;
    fn dlclose_raw(handle: *mut c_void) -> i32;
    fn dlerror_raw() -> *const c_char;
}

#[cfg(target_os = "macos")]
#[link(name = "System")]
extern "C" {
    fn dlopen_raw(filename: *const c_char, flags: i32) -> *mut c_void;
    fn dlsym_raw(handle: *mut c_void, symbol: *const c_char) -> *const c_void;
    fn dlclose_raw(handle: *mut c_void) -> i32;
    fn dlerror_raw() -> *const c_char;
}

#[cfg(windows)]
use std::ffi::OsStr;
#[cfg(windows)]
use std::os::windows::ffi::OsStrExt;

#[cfg(windows)]
unsafe fn dlopen(path: Option<&str>) -> Result<*mut c_void> {
    let path = path.ok_or_else(|| Error::from("ffi requires explicit library path on Windows"))?;
    let wide: Vec<u16> = OsStr::new(path)
        .encode_wide()
        .chain(std::iter::once(0))
        .collect();
    let handle = unsafe { LoadLibraryW(wide.as_ptr()) };
    if handle.is_null() {
        return Err(Error::from("LoadLibraryW failed"));
    }
    Ok(handle as *mut c_void)
}

#[cfg(windows)]
unsafe fn dlsym(handle: *mut c_void, name: &str) -> Result<*const c_void> {
    let c_name = CString::new(name).map_err(|err| Error::from(err.to_string()))?;
    let symbol = unsafe { GetProcAddress(handle as HMODULE, c_name.as_ptr()) };
    if symbol.is_null() {
        return Err(Error::from("GetProcAddress failed"));
    }
    Ok(symbol as *const c_void)
}

#[cfg(windows)]
unsafe fn dlclose(handle: *mut c_void) {
    unsafe {
        FreeLibrary(handle as HMODULE);
    }
}

#[cfg(windows)]
type HMODULE = *mut c_void;

#[cfg(windows)]
#[link(name = "Kernel32")]
extern "system" {
    fn LoadLibraryW(lpFileName: *const u16) -> HMODULE;
    fn GetProcAddress(hModule: HMODULE, lpProcName: *const c_char) -> *const c_void;
    fn FreeLibrary(hModule: HMODULE) -> i32;
}
