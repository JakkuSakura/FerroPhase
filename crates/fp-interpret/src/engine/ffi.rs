use fp_core::ast::FunctionSignature;
use fp_core::error::Result;
use fp_core::ast::Value;
use fp_native::ffi::FfiRuntime as NativeFfiRuntime;

#[derive(Debug)]
pub struct FfiRuntime {
    inner: NativeFfiRuntime,
}

impl FfiRuntime {
    pub fn new() -> Result<Self> {
        Ok(Self {
            inner: NativeFfiRuntime::new()?,
        })
    }

    pub fn call(&mut self, name: &str, sig: &FunctionSignature, args: &[Value]) -> Result<Value> {
        self.inner.call_fp(name, sig, args)
    }
}
