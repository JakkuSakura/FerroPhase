use super::*;

impl LirInterpreter {
    pub(super) fn prepare_ffi_args(
        &self,
        args: &[TypedValue],
        sig: &FfiSignature,
    ) -> LirResult<(Vec<u64>, Vec<CString>)> {
        let mut raws = Vec::with_capacity(args.len());
        let mut cstrings = Vec::new();
        for (arg, ty) in args.iter().zip(&sig.args) {
            let raw = self.encode_ffi_value(arg)?;
            if *ty == FfiType::Ptr && raw != 0 {
                let bytes =
                    self.state.mem.load_c_string(raw).map_err(|error| {
                        VmError::Runtime(format!("invalid VM pointer: {error}"))
                    })?;
                let cstring = CString::new(bytes).map_err(|error| {
                    VmError::Runtime(format!("invalid C string argument: {error}"))
                })?;
                raws.push(cstring.as_ptr() as u64);
                cstrings.push(cstring);
                continue;
            }
            raws.push(raw);
        }
        if args.len() != sig.args.len() {
            return Err(VmError::Runtime(format!(
                "ffi expects {} args, got {}",
                sig.args.len(),
                args.len()
            )));
        }
        Ok((raws, cstrings))
    }

    pub(super) fn encode_ffi_value(&self, value: &TypedValue) -> LirResult<u64> {
        match &value.value {
            Value::Int(value) => Ok(value.value as u64),
            Value::UInt(value) => Ok(value.value),
            Value::Bool(value) => Ok(u64::from(value.value)),
            Value::Decimal(value) => Ok(value.value.to_bits()),
            Value::Pointer(value) => Ok(value.value as u64),
            Value::Null(_) => Ok(0),
            value => Err(VmError::TypeMismatch {
                expected: "FFI scalar or pointer value".into(),
                found: format!("{value:?}"),
            }),
        }
    }
}
