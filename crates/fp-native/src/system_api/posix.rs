use super::*;

pub(super) fn normalize_proc_name(symbol: &str) -> String {
    let base = symbol.split('!').last().unwrap_or(symbol).trim();
    base.trim_start_matches('_').to_ascii_lowercase()
}

pub(super) fn detect_system_api_from_posix_call(
    kind: &AsmInstructionKind,
    dirent_style: PosixDirentStyle,
) -> Option<SystemApiOp> {
    let AsmInstructionKind::Call { function, args, .. } = kind else {
        return None;
    };
    let AsmValue::Function(symbol) = function else {
        return None;
    };
    let name = normalize_proc_name(symbol);
    match name.as_str() {
        "opendir" => Some(SystemApiOp::Opendir {
            path: args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8)))),
        }),
        "readdir" | "readdir64" => Some(SystemApiOp::Readdir {
            dir: args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8)))),
            dirent_style,
        }),
        "closedir" => Some(SystemApiOp::Closedir {
            dir: args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8)))),
        }),
        "dlopen" => Some(SystemApiOp::Dlopen {
            path: args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8)))),
            flags: args
                .get(1)
                .cloned()
                .unwrap_or_else(|| AsmValue::Constant(AsmConstant::UInt(0, AsmType::I32))),
        }),
        "dlsym" => Some(SystemApiOp::Dlsym {
            handle: args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmValue::Constant(AsmConstant::UInt(0, AsmType::I64))),
            symbol: args
                .get(1)
                .cloned()
                .unwrap_or_else(|| AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8)))),
        }),
        "dlclose" => Some(SystemApiOp::Dlclose {
            handle: args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmValue::Constant(AsmConstant::UInt(0, AsmType::I64))),
        }),
        "unlink" => Some(SystemApiOp::Unlink {
            path: args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8)))),
        }),
        "mkdir" => Some(SystemApiOp::Mkdir {
            path: args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8)))),
            mode: args
                .get(1)
                .cloned()
                .unwrap_or_else(|| AsmValue::Constant(AsmConstant::UInt(0, AsmType::I32))),
        }),
        "rmdir" => Some(SystemApiOp::Rmdir {
            path: args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8)))),
        }),
        "rename" => Some(SystemApiOp::Rename {
            from: args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8)))),
            to: args
                .get(1)
                .cloned()
                .unwrap_or_else(|| AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8)))),
        }),
        "access" => Some(SystemApiOp::Access {
            path: args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8)))),
            mode: args
                .get(1)
                .cloned()
                .unwrap_or_else(|| AsmValue::Constant(AsmConstant::UInt(0, AsmType::I32))),
        }),
        _ => None,
    }
}

pub(super) fn windows_createfile_disposition_from_flags(style: PosixFlagStyle, flags: i64) -> i64 {
    match style {
        PosixFlagStyle::Linux => windows_createfile_disposition_linux(flags),
        PosixFlagStyle::Darwin => windows_createfile_disposition_darwin(flags),
    }
}

pub(super) fn posix_mmap_flags_anonymous_private(style: PosixFlagStyle) -> i64 {
    match style {
        // MAP_PRIVATE=0x02, MAP_ANONYMOUS=0x20
        PosixFlagStyle::Linux => 0x02 | 0x20,
        // MAP_PRIVATE=0x02, MAP_ANON=0x1000
        PosixFlagStyle::Darwin => 0x02 | 0x1000,
    }
}

pub(super) fn windows_page_protection_from_posix(prot: i64) -> i64 {
    // PROT_READ=1, PROT_WRITE=2, PROT_EXEC=4
    // PAGE_NOACCESS=0x01
    // PAGE_READONLY=0x02
    // PAGE_READWRITE=0x04
    // PAGE_EXECUTE_READ=0x20
    // PAGE_EXECUTE_READWRITE=0x40
    let read = (prot & 1) != 0;
    let write = (prot & 2) != 0;
    let exec = (prot & 4) != 0;
    match (exec, write, read) {
        (true, true, _) => 0x40,
        (true, false, true) => 0x20,
        (false, true, _) => 0x04,
        (false, false, true) => 0x02,
        _ => 0x01,
    }
}

pub(super) fn windows_createfile_desired_access(flags: i64) -> i64 {
    // POSIX: O_RDONLY=0, O_WRONLY=1, O_RDWR=2
    // Win32: GENERIC_READ=0x80000000, GENERIC_WRITE=0x40000000
    const GENERIC_READ: i64 = 0x8000_0000u32 as i64;
    const GENERIC_WRITE: i64 = 0x4000_0000u32 as i64;
    match flags & 0b11 {
        0 => GENERIC_READ,
        1 => GENERIC_WRITE,
        2 => GENERIC_READ | GENERIC_WRITE,
        _ => GENERIC_READ,
    }
}

pub(super) fn windows_createfile_disposition_linux(flags: i64) -> i64 {
    // Win32 creation disposition values:
    // 1 CREATE_NEW, 2 CREATE_ALWAYS, 3 OPEN_EXISTING, 4 OPEN_ALWAYS, 5 TRUNCATE_EXISTING
    const O_CREAT: i64 = 64;
    const O_EXCL: i64 = 128;
    const O_TRUNC: i64 = 512;
    let has_creat = (flags & O_CREAT) != 0;
    let has_excl = (flags & O_EXCL) != 0;
    let has_trunc = (flags & O_TRUNC) != 0;
    match (has_creat, has_excl, has_trunc) {
        (true, true, _) => 1,
        (true, false, true) => 2,
        (true, false, false) => 4,
        (false, _, true) => 5,
        _ => 3,
    }
}

pub(super) fn windows_createfile_disposition_darwin(flags: i64) -> i64 {
    // Darwin flag constants differ.
    const O_CREAT: i64 = 0x200;
    const O_EXCL: i64 = 0x800;
    const O_TRUNC: i64 = 0x400;
    let has_creat = (flags & O_CREAT) != 0;
    let has_excl = (flags & O_EXCL) != 0;
    let has_trunc = (flags & O_TRUNC) != 0;
    match (has_creat, has_excl, has_trunc) {
        (true, true, _) => 1,
        (true, false, true) => 2,
        (true, false, false) => 4,
        (false, _, true) => 5,
        _ => 3,
    }
}

pub(super) fn match_closehandle_sequence_to_syscall(
    instructions: &[AsmInstruction],
    convention: AsmSyscallConvention,
) -> Result<Option<(AsmInstruction, usize)>> {
    // Pattern A (stdio):
    //   GetStdHandle; CloseHandle; Eq; Select
    // Pattern B (direct handle):
    //   CloseHandle; Eq; Select
    if instructions.len() < 3 {
        return Ok(None);
    }

    let mut base = 0usize;
    let mut fd_value: Option<AsmValue> = None;

    if is_call_named(&instructions[0], "kernel32.dll", "GetStdHandle") {
        if instructions.len() < 4 {
            return Ok(None);
        }
        let getstd = &instructions[0];
        let AsmInstructionKind::Call {
            args: getstd_args, ..
        } = &getstd.kind
        else {
            return Ok(None);
        };
        let Some(handle_code) = getstd_args.first().and_then(|value| {
            resolve_i64(value, instructions)
                .map_err(|e| {
                    eprintln!("[fp-native] Win32-to-POSIX arg resolution error: {e}");
                    e
                })
                .ok()
        }) else {
            return Ok(None);
        };
        let fd = match handle_code {
            Some(-10) => 0u64,
            Some(-11) => 1u64,
            Some(-12) => 2u64,
            _ => return Ok(None),
        };
        fd_value = Some(AsmValue::Constant(AsmConstant::UInt(fd, AsmType::I64)));
        base = 1;
    }

    let close = &instructions[base];
    let cmp = instructions.get(base + 1).ok_or_else(|| {
        fp_core::error::Error::from("missing Eq instruction in CloseHandle sequence")
    })?;
    let select = instructions.get(base + 2).ok_or_else(|| {
        fp_core::error::Error::from("missing Select instruction in CloseHandle sequence")
    })?;

    if !is_call_named(close, "kernel32.dll", "CloseHandle") {
        return Ok(None);
    }
    if !matches!(cmp.kind, AsmInstructionKind::Eq(_, _)) {
        return Ok(None);
    }
    if !matches!(select.kind, AsmInstructionKind::Select { .. }) {
        return Ok(None);
    }

    let AsmInstructionKind::Call {
        args: close_args, ..
    } = &close.kind
    else {
        return Ok(None);
    };
    if close_args.len() != 1 {
        return Ok(None);
    }
    if base == 1 && close_args[0] != AsmValue::Register(instructions[0].id) {
        return Ok(None);
    }

    let fd = fd_value.unwrap_or_else(|| close_args[0].clone());
    let op = SystemApiOp::Close { fd };
    let kind = lower_system_api_to_syscall(op, convention);

    Ok(Some((
        AsmInstruction {
            id: select.id,
            opcode: AsmOpcode::Generic(AsmGenericOpcode::Syscall),
            kind,
            ty: AsmType::I64,
            operands: Vec::new(),
            implicit_uses: Vec::new(),
            implicit_defs: Vec::new(),
            encoding: None,
            debug_info: None,
            annotations: Vec::new(),
        },
        base + 3,
    )))
}

pub(super) fn fd_to_std_handle_code(fd: i64) -> Option<i64> {
    // STD_INPUT_HANDLE=-10, STD_OUTPUT_HANDLE=-11, STD_ERROR_HANDLE=-12
    Some(match fd {
        0 => -10,
        1 => -11,
        2 => -12,
        _ => return None,
    })
}
