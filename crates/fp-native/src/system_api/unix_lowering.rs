use super::*;

pub(super) fn unix_calling_convention(convention: AsmSyscallConvention) -> CallingConvention {
    match convention {
        AsmSyscallConvention::LinuxX86_64 | AsmSyscallConvention::DarwinX86_64 => {
            CallingConvention::X86_64SysV
        }
        AsmSyscallConvention::LinuxAarch64 | AsmSyscallConvention::DarwinAarch64 => {
            CallingConvention::AAPCS
        }
    }
}

pub(super) fn lower_system_api_to_unix(
    op: SystemApiOp,
    convention: AsmSyscallConvention,
) -> (AsmOpcode, AsmInstructionKind, AsmType) {
    match op {
        SystemApiOp::Dlopen { path, flags } => (
            AsmOpcode::Generic(AsmGenericOpcode::Call),
            AsmInstructionKind::Call {
                function: AsmValue::Function("dlopen".to_string()),
                args: vec![path, flags],
                calling_convention: unix_calling_convention(convention),
                tail_call: false,
            },
            AsmType::I64,
        ),
        SystemApiOp::Dlsym { handle, symbol } => (
            AsmOpcode::Generic(AsmGenericOpcode::Call),
            AsmInstructionKind::Call {
                function: AsmValue::Function("dlsym".to_string()),
                args: vec![handle, symbol],
                calling_convention: unix_calling_convention(convention),
                tail_call: false,
            },
            AsmType::I64,
        ),
        SystemApiOp::Dlclose { handle } => (
            AsmOpcode::Generic(AsmGenericOpcode::Call),
            AsmInstructionKind::Call {
                function: AsmValue::Function("dlclose".to_string()),
                args: vec![handle],
                calling_convention: unix_calling_convention(convention),
                tail_call: false,
            },
            AsmType::I64,
        ),
        other => (
            AsmOpcode::Generic(AsmGenericOpcode::Syscall),
            lower_system_api_to_syscall(other, convention),
            AsmType::I64,
        ),
    }
}

pub(super) fn detect_system_api_from_windows_import(
    kind: &AsmInstructionKind,
    convention: AsmSyscallConvention,
) -> Option<SystemApiOp> {
    let AsmInstructionKind::Call { function, args, .. } = kind else {
        return None;
    };
    let AsmValue::Function(name) = function else {
        return None;
    };
    let (dll, proc_name) = split_import_symbol(name);
    let is_win32_dll =
        dll.eq_ignore_ascii_case("kernel32.dll") || dll.eq_ignore_ascii_case("kernelbase.dll");
    let is_ntdll = dll.eq_ignore_ascii_case("ntdll.dll");

    match proc_name.as_str() {
        "ExitProcess" => {
            if !is_win32_dll {
                return None;
            }
            let code = args
                .first()
                .cloned()
                .unwrap_or_else(|| AsmValue::Constant(AsmConstant::UInt(0, AsmType::I32)));
            Some(SystemApiOp::Exit { code })
        }
        "RtlExitUserProcess" => {
            if !is_ntdll {
                return None;
            }
            let code = args
                .first()
                .cloned()
                .unwrap_or_else(|| AsmValue::Constant(AsmConstant::UInt(0, AsmType::I32)));
            Some(SystemApiOp::Exit { code })
        }
        "GetCurrentProcessId" => {
            if !is_win32_dll {
                return None;
            }
            Some(SystemApiOp::GetPid)
        }
        "GetCurrentThreadId"
            if matches!(
                convention,
                AsmSyscallConvention::LinuxX86_64 | AsmSyscallConvention::LinuxAarch64
            ) =>
        {
            if !is_win32_dll {
                return None;
            }
            Some(SystemApiOp::GetTid)
        }
        "LoadLibraryA" => {
            if !is_win32_dll {
                return None;
            }
            let path = args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8))));
            Some(SystemApiOp::Dlopen {
                path,
                flags: AsmValue::Constant(AsmConstant::UInt(0, AsmType::I32)),
            })
        }
        "GetProcAddress" => {
            if !is_win32_dll {
                return None;
            }
            if args.len() < 2 {
                return None;
            }
            Some(SystemApiOp::Dlsym {
                handle: args[0].clone(),
                symbol: args[1].clone(),
            })
        }
        "FreeLibrary" => {
            if !is_win32_dll {
                return None;
            }
            if args.len() != 1 {
                return None;
            }
            Some(SystemApiOp::Dlclose {
                handle: args[0].clone(),
            })
        }
        "DeleteFileA" => {
            if !is_win32_dll {
                return None;
            }
            let path = args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8))));
            Some(SystemApiOp::Unlink { path })
        }
        "CreateDirectoryA" => {
            if !is_win32_dll {
                return None;
            }
            if args.len() < 1 {
                return None;
            }
            Some(SystemApiOp::Mkdir {
                path: args[0].clone(),
                mode: AsmValue::Constant(AsmConstant::UInt(0, AsmType::I32)),
            })
        }
        "RemoveDirectoryA" => {
            if !is_win32_dll {
                return None;
            }
            if args.len() < 1 {
                return None;
            }
            Some(SystemApiOp::Rmdir {
                path: args[0].clone(),
            })
        }
        "MoveFileExA" => {
            if !is_win32_dll {
                return None;
            }
            if args.len() < 2 {
                return None;
            }
            Some(SystemApiOp::Rename {
                from: args[0].clone(),
                to: args[1].clone(),
            })
        }
        "GetFileAttributesA" => {
            if !is_win32_dll {
                return None;
            }
            let path = args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8))));
            Some(SystemApiOp::Access {
                path,
                mode: AsmValue::Constant(AsmConstant::UInt(0, AsmType::I32)),
            })
        }
        "CreateFileA" => {
            if !is_win32_dll {
                return None;
            }
            if args.len() != 7 {
                return None;
            }
            let path = args[0].clone();
            let desired_access = resolve_i64(&args[1], &[])
                .map_err(|e| {
                    eprintln!("[fp-native] Win32-to-POSIX arg resolution error: {e}");
                    e
                })
                .ok()
                .flatten()?;
            let disposition = resolve_i64(&args[4], &[])
                .map_err(|e| {
                    eprintln!("[fp-native] Win32-to-POSIX arg resolution error: {e}");
                    e
                })
                .ok()
                .flatten()?;
            let flags = posix_flags_from_createfile(convention, desired_access, disposition);
            Some(SystemApiOp::Open {
                path,
                flags: AsmValue::Constant(AsmConstant::Int(flags, AsmType::I64)),
                mode: AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                flag_style: match convention {
                    AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                        PosixFlagStyle::Darwin
                    }
                    _ => PosixFlagStyle::Linux,
                },
            })
        }
        "WriteFile" => {
            if !is_win32_dll {
                return None;
            }
            if args.len() < 3 {
                return None;
            }
            Some(SystemApiOp::Write {
                fd: args[0].clone(),
                buffer: args[1].clone(),
                len: args[2].clone(),
            })
        }
        "ReadFile" => {
            if !is_win32_dll {
                return None;
            }
            if args.len() < 3 {
                return None;
            }
            Some(SystemApiOp::Read {
                fd: args[0].clone(),
                buffer: args[1].clone(),
                len: args[2].clone(),
            })
        }
        "CloseHandle" => {
            if !is_win32_dll {
                return None;
            }
            if args.len() != 1 {
                return None;
            }
            Some(SystemApiOp::Close {
                fd: args[0].clone(),
            })
        }
        "SetFilePointerEx" => {
            if !is_win32_dll {
                return None;
            }
            if args.len() != 4 {
                return None;
            }
            Some(SystemApiOp::Seek {
                fd: args[0].clone(),
                offset: args[1].clone(),
                // dwMoveMethod
                whence: args[3].clone(),
            })
        }
        "VirtualAlloc" => {
            if !is_win32_dll {
                return None;
            }
            if args.len() != 4 {
                return None;
            }
            // Treat VirtualAlloc as anonymous mmap.
            let style = match convention {
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    PosixFlagStyle::Darwin
                }
                _ => PosixFlagStyle::Linux,
            };
            let page_prot = resolve_i64(&args[3], &[])
                .map_err(|e| {
                    eprintln!("[fp-native] Win32-to-POSIX arg resolution error: {e}");
                    e
                })
                .ok()
                .flatten()
                .unwrap_or(0x04);
            let prot = match page_prot {
                0x40 | 0x20 => 0x1 | 0x4,
                0x04 => 0x1 | 0x2,
                0x02 => 0x1,
                _ => 0x1 | 0x2,
            };
            Some(SystemApiOp::Mmap {
                addr: args[0].clone(),
                len: args[1].clone(),
                prot: AsmValue::Constant(AsmConstant::Int(prot, AsmType::I64)),
                flags: AsmValue::Constant(AsmConstant::Int(
                    posix_mmap_flags_anonymous_private(style),
                    AsmType::I64,
                )),
                fd: AsmValue::Constant(AsmConstant::Int(-1, AsmType::I64)),
                offset: AsmValue::Constant(AsmConstant::UInt(0, AsmType::I64)),
            })
        }
        "VirtualFree" => {
            if !is_win32_dll {
                return None;
            }
            if args.len() != 3 {
                return None;
            }
            Some(SystemApiOp::Munmap {
                addr: args[0].clone(),
                len: args[1].clone(),
            })
        }
        "NtClose" | "ZwClose" => {
            if !is_ntdll || args.len() != 1 {
                return None;
            }
            Some(SystemApiOp::Close {
                fd: args[0].clone(),
            })
        }
        "NtWriteFile" | "ZwWriteFile" => {
            if !is_ntdll || args.len() < 7 {
                return None;
            }
            Some(SystemApiOp::Write {
                fd: args[0].clone(),
                buffer: args[5].clone(),
                len: args[6].clone(),
            })
        }
        "NtReadFile" | "ZwReadFile" => {
            if !is_ntdll || args.len() < 7 {
                return None;
            }
            Some(SystemApiOp::Read {
                fd: args[0].clone(),
                buffer: args[5].clone(),
                len: args[6].clone(),
            })
        }
        _ => None,
    }
}

pub(super) fn posix_flags_from_createfile(
    convention: AsmSyscallConvention,
    desired_access: i64,
    disposition: i64,
) -> i64 {
    // Win32:
    //   GENERIC_READ=0x80000000, GENERIC_WRITE=0x40000000
    // POSIX:
    //   O_RDONLY=0, O_WRONLY=1, O_RDWR=2
    //   O_CREAT,O_TRUNC,O_EXCL are platform-specific.
    const GENERIC_READ: i64 = 0x8000_0000u32 as i64;
    const GENERIC_WRITE: i64 = 0x4000_0000u32 as i64;

    let mut flags = match (
        (desired_access & GENERIC_READ) != 0,
        (desired_access & GENERIC_WRITE) != 0,
    ) {
        (true, true) => 2,
        (false, true) => 1,
        _ => 0,
    };

    let (o_creat, o_trunc, o_excl) = match convention {
        AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 =>
        // macOS
        {
            (0x200i64, 0x400i64, 0x800i64)
        }
        _ => (64i64, 512i64, 128i64),
    };

    match disposition {
        1 => flags |= o_creat | o_excl,
        2 => flags |= o_creat | o_trunc,
        4 => flags |= o_creat,
        5 => flags |= o_trunc,
        _ => {}
    }

    flags
}

pub(super) fn detect_system_api_from_syscall(
    convention: &AsmSyscallConvention,
    number: &AsmValue,
    args: &[AsmValue],
    instructions: &[AsmInstruction],
) -> Option<SystemApiOp> {
    let num = resolve_u64(number, instructions)?;

    match convention {
        AsmSyscallConvention::LinuxX86_64 if num == 60 => Some(SystemApiOp::Exit {
            code: args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmValue::Constant(AsmConstant::UInt(0, AsmType::I32))),
        }),
        AsmSyscallConvention::LinuxX86_64 if num == 39 => Some(SystemApiOp::GetPid),
        AsmSyscallConvention::LinuxX86_64 if num == 186 => Some(SystemApiOp::GetTid),
        AsmSyscallConvention::LinuxAarch64 if num == 93 => Some(SystemApiOp::Exit {
            code: args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmValue::Constant(AsmConstant::UInt(0, AsmType::I32))),
        }),
        AsmSyscallConvention::LinuxAarch64 if num == 172 => Some(SystemApiOp::GetPid),
        AsmSyscallConvention::LinuxAarch64 if num == 178 => Some(SystemApiOp::GetTid),
        AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64
            if num == 0x2000_0001 =>
        {
            Some(SystemApiOp::Exit {
                code: args
                    .get(0)
                    .cloned()
                    .unwrap_or_else(|| AsmValue::Constant(AsmConstant::UInt(0, AsmType::I32))),
            })
        }
        AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64
            if num == 0x2000_0014 =>
        {
            Some(SystemApiOp::GetPid)
        }
        AsmSyscallConvention::LinuxX86_64 if num == 1 => Some(SystemApiOp::Write {
            fd: args.get(0)?.clone(),
            buffer: args.get(1)?.clone(),
            len: args.get(2)?.clone(),
        }),
        AsmSyscallConvention::LinuxAarch64 if num == 64 => Some(SystemApiOp::Write {
            fd: args.get(0)?.clone(),
            buffer: args.get(1)?.clone(),
            len: args.get(2)?.clone(),
        }),
        AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64
            if num == 0x2000_0004 =>
        {
            Some(SystemApiOp::Write {
                fd: args.get(0)?.clone(),
                buffer: args.get(1)?.clone(),
                len: args.get(2)?.clone(),
            })
        }
        AsmSyscallConvention::LinuxX86_64 if num == 0 => Some(SystemApiOp::Read {
            fd: args.get(0)?.clone(),
            buffer: args.get(1)?.clone(),
            len: args.get(2)?.clone(),
        }),
        AsmSyscallConvention::LinuxAarch64 if num == 63 => Some(SystemApiOp::Read {
            fd: args.get(0)?.clone(),
            buffer: args.get(1)?.clone(),
            len: args.get(2)?.clone(),
        }),
        AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64
            if num == 0x2000_0003 =>
        {
            Some(SystemApiOp::Read {
                fd: args.get(0)?.clone(),
                buffer: args.get(1)?.clone(),
                len: args.get(2)?.clone(),
            })
        }
        AsmSyscallConvention::LinuxX86_64 if num == 3 => Some(SystemApiOp::Close {
            fd: args.get(0)?.clone(),
        }),
        AsmSyscallConvention::LinuxAarch64 if num == 57 => Some(SystemApiOp::Close {
            fd: args.get(0)?.clone(),
        }),
        AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64
            if num == 0x2000_0006 =>
        {
            Some(SystemApiOp::Close {
                fd: args.get(0)?.clone(),
            })
        }
        AsmSyscallConvention::LinuxX86_64 if num == 2 => Some(SystemApiOp::Open {
            path: args.get(0)?.clone(),
            flags: args.get(1)?.clone(),
            mode: args.get(2)?.clone(),
            flag_style: PosixFlagStyle::Linux,
        }),
        AsmSyscallConvention::LinuxX86_64 if num == 257 => {
            // openat(dirfd, path, flags, mode)
            let dirfd = args.get(0)?.clone();
            let dirfd = resolve_i64(&dirfd, instructions)
                .map_err(|e| {
                    eprintln!("[fp-native] Win32-to-POSIX arg resolution error: {e}");
                    e
                })
                .ok()
                .flatten()?;
            // AT_FDCWD=-100
            if dirfd != -100 {
                return None;
            }
            Some(SystemApiOp::Open {
                path: args.get(1)?.clone(),
                flags: args.get(2)?.clone(),
                mode: args.get(3)?.clone(),
                flag_style: PosixFlagStyle::Linux,
            })
        }
        AsmSyscallConvention::LinuxAarch64 if num == 56 => {
            // openat(dirfd, path, flags, mode)
            let dirfd = args.get(0)?.clone();
            let dirfd = resolve_i64(&dirfd, instructions)
                .map_err(|e| {
                    eprintln!("[fp-native] Win32-to-POSIX arg resolution error: {e}");
                    e
                })
                .ok()
                .flatten()?;
            if dirfd != -100 {
                return None;
            }
            Some(SystemApiOp::Open {
                path: args.get(1)?.clone(),
                flags: args.get(2)?.clone(),
                mode: args.get(3)?.clone(),
                flag_style: PosixFlagStyle::Linux,
            })
        }
        AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64
            if num == 0x2000_0005 =>
        {
            Some(SystemApiOp::Open {
                path: args.get(0)?.clone(),
                flags: args.get(1)?.clone(),
                mode: args.get(2)?.clone(),
                flag_style: PosixFlagStyle::Darwin,
            })
        }
        AsmSyscallConvention::LinuxX86_64 if num == 8 => Some(SystemApiOp::Seek {
            fd: args.get(0)?.clone(),
            offset: args.get(1)?.clone(),
            whence: args.get(2)?.clone(),
        }),
        AsmSyscallConvention::LinuxAarch64 if num == 62 => Some(SystemApiOp::Seek {
            fd: args.get(0)?.clone(),
            offset: args.get(1)?.clone(),
            whence: args.get(2)?.clone(),
        }),
        AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64
            if num == 0x2000_00c7 =>
        {
            Some(SystemApiOp::Seek {
                fd: args.get(0)?.clone(),
                offset: args.get(1)?.clone(),
                whence: args.get(2)?.clone(),
            })
        }
        AsmSyscallConvention::LinuxX86_64 if num == 87 => Some(SystemApiOp::Unlink {
            path: args.get(0)?.clone(),
        }),
        AsmSyscallConvention::LinuxX86_64 if num == 263 => {
            // unlinkat(dirfd, path, flags)
            let dirfd = args.get(0)?.clone();
            let dirfd = resolve_i64(&dirfd, instructions)
                .map_err(|e| {
                    eprintln!("[fp-native] Win32-to-POSIX arg resolution error: {e}");
                    e
                })
                .ok()
                .flatten()?;
            if dirfd != -100 {
                return None;
            }
            let flags = args.get(2)?.clone();
            let flags = resolve_i64(&flags, instructions)
                .map_err(|e| {
                    eprintln!("[fp-native] Win32-to-POSIX arg resolution error: {e}");
                    e
                })
                .ok()
                .flatten()?;
            // AT_REMOVEDIR=0x200
            if (flags & 0x200) != 0 {
                return Some(SystemApiOp::Rmdir {
                    path: args.get(1)?.clone(),
                });
            }
            Some(SystemApiOp::Unlink {
                path: args.get(1)?.clone(),
            })
        }
        AsmSyscallConvention::LinuxAarch64 if num == 35 => {
            // unlinkat(dirfd, path, flags)
            let dirfd = args.get(0)?.clone();
            let dirfd = resolve_i64(&dirfd, instructions)
                .map_err(|e| {
                    eprintln!("[fp-native] Win32-to-POSIX arg resolution error: {e}");
                    e
                })
                .ok()
                .flatten()?;
            if dirfd != -100 {
                return None;
            }
            let flags = args.get(2)?.clone();
            let flags = resolve_i64(&flags, instructions)
                .map_err(|e| {
                    eprintln!("[fp-native] Win32-to-POSIX arg resolution error: {e}");
                    e
                })
                .ok()
                .flatten()?;
            if (flags & 0x200) != 0 {
                return Some(SystemApiOp::Rmdir {
                    path: args.get(1)?.clone(),
                });
            }
            Some(SystemApiOp::Unlink {
                path: args.get(1)?.clone(),
            })
        }
        AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64
            if num == 0x2000_000a =>
        {
            Some(SystemApiOp::Unlink {
                path: args.get(0)?.clone(),
            })
        }
        AsmSyscallConvention::LinuxX86_64 if num == 83 => Some(SystemApiOp::Mkdir {
            path: args.get(0)?.clone(),
            mode: args.get(1)?.clone(),
        }),
        AsmSyscallConvention::LinuxX86_64 if num == 258 => {
            // mkdirat(dirfd, path, mode)
            let dirfd = args.get(0)?.clone();
            let dirfd = resolve_i64(&dirfd, instructions)
                .map_err(|e| {
                    eprintln!("[fp-native] Win32-to-POSIX arg resolution error: {e}");
                    e
                })
                .ok()
                .flatten()?;
            if dirfd != -100 {
                return None;
            }
            Some(SystemApiOp::Mkdir {
                path: args.get(1)?.clone(),
                mode: args.get(2)?.clone(),
            })
        }
        AsmSyscallConvention::LinuxAarch64 if num == 34 => {
            // mkdirat(dirfd, path, mode)
            let dirfd = args.get(0)?.clone();
            let dirfd = resolve_i64(&dirfd, instructions)
                .map_err(|e| {
                    eprintln!("[fp-native] Win32-to-POSIX arg resolution error: {e}");
                    e
                })
                .ok()
                .flatten()?;
            if dirfd != -100 {
                return None;
            }
            Some(SystemApiOp::Mkdir {
                path: args.get(1)?.clone(),
                mode: args.get(2)?.clone(),
            })
        }
        AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64
            if num == 0x2000_0088 =>
        {
            Some(SystemApiOp::Mkdir {
                path: args.get(0)?.clone(),
                mode: args.get(1)?.clone(),
            })
        }
        AsmSyscallConvention::LinuxX86_64 if num == 84 => Some(SystemApiOp::Rmdir {
            path: args.get(0)?.clone(),
        }),
        AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64
            if num == 0x2000_0089 =>
        {
            Some(SystemApiOp::Rmdir {
                path: args.get(0)?.clone(),
            })
        }
        AsmSyscallConvention::LinuxX86_64 if num == 82 => Some(SystemApiOp::Rename {
            from: args.get(0)?.clone(),
            to: args.get(1)?.clone(),
        }),
        AsmSyscallConvention::LinuxX86_64 if num == 264 => {
            // renameat(olddirfd, oldpath, newdirfd, newpath)
            let olddirfd = resolve_i64(args.get(0)?, instructions)
                .map_err(|e| {
                    eprintln!("[fp-native] Win32-to-POSIX arg resolution error: {e}");
                    e
                })
                .ok()
                .flatten()?;
            let newdirfd = resolve_i64(args.get(2)?, instructions)
                .map_err(|e| {
                    eprintln!("[fp-native] Win32-to-POSIX arg resolution error: {e}");
                    e
                })
                .ok()
                .flatten()?;
            if olddirfd != -100 || newdirfd != -100 {
                return None;
            }
            Some(SystemApiOp::Rename {
                from: args.get(1)?.clone(),
                to: args.get(3)?.clone(),
            })
        }
        AsmSyscallConvention::LinuxAarch64 if num == 38 => {
            let olddirfd = resolve_i64(args.get(0)?, instructions)
                .map_err(|e| {
                    eprintln!("[fp-native] Win32-to-POSIX arg resolution error: {e}");
                    e
                })
                .ok()
                .flatten()?;
            let newdirfd = resolve_i64(args.get(2)?, instructions)
                .map_err(|e| {
                    eprintln!("[fp-native] Win32-to-POSIX arg resolution error: {e}");
                    e
                })
                .ok()
                .flatten()?;
            if olddirfd != -100 || newdirfd != -100 {
                return None;
            }
            Some(SystemApiOp::Rename {
                from: args.get(1)?.clone(),
                to: args.get(3)?.clone(),
            })
        }
        AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64
            if num == 0x2000_0080 =>
        {
            Some(SystemApiOp::Rename {
                from: args.get(0)?.clone(),
                to: args.get(1)?.clone(),
            })
        }
        AsmSyscallConvention::LinuxX86_64 if num == 21 => Some(SystemApiOp::Access {
            path: args.get(0)?.clone(),
            mode: args.get(1)?.clone(),
        }),
        AsmSyscallConvention::LinuxX86_64 if num == 269 => {
            // faccessat(dirfd, path, mode, flags)
            let dirfd = resolve_i64(args.get(0)?, instructions)
                .map_err(|e| {
                    eprintln!("[fp-native] Win32-to-POSIX arg resolution error: {e}");
                    e
                })
                .ok()
                .flatten()?;
            if dirfd != -100 {
                return None;
            }
            Some(SystemApiOp::Access {
                path: args.get(1)?.clone(),
                mode: args.get(2)?.clone(),
            })
        }
        AsmSyscallConvention::LinuxAarch64 if num == 48 => {
            // faccessat(dirfd, path, mode, flags)
            let dirfd = resolve_i64(args.get(0)?, instructions)
                .map_err(|e| {
                    eprintln!("[fp-native] Win32-to-POSIX arg resolution error: {e}");
                    e
                })
                .ok()
                .flatten()?;
            if dirfd != -100 {
                return None;
            }
            Some(SystemApiOp::Access {
                path: args.get(1)?.clone(),
                mode: args.get(2)?.clone(),
            })
        }
        AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64
            if num == 0x2000_0021 =>
        {
            Some(SystemApiOp::Access {
                path: args.get(0)?.clone(),
                mode: args.get(1)?.clone(),
            })
        }
        AsmSyscallConvention::LinuxX86_64 if num == 9 => Some(SystemApiOp::Mmap {
            addr: args.get(0)?.clone(),
            len: args.get(1)?.clone(),
            prot: args.get(2)?.clone(),
            flags: args.get(3)?.clone(),
            fd: args.get(4)?.clone(),
            offset: args.get(5)?.clone(),
        }),
        AsmSyscallConvention::LinuxAarch64 if num == 222 => Some(SystemApiOp::Mmap {
            addr: args.get(0)?.clone(),
            len: args.get(1)?.clone(),
            prot: args.get(2)?.clone(),
            flags: args.get(3)?.clone(),
            fd: args.get(4)?.clone(),
            offset: args.get(5)?.clone(),
        }),
        AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64
            if num == 0x2000_00c5 =>
        {
            Some(SystemApiOp::Mmap {
                addr: args.get(0)?.clone(),
                len: args.get(1)?.clone(),
                prot: args.get(2)?.clone(),
                flags: args.get(3)?.clone(),
                fd: args.get(4)?.clone(),
                offset: args.get(5)?.clone(),
            })
        }
        AsmSyscallConvention::LinuxX86_64 if num == 11 => Some(SystemApiOp::Munmap {
            addr: args.get(0)?.clone(),
            len: args.get(1)?.clone(),
        }),
        AsmSyscallConvention::LinuxAarch64 if num == 215 => Some(SystemApiOp::Munmap {
            addr: args.get(0)?.clone(),
            len: args.get(1)?.clone(),
        }),
        AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64
            if num == 0x2000_0049 =>
        {
            Some(SystemApiOp::Munmap {
                addr: args.get(0)?.clone(),
                len: args.get(1)?.clone(),
            })
        }
        _ => None,
    }
}

pub(super) enum LoweredWindows {
    Unchanged,
    Single(AsmInstruction),
    Sequence(Vec<AsmInstruction>),
}
