use super::*;

pub(super) fn lower_system_api_to_syscall(
    op: SystemApiOp,
    convention: AsmSyscallConvention,
) -> AsmInstructionKind {
    match op {
        SystemApiOp::Exit { code } => {
            let number = match convention {
                AsmSyscallConvention::LinuxX86_64 => 60,
                AsmSyscallConvention::LinuxAarch64 => 93,
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    0x2000_0001
                }
            };
            AsmInstructionKind::Syscall {
                convention,
                number: AsmValue::Constant(AsmConstant::UInt(number, AsmType::I64)),
                args: vec![code],
            }
        }
        SystemApiOp::GetPid => {
            let number = match convention {
                AsmSyscallConvention::LinuxX86_64 => 39,
                AsmSyscallConvention::LinuxAarch64 => 172,
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    0x2000_0014
                }
            };
            AsmInstructionKind::Syscall {
                convention,
                number: AsmValue::Constant(AsmConstant::UInt(number, AsmType::I64)),
                args: Vec::new(),
            }
        }
        SystemApiOp::GetTid => {
            let number = match convention {
                AsmSyscallConvention::LinuxX86_64 => 186,
                AsmSyscallConvention::LinuxAarch64 => 178,
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    // No stable cross-version darwin thread id syscall.
                    0
                }
            };
            AsmInstructionKind::Syscall {
                convention,
                number: AsmValue::Constant(AsmConstant::UInt(number, AsmType::I64)),
                args: Vec::new(),
            }
        }
        SystemApiOp::Write { fd, buffer, len } => {
            let number = match convention {
                AsmSyscallConvention::LinuxX86_64 => 1,
                AsmSyscallConvention::LinuxAarch64 => 64,
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    0x2000_0004
                }
            };
            AsmInstructionKind::Syscall {
                convention,
                number: AsmValue::Constant(AsmConstant::UInt(number, AsmType::I64)),
                args: vec![fd, buffer, len],
            }
        }
        SystemApiOp::Dlopen { .. } | SystemApiOp::Dlsym { .. } | SystemApiOp::Dlclose { .. } => {
            AsmInstructionKind::Freeze(AsmValue::Undef(AsmType::I64))
        }
        SystemApiOp::Opendir { .. }
        | SystemApiOp::Readdir { .. }
        | SystemApiOp::Closedir { .. } => {
            unreachable!("directory SysOps must not be lowered via syscalls")
        }
        SystemApiOp::Unlink { path } => {
            let (number, args) = match convention {
                AsmSyscallConvention::LinuxX86_64 => (87, vec![path]),
                AsmSyscallConvention::LinuxAarch64 => (
                    35,
                    vec![
                        AsmValue::Constant(AsmConstant::Int(-100, AsmType::I64)),
                        path,
                        AsmValue::Constant(AsmConstant::UInt(0, AsmType::I64)),
                    ],
                ),
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    (0x2000_000a, vec![path])
                }
            };
            AsmInstructionKind::Syscall {
                convention,
                number: AsmValue::Constant(AsmConstant::UInt(number, AsmType::I64)),
                args,
            }
        }
        SystemApiOp::Mkdir { path, mode } => {
            let (number, args) = match convention {
                AsmSyscallConvention::LinuxX86_64 => (83, vec![path, mode]),
                AsmSyscallConvention::LinuxAarch64 => (
                    34,
                    vec![
                        AsmValue::Constant(AsmConstant::Int(-100, AsmType::I64)),
                        path,
                        mode,
                    ],
                ),
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    (0x2000_0088, vec![path, mode])
                }
            };
            AsmInstructionKind::Syscall {
                convention,
                number: AsmValue::Constant(AsmConstant::UInt(number, AsmType::I64)),
                args,
            }
        }
        SystemApiOp::Rmdir { path } => {
            let (number, args) = match convention {
                AsmSyscallConvention::LinuxX86_64 => (84, vec![path]),
                AsmSyscallConvention::LinuxAarch64 => (
                    35,
                    vec![
                        AsmValue::Constant(AsmConstant::Int(-100, AsmType::I64)),
                        path,
                        AsmValue::Constant(AsmConstant::Int(0x200, AsmType::I64)),
                    ],
                ),
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    (0x2000_0089, vec![path])
                }
            };
            AsmInstructionKind::Syscall {
                convention,
                number: AsmValue::Constant(AsmConstant::UInt(number, AsmType::I64)),
                args,
            }
        }
        SystemApiOp::Rename { from, to } => {
            let (number, args) = match convention {
                AsmSyscallConvention::LinuxX86_64 => (82, vec![from, to]),
                AsmSyscallConvention::LinuxAarch64 => (
                    38,
                    vec![
                        AsmValue::Constant(AsmConstant::Int(-100, AsmType::I64)),
                        from,
                        AsmValue::Constant(AsmConstant::Int(-100, AsmType::I64)),
                        to,
                    ],
                ),
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    (0x2000_0080, vec![from, to])
                }
            };
            AsmInstructionKind::Syscall {
                convention,
                number: AsmValue::Constant(AsmConstant::UInt(number, AsmType::I64)),
                args,
            }
        }
        SystemApiOp::Access { path, mode } => {
            let (number, args) = match convention {
                AsmSyscallConvention::LinuxX86_64 => (21, vec![path, mode]),
                AsmSyscallConvention::LinuxAarch64 => (
                    48,
                    vec![
                        AsmValue::Constant(AsmConstant::Int(-100, AsmType::I64)),
                        path,
                        mode,
                        AsmValue::Constant(AsmConstant::UInt(0, AsmType::I64)),
                    ],
                ),
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    (0x2000_0021, vec![path, mode])
                }
            };
            AsmInstructionKind::Syscall {
                convention,
                number: AsmValue::Constant(AsmConstant::UInt(number, AsmType::I64)),
                args,
            }
        }
        SystemApiOp::Read { fd, buffer, len } => {
            let number = match convention {
                AsmSyscallConvention::LinuxX86_64 => 0,
                AsmSyscallConvention::LinuxAarch64 => 63,
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    0x2000_0003
                }
            };
            AsmInstructionKind::Syscall {
                convention,
                number: AsmValue::Constant(AsmConstant::UInt(number, AsmType::I64)),
                args: vec![fd, buffer, len],
            }
        }
        SystemApiOp::Close { fd } => {
            let number = match convention {
                AsmSyscallConvention::LinuxX86_64 => 3,
                AsmSyscallConvention::LinuxAarch64 => 57,
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    0x2000_0006
                }
            };
            AsmInstructionKind::Syscall {
                convention,
                number: AsmValue::Constant(AsmConstant::UInt(number, AsmType::I64)),
                args: vec![fd],
            }
        }
        SystemApiOp::Open {
            path, flags, mode, ..
        } => {
            let number = match convention {
                AsmSyscallConvention::LinuxX86_64 => 2,
                AsmSyscallConvention::LinuxAarch64 => 56,
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    0x2000_0005
                }
            };
            let args = match convention {
                AsmSyscallConvention::LinuxAarch64 => vec![
                    AsmValue::Constant(AsmConstant::Int(-100, AsmType::I64)),
                    path,
                    flags,
                    mode,
                ],
                _ => vec![path, flags, mode],
            };
            AsmInstructionKind::Syscall {
                convention,
                number: AsmValue::Constant(AsmConstant::UInt(number, AsmType::I64)),
                args,
            }
        }
        SystemApiOp::Seek { fd, offset, whence } => {
            let number = match convention {
                AsmSyscallConvention::LinuxX86_64 => 8,
                AsmSyscallConvention::LinuxAarch64 => 62,
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    0x2000_00c7
                }
            };
            AsmInstructionKind::Syscall {
                convention,
                number: AsmValue::Constant(AsmConstant::UInt(number, AsmType::I64)),
                args: vec![fd, offset, whence],
            }
        }
        SystemApiOp::Mmap {
            addr,
            len,
            prot,
            flags,
            fd,
            offset,
        } => {
            let number = match convention {
                AsmSyscallConvention::LinuxX86_64 => 9,
                AsmSyscallConvention::LinuxAarch64 => 222,
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    0x2000_00c5
                }
            };
            AsmInstructionKind::Syscall {
                convention,
                number: AsmValue::Constant(AsmConstant::UInt(number, AsmType::I64)),
                args: vec![addr, len, prot, flags, fd, offset],
            }
        }
        SystemApiOp::Munmap { addr, len } => {
            let number = match convention {
                AsmSyscallConvention::LinuxX86_64 => 11,
                AsmSyscallConvention::LinuxAarch64 => 215,
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    0x2000_0049
                }
            };
            AsmInstructionKind::Syscall {
                convention,
                number: AsmValue::Constant(AsmConstant::UInt(number, AsmType::I64)),
                args: vec![addr, len],
            }
        }
    }
}
