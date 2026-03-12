use fp_core::asmir::{
    AsmConstant, AsmInstructionKind, AsmObjectFormat, AsmProgram, AsmSyscallConvention, AsmType,
    AsmValue,
};
use fp_core::error::Result;
use fp_core::lir::CallingConvention;

#[derive(Debug, Clone, PartialEq)]
pub enum SystemApiOp {
    Exit { code: AsmValue },
}

#[derive(Debug, Clone, PartialEq)]
pub enum SystemApiMechanism {
    LinuxSyscall,
    DarwinSyscall,
    WindowsImport { dll: String, name: String },
}

pub fn rewrite_program_for_target(program: &mut AsmProgram) -> Result<()> {
    if program.target.object_format == AsmObjectFormat::Coff
        || program.target.object_format == AsmObjectFormat::Pe
    {
        // Windows: avoid raw syscalls; prefer import calls.
        rewrite_syscalls_to_windows_imports(program);
    } else {
        // Unix targets: if we see a known Windows import, rewrite it back to syscalls.
        rewrite_windows_imports_to_syscalls(program);
    }
    Ok(())
}

fn rewrite_syscalls_to_windows_imports(program: &mut AsmProgram) {
    for function in &mut program.functions {
        if function.is_declaration {
            continue;
        }

        for block in &mut function.basic_blocks {
            let instructions_snapshot = block.instructions.clone();
            for inst in &mut block.instructions {
                let inst_id = inst.id;
                let AsmInstructionKind::Syscall {
                    convention,
                    number,
                    args,
                } = &inst.kind
                else {
                    continue;
                };

                let Some(op) = detect_system_api_from_syscall(
                    convention,
                    number,
                    args,
                    &instructions_snapshot,
                ) else {
                    continue;
                };

                if let Some(rewritten) = lower_system_api_to_windows_import(op) {
                    inst.kind = rewritten;
                    inst.opcode =
                        fp_core::asmir::AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Call);
                    inst.type_hint = Some(AsmType::Void);

                    if let fp_core::asmir::AsmTerminator::Return(Some(value)) = &block.terminator {
                        if value == &AsmValue::Register(inst_id) {
                            block.terminator = fp_core::asmir::AsmTerminator::Return(None);
                        }
                    }
                }
            }
        }
    }
}

fn rewrite_windows_imports_to_syscalls(program: &mut AsmProgram) {
    let convention = match program.target.object_format {
        AsmObjectFormat::Elf => match program.target.architecture {
            fp_core::asmir::AsmArchitecture::X86_64 => Some(AsmSyscallConvention::LinuxX86_64),
            fp_core::asmir::AsmArchitecture::Aarch64 => Some(AsmSyscallConvention::LinuxAarch64),
            _ => None,
        },
        AsmObjectFormat::MachO => match program.target.architecture {
            fp_core::asmir::AsmArchitecture::X86_64 => Some(AsmSyscallConvention::DarwinX86_64),
            fp_core::asmir::AsmArchitecture::Aarch64 => Some(AsmSyscallConvention::DarwinAarch64),
            _ => None,
        },
        _ => None,
    };

    let Some(convention) = convention else {
        return;
    };

    for function in &mut program.functions {
        if function.is_declaration {
            continue;
        }

        for block in &mut function.basic_blocks {
            for inst in &mut block.instructions {
                let AsmInstructionKind::Call { function, args, .. } = &inst.kind else {
                    continue;
                };

                let Some(op) = detect_system_api_from_windows_import(function, args) else {
                    continue;
                };

                inst.kind = lower_system_api_to_syscall(op, convention);
                inst.opcode =
                    fp_core::asmir::AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Syscall);
                inst.type_hint = Some(AsmType::I64);
            }
        }
    }
}

fn detect_system_api_from_windows_import(
    function: &AsmValue,
    args: &[AsmValue],
) -> Option<SystemApiOp> {
    let AsmValue::Function(name) = function else {
        return None;
    };
    let (dll, proc_name) = split_import_symbol(name);
    if !dll.eq_ignore_ascii_case("kernel32.dll") {
        return None;
    }
    if proc_name != "ExitProcess" {
        return None;
    }
    let code = args
        .first()
        .cloned()
        .unwrap_or_else(|| AsmValue::Constant(AsmConstant::UInt(0, AsmType::I32)));
    Some(SystemApiOp::Exit { code })
}

fn detect_system_api_from_syscall(
    convention: &AsmSyscallConvention,
    number: &AsmValue,
    args: &[AsmValue],
    instructions: &[fp_core::asmir::AsmInstruction],
) -> Option<SystemApiOp> {
    let num = resolve_u64(number, instructions)?;
    let is_exit = match convention {
        AsmSyscallConvention::LinuxX86_64 => num == 60,
        AsmSyscallConvention::LinuxAarch64 => num == 93,
        AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
            num == 0x2000_0001
        }
    };
    if !is_exit {
        return None;
    }
    let code = args
        .first()
        .cloned()
        .unwrap_or_else(|| AsmValue::Constant(AsmConstant::UInt(0, AsmType::I32)));
    Some(SystemApiOp::Exit { code })
}

fn lower_system_api_to_windows_import(op: SystemApiOp) -> Option<AsmInstructionKind> {
    match op {
        SystemApiOp::Exit { code } => Some(AsmInstructionKind::Call {
            function: AsmValue::Function("kernel32!ExitProcess".to_string()),
            args: vec![code],
            calling_convention: CallingConvention::Win64,
            tail_call: false,
        }),
    }
}

fn lower_system_api_to_syscall(
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
    }
}

fn resolve_u64(value: &AsmValue, instructions: &[fp_core::asmir::AsmInstruction]) -> Option<u64> {
    match value {
        AsmValue::Constant(AsmConstant::UInt(x, _)) => Some(*x),
        AsmValue::Constant(AsmConstant::Int(x, _)) => (*x).try_into().ok(),
        AsmValue::Register(id) => {
            let inst = instructions.iter().find(|inst| inst.id == *id)?;
            match &inst.kind {
                AsmInstructionKind::Freeze(inner) => resolve_u64(inner, instructions),
                _ => None,
            }
        }
        _ => None,
    }
}

fn split_import_symbol(symbol: &str) -> (String, String) {
    const DEFAULT_DLL: &str = "msvcrt.dll";
    if let Some((dll, name)) = symbol.split_once('!') {
        let mut dll = dll.trim().to_string();
        if !dll.to_ascii_lowercase().ends_with(".dll") {
            dll.push_str(".dll");
        }
        return (dll, name.trim().to_string());
    }
    (DEFAULT_DLL.to_string(), symbol.to_string())
}
