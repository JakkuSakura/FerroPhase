use fp_core::asmir::{
    AsmArchitecture, AsmFunction, AsmInstructionKind, AsmObjectFormat, AsmPhysicalRegister,
    AsmRegisterBank, AsmValue,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Abi {
    X86_64SysV,
    X86_64Win64,
    Aarch64Aapcs64,
}

pub fn raise_implicit_return_value(function: &mut AsmFunction, abi: Abi) {
    if matches!(function.signature.return_type, fp_core::lir::Ty::Void) {
        function.signature.return_type = fp_core::lir::Ty::I64;
    }

    let ret_reg = abi_int_return_register(abi);
    for block in &mut function.basic_blocks {
        let fp_core::asmir::AsmTerminator::Return(None) = &block.terminator else {
            continue;
        };
        block.terminator = fp_core::asmir::AsmTerminator::Return(Some(AsmValue::PhysicalRegister(
            abi_register(ret_reg),
        )));
    }
}

pub fn default_abi_for_target(arch: &AsmArchitecture, format: &AsmObjectFormat) -> Option<Abi> {
    match (arch, format) {
        (AsmArchitecture::X86_64, AsmObjectFormat::Coff | AsmObjectFormat::Pe) => {
            Some(Abi::X86_64Win64)
        }
        (AsmArchitecture::X86_64, _) => Some(Abi::X86_64SysV),
        (AsmArchitecture::Aarch64, _) => Some(Abi::Aarch64Aapcs64),
        _ => None,
    }
}

pub fn abi_int_arg_registers(abi: Abi) -> &'static [&'static str] {
    match abi {
        Abi::X86_64SysV => &["rdi", "rsi", "rdx", "rcx", "r8", "r9"],
        Abi::X86_64Win64 => &["rcx", "rdx", "r8", "r9"],
        Abi::Aarch64Aapcs64 => &["x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7"],
    }
}

pub fn abi_int_return_register(abi: Abi) -> &'static str {
    match abi {
        Abi::X86_64SysV | Abi::X86_64Win64 => "rax",
        Abi::Aarch64Aapcs64 => "x0",
    }
}

pub fn raise_implicit_call_arguments(function: &mut AsmFunction, abi: Abi) {
    let arg_regs = abi_int_arg_registers(abi);
    for block in &mut function.basic_blocks {
        for inst in &mut block.instructions {
            let AsmInstructionKind::Call { args, .. } = &mut inst.kind else {
                continue;
            };
            if !args.is_empty() {
                continue;
            }
            *args = arg_regs
                .iter()
                .map(|name| AsmValue::PhysicalRegister(abi_register(name)))
                .collect();
        }
    }
}

fn abi_register(name: &str) -> AsmPhysicalRegister {
    AsmPhysicalRegister {
        name: name.to_string(),
        bank: AsmRegisterBank::General,
        size_bits: 64,
    }
}
