use fp_core::lir::{
    CallingConvention, LirBasicBlock, LirConstant, LirFunction, LirFunctionSignature, LirInstruction,
    LirInstructionKind, LirIntrinsicKind, LirProgram, LirTerminator, LirType, LirValue, Linkage,
    Name,
};
use fp_native::emit::{self, RelocKind, TargetArch, TargetFormat};

fn host_arch() -> TargetArch {
    if cfg!(target_arch = "x86_64") {
        TargetArch::X86_64
    } else if cfg!(target_arch = "aarch64") {
        TargetArch::Aarch64
    } else {
        panic!("unsupported test architecture");
    }
}

fn minimal_program() -> LirProgram {
    let func = LirFunction {
        name: Name::new("main"),
        signature: LirFunctionSignature {
            params: Vec::new(),
            return_type: LirType::I32,
            is_variadic: false,
        },
        basic_blocks: vec![LirBasicBlock {
            id: 0,
            label: Some(Name::new("entry")),
            instructions: Vec::new(),
            terminator: LirTerminator::Return(None),
            predecessors: Vec::new(),
            successors: Vec::new(),
        }],
        locals: Vec::new(),
        stack_slots: Vec::new(),
        calling_convention: CallingConvention::C,
        linkage: Linkage::External,
    };

    LirProgram {
        functions: vec![func],
        globals: Vec::new(),
        type_definitions: Vec::new(),
    }
}

fn program_with_many_call_args() -> LirProgram {
    let callee = LirFunction {
        name: Name::new("callee"),
        signature: LirFunctionSignature {
            params: vec![LirType::I64; 10],
            return_type: LirType::I64,
            is_variadic: false,
        },
        basic_blocks: vec![LirBasicBlock {
            id: 0,
            label: Some(Name::new("entry")),
            instructions: Vec::new(),
            terminator: LirTerminator::Return(Some(LirValue::Constant(LirConstant::Int(
                0,
                LirType::I64,
            )))),
            predecessors: Vec::new(),
            successors: Vec::new(),
        }],
        locals: Vec::new(),
        stack_slots: Vec::new(),
        calling_convention: CallingConvention::C,
        linkage: Linkage::External,
    };

    let call_id = 1;
    let args = (0..10)
        .map(|idx| LirValue::Constant(LirConstant::Int(idx, LirType::I64)))
        .collect();
    let call_inst = LirInstruction {
        id: call_id,
        kind: LirInstructionKind::Call {
            function: LirValue::Function("callee".to_string()),
            args,
            calling_convention: CallingConvention::C,
            tail_call: false,
        },
        type_hint: Some(LirType::I64),
        debug_info: None,
    };

    let main = LirFunction {
        name: Name::new("main"),
        signature: LirFunctionSignature {
            params: Vec::new(),
            return_type: LirType::I64,
            is_variadic: false,
        },
        basic_blocks: vec![LirBasicBlock {
            id: 0,
            label: Some(Name::new("entry")),
            instructions: vec![call_inst],
            terminator: LirTerminator::Return(Some(LirValue::Register(call_id))),
            predecessors: Vec::new(),
            successors: Vec::new(),
        }],
        locals: Vec::new(),
        stack_slots: Vec::new(),
        calling_convention: CallingConvention::C,
        linkage: Linkage::External,
    };

    LirProgram {
        functions: vec![main, callee],
        globals: Vec::new(),
        type_definitions: Vec::new(),
    }
}

fn program_with_print() -> LirProgram {
    let print_inst = LirInstruction {
        id: 1,
        kind: LirInstructionKind::IntrinsicCall {
            kind: LirIntrinsicKind::Println,
            format: "hello from native\n".to_string(),
            args: Vec::new(),
        },
        type_hint: None,
        debug_info: None,
    };

    let func = LirFunction {
        name: Name::new("main"),
        signature: LirFunctionSignature {
            params: Vec::new(),
            return_type: LirType::I32,
            is_variadic: false,
        },
        basic_blocks: vec![LirBasicBlock {
            id: 0,
            label: Some(Name::new("entry")),
            instructions: vec![print_inst],
            terminator: LirTerminator::Return(None),
            predecessors: Vec::new(),
            successors: Vec::new(),
        }],
        locals: Vec::new(),
        stack_slots: Vec::new(),
        calling_convention: CallingConvention::C,
        linkage: Linkage::External,
    };

    LirProgram {
        functions: vec![func],
        globals: Vec::new(),
        type_definitions: Vec::new(),
    }
}

#[test]
#[cfg(any(target_arch = "x86_64", target_arch = "aarch64"))]
fn emitter_handles_stack_call_args() {
    let arch = host_arch();
    let plan = emit::emit_plan(&program_with_many_call_args(), TargetFormat::Elf, arch).unwrap();
    assert!(!plan.text.is_empty());
}

#[test]
#[cfg(any(target_arch = "x86_64", target_arch = "aarch64"))]
fn elf_executable_has_magic() {
    let arch = host_arch();
    let plan = emit::emit_plan(&minimal_program(), TargetFormat::Elf, arch).unwrap();
    let out_dir = tempfile::tempdir().unwrap();
    let exe = out_dir.path().join("smoke.elf");
    emit::write_executable(&exe, &plan).unwrap();
    let bytes = std::fs::read(&exe).unwrap();
    assert_eq!(&bytes[..4], b"\x7FELF");
}

#[test]
#[cfg(any(target_arch = "x86_64", target_arch = "aarch64"))]
fn macho_executable_has_magic() {
    let arch = host_arch();
    let plan = emit::emit_plan(&minimal_program(), TargetFormat::MachO, arch).unwrap();
    let out_dir = tempfile::tempdir().unwrap();
    let exe = out_dir.path().join("smoke.macho");
    emit::write_executable(&exe, &plan).unwrap();
    let bytes = std::fs::read(&exe).unwrap();
    let magic = u32::from_le_bytes(bytes[0..4].try_into().unwrap());
    assert_eq!(magic, 0xFEED_FACF);
}

#[test]
#[cfg(any(target_arch = "x86_64", target_arch = "aarch64"))]
fn pe_executable_has_magic() {
    let arch = host_arch();
    let plan = emit::emit_plan(&minimal_program(), TargetFormat::Coff, arch).unwrap();
    let out_dir = tempfile::tempdir().unwrap();
    let exe = out_dir.path().join("smoke.exe");
    emit::write_executable(&exe, &plan).unwrap();
    let bytes = std::fs::read(&exe).unwrap();
    assert_eq!(&bytes[..2], b"MZ");
    assert_eq!(&bytes[0x80..0x84], b"PE\0\0");
}

#[test]
#[cfg(any(target_arch = "x86_64", target_arch = "aarch64"))]
fn elf_executable_supports_printf() {
    let arch = host_arch();
    let plan = emit::emit_plan(&program_with_print(), TargetFormat::Elf, arch).unwrap();
    assert!(plan
        .relocs
        .iter()
        .any(|reloc| reloc.kind == RelocKind::CallRel32 && reloc.symbol == "printf"));
    assert!(plan
        .relocs
        .iter()
        .any(|reloc| reloc.kind == RelocKind::Abs64 && reloc.symbol == ".rodata"));

    let out_dir = tempfile::tempdir().unwrap();
    let exe = out_dir.path().join("printf.elf");
    emit::write_executable(&exe, &plan).unwrap();
    let bytes = std::fs::read(&exe).unwrap();
    assert_eq!(&bytes[..4], b"\x7FELF");
}

#[test]
#[cfg(any(target_arch = "x86_64", target_arch = "aarch64"))]
fn macho_executable_supports_printf() {
    let arch = host_arch();
    let plan = emit::emit_plan(&program_with_print(), TargetFormat::MachO, arch).unwrap();
    let out_dir = tempfile::tempdir().unwrap();
    let exe = out_dir.path().join("printf.macho");
    emit::write_executable(&exe, &plan).unwrap();
    let bytes = std::fs::read(&exe).unwrap();
    let magic = u32::from_le_bytes(bytes[0..4].try_into().unwrap());
    assert_eq!(magic, 0xFEED_FACF);
}

#[test]
#[cfg(any(target_arch = "x86_64", target_arch = "aarch64"))]
fn pe_executable_supports_printf() {
    let arch = host_arch();
    if matches!(arch, TargetArch::Aarch64) {
        return;
    }
    let plan = emit::emit_plan(&program_with_print(), TargetFormat::Coff, arch).unwrap();
    let out_dir = tempfile::tempdir().unwrap();
    let exe = out_dir.path().join("printf.exe");
    emit::write_executable(&exe, &plan).unwrap();
    let bytes = std::fs::read(&exe).unwrap();
    assert_eq!(&bytes[..2], b"MZ");
    assert_eq!(&bytes[0x80..0x84], b"PE\0\0");
}
