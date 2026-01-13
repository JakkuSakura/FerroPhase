use fp_core::lir::{
    CallingConvention, LirBasicBlock, LirConstant, LirFunction, LirFunctionSignature, LirInstruction,
    LirInstructionKind, LirIntrinsicKind, LirProgram, LirTerminator, LirType, LirValue, Linkage,
    Name,
};
use fp_native::emit::{self, RelocKind, TargetArch, TargetFormat};
use fp_native::link::dump::dump_macho;

fn host_arch() -> TargetArch {
    if cfg!(target_arch = "x86_64") {
        TargetArch::X86_64
    } else if cfg!(target_arch = "aarch64") {
        TargetArch::Aarch64
    } else {
        panic!("unsupported test architecture");
    }
}

#[cfg(target_arch = "x86_64")]
fn read_u32_le(bytes: &[u8], offset: usize) -> u32 {
    let chunk: [u8; 4] = bytes[offset..offset + 4].try_into().unwrap();
    u32::from_le_bytes(chunk)
}

#[cfg(target_arch = "x86_64")]
fn pe_data_directory(bytes: &[u8], index: usize) -> Option<(u32, u32)> {
    if bytes.len() < 0x40 {
        return None;
    }
    let pe_offset = read_u32_le(bytes, 0x3c) as usize;
    if bytes.len() < pe_offset + 4 + 20 + 0x70 + (index + 1) * 8 {
        return None;
    }
    let optional_offset = pe_offset + 4 + 20;
    let data_dir = optional_offset + 0x70 + index * 8;
    let rva = read_u32_le(bytes, data_dir);
    let size = read_u32_le(bytes, data_dir + 4);
    Some((rva, size))
}

fn read_u16_le(bytes: &[u8], offset: usize) -> u16 {
    let chunk: [u8; 2] = bytes[offset..offset + 2].try_into().unwrap();
    u16::from_le_bytes(chunk)
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

fn assert_range_in_file(label: &str, offset: u32, size: u32, len: usize) {
    if size == 0 {
        return;
    }
    let end = offset as usize + size as usize;
    assert!(
        end <= len,
        "{} out of range: {}..{} (len={})",
        label,
        offset,
        end,
        len
    );
}

fn read_u32(bytes: &[u8], offset: usize) -> u32 {
    let end = offset + 4;
    u32::from_le_bytes(bytes[offset..end].try_into().unwrap())
}

fn read_fixed_str(bytes: &[u8], offset: usize, len: usize) -> String {
    let end = offset + len;
    let raw = &bytes[offset..end];
    let s = std::str::from_utf8(raw).unwrap();
    s.trim_matches('\0').to_string()
}

fn text_section_file_offset(bytes: &[u8]) -> Option<u32> {
    const LC_SEGMENT_64: u32 = 0x19;
    let ncmds = read_u32(bytes, 16) as usize;
    let mut offset = 32usize;
    for _ in 0..ncmds {
        let cmd = read_u32(bytes, offset);
        let cmdsize = read_u32(bytes, offset + 4) as usize;
        if cmd == LC_SEGMENT_64 {
            let segname = read_fixed_str(bytes, offset + 8, 16);
            if segname == "__TEXT" {
                let nsects = read_u32(bytes, offset + 64) as usize;
                let section_base = offset + 72;
                for idx in 0..nsects {
                    let section_offset = section_base + idx * 80;
                    let sectname = read_fixed_str(bytes, section_offset, 16);
                    if sectname == "__text" {
                        return Some(read_u32(bytes, section_offset + 48));
                    }
                }
            }
        }
        offset += cmdsize;
    }
    None
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
fn emit_plan_records_function_symbols() {
    let arch = host_arch();
    let plan = emit::emit_plan(&program_with_many_call_args(), TargetFormat::Elf, arch).unwrap();
    assert!(plan.symbols.contains_key("main"));
    assert!(plan.symbols.contains_key("callee"));
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
fn macho_dump_has_expected_segments() {
    let arch = host_arch();
    let plan = emit::emit_plan(&minimal_program(), TargetFormat::MachO, arch).unwrap();
    let out_dir = tempfile::tempdir().unwrap();
    let exe = out_dir.path().join("dump.macho");
    emit::write_executable(&exe, &plan).unwrap();
    let bytes = std::fs::read(&exe).unwrap();
    let dump = dump_macho(&bytes).unwrap();
    assert!(dump.segments.iter().any(|seg| seg.name == "__TEXT"));
    assert!(dump.segments.iter().any(|seg| seg.name == "__LINKEDIT"));
}

#[test]
#[cfg(any(target_arch = "x86_64", target_arch = "aarch64"))]
fn macho_dump_offsets_stay_in_file() {
    let arch = host_arch();
    let plan = emit::emit_plan(&program_with_print(), TargetFormat::MachO, arch).unwrap();
    let out_dir = tempfile::tempdir().unwrap();
    let exe = out_dir.path().join("dump-printf.macho");
    emit::write_executable(&exe, &plan).unwrap();
    let bytes = std::fs::read(&exe).unwrap();
    let dump = dump_macho(&bytes).unwrap();
    if let Some(info) = dump.dyld_info {
        assert_range_in_file("rebase", info.rebase_off, info.rebase_size, bytes.len());
        assert_range_in_file("bind", info.bind_off, info.bind_size, bytes.len());
        assert_range_in_file("lazy_bind", info.lazy_bind_off, info.lazy_bind_size, bytes.len());
        assert_range_in_file("export", info.export_off, info.export_size, bytes.len());
    }
    if let Some(symtab) = dump.symtab {
        let sym_bytes = symtab.nsyms.saturating_mul(16);
        assert_range_in_file("symtab", symtab.symoff, sym_bytes, bytes.len());
        assert_range_in_file("strtab", symtab.stroff, symtab.strsize, bytes.len());
    }
    if let Some(dysymtab) = dump.dysymtab {
        let indirect_bytes = dysymtab.nindirectsyms.saturating_mul(4);
        assert_range_in_file(
            "indirectsym",
            dysymtab.indirectsymoff,
            indirect_bytes,
            bytes.len(),
        );
    }
}

#[test]
#[cfg(any(target_arch = "x86_64", target_arch = "aarch64"))]
fn macho_text_section_does_not_overlap_load_commands() {
    let arch = host_arch();
    let plan = emit::emit_plan(&program_with_print(), TargetFormat::MachO, arch).unwrap();
    let out_dir = tempfile::tempdir().unwrap();
    let exe = out_dir.path().join("codesign-slack.macho");
    emit::write_executable(&exe, &plan).unwrap();
    let bytes = std::fs::read(&exe).unwrap();
    let sizeofcmds = read_u32(&bytes, 20);
    let header_end = 32u32 + sizeofcmds;
    let text_offset = text_section_file_offset(&bytes).expect("missing __text section");
    assert!(
        text_offset >= header_end,
        "__text offset {} overlaps load commands (header_end={})",
        text_offset,
        header_end
    );
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

#[test]
#[cfg(target_arch = "x86_64")]
fn pe_executable_emits_base_relocs() {
    let arch = host_arch();
    let plan = emit::emit_plan(&program_with_print(), TargetFormat::Coff, arch).unwrap();
    let out_dir = tempfile::tempdir().unwrap();
    let exe = out_dir.path().join("printf-reloc.exe");
    emit::write_executable(&exe, &plan).unwrap();
    let bytes = std::fs::read(&exe).unwrap();
    let (_rva, size) = pe_data_directory(&bytes, 5).expect("missing PE data directory");
    assert!(size > 0, "expected base reloc directory to be populated");
}

#[test]
#[cfg(any(target_arch = "x86_64", target_arch = "aarch64"))]
fn coff_object_records_relocs() {
    let arch = host_arch();
    let plan = emit::emit_plan(&program_with_print(), TargetFormat::Coff, arch).unwrap();
    let out_dir = tempfile::tempdir().unwrap();
    let obj = out_dir.path().join("printf.obj");
    emit::write_object(&obj, &plan).unwrap();
    let bytes = std::fs::read(&obj).unwrap();
    let section_count = read_u16_le(&bytes, 2) as usize;
    let section_offset = 20;
    let text_header = section_offset;
    let reloc_count = read_u16_le(&bytes, text_header + 32);
    assert!(reloc_count > 0, "expected COFF relocations in .text");
    let _ = section_count;
}

#[test]
#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
fn elf_executable_runs_printf() {
    use std::process::Command;

    let arch = host_arch();
    let plan = emit::emit_plan(&program_with_print(), TargetFormat::Elf, arch).unwrap();
    let out_dir = tempfile::tempdir().unwrap();
    let exe = out_dir.path().join("printf-run.elf");
    emit::write_executable(&exe, &plan).unwrap();
    let output = Command::new(&exe).output().unwrap();
    assert!(output.status.success());
    assert_eq!(output.stdout, b"hello from native\n");
}
