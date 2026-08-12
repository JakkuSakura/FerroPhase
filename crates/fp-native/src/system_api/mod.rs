#![allow(dead_code)]

use fp_core::asmir::{
    AsmAttr, AsmBlock, AsmConstant, AsmFunction, AsmFunctionSignature, AsmGenericOpcode, AsmGlobal,
    AsmGlobalRelocation, AsmInstruction, AsmLocal, AsmObjectFormat, AsmOpcode, AsmOperand,
    AsmProgram, AsmRegister, AsmRegisterBank, AsmRelocationKind, AsmSection, AsmSectionFlag,
    AsmSectionKind, AsmSysOp, AsmSyscallConvention, AsmTerminator, AsmType, AsmVirtualRegId,
    OperandAccess, PosixDirentStyle, PosixFlagStyle,
};
use fp_core::error::{Error, Result};
use fp_core::lir::{CallingConvention, Linkage, Name, Visibility};

type SystemApiOp = AsmSysOp;

// ---------------------------------------------------------------------------
// Shared instruction-construction helpers.
//
// This module synthesizes a large amount of glue AsmIR (libc/syscall shims,
// Windows-import <-> syscall rewriting, ...). All of it funnels through these
// helpers so every synthesized `AsmOperand` list matches the canonical
// per-opcode operand schema (see `fp_core::asmir::AsmInstruction`) in one
// place, and every synthesized virtual register goes through
// `AsmFunction::alloc_virtual_register` rather than being hand-numbered.
// ---------------------------------------------------------------------------

fn register_bank_for(ty: &AsmType) -> AsmRegisterBank {
    match ty {
        AsmType::F32 | AsmType::F64 => AsmRegisterBank::Float,
        AsmType::Vector(..) => AsmRegisterBank::Vector,
        _ => AsmRegisterBank::General,
    }
}

fn type_bits_for(ty: &AsmType) -> u16 {
    let bytes: u64 = match ty {
        AsmType::I1 | AsmType::I8 => 1,
        AsmType::I16 => 2,
        AsmType::I32 | AsmType::F32 => 4,
        AsmType::I64 | AsmType::F64 | AsmType::Ptr(_) | AsmType::Function { .. } => 8,
        AsmType::I128 => 16,
        AsmType::Integer(width) => u64::from(width.div_ceil(8)),
        AsmType::Array(element, count) => u64::from(type_bits_for(element) / 8) * *count,
        AsmType::Vector(element, count) => {
            u64::from(type_bits_for(element) / 8) * u64::from(*count)
        }
        AsmType::Struct { fields, .. } => fields
            .iter()
            .map(|field| u64::from(type_bits_for(field) / 8))
            .sum(),
        AsmType::Void | AsmType::Label | AsmType::Token | AsmType::Metadata | AsmType::Error => 0,
    };
    let bytes = bytes.min(u64::from(u16::MAX));
    if bytes == 0 {
        64
    } else {
        (bytes as u16).saturating_mul(8)
    }
}

/// Allocates a fresh virtual register in `function` for a value of type
/// `ty`, deriving its register bank/width from the type. This is the sole
/// place synthesized glue code should mint new registers.
fn alloc_result(function: &mut AsmFunction, ty: AsmType) -> AsmVirtualRegId {
    let bank = register_bank_for(&ty);
    let bits = type_bits_for(&ty);
    function.alloc_virtual_register(ty, bank, bits)
}

fn vreg_read(reg: AsmVirtualRegId) -> AsmOperand {
    AsmOperand::Register {
        reg: AsmRegister::Virtual(reg),
        access: OperandAccess::Read,
    }
}

fn vreg_write(reg: AsmVirtualRegId) -> AsmOperand {
    AsmOperand::Register {
        reg: AsmRegister::Virtual(reg),
        access: OperandAccess::Write,
    }
}

/// Builds a `Call` instruction with operands in the canonical
/// `[dest?, Attr(CallingConv), Attr(TailCall)?, target, arg...]` order, so
/// `AsmInstruction::call_target_and_args` keeps working on it.
fn build_call(
    id: u32,
    dest: Option<AsmVirtualRegId>,
    target: AsmOperand,
    args: Vec<AsmOperand>,
    calling_convention: CallingConvention,
    tail_call: bool,
) -> AsmInstruction {
    let mut operands = Vec::with_capacity(3 + args.len());
    if let Some(dest) = dest {
        operands.push(vreg_write(dest));
    }
    operands.push(AsmOperand::Attr(AsmAttr::CallingConv(calling_convention)));
    if tail_call {
        operands.push(AsmOperand::Attr(AsmAttr::TailCall));
    }
    operands.push(target);
    operands.extend(args);
    AsmInstruction::new(id, AsmOpcode::Generic(AsmGenericOpcode::Call), operands)
}

/// `build_call` against a named symbol (the overwhelmingly common case in
/// this file: calling a libc function by name).
fn build_call_symbol(
    id: u32,
    dest: Option<AsmVirtualRegId>,
    name: &str,
    args: Vec<AsmOperand>,
    calling_convention: CallingConvention,
) -> AsmInstruction {
    build_call(
        id,
        dest,
        AsmOperand::Symbol(Name::new(name)),
        args,
        calling_convention,
        false,
    )
}

fn build_unary(id: u32, opcode: AsmGenericOpcode, dest: AsmVirtualRegId, src: AsmOperand) -> AsmInstruction {
    AsmInstruction::new(id, AsmOpcode::Generic(opcode), vec![vreg_write(dest), src])
}

/// Registers (if not already present) a function with an empty body that
/// unconditionally returns `return_value`. Used for no-op compatibility
/// stubs (e.g. Darwin stand-ins for glibc-only APIs like libcap).
fn ensure_constant_stub_function(
    program: &mut AsmProgram,
    name: &str,
    params: Vec<(&str, AsmType)>,
    return_type: AsmType,
    return_value: AsmOperand,
) {
    let mut f = AsmFunction::new(
        Name::new(name),
        AsmFunctionSignature {
            params: params.iter().map(|(_, ty)| ty.clone()).collect(),
            return_type,
            is_variadic: false,
        },
    );
    f.locals = params
        .iter()
        .enumerate()
        .map(|(i, (pname, ty))| AsmLocal {
            id: i as u32,
            name: Some((*pname).to_string()),
            ty: ty.clone(),
            is_argument: true,
        })
        .collect();
    f.basic_blocks = vec![AsmBlock {
        id: 0,
        label: None,
        instructions: Vec::new(),
        terminator: AsmTerminator::Return(Some(return_value)),
        terminator_encoding: None,
        predecessors: Vec::new(),
        successors: Vec::new(),
    }];
    f.linkage = Linkage::External;
    f.visibility = Visibility::Default;
    f.calling_convention = Some(CallingConvention::C);
    f.section = Some(".text".to_string());
    f.is_declaration = false;
    ensure_function(program, f);
}

/// Registers (if not already present) a trampoline function that forwards
/// all of its arguments verbatim to `target` and returns its result. This
/// is the common "glibc-only alias forwards to an equivalent libc function"
/// shape used throughout this module (e.g. `fflush_unlocked` -> `fflush`).
fn ensure_forwarding_function(
    program: &mut AsmProgram,
    name: &str,
    params: Vec<(&str, AsmType)>,
    return_type: AsmType,
    target: &str,
) {
    let mut f = AsmFunction::new(
        Name::new(name),
        AsmFunctionSignature {
            params: params.iter().map(|(_, ty)| ty.clone()).collect(),
            return_type: return_type.clone(),
            is_variadic: false,
        },
    );
    f.locals = params
        .iter()
        .enumerate()
        .map(|(i, (pname, ty))| AsmLocal {
            id: i as u32,
            name: Some((*pname).to_string()),
            ty: ty.clone(),
            is_argument: true,
        })
        .collect();
    let args: Vec<AsmOperand> = (0..params.len() as u32).map(AsmOperand::Local).collect();
    let result = alloc_result(&mut f, return_type);
    f.basic_blocks = vec![AsmBlock {
        id: 0,
        label: None,
        instructions: vec![build_call_symbol(0, Some(result), target, args, CallingConvention::C)],
        terminator: AsmTerminator::Return(Some(vreg_read(result))),
        terminator_encoding: None,
        predecessors: Vec::new(),
        successors: Vec::new(),
    }];
    f.linkage = Linkage::External;
    f.visibility = Visibility::Default;
    f.calling_convention = Some(CallingConvention::C);
    f.section = Some(".text".to_string());
    f.is_declaration = false;
    ensure_function(program, f);
}

fn build_binop(
    id: u32,
    opcode: AsmGenericOpcode,
    dest: AsmVirtualRegId,
    lhs: AsmOperand,
    rhs: AsmOperand,
) -> AsmInstruction {
    AsmInstruction::new(
        id,
        AsmOpcode::Generic(opcode),
        vec![vreg_write(dest), lhs, rhs],
    )
}

fn build_load(id: u32, dest: AsmVirtualRegId, address: AsmOperand) -> AsmInstruction {
    AsmInstruction::new(
        id,
        AsmOpcode::Generic(AsmGenericOpcode::Load),
        vec![vreg_write(dest), address],
    )
}

fn build_store(id: u32, value: AsmOperand, address: AsmOperand) -> AsmInstruction {
    AsmInstruction::new(
        id,
        AsmOpcode::Generic(AsmGenericOpcode::Store),
        vec![value, address],
    )
}

fn build_alloca(id: u32, dest: AsmVirtualRegId, size: AsmOperand, alignment: u32) -> AsmInstruction {
    AsmInstruction::new(
        id,
        AsmOpcode::Generic(AsmGenericOpcode::Alloca),
        vec![vreg_write(dest), size, AsmOperand::Attr(AsmAttr::Alignment(alignment))],
    )
}

fn build_gep(
    id: u32,
    dest: AsmVirtualRegId,
    ptr: AsmOperand,
    indices: Vec<AsmOperand>,
) -> AsmInstruction {
    let mut operands = Vec::with_capacity(2 + indices.len());
    operands.push(vreg_write(dest));
    operands.push(ptr);
    operands.extend(indices);
    AsmInstruction::new(id, AsmOpcode::Generic(AsmGenericOpcode::GetElementPtr), operands)
}

fn build_eq(id: u32, dest: AsmVirtualRegId, lhs: AsmOperand, rhs: AsmOperand) -> AsmInstruction {
    build_binop(id, AsmGenericOpcode::Eq, dest, lhs, rhs)
}

fn build_select(
    id: u32,
    dest: AsmVirtualRegId,
    condition: AsmOperand,
    if_true: AsmOperand,
    if_false: AsmOperand,
) -> AsmInstruction {
    AsmInstruction::new(
        id,
        AsmOpcode::Generic(AsmGenericOpcode::Select),
        vec![vreg_write(dest), condition, if_true, if_false],
    )
}

/// Builds a `Syscall` instruction with operands in the canonical
/// `[dest?, Attr(SyscallConvention), number, arg...]` order.
fn build_syscall(
    id: u32,
    dest: Option<AsmVirtualRegId>,
    convention: AsmSyscallConvention,
    number: AsmOperand,
    args: Vec<AsmOperand>,
) -> AsmInstruction {
    let mut operands = Vec::with_capacity(2 + args.len());
    if let Some(dest) = dest {
        operands.push(vreg_write(dest));
    }
    operands.push(AsmOperand::Attr(AsmAttr::SyscallConvention(convention)));
    operands.push(number);
    operands.extend(args);
    AsmInstruction::new(id, AsmOpcode::Generic(AsmGenericOpcode::Syscall), operands)
}

/// Builds a `SysOp` instruction with operands in the canonical
/// `[dest?, SysOp(op)]` order.
fn build_sysop(id: u32, dest: Option<AsmVirtualRegId>, op: AsmSysOp) -> AsmInstruction {
    let mut operands = Vec::new();
    if let Some(dest) = dest {
        operands.push(vreg_write(dest));
    }
    operands.push(AsmOperand::SysOp(Box::new(op)));
    AsmInstruction::new(id, AsmOpcode::Generic(AsmGenericOpcode::SysOp), operands)
}

fn match_getfileattributes_sequence_to_syscall(
    instructions: &[AsmInstruction],
    convention: AsmSyscallConvention,
) -> Result<Option<(AsmInstruction, usize)>> {
    // Pattern:
    //   GetFileAttributesA; Eq; Select
    if instructions.len() < 3 {
        return Ok(None);
    }

    let call = &instructions[0];
    if !is_call_named(call, "kernel32.dll", "GetFileAttributesA") {
        return Ok(None);
    }
    let Some((_, args)) = call.call_target_and_args() else {
        return Ok(None);
    };
    if args.len() != 1 {
        return Ok(None);
    }

    match_kernel32_bool_call_sequence_to_syscall(
        instructions,
        "GetFileAttributesA",
        SystemApiOp::Access {
            path: args[0].clone(),
            mode: AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I32)),
        },
        convention,
    )
}

fn ensure_glibc_progname_globals(program: &mut AsmProgram) {
    ensure_global(
        program,
        AsmGlobal {
            name: Name::new("fp_linux_progname_default"),
            ty: AsmType::Array(Box::new(AsmType::I8), 1),
            initializer: Some(AsmConstant::Bytes(vec![0])),
            relocations: Vec::new(),
            section: Some(".rodata".to_string()),
            linkage: Linkage::Private,
            visibility: Visibility::Default,
            alignment: Some(1),
            is_constant: true,
        },
    );

    for name in [
        "__progname",
        "__progname_full",
        "program_invocation_name",
        "program_invocation_short_name",
    ] {
        ensure_global(
            program,
            AsmGlobal {
                name: Name::new(name),
                ty: AsmType::Ptr(Box::new(AsmType::I8)),
                initializer: Some(AsmConstant::Bytes(vec![0; 8])),
                relocations: vec![AsmGlobalRelocation {
                    offset: 0,
                    kind: AsmRelocationKind::Abs64,
                    symbol: Name::new("fp_linux_progname_default"),
                    addend: 0,
                }],
                section: Some(".data".to_string()),
                linkage: Linkage::External,
                visibility: Visibility::Default,
                alignment: Some(8),
                is_constant: false,
            },
        );
    }
}

fn ensure_glibc_overflow(program: &mut AsmProgram) -> Result<()> {
    // glibc uses `__overflow(FILE*, int)` as an internal stdio helper.
    // Provide a compatibility definition that forwards to libc `fputc`.
    let mut f = AsmFunction::new(
        Name::new("__overflow"),
        AsmFunctionSignature {
            params: vec![AsmType::Ptr(Box::new(AsmType::I8)), AsmType::I32],
            return_type: AsmType::I32,
            is_variadic: false,
        },
    );
    f.locals = vec![
        AsmLocal {
            id: 0,
            name: Some("stream".to_string()),
            ty: AsmType::Ptr(Box::new(AsmType::I8)),
            is_argument: true,
        },
        AsmLocal {
            id: 1,
            name: Some("ch".to_string()),
            ty: AsmType::I32,
            is_argument: true,
        },
    ];
    let result = alloc_result(&mut f, AsmType::I32);
    f.basic_blocks = vec![AsmBlock {
        id: 0,
        label: None,
        instructions: vec![build_call_symbol(
            0,
            Some(result),
            "fputc",
            vec![AsmOperand::Local(1), AsmOperand::Local(0)],
            CallingConvention::C,
        )],
        terminator: AsmTerminator::Return(Some(vreg_read(result))),
        terminator_encoding: None,
        predecessors: Vec::new(),
        successors: Vec::new(),
    }];
    f.linkage = Linkage::External;
    f.visibility = Visibility::Default;
    f.calling_convention = Some(CallingConvention::C);
    f.section = Some(".text".to_string());
    f.is_declaration = false;
    ensure_function(program, f);
    Ok(())
}

fn ensure_glibc_mempcpy(program: &mut AsmProgram) -> Result<()> {
    // Darwin libc doesn't provide mempcpy, but glibc-compiled binaries may.
    // This is a minimal, unsafe compatibility implementation.
    let mut f = AsmFunction::new(
        Name::new("mempcpy"),
        AsmFunctionSignature {
            params: vec![
                AsmType::Ptr(Box::new(AsmType::I8)),
                AsmType::Ptr(Box::new(AsmType::I8)),
                AsmType::I64,
            ],
            return_type: AsmType::Ptr(Box::new(AsmType::I8)),
            is_variadic: false,
        },
    );
    f.locals = vec![
        AsmLocal {
            id: 0,
            name: Some("dest".to_string()),
            ty: AsmType::Ptr(Box::new(AsmType::I8)),
            is_argument: true,
        },
        AsmLocal {
            id: 1,
            name: Some("src".to_string()),
            ty: AsmType::Ptr(Box::new(AsmType::I8)),
            is_argument: true,
        },
        AsmLocal {
            id: 2,
            name: Some("len".to_string()),
            ty: AsmType::I64,
            is_argument: true,
        },
    ];
    let memcpy_result = alloc_result(&mut f, AsmType::Ptr(Box::new(AsmType::I8)));
    let gep_result = alloc_result(&mut f, AsmType::Ptr(Box::new(AsmType::I8)));
    f.basic_blocks = vec![AsmBlock {
        id: 0,
        label: None,
        instructions: vec![
            build_call_symbol(
                0,
                Some(memcpy_result),
                "memcpy",
                vec![
                    AsmOperand::Local(0),
                    AsmOperand::Local(1),
                    AsmOperand::Local(2),
                ],
                CallingConvention::C,
            ),
            build_gep(1, gep_result, AsmOperand::Local(0), vec![AsmOperand::Local(2)]),
        ],
        terminator: AsmTerminator::Return(Some(vreg_read(gep_result))),
        terminator_encoding: None,
        predecessors: Vec::new(),
        successors: Vec::new(),
    }];
    f.linkage = Linkage::External;
    f.visibility = Visibility::Default;
    f.calling_convention = Some(CallingConvention::C);
    f.section = Some(".text".to_string());
    f.is_declaration = false;
    ensure_function(program, f);
    Ok(())
}

fn ensure_glibc_start_main(program: &mut AsmProgram) -> Result<()> {
    // Minimal Linux/glibc entry shim for Darwin targets.
    //
    // We only need this to satisfy references from lifted ELF `_start` code paths.
    // The fp-cli wrapper prefers calling `fp_lifted_main` directly.
    let mut f = AsmFunction::new(
        Name::new("__libc_start_main"),
        AsmFunctionSignature {
            params: vec![
                AsmType::Ptr(Box::new(AsmType::I8)),
                AsmType::I32,
                AsmType::Ptr(Box::new(AsmType::Ptr(Box::new(AsmType::I8)))),
                AsmType::Ptr(Box::new(AsmType::I8)),
                AsmType::Ptr(Box::new(AsmType::I8)),
                AsmType::Ptr(Box::new(AsmType::I8)),
                AsmType::Ptr(Box::new(AsmType::I8)),
            ],
            return_type: AsmType::I32,
            is_variadic: false,
        },
    );
    f.locals = vec![
        AsmLocal {
            id: 0,
            name: Some("main".to_string()),
            ty: AsmType::Ptr(Box::new(AsmType::I8)),
            is_argument: true,
        },
        AsmLocal {
            id: 1,
            name: Some("argc".to_string()),
            ty: AsmType::I32,
            is_argument: true,
        },
        AsmLocal {
            id: 2,
            name: Some("argv".to_string()),
            ty: AsmType::Ptr(Box::new(AsmType::Ptr(Box::new(AsmType::I8)))),
            is_argument: true,
        },
        AsmLocal {
            id: 3,
            name: Some("init".to_string()),
            ty: AsmType::Ptr(Box::new(AsmType::I8)),
            is_argument: true,
        },
        AsmLocal {
            id: 4,
            name: Some("fini".to_string()),
            ty: AsmType::Ptr(Box::new(AsmType::I8)),
            is_argument: true,
        },
        AsmLocal {
            id: 5,
            name: Some("rtld_fini".to_string()),
            ty: AsmType::Ptr(Box::new(AsmType::I8)),
            is_argument: true,
        },
        AsmLocal {
            id: 6,
            name: Some("stack_end".to_string()),
            ty: AsmType::Ptr(Box::new(AsmType::I8)),
            is_argument: true,
        },
    ];
    let argc64 = alloc_result(&mut f, AsmType::I64);
    let argv_end_index = alloc_result(&mut f, AsmType::I64);
    let envp = alloc_result(
        &mut f,
        AsmType::Ptr(Box::new(AsmType::Ptr(Box::new(AsmType::I8)))),
    );
    let main_result = alloc_result(&mut f, AsmType::I32);
    f.basic_blocks = vec![AsmBlock {
        id: 0,
        label: None,
        instructions: vec![
            build_unary(0, AsmGenericOpcode::SExt, argc64, AsmOperand::Local(1)),
            build_binop(
                1,
                AsmGenericOpcode::Add,
                argv_end_index,
                vreg_read(argc64),
                AsmOperand::Constant(AsmConstant::UInt(1, AsmType::I64)),
            ),
            build_gep(2, envp, AsmOperand::Local(2), vec![vreg_read(argv_end_index)]),
            build_call(
                3,
                Some(main_result),
                AsmOperand::Local(0),
                vec![AsmOperand::Local(1), AsmOperand::Local(2), vreg_read(envp)],
                CallingConvention::C,
                false,
            ),
            build_call_symbol(
                4,
                None,
                "exit",
                vec![vreg_read(main_result)],
                CallingConvention::C,
            ),
        ],
        terminator: AsmTerminator::Unreachable,
        terminator_encoding: None,
        predecessors: Vec::new(),
        successors: Vec::new(),
    }];
    f.linkage = Linkage::External;
    f.visibility = Visibility::Default;
    f.calling_convention = Some(CallingConvention::C);
    f.section = Some(".text".to_string());
    f.is_declaration = false;
    ensure_function(program, f);
    Ok(())
}

/// Extracts a `Syscall` instruction's `(convention, number, args)` per the
/// canonical `[dest?, Attr(SyscallConvention(cc)), number, arg...]` schema.
fn syscall_parts(inst: &AsmInstruction) -> Option<(AsmSyscallConvention, &AsmOperand, &[AsmOperand])> {
    if !matches!(inst.opcode, AsmOpcode::Generic(AsmGenericOpcode::Syscall)) {
        return None;
    }
    let cc_idx = inst
        .operands
        .iter()
        .position(|op| matches!(op, AsmOperand::Attr(AsmAttr::SyscallConvention(_))))?;
    let AsmOperand::Attr(AsmAttr::SyscallConvention(convention)) = &inst.operands[cc_idx] else {
        unreachable!()
    };
    let number = inst.operands.get(cc_idx + 1)?;
    let args = &inst.operands[cc_idx + 2..];
    Some((*convention, number, args))
}

/// The register this instruction writes, if any, as a bare vreg id (for
/// reuse as the `dest` of a replacement instruction built in place).
fn result_vreg(inst: &AsmInstruction) -> Option<AsmVirtualRegId> {
    inst.result_register().and_then(|reg| match reg {
        AsmRegister::Virtual(id) => Some(*id),
        AsmRegister::Physical(_) => None,
    })
}

/// Extracts a `SysOp` instruction's op payload per the canonical
/// `[dest?, SysOp(op)]` schema.
fn sysop_of(inst: &AsmInstruction) -> Option<&AsmSysOp> {
    if !matches!(inst.opcode, AsmOpcode::Generic(AsmGenericOpcode::SysOp)) {
        return None;
    }
    inst.operands.iter().find_map(|op| match op {
        AsmOperand::SysOp(inner) => Some(inner.as_ref()),
        _ => None,
    })
}

/// Whether `operand` is exactly a (`Read`-or-otherwise) reference to the
/// register `inst` defines. Registers are no longer numbered the same as
/// instruction ids, so callers must compare against `inst.result_register()`
/// rather than reconstructing `AsmOperand::Register` from `inst.id`.
fn operand_is_result_of(operand: &AsmOperand, inst: &AsmInstruction) -> bool {
    matches!(
        (operand, inst.result_register()),
        (AsmOperand::Register { reg, .. }, Some(result_reg)) if reg == result_reg
    )
}

pub fn rewrite_program_to_sys_ops(program: &mut AsmProgram) -> Result<()> {
    let syscall_convention = target_syscall_convention(program);
    let target_object_format = program.target.object_format.clone();
    let source_format = program
        .container
        .as_ref()
        .map(|container| container.format.clone())
        .unwrap_or(target_object_format);
    let posix_dirent_style = match source_format {
        AsmObjectFormat::MachO => PosixDirentStyle::Darwin,
        _ => PosixDirentStyle::Linux,
    };
    for func in &mut program.functions {
        if func.is_declaration {
            continue;
        }
        for block in &mut func.basic_blocks {
            let snapshot = block.instructions.clone();
            for inst in &mut block.instructions {
                let dest = result_vreg(inst);

                if let Some((convention, number, args)) = syscall_parts(inst) {
                    if let Some(op) =
                        detect_system_api_from_syscall(&convention, number, args, &snapshot)
                    {
                        *inst = build_sysop(inst.id, dest, op);
                    }
                    continue;
                }

                if let Some(op) = detect_system_api_from_posix_call(inst, posix_dirent_style) {
                    *inst = build_sysop(inst.id, dest, op);
                    continue;
                }

                if let Some(convention) = syscall_convention {
                    if let Some(op) = detect_system_api_from_windows_import(inst, convention) {
                        *inst = build_sysop(inst.id, dest, op);
                    }
                }
            }
        }
    }
    Ok(())
}

fn rewrite_glibc_chk_calls_to_libc(program: &mut AsmProgram) {
    fn chk_call_rewrite(name: &str) -> Option<(&'static str, &'static [usize])> {
        Some(match name {
            "__fprintf_chk" => ("fprintf", &[1]),
            "__printf_chk" => ("printf", &[0]),
            "__sprintf_chk" => ("sprintf", &[1, 2]),
            "__snprintf_chk" => ("snprintf", &[2, 3]),
            "__vfprintf_chk" => ("vfprintf", &[1]),
            "__vsprintf_chk" => ("vsprintf", &[1, 2]),
            "__vsnprintf_chk" => ("vsnprintf", &[2, 3]),
            "__memcpy_chk" => ("memcpy", &[3]),
            "__mempcpy_chk" => ("mempcpy", &[3]),
            "__memmove_chk" => ("memmove", &[3]),
            "__memset_chk" => ("memset", &[3]),
            "__strcpy_chk" => ("strcpy", &[2]),
            "__stpcpy_chk" => ("stpcpy", &[2]),
            "__strncpy_chk" => ("strncpy", &[3]),
            "__strcat_chk" => ("strcat", &[2]),
            "__strncat_chk" => ("strncat", &[3]),
            "__readlink_chk" => ("readlink", &[3]),

            // glibc symbol aliases that exist on Linux but not Darwin.
            "__isoc23_strtoumax" => ("strtoumax", &[]),
            "__isoc23_strtoul" => ("strtoul", &[]),
            "__isoc23_strtol" => ("strtol", &[]),
            "__isoc23_strtoll" => ("strtoll", &[]),
            "__isoc23_strtoull" => ("strtoull", &[]),
            "__dcgettext" => ("dcgettext", &[]),
            "__dgettext" => ("dgettext", &[]),
            "__gettext" => ("gettext", &[]),
            _ => return None,
        })
    }

    fn chk_symbol_rewrite(name: &str) -> Option<&'static str> {
        Some(match name {
            "__fprintf_chk" => "fprintf",
            "__printf_chk" => "printf",
            "__sprintf_chk" => "sprintf",
            "__snprintf_chk" => "snprintf",
            "__vfprintf_chk" => "vfprintf",
            "__vsprintf_chk" => "vsprintf",
            "__vsnprintf_chk" => "vsnprintf",
            "__memcpy_chk" => "memcpy",
            "__mempcpy_chk" => "mempcpy",
            "__memmove_chk" => "memmove",
            "__memset_chk" => "memset",
            "__strcpy_chk" => "strcpy",
            "__stpcpy_chk" => "stpcpy",
            "__strncpy_chk" => "strncpy",
            "__strcat_chk" => "strcat",
            "__strncat_chk" => "strncat",
            "__readlink_chk" => "readlink",

            "__isoc23_strtoumax" => "strtoumax",
            "__isoc23_strtoul" => "strtoul",
            "__isoc23_strtol" => "strtol",
            "__isoc23_strtoll" => "strtoll",
            "__isoc23_strtoull" => "strtoull",
            "__dcgettext" => "dcgettext",
            "__dgettext" => "dgettext",
            "__gettext" => "gettext",
            _ => return None,
        })
    }

    // `Call` operand schema: `[dest?, Attr(CallingConv), Attr(TailCall)?,
    // target, arg...]` — the target is the first non-`Attr`, non-dest-`Write`
    // operand (mirrors `AsmInstruction::call_target_and_args`).
    fn call_target_index(operands: &[AsmOperand]) -> Option<usize> {
        operands.iter().position(|op| {
            !matches!(op, AsmOperand::Attr(_))
                && !matches!(
                    op,
                    AsmOperand::Register {
                        access: OperandAccess::Write,
                        ..
                    }
                )
        })
    }

    fn rewrite_variadic_call(
        operands: &mut Vec<AsmOperand>,
        target_idx: usize,
        new_name: &str,
        drop_indices: &[usize],
    ) {
        operands[target_idx] = AsmOperand::Symbol(Name::new(new_name));
        if drop_indices.is_empty() {
            return;
        }

        let arg_start = target_idx + 1;
        let kept: Vec<AsmOperand> = operands
            .drain(arg_start..)
            .enumerate()
            .filter(|(index, _)| !drop_indices.contains(index))
            .map(|(_, arg)| arg)
            .collect();
        operands.extend(kept);
    }

    for func in &mut program.functions {
        if func.is_declaration {
            continue;
        }
        for block in &mut func.basic_blocks {
            for inst in &mut block.instructions {
                if !matches!(inst.opcode, AsmOpcode::Generic(AsmGenericOpcode::Call)) {
                    continue;
                }
                let Some(target_idx) = call_target_index(&inst.operands) else {
                    continue;
                };
                let AsmOperand::Symbol(name) = &inst.operands[target_idx] else {
                    continue;
                };
                let name = name.to_string();

                let candidates = [
                    name.as_str(),
                    name.strip_prefix('_').unwrap_or(name.as_str()),
                ];
                for candidate in candidates {
                    if let Some((new_name, drop_indices)) = chk_call_rewrite(candidate) {
                        rewrite_variadic_call(&mut inst.operands, target_idx, new_name, drop_indices);
                        break;
                    }
                }
            }
        }
    }

    for global in &mut program.globals {
        for reloc in &mut global.relocations {
            let symbol = reloc.symbol.as_str().to_string();
            let candidates = [
                symbol.as_str(),
                symbol.strip_prefix('_').unwrap_or(symbol.as_str()),
            ];
            for candidate in candidates {
                if let Some(new_name) = chk_symbol_rewrite(candidate) {
                    reloc.symbol = Name::new(new_name);
                    break;
                }
            }
        }
    }
}

fn ensure_glibc_fpending(program: &mut AsmProgram) -> Result<()> {
    let mut f = AsmFunction::new(
        Name::new("__fpending"),
        AsmFunctionSignature {
            params: vec![AsmType::Ptr(Box::new(AsmType::I8))],
            return_type: AsmType::I64,
            is_variadic: false,
        },
    );
    f.basic_blocks = vec![AsmBlock {
        id: 0,
        label: None,
        instructions: Vec::new(),
        terminator: AsmTerminator::Return(Some(AsmOperand::Constant(AsmConstant::UInt(
            0,
            AsmType::I64,
        )))),
        terminator_encoding: None,
        predecessors: Vec::new(),
        successors: Vec::new(),
    }];
    f.linkage = Linkage::External;
    f.visibility = Visibility::Default;
    f.calling_convention = Some(CallingConvention::C);
    f.section = Some(".text".to_string());
    f.is_declaration = false;
    ensure_function(program, f);
    Ok(())
}

fn ensure_glibc_errno_location(program: &mut AsmProgram) -> Result<()> {
    let mut f = AsmFunction::new(
        Name::new("__errno_location"),
        AsmFunctionSignature {
            params: Vec::new(),
            return_type: AsmType::Ptr(Box::new(AsmType::I32)),
            is_variadic: false,
        },
    );
    let result = alloc_result(&mut f, AsmType::Ptr(Box::new(AsmType::I32)));
    f.basic_blocks = vec![AsmBlock {
        id: 0,
        label: None,
        instructions: vec![build_call_symbol(
            0,
            Some(result),
            "__error",
            Vec::new(),
            CallingConvention::C,
        )],
        terminator: AsmTerminator::Return(Some(vreg_read(result))),
        terminator_encoding: None,
        predecessors: Vec::new(),
        successors: Vec::new(),
    }];
    f.linkage = Linkage::External;
    f.visibility = Visibility::Default;
    f.calling_convention = Some(CallingConvention::C);
    f.section = Some(".text".to_string());
    f.is_declaration = false;
    ensure_function(program, f);
    Ok(())
}

fn ensure_glibc_assert_fail(program: &mut AsmProgram) -> Result<()> {
    let mut f = AsmFunction::new(
        Name::new("__assert_fail"),
        AsmFunctionSignature {
            params: vec![
                AsmType::Ptr(Box::new(AsmType::I8)),
                AsmType::Ptr(Box::new(AsmType::I8)),
                AsmType::I32,
                AsmType::Ptr(Box::new(AsmType::I8)),
            ],
            return_type: AsmType::Void,
            is_variadic: false,
        },
    );
    f.basic_blocks = vec![AsmBlock {
        id: 0,
        label: None,
        instructions: vec![build_call_symbol(
            0,
            None,
            "abort",
            Vec::new(),
            CallingConvention::C,
        )],
        terminator: AsmTerminator::Unreachable,
        terminator_encoding: None,
        predecessors: Vec::new(),
        successors: Vec::new(),
    }];
    f.linkage = Linkage::External;
    f.visibility = Visibility::Default;
    f.calling_convention = Some(CallingConvention::C);
    f.section = Some(".text".to_string());
    f.is_declaration = false;
    ensure_function(program, f);
    Ok(())
}

fn match_freelibrary_sequence_to_unix_call(
    instructions: &[AsmInstruction],
    convention: AsmSyscallConvention,
) -> Result<Option<(AsmInstruction, usize)>> {
    // Pattern:
    //   FreeLibrary; Eq; Select
    if instructions.len() < 3 {
        return Ok(None);
    }
    let call = &instructions[0];
    let eq = &instructions[1];
    let select = &instructions[2];

    if !is_call_named(call, "kernel32.dll", "FreeLibrary") {
        return Ok(None);
    }
    if !matches!(eq.opcode, AsmOpcode::Generic(AsmGenericOpcode::Eq)) {
        return Ok(None);
    }
    if !matches!(select.opcode, AsmOpcode::Generic(AsmGenericOpcode::Select)) {
        return Ok(None);
    }
    // Select operand schema: [dest, condition, if_true, if_false].
    if select.operands.get(2) != Some(&AsmOperand::Constant(AsmConstant::Int(-1, AsmType::I64))) {
        return Ok(None);
    }
    if select.operands.get(3) != Some(&AsmOperand::Constant(AsmConstant::Int(0, AsmType::I64))) {
        return Ok(None);
    }

    let Some((_, args)) = call.call_target_and_args() else {
        return Ok(None);
    };
    if args.len() != 1 {
        return Ok(None);
    }

    let op = SystemApiOp::Dlclose {
        handle: args[0].clone(),
    };
    let dest = result_vreg(select);
    let replacement = lower_system_api_to_unix(select.id, dest, op, convention);
    Ok(Some((replacement, 3)))
}

fn normalize_proc_name(symbol: &str) -> String {
    let base = symbol.split('!').last().unwrap_or(symbol).trim();
    base.trim_start_matches('_').to_ascii_lowercase()
}

fn detect_system_api_from_posix_call(
    inst: &AsmInstruction,
    dirent_style: PosixDirentStyle,
) -> Option<SystemApiOp> {
    let (target, args) = inst.call_target_and_args()?;
    let AsmOperand::Symbol(symbol) = target else {
        return None;
    };
    let name = normalize_proc_name(symbol);
    match name.as_str() {
        "opendir" => Some(SystemApiOp::Opendir {
            path: args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmOperand::Constant(AsmConstant::Null(AsmType::Ptr(Box::new(AsmType::I8))))),
        }),
        "readdir" | "readdir64" => Some(SystemApiOp::Readdir {
            dir: args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmOperand::Constant(AsmConstant::Null(AsmType::Ptr(Box::new(AsmType::I8))))),
            dirent_style,
        }),
        "closedir" => Some(SystemApiOp::Closedir {
            dir: args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmOperand::Constant(AsmConstant::Null(AsmType::Ptr(Box::new(AsmType::I8))))),
        }),
        "dlopen" => Some(SystemApiOp::Dlopen {
            path: args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmOperand::Constant(AsmConstant::Null(AsmType::Ptr(Box::new(AsmType::I8))))),
            flags: args
                .get(1)
                .cloned()
                .unwrap_or_else(|| AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I32))),
        }),
        "dlsym" => Some(SystemApiOp::Dlsym {
            handle: args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I64))),
            symbol: args
                .get(1)
                .cloned()
                .unwrap_or_else(|| AsmOperand::Constant(AsmConstant::Null(AsmType::Ptr(Box::new(AsmType::I8))))),
        }),
        "dlclose" => Some(SystemApiOp::Dlclose {
            handle: args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I64))),
        }),
        "unlink" => Some(SystemApiOp::Unlink {
            path: args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmOperand::Constant(AsmConstant::Null(AsmType::Ptr(Box::new(AsmType::I8))))),
        }),
        "mkdir" => Some(SystemApiOp::Mkdir {
            path: args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmOperand::Constant(AsmConstant::Null(AsmType::Ptr(Box::new(AsmType::I8))))),
            mode: args
                .get(1)
                .cloned()
                .unwrap_or_else(|| AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I32))),
        }),
        "rmdir" => Some(SystemApiOp::Rmdir {
            path: args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmOperand::Constant(AsmConstant::Null(AsmType::Ptr(Box::new(AsmType::I8))))),
        }),
        "rename" => Some(SystemApiOp::Rename {
            from: args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmOperand::Constant(AsmConstant::Null(AsmType::Ptr(Box::new(AsmType::I8))))),
            to: args
                .get(1)
                .cloned()
                .unwrap_or_else(|| AsmOperand::Constant(AsmConstant::Null(AsmType::Ptr(Box::new(AsmType::I8))))),
        }),
        "access" => Some(SystemApiOp::Access {
            path: args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmOperand::Constant(AsmConstant::Null(AsmType::Ptr(Box::new(AsmType::I8))))),
            mode: args
                .get(1)
                .cloned()
                .unwrap_or_else(|| AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I32))),
        }),
        _ => None,
    }
}

fn windows_createfile_disposition_from_flags(style: PosixFlagStyle, flags: i64) -> i64 {
    match style {
        PosixFlagStyle::Linux => windows_createfile_disposition_linux(flags),
        PosixFlagStyle::Darwin => windows_createfile_disposition_darwin(flags),
    }
}

fn posix_mmap_flags_anonymous_private(style: PosixFlagStyle) -> i64 {
    match style {
        // MAP_PRIVATE=0x02, MAP_ANONYMOUS=0x20
        PosixFlagStyle::Linux => 0x02 | 0x20,
        // MAP_PRIVATE=0x02, MAP_ANON=0x1000
        PosixFlagStyle::Darwin => 0x02 | 0x1000,
    }
}

fn windows_page_protection_from_posix(prot: i64) -> i64 {
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

fn windows_createfile_desired_access(flags: i64) -> i64 {
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

fn windows_createfile_disposition_linux(flags: i64) -> i64 {
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

fn windows_createfile_disposition_darwin(flags: i64) -> i64 {
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

fn match_closehandle_sequence_to_syscall(
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
    let mut fd_value: Option<AsmOperand> = None;

    if is_call_named(&instructions[0], "kernel32.dll", "GetStdHandle") {
        if instructions.len() < 4 {
            return Ok(None);
        }
        let getstd = &instructions[0];
        let Some((_, getstd_args)) = getstd.call_target_and_args() else {
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
        fd_value = Some(AsmOperand::Constant(AsmConstant::UInt(fd, AsmType::I64)));
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
    if !matches!(cmp.opcode, AsmOpcode::Generic(AsmGenericOpcode::Eq)) {
        return Ok(None);
    }
    if !matches!(select.opcode, AsmOpcode::Generic(AsmGenericOpcode::Select)) {
        return Ok(None);
    }

    let Some((_, close_args)) = close.call_target_and_args() else {
        return Ok(None);
    };
    if close_args.len() != 1 {
        return Ok(None);
    }
    if base == 1 && !operand_is_result_of(&close_args[0], &instructions[0]) {
        return Ok(None);
    }

    let fd = fd_value.unwrap_or_else(|| close_args[0].clone());
    let op = SystemApiOp::Close { fd };
    let dest = result_vreg(select);
    let replacement = lower_system_api_to_syscall(select.id, dest, op, convention);

    Ok(Some((replacement, base + 3)))
}

fn fd_to_std_handle_code(fd: i64) -> Option<i64> {
    // STD_INPUT_HANDLE=-10, STD_OUTPUT_HANDLE=-11, STD_ERROR_HANDLE=-12
    Some(match fd {
        0 => -10,
        1 => -11,
        2 => -12,
        _ => return None,
    })
}

pub fn rewrite_program_for_target(program: &mut AsmProgram) -> Result<()> {
    rewrite_program_to_sys_ops(program)?;
    lower_sys_ops_for_target(program)?;
    inject_linux_compat_runtime_for_darwin(program)?;
    Ok(())
}

fn inject_linux_compat_runtime_for_darwin(program: &mut AsmProgram) -> Result<()> {
    if program.target.object_format != AsmObjectFormat::MachO {
        return Ok(());
    }
    let Some(container) = program.container.as_ref() else {
        return Ok(());
    };
    if container.format != AsmObjectFormat::Elf {
        return Ok(());
    }

    rewrite_glibc_chk_calls_to_libc(program);

    ensure_section(
        program,
        ".rodata",
        AsmSectionKind::ReadOnlyData,
        vec![AsmSectionFlag::Allocate],
    );
    ensure_section(
        program,
        ".data",
        AsmSectionKind::Data,
        vec![AsmSectionFlag::Allocate, AsmSectionFlag::Write],
    );

    ensure_ctype_tables(program);
    ensure_ctype_loc_functions(program)?;
    ensure_ctype_mb_cur_max(program)?;
    ensure_glibc_assert_fail(program)?;
    ensure_glibc_errno_location(program)?;
    ensure_glibc_fpending(program)?;
    ensure_glibc_start_main(program)?;
    ensure_glibc_mempcpy(program)?;
    ensure_glibc_overflow(program)?;
    ensure_glibc_progname_globals(program);
    ensure_glibc_gettext_stubs(program)?;
    ensure_linux_libcap_stubs(program)?;
    ensure_glibc_stdio_unlocked(program)?;
    ensure_linux_xattr_wrappers(program)?;
    ensure_glibc_mbrtoc32(program)?;
    ensure_glibc_rawmemchr(program)?;
    ensure_linux_statx_stub(program)?;

    Ok(())
}

fn ensure_glibc_rawmemchr(program: &mut AsmProgram) -> Result<()> {
    // rawmemchr(const void *s, int c) -> memchr(s, c, SIZE_MAX)
    let void_ptr = AsmType::Ptr(Box::new(AsmType::I8));

    let mut f = AsmFunction::new(
        Name::new("rawmemchr"),
        AsmFunctionSignature {
            params: vec![void_ptr.clone(), AsmType::I32],
            return_type: void_ptr.clone(),
            is_variadic: false,
        },
    );
    f.locals = vec![
        AsmLocal {
            id: 0,
            name: Some("s".to_string()),
            ty: void_ptr.clone(),
            is_argument: true,
        },
        AsmLocal {
            id: 1,
            name: Some("c".to_string()),
            ty: AsmType::I32,
            is_argument: true,
        },
    ];
    let result = alloc_result(&mut f, void_ptr);
    f.basic_blocks = vec![AsmBlock {
        id: 0,
        label: None,
        instructions: vec![build_call_symbol(
            0,
            Some(result),
            "memchr",
            vec![
                AsmOperand::Local(0),
                AsmOperand::Local(1),
                AsmOperand::Constant(AsmConstant::UInt(u64::MAX, AsmType::I64)),
            ],
            CallingConvention::C,
        )],
        terminator: AsmTerminator::Return(Some(vreg_read(result))),
        terminator_encoding: None,
        predecessors: Vec::new(),
        successors: Vec::new(),
    }];
    f.linkage = Linkage::External;
    f.visibility = Visibility::Default;
    f.calling_convention = Some(CallingConvention::C);
    f.section = Some(".text".to_string());
    f.is_declaration = false;
    ensure_function(program, f);

    Ok(())
}

fn ensure_linux_statx_stub(program: &mut AsmProgram) -> Result<()> {
    // Linux `statx` is used by newer coreutils binaries.
    //
    // For now we intentionally force a fallback path by returning -1 and setting
    // errno=ENOSYS (38 on Linux). This keeps the function ABI-correct without
    // committing to a Linux `struct statx` layout translation on Darwin yet.
    //
    // int statx(int dirfd, const char *pathname, int flags, unsigned int mask, struct statx *buf);
    let ptr_i8 = AsmType::Ptr(Box::new(AsmType::I8));
    let ptr_i32 = AsmType::Ptr(Box::new(AsmType::I32));

    let mut f = AsmFunction::new(
        Name::new("statx"),
        AsmFunctionSignature {
            params: vec![
                AsmType::I32,
                ptr_i8.clone(),
                AsmType::I32,
                AsmType::I32,
                ptr_i8,
            ],
            return_type: AsmType::I32,
            is_variadic: false,
        },
    );
    let errno_ptr = alloc_result(&mut f, ptr_i32);
    f.basic_blocks = vec![AsmBlock {
        id: 0,
        label: None,
        instructions: vec![
            build_call_symbol(0, Some(errno_ptr), "__errno_location", Vec::new(), CallingConvention::C),
            build_store(
                1,
                AsmOperand::Constant(AsmConstant::UInt(38, AsmType::I32)),
                vreg_read(errno_ptr),
            ),
        ],
        terminator: AsmTerminator::Return(Some(AsmOperand::Constant(AsmConstant::Int(
            -1,
            AsmType::I32,
        )))),
        terminator_encoding: None,
        predecessors: Vec::new(),
        successors: Vec::new(),
    }];
    f.linkage = Linkage::External;
    f.visibility = Visibility::Default;
    f.calling_convention = Some(CallingConvention::C);
    f.section = Some(".text".to_string());
    f.is_declaration = false;
    ensure_function(program, f);

    Ok(())
}

fn ensure_glibc_mbrtoc32(program: &mut AsmProgram) -> Result<()> {
    // A pragmatic ASCII-only implementation.
    // size_t mbrtoc32(char32_t *pc32, const char *s, size_t n, mbstate_t *ps)

    let ptr_i8 = AsmType::Ptr(Box::new(AsmType::I8));
    let ptr_i32 = AsmType::Ptr(Box::new(AsmType::I32));

    let mut f = AsmFunction::new(
        Name::new("mbrtoc32"),
        AsmFunctionSignature {
            params: vec![
                ptr_i32.clone(),
                ptr_i8.clone(),
                AsmType::I64,
                ptr_i8.clone(),
            ],
            return_type: AsmType::I64,
            is_variadic: false,
        },
    );
    f.locals = vec![
        AsmLocal {
            id: 0,
            name: Some("pc32".to_string()),
            ty: ptr_i32,
            is_argument: true,
        },
        AsmLocal {
            id: 1,
            name: Some("s".to_string()),
            ty: ptr_i8.clone(),
            is_argument: true,
        },
        AsmLocal {
            id: 2,
            name: Some("n".to_string()),
            ty: AsmType::I64,
            is_argument: true,
        },
        AsmLocal {
            id: 3,
            name: Some("ps".to_string()),
            ty: ptr_i8,
            is_argument: true,
        },
    ];
    let byte_reg = alloc_result(&mut f, AsmType::I8);
    let widened_reg = alloc_result(&mut f, AsmType::I32);
    let is_nul_reg = alloc_result(&mut f, AsmType::I1);
    let result_reg = alloc_result(&mut f, AsmType::I64);
    f.basic_blocks = vec![AsmBlock {
        id: 0,
        label: None,
        instructions: vec![
            build_load(0, byte_reg, AsmOperand::Local(1)),
            build_unary(1, AsmGenericOpcode::ZExt, widened_reg, vreg_read(byte_reg)),
            build_store(2, vreg_read(widened_reg), AsmOperand::Local(0)),
            build_eq(
                3,
                is_nul_reg,
                vreg_read(byte_reg),
                AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I8)),
            ),
            build_select(
                4,
                result_reg,
                vreg_read(is_nul_reg),
                AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I64)),
                AsmOperand::Constant(AsmConstant::UInt(1, AsmType::I64)),
            ),
        ],
        terminator: AsmTerminator::Return(Some(vreg_read(result_reg))),
        terminator_encoding: None,
        predecessors: Vec::new(),
        successors: Vec::new(),
    }];
    f.linkage = Linkage::External;
    f.visibility = Visibility::Default;
    f.calling_convention = Some(CallingConvention::C);
    f.section = Some(".text".to_string());
    f.is_declaration = false;
    ensure_function(program, f);

    Ok(())
}

fn ensure_linux_xattr_wrappers(program: &mut AsmProgram) -> Result<()> {
    // Linux/glibc exposes `l* xattr` entrypoints that are absent on Darwin.
    // Provide wrappers over Darwin's xattr APIs.

    let ptr_i8 = AsmType::Ptr(Box::new(AsmType::I8));
    let void_ptr = AsmType::Ptr(Box::new(AsmType::I8));

    // ssize_t lgetxattr(const char *path, const char *name, void *value, size_t size)
    // -> getxattr(path, name, value, size, 0, 0)
    {
        let mut f = AsmFunction::new(
            Name::new("lgetxattr"),
            AsmFunctionSignature {
                params: vec![ptr_i8.clone(), ptr_i8.clone(), void_ptr.clone(), AsmType::I64],
                return_type: AsmType::I64,
                is_variadic: false,
            },
        );
        f.locals = vec![
            AsmLocal { id: 0, name: Some("path".to_string()), ty: ptr_i8.clone(), is_argument: true },
            AsmLocal { id: 1, name: Some("name".to_string()), ty: ptr_i8.clone(), is_argument: true },
            AsmLocal { id: 2, name: Some("value".to_string()), ty: void_ptr.clone(), is_argument: true },
            AsmLocal { id: 3, name: Some("size".to_string()), ty: AsmType::I64, is_argument: true },
        ];
        let result = alloc_result(&mut f, AsmType::I64);
        f.basic_blocks = vec![AsmBlock {
            id: 0,
            label: None,
            instructions: vec![build_call_symbol(
                0,
                Some(result),
                "getxattr",
                vec![
                    AsmOperand::Local(0),
                    AsmOperand::Local(1),
                    AsmOperand::Local(2),
                    AsmOperand::Local(3),
                    AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I32)),
                    AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I32)),
                ],
                CallingConvention::C,
            )],
            terminator: AsmTerminator::Return(Some(vreg_read(result))),
            terminator_encoding: None,
            predecessors: Vec::new(),
            successors: Vec::new(),
        }];
        f.linkage = Linkage::External;
        f.visibility = Visibility::Default;
        f.calling_convention = Some(CallingConvention::C);
        f.section = Some(".text".to_string());
        f.is_declaration = false;
        ensure_function(program, f);
    }

    // ssize_t llistxattr(const char *path, char *list, size_t size)
    // -> listxattr(path, list, size, 0)
    {
        let mut f = AsmFunction::new(
            Name::new("llistxattr"),
            AsmFunctionSignature {
                params: vec![ptr_i8.clone(), void_ptr.clone(), AsmType::I64],
                return_type: AsmType::I64,
                is_variadic: false,
            },
        );
        f.locals = vec![
            AsmLocal { id: 0, name: Some("path".to_string()), ty: ptr_i8.clone(), is_argument: true },
            AsmLocal { id: 1, name: Some("list".to_string()), ty: void_ptr.clone(), is_argument: true },
            AsmLocal { id: 2, name: Some("size".to_string()), ty: AsmType::I64, is_argument: true },
        ];
        let result = alloc_result(&mut f, AsmType::I64);
        f.basic_blocks = vec![AsmBlock {
            id: 0,
            label: None,
            instructions: vec![build_call_symbol(
                0,
                Some(result),
                "listxattr",
                vec![
                    AsmOperand::Local(0),
                    AsmOperand::Local(1),
                    AsmOperand::Local(2),
                    AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I32)),
                ],
                CallingConvention::C,
            )],
            terminator: AsmTerminator::Return(Some(vreg_read(result))),
            terminator_encoding: None,
            predecessors: Vec::new(),
            successors: Vec::new(),
        }];
        f.linkage = Linkage::External;
        f.visibility = Visibility::Default;
        f.calling_convention = Some(CallingConvention::C);
        f.section = Some(".text".to_string());
        f.is_declaration = false;
        ensure_function(program, f);
    }

    // int lsetxattr(const char *path, const char *name, const void *value, size_t size, int flags)
    // -> setxattr(path, name, value, size, 0, flags)
    {
        let mut f = AsmFunction::new(
            Name::new("lsetxattr"),
            AsmFunctionSignature {
                params: vec![
                    ptr_i8.clone(),
                    ptr_i8.clone(),
                    void_ptr.clone(),
                    AsmType::I64,
                    AsmType::I32,
                ],
                return_type: AsmType::I32,
                is_variadic: false,
            },
        );
        f.locals = vec![
            AsmLocal { id: 0, name: Some("path".to_string()), ty: ptr_i8.clone(), is_argument: true },
            AsmLocal { id: 1, name: Some("name".to_string()), ty: ptr_i8.clone(), is_argument: true },
            AsmLocal { id: 2, name: Some("value".to_string()), ty: void_ptr.clone(), is_argument: true },
            AsmLocal { id: 3, name: Some("size".to_string()), ty: AsmType::I64, is_argument: true },
            AsmLocal { id: 4, name: Some("flags".to_string()), ty: AsmType::I32, is_argument: true },
        ];
        let result = alloc_result(&mut f, AsmType::I32);
        f.basic_blocks = vec![AsmBlock {
            id: 0,
            label: None,
            instructions: vec![build_call_symbol(
                0,
                Some(result),
                "setxattr",
                vec![
                    AsmOperand::Local(0),
                    AsmOperand::Local(1),
                    AsmOperand::Local(2),
                    AsmOperand::Local(3),
                    AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I32)),
                    AsmOperand::Local(4),
                ],
                CallingConvention::C,
            )],
            terminator: AsmTerminator::Return(Some(vreg_read(result))),
            terminator_encoding: None,
            predecessors: Vec::new(),
            successors: Vec::new(),
        }];
        f.linkage = Linkage::External;
        f.visibility = Visibility::Default;
        f.calling_convention = Some(CallingConvention::C);
        f.section = Some(".text".to_string());
        f.is_declaration = false;
        ensure_function(program, f);
    }

    // int lremovexattr(const char *path, const char *name)
    // -> removexattr(path, name, 0)
    {
        let mut f = AsmFunction::new(
            Name::new("lremovexattr"),
            AsmFunctionSignature {
                params: vec![ptr_i8.clone(), ptr_i8.clone()],
                return_type: AsmType::I32,
                is_variadic: false,
            },
        );
        f.locals = vec![
            AsmLocal { id: 0, name: Some("path".to_string()), ty: ptr_i8.clone(), is_argument: true },
            AsmLocal { id: 1, name: Some("name".to_string()), ty: ptr_i8.clone(), is_argument: true },
        ];
        let result = alloc_result(&mut f, AsmType::I32);
        f.basic_blocks = vec![AsmBlock {
            id: 0,
            label: None,
            instructions: vec![build_call_symbol(
                0,
                Some(result),
                "removexattr",
                vec![
                    AsmOperand::Local(0),
                    AsmOperand::Local(1),
                    AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I32)),
                ],
                CallingConvention::C,
            )],
            terminator: AsmTerminator::Return(Some(vreg_read(result))),
            terminator_encoding: None,
            predecessors: Vec::new(),
            successors: Vec::new(),
        }];
        f.linkage = Linkage::External;
        f.visibility = Visibility::Default;
        f.calling_convention = Some(CallingConvention::C);
        f.section = Some(".text".to_string());
        f.is_declaration = false;
        ensure_function(program, f);
    }

    Ok(())
}

fn ensure_glibc_stdio_unlocked(program: &mut AsmProgram) -> Result<()> {
    // glibc provides *_unlocked stdio functions; Darwin libc typically doesn't.
    // Implement them as thin wrappers over their locked counterparts.

    let file_ptr = AsmType::Ptr(Box::new(AsmType::I8));
    let void_ptr = AsmType::Ptr(Box::new(AsmType::I8));

    // int fflush_unlocked(FILE *stream)
    ensure_forwarding_function(
        program,
        "fflush_unlocked",
        vec![("stream", file_ptr.clone())],
        AsmType::I32,
        "fflush",
    );

    // size_t fwrite_unlocked(const void *ptr, size_t size, size_t nmemb, FILE *stream)
    ensure_forwarding_function(
        program,
        "fwrite_unlocked",
        vec![
            ("ptr", void_ptr.clone()),
            ("size", AsmType::I64),
            ("nmemb", AsmType::I64),
            ("stream", file_ptr.clone()),
        ],
        AsmType::I64,
        "fwrite",
    );

    // size_t fread_unlocked(void *ptr, size_t size, size_t nmemb, FILE *stream)
    ensure_forwarding_function(
        program,
        "fread_unlocked",
        vec![
            ("ptr", void_ptr.clone()),
            ("size", AsmType::I64),
            ("nmemb", AsmType::I64),
            ("stream", file_ptr.clone()),
        ],
        AsmType::I64,
        "fread",
    );

    // int fputc_unlocked(int c, FILE *stream)
    ensure_forwarding_function(
        program,
        "fputc_unlocked",
        vec![("c", AsmType::I32), ("stream", file_ptr.clone())],
        AsmType::I32,
        "fputc",
    );

    // int fputs_unlocked(const char *s, FILE *stream)
    ensure_forwarding_function(
        program,
        "fputs_unlocked",
        vec![("s", void_ptr.clone()), ("stream", file_ptr.clone())],
        AsmType::I32,
        "fputs",
    );

    // int getc_unlocked(FILE *stream)
    ensure_forwarding_function(
        program,
        "getc_unlocked",
        vec![("stream", file_ptr.clone())],
        AsmType::I32,
        "getc",
    );

    // int putc_unlocked(int c, FILE *stream)
    ensure_forwarding_function(
        program,
        "putc_unlocked",
        vec![("c", AsmType::I32), ("stream", file_ptr)],
        AsmType::I32,
        "putc",
    );

    Ok(())
}

fn ensure_linux_libcap_stubs(program: &mut AsmProgram) -> Result<()> {
    // coreutils may be built with libcap support. Darwin doesn't ship libcap.
    // Provide no-op stubs so capability-aware paths degrade gracefully.

    let ptr_i8 = AsmType::Ptr(Box::new(AsmType::I8));

    ensure_global(
        program,
        AsmGlobal {
            name: Name::new("fp_linux_empty_cstring"),
            ty: AsmType::Array(Box::new(AsmType::I8), 1),
            initializer: Some(AsmConstant::Bytes(vec![0])),
            relocations: Vec::new(),
            section: Some(".rodata".to_string()),
            linkage: Linkage::Private,
            visibility: Visibility::Default,
            alignment: Some(1),
            is_constant: true,
        },
    );

    // int cap_free(void *ptr)
    ensure_constant_stub_function(
        program,
        "cap_free",
        vec![("ptr", ptr_i8.clone())],
        AsmType::I32,
        AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I32)),
    );

    // void *cap_get_file(const char *path)
    ensure_constant_stub_function(
        program,
        "cap_get_file",
        vec![("path", ptr_i8.clone())],
        ptr_i8.clone(),
        AsmOperand::Constant(AsmConstant::Null(ptr_i8.clone())),
    );

    // int cap_set_file(const char *path, void *cap)
    ensure_constant_stub_function(
        program,
        "cap_set_file",
        vec![("path", ptr_i8.clone()), ("cap", ptr_i8.clone())],
        AsmType::I32,
        AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I32)),
    );

    // char *cap_to_text(void *cap, ssize_t *len)
    ensure_constant_stub_function(
        program,
        "cap_to_text",
        vec![("cap", ptr_i8.clone()), ("len", AsmType::Ptr(Box::new(AsmType::I64)))],
        ptr_i8,
        AsmOperand::Symbol(Name::new("fp_linux_empty_cstring")),
    );

    Ok(())
}

fn ensure_glibc_gettext_stubs(program: &mut AsmProgram) -> Result<()> {
    let ptr_i8 = AsmType::Ptr(Box::new(AsmType::I8));

    // const char *bindtextdomain(const char *domain, const char *dir)
    ensure_constant_stub_function(
        program,
        "bindtextdomain",
        vec![("domain", ptr_i8.clone()), ("dir", ptr_i8.clone())],
        ptr_i8.clone(),
        AsmOperand::Local(1),
    );

    // const char *textdomain(const char *domain)
    ensure_constant_stub_function(
        program,
        "textdomain",
        vec![("domain", ptr_i8.clone())],
        ptr_i8.clone(),
        AsmOperand::Local(0),
    );

    // const char *dcgettext(const char *domain, const char *msgid, int category)
    ensure_constant_stub_function(
        program,
        "dcgettext",
        vec![
            ("domain", ptr_i8.clone()),
            ("msgid", ptr_i8.clone()),
            ("category", AsmType::I32),
        ],
        ptr_i8.clone(),
        AsmOperand::Local(1),
    );

    // const char *dgettext(const char *domain, const char *msgid)
    ensure_constant_stub_function(
        program,
        "dgettext",
        vec![("domain", ptr_i8.clone()), ("msgid", ptr_i8.clone())],
        ptr_i8.clone(),
        AsmOperand::Local(1),
    );

    // const char *gettext(const char *msgid)
    ensure_constant_stub_function(
        program,
        "gettext",
        vec![("msgid", ptr_i8.clone())],
        ptr_i8,
        AsmOperand::Local(0),
    );

    Ok(())
}

fn ensure_section(
    program: &mut AsmProgram,
    name: &str,
    kind: AsmSectionKind,
    flags: Vec<AsmSectionFlag>,
) {
    if program.sections.iter().any(|section| section.name == name) {
        return;
    }
    program.sections.push(AsmSection {
        name: name.to_string(),
        kind,
        flags,
        alignment: Some(16),
    });
}

fn ensure_global(program: &mut AsmProgram, global: AsmGlobal) {
    if let Some(existing) = program
        .globals
        .iter_mut()
        .find(|item| item.name.as_str() == global.name.as_str())
    {
        *existing = global;
        return;
    }
    program.globals.push(global);
}

fn ensure_function(program: &mut AsmProgram, function: AsmFunction) {
    if let Some(existing) = program
        .functions
        .iter_mut()
        .find(|item| item.name.as_str() == function.name.as_str())
    {
        if existing.is_declaration {
            *existing = function;
        }
        return;
    }
    program.functions.push(function);
}

fn build_ascii_tolower_table_bytes() -> Vec<u8> {
    let mut out = Vec::with_capacity(256 * 4);
    for byte in 0u8..=255 {
        let lowered = if (b'A'..=b'Z').contains(&byte) {
            byte + 32
        } else {
            byte
        };
        out.extend_from_slice(&(lowered as i32).to_le_bytes());
    }
    out
}

fn build_ascii_toupper_table_bytes() -> Vec<u8> {
    let mut out = Vec::with_capacity(256 * 4);
    for byte in 0u8..=255 {
        let upper = if (b'a'..=b'z').contains(&byte) {
            byte - 32
        } else {
            byte
        };
        out.extend_from_slice(&(upper as i32).to_le_bytes());
    }
    out
}

fn ensure_ctype_tables(program: &mut AsmProgram) {
    ensure_global(
        program,
        AsmGlobal {
            name: Name::new("fp_linux_ctype_tolower_table"),
            ty: AsmType::Array(Box::new(AsmType::I8), 256 * 4),
            initializer: Some(AsmConstant::Bytes(build_ascii_tolower_table_bytes())),
            relocations: Vec::new(),
            section: Some(".rodata".to_string()),
            linkage: Linkage::Private,
            visibility: Visibility::Default,
            alignment: Some(16),
            is_constant: true,
        },
    );
    ensure_global(
        program,
        AsmGlobal {
            name: Name::new("fp_linux_ctype_tolower_ptr"),
            ty: AsmType::I64,
            initializer: Some(AsmConstant::Bytes(vec![0; 8])),
            relocations: vec![AsmGlobalRelocation {
                offset: 0,
                kind: AsmRelocationKind::Abs64,
                symbol: Name::new("fp_linux_ctype_tolower_table"),
                addend: 0,
            }],
            section: Some(".data".to_string()),
            linkage: Linkage::Private,
            visibility: Visibility::Default,
            alignment: Some(8),
            is_constant: false,
        },
    );

    ensure_global(
        program,
        AsmGlobal {
            name: Name::new("fp_linux_ctype_toupper_table"),
            ty: AsmType::Array(Box::new(AsmType::I8), 256 * 4),
            initializer: Some(AsmConstant::Bytes(build_ascii_toupper_table_bytes())),
            relocations: Vec::new(),
            section: Some(".rodata".to_string()),
            linkage: Linkage::Private,
            visibility: Visibility::Default,
            alignment: Some(16),
            is_constant: true,
        },
    );
    ensure_global(
        program,
        AsmGlobal {
            name: Name::new("fp_linux_ctype_toupper_ptr"),
            ty: AsmType::I64,
            initializer: Some(AsmConstant::Bytes(vec![0; 8])),
            relocations: vec![AsmGlobalRelocation {
                offset: 0,
                kind: AsmRelocationKind::Abs64,
                symbol: Name::new("fp_linux_ctype_toupper_table"),
                addend: 0,
            }],
            section: Some(".data".to_string()),
            linkage: Linkage::Private,
            visibility: Visibility::Default,
            alignment: Some(8),
            is_constant: false,
        },
    );

    ensure_global(
        program,
        AsmGlobal {
            name: Name::new("fp_linux_ctype_b_table"),
            ty: AsmType::Array(Box::new(AsmType::I8), 256 * 2),
            initializer: Some(AsmConstant::Bytes(vec![0xffu8; 256 * 2])),
            relocations: Vec::new(),
            section: Some(".rodata".to_string()),
            linkage: Linkage::Private,
            visibility: Visibility::Default,
            alignment: Some(16),
            is_constant: true,
        },
    );
    ensure_global(
        program,
        AsmGlobal {
            name: Name::new("fp_linux_ctype_b_ptr"),
            ty: AsmType::I64,
            initializer: Some(AsmConstant::Bytes(vec![0; 8])),
            relocations: vec![AsmGlobalRelocation {
                offset: 0,
                kind: AsmRelocationKind::Abs64,
                symbol: Name::new("fp_linux_ctype_b_table"),
                addend: 0,
            }],
            section: Some(".data".to_string()),
            linkage: Linkage::Private,
            visibility: Visibility::Default,
            alignment: Some(8),
            is_constant: false,
        },
    );
}

fn ensure_ctype_loc_functions(program: &mut AsmProgram) -> Result<()> {
    let ptr_i8 = AsmType::Ptr(Box::new(AsmType::I8));
    let ptr_return = AsmType::Ptr(Box::new(ptr_i8.clone()));

    ensure_constant_stub_function(
        program,
        "__ctype_tolower_loc",
        Vec::new(),
        ptr_return.clone(),
        AsmOperand::Constant(AsmConstant::GlobalRef(
            Name::new("fp_linux_ctype_tolower_ptr"),
            ptr_i8.clone(),
            vec![0],
        )),
    );

    ensure_constant_stub_function(
        program,
        "__ctype_toupper_loc",
        Vec::new(),
        ptr_return.clone(),
        AsmOperand::Constant(AsmConstant::GlobalRef(
            Name::new("fp_linux_ctype_toupper_ptr"),
            ptr_i8.clone(),
            vec![0],
        )),
    );

    ensure_constant_stub_function(
        program,
        "__ctype_b_loc",
        Vec::new(),
        ptr_return,
        AsmOperand::Constant(AsmConstant::GlobalRef(
            Name::new("fp_linux_ctype_b_ptr"),
            ptr_i8,
            vec![0],
        )),
    );

    Ok(())
}

fn ensure_ctype_mb_cur_max(program: &mut AsmProgram) -> Result<()> {
    ensure_constant_stub_function(
        program,
        "__ctype_get_mb_cur_max",
        Vec::new(),
        AsmType::I64,
        AsmOperand::Constant(AsmConstant::UInt(1, AsmType::I64)),
    );

    Ok(())
}

pub fn lower_sys_ops_for_target(program: &mut AsmProgram) -> Result<()> {
    if program.target.object_format == AsmObjectFormat::Coff
        || program.target.object_format == AsmObjectFormat::Pe
    {
        lower_sys_ops_to_windows_imports(program)
    } else {
        lower_sys_ops_to_unix_syscalls(program)
    }
}

fn lower_sys_ops_to_unix_syscalls(program: &mut AsmProgram) -> Result<()> {
    let Some(target_convention) = target_syscall_convention(program) else {
        return Ok(());
    };

    let default_cc = program
        .target
        .default_calling_convention
        .clone()
        .unwrap_or(CallingConvention::C);
    let target_dirent_style = match program.target.object_format {
        AsmObjectFormat::MachO => PosixDirentStyle::Darwin,
        _ => PosixDirentStyle::Linux,
    };

    if target_dirent_style == PosixDirentStyle::Darwin
        && program
            .functions
            .iter()
            .filter(|f| !f.is_declaration)
            .flat_map(|f| f.basic_blocks.iter())
            .flat_map(|b| b.instructions.iter())
            .any(|inst| {
                matches!(
                    sysop_of(inst),
                    Some(AsmSysOp::Readdir {
                        dirent_style: PosixDirentStyle::Linux,
                        ..
                    })
                )
            })
    {
        inject_linux_readdir_shim(program, default_cc.clone())?;
    }

    for function in &mut program.functions {
        if function.is_declaration {
            continue;
        }

        for block in &mut function.basic_blocks {
            for inst in &mut block.instructions {
                let Some(op) = sysop_of(inst).cloned() else {
                    continue;
                };
                let dest = result_vreg(inst);

                match op {
                    AsmSysOp::Opendir { path } => {
                        *inst = build_call_symbol(inst.id, dest, "opendir", vec![path], default_cc.clone());
                    }
                    AsmSysOp::Readdir { dir, dirent_style } => {
                        let name = if dirent_style != target_dirent_style {
                            "fp_linux_readdir"
                        } else {
                            "readdir"
                        };
                        *inst = build_call_symbol(inst.id, dest, name, vec![dir], default_cc.clone());
                    }
                    AsmSysOp::Closedir { dir } => {
                        *inst = build_call_symbol(inst.id, dest, "closedir", vec![dir], default_cc.clone());
                    }
                    other => {
                        *inst = lower_system_api_to_syscall(inst.id, dest, other, target_convention);
                    }
                }
            }
        }
    }
    Ok(())
}

fn inject_linux_readdir_shim(program: &mut AsmProgram, cc: CallingConvention) -> Result<()> {
    if program
        .functions
        .iter()
        .any(|f| f.name.as_str() == "fp_linux_readdir")
    {
        return Ok(());
    }

    #[cfg(not(unix))]
    {
        let _ = (program, cc);
        return Err(Error::from("fp_linux_readdir shim requires a unix host"));
    }

    #[cfg(unix)]
    {
        use fp_core::asmir::{
            AsmBlock, AsmFunction, AsmFunctionSignature, AsmLocal, AsmTerminator,
        };
        use fp_core::lir::{Linkage, Name, Visibility};

        let ptr_i8 = AsmType::Ptr(Box::new(AsmType::I8));
        let null_ptr = AsmOperand::Constant(AsmConstant::Null(ptr_i8.clone()));

        const LINUX_DIRENT_SIZE: u64 = 280;
        const LINUX_D_NAME_OFFSET: u64 = 19;
        const LINUX_D_INO_OFFSET: u64 = 0;
        const LINUX_D_RECLEN_OFFSET: u64 = 16;
        const LINUX_D_TYPE_OFFSET: u64 = 18;
        const LINUX_D_NAME_MAX: u64 = 255;

        let host_d_name_offset: u64 = core::mem::offset_of!(libc::dirent, d_name) as u64;
        let host_d_ino_offset: u64 = core::mem::offset_of!(libc::dirent, d_ino) as u64;
        let host_d_type_offset: u64 = core::mem::offset_of!(libc::dirent, d_type) as u64;

        let mut next_id: u32 = program
            .functions
            .iter()
            .flat_map(|f| f.basic_blocks.iter())
            .flat_map(|b| b.instructions.iter().map(|i| i.id))
            .max()
            .unwrap_or(0)
            .saturating_add(1);
        let mut fresh_id = move || {
            let id = next_id;
            next_id += 1;
            id
        };

        let mut f = AsmFunction::new(
            Name::new("fp_linux_readdir"),
            AsmFunctionSignature {
                params: vec![ptr_i8.clone()],
                return_type: ptr_i8.clone(),
                is_variadic: false,
            },
        );

        let dir_local = AsmLocal {
            id: 0,
            ty: ptr_i8.clone(),
            name: Some("dir".to_string()),
            is_argument: true,
        };
        f.locals = vec![dir_local.clone()];

        // entry:
        //   entry = readdir(dir)
        //   if entry == null { return null }
        //   out = malloc(LINUX_DIRENT_SIZE)
        //   memset(out, 0, LINUX_DIRENT_SIZE)
        //   out->d_ino = entry->d_ino
        //   out->d_reclen = LINUX_DIRENT_SIZE
        //   out->d_type = entry->d_type
        //   strncpy(out->d_name, entry->d_name, LINUX_D_NAME_MAX)
        //   return out

        let entry_ptr_reg = alloc_result(&mut f, ptr_i8.clone());
        let is_null_reg = alloc_result(&mut f, AsmType::I1);
        let out_ptr_reg = alloc_result(&mut f, ptr_i8.clone());
        let entry_ino_addr_reg = alloc_result(&mut f, ptr_i8.clone());
        let entry_ino_reg = alloc_result(&mut f, AsmType::I64);
        let out_ino_addr_reg = alloc_result(&mut f, ptr_i8.clone());
        let out_reclen_addr_reg = alloc_result(&mut f, ptr_i8.clone());
        let entry_type_addr_reg = alloc_result(&mut f, ptr_i8.clone());
        let entry_type_reg = alloc_result(&mut f, AsmType::I8);
        let out_type_addr_reg = alloc_result(&mut f, ptr_i8.clone());
        let out_name_ptr_reg = alloc_result(&mut f, ptr_i8.clone());
        let entry_name_ptr_reg = alloc_result(&mut f, ptr_i8.clone());

        let entry_ptr = vreg_read(entry_ptr_reg);
        let out_ptr = vreg_read(out_ptr_reg);

        let mut entry_insts = Vec::new();
        entry_insts.push(build_call_symbol(
            fresh_id(),
            Some(entry_ptr_reg),
            "readdir",
            vec![AsmOperand::Local(dir_local.id)],
            cc.clone(),
        ));
        entry_insts.push(build_eq(
            fresh_id(),
            is_null_reg,
            entry_ptr.clone(),
            null_ptr.clone(),
        ));

        let entry_block = AsmBlock {
            id: 0,
            label: Some(Name::new("entry")),
            instructions: entry_insts,
            terminator: AsmTerminator::CondBr {
                condition: vreg_read(is_null_reg),
                if_true: 1,
                if_false: 2,
            },
            terminator_encoding: None,
            predecessors: Vec::new(),
            successors: vec![1, 2],
        };

        let null_block = AsmBlock {
            id: 1,
            label: Some(Name::new("return_null")),
            instructions: Vec::new(),
            terminator: AsmTerminator::Return(Some(null_ptr.clone())),
            terminator_encoding: None,
            predecessors: vec![0],
            successors: Vec::new(),
        };

        let mut alloc_insts = Vec::new();
        alloc_insts.push(build_call_symbol(
            fresh_id(),
            Some(out_ptr_reg),
            "malloc",
            vec![AsmOperand::Constant(AsmConstant::UInt(
                LINUX_DIRENT_SIZE,
                AsmType::I64,
            ))],
            cc.clone(),
        ));
        alloc_insts.push(build_call_symbol(
            fresh_id(),
            None,
            "memset",
            vec![
                out_ptr.clone(),
                AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I32)),
                AsmOperand::Constant(AsmConstant::UInt(LINUX_DIRENT_SIZE, AsmType::I64)),
            ],
            cc.clone(),
        ));

        alloc_insts.push(build_binop(
            fresh_id(),
            AsmGenericOpcode::Add,
            entry_ino_addr_reg,
            entry_ptr.clone(),
            AsmOperand::Constant(AsmConstant::UInt(host_d_ino_offset, AsmType::I64)),
        ));
        alloc_insts.push(build_load(
            fresh_id(),
            entry_ino_reg,
            vreg_read(entry_ino_addr_reg),
        ));

        alloc_insts.push(build_binop(
            fresh_id(),
            AsmGenericOpcode::Add,
            out_ino_addr_reg,
            out_ptr.clone(),
            AsmOperand::Constant(AsmConstant::UInt(LINUX_D_INO_OFFSET, AsmType::I64)),
        ));
        alloc_insts.push(build_store(
            fresh_id(),
            vreg_read(entry_ino_reg),
            vreg_read(out_ino_addr_reg),
        ));

        alloc_insts.push(build_binop(
            fresh_id(),
            AsmGenericOpcode::Add,
            out_reclen_addr_reg,
            out_ptr.clone(),
            AsmOperand::Constant(AsmConstant::UInt(LINUX_D_RECLEN_OFFSET, AsmType::I64)),
        ));
        alloc_insts.push(build_store(
            fresh_id(),
            AsmOperand::Constant(AsmConstant::UInt(LINUX_DIRENT_SIZE, AsmType::I16)),
            vreg_read(out_reclen_addr_reg),
        ));

        alloc_insts.push(build_binop(
            fresh_id(),
            AsmGenericOpcode::Add,
            entry_type_addr_reg,
            entry_ptr.clone(),
            AsmOperand::Constant(AsmConstant::UInt(host_d_type_offset, AsmType::I64)),
        ));
        alloc_insts.push(build_load(
            fresh_id(),
            entry_type_reg,
            vreg_read(entry_type_addr_reg),
        ));
        alloc_insts.push(build_binop(
            fresh_id(),
            AsmGenericOpcode::Add,
            out_type_addr_reg,
            out_ptr.clone(),
            AsmOperand::Constant(AsmConstant::UInt(LINUX_D_TYPE_OFFSET, AsmType::I64)),
        ));
        alloc_insts.push(build_store(
            fresh_id(),
            vreg_read(entry_type_reg),
            vreg_read(out_type_addr_reg),
        ));

        alloc_insts.push(build_binop(
            fresh_id(),
            AsmGenericOpcode::Add,
            out_name_ptr_reg,
            out_ptr.clone(),
            AsmOperand::Constant(AsmConstant::UInt(LINUX_D_NAME_OFFSET, AsmType::I64)),
        ));
        alloc_insts.push(build_binop(
            fresh_id(),
            AsmGenericOpcode::Add,
            entry_name_ptr_reg,
            entry_ptr.clone(),
            AsmOperand::Constant(AsmConstant::UInt(host_d_name_offset, AsmType::I64)),
        ));

        alloc_insts.push(build_call_symbol(
            fresh_id(),
            None,
            "strncpy",
            vec![
                vreg_read(out_name_ptr_reg),
                vreg_read(entry_name_ptr_reg),
                AsmOperand::Constant(AsmConstant::UInt(LINUX_D_NAME_MAX, AsmType::I64)),
            ],
            cc.clone(),
        ));

        let alloc_block = AsmBlock {
            id: 2,
            label: Some(Name::new("alloc")),
            instructions: alloc_insts,
            terminator: AsmTerminator::Return(Some(out_ptr.clone())),
            terminator_encoding: None,
            predecessors: vec![0],
            successors: Vec::new(),
        };

        f.basic_blocks = vec![entry_block, null_block, alloc_block];
        f.linkage = Linkage::External;
        f.visibility = Visibility::Default;
        f.calling_convention = Some(cc);
        f.section = Some(".text".to_string());
        f.is_declaration = false;
        program.functions.push(f);
        Ok(())
    }
}

fn lower_sys_ops_to_windows_imports(program: &mut AsmProgram) -> Result<()> {
    for function in &mut program.functions {
        if function.is_declaration {
            continue;
        }

        let mut next_id = function
            .basic_blocks
            .iter()
            .flat_map(|block| block.instructions.iter().map(|inst| inst.id))
            .max()
            .unwrap_or(0)
            .saturating_add(1);

        // Temporarily move `basic_blocks` out so `function` is free to be
        // borrowed mutably (for `alloc_virtual_register`) while iterating.
        let mut blocks = std::mem::take(&mut function.basic_blocks);
        for block in &mut blocks {
            let snapshot = block.instructions.clone();
            let mut out = Vec::with_capacity(block.instructions.len());

            for inst in &block.instructions {
                let Some(op) = sysop_of(inst) else {
                    out.push(inst.clone());
                    continue;
                };
                let dest = result_vreg(inst);

                match lower_system_api_to_windows_import(
                    op.clone(),
                    inst.id,
                    dest,
                    &snapshot,
                    &mut next_id,
                    function,
                )? {
                    LoweredWindows::Unchanged => out.push(inst.clone()),
                    LoweredWindows::Single(lowered) => out.push(lowered),
                    LoweredWindows::Sequence(mut seq) => out.append(&mut seq),
                }
            }

            block.instructions = out;
        }
        function.basic_blocks = blocks;
    }
    Ok(())
}

fn target_syscall_convention(program: &AsmProgram) -> Option<AsmSyscallConvention> {
    match program.target.object_format {
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
    }
}

fn rewrite_syscalls_to_target_unix_convention(program: &mut AsmProgram) -> Result<()> {
    let Some(target_convention) = target_syscall_convention(program) else {
        return Ok(());
    };

    for function in &mut program.functions {
        if function.is_declaration {
            continue;
        }

        for block in &mut function.basic_blocks {
            let snapshot = block.instructions.clone();
            for inst in &mut block.instructions {
                let Some((convention, number, args)) = syscall_parts(inst) else {
                    continue;
                };
                if convention == target_convention {
                    continue;
                }

                let Some(op) = detect_system_api_from_syscall(&convention, number, args, &snapshot)
                else {
                    continue;
                };
                let dest = result_vreg(inst);
                *inst = lower_system_api_to_syscall(inst.id, dest, op, target_convention);
            }
        }
    }
    Ok(())
}

fn rewrite_posix_calls_to_windows_imports(program: &mut AsmProgram) -> Result<()> {
    for function in &mut program.functions {
        if function.is_declaration {
            continue;
        }

        let mut next_id = function
            .basic_blocks
            .iter()
            .flat_map(|block| block.instructions.iter().map(|inst| inst.id))
            .max()
            .unwrap_or(0)
            .saturating_add(1);

        let mut blocks = std::mem::take(&mut function.basic_blocks);
        for block in &mut blocks {
            let snapshot = block.instructions.clone();
            let mut out = Vec::with_capacity(block.instructions.len());

            for inst in &block.instructions {
                let Some(op) = detect_system_api_from_posix_call(inst, PosixDirentStyle::Linux)
                else {
                    out.push(inst.clone());
                    continue;
                };
                let dest = result_vreg(inst);

                match lower_system_api_to_windows_import(
                    op,
                    inst.id,
                    dest,
                    &snapshot,
                    &mut next_id,
                    function,
                )? {
                    LoweredWindows::Unchanged => out.push(inst.clone()),
                    LoweredWindows::Single(lowered) => out.push(lowered),
                    LoweredWindows::Sequence(mut seq) => out.append(&mut seq),
                }
            }

            block.instructions = out;
        }
        function.basic_blocks = blocks;
    }
    Ok(())
}

fn rewrite_syscalls_to_windows_imports(program: &mut AsmProgram) -> Result<()> {
    for function in &mut program.functions {
        if function.is_declaration {
            continue;
        }

        let mut next_id = function
            .basic_blocks
            .iter()
            .flat_map(|block| block.instructions.iter().map(|inst| inst.id))
            .max()
            .unwrap_or(0)
            .saturating_add(1);

        let mut blocks = std::mem::take(&mut function.basic_blocks);
        for block in &mut blocks {
            let snapshot = block.instructions.clone();
            let mut out = Vec::with_capacity(block.instructions.len());

            for inst in &block.instructions {
                let Some((convention, number, args)) = syscall_parts(inst) else {
                    out.push(inst.clone());
                    continue;
                };

                let Some(op) = detect_system_api_from_syscall(&convention, number, args, &snapshot)
                else {
                    out.push(inst.clone());
                    continue;
                };
                let dest = result_vreg(inst);
                let result_reg = inst.result_register().cloned();

                match lower_system_api_to_windows_import(
                    op,
                    inst.id,
                    dest,
                    &snapshot,
                    &mut next_id,
                    function,
                )? {
                    LoweredWindows::Unchanged => out.push(inst.clone()),
                    LoweredWindows::Single(lowered) => {
                        if let fp_core::asmir::AsmTerminator::Return(Some(value)) =
                            &block.terminator
                        {
                            if let (AsmOperand::Register { reg, .. }, Some(result_reg)) =
                                (value, &result_reg)
                            {
                                if reg == result_reg {
                                    block.terminator = fp_core::asmir::AsmTerminator::Return(None);
                                }
                            }
                        }
                        out.push(lowered);
                    }
                    LoweredWindows::Sequence(mut seq) => out.append(&mut seq),
                }
            }

            block.instructions = out;
        }
        function.basic_blocks = blocks;
    }
    Ok(())
}

fn rewrite_windows_imports_to_syscalls(program: &mut AsmProgram) -> Result<()> {
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
        return Ok(());
    };

    for function in &mut program.functions {
        if function.is_declaration {
            continue;
        }

        for block in &mut function.basic_blocks {
            let mut out = Vec::with_capacity(block.instructions.len());
            let mut i = 0usize;
            while i < block.instructions.len() {
                if let Some((rewritten, consumed)) =
                    match_writefile_sequence_to_syscall(&block.instructions[i..], convention)?
                {
                    out.push(rewritten);
                    i = i.saturating_add(consumed);
                    continue;
                }

                if let Some((rewritten, consumed)) =
                    match_readfile_sequence_to_syscall(&block.instructions[i..], convention)?
                {
                    out.push(rewritten);
                    i = i.saturating_add(consumed);
                    continue;
                }

                if let Some((rewritten, consumed)) =
                    match_closehandle_sequence_to_syscall(&block.instructions[i..], convention)?
                {
                    out.push(rewritten);
                    i = i.saturating_add(consumed);
                    continue;
                }

                if let Some((rewritten, consumed)) = match_setfilepointerex_sequence_to_syscall(
                    &block.instructions[i..],
                    convention,
                )? {
                    out.push(rewritten);
                    i = i.saturating_add(consumed);
                    continue;
                }

                if let Some((rewritten, consumed)) =
                    match_virtualalloc_sequence_to_syscall(&block.instructions[i..], convention)?
                {
                    out.push(rewritten);
                    i = i.saturating_add(consumed);
                    continue;
                }

                if let Some((rewritten, consumed)) =
                    match_virtualfree_sequence_to_syscall(&block.instructions[i..], convention)?
                {
                    out.push(rewritten);
                    i = i.saturating_add(consumed);
                    continue;
                }

                if let Some((rewritten, consumed)) =
                    match_deletefile_sequence_to_syscall(&block.instructions[i..], convention)?
                {
                    out.push(rewritten);
                    i = i.saturating_add(consumed);
                    continue;
                }

                if let Some((rewritten, consumed)) =
                    match_createdirectory_sequence_to_syscall(&block.instructions[i..], convention)?
                {
                    out.push(rewritten);
                    i = i.saturating_add(consumed);
                    continue;
                }

                if let Some((rewritten, consumed)) =
                    match_removedirectory_sequence_to_syscall(&block.instructions[i..], convention)?
                {
                    out.push(rewritten);
                    i = i.saturating_add(consumed);
                    continue;
                }

                if let Some((rewritten, consumed)) =
                    match_movefileex_sequence_to_syscall(&block.instructions[i..], convention)?
                {
                    out.push(rewritten);
                    i = i.saturating_add(consumed);
                    continue;
                }

                if let Some((rewritten, consumed)) = match_getfileattributes_sequence_to_syscall(
                    &block.instructions[i..],
                    convention,
                )? {
                    out.push(rewritten);
                    i = i.saturating_add(consumed);
                    continue;
                }

                if let Some((rewritten, consumed)) =
                    match_freelibrary_sequence_to_unix_call(&block.instructions[i..], convention)?
                {
                    out.push(rewritten);
                    i = i.saturating_add(consumed);
                    continue;
                }

                let mut inst = block.instructions[i].clone();
                if let Some(op) = detect_system_api_from_windows_import(&inst, convention) {
                    let dest = result_vreg(&inst);
                    inst = lower_system_api_to_unix(inst.id, dest, op, convention);
                }
                out.push(inst);
                i += 1;
            }
            block.instructions = out;
        }
    }
    Ok(())
}

fn unix_calling_convention(convention: AsmSyscallConvention) -> CallingConvention {
    match convention {
        AsmSyscallConvention::LinuxX86_64 | AsmSyscallConvention::DarwinX86_64 => {
            CallingConvention::X86_64SysV
        }
        AsmSyscallConvention::LinuxAarch64 | AsmSyscallConvention::DarwinAarch64 => {
            CallingConvention::AAPCS
        }
    }
}

fn lower_system_api_to_unix(
    id: u32,
    dest: Option<AsmVirtualRegId>,
    op: SystemApiOp,
    convention: AsmSyscallConvention,
) -> AsmInstruction {
    match op {
        SystemApiOp::Dlopen { path, flags } => build_call(
            id,
            dest,
            AsmOperand::Symbol(Name::new("dlopen")),
            vec![path, flags],
            unix_calling_convention(convention),
            false,
        ),
        SystemApiOp::Dlsym { handle, symbol } => build_call(
            id,
            dest,
            AsmOperand::Symbol(Name::new("dlsym")),
            vec![handle, symbol],
            unix_calling_convention(convention),
            false,
        ),
        SystemApiOp::Dlclose { handle } => build_call(
            id,
            dest,
            AsmOperand::Symbol(Name::new("dlclose")),
            vec![handle],
            unix_calling_convention(convention),
            false,
        ),
        other => lower_system_api_to_syscall(id, dest, other, convention),
    }
}

fn detect_system_api_from_windows_import(
    inst: &AsmInstruction,
    convention: AsmSyscallConvention,
) -> Option<SystemApiOp> {
    let (target, args) = inst.call_target_and_args()?;
    let AsmOperand::Symbol(name) = target else {
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
                .unwrap_or_else(|| AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I32)));
            Some(SystemApiOp::Exit { code })
        }
        "RtlExitUserProcess" => {
            if !is_ntdll {
                return None;
            }
            let code = args
                .first()
                .cloned()
                .unwrap_or_else(|| AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I32)));
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
                .unwrap_or_else(|| AsmOperand::Constant(AsmConstant::Null(AsmType::Ptr(Box::new(AsmType::I8)))));
            Some(SystemApiOp::Dlopen {
                path,
                flags: AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I32)),
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
                .unwrap_or_else(|| AsmOperand::Constant(AsmConstant::Null(AsmType::Ptr(Box::new(AsmType::I8)))));
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
                mode: AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I32)),
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
                .unwrap_or_else(|| AsmOperand::Constant(AsmConstant::Null(AsmType::Ptr(Box::new(AsmType::I8)))));
            Some(SystemApiOp::Access {
                path,
                mode: AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I32)),
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
                flags: AsmOperand::Constant(AsmConstant::Int(flags, AsmType::I64)),
                mode: AsmOperand::Constant(AsmConstant::Int(0, AsmType::I64)),
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
                prot: AsmOperand::Constant(AsmConstant::Int(prot, AsmType::I64)),
                flags: AsmOperand::Constant(AsmConstant::Int(
                    posix_mmap_flags_anonymous_private(style),
                    AsmType::I64,
                )),
                fd: AsmOperand::Constant(AsmConstant::Int(-1, AsmType::I64)),
                offset: AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I64)),
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

fn posix_flags_from_createfile(
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

fn detect_system_api_from_syscall(
    convention: &AsmSyscallConvention,
    number: &AsmOperand,
    args: &[AsmOperand],
    instructions: &[AsmInstruction],
) -> Option<SystemApiOp> {
    let num = resolve_u64(number, instructions)?;

    match convention {
        AsmSyscallConvention::LinuxX86_64 if num == 60 => Some(SystemApiOp::Exit {
            code: args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I32))),
        }),
        AsmSyscallConvention::LinuxX86_64 if num == 39 => Some(SystemApiOp::GetPid),
        AsmSyscallConvention::LinuxX86_64 if num == 186 => Some(SystemApiOp::GetTid),
        AsmSyscallConvention::LinuxAarch64 if num == 93 => Some(SystemApiOp::Exit {
            code: args
                .get(0)
                .cloned()
                .unwrap_or_else(|| AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I32))),
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
                    .unwrap_or_else(|| AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I32))),
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

enum LoweredWindows {
    Unchanged,
    Single(AsmInstruction),
    Sequence(Vec<AsmInstruction>),
}

/// Builds the common `call; cmp; select` idiom used across most POSIX ->
/// Win32 lowerings: `select (call(...) == failure) ? -1 : 0`.
fn win_bool_call_to_select(
    function: &mut AsmFunction,
    next_id: &mut u32,
    replaces_id: u32,
    dest: Option<AsmVirtualRegId>,
    target: &str,
    args: Vec<AsmOperand>,
    failure: AsmConstant,
) -> LoweredWindows {
    let call_dest = alloc_result(function, AsmType::I64);
    let cmp_dest = alloc_result(function, AsmType::I1);
    let call_id = *next_id;
    *next_id = next_id.saturating_add(1);
    let cmp_id = *next_id;
    *next_id = next_id.saturating_add(1);

    let call = build_call_symbol(call_id, Some(call_dest), target, args, CallingConvention::Win64);
    let cmp = build_eq(
        cmp_id,
        cmp_dest,
        vreg_read(call_dest),
        AsmOperand::Constant(failure),
    );
    let select = match dest {
        Some(dest) => build_select(
            replaces_id,
            dest,
            vreg_read(cmp_dest),
            AsmOperand::Constant(AsmConstant::Int(-1, AsmType::I64)),
            AsmOperand::Constant(AsmConstant::Int(0, AsmType::I64)),
        ),
        None => AsmInstruction::new(replaces_id, AsmOpcode::Generic(AsmGenericOpcode::Nop), Vec::new()),
    };
    LoweredWindows::Sequence(vec![call, cmp, select])
}

/// Builds a `GetStdHandle(code)` prefix call, returning the instruction and
/// the virtual register holding its result.
fn win_getstdhandle(
    function: &mut AsmFunction,
    id: u32,
    std_handle_code: i64,
) -> (AsmInstruction, AsmVirtualRegId) {
    let dest = alloc_result(function, AsmType::Ptr(Box::new(AsmType::I8)));
    let inst = build_call_symbol(
        id,
        Some(dest),
        "kernel32!GetStdHandle",
        vec![AsmOperand::Constant(AsmConstant::Int(
            std_handle_code,
            AsmType::I64,
        ))],
        CallingConvention::Win64,
    );
    (inst, dest)
}

fn lower_system_api_to_windows_import(
    op: SystemApiOp,
    replaces_id: u32,
    dest: Option<AsmVirtualRegId>,
    instructions: &[AsmInstruction],
    next_id: &mut u32,
    function: &mut AsmFunction,
) -> Result<LoweredWindows> {
    match op {
        SystemApiOp::Exit { code } => Ok(LoweredWindows::Single(build_call_symbol(
            replaces_id,
            dest,
            "kernel32!ExitProcess",
            vec![code],
            CallingConvention::Win64,
        ))),
        SystemApiOp::GetPid => Ok(LoweredWindows::Single(build_call_symbol(
            replaces_id,
            dest,
            "kernel32!GetCurrentProcessId",
            Vec::new(),
            CallingConvention::Win64,
        ))),
        SystemApiOp::GetTid => Ok(LoweredWindows::Single(build_call_symbol(
            replaces_id,
            dest,
            "kernel32!GetCurrentThreadId",
            Vec::new(),
            CallingConvention::Win64,
        ))),
        SystemApiOp::Dlopen { path, .. } => Ok(LoweredWindows::Single(build_call_symbol(
            replaces_id,
            dest,
            "kernel32!LoadLibraryA",
            vec![path],
            CallingConvention::Win64,
        ))),
        SystemApiOp::Dlsym { handle, symbol } => Ok(LoweredWindows::Single(build_call_symbol(
            replaces_id,
            dest,
            "kernel32!GetProcAddress",
            vec![handle, symbol],
            CallingConvention::Win64,
        ))),
        SystemApiOp::Dlclose { handle } => Ok(win_bool_call_to_select(
            function,
            next_id,
            replaces_id,
            dest,
            "kernel32!FreeLibrary",
            vec![handle],
            AsmConstant::Bool(false),
        )),
        SystemApiOp::Unlink { path } => Ok(win_bool_call_to_select(
            function,
            next_id,
            replaces_id,
            dest,
            "kernel32!DeleteFileA",
            vec![path],
            AsmConstant::Bool(false),
        )),
        SystemApiOp::Mkdir { path, .. } => Ok(win_bool_call_to_select(
            function,
            next_id,
            replaces_id,
            dest,
            "kernel32!CreateDirectoryA",
            vec![
                path,
                AsmOperand::Constant(AsmConstant::Null(AsmType::Ptr(Box::new(AsmType::I8)))),
            ],
            AsmConstant::Bool(false),
        )),
        SystemApiOp::Rmdir { path } => Ok(win_bool_call_to_select(
            function,
            next_id,
            replaces_id,
            dest,
            "kernel32!RemoveDirectoryA",
            vec![path],
            AsmConstant::Bool(false),
        )),
        SystemApiOp::Rename { from, to } => {
            // MOVEFILE_REPLACE_EXISTING=1
            const MOVEFILE_REPLACE_EXISTING: i64 = 1;
            Ok(win_bool_call_to_select(
                function,
                next_id,
                replaces_id,
                dest,
                "kernel32!MoveFileExA",
                vec![
                    from,
                    to,
                    AsmOperand::Constant(AsmConstant::Int(
                        MOVEFILE_REPLACE_EXISTING,
                        AsmType::I64,
                    )),
                ],
                AsmConstant::Bool(false),
            ))
        }
        SystemApiOp::Access { path, .. } => Ok(win_bool_call_to_select(
            function,
            next_id,
            replaces_id,
            dest,
            "kernel32!GetFileAttributesA",
            vec![path],
            AsmConstant::Int(-1, AsmType::I64),
        )),
        SystemApiOp::Write { fd, buffer, len } => {
            let (handle_value, std_handle_code) = match resolve_i64(&fd, instructions)
                .map_err(|e| {
                    eprintln!("[fp-native] Win32-to-POSIX arg resolution error: {e}");
                    e
                })
                .ok()
                .flatten()
            {
                Some(fd) => {
                    if fd == 0 {
                        return Ok(LoweredWindows::Unchanged);
                    }
                    let Some(code) = fd_to_std_handle_code(fd) else {
                        return Ok(LoweredWindows::Unchanged);
                    };
                    (None, Some(code))
                }
                None => (Some(fd), None),
            };

            let getstd_id = *next_id;
            *next_id = next_id.saturating_add(1);
            let alloca_id = *next_id;
            *next_id = next_id.saturating_add(1);
            let writefile_id = *next_id;
            *next_id = next_id.saturating_add(1);
            let load_id = *next_id;
            *next_id = next_id.saturating_add(1);
            let cmp_id = *next_id;
            *next_id = next_id.saturating_add(1);

            let (prefix, handle_arg) = if let Some(std_handle_code) = std_handle_code {
                let (inst, reg) = win_getstdhandle(function, getstd_id, std_handle_code);
                (Some(inst), vreg_read(reg))
            } else {
                (
                    None,
                    handle_value
                        .ok_or_else(|| fp_core::error::Error::from("missing write handle"))?,
                )
            };

            let alloca_reg = alloc_result(function, AsmType::Ptr(Box::new(AsmType::I64)));
            let alloca_written = build_alloca(
                alloca_id,
                alloca_reg,
                AsmOperand::Constant(AsmConstant::UInt(1, AsmType::I64)),
                8,
            );

            let writefile_reg = alloc_result(function, AsmType::I1);
            let writefile = build_call_symbol(
                writefile_id,
                Some(writefile_reg),
                "kernel32!WriteFile",
                vec![
                    handle_arg,
                    buffer,
                    len,
                    vreg_read(alloca_reg),
                    AsmOperand::Constant(AsmConstant::Null(AsmType::Ptr(Box::new(AsmType::I8)))),
                ],
                CallingConvention::Win64,
            );

            let load_reg = alloc_result(function, AsmType::I64);
            let load_written = build_load(load_id, load_reg, vreg_read(alloca_reg));

            let cmp_reg = alloc_result(function, AsmType::I1);
            let cmp = build_eq(
                cmp_id,
                cmp_reg,
                vreg_read(writefile_reg),
                AsmOperand::Constant(AsmConstant::Bool(false)),
            );

            let select = match dest {
                Some(dest) => build_select(
                    replaces_id,
                    dest,
                    vreg_read(cmp_reg),
                    AsmOperand::Constant(AsmConstant::Int(-1, AsmType::I64)),
                    vreg_read(load_reg),
                ),
                None => {
                    AsmInstruction::new(replaces_id, AsmOpcode::Generic(AsmGenericOpcode::Nop), Vec::new())
                }
            };

            let mut seq = Vec::new();
            if let Some(prefix) = prefix {
                seq.push(prefix);
            }
            seq.extend_from_slice(&[alloca_written, writefile, load_written, cmp, select]);
            Ok(LoweredWindows::Sequence(seq))
        }
        SystemApiOp::Read { fd, buffer, len } => {
            let (handle_value, use_stdio) = match resolve_i64(&fd, instructions)
                .map_err(|e| {
                    eprintln!("[fp-native] Win32-to-POSIX arg resolution error: {e}");
                    e
                })
                .ok()
                .flatten()
            {
                Some(0) => (None, true),
                Some(_) => return Ok(LoweredWindows::Unchanged),
                None => (Some(fd), false),
            };

            let getstd_id = *next_id;
            *next_id = next_id.saturating_add(1);
            let alloca_id = *next_id;
            *next_id = next_id.saturating_add(1);
            let readfile_id = *next_id;
            *next_id = next_id.saturating_add(1);
            let load_id = *next_id;
            *next_id = next_id.saturating_add(1);
            let cmp_id = *next_id;
            *next_id = next_id.saturating_add(1);

            let (prefix, handle_arg) = if use_stdio {
                let (inst, reg) = win_getstdhandle(function, getstd_id, -10);
                (Some(inst), vreg_read(reg))
            } else {
                (
                    None,
                    handle_value
                        .ok_or_else(|| fp_core::error::Error::from("missing read handle"))?,
                )
            };

            let alloca_reg = alloc_result(function, AsmType::Ptr(Box::new(AsmType::I64)));
            let alloca_read = build_alloca(
                alloca_id,
                alloca_reg,
                AsmOperand::Constant(AsmConstant::UInt(1, AsmType::I64)),
                8,
            );

            let readfile_reg = alloc_result(function, AsmType::I1);
            let readfile = build_call_symbol(
                readfile_id,
                Some(readfile_reg),
                "kernel32!ReadFile",
                vec![
                    handle_arg,
                    buffer,
                    len,
                    vreg_read(alloca_reg),
                    AsmOperand::Constant(AsmConstant::Null(AsmType::Ptr(Box::new(AsmType::I8)))),
                ],
                CallingConvention::Win64,
            );

            let load_reg = alloc_result(function, AsmType::I64);
            let load_read = build_load(load_id, load_reg, vreg_read(alloca_reg));

            let cmp_reg = alloc_result(function, AsmType::I1);
            let cmp = build_eq(
                cmp_id,
                cmp_reg,
                vreg_read(readfile_reg),
                AsmOperand::Constant(AsmConstant::Bool(false)),
            );

            let select = match dest {
                Some(dest) => build_select(
                    replaces_id,
                    dest,
                    vreg_read(cmp_reg),
                    AsmOperand::Constant(AsmConstant::Int(-1, AsmType::I64)),
                    vreg_read(load_reg),
                ),
                None => {
                    AsmInstruction::new(replaces_id, AsmOpcode::Generic(AsmGenericOpcode::Nop), Vec::new())
                }
            };

            let mut seq = Vec::new();
            if let Some(prefix) = prefix {
                seq.push(prefix);
            }
            seq.extend_from_slice(&[alloca_read, readfile, load_read, cmp, select]);
            Ok(LoweredWindows::Sequence(seq))
        }
        SystemApiOp::Close { fd } => {
            let (handle_value, std_handle_code) = match resolve_i64(&fd, instructions)
                .map_err(|e| {
                    eprintln!("[fp-native] Win32-to-POSIX arg resolution error: {e}");
                    e
                })
                .ok()
                .flatten()
            {
                Some(fd) => {
                    let Some(code) = fd_to_std_handle_code(fd) else {
                        return Ok(LoweredWindows::Unchanged);
                    };
                    (None, Some(code))
                }
                None => (Some(fd), None),
            };

            let getstd_id = *next_id;
            *next_id = next_id.saturating_add(1);
            let close_id = *next_id;
            *next_id = next_id.saturating_add(1);
            let cmp_id = *next_id;
            *next_id = next_id.saturating_add(1);

            let (prefix, handle_arg) = if let Some(std_handle_code) = std_handle_code {
                let (inst, reg) = win_getstdhandle(function, getstd_id, std_handle_code);
                (Some(inst), vreg_read(reg))
            } else {
                (
                    None,
                    handle_value
                        .ok_or_else(|| fp_core::error::Error::from("missing close handle"))?,
                )
            };

            let close_reg = alloc_result(function, AsmType::I1);
            let close = build_call_symbol(
                close_id,
                Some(close_reg),
                "kernel32!CloseHandle",
                vec![handle_arg],
                CallingConvention::Win64,
            );

            let cmp_reg = alloc_result(function, AsmType::I1);
            let cmp = build_eq(
                cmp_id,
                cmp_reg,
                vreg_read(close_reg),
                AsmOperand::Constant(AsmConstant::Bool(false)),
            );

            let select = match dest {
                Some(dest) => build_select(
                    replaces_id,
                    dest,
                    vreg_read(cmp_reg),
                    AsmOperand::Constant(AsmConstant::Int(-1, AsmType::I64)),
                    AsmOperand::Constant(AsmConstant::Int(0, AsmType::I64)),
                ),
                None => {
                    AsmInstruction::new(replaces_id, AsmOpcode::Generic(AsmGenericOpcode::Nop), Vec::new())
                }
            };

            let mut seq = Vec::new();
            if let Some(prefix) = prefix {
                seq.push(prefix);
            }
            seq.extend_from_slice(&[close, cmp, select]);
            Ok(LoweredWindows::Sequence(seq))
        }
        SystemApiOp::Open {
            path,
            flags,
            flag_style,
            ..
        } => {
            let Some(flags) = resolve_i64(&flags, instructions)? else {
                return Ok(LoweredWindows::Unchanged);
            };

            // Win32 constants.
            const FILE_SHARE_READ: i64 = 0x0000_0001;
            const FILE_SHARE_WRITE: i64 = 0x0000_0002;
            const FILE_SHARE_DELETE: i64 = 0x0000_0004;
            const FILE_ATTRIBUTE_NORMAL: i64 = 0x0000_0080;

            let desired_access = windows_createfile_desired_access(flags);
            let disposition = windows_createfile_disposition_from_flags(flag_style, flags);

            Ok(LoweredWindows::Single(build_call_symbol(
                replaces_id,
                dest,
                "kernel32!CreateFileA",
                vec![
                    path,
                    AsmOperand::Constant(AsmConstant::Int(desired_access, AsmType::I64)),
                    AsmOperand::Constant(AsmConstant::Int(
                        FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
                        AsmType::I64,
                    )),
                    AsmOperand::Constant(AsmConstant::Null(AsmType::Ptr(Box::new(AsmType::I8)))),
                    AsmOperand::Constant(AsmConstant::Int(disposition, AsmType::I64)),
                    AsmOperand::Constant(AsmConstant::Int(FILE_ATTRIBUTE_NORMAL, AsmType::I64)),
                    AsmOperand::Constant(AsmConstant::Null(AsmType::Ptr(Box::new(AsmType::I8)))),
                ],
                CallingConvention::Win64,
            )))
        }
        SystemApiOp::Seek { fd, offset, whence } => {
            let (handle_value, std_handle_code) = match resolve_i64(&fd, instructions)
                .map_err(|e| {
                    eprintln!("[fp-native] Win32-to-POSIX arg resolution error: {e}");
                    e
                })
                .ok()
                .flatten()
            {
                Some(fd) => {
                    let Some(code) = fd_to_std_handle_code(fd) else {
                        return Ok(LoweredWindows::Unchanged);
                    };
                    (None, Some(code))
                }
                None => (Some(fd), None),
            };

            let getstd_id = *next_id;
            *next_id = next_id.saturating_add(1);
            let alloca_id = *next_id;
            *next_id = next_id.saturating_add(1);
            let setfp_id = *next_id;
            *next_id = next_id.saturating_add(1);
            let load_id = *next_id;
            *next_id = next_id.saturating_add(1);
            let cmp_id = *next_id;
            *next_id = next_id.saturating_add(1);

            let (prefix, handle_arg) = if let Some(std_handle_code) = std_handle_code {
                let (inst, reg) = win_getstdhandle(function, getstd_id, std_handle_code);
                (Some(inst), vreg_read(reg))
            } else {
                (
                    None,
                    handle_value
                        .ok_or_else(|| fp_core::error::Error::from("missing seek handle"))?,
                )
            };

            let alloca_reg = alloc_result(function, AsmType::Ptr(Box::new(AsmType::I64)));
            let alloca_new_pos = build_alloca(
                alloca_id,
                alloca_reg,
                AsmOperand::Constant(AsmConstant::UInt(1, AsmType::I64)),
                8,
            );

            let setfp_reg = alloc_result(function, AsmType::I1);
            let setfp = build_call_symbol(
                setfp_id,
                Some(setfp_reg),
                "kernel32!SetFilePointerEx",
                vec![handle_arg, offset, vreg_read(alloca_reg), whence],
                CallingConvention::Win64,
            );

            let load_reg = alloc_result(function, AsmType::I64);
            let load_new_pos = build_load(load_id, load_reg, vreg_read(alloca_reg));

            let cmp_reg = alloc_result(function, AsmType::I1);
            let cmp = build_eq(
                cmp_id,
                cmp_reg,
                vreg_read(setfp_reg),
                AsmOperand::Constant(AsmConstant::Bool(false)),
            );

            let select = match dest {
                Some(dest) => build_select(
                    replaces_id,
                    dest,
                    vreg_read(cmp_reg),
                    AsmOperand::Constant(AsmConstant::Int(-1, AsmType::I64)),
                    vreg_read(load_reg),
                ),
                None => {
                    AsmInstruction::new(replaces_id, AsmOpcode::Generic(AsmGenericOpcode::Nop), Vec::new())
                }
            };

            let mut seq = Vec::new();
            if let Some(prefix) = prefix {
                seq.push(prefix);
            }
            seq.extend_from_slice(&[alloca_new_pos, setfp, load_new_pos, cmp, select]);
            Ok(LoweredWindows::Sequence(seq))
        }
        SystemApiOp::Mmap {
            addr,
            len,
            prot,
            flags: _,
            fd,
            offset,
        } => {
            let fd_value = resolve_i64(&fd, instructions)
                .map_err(|e| {
                    eprintln!("[fp-native] Win32-to-POSIX arg resolution error: {e}");
                    e
                })
                .ok()
                .flatten();
            let offset_value = resolve_i64(&offset, instructions)
                .map_err(|e| {
                    eprintln!("[fp-native] Win32-to-POSIX arg resolution error: {e}");
                    e
                })
                .ok()
                .flatten();
            if fd_value != Some(-1) || offset_value != Some(0) {
                return Ok(LoweredWindows::Unchanged);
            }
            let Some(prot) = resolve_i64(&prot, instructions)
                .map_err(|e| {
                    eprintln!("[fp-native] Win32-to-POSIX arg resolution error: {e}");
                    e
                })
                .ok()
                .flatten()
            else {
                return Ok(LoweredWindows::Unchanged);
            };

            // MEM_COMMIT=0x1000, MEM_RESERVE=0x2000
            const MEM_COMMIT_RESERVE: i64 = 0x3000;
            let protection = windows_page_protection_from_posix(prot);

            let call_id = *next_id;
            *next_id = next_id.saturating_add(1);
            let cmp_id = *next_id;
            *next_id = next_id.saturating_add(1);

            let call_reg = alloc_result(function, AsmType::I64);
            let call = build_call_symbol(
                call_id,
                Some(call_reg),
                "kernel32!VirtualAlloc",
                vec![
                    addr,
                    len,
                    AsmOperand::Constant(AsmConstant::Int(MEM_COMMIT_RESERVE, AsmType::I64)),
                    AsmOperand::Constant(AsmConstant::Int(protection, AsmType::I64)),
                ],
                CallingConvention::Win64,
            );

            let cmp_reg = alloc_result(function, AsmType::I1);
            let cmp = build_eq(
                cmp_id,
                cmp_reg,
                vreg_read(call_reg),
                AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I64)),
            );

            let select = match dest {
                Some(dest) => build_select(
                    replaces_id,
                    dest,
                    vreg_read(cmp_reg),
                    AsmOperand::Constant(AsmConstant::Int(-1, AsmType::I64)),
                    vreg_read(call_reg),
                ),
                None => {
                    AsmInstruction::new(replaces_id, AsmOpcode::Generic(AsmGenericOpcode::Nop), Vec::new())
                }
            };

            Ok(LoweredWindows::Sequence(vec![call, cmp, select]))
        }
        SystemApiOp::Munmap { addr, len: _ } => {
            const MEM_RELEASE: i64 = 0x8000;
            let call_id = *next_id;
            *next_id = next_id.saturating_add(1);
            let cmp_id = *next_id;
            *next_id = next_id.saturating_add(1);

            let call_reg = alloc_result(function, AsmType::I1);
            let call = build_call_symbol(
                call_id,
                Some(call_reg),
                "kernel32!VirtualFree",
                vec![
                    addr,
                    AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I64)),
                    AsmOperand::Constant(AsmConstant::Int(MEM_RELEASE, AsmType::I64)),
                ],
                CallingConvention::Win64,
            );

            let cmp_reg = alloc_result(function, AsmType::I1);
            let cmp = build_eq(
                cmp_id,
                cmp_reg,
                vreg_read(call_reg),
                AsmOperand::Constant(AsmConstant::Bool(false)),
            );

            let select = match dest {
                Some(dest) => build_select(
                    replaces_id,
                    dest,
                    vreg_read(cmp_reg),
                    AsmOperand::Constant(AsmConstant::Int(-1, AsmType::I64)),
                    AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I64)),
                ),
                None => {
                    AsmInstruction::new(replaces_id, AsmOpcode::Generic(AsmGenericOpcode::Nop), Vec::new())
                }
            };

            Ok(LoweredWindows::Sequence(vec![call, cmp, select]))
        }

        SystemApiOp::Opendir { .. }
        | SystemApiOp::Readdir { .. }
        | SystemApiOp::Closedir { .. } => Err(Error::from(
            "directory SysOps are not supported for Windows targets yet",
        )),
    }
}

fn match_writefile_sequence_to_syscall(
    instructions: &[AsmInstruction],
    convention: AsmSyscallConvention,
) -> Result<Option<(AsmInstruction, usize)>> {
    // Pattern A (stdio):
    //   GetStdHandle; Alloca; WriteFile; Load; [Eq; Select]
    // Pattern B (direct handle):
    //   Alloca; WriteFile; Load; [Eq; Select]
    if instructions.len() < 3 {
        return Ok(None);
    }

    let mut base = 0usize;
    let mut fd_value: Option<AsmOperand> = None;
    let handle_value: AsmOperand;

    if is_call_named(&instructions[0], "kernel32.dll", "GetStdHandle") {
        if instructions.len() < 4 {
            return Ok(None);
        }
        let getstd = &instructions[0];
        let Some((_, args)) = getstd.call_target_and_args() else {
            return Ok(None);
        };
        let Some(handle_code) = args.first().and_then(|value| {
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
            Some(-11) => 1u64,
            Some(-12) => 2u64,
            _ => return Ok(None),
        };
        fd_value = Some(AsmOperand::Constant(AsmConstant::UInt(fd, AsmType::I64)));
        handle_value = vreg_read(result_vreg(getstd).ok_or_else(|| {
            fp_core::error::Error::from("GetStdHandle call has no result register")
        })?);
        base = 1;
    } else {
        // Handle comes directly from the WriteFile call's first arg.
        handle_value = AsmOperand::Constant(AsmConstant::Undef(AsmType::I64));
    }

    let alloca = &instructions[base];
    let writefile = instructions
        .get(base + 1)
        .ok_or_else(|| fp_core::error::Error::from("missing WriteFile instruction in sequence"))?;
    let load = instructions
        .get(base + 2)
        .ok_or_else(|| fp_core::error::Error::from("missing Load instruction in sequence"))?;

    if !matches!(alloca.opcode, AsmOpcode::Generic(AsmGenericOpcode::Alloca)) {
        return Ok(None);
    }
    if !is_call_named(writefile, "kernel32.dll", "WriteFile") {
        return Ok(None);
    }
    if !matches!(load.opcode, AsmOpcode::Generic(AsmGenericOpcode::Load)) {
        return Ok(None);
    }
    // Load operand schema: [dest, address].
    let Some(address) = load.operands.get(1) else {
        return Ok(None);
    };
    if !operand_is_result_of(address, alloca) {
        return Ok(None);
    }

    let Some((_, args)) = writefile.call_target_and_args() else {
        return Ok(None);
    };
    if args.len() < 5 {
        return Ok(None);
    }
    if !operand_is_result_of(&args[3], alloca) {
        return Ok(None);
    }
    let handle_arg = if base == 1 {
        if args[0] != handle_value {
            return Ok(None);
        }
        handle_value
    } else {
        args[0].clone()
    };

    let fd = fd_value.unwrap_or(handle_arg);
    let op = SystemApiOp::Write {
        fd,
        buffer: args[1].clone(),
        len: args[2].clone(),
    };

    let load_index = base + 2;
    let (dest_inst, consumed_tail) = match_result_chain_at(instructions, load_index, load);
    let dest = result_vreg(dest_inst);
    let replacement = lower_system_api_to_syscall(dest_inst.id, dest, op, convention);

    Ok(Some((replacement, consumed_tail)))
}

fn match_readfile_sequence_to_syscall(
    instructions: &[AsmInstruction],
    convention: AsmSyscallConvention,
) -> Result<Option<(AsmInstruction, usize)>> {
    // Pattern A (stdio):
    //   GetStdHandle; Alloca; ReadFile; Load; [Eq; Select]
    // Pattern B (direct handle):
    //   Alloca; ReadFile; Load; [Eq; Select]
    if instructions.len() < 3 {
        return Ok(None);
    }

    let mut base = 0usize;
    let mut fd_value: Option<AsmOperand> = None;
    let handle_value: AsmOperand;

    if is_call_named(&instructions[0], "kernel32.dll", "GetStdHandle") {
        if instructions.len() < 4 {
            return Ok(None);
        }
        let getstd = &instructions[0];
        let Some((_, args)) = getstd.call_target_and_args() else {
            return Ok(None);
        };
        let Some(handle_code) = args.first().and_then(|value| {
            resolve_i64(value, instructions)
                .map_err(|e| {
                    eprintln!("[fp-native] Win32-to-POSIX arg resolution error: {e}");
                    e
                })
                .ok()
        }) else {
            return Ok(None);
        };
        match handle_code {
            Some(-10) => {}
            _ => return Ok(None),
        }
        fd_value = Some(AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I64)));
        handle_value = vreg_read(result_vreg(getstd).ok_or_else(|| {
            fp_core::error::Error::from("GetStdHandle call has no result register")
        })?);
        base = 1;
    } else {
        handle_value = AsmOperand::Constant(AsmConstant::Undef(AsmType::I64));
    }

    let alloca = &instructions[base];
    let readfile = instructions
        .get(base + 1)
        .ok_or_else(|| fp_core::error::Error::from("missing ReadFile instruction in sequence"))?;
    let load = instructions
        .get(base + 2)
        .ok_or_else(|| fp_core::error::Error::from("missing Load instruction in sequence"))?;

    if !matches!(alloca.opcode, AsmOpcode::Generic(AsmGenericOpcode::Alloca)) {
        return Ok(None);
    }
    if !is_call_named(readfile, "kernel32.dll", "ReadFile") {
        return Ok(None);
    }
    if !matches!(load.opcode, AsmOpcode::Generic(AsmGenericOpcode::Load)) {
        return Ok(None);
    }
    let Some(address) = load.operands.get(1) else {
        return Ok(None);
    };
    if !operand_is_result_of(address, alloca) {
        return Ok(None);
    }

    let Some((_, args)) = readfile.call_target_and_args() else {
        return Ok(None);
    };
    if args.len() < 5 {
        return Ok(None);
    }
    if !operand_is_result_of(&args[3], alloca) {
        return Ok(None);
    }
    let handle_arg = if base == 1 {
        if args[0] != handle_value {
            return Ok(None);
        }
        handle_value
    } else {
        args[0].clone()
    };

    let fd = fd_value.unwrap_or(handle_arg);
    let op = SystemApiOp::Read {
        fd,
        buffer: args[1].clone(),
        len: args[2].clone(),
    };

    let load_index = base + 2;
    let (dest_inst, consumed_tail) = match_result_chain_at(instructions, load_index, load);
    let dest = result_vreg(dest_inst);
    let replacement = lower_system_api_to_syscall(dest_inst.id, dest, op, convention);

    Ok(Some((replacement, consumed_tail)))
}

fn match_setfilepointerex_sequence_to_syscall(
    instructions: &[AsmInstruction],
    convention: AsmSyscallConvention,
) -> Result<Option<(AsmInstruction, usize)>> {
    // Pattern A (stdio):
    //   GetStdHandle; Alloca; SetFilePointerEx; Load; [Eq; Select]
    // Pattern B (direct handle):
    //   Alloca; SetFilePointerEx; Load; [Eq; Select]
    if instructions.len() < 3 {
        return Ok(None);
    }

    let mut base = 0usize;
    let mut fd_value: Option<AsmOperand> = None;
    let handle_value: AsmOperand;

    if is_call_named(&instructions[0], "kernel32.dll", "GetStdHandle") {
        if instructions.len() < 4 {
            return Ok(None);
        }
        let getstd = &instructions[0];
        let Some((_, args)) = getstd.call_target_and_args() else {
            return Ok(None);
        };
        let Some(handle_code) = args.first().and_then(|value| {
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
        fd_value = Some(AsmOperand::Constant(AsmConstant::UInt(fd, AsmType::I64)));
        handle_value = vreg_read(result_vreg(getstd).ok_or_else(|| {
            fp_core::error::Error::from("GetStdHandle call has no result register")
        })?);
        base = 1;
    } else {
        handle_value = AsmOperand::Constant(AsmConstant::Undef(AsmType::I64));
    }

    let alloca = &instructions[base];
    let setfp = instructions.get(base + 1).ok_or_else(|| {
        fp_core::error::Error::from("missing SetFilePointerEx instruction in sequence")
    })?;
    let load = instructions
        .get(base + 2)
        .ok_or_else(|| fp_core::error::Error::from("missing Load instruction in sequence"))?;

    if !matches!(alloca.opcode, AsmOpcode::Generic(AsmGenericOpcode::Alloca)) {
        return Ok(None);
    }
    if !is_call_named(setfp, "kernel32.dll", "SetFilePointerEx") {
        return Ok(None);
    }
    if !matches!(load.opcode, AsmOpcode::Generic(AsmGenericOpcode::Load)) {
        return Ok(None);
    }
    let Some(address) = load.operands.get(1) else {
        return Ok(None);
    };
    if !operand_is_result_of(address, alloca) {
        return Ok(None);
    }

    let Some((_, args)) = setfp.call_target_and_args() else {
        return Ok(None);
    };
    if args.len() != 4 {
        return Ok(None);
    }
    if !operand_is_result_of(&args[2], alloca) {
        return Ok(None);
    }
    let handle_arg = if base == 1 {
        if args[0] != handle_value {
            return Ok(None);
        }
        handle_value
    } else {
        args[0].clone()
    };

    let fd = fd_value.unwrap_or(handle_arg);
    let op = SystemApiOp::Seek {
        fd,
        offset: args[1].clone(),
        whence: args[3].clone(),
    };

    let load_index = base + 2;
    let (dest_inst, consumed_tail) = match_result_chain_at(instructions, load_index, load);
    let dest = result_vreg(dest_inst);
    let replacement = lower_system_api_to_syscall(dest_inst.id, dest, op, convention);

    Ok(Some((replacement, consumed_tail)))
}

fn match_virtualalloc_sequence_to_syscall(
    instructions: &[AsmInstruction],
    convention: AsmSyscallConvention,
) -> Result<Option<(AsmInstruction, usize)>> {
    // Pattern:
    //   VirtualAlloc; Eq; Select
    if instructions.len() < 3 {
        return Ok(None);
    }
    let call = &instructions[0];
    let eq = &instructions[1];
    let select = &instructions[2];

    if !is_call_named(call, "kernel32.dll", "VirtualAlloc") {
        return Ok(None);
    }
    if !matches!(eq.opcode, AsmOpcode::Generic(AsmGenericOpcode::Eq)) {
        return Ok(None);
    }
    if !matches!(select.opcode, AsmOpcode::Generic(AsmGenericOpcode::Select)) {
        return Ok(None);
    }
    // Select operand schema: [dest, condition, if_true, if_false].
    let Some(if_true) = select.operands.get(2) else {
        return Ok(None);
    };
    let Some(if_false) = select.operands.get(3) else {
        return Ok(None);
    };
    if !operand_is_result_of(if_false, call) {
        return Ok(None);
    }
    if if_true != &AsmOperand::Constant(AsmConstant::Int(-1, AsmType::I64)) {
        return Ok(None);
    }

    let Some((_, args)) = call.call_target_and_args() else {
        return Ok(None);
    };
    if args.len() != 4 {
        return Ok(None);
    }

    let style = match convention {
        AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
            PosixFlagStyle::Darwin
        }
        _ => PosixFlagStyle::Linux,
    };

    let page_prot = resolve_i64(&args[3], instructions)
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

    let op = SystemApiOp::Mmap {
        addr: args[0].clone(),
        len: args[1].clone(),
        prot: AsmOperand::Constant(AsmConstant::Int(prot, AsmType::I64)),
        flags: AsmOperand::Constant(AsmConstant::Int(
            posix_mmap_flags_anonymous_private(style),
            AsmType::I64,
        )),
        fd: AsmOperand::Constant(AsmConstant::Int(-1, AsmType::I64)),
        offset: AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I64)),
    };

    let dest = result_vreg(select);
    let replacement = lower_system_api_to_syscall(select.id, dest, op, convention);
    Ok(Some((replacement, 3)))
}

fn match_virtualfree_sequence_to_syscall(
    instructions: &[AsmInstruction],
    convention: AsmSyscallConvention,
) -> Result<Option<(AsmInstruction, usize)>> {
    // Pattern:
    //   VirtualFree; Eq; Select
    if instructions.len() < 3 {
        return Ok(None);
    }
    let call = &instructions[0];
    let eq = &instructions[1];
    let select = &instructions[2];

    if !is_call_named(call, "kernel32.dll", "VirtualFree") {
        return Ok(None);
    }
    if !matches!(eq.opcode, AsmOpcode::Generic(AsmGenericOpcode::Eq)) {
        return Ok(None);
    }
    if !matches!(select.opcode, AsmOpcode::Generic(AsmGenericOpcode::Select)) {
        return Ok(None);
    }
    if select.operands.get(2) != Some(&AsmOperand::Constant(AsmConstant::Int(-1, AsmType::I64))) {
        return Ok(None);
    }
    if select.operands.get(3) != Some(&AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I64))) {
        return Ok(None);
    }

    let Some((_, args)) = call.call_target_and_args() else {
        return Ok(None);
    };
    if args.len() != 3 {
        return Ok(None);
    }

    let op = SystemApiOp::Munmap {
        addr: args[0].clone(),
        len: args[1].clone(),
    };

    let dest = result_vreg(select);
    let replacement = lower_system_api_to_syscall(select.id, dest, op, convention);
    Ok(Some((replacement, 3)))
}

fn match_kernel32_bool_call_sequence_to_syscall(
    instructions: &[AsmInstruction],
    proc_name: &str,
    op: SystemApiOp,
    convention: AsmSyscallConvention,
) -> Result<Option<(AsmInstruction, usize)>> {
    // Pattern:
    //   <proc>; Eq; Select
    if instructions.len() < 3 {
        return Ok(None);
    }
    let call = &instructions[0];
    let eq = &instructions[1];
    let select = &instructions[2];

    if !is_call_named(call, "kernel32.dll", proc_name) {
        return Ok(None);
    }
    if !matches!(eq.opcode, AsmOpcode::Generic(AsmGenericOpcode::Eq)) {
        return Ok(None);
    }
    if !matches!(select.opcode, AsmOpcode::Generic(AsmGenericOpcode::Select)) {
        return Ok(None);
    }
    let Some(if_true) = select.operands.get(2) else {
        return Ok(None);
    };
    let Some(if_false) = select.operands.get(3) else {
        return Ok(None);
    };
    if if_true != &AsmOperand::Constant(AsmConstant::Int(-1, AsmType::I64)) {
        return Ok(None);
    }
    if if_false != &AsmOperand::Constant(AsmConstant::Int(0, AsmType::I64))
        && if_false != &AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I64))
    {
        return Ok(None);
    }

    let dest = result_vreg(select);
    let replacement = lower_system_api_to_syscall(select.id, dest, op, convention);
    Ok(Some((replacement, 3)))
}

fn match_deletefile_sequence_to_syscall(
    instructions: &[AsmInstruction],
    convention: AsmSyscallConvention,
) -> Result<Option<(AsmInstruction, usize)>> {
    let call = instructions.first();
    let Some(call) = call else {
        return Ok(None);
    };
    let Some((_, args)) = call.call_target_and_args() else {
        return Ok(None);
    };
    if args.len() != 1 {
        return Ok(None);
    }
    match_kernel32_bool_call_sequence_to_syscall(
        instructions,
        "DeleteFileA",
        SystemApiOp::Unlink {
            path: args[0].clone(),
        },
        convention,
    )
}

fn match_createdirectory_sequence_to_syscall(
    instructions: &[AsmInstruction],
    convention: AsmSyscallConvention,
) -> Result<Option<(AsmInstruction, usize)>> {
    let call = instructions.first();
    let Some(call) = call else {
        return Ok(None);
    };
    let Some((_, args)) = call.call_target_and_args() else {
        return Ok(None);
    };
    if args.len() != 2 {
        return Ok(None);
    }
    match_kernel32_bool_call_sequence_to_syscall(
        instructions,
        "CreateDirectoryA",
        SystemApiOp::Mkdir {
            path: args[0].clone(),
            mode: AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I32)),
        },
        convention,
    )
}

fn match_removedirectory_sequence_to_syscall(
    instructions: &[AsmInstruction],
    convention: AsmSyscallConvention,
) -> Result<Option<(AsmInstruction, usize)>> {
    let call = instructions.first();
    let Some(call) = call else {
        return Ok(None);
    };
    let Some((_, args)) = call.call_target_and_args() else {
        return Ok(None);
    };
    if args.len() != 1 {
        return Ok(None);
    }
    match_kernel32_bool_call_sequence_to_syscall(
        instructions,
        "RemoveDirectoryA",
        SystemApiOp::Rmdir {
            path: args[0].clone(),
        },
        convention,
    )
}

fn match_movefileex_sequence_to_syscall(
    instructions: &[AsmInstruction],
    convention: AsmSyscallConvention,
) -> Result<Option<(AsmInstruction, usize)>> {
    let call = instructions.first();
    let Some(call) = call else {
        return Ok(None);
    };
    let Some((_, args)) = call.call_target_and_args() else {
        return Ok(None);
    };
    if args.len() != 3 {
        return Ok(None);
    }
    match_kernel32_bool_call_sequence_to_syscall(
        instructions,
        "MoveFileExA",
        SystemApiOp::Rename {
            from: args[0].clone(),
            to: args[1].clone(),
        },
        convention,
    )
}

/// Finds the instruction that carries the final result of a
/// `...; Load; [Eq; Select]` idiom: the `Load` itself, unless it is
/// immediately followed by an `Eq`/`Select` pair where the `Select`'s
/// `if_false` operand is exactly a reference to the `Load`'s result (in
/// which case the `Select` is the true final result).
fn match_result_chain_at<'a>(
    instructions: &'a [AsmInstruction],
    load_index: usize,
    load: &'a AsmInstruction,
) -> (&'a AsmInstruction, usize) {
    // Accept both:
    //   ...; Load
    //   ...; Load; Eq; Select  (Select.if_false == Load)
    if instructions.len() >= load_index + 3 {
        let eq = &instructions[load_index + 1];
        let select = &instructions[load_index + 2];
        if matches!(eq.opcode, AsmOpcode::Generic(AsmGenericOpcode::Eq))
            && matches!(select.opcode, AsmOpcode::Generic(AsmGenericOpcode::Select))
        {
            if let Some(if_false) = select.operands.get(3) {
                if operand_is_result_of(if_false, load) {
                    return (select, load_index + 3);
                }
            }
        }
    }
    (load, load_index + 1)
}

fn is_call_named(inst: &AsmInstruction, dll: &str, name: &str) -> bool {
    let Some((target, _)) = inst.call_target_and_args() else {
        return false;
    };
    let AsmOperand::Symbol(symbol) = target else {
        return false;
    };
    let (sym_dll, sym_name) = split_import_symbol(symbol.as_str());
    import_dll_matches(&sym_dll, dll) && sym_name == name
}

fn import_dll_matches(actual: &str, expected: &str) -> bool {
    if actual.eq_ignore_ascii_case(expected) {
        return true;
    }

    matches!(
        (
            actual.to_ascii_lowercase().as_str(),
            expected.to_ascii_lowercase().as_str(),
        ),
        ("kernelbase.dll", "kernel32.dll") | ("kernel32.dll", "kernelbase.dll")
    )
}

fn lower_system_api_to_syscall(
    id: u32,
    dest: Option<AsmVirtualRegId>,
    op: SystemApiOp,
    convention: AsmSyscallConvention,
) -> AsmInstruction {
    match op {
        SystemApiOp::Exit { code } => {
            let number = match convention {
                AsmSyscallConvention::LinuxX86_64 => 60,
                AsmSyscallConvention::LinuxAarch64 => 93,
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    0x2000_0001
                }
            };
            build_syscall(
                id,
                dest,
                convention,
                AsmOperand::Constant(AsmConstant::UInt(number, AsmType::I64)),
                vec![code],
            )
        }
        SystemApiOp::GetPid => {
            let number = match convention {
                AsmSyscallConvention::LinuxX86_64 => 39,
                AsmSyscallConvention::LinuxAarch64 => 172,
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    0x2000_0014
                }
            };
            build_syscall(
                id,
                dest,
                convention,
                AsmOperand::Constant(AsmConstant::UInt(number, AsmType::I64)),
                Vec::new(),
            )
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
            build_syscall(
                id,
                dest,
                convention,
                AsmOperand::Constant(AsmConstant::UInt(number, AsmType::I64)),
                Vec::new(),
            )
        }
        SystemApiOp::Write { fd, buffer, len } => {
            let number = match convention {
                AsmSyscallConvention::LinuxX86_64 => 1,
                AsmSyscallConvention::LinuxAarch64 => 64,
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    0x2000_0004
                }
            };
            build_syscall(
                id,
                dest,
                convention,
                AsmOperand::Constant(AsmConstant::UInt(number, AsmType::I64)),
                vec![fd, buffer, len],
            )
        }
        SystemApiOp::Dlopen { .. } | SystemApiOp::Dlsym { .. } | SystemApiOp::Dlclose { .. } => {
            // Not actually supported over raw syscalls (dlopen/dlsym/dlclose
            // are libc-level, not syscalls); `lower_system_api_to_unix`
            // handles these directly as libc calls instead. This arm only
            // exists so callers with just a `SystemApiOp` (e.g. the generic
            // `_` fallback in `lower_sys_ops_to_unix_syscalls`) don't panic;
            // preserve the register (if any) as frozen-undef.
            match dest {
                Some(dest) => build_unary(
                    id,
                    AsmGenericOpcode::Freeze,
                    dest,
                    AsmOperand::Constant(AsmConstant::Undef(AsmType::I64)),
                ),
                None => AsmInstruction::new(id, AsmOpcode::Generic(AsmGenericOpcode::Nop), Vec::new()),
            }
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
                        AsmOperand::Constant(AsmConstant::Int(-100, AsmType::I64)),
                        path,
                        AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I64)),
                    ],
                ),
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    (0x2000_000a, vec![path])
                }
            };
            build_syscall(
                id,
                dest,
                convention,
                AsmOperand::Constant(AsmConstant::UInt(number, AsmType::I64)),
                args,
            )
        }
        SystemApiOp::Mkdir { path, mode } => {
            let (number, args) = match convention {
                AsmSyscallConvention::LinuxX86_64 => (83, vec![path, mode]),
                AsmSyscallConvention::LinuxAarch64 => (
                    34,
                    vec![
                        AsmOperand::Constant(AsmConstant::Int(-100, AsmType::I64)),
                        path,
                        mode,
                    ],
                ),
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    (0x2000_0088, vec![path, mode])
                }
            };
            build_syscall(
                id,
                dest,
                convention,
                AsmOperand::Constant(AsmConstant::UInt(number, AsmType::I64)),
                args,
            )
        }
        SystemApiOp::Rmdir { path } => {
            let (number, args) = match convention {
                AsmSyscallConvention::LinuxX86_64 => (84, vec![path]),
                AsmSyscallConvention::LinuxAarch64 => (
                    35,
                    vec![
                        AsmOperand::Constant(AsmConstant::Int(-100, AsmType::I64)),
                        path,
                        AsmOperand::Constant(AsmConstant::Int(0x200, AsmType::I64)),
                    ],
                ),
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    (0x2000_0089, vec![path])
                }
            };
            build_syscall(
                id,
                dest,
                convention,
                AsmOperand::Constant(AsmConstant::UInt(number, AsmType::I64)),
                args,
            )
        }
        SystemApiOp::Rename { from, to } => {
            let (number, args) = match convention {
                AsmSyscallConvention::LinuxX86_64 => (82, vec![from, to]),
                AsmSyscallConvention::LinuxAarch64 => (
                    38,
                    vec![
                        AsmOperand::Constant(AsmConstant::Int(-100, AsmType::I64)),
                        from,
                        AsmOperand::Constant(AsmConstant::Int(-100, AsmType::I64)),
                        to,
                    ],
                ),
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    (0x2000_0080, vec![from, to])
                }
            };
            build_syscall(
                id,
                dest,
                convention,
                AsmOperand::Constant(AsmConstant::UInt(number, AsmType::I64)),
                args,
            )
        }
        SystemApiOp::Access { path, mode } => {
            let (number, args) = match convention {
                AsmSyscallConvention::LinuxX86_64 => (21, vec![path, mode]),
                AsmSyscallConvention::LinuxAarch64 => (
                    48,
                    vec![
                        AsmOperand::Constant(AsmConstant::Int(-100, AsmType::I64)),
                        path,
                        mode,
                        AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I64)),
                    ],
                ),
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    (0x2000_0021, vec![path, mode])
                }
            };
            build_syscall(
                id,
                dest,
                convention,
                AsmOperand::Constant(AsmConstant::UInt(number, AsmType::I64)),
                args,
            )
        }
        SystemApiOp::Read { fd, buffer, len } => {
            let number = match convention {
                AsmSyscallConvention::LinuxX86_64 => 0,
                AsmSyscallConvention::LinuxAarch64 => 63,
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    0x2000_0003
                }
            };
            build_syscall(
                id,
                dest,
                convention,
                AsmOperand::Constant(AsmConstant::UInt(number, AsmType::I64)),
                vec![fd, buffer, len],
            )
        }
        SystemApiOp::Close { fd } => {
            let number = match convention {
                AsmSyscallConvention::LinuxX86_64 => 3,
                AsmSyscallConvention::LinuxAarch64 => 57,
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    0x2000_0006
                }
            };
            build_syscall(
                id,
                dest,
                convention,
                AsmOperand::Constant(AsmConstant::UInt(number, AsmType::I64)),
                vec![fd],
            )
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
                    AsmOperand::Constant(AsmConstant::Int(-100, AsmType::I64)),
                    path,
                    flags,
                    mode,
                ],
                _ => vec![path, flags, mode],
            };
            build_syscall(
                id,
                dest,
                convention,
                AsmOperand::Constant(AsmConstant::UInt(number, AsmType::I64)),
                args,
            )
        }
        SystemApiOp::Seek { fd, offset, whence } => {
            let number = match convention {
                AsmSyscallConvention::LinuxX86_64 => 8,
                AsmSyscallConvention::LinuxAarch64 => 62,
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    0x2000_00c7
                }
            };
            build_syscall(
                id,
                dest,
                convention,
                AsmOperand::Constant(AsmConstant::UInt(number, AsmType::I64)),
                vec![fd, offset, whence],
            )
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
            build_syscall(
                id,
                dest,
                convention,
                AsmOperand::Constant(AsmConstant::UInt(number, AsmType::I64)),
                vec![addr, len, prot, flags, fd, offset],
            )
        }
        SystemApiOp::Munmap { addr, len } => {
            let number = match convention {
                AsmSyscallConvention::LinuxX86_64 => 11,
                AsmSyscallConvention::LinuxAarch64 => 215,
                AsmSyscallConvention::DarwinX86_64 | AsmSyscallConvention::DarwinAarch64 => {
                    0x2000_0049
                }
            };
            build_syscall(
                id,
                dest,
                convention,
                AsmOperand::Constant(AsmConstant::UInt(number, AsmType::I64)),
                vec![addr, len],
            )
        }
    }
}

/// Finds the instruction in `instructions` whose result is virtual register
/// `id`, if it is a `Freeze`, and returns its source operand. Registers are
/// no longer numbered the same as instruction ids, so this must search by
/// `result_register()` rather than by `inst.id`.
fn freeze_source_for_register<'a>(
    id: AsmVirtualRegId,
    instructions: &'a [AsmInstruction],
) -> Option<&'a AsmOperand> {
    let inst = instructions.iter().find(|inst| {
        matches!(
            inst.result_register(),
            Some(AsmRegister::Virtual(rid)) if *rid == id
        )
    })?;
    if !matches!(inst.opcode, AsmOpcode::Generic(AsmGenericOpcode::Freeze)) {
        return None;
    }
    // Freeze operand schema: [Write dest, Read src].
    inst.operands.get(1)
}

fn resolve_u64(value: &AsmOperand, instructions: &[AsmInstruction]) -> Option<u64> {
    match value {
        AsmOperand::Constant(AsmConstant::UInt(x, _)) => Some(*x),
        AsmOperand::Constant(AsmConstant::Int(x, _)) => (*x)
            .try_into()
            .map_err(|e| {
                eprintln!("[fp-native] Win32-to-POSIX value conversion error: {e}");
                e
            })
            .ok(),
        AsmOperand::Register {
            reg: AsmRegister::Virtual(id),
            ..
        } => {
            let inner = freeze_source_for_register(*id, instructions)?;
            resolve_u64(inner, instructions)
        }
        _ => None,
    }
}

fn resolve_i64(value: &AsmOperand, instructions: &[AsmInstruction]) -> Result<Option<i64>> {
    Ok(match value {
        AsmOperand::Constant(AsmConstant::Int(x, _)) => Some(*x),
        AsmOperand::Constant(AsmConstant::UInt(x, _)) => i64::try_from(*x)
            .map_err(|e| {
                eprintln!("[fp-native] Win32-to-POSIX value conversion error: {e}");
                e
            })
            .ok(),
        AsmOperand::Register {
            reg: AsmRegister::Virtual(id),
            ..
        } => {
            let Some(inner) = freeze_source_for_register(*id, instructions) else {
                return Ok(None);
            };
            resolve_i64(inner, instructions)?
        }
        _ => None,
    })
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

#[cfg(test)]
mod tests {
    use super::*;
    use fp_core::asmir::{AsmArchitecture, AsmEndianness, AsmTarget};
    use fp_core::container::{
        ContainerArchitecture, ContainerEndianness, ContainerFile, ContainerKind,
    };

    fn program(target_format: AsmObjectFormat) -> AsmProgram {
        let target = AsmTarget {
            architecture: AsmArchitecture::X86_64,
            object_format: target_format,
            endianness: AsmEndianness::Little,
            pointer_width: 64,
            default_calling_convention: None,
        };
        AsmProgram::new(target.clone(), target.data_layout())
    }

    /// Builds a `main` function whose sole instruction is a call to `target`
    /// returning `ret_ty`, terminated by returning that call's result.
    fn single_call_function(
        target: &str,
        args: Vec<AsmOperand>,
        cc: CallingConvention,
        ret_ty: AsmType,
    ) -> AsmFunction {
        let mut f = AsmFunction::new(
            Name::new("main"),
            AsmFunctionSignature {
                params: Vec::new(),
                return_type: AsmType::Void,
                is_variadic: false,
            },
        );
        let result = alloc_result(&mut f, ret_ty);
        let inst = build_call_symbol(0, Some(result), target, args, cc);
        f.basic_blocks = vec![AsmBlock {
            id: 0,
            label: None,
            instructions: vec![inst],
            terminator: AsmTerminator::Return(Some(vreg_read(result))),
            terminator_encoding: None,
            predecessors: Vec::new(),
            successors: Vec::new(),
        }];
        f
    }

    /// Builds a `main` function whose sole instruction is a raw syscall,
    /// terminated by returning that syscall's result.
    fn single_syscall_function(
        convention: AsmSyscallConvention,
        number: u64,
        args: Vec<AsmOperand>,
    ) -> AsmFunction {
        let mut f = AsmFunction::new(
            Name::new("main"),
            AsmFunctionSignature {
                params: Vec::new(),
                return_type: AsmType::Void,
                is_variadic: false,
            },
        );
        let result = alloc_result(&mut f, AsmType::I64);
        let inst = build_syscall(
            0,
            Some(result),
            convention,
            AsmOperand::Constant(AsmConstant::UInt(number, AsmType::I64)),
            args,
        );
        f.basic_blocks = vec![AsmBlock {
            id: 0,
            label: None,
            instructions: vec![inst],
            terminator: AsmTerminator::Return(Some(vreg_read(result))),
            terminator_encoding: None,
            predecessors: Vec::new(),
            successors: Vec::new(),
        }];
        f
    }

    fn assert_has_syscall_number(block: &fp_core::asmir::AsmBlock, number: u64) {
        assert!(
            block.instructions.iter().any(|inst| {
                syscall_parts(inst)
                    .map(|(_, num, _)| {
                        matches!(num, AsmOperand::Constant(AsmConstant::UInt(n, _)) if *n == number)
                    })
                    .unwrap_or(false)
            }),
            "expected a syscall with number {number} in {:#?}",
            block.instructions
        );
    }

    #[test]
    fn rewrite_linux_readdir_call_to_darwin_shim() {
        let mut prog = program(AsmObjectFormat::MachO);
        prog.container = Some(ContainerFile::new(
            ContainerKind::Object,
            AsmObjectFormat::Elf,
            ContainerArchitecture::X86_64,
            ContainerEndianness::Little,
        ));

        let ptr_i8 = AsmType::Ptr(Box::new(AsmType::I8));
        let mut f = AsmFunction::new(
            Name::new("main"),
            AsmFunctionSignature {
                params: Vec::new(),
                return_type: ptr_i8.clone(),
                is_variadic: false,
            },
        );
        let opendir_result = alloc_result(&mut f, ptr_i8.clone());
        let readdir_result = alloc_result(&mut f, ptr_i8.clone());
        f.basic_blocks = vec![AsmBlock {
            id: 0,
            label: None,
            instructions: vec![
                build_call_symbol(
                    0,
                    Some(opendir_result),
                    "opendir",
                    vec![AsmOperand::Constant(AsmConstant::Null(ptr_i8.clone()))],
                    CallingConvention::C,
                ),
                build_call_symbol(
                    1,
                    Some(readdir_result),
                    "readdir",
                    vec![vreg_read(opendir_result)],
                    CallingConvention::C,
                ),
            ],
            terminator: AsmTerminator::Return(Some(vreg_read(readdir_result))),
            terminator_encoding: None,
            predecessors: Vec::new(),
            successors: Vec::new(),
        }];
        prog.functions.push(f);

        rewrite_program_for_target(&mut prog).unwrap();
        assert!(
            prog.functions
                .iter()
                .any(|f| f.name.as_str() == "fp_linux_readdir"),
            "expected fp_linux_readdir shim to be injected"
        );

        let block = &prog
            .functions
            .iter()
            .find(|f| f.name.as_str() == "main")
            .unwrap()
            .basic_blocks[0];
        assert!(block.instructions.iter().any(|inst| {
            inst.call_target_and_args()
                .map(|(target, _)| {
                    matches!(target, AsmOperand::Symbol(name) if name.as_str() == "fp_linux_readdir")
                })
                .unwrap_or(false)
        }));
    }

    #[test]
    fn rewrite_linux_write_syscall_to_windows_writefile_sequence() {
        let mut prog = program(AsmObjectFormat::Coff);
        prog.functions.push(single_syscall_function(
            AsmSyscallConvention::LinuxX86_64,
            1,
            vec![
                AsmOperand::Constant(AsmConstant::UInt(1, AsmType::I64)),
                AsmOperand::Constant(AsmConstant::Null(AsmType::Ptr(Box::new(AsmType::I8)))),
                AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I64)),
            ],
        ));

        rewrite_program_for_target(&mut prog).unwrap();
        let block = &prog.functions[0].basic_blocks[0];
        assert!(block.instructions.iter().any(|inst| is_call_named(
            inst,
            "kernel32.dll",
            "WriteFile"
        )));
        assert!(block.instructions.iter().any(|inst| is_call_named(
            inst,
            "kernel32.dll",
            "GetStdHandle"
        )));
        assert!(matches!(
            block.terminator,
            AsmTerminator::Return(Some(AsmOperand::Register { .. }))
        ));
    }

    #[test]
    fn rewrite_windows_writefile_sequence_back_to_linux_syscall() {
        let mut prog = program(AsmObjectFormat::Elf);
        let ptr_i8 = AsmType::Ptr(Box::new(AsmType::I8));
        let mut f = AsmFunction::new(
            Name::new("main"),
            AsmFunctionSignature {
                params: Vec::new(),
                return_type: AsmType::Void,
                is_variadic: false,
            },
        );
        let getstd_reg = alloc_result(&mut f, ptr_i8.clone());
        let alloca_reg = alloc_result(&mut f, AsmType::Ptr(Box::new(AsmType::I64)));
        let writefile_reg = alloc_result(&mut f, AsmType::I1);
        let load_reg = alloc_result(&mut f, AsmType::I64);
        f.basic_blocks = vec![AsmBlock {
            id: 0,
            label: None,
            instructions: vec![
                // GetStdHandle(-11)
                build_call_symbol(
                    1,
                    Some(getstd_reg),
                    "kernel32!GetStdHandle",
                    vec![AsmOperand::Constant(AsmConstant::Int(-11, AsmType::I64))],
                    CallingConvention::Win64,
                ),
                // alloca written
                build_alloca(
                    2,
                    alloca_reg,
                    AsmOperand::Constant(AsmConstant::UInt(1, AsmType::I64)),
                    8,
                ),
                // WriteFile(handle, null, 0, ptr, null)
                build_call_symbol(
                    3,
                    Some(writefile_reg),
                    "kernel32!WriteFile",
                    vec![
                        vreg_read(getstd_reg),
                        AsmOperand::Constant(AsmConstant::Null(ptr_i8.clone())),
                        AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I64)),
                        vreg_read(alloca_reg),
                        AsmOperand::Constant(AsmConstant::Null(ptr_i8.clone())),
                    ],
                    CallingConvention::Win64,
                ),
                // load written
                build_load(4, load_reg, vreg_read(alloca_reg)),
            ],
            terminator: AsmTerminator::Return(Some(vreg_read(load_reg))),
            terminator_encoding: None,
            predecessors: Vec::new(),
            successors: Vec::new(),
        }];
        prog.functions.push(f);

        rewrite_program_for_target(&mut prog).unwrap();
        let block = &prog.functions[0].basic_blocks[0];
        assert_has_syscall_number(block, 1);
    }

    #[test]
    fn rewrite_windows_kernelbase_writefile_sequence_back_to_linux_syscall() {
        let mut prog = program(AsmObjectFormat::Elf);
        let ptr_i8 = AsmType::Ptr(Box::new(AsmType::I8));
        let mut f = AsmFunction::new(
            Name::new("main"),
            AsmFunctionSignature {
                params: Vec::new(),
                return_type: AsmType::Void,
                is_variadic: false,
            },
        );
        let getstd_reg = alloc_result(&mut f, ptr_i8.clone());
        let alloca_reg = alloc_result(&mut f, AsmType::Ptr(Box::new(AsmType::I64)));
        let writefile_reg = alloc_result(&mut f, AsmType::I1);
        let load_reg = alloc_result(&mut f, AsmType::I64);
        f.basic_blocks = vec![AsmBlock {
            id: 0,
            label: None,
            instructions: vec![
                build_call_symbol(
                    1,
                    Some(getstd_reg),
                    "kernelbase!GetStdHandle",
                    vec![AsmOperand::Constant(AsmConstant::Int(-11, AsmType::I64))],
                    CallingConvention::Win64,
                ),
                build_alloca(
                    2,
                    alloca_reg,
                    AsmOperand::Constant(AsmConstant::UInt(1, AsmType::I64)),
                    8,
                ),
                build_call_symbol(
                    3,
                    Some(writefile_reg),
                    "kernelbase!WriteFile",
                    vec![
                        vreg_read(getstd_reg),
                        AsmOperand::Constant(AsmConstant::Null(ptr_i8.clone())),
                        AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I64)),
                        vreg_read(alloca_reg),
                        AsmOperand::Constant(AsmConstant::Null(ptr_i8.clone())),
                    ],
                    CallingConvention::Win64,
                ),
                build_load(4, load_reg, vreg_read(alloca_reg)),
            ],
            terminator: AsmTerminator::Return(Some(vreg_read(load_reg))),
            terminator_encoding: None,
            predecessors: Vec::new(),
            successors: Vec::new(),
        }];
        prog.functions.push(f);

        rewrite_program_for_target(&mut prog).unwrap();
        let block = &prog.functions[0].basic_blocks[0];
        assert_has_syscall_number(block, 1);
    }

    #[test]
    fn rewrite_ntdll_writefile_import_to_linux_syscall() {
        let mut prog = program(AsmObjectFormat::Elf);
        let ptr_i8 = AsmType::Ptr(Box::new(AsmType::I8));
        prog.functions.push(single_call_function(
            "ntdll!NtWriteFile",
            vec![
                AsmOperand::Constant(AsmConstant::UInt(1, AsmType::I64)),
                AsmOperand::Constant(AsmConstant::Null(ptr_i8.clone())),
                AsmOperand::Constant(AsmConstant::Null(ptr_i8.clone())),
                AsmOperand::Constant(AsmConstant::Null(ptr_i8.clone())),
                AsmOperand::Constant(AsmConstant::Null(ptr_i8.clone())),
                AsmOperand::Constant(AsmConstant::Null(ptr_i8.clone())),
                AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I64)),
                AsmOperand::Constant(AsmConstant::Null(ptr_i8.clone())),
                AsmOperand::Constant(AsmConstant::Null(ptr_i8)),
            ],
            CallingConvention::Win64,
            AsmType::I64,
        ));

        rewrite_program_for_target(&mut prog).unwrap();
        let block = &prog.functions[0].basic_blocks[0];
        assert_has_syscall_number(block, 1);
    }

    #[test]
    fn rewrite_ntdll_close_import_to_linux_syscall() {
        let mut prog = program(AsmObjectFormat::Elf);
        prog.functions.push(single_call_function(
            "ntdll!ZwClose",
            vec![AsmOperand::Constant(AsmConstant::UInt(3, AsmType::I64))],
            CallingConvention::Win64,
            AsmType::I64,
        ));

        rewrite_program_for_target(&mut prog).unwrap();
        let block = &prog.functions[0].basic_blocks[0];
        assert_has_syscall_number(block, 3);
    }

    #[test]
    fn rewrite_kernelbase_createfile_import_to_linux_open_syscall() {
        let mut prog = program(AsmObjectFormat::Elf);
        let ptr_i8 = AsmType::Ptr(Box::new(AsmType::I8));
        prog.functions.push(single_call_function(
            "kernelbase!CreateFileA",
            vec![
                AsmOperand::Constant(AsmConstant::Null(ptr_i8.clone())),
                AsmOperand::Constant(AsmConstant::Int(0x8000_0000u32 as i64, AsmType::I64)),
                AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I64)),
                AsmOperand::Constant(AsmConstant::Null(ptr_i8.clone())),
                AsmOperand::Constant(AsmConstant::Int(3, AsmType::I64)),
                AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I64)),
                AsmOperand::Constant(AsmConstant::Null(ptr_i8)),
            ],
            CallingConvention::Win64,
            AsmType::I64,
        ));

        rewrite_program_for_target(&mut prog).unwrap();
        let block = &prog.functions[0].basic_blocks[0];
        assert_has_syscall_number(block, 2);
    }

    #[test]
    fn rewrite_linux_read_syscall_to_windows_readfile_sequence() {
        let mut prog = program(AsmObjectFormat::Coff);
        prog.functions.push(single_syscall_function(
            AsmSyscallConvention::LinuxX86_64,
            0,
            vec![
                AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I64)),
                AsmOperand::Constant(AsmConstant::Null(AsmType::Ptr(Box::new(AsmType::I8)))),
                AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I64)),
            ],
        ));

        rewrite_program_for_target(&mut prog).unwrap();
        let block = &prog.functions[0].basic_blocks[0];
        assert!(block.instructions.iter().any(|inst| is_call_named(
            inst,
            "kernel32.dll",
            "ReadFile"
        )));
        assert!(block.instructions.iter().any(|inst| is_call_named(
            inst,
            "kernel32.dll",
            "GetStdHandle"
        )));
        assert!(matches!(
            block.terminator,
            AsmTerminator::Return(Some(AsmOperand::Register { .. }))
        ));
    }

    #[test]
    fn rewrite_windows_readfile_sequence_back_to_linux_syscall() {
        let mut prog = program(AsmObjectFormat::Elf);
        let ptr_i8 = AsmType::Ptr(Box::new(AsmType::I8));
        let mut f = AsmFunction::new(
            Name::new("main"),
            AsmFunctionSignature {
                params: Vec::new(),
                return_type: AsmType::Void,
                is_variadic: false,
            },
        );
        let getstd_reg = alloc_result(&mut f, ptr_i8.clone());
        let alloca_reg = alloc_result(&mut f, AsmType::Ptr(Box::new(AsmType::I64)));
        let readfile_reg = alloc_result(&mut f, AsmType::I1);
        let load_reg = alloc_result(&mut f, AsmType::I64);
        f.basic_blocks = vec![AsmBlock {
            id: 0,
            label: None,
            instructions: vec![
                // GetStdHandle(-10)
                build_call_symbol(
                    1,
                    Some(getstd_reg),
                    "kernel32!GetStdHandle",
                    vec![AsmOperand::Constant(AsmConstant::Int(-10, AsmType::I64))],
                    CallingConvention::Win64,
                ),
                // alloca read
                build_alloca(
                    2,
                    alloca_reg,
                    AsmOperand::Constant(AsmConstant::UInt(1, AsmType::I64)),
                    8,
                ),
                // ReadFile(handle, null, 0, ptr, null)
                build_call_symbol(
                    3,
                    Some(readfile_reg),
                    "kernel32!ReadFile",
                    vec![
                        vreg_read(getstd_reg),
                        AsmOperand::Constant(AsmConstant::Null(ptr_i8.clone())),
                        AsmOperand::Constant(AsmConstant::UInt(0, AsmType::I64)),
                        vreg_read(alloca_reg),
                        AsmOperand::Constant(AsmConstant::Null(ptr_i8.clone())),
                    ],
                    CallingConvention::Win64,
                ),
                // load read
                build_load(4, load_reg, vreg_read(alloca_reg)),
            ],
            terminator: AsmTerminator::Return(Some(vreg_read(load_reg))),
            terminator_encoding: None,
            predecessors: Vec::new(),
            successors: Vec::new(),
        }];
        prog.functions.push(f);

        rewrite_program_for_target(&mut prog).unwrap();
        let block = &prog.functions[0].basic_blocks[0];
        assert_has_syscall_number(block, 0);
    }

    #[test]
    fn rewrite_linux_close_syscall_to_windows_closehandle_sequence() {
        let mut prog = program(AsmObjectFormat::Coff);
        prog.functions.push(single_syscall_function(
            AsmSyscallConvention::LinuxX86_64,
            3,
            vec![AsmOperand::Constant(AsmConstant::UInt(1, AsmType::I64))],
        ));

        rewrite_program_for_target(&mut prog).unwrap();
        let block = &prog.functions[0].basic_blocks[0];
        assert!(block.instructions.iter().any(|inst| is_call_named(
            inst,
            "kernel32.dll",
            "CloseHandle"
        )));
        assert!(block.instructions.iter().any(|inst| is_call_named(
            inst,
            "kernel32.dll",
            "GetStdHandle"
        )));
    }

    #[test]
    fn rewrite_windows_closehandle_sequence_back_to_linux_syscall() {
        let mut prog = program(AsmObjectFormat::Elf);
        let mut f = AsmFunction::new(
            Name::new("main"),
            AsmFunctionSignature {
                params: Vec::new(),
                return_type: AsmType::Void,
                is_variadic: false,
            },
        );
        let getstd_reg = alloc_result(&mut f, AsmType::Ptr(Box::new(AsmType::I8)));
        let close_reg = alloc_result(&mut f, AsmType::I1);
        let cmp_reg = alloc_result(&mut f, AsmType::I1);
        let select_reg = alloc_result(&mut f, AsmType::I64);
        f.basic_blocks = vec![AsmBlock {
            id: 0,
            label: None,
            instructions: vec![
                build_call_symbol(
                    1,
                    Some(getstd_reg),
                    "kernel32!GetStdHandle",
                    vec![AsmOperand::Constant(AsmConstant::Int(-11, AsmType::I64))],
                    CallingConvention::Win64,
                ),
                build_call_symbol(
                    2,
                    Some(close_reg),
                    "kernel32!CloseHandle",
                    vec![vreg_read(getstd_reg)],
                    CallingConvention::Win64,
                ),
                build_eq(
                    3,
                    cmp_reg,
                    vreg_read(close_reg),
                    AsmOperand::Constant(AsmConstant::Bool(false)),
                ),
                build_select(
                    4,
                    select_reg,
                    vreg_read(cmp_reg),
                    AsmOperand::Constant(AsmConstant::Int(-1, AsmType::I64)),
                    AsmOperand::Constant(AsmConstant::Int(0, AsmType::I64)),
                ),
            ],
            terminator: AsmTerminator::Return(Some(vreg_read(select_reg))),
            terminator_encoding: None,
            predecessors: Vec::new(),
            successors: Vec::new(),
        }];
        prog.functions.push(f);

        rewrite_program_for_target(&mut prog).unwrap();
        let block = &prog.functions[0].basic_blocks[0];
        assert_has_syscall_number(block, 3);
    }
}
