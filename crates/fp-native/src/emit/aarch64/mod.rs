use fp_core::asmir::{
    AsmBlock, AsmBlockId as BasicBlockId, AsmConstant, AsmFunction, AsmInstructionKind,
    AsmIntrinsicKind, AsmProgram, AsmSyscallConvention, AsmTerminator, AsmType, AsmValue,
};
use fp_core::container::ContainerKind;
use fp_core::error::{Error, Result};
use fp_core::lir::LirDataLayout;
use fp_core::lir::{CallingConvention, LirType};
use std::collections::{BTreeSet, HashMap, HashSet};

use crate::emit::{CodegenOutput, RelocKind, Relocation, TargetFormat};

mod instructions;
use instructions::*;
mod globals;
use globals::*;
mod layout;
mod memory_encoding;
use layout::*;
use memory_encoding::*;
mod intrinsics;
use intrinsics::*;
mod calls;
use calls::*;
mod memory;
use memory::*;
mod addressing;
use addressing::*;
mod control_flow;
use control_flow::*;
mod values;
use values::*;
mod aggregate_values;
use aggregate_values::*;
mod floating_point;
use floating_point::*;
mod integer_ops;
use integer_ops::*;
mod aggregates;
use aggregates::*;
mod preserved;
mod type_utils;
use preserved::*;
use type_utils::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Reg {
    X0,
    X1,
    X2,
    X3,
    X4,
    X5,
    X6,
    X7,
    X8,
    X9,
    X10,
    X11,
    X12,
    X13,
    X14,
    X15,
    X16,
    X17,
    X19,
    X29,
    X30,
    X31,
}

fn annotation_value<'a>(
    annotations: &'a [fp_core::asmir::AsmAnnotation],
    key: &str,
) -> Option<&'a str> {
    annotations
        .iter()
        .find(|annotation| annotation.key == key)
        .map(|annotation| annotation.value.as_str())
}

fn emit_load_symbol_addr_explicit(
    asm: &mut Assembler,
    dst: Reg,
    symbol: &str,
    kind: fp_core::asmir::AsmSymbolAddressKind,
) -> Result<()> {
    match kind {
        fp_core::asmir::AsmSymbolAddressKind::Direct => {
            if asm.target_format == TargetFormat::MachO {
                let offset = asm.buf.len();
                // ADRP dst, symbol@PAGE
                let adrp = 0x9000_0000u32 | dst.id();
                asm.emit_u32(adrp);
                // ADD dst, dst, symbol@PAGEOFF
                let add = 0x9100_0000u32 | (dst.id() << 5) | dst.id();
                asm.emit_u32(add);
                asm.relocs.push(Relocation {
                    offset: offset as u64,
                    kind: RelocKind::Aarch64AdrpAdd,
                    section: crate::emit::RelocSection::Text,
                    symbol: symbol.to_string(),
                    addend: 0,
                });
                return Ok(());
            }
            emit_load_symbol_addr(asm, dst, symbol, 0)
        }
        fp_core::asmir::AsmSymbolAddressKind::Got => {
            if asm.target_format != TargetFormat::MachO {
                return emit_load_symbol_addr(asm, dst, symbol, 0);
            }

            let offset = asm.buf.len();
            // ADRP dst, symbol@GOTPAGE
            let adrp = 0x9000_0000u32 | dst.id();
            asm.emit_u32(adrp);
            // LDR dst, [dst, symbol@GOTPAGEOFF]
            let ldr = 0xF940_0000u32 | (dst.id() << 5) | dst.id();
            asm.emit_u32(ldr);
            asm.relocs.push(Relocation {
                offset: offset as u64,
                kind: RelocKind::Aarch64GotLoad,
                section: crate::emit::RelocSection::Text,
                symbol: symbol.to_string(),
                addend: 0,
            });
            Ok(())
        }
    }
}

fn initialize_lifted_x86_regfile(
    asm: &mut Assembler,
    layout: &FrameLayout,
    func: &AsmFunction,
) -> Result<()> {
    let _ = layout;

    if func.name.as_str() == "fp_lifted_main" {
        const REGFILE_SIZE: u64 = 16 * 8;

        // void *mmap(void *addr, size_t len, int prot, int flags, int fd, off_t offset);
        emit_mov_imm16(asm, Reg::X0, 0);
        emit_mov_imm64(asm, Reg::X1, REGFILE_SIZE);
        emit_mov_imm16(asm, Reg::X2, 3);
        emit_mov_imm16(asm, Reg::X3, 0x1002);
        emit_mov_imm64(asm, Reg::X4, u64::MAX);
        emit_mov_imm16(asm, Reg::X5, 0);
        asm.emit_bl_external("mmap");

        emit_mov_reg(asm, Reg::X19, Reg::X0);

        emit_mov_imm16(asm, Reg::X16, 0);
        for idx in 0..16i64 {
            emit_mov_reg(asm, Reg::X17, Reg::X19);
            add_immediate_offset(asm, Reg::X17, idx * 8);
            emit_store_to_reg(asm, Reg::X16, Reg::X17);
        }
        return Ok(());
    }

    emit_mov_reg(asm, Reg::X19, Reg::X0);
    Ok(())
}

fn emit_darwin_variadic_format_call(
    asm: &mut Assembler,
    layout: &FrameLayout,
    function: &AsmValue,
    args: &[AsmValue],
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
    rodata: &mut Vec<u8>,
    rodata_pool: &mut HashMap<String, u64>,
) -> Result<i32> {
    let Some(vararg_start) = darwin_variadic_format_start(function, args) else {
        return Err(Error::from("unsupported darwin variadic call"));
    };

    // Apple arm64 ABI: variadic arguments are passed via the stack argument
    // area even when they would normally fit in registers.
    match (function, vararg_start) {
        (AsmValue::Function(name), 1) if name == "printf" => {
            let format_arg = &args[0];
            if let AsmValue::Constant(AsmConstant::String(text)) = format_arg {
                let offset = intern_cstring(rodata, rodata_pool, text);
                emit_load_rodata_addr(asm, Reg::X0, offset as i64)?;
            } else {
                load_value(asm, layout, format_arg, Reg::X0, reg_types, local_types)?;
            }
        }
        (AsmValue::Function(name), 2) if name == "fprintf" || name == "dprintf" => {
            load_value(asm, layout, &args[0], Reg::X0, reg_types, local_types)?;
            let format_arg = &args[1];
            if let AsmValue::Constant(AsmConstant::String(text)) = format_arg {
                let offset = intern_cstring(rodata, rodata_pool, text);
                emit_load_rodata_addr(asm, Reg::X1, offset as i64)?;
            } else {
                load_value(asm, layout, format_arg, Reg::X1, reg_types, local_types)?;
            }
        }
        _ => {
            return Err(Error::from("unsupported darwin variadic format function"));
        }
    }

    let mut stack_offset = 0i32;
    for arg in args.iter().skip(vararg_start) {
        if let AsmValue::Constant(AsmConstant::String(text)) = arg {
            let offset = intern_cstring(rodata, rodata_pool, text);
            emit_load_rodata_addr(asm, Reg::X16, offset as i64)?;
            emit_store_to_sp(asm, Reg::X16, stack_offset);
            stack_offset += 8;
            continue;
        }
        let arg_ty = value_type(arg, reg_types, local_types)?;
        let size = store_vararg_value(
            asm,
            layout,
            stack_offset,
            arg,
            &arg_ty,
            reg_types,
            local_types,
        )?;
        stack_offset += size;
    }

    if abi_debug_enabled() {
        let name = match function {
            AsmValue::Function(name) => name.as_str(),
            _ => "<unknown>",
        };
        abi_log(&format!(
            "  call {} (darwin varargs): stack_bytes={} outgoing_cap={}",
            name, stack_offset, layout.outgoing_size
        ));
    }
    Ok(stack_offset)
}

fn emit_syscall(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    convention: AsmSyscallConvention,
    number: &AsmValue,
    args: &[AsmValue],
    ret_ty: &AsmType,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
    format: TargetFormat,
) -> Result<()> {
    let (number_reg, arg_regs, svc_imm) = match convention {
        AsmSyscallConvention::LinuxAarch64 => (
            Reg::X8,
            [
                Reg::X0,
                Reg::X1,
                Reg::X2,
                Reg::X3,
                Reg::X4,
                Reg::X5,
                Reg::X6,
                Reg::X7,
            ],
            0u16,
        ),
        AsmSyscallConvention::DarwinAarch64 => (
            Reg::X16,
            [
                Reg::X0,
                Reg::X1,
                Reg::X2,
                Reg::X3,
                Reg::X4,
                Reg::X5,
                Reg::X6,
                Reg::X7,
            ],
            0x80u16,
        ),
        _ => {
            return Err(Error::from(
                "unsupported syscall convention for aarch64 emitter",
            ));
        }
    };

    match (format, convention) {
        (TargetFormat::Elf, AsmSyscallConvention::LinuxAarch64)
        | (TargetFormat::MachO, AsmSyscallConvention::DarwinAarch64) => {}
        (TargetFormat::Coff, _) => {
            return Err(Error::from(
                "syscall emission is not supported for COFF targets",
            ));
        }
        _ => {
            return Err(Error::from(
                "syscall convention does not match output target",
            ));
        }
    }

    load_value(asm, layout, number, number_reg, reg_types, local_types)?;
    for (idx, arg) in args.iter().take(arg_regs.len()).enumerate() {
        load_value(asm, layout, arg, arg_regs[idx], reg_types, local_types)?;
    }
    emit_svc_imm(asm, svc_imm);

    if !matches!(ret_ty, AsmType::Void) {
        store_vreg(asm, layout, dst_id, Reg::X0)?;
    }
    Ok(())
}

fn initialize_lifted_x86_argument_locals(
    asm: &mut Assembler,
    layout: &FrameLayout,
    func: &AsmFunction,
) -> Result<()> {
    // Lifted x86_64 functions model SysV argument registers (rdi, rsi, rdx, rcx,
    // r8, r9) as locals. Internal translated calls pass arguments using the
    // host AArch64 ABI (x0..), so we must seed those locals at each function
    // entry before any prologue helper clobbers x0.. (for example the emulated
    // stack allocator).
    let mut store_arg = |name: &str, src: Reg| -> Result<()> {
        let Some(local) = func
            .locals
            .iter()
            .find(|local| local.is_argument && local.name.as_deref() == Some(name))
        else {
            return Ok(());
        };
        let offset = local_offset(layout, local.id)?;
        emit_store_to_sp(asm, src, offset);
        Ok(())
    };

    store_arg("rdi", Reg::X0)?;
    store_arg("rsi", Reg::X1)?;
    store_arg("rdx", Reg::X2)?;
    store_arg("rcx", Reg::X3)?;
    store_arg("r8", Reg::X4)?;
    store_arg("r9", Reg::X5)?;
    Ok(())
}

impl Reg {
    fn id(self) -> u32 {
        match self {
            Reg::X0 => 0,
            Reg::X1 => 1,
            Reg::X2 => 2,
            Reg::X3 => 3,
            Reg::X4 => 4,
            Reg::X5 => 5,
            Reg::X6 => 6,
            Reg::X7 => 7,
            Reg::X8 => 8,
            Reg::X9 => 9,
            Reg::X10 => 10,
            Reg::X11 => 11,
            Reg::X12 => 12,
            Reg::X13 => 13,
            Reg::X14 => 14,
            Reg::X15 => 15,
            Reg::X16 => 16,
            Reg::X17 => 17,
            Reg::X19 => 19,
            Reg::X29 => 29,
            Reg::X30 => 30,
            Reg::X31 => 31,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum FReg {
    V0,
    V1,
    V2,
    V3,
    V4,
    V5,
    V6,
    V7,
}

impl FReg {
    fn id(self) -> u32 {
        match self {
            FReg::V0 => 0,
            FReg::V1 => 1,
            FReg::V2 => 2,
            FReg::V3 => 3,
            FReg::V4 => 4,
            FReg::V5 => 5,
            FReg::V6 => 6,
            FReg::V7 => 7,
        }
    }
}

pub fn emit_text_from_asmir(program: &AsmProgram, format: TargetFormat) -> Result<CodegenOutput> {
    let mut func_map = build_function_map(program)?;
    let needs_panic_stub = program_uses_fp_panic(program) && !func_map.contains_key("fp_panic");
    let panic_id = if needs_panic_stub {
        let id = func_map.len() as u32;
        func_map.insert("fp_panic".to_string(), id);
        Some(id)
    } else {
        None
    };
    let mut rodata = Vec::new();
    let mut data = Vec::new();
    let mut rodata_pool = HashMap::new();
    let mut rodata_symbols = HashMap::new();
    let mut data_symbols = HashMap::new();
    let mut global_relocs = Vec::new();
    let mut entry_offset = None;

    emit_const_globals(
        program,
        &program.data_layout,
        &mut rodata,
        &mut rodata_symbols,
        &mut data,
        &mut data_symbols,
        &mut global_relocs,
    )?;

    // Mach-O needs a stable, symbol-based anchor for ADRP+ADD relocations.
    // Using a section relocation for `.rodata` triggers unsupported fixups in
    // the external linker.
    rodata_symbols
        .entry("fp_rodata_base".to_string())
        .or_insert(0);

    fn insert_symbol_variants(out: &mut HashSet<String>, symbol: &str) {
        out.insert(symbol.to_string());
        if let Some(stripped) = symbol.strip_prefix('_') {
            out.insert(stripped.to_string());
        } else {
            out.insert(format!("_{symbol}"));
        }
    }

    let mut defined_symbols = HashSet::new();
    for func in program.functions.iter().filter(|func| !func.is_declaration) {
        insert_symbol_variants(&mut defined_symbols, func.name.as_str());
    }
    for global in program
        .globals
        .iter()
        .filter(|global| global.initializer.is_some())
    {
        insert_symbol_variants(&mut defined_symbols, global.name.as_str());
    }
    for name in rodata_symbols.keys() {
        insert_symbol_variants(&mut defined_symbols, name);
    }
    for name in data_symbols.keys() {
        insert_symbol_variants(&mut defined_symbols, name);
    }
    if let Some(container) = &program.container {
        for symbol in container
            .symbols
            .iter()
            .filter(|symbol| symbol.section.is_some())
        {
            let name = symbol
                .name
                .split_once('@')
                .map(|(head, _)| head)
                .unwrap_or(symbol.name.as_str());
            insert_symbol_variants(&mut defined_symbols, name);
        }
    }
    let mut asm = Assembler::new(format, defined_symbols, program.data_layout.clone());
    asm.entry_returns_exit = matches!(format, TargetFormat::Elf)
        && program
            .container
            .as_ref()
            .is_some_and(|container| container.kind == ContainerKind::Executable);

    let use_x86_regfile = program.functions.iter().any(|func| {
        if func.name.as_str() == "fp_lifted_main" {
            return true;
        }
        func.basic_blocks.iter().any(|block| {
            block.instructions.iter().any(|inst| match &inst.kind {
                AsmInstructionKind::Call {
                    calling_convention, ..
                } => matches!(calling_convention, CallingConvention::FpLiftedX86_64RegFile),
                _ => false,
            }) || matches!(
                block.terminator,
                AsmTerminator::Invoke {
                    calling_convention: CallingConvention::FpLiftedX86_64RegFile,
                    ..
                }
            )
        })
    });

    for (index, func) in program.functions.iter().enumerate() {
        if func.is_declaration {
            continue;
        }
        asm.bind(Label::Function(index as u32));
        if entry_offset.is_none() && func.name.as_str() == "main" {
            entry_offset = Some(asm.buf.len() as u64);
        }

        if let Some(preserved) = collect_preserved_single_block_bytes(program, func) {
            let block_id = func.basic_blocks.first().map(|block| block.id).unwrap_or(0);
            asm.bind(Label::Block(index as u32, block_id));
            asm.extend(&preserved);
            continue;
        }

        let mut reg_types = build_reg_types(func);
        let source_types = crate::asmir::merged_register_types(program, func);
        reg_types.extend(source_types);
        let layout = build_frame_layout(func, &reg_types, use_x86_regfile, &program.data_layout)?;
        let local_types = build_local_types(func);
        asm.set_layout_context(func.name.as_str(), layout.frame_size);
        asm.needs_frame = layout.frame_size > 0;
        if layout.frame_size > 0 {
            emit_prologue(&mut asm, &layout)?;
            spill_arguments(&mut asm, &layout, func, &local_types)?;
            if func.name.as_str() == "fp_lifted_main" {
                initialize_lifted_x86_argument_locals(&mut asm, &layout, func)?;
            }
            if !layout.x86_regfile_offsets.is_empty() {
                initialize_lifted_x86_regfile(&mut asm, &layout, func)?;
            }
            initialize_lifted_stack_pointer(&mut asm, &layout, func)?;
            zero_initialize_lifted_x86_register_locals(&mut asm, &layout, func)?;
        }
        for block in &func.basic_blocks {
            asm.bind(Label::Block(index as u32, block.id));
            emit_block(
                &mut asm,
                block,
                format,
                &func_map,
                &layout,
                &reg_types,
                &local_types,
                &func.signature.return_type,
                &mut rodata,
                &mut rodata_pool,
            )?;
        }
        asm.clear_layout_context();
    }

    if let Some(id) = panic_id {
        emit_panic_stub(&mut asm, id);
    }

    // `None` when `program` has no `main` (e.g. a plain object/archive
    // transpile roundtrip) — only producing an executable or JIT-executing
    // this plan actually needs a real entrypoint; both check for that
    // explicitly at that point instead of it being required unconditionally
    // here for every plan.
    let func_offsets = asm.function_offsets();
    let mut symbols = HashMap::new();
    for (idx, func) in program.functions.iter().enumerate() {
        if let Some(offset) = func_offsets.get(&(idx as u32)) {
            symbols.insert(func.name.to_string(), *offset);
        }
    }
    if let Some(id) = panic_id {
        if let Some(offset) = func_offsets.get(&id) {
            symbols.insert("fp_panic".to_string(), *offset);
        }
    }
    let (text, relocs) = asm.finish()?;
    Ok(CodegenOutput {
        text,
        rodata,
        data,
        relocs,
        section_relocs: global_relocs,
        symbols,
        rodata_symbols,
        data_symbols,
        entry_offset,
    })
}

fn initialize_lifted_stack_pointer(
    asm: &mut Assembler,
    layout: &FrameLayout,
    func: &AsmFunction,
) -> Result<()> {
    // When translating lifted x86_64 code into AArch64, we cannot rely on the
    // host stack layout matching x86 calling convention expectations.
    //
    // Provide a dedicated emulated stack region so x86 stack probes operate on
    // mapped memory.
    //
    // IMPORTANT: when we are using the shared x86 regfile calling convention
    // (i.e. stack slots are backed by the regfile pointer in `x19`), the stack
    // pointer must remain shared across lifted functions. In that mode we only
    // initialize the stack once in `fp_lifted_main`.
    let rsp_stack_slot = func
        .stack_slots
        .iter()
        .find(|slot| slot.name.as_deref() == Some("x86.rsp"))
        .map(|slot| slot.id);
    let rsp_local = func
        .locals
        .iter()
        .find(|local| local.name.as_deref() == Some("rsp"))
        .map(|local| local.id);

    if rsp_stack_slot.is_none() && rsp_local.is_none() {
        return Ok(());
    }

    if !layout.x86_regfile_offsets.is_empty() && func.name.as_str() != "fp_lifted_main" {
        return Ok(());
    }

    // Provide ample headroom for translated userspace stacks while we iterate.
    const EMULATED_STACK_SIZE: u64 = 16 * 1024 * 1024;

    // Prefer `mmap` over `malloc` so we always get a contiguous read/write
    // region without allocator guard-page surprises.
    //
    // void *mmap(void *addr, size_t len, int prot, int flags, int fd, off_t offset);
    //
    // addr = NULL
    emit_mov_imm16(asm, Reg::X0, 0);
    // len
    emit_mov_imm64(asm, Reg::X1, EMULATED_STACK_SIZE);
    // prot = PROT_READ|PROT_WRITE
    emit_mov_imm16(asm, Reg::X2, 3);
    // flags = MAP_PRIVATE|MAP_ANON (Darwin: 0x2 | 0x1000)
    emit_mov_imm16(asm, Reg::X3, 0x1002);
    // fd = -1
    emit_mov_imm64(asm, Reg::X4, u64::MAX);
    // offset = 0
    emit_mov_imm16(asm, Reg::X5, 0);
    asm.emit_bl_external("mmap");

    // rsp = align_down(ptr + size - slack, 16)
    //
    // Some allocators place guard pages near the end of large allocations.
    // Leave headroom so typical x86 red-zone/prologue stores do not fault.
    const EMULATED_STACK_SLACK: u64 = 128 * 1024;

    emit_mov_reg(asm, Reg::X16, Reg::X0);
    emit_mov_imm64(asm, Reg::X17, EMULATED_STACK_SIZE);
    emit_add_reg(asm, Reg::X16, Reg::X16, Reg::X17);
    emit_mov_imm64(asm, Reg::X17, EMULATED_STACK_SLACK);
    emit_sub_reg(asm, Reg::X16, Reg::X16, Reg::X17);
    emit_mov_imm64(asm, Reg::X17, !0xFu64);
    emit_and_reg(asm, Reg::X16, Reg::X16, Reg::X17);

    // Store a dummy return address so x86 prologues that copy it do not fault.
    emit_store_to_reg(asm, Reg::X31, Reg::X16);

    if let Some(slot_id) = rsp_stack_slot {
        let (base, offset) = stack_slot_base_and_offset(layout, slot_id)?;
        if base == Reg::X31 {
            emit_store_to_sp(asm, Reg::X16, offset);
        } else {
            emit_mov_reg(asm, Reg::X17, base);
            add_immediate_offset(asm, Reg::X17, offset as i64);
            emit_store_to_reg(asm, Reg::X16, Reg::X17);
        }
    }
    if let Some(local_id) = rsp_local {
        let offset = local_offset(layout, local_id)?;
        emit_store_to_sp(asm, Reg::X16, offset);
    }
    Ok(())
}

fn zero_initialize_lifted_x86_register_locals(
    asm: &mut Assembler,
    layout: &FrameLayout,
    func: &AsmFunction,
) -> Result<()> {
    // Lifted x86 code models architectural registers as locals. When a callee-saved
    // register is read before it is written (for example, pushed during a prologue),
    // the emitter would otherwise read uninitialized stack bytes and accidentally
    // turn them into pointers. Zero-initialize non-argument register locals to keep
    // execution deterministic.
    let Some(rsp_local_id) = func
        .locals
        .iter()
        .find(|local| local.name.as_deref() == Some("rsp"))
        .map(|local| local.id)
    else {
        return Ok(());
    };

    emit_mov_imm16(asm, Reg::X16, 0);

    for local in &func.locals {
        if local.is_argument {
            continue;
        }
        if local.id == rsp_local_id {
            continue;
        }
        let Some(name) = local.name.as_deref() else {
            continue;
        };
        let is_x86_gpr = matches!(
            name,
            "rax"
                | "rcx"
                | "rdx"
                | "rbx"
                | "rbp"
                | "rsi"
                | "rdi"
                | "r8"
                | "r9"
                | "r10"
                | "r11"
                | "r12"
                | "r13"
                | "r14"
                | "r15"
        );
        if !is_x86_gpr {
            continue;
        }

        let offset = local_offset(layout, local.id)?;
        emit_store_to_sp(asm, Reg::X16, offset);
    }

    Ok(())
}

fn const_to_u8(constant: &AsmConstant) -> Result<u8> {
    match constant {
        AsmConstant::Int(value, _) => Ok(*value as u8),
        AsmConstant::UInt(value, _) => Ok(*value as u8),
        AsmConstant::Bool(value) => Ok(if *value { 1 } else { 0 }),
        AsmConstant::Null(_) | AsmConstant::Undef(_) => Ok(0),
        _ => Err(Error::from(
            "unsupported global array element for native rodata",
        )),
    }
}

enum BinOp {
    Add,
    Sub,
    Mul,
}

enum BitOp {
    And,
    Or,
    Xor,
}

enum ShiftKind {
    Left,
    Right,
}

fn emit_inline_asm(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    output_ty: &AsmType,
) -> Result<()> {
    let size_of = |ty: &LirType| layout.data_layout.size_of(ty).expect("layout query failed");
    if matches!(output_ty, AsmType::Void) {
        return Ok(());
    }
    let size = size_of(output_ty) as i32;
    let dst_offset = vreg_offset(layout, dst_id)?;
    if is_aggregate_type(output_ty) && size > 8 {
        zero_sp_range(asm, dst_offset, size)?;
        return Ok(());
    }
    if matches!(output_ty, AsmType::I128) {
        emit_mov_imm16(asm, Reg::X16, 0);
        emit_mov_imm16(asm, Reg::X17, 0);
        store_i128_value(asm, layout, dst_id, Reg::X16, Reg::X17)?;
        return Ok(());
    }
    emit_mov_imm16(asm, Reg::X16, 0);
    store_vreg(asm, layout, dst_id, Reg::X16)?;
    Ok(())
}

fn emit_binop(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    lhs: &AsmValue,
    rhs: &AsmValue,
    op: BinOp,
    ty: &AsmType,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    if is_float_type(ty) {
        load_value_float(asm, layout, lhs, FReg::V0, ty, reg_types, local_types)?;
        load_value_float(asm, layout, rhs, FReg::V1, ty, reg_types, local_types)?;
        match op {
            BinOp::Add => emit_fadd(asm, FReg::V0, FReg::V0, FReg::V1, ty),
            BinOp::Sub => emit_fsub(asm, FReg::V0, FReg::V0, FReg::V1, ty),
            BinOp::Mul => emit_fmul(asm, FReg::V0, FReg::V0, FReg::V1, ty),
        }
        store_vreg_float(asm, layout, dst_id, FReg::V0, ty)?;
        return Ok(());
    }
    if matches!(ty, AsmType::I128) {
        return emit_i128_binop(asm, layout, dst_id, lhs, rhs, op, reg_types, local_types);
    }

    load_value(asm, layout, lhs, Reg::X16, reg_types, local_types)?;
    match rhs {
        AsmValue::Register(_) => {
            load_value(asm, layout, rhs, Reg::X17, reg_types, local_types)?;
            match op {
                BinOp::Add => emit_add_reg(asm, Reg::X16, Reg::X16, Reg::X17),
                BinOp::Sub => emit_sub_reg(asm, Reg::X16, Reg::X16, Reg::X17),
                BinOp::Mul => emit_mul_reg(asm, Reg::X16, Reg::X16, Reg::X17),
            }
        }
        AsmValue::Constant(constant) => {
            if matches!(
                constant,
                AsmConstant::GlobalRef(_, _, _) | AsmConstant::FunctionRef(_, _)
            ) {
                load_value(asm, layout, rhs, Reg::X17, reg_types, local_types)?;
                match op {
                    BinOp::Add => emit_add_reg(asm, Reg::X16, Reg::X16, Reg::X17),
                    BinOp::Sub => emit_sub_reg(asm, Reg::X16, Reg::X16, Reg::X17),
                    BinOp::Mul => emit_mul_reg(asm, Reg::X16, Reg::X16, Reg::X17),
                }
            } else {
                let imm = constant_to_i64(constant, &layout.data_layout)?;
                if imm < 0 || imm > u16::MAX as i64 {
                    emit_mov_imm64(asm, Reg::X17, imm as u64);
                    match op {
                        BinOp::Add => emit_add_reg(asm, Reg::X16, Reg::X16, Reg::X17),
                        BinOp::Sub => emit_sub_reg(asm, Reg::X16, Reg::X16, Reg::X17),
                        BinOp::Mul => emit_mul_reg(asm, Reg::X16, Reg::X16, Reg::X17),
                    }
                } else {
                    match op {
                        BinOp::Add if imm <= 4095 => {
                            emit_add_imm12(asm, Reg::X16, Reg::X16, imm as u32)
                        }
                        BinOp::Sub if imm <= 4095 => {
                            emit_sub_imm12(asm, Reg::X16, Reg::X16, imm as u32)
                        }
                        BinOp::Mul => {
                            emit_mov_imm16(asm, Reg::X17, imm as u16);
                            emit_mul_reg(asm, Reg::X16, Reg::X16, Reg::X17);
                        }
                        _ => {
                            emit_mov_imm16(asm, Reg::X17, imm as u16);
                            match op {
                                BinOp::Add => emit_add_reg(asm, Reg::X16, Reg::X16, Reg::X17),
                                BinOp::Sub => emit_sub_reg(asm, Reg::X16, Reg::X16, Reg::X17),
                                BinOp::Mul => unreachable!(),
                            }
                        }
                    }
                }
            }
        }
        _ => {
            load_value(asm, layout, rhs, Reg::X17, reg_types, local_types)?;
            match op {
                BinOp::Add => emit_add_reg(asm, Reg::X16, Reg::X16, Reg::X17),
                BinOp::Sub => emit_sub_reg(asm, Reg::X16, Reg::X16, Reg::X17),
                BinOp::Mul => emit_mul_reg(asm, Reg::X16, Reg::X16, Reg::X17),
            }
        }
    }
    store_vreg(asm, layout, dst_id, Reg::X16)?;
    Ok(())
}

fn emit_divrem(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    lhs: &AsmValue,
    rhs: &AsmValue,
    want_rem: bool,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    let lhs_ty = value_type(lhs, reg_types, local_types)?;
    if is_float_type(&lhs_ty) {
        if want_rem {
            load_value_float(asm, layout, lhs, FReg::V0, &lhs_ty, reg_types, local_types)?;
            load_value_float(asm, layout, rhs, FReg::V1, &lhs_ty, reg_types, local_types)?;
            let symbol = if matches!(lhs_ty, AsmType::F32) {
                "fmodf"
            } else {
                "fmod"
            };
            asm.emit_bl_external(symbol);
            store_vreg_float(asm, layout, dst_id, FReg::V0, &lhs_ty)?;
            return Ok(());
        }
        load_value_float(asm, layout, lhs, FReg::V0, &lhs_ty, reg_types, local_types)?;
        load_value_float(asm, layout, rhs, FReg::V1, &lhs_ty, reg_types, local_types)?;
        emit_fdiv(asm, FReg::V0, FReg::V0, FReg::V1, &lhs_ty);
        store_vreg_float(asm, layout, dst_id, FReg::V0, &lhs_ty)?;
        return Ok(());
    }
    if matches!(lhs_ty, AsmType::I128) {
        return emit_i128_divrem(
            asm,
            layout,
            dst_id,
            lhs,
            rhs,
            want_rem,
            reg_types,
            local_types,
        );
    }

    load_value(asm, layout, lhs, Reg::X16, reg_types, local_types)?;
    load_value(asm, layout, rhs, Reg::X17, reg_types, local_types)?;

    emit_sdiv(asm, Reg::X9, Reg::X16, Reg::X17);

    if want_rem {
        emit_msub(asm, Reg::X16, Reg::X9, Reg::X17, Reg::X16);
    } else {
        emit_mov_reg(asm, Reg::X16, Reg::X9);
    }
    store_vreg(asm, layout, dst_id, Reg::X16)?;

    Ok(())
}

fn emit_landingpad(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    result_ty: &AsmType,
) -> Result<()> {
    let size_of = |ty: &LirType| layout.data_layout.size_of(ty).expect("layout query failed");
    let _align_of = |ty: &LirType| {
        layout
            .data_layout
            .align_of(ty)
            .expect("layout query failed")
    };
    let _struct_layout = |ty: &LirType| {
        layout
            .data_layout
            .struct_layout(ty)
            .expect("layout query failed")
    };
    let size = size_of(result_ty) as i32;
    if size == 0 {
        return Ok(());
    }
    if is_large_aggregate(result_ty, &layout.data_layout) {
        let dst_offset = agg_offset(layout, dst_id)?;
        zero_sp_range(asm, dst_offset, size)?;
        emit_mov_reg(asm, Reg::X16, Reg::X31);
        add_immediate_offset(asm, Reg::X16, dst_offset as i64);
        store_vreg(asm, layout, dst_id, Reg::X16)?;
        return Ok(());
    }
    emit_mov_imm16(asm, Reg::X16, 0);
    store_vreg(asm, layout, dst_id, Reg::X16)?;
    Ok(())
}

fn intern_cstring(rodata: &mut Vec<u8>, pool: &mut HashMap<String, u64>, text: &str) -> u64 {
    if let Some(offset) = pool.get(text) {
        return *offset;
    }
    align_rodata(rodata, 8);
    let offset = rodata.len() as u64;
    rodata.extend_from_slice(text.as_bytes());
    rodata.push(0);
    pool.insert(text.to_string(), offset);
    offset
}

fn align_rodata(rodata: &mut Vec<u8>, align: usize) {
    while rodata.len() % align != 0 {
        rodata.push(0);
    }
}

fn emit_load_rodata_addr(asm: &mut Assembler, dst: Reg, addend: i64) -> Result<()> {
    if asm.target_format == TargetFormat::MachO {
        let offset = asm.buf.len();
        // Mach-O does not allow absolute (unsigned) relocations in __TEXT.
        // Materialize addresses in .rodata using the usual ADRP+ADD sequence.
        //
        // ADRP dst, .rodata@PAGE
        let adrp = 0x9000_0000u32 | dst.id();
        asm.emit_u32(adrp);
        // ADD dst, dst, .rodata@PAGEOFF
        let add = 0x9100_0000u32 | (dst.id() << 5) | dst.id();
        asm.emit_u32(add);
        asm.relocs.push(Relocation {
            offset: offset as u64,
            kind: RelocKind::Aarch64AdrpAdd,
            section: crate::emit::RelocSection::Text,
            symbol: "fp_rodata_base".to_string(),
            addend: 0,
        });
        add_immediate_offset(asm, dst, addend);
        return Ok(());
    }

    if asm.buf.len() % 8 != 0 {
        emit_nop(asm);
    }
    let ldr_instr = 0x5800_0000u32 | ((2u32 & 0x7ffff) << 5) | dst.id();
    asm.emit_u32(ldr_instr);
    let b_instr = 0x1400_0000u32 | 3;
    asm.emit_u32(b_instr);
    let literal_offset = asm.buf.len();
    asm.relocs.push(Relocation {
        offset: literal_offset as u64,
        kind: RelocKind::Abs64,
        section: crate::emit::RelocSection::Text,
        symbol: ".rodata".to_string(),
        addend,
    });
    asm.extend(&0u64.to_le_bytes());
    Ok(())
}

fn emit_load_symbol_addr(asm: &mut Assembler, dst: Reg, symbol: &str, addend: i64) -> Result<()> {
    if asm.target_format == TargetFormat::MachO {
        let offset = asm.buf.len();
        // When taking the address of an undefined symbol on Mach-O/AArch64,
        // use a GOT load so the linker can keep it unresolved under
        // `-undefined dynamic_lookup`.
        let reloc_kind = if asm.defined_symbols.contains(symbol) {
            // ADRP dst, symbol@PAGE
            let adrp = 0x9000_0000u32 | dst.id();
            asm.emit_u32(adrp);
            // ADD dst, dst, symbol@PAGEOFF
            let add = 0x9100_0000u32 | (dst.id() << 5) | dst.id();
            asm.emit_u32(add);
            RelocKind::Aarch64AdrpAdd
        } else {
            // ADRP dst, symbol@GOTPAGE
            let adrp = 0x9000_0000u32 | dst.id();
            asm.emit_u32(adrp);
            // LDR dst, [dst, symbol@GOTPAGEOFF]
            let ldr = 0xF940_0000u32 | (dst.id() << 5) | dst.id();
            asm.emit_u32(ldr);
            RelocKind::Aarch64GotLoad
        };

        asm.relocs.push(Relocation {
            offset: offset as u64,
            kind: reloc_kind,
            section: crate::emit::RelocSection::Text,
            symbol: symbol.to_string(),
            addend: 0,
        });
        add_immediate_offset(asm, dst, addend);
        return Ok(());
    }

    if asm.buf.len() % 8 != 0 {
        emit_nop(asm);
    }
    let ldr_instr = 0x5800_0000u32 | ((2u32 & 0x7ffff) << 5) | dst.id();
    asm.emit_u32(ldr_instr);
    let b_instr = 0x1400_0000u32 | 3;
    asm.emit_u32(b_instr);
    let literal_offset = asm.buf.len();
    asm.relocs.push(Relocation {
        offset: literal_offset as u64,
        kind: RelocKind::Abs64,
        section: crate::emit::RelocSection::Text,
        symbol: symbol.to_string(),
        addend,
    });
    asm.extend(&0u64.to_le_bytes());
    Ok(())
}
fn store_outgoing_arg(
    asm: &mut Assembler,
    layout: &FrameLayout,
    offset: i32,
    value: &AsmValue,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    if offset < 0 || offset + 8 > layout.outgoing_size {
        return Err(Error::from("outgoing arg offset out of range"));
    }
    let ty = value_type(value, reg_types, local_types)?;
    if is_float_type(&ty) {
        load_value_float(asm, layout, value, FReg::V0, &ty, reg_types, local_types)?;
        emit_store_float_to_sp(asm, FReg::V0, offset, &ty);
    } else {
        load_value(asm, layout, value, Reg::X16, reg_types, local_types)?;
        emit_store_to_sp(asm, Reg::X16, offset);
    }
    Ok(())
}

fn store_vararg_value(
    asm: &mut Assembler,
    layout: &FrameLayout,
    offset: i32,
    value: &AsmValue,
    ty: &AsmType,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<i32> {
    let size_of = |ty: &LirType| layout.data_layout.size_of(ty).expect("layout query failed");
    let _align_of = |ty: &LirType| {
        layout
            .data_layout
            .align_of(ty)
            .expect("layout query failed")
    };
    let _struct_layout = |ty: &LirType| {
        layout
            .data_layout
            .struct_layout(ty)
            .expect("layout query failed")
    };
    let size = align8(size_of(ty) as i32);
    if size == 0 {
        return Ok(0);
    }
    if offset < 0 || offset + size > layout.outgoing_size {
        return Err(Error::from("outgoing vararg offset out of range"));
    }
    if size <= 8 {
        store_outgoing_arg(asm, layout, offset, value, reg_types, local_types)?;
        return Ok(size);
    }
    match value {
        AsmValue::Register(id) => {
            let src_offset = agg_offset(layout, *id)?;
            copy_sp_to_sp(asm, src_offset, offset, size)?;
            Ok(size)
        }
        _ => Err(Error::from(
            "unsupported large aggregate for varargs on aarch64",
        )),
    }
}

fn spill_arguments(
    asm: &mut Assembler,
    layout: &FrameLayout,
    func: &AsmFunction,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    let size_of = |ty: &LirType| layout.data_layout.size_of(ty).expect("layout query failed");
    let _align_of = |ty: &LirType| {
        layout
            .data_layout
            .align_of(ty)
            .expect("layout query failed")
    };
    let _struct_layout = |ty: &LirType| {
        layout
            .data_layout
            .struct_layout(ty)
            .expect("layout query failed")
    };
    let arg_regs = [
        Reg::X0,
        Reg::X1,
        Reg::X2,
        Reg::X3,
        Reg::X4,
        Reg::X5,
        Reg::X6,
        Reg::X7,
    ];
    let float_regs = [
        FReg::V0,
        FReg::V1,
        FReg::V2,
        FReg::V3,
        FReg::V4,
        FReg::V5,
        FReg::V6,
        FReg::V7,
    ];

    let mut int_idx = 0usize;
    let mut float_idx = 0usize;
    let mut stack_idx = 0usize;

    if let Some(offset) = layout.sret_offset {
        emit_store_to_sp(asm, arg_regs[0], offset);
        int_idx = 1;
    }

    for local in func.locals.iter().filter(|local| local.is_argument) {
        let ty = local_types
            .get(&local.id)
            .ok_or_else(|| Error::from("missing local type"))?;
        if matches!(ty, AsmType::Void) {
            continue;
        }
        let offset = local_offset(layout, local.id)?;
        if is_large_aggregate(ty, &layout.data_layout) {
            let size = size_of(ty) as i32;
            if int_idx < arg_regs.len() {
                copy_reg_to_sp(asm, arg_regs[int_idx], offset, size)?;
                int_idx += 1;
            } else {
                let incoming = layout.frame_size + (stack_idx as i32) * 8;
                emit_load_from_sp(asm, Reg::X16, incoming);
                copy_reg_to_sp(asm, Reg::X16, offset, size)?;
                stack_idx += 1;
            }
            continue;
        }
        if is_float_type(ty) {
            if float_idx < float_regs.len() {
                emit_store_float_to_sp(asm, float_regs[float_idx], offset, ty);
                float_idx += 1;
            } else {
                let incoming = layout.frame_size + (stack_idx as i32) * 8;
                emit_load_float_from_sp(asm, FReg::V0, incoming, ty);
                emit_store_float_to_sp(asm, FReg::V0, offset, ty);
                stack_idx += 1;
            }
        } else if int_idx < arg_regs.len() {
            match size_of(ty) {
                1 => emit_store8_to_sp(asm, arg_regs[int_idx], offset)?,
                2 => emit_store16_to_sp(asm, arg_regs[int_idx], offset)?,
                4 => emit_store32_to_sp(asm, arg_regs[int_idx], offset)?,
                _ => emit_store_to_sp(asm, arg_regs[int_idx], offset),
            }
            int_idx += 1;
        } else {
            let incoming = layout.frame_size + (stack_idx as i32) * 8;
            emit_load_from_sp(asm, Reg::X16, incoming);
            match size_of(ty) {
                1 => emit_store8_to_sp(asm, Reg::X16, offset)?,
                2 => emit_store16_to_sp(asm, Reg::X16, offset)?,
                4 => emit_store32_to_sp(asm, Reg::X16, offset)?,
                _ => emit_store_to_sp(asm, Reg::X16, offset),
            }
            stack_idx += 1;
        }
    }
    asm.extend(&0xD503_201Fu32.to_le_bytes());
    Ok(())
}

fn emit_block(
    asm: &mut Assembler,
    block: &AsmBlock,
    format: TargetFormat,
    func_map: &HashMap<String, u32>,
    layout: &FrameLayout,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
    return_ty: &AsmType,
    rodata: &mut Vec<u8>,
    rodata_pool: &mut HashMap<String, u64>,
) -> Result<()> {
    let size_of = |ty: &LirType| layout.data_layout.size_of(ty).expect("layout query failed");
    let _align_of = |ty: &LirType| {
        layout
            .data_layout
            .align_of(ty)
            .expect("layout query failed")
    };
    let _struct_layout = |ty: &LirType| {
        layout
            .data_layout
            .struct_layout(ty)
            .expect("layout query failed")
    };
    for inst in &block.instructions {
        match &inst.kind {
            AsmInstructionKind::Nop => {
                asm.emit_u32(0xD503201F);
            }
            AsmInstructionKind::Add(lhs, rhs) => {
                let ty = inst.ty.clone();
                if matches!(ty, AsmType::Void) {
                    return Err(Error::from("add requires a concrete type"));
                }
                if matches!(ty, AsmType::Ptr(_)) {
                    if let (
                        AsmValue::Constant(AsmConstant::String(lhs_text)),
                        AsmValue::Constant(AsmConstant::String(rhs_text)),
                    ) = (lhs, rhs)
                    {
                        let mut combined = String::with_capacity(lhs_text.len() + rhs_text.len());
                        combined.push_str(lhs_text);
                        combined.push_str(rhs_text);
                        let offset = intern_cstring(rodata, rodata_pool, &combined);
                        emit_load_rodata_addr(asm, Reg::X16, offset as i64)?;
                        store_vreg(asm, layout, inst.id, Reg::X16)?;
                        continue;
                    }
                }
                emit_binop(
                    asm,
                    layout,
                    inst.id,
                    lhs,
                    rhs,
                    BinOp::Add,
                    &ty,
                    reg_types,
                    local_types,
                )?
            }
            AsmInstructionKind::Sub(lhs, rhs) => {
                let ty = inst.ty.clone();
                if matches!(ty, AsmType::Void) {
                    return Err(Error::from("sub requires a concrete type"));
                }
                emit_binop(
                    asm,
                    layout,
                    inst.id,
                    lhs,
                    rhs,
                    BinOp::Sub,
                    &ty,
                    reg_types,
                    local_types,
                )?
            }
            AsmInstructionKind::Mul(lhs, rhs) => {
                let ty = inst.ty.clone();
                if matches!(ty, AsmType::Void) {
                    return Err(Error::from("mul requires a concrete type"));
                }
                emit_binop(
                    asm,
                    layout,
                    inst.id,
                    lhs,
                    rhs,
                    BinOp::Mul,
                    &ty,
                    reg_types,
                    local_types,
                )?
            }
            AsmInstructionKind::Splat {
                value,
                lane_bits,
                lanes,
            } => {
                let result_ty = inst.ty.clone();
                if matches!(result_ty, AsmType::Void) {
                    return Err(Error::from("splat requires a concrete result type"));
                }
                if !matches!(result_ty, AsmType::Vector(_, _) if size_of(&result_ty) == 16) {
                    return Err(Error::from("splat expects 128-bit vector result"));
                }

                load_value(asm, layout, value, Reg::X16, reg_types, local_types)?;
                emit_dup_from_gpr(asm, FReg::V0, Reg::X16, *lane_bits, *lanes)?;
                store_vreg_float(asm, layout, inst.id, FReg::V0, &result_ty)?;
            }
            AsmInstructionKind::BuildVector { elements } => {
                let result_ty = inst.ty.clone();
                if matches!(result_ty, AsmType::Void) {
                    return Err(Error::from("build_vector requires a concrete result type"));
                }
                let AsmType::Vector(elem_ty, lanes) = &result_ty else {
                    return Err(Error::from("build_vector expects vector result type"));
                };
                if size_of(&result_ty) != 16 {
                    return Err(Error::from("build_vector only supports 128-bit vectors"));
                }
                if *elem_ty.as_ref() != AsmType::I64 || *lanes != 2 {
                    return Err(Error::from(
                        "build_vector currently only supports <2 x i64> on aarch64",
                    ));
                }
                if elements.len() != 2 {
                    return Err(Error::from("build_vector lane count mismatch"));
                }
                load_value(asm, layout, &elements[0], Reg::X16, reg_types, local_types)?;
                load_value(asm, layout, &elements[1], Reg::X17, reg_types, local_types)?;
                asm.emit_u32(0x6F00_E400u32 | FReg::V0.id());
                asm.emit_u32(0x4E08_1E00u32 | (Reg::X16.id() << 5) | FReg::V0.id());
                asm.emit_u32(0x4E18_1E00u32 | (Reg::X17.id() << 5) | FReg::V0.id());
                store_vreg_float(asm, layout, inst.id, FReg::V0, &result_ty)?;
            }
            AsmInstructionKind::ExtractLane { vector, lane } => {
                let result_ty = inst.ty.clone();
                if matches!(result_ty, AsmType::Void) {
                    return Err(Error::from("extract_lane requires a concrete result type"));
                }
                if result_ty != AsmType::I64 {
                    return Err(Error::from("extract_lane only supports i64 for now"));
                }
                let vector_ty = value_type(vector, reg_types, local_types)?;
                if !matches!(vector_ty, AsmType::Vector(_, _) if size_of(&vector_ty) == 16) {
                    return Err(Error::from("extract_lane expects 128-bit vector input"));
                }
                let lane_index = *lane;
                if lane_index > 1 {
                    return Err(Error::from("extract_lane lane out of range"));
                }
                load_value_float(
                    asm,
                    layout,
                    vector,
                    FReg::V0,
                    &vector_ty,
                    reg_types,
                    local_types,
                )?;
                let base = if lane_index == 0 {
                    0x4E08_3C00u32
                } else {
                    0x4E18_3C00u32
                };
                asm.emit_u32(base | (FReg::V0.id() << 5) | Reg::X16.id());
                store_vreg(asm, layout, inst.id, Reg::X16)?;
            }
            AsmInstructionKind::InsertLane {
                vector,
                lane,
                value,
            } => {
                let result_ty = inst.ty.clone();
                if matches!(result_ty, AsmType::Void) {
                    return Err(Error::from("insert_lane requires a concrete result type"));
                }
                if !matches!(result_ty, AsmType::Vector(_, _) if size_of(&result_ty) == 16) {
                    return Err(Error::from("insert_lane expects 128-bit vector result"));
                }
                if *lane > 1 {
                    return Err(Error::from("insert_lane lane out of range"));
                }

                let vector_ty = value_type(vector, reg_types, local_types)?;
                load_value_float(
                    asm,
                    layout,
                    vector,
                    FReg::V0,
                    &vector_ty,
                    reg_types,
                    local_types,
                )?;
                load_value(asm, layout, value, Reg::X16, reg_types, local_types)?;

                let base = if *lane == 0 {
                    0x4E08_1E00u32
                } else {
                    0x4E18_1E00u32
                };
                asm.emit_u32(base | (Reg::X16.id() << 5) | FReg::V0.id());
                store_vreg_float(asm, layout, inst.id, FReg::V0, &result_ty)?;
            }
            AsmInstructionKind::ZipLow {
                lhs,
                rhs,
                lane_bits,
            } => {
                let result_ty = inst.ty.clone();
                if matches!(result_ty, AsmType::Void) {
                    return Err(Error::from("zip_low requires a concrete result type"));
                }
                if !matches!(result_ty, AsmType::Vector(_, _) if size_of(&result_ty) == 16) {
                    return Err(Error::from("zip_low expects 128-bit vector result"));
                }
                let base = match *lane_bits {
                    16 => 0x4E40_3800u32,
                    32 => 0x4E80_3800u32,
                    64 => 0x4EC0_3800u32,
                    _ => {
                        return Err(Error::from(
                            "aarch64 zip_low only supports 16/32/64-bit lanes for now",
                        ));
                    }
                };

                let lhs_ty = value_type(lhs, reg_types, local_types)?;
                load_value_float(asm, layout, lhs, FReg::V0, &lhs_ty, reg_types, local_types)?;
                let rhs_ty = value_type(rhs, reg_types, local_types)?;
                load_value_float(asm, layout, rhs, FReg::V1, &rhs_ty, reg_types, local_types)?;

                // zip1 v0.(lanes), v0.(lanes), v1.(lanes)
                asm.emit_u32(base | (FReg::V1.id() << 16) | (FReg::V0.id() << 5) | FReg::V0.id());

                store_vreg_float(asm, layout, inst.id, FReg::V0, &result_ty)?;
            }
            AsmInstructionKind::And(lhs, rhs) => emit_bitwise_binop(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                BitOp::And,
                reg_types,
                local_types,
            )?,
            AsmInstructionKind::Or(lhs, rhs) => emit_bitwise_binop(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                BitOp::Or,
                reg_types,
                local_types,
            )?,
            AsmInstructionKind::Xor(lhs, rhs) => emit_bitwise_binop(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                BitOp::Xor,
                reg_types,
                local_types,
            )?,
            AsmInstructionKind::Shl(lhs, rhs) => emit_shift(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                ShiftKind::Left,
                reg_types,
                local_types,
            )?,
            AsmInstructionKind::Shr(lhs, rhs) => emit_shift(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                ShiftKind::Right,
                reg_types,
                local_types,
            )?,
            AsmInstructionKind::Eq(lhs, rhs) => emit_cmp(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                CmpKind::Eq,
                reg_types,
                local_types,
            )?,
            AsmInstructionKind::Ne(lhs, rhs) => emit_cmp(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                CmpKind::Ne,
                reg_types,
                local_types,
            )?,
            AsmInstructionKind::Lt(lhs, rhs) => emit_cmp(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                CmpKind::Lt,
                reg_types,
                local_types,
            )?,
            AsmInstructionKind::Le(lhs, rhs) => emit_cmp(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                CmpKind::Le,
                reg_types,
                local_types,
            )?,
            AsmInstructionKind::Gt(lhs, rhs) => emit_cmp(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                CmpKind::Gt,
                reg_types,
                local_types,
            )?,
            AsmInstructionKind::Ge(lhs, rhs) => emit_cmp(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                CmpKind::Ge,
                reg_types,
                local_types,
            )?,
            AsmInstructionKind::Ult(lhs, rhs) => emit_cmp(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                CmpKind::Ult,
                reg_types,
                local_types,
            )?,
            AsmInstructionKind::Ule(lhs, rhs) => emit_cmp(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                CmpKind::Ule,
                reg_types,
                local_types,
            )?,
            AsmInstructionKind::Ugt(lhs, rhs) => emit_cmp(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                CmpKind::Ugt,
                reg_types,
                local_types,
            )?,
            AsmInstructionKind::Uge(lhs, rhs) => emit_cmp(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                CmpKind::Uge,
                reg_types,
                local_types,
            )?,
            AsmInstructionKind::Div(lhs, rhs) => emit_divrem(
                asm,
                layout,
                inst.id,
                lhs,
                rhs,
                false,
                reg_types,
                local_types,
            )?,
            AsmInstructionKind::Rem(lhs, rhs) => {
                emit_divrem(asm, layout, inst.id, lhs, rhs, true, reg_types, local_types)?
            }
            AsmInstructionKind::Not(value) => {
                emit_not(asm, layout, inst.id, value, reg_types, local_types)?;
            }
            AsmInstructionKind::Alloca { .. } => {
                let offset = alloca_offset(layout, inst.id)?;
                emit_mov_reg(asm, Reg::X16, Reg::X31);
                add_immediate_offset(asm, Reg::X16, offset as i64);
                store_vreg(asm, layout, inst.id, Reg::X16)?;
                asm.record_vreg_sp_offset(inst.id, offset);
            }
            AsmInstructionKind::Load { address, .. } => {
                if matches!(inst.ty, AsmType::Void) {
                    return Err(Error::from("load requires a concrete type"));
                }
                emit_load(asm, layout, inst.id, address, &inst.ty)?;
            }
            AsmInstructionKind::Store { value, address, .. } => {
                emit_store(
                    asm,
                    layout,
                    value,
                    address,
                    reg_types,
                    local_types,
                    rodata,
                    rodata_pool,
                )?;
            }
            AsmInstructionKind::GetElementPtr { ptr, indices, .. } => {
                emit_gep(asm, layout, inst.id, ptr, indices, reg_types, local_types)?;
            }
            AsmInstructionKind::SymbolAddress { symbol, kind } => {
                emit_load_symbol_addr_explicit(asm, Reg::X16, symbol.as_str(), *kind)?;
                store_vreg(asm, layout, inst.id, Reg::X16)?;
            }
            AsmInstructionKind::Call {
                function,
                args,
                calling_convention,
                ..
            } => {
                let ty = inst.ty.clone();
                emit_call(
                    asm,
                    layout,
                    inst.id,
                    function,
                    args,
                    calling_convention,
                    func_map,
                    &ty,
                    reg_types,
                    local_types,
                    format,
                    rodata,
                    rodata_pool,
                )?;
            }
            AsmInstructionKind::Syscall {
                convention,
                number,
                args,
            } => {
                let ty = inst.ty.clone();
                emit_syscall(
                    asm,
                    layout,
                    inst.id,
                    *convention,
                    number,
                    args,
                    &ty,
                    reg_types,
                    local_types,
                    format,
                )?;
            }
            AsmInstructionKind::IntrinsicCall {
                kind,
                format: format_str,
                args,
            } => {
                let ty = inst.ty.clone();
                emit_intrinsic_call(
                    asm,
                    layout,
                    inst.id,
                    kind,
                    format_str,
                    args,
                    &ty,
                    reg_types,
                    local_types,
                    rodata,
                    rodata_pool,
                    format,
                )?;
            }
            AsmInstructionKind::SIToFP(value, ty) => {
                emit_int_to_float(
                    asm,
                    layout,
                    inst.id,
                    value,
                    ty,
                    reg_types,
                    local_types,
                    true,
                )?;
            }
            AsmInstructionKind::UIToFP(value, ty) => {
                emit_int_to_float(
                    asm,
                    layout,
                    inst.id,
                    value,
                    ty,
                    reg_types,
                    local_types,
                    false,
                )?;
            }
            AsmInstructionKind::Trunc(value, ty) => {
                emit_trunc(asm, layout, inst.id, value, ty, reg_types, local_types)?;
            }
            AsmInstructionKind::ZExt(value, ty) => {
                emit_zext(asm, layout, inst.id, value, ty, reg_types, local_types)?;
            }
            AsmInstructionKind::FPToSI(value, ty) => {
                emit_float_to_int(
                    asm,
                    layout,
                    inst.id,
                    value,
                    ty,
                    reg_types,
                    local_types,
                    true,
                )?;
            }
            AsmInstructionKind::FPToUI(value, ty) => {
                emit_float_to_int(
                    asm,
                    layout,
                    inst.id,
                    value,
                    ty,
                    reg_types,
                    local_types,
                    false,
                )?;
            }
            AsmInstructionKind::FPTrunc(value, ty) => {
                emit_fp_trunc(asm, layout, inst.id, value, ty, reg_types, local_types)?;
            }
            AsmInstructionKind::FPExt(value, ty) => {
                emit_fp_ext(asm, layout, inst.id, value, ty, reg_types, local_types)?;
            }
            AsmInstructionKind::SExt(value, ty) => {
                emit_sext(asm, layout, inst.id, value, ty, reg_types, local_types)?;
            }
            AsmInstructionKind::SextOrTrunc(value, ty) => {
                emit_sext_or_trunc(asm, layout, inst.id, value, ty, reg_types, local_types)?;
            }
            AsmInstructionKind::Bitcast(value, ty) => {
                emit_bitcast(asm, layout, inst.id, value, ty, reg_types, local_types)?;
            }
            AsmInstructionKind::PtrToInt(value) => {
                emit_ptr_to_int(asm, layout, inst.id, value, reg_types, local_types)?;
            }
            AsmInstructionKind::IntToPtr(value) => {
                emit_int_to_ptr(asm, layout, inst.id, value, reg_types, local_types)?;
            }
            AsmInstructionKind::InsertValue {
                aggregate,
                element,
                indices,
            } => {
                emit_insert_value(
                    asm,
                    layout,
                    inst.id,
                    aggregate,
                    element,
                    indices,
                    reg_types,
                    local_types,
                    rodata,
                    rodata_pool,
                )?;
            }
            AsmInstructionKind::ExtractValue { aggregate, indices } => {
                emit_extract_value(
                    asm,
                    layout,
                    inst.id,
                    aggregate,
                    indices,
                    reg_types,
                    local_types,
                )?;
            }
            AsmInstructionKind::Select {
                condition,
                if_true,
                if_false,
            } => {
                emit_select(
                    asm,
                    layout,
                    inst.id,
                    condition,
                    if_true,
                    if_false,
                    reg_types,
                    local_types,
                )?;
            }
            AsmInstructionKind::LandingPad { result_type, .. } => {
                emit_landingpad(asm, layout, inst.id, result_type)?;
            }
            AsmInstructionKind::Freeze(value) => {
                emit_freeze(asm, layout, inst.id, value, reg_types, local_types)?;
            }
            AsmInstructionKind::InlineAsm { output_type, .. } => {
                emit_inline_asm(asm, layout, inst.id, output_type)?;
            }
            AsmInstructionKind::Unreachable => {
                emit_trap(asm);
            }
            other => {
                return Err(Error::from(format!(
                    "unsupported AsmIR instruction for aarch64: {other:?}"
                )));
            }
        }
    }

    match &block.terminator {
        AsmTerminator::Return(None) => {
            if asm.needs_frame {
                emit_epilogue(asm, layout);
            }
            if asm.entry_returns_exit && asm.is_entry() {
                emit_exit_syscall(asm, 0)?;
            } else {
                emit_mov_imm16(asm, Reg::X0, 0);
                emit_ret(asm);
            }
        }
        AsmTerminator::Return(Some(value)) => {
            let mut exit_reg = None;
            if returns_aggregate(return_ty, &layout.data_layout) {
                let sret_offset = layout
                    .sret_offset
                    .ok_or_else(|| Error::from("missing sret pointer for aggregate return"))?;
                emit_load_from_sp(asm, Reg::X17, sret_offset);
                match value {
                    AsmValue::Register(id) => {
                        let src_offset = agg_offset(layout, *id)?;
                        copy_sp_to_reg(asm, src_offset, Reg::X17, size_of(return_ty) as i32)?;
                    }
                    AsmValue::Local(id) => {
                        let src_offset = local_offset(layout, *id)?;
                        copy_sp_to_reg(asm, src_offset, Reg::X17, size_of(return_ty) as i32)?;
                    }
                    AsmValue::Constant(constant) => {
                        store_constant_aggregate_to_reg(
                            asm,
                            &layout.data_layout,
                            Reg::X17,
                            constant,
                            return_ty,
                            rodata,
                            rodata_pool,
                        )?;
                    }
                    _ => return Err(Error::from("unsupported aggregate return value")),
                }
                if asm.needs_frame {
                    emit_epilogue(asm, layout);
                }
                emit_ret(asm);
                return Ok(());
            }
            if matches!(return_ty, AsmType::I128) {
                load_i128_value(asm, layout, value, Reg::X0, Reg::X1, reg_types, local_types)?;
                exit_reg = Some(Reg::X0);
            } else if is_float_type(return_ty) {
                load_value_float(
                    asm,
                    layout,
                    value,
                    FReg::V0,
                    return_ty,
                    reg_types,
                    local_types,
                )?;
            } else {
                load_value(asm, layout, value, Reg::X0, reg_types, local_types)?;
                exit_reg = Some(Reg::X0);
            }
            if asm.needs_frame {
                emit_epilogue(asm, layout);
            }
            if asm.entry_returns_exit && asm.is_entry() {
                if let Some(reg) = exit_reg {
                    emit_exit_syscall_reg(asm, reg)?;
                } else {
                    emit_exit_syscall(asm, 0)?;
                }
            } else {
                emit_ret(asm);
            }
        }
        AsmTerminator::Br(target) => {
            asm.emit_b(Label::Block(asm.current_function, *target));
        }
        AsmTerminator::CondBr {
            condition,
            if_true,
            if_false,
        } => {
            emit_cond_branch(
                asm,
                layout,
                condition,
                Label::Block(asm.current_function, *if_true),
                Label::Block(asm.current_function, *if_false),
            )?;
        }
        AsmTerminator::Invoke {
            function,
            args,
            normal_dest,
            calling_convention,
            ..
        } => {
            emit_call(
                asm,
                layout,
                0,
                function,
                args,
                calling_convention,
                func_map,
                &AsmType::Void,
                reg_types,
                local_types,
                format,
                rodata,
                rodata_pool,
            )?;
            asm.emit_b(Label::Block(asm.current_function, *normal_dest));
        }
        AsmTerminator::Switch {
            value,
            default,
            cases,
        } => {
            emit_switch(asm, layout, value, *default, cases, reg_types, local_types)?;
        }
        AsmTerminator::IndirectBr {
            address,
            destinations,
        } => {
            // Some lifted artifacts (notably ELF PLT stubs) behave like a tail
            // call: they branch to a resolved target and expect that target to
            // return directly to the original caller. When we translate such a
            // stub into a standalone function it may still allocate a frame
            // for lifted register locals.
            //
            // If we `br` without tearing down our frame, the caller resumes
            // with a corrupted stack pointer. Heuristic: an indirect branch
            // with no known in-function destinations is treated as a tailcall.
            // Load the target address before any epilogue adjustment so we do
            // not lose access to stack-backed locals.
            load_value(asm, layout, address, Reg::X9, reg_types, local_types)?;

            if destinations.is_empty() && asm.needs_frame {
                emit_epilogue(asm, layout);
            }

            emit_br_reg(asm, Reg::X9);
        }
        AsmTerminator::Unreachable => {
            emit_trap(asm);
        }
        other => {
            return Err(Error::from(format!(
                "unsupported terminator for aarch64: {other:?}"
            )));
        }
    }

    Ok(())
}

fn emit_switch(
    asm: &mut Assembler,
    layout: &FrameLayout,
    value: &AsmValue,
    default: BasicBlockId,
    cases: &[(u64, BasicBlockId)],
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<()> {
    load_value(asm, layout, value, Reg::X16, reg_types, local_types)?;
    for (case_val, target) in cases {
        if *case_val <= 4095 {
            emit_cmp_imm12(asm, Reg::X16, *case_val as u32);
        } else {
            emit_mov_imm64(asm, Reg::X17, *case_val);
            emit_cmp_reg(asm, Reg::X16, Reg::X17);
        }
        asm.emit_b_cond(0, Label::Block(asm.current_function, *target));
    }
    asm.emit_b(Label::Block(asm.current_function, default));
    Ok(())
}

#[derive(Clone, Copy, Debug)]
struct Fixup {
    pos: usize,
    target: Label,
    kind: FixupKind,
}

#[derive(Clone, Copy, Debug)]
enum FixupKind {
    B,
    BCond(u32),
    Bl,
}

struct Assembler {
    buf: Vec<u8>,
    labels: HashMap<Label, usize>,
    fixups: Vec<Fixup>,
    needs_frame: bool,
    current_function: u32,
    relocs: Vec<Relocation>,
    target_format: TargetFormat,
    defined_symbols: HashSet<String>,
    entry_returns_exit: bool,
    current_layout: Option<LayoutContext>,
    vreg_sp_offsets: HashMap<u32, i32>,
    data_layout: LirDataLayout,
}

struct LayoutContext {
    func: String,
    _frame_size: i32,
    save_offset: i32,
}

fn emit_prologue(asm: &mut Assembler, layout: &FrameLayout) -> Result<()> {
    let frame = layout.frame_size;
    if frame > 0 {
        emit_adjust_sp(asm, frame, false);
    }
    let save_offset = layout.frame_size - 16;
    if save_offset > 504 {
        emit_mov_reg(asm, Reg::X16, Reg::X31);
        add_immediate_offset(asm, Reg::X16, save_offset as i64);
        emit_store_pair_base(asm, Reg::X16, Reg::X29, Reg::X30, 0);
    } else {
        emit_store_pair(asm, Reg::X29, Reg::X30, save_offset);
    }
    emit_mov_reg(asm, Reg::X29, Reg::X31);
    Ok(())
}

fn emit_epilogue(asm: &mut Assembler, layout: &FrameLayout) {
    emit_mov_reg(asm, Reg::X31, Reg::X29);
    let save_offset = layout.frame_size - 16;
    if save_offset > 504 {
        emit_mov_reg(asm, Reg::X16, Reg::X31);
        add_immediate_offset(asm, Reg::X16, save_offset as i64);
        emit_load_pair_base(asm, Reg::X16, Reg::X29, Reg::X30, 0);
    } else {
        emit_load_pair(asm, Reg::X29, Reg::X30, save_offset);
    }
    if layout.frame_size > 0 {
        emit_adjust_sp(asm, layout.frame_size, true);
    }
}

fn emit_panic_stub(asm: &mut Assembler, id: u32) {
    asm.needs_frame = false;
    asm.bind(Label::Function(id));
    asm.emit_bl_external("abort");
    emit_ret(asm);
}

impl Assembler {
    fn new(
        target_format: TargetFormat,
        defined_symbols: HashSet<String>,
        data_layout: LirDataLayout,
    ) -> Self {
        Self {
            buf: Vec::new(),
            labels: HashMap::new(),
            fixups: Vec::new(),
            needs_frame: false,
            current_function: 0,
            relocs: Vec::new(),
            target_format,
            defined_symbols,
            entry_returns_exit: false,
            current_layout: None,
            vreg_sp_offsets: HashMap::new(),
            data_layout,
        }
    }

    fn bind(&mut self, label: Label) {
        if let Label::Function(id) = label {
            self.current_function = id;
        }
        self.labels.insert(label, self.buf.len());
    }

    fn set_layout_context(&mut self, func: &str, frame_size: i32) {
        let save_offset = if frame_size > 0 { frame_size - 16 } else { -1 };
        self.current_layout = Some(LayoutContext {
            func: func.to_string(),
            _frame_size: frame_size,
            save_offset,
        });
        self.vreg_sp_offsets.clear();
    }

    fn clear_layout_context(&mut self) {
        self.current_layout = None;
        self.vreg_sp_offsets.clear();
    }

    fn record_vreg_sp_offset(&mut self, id: u32, offset: i32) {
        self.vreg_sp_offsets.insert(id, offset);
    }

    fn vreg_sp_offset(&self, id: u32) -> Option<i32> {
        self.vreg_sp_offsets.get(&id).copied()
    }

    fn log_stack_write(&self, offset: i32, size: i32, kind: &str) {
        if !stack_debug_enabled() || size <= 0 {
            return;
        }
        let Some(ctx) = self.current_layout.as_ref() else {
            return;
        };
        if ctx.save_offset < 0 {
            return;
        }
        let start = offset;
        let end = offset + size;
        let save_start = ctx.save_offset;
        let save_end = ctx.save_offset + 16;
        if start < save_end && end > save_start {
            eprintln!(
                "[fp-native][stack] {} write {} bytes at sp+{} overlaps save area [{}, {}) ({})",
                ctx.func, size, offset, save_start, save_end, kind
            );
        }
    }

    fn function_offsets(&self) -> HashMap<u32, u64> {
        let mut out = HashMap::new();
        for (label, pos) in &self.labels {
            if let Label::Function(id) = label {
                out.insert(*id, *pos as u64);
            }
        }
        out
    }

    fn is_entry(&self) -> bool {
        self.current_function == 0
    }

    fn emit_b(&mut self, target: Label) {
        let pos = self.buf.len();
        self.emit_u32(0x1400_0000);
        self.fixups.push(Fixup {
            pos,
            target,
            kind: FixupKind::B,
        });
    }

    fn emit_b_cond(&mut self, cond: u32, target: Label) {
        let pos = self.buf.len();
        self.emit_u32(0x5400_0000);
        self.fixups.push(Fixup {
            pos,
            target,
            kind: FixupKind::BCond(cond),
        });
    }

    fn emit_bl(&mut self, target: Label) {
        let pos = self.buf.len();
        self.emit_u32(0x9400_0000);
        self.fixups.push(Fixup {
            pos,
            target,
            kind: FixupKind::Bl,
        });
    }

    fn emit_bl_external(&mut self, symbol: &str) {
        let pos = self.buf.len();
        self.emit_u32(0x9400_0000);
        self.relocs.push(Relocation {
            offset: pos as u64,
            kind: RelocKind::CallRel32,
            section: crate::emit::RelocSection::Text,
            symbol: symbol.to_string(),
            addend: 0,
        });
    }

    fn emit_u32(&mut self, word: u32) {
        self.buf.extend_from_slice(&word.to_le_bytes());
    }

    fn extend(&mut self, bytes: &[u8]) {
        self.buf.extend_from_slice(bytes);
    }

    fn finish(mut self) -> Result<(Vec<u8>, Vec<Relocation>)> {
        let fixups = self.fixups.clone();
        for fixup in fixups {
            let target = self
                .labels
                .get(&fixup.target)
                .ok_or_else(|| Error::from("unknown branch target"))?;
            let origin = fixup.pos;
            let delta = (*target as i64) - (origin as i64);
            let imm = delta / 4;
            match fixup.kind {
                FixupKind::B => {
                    let imm26 = i32::try_from(imm)
                        .map_err(|e| Error::from(format!("branch out of range: {imm}: {e}")))?;
                    if imm26 < -(1 << 25) || imm26 > (1 << 25) - 1 {
                        return Err(Error::from("branch out of range"));
                    }
                    let encoded = 0x1400_0000u32 | ((imm26 as u32) & 0x03FF_FFFF);
                    self.patch_u32(origin, encoded);
                }
                FixupKind::BCond(cond) => {
                    let imm19 = i32::try_from(imm)
                        .map_err(|e| Error::from(format!("branch out of range: {imm}: {e}")))?;
                    if imm19 < -(1 << 18) || imm19 > (1 << 18) - 1 {
                        return Err(Error::from("conditional branch out of range"));
                    }
                    let encoded = 0x5400_0000u32 | (((imm19 as u32) & 0x7FFFF) << 5) | (cond & 0xF);
                    self.patch_u32(origin, encoded);
                }
                FixupKind::Bl => {
                    let imm26 = i32::try_from(imm)
                        .map_err(|e| Error::from(format!("branch out of range: {imm}: {e}")))?;
                    if imm26 < -(1 << 25) || imm26 > (1 << 25) - 1 {
                        return Err(Error::from("call target out of range"));
                    }
                    let encoded = 0x9400_0000u32 | ((imm26 as u32) & 0x03FF_FFFF);
                    self.patch_u32(origin, encoded);
                }
            }
        }
        Ok((self.buf, self.relocs))
    }

    fn patch_u32(&mut self, pos: usize, word: u32) {
        self.buf[pos..pos + 4].copy_from_slice(&word.to_le_bytes());
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum Label {
    Function(u32),
    Block(u32, BasicBlockId),
}

enum CallTarget {
    Internal(u32),
    External(String),
    Indirect,
}

fn build_function_map(program: &AsmProgram) -> Result<HashMap<String, u32>> {
    let mut map = HashMap::new();
    for (idx, func) in program.functions.iter().enumerate() {
        if func.is_declaration {
            continue;
        }
        let name = String::from(func.name.clone());
        map.insert(name, idx as u32);
    }
    Ok(map)
}

fn program_uses_fp_panic(program: &AsmProgram) -> bool {
    for func in &program.functions {
        for block in &func.basic_blocks {
            for inst in &block.instructions {
                if let AsmInstructionKind::Call { function, .. } = &inst.kind {
                    if matches!(function, AsmValue::Function(name) if name == "fp_panic") {
                        return true;
                    }
                }
            }
        }
    }
    false
}

impl Reg {
    fn is_sp(self) -> bool {
        matches!(self, Reg::X31)
    }
}

#[cfg(test)]
mod tests;
