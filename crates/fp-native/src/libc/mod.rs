#![allow(dead_code)]

use fp_core::asmir::{
    AsmAttr, AsmConstant, AsmFunction, AsmGenericOpcode, AsmGlobal, AsmInstrId, AsmInstruction,
    AsmObjectFormat, AsmOpcode, AsmOperand, AsmProgram, AsmRegister, AsmRegisterBank,
    AsmTerminator, AsmType, AsmVirtualRegId, OperandAccess,
};
use fp_core::lir::{CallingConvention, Linkage, Name, Visibility};
use std::collections::HashMap;

/// The operand index of a `Call` instruction's target, if `inst` is a
/// `Call`-opcode instruction. Mirrors `AsmInstruction::call_target_and_args`,
/// but returns just the index so callers can mutate `inst.operands` in
/// place (that method only hands back shared references).
fn call_target_index(inst: &AsmInstruction) -> Option<usize> {
    if !matches!(inst.opcode, AsmOpcode::Generic(AsmGenericOpcode::Call)) {
        return None;
    }
    inst.operands.iter().position(|op| {
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

/// Builds a map from virtual-register id to the (cloned) instruction that
/// defines it, for the given instructions. Used to chase `Freeze`/
/// `SymbolAddress`/`Call` definitions back to a symbolic origin.
fn build_def_by_reg<'a>(
    instructions: impl IntoIterator<Item = &'a AsmInstruction>,
) -> HashMap<AsmVirtualRegId, AsmInstruction> {
    let mut map = HashMap::new();
    for inst in instructions {
        if let Some(AsmRegister::Virtual(id)) = inst.result_register() {
            map.insert(*id, inst.clone());
        }
    }
    map
}

/// Target-agnostic normalization of lifted libc interactions.
///
/// This pass is expected to run after lifting to `AsmIR`, before target-specific
/// materialization.
pub fn normalize(program: &mut AsmProgram) {
    #[derive(Debug, Clone, Copy)]
    enum ArgRewrite {
        RemoveAt(usize),
        RemoveLast,
    }

    fn normalize_function_name(name: &str) -> Option<(&'static str, ArgRewrite)> {
        let name = name.split_once('@').map(|(head, _)| head).unwrap_or(name);
        Some(match name {
            "__fprintf_chk" => ("fprintf", ArgRewrite::RemoveAt(1)),
            "__vfprintf_chk" => ("vfprintf", ArgRewrite::RemoveAt(1)),
            "__printf_chk" => ("printf", ArgRewrite::RemoveAt(0)),
            "__vprintf_chk" => ("vprintf", ArgRewrite::RemoveAt(0)),
            "__dprintf_chk" => ("dprintf", ArgRewrite::RemoveAt(1)),

            // Common FORTIFY wrappers that are frequently emitted by glibc.
            "__memcpy_chk" => ("memcpy", ArgRewrite::RemoveLast),
            "__memmove_chk" => ("memmove", ArgRewrite::RemoveLast),
            "__memset_chk" => ("memset", ArgRewrite::RemoveLast),
            "__strcpy_chk" => ("strcpy", ArgRewrite::RemoveLast),
            "__stpcpy_chk" => ("stpcpy", ArgRewrite::RemoveLast),
            "__strncpy_chk" => ("strncpy", ArgRewrite::RemoveLast),
            "__strcat_chk" => ("strcat", ArgRewrite::RemoveLast),
            "__strncat_chk" => ("strncat", ArgRewrite::RemoveLast),
            "__sprintf_chk" => ("sprintf", ArgRewrite::RemoveAt(1)),
            "__snprintf_chk" => ("snprintf", ArgRewrite::RemoveAt(2)),
            "__vsprintf_chk" => ("vsprintf", ArgRewrite::RemoveAt(1)),
            "__vsnprintf_chk" => ("vsnprintf", ArgRewrite::RemoveAt(2)),
            _ => return None,
        })
    }

    fn apply_arg_rewrite(args: &mut Vec<AsmOperand>, rewrite: ArgRewrite) {
        match rewrite {
            ArgRewrite::RemoveAt(idx) => {
                if idx < args.len() {
                    args.remove(idx);
                }
            }
            ArgRewrite::RemoveLast => {
                args.pop();
            }
        }
    }

    fn is_small_int_constant(value: &AsmOperand) -> bool {
        match value {
            AsmOperand::Constant(AsmConstant::Int(v, _)) => (0..=2).contains(v),
            AsmOperand::Constant(AsmConstant::UInt(v, _)) => *v <= 2,
            AsmOperand::Constant(AsmConstant::Bool(_)) => true,
            _ => false,
        }
    }

    fn looks_like_chk_format_pointer(value: &AsmOperand) -> bool {
        // We don't have reliable full type information at this stage.
        // Use a conservative heuristic: format strings are almost never small integer immediates.
        !matches!(
            value,
            AsmOperand::Constant(AsmConstant::Int(_, _))
                | AsmOperand::Constant(AsmConstant::UInt(_, _))
                | AsmOperand::Constant(AsmConstant::Bool(_))
                | AsmOperand::Constant(AsmConstant::Null(_))
        )
    }

    /// Shared rewrite logic for both `Call`-opcode instructions and
    /// `Invoke` terminators: normalize FORTIFY (`__*_chk`) names, recover
    /// dropped `_chk`-style leading args, pick a calling convention, and
    /// materialize any recoverable format-string constant.
    fn normalize_call_like(
        target: &mut AsmOperand,
        args: &mut Vec<AsmOperand>,
        calling_convention: &mut CallingConvention,
        defined_calling_conventions: &HashMap<String, Option<CallingConvention>>,
        default_cc: &CallingConvention,
        globals: &[AsmGlobal],
        reg_defs: &HashMap<AsmVirtualRegId, RegDef>,
    ) {
        let mut apply_default_cc = false;
        if let AsmOperand::Symbol(name) = target {
            if let Some((normalized, rewrite)) = normalize_function_name(name.as_str()) {
                *name = Name::new(normalized);
                apply_arg_rewrite(args, rewrite);
                apply_default_cc = true;
            }

            // If earlier passes already rewrote `__*_chk` symbols to the
            // non-chk name but kept the original argument list, recover it here.
            match name.as_str() {
                "fprintf" | "vfprintf" | "dprintf" => {
                    if args.len() >= 3
                        && is_small_int_constant(&args[1])
                        && looks_like_chk_format_pointer(&args[2])
                    {
                        args.remove(1);
                    }
                }
                "printf" | "vprintf" => {
                    if args.len() >= 2
                        && is_small_int_constant(&args[0])
                        && looks_like_chk_format_pointer(&args[1])
                    {
                        args.remove(0);
                    }
                }
                _ => {}
            }

            if !apply_default_cc {
                match defined_calling_conventions.get(name.as_str()) {
                    Some(Some(cc)) => {
                        *calling_convention = cc.clone();
                        return;
                    }
                    Some(None) => {
                        // A defined function without an explicit
                        // calling convention should keep whatever
                        // the lifter recorded.
                        return;
                    }
                    None => {
                        apply_default_cc = true;
                    }
                }
            }
        }
        if apply_default_cc {
            *calling_convention = default_cc.clone();
        }

        materialize_format_string_from_elf_rodata(globals, target, args, reg_defs);
    }

    fn normalize_call_instruction(
        inst: &mut AsmInstruction,
        defined_calling_conventions: &HashMap<String, Option<CallingConvention>>,
        default_cc: &CallingConvention,
        globals: &[AsmGlobal],
        reg_defs: &HashMap<AsmVirtualRegId, RegDef>,
    ) {
        let Some(target_idx) = call_target_index(inst) else {
            return;
        };
        let Some(cc_idx) = inst
            .operands
            .iter()
            .position(|op| matches!(op, AsmOperand::Attr(AsmAttr::CallingConv(_))))
        else {
            return;
        };

        let mut target = inst.operands[target_idx].clone();
        let mut args: Vec<AsmOperand> = inst.operands.split_off(target_idx + 1);
        let mut calling_convention = match &inst.operands[cc_idx] {
            AsmOperand::Attr(AsmAttr::CallingConv(cc)) => cc.clone(),
            _ => return,
        };

        normalize_call_like(
            &mut target,
            &mut args,
            &mut calling_convention,
            defined_calling_conventions,
            default_cc,
            globals,
            reg_defs,
        );

        inst.operands[target_idx] = target;
        inst.operands[cc_idx] = AsmOperand::Attr(AsmAttr::CallingConv(calling_convention));
        inst.operands.truncate(target_idx + 1);
        inst.operands.extend(args);
    }

    let default_cc = program
        .target
        .default_calling_convention
        .clone()
        .unwrap_or(CallingConvention::C);

    let mut defined_calling_conventions: std::collections::HashMap<
        String,
        Option<CallingConvention>,
    > = std::collections::HashMap::new();
    for func in &program.functions {
        if func.is_declaration {
            continue;
        }
        defined_calling_conventions.insert(
            func.name.as_str().to_string(),
            func.calling_convention.clone(),
        );
    }

    let globals = &program.globals;

    for func in &mut program.functions {
        if func.is_declaration {
            continue;
        }
        let reg_defs = build_reg_defs(func);
        for block in &mut func.basic_blocks {
            for inst in &mut block.instructions {
                normalize_call_instruction(
                    inst,
                    &defined_calling_conventions,
                    &default_cc,
                    globals,
                    &reg_defs,
                );
            }
            if let AsmTerminator::Invoke {
                function,
                args,
                calling_convention,
                ..
            } = &mut block.terminator
            {
                normalize_call_like(
                    function,
                    args,
                    calling_convention,
                    &defined_calling_conventions,
                    &default_cc,
                    globals,
                    &reg_defs,
                );
            }
        }
    }
}

fn read_cstring_from_any_global(
    globals: &[AsmGlobal],
    global: &str,
    offset: i64,
) -> Option<String> {
    if offset < 0 {
        return None;
    }
    let offset = usize::try_from(offset).ok()?;
    let data = globals.iter().find(|g| g.name.as_str() == global)?;
    let bytes = global_bytes(data)?;
    if offset >= bytes.len() {
        return None;
    }
    let rest = &bytes[offset..];
    let nul = rest.iter().position(|byte| *byte == 0)?;
    std::str::from_utf8(&rest[..nul])
        .ok()
        .map(|s| s.to_string())
}

fn materialize_format_string_from_elf_rodata(
    globals: &[AsmGlobal],
    target: &AsmOperand,
    args: &mut [AsmOperand],
    reg_defs: &HashMap<AsmVirtualRegId, RegDef>,
) {
    let callee = match target {
        AsmOperand::Symbol(name) => name.as_str(),
        _ => return,
    };

    let format_arg_idx = match callee {
        "printf" => 0,
        "fprintf" => 1,
        _ => return,
    };

    let Some(format_arg) = args.get(format_arg_idx) else {
        return;
    };
    if matches!(format_arg, AsmOperand::Constant(AsmConstant::String(_))) {
        return;
    }

    if let AsmOperand::Constant(AsmConstant::GlobalRef(name, _, indices)) = format_arg {
        if indices.iter().all(|idx| *idx == 0) {
            if let Some(text) = read_cstring_from_any_global(globals, name.as_str(), 0) {
                args[format_arg_idx] = AsmOperand::Constant(AsmConstant::String(text));
                return;
            }
        }
    }

    let Some(text) = resolve_cstring_from_elf_rodata(globals, reg_defs, format_arg) else {
        return;
    };
    args[format_arg_idx] = AsmOperand::Constant(AsmConstant::String(text));
}

fn resolve_cstring_from_elf_rodata(
    globals: &[AsmGlobal],
    reg_defs: &HashMap<AsmVirtualRegId, RegDef>,
    value: &AsmOperand,
) -> Option<String> {
    let (global, offset) = resolve_rodata_pointer(reg_defs, value)?;
    read_cstring_from_global_bytes(globals, global.as_str(), offset)
}

fn resolve_rodata_pointer(
    reg_defs: &HashMap<AsmVirtualRegId, RegDef>,
    value: &AsmOperand,
) -> Option<(String, i64)> {
    let AsmOperand::Register {
        reg: AsmRegister::Virtual(id),
        ..
    } = value
    else {
        return None;
    };
    resolve_rodata_pointer_from_reg(reg_defs, *id, 0)
}

fn resolve_rodata_pointer_from_reg(
    reg_defs: &HashMap<AsmVirtualRegId, RegDef>,
    id: AsmVirtualRegId,
    accumulated_offset: i64,
) -> Option<(String, i64)> {
    match reg_defs.get(&id)? {
        RegDef::Freeze(source) => {
            let AsmOperand::Register {
                reg: AsmRegister::Virtual(source_id),
                ..
            } = source
            else {
                return None;
            };
            resolve_rodata_pointer_from_reg(reg_defs, *source_id, accumulated_offset)
        }
        RegDef::GlobalRef { name } => Some((name.clone(), accumulated_offset)),
        RegDef::Add { base, offset } => {
            let accumulated_offset = accumulated_offset.checked_add(*offset)?;
            resolve_rodata_pointer_from_reg(reg_defs, *base, accumulated_offset)
        }
    }
}

fn read_cstring_from_global_bytes(
    globals: &[AsmGlobal],
    global: &str,
    offset: i64,
) -> Option<String> {
    if offset < 0 {
        return None;
    }
    if !global.starts_with("fp_elf_rodata_") {
        return None;
    }
    let offset = usize::try_from(offset).ok()?;
    let data = globals.iter().find(|g| g.name.as_str() == global)?;
    let bytes = global_bytes(data)?;
    if offset >= bytes.len() {
        return None;
    }
    let rest = &bytes[offset..];
    let nul = rest.iter().position(|byte| *byte == 0)?;
    std::str::from_utf8(&rest[..nul])
        .ok()
        .map(|s| s.to_string())
}

#[derive(Clone, Debug)]
enum RegDef {
    Freeze(AsmOperand),
    GlobalRef { name: String },
    Add { base: AsmVirtualRegId, offset: i64 },
}

fn build_reg_defs(func: &AsmFunction) -> HashMap<AsmVirtualRegId, RegDef> {
    let mut defs = HashMap::new();
    for block in &func.basic_blocks {
        for inst in &block.instructions {
            let Some(AsmRegister::Virtual(dest_id)) = inst.result_register().cloned() else {
                continue;
            };
            match &inst.opcode {
                AsmOpcode::Generic(AsmGenericOpcode::Freeze) => {
                    // Operand schema: [Write dest, Read src].
                    let Some(value) = inst.operands.get(1) else {
                        continue;
                    };
                    if let AsmOperand::Constant(AsmConstant::GlobalRef(name, _, indices)) = value {
                        if indices.iter().all(|idx| *idx == 0) {
                            defs.insert(
                                dest_id,
                                RegDef::GlobalRef {
                                    name: name.to_string(),
                                },
                            );
                            continue;
                        }
                    }
                    defs.insert(dest_id, RegDef::Freeze(value.clone()));
                }
                AsmOpcode::Generic(AsmGenericOpcode::Add) => {
                    // Operand schema: [Write dest, Read lhs, Read rhs].
                    let (Some(lhs), Some(rhs)) = (inst.operands.get(1), inst.operands.get(2))
                    else {
                        continue;
                    };
                    let AsmOperand::Register {
                        reg: AsmRegister::Virtual(base),
                        ..
                    } = lhs
                    else {
                        continue;
                    };
                    let AsmOperand::Constant(constant) = rhs else {
                        continue;
                    };
                    let Some(offset) = constant_to_i64(constant) else {
                        continue;
                    };
                    defs.insert(
                        dest_id,
                        RegDef::Add {
                            base: *base,
                            offset,
                        },
                    );
                }
                _ => {}
            }
        }
    }
    defs
}

fn constant_to_i64(constant: &AsmConstant) -> Option<i64> {
    match constant {
        AsmConstant::Int(value, _) => Some(*value),
        AsmConstant::UInt(value, _) => i64::try_from(*value).ok(),
        AsmConstant::Bool(value) => Some(if *value { 1 } else { 0 }),
        _ => None,
    }
}

/// Target-specific materialization of normalized libc interactions.
///
/// This pass is expected to run just before emission.
pub fn materialize(program: &mut AsmProgram) {
    if program.target.object_format != AsmObjectFormat::MachO {
        return;
    }
    let Some(container) = program.container.as_ref() else {
        return;
    };
    // Only apply when we are cross-materializing an ELF binary onto Mach-O.
    if container.format != AsmObjectFormat::Elf {
        return;
    }

    materialize_darwin_stdio(program);
    materialize_darwin_getopt_globals(program);
    materialize_darwin_progname(program);
    materialize_disable_darwin_cxa_atexit(program);
    materialize_rewrite_darwin_exit(program);
}

fn materialize_darwin_getopt_globals(program: &mut AsmProgram) {
    let targets: [(&str, AsmType, u32); 4] = [
        ("optind", AsmType::I32, 4),
        ("opterr", AsmType::I32, 4),
        ("optopt", AsmType::I32, 4),
        ("optarg", AsmType::Ptr(Box::new(AsmType::I8)), 8),
    ];
    for (name, ty, align) in targets {
        for global in &mut program.globals {
            if global.name.as_str() != name {
                continue;
            }
            global.ty = ty.clone();
            global.clear_initializer();
            global.section = None;
            global.linkage = Linkage::External;
            global.visibility = Visibility::Default;
            global.alignment = Some(align);
            global.is_constant = false;
        }
    }
}

/// Whether `value` refers (possibly through a `Freeze` or `SymbolAddress`
/// definition) to the bare symbol name `symbol` (ignoring any `@version`
/// suffix).
fn operand_targets_symbol(
    def_by_reg: &HashMap<AsmVirtualRegId, AsmInstruction>,
    value: &AsmOperand,
    symbol: &str,
) -> bool {
    fn bare(name: &str) -> &str {
        name.split_once('@').map(|(head, _)| head).unwrap_or(name)
    }

    match value {
        AsmOperand::Symbol(name) => bare(name.as_str()) == symbol,
        AsmOperand::Register {
            reg: AsmRegister::Virtual(id),
            ..
        } => match def_by_reg.get(id) {
            Some(inst) if matches!(inst.opcode, AsmOpcode::Generic(AsmGenericOpcode::Freeze)) => {
                inst
                    .operands
                    .get(1)
                    .is_some_and(|src| operand_targets_symbol(def_by_reg, src, symbol))
            }
            Some(inst)
                if matches!(
                    inst.opcode,
                    AsmOpcode::Generic(AsmGenericOpcode::SymbolAddress)
                ) =>
            {
                inst.operands.iter().any(|op| {
                    matches!(op, AsmOperand::Symbol(name) if bare(name.as_str()) == symbol)
                })
            }
            _ => false,
        },
        _ => false,
    }
}

fn materialize_rewrite_darwin_exit(program: &mut AsmProgram) {
    for func in &mut program.functions {
        if func.is_declaration {
            continue;
        }
        let def_by_reg = build_def_by_reg(
            func.basic_blocks
                .iter()
                .flat_map(|block| block.instructions.iter()),
        );
        for block in &mut func.basic_blocks {
            for inst in &mut block.instructions {
                if let Some(target_idx) = call_target_index(inst) {
                    if operand_targets_symbol(&def_by_reg, &inst.operands[target_idx], "exit") {
                        inst.operands[target_idx] = AsmOperand::Symbol(Name::new("_exit"));
                    }
                }
            }
            if let AsmTerminator::Invoke { function, .. } = &mut block.terminator {
                if operand_targets_symbol(&def_by_reg, function, "exit") {
                    *function = AsmOperand::Symbol(Name::new("_exit"));
                }
            }
        }
    }
}

fn materialize_disable_darwin_cxa_atexit(program: &mut AsmProgram) {
    fn ensure_stub(program: &mut AsmProgram) {
        if program
            .functions
            .iter()
            .any(|func| func.name.as_str() == "fp_noop_cxa_atexit")
        {
            return;
        }
        let mut function = AsmFunction::new(
            Name::new("fp_noop_cxa_atexit"),
            fp_core::asmir::AsmFunctionSignature {
                params: vec![
                    AsmType::Ptr(Box::new(AsmType::I8)),
                    AsmType::Ptr(Box::new(AsmType::I8)),
                    AsmType::Ptr(Box::new(AsmType::I8)),
                ],
                return_type: AsmType::I32,
                is_variadic: false,
            },
        );
        function.locals = vec![
            fp_core::asmir::AsmLocal {
                id: 0,
                name: Some("destructor".to_string()),
                ty: AsmType::Ptr(Box::new(AsmType::I8)),
                is_argument: true,
            },
            fp_core::asmir::AsmLocal {
                id: 1,
                name: Some("arg".to_string()),
                ty: AsmType::Ptr(Box::new(AsmType::I8)),
                is_argument: true,
            },
            fp_core::asmir::AsmLocal {
                id: 2,
                name: Some("dso_handle".to_string()),
                ty: AsmType::Ptr(Box::new(AsmType::I8)),
                is_argument: true,
            },
        ];
        function.linkage = Linkage::External;
        function.visibility = Visibility::Default;
        function.calling_convention = Some(CallingConvention::C);
        function.section = Some(".text".to_string());
        function.basic_blocks = vec![fp_core::asmir::AsmBlock {
            id: 0,
            label: None,
            instructions: Vec::new(),
            terminator: AsmTerminator::Return(Some(AsmOperand::Constant(AsmConstant::Int(
                0,
                AsmType::I32,
            )))),
            terminator_encoding: None,
            predecessors: Vec::new(),
            successors: Vec::new(),
        }];
        program.functions.push(function);
    }

    ensure_stub(program);

    for func in &mut program.functions {
        if func.is_declaration {
            continue;
        }
        for block in &mut func.basic_blocks {
            let def_by_reg = build_def_by_reg(block.instructions.iter());
            for inst in &mut block.instructions {
                if let Some(target_idx) = call_target_index(inst) {
                    if operand_targets_symbol(
                        &def_by_reg,
                        &inst.operands[target_idx],
                        "__cxa_atexit",
                    ) {
                        inst.operands[target_idx] =
                            AsmOperand::Symbol(Name::new("fp_noop_cxa_atexit"));
                    }
                }
            }
            if let AsmTerminator::Invoke { function, .. } = &mut block.terminator {
                if operand_targets_symbol(&def_by_reg, function, "__cxa_atexit") {
                    *function = AsmOperand::Symbol(Name::new("fp_noop_cxa_atexit"));
                }
            }
        }
    }
}

fn materialize_darwin_progname(program: &mut AsmProgram) {
    fn cstring_from_bytes(init: &[u8]) -> Option<String> {
        let nul = init.iter().position(|byte| *byte == 0)?;
        std::str::from_utf8(&init[..nul])
            .ok()
            .map(|s| s.to_string())
    }

    let global_cstrings: HashMap<String, String> = program
        .globals
        .iter()
        .filter_map(|global| {
            let name = global.name.as_str();
            if !name.starts_with("fp_str_") {
                return None;
            }
            let init = global_bytes(global)?;
            let text = cstring_from_bytes(init)?;
            Some((name.to_string(), text))
        })
        .collect();

    fn resolve_cstring_from_value(
        global_cstrings: &HashMap<String, String>,
        def_by_reg: &HashMap<AsmVirtualRegId, AsmInstruction>,
        value: &AsmOperand,
    ) -> Option<String> {
        match value {
            AsmOperand::Constant(AsmConstant::String(text)) => Some(text.clone()),
            AsmOperand::Constant(AsmConstant::GlobalRef(name, _, indices)) => {
                if indices.iter().all(|idx| *idx == 0) {
                    global_cstrings.get(name.as_str()).cloned()
                } else {
                    None
                }
            }
            AsmOperand::Symbol(name) => global_cstrings.get(name.as_str()).cloned(),
            AsmOperand::Register {
                reg: AsmRegister::Virtual(id),
                ..
            } => {
                let inst = def_by_reg.get(id)?;
                match &inst.opcode {
                    AsmOpcode::Generic(AsmGenericOpcode::Freeze) => {
                        let source = inst.operands.get(1)?;
                        resolve_cstring_from_value(global_cstrings, def_by_reg, source)
                    }
                    AsmOpcode::Generic(AsmGenericOpcode::Call) => {
                        let (target, args) = inst.call_target_and_args()?;
                        let AsmOperand::Symbol(name) = target else {
                            return None;
                        };
                        let msgid_idx = match name.as_str() {
                            "gettext" => 0,
                            "dgettext" | "dcgettext" => 1,
                            _ => return None,
                        };
                        let msgid = args.get(msgid_idx)?;
                        resolve_cstring_from_value(global_cstrings, def_by_reg, msgid)
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }

    fn is_try_help_format(text: &str) -> bool {
        text.contains("Try '%s --help'") || text.starts_with("Usage: %s")
    }

    for func in &mut program.functions {
        if func.is_declaration {
            continue;
        }

        let mut next_id: AsmInstrId = func
            .basic_blocks
            .iter()
            .flat_map(|bb| bb.instructions.iter().map(|inst| inst.id))
            .max()
            .unwrap_or(0)
            .saturating_add(1);

        // Take ownership of the block list so that `func` (specifically
        // `func.alloc_virtual_register`) can still be borrowed mutably from
        // inside the loop below without conflicting with a live borrow of
        // `func.basic_blocks`.
        let mut blocks = std::mem::take(&mut func.basic_blocks);
        for block in &mut blocks {
            let mut rewritten = Vec::with_capacity(block.instructions.len());
            let mut original = std::mem::take(&mut block.instructions);
            let def_by_reg = build_def_by_reg(original.iter());

            for mut inst in original.drain(..) {
                if !matches!(inst.opcode, AsmOpcode::Generic(AsmGenericOpcode::Call)) {
                    rewritten.push(inst);
                    continue;
                }

                // Resolves (progname_idx, args_start) if `inst` is a
                // printf/fprintf call whose format string is a recognized
                // "Try '%s --help'"/"Usage: %s" pattern with room for a
                // progname argument. Kept as a closure (rather than inline
                // early-`continue`s) since it only borrows `inst`
                // immutably, while the fallback/success paths below need to
                // consume or mutate `inst`.
                let resolved = (|| -> Option<(usize, usize)> {
                    let (target, args) = inst.call_target_and_args()?;
                    let AsmOperand::Symbol(name) = target else {
                        return None;
                    };
                    let (format_idx, progname_idx) = match name.as_str() {
                        "printf" => (0usize, 1usize),
                        "fprintf" => (1usize, 2usize),
                        _ => return None,
                    };
                    let format_arg = args.get(format_idx)?;
                    let text =
                        resolve_cstring_from_value(&global_cstrings, &def_by_reg, format_arg)?;
                    if !is_try_help_format(&text) {
                        return None;
                    }
                    if args.len() <= progname_idx {
                        return None;
                    }
                    let args_start = inst.operands.len() - args.len();
                    Some((progname_idx, args_start))
                })();

                let Some((progname_idx, args_start)) = resolved else {
                    rewritten.push(inst);
                    continue;
                };

                let getprogname_id = next_id;
                next_id += 1;
                let dest_vreg = func.alloc_virtual_register(
                    AsmType::Ptr(Box::new(AsmType::I8)),
                    AsmRegisterBank::General,
                    64,
                );
                rewritten.push(AsmInstruction::new(
                    getprogname_id,
                    AsmOpcode::Generic(AsmGenericOpcode::Call),
                    vec![
                        AsmOperand::Register {
                            reg: AsmRegister::Virtual(dest_vreg),
                            access: OperandAccess::Write,
                        },
                        AsmOperand::Attr(AsmAttr::CallingConv(CallingConvention::C)),
                        AsmOperand::Symbol(Name::new("getprogname")),
                    ],
                ));

                inst.operands[args_start + progname_idx] = AsmOperand::Register {
                    reg: AsmRegister::Virtual(dest_vreg),
                    access: OperandAccess::Read,
                };
                if let Some(cc_idx) = inst
                    .operands
                    .iter()
                    .position(|op| matches!(op, AsmOperand::Attr(AsmAttr::CallingConv(_))))
                {
                    inst.operands[cc_idx] =
                        AsmOperand::Attr(AsmAttr::CallingConv(CallingConvention::C));
                }
                inst.operands
                    .retain(|op| !matches!(op, AsmOperand::Attr(AsmAttr::TailCall)));
                rewritten.push(inst);
            }
            block.instructions = rewritten;
        }
        func.basic_blocks = blocks;
    }
}

fn map_stdio_symbol(name: &str) -> Option<&'static str> {
    Some(match name {
        // Direct glibc globals.
        "stderr" => "__stderrp",
        "stdout" => "__stdoutp",
        "stdin" => "__stdinp",

        _ => return None,
    })
}

fn materialize_darwin_stdio(program: &mut AsmProgram) {
    fn rewrite_operand(operand: &mut AsmOperand) {
        match operand {
            AsmOperand::Constant(constant) => rewrite_constant(constant),
            AsmOperand::Symbol(name) => {
                if let Some(mapped) = map_stdio_symbol(name.as_str()) {
                    *name = Name::new(mapped);
                }
            }
            AsmOperand::SysOp(op) => rewrite_sysop(op),
            _ => {}
        }
    }

    fn rewrite_constant(constant: &mut AsmConstant) {
        match constant {
            AsmConstant::Array(values, _) | AsmConstant::Struct(values, _) => {
                for value in values {
                    rewrite_constant(value);
                }
            }
            AsmConstant::GlobalRef(name, _, _) => {
                if let Some(mapped) = map_stdio_symbol(name.as_str()) {
                    *name = Name::new(mapped);
                }
            }
            _ => {}
        }
    }

    fn rewrite_sysop(op: &mut fp_core::asmir::AsmSysOp) {
        use fp_core::asmir::AsmSysOp;
        match op {
            AsmSysOp::Exit { code } => rewrite_operand(code),
            AsmSysOp::GetPid | AsmSysOp::GetTid => {}
            AsmSysOp::Dlopen { path, flags } => {
                rewrite_operand(path);
                rewrite_operand(flags);
            }
            AsmSysOp::Dlsym { handle, symbol } => {
                rewrite_operand(handle);
                rewrite_operand(symbol);
            }
            AsmSysOp::Dlclose { handle } => rewrite_operand(handle),
            AsmSysOp::Unlink { path } | AsmSysOp::Rmdir { path } | AsmSysOp::Opendir { path } => {
                rewrite_operand(path)
            }
            AsmSysOp::Mkdir { path, mode } => {
                rewrite_operand(path);
                rewrite_operand(mode);
            }
            AsmSysOp::Rename { from, to } => {
                rewrite_operand(from);
                rewrite_operand(to);
            }
            AsmSysOp::Access { path, mode } => {
                rewrite_operand(path);
                rewrite_operand(mode);
            }
            AsmSysOp::Write { fd, buffer, len } | AsmSysOp::Read { fd, buffer, len } => {
                rewrite_operand(fd);
                rewrite_operand(buffer);
                rewrite_operand(len);
            }
            AsmSysOp::Close { fd } => rewrite_operand(fd),
            AsmSysOp::Open {
                path, flags, mode, ..
            } => {
                rewrite_operand(path);
                rewrite_operand(flags);
                rewrite_operand(mode);
            }
            AsmSysOp::Seek { fd, offset, whence } => {
                rewrite_operand(fd);
                rewrite_operand(offset);
                rewrite_operand(whence);
            }
            AsmSysOp::Mmap {
                addr,
                len,
                prot,
                flags,
                fd,
                offset,
            } => {
                rewrite_operand(addr);
                rewrite_operand(len);
                rewrite_operand(prot);
                rewrite_operand(flags);
                rewrite_operand(fd);
                rewrite_operand(offset);
            }
            AsmSysOp::Munmap { addr, len } => {
                rewrite_operand(addr);
                rewrite_operand(len);
            }
            AsmSysOp::Readdir { dir, .. } | AsmSysOp::Closedir { dir } => rewrite_operand(dir),
        }
    }

    fn rewrite_terminator(terminator: &mut AsmTerminator) {
        match terminator {
            AsmTerminator::Return(Some(value)) => rewrite_operand(value),
            AsmTerminator::CondBr { condition, .. } => rewrite_operand(condition),
            AsmTerminator::Switch { value, .. } => rewrite_operand(value),
            AsmTerminator::IndirectBr { address, .. } => rewrite_operand(address),
            AsmTerminator::Invoke { function, args, .. } => {
                rewrite_operand(function);
                for arg in args {
                    rewrite_operand(arg);
                }
            }
            AsmTerminator::Resume(value) => rewrite_operand(value),
            AsmTerminator::CleanupRet { cleanup_pad, .. } => rewrite_operand(cleanup_pad),
            AsmTerminator::CatchRet { catch_pad, .. } => rewrite_operand(catch_pad),
            AsmTerminator::CatchSwitch { parent_pad, .. } => {
                if let Some(parent_pad) = parent_pad.as_mut() {
                    rewrite_operand(parent_pad);
                }
            }
            _ => {}
        }
    }

    for global in &mut program.globals {
        for reloc in &mut global.relocations {
            if let Some(mapped) = map_stdio_symbol(reloc.symbol.as_str()) {
                reloc.symbol = Name::new(mapped);
            }
        }
    }

    for func in &mut program.functions {
        if func.is_declaration {
            continue;
        }
        for block in &mut func.basic_blocks {
            for inst in &mut block.instructions {
                for operand in &mut inst.operands {
                    rewrite_operand(operand);
                }
            }
            rewrite_terminator(&mut block.terminator);
        }
    }
}

fn global_bytes(global: &AsmGlobal) -> Option<&[u8]> {
    match global.initializer.as_ref()? {
        AsmConstant::Bytes(bytes) => Some(bytes.as_slice()),
        _ => None,
    }
}
