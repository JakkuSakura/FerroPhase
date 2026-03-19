use fp_core::asmir::{
    AsmConstant, AsmGenericOpcode, AsmInstruction, AsmInstructionKind, AsmObjectFormat, AsmOpcode,
    AsmProgram, AsmTerminator, AsmType, AsmValue,
};
use fp_core::lir::{CallingConvention, Linkage, Visibility};
use std::collections::HashMap;

/// Target-agnostic normalization of lifted libc interactions.
///
/// This pass is expected to run after lifting to `AsmIR`, before target-specific
/// materialization.
pub fn normalize(program: &mut AsmProgram) {
    #[derive(Debug, Clone, Copy)]
    enum ArgRewrite {
        None,
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

    fn apply_arg_rewrite(args: &mut Vec<AsmValue>, rewrite: ArgRewrite) {
        match rewrite {
            ArgRewrite::None => {}
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

    fn is_small_int_constant(value: &AsmValue) -> bool {
        match value {
            AsmValue::Constant(AsmConstant::Int(v, _)) => (0..=2).contains(v),
            AsmValue::Constant(AsmConstant::UInt(v, _)) => *v <= 2,
            AsmValue::Constant(AsmConstant::Bool(_)) => true,
            _ => false,
        }
    }

    fn looks_like_chk_format_pointer(value: &AsmValue) -> bool {
        // We don't have reliable full type information at this stage.
        // Use a conservative heuristic: format strings are almost never small integer immediates.
        !matches!(
            value,
            AsmValue::Constant(AsmConstant::Int(_, _))
                | AsmValue::Constant(AsmConstant::UInt(_, _))
                | AsmValue::Constant(AsmConstant::Bool(_))
                | AsmValue::Null(_)
        )
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
                if let AsmInstructionKind::Call {
                    function,
                    args,
                    calling_convention,
                    ..
                } = &mut inst.kind
                {
                    let mut apply_default_cc = false;
                    if let AsmValue::Function(name) = function {
                        if let Some((normalized, rewrite)) = normalize_function_name(name.as_str())
                        {
                            *name = normalized.to_string();
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
                                    continue;
                                }
                                Some(None) => {
                                    // A defined function without an explicit
                                    // calling convention should keep whatever
                                    // the lifter recorded.
                                    continue;
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

                    materialize_format_string_from_elf_rodata(globals, function, args, &reg_defs);
                }
            }
            if let AsmTerminator::Invoke {
                function,
                args,
                calling_convention,
                ..
            } = &mut block.terminator
            {
                let mut apply_default_cc = false;
                if let AsmValue::Function(name) = function {
                    if let Some((normalized, rewrite)) = normalize_function_name(name.as_str()) {
                        *name = normalized.to_string();
                        apply_arg_rewrite(args, rewrite);
                        apply_default_cc = true;
                    }

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
                                continue;
                            }
                            Some(None) => {
                                continue;
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

                materialize_format_string_from_elf_rodata(globals, function, args, &reg_defs);
            }
        }
    }
}

fn read_cstring_from_any_global(
    globals: &[fp_core::asmir::AsmGlobal],
    global: &str,
    offset: i64,
) -> Option<String> {
    if offset < 0 {
        return None;
    }
    let offset = usize::try_from(offset).ok()?;
    let init = globals
        .iter()
        .find(|g| g.name.as_str() == global)?
        .initializer
        .as_ref()?;
    let bytes = match init {
        AsmConstant::Bytes(bytes) => bytes.as_slice(),
        _ => return None,
    };
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
    globals: &[fp_core::asmir::AsmGlobal],
    function: &AsmValue,
    args: &mut Vec<AsmValue>,
    reg_defs: &HashMap<u32, RegDef>,
) {
    let callee = match function {
        AsmValue::Function(name) => name.as_str(),
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
    if matches!(format_arg, AsmValue::Constant(AsmConstant::String(_))) {
        return;
    }

    if let AsmValue::Constant(AsmConstant::GlobalRef(name, _, indices)) = format_arg {
        if indices.iter().all(|idx| *idx == 0) {
            if let Some(text) = read_cstring_from_any_global(globals, name.as_str(), 0) {
                args[format_arg_idx] = AsmValue::Constant(AsmConstant::String(text));
                return;
            }
        }
    }

    let Some(text) = resolve_cstring_from_elf_rodata(globals, reg_defs, format_arg) else {
        return;
    };
    args[format_arg_idx] = AsmValue::Constant(AsmConstant::String(text));
}

fn resolve_cstring_from_elf_rodata(
    globals: &[fp_core::asmir::AsmGlobal],
    reg_defs: &HashMap<u32, RegDef>,
    value: &AsmValue,
) -> Option<String> {
    let (global, offset) = resolve_rodata_pointer(reg_defs, value)?;
    read_cstring_from_global_bytes(globals, global.as_str(), offset)
}

fn resolve_rodata_pointer(
    reg_defs: &HashMap<u32, RegDef>,
    value: &AsmValue,
) -> Option<(String, i64)> {
    let AsmValue::Register(id) = value else {
        return None;
    };
    resolve_rodata_pointer_from_reg(reg_defs, *id, 0)
}

fn resolve_rodata_pointer_from_reg(
    reg_defs: &HashMap<u32, RegDef>,
    id: u32,
    accumulated_offset: i64,
) -> Option<(String, i64)> {
    match reg_defs.get(&id)? {
        RegDef::Freeze(source) => {
            let AsmValue::Register(source_id) = source else {
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
    globals: &[fp_core::asmir::AsmGlobal],
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
    let init = globals
        .iter()
        .find(|g| g.name.as_str() == global)?
        .initializer
        .as_ref()?;
    let bytes = match init {
        AsmConstant::Bytes(bytes) => bytes.as_slice(),
        _ => return None,
    };
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
    Freeze(AsmValue),
    GlobalRef { name: String },
    Add { base: u32, offset: i64 },
}

fn build_reg_defs(func: &fp_core::asmir::AsmFunction) -> HashMap<u32, RegDef> {
    let mut defs = HashMap::new();
    for block in &func.basic_blocks {
        for inst in &block.instructions {
            match &inst.kind {
                AsmInstructionKind::Freeze(value) => {
                    if let AsmValue::Constant(AsmConstant::GlobalRef(name, _, indices)) = value {
                        if indices.iter().all(|idx| *idx == 0) {
                            defs.insert(
                                inst.id,
                                RegDef::GlobalRef {
                                    name: name.to_string(),
                                },
                            );
                            continue;
                        }
                    }
                    defs.insert(inst.id, RegDef::Freeze(value.clone()));
                }
                AsmInstructionKind::Add(lhs, rhs) => {
                    let AsmValue::Register(base) = lhs else {
                        continue;
                    };
                    let AsmValue::Constant(constant) = rhs else {
                        continue;
                    };
                    let Some(offset) = constant_to_i64(constant) else {
                        continue;
                    };
                    defs.insert(
                        inst.id,
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
            global.initializer = None;
            global.relocations.clear();
            global.section = None;
            global.linkage = Linkage::External;
            global.visibility = Visibility::Default;
            global.alignment = Some(align);
            global.is_constant = false;
        }
    }
}

fn materialize_rewrite_darwin_exit(program: &mut AsmProgram) {
    fn refers_to_symbol(
        inst_by_id: &HashMap<u32, AsmInstructionKind>,
        value: &AsmValue,
        symbol: &str,
    ) -> bool {
        match value {
            AsmValue::Global(name, _) => name == symbol,
            AsmValue::Constant(AsmConstant::GlobalRef(name, _, indices)) => {
                indices.iter().all(|idx| *idx == 0) && name.as_str() == symbol
            }
            AsmValue::Address(address) => {
                address
                    .base
                    .as_ref()
                    .is_some_and(|base| refers_to_symbol(inst_by_id, base, symbol))
                    || address
                        .index
                        .as_ref()
                        .is_some_and(|index| refers_to_symbol(inst_by_id, index, symbol))
                    || address
                        .segment
                        .as_ref()
                        .is_some_and(|segment| refers_to_symbol(inst_by_id, segment, symbol))
            }
            AsmValue::Register(id) => match inst_by_id.get(id) {
                Some(AsmInstructionKind::Freeze(inner)) => {
                    refers_to_symbol(inst_by_id, inner, symbol)
                }
                Some(AsmInstructionKind::SymbolAddress { symbol: target, .. }) => target == symbol,
                _ => false,
            },
            _ => false,
        }
    }

    fn call_target_is_exit(
        inst_by_id: &HashMap<u32, AsmInstructionKind>,
        value: &AsmValue,
    ) -> bool {
        match value {
            AsmValue::Function(name) => {
                name.split_once('@').map(|(h, _)| h).unwrap_or(name) == "exit"
            }
            AsmValue::Register(id) => match inst_by_id.get(id) {
                Some(AsmInstructionKind::Freeze(inner)) => call_target_is_exit(inst_by_id, inner),
                Some(AsmInstructionKind::SymbolAddress { symbol, .. }) => {
                    symbol.split_once('@').map(|(h, _)| h).unwrap_or(symbol) == "exit"
                }
                _ => false,
            },
            _ => false,
        }
    }

    for func in &mut program.functions {
        if func.is_declaration {
            continue;
        }
        let inst_by_id = func
            .basic_blocks
            .iter()
            .flat_map(|block| block.instructions.iter())
            .map(|inst| (inst.id, inst.kind.clone()))
            .collect::<HashMap<_, _>>();
        for block in &mut func.basic_blocks {
            for inst in &mut block.instructions {
                if let AsmInstructionKind::Call { function, .. } = &mut inst.kind {
                    if call_target_is_exit(&inst_by_id, function) {
                        *function = AsmValue::Function("_exit".to_string());
                    }
                }
            }
            if let AsmTerminator::Invoke { function, .. } = &mut block.terminator {
                if call_target_is_exit(&inst_by_id, function) {
                    *function = AsmValue::Function("_exit".to_string());
                }
            }
        }
    }
}

fn materialize_disable_darwin_cxa_atexit(program: &mut AsmProgram) {
    fn refers_to_symbol(
        inst_by_id: &HashMap<u32, AsmInstructionKind>,
        value: &AsmValue,
        symbol: &str,
    ) -> bool {
        match value {
            AsmValue::Global(name, _) => name == symbol,
            AsmValue::Constant(AsmConstant::GlobalRef(name, _, indices)) => {
                indices.iter().all(|idx| *idx == 0) && name.as_str() == symbol
            }
            AsmValue::Address(address) => {
                address
                    .base
                    .as_ref()
                    .is_some_and(|base| refers_to_symbol(inst_by_id, base, symbol))
                    || address
                        .index
                        .as_ref()
                        .is_some_and(|index| refers_to_symbol(inst_by_id, index, symbol))
                    || address
                        .segment
                        .as_ref()
                        .is_some_and(|segment| refers_to_symbol(inst_by_id, segment, symbol))
            }
            AsmValue::Register(id) => match inst_by_id.get(id) {
                Some(AsmInstructionKind::Freeze(inner)) => {
                    refers_to_symbol(inst_by_id, inner, symbol)
                }
                Some(AsmInstructionKind::SymbolAddress { symbol: target, .. }) => target == symbol,
                _ => false,
            },
            _ => false,
        }
    }

    fn call_target_is_cxa_atexit(
        inst_by_id: &HashMap<u32, AsmInstructionKind>,
        value: &AsmValue,
    ) -> bool {
        match value {
            AsmValue::Function(name) => {
                name.split_once('@').map(|(h, _)| h).unwrap_or(name) == "__cxa_atexit"
            }
            AsmValue::Register(id) => match inst_by_id.get(id) {
                Some(AsmInstructionKind::Freeze(inner)) => {
                    call_target_is_cxa_atexit(inst_by_id, inner)
                }
                Some(AsmInstructionKind::SymbolAddress { symbol, .. }) => {
                    symbol.split_once('@').map(|(h, _)| h).unwrap_or(symbol) == "__cxa_atexit"
                }
                _ => false,
            },
            _ => false,
        }
    }

    fn ensure_stub(program: &mut AsmProgram) {
        if program
            .functions
            .iter()
            .any(|func| func.name.as_str() == "fp_noop_cxa_atexit")
        {
            return;
        }
        program.functions.push(fp_core::asmir::AsmFunction {
            name: fp_core::lir::Name::new("fp_noop_cxa_atexit"),
            signature: fp_core::asmir::AsmFunctionSignature {
                params: vec![
                    AsmType::Ptr(Box::new(AsmType::I8)),
                    AsmType::Ptr(Box::new(AsmType::I8)),
                    AsmType::Ptr(Box::new(AsmType::I8)),
                ],
                return_type: AsmType::I32,
                is_variadic: false,
            },
            basic_blocks: vec![fp_core::asmir::AsmBlock {
                id: 0,
                label: None,
                instructions: Vec::new(),
                terminator: AsmTerminator::Return(Some(AsmValue::Constant(AsmConstant::Int(
                    0,
                    AsmType::I32,
                )))),
                terminator_encoding: None,
                predecessors: Vec::new(),
                successors: Vec::new(),
            }],
            locals: vec![
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
            ],
            stack_slots: Vec::new(),
            frame: None,
            linkage: fp_core::lir::Linkage::External,
            visibility: fp_core::lir::Visibility::Default,
            calling_convention: Some(CallingConvention::C),
            section: Some(".text".to_string()),
            is_declaration: false,
        });
    }

    ensure_stub(program);

    for func in &mut program.functions {
        if func.is_declaration {
            continue;
        }
        for block in &mut func.basic_blocks {
            let inst_by_id: HashMap<u32, AsmInstructionKind> = block
                .instructions
                .iter()
                .map(|inst| (inst.id, inst.kind.clone()))
                .collect();
            for inst in &mut block.instructions {
                if let AsmInstructionKind::Call { function, .. } = &mut inst.kind {
                    if call_target_is_cxa_atexit(&inst_by_id, function) {
                        *function = AsmValue::Function("fp_noop_cxa_atexit".to_string());
                    }
                }
            }
            if let AsmTerminator::Invoke { function, .. } = &mut block.terminator {
                if call_target_is_cxa_atexit(&inst_by_id, function) {
                    *function = AsmValue::Function("fp_noop_cxa_atexit".to_string());
                }
            }
        }
    }
}

fn materialize_darwin_progname(program: &mut AsmProgram) {
    fn cstring_from_initializer(init: &AsmConstant) -> Option<String> {
        match init {
            AsmConstant::Bytes(bytes) => {
                let nul = bytes.iter().position(|byte| *byte == 0)?;
                std::str::from_utf8(&bytes[..nul])
                    .ok()
                    .map(|s| s.to_string())
            }
            AsmConstant::Array(values, _) | AsmConstant::Struct(values, _) => {
                let mut bytes = Vec::new();
                for elem in values {
                    let byte = match elem {
                        AsmConstant::UInt(v, _) => *v as u8,
                        AsmConstant::Int(v, _) => *v as u8,
                        AsmConstant::Bool(v) => {
                            if *v {
                                1
                            } else {
                                0
                            }
                        }
                        _ => return None,
                    };
                    if byte == 0 {
                        break;
                    }
                    bytes.push(byte);
                }
                std::str::from_utf8(&bytes).ok().map(|s| s.to_string())
            }
            _ => None,
        }
    }

    let global_cstrings: HashMap<String, String> = program
        .globals
        .iter()
        .filter_map(|global| {
            let name = global.name.as_str();
            if !name.starts_with("fp_str_") {
                return None;
            }
            let init = global.initializer.as_ref()?;
            let text = cstring_from_initializer(init)?;
            Some((name.to_string(), text))
        })
        .collect();

    fn resolve_cstring_from_value(
        global_cstrings: &HashMap<String, String>,
        inst_kinds: &HashMap<u32, AsmInstructionKind>,
        value: &AsmValue,
    ) -> Option<String> {
        match value {
            AsmValue::Constant(AsmConstant::String(text)) => Some(text.clone()),
            AsmValue::Constant(AsmConstant::GlobalRef(name, _, indices)) => {
                if indices.iter().all(|idx| *idx == 0) {
                    global_cstrings.get(name.as_str()).cloned()
                } else {
                    None
                }
            }
            AsmValue::Global(name, _) => global_cstrings.get(name).cloned(),
            AsmValue::Register(id) => {
                let kind = inst_kinds.get(id)?;
                match kind {
                    AsmInstructionKind::Freeze(source) => {
                        resolve_cstring_from_value(global_cstrings, inst_kinds, source)
                    }
                    AsmInstructionKind::Call { function, args, .. } => {
                        let AsmValue::Function(name) = function else {
                            return None;
                        };
                        let msgid_idx = match name.as_str() {
                            "gettext" => 0,
                            "dgettext" | "dcgettext" => 1,
                            _ => return None,
                        };
                        let msgid = args.get(msgid_idx)?;
                        resolve_cstring_from_value(global_cstrings, inst_kinds, msgid)
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

        let mut next_id = func
            .basic_blocks
            .iter()
            .flat_map(|bb| bb.instructions.iter().map(|inst| inst.id))
            .max()
            .unwrap_or(0)
            .saturating_add(1);

        for block in &mut func.basic_blocks {
            let mut rewritten = Vec::with_capacity(block.instructions.len());
            let mut original = std::mem::take(&mut block.instructions);
            let inst_kinds: HashMap<u32, AsmInstructionKind> = original
                .iter()
                .map(|inst| (inst.id, inst.kind.clone()))
                .collect();

            for mut inst in original.drain(..) {
                let AsmInstructionKind::Call {
                    function,
                    args,
                    calling_convention,
                    tail_call,
                } = &mut inst.kind
                else {
                    rewritten.push(inst);
                    continue;
                };

                let AsmValue::Function(name) = function else {
                    rewritten.push(inst);
                    continue;
                };

                let (format_idx, progname_idx) = match name.as_str() {
                    "printf" => (0usize, 1usize),
                    "fprintf" => (1usize, 2usize),
                    _ => {
                        rewritten.push(inst);
                        continue;
                    }
                };

                let Some(format_arg) = args.get(format_idx) else {
                    rewritten.push(inst);
                    continue;
                };
                let Some(text) =
                    resolve_cstring_from_value(&global_cstrings, &inst_kinds, format_arg)
                else {
                    rewritten.push(inst);
                    continue;
                };

                if !is_try_help_format(&text) {
                    rewritten.push(inst);
                    continue;
                }

                if args.len() <= progname_idx {
                    rewritten.push(inst);
                    continue;
                }

                let getprogname_id = next_id;
                next_id += 1;
                rewritten.push(AsmInstruction {
                    id: getprogname_id,
                    opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                    kind: AsmInstructionKind::Call {
                        function: AsmValue::Function("getprogname".to_string()),
                        args: Vec::new(),
                        calling_convention: CallingConvention::C,
                        tail_call: false,
                    },
                    type_hint: Some(AsmType::Ptr(Box::new(AsmType::I8))),
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                });

                args[progname_idx] = AsmValue::Register(getprogname_id);
                *calling_convention = CallingConvention::C;
                *tail_call = false;
                rewritten.push(inst);
            }
            block.instructions = rewritten;
        }
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
    fn rewrite_value(value: &mut AsmValue) {
        match value {
            AsmValue::Constant(constant) => {
                rewrite_constant(constant);
            }
            AsmValue::Global(name, ty) => {
                if let Some(mapped) = map_stdio_symbol(name.as_str()) {
                    *name = mapped.to_string();
                    // Preserve the original type to avoid cascading type-hint
                    // changes; the underlying value will be produced by `Load`.
                    let _ = ty;
                }
            }
            AsmValue::Address(address) => {
                if let Some(base) = address.base.as_mut() {
                    rewrite_value(base);
                }
                if let Some(index) = address.index.as_mut() {
                    rewrite_value(index);
                }
                if let Some(segment) = address.segment.as_mut() {
                    rewrite_value(segment);
                }
            }
            AsmValue::Comparison(comparison) => {
                rewrite_value(&mut comparison.lhs);
                rewrite_value(&mut comparison.rhs);
            }
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
                    *name = fp_core::lir::Name::new(mapped);
                }
            }
            _ => {}
        }
    }

    fn rewrite_instruction_kind(kind: &mut AsmInstructionKind) {
        match kind {
            AsmInstructionKind::Nop => {}
            AsmInstructionKind::Add(a, b)
            | AsmInstructionKind::Sub(a, b)
            | AsmInstructionKind::Mul(a, b)
            | AsmInstructionKind::Div(a, b)
            | AsmInstructionKind::Rem(a, b)
            | AsmInstructionKind::And(a, b)
            | AsmInstructionKind::Or(a, b)
            | AsmInstructionKind::Xor(a, b)
            | AsmInstructionKind::Shl(a, b)
            | AsmInstructionKind::Shr(a, b)
            | AsmInstructionKind::Eq(a, b)
            | AsmInstructionKind::Ne(a, b)
            | AsmInstructionKind::Lt(a, b)
            | AsmInstructionKind::Le(a, b)
            | AsmInstructionKind::Gt(a, b)
            | AsmInstructionKind::Ge(a, b)
            | AsmInstructionKind::Ult(a, b)
            | AsmInstructionKind::Ule(a, b)
            | AsmInstructionKind::Ugt(a, b)
            | AsmInstructionKind::Uge(a, b) => {
                rewrite_value(a);
                rewrite_value(b);
            }
            AsmInstructionKind::Not(a)
            | AsmInstructionKind::Freeze(a)
            | AsmInstructionKind::PtrToInt(a)
            | AsmInstructionKind::IntToPtr(a)
            | AsmInstructionKind::ExtractLane { vector: a, .. } => {
                rewrite_value(a);
            }
            AsmInstructionKind::Load { address, .. } => rewrite_value(address),
            AsmInstructionKind::Store { value, address, .. } => {
                rewrite_value(value);
                rewrite_value(address);
            }
            AsmInstructionKind::Alloca { size, .. } => rewrite_value(size),
            AsmInstructionKind::GetElementPtr { ptr, indices, .. } => {
                rewrite_value(ptr);
                for index in indices {
                    rewrite_value(index);
                }
            }
            AsmInstructionKind::Bitcast(value, _)
            | AsmInstructionKind::Trunc(value, _)
            | AsmInstructionKind::ZExt(value, _)
            | AsmInstructionKind::SExt(value, _)
            | AsmInstructionKind::FPExt(value, _)
            | AsmInstructionKind::FPTrunc(value, _)
            | AsmInstructionKind::FPToUI(value, _)
            | AsmInstructionKind::FPToSI(value, _)
            | AsmInstructionKind::UIToFP(value, _)
            | AsmInstructionKind::SIToFP(value, _)
            | AsmInstructionKind::SextOrTrunc(value, _) => rewrite_value(value),
            AsmInstructionKind::ExtractValue { aggregate, .. } => rewrite_value(aggregate),
            AsmInstructionKind::InsertValue {
                aggregate, element, ..
            } => {
                rewrite_value(aggregate);
                rewrite_value(element);
            }
            AsmInstructionKind::Call { function, args, .. } => {
                rewrite_value(function);
                for arg in args {
                    rewrite_value(arg);
                }
            }
            AsmInstructionKind::IntrinsicCall { args, .. } => {
                for arg in args {
                    rewrite_value(arg);
                }
            }
            AsmInstructionKind::Phi { incoming } => {
                for (value, _) in incoming {
                    rewrite_value(value);
                }
            }
            AsmInstructionKind::Select {
                condition,
                if_true,
                if_false,
            } => {
                rewrite_value(condition);
                rewrite_value(if_true);
                rewrite_value(if_false);
            }
            AsmInstructionKind::InlineAsm { inputs, .. } => {
                for value in inputs {
                    rewrite_value(value);
                }
            }
            AsmInstructionKind::LandingPad {
                personality,
                clauses,
                ..
            } => {
                if let Some(personality) = personality.as_mut() {
                    rewrite_value(personality);
                }
                for clause in clauses {
                    match clause {
                        fp_core::asmir::AsmLandingPadClause::Catch(value) => rewrite_value(value),
                        fp_core::asmir::AsmLandingPadClause::Filter(values) => {
                            for value in values {
                                rewrite_value(value);
                            }
                        }
                    }
                }
            }
            AsmInstructionKind::Syscall { number, args, .. } => {
                rewrite_value(number);
                for value in args {
                    rewrite_value(value);
                }
            }
            AsmInstructionKind::Splat { value, .. } => rewrite_value(value),
            AsmInstructionKind::BuildVector { elements } => {
                for value in elements {
                    rewrite_value(value);
                }
            }
            AsmInstructionKind::InsertLane { vector, value, .. } => {
                rewrite_value(vector);
                rewrite_value(value);
            }
            AsmInstructionKind::ZipLow { lhs, rhs, .. } => {
                rewrite_value(lhs);
                rewrite_value(rhs);
            }
            AsmInstructionKind::SymbolAddress { symbol, .. } => {
                if let Some(mapped) = map_stdio_symbol(symbol.as_str()) {
                    *symbol = mapped.to_string();
                }
            }
            AsmInstructionKind::Unreachable => {}
            AsmInstructionKind::SysOp(_) => {}
        }
    }

    fn rewrite_terminator(terminator: &mut AsmTerminator) {
        match terminator {
            AsmTerminator::Return(Some(value)) => rewrite_value(value),
            AsmTerminator::CondBr { condition, .. } => rewrite_value(condition),
            AsmTerminator::Switch { value, .. } => rewrite_value(value),
            AsmTerminator::IndirectBr { address, .. } => rewrite_value(address),
            AsmTerminator::Invoke { function, args, .. } => {
                rewrite_value(function);
                for arg in args {
                    rewrite_value(arg);
                }
            }
            AsmTerminator::Resume(value) => rewrite_value(value),
            AsmTerminator::CleanupRet { cleanup_pad, .. } => rewrite_value(cleanup_pad),
            AsmTerminator::CatchRet { catch_pad, .. } => rewrite_value(catch_pad),
            AsmTerminator::CatchSwitch { parent_pad, .. } => {
                if let Some(parent_pad) = parent_pad.as_mut() {
                    rewrite_value(parent_pad);
                }
            }
            _ => {}
        }
    }

    for global in &mut program.globals {
        if let Some(initializer) = global.initializer.as_mut() {
            rewrite_constant(initializer);
        }
        for reloc in &mut global.relocations {
            if let Some(mapped) = map_stdio_symbol(reloc.symbol.as_str()) {
                reloc.symbol = fp_core::lir::Name::new(mapped);
            }
        }
    }

    for func in &mut program.functions {
        if func.is_declaration {
            continue;
        }
        for block in &mut func.basic_blocks {
            let mut idx = 0usize;
            while idx < block.instructions.len() {
                rewrite_instruction_kind(&mut block.instructions[idx].kind);
                idx += 1;
            }
            rewrite_terminator(&mut block.terminator);
        }
    }
}
