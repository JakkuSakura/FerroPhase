use super::*;

pub(super) fn ensure_section(
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

pub(super) fn ensure_global(program: &mut AsmProgram, global: AsmGlobal) {
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

pub(super) fn ensure_function(program: &mut AsmProgram, function: AsmFunction) {
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

pub(super) fn resolve_u64(value: &AsmValue, instructions: &[AsmInstruction]) -> Option<u64> {
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

pub(super) fn resolve_i64(
    value: &AsmValue,
    instructions: &[AsmInstruction],
) -> Result<Option<i64>> {
    Ok(match value {
        AsmValue::Constant(AsmConstant::Int(x, _)) => Some(*x),
        AsmValue::Constant(AsmConstant::UInt(x, _)) => i64::try_from(*x).ok(),
        AsmValue::Register(id) => {
            let Some(inst) = instructions.iter().find(|inst| inst.id == *id) else {
                return Ok(None);
            };
            match &inst.kind {
                AsmInstructionKind::Freeze(inner) => resolve_i64(inner, instructions)?,
                _ => None,
            }
        }
        _ => None,
    })
}

pub(super) fn split_import_symbol(symbol: &str) -> (String, String) {
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
