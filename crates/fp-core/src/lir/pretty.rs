use std::fmt::{self, Formatter};

use crate::pretty::{PrettyCtx, PrettyPrintable};

use super::ty::Ty;
use super::{
    CallingConvention, Linkage, LirBasicBlock, LirConstant, LirFunction, LirGlobal, LirInstruction,
    LirInstructionKind, LirProgram, LirTerminator, LirValue, Visibility,
};

impl PrettyPrintable for LirProgram {
    fn fmt_pretty(&self, f: &mut Formatter<'_>, ctx: &mut PrettyCtx<'_>) -> fmt::Result {
        ctx.writeln(f, "lir::Program {")?;
        ctx.with_indent(|ctx| {
            if !self.type_definitions.is_empty() {
                ctx.writeln(f, "types:")?;
                ctx.with_indent(|ctx| {
                    for typedef in &self.type_definitions {
                        ctx.writeln(
                            f,
                            format!("%{} = {}", typedef.name, format_type(&typedef.ty)),
                        )?;
                    }
                    Ok(())
                })?;
            }

            if !self.globals.is_empty() {
                ctx.writeln(f, "globals:")?;
                ctx.with_indent(|ctx| {
                    for global in &self.globals {
                        write_global(global, f, ctx)?;
                    }
                    Ok(())
                })?;
            }

            if !self.functions.is_empty() {
                ctx.writeln(f, "functions:")?;
                ctx.with_indent(|ctx| {
                    for (idx, func) in self.functions.iter().enumerate() {
                        write_function(func, f, ctx)?;
                        if idx + 1 < self.functions.len() {
                            writeln!(f)?;
                        }
                    }
                    Ok(())
                })?;
            }

            Ok(())
        })?;
        ctx.writeln(f, "}")
    }
}

fn write_global(global: &LirGlobal, f: &mut Formatter<'_>, ctx: &mut PrettyCtx<'_>) -> fmt::Result {
    let mut line = format!("@{}: {}", global.name, format_type(&global.ty));
    line.push_str(&format!(" [linkage: {}]", format_linkage(&global.linkage)));
    line.push_str(&format!(
        " [visibility: {}]",
        format_visibility(&global.visibility)
    ));
    if global.is_constant {
        line.push_str(" const");
    }
    if let Some(align) = global.alignment {
        line.push_str(&format!(" align {}", align));
    }
    if let Some(section) = &global.section {
        line.push_str(&format!(" section \"{}\"", section));
    }
    if let Some(initializer) = &global.initializer {
        line.push_str(" = ");
        line.push_str(&format_constant(initializer));
    }
    ctx.writeln(f, line)
}

fn write_function(
    func: &LirFunction,
    f: &mut Formatter<'_>,
    ctx: &mut PrettyCtx<'_>,
) -> fmt::Result {
    let params = func
        .signature
        .params
        .iter()
        .enumerate()
        .map(|(idx, ty)| format!("arg{}: {}", idx, format_type(ty)))
        .collect::<Vec<_>>()
        .join(", ");
    let header = format!(
        "fn {}({}) -> {} [cc: {}, linkage: {}] {{",
        func.name,
        params,
        format_type(&func.signature.return_type),
        format_calling_convention(&func.calling_convention),
        format_linkage(&func.linkage)
    );

    ctx.writeln(f, header)?;
    ctx.with_indent(|ctx| {
        if !func.locals.is_empty() {
            ctx.writeln(f, "locals:")?;
            ctx.with_indent(|ctx| {
                for local in &func.locals {
                    let mut line = format!("%{}: {}", local.id, format_type(&local.ty));
                    if let Some(name) = &local.name {
                        line.push_str(&format!(" // name: {}", name));
                    }
                    if local.is_argument {
                        line.push_str(" // arg");
                    }
                    ctx.writeln(f, line)?;
                }
                Ok(())
            })?;
        }

        if !func.stack_slots.is_empty() {
            ctx.writeln(f, "stack_slots:")?;
            ctx.with_indent(|ctx| {
                for slot in &func.stack_slots {
                    let mut line = format!(
                        "slot {}: size {}, align {}",
                        slot.id, slot.size, slot.alignment
                    );
                    if let Some(name) = &slot.name {
                        line.push_str(&format!(" // name: {}", name));
                    }
                    ctx.writeln(f, line)?;
                }
                Ok(())
            })?;
        }

        for block in &func.basic_blocks {
            write_block(block, f, ctx)?;
        }

        Ok(())
    })?;
    ctx.writeln(f, "}")
}

fn write_block(
    block: &LirBasicBlock,
    f: &mut Formatter<'_>,
    ctx: &mut PrettyCtx<'_>,
) -> fmt::Result {
    let mut header = format!("bb{}", block.id);
    if let Some(label) = &block.label {
        header.push_str(&format!(" // label: {}", label));
    }
    if !block.predecessors.is_empty() {
        let preds = block
            .predecessors
            .iter()
            .map(|id| format!("bb{}", id))
            .collect::<Vec<_>>()
            .join(", ");
        header.push_str(&format!(" // preds: [{}]", preds));
    }
    if !block.successors.is_empty() {
        let succs = block
            .successors
            .iter()
            .map(|id| format!("bb{}", id))
            .collect::<Vec<_>>()
            .join(", ");
        header.push_str(&format!(" // succs: [{}]", succs));
    }
    ctx.writeln(f, header + ":")?;
    ctx.with_indent(|ctx| {
        for inst in &block.instructions {
            let mut line = format!("i{}: {}", inst.id, summarize_instruction(inst));
            if ctx.options.show_types {
                if let Some(ty) = &inst.type_hint {
                    line.push_str(&format!(" : {}", format_type(ty)));
                }
            }
            ctx.writeln(f, line)?;
        }
        ctx.writeln(
            f,
            format!("terminator: {}", summarize_terminator(&block.terminator)),
        )
    })
}

fn summarize_instruction(inst: &LirInstruction) -> String {
    use LirInstructionKind::*;

    match &inst.kind {
        Add(lhs, rhs) => format!(
            "%r{} = add {}, {}",
            inst.id,
            format_value(lhs),
            format_value(rhs)
        ),
        Sub(lhs, rhs) => format!(
            "%r{} = sub {}, {}",
            inst.id,
            format_value(lhs),
            format_value(rhs)
        ),
        Mul(lhs, rhs) => format!(
            "%r{} = mul {}, {}",
            inst.id,
            format_value(lhs),
            format_value(rhs)
        ),
        Div(lhs, rhs) => format!(
            "%r{} = div {}, {}",
            inst.id,
            format_value(lhs),
            format_value(rhs)
        ),
        Rem(lhs, rhs) => format!(
            "%r{} = rem {}, {}",
            inst.id,
            format_value(lhs),
            format_value(rhs)
        ),
        And(lhs, rhs) => format!(
            "%r{} = and {}, {}",
            inst.id,
            format_value(lhs),
            format_value(rhs)
        ),
        Or(lhs, rhs) => format!(
            "%r{} = or {}, {}",
            inst.id,
            format_value(lhs),
            format_value(rhs)
        ),
        Xor(lhs, rhs) => format!(
            "%r{} = xor {}, {}",
            inst.id,
            format_value(lhs),
            format_value(rhs)
        ),
        Shl(lhs, rhs) => format!(
            "%r{} = shl {}, {}",
            inst.id,
            format_value(lhs),
            format_value(rhs)
        ),
        Shr(lhs, rhs) => format!(
            "%r{} = shr {}, {}",
            inst.id,
            format_value(lhs),
            format_value(rhs)
        ),
        Not(value) => format!("%r{} = not {}", inst.id, format_value(value)),
        Eq(lhs, rhs) => format!(
            "%r{} = eq {}, {}",
            inst.id,
            format_value(lhs),
            format_value(rhs)
        ),
        Ne(lhs, rhs) => format!(
            "%r{} = ne {}, {}",
            inst.id,
            format_value(lhs),
            format_value(rhs)
        ),
        Lt(lhs, rhs) => format!(
            "%r{} = lt {}, {}",
            inst.id,
            format_value(lhs),
            format_value(rhs)
        ),
        Le(lhs, rhs) => format!(
            "%r{} = le {}, {}",
            inst.id,
            format_value(lhs),
            format_value(rhs)
        ),
        Gt(lhs, rhs) => format!(
            "%r{} = gt {}, {}",
            inst.id,
            format_value(lhs),
            format_value(rhs)
        ),
        Ge(lhs, rhs) => format!(
            "%r{} = ge {}, {}",
            inst.id,
            format_value(lhs),
            format_value(rhs)
        ),
        Load {
            address,
            alignment,
            volatile,
        } => {
            let mut text = format!("%r{} = load {}", inst.id, format_value(address));
            if let Some(align) = alignment {
                text.push_str(&format!(" align {}", align));
            }
            if *volatile {
                text.push_str(" volatile");
            }
            text
        }
        Store {
            value,
            address,
            alignment,
            volatile,
        } => {
            let mut text = format!("store {}, {}", format_value(value), format_value(address));
            if let Some(align) = alignment {
                text.push_str(&format!(" align {}", align));
            }
            if *volatile {
                text.push_str(" volatile");
            }
            text
        }
        Alloca { size, alignment } => {
            format!(
                "%r{} = alloca {} align {}",
                inst.id,
                format_value(size),
                alignment
            )
        }
        GetElementPtr {
            ptr,
            indices,
            inbounds,
        } => {
            let prefix = if *inbounds { "gep inbounds" } else { "gep" };
            let mut text = format!("%r{} = {} {}", inst.id, prefix, format_value(ptr));
            if !indices.is_empty() {
                let idx = indices
                    .iter()
                    .map(format_value)
                    .collect::<Vec<_>>()
                    .join(", ");
                text.push_str(&format!(" [{}]", idx));
            }
            text
        }
        PtrToInt(value) => format!("%r{} = ptrtoint {}", inst.id, format_value(value)),
        IntToPtr(value) => format!("%r{} = inttoptr {}", inst.id, format_value(value)),
        Trunc(value, ty) => format!(
            "%r{} = trunc {} to {}",
            inst.id,
            format_value(value),
            format_type(ty)
        ),
        ZExt(value, ty) => format!(
            "%r{} = zext {} to {}",
            inst.id,
            format_value(value),
            format_type(ty)
        ),
        SExt(value, ty) => format!(
            "%r{} = sext {} to {}",
            inst.id,
            format_value(value),
            format_type(ty)
        ),
        FPTrunc(value, ty) => format!(
            "%r{} = fptrunc {} to {}",
            inst.id,
            format_value(value),
            format_type(ty)
        ),
        FPExt(value, ty) => format!(
            "%r{} = fpext {} to {}",
            inst.id,
            format_value(value),
            format_type(ty)
        ),
        FPToUI(value, ty) => format!(
            "%r{} = fptoui {} to {}",
            inst.id,
            format_value(value),
            format_type(ty)
        ),
        FPToSI(value, ty) => format!(
            "%r{} = fptosi {} to {}",
            inst.id,
            format_value(value),
            format_type(ty)
        ),
        UIToFP(value, ty) => format!(
            "%r{} = uitofp {} to {}",
            inst.id,
            format_value(value),
            format_type(ty)
        ),
        SIToFP(value, ty) => format!(
            "%r{} = sitofp {} to {}",
            inst.id,
            format_value(value),
            format_type(ty)
        ),
        Bitcast(value, ty) => format!(
            "%r{} = bitcast {} to {}",
            inst.id,
            format_value(value),
            format_type(ty)
        ),
        ExtractValue { aggregate, indices } => {
            let idx = indices
                .iter()
                .map(|i| i.to_string())
                .collect::<Vec<_>>()
                .join(", ");
            format!(
                "%r{} = extractvalue {}, [{}]",
                inst.id,
                format_value(aggregate),
                idx
            )
        }
        InsertValue {
            aggregate,
            element,
            indices,
        } => {
            let idx = indices
                .iter()
                .map(|i| i.to_string())
                .collect::<Vec<_>>()
                .join(", ");
            format!(
                "%r{} = insertvalue {}, {}, [{}]",
                inst.id,
                format_value(aggregate),
                format_value(element),
                idx
            )
        }
        Call {
            function,
            args,
            calling_convention,
            tail_call,
        } => {
            let mut text = String::new();
            if *tail_call {
                text.push_str("tail ");
            }
            text.push_str(&format!(
                "%r{} = call {} {}",
                inst.id,
                format_calling_convention(calling_convention),
                format_value(function)
            ));
            let args = args.iter().map(format_value).collect::<Vec<_>>().join(", ");
            text.push_str(&format!("({})", args));
            text
        }
        IntrinsicCall { kind, format, args } => {
            let args = args.iter().map(format_value).collect::<Vec<_>>().join(", ");
            format!(
                "%r{} = intrinsic {:?} \"{}\" ({})",
                inst.id, kind, format, args
            )
        }
        SextOrTrunc(value, ty) => {
            format!(
                "%r{} = sext_or_trunc {} to {}",
                inst.id,
                format_value(value),
                format_type(ty)
            )
        }
        Phi { incoming } => {
            let arms = incoming
                .iter()
                .map(|(val, bb)| format!("[{}, bb{}]", format_value(val), bb))
                .collect::<Vec<_>>()
                .join(", ");
            format!("%r{} = phi {}", inst.id, arms)
        }
        Select {
            condition,
            if_true,
            if_false,
        } => format!(
            "%r{} = select {}, {}, {}",
            inst.id,
            format_value(condition),
            format_value(if_true),
            format_value(if_false)
        ),
        InlineAsm {
            asm_string,
            constraints,
            inputs,
            output_type,
            side_effects,
            align_stack,
        } => {
            let mut text = format!(
                "%r{} = asm \"{}\" #{} : {}",
                inst.id,
                asm_string,
                constraints,
                format_type(output_type)
            );
            if !inputs.is_empty() {
                let values = inputs
                    .iter()
                    .map(format_value)
                    .collect::<Vec<_>>()
                    .join(", ");
                text.push_str(&format!(" [{}]", values));
            }
            if *side_effects {
                text.push_str(" side-effects");
            }
            if *align_stack {
                text.push_str(" align-stack");
            }
            text
        }
        LandingPad {
            result_type,
            personality,
            cleanup,
            clauses,
        } => {
            let mut text = format!("%r{} = landingpad {}", inst.id, format_type(result_type));
            if let Some(pers) = personality {
                text.push_str(&format!(" personality {}", format_value(pers)));
            }
            if *cleanup {
                text.push_str(" cleanup");
            }
            if !clauses.is_empty() {
                let clauses = clauses
                    .iter()
                    .map(|clause| match clause {
                        super::LandingPadClause::Catch(val) => {
                            format!("catch {}", format_value(val))
                        }
                        super::LandingPadClause::Filter(list) => {
                            let values =
                                list.iter().map(format_value).collect::<Vec<_>>().join(", ");
                            format!("filter [{}]", values)
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                text.push_str(&format!(" {{ {} }}", clauses));
            }
            text
        }
        Unreachable => "unreachable".to_string(),
        Freeze(value) => format!("%r{} = freeze {}", inst.id, format_value(value)),
    }
}

fn summarize_terminator(term: &LirTerminator) -> String {
    use LirTerminator::*;

    match term {
        Return(None) => "ret void".to_string(),
        Return(Some(value)) => format!("ret {}", format_value(value)),
        Br(target) => format!("br bb{}", target),
        CondBr {
            condition,
            if_true,
            if_false,
        } => format!(
            "cond_br {}, bb{}, bb{}",
            format_value(condition),
            if_true,
            if_false
        ),
        Switch {
            value,
            default,
            cases,
        } => {
            let mut text = format!("switch {} default bb{}", format_value(value), default);
            if !cases.is_empty() {
                let list = cases
                    .iter()
                    .map(|(val, bb)| format!("{} -> bb{}", val, bb))
                    .collect::<Vec<_>>()
                    .join(", ");
                text.push_str(&format!(" [{}]", list));
            }
            text
        }
        IndirectBr {
            address,
            destinations,
        } => {
            let dests = destinations
                .iter()
                .map(|bb| format!("bb{}", bb))
                .collect::<Vec<_>>()
                .join(", ");
            format!("indirect_br {}, [{}]", format_value(address), dests)
        }
        Invoke {
            function,
            args,
            normal_dest,
            unwind_dest,
            calling_convention,
        } => {
            let args = args.iter().map(format_value).collect::<Vec<_>>().join(", ");
            format!(
                "invoke {} {}({}) to bb{} unwind bb{}",
                format_calling_convention(calling_convention),
                format_value(function),
                args,
                normal_dest,
                unwind_dest
            )
        }
        Resume(value) => format!("resume {}", format_value(value)),
        Unreachable => "unreachable".to_string(),
        CleanupRet {
            cleanup_pad,
            unwind_dest,
        } => {
            if let Some(dest) = unwind_dest {
                format!("cleanupret {} unwind bb{}", format_value(cleanup_pad), dest)
            } else {
                format!("cleanupret {} unwind none", format_value(cleanup_pad))
            }
        }
        CatchRet {
            catch_pad,
            successor,
        } => {
            format!("catchret {} to bb{}", format_value(catch_pad), successor)
        }
        CatchSwitch {
            parent_pad,
            handlers,
            unwind_dest,
        } => {
            let mut text = String::from("catchswitch");
            if let Some(pad) = parent_pad {
                text.push_str(&format!(" within {}", format_value(pad)));
            }
            if !handlers.is_empty() {
                let list = handlers
                    .iter()
                    .map(|bb| format!("bb{}", bb))
                    .collect::<Vec<_>>()
                    .join(", ");
                text.push_str(&format!(" [handlers: {}]", list));
            }
            if let Some(dest) = unwind_dest {
                text.push_str(&format!(" unwind bb{}", dest));
            }
            text
        }
    }
}

fn format_value(value: &LirValue) -> String {
    use LirValue::*;

    match value {
        Register(id) => format!("%r{}", id),
        Constant(constant) => format_constant(constant),
        Global(name, _) => format!("@{}", name),
        Function(name) => format!("@{}", name),
        Local(id) => format!("%local{}", id),
        StackSlot(id) => format!("%stack{}", id),
        Undef(ty) => format!("undef {}", format_type(ty)),
        Null(ty) => format!("null {}", format_type(ty)),
    }
}

fn format_constant(constant: &LirConstant) -> String {
    use LirConstant::*;

    match constant {
        Int(value, ty) => format!("{} {}", format_type(ty), value),
        UInt(value, ty) => format!("{} {}", format_type(ty), value),
        Float(value, ty) => format!("{} {}", format_type(ty), value),
        Bool(value) => format!("bool {}", value),
        String(s) => format!("c\"{}\"", s),
        Array(elements, ty) => {
            let elems = elements
                .iter()
                .map(format_constant)
                .collect::<Vec<_>>()
                .join(", ");
            format!("[{} x {}] {{ {} }}", elements.len(), format_type(ty), elems)
        }
        Struct(fields, ty) => {
            let elems = fields
                .iter()
                .map(format_constant)
                .collect::<Vec<_>>()
                .join(", ");
            format!("struct {} {{ {} }}", format_type(ty), elems)
        }
        Null(ty) => format!("null {}", format_type(ty)),
        Undef(ty) => format!("undef {}", format_type(ty)),
    }
}

fn format_type(ty: &Ty) -> String {
    use Ty::*;

    match ty {
        I1 => "i1".into(),
        I8 => "i8".into(),
        I16 => "i16".into(),
        I32 => "i32".into(),
        I64 => "i64".into(),
        I128 => "i128".into(),
        F32 => "f32".into(),
        F64 => "f64".into(),
        Void => "void".into(),
        Ptr(inner) => format!("ptr {}", format_type(inner)),
        Array(inner, count) => format!("[{} x {}]", count, format_type(inner)),
        Struct {
            fields,
            packed,
            name,
        } => {
            if let Some(name) = name {
                format!("%{}", name)
            } else {
                let body = fields
                    .iter()
                    .map(format_type)
                    .collect::<Vec<_>>()
                    .join(", ");
                if *packed {
                    format!("<{{ {} }}>", body)
                } else {
                    format!("{{ {} }}", body)
                }
            }
        }
        Function {
            return_type,
            param_types,
            is_variadic,
        } => {
            let mut params = param_types.iter().map(format_type).collect::<Vec<_>>();
            if *is_variadic {
                params.push("...".into());
            }
            format!("fn({}) -> {}", params.join(", "), format_type(return_type))
        }
        Vector(inner, count) => format!("<{} x {}>", count, format_type(inner)),
        Label => "label".into(),
        Token => "token".into(),
        Metadata => "metadata".into(),
        Error => "<error>".into(),
    }
}

fn format_calling_convention(cc: &CallingConvention) -> &'static str {
    use CallingConvention::*;

    match cc {
        C => "c",
        Fast => "fast",
        Cold => "cold",
        WebKitJS => "webkit_js",
        AnyReg => "anyreg",
        PreserveMost => "preserve_most",
        PreserveAll => "preserve_all",
        Swift => "swift",
        CxxFastTLS => "cxx_fast_tls",
        X86StdCall => "x86_stdcall",
        X86FastCall => "x86_fastcall",
        X86ThisCall => "x86_thiscall",
        X86VectorCall => "x86_vectorcall",
        Win64 => "win64",
        X86_64SysV => "x86_64_sysv",
        AAPCS => "aapcs",
        AAPCSVfp => "aapcs_vfp",
    }
}

fn format_linkage(linkage: &Linkage) -> &'static str {
    use Linkage::*;

    match linkage {
        External => "external",
        AvailableExternally => "available_externally",
        LinkOnceAny => "linkonce",
        LinkOnceOdr => "linkonce_odr",
        WeakAny => "weak",
        WeakOdr => "weak_odr",
        Appending => "appending",
        Internal => "internal",
        Private => "private",
        ExternalWeak => "external_weak",
        Common => "common",
    }
}

fn format_visibility(visibility: &Visibility) -> &'static str {
    use Visibility::*;

    match visibility {
        Default => "default",
        Hidden => "hidden",
        Protected => "protected",
    }
}
