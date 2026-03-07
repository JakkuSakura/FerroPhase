use crate::config::GoAsmTarget;
use fp_core::error::Result;
use fp_core::lir::{
    BasicBlockId, LirBasicBlock, LirConstant, LirFunction, LirInstruction, LirInstructionKind,
    LirIntrinsicKind, LirProgram, LirTerminator, LirValue,
};
use std::fmt::Write;

macro_rules! line {
    ($out:expr) => {
        writeln!($out).map_err(|err| fp_core::error::Error::from(err.to_string()))
    };
    ($out:expr, $($arg:tt)*) => {
        writeln!($out, $($arg)*).map_err(|err| fp_core::error::Error::from(err.to_string()))
    };
}

pub fn emit_program(program: &LirProgram, target: GoAsmTarget) -> Result<String> {
    let mut out = String::new();
    line!(&mut out, "#include \"textflag.h\"")?;
    line!(&mut out, "// fp-goasm ({})", arch_name(target))?;
    line!(&mut out)?;

    for global in &program.globals {
        line!(
            &mut out,
            "GLOBL {}(SB), NOPTR, $8",
            go_symbol(global.name.as_str())
        )?;
        if let Some(initializer) = global.initializer.as_ref() {
            line!(
                &mut out,
                "DATA {}+0(SB)/8, {}",
                go_symbol(global.name.as_str()),
                format_constant(initializer)
            )?;
        }
        line!(&mut out)?;
    }

    for function in &program.functions {
        emit_function(&mut out, function, target)?;
    }

    Ok(out)
}

fn emit_function(out: &mut String, function: &LirFunction, target: GoAsmTarget) -> Result<()> {
    line!(
        out,
        "TEXT {}(SB), NOSPLIT, $0-0",
        go_symbol(function.name.as_str())
    )?;
    for block in &function.basic_blocks {
        emit_block(out, function, block, target)?;
    }
    line!(out)?;
    Ok(())
}

fn emit_block(
    out: &mut String,
    function: &LirFunction,
    block: &LirBasicBlock,
    target: GoAsmTarget,
) -> Result<()> {
    line!(out, "{}:", block_label(function, block.id))?;
    for inst in &block.instructions {
        emit_instruction(out, inst, target)?;
    }
    emit_terminator(out, function, &block.terminator, target)
}

fn emit_instruction(out: &mut String, inst: &LirInstruction, target: GoAsmTarget) -> Result<()> {
    let dst = reg(inst.id);
    let mov = move_op(target);
    match &inst.kind {
        LirInstructionKind::Add(lhs, rhs) => {
            emit_binop(out, target, add_op(target), &dst, lhs, rhs)
        }
        LirInstructionKind::Sub(lhs, rhs) => {
            emit_binop(out, target, sub_op(target), &dst, lhs, rhs)
        }
        LirInstructionKind::Mul(lhs, rhs) => {
            emit_binop(out, target, mul_op(target), &dst, lhs, rhs)
        }
        LirInstructionKind::And(lhs, rhs) => {
            emit_binop(out, target, and_op(target), &dst, lhs, rhs)
        }
        LirInstructionKind::Or(lhs, rhs) => emit_binop(out, target, or_op(target), &dst, lhs, rhs),
        LirInstructionKind::Xor(lhs, rhs) => {
            emit_binop(out, target, xor_op(target), &dst, lhs, rhs)
        }
        LirInstructionKind::Shl(lhs, rhs) => {
            emit_binop(out, target, shl_op(target), &dst, lhs, rhs)
        }
        LirInstructionKind::Shr(lhs, rhs) => {
            emit_binop(out, target, shr_op(target), &dst, lhs, rhs)
        }
        LirInstructionKind::Eq(lhs, rhs)
        | LirInstructionKind::Ne(lhs, rhs)
        | LirInstructionKind::Lt(lhs, rhs)
        | LirInstructionKind::Le(lhs, rhs)
        | LirInstructionKind::Gt(lhs, rhs)
        | LirInstructionKind::Ge(lhs, rhs) => emit_compare(out, inst, lhs, rhs, &dst, target),
        LirInstructionKind::Not(value) => {
            line!(out, "\t{} {}, {}", mov, format_value(value), dst)?;
            match target {
                GoAsmTarget::Amd64 => line!(out, "\tNOTQ {}", dst),
                GoAsmTarget::Arm64 => line!(out, "\tMVN {}, {}", dst, dst),
            }
        }
        LirInstructionKind::PtrToInt(value)
        | LirInstructionKind::IntToPtr(value)
        | LirInstructionKind::Trunc(value, _)
        | LirInstructionKind::ZExt(value, _)
        | LirInstructionKind::SExt(value, _)
        | LirInstructionKind::FPTrunc(value, _)
        | LirInstructionKind::FPExt(value, _)
        | LirInstructionKind::FPToUI(value, _)
        | LirInstructionKind::FPToSI(value, _)
        | LirInstructionKind::UIToFP(value, _)
        | LirInstructionKind::SIToFP(value, _)
        | LirInstructionKind::Bitcast(value, _)
        | LirInstructionKind::SextOrTrunc(value, _)
        | LirInstructionKind::Freeze(value) => {
            line!(out, "\t{} {}, {}", mov, format_value(value), dst)
        }
        LirInstructionKind::Call { function, args, .. } => {
            for (index, arg) in args.iter().enumerate() {
                line!(
                    out,
                    "\t{} {}, {}",
                    mov,
                    format_value(arg),
                    arg_reg(target, index)
                )?;
            }
            line!(out, "\tCALL {}", format_call_target(function))?;
            line!(out, "\t{} {}, {}", mov, ret_reg(target), dst)
        }
        LirInstructionKind::IntrinsicCall { kind, format, args } => {
            emit_intrinsic(out, kind, format, args, &dst, target)
        }
        LirInstructionKind::Unreachable => line!(out, "\tUNDEF"),
        other => emit_unsupported(out, &format!("{:?}", other)),
    }
}

fn emit_intrinsic(
    out: &mut String,
    kind: &LirIntrinsicKind,
    format: &str,
    args: &[LirValue],
    dst: &str,
    target: GoAsmTarget,
) -> Result<()> {
    let mov = move_op(target);
    line!(
        out,
        "\t// intrinsic {:?} \"{}\"",
        kind,
        format.escape_default()
    )?;
    for (index, arg) in args.iter().enumerate() {
        line!(
            out,
            "\t{} {}, {}",
            mov,
            format_value(arg),
            arg_reg(target, index)
        )?;
    }
    line!(out, "\t{} $0, {}", mov, dst)
}

fn emit_terminator(
    out: &mut String,
    function: &LirFunction,
    term: &LirTerminator,
    target: GoAsmTarget,
) -> Result<()> {
    let mov = move_op(target);
    match term {
        LirTerminator::Return(Some(value)) => {
            line!(
                out,
                "\t{} {}, {}",
                mov,
                format_value(value),
                ret_reg(target)
            )?;
            line!(out, "\tRET")
        }
        LirTerminator::Return(None) => line!(out, "\tRET"),
        LirTerminator::Br(target_block) => {
            line!(out, "\tJMP {}", block_label(function, *target_block))
        }
        LirTerminator::CondBr {
            condition,
            if_true,
            if_false,
        } => {
            line!(out, "\t{} $0, {}", cmp_op(target), format_value(condition))?;
            line!(
                out,
                "\t{} {}",
                branch_ne_op(target),
                block_label(function, *if_true)
            )?;
            line!(out, "\tJMP {}", block_label(function, *if_false))
        }
        LirTerminator::Switch {
            value,
            default,
            cases,
        } => {
            for (case, dest) in cases {
                line!(
                    out,
                    "\t{} ${}, {}",
                    cmp_op(target),
                    case,
                    format_value(value)
                )?;
                line!(
                    out,
                    "\t{} {}",
                    branch_eq_op(target),
                    block_label(function, *dest)
                )?;
            }
            line!(out, "\tJMP {}", block_label(function, *default))
        }
        LirTerminator::Invoke {
            function: callee,
            args,
            normal_dest,
            ..
        } => {
            for (index, arg) in args.iter().enumerate() {
                line!(
                    out,
                    "\t{} {}, {}",
                    mov,
                    format_value(arg),
                    arg_reg(target, index)
                )?;
            }
            line!(out, "\tCALL {}", format_call_target(callee))?;
            line!(out, "\tJMP {}", block_label(function, *normal_dest))
        }
        LirTerminator::Unreachable => line!(out, "\tUNDEF"),
        other => emit_unsupported(out, &format!("terminator {:?}", other)),
    }
}

fn emit_binop(
    out: &mut String,
    target: GoAsmTarget,
    op: &str,
    dst: &str,
    lhs: &LirValue,
    rhs: &LirValue,
) -> Result<()> {
    let mov = move_op(target);
    line!(out, "\t{} {}, {}", mov, format_value(lhs), dst)?;
    match target {
        GoAsmTarget::Amd64 => line!(out, "\t{} {}, {}", op, format_value(rhs), dst),
        GoAsmTarget::Arm64 => line!(out, "\t{} {}, {}, {}", op, format_value(rhs), dst, dst),
    }
}

fn emit_compare(
    out: &mut String,
    inst: &LirInstruction,
    lhs: &LirValue,
    rhs: &LirValue,
    dst: &str,
    target: GoAsmTarget,
) -> Result<()> {
    let mov = move_op(target);
    let true_label = label_name(inst.id, "cmp_true");
    let done_label = label_name(inst.id, "cmp_done");
    line!(out, "\t{} $0, {}", mov, dst)?;
    match target {
        GoAsmTarget::Amd64 => line!(
            out,
            "\t{} {}, {}",
            cmp_op(target),
            format_value(lhs),
            format_value(rhs)
        )?,
        GoAsmTarget::Arm64 => line!(
            out,
            "\t{} {}, {}",
            cmp_op(target),
            format_value(rhs),
            format_value(lhs)
        )?,
    }
    line!(out, "\t{} {}", compare_branch(inst, target), true_label)?;
    line!(out, "\tJMP {}", done_label)?;
    line!(out, "{}:", true_label)?;
    line!(out, "\t{} $1, {}", mov, dst)?;
    line!(out, "{}:", done_label)
}

fn emit_unsupported(out: &mut String, detail: &str) -> Result<()> {
    line!(out, "\t// unsupported: {}", detail)
}

fn go_symbol(name: &str) -> String {
    format!("·{}", name)
}
fn label_name(id: u32, suffix: &str) -> String {
    format!("L{}_{}", id, suffix)
}
fn arch_name(target: GoAsmTarget) -> &'static str {
    match target {
        GoAsmTarget::Amd64 => "amd64",
        GoAsmTarget::Arm64 => "arm64",
    }
}
fn move_op(target: GoAsmTarget) -> &'static str {
    match target {
        GoAsmTarget::Amd64 => "MOVQ",
        GoAsmTarget::Arm64 => "MOVD",
    }
}
fn add_op(target: GoAsmTarget) -> &'static str {
    match target {
        GoAsmTarget::Amd64 => "ADDQ",
        GoAsmTarget::Arm64 => "ADD",
    }
}
fn sub_op(target: GoAsmTarget) -> &'static str {
    match target {
        GoAsmTarget::Amd64 => "SUBQ",
        GoAsmTarget::Arm64 => "SUB",
    }
}
fn mul_op(target: GoAsmTarget) -> &'static str {
    match target {
        GoAsmTarget::Amd64 => "IMULQ",
        GoAsmTarget::Arm64 => "MUL",
    }
}
fn and_op(target: GoAsmTarget) -> &'static str {
    match target {
        GoAsmTarget::Amd64 => "ANDQ",
        GoAsmTarget::Arm64 => "AND",
    }
}
fn or_op(target: GoAsmTarget) -> &'static str {
    match target {
        GoAsmTarget::Amd64 => "ORQ",
        GoAsmTarget::Arm64 => "ORR",
    }
}
fn xor_op(target: GoAsmTarget) -> &'static str {
    match target {
        GoAsmTarget::Amd64 => "XORQ",
        GoAsmTarget::Arm64 => "EOR",
    }
}
fn shl_op(target: GoAsmTarget) -> &'static str {
    match target {
        GoAsmTarget::Amd64 => "SHLQ",
        GoAsmTarget::Arm64 => "LSL",
    }
}
fn shr_op(target: GoAsmTarget) -> &'static str {
    match target {
        GoAsmTarget::Amd64 => "SHRQ",
        GoAsmTarget::Arm64 => "LSR",
    }
}
fn cmp_op(target: GoAsmTarget) -> &'static str {
    match target {
        GoAsmTarget::Amd64 => "CMPQ",
        GoAsmTarget::Arm64 => "CMP",
    }
}
fn branch_eq_op(target: GoAsmTarget) -> &'static str {
    match target {
        GoAsmTarget::Amd64 => "JEQ",
        GoAsmTarget::Arm64 => "BEQ",
    }
}
fn branch_ne_op(target: GoAsmTarget) -> &'static str {
    match target {
        GoAsmTarget::Amd64 => "JNE",
        GoAsmTarget::Arm64 => "BNE",
    }
}
fn ret_reg(target: GoAsmTarget) -> &'static str {
    match target {
        GoAsmTarget::Amd64 => "AX",
        GoAsmTarget::Arm64 => "R0",
    }
}
fn arg_reg(target: GoAsmTarget, index: usize) -> String {
    match target {
        GoAsmTarget::Amd64 => match index {
            0 => "DI".into(),
            1 => "SI".into(),
            2 => "DX".into(),
            3 => "CX".into(),
            4 => "R8".into(),
            5 => "R9".into(),
            _ => format!("arg{}", index),
        },
        GoAsmTarget::Arm64 => format!("R{}", index),
    }
}
fn reg(id: u32) -> String {
    format!("R{}", id + 10)
}

fn compare_branch(inst: &LirInstruction, target: GoAsmTarget) -> &'static str {
    match (target, &inst.kind) {
        (GoAsmTarget::Amd64, LirInstructionKind::Eq(_, _)) => "JEQ",
        (GoAsmTarget::Amd64, LirInstructionKind::Ne(_, _)) => "JNE",
        (GoAsmTarget::Amd64, LirInstructionKind::Lt(_, _)) => "JLT",
        (GoAsmTarget::Amd64, LirInstructionKind::Le(_, _)) => "JLE",
        (GoAsmTarget::Amd64, LirInstructionKind::Gt(_, _)) => "JGT",
        (GoAsmTarget::Amd64, LirInstructionKind::Ge(_, _)) => "JGE",
        (GoAsmTarget::Arm64, LirInstructionKind::Eq(_, _)) => "BEQ",
        (GoAsmTarget::Arm64, LirInstructionKind::Ne(_, _)) => "BNE",
        (GoAsmTarget::Arm64, LirInstructionKind::Lt(_, _)) => "BLT",
        (GoAsmTarget::Arm64, LirInstructionKind::Le(_, _)) => "BLE",
        (GoAsmTarget::Arm64, LirInstructionKind::Gt(_, _)) => "BGT",
        (GoAsmTarget::Arm64, LirInstructionKind::Ge(_, _)) => "BGE",
        _ => unreachable!(),
    }
}

fn block_label(function: &LirFunction, block_id: BasicBlockId) -> String {
    if let Some(block) = function
        .basic_blocks
        .iter()
        .find(|block| block.id == block_id)
    {
        if let Some(label) = block.label.as_ref() {
            return format!("{}_{}", function.name.as_str(), label.as_str());
        }
    }
    format!("{}_bb{}", function.name.as_str(), block_id)
}

fn format_call_target(value: &LirValue) -> String {
    match value {
        LirValue::Function(name) => format!("{}(SB)", go_symbol(name)),
        other => format_value(other),
    }
}

fn format_value(value: &LirValue) -> String {
    match value {
        LirValue::Register(id) => reg(*id),
        LirValue::Constant(constant) => format_constant(constant),
        LirValue::Global(name, _) => format!("{}(SB)", go_symbol(name)),
        LirValue::Function(name) => format!("{}(SB)", go_symbol(name)),
        LirValue::Local(id) => format!("local{}", id),
        LirValue::StackSlot(id) => format!("stack{}", id),
        LirValue::Undef(_) | LirValue::Null(_) => "$0".into(),
    }
}

fn format_constant(constant: &LirConstant) -> String {
    match constant {
        LirConstant::Int(value, _) => format!("${}", value),
        LirConstant::UInt(value, _) => format!("${}", value),
        LirConstant::Float(value, _) => format!("${}", value),
        LirConstant::Bool(value) => format!("${}", *value as u8),
        LirConstant::String(value) => format!("$\"{}\"", value.escape_default()),
        LirConstant::Array(values, _) => format!(
            "$[{}]",
            values
                .iter()
                .map(format_constant)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        LirConstant::Struct(values, _) => format!(
            "${{{}}}",
            values
                .iter()
                .map(format_constant)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        LirConstant::GlobalRef(name, _, _) => format!("${}(SB)", go_symbol(name.as_str())),
        LirConstant::FunctionRef(name, _) => format!("${}(SB)", go_symbol(name.as_str())),
        LirConstant::Null(_) | LirConstant::Undef(_) => "$0".into(),
    }
}
