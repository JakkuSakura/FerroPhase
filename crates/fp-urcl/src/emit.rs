use fp_core::error::Result;
use fp_core::lir::{
    BasicBlockId, LirBasicBlock, LirBlob, LirConstant, LirConstantData, LirConstantKind,
    LirFunction, LirFunctionRef, LirInstruction, LirInstructionKind, LirIntrinsicKind,
    LirTerminator, LirValue, LirValueKind,
};
use std::fmt::Write;

pub fn emit_program(program: &LirBlob) -> Result<String> {
    let mut out = String::new();
    writeln!(&mut out, "BITS 64").ok();
    writeln!(&mut out, "MINREG 32").ok();
    writeln!(&mut out).ok();

    for global in &program.globals {
        writeln!(&mut out, ".data {}", global.name.as_str()).ok();
        if let Some(initializer) = global.initializer.as_ref() {
            writeln!(&mut out, "    DW {}", format_constant(initializer)).ok();
        }
        writeln!(&mut out).ok();
    }

    for function in &program.functions {
        emit_function(&mut out, function);
    }

    Ok(out)
}

fn emit_function(out: &mut String, function: &LirFunction) {
    writeln!(out, ".function {}", function.name.as_str()).ok();
    for block in &function.basic_blocks {
        emit_block(out, function, block);
    }
    writeln!(out).ok();
}

fn emit_block(out: &mut String, function: &LirFunction, block: &LirBasicBlock) {
    writeln!(out, "{}:", block_label(function, block.id)).ok();
    for inst in &block.instructions {
        emit_instruction(out, inst);
    }
    emit_terminator(out, function, &block.terminator);
}

fn emit_instruction(out: &mut String, inst: &LirInstruction) {
    let dst = reg(inst.id);
    match &inst.kind {
        LirInstructionKind::Add(lhs, rhs) => emit_tri(out, "ADD", &dst, lhs, rhs),
        LirInstructionKind::Sub(lhs, rhs) => emit_tri(out, "SUB", &dst, lhs, rhs),
        LirInstructionKind::Mul(lhs, rhs) => emit_tri(out, "MLT", &dst, lhs, rhs),
        LirInstructionKind::Div(lhs, rhs) => emit_tri(out, "DIV", &dst, lhs, rhs),
        LirInstructionKind::Rem(lhs, rhs) => emit_tri(out, "MOD", &dst, lhs, rhs),
        LirInstructionKind::And(lhs, rhs) => emit_tri(out, "AND", &dst, lhs, rhs),
        LirInstructionKind::Or(lhs, rhs) => emit_tri(out, "OR", &dst, lhs, rhs),
        LirInstructionKind::Xor(lhs, rhs) => emit_tri(out, "XOR", &dst, lhs, rhs),
        LirInstructionKind::Shl(lhs, rhs) => emit_tri(out, "LSH", &dst, lhs, rhs),
        LirInstructionKind::Shr(lhs, rhs) => emit_tri(out, "RSH", &dst, lhs, rhs),
        LirInstructionKind::Eq(lhs, rhs) => emit_compare(out, inst.id, "BRE", &dst, lhs, rhs),
        LirInstructionKind::Ne(lhs, rhs) => emit_compare(out, inst.id, "BNE", &dst, lhs, rhs),
        LirInstructionKind::Lt(lhs, rhs) => emit_compare(out, inst.id, "BRL", &dst, lhs, rhs),
        LirInstructionKind::Le(lhs, rhs) => emit_compare(out, inst.id, "BLE", &dst, lhs, rhs),
        LirInstructionKind::Gt(lhs, rhs) => emit_compare(out, inst.id, "BRG", &dst, lhs, rhs),
        LirInstructionKind::Ge(lhs, rhs) => emit_compare(out, inst.id, "BGE", &dst, lhs, rhs),
        LirInstructionKind::Not(value) => {
            writeln!(out, "    NOT {}, {}", dst, format_value(value)).ok();
        }
        LirInstructionKind::Load { address, .. } => {
            writeln!(out, "    LOD {}, {}", dst, format_value(address)).ok();
        }
        LirInstructionKind::Store { value, address, .. } => {
            writeln!(
                out,
                "    STR {}, {}",
                format_value(address),
                format_value(value)
            )
            .ok();
        }
        LirInstructionKind::Alloca { size, .. } => {
            writeln!(out, "    PSH {}, {}", dst, format_value(size)).ok();
        }
        LirInstructionKind::GetElementPtr { ptr, indices, .. } => {
            writeln!(out, "    MOV {}, {}", dst, format_value(ptr)).ok();
            for index in indices {
                writeln!(out, "    ADD {}, {}, {}", dst, dst, format_value(index)).ok();
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
            writeln!(out, "    MOV {}, {}", dst, format_value(value)).ok();
        }
        LirInstructionKind::ExtractValue { aggregate, indices } => {
            writeln!(out, "    MOV {}, {}", dst, format_value(aggregate)).ok();
            for index in indices {
                writeln!(out, "    ADD {}, {}, {}", dst, dst, index).ok();
            }
        }
        LirInstructionKind::InsertValue {
            aggregate,
            element,
            indices,
        } => {
            writeln!(out, "    MOV {}, {}", dst, format_value(aggregate)).ok();
            writeln!(
                out,
                "    ; insert {} at {:?}",
                format_value(element),
                indices
            )
            .ok();
        }
        LirInstructionKind::Call { function, args, .. } => {
            for (index, arg) in args.iter().enumerate() {
                writeln!(out, "    MOV R{}, {}", 20 + index, format_value(arg)).ok();
            }
            writeln!(out, "    CAL {}", format_value(function)).ok();
            writeln!(out, "    MOV {}, R1", dst).ok();
        }
        LirInstructionKind::ExecQuery(_) => {
            writeln!(
                out,
                "    ; unsupported exec query: lowered only by pxc whole-file backend"
            )
            .ok();
        }
        LirInstructionKind::IntrinsicCall { kind, format, args } => {
            emit_intrinsic(out, &dst, kind, format, args)
        }
        LirInstructionKind::Phi { incoming } => {
            writeln!(out, "    ; phi {} <- {:?}", dst, incoming).ok();
        }
        LirInstructionKind::Select {
            condition,
            if_true,
            if_false,
        } => {
            writeln!(out, "    MOV {}, {}", dst, format_value(if_false)).ok();
            writeln!(
                out,
                "    BNZ {}, {}",
                format_value(condition),
                label_name(inst.id, "select_true")
            )
            .ok();
            writeln!(out, "    JMP {}", label_name(inst.id, "select_done")).ok();
            writeln!(out, "{}:", label_name(inst.id, "select_true")).ok();
            writeln!(out, "    MOV {}, {}", dst, format_value(if_true)).ok();
            writeln!(out, "{}:", label_name(inst.id, "select_done")).ok();
        }
        LirInstructionKind::InlineAsm { asm_string, .. } => {
            writeln!(out, "    ; inline asm: {}", asm_string).ok();
        }
        LirInstructionKind::LandingPad { .. } => {
            writeln!(out, "    ; landingpad {}", dst).ok();
        }
        LirInstructionKind::Unreachable | LirInstructionKind::ComptimeOp(_) => {
            writeln!(out, "    HLT").ok();
        }
    }
}

fn emit_intrinsic(
    out: &mut String,
    dst: &str,
    kind: &LirIntrinsicKind,
    format: &str,
    args: &[LirValue],
) {
    match kind {
        LirIntrinsicKind::Print | LirIntrinsicKind::Println | LirIntrinsicKind::Format => {
            writeln!(
                out,
                "    ; intrinsic {:?} \"{}\"",
                kind,
                format.escape_default()
            )
            .ok();
            for (index, arg) in args.iter().enumerate() {
                writeln!(out, "    MOV R{}, {}", 24 + index, format_value(arg)).ok();
            }
            writeln!(out, "    MOV {}, 0", dst).ok();
        }
        LirIntrinsicKind::TimeNow
        | LirIntrinsicKind::ProcMacroTokenStreamFromStr
        | LirIntrinsicKind::ProcMacroTokenStreamToString => {
            writeln!(out, "    ; intrinsic {:?}", kind).ok();
            writeln!(out, "    MOV {}, 0", dst).ok();
        }
    }
}

fn emit_terminator(out: &mut String, function: &LirFunction, term: &LirTerminator) {
    match term {
        LirTerminator::Return(Some(value)) => {
            writeln!(out, "    MOV R1, {}", format_value(value)).ok();
            writeln!(out, "    RET").ok();
        }
        LirTerminator::Return(None) => {
            writeln!(out, "    RET").ok();
        }
        LirTerminator::Br(target) => {
            writeln!(out, "    JMP {}", block_label(function, *target)).ok();
        }
        LirTerminator::CondBr {
            condition,
            if_true,
            if_false,
        } => {
            writeln!(
                out,
                "    BNZ {}, {}",
                format_value(condition),
                block_label(function, *if_true)
            )
            .ok();
            writeln!(out, "    JMP {}", block_label(function, *if_false)).ok();
        }
        LirTerminator::Switch {
            value,
            default,
            cases,
        } => {
            for (case, target) in cases {
                writeln!(
                    out,
                    "    BRE {}, {}, {}",
                    format_value(value),
                    case,
                    block_label(function, *target)
                )
                .ok();
            }
            writeln!(out, "    JMP {}", block_label(function, *default)).ok();
        }
        LirTerminator::Invoke {
            function: callee,
            args,
            normal_dest,
            ..
        } => {
            for (index, arg) in args.iter().enumerate() {
                writeln!(out, "    MOV R{}, {}", 20 + index, format_value(arg)).ok();
            }
            writeln!(out, "    CAL {}", format_value(callee)).ok();
            writeln!(out, "    JMP {}", block_label(function, *normal_dest)).ok();
        }
        LirTerminator::Unreachable => {
            writeln!(out, "    HLT").ok();
        }
        other => {
            writeln!(out, "    ; unsupported terminator: {:?}", other).ok();
        }
    }
}

fn emit_tri(out: &mut String, op: &str, dst: &str, lhs: &LirValue, rhs: &LirValue) {
    writeln!(
        out,
        "    {} {}, {}, {}",
        op,
        dst,
        format_value(lhs),
        format_value(rhs)
    )
    .ok();
}

fn emit_compare(
    out: &mut String,
    inst_id: u32,
    branch_op: &str,
    dst: &str,
    lhs: &LirValue,
    rhs: &LirValue,
) {
    let true_label = label_name(inst_id, "cmp_true");
    let done_label = label_name(inst_id, "cmp_done");
    writeln!(out, "    MOV {}, 0", dst).ok();
    writeln!(
        out,
        "    {} {}, {}, {}",
        branch_op,
        format_value(lhs),
        format_value(rhs),
        true_label
    )
    .ok();
    writeln!(out, "    JMP {}", done_label).ok();
    writeln!(out, "{}:", true_label).ok();
    writeln!(out, "    MOV {}, 1", dst).ok();
    writeln!(out, "{}:", done_label).ok();
}

fn label_name(inst_id: u32, suffix: &str) -> String {
    format!(".L{}_{}", inst_id, suffix)
}
fn reg(id: u32) -> String {
    format!("R{}", id + 2)
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

fn format_value(value: &LirValue) -> String {
    match &value.kind {
        LirValueKind::Register(id) => reg(*id),
        LirValueKind::Constant(kind) => format_constant(&LirConstant {
            ty: value.ty.clone(),
            kind: kind.clone(),
        }),
        LirValueKind::Global(name) => format!("@{}", name),
        LirValueKind::Function(LirFunctionRef::Name(name)) => name.to_string(),
        LirValueKind::Function(LirFunctionRef::Package { name, .. }) => name.to_string(),
        // Definition references are resolved to symbols by the surrounding
        // compiler when possible. URCL is text, so retaining the definition
        // id gives the assembler a stable symbolic target instead of
        // aborting emission for an otherwise representable call.
        LirValueKind::Function(LirFunctionRef::Definition(def_id)) => def_id.to_string(),
        LirValueKind::Local(id) => format!("local{}", id),
        LirValueKind::StackSlot(id) => format!("stack{}", id),
    }
}

fn format_constant(constant: &LirConstant) -> String {
    match &constant.kind {
        LirConstantKind::Data(LirConstantData::Integer(value)) => match value {
            fp_core::lir::LirInteger::I1(value) => (*value as u8).to_string(),
            fp_core::lir::LirInteger::I8(value) => value.to_string(),
            fp_core::lir::LirInteger::I16(value) => value.to_string(),
            fp_core::lir::LirInteger::I32(value) => value.to_string(),
            fp_core::lir::LirInteger::I64(value) => value.to_string(),
            fp_core::lir::LirInteger::I128(value) => value.to_string(),
            fp_core::lir::LirInteger::Arbitrary(value) => format!("{:?}", value),
        },
        LirConstantKind::Data(LirConstantData::Float(value)) => format!("{:?}", value),
        LirConstantKind::Data(LirConstantData::Bytes(bytes)) => {
            format!("bytes(len={})", bytes.len())
        }
        LirConstantKind::Aggregate(fp_core::lir::LirConstantAggregate::Array(values))
        | LirConstantKind::Aggregate(fp_core::lir::LirConstantAggregate::Vector(values)) => {
            format!(
                "[{}]",
                values
                    .iter()
                    .map(format_constant)
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        }
        LirConstantKind::Aggregate(fp_core::lir::LirConstantAggregate::Struct(values)) => format!(
            "{{{}}}",
            values
                .iter()
                .map(format_constant)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        LirConstantKind::GlobalAddress { global } => format!("@{}", global.as_str()),
        LirConstantKind::FunctionAddress(LirFunctionRef::Name(name)) => name.as_str().to_string(),
        LirConstantKind::Null => "0".into(),
        LirConstantKind::Undef => "undef".into(),
        _ => "unsupported".into(),
    }
}
