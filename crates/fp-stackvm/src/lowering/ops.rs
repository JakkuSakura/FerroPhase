//! Sub-lowerings for individual bytecode operations.
//!
//! Each function takes a [`FunctionLowering`] reference plus the
//! operands and emits the appropriate LIR instruction(s).  These are
//! called from the main dispatch loop in [`FunctionLowering::lower_block`].

use fp_bytecode::{
    BytecodeBinOp, BytecodeConst, BytecodePlace, BytecodePlaceElem, BytecodeUnOp, IntrinsicKind,
};
use fp_core::lir::{
    BasicBlockId, CallingConvention, LirConstant, LirFloat, LirInstructionKind, LirInteger,
    LirType, LirValue, RegisterId,
};

use super::constants;
use super::function::FunctionLowering;
use super::{LowerError, LowerResult};

fn i64_value(value: u64) -> LirValue {
    LirValue::constant(
        LirConstant::integer(LirType::I64, LirInteger::I64(value)).expect("valid i64 constant"),
    )
}

fn i1_value(value: u64) -> LirValue {
    LirValue::constant(
        LirConstant::integer(LirType::I1, LirInteger::I1(value != 0)).expect("valid i1 constant"),
    )
}

fn f64_value(value: f64) -> LirValue {
    LirValue::constant(
        LirConstant::float(LirType::F64, LirFloat::F64(value.to_bits()))
            .expect("valid f64 constant"),
    )
}

// -----------------------------------------------------------------
// Constants
// -----------------------------------------------------------------

/// Lower a single [`BytecodeConst`] into a sequence of instructions
/// that materialise the value in a register.
///
/// Scalars are materialised via `Add(x, 0)` (a common SSA trick to
/// copy a constant into a register).  Compound and string constants
/// emit calls to the heap-allocation intrinsics.
pub(crate) fn lower_load_const(
    fl: &mut FunctionLowering,
    block_id: BasicBlockId,
    value: &BytecodeConst,
) -> LowerResult<RegisterId> {
    match value {
        BytecodeConst::Unit => fl.emit_in_block(
            block_id,
            LirInstructionKind::Add(
                i64_value(0),
                i64_value(0),
            ),
        ),
        BytecodeConst::Bool(b) => {
            let val = if *b { 1u64 } else { 0u64 };
            fl.emit_in_block(
                block_id,
                LirInstructionKind::Add(
                    i1_value(val),
                    i1_value(0),
                ),
            )
        }
        BytecodeConst::Int(i) => fl.emit_in_block(
            block_id,
            LirInstructionKind::Add(
                i64_value(*i as u64),
                i64_value(0),
            ),
        ),
        BytecodeConst::UInt(u) => fl.emit_in_block(
            block_id,
            LirInstructionKind::Add(
                i64_value(*u),
                i64_value(0),
            ),
        ),
        BytecodeConst::Float(f) => fl.emit_in_block(
            block_id,
            LirInstructionKind::Add(
                f64_value(*f),
                f64_value(0.0),
            ),
        ),
        BytecodeConst::Str(_s) => {
            // String bodies are stored in the const pool but their
            // runtime representation requires heap allocation.  For
            // now we emit a placeholder call to __bc_str_alloc.
            // A full implementation would encode the string bytes as
            // inline data or a global initialiser.
            let len_reg = fl.emit_in_block(
                block_id,
                LirInstructionKind::Add(
                    i64_value(0),
                    i64_value(0),
                ),
            )?;
            lower_call_intrinsic(
                fl,
                block_id,
                constants::INTRINSIC_STR_ALLOC,
                &[FunctionLowering::reg_val(len_reg)],
            )
        }
        BytecodeConst::Function(_name) => {
            // Function references are lowered identically to strings.
            let len_reg = fl.emit_in_block(
                block_id,
                LirInstructionKind::Add(
                    i64_value(0),
                    i64_value(0),
                ),
            )?;
            lower_call_intrinsic(
                fl,
                block_id,
                constants::INTRINSIC_STR_ALLOC,
                &[FunctionLowering::reg_val(len_reg)],
            )
        }
        BytecodeConst::Null => fl.emit_in_block(
            block_id,
            LirInstructionKind::Add(
                LirValue::constant(LirConstant::null(LirType::Ptr(Box::new(LirType::I64)))),
                i64_value(0),
            ),
        ),
        BytecodeConst::Undef => fl.emit_in_block(
            block_id,
            LirInstructionKind::Add(
                LirValue::constant(LirConstant::undef(LirType::I64)),
                i64_value(0),
            ),
        ),
        BytecodeConst::Tuple(items) => {
            let mut element_regs = Vec::new();
            for item in items {
                let reg = lower_load_const(fl, block_id, item)?;
                element_regs.push(FunctionLowering::reg_val(reg));
            }
            let mut args = vec![i64_value(element_regs.len() as u64)];
            args.extend(element_regs);
            lower_call_intrinsic(fl, block_id, constants::INTRINSIC_MAKE_TUPLE, &args)
        }
        BytecodeConst::Array(items) => {
            let mut element_regs = Vec::new();
            for item in items {
                let reg = lower_load_const(fl, block_id, item)?;
                element_regs.push(FunctionLowering::reg_val(reg));
            }
            let mut args = vec![i64_value(element_regs.len() as u64)];
            args.extend(element_regs);
            lower_call_intrinsic(fl, block_id, constants::INTRINSIC_MAKE_ARRAY, &args)
        }
        BytecodeConst::List(items) => {
            let mut element_regs = Vec::new();
            for item in items {
                let reg = lower_load_const(fl, block_id, item)?;
                element_regs.push(FunctionLowering::reg_val(reg));
            }
            let mut args = vec![i64_value(element_regs.len() as u64)];
            args.extend(element_regs);
            lower_call_intrinsic(fl, block_id, constants::INTRINSIC_MAKE_LIST, &args)
        }
        BytecodeConst::Map(entries) => {
            let mut arg_regs = Vec::new();
            for (key, value) in entries {
                let k = lower_load_const(fl, block_id, key)?;
                let v = lower_load_const(fl, block_id, value)?;
                arg_regs.push(FunctionLowering::reg_val(k));
                arg_regs.push(FunctionLowering::reg_val(v));
            }
            let mut args = vec![i64_value(entries.len() as u64)];
            args.extend(arg_regs);
            lower_call_intrinsic(fl, block_id, constants::INTRINSIC_MAKE_MAP, &args)
        }
    }
}

// -----------------------------------------------------------------
// Arithmetic
// -----------------------------------------------------------------

/// Lower a binary operation on two registers.
pub(crate) fn lower_binop(
    fl: &mut FunctionLowering,
    block_id: BasicBlockId,
    op: &BytecodeBinOp,
    left: RegisterId,
    right: RegisterId,
) -> LowerResult<RegisterId> {
    let kind = match op {
        BytecodeBinOp::Add => LirInstructionKind::Add(
            FunctionLowering::reg_val(left),
            FunctionLowering::reg_val(right),
        ),
        BytecodeBinOp::Sub => LirInstructionKind::Sub(
            FunctionLowering::reg_val(left),
            FunctionLowering::reg_val(right),
        ),
        BytecodeBinOp::Mul => LirInstructionKind::Mul(
            FunctionLowering::reg_val(left),
            FunctionLowering::reg_val(right),
        ),
        BytecodeBinOp::Div => LirInstructionKind::Div(
            FunctionLowering::reg_val(left),
            FunctionLowering::reg_val(right),
        ),
        BytecodeBinOp::Rem => LirInstructionKind::Rem(
            FunctionLowering::reg_val(left),
            FunctionLowering::reg_val(right),
        ),
        BytecodeBinOp::And => LirInstructionKind::And(
            FunctionLowering::reg_val(left),
            FunctionLowering::reg_val(right),
        ),
        BytecodeBinOp::Or => LirInstructionKind::Or(
            FunctionLowering::reg_val(left),
            FunctionLowering::reg_val(right),
        ),
        BytecodeBinOp::BitXor => LirInstructionKind::Xor(
            FunctionLowering::reg_val(left),
            FunctionLowering::reg_val(right),
        ),
        BytecodeBinOp::BitAnd => LirInstructionKind::And(
            FunctionLowering::reg_val(left),
            FunctionLowering::reg_val(right),
        ),
        BytecodeBinOp::BitOr => LirInstructionKind::Or(
            FunctionLowering::reg_val(left),
            FunctionLowering::reg_val(right),
        ),
        BytecodeBinOp::Shl => LirInstructionKind::Shl(
            FunctionLowering::reg_val(left),
            FunctionLowering::reg_val(right),
        ),
        BytecodeBinOp::Shr => LirInstructionKind::Shr(
            FunctionLowering::reg_val(left),
            FunctionLowering::reg_val(right),
        ),
        BytecodeBinOp::Eq => LirInstructionKind::Eq(
            FunctionLowering::reg_val(left),
            FunctionLowering::reg_val(right),
        ),
        BytecodeBinOp::Ne => LirInstructionKind::Ne(
            FunctionLowering::reg_val(left),
            FunctionLowering::reg_val(right),
        ),
        BytecodeBinOp::Lt => LirInstructionKind::Lt(
            FunctionLowering::reg_val(left),
            FunctionLowering::reg_val(right),
        ),
        BytecodeBinOp::Le => LirInstructionKind::Le(
            FunctionLowering::reg_val(left),
            FunctionLowering::reg_val(right),
        ),
        BytecodeBinOp::Ge => LirInstructionKind::Ge(
            FunctionLowering::reg_val(left),
            FunctionLowering::reg_val(right),
        ),
        BytecodeBinOp::Gt => LirInstructionKind::Gt(
            FunctionLowering::reg_val(left),
            FunctionLowering::reg_val(right),
        ),
    };
    fl.emit_in_block(block_id, kind)
}

/// Lower a unary operation.
///
/// `Neg` is implemented as `0 - operand` since LIR has no dedicated
/// negate instruction.
pub(crate) fn lower_unop(
    fl: &mut FunctionLowering,
    block_id: BasicBlockId,
    op: &BytecodeUnOp,
    operand: RegisterId,
) -> LowerResult<RegisterId> {
    let kind = match op {
        BytecodeUnOp::Not => LirInstructionKind::Not(FunctionLowering::reg_val(operand)),
        BytecodeUnOp::Neg => LirInstructionKind::Sub(
            i64_value(0),
            FunctionLowering::reg_val(operand),
        ),
    };
    fl.emit_in_block(block_id, kind)
}

// -----------------------------------------------------------------
// Intrinsics
// -----------------------------------------------------------------

/// Lower a bytecode intrinsic call.
///
/// Returns `None` for void intrinsics (e.g. `Println`).  Returns the
/// result register for value-producing intrinsics (e.g. `Format`,
/// `TimeNow`).
pub(crate) fn lower_intrinsic(
    fl: &mut FunctionLowering,
    block_id: BasicBlockId,
    kind: IntrinsicKind,
    _format: Option<&str>,
    args: Vec<LirValue>,
) -> LowerResult<Option<RegisterId>> {
    match kind {
        IntrinsicKind::Println | IntrinsicKind::Print | IntrinsicKind::Format => {
            let reg = lower_call_intrinsic(
                fl,
                block_id,
                constants::intrinsic_to_runtime_name(kind),
                &args,
            )?;
            Ok(Some(reg))
        }
        IntrinsicKind::Len => {
            let reg =
                lower_call_intrinsic(fl, block_id, constants::INTRINSIC_CONTAINER_LEN, &args)?;
            Ok(Some(reg))
        }
        IntrinsicKind::TimeNow => {
            let reg = lower_call_intrinsic(fl, block_id, "__bc_time_now", &args)?;
            Ok(Some(reg))
        }
        _ => Err(LowerError::Unsupported(format!(
            "intrinsic {kind:?} not yet lowered"
        ))),
    }
}

// -----------------------------------------------------------------
// Compound value construction
// -----------------------------------------------------------------

/// Pop `count` elements from the simulated stack and emit a call to
/// the named make- intrinsic (e.g. `__bc_make_tuple`).
pub(crate) fn lower_make_compound(
    fl: &mut FunctionLowering,
    block_id: BasicBlockId,
    intrinsic_name: &str,
    count: u32,
) -> LowerResult<RegisterId> {
    let mut element_regs = Vec::with_capacity(count as usize);
    for _ in 0..count {
        element_regs.push(FunctionLowering::reg_val(fl.pop_reg()?));
    }
    element_regs.reverse();
    let mut args = vec![i64_value(count as u64)];
    args.extend(element_regs);
    lower_call_intrinsic(fl, block_id, intrinsic_name, &args)
}

// -----------------------------------------------------------------
// Container access
// -----------------------------------------------------------------

/// Lower `ContainerGet` — dispatch to the array get intrinsic.
///
/// TODO: once `fp-interpret` supports typed container handles, this
/// should dispatch to the appropriate intrinsic based on the
/// container's type tag.
pub(crate) fn lower_container_get(
    fl: &mut FunctionLowering,
    block_id: BasicBlockId,
    container: RegisterId,
    key: RegisterId,
) -> LowerResult<RegisterId> {
    lower_call_intrinsic(
        fl,
        block_id,
        constants::INTRINSIC_ARRAY_GET,
        &[
            FunctionLowering::reg_val(container),
            FunctionLowering::reg_val(key),
        ],
    )
}

// -----------------------------------------------------------------
// Place access (projections)
// -----------------------------------------------------------------

/// Lower `LoadPlace` with an optional projection chain.
///
/// Loads the base local's value, then walks the projection elements,
/// emitting intrinsic calls for each field/index access.
pub(crate) fn lower_load_place(
    fl: &mut FunctionLowering,
    block_id: BasicBlockId,
    place: &BytecodePlace,
) -> LowerResult<RegisterId> {
    let mut current_val = fl.emit_in_block(
        block_id,
        LirInstructionKind::Load {
            address: LirValue::local(place.local, LirType::I64),
            alignment: Some(8),
            volatile: false,
        },
    )?;

    for elem in &place.projection {
        let index_reg = lower_projection_index(fl, block_id, elem)?;
        let get_intrinsic = projection_get_intrinsic(elem);
        current_val = lower_call_intrinsic(
            fl,
            block_id,
            get_intrinsic,
            &[
                FunctionLowering::reg_val(current_val),
                FunctionLowering::reg_val(index_reg),
            ],
        )?;
    }

    Ok(current_val)
}

/// Lower `StorePlace` with an optional projection chain.
///
/// If there are no projections, stores directly to the local's alloca.
/// Otherwise walks the projection chain (loading intermediate handles,
/// applying the innermost update via intrinsic, and storing the new
/// handle back).
pub(crate) fn lower_store_place(
    fl: &mut FunctionLowering,
    block_id: BasicBlockId,
    place: &BytecodePlace,
    value_reg: RegisterId,
) -> LowerResult<()> {
    if place.projection.is_empty() {
        fl.emit_in_block(
            block_id,
            LirInstructionKind::Store {
                value: FunctionLowering::reg_val(value_reg),
                address: LirValue::local(place.local, LirType::I64),
                alignment: Some(8),
                volatile: false,
            },
        )?;
        return Ok(());
    }

    let mut base_val_reg = fl.emit_in_block(
        block_id,
        LirInstructionKind::Load {
            address: LirValue::local(place.local, LirType::I64),
            alignment: Some(8),
            volatile: false,
        },
    )?;

    let last = place.projection.len() - 1;
    for (i, elem) in place.projection.iter().enumerate() {
        let index_reg = lower_projection_index(fl, block_id, elem)?;

        if i == last {
            let set_intrinsic = match elem {
                BytecodePlaceElem::Field(_) => constants::INTRINSIC_TUPLE_SET,
                BytecodePlaceElem::Index(_) => constants::INTRINSIC_ARRAY_GET,
            };
            let new_handle = lower_call_intrinsic(
                fl,
                block_id,
                set_intrinsic,
                &[
                    FunctionLowering::reg_val(base_val_reg),
                    FunctionLowering::reg_val(index_reg),
                    FunctionLowering::reg_val(value_reg),
                ],
            )?;
            fl.emit_in_block(
                block_id,
                LirInstructionKind::Store {
                    value: FunctionLowering::reg_val(new_handle),
                    address: LirValue::local(place.local, LirType::I64),
                    alignment: Some(8),
                    volatile: false,
                },
            )?;
        } else {
            let get_intrinsic = projection_get_intrinsic(elem);
            base_val_reg = lower_call_intrinsic(
                fl,
                block_id,
                get_intrinsic,
                &[
                    FunctionLowering::reg_val(base_val_reg),
                    FunctionLowering::reg_val(index_reg),
                ],
            )?;
        }
    }

    Ok(())
}

// -----------------------------------------------------------------
// Helpers
// -----------------------------------------------------------------

/// Emit a `Call` instruction to a runtime intrinsic and return the
/// result register.
pub(crate) fn lower_call_intrinsic(
    fl: &mut FunctionLowering,
    block_id: BasicBlockId,
    name: &str,
    args: &[LirValue],
) -> LowerResult<RegisterId> {
    fl.emit_in_block(
        block_id,
        LirInstructionKind::Call {
            function: LirValue::function(
                fp_core::lir::LirFunctionRef::Name(fp_core::lir::Name::new(name)),
                LirType::Ptr(Box::new(LirType::I8)),
            ),
            args: args.to_vec(),
            calling_convention: CallingConvention::C,
            tail_call: false,
        },
    )
}

/// Materialise the index operand for a projection element.
///
/// `Field(n)` → constant `n`.
/// `Index(local)` → load from the given local's alloca.
fn lower_projection_index(
    fl: &mut FunctionLowering,
    block_id: BasicBlockId,
    elem: &BytecodePlaceElem,
) -> LowerResult<RegisterId> {
    match elem {
        BytecodePlaceElem::Field(idx) => fl.emit_in_block(
            block_id,
            LirInstructionKind::Add(
                i64_value(*idx as u64),
                i64_value(0),
            ),
        ),
        BytecodePlaceElem::Index(local_idx) => fl.emit_in_block(
            block_id,
            LirInstructionKind::Load {
                address: LirValue::local(*local_idx, LirType::I64),
                alignment: Some(8),
                volatile: false,
            },
        ),
    }
}

/// Return the appropriate get-intrinsic name for a projection element.
fn projection_get_intrinsic(elem: &BytecodePlaceElem) -> &'static str {
    match elem {
        BytecodePlaceElem::Field(_) => constants::INTRINSIC_TUPLE_GET,
        BytecodePlaceElem::Index(_) => constants::INTRINSIC_ARRAY_GET,
    }
}
