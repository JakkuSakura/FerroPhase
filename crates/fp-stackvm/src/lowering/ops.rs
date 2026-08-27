//! Sub-lowerings for individual bytecode operations.
//!
//! Each function takes a [`FunctionLowering`] reference plus the
//! operands and emits the appropriate LIR instruction(s).  These are
//! called from the main dispatch loop in [`FunctionLowering::lower_block`].

use fp_bytecode::{
    BytecodeBinOp, BytecodeConst, BytecodePlace, BytecodePlaceElem, BytecodeUnOp, IntrinsicKind,
};
use fp_core::lir::{
    BasicBlockId, CallingConvention, ComptimeOp, LirConstant, LirFloat, LirInstructionKind,
    LirInteger, LirType, LirValue, RegisterId,
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
            LirInstructionKind::Add(i64_value(0), i64_value(0)),
        ),
        BytecodeConst::Bool(b) => {
            let val = if *b { 1u64 } else { 0u64 };
            fl.emit_in_block(
                block_id,
                LirInstructionKind::Add(i1_value(val), i1_value(0)),
            )
        }
        BytecodeConst::Int(i) => fl.emit_in_block(
            block_id,
            LirInstructionKind::Add(i64_value(*i as u64), i64_value(0)),
        ),
        BytecodeConst::UInt(u) => fl.emit_in_block(
            block_id,
            LirInstructionKind::Add(i64_value(*u), i64_value(0)),
        ),
        BytecodeConst::Float(f) => fl.emit_in_block(
            block_id,
            LirInstructionKind::Add(f64_value(*f), f64_value(0.0)),
        ),
        BytecodeConst::Str(s) => {
            let mut args = Vec::with_capacity(s.len());
            for byte in s.as_bytes() {
                args.push(i64_value(*byte as u64));
            }
            lower_call_intrinsic_typed(
                fl,
                block_id,
                constants::INTRINSIC_STR_CONST,
                &args,
                LirType::Ptr(Box::new(LirType::I8)),
            )
        }
        BytecodeConst::Function(name) => {
            let mut args = Vec::with_capacity(name.len());
            for byte in name.as_bytes() {
                args.push(i64_value(*byte as u64));
            }
            lower_call_intrinsic_typed(
                fl,
                block_id,
                constants::INTRINSIC_STR_CONST,
                &args,
                LirType::Ptr(Box::new(LirType::I8)),
            )
        }
        BytecodeConst::Global(name) => {
            let function = fl
                .bytecode
                .functions
                .iter()
                .find(|function| function.name == *name)
                .ok_or_else(|| {
                    LowerError::Unsupported(format!(
                        "global bytecode constant `{name}` has no executable function"
                    ))
                })?;
            if matches!(function.return_type, LirType::Void) {
                return Err(LowerError::Unsupported(format!(
                    "global bytecode constant `{name}` has a void result"
                )));
            }
            lower_call_intrinsic_typed(fl, block_id, name, &[], function.return_type.clone())
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
                element_regs.push(fl.reg_val(reg)?);
            }
            let mut args = vec![i64_value(element_regs.len() as u64)];
            args.extend(element_regs);
            lower_call_intrinsic(fl, block_id, constants::INTRINSIC_MAKE_TUPLE, &args)
        }
        BytecodeConst::Array(items) => {
            let mut element_regs = Vec::new();
            for item in items {
                let reg = lower_load_const(fl, block_id, item)?;
                element_regs.push(fl.reg_val(reg)?);
            }
            let mut args = vec![i64_value(element_regs.len() as u64)];
            args.extend(element_regs);
            lower_call_intrinsic(fl, block_id, constants::INTRINSIC_MAKE_ARRAY, &args)
        }
        BytecodeConst::List(items) => {
            let mut element_regs = Vec::new();
            for item in items {
                let reg = lower_load_const(fl, block_id, item)?;
                element_regs.push(fl.reg_val(reg)?);
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
                arg_regs.push(fl.reg_val(k)?);
                arg_regs.push(fl.reg_val(v)?);
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
        BytecodeBinOp::Add => LirInstructionKind::Add(fl.reg_val(left)?, fl.reg_val(right)?),
        BytecodeBinOp::Sub => LirInstructionKind::Sub(fl.reg_val(left)?, fl.reg_val(right)?),
        BytecodeBinOp::Mul => LirInstructionKind::Mul(fl.reg_val(left)?, fl.reg_val(right)?),
        BytecodeBinOp::Div => LirInstructionKind::Div(fl.reg_val(left)?, fl.reg_val(right)?),
        BytecodeBinOp::Rem => LirInstructionKind::Rem(fl.reg_val(left)?, fl.reg_val(right)?),
        BytecodeBinOp::And => LirInstructionKind::And(fl.reg_val(left)?, fl.reg_val(right)?),
        BytecodeBinOp::Or => LirInstructionKind::Or(fl.reg_val(left)?, fl.reg_val(right)?),
        BytecodeBinOp::BitXor => LirInstructionKind::Xor(fl.reg_val(left)?, fl.reg_val(right)?),
        BytecodeBinOp::BitAnd => LirInstructionKind::And(fl.reg_val(left)?, fl.reg_val(right)?),
        BytecodeBinOp::BitOr => LirInstructionKind::Or(fl.reg_val(left)?, fl.reg_val(right)?),
        BytecodeBinOp::Shl => LirInstructionKind::Shl(fl.reg_val(left)?, fl.reg_val(right)?),
        BytecodeBinOp::Shr => LirInstructionKind::Shr(fl.reg_val(left)?, fl.reg_val(right)?),
        BytecodeBinOp::Eq => LirInstructionKind::Eq(fl.reg_val(left)?, fl.reg_val(right)?),
        BytecodeBinOp::Ne => LirInstructionKind::Ne(fl.reg_val(left)?, fl.reg_val(right)?),
        BytecodeBinOp::Lt => LirInstructionKind::Lt(fl.reg_val(left)?, fl.reg_val(right)?),
        BytecodeBinOp::Le => LirInstructionKind::Le(fl.reg_val(left)?, fl.reg_val(right)?),
        BytecodeBinOp::Ge => LirInstructionKind::Ge(fl.reg_val(left)?, fl.reg_val(right)?),
        BytecodeBinOp::Gt => LirInstructionKind::Gt(fl.reg_val(left)?, fl.reg_val(right)?),
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
        BytecodeUnOp::Not => LirInstructionKind::Not(fl.reg_val(operand)?),
        BytecodeUnOp::Neg => LirInstructionKind::Sub(i64_value(0), fl.reg_val(operand)?),
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
    result_type: LirType,
) -> LowerResult<Option<RegisterId>> {
    match kind {
        IntrinsicKind::CompileWarning | IntrinsicKind::CompileError => {
            let message = args.into_iter().next().ok_or_else(|| {
                LowerError::Internal(format!("intrinsic {kind:?} requires one argument"))
            })?;
            let op = match kind {
                IntrinsicKind::CompileWarning => ComptimeOp::CompileWarning { message },
                IntrinsicKind::CompileError => ComptimeOp::CompileError { message },
                _ => unreachable!(),
            };
            let reg =
                fl.emit_typed_in_block(block_id, LirInstructionKind::ComptimeOp(op), result_type)?;
            Ok(Some(reg))
        }
        IntrinsicKind::Println | IntrinsicKind::Print => {
            lower_call_intrinsic_void(
                fl,
                block_id,
                constants::intrinsic_to_runtime_name(kind),
                &args,
            )?;
            Ok(None)
        }
        IntrinsicKind::Format => {
            let reg = lower_call_intrinsic_typed(
                fl,
                block_id,
                constants::intrinsic_to_runtime_name(kind),
                &args,
                result_type,
            )?;
            Ok(Some(reg))
        }
        IntrinsicKind::Len => {
            let reg = lower_call_intrinsic_typed(
                fl,
                block_id,
                constants::INTRINSIC_CONTAINER_LEN,
                &args,
                result_type,
            )?;
            Ok(Some(reg))
        }
        IntrinsicKind::TimeNow => {
            let reg =
                lower_call_intrinsic_typed(fl, block_id, "__bc_time_now", &args, result_type)?;
            Ok(Some(reg))
        }
        IntrinsicKind::Panic => {
            let reg = lower_call_intrinsic_typed(fl, block_id, "__bc_panic", &args, result_type)?;
            Ok(Some(reg))
        }
        IntrinsicKind::CatchUnwind => {
            if args.len() != 1 {
                return Err(LowerError::Internal(format!(
                    "intrinsic CatchUnwind requires 1 argument, got {}",
                    args.len()
                )));
            }
            let reg = lower_call_intrinsic_typed(
                fl,
                block_id,
                constants::intrinsic_to_runtime_name(kind),
                &args,
                result_type,
            )?;
            Ok(Some(reg))
        }
        IntrinsicKind::JsonParse => {
            let reg =
                lower_call_intrinsic_typed(fl, block_id, "__bc_json_parse", &args, result_type)?;
            Ok(Some(reg))
        }
        IntrinsicKind::Slice => {
            if args.len() != 3 {
                return Err(LowerError::Internal(format!(
                    "intrinsic Slice requires 3 arguments, got {}",
                    args.len()
                )));
            }
            let reg = lower_call_intrinsic_typed(
                fl,
                block_id,
                constants::INTRINSIC_SLICE,
                &args,
                result_type,
            )?;
            Ok(Some(reg))
        }
        IntrinsicKind::FsExists | IntrinsicKind::FsIsFile | IntrinsicKind::FsIsDir => {
            let name = match kind {
                IntrinsicKind::FsExists => "__bc_fs_exists",
                IntrinsicKind::FsIsFile => "__bc_fs_is_file",
                IntrinsicKind::FsIsDir => "__bc_fs_is_dir",
                _ => unreachable!(),
            };
            let reg = lower_call_intrinsic_typed(fl, block_id, name, &args, result_type)?;
            Ok(Some(reg))
        }
        IntrinsicKind::FsReadToString => {
            let reg = lower_call_intrinsic_typed(
                fl,
                block_id,
                "__bc_fs_read_to_string",
                &args,
                result_type,
            )?;
            Ok(Some(reg))
        }
        IntrinsicKind::FsWriteString | IntrinsicKind::FsAppendString => {
            let name = match kind {
                IntrinsicKind::FsWriteString => "__bc_fs_write_string",
                IntrinsicKind::FsAppendString => "__bc_fs_append_string",
                _ => unreachable!(),
            };
            let reg = lower_call_intrinsic_typed(fl, block_id, name, &args, result_type)?;
            Ok(Some(reg))
        }
        IntrinsicKind::FsCreateDirAll
        | IntrinsicKind::FsRemoveFile
        | IntrinsicKind::FsRemoveDirAll => {
            let name = match kind {
                IntrinsicKind::FsCreateDirAll => "__bc_fs_create_dir_all",
                IntrinsicKind::FsRemoveFile => "__bc_fs_remove_file",
                IntrinsicKind::FsRemoveDirAll => "__bc_fs_remove_dir_all",
                _ => unreachable!(),
            };
            let reg = lower_call_intrinsic_typed(fl, block_id, name, &args, result_type)?;
            Ok(Some(reg))
        }
        IntrinsicKind::FsReadDir => {
            let reg =
                lower_call_intrinsic_typed(fl, block_id, "__bc_fs_read_dir", &args, result_type)?;
            Ok(Some(reg))
        }
        IntrinsicKind::FsWalkDir => {
            let reg =
                lower_call_intrinsic_typed(fl, block_id, "__bc_fs_walk_dir", &args, result_type)?;
            Ok(Some(reg))
        }
        IntrinsicKind::FsGlob => {
            let reg = lower_call_intrinsic_typed(fl, block_id, "__bc_fs_glob", &args, result_type)?;
            Ok(Some(reg))
        }
        IntrinsicKind::PathJoin
        | IntrinsicKind::PathParent
        | IntrinsicKind::PathFileName
        | IntrinsicKind::PathExtension
        | IntrinsicKind::PathStem
        | IntrinsicKind::PathNormalize
        | IntrinsicKind::PathIsAbsolute => {
            let name = match kind {
                IntrinsicKind::PathJoin => "__bc_path_join",
                IntrinsicKind::PathParent => "__bc_path_parent",
                IntrinsicKind::PathFileName => "__bc_path_file_name",
                IntrinsicKind::PathExtension => "__bc_path_extension",
                IntrinsicKind::PathStem => "__bc_path_stem",
                IntrinsicKind::PathNormalize => "__bc_path_normalize",
                IntrinsicKind::PathIsAbsolute => "__bc_path_is_absolute",
                _ => unreachable!(),
            };
            let reg = lower_call_intrinsic_typed(fl, block_id, name, &args, result_type)?;
            Ok(Some(reg))
        }
        IntrinsicKind::EnvCurrentDir
        | IntrinsicKind::EnvTempDir
        | IntrinsicKind::EnvHomeDir
        | IntrinsicKind::EnvVar
        | IntrinsicKind::EnvVarExists => {
            let name = match kind {
                IntrinsicKind::EnvCurrentDir => "__bc_env_current_dir",
                IntrinsicKind::EnvTempDir => "__bc_env_temp_dir",
                IntrinsicKind::EnvHomeDir => "__bc_env_home_dir",
                IntrinsicKind::EnvVar => "__bc_env_var",
                IntrinsicKind::EnvVarExists => "__bc_env_var_exists",
                _ => unreachable!(),
            };
            let reg = lower_call_intrinsic_typed(fl, block_id, name, &args, result_type)?;
            Ok(Some(reg))
        }
        IntrinsicKind::IoReadStdinToString
        | IntrinsicKind::IoWriteStdout
        | IntrinsicKind::IoWriteStderr => {
            let name = match kind {
                IntrinsicKind::IoReadStdinToString => "__bc_io_read_stdin_to_string",
                IntrinsicKind::IoWriteStdout => "__bc_io_write_stdout",
                IntrinsicKind::IoWriteStderr => "__bc_io_write_stderr",
                _ => unreachable!(),
            };
            let reg = lower_call_intrinsic_typed(fl, block_id, name, &args, result_type)?;
            Ok(Some(reg))
        }
        IntrinsicKind::DebugAssertions => {
            let reg = lower_call_intrinsic_typed(
                fl,
                block_id,
                "__bc_debug_assertions",
                &args,
                result_type,
            )?;
            Ok(Some(reg))
        }
        IntrinsicKind::Input => {
            let reg = lower_call_intrinsic_typed(fl, block_id, "__bc_input", &args, result_type)?;
            Ok(Some(reg))
        }
        IntrinsicKind::TypeName => {
            let reg =
                lower_call_intrinsic_typed(fl, block_id, "__bc_type_name", &args, result_type)?;
            Ok(Some(reg))
        }
        IntrinsicKind::TypeOf => {
            if args.len() != 1 {
                return Err(LowerError::Internal(format!(
                    "intrinsic TypeOf requires 1 argument, got {}",
                    args.len()
                )));
            }
            let reg = lower_call_intrinsic_typed(fl, block_id, "__bc_type_of", &args, result_type)?;
            Ok(Some(reg))
        }
        IntrinsicKind::Sleep => {
            let reg = lower_call_intrinsic_typed(fl, block_id, "__bc_sleep", &args, result_type)?;
            Ok(Some(reg))
        }
        IntrinsicKind::Spawn | IntrinsicKind::Join | IntrinsicKind::Select => {
            if args.is_empty() {
                return Err(LowerError::Internal(format!(
                    "intrinsic {kind:?} requires at least one argument"
                )));
            }
            let reg = lower_call_intrinsic_typed(
                fl,
                block_id,
                constants::intrinsic_to_runtime_name(kind),
                &args,
                result_type,
            )?;
            Ok(Some(reg))
        }
        IntrinsicKind::Yield => {
            lower_call_intrinsic_void(
                fl,
                block_id,
                constants::intrinsic_to_runtime_name(kind),
                &args,
            )?;
            Ok(None)
        }
        IntrinsicKind::SizeOf => {
            let reg = lower_call_intrinsic_typed(fl, block_id, "__bc_size_of", &args, result_type)?;
            Ok(Some(reg))
        }
        IntrinsicKind::FieldCount => {
            let reg =
                lower_call_intrinsic_typed(fl, block_id, "__bc_field_count", &args, result_type)?;
            Ok(Some(reg))
        }
        IntrinsicKind::FieldNameAt => {
            let reg =
                lower_call_intrinsic_typed(fl, block_id, "__bc_field_name_at", &args, result_type)?;
            Ok(Some(reg))
        }
        IntrinsicKind::HasField => {
            let reg =
                lower_call_intrinsic_typed(fl, block_id, "__bc_has_field", &args, result_type)?;
            Ok(Some(reg))
        }
        IntrinsicKind::HasMethod => {
            let reg =
                lower_call_intrinsic_typed(fl, block_id, "__bc_has_method", &args, result_type)?;
            Ok(Some(reg))
        }
        IntrinsicKind::MethodCount => {
            let reg =
                lower_call_intrinsic_typed(fl, block_id, "__bc_method_count", &args, result_type)?;
            Ok(Some(reg))
        }
        IntrinsicKind::FieldType => {
            let reg =
                lower_call_intrinsic_typed(fl, block_id, "__bc_field_type", &args, result_type)?;
            Ok(Some(reg))
        }
        IntrinsicKind::StructSize => {
            let reg =
                lower_call_intrinsic_typed(fl, block_id, "__bc_struct_size", &args, result_type)?;
            Ok(Some(reg))
        }
        IntrinsicKind::ReflectFields => {
            let reg = lower_call_intrinsic_typed(
                fl,
                block_id,
                "__bc_reflect_fields",
                &args,
                result_type,
            )?;
            Ok(Some(reg))
        }
        IntrinsicKind::CreateStruct => {
            let reg =
                lower_call_intrinsic_typed(fl, block_id, "__bc_create_struct", &args, result_type)?;
            Ok(Some(reg))
        }
        IntrinsicKind::YamlToJson => {
            if args.len() != 1 {
                return Err(LowerError::Internal(format!(
                    "intrinsic YamlToJson requires 1 argument, got {}",
                    args.len()
                )));
            }
            let reg = lower_call_intrinsic_typed(
                fl,
                block_id,
                constants::intrinsic_to_runtime_name(kind),
                &args,
                result_type,
            )?;
            Ok(Some(reg))
        }
        IntrinsicKind::ShellExec => {
            if args.len() != 1 {
                return Err(LowerError::Internal(format!(
                    "intrinsic ShellExec requires 1 argument, got {}",
                    args.len()
                )));
            }
            let reg = lower_call_intrinsic_typed(
                fl,
                block_id,
                constants::intrinsic_to_runtime_name(kind),
                &args,
                result_type,
            )?;
            Ok(Some(reg))
        }
        IntrinsicKind::ProcMacroTokenStreamFromStr => {
            if args.len() != 1 {
                return Err(LowerError::Internal(format!(
                    "intrinsic ProcMacroTokenStreamFromStr requires 1 argument, got {}",
                    args.len()
                )));
            }
            let reg = lower_call_intrinsic_typed(
                fl,
                block_id,
                constants::intrinsic_to_runtime_name(kind),
                &args,
                result_type,
            )?;
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
    let element_count = if intrinsic_name == constants::INTRINSIC_MAKE_MAP {
        count.saturating_mul(2)
    } else {
        count
    };
    let mut element_regs = Vec::with_capacity(element_count as usize);
    for _ in 0..element_count {
        let element_reg = fl.pop_reg()?;
        element_regs.push(fl.reg_val(element_reg)?);
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
        &[fl.reg_val(container)?, fl.reg_val(key)?],
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
    let local_type = fl.local_type(place.local)?;
    let mut current_val = fl.emit_in_block(
        block_id,
        LirInstructionKind::Load {
            address: LirValue::local(place.local, local_type),
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
            &[fl.reg_val(current_val)?, fl.reg_val(index_reg)?],
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
        let value_type = fl.register_types.get(&value_reg).cloned().ok_or_else(|| {
            LowerError::Internal(format!("register %{value_reg} has no lowered type"))
        })?;
        fl.set_local_type(place.local, value_type.clone())?;
        fl.emit_void_in_block(
            block_id,
            LirInstructionKind::Store {
                value: fl.reg_val(value_reg)?,
                address: LirValue::local(place.local, value_type),
                alignment: Some(8),
                volatile: false,
            },
        )?;
        return Ok(());
    }

    let local_type = fl.local_type(place.local)?;
    let mut base_val_reg = fl.emit_in_block(
        block_id,
        LirInstructionKind::Load {
            address: LirValue::local(place.local, local_type.clone()),
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
                BytecodePlaceElem::Index(_) => constants::INTRINSIC_ARRAY_SET,
            };
            let new_handle = lower_call_intrinsic(
                fl,
                block_id,
                set_intrinsic,
                &[
                    fl.reg_val(base_val_reg)?,
                    fl.reg_val(index_reg)?,
                    fl.reg_val(value_reg)?,
                ],
            )?;
            fl.emit_void_in_block(
                block_id,
                LirInstructionKind::Store {
                    value: fl.reg_val(new_handle)?,
                    address: LirValue::local(place.local, local_type.clone()),
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
                &[fl.reg_val(base_val_reg)?, fl.reg_val(index_reg)?],
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
    lower_call_intrinsic_typed(fl, block_id, name, args, LirType::I64)
}

pub(crate) fn lower_call_intrinsic_typed(
    fl: &mut FunctionLowering,
    block_id: BasicBlockId,
    name: &str,
    args: &[LirValue],
    result_type: LirType,
) -> LowerResult<RegisterId> {
    fl.emit_typed_in_block(
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
        result_type,
    )
}

fn lower_call_intrinsic_void(
    fl: &mut FunctionLowering,
    block_id: BasicBlockId,
    name: &str,
    args: &[LirValue],
) -> LowerResult<()> {
    fl.emit_void_in_block(
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
    )?;
    Ok(())
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
            LirInstructionKind::Add(i64_value(*idx as u64), i64_value(0)),
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
