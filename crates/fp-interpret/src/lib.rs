mod ffi;
mod host_globals;
mod host_functions;
mod integer;
mod interpreter;
mod render;
mod vm;
pub use interpreter::LirInterpreter;
pub use host_globals::{HostGlobal, HostGlobalDescriptor, HostGlobalError, HostGlobalRegistry};
pub use host_functions::{HostFunction, HostFunctionError, HostFunctionRegistry};
pub(crate) use interpreter::TypedValue;

use std::collections::HashMap;
use std::ffi::CString;
use std::rc::Rc;

use fp_core::ast::package::PackageId;
use fp_core::ast::{
    Ty, TypePrimitive, TypeStruct, TypeType, TypeUnknown, Value, ValueList, ValueMapEntry,
    ValueTuple,
};
use fp_core::lir::{
    BasicBlockId, ComptimeOp, LirBasicBlock, LirBlob, LirConstant,
    LirConstantAggregate, LirConstantData, LirConstantExpr, LirConstantKind, LirDataLayout,
    LirFloat, LirFunction, LirFunctionRef, LirInstruction, LirInstructionKind, LirInteger,
    LirLocal, LirTerminator, LirType, LirValue, LirValueKind, Name, RegisterId,
};
use fp_ffi::{FfiRuntime, FfiSignature, FfiType};

use crate::vm::{ThreadState, lir_type_info, mem_load, mem_store};
use integer::*;

pub use crate::vm::VmError;

type LirResult<T> = Result<T, VmError>;

/// The Rust-side implementation of `std::intrinsics::primitive_type` —
/// the single canonical string->`ast::Ty` mapping for a primitive/
/// reference-to-primitive type-value name, reusing `TypePrimitive::
/// from_name` (the same reverse mapping the surface-syntax type-expr
/// parser's names round-trip through). A `&`-prefixed name (optionally
/// carrying a `'lifetime ` token, e.g. `"&'static str"`) recurses on the
/// inner name and wraps the result in `Ty::reference`.
fn primitive_type_value_ty(name: &str) -> Option<Ty> {
    if let Some(rest) = name.strip_prefix('&') {
        let rest = rest.trim_start();
        let rest = rest
            .strip_prefix('\'')
            .map(|after_quote| {
                after_quote
                    .find(char::is_whitespace)
                    .map(|idx| after_quote[idx..].trim_start())
                    .unwrap_or("")
            })
            .unwrap_or(rest);
        return primitive_type_value_ty(rest).map(Ty::reference);
    }
    TypePrimitive::from_name(name).map(Ty::Primitive)
}

/// Flattens a `Ty::Literal`/`Ty::TypeBinaryOp(Union)` tree of string
/// literal types into its member strings, in left-to-right order — the
/// same shape `unionify` (`ComptimeOp::Unionify`) both reads and rebuilds.
/// `None` if `ty` isn't (recursively) built purely from string literal
/// types and unions of them.
fn collect_literal_union_members(ty: &Ty) -> Option<Vec<String>> {
    match ty {
        Ty::Literal(lit) => Some(vec![lit.value.clone()]),
        Ty::TypeBinaryOp(op) if matches!(op.kind, fp_core::ast::TypeBinaryOpKind::Union) => {
            let mut lhs = collect_literal_union_members(&op.lhs)?;
            let rhs = collect_literal_union_members(&op.rhs)?;
            lhs.extend(rhs);
            Some(lhs)
        }
        _ => None,
    }
}

/// The inverse of `collect_literal_union_members` — rebuilds a
/// `Ty::Literal`/`Ty::TypeBinaryOp(Union)` tree from a flat list of
/// strings, left-associated (matching how the parser's own `|` chains
/// associate).
fn build_literal_union(values: Vec<String>) -> Ty {
    let mut iter = values.into_iter();
    let first = iter
        .next()
        .map(|value| Ty::Literal(fp_core::ast::TypeLiteralString { value }))
        .unwrap_or(Ty::Literal(fp_core::ast::TypeLiteralString {
            value: String::new(),
        }));
    iter.fold(first, |acc, value| {
        Ty::TypeBinaryOp(Box::new(fp_core::ast::TypeBinaryOp {
            kind: fp_core::ast::TypeBinaryOpKind::Union,
            lhs: Box::new(acc),
            rhs: Box::new(Ty::Literal(fp_core::ast::TypeLiteralString { value })),
        }))
    })
}

impl Default for LirInterpreter {
    fn default() -> Self {
        Self::new()
    }
}

/// Convert a LIR function signature to an FFI signature.
fn lir_sig_to_ffi(sig: &fp_core::lir::LirFunctionSignature) -> FfiSignature {
    let args = sig.params.iter().map(lir_ty_to_ffi).collect();
    let ret = lir_ty_to_ffi(&sig.return_type);
    FfiSignature { args, ret }
}

fn lir_ty_to_ffi(ty: &LirType) -> FfiType {
    match ty {
        LirType::Ptr(_) => FfiType::Ptr,
        LirType::Void => FfiType::Void,
        // All scalar types pass through 64-bit registers.
        _ => FfiType::U64,
    }
}

/// `TargetBackend` for the `--target interpret` target — merges the
/// package's LIR off the shared workspace exactly like `NativeEmitter`
/// does, then runs it directly instead of emitting an artifact.
/// `emit_package_artifact`'s `Result<()>` has no channel for the interpreted
/// `Value`, so it's printed as a side effect; the CLI previously discarded
/// this value entirely, so this is new information, not a regression.
pub struct InterpreterBackend;

impl fp_core::backend::TargetBackend for InterpreterBackend {
    fn capabilities(&self) -> fp_core::capabilities::LanguageCapabilities {
        fp_core::capabilities::LanguageCapabilities::NATIVE
    }

    fn emit_package_artifact(
        &self,
        workspace: &fp_core::ast::program::AstProgram,
        package_id: &PackageId,
        mir: &fp_core::mir::MirCodeUnit,
        lir: Option<&fp_core::lir::LirBlob>,
    ) -> fp_core::error::Result<()> {
        let _ = mir;
        let _ = workspace;
        let lir = lir
            .ok_or_else(|| {
                fp_core::error::Error::from(format!("package `{package_id}` has no compiled LIR"))
            })?
            .clone();
        let def_id = lir
            .functions
            .iter()
            .find(|function| function.name.as_str() == "main")
            .and_then(|function| function.def_id.clone())
            .ok_or_else(|| {
                fp_core::error::Error::from(format!(
                    "package `{package_id}` has no `main` entrypoint"
                ))
            })?;
        let program = fp_core::lir::LirProgram::from_single_blob(package_id.clone(), lir);
        let mut interpreter = LirInterpreter::new();
        interpreter
            .load_program(Rc::new(program))
            .map_err(|error| fp_core::error::Error::from(error.to_string()))?;
        let value = interpreter
            .run_entrypoint(package_id, &def_id)
            .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
        println!("{value:?}");
        Ok(())
    }
}

#[cfg(test)]
mod tests;
