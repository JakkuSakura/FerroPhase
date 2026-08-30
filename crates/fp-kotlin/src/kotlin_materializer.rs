use fp_core::ast::{
    BlockStmt, Expr, ExprAssign, ExprAwait, ExprBinOp, ExprBlock, ExprClosure, ExprIntrinsicCall,
    ExprIntrinsicContainer, ExprInvoke, ExprInvokeTarget, ExprKind, ExprSelect, ExprSelectType,
    Ident, Name, Path, Pattern, PatternIdent, PatternKind, PatternType, Ty, TySlot, TypeInt,
    TypePrimitive, Value,
};
use fp_core::error::Result;
use fp_core::intrinsics::{CallKind, IntrinsicMaterializer, PortableOpCall};
use fp_core::ops::BinOpKind;

/// Materializes fp-lang portable operations into Kotlin AST constructs.
///
/// The Rust frontend supplies operation identity via `#[op]`; this target
/// pass selects Kotlin's nullable, collection, and exception equivalents.
pub struct KotlinMaterializer;

impl IntrinsicMaterializer for KotlinMaterializer {
    fn capabilities(&self) -> fp_core::capabilities::LanguageCapabilities {
        crate::CAPABILITIES
    }

    fn materialize_portable_op(
        &self,
        call: &mut PortableOpCall,
        ty: &TySlot,
    ) -> Result<Option<Expr>> {
        let receiver = || {
            call.args
                .first()
                .cloned()
                .unwrap_or_else(|| Expr::value(Value::Null(Default::default())))
        };
        match call.op.name() {
            "default" => Ok(kotlin_default_value(ty)),
            "from_str" | "str_parse" => Ok(Some(runtime_method("parse", vec![receiver()]))),
            "option_some" | "as_ref" | "iter" | "as_str" | "as_deref" => Ok(Some(receiver())),
            // Kotlin strings and paths are immutable values, but arrays and mutable
            // lists must still receive a real copy. Keeping this here prevents the
            // generic serializer fallback from emitting Rust's `clone` or Kotlin's
            // data-class-only `copy()` for standard-library collection values.
            "clone" | "to_owned" => Ok(Some(kotlin_owned_value(receiver(), ty))),
            "option_none" => Ok(Some(Expr::value(Value::Null(Default::default())))),
            "option_unwrap" => Ok(Some(runtime_method("optionUnwrap", vec![receiver()]))),
            "option_take" => Ok(Some(receiver())),
            "option_filter" => Ok(Some(invoke_method(
                receiver(),
                "takeIf",
                portable_op_args_after_receiver(call),
            ))),
            "result_ok" => Ok(Some(runtime_method(
                "resultSuccess",
                vec![result_success_arg(call)],
            ))),
            "result_err" => Ok(Some(runtime_method(
                "resultFailure",
                vec![normalize_error(result_constructor_arg(call))],
            ))),
            "result_propagate" => {
                let source = call.args.first().cloned().unwrap_or_else(Expr::unit);
                // Kotlin has no postfix `?`. The source expression is a
                // Result<T>, and this operation is the one place where Rust
                // propagates the error while producing T for the caller.
                // Lower it explicitly so the serializer cannot accidentally
                // pass Result<T> to code expecting T.
                Ok(Some(runtime_method("resultUnwrap", vec![source])))
            }
            "result_map" => Ok(Some(runtime_method(
                "mapResult",
                std::iter::once(receiver())
                    .chain(portable_op_args_after_receiver(call))
                    .collect(),
            ))),
            "result_map_err" => Ok(Some(runtime_method(
                "mapError",
                vec![receiver(), error_mapping(call)],
            ))),
            "result_is_ok" => Ok(Some(runtime_method("resultIsSuccess", vec![receiver()]))),
            "result_is_err" => Ok(Some(runtime_method("resultIsFailure", vec![receiver()]))),
            "result_ok_value" => Ok(Some(runtime_method("resultOkValue", vec![receiver()]))),
            "result_err_value" => Ok(Some(runtime_method("resultErrValue", vec![receiver()]))),
            "result_unwrap" => Ok(Some(runtime_method("resultUnwrap", vec![receiver()]))),
            "result_unwrap_or" => Ok(Some(runtime_method(
                "resultDefault",
                std::iter::once(receiver())
                    .chain(portable_op_args_after_receiver(call))
                    .collect(),
            ))),
            "io_error_new" => Ok(Some(runtime_method(
                "ioError",
                call.args.iter().skip(1).cloned().collect(),
            ))),
            "unwrap_or" => Ok(Some(runtime_method(
                "unwrapOr",
                std::iter::once(receiver())
                    .chain(portable_op_args_after_receiver(call))
                    .collect(),
            ))),
            "map_or" => Ok(Some(runtime_method(
                "mapOr",
                std::iter::once(receiver())
                    .chain(portable_op_args_after_receiver(call))
                    .collect(),
            ))),
            "vec_new" if is_byte_vector(ty) => Ok(Some(invoke_function(
                "ByteArray",
                vec![Expr::value(Value::int(0))],
            ))),
            "vec_new" => Ok(Some(invoke_function("mutableListOf", Vec::new()))),
            "vec_from" => Ok(Some(invoke_method(
                result_constructor_arg(call),
                if is_byte_vector(ty) {
                    "toByteArray"
                } else {
                    "toMutableList"
                },
                Vec::new(),
            ))),
            "vec_push" if is_byte_vector(ty) => Ok(Some(assign_byte_vector(
                receiver(),
                runtime_method("appendByte", call.args.clone()),
            ))),
            "vec_push" => Ok(Some(runtime_method(
                "listPush",
                std::iter::once(receiver())
                    .chain(portable_op_args_after_receiver(call))
                    .collect(),
            ))),
            "vec_extend" if is_byte_vector(ty) => Ok(Some(assign_byte_vector(
                receiver(),
                runtime_method("appendBytes", call.args.clone()),
            ))),
            "vec_extend" => Ok(Some(runtime_method(
                "listExtend",
                std::iter::once(receiver())
                    .chain(portable_op_args_after_receiver(call))
                    .collect(),
            ))),
            // These are ordinary Kotlin collection operations. Keep the
            // lowering in the target materializer so the serializer sees
            // target-shaped method calls, and preserve ByteArray's distinct
            // JVM representation where it is required by the inferred type.
            "vec_from_iter" | "collect" => {
                if is_byte_vector(ty) {
                    Ok(Some(invoke_method(receiver(), "toByteArray", Vec::new())))
                } else {
                    Ok(Some(invoke_method(receiver(), "toMutableList", Vec::new())))
                }
            }
            "slice_to_vec" | "slice_to_vec_in" => {
                if is_byte_vector(ty) {
                    Ok(Some(invoke_method(receiver(), "toByteArray", Vec::new())))
                } else {
                    Ok(Some(invoke_method(receiver(), "toMutableList", Vec::new())))
                }
            }
            "filter" => Ok(Some(invoke_method(
                receiver(),
                "filter",
                portable_op_args_after_receiver(call),
            ))),
            "trim" | "trim_end" | "trim_start" => Ok(Some(invoke_method(
                receiver(),
                match call.op.name() {
                    "trim" => "trim",
                    "trim_end" => "trimEnd",
                    _ => "trimStart",
                },
                Vec::new(),
            ))),
            "is_none" => Ok(Some(Expr::new(ExprKind::BinOp(ExprBinOp {
                span: Default::default(),
                kind: BinOpKind::Eq,
                lhs: Box::new(receiver()),
                rhs: Box::new(Expr::value(Value::Null(Default::default()))),
            })))),
            "position" => {
                let mut args = call.args.drain(..);
                let receiver = args
                    .next()
                    .unwrap_or_else(|| Expr::value(Value::Null(Default::default())));
                Ok(Some(invoke_method(
                    receiver,
                    "indexOfFirst",
                    args.collect(),
                )))
            }
            "split_whitespace" => Ok(Some(runtime_method("splitWhitespace", vec![receiver()]))),
            "str_char_indices" => Ok(Some(runtime_method("charIndices", vec![receiver()]))),
            "str_split_at" => Ok(Some(runtime_method(
                "splitAt",
                std::iter::once(receiver())
                    .chain(portable_op_args_after_receiver(call))
                    .collect(),
            ))),
            "str_strip_prefix" => Ok(Some(runtime_method(
                "stripPrefix",
                std::iter::once(receiver())
                    .chain(portable_op_args_after_receiver(call))
                    .collect(),
            ))),
            "bool_then_some" => Ok(Some(runtime_method(
                "thenSome",
                std::iter::once(receiver())
                    .chain(portable_op_args_after_receiver(call))
                    .collect(),
            ))),
            "find_map" => Ok(Some(runtime_method(
                "findMap",
                std::iter::once(receiver())
                    .chain(portable_op_args_after_receiver(call))
                    .collect(),
            ))),
            "range_inclusive_contains" => Ok(Some(runtime_method(
                "rangeInclusiveContains",
                std::iter::once(receiver())
                    .chain(portable_op_args_after_receiver(call))
                    .collect(),
            ))),
            "split" => Ok(Some(runtime_method(
                "splitString",
                std::iter::once(receiver())
                    .chain(portable_op_args_after_receiver(call))
                    .collect(),
            ))),
            "lines" => Ok(Some(invoke_method(receiver(), "lines", Vec::new()))),
            "starts_with" => Ok(Some(invoke_method(
                receiver(),
                "startsWith",
                portable_op_args_after_receiver(call),
            ))),
            "ends_with" => Ok(Some(invoke_method(
                receiver(),
                "endsWith",
                portable_op_args_after_receiver(call),
            ))),
            "char_is_digit" => Ok(Some(runtime_method("charIsDigit", call.args.clone()))),
            "char_is_alphabetic" => Ok(Some(runtime_method("charIsAlphabetic", vec![receiver()]))),
            "char_is_whitespace" => Ok(Some(runtime_method("charIsWhitespace", vec![receiver()]))),
            "char_is_ascii_alphabetic" => Ok(Some(runtime_method(
                "charIsAsciiAlphabetic",
                vec![receiver()],
            ))),
            "char_is_ascii_digit" => Ok(Some(runtime_method("charIsAsciiDigit", vec![receiver()]))),
            "char_is_ascii_hexdigit" => Ok(Some(runtime_method(
                "charIsAsciiHexDigit",
                vec![receiver()],
            ))),
            "string_from_utf8_lossy" | "string_from_utf8" => {
                Ok(Some(runtime_method("decodeUtf8", vec![receiver()])))
            }
            "fs_read" => Ok(Some(run_catching(invoke_static_method(
                &["java", "nio", "file", "Files"],
                "readAllBytes",
                call.args.clone(),
            )))),
            "fs_read_dir" => Ok(Some(runtime_method("readDirectory", call.args.clone()))),
            "fs_create_dir" => Ok(Some(runtime_method("createDirectory", call.args.clone()))),
            "fs_create_dir_all" => Ok(Some(runtime_method("createDirectories", call.args.clone()))),
            "file_create" => Ok(Some(runtime_method("createFile", call.args.clone()))),
            "fs_canonicalize" => Ok(Some(runtime_method("canonicalize", call.args.clone()))),
            "path_canonicalize" => Ok(Some(runtime_method("canonicalize", vec![receiver()]))),
            "path_exists" => Ok(Some(invoke_static_method(
                &["java", "nio", "file", "Files"],
                "exists",
                vec![receiver()],
            ))),
            "path_parent" => Ok(Some(select_property(receiver(), "parent"))),
            "path_to_path_buf" => Ok(Some(receiver())),
            "path_join" => Ok(Some(invoke_method(
                receiver(),
                "resolve",
                portable_op_args_after_receiver(call),
            ))),
            "path_file_name" => Ok(Some(select_property(receiver(), "fileName"))),
            "path_to_string_lossy" | "os_str_to_string_lossy" => {
                Ok(Some(invoke_method(receiver(), "toString", Vec::new())))
            }
            "dir_entry_path" => Ok(Some(invoke_method(receiver(), "path", Vec::new()))),
            "dir_entry_file_type" => Ok(Some(run_catching(invoke_method(
                receiver(),
                "fileType",
                Vec::new(),
            )))),
            "dir_entry_file_name" => Ok(Some(invoke_method(receiver(), "fileName", Vec::new()))),
            "file_type_is_dir" => Ok(Some(invoke_method(receiver(), "isDirectory", Vec::new()))),
            "slice_join" => Ok(Some(invoke_method(
                receiver(),
                "joinToString",
                portable_op_args_after_receiver(call),
            ))),
            "duration_from_secs" => Ok(Some(invoke_static_method(
                &["java", "time", "Duration"],
                "ofSeconds",
                call.args.clone(),
            ))),
            "duration_from_millis" => Ok(Some(invoke_static_method(
                &["java", "time", "Duration"],
                "ofMillis",
                call.args.clone(),
            ))),
            "write_all" => Ok(Some(runtime_method(
                "writeAll",
                std::iter::once(receiver())
                    .chain(portable_op_args_after_receiver(call))
                    .collect(),
            ))),
            "command_new" => Ok(Some(runtime_method("command", call.args.clone()))),
            "command_arg" => Ok(Some(runtime_method(
                "commandArg",
                std::iter::once(receiver())
                    .chain(portable_op_args_after_receiver(call))
                    .collect(),
            ))),
            "command_args" => Ok(Some(runtime_method(
                "commandArgs",
                std::iter::once(receiver())
                    .chain(portable_op_args_after_receiver(call))
                    .collect(),
            ))),
            "command_current_dir" => Ok(Some(runtime_method(
                "commandCurrentDir",
                std::iter::once(receiver())
                    .chain(portable_op_args_after_receiver(call))
                    .collect(),
            ))),
            "command_stdin" | "command_stdout" | "command_stderr" => Ok(Some(runtime_method(
                match call.op.name() {
                    "command_stdin" => "commandStdin",
                    "command_stdout" => "commandStdout",
                    "command_stderr" => "commandStderr",
                    _ => unreachable!(),
                },
                std::iter::once(receiver())
                    .chain(portable_op_args_after_receiver(call))
                    .collect(),
            ))),
            "command_spawn" => Ok(Some(runtime_method("commandSpawn", vec![receiver()]))),
            "command_output" => Ok(Some(runtime_method("commandOutput", vec![receiver()]))),
            "command_status" => Ok(Some(runtime_method("commandStatus", vec![receiver()]))),
            "stdio_piped" | "stdio_inherit" | "stdio_null" => Ok(Some(runtime_method(
                match call.op.name() {
                    "stdio_piped" => "pipedStdio",
                    "stdio_inherit" => "inheritStdio",
                    "stdio_null" => "nullStdio",
                    _ => unreachable!(),
                },
                Vec::new(),
            ))),
            "child_kill" => Ok(Some(runtime_method("childKill", vec![receiver()]))),
            "child_wait" => Ok(Some(runtime_method("childWait", vec![receiver()]))),
            "child_try_wait" => Ok(Some(runtime_method("childTryWait", vec![receiver()]))),
            "child_wait_with_output" => Ok(Some(runtime_method(
                "childWaitWithOutput",
                vec![receiver()],
            ))),
            "exit_status_success" => {
                Ok(Some(runtime_method("exitStatusSuccess", vec![receiver()])))
            }
            _ => Ok(None),
        }
    }

    fn materialize_invoke(&self, invoke: &mut ExprInvoke, ty: &TySlot) -> Result<Option<Expr>> {
        // Rust error constructors retain concrete payload types, while Kotlin
        // Result callbacks expose Throwable. Normalize this payload at the
        // target materialization boundary, before syntax serialization.
        if is_io_constructor(&invoke.target) && invoke.args.len() == 1 {
            let error = invoke.args.pop().expect("one Io constructor argument");
            invoke.args.push(runtime_method("ioError", vec![error]));
        }
        let ExprInvokeTarget::Method(select) = &invoke.target else {
            return Ok(None);
        };
        let receiver = (*select.obj).clone();
        let replacement = match select.field.as_str() {
            "trim" => invoke_method(receiver, "trim", invoke.args.clone()),
            "trim_end" => invoke_method(receiver, "trimEnd", invoke.args.clone()),
            "trim_start" => invoke_method(receiver, "trimStart", invoke.args.clone()),
            "starts_with" => invoke_method(receiver, "startsWith", invoke.args.clone()),
            "ends_with" => invoke_method(receiver, "endsWith", invoke.args.clone()),
            "lines" => invoke_method(receiver, "lines", invoke.args.clone()),
            "is_none" => Expr::new(ExprKind::BinOp(ExprBinOp {
                span: Default::default(),
                kind: BinOpKind::Eq,
                lhs: Box::new(receiver),
                rhs: Box::new(Expr::value(Value::Null(Default::default()))),
            })),
            "position" => invoke_method(receiver, "indexOfFirst", invoke.args.clone()),
            "clone" | "copy" | "into_owned" | "to_owned" => kotlin_owned_value(receiver, ty),
            "resolve" => invoke_method(receiver, "resolve", invoke.args.clone()),
            "exists" => {
                invoke_static_method(&["java", "nio", "file", "Files"], "exists", vec![receiver])
            }
            "arg" => runtime_method("commandArg", vec![receiver, invoke.args[0].clone()]),
            "args" => runtime_method("commandArgs", vec![receiver, invoke.args[0].clone()]),
            "current_dir" => {
                runtime_method("commandCurrentDir", vec![receiver, invoke.args[0].clone()])
            }
            "stdin" => runtime_method("commandStdin", vec![receiver, invoke.args[0].clone()]),
            "stdout" => runtime_method("commandStdout", vec![receiver, invoke.args[0].clone()]),
            "stderr" => runtime_method("commandStderr", vec![receiver, invoke.args[0].clone()]),
            "spawn" => runtime_method("commandSpawn", vec![receiver]),
            "output" => runtime_method("commandOutput", vec![receiver]),
            "status" => runtime_method("commandStatus", vec![receiver]),
            "kill" => runtime_method("childKill", vec![receiver]),
            "wait" => runtime_method("childWait", vec![receiver]),
            "try_wait" => runtime_method("childTryWait", vec![receiver]),
            "wait_with_output" => runtime_method("childWaitWithOutput", vec![receiver]),
            "success" => runtime_method("exitStatusSuccess", vec![receiver]),
            "isSuccess" => runtime_method("resultIsSuccess", vec![receiver]),
            "isFailure" => runtime_method("resultIsFailure", vec![receiver]),
            "map_err" => runtime_method(
                "mapError",
                vec![receiver, result_error_mapping(invoke.args.first().cloned())],
            ),
            "is_ok" => runtime_method("resultIsSuccess", vec![receiver]),
            "is_err" => runtime_method("resultIsFailure", vec![receiver]),
            "ok" => runtime_method("resultOkValue", vec![receiver]),
            "err" => runtime_method("resultErrValue", vec![receiver]),
            _ => return Ok(None),
        };
        Ok(Some(replacement))
    }

    fn materialize_select(&self, select: &mut ExprSelect, _ty: &TySlot) -> Result<Option<Expr>> {
        let receiver = (*select.obj).clone();
        let replacement = match select.field.as_str() {
            "isSuccess" => runtime_method("resultIsSuccess", vec![receiver]),
            "isFailure" => runtime_method("resultIsFailure", vec![receiver]),
            _ => return Ok(None),
        };
        Ok(Some(replacement))
    }

    fn materialize_await(&self, await_expr: &mut ExprAwait, _ty: &TySlot) -> Result<Option<Expr>> {
        // Kotlin suspension is expressed by calling a `suspend` function
        // directly. The operand has already been materialized into that call.
        Ok(Some((*await_expr.base).clone()))
    }

    fn materialize_call(&self, call: &mut ExprIntrinsicCall, _ty: &TySlot) -> Result<Option<Expr>> {
        let args = call.args.clone();
        let replacement = match call.kind {
            CallKind::FsReadToString => run_catching(invoke_static_method(
                &["java", "nio", "file", "Files"],
                "readString",
                args,
            )),
            CallKind::FsWriteString => run_catching(unit_block(invoke_static_method(
                &["java", "nio", "file", "Files"],
                "writeString",
                args,
            ))),
            CallKind::FsAppendString => {
                let mut args = args;
                args.push(static_property(
                    &["java", "nio", "file", "StandardOpenOption"],
                    "APPEND",
                ));
                run_catching(unit_block(invoke_static_method(
                    &["java", "nio", "file", "Files"],
                    "writeString",
                    args,
                )))
            }
            CallKind::FsExists => {
                invoke_static_method(&["java", "nio", "file", "Files"], "exists", args)
            }
            CallKind::FsIsDir => {
                invoke_static_method(&["java", "nio", "file", "Files"], "isDirectory", args)
            }
            CallKind::FsIsFile => {
                invoke_static_method(&["java", "nio", "file", "Files"], "isRegularFile", args)
            }
            CallKind::FsCreateDirAll => run_catching(unit_block(invoke_static_method(
                &["java", "nio", "file", "Files"],
                "createDirectories",
                args,
            ))),
            CallKind::FsRemoveFile => run_catching(unit_block(invoke_static_method(
                &["java", "nio", "file", "Files"],
                "delete",
                args,
            ))),
            CallKind::FsRemoveDirAll => run_catching(runtime_method("deleteRecursively", args)),
            CallKind::SerdeJsonFromStr => runtime_method("jsonFromString", args),
            CallKind::SerdeJsonToString => runtime_method("jsonToString", args),
            CallKind::TomlFromStr => runtime_method("tomlFromString", args),
            CallKind::TokioTcpConnect => runtime_method("tcpConnect", args),
            CallKind::TokioTcpWriteAll => runtime_method("tcpWriteAll", args),
            CallKind::Sleep => runtime_method("sleep", args),
            _ => return Ok(None),
        };
        Ok(Some(replacement))
    }

    fn materialize_container(
        &self,
        container: &mut ExprIntrinsicContainer,
        ty: &TySlot,
    ) -> Result<Option<Expr>> {
        if !is_byte_vector(ty) {
            return Ok(None);
        }
        let expression = match std::mem::replace(
            container,
            ExprIntrinsicContainer::VecElements {
                elements: Vec::new(),
            },
        ) {
            ExprIntrinsicContainer::VecElements { elements } => {
                invoke_function("byteArrayOf", elements)
            }
            ExprIntrinsicContainer::VecRepeat { elem, len } => {
                runtime_method("repeatByte", vec![*elem, *len])
            }
            ExprIntrinsicContainer::HashMapEntries { .. } => return Ok(None),
        };
        Ok(Some(expression))
    }
}

fn is_io_constructor(target: &ExprInvokeTarget) -> bool {
    let name = match target {
        ExprInvokeTarget::Function(Name::Ident(name)) => name.as_str(),
        ExprInvokeTarget::Function(Name::Path(path)) => path.last().as_str(),
        ExprInvokeTarget::Function(Name::ParameterPath(path)) => path
            .last()
            .map(|segment| segment.ident.as_str())
            .unwrap_or(""),
        _ => return false,
    };
    name == "Io"
}

fn assign_byte_vector(target: Expr, value: Expr) -> Expr {
    Expr::new(ExprKind::Assign(ExprAssign {
        span: Default::default(),
        target: Box::new(target),
        value: Box::new(value),
    }))
}

fn kotlin_owned_value(value: Expr, ty: &TySlot) -> Expr {
    if is_byte_vector(ty) {
        return runtime_method("bytesFromIterable", vec![value]);
    }
    if ty.as_ref().is_some_and(is_collection_ty) {
        return runtime_method("mutableListFromIterable", vec![value]);
    }
    value
}

fn is_byte_vector(ty: &TySlot) -> bool {
    ty.as_ref().is_some_and(is_byte_vector_ty)
}

fn is_byte_vector_ty(ty: &Ty) -> bool {
    match ty {
        Ty::Vec(vector) => is_u8_type(&vector.ty),
        Ty::Expr(expr) => match expr.kind() {
            ExprKind::Name(Name::ParameterPath(path)) => path.last().is_some_and(|segment| {
                segment.ident.as_str() == "Vec"
                    && segment.args.len() == 1
                    && is_u8_type(&segment.args[0])
            }),
            _ => false,
        },
        _ => false,
    }
}

fn is_collection_ty(ty: &Ty) -> bool {
    match ty {
        Ty::Vec(_) | Ty::Slice(_) => true,
        Ty::Expr(expr) => match expr.kind() {
            ExprKind::Name(Name::ParameterPath(path)) => path.last().is_some_and(|segment| {
                matches!(
                    segment.ident.as_str(),
                    "Vec"
                        | "MutableList"
                        | "List"
                        | "to_vec"
                        | "to_vec_in"
                        | "slice_to_vec"
                        | "slice_to_vec_in"
                )
            }),
            _ => false,
        },
        _ => false,
    }
}

fn is_u8_type(ty: &Ty) -> bool {
    matches!(ty, Ty::Primitive(TypePrimitive::Int(TypeInt::U8)))
        || matches!(ty, Ty::Expr(expr) if matches!(expr.kind(), ExprKind::Name(Name::Ident(name)) if name.as_str() == "u8"))
}

fn kotlin_default_value(ty: &TySlot) -> Option<Expr> {
    let ty = ty.as_ref()?;
    if is_byte_vector_ty(ty) {
        return Some(invoke_function(
            "ByteArray",
            vec![Expr::value(Value::int(0))],
        ));
    }
    if is_collection_ty(ty) {
        return Some(invoke_function("mutableListOf", Vec::new()));
    }
    match ty {
        Ty::Primitive(TypePrimitive::Bool) => Some(Expr::value(Value::bool(false))),
        Ty::Primitive(TypePrimitive::Char) => {
            Some(Expr::value(Value::Char(fp_core::ast::ValueChar::new('\0'))))
        }
        Ty::Primitive(TypePrimitive::String) => Some(Expr::value(Value::string(String::new()))),
        Ty::Primitive(TypePrimitive::Int(_)) => Some(Expr::value(Value::int(0))),
        Ty::Primitive(TypePrimitive::Decimal(_)) => Some(Expr::value(Value::decimal(0.0))),
        Ty::Expr(expr) => match expr.kind() {
            ExprKind::Name(Name::Ident(name)) => Some(invoke_static_method(
                &[name.as_str()],
                "default",
                Vec::new(),
            )),
            ExprKind::Name(Name::Path(path)) => {
                let names = path
                    .segments
                    .iter()
                    .map(|segment| segment.as_str())
                    .collect::<Vec<_>>();
                Some(invoke_static_method(&names, "default", Vec::new()))
            }
            _ => None,
        },
        _ => None,
    }
}

fn invoke_function(name: &str, args: Vec<Expr>) -> Expr {
    Expr::new(ExprKind::Invoke(ExprInvoke {
        span: Default::default(),
        target: ExprInvokeTarget::Function(Name::ident(name)),
        args,
        kwargs: Vec::new(),
    }))
}

fn runtime_method(method: &str, args: Vec<Expr>) -> Expr {
    invoke_static_method(&["RustKotlinRuntime"], method, args)
}

fn invoke_static_method(receiver: &[&str], method: &str, args: Vec<Expr>) -> Expr {
    Expr::new(ExprKind::Invoke(ExprInvoke {
        span: Default::default(),
        target: ExprInvokeTarget::Method(ExprSelect {
            span: Default::default(),
            obj: Box::new(Expr::name(Name::path(Path::plain(
                receiver
                    .iter()
                    .map(|segment| Ident::new(*segment))
                    .collect(),
            )))),
            field: Ident::new(method),
            generic_args: Vec::new(),
            select: ExprSelectType::Method,
        }),
        args,
        kwargs: Vec::new(),
    }))
}

fn run_catching(body: Expr) -> Expr {
    Expr::new(ExprKind::Invoke(ExprInvoke {
        span: Default::default(),
        target: ExprInvokeTarget::Function(Name::ident("runCatching")),
        args: vec![Expr::new(ExprKind::Closure(ExprClosure {
            span: Default::default(),
            params: Vec::new(),
            ret_ty: None,
            movability: None,
            body: Box::new(body),
        }))],
        kwargs: Vec::new(),
    }))
}

fn unit_block(statement: Expr) -> Expr {
    Expr::new(ExprKind::Block(ExprBlock::new_stmts_expr(
        vec![BlockStmt::Expr(
            fp_core::ast::BlockStmtExpr::new(statement).with_semicolon(true),
        )],
        Expr::name(Name::ident("Unit")),
    )))
}

fn static_property(receiver: &[&str], property: &str) -> Expr {
    select_property(
        Expr::name(Name::path(Path::plain(
            receiver
                .iter()
                .map(|segment| Ident::new(*segment))
                .collect(),
        ))),
        property,
    )
}

/// Kotlin's `Result` fixes its error carrier to `Throwable`, while Rust
/// permits any `E`. Normalize it in the runtime so constructors and mapped
/// errors follow one target-specific rule.
fn normalize_error(error: Expr) -> Expr {
    runtime_method("normalizeError", vec![error])
}

fn throwable_pattern(ident: Ident) -> Pattern {
    Pattern::from(PatternKind::Type(PatternType::new(
        Pattern::from(PatternKind::Ident(PatternIdent {
            ident,
            mutability: None,
        })),
        Ty::path(Path::plain(vec![Ident::new("Throwable")])),
    )))
}

fn error_mapping(call: &PortableOpCall) -> Expr {
    result_error_mapping(call.args.get(1).cloned())
}

fn result_error_mapping(mapper: Option<Expr>) -> Expr {
    let mapper = mapper.unwrap_or_else(|| Expr::value(Value::Null(Default::default())));
    match mapper.kind().clone() {
        ExprKind::Closure(mut closure) => {
            normalize_error_closure_params(&mut closure);
            closure.body = Box::new(normalize_error(*closure.body));
            Expr::new(ExprKind::Closure(closure))
        }
        _ => {
            let error_ident = Ident::new("__fp_error");
            let mapped = Expr::new(ExprKind::Invoke(ExprInvoke {
                span: Default::default(),
                target: ExprInvokeTarget::expr(mapper),
                args: vec![Expr::name(Name::ident(error_ident.name.clone()))],
                kwargs: Vec::new(),
            }));
            Expr::new(ExprKind::Closure(ExprClosure {
                span: Default::default(),
                params: vec![throwable_pattern(error_ident)],
                ret_ty: Some(Box::new(Ty::path(Path::plain(vec![Ident::new(
                    "Throwable",
                )])))),
                movability: None,
                body: Box::new(normalize_error(mapped)),
            }))
        }
    }
}

fn normalize_error_closure_params(closure: &mut ExprClosure) {
    let Some(param) = closure.params.first_mut() else {
        return;
    };
    let throwable = Ty::path(Path::plain(vec![Ident::new("Throwable")]));
    let original = param.clone();
    *param = match original.kind().clone() {
        PatternKind::Type(mut typed) => {
            typed.ty = throwable;
            Pattern::from(PatternKind::Type(typed))
        }
        _ => Pattern::from(PatternKind::Type(PatternType::new(original, throwable))),
    };
}

fn result_constructor_arg(call: &PortableOpCall) -> Expr {
    call.args
        .first()
        .cloned()
        .unwrap_or_else(|| Expr::value(Value::Null(Default::default())))
}

fn result_success_arg(call: &PortableOpCall) -> Expr {
    let arg = result_constructor_arg(call);
    if matches!(arg.kind(), ExprKind::Value(value) if matches!(&**value, Value::Unit(_))) {
        Expr::name(Name::ident("Unit"))
    } else {
        arg
    }
}

fn portable_op_args_after_receiver(call: &PortableOpCall) -> Vec<Expr> {
    call.args.iter().skip(1).cloned().collect()
}

#[cfg(test)]
mod tests {
    use fp_core::ast::{
        BlockStmt, ExprBlock, ExprIntrinsicCall, ExprInvoke, ExprInvokeTarget, ExprKind,
        ExprSelect, ExprSelectType, File, Ident, Item, ItemDefFunction, ItemKind, Name,
        PatternKind, Ty, Value,
    };
    use fp_core::intrinsics::{PortableOpCall, PortableOpRegistry, materialize_file};

    use super::*;

    #[test]
    fn materializes_result_constructors_without_erasing_them() {
        let registry = PortableOpRegistry::builtin();
        let mut ok = PortableOpCall {
            span: Default::default(),
            op: registry.resolve("result_ok").expect("registered result_ok"),
            args: vec![Expr::value(Value::string("value".to_string()))],
            kwargs: Vec::new(),
        };
        let mut err = PortableOpCall {
            span: Default::default(),
            op: registry
                .resolve("result_err")
                .expect("registered result_err"),
            args: vec![Expr::value(Value::string("failure".to_string()))],
            kwargs: Vec::new(),
        };

        let materializer = KotlinMaterializer;
        let ok = materializer
            .materialize_portable_op(&mut ok, &None)
            .expect("materialize Ok")
            .expect("Ok replacement");
        let err = materializer
            .materialize_portable_op(&mut err, &None)
            .expect("materialize Err")
            .expect("Err replacement");

        assert!(matches!(ok.kind(), ExprKind::Invoke(_)));
        assert!(matches!(err.kind(), ExprKind::Invoke(_)));
        assert_eq!(render_invoke_name(&ok), "RustKotlinRuntime.resultSuccess");
        assert_eq!(render_invoke_name(&err), "RustKotlinRuntime.resultFailure");
        let ExprKind::Invoke(err_call) = err.kind() else {
            panic!("expected Result failure adapter invocation");
        };
        assert_eq!(
            render_invoke_name(&err_call.args[0]),
            "RustKotlinRuntime.normalizeError"
        );
    }

    #[test]
    fn materializes_result_unit_success_as_kotlin_unit() {
        let registry = PortableOpRegistry::builtin();
        let mut ok = PortableOpCall {
            span: Default::default(),
            op: registry.resolve("result_ok").expect("registered result ok"),
            args: vec![Expr::unit()],
            kwargs: Vec::new(),
        };

        let materialized = KotlinMaterializer
            .materialize_portable_op(&mut ok, &None)
            .expect("materialize Ok(())")
            .expect("Ok(()) replacement");
        let ExprKind::Invoke(invoke) = materialized.kind() else {
            panic!("expected Result success adapter invocation");
        };
        assert!(
            matches!(invoke.args[0].kind(), ExprKind::Name(Name::Ident(ident)) if ident.as_str() == "Unit")
        );
    }

    #[test]
    fn materializes_checked_option_and_result_unwraps() {
        let registry = PortableOpRegistry::builtin();
        let materializer = KotlinMaterializer;
        let mut option = PortableOpCall {
            span: Default::default(),
            op: registry
                .resolve("option_unwrap")
                .expect("registered option_unwrap"),
            args: vec![Expr::name(Name::ident("value"))],
            kwargs: Vec::new(),
        };
        let mut result = PortableOpCall {
            span: Default::default(),
            op: registry
                .resolve("result_unwrap")
                .expect("registered result_unwrap"),
            args: vec![Expr::name(Name::ident("result"))],
            kwargs: Vec::new(),
        };

        let option = materializer
            .materialize_portable_op(&mut option, &None)
            .expect("materialize Option::unwrap")
            .expect("Option::unwrap replacement");
        let result = materializer
            .materialize_portable_op(&mut result, &None)
            .expect("materialize Result::unwrap")
            .expect("Result::unwrap replacement");

        assert_eq!(
            render_invoke_name(&option),
            "RustKotlinRuntime.optionUnwrap"
        );
        assert_eq!(
            render_invoke_name(&result),
            "RustKotlinRuntime.resultUnwrap"
        );
    }

    #[test]
    fn materializes_io_error_without_leaking_error_kind() {
        let registry = PortableOpRegistry::builtin();
        let mut call = PortableOpCall {
            span: Default::default(),
            op: registry
                .resolve("io_error_new")
                .expect("registered io error"),
            args: vec![
                Expr::name(Name::ident("ErrorKind::InvalidData")),
                Expr::name(Name::ident("source")),
            ],
            kwargs: Vec::new(),
        };

        let materialized = KotlinMaterializer
            .materialize_portable_op(&mut call, &None)
            .expect("materialize io error")
            .expect("io error replacement");
        assert_eq!(
            render_invoke_name(&materialized),
            "RustKotlinRuntime.ioError"
        );
        let ExprKind::Invoke(invoke) = materialized.kind() else {
            panic!("expected ioError invocation");
        };
        assert_eq!(invoke.args.len(), 1);
        assert!(matches!(invoke.args[0].kind(), ExprKind::Name(_)));
    }

    #[test]
    fn materializes_io_constructor_payload_as_jvm_io_exception() {
        let mut invoke = ExprInvoke {
            span: Default::default(),
            target: ExprInvokeTarget::Function(Name::path(Path::plain(vec![
                Ident::new("ConfigError"),
                Ident::new("Io"),
            ]))),
            args: vec![Expr::name(Name::ident("source"))],
            kwargs: Vec::new(),
        };

        KotlinMaterializer
            .materialize_invoke(&mut invoke, &None)
            .expect("materialize Io constructor");

        let ExprKind::Invoke(_) = invoke.args[0].kind() else {
            panic!("expected IOException adapter");
        };
        assert_eq!(
            render_invoke_name(&invoke.args[0]),
            "RustKotlinRuntime.ioError"
        );
    }

    #[test]
    fn materializes_str_parse_as_typed_kotlin_result() {
        let registry = PortableOpRegistry::builtin();
        let mut call = PortableOpCall {
            span: Default::default(),
            op: registry.resolve("str_parse").expect("registered str_parse"),
            args: vec![Expr::name(Name::ident("input"))],
            kwargs: Vec::new(),
        };

        let parsed = KotlinMaterializer
            .materialize_portable_op(&mut call, &None)
            .expect("materialize str::parse")
            .expect("str::parse replacement");
        assert_eq!(render_invoke_name(&parsed), "RustKotlinRuntime.parse");
    }

    #[test]
    fn materializes_result_propagation_as_single_unwrap() {
        let registry = PortableOpRegistry::builtin();
        let mut call = PortableOpCall {
            span: Default::default(),
            op: registry
                .resolve("result_propagate")
                .expect("registered result_propagate"),
            args: vec![Expr::name(Name::ident("source"))],
            kwargs: Vec::new(),
        };

        let materialized = KotlinMaterializer
            .materialize_portable_op(&mut call, &None)
            .expect("materialize Result propagation")
            .expect("Result propagation replacement");
        assert_eq!(
            render_invoke_name(&materialized),
            "RustKotlinRuntime.resultUnwrap"
        );
    }

    #[test]
    fn materializes_resolved_filesystem_calls_to_run_catching() {
        let mut call = ExprIntrinsicCall {
            span: Default::default(),
            kind: CallKind::FsReadToString,
            args: vec![Expr::name(Name::ident("path"))],
            kwargs: Vec::new(),
        };

        let materialized = KotlinMaterializer
            .materialize_call(&mut call, &None)
            .expect("materialize filesystem call")
            .expect("filesystem replacement");
        assert_eq!(render_invoke_name(&materialized), "runCatching");
    }

    #[test]
    fn materializes_result_error_mapping_through_the_runtime() {
        let registry = PortableOpRegistry::builtin();
        let mut call = PortableOpCall {
            span: Default::default(),
            op: registry
                .resolve("result_map_err")
                .expect("registered result_map_err"),
            args: vec![
                Expr::name(Name::ident("result")),
                Expr::name(Name::ident("convert_error")),
            ],
            kwargs: Vec::new(),
        };

        let materialized = KotlinMaterializer
            .materialize_portable_op(&mut call, &None)
            .expect("materialize result map_err")
            .expect("result map_err replacement");
        assert_eq!(
            render_invoke_name(&materialized),
            "RustKotlinRuntime.mapError"
        );
        let ExprKind::Invoke(runtime_call) = materialized.kind() else {
            panic!("expected runtime invocation");
        };
        let ExprKind::Closure(mapping) = runtime_call.args[1].kind() else {
            panic!("expected Throwable-normalizing mapping closure");
        };
        assert_eq!(mapping.params.len(), 1);
        let PatternKind::Type(param) = mapping.params[0].kind() else {
            panic!("map_err callback parameter must be typed");
        };
        let Ty::Expr(throwable) = &param.ty else {
            panic!("map_err callback parameter must use a Kotlin Throwable type");
        };
        assert!(
            matches!(throwable.kind(), ExprKind::Name(Name::Ident(name)) if name.as_str() == "Throwable")
        );
        assert_eq!(
            render_invoke_name(&mapping.body),
            "RustKotlinRuntime.normalizeError"
        );
    }

    #[test]
    fn materializes_result_operations_through_the_runtime() {
        let registry = PortableOpRegistry::builtin();
        let materializer = KotlinMaterializer;

        for (op, expected) in [
            ("result_map", "RustKotlinRuntime.mapResult"),
            ("result_map_err", "RustKotlinRuntime.mapError"),
            ("result_is_ok", "RustKotlinRuntime.resultIsSuccess"),
            ("result_is_err", "RustKotlinRuntime.resultIsFailure"),
            ("result_ok_value", "RustKotlinRuntime.resultOkValue"),
            ("result_err_value", "RustKotlinRuntime.resultErrValue"),
            ("result_unwrap", "RustKotlinRuntime.resultUnwrap"),
            ("result_unwrap_or", "RustKotlinRuntime.resultDefault"),
        ] {
            let mut call = PortableOpCall {
                span: Default::default(),
                op: registry.resolve(op).expect("registered Result operation"),
                args: vec![
                    Expr::name(Name::ident("result")),
                    Expr::name(Name::ident("value")),
                ],
                kwargs: Vec::new(),
            };
            let materialized = materializer
                .materialize_portable_op(&mut call, &None)
                .expect("materialize Result operation")
                .expect("Result operation replacement");
            assert_eq!(render_invoke_name(&materialized), expected);
        }
    }

    #[test]
    fn materializes_unresolved_result_methods_through_the_runtime() {
        let materializer = KotlinMaterializer;
        for (method, expected) in [
            ("map_err", "RustKotlinRuntime.mapError"),
            ("is_ok", "RustKotlinRuntime.resultIsSuccess"),
            ("is_err", "RustKotlinRuntime.resultIsFailure"),
            ("ok", "RustKotlinRuntime.resultOkValue"),
            ("err", "RustKotlinRuntime.resultErrValue"),
        ] {
            let mut invoke = ExprInvoke {
                span: Default::default(),
                target: ExprInvokeTarget::Method(ExprSelect {
                    span: Default::default(),
                    obj: Box::new(Expr::name(Name::ident("result"))),
                    field: Ident::new(method),
                    generic_args: Vec::new(),
                    select: ExprSelectType::Method,
                }),
                args: vec![Expr::name(Name::ident("mapper"))],
                kwargs: Vec::new(),
            };
            let materialized = materializer
                .materialize_invoke(&mut invoke, &None)
                .expect("materialize unresolved Result method")
                .expect("Result method replacement");
            assert_eq!(
                render_invoke_name(&materialized),
                expected,
                "method: {method}"
            );
        }
    }

    #[test]
    fn materializes_kotlin_result_status_properties_through_the_runtime() {
        let materializer = KotlinMaterializer;
        for (field, expected) in [
            ("isSuccess", "RustKotlinRuntime.resultIsSuccess"),
            ("isFailure", "RustKotlinRuntime.resultIsFailure"),
        ] {
            let mut select = ExprSelect {
                span: Default::default(),
                obj: Box::new(Expr::name(Name::ident("result"))),
                field: Ident::new(field),
                generic_args: Vec::new(),
                select: ExprSelectType::Field,
            };
            let materialized = materializer
                .materialize_select(&mut select, &None)
                .expect("materialize Result property")
                .expect("Result property replacement");
            assert_eq!(
                render_invoke_name(&materialized),
                expected,
                "property: {field}"
            );
        }
    }

    #[test]
    fn serializes_unresolved_result_operations_through_runtime_adapters() {
        let map_err = Expr::new(ExprKind::Invoke(ExprInvoke {
            span: Default::default(),
            target: ExprInvokeTarget::Method(ExprSelect {
                span: Default::default(),
                obj: Box::new(Expr::name(Name::ident("result"))),
                field: Ident::new("map_err"),
                generic_args: Vec::new(),
                select: ExprSelectType::Method,
            }),
            args: vec![Expr::name(Name::ident("convert_error"))],
            kwargs: Vec::new(),
        }));
        let is_success = Expr::new(ExprKind::Select(ExprSelect {
            span: Default::default(),
            obj: Box::new(Expr::name(Name::ident("result"))),
            field: Ident::new("isSuccess"),
            generic_args: Vec::new(),
            select: ExprSelectType::Field,
        }));
        let ok = Expr::new(ExprKind::Invoke(ExprInvoke {
            span: Default::default(),
            target: ExprInvokeTarget::Method(ExprSelect {
                span: Default::default(),
                obj: Box::new(Expr::name(Name::ident("result"))),
                field: Ident::new("ok"),
                generic_args: Vec::new(),
                select: ExprSelectType::Method,
            }),
            args: Vec::new(),
            kwargs: Vec::new(),
        }));
        let file = materialize_file(
            File {
                path: Default::default(),
                attrs: Vec::new(),
                collected_items: Vec::new(),
                items: vec![Item::new(ItemKind::DefFunction(
                    ItemDefFunction::new_simple(
                        Ident::new("adapt"),
                        ExprBlock::new_stmts(vec![
                            BlockStmt::Expr(fp_core::ast::BlockStmtExpr::new(map_err)),
                            BlockStmt::Expr(fp_core::ast::BlockStmtExpr::new(is_success)),
                            BlockStmt::Expr(fp_core::ast::BlockStmtExpr::new(ok)),
                        ]),
                    ),
                ))],
            },
            &KotlinMaterializer,
        )
        .expect("materialize unresolved Result operations");
        let rendered = crate::serializer::KotlinSerializer
            .serialize_file(&file)
            .expect("serialize unresolved Result operations");
        assert!(
            rendered.contains("RustKotlinRuntime.mapError("),
            "{rendered}"
        );
        assert!(
            rendered.contains("RustKotlinRuntime.resultIsSuccess(result)"),
            "{rendered}"
        );
        assert!(
            rendered.contains("RustKotlinRuntime.resultOkValue(result)"),
            "{rendered}"
        );
        assert!(!rendered.contains(".map_err("), "{rendered}");
        assert!(!rendered.contains(".ok()"), "{rendered}");
        assert!(!rendered.contains(".isSuccess"), "{rendered}");
    }

    #[test]
    fn materializes_vec_push_through_the_runtime() {
        let registry = PortableOpRegistry::builtin();
        let mut call = PortableOpCall {
            span: Default::default(),
            op: registry.resolve("vec_push").expect("registered vec_push"),
            args: vec![Expr::name(Name::ident("items")), Expr::value(Value::int(1))],
            kwargs: Vec::new(),
        };

        let materialized = KotlinMaterializer
            .materialize_portable_op(&mut call, &None)
            .expect("materialize vec push")
            .expect("vec push replacement");
        assert_eq!(
            render_invoke_name(&materialized),
            "RustKotlinRuntime.listPush"
        );
    }

    #[test]
    fn materializes_owned_values_without_rust_copy_methods() {
        let registry = PortableOpRegistry::builtin();
        let bytes_ty = Some(Ty::Vec(fp_core::ast::TypeVec {
            ty: Box::new(Ty::Primitive(fp_core::ast::TypePrimitive::Int(
                fp_core::ast::TypeInt::U8,
            ))),
        }));
        let mut clone = PortableOpCall {
            span: Default::default(),
            op: registry.resolve("clone").expect("registered clone"),
            args: vec![Expr::name(Name::ident("bytes"))],
            kwargs: Vec::new(),
        };
        let cloned = KotlinMaterializer
            .materialize_portable_op(&mut clone, &bytes_ty)
            .expect("materialize byte clone")
            .expect("byte clone replacement");
        assert_eq!(
            render_invoke_name(&cloned),
            "RustKotlinRuntime.bytesFromIterable"
        );

        for method in ["copy", "into_owned", "to_owned"] {
            let mut invoke = ExprInvoke {
                span: Default::default(),
                target: ExprInvokeTarget::Method(ExprSelect {
                    span: Default::default(),
                    obj: Box::new(Expr::name(Name::ident("bytes"))),
                    field: Ident::new(method),
                    generic_args: Vec::new(),
                    select: ExprSelectType::Method,
                }),
                args: Vec::new(),
                kwargs: Vec::new(),
            };
            let materialized = KotlinMaterializer
                .materialize_invoke(&mut invoke, &bytes_ty)
                .expect("materialize ownership method")
                .expect("ownership replacement");
            assert_eq!(
                render_invoke_name(&materialized),
                "RustKotlinRuntime.bytesFromIterable"
            );
        }
    }

    #[test]
    fn materializes_collection_defaults_without_rust_default_calls() {
        let registry = PortableOpRegistry::builtin();
        let bytes_ty = Some(Ty::name(Name::parameter_path(
            fp_core::ast::ParameterPath::new(
                fp_core::ast::path::PathPrefix::Plain,
                vec![fp_core::ast::ParameterPathSegment::new(
                    Ident::new("Vec"),
                    vec![Ty::ident(Ident::new("u8"))],
                )],
            ),
        )));
        let lists_ty = Some(Ty::Vec(fp_core::ast::TypeVec {
            ty: Box::new(Ty::ident(Ident::new("Entry"))),
        }));

        for (ty, expected) in [(bytes_ty, "ByteArray"), (lists_ty, "mutableListOf")] {
            let mut call = PortableOpCall {
                span: Default::default(),
                op: registry.resolve("default").expect("registered default"),
                args: Vec::new(),
                kwargs: Vec::new(),
            };
            let materialized = KotlinMaterializer
                .materialize_portable_op(&mut call, &ty)
                .expect("materialize collection default")
                .expect("collection default replacement");
            assert_eq!(render_invoke_name(&materialized), expected);
        }
    }

    #[test]
    fn materializes_vec_constructors_without_rust_factory_names() {
        let registry = PortableOpRegistry::builtin();
        let materializer = KotlinMaterializer;

        let mut new_call = PortableOpCall {
            span: Default::default(),
            op: registry.resolve("vec_new").expect("registered vec_new"),
            args: Vec::new(),
            kwargs: Vec::new(),
        };
        let new_expr = materializer
            .materialize_portable_op(&mut new_call, &None)
            .expect("materialize Vec::new")
            .expect("Vec::new replacement");
        assert_eq!(render_invoke_name(&new_expr), "mutableListOf");

        let mut from_call = PortableOpCall {
            span: Default::default(),
            op: registry.resolve("vec_from").expect("registered vec_from"),
            args: vec![Expr::name(Name::ident("source"))],
            kwargs: Vec::new(),
        };
        let from_expr = materializer
            .materialize_portable_op(&mut from_call, &None)
            .expect("materialize Vec::from")
            .expect("Vec::from replacement");
        assert_eq!(render_invoke_name(&from_expr), "source.toMutableList");

        let mut from_iter_call = PortableOpCall {
            span: Default::default(),
            op: registry
                .resolve("vec_from_iter")
                .expect("registered vec_from_iter"),
            args: vec![Expr::name(Name::ident("source"))],
            kwargs: Vec::new(),
        };
        let from_iter_expr = materializer
            .materialize_portable_op(&mut from_iter_call, &None)
            .expect("materialize Vec::from_iter")
            .expect("Vec::from_iter replacement");
        assert_eq!(
            render_invoke_name(&from_iter_expr),
            "RustKotlinRuntime.mutableListFromIterable"
        );
    }

    #[test]
    fn materializes_slice_vector_cloning_without_allocator_operations() {
        let registry = PortableOpRegistry::builtin();
        let materializer = KotlinMaterializer;

        for op in ["slice_to_vec", "slice_to_vec_in"] {
            let mut call = PortableOpCall {
                span: Default::default(),
                op: registry
                    .resolve(op)
                    .expect("registered slice clone operation"),
                args: vec![
                    Expr::name(Name::ident("source")),
                    Expr::name(Name::ident("allocator")),
                ],
                kwargs: Vec::new(),
            };
            let materialized = materializer
                .materialize_portable_op(&mut call, &None)
                .expect("materialize slice clone")
                .expect("slice clone replacement");
            assert_eq!(
                render_invoke_name(&materialized),
                "RustKotlinRuntime.mutableListFromIterable"
            );
        }

        let bytes_ty = Some(Ty::Vec(fp_core::ast::TypeVec {
            ty: Box::new(Ty::Primitive(fp_core::ast::TypePrimitive::Int(
                fp_core::ast::TypeInt::U8,
            ))),
        }));
        let mut byte_call = PortableOpCall {
            span: Default::default(),
            op: registry
                .resolve("slice_to_vec_in")
                .expect("registered allocator-aware slice clone operation"),
            args: vec![
                Expr::name(Name::ident("bytes")),
                Expr::name(Name::ident("allocator")),
            ],
            kwargs: Vec::new(),
        };
        let materialized = materializer
            .materialize_portable_op(&mut byte_call, &bytes_ty)
            .expect("materialize byte slice clone")
            .expect("ByteArray clone replacement");
        assert_eq!(
            render_invoke_name(&materialized),
            "RustKotlinRuntime.bytesFromIterable"
        );
    }

    #[test]
    fn materializes_byte_vectors_as_byte_arrays() {
        let registry = PortableOpRegistry::builtin();
        let bytes_ty = Some(Ty::Vec(fp_core::ast::TypeVec {
            ty: Box::new(Ty::Primitive(fp_core::ast::TypePrimitive::Int(
                fp_core::ast::TypeInt::U8,
            ))),
        }));
        let materializer = KotlinMaterializer;

        let mut new_call = PortableOpCall {
            span: Default::default(),
            op: registry.resolve("vec_new").expect("registered vec_new"),
            args: Vec::new(),
            kwargs: Vec::new(),
        };
        let new_expr = materializer
            .materialize_portable_op(&mut new_call, &bytes_ty)
            .expect("materialize Vec::new")
            .expect("ByteArray replacement");
        assert_eq!(render_invoke_name(&new_expr), "ByteArray");

        let mut push_call = PortableOpCall {
            span: Default::default(),
            op: registry.resolve("vec_push").expect("registered vec_push"),
            args: vec![Expr::name(Name::ident("bytes")), Expr::value(Value::int(1))],
            kwargs: Vec::new(),
        };
        let push_expr = materializer
            .materialize_portable_op(&mut push_call, &bytes_ty)
            .expect("materialize byte Vec::push")
            .expect("ByteArray append replacement");
        let ExprKind::Assign(assign) = push_expr.kind() else {
            panic!("expected ByteArray reassignment");
        };
        assert_eq!(
            render_invoke_name(&assign.value),
            "RustKotlinRuntime.appendByte"
        );

        let mut collect_call = PortableOpCall {
            span: Default::default(),
            op: registry.resolve("collect").expect("registered collect"),
            args: vec![Expr::name(Name::ident("source"))],
            kwargs: Vec::new(),
        };
        let collect_expr = materializer
            .materialize_portable_op(&mut collect_call, &bytes_ty)
            .expect("materialize byte collect")
            .expect("ByteArray collection replacement");
        assert_eq!(
            render_invoke_name(&collect_expr),
            "RustKotlinRuntime.bytesFromIterable"
        );
    }

    #[test]
    fn materializes_byte_vector_literals_and_utf8_decoding_through_runtime() {
        let registry = PortableOpRegistry::builtin();
        let bytes_ty = Some(Ty::Vec(fp_core::ast::TypeVec {
            ty: Box::new(Ty::Primitive(fp_core::ast::TypePrimitive::Int(
                fp_core::ast::TypeInt::U8,
            ))),
        }));
        let mut container = ExprIntrinsicContainer::VecElements {
            elements: vec![Expr::value(Value::int(65))],
        };
        let bytes = KotlinMaterializer
            .materialize_container(&mut container, &bytes_ty)
            .expect("materialize byte literal")
            .expect("ByteArray literal replacement");
        assert_eq!(render_invoke_name(&bytes), "byteArrayOf");

        let mut decode_call = PortableOpCall {
            span: Default::default(),
            op: registry
                .resolve("string_from_utf8_lossy")
                .expect("registered UTF-8 decoder"),
            args: vec![Expr::name(Name::ident("bytes"))],
            kwargs: Vec::new(),
        };
        let decoded = KotlinMaterializer
            .materialize_portable_op(&mut decode_call, &None)
            .expect("materialize UTF-8 decode")
            .expect("runtime decoder replacement");
        assert_eq!(render_invoke_name(&decoded), "RustKotlinRuntime.decodeUtf8");
    }

    #[test]
    fn materializes_external_api_calls_through_runtime_identities() {
        let cases = [
            (
                CallKind::SerdeJsonFromStr,
                "RustKotlinRuntime.jsonFromString",
            ),
            (
                CallKind::SerdeJsonToString,
                "RustKotlinRuntime.jsonToString",
            ),
            (CallKind::TomlFromStr, "RustKotlinRuntime.tomlFromString"),
            (CallKind::TokioTcpConnect, "RustKotlinRuntime.tcpConnect"),
            (CallKind::TokioTcpWriteAll, "RustKotlinRuntime.tcpWriteAll"),
            (CallKind::Sleep, "RustKotlinRuntime.sleep"),
        ];
        for (kind, expected) in cases {
            let mut call = ExprIntrinsicCall {
                span: Default::default(),
                kind,
                args: vec![Expr::name(Name::ident("value"))],
                kwargs: Vec::new(),
            };
            let materialized = KotlinMaterializer
                .materialize_call(&mut call, &None)
                .expect("materialize external API call")
                .expect("external API replacement");
            assert_eq!(render_invoke_name(&materialized), expected);
        }
    }

    #[test]
    fn materializes_jvm_nio_intrinsics_with_kotlin_result_shapes() {
        let materializer = KotlinMaterializer;
        for (kind, expected) in [
            (CallKind::FsReadToString, "runCatching"),
            (CallKind::FsWriteString, "runCatching"),
            (CallKind::FsAppendString, "runCatching"),
            (CallKind::FsExists, "java.nio.file.Files.exists"),
            (CallKind::FsIsDir, "java.nio.file.Files.isDirectory"),
            (CallKind::FsIsFile, "java.nio.file.Files.isRegularFile"),
            (CallKind::FsCreateDirAll, "runCatching"),
            (CallKind::FsRemoveFile, "runCatching"),
            (CallKind::FsRemoveDirAll, "runCatching"),
        ] {
            let args = match kind {
                CallKind::FsWriteString | CallKind::FsAppendString => vec![
                    Expr::name(Name::ident("path")),
                    Expr::name(Name::ident("contents")),
                ],
                _ => vec![Expr::name(Name::ident("path"))],
            };
            let mut call = ExprIntrinsicCall {
                span: Default::default(),
                kind,
                args,
                kwargs: Vec::new(),
            };
            let materialized = materializer
                .materialize_call(&mut call, &None)
                .expect("materialize NIO intrinsic")
                .expect("NIO replacement");
            assert_eq!(
                render_invoke_name(&materialized),
                expected,
                "kind: {kind:?}"
            );

            if matches!(
                kind,
                CallKind::FsWriteString
                    | CallKind::FsAppendString
                    | CallKind::FsCreateDirAll
                    | CallKind::FsRemoveFile
            ) {
                let ExprKind::Invoke(invoke) = materialized.kind() else {
                    panic!("expected runCatching invocation");
                };
                let ExprKind::Closure(closure) = invoke.args[0].kind() else {
                    panic!("expected runCatching closure");
                };
                assert!(matches!(closure.body.kind(), ExprKind::Block(_)));
            }
        }
    }

    #[test]
    fn materializes_path_and_process_methods_through_runtime_adapters() {
        let materializer = KotlinMaterializer;
        for (method, args, expected) in [
            ("resolve", 1, "path.resolve"),
            ("exists", 0, "java.nio.file.Files.exists"),
            ("arg", 1, "RustKotlinRuntime.commandArg"),
            ("args", 1, "RustKotlinRuntime.commandArgs"),
            ("current_dir", 1, "RustKotlinRuntime.commandCurrentDir"),
            ("stdin", 1, "RustKotlinRuntime.commandStdin"),
            ("stdout", 1, "RustKotlinRuntime.commandStdout"),
            ("stderr", 1, "RustKotlinRuntime.commandStderr"),
            ("spawn", 0, "RustKotlinRuntime.commandSpawn"),
            ("output", 0, "RustKotlinRuntime.commandOutput"),
            ("status", 0, "RustKotlinRuntime.commandStatus"),
            ("kill", 0, "RustKotlinRuntime.childKill"),
            ("wait", 0, "RustKotlinRuntime.childWait"),
            ("try_wait", 0, "RustKotlinRuntime.childTryWait"),
            (
                "wait_with_output",
                0,
                "RustKotlinRuntime.childWaitWithOutput",
            ),
            ("success", 0, "RustKotlinRuntime.exitStatusSuccess"),
        ] {
            let mut invoke = ExprInvoke {
                span: Default::default(),
                target: ExprInvokeTarget::Method(ExprSelect {
                    span: Default::default(),
                    obj: Box::new(Expr::name(Name::ident("receiver"))),
                    field: Ident::new(method),
                    generic_args: Vec::new(),
                    select: ExprSelectType::Method,
                }),
                args: (0..args)
                    .map(|index| Expr::name(Name::ident(format!("arg_{index}"))))
                    .collect(),
                kwargs: Vec::new(),
            };
            let materialized = materializer
                .materialize_invoke(&mut invoke, &None)
                .expect("materialize Path or process method")
                .expect("runtime adapter replacement");
            assert_eq!(
                render_invoke_name(&materialized),
                expected,
                "method: {method}"
            );
        }
    }

    #[test]
    fn materializes_process_operations_through_the_runtime_model() {
        let registry = PortableOpRegistry::builtin();
        let materializer = KotlinMaterializer;
        for (op, args, expected) in [
            ("command_new", 1, "RustKotlinRuntime.command"),
            ("command_arg", 2, "RustKotlinRuntime.commandArg"),
            ("command_args", 2, "RustKotlinRuntime.commandArgs"),
            (
                "command_current_dir",
                2,
                "RustKotlinRuntime.commandCurrentDir",
            ),
            ("command_stdin", 2, "RustKotlinRuntime.commandStdin"),
            ("command_stdout", 2, "RustKotlinRuntime.commandStdout"),
            ("command_stderr", 2, "RustKotlinRuntime.commandStderr"),
            ("command_spawn", 1, "RustKotlinRuntime.commandSpawn"),
            ("command_output", 1, "RustKotlinRuntime.commandOutput"),
            ("command_status", 1, "RustKotlinRuntime.commandStatus"),
            ("child_kill", 1, "RustKotlinRuntime.childKill"),
            ("child_wait", 1, "RustKotlinRuntime.childWait"),
            ("child_try_wait", 1, "RustKotlinRuntime.childTryWait"),
            (
                "child_wait_with_output",
                1,
                "RustKotlinRuntime.childWaitWithOutput",
            ),
            (
                "exit_status_success",
                1,
                "RustKotlinRuntime.exitStatusSuccess",
            ),
        ] {
            let mut call = PortableOpCall {
                span: Default::default(),
                op: registry.resolve(op).expect("registered process operation"),
                args: (0..args)
                    .map(|index| Expr::name(Name::ident(format!("value_{index}"))))
                    .collect(),
                kwargs: Vec::new(),
            };
            let materialized = materializer
                .materialize_portable_op(&mut call, &None)
                .expect("materialize process operation")
                .expect("process operation replacement");
            assert_eq!(
                render_invoke_name(&materialized),
                expected,
                "operation: {op}"
            );
        }
    }

    #[test]
    fn materializes_child_and_duration_operations_without_rust_members() {
        let registry = PortableOpRegistry::builtin();
        let materializer = KotlinMaterializer;
        for (op, receiver, expected) in [
            ("option_take", "stdin", "stdin"),
            (
                "duration_from_millis",
                "millis",
                "java.time.Duration.ofMillis",
            ),
            (
                "duration_from_secs",
                "seconds",
                "java.time.Duration.ofSeconds",
            ),
            (
                "child_wait_with_output",
                "child",
                "RustKotlinRuntime.childWaitWithOutput",
            ),
        ] {
            let mut call = PortableOpCall {
                span: Default::default(),
                op: registry.resolve(op).expect("registered portable operation"),
                args: vec![Expr::name(Name::ident(receiver))],
                kwargs: Vec::new(),
            };
            let materialized = materializer
                .materialize_portable_op(&mut call, &None)
                .expect("materialize portable operation")
                .expect("portable operation replacement");
            if op == "option_take" {
                let ExprKind::Name(Name::Ident(name)) = materialized.kind() else {
                    panic!("expected Kotlin nullable value");
                };
                assert_eq!(name.name, expected);
            } else {
                let ExprKind::Invoke(invoke) = materialized.kind() else {
                    panic!("expected Kotlin operation invocation");
                };
                match &invoke.target {
                    ExprInvokeTarget::Function(Name::Ident(name)) => {
                        assert_eq!(name.name, expected)
                    }
                    ExprInvokeTarget::Method(select) => {
                        let receiver = match select.obj.kind() {
                            ExprKind::Name(Name::Ident(name)) => name.name.clone(),
                            ExprKind::Name(Name::Path(path)) => path.join("."),
                            _ => panic!("expected static Kotlin receiver"),
                        };
                        assert_eq!(format!("{receiver}.{}", select.field.name), expected);
                    }
                    _ => panic!("expected function or static Kotlin method"),
                }
            }
        }
    }

    #[test]
    fn materializes_collection_operations_through_the_runtime() {
        let registry = PortableOpRegistry::builtin();
        let materializer = KotlinMaterializer;

        for (op, expected) in [
            ("vec_extend", "RustKotlinRuntime.listExtend"),
            ("collect", "items.toMutableList"),
            ("filter", "items.filter"),
        ] {
            let mut call = PortableOpCall {
                span: Default::default(),
                op: registry.resolve(op).expect("registered portable op"),
                args: vec![
                    Expr::name(Name::ident("items")),
                    Expr::name(Name::ident("f")),
                ],
                kwargs: Vec::new(),
            };
            let materialized = materializer
                .materialize_portable_op(&mut call, &None)
                .expect("materialize collection operation")
                .expect("collection replacement");
            assert_eq!(render_invoke_name(&materialized), expected);
        }
    }

    #[test]
    fn materializes_byte_collection_conversion_with_kotlin_array_api() {
        let registry = PortableOpRegistry::builtin();
        let bytes = TySlot::from(Ty::Vec(fp_core::ast::TypeVec {
            ty: Box::new(Ty::Primitive(TypePrimitive::Int(TypeInt::U8))),
        }));

        for op in [
            "collect",
            "vec_from_iter",
            "slice_to_vec",
            "slice_to_vec_in",
        ] {
            let mut call = PortableOpCall {
                span: Default::default(),
                op: registry
                    .resolve(op)
                    .expect("registered collection operation"),
                args: vec![Expr::name(Name::ident("values"))],
                kwargs: Vec::new(),
            };
            let materialized = KotlinMaterializer
                .materialize_portable_op(&mut call, &bytes)
                .expect("materialize byte collection conversion")
                .expect("byte collection replacement");
            assert_eq!(render_invoke_name(&materialized), "values.toByteArray");
        }
    }

    #[test]
    fn materializes_kotlin_native_operations_from_ordinary_invokes() {
        let materializer = KotlinMaterializer;

        for (method, expected) in [
            ("trim", "text.trim"),
            ("trim_start", "text.trimStart"),
            ("trim_end", "text.trimEnd"),
            ("lines", "text.lines"),
            ("starts_with", "text.startsWith"),
            ("ends_with", "text.endsWith"),
        ] {
            let mut call = ExprInvoke {
                span: Default::default(),
                target: ExprInvokeTarget::Method(ExprSelect {
                    span: Default::default(),
                    obj: Box::new(Expr::name(Name::ident("text"))),
                    field: Ident::new(method),
                    generic_args: Vec::new(),
                    select: ExprSelectType::Method,
                }),
                args: Vec::new(),
                kwargs: Vec::new(),
            };
            let materialized = materializer
                .materialize_invoke(&mut call, &None)
                .expect("materialize native operation")
                .expect("native operation replacement");
            assert_eq!(render_invoke_name(&materialized), expected);
        }
    }

    #[test]
    fn materializes_option_is_none_as_a_null_check() {
        let mut call = ExprInvoke {
            span: Default::default(),
            target: ExprInvokeTarget::Method(ExprSelect {
                span: Default::default(),
                obj: Box::new(Expr::name(Name::ident("value"))),
                field: Ident::new("is_none"),
                generic_args: Vec::new(),
                select: ExprSelectType::Method,
            }),
            args: Vec::new(),
            kwargs: Vec::new(),
        };
        let materialized = KotlinMaterializer
            .materialize_invoke(&mut call, &None)
            .expect("materialize Option::is_none")
            .expect("null-check replacement");
        assert!(matches!(materialized.kind(), ExprKind::BinOp(_)));
    }

    #[test]
    fn materializes_filesystem_and_stream_operations_through_jvm_runtime_apis() {
        let registry = PortableOpRegistry::builtin();
        let materializer = KotlinMaterializer;
        for (op, expected) in [
            ("fs_read", "runCatching"),
            ("fs_read_dir", "RustKotlinRuntime.readDirectory"),
            ("fs_create_dir", "RustKotlinRuntime.createDirectory"),
            ("fs_create_dir_all", "RustKotlinRuntime.createDirectories"),
            ("file_create", "RustKotlinRuntime.createFile"),
            ("fs_canonicalize", "RustKotlinRuntime.canonicalize"),
            ("path_canonicalize", "RustKotlinRuntime.canonicalize"),
            ("path_exists", "java.nio.file.Files.exists"),
            ("path_join", "stream.resolve"),
            ("path_to_string_lossy", "stream.toString"),
            ("dir_entry_path", "stream.path"),
            ("dir_entry_file_type", "runCatching"),
            ("dir_entry_file_name", "stream.fileName"),
            ("file_type_is_dir", "stream.isDirectory"),
            ("os_str_to_string_lossy", "stream.toString"),
            ("slice_join", "stream.joinToString"),
            ("write_all", "RustKotlinRuntime.writeAll"),
        ] {
            let mut call = PortableOpCall {
                span: Default::default(),
                op: registry.resolve(op).expect("registered portable operation"),
                args: vec![
                    Expr::name(Name::ident("stream")),
                    Expr::name(Name::ident("bytes")),
                ],
                kwargs: Vec::new(),
            };
            let materialized = materializer
                .materialize_portable_op(&mut call, &None)
                .expect("materialize portable operation")
                .expect("portable operation replacement");
            assert_eq!(render_invoke_name(&materialized), expected);
        }

        for (op, expected) in [("path_parent", "parent"), ("path_file_name", "fileName")] {
            let mut call = PortableOpCall {
                span: Default::default(),
                op: registry.resolve(op).expect("registered portable operation"),
                args: vec![Expr::name(Name::ident("stream"))],
                kwargs: Vec::new(),
            };
            let materialized = materializer
                .materialize_portable_op(&mut call, &None)
                .expect("materialize Path property")
                .expect("Path property replacement");
            let ExprKind::Select(select) = materialized.kind() else {
                panic!("expected Kotlin property selection");
            };
            assert_eq!(select.field.as_str(), expected);
        }

        let mut to_path_buf = PortableOpCall {
            span: Default::default(),
            op: registry
                .resolve("path_to_path_buf")
                .expect("registered portable operation"),
            args: vec![Expr::name(Name::ident("path"))],
            kwargs: Vec::new(),
        };
        let materialized = materializer
            .materialize_portable_op(&mut to_path_buf, &None)
            .expect("materialize Path::to_path_buf")
            .expect("Path::to_path_buf replacement");
        assert!(
            matches!(materialized.kind(), ExprKind::Name(Name::Ident(name)) if name.name == "path")
        );
    }

    fn render_invoke_name(expr: &Expr) -> String {
        let ExprKind::Invoke(invoke) = expr.kind() else {
            panic!("expected invocation");
        };
        match &invoke.target {
            ExprInvokeTarget::Function(Name::Ident(name)) => return name.name.clone(),
            ExprInvokeTarget::Method(select) => {
                let receiver = match select.obj.kind() {
                    ExprKind::Name(Name::Ident(receiver)) => receiver.name.clone(),
                    ExprKind::Name(Name::Path(path)) => path.join("."),
                    _ => panic!("expected static receiver"),
                };
                format!("{receiver}.{}", select.field.name)
            }
            _ => panic!("expected function or static method invocation"),
        }
    }
}

fn invoke_method(receiver: Expr, method: &str, args: Vec<Expr>) -> Expr {
    Expr::new(ExprKind::Invoke(ExprInvoke {
        span: Default::default(),
        target: ExprInvokeTarget::Method(ExprSelect {
            span: Default::default(),
            obj: Box::new(receiver),
            field: Ident::new(method),
            generic_args: Vec::new(),
            select: ExprSelectType::Method,
        }),
        args,
        kwargs: Vec::new(),
    }))
}

fn select_property(receiver: Expr, property: &str) -> Expr {
    Expr::new(ExprKind::Select(ExprSelect {
        span: Default::default(),
        obj: Box::new(receiver),
        field: Ident::new(property),
        generic_args: Vec::new(),
        select: ExprSelectType::Field,
    }))
}
