use super::*;

/// Materializes fp-lang portable operations into Kotlin AST constructs.
///
/// The Rust frontend supplies operation identity via `#[op]`; this target
/// pass selects Kotlin's nullable, collection, and exception equivalents.
pub struct KotlinMaterializer;

impl KotlinMaterializer {
    fn materialize_type_mapping(&self, ty: &Ty) -> Result<MaterializeOutcome<Ty>> {
        let materialized = materialize_aliases(materialize_jvm_type(ty.clone()));
        if materialized == *ty { Ok(MaterializeOutcome::Unchanged) } else { Ok(MaterializeOutcome::Replaced(materialized)) }
    }

    pub(crate) fn lower_portable_operation(
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
            "path_exists" => Ok(Some(runtime_method("pathExists", vec![receiver()]))),
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

    pub(crate) fn lower_invoke(&self, invoke: &mut ExprInvoke, ty: &TySlot) -> Result<Option<Expr>> {
        // Rust error constructors retain concrete payload types, while Kotlin
        // Result callbacks expose Throwable. Normalize this payload at the
        // target materialization boundary, before syntax serialization.
        if is_io_constructor(&invoke.target) && invoke.args.len() == 1 {
            let error = invoke.args.pop().expect("one Io constructor argument");
            invoke.args.push(runtime_method("ioError", vec![error]));
        }
        // Keep Rust standard-library constructors target-independent until this
        // backend boundary.  Unresolved dependency HIR may leave these as
        // ordinary function paths, so relying only on the portable-op registry
        // would leak `ProcessBuilder`/`Path` spellings into Kotlin.
        if let ExprInvokeTarget::Function(name) = &invoke.target {
            let path = invoke_name_segments(name);
            if path.as_slice() == ["IOException"] {
                return Ok(Some(runtime_method("ioError", invoke.args.clone())));
            }
            if let Some(replacement) = materialize_std_function(&path, &invoke.args) {
                return Ok(Some(replacement));
            }
            let last = path.last().map(String::as_str).unwrap_or_default();
            let owner = path
                .iter()
                .rev()
                .nth(1)
                .map(String::as_str)
                .unwrap_or_default();
            if last == "new" && owner == "Command" {
                return Ok(Some(runtime_method("command", invoke.args.clone())));
            }
            if matches!(last, "from" | "new") && matches!(owner, "Path" | "PathBuf") {
                return Ok(Some(invoke_static_method(
                    &["java", "nio", "file", "Paths"],
                    "get",
                    invoke.args.clone(),
                )));
            }
            if owner == "thread" || owner == "Thread" {
                if last == "sleep" {
                    return Ok(Some(runtime_method("sleep", invoke.args.clone())));
                }
            }
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
            "char_indices" => runtime_method("charIndices", vec![receiver]),
            "split_at" => runtime_method(
                "splitAt",
                std::iter::once(receiver)
                    .chain(invoke.args.clone())
                    .collect(),
            ),
            "strip_prefix" => runtime_method(
                "stripPrefix",
                std::iter::once(receiver)
                    .chain(invoke.args.clone())
                    .collect(),
            ),
            "is_letter_or_digit" => invoke_method(receiver, "isLetterOrDigit", invoke.args.clone()),
            "is_ascii_hexdigit" => runtime_method("charIsAsciiHexDigit", vec![receiver]),
            "is_ascii_digit" => runtime_method("charIsAsciiDigit", vec![receiver]),
            "is_ascii_alphabetic" => runtime_method("charIsAsciiAlphabetic", vec![receiver]),
            "is_alphabetic" => runtime_method("charIsAlphabetic", vec![receiver]),
            "is_whitespace" => runtime_method("charIsWhitespace", vec![receiver]),
            "is_digit" => runtime_method(
                "charIsDigit",
                std::iter::once(receiver)
                    .chain(invoke.args.clone())
                    .collect(),
            ),
            "to_owned" => kotlin_owned_value(receiver, ty),
            "to_string" => invoke_method(receiver, "toString", invoke.args.clone()),
            "is_none" => Expr::new(ExprKind::BinOp(ExprBinOp {
                span: Default::default(),
                kind: BinOpKind::Eq,
                lhs: Box::new(receiver),
                rhs: Box::new(Expr::value(Value::Null(Default::default()))),
            })),
            "position" => invoke_method(receiver, "indexOfFirst", invoke.args.clone()),
            "clone" | "copy" | "into_owned" => kotlin_owned_value(receiver, ty),
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

    pub(crate) fn lower_select(&self, select: &mut ExprSelect, _ty: &TySlot) -> Result<Option<Expr>> {
        let receiver = (*select.obj).clone();
        let replacement = match select.field.as_str() {
            "isSuccess" | "is_ok" => runtime_method("resultIsSuccess", vec![receiver]),
            "isFailure" | "is_err" => runtime_method("resultIsFailure", vec![receiver]),
            _ => return Ok(None),
        };
        Ok(Some(replacement))
    }

    pub(crate) fn lower_await(&self, await_expr: &mut ExprAwait, _ty: &TySlot) -> Result<Option<Expr>> {
        // Kotlin suspension is expressed by calling a `suspend` function
        // directly. The operand has already been materialized into that call.
        Ok(Some((*await_expr.base).clone()))
    }

    pub(crate) fn lower_intrinsic_call(&self, call: &mut ExprIntrinsicCall, _ty: &TySlot) -> Result<Option<Expr>> {
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

    pub(crate) fn lower_intrinsic_container(
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

impl IntrinsicMaterializer for KotlinMaterializer {
    fn capabilities(&self) -> fp_core::capabilities::LanguageCapabilities { crate::CAPABILITIES }
    fn materialize_type_mapping(&self, ty: &Ty) -> Result<MaterializeOutcome<Ty>> { self.materialize_type_mapping(ty) }
    fn materialize_invoke_expression(&self, invoke: ExprInvoke, ty: &TySlot) -> Result<MaterializeOutcome<Expr>> {
        Ok(self.lower_invoke(&mut invoke.clone(), ty)?.map_or(MaterializeOutcome::Unchanged, MaterializeOutcome::Replaced))
    }
    fn materialize_select_expression(&self, select: ExprSelect, ty: &TySlot) -> Result<MaterializeOutcome<Expr>> {
        Ok(self.lower_select(&mut select.clone(), ty)?.map_or(MaterializeOutcome::Unchanged, MaterializeOutcome::Replaced))
    }
    fn materialize_await_expression(&self, expr: ExprAwait, ty: &TySlot) -> Result<MaterializeOutcome<Expr>> {
        Ok(self.lower_await(&mut expr.clone(), ty)?.map_or(MaterializeOutcome::Unchanged, MaterializeOutcome::Replaced))
    }
    fn materialize_intrinsic_call(&self, call: ExprIntrinsicCall, ty: &TySlot) -> Result<MaterializeOutcome<Expr>> {
        Ok(self.lower_intrinsic_call(&mut call.clone(), ty)?.map_or(MaterializeOutcome::Unchanged, MaterializeOutcome::Replaced))
    }
    fn materialize_portable_operation(&self, call: PortableOpCall, ty: &TySlot) -> Result<MaterializeOutcome<Expr>> {
        Ok(self.lower_portable_operation(&mut call.clone(), ty)?.map_or(MaterializeOutcome::Unchanged, MaterializeOutcome::Replaced))
    }
    fn materialize_intrinsic_container(&self, container: ExprIntrinsicContainer, ty: &TySlot) -> Result<MaterializeOutcome<Expr>> {
        Ok(self.lower_intrinsic_container(&mut container.clone(), ty)?.map_or(MaterializeOutcome::Unchanged, MaterializeOutcome::Replaced))
    }
}
