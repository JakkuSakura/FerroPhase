use fp_core::ast::{
    BlockStmt, ExprBlock, ExprFieldAccess, ExprIntrinsicCall, ExprInvoke, ExprInvokeTarget,
    ExprKind, File, Ident, Item, ItemDefFunction, ItemKind, Name, PathArguments, PatternKind, Ty,
    Value,
};
use fp_core::intrinsics::{PortableOpCall, materialize_file};
use fp_core::lang::LangItemRegistry;

use super::*;

fn test_registry() -> LangItemRegistry {
    let mut registry = LangItemRegistry::default();
    for name in [
        "default",
        "result_ok",
        "result_err",
        "option_unwrap",
        "option_take",
        "result_unwrap",
        "result_map",
        "result_map_err",
        "result_is_ok",
        "result_is_err",
        "result_ok_value",
        "result_err_value",
        "result_unwrap_or",
        "io_error_new",
        "str_parse",
        "result_propagate",
        "fs_read",
        "fs_read_to_string",
        "fs_read_dir",
        "fs_create_dir",
        "fs_write_string",
        "fs_append_string",
        "fs_is_dir",
        "fs_is_file",
        "fs_create_dir_all",
        "fs_remove_file",
        "fs_remove_dir_all",
        "fs_canonicalize",
        "fs_glob",
        "file_create",
        "path_canonicalize",
        "path_exists",
        "path_parent",
        "path_join",
        "path_file_name",
        "path_to_path_buf",
        "path_to_string_lossy",
        "os_str_to_string_lossy",
        "dir_entry_path",
        "dir_entry_file_type",
        "dir_entry_file_name",
        "file_type_is_dir",
        "slice_join",
        "write_all",
        "command_new",
        "command_arg",
        "command_args",
        "command_current_dir",
        "command_stdin",
        "command_stdout",
        "command_stderr",
        "command_spawn",
        "command_output",
        "command_status",
        "stdio_piped",
        "stdio_inherit",
        "stdio_null",
        "child_kill",
        "child_wait",
        "child_try_wait",
        "child_wait_with_output",
        "exit_status_success",
        "vec_new",
        "vec_from",
        "vec_from_iter",
        "vec_push",
        "vec_extend",
        "slice_to_vec",
        "slice_to_vec_in",
        "clone",
        "to_owned",
        "as_ref",
        "as_str",
        "unwrap_or",
        "map_or",
        "iter",
        "collect",
        "filter",
        "find_map",
        "split_whitespace",
        "split",
        "str_char_indices",
        "str_split_at",
        "str_strip_prefix",
        "bool_then_some",
        "range_inclusive_contains",
        "char_is_digit",
        "char_is_alphabetic",
        "char_is_whitespace",
        "char_is_ascii_alphabetic",
        "char_is_ascii_digit",
        "char_is_ascii_hexdigit",
        "string_from_utf8_lossy",
        "string_from_utf8",
        "duration_from_secs",
        "duration_from_millis",
    ] {
        registry.insert_op(
            name,
            fp_core::ast::Path::plain(vec![fp_core::ast::Ident::new(name)]),
        );
    }
    registry
}

#[test]
fn materializes_result_constructors_without_erasing_them() {
    let registry = test_registry();
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
        .lower_portable_operation(&mut ok, &None)
        .expect("materialize Ok")
        .expect("Ok replacement");
    let err = materializer
        .lower_portable_operation(&mut err, &None)
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
    let registry = test_registry();
    let mut ok = PortableOpCall {
        span: Default::default(),
        op: registry.resolve("result_ok").expect("registered result ok"),
        args: vec![Expr::unit()],
        kwargs: Vec::new(),
    };

    let materialized = KotlinMaterializer
        .lower_portable_operation(&mut ok, &None)
        .expect("materialize Ok(())")
        .expect("Ok(()) replacement");
    let ExprKind::Invoke(invoke) = materialized.kind() else {
        panic!("expected Result success adapter invocation");
    };
    assert!(
        matches!(invoke.args[0].kind(), ExprKind::Name(Name { path, .. }) if path.last().as_str() == "Unit")
    );
}

#[test]
fn materializes_checked_option_and_result_unwraps() {
    let registry = test_registry();
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
        .lower_portable_operation(&mut option, &None)
        .expect("materialize Option::unwrap")
        .expect("Option::unwrap replacement");
    let result = materializer
        .lower_portable_operation(&mut result, &None)
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
    let registry = test_registry();
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
        .lower_portable_operation(&mut call, &None)
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
fn materializes_str_parse_as_typed_kotlin_result() {
    let registry = test_registry();
    let mut call = PortableOpCall {
        span: Default::default(),
        op: registry.resolve("str_parse").expect("registered str_parse"),
        args: vec![Expr::name(Name::ident("input"))],
        kwargs: Vec::new(),
    };

    let parsed = KotlinMaterializer
        .lower_portable_operation(&mut call, &None)
        .expect("materialize str::parse")
        .expect("str::parse replacement");
    assert_eq!(render_invoke_name(&parsed), "RustKotlinRuntime.parse");
}

#[test]
fn materializes_result_propagation_as_single_unwrap() {
    let registry = test_registry();
    let mut call = PortableOpCall {
        span: Default::default(),
        op: registry
            .resolve("result_propagate")
            .expect("registered result_propagate"),
        args: vec![Expr::name(Name::ident("source"))],
        kwargs: Vec::new(),
    };

    let materialized = KotlinMaterializer
        .lower_portable_operation(&mut call, &None)
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
        .lower_intrinsic_call(&mut call, &None)
        .expect("materialize filesystem call")
        .expect("filesystem replacement");
    assert_eq!(render_invoke_name(&materialized), "runCatching");
}

#[test]
fn materializes_result_error_mapping_through_the_runtime() {
    let registry = test_registry();
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
        .lower_portable_operation(&mut call, &None)
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
        matches!(throwable.kind(), ExprKind::Name(Name { path, .. }) if path.last().as_str() == "Throwable")
    );
    assert_eq!(
        render_invoke_name(&mapping.body),
        "RustKotlinRuntime.normalizeError"
    );
}

#[test]
fn materializes_result_operations_through_the_runtime() {
    let registry = test_registry();
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
            .lower_portable_operation(&mut call, &None)
            .expect("materialize Result operation")
            .expect("Result operation replacement");
        assert_eq!(render_invoke_name(&materialized), expected);
    }
}

#[test]
fn materializes_kotlin_result_status_properties_through_the_runtime() {
    let materializer = KotlinMaterializer;
    for (field, expected) in [
        ("isSuccess", "RustKotlinRuntime.resultIsSuccess"),
        ("isFailure", "RustKotlinRuntime.resultIsFailure"),
    ] {
        let mut select = ExprFieldAccess {
            span: Default::default(),
            obj: Box::new(Expr::name(Name::ident("result"))),
            field: Ident::new(field),
            generic_args: PathArguments::None,
        };
        let materialized = materializer
            .lower_select(&mut select, &None)
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
fn leaves_unresolved_result_operations_for_resolution() {
    let map_err = Expr::new(ExprKind::Invoke(ExprInvoke {
        span: Default::default(),
        target: ExprInvokeTarget::Method(ExprFieldAccess {
            span: Default::default(),
            obj: Box::new(Expr::name(Name::ident("result"))),
            field: Ident::new("map_err"),
            generic_args: PathArguments::None,
        }),
        args: vec![Expr::name(Name::ident("convert_error"))],
        kwargs: Vec::new(),
    }));
    let is_success = Expr::new(ExprKind::FieldAccess(ExprFieldAccess {
        span: Default::default(),
        obj: Box::new(Expr::name(Name::ident("result"))),
        field: Ident::new("isSuccess"),
        generic_args: PathArguments::None,
    }));
    let ok = Expr::new(ExprKind::Invoke(ExprInvoke {
        span: Default::default(),
        target: ExprInvokeTarget::Method(ExprFieldAccess {
            span: Default::default(),
            obj: Box::new(Expr::name(Name::ident("result"))),
            field: Ident::new("ok"),
            generic_args: PathArguments::None,
        }),
        args: Vec::new(),
        kwargs: Vec::new(),
    }));
    let file = materialize_file(
        File {
            path: Default::default(),
            attrs: Vec::new(),
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
        rendered.contains("result.map_err(convert_error)"),
        "{rendered}"
    );
    assert!(
        rendered.contains("RustKotlinRuntime.resultIsSuccess(result)"),
        "{rendered}"
    );
    assert!(rendered.contains("result.ok()"), "{rendered}");
}

#[test]
fn materializes_vec_push_through_the_runtime() {
    let registry = test_registry();
    let mut call = PortableOpCall {
        span: Default::default(),
        op: registry.resolve("vec_push").expect("registered vec_push"),
        args: vec![Expr::name(Name::ident("items")), Expr::value(Value::int(1))],
        kwargs: Vec::new(),
    };

    let materialized = KotlinMaterializer
        .lower_portable_operation(&mut call, &None)
        .expect("materialize vec push")
        .expect("vec push replacement");
    assert_eq!(
        render_invoke_name(&materialized),
        "RustKotlinRuntime.listPush"
    );
}

#[test]
fn materializes_owned_values_without_rust_copy_methods() {
    let registry = test_registry();
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
        .lower_portable_operation(&mut clone, &bytes_ty)
        .expect("materialize byte clone")
        .expect("byte clone replacement");
    assert_eq!(
        render_invoke_name(&cloned),
        "RustKotlinRuntime.bytesFromIterable"
    );
}

#[test]
fn materializes_collection_defaults_without_rust_default_calls() {
    let registry = test_registry();
    let bytes_ty = Some(Ty::name(Name::path(fp_core::ast::Path::new(
        fp_core::ast::path::PathPrefix::Plain,
        vec![fp_core::ast::PathSegment::new(
            Ident::new("Vec"),
            vec![Ty::ident(Ident::new("u8"))],
        )],
    ))));
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
            .lower_portable_operation(&mut call, &ty)
            .expect("materialize collection default")
            .expect("collection default replacement");
        assert_eq!(render_invoke_name(&materialized), expected);
    }
}

#[test]
fn materializes_vec_constructors_without_rust_factory_names() {
    let registry = test_registry();
    let materializer = KotlinMaterializer;

    let mut new_call = PortableOpCall {
        span: Default::default(),
        op: registry.resolve("vec_new").expect("registered vec_new"),
        args: Vec::new(),
        kwargs: Vec::new(),
    };
    let new_expr = materializer
        .lower_portable_operation(&mut new_call, &None)
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
        .lower_portable_operation(&mut from_call, &None)
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
        .lower_portable_operation(&mut from_iter_call, &None)
        .expect("materialize Vec::from_iter")
        .expect("Vec::from_iter replacement");
    assert_eq!(render_invoke_name(&from_iter_expr), "source.toMutableList");
}

#[test]
fn materializes_slice_vector_cloning_without_allocator_operations() {
    let registry = test_registry();
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
            .lower_portable_operation(&mut call, &None)
            .expect("materialize slice clone")
            .expect("slice clone replacement");
        assert_eq!(render_invoke_name(&materialized), "source.toMutableList");
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
        .lower_portable_operation(&mut byte_call, &bytes_ty)
        .expect("materialize byte slice clone")
        .expect("ByteArray clone replacement");
    assert_eq!(render_invoke_name(&materialized), "bytes.toByteArray");
}

#[test]
fn materializes_byte_vectors_as_byte_arrays() {
    let registry = test_registry();
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
        .lower_portable_operation(&mut new_call, &bytes_ty)
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
        .lower_portable_operation(&mut push_call, &bytes_ty)
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
        .lower_portable_operation(&mut collect_call, &bytes_ty)
        .expect("materialize byte collect")
        .expect("ByteArray collection replacement");
    assert_eq!(render_invoke_name(&collect_expr), "source.toByteArray");
}

#[test]
fn materializes_byte_vector_literals_and_utf8_decoding_through_runtime() {
    let registry = test_registry();
    let bytes_ty = Some(Ty::Vec(fp_core::ast::TypeVec {
        ty: Box::new(Ty::Primitive(fp_core::ast::TypePrimitive::Int(
            fp_core::ast::TypeInt::U8,
        ))),
    }));
    let mut container = ExprIntrinsicContainer::VecElements {
        elements: vec![Expr::value(Value::int(65))],
    };
    let bytes = KotlinMaterializer
        .lower_intrinsic_container(&mut container, &bytes_ty)
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
        .lower_portable_operation(&mut decode_call, &None)
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
            .lower_intrinsic_call(&mut call, &None)
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
            .lower_intrinsic_call(&mut call, &None)
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
fn materializes_process_operations_through_the_runtime_model() {
    let registry = test_registry();
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
            .lower_portable_operation(&mut call, &None)
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
    let registry = test_registry();
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
            .lower_portable_operation(&mut call, &None)
            .expect("materialize portable operation")
            .expect("portable operation replacement");
        if op == "option_take" {
            let ExprKind::Name(Name { path, .. }) = materialized.kind() else {
                panic!("expected Kotlin nullable value");
            };
            assert_eq!(path.join("."), expected);
        } else {
            let ExprKind::Invoke(invoke) = materialized.kind() else {
                panic!("expected Kotlin operation invocation");
            };
            match &invoke.target {
                ExprInvokeTarget::Function(Name { path, .. }) => {
                    assert_eq!(path.join("."), expected)
                }
                ExprInvokeTarget::Method(select) => {
                    let receiver = match select.obj.kind() {
                        ExprKind::Name(Name { path, .. }) => path.join("."),
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
    let registry = test_registry();
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
            .lower_portable_operation(&mut call, &None)
            .expect("materialize collection operation")
            .expect("collection replacement");
        assert_eq!(render_invoke_name(&materialized), expected);
    }
}

#[test]
fn materializes_byte_collection_conversion_with_kotlin_array_api() {
    let registry = test_registry();
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
            .lower_portable_operation(&mut call, &bytes)
            .expect("materialize byte collection conversion")
            .expect("byte collection replacement");
        assert_eq!(render_invoke_name(&materialized), "values.toByteArray");
    }
}

#[test]
fn materializes_filesystem_and_stream_operations_through_jvm_runtime_apis() {
    let registry = test_registry();
    let materializer = KotlinMaterializer;
    for (op, expected) in [
        ("fs_read", "runCatching"),
        ("fs_read_dir", "RustKotlinRuntime.readDirectory"),
        ("fs_create_dir", "RustKotlinRuntime.createDirectory"),
        ("fs_create_dir_all", "RustKotlinRuntime.createDirectories"),
        ("file_create", "RustKotlinRuntime.createFile"),
        ("fs_canonicalize", "RustKotlinRuntime.canonicalize"),
        ("path_canonicalize", "RustKotlinRuntime.canonicalize"),
        ("path_exists", "RustKotlinRuntime.pathExists"),
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
            .lower_portable_operation(&mut call, &None)
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
            .lower_portable_operation(&mut call, &None)
            .expect("materialize Path property")
            .expect("Path property replacement");
        let ExprKind::FieldAccess(select) = materialized.kind() else {
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
        .lower_portable_operation(&mut to_path_buf, &None)
        .expect("materialize Path::to_path_buf")
        .expect("Path::to_path_buf replacement");
    assert!(
        matches!(materialized.kind(), ExprKind::Name(Name { path, .. }) if path.last().as_str() == "path")
    );
}

fn render_invoke_name(expr: &Expr) -> String {
    let ExprKind::Invoke(invoke) = expr.kind() else {
        panic!("expected invocation");
    };
    match &invoke.target {
        ExprInvokeTarget::Function(Name { path, .. }) => return path.join("."),
        ExprInvokeTarget::Method(select) => {
            let receiver = match select.obj.kind() {
                ExprKind::Name(Name { path, .. }) => path.join("."),
                _ => panic!("expected static receiver"),
            };
            format!("{receiver}.{}", select.field.name)
        }
        _ => panic!("expected function or static method invocation"),
    }
}
