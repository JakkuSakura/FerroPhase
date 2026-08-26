use super::body::BodyBuilder;
use super::*;
use fp_core::error::Result;
use fp_core::hir;
use fp_core::mir;
use fp_core::mir::ty::{ConstKind, ConstValue, Ty, TyKind};
use fp_core::span::Span;

impl<'a> BodyBuilder<'a> {
    pub(super) fn lower_literal(
        &mut self,
        lit: &hir::Lit,
        expected: Option<&Ty>,
    ) -> (mir::ConstantKind, Ty) {
        match lit {
            hir::Lit::Bool(value) => (mir::ConstantKind::Bool(*value), Ty { kind: TyKind::Bool }),
            hir::Lit::Integer(value) => {
                if let Some(expected_ty) = expected {
                    match &expected_ty.kind {
                        TyKind::Uint(_) => {
                            (mir::ConstantKind::UInt(*value as u64), expected_ty.clone())
                        }
                        TyKind::Int(_) => (mir::ConstantKind::Int(*value), expected_ty.clone()),
                        _ => (
                            mir::ConstantKind::Int(*value),
                            Ty {
                                kind: TyKind::Int(IntTy::I64),
                            },
                        ),
                    }
                } else {
                    (
                        mir::ConstantKind::Int(*value),
                        Ty {
                            kind: TyKind::Int(IntTy::I64),
                        },
                    )
                }
            }
            hir::Lit::Float(value) => (
                mir::ConstantKind::Float(*value),
                Ty {
                    kind: TyKind::Float(FloatTy::F64),
                },
            ),
            hir::Lit::Str(value) => (
                mir::ConstantKind::Str(value.clone()),
                self.lowering.string_slice_ty(),
            ),
            hir::Lit::Char(value) => (
                mir::ConstantKind::Int(*value as i64),
                Ty {
                    kind: TyKind::Int(IntTy::I32),
                },
            ),
            hir::Lit::Null => {
                let ty = expected.cloned().unwrap_or_else(|| Ty {
                    kind: TyKind::RawPtr(TypeAndMut {
                        ty: Box::new(Ty {
                            kind: TyKind::Int(IntTy::I8),
                        }),
                        mutbl: Mutability::Not,
                    }),
                });
                (mir::ConstantKind::Null, ty)
            }
            // `expected` should always be populated in practice (a
            // `b"..."`/`c"..."` literal only ever appears where a
            // `&[u8; N]`/`&CStr`-typed context already exists), matching
            // what HIR-typeck already resolved (`literal_ty` in
            // `fp-typing/src/hir_typeck.rs`) — the fallback here is a
            // best-effort default for the rare case it isn't.
            hir::Lit::Bytes(bytes) => {
                let ty = expected.cloned().unwrap_or_else(|| Ty {
                    kind: TyKind::Ref(
                        mir::ty::Region::ReErased,
                        Box::new(Ty {
                            kind: TyKind::Array(
                                Box::new(Ty {
                                    kind: TyKind::Uint(UintTy::U8),
                                }),
                                ConstKind::Value(ConstValue::Scalar(Scalar::Int(ScalarInt {
                                    data: bytes.len() as u128,
                                    size: 8,
                                }))),
                            ),
                        }),
                        Mutability::Not,
                    ),
                });
                (
                    mir::ConstantKind::Str(String::from_utf8_lossy(bytes).into_owned()),
                    ty,
                )
            }
            hir::Lit::CStr(bytes) => {
                let ty = expected
                    .cloned()
                    .unwrap_or_else(|| self.lowering.string_slice_ty());
                (
                    mir::ConstantKind::Str(String::from_utf8_lossy(bytes).into_owned()),
                    ty,
                )
            }
        }
    }

    pub(super) fn lower_intrinsic_constant(
        &mut self,
        call: &hir::IntrinsicCallExpr,
        span: Span,
    ) -> Option<(mir::ConstantKind, Ty)> {
        let args = &call.callargs;
        if call
            .callargs
            .first()
            .is_some_and(|arg| matches!(arg.value.kind, hir::ExprKind::FormatString(_)))
        {
            self.lowering.emit_warning(
                span,
                "treating formatted intrinsic payload as opaque during MIR lowering",
            );
            return None;
        }
        let arg_values: Vec<&hir::Expr> = args.iter().map(|arg| &arg.value).collect();

        let kind = call.kind;

        match kind {
            IntrinsicKind::SizeOf => {
                let target_expr = match arg_values.get(0) {
                    Some(expr) => *expr,
                    None => {
                        self.lowering
                            .emit_error(span, "sizeof! intrinsic expects one argument");
                        return None;
                    }
                };

                // `sizeof!(T)` where `T` is the enclosing function/method's
                // own generic type parameter (e.g. `impl<T> Vec<T> { fn
                // push(&mut self, value: T) { ... sizeof!(T) ... } }`) — `T`
                // has no struct definition for `resolve_struct_ref` to find,
                // but by the time this specialized body is lowered,
                // `self.type_substs` (the same per-specialization map
                // `payload_types_from_type_substs` reads for enum payloads)
                // already holds the concrete substitution for it. AST→HIR
                // still lowers an unresolved bare identifier like `T` to a
                // usable `hir::Path` (segment name preserved, `res: None`),
                // so check `type_substs` by name before falling through to
                // the struct-only path below.
                if let hir::ExprKind::Path(path) = &target_expr.kind {
                    if let [segment] = path.segments.as_slice() {
                        if let Some(resolved_ty) =
                            self.type_substs.get(segment.name.as_str()).cloned()
                        {
                            let size = match self.compute_ty_size(span, &resolved_ty) {
                                Some(value) => value,
                                None => return None,
                            };
                            return Some((
                                mir::ConstantKind::UInt(size),
                                Ty {
                                    kind: TyKind::Uint(UintTy::U64),
                                },
                            ));
                        }
                    }
                }

                let struct_ref = match self.resolve_struct_ref(target_expr) {
                    Some(value) => value,
                    None => {
                        self.lowering
                            .emit_error(span, "sizeof! only supports struct types at the moment");
                        return None;
                    }
                };

                let size = match self.compute_struct_size(span, &struct_ref) {
                    Some(value) => value,
                    None => return None,
                };

                Some((
                    mir::ConstantKind::UInt(size),
                    Ty {
                        kind: TyKind::Uint(UintTy::U64),
                    },
                ))
            }
            IntrinsicKind::FieldCount => {
                let target_expr = match arg_values.get(0) {
                    Some(expr) => *expr,
                    None => {
                        self.lowering
                            .emit_error(span, "field_count! intrinsic expects one argument");
                        return None;
                    }
                };

                let struct_ref = match self.resolve_struct_ref(target_expr) {
                    Some(value) => value,
                    None => {
                        self.lowering
                            .emit_error(span, "field_count! only supports struct types");
                        return None;
                    }
                };

                let field_count = match self.lowering.struct_def(&struct_ref.def_id) {
                    Some(info) => info.fields.len() as u64,
                    None => {
                        self.lowering
                            .emit_error(span, "struct metadata is unavailable during MIR lowering");
                        return None;
                    }
                };

                Some((
                    mir::ConstantKind::UInt(field_count),
                    Ty {
                        kind: TyKind::Uint(UintTy::U64),
                    },
                ))
            }
            IntrinsicKind::HasField => {
                if args.len() != 2 {
                    self.lowering
                        .emit_error(span, "hasfield! intrinsic expects a type and field name");
                    return None;
                }

                let struct_ref = match self.resolve_struct_ref(arg_values[0]) {
                    Some(value) => value,
                    None => {
                        self.lowering
                            .emit_error(span, "hasfield! only supports struct types");
                        return None;
                    }
                };

                let field_name = match self.expect_string_literal(arg_values[1], span) {
                    Some(name) => name,
                    None => return None,
                };

                let has_field = match self.lowering.struct_def(&struct_ref.def_id) {
                    Some(info) => info.field_index.contains_key(&field_name),
                    None => {
                        self.lowering
                            .emit_error(span, "struct metadata is unavailable during MIR lowering");
                        return None;
                    }
                };

                Some((
                    mir::ConstantKind::Bool(has_field),
                    Ty { kind: TyKind::Bool },
                ))
            }
            IntrinsicKind::MethodCount => {
                let target_expr = match arg_values.get(0) {
                    Some(expr) => *expr,
                    None => {
                        self.lowering
                            .emit_error(span, "method_count! intrinsic expects one argument");
                        return None;
                    }
                };

                let struct_ref = match self.resolve_struct_ref(target_expr) {
                    Some(value) => value,
                    None => {
                        self.lowering
                            .emit_error(span, "method_count! only supports struct types");
                        return None;
                    }
                };

                let struct_name = match self.lowering.struct_def(&struct_ref.def_id) {
                    Some(info) => info.name.clone(),
                    None => {
                        self.lowering
                            .emit_error(span, "struct metadata is unavailable during MIR lowering");
                        return None;
                    }
                };

                let method_count = self
                    .lowering
                    .mir_package
                    .borrow()
                    .struct_methods
                    .get(&struct_name)
                    .map(|methods| methods.len() as u64)
                    .unwrap_or(0);

                Some((
                    mir::ConstantKind::UInt(method_count),
                    Ty {
                        kind: TyKind::Uint(UintTy::U64),
                    },
                ))
            }
            IntrinsicKind::HasMethod => {
                if args.len() != 2 {
                    self.lowering
                        .emit_error(span, "hasmethod! intrinsic expects a type and method name");
                    return None;
                }
                let struct_ref = match self.resolve_struct_ref(arg_values[0]) {
                    Some(value) => value,
                    None => {
                        self.lowering
                            .emit_error(span, "hasmethod! only supports struct types");
                        return None;
                    }
                };
                let method_name = match self.expect_string_literal(arg_values[1], span) {
                    Some(name) => name,
                    None => return None,
                };
                let struct_name = match self.lowering.struct_def(&struct_ref.def_id) {
                    Some(info) => info.name.clone(),
                    None => {
                        self.lowering
                            .emit_error(span, "struct metadata is unavailable during MIR lowering");
                        return None;
                    }
                };
                let has_method = self
                    .lowering
                    .mir_package
                    .borrow()
                    .struct_methods
                    .get(&struct_name)
                    .is_some_and(|methods| methods.contains_key(&method_name));
                Some((
                    mir::ConstantKind::Bool(has_method),
                    Ty { kind: TyKind::Bool },
                ))
            }
            _ => None,
        }
    }

    pub(super) fn emit_printf_call(
        &mut self,
        call: &hir::IntrinsicCallExpr,
        span: Span,
    ) -> Result<()> {
        let Some((template, positional_slots, named_args, name_map)) =
            self.format_call_parts(call, span)
        else {
            return Ok(());
        };

        let mut prepared_positional = Vec::with_capacity(positional_slots.len());
        for slot in positional_slots {
            if let Some(arg) = slot {
                let lowered = if let Some(formatted) =
                    self.try_format_const_expr_for_printf(&arg.value, span)
                {
                    formatted
                } else {
                    self.lower_operand(&arg.value, None)?
                };
                prepared_positional.push(Some(self.prepare_printf_arg(lowered, span)?));
            } else {
                prepared_positional.push(None);
            }
        }

        let mut prepared_named = Vec::with_capacity(named_args.len());
        for arg in named_args {
            let lowered =
                if let Some(formatted) = self.try_format_const_expr_for_printf(&arg.value, span) {
                    formatted
                } else {
                    self.lower_operand(&arg.value, None)?
                };
            prepared_named.push(self.prepare_printf_arg(lowered, span)?);
        }

        let mut format = String::new();
        let mut implicit_index = 0usize;
        let mut ordered_operands = Vec::new();

        for part in &template.parts {
            match part {
                hir::FormatTemplatePart::Literal(text) => format.push_str(text.as_str()),
                hir::FormatTemplatePart::Placeholder(placeholder) => {
                    let (prepared, missing_message) = match &placeholder.arg_ref {
                        hir::FormatArgRef::Implicit => {
                            let current = implicit_index;
                            implicit_index += 1;
                            (
                                prepared_positional.get(current).cloned().flatten(),
                                format!(
                                    "format placeholder references missing argument at index {}",
                                    current
                                ),
                            )
                        }
                        hir::FormatArgRef::Positional(index) => (
                            prepared_positional.get(*index).cloned().flatten(),
                            format!(
                                "format placeholder references missing argument at index {}",
                                index
                            ),
                        ),
                        hir::FormatArgRef::Named(name) => (
                            name_map
                                .get(name)
                                .and_then(|index| prepared_named.get(*index).cloned()),
                            format!("format placeholder references missing argument `{name}`"),
                        ),
                    };

                    let Some((operand, _ty, spec)) = prepared else {
                        self.lowering.emit_error(span, missing_message);
                        return Ok(());
                    };
                    ordered_operands.push(operand);

                    if let Some(explicit) = &placeholder.format_spec {
                        let trimmed = explicit.raw.trim();
                        if trimmed.starts_with('%') {
                            format.push_str(&explicit.raw);
                        } else {
                            format.push('%');
                            format.push_str(trimmed);
                            if !trimmed.chars().any(|c| c.is_ascii_alphabetic()) {
                                format.push_str(spec.trim_start_matches('%'));
                            }
                        }
                    } else {
                        format.push_str(&spec);
                    }
                }
            }
        }

        let printf_kind = match call.kind {
            IntrinsicKind::Println => IntrinsicKind::Println,
            _ => IntrinsicKind::Print,
        };
        if printf_kind == IntrinsicKind::Println {
            format.push('\n');
        }

        self.push_statement(mir::Statement {
            source_info: span,
            kind: mir::StatementKind::IntrinsicCall {
                kind: printf_kind,
                format,
                args: ordered_operands,
            },
        });
        Ok(())
    }

    pub(super) fn prepare_format_call(
        &mut self,
        call: &hir::IntrinsicCallExpr,
        span: Span,
    ) -> Result<(String, Vec<mir::Operand>)> {
        let Some((template, positional_slots, named_args, name_map)) =
            self.format_call_parts(call, span)
        else {
            return Ok((String::new(), Vec::new()));
        };

        let mut prepared_positional = Vec::with_capacity(positional_slots.len());
        for slot in positional_slots {
            if let Some(arg) = slot {
                let lowered = if let Some(formatted) =
                    self.try_format_const_expr_for_printf(&arg.value, span)
                {
                    formatted
                } else {
                    self.lower_operand(&arg.value, None)?
                };
                prepared_positional.push(Some(self.prepare_printf_arg(lowered, span)?));
            } else {
                prepared_positional.push(None);
            }
        }

        let mut prepared_named = Vec::with_capacity(named_args.len());
        for arg in named_args {
            let lowered =
                if let Some(formatted) = self.try_format_const_expr_for_printf(&arg.value, span) {
                    formatted
                } else {
                    self.lower_operand(&arg.value, None)?
                };
            prepared_named.push(self.prepare_printf_arg(lowered, span)?);
        }

        let mut format = String::new();
        let mut implicit_index = 0usize;
        let mut ordered_operands = Vec::new();

        for part in &template.parts {
            match part {
                hir::FormatTemplatePart::Literal(text) => format.push_str(text.as_str()),
                hir::FormatTemplatePart::Placeholder(placeholder) => {
                    let (prepared, missing_message) = match &placeholder.arg_ref {
                        hir::FormatArgRef::Implicit => {
                            let current = implicit_index;
                            implicit_index += 1;
                            (
                                prepared_positional.get(current).cloned().flatten(),
                                format!(
                                    "format placeholder references missing argument at index {}",
                                    current
                                ),
                            )
                        }
                        hir::FormatArgRef::Positional(index) => (
                            prepared_positional.get(*index).cloned().flatten(),
                            format!(
                                "format placeholder references missing argument at index {}",
                                index
                            ),
                        ),
                        hir::FormatArgRef::Named(name) => (
                            name_map
                                .get(name)
                                .and_then(|index| prepared_named.get(*index).cloned()),
                            format!("format placeholder references missing argument `{name}`"),
                        ),
                    };

                    let Some((operand, _ty, spec)) = prepared else {
                        self.lowering.emit_error(span, missing_message);
                        return Ok((String::new(), Vec::new()));
                    };
                    ordered_operands.push(operand);

                    if let Some(explicit) = &placeholder.format_spec {
                        let trimmed = explicit.raw.trim();
                        if trimmed.starts_with('%') {
                            format.push_str(&explicit.raw);
                        } else {
                            format.push('%');
                            format.push_str(trimmed);
                            if !trimmed.chars().any(|c| c.is_ascii_alphabetic()) {
                                format.push_str(spec.trim_start_matches('%'));
                            }
                        }
                    } else {
                        format.push_str(&spec);
                    }
                }
            }
        }

        Ok((format, ordered_operands))
    }

    pub(super) fn format_call_parts(
        &mut self,
        call: &hir::IntrinsicCallExpr,
        span: Span,
    ) -> Option<(
        hir::FormatString,
        Vec<Option<hir::CallArg>>,
        Vec<hir::CallArg>,
        HashMap<String, usize>,
    )> {
        let Some(first) = call.callargs.first() else {
            self.lowering
                .emit_error(span, "format intrinsic requires a template argument");
            return None;
        };

        let template = match &first.value.kind {
            hir::ExprKind::FormatString(template) => template.clone(),
            hir::ExprKind::Literal(hir::Lit::Str(text)) => hir::FormatString {
                parts: vec![hir::FormatTemplatePart::Literal(text.clone())],
            },
            _ => {
                self.lowering
                    .emit_error(span, "format intrinsic requires a template argument");
                return None;
            }
        };

        let mut positional_slots: Vec<Option<hir::CallArg>> = Vec::new();
        let mut named_args = Vec::new();
        for arg in &call.callargs[1..] {
            let name = arg.name.as_str();
            if let Some(index) = name.strip_prefix("arg") {
                if index.chars().all(|ch| ch.is_ascii_digit()) {
                    let idx = index.parse::<usize>().unwrap_or(0);
                    if idx == 0 {
                        named_args.push(arg.clone());
                        continue;
                    }
                    let idx = idx - 1;
                    if positional_slots.len() <= idx {
                        positional_slots.resize(idx + 1, None);
                    }
                    if positional_slots[idx].is_some() {
                        self.lowering.emit_error(
                            span,
                            format!("format argument index {idx} is provided more than once"),
                        );
                        return None;
                    }
                    positional_slots[idx] = Some(arg.clone());
                    continue;
                }
            }
            named_args.push(arg.clone());
        }

        let mut name_map = HashMap::new();
        for (offset, arg) in named_args.iter().enumerate() {
            let index = offset;
            let name = arg.name.as_str().to_string();
            if name_map.insert(name.clone(), index).is_some() {
                self.lowering.emit_error(
                    span,
                    format!("format argument '{name}' is provided more than once"),
                );
                return None;
            }
        }

        Some((template, positional_slots, named_args, name_map))
    }

    pub(super) fn emit_panic_intrinsic(
        &mut self,
        call: &hir::IntrinsicCallExpr,
        span: Span,
    ) -> Result<()> {
        let message = if call.callargs.is_empty() {
            "panic! macro triggered".to_string()
        } else if call.callargs.len() == 1 {
            match &call.callargs[0].value.kind {
                hir::ExprKind::Literal(hir::Lit::Str(text)) => text.clone(),
                hir::ExprKind::FormatString(template) => {
                    let has_placeholders = template
                        .parts
                        .iter()
                        .any(|part| matches!(part, hir::FormatTemplatePart::Placeholder(_)));
                    if has_placeholders {
                        let format_call = hir::IntrinsicCallExpr {
                            kind: fp_core::intrinsics::IntrinsicKind::Format,
                            callargs: call.callargs.clone(),
                        };
                        let (format, args) = match self.prepare_format_call(&format_call, span) {
                            Ok(value) => value,
                            Err(_) => (String::new(), Vec::new()),
                        };
                        if format.is_empty() && args.is_empty() {
                            self.lowering.emit_error(
                                span,
                                "panic format payload is not supported in compiled backends",
                            );
                            "<panic message unavailable>".to_string()
                        } else {
                            let string_ty = self.lowering.raw_string_ptr_ty();
                            let local_id = self.allocate_temp(string_ty.clone(), span);
                            let local_place = mir::Place::from_local(local_id);
                            self.push_statement(mir::Statement {
                                source_info: span,
                                kind: mir::StatementKind::Assign(
                                    local_place.clone(),
                                    mir::Rvalue::IntrinsicCall {
                                        kind: IntrinsicKind::Format,
                                        format,
                                        args,
                                    },
                                ),
                            });
                            self.locals[local_id as usize].ty = string_ty.clone();
                            let sig = mir::FunctionSig {
                                inputs: vec![string_ty.clone()],
                                output: HirToMirLowerer::unit_ty(),
                            };
                            let fn_ty = self.lowering.function_pointer_ty(&sig);
                            let func = mir::Operand::Constant(mir::Constant {
                                span,
                                ty: fn_ty.clone(),
                                user_ty: None,
                                literal: mir::ConstantKind::Fn(mir::Symbol::new(
                                    "fp_panic".to_string(),
                                )),
                            });
                            let args = vec![mir::Operand::Copy(local_place)];

                            let result_local = self.allocate_temp(HirToMirLowerer::unit_ty(), span);
                            let after_block = self.new_block();
                            let terminator = mir::Terminator {
                                source_info: span,
                                kind: mir::TerminatorKind::Call {
                                    func,
                                    args,
                                    destination: Some((
                                        mir::Place::from_local(result_local),
                                        after_block,
                                    )),
                                    cleanup: self.current_unwind_target,
                                    from_hir_call: true,
                                    fn_span: span,
                                },
                            };
                            self.blocks[self.current_block as usize].terminator = Some(terminator);

                            self.current_block = after_block;
                            self.set_current_terminator(mir::Terminator {
                                source_info: span,
                                kind: mir::TerminatorKind::Unreachable,
                            });
                            self.current_block = self.new_block();
                            return Ok(());
                        }
                    } else {
                        template
                            .parts
                            .iter()
                            .map(|part| match part {
                                hir::FormatTemplatePart::Literal(text) => text.as_str(),
                                hir::FormatTemplatePart::Placeholder(_) => "",
                            })
                            .collect::<Vec<_>>()
                            .join("")
                    }
                }
                _ => {
                    // A non-literal panic argument (e.g. `Option::expect`'s
                    // forwarded `message: &str` parameter — `panic!(message)`
                    // in `crates/fp-lang/src/std/option/mod.fp`) is a
                    // legitimate, valid program: forwarding a caller-supplied
                    // message is normal. `fp_panic`'s runtime call
                    // convention already takes a *runtime* string pointer
                    // (see the `FormatString`-with-placeholders branch
                    // above), not a compile-time constant, so there's no
                    // runtime-side reason to require a literal here either —
                    // lower the argument as a normal operand and call
                    // `fp_panic` with it directly.
                    let string_ty = self.lowering.raw_string_ptr_ty();
                    let mut message_operand =
                        self.lower_operand(&call.callargs[0].value, Some(&string_ty))?;
                    // `expected` above is only a hint — if the argument's
                    // real type is still a `&str`/slice (a fat pointer:
                    // data ptr + length), not yet the bare byte pointer
                    // `fp_panic`'s C-ABI signature requires, extract just
                    // its data-pointer field (mirrors how other C-ABI call
                    // sites in this file convert a slice argument via
                    // `lower_slice_ptr_place`).
                    if message_operand.ty != string_ty {
                        if let mir::Operand::Copy(place) | mir::Operand::Move(place) =
                            &message_operand.operand
                        {
                            let ptr_place = self.lower_slice_ptr_place(place.clone());
                            message_operand = OperandInfo {
                                operand: mir::Operand::Copy(ptr_place),
                                ty: string_ty.clone(),
                            };
                        }
                    }
                    let sig = mir::FunctionSig {
                        inputs: vec![string_ty.clone()],
                        output: HirToMirLowerer::unit_ty(),
                    };
                    let fn_ty = self.lowering.function_pointer_ty(&sig);
                    let func = mir::Operand::Constant(mir::Constant {
                        span,
                        ty: fn_ty.clone(),
                        user_ty: None,
                        literal: mir::ConstantKind::Fn(mir::Symbol::new("fp_panic".to_string())),
                    });
                    let args = vec![message_operand.operand];

                    let result_local = self.allocate_temp(HirToMirLowerer::unit_ty(), span);
                    let after_block = self.new_block();
                    let terminator = mir::Terminator {
                        source_info: span,
                        kind: mir::TerminatorKind::Call {
                            func,
                            args,
                            destination: Some((mir::Place::from_local(result_local), after_block)),
                            cleanup: self.current_unwind_target,
                            from_hir_call: true,
                            fn_span: span,
                        },
                    };
                    self.blocks[self.current_block as usize].terminator = Some(terminator);

                    self.current_block = after_block;
                    self.set_current_terminator(mir::Terminator {
                        source_info: span,
                        kind: mir::TerminatorKind::Unreachable,
                    });
                    self.current_block = self.new_block();
                    return Ok(());
                }
            }
        } else {
            self.lowering
                .emit_error(span, "panic expects zero or one argument");
            "<panic message unavailable>".to_string()
        };

        let sig = mir::FunctionSig {
            inputs: vec![self.lowering.raw_string_ptr_ty()],
            output: HirToMirLowerer::unit_ty(),
        };
        let fn_ty = self.lowering.function_pointer_ty(&sig);
        let func = mir::Operand::Constant(mir::Constant {
            span,
            ty: fn_ty.clone(),
            user_ty: None,
            literal: mir::ConstantKind::Fn(mir::Symbol::new("fp_panic".to_string())),
        });
        let args = vec![mir::Operand::Constant(mir::Constant {
            span,
            ty: self.lowering.raw_string_ptr_ty(),
            user_ty: None,
            literal: mir::ConstantKind::Str(message),
        })];

        let result_local = self.allocate_temp(HirToMirLowerer::unit_ty(), span);
        let after_block = self.new_block();
        let terminator = mir::Terminator {
            source_info: span,
            kind: mir::TerminatorKind::Call {
                func,
                args,
                destination: Some((mir::Place::from_local(result_local), after_block)),
                cleanup: self.current_unwind_target,
                from_hir_call: true,
                fn_span: span,
            },
        };
        self.blocks[self.current_block as usize].terminator = Some(terminator);

        self.current_block = after_block;
        self.set_current_terminator(mir::Terminator {
            source_info: span,
            kind: mir::TerminatorKind::Unreachable,
        });
        self.current_block = self.new_block();
        Ok(())
    }

    pub(super) fn lower_panic(&mut self, span: Span, args: &[hir::CallArg]) -> Result<()> {
        let string_ty = self.lowering.raw_string_ptr_ty();
        // Non-literal messages (e.g. a forwarded `&str` parameter) are a
        // legitimate, valid program — see the identical fallback in
        // `emit_panic_intrinsic` for the full reasoning. Lower the
        // argument as a normal operand instead of requiring a literal.
        let message_operand = match args.first() {
            Some(arg) => match &arg.value.kind {
                hir::ExprKind::Literal(hir::Lit::Str(message)) => {
                    mir::Operand::Constant(mir::Constant {
                        span,
                        ty: string_ty.clone(),
                        user_ty: None,
                        literal: mir::ConstantKind::Str(message.clone()),
                    })
                }
                _ => self.lower_operand(&arg.value, Some(&string_ty))?.operand,
            },
            None => mir::Operand::Constant(mir::Constant {
                span,
                ty: string_ty.clone(),
                user_ty: None,
                literal: mir::ConstantKind::Str("panic".to_string()),
            }),
        };

        let sig = mir::FunctionSig {
            inputs: vec![string_ty.clone()],
            output: HirToMirLowerer::unit_ty(),
        };
        let fn_ty = self.lowering.function_pointer_ty(&sig);
        let func = mir::Operand::Constant(mir::Constant {
            span,
            ty: fn_ty.clone(),
            user_ty: None,
            literal: mir::ConstantKind::Fn(mir::Symbol::new("fp_panic".to_string())),
        });
        let args = vec![message_operand];

        let result_local = self.allocate_temp(HirToMirLowerer::unit_ty(), span);
        let after_block = self.new_block();
        let terminator = mir::Terminator {
            source_info: span,
            kind: mir::TerminatorKind::Call {
                func,
                args,
                destination: Some((mir::Place::from_local(result_local), after_block)),
                cleanup: self.current_unwind_target,
                from_hir_call: true,
                fn_span: span,
            },
        };
        self.blocks[self.current_block as usize].terminator = Some(terminator);

        self.current_block = after_block;
        self.set_current_terminator(mir::Terminator {
            source_info: span,
            kind: mir::TerminatorKind::Unreachable,
        });
        self.current_block = self.new_block();
        self.control_flow_emitted = true;
        Ok(())
    }

    pub(super) fn lower_catch_unwind(
        &mut self,
        expr: &hir::Expr,
        call: &hir::IntrinsicCallExpr,
        destination: Option<mir::Place>,
    ) -> Result<OperandInfo> {
        let args = &call.callargs;
        let arg_values: Vec<&hir::Expr> = args.iter().map(|arg| &arg.value).collect();

        if args.len() != 1 {
            self.lowering.emit_error(
                expr.span,
                "catch_unwind expects exactly one callable argument",
            );
            return Ok(self.constant_bool_operand(false, expr.span));
        }

        let callee = arg_values[0];
        let mut call_args: Vec<mir::Operand> = Vec::new();
        let (func, sig, _name) = if let hir::ExprKind::Struct(path, _) = &callee.kind {
            let struct_name = path.segments.last().map(|seg| seg.name.as_str());
            let closure_suffix = struct_name.and_then(|name| name.strip_prefix("__Closure"));
            if let Some(suffix) = closure_suffix {
                let env = self.lower_operand(callee, None)?;
                let call_name = format!("__closure{}_call", suffix);
                let path = hir::Path {
                    segments: vec![hir::PathSegment {
                        name: hir::Symbol::new(call_name),
                        args: None,
                    }],
                    res: None,
                };
                let call_expr = hir::Expr {
                    hir_id: expr.hir_id.clone(),
                    kind: hir::ExprKind::Path(path),
                    span: expr.span,
                };
                call_args.push(env.operand);
                self.resolve_callee(&call_expr)?
            } else {
                self.resolve_callee(callee)?
            }
        } else {
            self.resolve_callee(callee)?
        };
        if call_args.is_empty() {
            if !sig.inputs.is_empty() {
                self.lowering.emit_error(
                    expr.span,
                    "catch_unwind only supports zero-argument callables",
                );
            }
        } else if sig.inputs.len() != call_args.len() {
            self.lowering.emit_error(
                expr.span,
                "catch_unwind closure must not take user arguments",
            );
        }
        if !HirToMirLowerer::is_unit_ty(&sig.output) {
            self.lowering.emit_error(
                expr.span,
                "catch_unwind only supports callables that return unit",
            );
        }

        let result_ty = Ty { kind: TyKind::Bool };
        let result_place = destination.unwrap_or_else(|| {
            let local_id = self.allocate_temp(result_ty.clone(), expr.span);
            mir::Place::from_local(local_id)
        });
        if (result_place.local as usize) < self.locals.len() {
            self.locals[result_place.local as usize].ty = result_ty.clone();
        }

        let call_result_local = self.allocate_temp(sig.output.clone(), expr.span);
        let call_result_place = mir::Place::from_local(call_result_local);

        let ok_block = self.new_block();
        let unwind_block = self.new_block();
        if let Some(block) = self.blocks.get_mut(unwind_block as usize) {
            block.is_cleanup = true;
        }
        let join_block = self.new_block();

        let terminator = mir::Terminator {
            source_info: expr.span,
            kind: mir::TerminatorKind::Call {
                func,
                args: call_args,
                destination: Some((call_result_place, ok_block)),
                cleanup: Some(unwind_block),
                from_hir_call: true,
                fn_span: expr.span,
            },
        };
        self.blocks[self.current_block as usize].terminator = Some(terminator);

        self.current_block = ok_block;
        self.push_statement(mir::Statement {
            source_info: expr.span,
            kind: mir::StatementKind::Assign(
                result_place.clone(),
                mir::Rvalue::Use(mir::Operand::Constant(mir::Constant {
                    span: expr.span,
                    ty: Ty { kind: TyKind::Bool },
                    user_ty: None,
                    literal: mir::ConstantKind::Bool(true),
                })),
            ),
        });
        self.set_current_terminator(mir::Terminator {
            source_info: expr.span,
            kind: mir::TerminatorKind::Goto { target: join_block },
        });

        self.current_block = unwind_block;
        self.push_statement(mir::Statement {
            source_info: expr.span,
            kind: mir::StatementKind::Assign(
                result_place.clone(),
                mir::Rvalue::Use(mir::Operand::Constant(mir::Constant {
                    span: expr.span,
                    ty: Ty { kind: TyKind::Bool },
                    user_ty: None,
                    literal: mir::ConstantKind::Bool(false),
                })),
            ),
        });
        self.set_current_terminator(mir::Terminator {
            source_info: expr.span,
            kind: mir::TerminatorKind::Goto { target: join_block },
        });

        self.current_block = join_block;
        Ok(OperandInfo {
            operand: mir::Operand::copy(result_place),
            ty: result_ty,
        })
    }

    pub(super) fn lower_catch_unwind_result(
        &mut self,
        expr: &hir::Expr,
        call: &hir::IntrinsicCallExpr,
        destination: Option<mir::Place>,
    ) -> Result<OperandInfo> {
        let args = &call.callargs;
        let arg_values: Vec<&hir::Expr> = args.iter().map(|arg| &arg.value).collect();

        if args.len() != 1 {
            self.lowering.emit_error(
                expr.span,
                "catch_unwind_result expects exactly one callable argument",
            );
            return Ok(self.constant_bool_operand(false, expr.span));
        }

        let callee = arg_values[0];
        let mut call_args: Vec<mir::Operand> = Vec::new();
        let (func, sig, _name) = if let hir::ExprKind::Struct(path, _) = &callee.kind {
            let struct_name = path.segments.last().map(|seg| seg.name.as_str());
            let closure_suffix = struct_name.and_then(|name| name.strip_prefix("__Closure"));
            if let Some(suffix) = closure_suffix {
                let env = self.lower_operand(callee, None)?;
                let call_name = format!("__closure{}_call", suffix);
                let path = hir::Path {
                    segments: vec![hir::PathSegment {
                        name: hir::Symbol::new(call_name),
                        args: None,
                    }],
                    res: None,
                };
                let call_expr = hir::Expr {
                    hir_id: expr.hir_id.clone(),
                    kind: hir::ExprKind::Path(path),
                    span: expr.span,
                };
                call_args.push(env.operand);
                self.resolve_callee(&call_expr)?
            } else {
                self.resolve_callee(callee)?
            }
        } else {
            self.resolve_callee(callee)?
        };
        match (call_args.is_empty(), sig.inputs.len(), call_args.len()) {
            (true, 0, _) => {}
            (true, _, _) => {
                self.lowering.emit_error(
                    expr.span,
                    "catch_unwind_result only supports zero-argument callables",
                );
            }
            (false, expected, actual) if expected != actual => {
                self.lowering.emit_error(
                    expr.span,
                    "catch_unwind_result closure must not take user arguments",
                );
            }
            (false, _, _) => {}
        }

        let result_ty = Ty {
            kind: TyKind::Tuple(vec![
                Box::new(Ty { kind: TyKind::Bool }),
                Box::new(sig.output.clone()),
            ]),
        };
        let result_place = destination.unwrap_or_else(|| {
            let local_id = self.allocate_temp(result_ty.clone(), expr.span);
            mir::Place::from_local(local_id)
        });
        if (result_place.local as usize) < self.locals.len() {
            self.locals[result_place.local as usize].ty = result_ty.clone();
        }

        let call_result_local = self.allocate_temp(sig.output.clone(), expr.span);
        let call_result_place = mir::Place::from_local(call_result_local);

        let ok_block = self.new_block();
        let unwind_block = self.new_block();
        if let Some(block) = self.blocks.get_mut(unwind_block as usize) {
            block.is_cleanup = true;
        }
        let join_block = self.new_block();

        let terminator = mir::Terminator {
            source_info: expr.span,
            kind: mir::TerminatorKind::Call {
                func,
                args: call_args,
                destination: Some((call_result_place.clone(), ok_block)),
                cleanup: Some(unwind_block),
                from_hir_call: true,
                fn_span: expr.span,
            },
        };
        self.blocks[self.current_block as usize].terminator = Some(terminator);

        self.current_block = ok_block;
        self.push_statement(mir::Statement {
            source_info: expr.span,
            kind: mir::StatementKind::Assign(
                result_place.clone(),
                mir::Rvalue::Aggregate(
                    mir::AggregateKind::Tuple,
                    vec![
                        mir::Operand::Constant(mir::Constant {
                            span: expr.span,
                            ty: Ty { kind: TyKind::Bool },
                            user_ty: None,
                            literal: mir::ConstantKind::Bool(true),
                        }),
                        mir::Operand::Copy(call_result_place),
                    ],
                ),
            ),
        });
        self.set_current_terminator(mir::Terminator {
            source_info: expr.span,
            kind: mir::TerminatorKind::Goto { target: join_block },
        });

        self.current_block = unwind_block;
        let unwind_default = self
            .lowering
            .catch_unwind_default_constant_for_ty(&sig.output)?;
        self.push_statement(mir::Statement {
            source_info: expr.span,
            kind: mir::StatementKind::Assign(
                result_place.clone(),
                mir::Rvalue::Aggregate(
                    mir::AggregateKind::Tuple,
                    vec![
                        mir::Operand::Constant(mir::Constant {
                            span: expr.span,
                            ty: sig.output.clone(),
                            user_ty: None,
                            literal: mir::ConstantKind::Bool(false),
                        }),
                        mir::Operand::Constant(mir::Constant {
                            span: expr.span,
                            ty: sig.output.clone(),
                            user_ty: None,
                            literal: unwind_default,
                        }),
                    ],
                ),
            ),
        });
        self.set_current_terminator(mir::Terminator {
            source_info: expr.span,
            kind: mir::TerminatorKind::Goto { target: join_block },
        });

        self.current_block = join_block;
        Ok(OperandInfo {
            operand: mir::Operand::copy(result_place),
            ty: result_ty,
        })
    }

    pub(super) fn prepare_printf_arg(
        &mut self,
        arg: OperandInfo,
        span: Span,
    ) -> Result<(mir::Operand, Ty, String)> {
        let (operand, ty) = (arg.operand, arg.ty);
        if let mir::Operand::Constant(constant) = &operand {
            if matches!(constant.literal, mir::ConstantKind::Null) {
                return Ok((
                    mir::Operand::Constant(mir::Constant {
                        span,
                        ty: self.lowering.raw_string_ptr_ty(),
                        user_ty: None,
                        literal: mir::ConstantKind::Str("null".to_string()),
                    }),
                    self.lowering.raw_string_ptr_ty(),
                    "%s".to_string(),
                ));
            }
        }
        if let mir::Operand::Copy(place) | mir::Operand::Move(place) = &operand {
            if place.projection.is_empty() && self.null_locals.contains(&place.local) {
                return Ok((
                    mir::Operand::Constant(mir::Constant {
                        span,
                        ty: self.lowering.raw_string_ptr_ty(),
                        user_ty: None,
                        literal: mir::ConstantKind::Str("null".to_string()),
                    }),
                    self.lowering.raw_string_ptr_ty(),
                    "%s".to_string(),
                ));
            }
        }
        match &ty.kind {
            TyKind::Bool => Ok((operand, ty.clone(), "%d".to_string())),
            TyKind::Char => Ok((operand, ty.clone(), "%c".to_string())),
            TyKind::Int(int_ty) => Ok((
                operand,
                ty.clone(),
                match int_ty {
                    IntTy::I8 => "%hhd",
                    IntTy::I16 => "%hd",
                    IntTy::I32 => "%d",
                    IntTy::I64 => "%lld",
                    IntTy::I128 => "%lld",
                    IntTy::Isize => "%lld",
                }
                .to_string(),
            )),
            TyKind::Uint(uint_ty) => Ok((
                operand,
                ty.clone(),
                match uint_ty {
                    UintTy::U8 => "%hhu",
                    UintTy::U16 => "%hu",
                    UintTy::U32 => "%u",
                    UintTy::U64 => "%llu",
                    UintTy::U128 => "%llu",
                    UintTy::Usize => "%llu",
                }
                .to_string(),
            )),
            TyKind::Float(_) => Ok((operand, ty.clone(), "%f".to_string())),
            TyKind::RawPtr(type_and_mut) => {
                if self.is_c_string_ptr(type_and_mut.ty.as_ref()) {
                    Ok((operand, ty.clone(), "%s".to_string()))
                } else {
                    let spec = self.printf_spec_for_ty(&ty, span)?;
                    Ok((operand, ty.clone(), spec))
                }
            }
            TyKind::Slice(elem) => {
                if self.is_c_string_ptr(elem.as_ref()) {
                    let ptr_ty = self.lowering.raw_string_ptr_ty();
                    let ptr_operand = match operand {
                        mir::Operand::Constant(constant)
                            if matches!(constant.literal, mir::ConstantKind::Str(_)) =>
                        {
                            mir::Operand::Constant(mir::Constant {
                                span: constant.span,
                                ty: ptr_ty.clone(),
                                user_ty: constant.user_ty,
                                literal: constant.literal,
                            })
                        }
                        mir::Operand::Copy(place) | mir::Operand::Move(place) => {
                            let mut ptr_place = place;
                            ptr_place
                                .projection
                                .push(mir::PlaceElem::Field(0, ptr_ty.clone()));
                            mir::Operand::Copy(ptr_place)
                        }
                        operand => {
                            let local = self.allocate_temp(ty.clone(), span);
                            let place = mir::Place::from_local(local);
                            self.push_statement(mir::Statement {
                                source_info: span,
                                kind: mir::StatementKind::Assign(
                                    place.clone(),
                                    mir::Rvalue::Use(operand),
                                ),
                            });
                            let mut ptr_place = place;
                            ptr_place
                                .projection
                                .push(mir::PlaceElem::Field(0, ptr_ty.clone()));
                            mir::Operand::Copy(ptr_place)
                        }
                    };
                    Ok((ptr_operand, ptr_ty, "%s".to_string()))
                } else {
                    self.lowering
                        .emit_warning(span, "printf using %p for non-string slice argument");
                    Ok((operand, ty.clone(), "%p".to_string()))
                }
            }
            TyKind::Tuple(elements) if elements.is_empty() => Ok((
                mir::Operand::Constant(mir::Constant {
                    span,
                    ty: self.lowering.raw_string_ptr_ty(),
                    user_ty: None,
                    literal: mir::ConstantKind::Str("()".to_string()),
                }),
                self.lowering.raw_string_ptr_ty(),
                "%s".to_string(),
            )),
            TyKind::Tuple(_) | TyKind::Array(_, _) | TyKind::Adt(_, _) => {
                if let Some((string_operand, string_ty)) =
                    self.format_const_operand_for_printf(&operand, span)
                {
                    return Ok((string_operand, string_ty, "%s".to_string()));
                }
                self.lowering.emit_warning(
                    span,
                    "printf lowering tuple/array/struct argument as opaque pointer",
                );
                Ok((operand, ty.clone(), "%p".to_string()))
            }
            TyKind::Ref(_, inner, _) => {
                if let TyKind::RawPtr(type_and_mut) = &inner.kind {
                    if self.is_c_string_ptr(type_and_mut.ty.as_ref()) {
                        let place = match operand {
                            mir::Operand::Copy(place) | mir::Operand::Move(place) => place,
                            _ => {
                                self.lowering.emit_error(
                                    span,
                                    "printf cannot dereference non-place arguments",
                                );
                                return Ok((operand, ty.clone(), "%s".to_string()));
                            }
                        };
                        let mut deref_place = place.clone();
                        deref_place.projection.push(mir::PlaceElem::Deref);
                        return Ok((
                            mir::Operand::Copy(deref_place),
                            (*inner.as_ref()).clone(),
                            "%s".to_string(),
                        ));
                    }
                }
                if let TyKind::Slice(elem) = &inner.kind {
                    if self.is_c_string_ptr(elem.as_ref()) {
                        let place = match operand {
                            mir::Operand::Copy(place) | mir::Operand::Move(place) => place,
                            _ => {
                                self.lowering.emit_error(
                                    span,
                                    "printf cannot dereference non-place arguments",
                                );
                                return Ok((operand, ty.clone(), "%s".to_string()));
                            }
                        };
                        let mut deref_place = place.clone();
                        deref_place.projection.push(mir::PlaceElem::Deref);
                        return Ok((
                            mir::Operand::Copy(deref_place),
                            (*inner.as_ref()).clone(),
                            "%s".to_string(),
                        ));
                    }
                }
                if self.is_c_string_ptr(inner.as_ref()) {
                    return Ok((operand, ty.clone(), "%s".to_string()));
                }
                let spec = self.printf_spec_for_ty(&ty, span)?;
                Ok((operand, ty.clone(), spec))
            }
            _ => {
                if let Some((string_operand, string_ty)) =
                    self.format_const_operand_for_printf(&operand, span)
                {
                    return Ok((string_operand, string_ty, "%s".to_string()));
                }
                if self.lowering.is_opaque_ty(&ty) {
                    return Ok((operand, ty.clone(), "%p".to_string()));
                }
                let ty_name = self
                    .lowering
                    .display_type_name(&ty)
                    .unwrap_or_else(|| format!("{:?}", ty.kind));
                self.lowering.emit_warning(
                    span,
                    format!(
                        "printf argument type is not supported: {}; using %p",
                        ty_name
                    ),
                );
                Ok((operand, ty.clone(), "%p".to_string()))
            }
        }
    }

    pub(super) fn format_const_operand_for_printf(
        &mut self,
        operand: &mir::Operand,
        span: Span,
    ) -> Option<(mir::Operand, Ty)> {
        let mir::Operand::Constant(constant) = operand else {
            return None;
        };
        let mir::ConstantKind::Val(value) = &constant.literal else {
            return None;
        };
        let ast_value = self.const_value_to_ast_value(value)?;
        let formatted = match format_value_with_spec(&ast_value, None) {
            Ok(text) => text,
            Err(err) => {
                self.lowering.emit_error(
                    span,
                    format!("failed to format const value for printf: {}", err),
                );
                return None;
            }
        };
        let ty = Ty {
            kind: TyKind::RawPtr(TypeAndMut {
                ty: Box::new(Ty {
                    kind: TyKind::Int(IntTy::I8),
                }),
                mutbl: Mutability::Not,
            }),
        };
        let constant = mir::Constant {
            span,
            ty: ty.clone(),
            user_ty: None,
            literal: mir::ConstantKind::Str(formatted),
        };
        Some((mir::Operand::Constant(constant), ty))
    }

    pub(super) fn try_format_const_expr_for_printf(
        &mut self,
        expr: &hir::Expr,
        span: Span,
    ) -> Option<OperandInfo> {
        let hir::ExprKind::Path(path) = &expr.kind else {
            return None;
        };
        let Some(hir::Res::Def(def_id)) = &path.res else {
            return None;
        };
        let const_info = self.lowering.ensure_const_info(def_id.clone())?;
        let mir::ConstantKind::Val(value) = &const_info.value.literal else {
            return None;
        };
        let value = value.clone();
        if !matches!(
            value,
            mir::ConstValue::Array(_)
                | mir::ConstValue::List { .. }
                | mir::ConstValue::Map { .. }
                | mir::ConstValue::Tuple(_)
                | mir::ConstValue::Struct(_)
        ) {
            return None;
        }
        let ast_value = self.const_value_to_ast_value(&value)?;
        let formatted = match format_value_with_spec(&ast_value, None) {
            Ok(text) => text,
            Err(err) => {
                self.lowering.emit_error(
                    span,
                    format!("failed to format const value for printf: {}", err),
                );
                return None;
            }
        };
        let ty = Ty {
            kind: TyKind::RawPtr(TypeAndMut {
                ty: Box::new(Ty {
                    kind: TyKind::Int(IntTy::I8),
                }),
                mutbl: Mutability::Not,
            }),
        };
        Some(OperandInfo::constant(
            span,
            ty,
            mir::ConstantKind::Str(formatted),
        ))
    }

    pub(super) fn const_value_to_ast_value(&mut self, value: &mir::ConstValue) -> Option<Value> {
        match value {
            mir::ConstValue::Unit => Some(Value::unit()),
            mir::ConstValue::Bool(value) => Some(Value::bool(*value)),
            mir::ConstValue::Int(value) => Some(Value::int(*value)),
            mir::ConstValue::UInt(value) => Some(Value::int(*value as i64)),
            mir::ConstValue::Float(value) => Some(Value::decimal(*value)),
            mir::ConstValue::Str(value) => Some(Value::string(value.clone())),
            mir::ConstValue::Null => Some(Value::null()),
            mir::ConstValue::Fn(_) => None,
            mir::ConstValue::Tuple(values) | mir::ConstValue::Struct(values) => {
                let mut elements = Vec::with_capacity(values.len());
                for element in values {
                    elements.push(self.const_value_to_ast_value(element)?);
                }
                Some(Value::Tuple(ValueTuple::new(elements)))
            }
            mir::ConstValue::Array(values) => {
                let mut elements = Vec::with_capacity(values.len());
                for element in values {
                    elements.push(self.const_value_to_ast_value(element)?);
                }
                Some(Value::List(ValueList::new(elements)))
            }
            mir::ConstValue::List { elements, .. } => {
                let mut items = Vec::with_capacity(elements.len());
                for element in elements {
                    items.push(self.const_value_to_ast_value(element)?);
                }
                Some(Value::List(ValueList::new(items)))
            }
            mir::ConstValue::Map { entries, .. } => {
                let mut items = Vec::with_capacity(entries.len());
                for (key, value) in entries {
                    let key_value = self.const_value_to_ast_value(key)?;
                    let value_value = self.const_value_to_ast_value(value)?;
                    items.push((key_value, value_value));
                }
                // `entries` is already a valid runtime map's contents (from
                // `mir::ConstValue::Map`), so keys are already guaranteed
                // unique — skip `from_pairs`'s per-key duplicate scan.
                Some(Value::Map(ValueMap::from_unique_pairs(items)))
            }
        }
    }

    pub(super) fn printf_spec_for_ty(&mut self, ty: &Ty, span: Span) -> Result<String> {
        let spec = match &ty.kind {
            TyKind::Bool => "%d",
            TyKind::Char => "%c",
            TyKind::Int(int_ty) => match int_ty {
                IntTy::I8 => "%hhd",
                IntTy::I16 => "%hd",
                IntTy::I32 => "%d",
                IntTy::I64 => "%lld",
                IntTy::I128 => "%lld",
                IntTy::Isize => "%lld",
            },
            TyKind::Uint(uint_ty) => match uint_ty {
                UintTy::U8 => "%hhu",
                UintTy::U16 => "%hu",
                UintTy::U32 => "%u",
                UintTy::U64 => "%llu",
                UintTy::U128 => "%llu",
                UintTy::Usize => "%llu",
            },
            TyKind::Float(_) => "%f",
            TyKind::RawPtr(type_and_mut) => {
                if self.is_c_string_ptr(type_and_mut.ty.as_ref()) {
                    "%s"
                } else {
                    self.lowering
                        .emit_warning(span, "printf using %p for non-string raw pointer argument");
                    "%p"
                }
            }
            TyKind::Ref(_, _, _) => {
                self.lowering
                    .emit_warning(span, "printf using %p for non-string reference argument");
                "%p"
            }
            _ => {
                if self.lowering.is_opaque_ty(ty) {
                    "%p"
                } else {
                    self.lowering
                        .emit_warning(span, "printf argument type is not supported; using %p");
                    "%p"
                }
            }
        };
        Ok(spec.to_string())
    }

    pub(super) fn is_c_string_ptr(&self, ty: &Ty) -> bool {
        matches!(ty.kind, TyKind::Int(IntTy::I8) | TyKind::Uint(UintTy::U8))
    }

    pub(super) fn resolve_struct_ref(&mut self, expr: &hir::Expr) -> Option<StructRef> {
        let hir::ExprKind::Path(path) = &expr.kind else {
            return None;
        };

        let args = path
            .segments
            .iter()
            .find_map(|segment| segment.args.as_ref())
            .map(|args| self.lowering.lower_generic_args(Some(args), expr.span))
            .unwrap_or_default();

        if let Some(hir::Res::Def(def_id)) = &path.res {
            return Some(StructRef {
                def_id: def_id.clone(),
                args,
            });
        }

        if let Some(segment) = path.segments.last() {
            let name = segment.name.as_str();
            let mut matches = self
                .lowering
                .mir_package
                .borrow()
                .struct_defs
                .iter()
                .filter_map(|(def_id, info)| (info.name == name).then_some(def_id.clone()))
                .collect::<Vec<_>>();
            if matches.len() == 1 {
                return Some(StructRef {
                    def_id: matches.pop()?,
                    args,
                });
            }
        }

        None
    }

    pub(super) fn compute_struct_size(
        &mut self,
        span: Span,
        struct_ref: &StructRef,
    ) -> Option<u64> {
        let layout = match self.lowering.struct_layout_for_instance(
            struct_ref.def_id.clone(),
            &struct_ref.args,
            span,
        ) {
            Some(layout) => layout,
            None => return None,
        };

        let mut total = 0u64;
        for field_ty in layout.field_tys {
            let field_size = match self.compute_ty_size(span, &field_ty) {
                Some(size) => size,
                None => return None,
            };
            total = total.saturating_add(field_size);
        }
        Some(total)
    }

    pub(super) fn compute_ty_size(&mut self, span: Span, ty: &Ty) -> Option<u64> {
        match &ty.kind {
            TyKind::Bool => Some(1),
            TyKind::Char => Some(4),
            TyKind::Int(int_ty) => Some(match int_ty {
                IntTy::I8 => 1,
                IntTy::I16 => 2,
                IntTy::I32 => 4,
                IntTy::I64 => 8,
                IntTy::I128 => 16,
                IntTy::Isize => 8,
            }),
            TyKind::Uint(uint_ty) => Some(match uint_ty {
                UintTy::U8 => 1,
                UintTy::U16 => 2,
                UintTy::U32 => 4,
                UintTy::U64 => 8,
                UintTy::U128 => 16,
                UintTy::Usize => 8,
            }),
            TyKind::Float(float_ty) => Some(match float_ty {
                FloatTy::F16 => 2,
                FloatTy::F32 => 4,
                FloatTy::F64 => 8,
                FloatTy::F128 => 16,
            }),
            TyKind::Tuple(elements) => {
                let mut total = 0u64;
                for elem in elements {
                    let size = match self.compute_ty_size(span, elem) {
                        Some(value) => value,
                        None => return None,
                    };
                    total = total.saturating_add(size);
                }
                Some(total)
            }
            TyKind::Array(elem_ty, len) => {
                let len = match self.const_kind_to_u64(span, len) {
                    Some(value) => value,
                    None => return None,
                };
                let elem_size = match self.compute_ty_size(span, elem_ty) {
                    Some(value) => value,
                    None => return None,
                };
                Some(elem_size.saturating_mul(len))
            }
            TyKind::Ref(_, _, _) | TyKind::RawPtr(_) | TyKind::FnPtr(_) | TyKind::FnDef(_, _) => {
                Some(8)
            }
            TyKind::Never => Some(0),
            TyKind::Error(_) => None,
            TyKind::Slice(_) => {
                // Slices are fat pointers (data + length).
                Some(16)
            }
            TyKind::Adt(_, _)
            | TyKind::Dynamic(_, _)
            | TyKind::Closure(_, _)
            | TyKind::Generator(_, _, _)
            | TyKind::GeneratorWitness(_)
            | TyKind::Projection(_)
            | TyKind::Opaque(_, _)
            | TyKind::Param(_)
            | TyKind::Placeholder(_)
            | TyKind::Bound(_, _)
            | TyKind::Infer(_)
            | TyKind::Type
            | TyKind::Any => {
                if let TyKind::Adt(adt, substs) = &ty.kind {
                    // A payload slot opaqued out by `enum_layout_for_
                    // instance` (heterogeneous per-variant types sharing a
                    // slot) has no fields to size structurally — its size
                    // was already computed there as the max over every
                    // contributing variant's own type at that slot.
                    if let Some(size) = self.lowering.display_type_name(ty).and_then(|name| {
                        self.lowering
                            .mir_package
                            .borrow()
                            .opaque_ty_sizes
                            .get(&name)
                            .copied()
                    }) {
                        return Some(size);
                    }
                    let args: Vec<Ty> = substs
                        .iter()
                        .filter_map(|arg| match arg {
                            mir::ty::GenericArg::Type(inner) => Some(inner.clone()),
                            _ => None,
                        })
                        .collect();
                    // `struct_layout_for_ty` is a cache-only reverse lookup
                    // (`&self`, can't trigger computation) — if nothing has
                    // needed this struct's layout yet (e.g. `sizeof!(T)` is
                    // the *first* thing to ask for `String`'s size while
                    // specializing `Vec<String>::push`), it simply misses.
                    // Fall back to `struct_layout_for_instance`, which
                    // computes and caches the layout on demand from the
                    // struct's own `DefId` + concrete generic args, exactly
                    // as a struct-literal use of this same type would.
                    if self
                        .lowering
                        .mir_package
                        .borrow()
                        .struct_defs
                        .contains_key(&adt.did)
                    {
                        let layout = self.lowering.struct_layout_for_ty(ty).or_else(|| {
                            self.lowering
                                .struct_layout_for_instance(adt.did.clone(), &args, span)
                        });
                        if let Some(layout) = layout {
                            let mut total = 0u64;
                            for field in &layout.field_tys {
                                let size = match self.compute_ty_size(span, field) {
                                    Some(value) => value,
                                    None => return None,
                                };
                                total = total.saturating_add(size);
                            }
                            return Some(total);
                        }
                    }
                    // Enums are nominal (`TyKind::Adt`) now too, but their
                    // actual byte layout is still the flattened
                    // `tag + payload...` shape computed by
                    // `enum_layout_for_instance` — mirror that shape's own
                    // size (tag plus every payload slot) rather than trying
                    // `struct_layout_for_instance` against an enum `DefId`.
                    if self
                        .lowering
                        .mir_package
                        .borrow()
                        .enum_defs
                        .contains_key(&adt.did)
                    {
                        if let Some(layout) =
                            self.lowering
                                .enum_layout_for_instance(adt.did.clone(), &args, span)
                        {
                            let mut total = self.compute_ty_size(span, &layout.tag_ty)?;
                            for payload in &layout.payload_tys {
                                let size = self.compute_ty_size(span, payload)?;
                                total = total.saturating_add(size);
                            }
                            return Some(total);
                        }
                    }
                }
                // `sizeof!(T)` called on a function/method's own generic type
                // parameter (e.g. inside `impl<T> Vec<T> { fn push(&mut self,
                // value: T) { ... sizeof!(T) ... } }`) — `T` isn't a concrete
                // type in general, but `self.type_substs` (populated per
                // specialization by the same mechanism
                // `payload_types_from_type_substs` already reads for enum
                // payloads) holds the concrete substitution for *this*
                // specialization. Resolve and recurse before giving up.
                if let TyKind::Param(param) = &ty.kind {
                    if let Some(resolved) = self.type_substs.get(param.name.as_str()).cloned() {
                        // Guard against a self-referential/unresolved
                        // substitution (`type_substs["T"]` itself being
                        // `Param("T")`, e.g. when specialization couldn't
                        // infer a concrete type and left an identity
                        // placeholder) — recursing on that would loop
                        // forever instead of erroring.
                        let made_progress = !matches!(
                            &resolved.kind,
                            TyKind::Param(resolved_param) if resolved_param.name == param.name
                        );
                        if made_progress {
                            return self.compute_ty_size(span, &resolved);
                        }
                    }
                }
                self.lowering.emit_error(
                    span,
                    format!("size_of for type `{:?}` is not supported", ty.kind),
                );
                None
            }
        }
    }

    pub(super) fn const_kind_to_u64(&mut self, span: Span, konst: &ConstKind) -> Option<u64> {
        match konst {
            ConstKind::Value(ConstValue::Scalar(Scalar::Int(int))) => Some(int.data as u64),
            ConstKind::Value(ConstValue::Scalar(Scalar::Ptr(_))) => {
                self.lowering.emit_warning(
                    span,
                    "array length uses a pointer value; treating length as zero",
                );
                Some(0)
            }
            ConstKind::Value(ConstValue::ZeroSized) => Some(0),
            _ => {
                self.lowering
                    .emit_error(span, "array length is not a compile-time integer constant");
                None
            }
        }
    }

    pub(super) fn expect_string_literal(&mut self, expr: &hir::Expr, span: Span) -> Option<String> {
        match &expr.kind {
            hir::ExprKind::Literal(hir::Lit::Str(value)) => Some(value.clone()),
            _ => {
                self.lowering
                    .emit_error(span, "intrinsic argument must be a string literal");
                None
            }
        }
    }
}
