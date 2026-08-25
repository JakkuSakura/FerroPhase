use super::body::BodyBuilder;
use fp_core::error::Result;
use fp_core::hir;
use fp_core::mir;
use fp_core::mir::ty::{IntTy, Ty, TyKind, UintTy};
use fp_core::span::Span;

impl<'a> BodyBuilder<'a> {
    pub(super) fn emit_c_call(
        &mut self,
        name: &str,
        sig: mir::FunctionSig,
        args: Vec<mir::Operand>,
        destination: mir::Place,
        span: Span,
    ) -> Result<()> {
        let continue_block = self.new_block();

        let fn_ty = self.lowering.c_function_pointer_ty(&sig);
        let func_operand = mir::Operand::Constant(mir::Constant {
            span,
            ty: fn_ty.clone(),
            user_ty: None,
            literal: mir::ConstantKind::Fn(mir::Symbol::from(name.to_string())),
        });

        self.blocks[self.current_block as usize].terminator = Some(mir::Terminator {
            source_info: span,
            kind: mir::TerminatorKind::Call {
                func: func_operand,
                args,
                destination: Some((destination.clone(), continue_block)),
                cleanup: self.current_unwind_target,
                from_hir_call: false,
                fn_span: span,
            },
        });

        self.current_block = continue_block;
        Ok(())
    }

    pub(super) fn lower_path_inner_str(&mut self, path_expr: &hir::Expr) -> Result<mir::Place> {
        // std::path::Path { inner: str }
        let path_place = if let Some(place_info) = self.lower_place(path_expr)? {
            place_info.place
        } else {
            let lowered = self.lower_operand(path_expr, None)?;
            match lowered.operand {
                mir::Operand::Copy(place) | mir::Operand::Move(place) => place,
                other => {
                    let local_id = self.allocate_temp(lowered.ty.clone(), path_expr.span);
                    let temp_place = mir::Place::from_local(local_id);
                    self.push_statement(mir::Statement {
                        source_info: path_expr.span,
                        kind: mir::StatementKind::Assign(
                            temp_place.clone(),
                            mir::Rvalue::Use(other),
                        ),
                    });
                    temp_place
                }
            }
        };

        let str_ty = Ty {
            kind: TyKind::Slice(Box::new(Ty {
                kind: TyKind::Int(IntTy::I8),
            })),
        };

        Ok(mir::Place {
            local: path_place.local,
            projection: path_place
                .projection
                .into_iter()
                .chain([mir::PlaceElem::Deref, mir::PlaceElem::Field(0, str_ty)])
                .collect(),
        })
    }

    pub(super) fn lower_slice_ptr_place(&self, slice_place: mir::Place) -> mir::Place {
        let elem_ty = self.lowering.raw_string_ptr_ty();
        mir::Place {
            local: slice_place.local,
            projection: slice_place
                .projection
                .into_iter()
                .chain([mir::PlaceElem::Field(0, elem_ty)])
                .collect(),
        }
    }

    pub(super) fn lower_slice_len_place(&self, slice_place: mir::Place) -> mir::Place {
        let len_ty = Ty {
            kind: TyKind::Int(IntTy::I64),
        };
        mir::Place {
            local: slice_place.local,
            projection: slice_place
                .projection
                .into_iter()
                .chain([mir::PlaceElem::Field(1, len_ty)])
                .collect(),
        }
    }

    pub(super) fn lower_env_var_exists_into_place(
        &mut self,
        expr: &hir::Expr,
        call: &hir::IntrinsicCallExpr,
        place: mir::Place,
        expected_ty: &Ty,
    ) -> Result<()> {
        let args = &call.callargs;
        if args.len() != 1 {
            self.lowering
                .emit_error(expr.span, "env::exists intrinsic expects one name argument");
        }

        let name_ty = Ty {
            kind: TyKind::Slice(Box::new(Ty {
                kind: TyKind::Int(IntTy::I8),
            })),
        };
        let name_info = args
            .get(0)
            .map(|arg| self.lower_operand(&arg.value, Some(&name_ty)))
            .transpose()?;

        let name_place = if let Some(info) = &name_info {
            if let mir::Operand::Copy(place) | mir::Operand::Move(place) = &info.operand {
                place.clone()
            } else {
                let local_id = self.allocate_temp(name_ty.clone(), expr.span);
                let local_place = mir::Place::from_local(local_id);
                self.push_statement(mir::Statement {
                    source_info: expr.span,
                    kind: mir::StatementKind::Assign(
                        local_place.clone(),
                        mir::Rvalue::Use(info.operand.clone()),
                    ),
                });
                local_place
            }
        } else {
            let local_id = self.allocate_temp(name_ty.clone(), expr.span);
            mir::Place::from_local(local_id)
        };

        let name_ptr_place = self.lower_slice_ptr_place(name_place);
        let name_ptr_op = mir::Operand::copy(name_ptr_place);

        let getenv_ret_ty = self.lowering.raw_string_ptr_ty();
        let getenv_local = self.allocate_temp(getenv_ret_ty.clone(), expr.span);
        let getenv_place = mir::Place::from_local(getenv_local);

        self.emit_c_call(
            "getenv",
            mir::FunctionSig {
                inputs: vec![getenv_ret_ty.clone()],
                output: getenv_ret_ty.clone(),
            },
            vec![name_ptr_op],
            getenv_place.clone(),
            expr.span,
        )?;

        let is_null_local = self.allocate_temp(Ty { kind: TyKind::Bool }, expr.span);
        let is_null_place = mir::Place::from_local(is_null_local);
        let null_const = mir::Operand::Constant(mir::Constant {
            span: expr.span,
            ty: getenv_ret_ty,
            user_ty: None,
            literal: mir::ConstantKind::Null,
        });
        self.push_statement(mir::Statement {
            source_info: expr.span,
            kind: mir::StatementKind::Assign(
                is_null_place.clone(),
                mir::Rvalue::BinaryOp(mir::BinOp::Eq, mir::Operand::copy(getenv_place), null_const),
            ),
        });

        self.push_statement(mir::Statement {
            source_info: expr.span,
            kind: mir::StatementKind::Assign(
                place,
                mir::Rvalue::UnaryOp(mir::UnOp::Not, mir::Operand::copy(is_null_place)),
            ),
        });

        let _ = expected_ty;
        Ok(())
    }

    pub(super) fn lower_env_var_into_place(
        &mut self,
        expr: &hir::Expr,
        call: &hir::IntrinsicCallExpr,
        place: mir::Place,
        expected_ty: &Ty,
    ) -> Result<()> {
        let args = &call.callargs;
        if args.len() != 1 {
            self.lowering
                .emit_error(expr.span, "env::var intrinsic expects one name argument");
        }

        let str_ty = Ty {
            kind: TyKind::Slice(Box::new(Ty {
                kind: TyKind::Int(IntTy::I8),
            })),
        };
        let name_info = args
            .get(0)
            .map(|arg| self.lower_operand(&arg.value, Some(&str_ty)))
            .transpose()?;

        let name_place = if let Some(info) = &name_info {
            if let mir::Operand::Copy(place) | mir::Operand::Move(place) = &info.operand {
                place.clone()
            } else {
                let local_id = self.allocate_temp(str_ty.clone(), expr.span);
                let local_place = mir::Place::from_local(local_id);
                self.push_statement(mir::Statement {
                    source_info: expr.span,
                    kind: mir::StatementKind::Assign(
                        local_place.clone(),
                        mir::Rvalue::Use(info.operand.clone()),
                    ),
                });
                local_place
            }
        } else {
            let local_id = self.allocate_temp(str_ty.clone(), expr.span);
            mir::Place::from_local(local_id)
        };

        let name_ptr_place = self.lower_slice_ptr_place(name_place);

        let getenv_ret_ty = self.lowering.raw_string_ptr_ty();
        let getenv_local = self.allocate_temp(getenv_ret_ty.clone(), expr.span);
        let getenv_place = mir::Place::from_local(getenv_local);
        self.emit_c_call(
            "getenv",
            mir::FunctionSig {
                inputs: vec![getenv_ret_ty.clone()],
                output: getenv_ret_ty.clone(),
            },
            vec![mir::Operand::copy(name_ptr_place)],
            getenv_place.clone(),
            expr.span,
        )?;

        let strlen_ret_ty = Ty {
            kind: TyKind::Uint(UintTy::Usize),
        };
        let strlen_local = self.allocate_temp(strlen_ret_ty.clone(), expr.span);
        let strlen_place = mir::Place::from_local(strlen_local);
        self.emit_c_call(
            "strlen",
            mir::FunctionSig {
                inputs: vec![getenv_ret_ty.clone()],
                output: strlen_ret_ty.clone(),
            },
            vec![mir::Operand::copy(getenv_place.clone())],
            strlen_place.clone(),
            expr.span,
        )?;

        // Build `str` slice in `place`: { ptr, len }
        let ptr_field_place = self.lower_slice_ptr_place(place.clone());
        let len_field_place = self.lower_slice_len_place(place.clone());
        self.push_statement(mir::Statement {
            source_info: expr.span,
            kind: mir::StatementKind::Assign(
                ptr_field_place,
                mir::Rvalue::Use(mir::Operand::copy(getenv_place)),
            ),
        });
        self.push_statement(mir::Statement {
            source_info: expr.span,
            kind: mir::StatementKind::Assign(
                len_field_place,
                mir::Rvalue::Cast(
                    mir::CastKind::Misc,
                    mir::Operand::copy(strlen_place),
                    Ty {
                        kind: TyKind::Int(IntTy::I64),
                    },
                ),
            ),
        });

        let _ = expected_ty;
        Ok(())
    }

    pub(super) fn lower_fs_exists_into_place(
        &mut self,
        expr: &hir::Expr,
        call: &hir::IntrinsicCallExpr,
        place: mir::Place,
        expected_ty: &Ty,
    ) -> Result<()> {
        let args = &call.callargs;
        if args.len() != 1 {
            self.lowering
                .emit_error(expr.span, "fs::exists intrinsic expects one path argument");
        }

        let path_inner = args
            .get(0)
            .map(|arg| self.lower_path_inner_str(&arg.value))
            .transpose()?;

        let path_ptr = path_inner
            .map(|p| self.lower_slice_ptr_place(p))
            .unwrap_or_else(|| {
                let local = self.allocate_temp(self.lowering.raw_string_ptr_ty(), expr.span);
                let place = mir::Place::from_local(local);
                self.push_statement(mir::Statement {
                    source_info: expr.span,
                    kind: mir::StatementKind::Assign(
                        place.clone(),
                        mir::Rvalue::Use(mir::Operand::Constant(mir::Constant {
                            span: expr.span,
                            ty: self.lowering.raw_string_ptr_ty(),
                            user_ty: None,
                            literal: mir::ConstantKind::Null,
                        })),
                    ),
                });
                place
            });

        let ret_ty = Ty {
            kind: TyKind::Int(IntTy::I32),
        };
        let access_local = self.allocate_temp(ret_ty.clone(), expr.span);
        let access_place = mir::Place::from_local(access_local);
        let f_ok = mir::Operand::Constant(mir::Constant {
            span: expr.span,
            ty: ret_ty.clone(),
            user_ty: None,
            literal: mir::ConstantKind::Int(0),
        });
        self.emit_c_call(
            "access",
            mir::FunctionSig {
                inputs: vec![self.lowering.raw_string_ptr_ty(), ret_ty.clone()],
                output: ret_ty.clone(),
            },
            vec![mir::Operand::copy(path_ptr), f_ok],
            access_place.clone(),
            expr.span,
        )?;

        self.push_statement(mir::Statement {
            source_info: expr.span,
            kind: mir::StatementKind::Assign(
                place,
                mir::Rvalue::BinaryOp(
                    mir::BinOp::Eq,
                    mir::Operand::copy(access_place),
                    mir::Operand::Constant(mir::Constant {
                        span: expr.span,
                        ty: ret_ty.clone(),
                        user_ty: None,
                        literal: mir::ConstantKind::Int(0),
                    }),
                ),
            ),
        });

        let _ = expected_ty;
        Ok(())
    }

    pub(super) fn lower_fs_remove_file_as_statement(
        &mut self,
        expr: &hir::Expr,
        call: &hir::IntrinsicCallExpr,
    ) -> Result<()> {
        let args = &call.callargs;
        if args.len() != 1 {
            self.lowering.emit_error(
                expr.span,
                "fs::remove_file intrinsic expects one path argument",
            );
        }
        let path_inner = args
            .get(0)
            .map(|arg| self.lower_path_inner_str(&arg.value))
            .transpose()?;
        let path_ptr = path_inner
            .map(|p| self.lower_slice_ptr_place(p))
            .unwrap_or_else(|| {
                let local = self.allocate_temp(self.lowering.raw_string_ptr_ty(), expr.span);
                let place = mir::Place::from_local(local);
                self.push_statement(mir::Statement {
                    source_info: expr.span,
                    kind: mir::StatementKind::Assign(
                        place.clone(),
                        mir::Rvalue::Use(mir::Operand::Constant(mir::Constant {
                            span: expr.span,
                            ty: self.lowering.raw_string_ptr_ty(),
                            user_ty: None,
                            literal: mir::ConstantKind::Null,
                        })),
                    ),
                });
                place
            });

        let ret_ty = Ty {
            kind: TyKind::Int(IntTy::I32),
        };
        let local_id = self.allocate_temp(ret_ty.clone(), expr.span);
        let temp_place = mir::Place::from_local(local_id);
        self.emit_c_call(
            "remove",
            mir::FunctionSig {
                inputs: vec![self.lowering.raw_string_ptr_ty()],
                output: ret_ty,
            },
            vec![mir::Operand::copy(path_ptr)],
            temp_place,
            expr.span,
        )
    }

    pub(super) fn lower_fs_read_to_string_into_place(
        &mut self,
        expr: &hir::Expr,
        call: &hir::IntrinsicCallExpr,
        place: mir::Place,
        expected_ty: &Ty,
    ) -> Result<()> {
        let args = &call.callargs;
        if args.len() != 1 {
            self.lowering.emit_error(
                expr.span,
                "fs_read_to_string intrinsic expects one path argument",
            );
        }

        let path_inner = args
            .get(0)
            .map(|arg| self.lower_path_inner_str(&arg.value))
            .transpose()?;
        let path_ptr_place = path_inner
            .map(|p| self.lower_slice_ptr_place(p))
            .unwrap_or_else(|| {
                let local = self.allocate_temp(self.lowering.raw_string_ptr_ty(), expr.span);
                let place = mir::Place::from_local(local);
                self.push_statement(mir::Statement {
                    source_info: expr.span,
                    kind: mir::StatementKind::Assign(
                        place.clone(),
                        mir::Rvalue::Use(mir::Operand::Constant(mir::Constant {
                            span: expr.span,
                            ty: self.lowering.raw_string_ptr_ty(),
                            user_ty: None,
                            literal: mir::ConstantKind::Null,
                        })),
                    ),
                });
                place
            });

        let file_ty = self.lowering.raw_string_ptr_ty();
        let file_local = self.allocate_temp(file_ty.clone(), expr.span);
        let file_place = mir::Place::from_local(file_local);
        // mode = "rb"
        let mode_const = mir::Operand::Constant(mir::Constant {
            span: expr.span,
            ty: self.lowering.raw_string_ptr_ty(),
            user_ty: None,
            literal: mir::ConstantKind::Str("rb".to_string()),
        });

        self.emit_c_call(
            "fopen",
            mir::FunctionSig {
                inputs: vec![
                    self.lowering.raw_string_ptr_ty(),
                    self.lowering.raw_string_ptr_ty(),
                ],
                output: file_ty.clone(),
            },
            vec![mir::Operand::copy(path_ptr_place), mode_const],
            file_place.clone(),
            expr.span,
        )?;

        // If fopen failed, return empty string slice.
        let ok_block = self.new_block();
        let fail_block = self.new_block();
        let join_block = self.new_block();

        let is_null_local = self.allocate_temp(Ty { kind: TyKind::Bool }, expr.span);
        let is_null_place = mir::Place::from_local(is_null_local);
        self.push_statement(mir::Statement {
            source_info: expr.span,
            kind: mir::StatementKind::Assign(
                is_null_place.clone(),
                mir::Rvalue::BinaryOp(
                    mir::BinOp::Eq,
                    mir::Operand::copy(file_place.clone()),
                    mir::Operand::Constant(mir::Constant {
                        span: expr.span,
                        ty: file_ty.clone(),
                        user_ty: None,
                        literal: mir::ConstantKind::Null,
                    }),
                ),
            ),
        });

        self.set_current_terminator(mir::Terminator {
            source_info: expr.span,
            kind: mir::TerminatorKind::SwitchInt {
                discr: mir::Operand::copy(is_null_place),
                switch_ty: Ty { kind: TyKind::Bool },
                targets: mir::SwitchTargets {
                    values: vec![1],
                    targets: vec![fail_block],
                    otherwise: ok_block,
                },
            },
        });

        // fail: set place to empty slice
        self.current_block = fail_block;
        let ptr_field = self.lower_slice_ptr_place(place.clone());
        let len_field = self.lower_slice_len_place(place.clone());
        self.push_statement(mir::Statement {
            source_info: expr.span,
            kind: mir::StatementKind::Assign(
                ptr_field,
                mir::Rvalue::Use(mir::Operand::Constant(mir::Constant {
                    span: expr.span,
                    ty: self.lowering.raw_string_ptr_ty(),
                    user_ty: None,
                    literal: mir::ConstantKind::Str("".to_string()),
                })),
            ),
        });
        self.push_statement(mir::Statement {
            source_info: expr.span,
            kind: mir::StatementKind::Assign(
                len_field,
                mir::Rvalue::Use(mir::Operand::Constant(mir::Constant {
                    span: expr.span,
                    ty: Ty {
                        kind: TyKind::Int(IntTy::I64),
                    },
                    user_ty: None,
                    literal: mir::ConstantKind::Int(0),
                })),
            ),
        });
        self.set_current_terminator(mir::Terminator {
            source_info: expr.span,
            kind: mir::TerminatorKind::Goto { target: join_block },
        });

        // ok: read file size via fseek/ftell, malloc, fread, fclose
        self.current_block = ok_block;
        let int_ty = Ty {
            kind: TyKind::Int(IntTy::I32),
        };
        let long_ty = Ty {
            kind: TyKind::Int(IntTy::I64),
        };
        let size_ty = Ty {
            kind: TyKind::Uint(UintTy::Usize),
        };

        let seek_ret_local = self.allocate_temp(int_ty.clone(), expr.span);
        self.emit_c_call(
            "fseek",
            mir::FunctionSig {
                inputs: vec![file_ty.clone(), long_ty.clone(), int_ty.clone()],
                output: int_ty.clone(),
            },
            vec![
                mir::Operand::copy(file_place.clone()),
                mir::Operand::Constant(mir::Constant {
                    span: expr.span,
                    ty: long_ty.clone(),
                    user_ty: None,
                    literal: mir::ConstantKind::Int(0),
                }),
                mir::Operand::Constant(mir::Constant {
                    span: expr.span,
                    ty: int_ty.clone(),
                    user_ty: None,
                    literal: mir::ConstantKind::Int(2), // SEEK_END
                }),
            ],
            mir::Place::from_local(seek_ret_local),
            expr.span,
        )?;

        let len_local = self.allocate_temp(long_ty.clone(), expr.span);
        let len_place = mir::Place::from_local(len_local);
        self.emit_c_call(
            "ftell",
            mir::FunctionSig {
                inputs: vec![file_ty.clone()],
                output: long_ty.clone(),
            },
            vec![mir::Operand::copy(file_place.clone())],
            len_place.clone(),
            expr.span,
        )?;

        let rewind_ret_local = self.allocate_temp(int_ty.clone(), expr.span);
        self.emit_c_call(
            "fseek",
            mir::FunctionSig {
                inputs: vec![file_ty.clone(), long_ty.clone(), int_ty.clone()],
                output: int_ty.clone(),
            },
            vec![
                mir::Operand::copy(file_place.clone()),
                mir::Operand::Constant(mir::Constant {
                    span: expr.span,
                    ty: long_ty.clone(),
                    user_ty: None,
                    literal: mir::ConstantKind::Int(0),
                }),
                mir::Operand::Constant(mir::Constant {
                    span: expr.span,
                    ty: int_ty.clone(),
                    user_ty: None,
                    literal: mir::ConstantKind::Int(0), // SEEK_SET
                }),
            ],
            mir::Place::from_local(rewind_ret_local),
            expr.span,
        )?;

        let malloc_ret_ty = self.lowering.raw_string_ptr_ty();
        let buf_local = self.allocate_temp(malloc_ret_ty.clone(), expr.span);
        let buf_place = mir::Place::from_local(buf_local);
        let size_cast_local = self.allocate_temp(size_ty.clone(), expr.span);
        let size_cast_place = mir::Place::from_local(size_cast_local);
        self.push_statement(mir::Statement {
            source_info: expr.span,
            kind: mir::StatementKind::Assign(
                size_cast_place.clone(),
                mir::Rvalue::Cast(
                    mir::CastKind::Misc,
                    mir::Operand::copy(len_place.clone()),
                    size_ty.clone(),
                ),
            ),
        });
        self.emit_c_call(
            "malloc",
            mir::FunctionSig {
                inputs: vec![size_ty.clone()],
                output: malloc_ret_ty.clone(),
            },
            vec![mir::Operand::copy(size_cast_place.clone())],
            buf_place.clone(),
            expr.span,
        )?;

        let fread_ret_local = self.allocate_temp(size_ty.clone(), expr.span);
        let fread_len_cast_local = self.allocate_temp(size_ty.clone(), expr.span);
        let fread_len_cast_place = mir::Place::from_local(fread_len_cast_local);
        self.push_statement(mir::Statement {
            source_info: expr.span,
            kind: mir::StatementKind::Assign(
                fread_len_cast_place.clone(),
                mir::Rvalue::Cast(
                    mir::CastKind::Misc,
                    mir::Operand::copy(len_place.clone()),
                    size_ty.clone(),
                ),
            ),
        });
        self.emit_c_call(
            "fread",
            mir::FunctionSig {
                inputs: vec![
                    malloc_ret_ty.clone(),
                    size_ty.clone(),
                    size_ty.clone(),
                    file_ty.clone(),
                ],
                output: size_ty.clone(),
            },
            vec![
                mir::Operand::copy(buf_place.clone()),
                mir::Operand::Constant(mir::Constant {
                    span: expr.span,
                    ty: size_ty.clone(),
                    user_ty: None,
                    literal: mir::ConstantKind::UInt(1),
                }),
                mir::Operand::copy(fread_len_cast_place),
                mir::Operand::copy(file_place.clone()),
            ],
            mir::Place::from_local(fread_ret_local),
            expr.span,
        )?;

        let fclose_ret_local = self.allocate_temp(int_ty.clone(), expr.span);
        self.emit_c_call(
            "fclose",
            mir::FunctionSig {
                inputs: vec![file_ty.clone()],
                output: int_ty,
            },
            vec![mir::Operand::copy(file_place)],
            mir::Place::from_local(fclose_ret_local),
            expr.span,
        )?;

        // write slice fields
        let ptr_field_place = self.lower_slice_ptr_place(place.clone());
        let len_field_place = self.lower_slice_len_place(place);
        self.push_statement(mir::Statement {
            source_info: expr.span,
            kind: mir::StatementKind::Assign(
                ptr_field_place,
                mir::Rvalue::Use(mir::Operand::copy(buf_place)),
            ),
        });
        self.push_statement(mir::Statement {
            source_info: expr.span,
            kind: mir::StatementKind::Assign(
                len_field_place,
                mir::Rvalue::Use(mir::Operand::copy(len_place)),
            ),
        });
        self.set_current_terminator(mir::Terminator {
            source_info: expr.span,
            kind: mir::TerminatorKind::Goto { target: join_block },
        });

        self.current_block = join_block;
        let _ = expected_ty;
        Ok(())
    }
}
