use super::*;

impl HirTypeChecker {
    pub(super) async fn check_intrinsic(&mut self, call: &hir::IntrinsicCallExpr) -> Result<Ty> {
        use fp_core::intrinsics::IntrinsicKind;
        let kind = call.kind;
        if matches!(
            kind,
            IntrinsicKind::SizeOf | IntrinsicKind::FieldCount | IntrinsicKind::MethodCount
        ) {
            return Ok(Ty::uint(ty::UintTy::U64));
        }
        let mut arg_types = Vec::with_capacity(call.callargs.len());
        for arg in &call.callargs {
            arg_types.push(self.check_expr(&arg.value).await?);
        }
        Ok(match kind {
            IntrinsicKind::Print | IntrinsicKind::Println => Ty {
                kind: TyKind::Tuple(Vec::new()),
            },
            IntrinsicKind::Panic => Ty::never(),
            IntrinsicKind::Format => Ty {
                kind: TyKind::Slice(Box::new(Ty::int(ty::IntTy::I8))),
            },
            IntrinsicKind::Len => Ty::uint(ty::UintTy::Usize),
            IntrinsicKind::Slice => match arg_types.first() {
                None => self.error_ty("slice intrinsic requires a base expression"),
                Some(base) => match &base.kind {
                    TyKind::Array(inner, _) | TyKind::Slice(inner) => Ty {
                        kind: TyKind::Slice(inner.clone()),
                    },
                    _ => self.error_ty("slice intrinsic base must be an array or slice"),
                },
            },
            IntrinsicKind::DebugAssertions
            | IntrinsicKind::FsExists
            | IntrinsicKind::FsIsDir
            | IntrinsicKind::FsIsFile
            | IntrinsicKind::EnvVarExists
            | IntrinsicKind::HasField
            | IntrinsicKind::HasMethod
            | IntrinsicKind::PathIsAbsolute
            | IntrinsicKind::CatchUnwind => Ty::bool(),
            IntrinsicKind::Input
            | IntrinsicKind::FsReadToString
            | IntrinsicKind::FsReadDir
            | IntrinsicKind::FsWalkDir
            | IntrinsicKind::FsGlob
            | IntrinsicKind::EnvCurrentDir
            | IntrinsicKind::EnvTempDir
            | IntrinsicKind::EnvHomeDir
            | IntrinsicKind::EnvVar
            | IntrinsicKind::PathJoin
            | IntrinsicKind::PathParent
            | IntrinsicKind::PathFileName
            | IntrinsicKind::PathExtension
            | IntrinsicKind::PathStem
            | IntrinsicKind::PathNormalize
            | IntrinsicKind::IoReadStdinToString
            | IntrinsicKind::YamlToJson
            | IntrinsicKind::JsonParse
            | IntrinsicKind::ProcMacroTokenStreamToString
            | IntrinsicKind::FieldNameAt
            | IntrinsicKind::TypeName
            | IntrinsicKind::ProcMacroTokenStreamFromStr => Ty {
                kind: TyKind::Slice(Box::new(Ty::int(ty::IntTy::I8))),
            },
            IntrinsicKind::TimeNow => Ty::float(ty::FloatTy::F64),
            IntrinsicKind::CatchUnwindResult => match arg_types.first().cloned() {
                None => self.error_ty("catch_unwind_result requires a callable argument"),
                Some(value) => Ty {
                    kind: TyKind::Tuple(vec![Box::new(Ty::bool()), Box::new(value)]),
                },
            },
            IntrinsicKind::Spawn | IntrinsicKind::Select => match arg_types.first() {
                Some(value) => value.clone(),
                None => self.error_ty(format!("{:?} intrinsic requires an argument", kind)),
            },
            IntrinsicKind::Join => match arg_types.as_slice() {
                [value] => value.clone(),
                [] => self.error_ty("join intrinsic requires an argument"),
                values => Ty {
                    kind: TyKind::Tuple(values.iter().cloned().map(Box::new).collect()),
                },
            },
            IntrinsicKind::VecType => {
                self.error_ty("type-valued intrinsic has no HIR type representation")
            }
            IntrinsicKind::TypeOf => self
                .well_known_struct_ty("TypeDescriptor", Vec::new())
                .unwrap_or_else(|| self.error_ty("std::meta::TypeDescriptor is not declared")),
            IntrinsicKind::FieldType => self
                .well_known_struct_ty("FieldTypeDescriptor", Vec::new())
                .unwrap_or_else(|| self.error_ty("std::meta::FieldTypeDescriptor is not declared")),
            IntrinsicKind::CreateStruct
            | IntrinsicKind::AddField
            | IntrinsicKind::BuildType
            | IntrinsicKind::PrimitiveType => Ty { kind: TyKind::Type },
            IntrinsicKind::FsWriteString
            | IntrinsicKind::FsAppendString
            | IntrinsicKind::FsCreateDirAll
            | IntrinsicKind::FsRemoveFile
            | IntrinsicKind::FsRemoveDirAll
            | IntrinsicKind::IoWriteStdout
            | IntrinsicKind::IoWriteStderr
            | IntrinsicKind::TestCommandMockReset
            | IntrinsicKind::TestCommandMockPush
            | IntrinsicKind::TestCommandMockApply
            | IntrinsicKind::Sleep
            | IntrinsicKind::Yield
            | IntrinsicKind::CompileWarning => self.unit_ty(),
            IntrinsicKind::TestCommandMockTakeCalls => self
                .well_known_struct_ty(
                    "Vec",
                    vec![ty::GenericArg::Type(Ty {
                        kind: TyKind::Slice(Box::new(Ty::int(ty::IntTy::I8))),
                    })],
                )
                .unwrap_or_else(|| self.error_ty("std::alloc::Vec is not declared")),
            IntrinsicKind::CompileError => {
                self.error_ty("compile_error intrinsic requested an error")
            }
            _ => self.error_ty(format!("intrinsic `{:?}` has no HIR type rule", kind)),
        })
    }
}
