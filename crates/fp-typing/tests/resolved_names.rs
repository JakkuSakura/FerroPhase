use fp_core::ast::*;
use fp_core::module::path::QualifiedPath;
use fp_typing::{AstTypeInferencer, ResolvedNameNamespace, TypeResolutionHook, TypingContext};

/// A bare `AstTypeInferencer` has no compiler driver attached, so a
/// `DefConst`'s value only ever resolves via a `resolution_hook` (see
/// `TypeResolutionHook`'s contract) -- there is no other source of truth for
/// a comptime value. This test only cares about `resolved_names` bookkeeping
/// for a symbol reference, not the const's concrete value, but the typer
/// still awaits that value as part of processing the `DefConst` item, so it
/// needs *some* hook that can resolve a plain literal -- mirroring what
/// `fp-compiler`'s real `ComptimeHook` does for a trivial case like this one,
/// without pulling in the rest of the driver.
struct LiteralComptimeHook {
    typing_ctx: std::rc::Rc<TypingContext>,
}

impl TypeResolutionHook for LiteralComptimeHook {
    fn resolve_symbol(&mut self, _name: &str) -> bool {
        false
    }

    fn request_comptime(&mut self, key: &str, expr: &Expr) -> bool {
        let ExprKind::Value(value) = expr.kind() else {
            return false;
        };
        self.typing_ctx
            .resolved_consts
            .borrow_mut()
            .insert(key.to_string(), (**value).clone());
        self.typing_ctx.wake_comptime(key);
        true
    }
}

#[test]
fn type_inference_records_resolved_name_on_tast_expr() {
    let const_item = Item::from(ItemKind::DefConst(ItemDefConst {
        attrs: Vec::new(),
        mutable: None,
        ty_annotation: None,
        visibility: Visibility::Public,
        name: Ident::new("VALUE"),
        ty: Some(Ty::Primitive(TypePrimitive::Int(TypeInt::I64))),
        value: Box::new(Expr::value(Value::int(1))),
    }));
    let mut expr = Expr::name(Name::path(Path::new(
        fp_core::module::path::PathPrefix::Crate,
        vec![Ident::new("VALUE")],
    )));
    let mut file = File {
        path: "resolved_names.fp".into(),
        attrs: Vec::new(),
        collected_items: Vec::new(),
        items: vec![const_item, Item::from(ItemKind::Expr(expr.clone()))],
    };

    let typing_ctx = std::rc::Rc::new(fp_typing::TypingContext::new(std::rc::Rc::new(
        fp_core::workspace::WorkspaceContext::new(),
    )));
    let typer = AstTypeInferencer::new(typing_ctx.clone());
    typer.set_resolution_hook(Box::new(LiteralComptimeHook {
        typing_ctx: typing_ctx.clone(),
    }));
    let outcome = fp_typing::block_on(typer.infer_file(&mut file)).expect("infer");
    let has_errors = typing_ctx
        .diagnostics
        .borrow()
        .iter()
        .any(|d| matches!(d.level, fp_typing::TypingDiagnosticLevel::Error));
    assert!(!has_errors, "expected typing to succeed");

    let ItemKind::Expr(typed_expr) = file.items[1].kind() else {
        panic!("expected expr item");
    };
    expr = typed_expr.clone();
    let resolved = outcome
        .resolved_names
        .get(&expr.id())
        .expect("typing outcome should carry resolved name for expression");
    assert_eq!(resolved.path, QualifiedPath::new(vec!["VALUE".to_string()]));
    assert!(matches!(resolved.namespace, ResolvedNameNamespace::Value));
}

#[test]
fn type_inference_resolves_preloaded_cross_crate_enum_variant() {
    let workspace = std::rc::Rc::new(fp_core::workspace::WorkspaceContext::new());
    let dependency = workspace.begin_crate(
        "dep",
        fp_core::package::graph::PackageGraph::new(Vec::new()),
    );
    dependency
        .borrow_mut()
        .module_paths
        .insert(QualifiedPath::new(vec!["dep".to_string()]));
    dependency.borrow_mut().enum_defs.insert(
        QualifiedPath::new(vec!["dep".to_string(), "Status".to_string()]),
        TypeEnum {
            name: Ident::new("Status"),
            generics_params: Vec::new(),
            repr: ReprOptions::default(),
            variants: vec![EnumTypeVariant {
                name: Ident::new("Ready"),
                value: Ty::Unit(TypeUnit),
                discriminant: None,
            }],
        },
    );

    let mut expr = Expr::name(Name::path(Path::new(
        fp_core::module::path::PathPrefix::Plain,
        vec![Ident::new("dep"), Ident::new("Status"), Ident::new("Ready")],
    )));
    let typer = AstTypeInferencer::new(std::rc::Rc::new(TypingContext::new(workspace)));
    typer.seed_workspace_graph();
    fp_typing::block_on(typer.infer_expression(&mut expr))
        .expect("cross-crate enum variant should resolve");

    let Some(Ty::Enum(enum_ty)) = expr.ty() else {
        panic!("expected a resolved enum variant type, got {:?}", expr.ty());
    };
    assert_eq!(enum_ty.name.as_str(), "Status");
}
