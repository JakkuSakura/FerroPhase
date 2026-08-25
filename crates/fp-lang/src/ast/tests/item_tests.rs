use super::*;

#[test]
fn parse_items_ast_handles_const_item() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser.parse_items_ast("const X: i64 = 1;").unwrap();
    assert!(matches!(items[0].kind(), ItemKind::DefConst(_)));
}

#[test]
fn parse_items_ast_handles_static_item() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser.parse_items_ast("static X: i64 = 1;").unwrap();
    assert!(matches!(items[0].kind(), ItemKind::DefStatic(_)));
}

#[test]
fn parse_items_ast_handles_type_alias() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser.parse_items_ast("type X = i64;").unwrap();
    assert!(matches!(items[0].kind(), ItemKind::DefType(_)));
}

#[test]
fn parse_items_ast_handles_enum_item() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser.parse_items_ast("enum E { A = 1, B }").unwrap();
    assert!(matches!(items[0].kind(), ItemKind::DefEnum(_)));
}

#[test]
fn parse_items_ast_handles_enum_struct_variants() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast("enum E { Named { path: String, code: i64 }, Unit }")
        .unwrap();
    let ItemKind::DefEnum(item) = items[0].kind() else {
        panic!("expected enum item");
    };
    let named = &item.value.variants[0];
    assert!(matches!(named.value, Ty::Structural(_)));
}

#[test]
fn parse_items_ast_handles_module_item() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser.parse_items_ast("mod foo {}").unwrap();
    assert!(matches!(items[0].kind(), ItemKind::Module(_)));
}

#[test]
fn parse_items_ast_supports_inner_doc_include_str() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast("mod m { #![doc = include_str!(\"foo\")] }")
        .unwrap();
    let ItemKind::Module(module) = items[0].kind() else {
        panic!("expected module item");
    };
    let attr = module
        .attrs
        .iter()
        .find(|attr| matches!(attr.style, AttrStyle::Inner))
        .expect("expected inner attribute");
    let AttrMeta::NameValue(meta) = &attr.meta else {
        panic!("expected name-value attribute");
    };
    assert_eq!(meta.name.last().as_str(), "doc");
    assert!(matches!(meta.value.kind, ExprKind::Macro(_)));
}

#[test]
fn parse_items_ast_handles_external_module_item() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser.parse_items_ast("mod foo;").unwrap();
    assert!(matches!(items[0].kind(), ItemKind::Module(_)));
}

#[test]
fn parse_items_ast_handles_trait_item() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser.parse_items_ast("trait T { fn f(); }").unwrap();
    assert!(matches!(items[0].kind(), ItemKind::DefTrait(_)));
}

#[test]
fn parse_items_ast_handles_trait_generics() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast("pub trait TryConv<T> { fn try_conv(self) -> T; }")
        .unwrap();
    let ItemKind::DefTrait(trait_item) = items[0].kind() else {
        panic!("expected trait item");
    };
    assert_eq!(trait_item.generics_params.len(), 1);
    assert_eq!(trait_item.generics_params[0].name.as_str(), "T");
}

#[test]
fn parse_items_ast_handles_trait_receiver_and_self_assoc_type() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast(
            "trait PipelineStage: Send + Sync { type SrcCtx; type DstCtx; fn name(&self) -> &'static str; fn run(&self, context: Self::SrcCtx, diagnostics: &mut PipelineDiagnostics) -> Result<Self::DstCtx, PipelineError>; }",
        )
        .unwrap();
    assert!(matches!(items[0].kind(), ItemKind::DefTrait(_)));
}

#[test]
fn parse_items_ast_handles_return_if_with_self_and_turbofish() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast(
            "impl VirtualPath { fn parent(&self) -> Option<Self> { if self.segments.is_empty() { return if self.absolute { Some(Self::new_internal(true, Vec::<String>::new())) } else { None }; } Some(self) } }",
        )
        .unwrap();
    assert!(matches!(items[0].kind(), ItemKind::Impl(_)));
}

#[test]
fn parse_items_ast_handles_for_loop_with_let_else_body() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast(
            "fn raise_implicit_call_arguments(function: &mut AsmFunction, abi: Abi) { let arg_regs = abi_int_arg_registers(abi); for block in &mut function.basic_blocks { for inst in &mut block.instructions { let AsmInstructionKind::Call { args, .. } = &mut inst.kind else { continue; }; if !args.is_empty() { continue; } *args = arg_regs.iter().map(|name| AsmValue::PhysicalRegister(abi_register(name))).collect(); } } }",
        )
        .unwrap();
    assert!(matches!(items[0].kind(), ItemKind::DefFunction(_)));
}

#[test]
fn parse_items_ast_handles_impl_item() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser.parse_items_ast("impl Foo { fn f() {} }").unwrap();
    assert!(matches!(items[0].kind(), ItemKind::Impl(_)));
}

#[test]
fn parse_items_ast_handles_trait_impl_item() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast("impl Foo for Bar { fn f() {} }")
        .unwrap();
    assert!(matches!(items[0].kind(), ItemKind::Impl(_)));
}

#[test]
fn parse_items_ast_handles_impl_associated_type() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast("impl FromStr for RefNode { type Err = CoreError; fn from_str() {} }")
        .unwrap();
    let ItemKind::Impl(item) = items[0].kind() else {
        panic!("expected impl item");
    };
    assert!(matches!(item.items[0].kind(), ItemKind::DefType(_)));
    assert!(matches!(item.items[1].kind(), ItemKind::DefFunction(_)));
}

#[test]
fn parse_items_ast_handles_lifetime_self_receiver() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast("impl Foo { fn resolve<'a>(&'a self) -> &'a Self { self } }")
        .unwrap();
    assert!(matches!(items[0].kind(), ItemKind::Impl(_)));
}

#[test]
fn parse_items_ast_handles_generic_fn_with_where() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast("fn f<T>(x: T) where T: Foo { x }")
        .unwrap();
    assert!(matches!(items[0].kind(), ItemKind::DefFunction(_)));
}

#[test]
fn parse_items_ast_handles_octal_literals_in_impl_methods() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser
        .parse_items_ast(
            "impl OpenOptions { pub fn new() -> OpenOptions { OpenOptions { mode: 0o666 } } }",
        )
        .unwrap();
    assert!(matches!(items[0].kind(), ItemKind::Impl(_)));
}

#[test]
fn parse_items_supports_fn_struct_and_use() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = "use foo::bar; struct S { x: i64 } fn f() {}";
    let items = parser.parse_items_ast(src).unwrap();
    assert!(items.len() >= 3);
}

#[test]
fn parse_items_supports_typed_params_and_fields() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = "struct S { x: i64 } fn f(x: i64) -> i64 { x }";
    let items = parser.parse_items_ast(src).unwrap();
    assert!(items.len() >= 2);
}

#[test]
fn parse_items_supports_fn_attributes() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = "#[inline] fn f() {}";
    let items = parser.parse_items_ast(src).unwrap();
    assert!(matches!(items[0].kind(), ItemKind::DefFunction(_)));
}

#[test]
fn parse_items_supports_lang_name_value_attributes() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = "#[intrinsic = \"time_now\"] fn f() {}";
    let items = parser.parse_items_ast(src).unwrap();
    let ItemKind::DefFunction(function) = items[0].kind() else {
        panic!("expected function");
    };
    let attr = function
        .attrs
        .iter()
        .find(|attr| matches!(&attr.meta, AttrMeta::NameValue(_)))
        .expect("expected name-value attribute");
    let AttrMeta::NameValue(meta) = &attr.meta else {
        unreachable!();
    };
    assert_eq!(meta.name.last().as_str(), "intrinsic");
}

#[test]
fn parse_items_supports_bool_and_int_name_value_attributes() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = "#[defaults(present = true, retries = 3)] fn f() {}";
    let items = parser.parse_items_ast(src).unwrap();
    let ItemKind::DefFunction(function) = items[0].kind() else {
        panic!("expected function");
    };
    let defaults = function
        .attrs
        .iter()
        .find(|attr| matches!(&attr.meta, AttrMeta::List(list) if list.name.last().as_str() == "defaults"))
        .expect("expected defaults attribute");
    let AttrMeta::List(list) = &defaults.meta else {
        panic!("expected defaults list");
    };
    assert_eq!(list.items.len(), 2);

    let AttrMeta::NameValue(present) = &list.items[0] else {
        panic!("expected present default");
    };
    assert_eq!(present.name.last().as_str(), "present");
    assert!(
        matches!(present.value.kind(), ExprKind::Value(value) if matches!(value.as_ref(), Value::Bool(flag) if flag.value))
    );

    let AttrMeta::NameValue(retries) = &list.items[1] else {
        panic!("expected retries default");
    };
    assert_eq!(retries.name.last().as_str(), "retries");
    assert!(
        matches!(retries.value.kind(), ExprKind::Value(value) if matches!(value.as_ref(), Value::Int(number) if number.value == 3))
    );
}

#[test]
fn parse_items_supports_python_like_function_defaults() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = "fn f(x: str = \"hi\", y: bool = true, z: i64 = 3) {}";
    let items = parser.parse_items_ast(src).unwrap();
    let ItemKind::DefFunction(function) = items[0].kind() else {
        panic!("expected function");
    };
    let params = &function.sig.params;
    assert_eq!(params.len(), 3);
    assert!(matches!(params[0].default, Some(Value::String(_))));
    assert!(matches!(params[1].default, Some(Value::Bool(_))));
    assert!(matches!(params[2].default, Some(Value::Int(_))));
}

#[test]
fn parse_items_supports_python_like_param_markers() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let src = "fn f(a: str, /, b: str, *args: str, c: str, **kwargs: str) {}";
    let items = parser.parse_items_ast(src).unwrap();
    let ItemKind::DefFunction(function) = items[0].kind() else {
        panic!("expected function");
    };
    let params = &function.sig.params;
    assert_eq!(params.len(), 5);
    assert!(params[0].positional_only);
    assert!(!params[1].positional_only);
    assert!(params[2].as_tuple);
    assert!(params[3].keyword_only);
    assert!(params[4].as_dict);
    assert!(params[4].keyword_only);
}

#[test]
fn parse_items_supports_item_macro() {
    let parser = FerroPhaseParser::new();
    parser.clear_diagnostics();
    let items = parser.parse_items_ast("foo!{ bar }").unwrap();
    assert!(matches!(items[0].kind(), ItemKind::Macro(_)));
}
