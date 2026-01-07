use fp_core::ast::{
    Expr, File, FunctionParam, Ident, Item, ItemDefConst, ItemDefFunction, ItemKind, Ty,
    TypePrimitive, Visibility,
};
use fp_core::intrinsics::{
    ensure_function_decl, make_function_decl, IntrinsicCall, IntrinsicCallKind,
    IntrinsicCallPayload, StdIntrinsic,
};
use std::collections::HashSet;
use std::path::PathBuf;

fn empty_file() -> File {
    File {
        path: PathBuf::from("test.fp"),
        items: Vec::new(),
    }
}

fn push_const_item(file: &mut File, name: &str) {
    file.items.push(Item::from(ItemDefConst {
        mutable: None,
        ty_annotation: None,
        visibility: Visibility::Private,
        name: Ident::new(name),
        ty: None,
        value: Box::new(Expr::unit()),
    }));
}

fn string_param(name: &str) -> FunctionParam {
    let mut param = FunctionParam::new(Ident::new(name), Ty::Primitive(TypePrimitive::String));
    param.ty_annotation = Some(param.ty.clone());
    param
}

fn any_tuple_param(name: &str) -> FunctionParam {
    let mut param = FunctionParam::new(Ident::new(name), Ty::Any(fp_core::ast::TypeAny));
    param.ty_annotation = Some(param.ty.clone());
    param.as_tuple = true;
    param
}

#[test]
fn ensure_function_decl_inserts_declaration() {
    let mut file = empty_file();
    let decl = make_function_decl(
        "printf",
        vec![string_param("message"), any_tuple_param("rest")],
        Ty::unit(),
    );
    ensure_function_decl(&mut file, decl);

    assert_eq!(file.items.len(), 1);
    match file.items[0].kind() {
        ItemKind::DeclFunction(decl) => {
            assert_eq!(decl.name.as_str(), "printf");
            assert_eq!(decl.sig.params.len(), 2);
            assert!(decl.sig.params[1].as_tuple);
            assert!(matches!(decl.sig.ret_ty, Some(Ty::Unit(_))));
        }
        other => panic!("expected function declaration, found {:?}", other),
    }
}

#[test]
fn ensure_function_decl_populates_type_annotation() {
    let mut file = empty_file();
    let decl = make_function_decl("puts", vec![string_param("msg")], Ty::unit());
    ensure_function_decl(&mut file, decl);

    match file.items[0].kind() {
        ItemKind::DeclFunction(decl) => {
            let annotation = decl.ty_annotation.as_ref().expect("annotation missing");
            if let Ty::Function(function) = annotation {
                assert_eq!(function.params.len(), 1);
            } else {
                panic!("expected function annotation");
            }
        }
        _ => panic!("unexpected item kind"),
    }
}

#[test]
fn ensure_function_decl_does_not_duplicate_decl() {
    let mut file = empty_file();
    let decl = make_function_decl("puts", Vec::new(), Ty::unit());
    ensure_function_decl(&mut file, decl.clone());
    ensure_function_decl(&mut file, decl);

    assert_eq!(file.items.len(), 1);
}

#[test]
fn ensure_function_decl_skips_existing_definition() {
    let mut file = empty_file();
    let body = Box::new(Expr::unit());
    let def = ItemDefFunction::new_simple(Ident::new("puts"), body);
    file.items.push(Item::from(def));

    let decl = make_function_decl("puts", Vec::new(), Ty::unit());
    ensure_function_decl(&mut file, decl);
    assert_eq!(file.items.len(), 1);
    match file.items[0].kind() {
        ItemKind::DefFunction(def) => assert_eq!(def.name.as_str(), "puts"),
        other => panic!("expected definition to remain, found {:?}", other),
    }
}

#[test]
fn ensure_function_decl_inserts_at_front() {
    let mut file = empty_file();
    push_const_item(&mut file, "CONST");

    let decl = make_function_decl("logger", Vec::new(), Ty::unit());
    ensure_function_decl(&mut file, decl);

    assert_eq!(file.items.len(), 2);
    assert!(matches!(file.items[0].kind(), ItemKind::DeclFunction(_)));
}

#[test]
fn ensure_function_decl_preserves_file_path() {
    let mut file = File {
        path: PathBuf::from("sample.fp"),
        items: Vec::new(),
    };
    let decl = make_function_decl("noop", Vec::new(), Ty::unit());
    ensure_function_decl(&mut file, decl);
    assert_eq!(file.path, PathBuf::from("sample.fp"));
}

#[test]
fn std_intrinsic_variants_are_hashable() {
    let mut set = HashSet::new();
    set.insert(StdIntrinsic::IoPrint);
    set.insert(StdIntrinsic::IoPrintln);
    assert_eq!(set.len(), 2);
    assert!(set.contains(&StdIntrinsic::IoPrint));
}

#[test]
fn intrinsic_call_maps_payload() {
    let call: IntrinsicCall<IntrinsicCallPayload<i32, String>> = IntrinsicCall::new(
        IntrinsicCallKind::Println,
        IntrinsicCallPayload::Args { args: vec![1] },
    );

    let mapped = call.map_payload(|payload| match payload {
        IntrinsicCallPayload::Args { mut args } => {
            args.push(2);
            IntrinsicCallPayload::Args { args }
        }
        other => other,
    });

    match mapped.payload {
        IntrinsicCallPayload::Args { args } => assert_eq!(args, vec![1, 2]),
        _ => panic!("expected args payload"),
    }
}

#[test]
fn intrinsic_call_payload_maps_exprs_and_format() {
    let payload: IntrinsicCallPayload<i32, String> = IntrinsicCallPayload::Args {
        args: vec![1, 2, 3],
    };
    let mapped = payload.map_exprs(|value| value + 1);
    match mapped {
        IntrinsicCallPayload::Args { args } => assert_eq!(args, vec![2, 3, 4]),
        _ => panic!("expected args payload"),
    }

    let payload: IntrinsicCallPayload<i32, String> = IntrinsicCallPayload::Format {
        template: String::from("{}"),
    };
    let mapped = payload.map_format(|template| template + "!");
    match mapped {
        IntrinsicCallPayload::Format { template } => assert_eq!(template, "{}!"),
        _ => panic!("expected format payload"),
    }
}

#[test]
fn intrinsic_call_kind_matches_expected_variants() {
    assert_eq!(IntrinsicCallKind::Print, IntrinsicCallKind::Print);
    assert_ne!(IntrinsicCallKind::Print, IntrinsicCallKind::Println);
}
