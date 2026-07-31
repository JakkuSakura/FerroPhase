use fp_core::ast::*;
use fp_core::module::path::PathPrefix;
use fp_typing::{AstTypeInferencer, TypingDiagnosticLevel};

/// Type-checks `file` with a fresh typer/context and returns whether any
/// error was reported, either as a hard `Result::Err` or as an error-level
/// diagnostic recorded on the shared typing context.
fn file_has_errors(mut file: File) -> bool {
    let typing_ctx = std::rc::Rc::new(fp_typing::TypingContext::new(std::rc::Rc::new(
        fp_core::workspace::WorkspaceContext::new(),
    )));
    let mut typer = AstTypeInferencer::new(typing_ctx.clone());
    let result = fp_typing::block_on(typer.infer_file(&mut file));
    result.is_err()
        || typing_ctx
            .diagnostics
            .borrow()
            .iter()
            .any(|d| matches!(d.level, TypingDiagnosticLevel::Error))
}

fn make_box_type(inner: Ty) -> Ty {
    let segment = ParameterPathSegment::new(Ident::new("Box"), vec![inner]);
    let path = ParameterPath::new(PathPrefix::Plain, vec![segment]);
    Ty::expr(Expr::name(Name::parameter_path(path)))
}

fn make_arc_type(inner: Ty) -> Ty {
    let segment = ParameterPathSegment::new(Ident::new("Arc"), vec![inner]);
    let path = ParameterPath::new(PathPrefix::Plain, vec![segment]);
    Ty::expr(Expr::name(Name::parameter_path(path)))
}

fn make_rc_type(inner: Ty) -> Ty {
    let segment = ParameterPathSegment::new(Ident::new("Rc"), vec![inner]);
    let path = ParameterPath::new(PathPrefix::Plain, vec![segment]);
    Ty::expr(Expr::name(Name::parameter_path(path)))
}

fn make_weak_type(inner: Ty) -> Ty {
    let segment = ParameterPathSegment::new(Ident::new("Weak"), vec![inner]);
    let path = ParameterPath::new(PathPrefix::Plain, vec![segment]);
    Ty::expr(Expr::name(Name::parameter_path(path)))
}

#[test]
fn recursive_struct_rejected_without_box() {
    let name = Ident::new("Node");
    let field = StructuralField::new(Ident::new("next"), Ty::expr(Expr::ident(name.clone())));
    let def = ItemDefStruct::new(name.clone(), vec![field]);
    let file = File {
        path: "recursive.fp".into(),
        attrs: Vec::new(),
        collected_items: Vec::new(),
        items: vec![Item::from(ItemKind::DefStruct(def))],
    };

    assert!(file_has_errors(file), "expected recursion error");
}

#[test]
fn recursive_struct_allowed_with_box() {
    let name = Ident::new("Node");
    let boxed = make_box_type(Ty::expr(Expr::ident(name.clone())));
    let field = StructuralField::new(Ident::new("next"), boxed);
    let def = ItemDefStruct::new(name.clone(), vec![field]);
    let file = File {
        path: "recursive_box.fp".into(),
        attrs: Vec::new(),
        collected_items: Vec::new(),
        items: vec![Item::from(ItemKind::DefStruct(def))],
    };

    assert!(!file_has_errors(file), "boxed recursion should be allowed");
}

#[test]
fn recursive_struct_allowed_with_vec() {
    let name = Ident::new("NodeVec");
    let field = StructuralField::new(
        Ident::new("next"),
        Ty::Vec(TypeVec {
            ty: Box::new(Ty::expr(Expr::ident(name.clone()))),
        }),
    );
    let def = ItemDefStruct::new(name.clone(), vec![field]);
    let file = File {
        path: "recursive_vec.fp".into(),
        attrs: Vec::new(),
        collected_items: Vec::new(),
        items: vec![Item::from(ItemKind::DefStruct(def))],
    };

    assert!(!file_has_errors(file), "vec recursion should be allowed");
}

#[test]
fn recursive_struct_allowed_with_ref() {
    let name = Ident::new("NodeRef");
    let field = StructuralField::new(
        Ident::new("next"),
        Ty::Reference(TypeReference {
            ty: Box::new(Ty::expr(Expr::ident(name.clone()))),
            mutability: None,
            lifetime: None,
        }),
    );
    let def = ItemDefStruct::new(name.clone(), vec![field]);
    let file = File {
        path: "recursive_ref.fp".into(),
        attrs: Vec::new(),
        collected_items: Vec::new(),
        items: vec![Item::from(ItemKind::DefStruct(def))],
    };

    assert!(
        !file_has_errors(file),
        "reference recursion should be allowed"
    );
}

#[test]
fn recursive_struct_allowed_with_arc() {
    let name = Ident::new("NodeArc");
    let field = StructuralField::new(
        Ident::new("next"),
        make_arc_type(Ty::expr(Expr::ident(name.clone()))),
    );
    let def = ItemDefStruct::new(name.clone(), vec![field]);
    let file = File {
        path: "recursive_arc.fp".into(),
        attrs: Vec::new(),
        collected_items: Vec::new(),
        items: vec![Item::from(ItemKind::DefStruct(def))],
    };

    assert!(!file_has_errors(file), "arc recursion should be allowed");
}

#[test]
fn recursive_struct_allowed_with_rc() {
    let name = Ident::new("NodeRc");
    let field = StructuralField::new(
        Ident::new("next"),
        make_rc_type(Ty::expr(Expr::ident(name.clone()))),
    );
    let def = ItemDefStruct::new(name.clone(), vec![field]);
    let file = File {
        path: "recursive_rc.fp".into(),
        attrs: Vec::new(),
        collected_items: Vec::new(),
        items: vec![Item::from(ItemKind::DefStruct(def))],
    };

    assert!(!file_has_errors(file), "rc recursion should be allowed");
}

#[test]
fn recursive_struct_allowed_with_weak() {
    let name = Ident::new("NodeWeak");
    let field = StructuralField::new(
        Ident::new("next"),
        make_weak_type(Ty::expr(Expr::ident(name.clone()))),
    );
    let def = ItemDefStruct::new(name.clone(), vec![field]);
    let file = File {
        path: "recursive_weak.fp".into(),
        attrs: Vec::new(),
        collected_items: Vec::new(),
        items: vec![Item::from(ItemKind::DefStruct(def))],
    };

    assert!(!file_has_errors(file), "weak recursion should be allowed");
}
