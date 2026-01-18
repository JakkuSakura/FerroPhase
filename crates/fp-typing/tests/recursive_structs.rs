use fp_core::ast::*;
use fp_typing::AstTypeInferencer;

fn make_box_type(inner: Ty) -> Ty {
    let segment = ParameterPathSegment::new(Ident::new("Box"), vec![inner]);
    let path = ParameterPath::new(vec![segment]);
    Ty::expr(Expr::locator(Locator::parameter_path(path)))
}

fn make_arc_type(inner: Ty) -> Ty {
    let segment = ParameterPathSegment::new(Ident::new("Arc"), vec![inner]);
    let path = ParameterPath::new(vec![segment]);
    Ty::expr(Expr::locator(Locator::parameter_path(path)))
}

fn make_rc_type(inner: Ty) -> Ty {
    let segment = ParameterPathSegment::new(Ident::new("Rc"), vec![inner]);
    let path = ParameterPath::new(vec![segment]);
    Ty::expr(Expr::locator(Locator::parameter_path(path)))
}

fn make_weak_type(inner: Ty) -> Ty {
    let segment = ParameterPathSegment::new(Ident::new("Weak"), vec![inner]);
    let path = ParameterPath::new(vec![segment]);
    Ty::expr(Expr::locator(Locator::parameter_path(path)))
}

#[test]
fn recursive_struct_rejected_without_box() {
    let name = Ident::new("Node");
    let field = StructuralField::new(Ident::new("next"), Ty::expr(Expr::ident(name.clone())));
    let def = ItemDefStruct::new(name.clone(), vec![field]);
    let file = File {
        path: "recursive.fp".into(),
        items: vec![Item::from(ItemKind::DefStruct(def))],
    };

    let mut node = Node::new(NodeKind::File(file));
    let mut typer = AstTypeInferencer::new();
    let outcome = typer.infer(&mut node).expect("infer");
    assert!(outcome.has_errors, "expected recursion error");
}

#[test]
fn recursive_struct_allowed_with_box() {
    let name = Ident::new("Node");
    let boxed = make_box_type(Ty::expr(Expr::ident(name.clone())));
    let field = StructuralField::new(Ident::new("next"), boxed);
    let def = ItemDefStruct::new(name.clone(), vec![field]);
    let file = File {
        path: "recursive_box.fp".into(),
        items: vec![Item::from(ItemKind::DefStruct(def))],
    };

    let mut node = Node::new(NodeKind::File(file));
    let mut typer = AstTypeInferencer::new();
    let outcome = typer.infer(&mut node).expect("infer");
    assert!(!outcome.has_errors, "boxed recursion should be allowed");
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
        items: vec![Item::from(ItemKind::DefStruct(def))],
    };

    let mut node = Node::new(NodeKind::File(file));
    let mut typer = AstTypeInferencer::new();
    let outcome = typer.infer(&mut node).expect("infer");
    assert!(!outcome.has_errors, "vec recursion should be allowed");
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
        items: vec![Item::from(ItemKind::DefStruct(def))],
    };

    let mut node = Node::new(NodeKind::File(file));
    let mut typer = AstTypeInferencer::new();
    let outcome = typer.infer(&mut node).expect("infer");
    assert!(!outcome.has_errors, "reference recursion should be allowed");
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
        items: vec![Item::from(ItemKind::DefStruct(def))],
    };

    let mut node = Node::new(NodeKind::File(file));
    let mut typer = AstTypeInferencer::new();
    let outcome = typer.infer(&mut node).expect("infer");
    assert!(!outcome.has_errors, "arc recursion should be allowed");
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
        items: vec![Item::from(ItemKind::DefStruct(def))],
    };

    let mut node = Node::new(NodeKind::File(file));
    let mut typer = AstTypeInferencer::new();
    let outcome = typer.infer(&mut node).expect("infer");
    assert!(!outcome.has_errors, "rc recursion should be allowed");
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
        items: vec![Item::from(ItemKind::DefStruct(def))],
    };

    let mut node = Node::new(NodeKind::File(file));
    let mut typer = AstTypeInferencer::new();
    let outcome = typer.infer(&mut node).expect("infer");
    assert!(!outcome.has_errors, "weak recursion should be allowed");
}
