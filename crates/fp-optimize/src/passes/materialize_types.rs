use fp_core::ast::{
    Expr, ExprKind, Ident, Item, ItemDefEnum, ItemDefStructural, ItemKind, Locator, Node, NodeKind,
    Path, StructuralField, Ty, TypeBinaryOpKind, TypeEnum, TypeStructural, Value, ValueNone,
};
use fp_core::error::Result;
use std::collections::{HashMap, HashSet};

/// Materialize structural type aliases and basic type arithmetic into concrete
/// named struct definitions.
///
/// This pass exists primarily for backends like Rust that cannot represent
/// anonymous structural types (e.g. `struct { a: i64 }`) or symbolic type-level
/// arithmetic (e.g. `A + B`) directly.
pub struct MaterializeTypesOptions {
    pub include_unions: bool,
}

pub fn materialize_structural_types(node: &mut Node) -> Result<()> {
    materialize_structural_types_with(
        node,
        &MaterializeTypesOptions {
            include_unions: false,
        },
    )
}

pub fn materialize_structural_types_with_unions(node: &mut Node) -> Result<()> {
    materialize_structural_types_with(
        node,
        &MaterializeTypesOptions {
            include_unions: true,
        },
    )
}

fn materialize_structural_types_with(
    node: &mut Node,
    options: &MaterializeTypesOptions,
) -> Result<()> {
    match node.kind_mut() {
        NodeKind::File(file) => materialize_items(&mut file.items, options),
        NodeKind::Item(item) => materialize_item(item, options),
        NodeKind::Expr(_) | NodeKind::Schema(_) | NodeKind::Query(_) | NodeKind::Workspace(_) => {
            Ok(())
        }
    }
}

fn materialize_item(item: &mut Item, options: &MaterializeTypesOptions) -> Result<()> {
    match item.kind_mut() {
        ItemKind::Module(module) => materialize_items(&mut module.items, options),
        ItemKind::Impl(impl_) => materialize_items(&mut impl_.items, options),
        ItemKind::DefTrait(def) => materialize_items(&mut def.items, options),
        ItemKind::DefFunction(func) => {
            // Materialize items inside function bodies (nested items).
            if let fp_core::ast::ExprKind::Block(block) = func.body.kind_mut() {
                for stmt in &mut block.stmts {
                    if let fp_core::ast::BlockStmt::Item(item) = stmt {
                        materialize_item(item.as_mut(), options)?;
                    }
                }
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

fn materialize_items(items: &mut Vec<Item>, options: &MaterializeTypesOptions) -> Result<()> {
    // Process nested modules first.
    for item in items.iter_mut() {
        materialize_item(item, options)?;
    }

    // Build a best-effort environment of known structural shapes.
    let mut shapes: HashMap<String, Vec<StructuralField>> = HashMap::new();

    let mut seed_shapes = |item: &Item| match item.kind() {
        ItemKind::DefStruct(def) => {
            shapes.insert(def.name.as_str().to_string(), def.value.fields.clone());
        }
        ItemKind::DefStructural(def) => {
            shapes.insert(def.name.as_str().to_string(), def.value.fields.clone());
        }
        ItemKind::DefType(def) => {
            if let Ty::Structural(st) = &def.value {
                shapes.insert(def.name.as_str().to_string(), st.fields.clone());
            }
        }
        _ => {}
    };
    for item in items.iter() {
        seed_shapes(item);
    }

    // Fixed point: materialize aliases once their operands become known.
    loop {
        let mut changed = false;
        for item in items.iter_mut() {
            let ItemKind::DefType(def) = item.kind_mut() else {
                continue;
            };

            if matches!(def.value, Ty::Structural(_)) {
                // Direct structural aliases are handled below.
            }

            if let Ty::TypeBinaryOp(op) = &def.value {
                if options.include_unions && matches!(op.kind, TypeBinaryOpKind::Union) {
                    if let (Some(lhs), Some(rhs)) = (
                        union_variant_from_ty(&op.lhs),
                        union_variant_from_ty(&op.rhs),
                    ) {
                        let enum_def = ItemDefEnum {
                            visibility: def.visibility.clone(),
                            name: def.name.clone(),
                            value: TypeEnum {
                                name: def.name.clone(),
                                generics_params: Vec::new(),
                                variants: vec![lhs, rhs],
                            },
                        };
                        *item = Item::new(ItemKind::DefEnum(enum_def));
                        changed = true;
                        continue;
                    }
                }
            }

            if let Some(fields) =
                resolve_structural_fields(&def.value, &shapes, &mut HashSet::new())
            {
                let def_structural = ItemDefStructural {
                    visibility: def.visibility.clone(),
                    name: def.name.clone(),
                    value: TypeStructural {
                        fields: fields.clone(),
                    },
                };
                let key = def_structural.name.as_str().to_string();
                *item = Item::new(ItemKind::DefStructural(def_structural));
                shapes.insert(key, fields);
                changed = true;
            }
        }

        if !changed {
            break;
        }
    }

    Ok(())
}

fn union_variant_from_ty(ty: &Ty) -> Option<fp_core::ast::EnumTypeVariant> {
    let (ident, payload) = match ty {
        Ty::Struct(struct_ty) => (Some(struct_ty.name.clone()), Some(Ty::expr(Expr::ident(struct_ty.name.clone())))),
        Ty::Expr(expr) => match expr.kind() {
            ExprKind::Locator(locator) => match locator {
                Locator::Path(path) => {
                    let ident = path.segments.last().cloned();
                    (ident.clone(), ident.map(|name| Ty::expr(Expr::ident(name))))
                }
                Locator::Ident(ident) => (Some(ident.clone()), Some(Ty::expr(Expr::ident(ident.clone())))),
                Locator::ParameterPath(path) => {
                    let ident = path.segments.last().map(|seg| seg.ident.clone());
                    (ident.clone(), ident.map(|name| Ty::expr(Expr::ident(name))))
                }
            },
            _ => (None, None),
        },
        Ty::Value(value) => match value.value.as_ref() {
            Value::None(ValueNone) => (Some(Ident::new("None")), Some(Ty::unit())),
            _ => (None, None),
        },
        _ => (None, None),
    };

    let ident = ident?;
    let payload = payload?;

    Some(fp_core::ast::EnumTypeVariant {
        name: ident,
        value: payload,
        discriminant: None,
    })
}

fn resolve_structural_fields(
    ty: &Ty,
    shapes: &HashMap<String, Vec<StructuralField>>,
    visiting: &mut HashSet<String>,
) -> Option<Vec<StructuralField>> {
    match ty {
        Ty::Struct(s) => Some(s.fields.clone()),
        Ty::Structural(s) => Some(s.fields.clone()),
        Ty::TypeBinaryOp(op) => {
            let lhs = resolve_structural_fields(op.lhs.as_ref(), shapes, visiting)?;
            let rhs = resolve_structural_fields(op.rhs.as_ref(), shapes, visiting)?;
            match op.kind {
                TypeBinaryOpKind::Add => Some(merge_structural_fields(lhs, rhs)),
                TypeBinaryOpKind::Intersect => Some(intersect_structural_fields(lhs, rhs)),
                TypeBinaryOpKind::Subtract => Some(subtract_structural_fields(lhs, rhs)),
                TypeBinaryOpKind::Union => None,
            }
        }
        Ty::Expr(expr) => match expr.kind() {
            ExprKind::Locator(locator) => {
                let key = locator_key(locator);
                if let Some(key) = key {
                    if visiting.contains(&key) {
                        return None;
                    }
                    if let Some(shape) = shapes.get(&key) {
                        return Some(shape.clone());
                    }
                    visiting.insert(key);
                }
                None
            }
            _ => None,
        },
        _ => None,
    }
}

fn locator_key(locator: &Locator) -> Option<String> {
    match locator {
        Locator::Ident(ident) => Some(ident.as_str().to_string()),
        Locator::Path(Path { segments }) => segments.last().map(|ident| ident.as_str().to_string()),
        Locator::ParameterPath(path) => path
            .segments
            .last()
            .map(|seg| seg.ident.as_str().to_string()),
    }
}

fn merge_structural_fields(
    mut lhs: Vec<StructuralField>,
    rhs: Vec<StructuralField>,
) -> Vec<StructuralField> {
    for rhs_field in rhs {
        if lhs
            .iter()
            .any(|field| field.name.as_str() == rhs_field.name.as_str())
        {
            continue;
        }
        lhs.push(rhs_field);
    }
    lhs
}

fn intersect_structural_fields(
    lhs: Vec<StructuralField>,
    rhs: Vec<StructuralField>,
) -> Vec<StructuralField> {
    let rhs_names: HashSet<&str> = rhs.iter().map(|f| f.name.as_str()).collect();
    lhs.into_iter()
        .filter(|f| rhs_names.contains(f.name.as_str()))
        .collect()
}

fn subtract_structural_fields(
    lhs: Vec<StructuralField>,
    rhs: Vec<StructuralField>,
) -> Vec<StructuralField> {
    let rhs_names: HashSet<&str> = rhs.iter().map(|f| f.name.as_str()).collect();
    lhs.into_iter()
        .filter(|f| !rhs_names.contains(f.name.as_str()))
        .collect()
}
