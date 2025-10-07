use crate::ast::{
    BExpr, Expr, ExprArray, ExprArrayRepeat, ExprBlock, ExprInvoke, ExprInvokeTarget, ExprKind,
    ExprTuple, Ident, Locator, Path,
};
use crate::common_enum;
use crate::common_struct;
use crate::intrinsics::{IntrinsicCall, IntrinsicCallKind, IntrinsicCallPayload};

common_struct! {
    pub struct ExprIntrinsicCollectionEntry {
        pub key: Expr,
        pub value: Expr,
    }
}

common_enum! {
    pub enum ExprIntrinsicCollection {
        VecElements {
            elements: Vec<Expr>,
        },
        VecRepeat {
            elem: BExpr,
            len: BExpr,
        },
        HashMapEntries {
            entries: Vec<ExprIntrinsicCollectionEntry>,
        },
    }
}

impl ExprIntrinsicCollection {
    pub fn for_each_expr_mut<F>(&mut self, mut f: F)
    where
        F: FnMut(&mut Expr),
    {
        match self {
            ExprIntrinsicCollection::VecElements { elements } => {
                for element in elements {
                    f(element);
                }
            }
            ExprIntrinsicCollection::VecRepeat { elem, len } => {
                f(elem.as_mut());
                f(len.as_mut());
            }
            ExprIntrinsicCollection::HashMapEntries { entries } => {
                for entry in entries {
                    f(&mut entry.key);
                    f(&mut entry.value);
                }
            }
        }
    }

    pub fn into_const_expr(self) -> Expr {
        match self {
            ExprIntrinsicCollection::VecElements { elements } => {
                let array_expr = ExprKind::Array(ExprArray { values: elements }).into();
                make_const_collection_call(vec_from_call(array_expr))
            }
            ExprIntrinsicCollection::VecRepeat { elem, len } => {
                let repeat = ExprArrayRepeat { elem, len };
                let array_expr = ExprKind::ArrayRepeat(repeat).into();
                make_const_collection_call(vec_from_call(array_expr))
            }
            ExprIntrinsicCollection::HashMapEntries { entries } => {
                let tuples: Vec<Expr> = entries
                    .into_iter()
                    .map(|entry| {
                        let tuple = ExprTuple {
                            values: vec![entry.key, entry.value],
                        };
                        ExprKind::Tuple(tuple).into()
                    })
                    .collect();
                let array_expr = ExprKind::Array(ExprArray { values: tuples }).into();
                make_const_collection_call(hash_map_from_call(array_expr))
            }
        }
    }

    pub fn from_invoke(invoke: &ExprInvoke) -> Option<Self> {
        let segments = locator_segments(match &invoke.target {
            ExprInvokeTarget::Function(locator) => locator,
            _ => return None,
        });

        if ends_with(&segments, &["Vec", "new"]) {
            return Some(ExprIntrinsicCollection::VecElements {
                elements: Vec::new(),
            });
        }

        if ends_with(&segments, &["Vec", "with_capacity"]) {
            if invoke.args.len() != 1 {
                return None;
            }
            return Some(ExprIntrinsicCollection::VecElements {
                elements: Vec::new(),
            });
        }

        if ends_with(&segments, &["Vec", "from"]) {
            if invoke.args.len() != 1 {
                return None;
            }
            return match invoke.args[0].kind() {
                ExprKind::Array(array) => Some(ExprIntrinsicCollection::VecElements {
                    elements: array.values.clone(),
                }),
                ExprKind::ArrayRepeat(repeat) => Some(ExprIntrinsicCollection::VecRepeat {
                    elem: repeat.elem.clone(),
                    len: repeat.len.clone(),
                }),
                _ => None,
            };
        }

        if ends_with(&segments, &["HashMap", "new"]) {
            return Some(ExprIntrinsicCollection::HashMapEntries {
                entries: Vec::new(),
            });
        }

        if ends_with(&segments, &["HashMap", "with_capacity"]) {
            if invoke.args.len() != 1 {
                return None;
            }
            return Some(ExprIntrinsicCollection::HashMapEntries {
                entries: Vec::new(),
            });
        }

        if ends_with(&segments, &["HashMap", "from"]) {
            if invoke.args.len() != 1 {
                return None;
            }
            return match invoke.args[0].kind() {
                ExprKind::Array(array) => {
                    let mut entries = Vec::with_capacity(array.values.len());
                    for value in &array.values {
                        match value.kind() {
                            ExprKind::Tuple(tuple) if tuple.values.len() == 2 => {
                                entries.push(ExprIntrinsicCollectionEntry {
                                    key: tuple.values[0].clone(),
                                    value: tuple.values[1].clone(),
                                });
                            }
                            ExprKind::Array(inner) if inner.values.len() == 2 => {
                                entries.push(ExprIntrinsicCollectionEntry {
                                    key: inner.values[0].clone(),
                                    value: inner.values[1].clone(),
                                });
                            }
                            _ => return None,
                        }
                    }
                    Some(ExprIntrinsicCollection::HashMapEntries { entries })
                }
                _ => None,
            };
        }

        None
    }
}

fn make_const_collection_call(expr: Expr) -> Expr {
    let block = ExprBlock::new_expr(expr);
    ExprKind::IntrinsicCall(IntrinsicCall::new(
        IntrinsicCallKind::ConstBlock,
        IntrinsicCallPayload::Args {
            args: vec![Expr::block(block)],
        },
    ))
    .into()
}

fn vec_from_call(iterable: Expr) -> Expr {
    make_function_call(&["Vec", "from"], vec![iterable])
}

fn hash_map_from_call(iterable: Expr) -> Expr {
    make_function_call(&["HashMap", "from"], vec![iterable])
}

fn make_function_call(path: &[&str], args: Vec<Expr>) -> Expr {
    let locator = Locator::path(Path::new(
        path.iter().map(|segment| Ident::new(*segment)).collect(),
    ));
    ExprKind::Invoke(ExprInvoke {
        target: ExprInvokeTarget::Function(locator),
        args,
    })
    .into()
}

fn locator_segments(locator: &Locator) -> Vec<&str> {
    match locator {
        Locator::Ident(ident) => vec![ident.as_str()],
        Locator::Path(path) => path.segments.iter().map(|seg| seg.as_str()).collect(),
        Locator::ParameterPath(path) => {
            path.segments.iter().map(|seg| seg.ident.as_str()).collect()
        }
    }
}

fn ends_with<'a>(segments: &[&'a str], suffix: &[&'a str]) -> bool {
    if segments.len() < suffix.len() {
        return false;
    }
    segments
        .iter()
        .rev()
        .zip(suffix.iter().rev())
        .all(|(segment, expected)| segment == expected)
}
