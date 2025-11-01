use crate::ast::{
    BExpr, Expr, ExprArray, ExprArrayRepeat, ExprBlock, ExprInvoke, ExprInvokeTarget, ExprKind,
    ExprTuple, Ident, Locator, Path,
};
use crate::common_enum;
use crate::common_struct;
use crate::intrinsics::{IntrinsicCall, IntrinsicCallKind, IntrinsicCallPayload};

common_struct! {
    pub struct ExprIntrinsicContainerEntry {
        pub key: Expr,
        pub value: Expr,
    }
}

common_enum! {
    pub enum ExprIntrinsicContainer {
        VecElements {
            elements: Vec<Expr>,
        },
        VecRepeat {
            elem: BExpr,
            len: BExpr,
        },
        HashMapEntries {
            entries: Vec<ExprIntrinsicContainerEntry>,
        },
    }
}

impl ExprIntrinsicContainer {
    pub fn for_each_expr_mut<F>(&mut self, mut f: F)
    where
        F: FnMut(&mut Expr),
    {
        match self {
            ExprIntrinsicContainer::VecElements { elements } => {
                for element in elements {
                    f(element);
                }
            }
            ExprIntrinsicContainer::VecRepeat { elem, len } => {
                f(elem.as_mut());
                f(len.as_mut());
            }
            ExprIntrinsicContainer::HashMapEntries { entries } => {
                for entry in entries {
                    f(&mut entry.key);
                    f(&mut entry.value);
                }
            }
        }
    }

    pub fn into_const_expr(self) -> Expr {
        match self {
            ExprIntrinsicContainer::VecElements { elements } => {
                let array_expr = ExprKind::Array(ExprArray { values: elements }).into();
                make_const_collection_call(vec_from_call(array_expr))
            }
            ExprIntrinsicContainer::VecRepeat { elem, len } => {
                let repeat = ExprArrayRepeat { elem, len };
                let array_expr = ExprKind::ArrayRepeat(repeat).into();
                make_const_collection_call(vec_from_call(array_expr))
            }
            ExprIntrinsicContainer::HashMapEntries { entries } => {
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
            return Some(ExprIntrinsicContainer::VecElements {
                elements: Vec::new(),
            });
        }

        if ends_with(&segments, &["Vec", "with_capacity"]) {
            if invoke.args.len() != 1 {
                return None;
            }
            return Some(ExprIntrinsicContainer::VecElements {
                elements: Vec::new(),
            });
        }

        if ends_with(&segments, &["Vec", "from"]) {
            if invoke.args.len() != 1 {
                return None;
            }
            return match invoke.args[0].kind() {
                ExprKind::Array(array) => Some(ExprIntrinsicContainer::VecElements {
                    elements: array.values.clone(),
                }),
                ExprKind::ArrayRepeat(repeat) => Some(ExprIntrinsicContainer::VecRepeat {
                    elem: repeat.elem.clone(),
                    len: repeat.len.clone(),
                }),
                _ => None,
            };
        }

        if ends_with(&segments, &["HashMap", "new"]) {
            return Some(ExprIntrinsicContainer::HashMapEntries {
                entries: Vec::new(),
            });
        }

        if ends_with(&segments, &["HashMap", "with_capacity"]) {
            if invoke.args.len() != 1 {
                return None;
            }
            return Some(ExprIntrinsicContainer::HashMapEntries {
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
                                entries.push(ExprIntrinsicContainerEntry {
                                    key: tuple.values[0].clone(),
                                    value: tuple.values[1].clone(),
                                });
                            }
                            ExprKind::Array(inner) if inner.values.len() == 2 => {
                                entries.push(ExprIntrinsicContainerEntry {
                                    key: inner.values[0].clone(),
                                    value: inner.values[1].clone(),
                                });
                            }
                            _ => return None,
                        }
                    }
                    Some(ExprIntrinsicContainer::HashMapEntries { entries })
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

#[cfg(test)]
mod tests {
    use super::*;

    fn ident_path<'a>(segs: impl IntoIterator<Item = &'a str>) -> Locator {
        Locator::path(Path::new(segs.into_iter().map(Ident::new).collect()))
    }

    #[test]
    fn vec_new_becomes_empty_elements() {
        let invoke = ExprInvoke {
            target: ExprInvokeTarget::Function(ident_path(["Vec", "new"])),
            args: vec![],
        };
        let got = ExprIntrinsicContainer::from_invoke(&invoke);
        match got {
            Some(ExprIntrinsicContainer::VecElements { elements }) => assert!(elements.is_empty()),
            other => panic!("unexpected: {:?}", other),
        }
    }

    #[test]
    fn vec_from_array_maps_to_elements() {
        let array = ExprArray { values: vec![Expr::unit(), Expr::unit()] };
        let invoke = ExprInvoke {
            target: ExprInvokeTarget::Function(ident_path(["Vec", "from"])),
            args: vec![ExprKind::Array(array).into()],
        };
        let got = ExprIntrinsicContainer::from_invoke(&invoke);
        match got {
            Some(ExprIntrinsicContainer::VecElements { elements }) => assert_eq!(elements.len(), 2),
            other => panic!("unexpected: {:?}", other),
        }
    }

    #[test]
    fn vec_from_repeat_maps_to_repeat_variant() {
        let repeat = ExprArrayRepeat { elem: Expr::unit().into(), len: Expr::unit().into() };
        let invoke = ExprInvoke {
            target: ExprInvokeTarget::Function(ident_path(["Vec", "from"])),
            args: vec![ExprKind::ArrayRepeat(repeat.clone()).into()],
        };
        let got = ExprIntrinsicContainer::from_invoke(&invoke);
        match got {
            Some(ExprIntrinsicContainer::VecRepeat { .. }) => {}
            other => panic!("unexpected: {:?}", other),
        }
    }

    #[test]
    fn hashmap_from_array_of_tuples_maps_entries() {
        let tuple1 = ExprKind::Tuple(ExprTuple { values: vec![Expr::unit(), Expr::unit()] }).into();
        let tuple2 = ExprKind::Tuple(ExprTuple { values: vec![Expr::unit(), Expr::unit()] }).into();
        let array = ExprArray { values: vec![tuple1, tuple2] };
        let invoke = ExprInvoke {
            target: ExprInvokeTarget::Function(ident_path(["HashMap", "from"])),
            args: vec![ExprKind::Array(array).into()],
        };
        let got = ExprIntrinsicContainer::from_invoke(&invoke);
        match got {
            Some(ExprIntrinsicContainer::HashMapEntries { entries }) => assert_eq!(entries.len(), 2),
            other => panic!("unexpected: {:?}", other),
        }
    }

    #[test]
    fn into_const_expr_wraps_vec_from() {
        let container = ExprIntrinsicContainer::VecElements { elements: vec![Expr::unit()] };
        let expr = container.into_const_expr();
        // Expect an intrinsic const block wrapping a Vec::from(array)
        match expr.kind() {
            ExprKind::IntrinsicCall(call) => match &call.payload {
                IntrinsicCallPayload::Args { args } => {
                    assert_eq!(args.len(), 1);
                    let inner = match args[0].kind() {
                        ExprKind::Block(block) => block.last_expr().cloned().unwrap_or_else(Expr::unit),
                        other => args[0].clone(),
                    };
                    match inner.kind() {
                        ExprKind::Invoke(invoke) => match &invoke.target {
                            ExprInvokeTarget::Function(loc) => {
                                let segs = super::locator_segments(loc);
                                assert!(super::ends_with(&segs, &["Vec", "from"]))
                            }
                            _ => panic!("not a function call"),
                        },
                        other => panic!("unexpected inner kind: {:?}", other),
                    }
                }
                _ => panic!("unexpected payload"),
            },
            other => panic!("unexpected expr: {:?}", other),
        }
    }
}
