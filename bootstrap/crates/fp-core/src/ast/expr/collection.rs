use crate::ast::{
    BExpr, Expr, ExprArray, ExprArrayRepeat, ExprBlock, ExprConstBlock, ExprInvoke,
    ExprInvokeTarget, ExprKind, ExprTuple, Ident, Name, Path,
};
use crate::common_struct;
use crate::span::Span;

common_struct! {
    pub struct ExprIntrinsicContainerEntry {
        pub key: Expr,
        pub value: Expr,
    }
}
impl ExprIntrinsicContainerEntry {
    pub fn span(&self) -> Span {
        Span::union([self.key.span(), self.value.span()])
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
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

impl ExprIntrinsicContainer {
    pub fn span(&self) -> Span {
        match self {
            ExprIntrinsicContainer::VecElements { elements } => {
                Span::union(elements.iter().map(Expr::span))
            }
            ExprIntrinsicContainer::VecRepeat { elem, len } => {
                Span::union([elem.span(), len.span()])
            }
            ExprIntrinsicContainer::HashMapEntries { entries } => {
                Span::union(entries.iter().map(ExprIntrinsicContainerEntry::span))
            }
        }
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
                let array_expr =
                    ExprKind::Array(ExprArray { span: Span::null(), values: elements }).into();
                make_const_collection_call(vec_from_call(array_expr))
            }
            ExprIntrinsicContainer::VecRepeat { elem, len } => {
                let repeat = ExprArrayRepeat {
                    span: Span::null(),
                    elem,
                    len,
                };
                let array_expr = ExprKind::ArrayRepeat(repeat).into();
                make_const_collection_call(vec_from_call(array_expr))
            }
            ExprIntrinsicContainer::HashMapEntries { entries } => {
                let tuples: Vec<Expr> = entries
                    .into_iter()
                    .map(|entry| {
                        let tuple = ExprTuple {
                            span: Span::null(),
                            values: vec![entry.key, entry.value],
                        };
                        ExprKind::Tuple(tuple).into()
                    })
                    .collect();
                let array_expr =
                    ExprKind::Array(ExprArray { span: Span::null(), values: tuples }).into();
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
    ExprKind::ConstBlock(ExprConstBlock {
        span: Span::null(),
        expr: Expr::block(block).into(),
    })
    .into()
}

fn vec_from_call(iterable: Expr) -> Expr {
    make_function_call(&["Vec", "from"], vec![iterable])
}

fn hash_map_from_call(iterable: Expr) -> Expr {
    make_function_call(&["HashMap", "from"], vec![iterable])
}

fn make_function_call(path: &[&str], args: Vec<Expr>) -> Expr {
    let locator = Name::path(Path::new(
        path.iter().map(|segment| Ident::new(*segment)).collect(),
    ));
    ExprKind::Invoke(ExprInvoke {
        span: Span::null(),
        target: ExprInvokeTarget::Function(locator),
        args,
        kwargs: Vec::new(),
    })
    .into()
}

fn locator_segments(locator: &Name) -> Vec<&str> {
    match locator {
        Name::Ident(ident) => vec![ident.as_str()],
        Name::Path(path) => path.segments.iter().map(|seg| seg.as_str()).collect(),
        Name::ParameterPath(path) => {
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

    fn ident_path<'a>(segs: impl IntoIterator<Item = &'a str>) -> Name {
        Name::path(Path::new(segs.into_iter().map(Ident::new).collect()))
    }

    #[test]
    fn vec_new_becomes_empty_elements() {
        let invoke = ExprInvoke {
            span: Span::null(),
            target: ExprInvokeTarget::Function(ident_path(["Vec", "new"])),
            args: vec![],
            kwargs: Vec::new(),
        };
        let got = ExprIntrinsicContainer::from_invoke(&invoke);
        match got {
            Some(ExprIntrinsicContainer::VecElements { elements }) => assert!(elements.is_empty()),
            other => panic!("unexpected: {:?}", other),
        }
    }

    #[test]
    fn vec_from_array_maps_to_elements() {
        let array = ExprArray {
            span: Span::null(),
            values: vec![Expr::unit(), Expr::unit()],
        };
        let invoke = ExprInvoke {
            span: Span::null(),
            target: ExprInvokeTarget::Function(ident_path(["Vec", "from"])),
            args: vec![ExprKind::Array(array).into()],
            kwargs: Vec::new(),
        };
        let got = ExprIntrinsicContainer::from_invoke(&invoke);
        match got {
            Some(ExprIntrinsicContainer::VecElements { elements }) => assert_eq!(elements.len(), 2),
            other => panic!("unexpected: {:?}", other),
        }
    }

    #[test]
    fn vec_from_repeat_maps_to_repeat_variant() {
        let repeat = ExprArrayRepeat {
            span: Span::null(),
            elem: Expr::unit().into(),
            len: Expr::unit().into(),
        };
        let invoke = ExprInvoke {
            span: Span::null(),
            target: ExprInvokeTarget::Function(ident_path(["Vec", "from"])),
            args: vec![ExprKind::ArrayRepeat(repeat.clone()).into()],
            kwargs: Vec::new(),
        };
        let got = ExprIntrinsicContainer::from_invoke(&invoke);
        match got {
            Some(ExprIntrinsicContainer::VecRepeat { .. }) => {}
            other => panic!("unexpected: {:?}", other),
        }
    }

    #[test]
    fn hashmap_from_array_of_tuples_maps_entries() {
        let tuple1 = ExprKind::Tuple(ExprTuple {
            span: Span::null(),
            values: vec![Expr::unit(), Expr::unit()],
        })
        .into();
        let tuple2 = ExprKind::Tuple(ExprTuple {
            span: Span::null(),
            values: vec![Expr::unit(), Expr::unit()],
        })
        .into();
        let array = ExprArray {
            span: Span::null(),
            values: vec![tuple1, tuple2],
        };
        let invoke = ExprInvoke {
            span: Span::null(),
            target: ExprInvokeTarget::Function(ident_path(["HashMap", "from"])),
            args: vec![ExprKind::Array(array).into()],
            kwargs: Vec::new(),
        };
        let got = ExprIntrinsicContainer::from_invoke(&invoke);
        match got {
            Some(ExprIntrinsicContainer::HashMapEntries { entries }) => {
                assert_eq!(entries.len(), 2)
            }
            other => panic!("unexpected: {:?}", other),
        }
    }

    #[test]
    fn into_const_expr_wraps_vec_from() {
        let container = ExprIntrinsicContainer::VecElements {
            elements: vec![Expr::unit()],
        };
        let expr = container.into_const_expr();
        // Expect an intrinsic const block wrapping a Vec::from(array)
        match expr.kind() {
            ExprKind::ConstBlock(block) => match block.expr.kind() {
                ExprKind::IntrinsicCall(call) => {
                    assert_eq!(call.args.len(), 1);
                    let inner = match call.args[0].kind() {
                        ExprKind::Block(block) => {
                            block.last_expr().cloned().unwrap_or_else(Expr::unit)
                        }
                        _other => call.args[0].clone(),
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
                ExprKind::Invoke(invoke) => match &invoke.target {
                    ExprInvokeTarget::Function(loc) => {
                        let segs = super::locator_segments(loc);
                        assert!(super::ends_with(&segs, &["Vec", "from"]))
                    }
                    _ => panic!("not a function call"),
                },
                other => panic!("unexpected const block expr: {:?}", other),
            },
            ExprKind::IntrinsicCall(call) => {
                assert_eq!(call.args.len(), 1);
                let inner = match call.args[0].kind() {
                    ExprKind::Block(block) => block.last_expr().cloned().unwrap_or_else(Expr::unit),
                    _other => call.args[0].clone(),
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
            other => panic!("unexpected expr: {:?}", other),
        }
    }
}
