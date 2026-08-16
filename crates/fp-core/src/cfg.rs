use crate::ast::{self, File, ItemKind};
use cfg_expr::targets::{Family, TargetInfo, get_builtin_target_by_triple};
use cfg_expr::{Expression, Predicate};
use std::collections::BTreeSet;
use target_lexicon::HOST;
use thiserror::Error;

#[derive(Debug, Error)]
#[error("unsupported Rust target triple '{triple}'")]
pub struct TargetEnvError {
    triple: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TargetEnv {
    pub os: String,
    pub lang: Option<String>,
    triple: String,
    target: &'static TargetInfo,
    features: BTreeSet<String>,
}

impl TargetEnv {
    pub fn host() -> Self {
        Self::from_triple(None).expect("target-lexicon host triple must be known by cfg-expr")
    }

    pub fn from_triple(triple: Option<&str>) -> Result<Self, TargetEnvError> {
        let triple = triple
            .map(str::to_owned)
            .unwrap_or_else(|| HOST.to_string());
        let target = get_builtin_target_by_triple(&triple).ok_or_else(|| TargetEnvError {
            triple: triple.clone(),
        })?;
        Ok(Self {
            os: target
                .os
                .as_ref()
                .map(|os| os.to_string())
                .unwrap_or_else(|| "unknown".to_string()),
            lang: None,
            triple,
            target,
            features: BTreeSet::new(),
        })
    }

    pub fn with_features<I, S>(mut self, features: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        self.features = features.into_iter().map(Into::into).collect();
        self
    }

    pub fn target_triple(&self) -> &str {
        &self.triple
    }

    pub fn has_feature(&self, feature: &str) -> bool {
        self.features.contains(feature)
    }
}

pub fn filter_items_in_file(file: &mut File, env: &TargetEnv) {
    filter_items(&mut file.items, env);
}

fn filter_items(items: &mut Vec<ast::Item>, env: &TargetEnv) {
    items.retain(|item| item_enabled_by_cfg(item, env));
    for item in items.iter_mut() {
        filter_item(item, env);
    }
}

fn filter_item(item: &mut ast::Item, env: &TargetEnv) {
    if let ItemKind::Module(module) = item.kind_mut() {
        filter_items(&mut module.items, env);
    }
}

pub fn item_enabled_by_cfg(item: &ast::Item, env: &TargetEnv) -> bool {
    let Some(attrs) = item_attrs(item) else {
        return true;
    };
    cfg_attrs_enabled(attrs, env)
}

fn item_attrs(item: &ast::Item) -> Option<&[ast::Attribute]> {
    match item.kind() {
        ItemKind::Module(module) => Some(&module.attrs),
        ItemKind::DefStruct(def) => Some(&def.attrs),
        ItemKind::DefStructural(def) => Some(&def.attrs),
        ItemKind::DefEnum(def) => Some(&def.attrs),
        ItemKind::DefType(def) => Some(&def.attrs),
        ItemKind::OpaqueType(def) => Some(&def.attrs),
        ItemKind::DefConst(def) => Some(&def.attrs),
        ItemKind::DefStatic(def) => Some(&def.attrs),
        ItemKind::DefFunction(def) => Some(&def.attrs),
        ItemKind::DefTrait(def) => Some(&def.attrs),
        ItemKind::DeclFunction(decl) => Some(&decl.attrs),
        ItemKind::Import(import) => Some(&import.attrs),
        ItemKind::Impl(impl_block) => Some(&impl_block.attrs),
        _ => None,
    }
}

fn cfg_attrs_enabled(attrs: &[ast::Attribute], env: &TargetEnv) -> bool {
    for attr in attrs {
        let ast::AttrMeta::List(list) = &attr.meta else {
            continue;
        };
        if list.name.last().as_str() != "cfg" {
            continue;
        }
        if !cfg_list_items_enabled(&list.items, env) {
            return false;
        }
    }
    true
}

fn cfg_list_items_enabled(items: &[ast::AttrMeta], env: &TargetEnv) -> bool {
    !items.is_empty() && items.iter().all(|item| cfg_meta_enabled(item, env))
}

/// Evaluates a single `cfg` predicate (the parsed inner content of
/// `#[cfg(...)]`, or of a `cfg!(...)` macro invocation, which uses the
/// identical grammar). `pub` so it can also be reused to normalize `cfg!()`
/// as an expression-position macro, not just attribute-position filtering.
pub fn cfg_meta_enabled(meta: &ast::AttrMeta, env: &TargetEnv) -> bool {
    let Some(source) = cfg_expression(meta) else {
        return false;
    };
    let Ok(expression) = Expression::parse(&source) else {
        return false;
    };
    expression.eval(|predicate| predicate_enabled(predicate, env))
}

fn cfg_expression(meta: &ast::AttrMeta) -> Option<String> {
    match meta {
        ast::AttrMeta::Path(path) => Some(path.last().as_str().to_owned()),
        ast::AttrMeta::NameValue(name_value) => {
            let value = string_literal_value(&name_value.value)?;
            Some(format!("{} = {value:?}", name_value.name.last().as_str()))
        }
        ast::AttrMeta::List(list) => {
            let items = list
                .items
                .iter()
                .map(cfg_expression)
                .collect::<Option<Vec<_>>>()?;
            Some(format!(
                "{}({})",
                list.name.last().as_str(),
                items.join(", ")
            ))
        }
    }
}

fn predicate_enabled(predicate: &Predicate<'_>, env: &TargetEnv) -> bool {
    match predicate {
        Predicate::Target(target) => target.matches(env.target),
        Predicate::Feature(feature) => env.has_feature(feature),
        Predicate::Flag(flag) => match *flag {
            "unix" => env.target.families.contains(&Family::unix),
            "windows" => env.target.families.contains(&Family::windows),
            "wasm" => env.target.families.contains(&Family::wasm),
            _ => false,
        },
        Predicate::KeyValue { key, val } => {
            *key == "target_lang" && env.lang.as_deref() == Some(*val)
        }
        Predicate::Test
        | Predicate::DebugAssertions
        | Predicate::ProcMacro
        | Predicate::TargetFeature(_) => false,
    }
}

fn string_literal_value(expr: &ast::Expr) -> Option<String> {
    if let ast::ExprKind::Value(value) = expr.kind()
        && let ast::Value::String(string) = &**value
    {
        return Some(string.value.clone());
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{
        AttrMeta, AttrMetaList, AttrMetaNameValue, AttrStyle, Attribute, Expr, ExprBlock, File,
        Ident, Item, ItemDefFunction, ItemKind, Path, Value,
    };
    use std::path::PathBuf;

    fn path(name: &str) -> Path {
        Path::from_ident(Ident::new(name))
    }

    fn cfg_attribute(meta: AttrMeta) -> Attribute {
        Attribute {
            style: AttrStyle::Outer,
            meta: AttrMeta::List(AttrMetaList {
                name: path("cfg"),
                items: vec![meta],
            }),
        }
    }

    fn cfg_feature(name: &str) -> AttrMeta {
        AttrMeta::NameValue(AttrMetaNameValue {
            name: path("feature"),
            value: Expr::value(Value::string(name.to_string())).into(),
        })
    }

    fn cfg_target_os(name: &str) -> AttrMeta {
        AttrMeta::NameValue(AttrMetaNameValue {
            name: path("target_os"),
            value: Expr::value(Value::string(name.to_string())).into(),
        })
    }

    fn function(name: &str, cfg: AttrMeta) -> Item {
        let mut item = Item::from(ItemKind::DefFunction(ItemDefFunction::new_simple(
            Ident::new(name),
            ExprBlock::new_expr(Expr::value(Value::unit())),
        )));
        let ItemKind::DefFunction(def) = item.kind_mut() else {
            unreachable!("constructed function item has the wrong kind");
        };
        def.attrs.push(cfg_attribute(cfg));
        item
    }

    #[test]
    fn filters_items_by_target_and_feature_set() -> Result<(), TargetEnvError> {
        let env =
            TargetEnv::from_triple(Some("x86_64-pc-windows-msvc"))?.with_features(["selected"]);
        let mut file = File {
            path: PathBuf::from("lib.rs"),
            attrs: Vec::new(),
            collected_items: Vec::new(),
            items: vec![
                function(
                    "selected_windows",
                    AttrMeta::List(AttrMetaList {
                        name: path("all"),
                        items: vec![AttrMeta::Path(path("windows")), cfg_feature("selected")],
                    }),
                ),
                function("other_feature", cfg_feature("other")),
                function("linux_only", cfg_target_os("linux")),
            ],
        };

        filter_items_in_file(&mut file, &env);

        let names = file
            .items
            .iter()
            .filter_map(|item| match item.kind() {
                ItemKind::DefFunction(def) => Some(def.name.name.as_str()),
                _ => None,
            })
            .collect::<Vec<_>>();
        assert_eq!(names, vec!["selected_windows"]);
        Ok(())
    }

    #[test]
    fn rejects_unknown_target_triples() {
        assert!(TargetEnv::from_triple(Some("unknown-target")).is_err());
    }
}
