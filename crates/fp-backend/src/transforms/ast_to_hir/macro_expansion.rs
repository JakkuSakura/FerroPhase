use super::*;

impl AstToHirLowerer {
    pub(super) fn expand_item_macros(
        &mut self,
        items: Vec<fp_core::ast::package::PackageItem>,
    ) -> Vec<fp_core::ast::package::PackageItem> {
        let mut defs: HashMap<String, fp_core::ast::MacroRulesDef> = HashMap::new();
        let mut depths: HashMap<String, usize> = HashMap::new();
        let expanded = {
            let Some(normalizer) = self.intrinsic_normalizer.as_deref() else {
                return items;
            };
            items
                .into_iter()
                .flat_map(|package_item| {
                    let module_path = package_item.module_path;
                    let depth = module_path.segments.len();
                    self.expand_item_macros_in_item(
                        package_item.item,
                        normalizer,
                        &mut defs,
                        &mut depths,
                        depth,
                    )
                    .into_iter()
                    .map(move |item| fp_core::ast::package::PackageItem {
                        module_path: module_path.clone(),
                        item,
                    })
                    .collect::<Vec<_>>()
                })
                .collect()
        };
        if let Some(normalizer) = self.intrinsic_normalizer.as_mut() {
            normalizer.set_macro_rules_defs(defs);
        }
        expanded
    }

    fn expand_item_macros_in_item(
        &self,
        item: ast::Item,
        normalizer: &dyn IntrinsicNormalizer,
        defs: &mut HashMap<String, fp_core::ast::MacroRulesDef>,
        depths: &mut HashMap<String, usize>,
        depth: usize,
    ) -> Vec<ast::Item> {
        match item.kind {
            ItemKind::DefStruct(_) => {
                let mut derived = normalizer.expand_derive(&item);
                let mut items = Vec::with_capacity(1 + derived.len());
                items.push(item);
                items.append(&mut derived);
                items
            }
            ItemKind::Macro(ref item_macro) if item_macro.declared_name.is_some() => {
                let name = item_macro
                    .declared_name
                    .as_ref()
                    .expect("declared_name.is_some() checked above")
                    .as_str()
                    .to_string();
                let keep = match depths.get(&name) {
                    Some(&existing_depth) => {
                        normalizer.prefer_macro_rules_def(existing_depth, depth)
                    }
                    None => true,
                };
                if keep {
                    let def =
                        normalizer.parse_macro_rules_def(&name, &item_macro.invocation.token_trees);
                    defs.insert(name.clone(), def);
                    depths.insert(name, depth);
                }
                vec![item]
            }
            ItemKind::Macro(ref item_macro) => {
                match normalizer.expand_item_macro(&item_macro.invocation, defs) {
                    Some(expanded) => expanded
                        .into_iter()
                        .flat_map(|expanded_item| {
                            self.expand_item_macros_in_item(
                                expanded_item,
                                normalizer,
                                defs,
                                depths,
                                depth,
                            )
                        })
                        .collect(),
                    None => vec![item],
                }
            }
            ItemKind::Module(mut module) => {
                module.items = module
                    .items
                    .into_iter()
                    .flat_map(|inner| {
                        self.expand_item_macros_in_item(inner, normalizer, defs, depths, depth + 1)
                    })
                    .collect();
                vec![ast::Item::from(ItemKind::Module(module))]
            }
            ItemKind::Impl(mut impl_block) => {
                impl_block.items = impl_block
                    .items
                    .into_iter()
                    .flat_map(|inner| {
                        self.expand_item_macros_in_item(inner, normalizer, defs, depths, depth)
                    })
                    .collect();
                vec![ast::Item::from(ItemKind::Impl(impl_block))]
            }
            kind => vec![ast::Item { kind, ..item }],
        }
    }
}
