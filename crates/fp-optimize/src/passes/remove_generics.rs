/// Remove generic template functions from the AST after specialization.
/// Generic functions should have been fully specialized during const evaluation,
/// so the templates are no longer needed.
use fp_core::ast::{Item, ItemKind, Node, NodeKind};
use fp_core::error::Result;

pub fn remove_generic_templates(ast: &mut Node) -> Result<()> {
    match ast.kind_mut() {
        NodeKind::File(file) => {
            file.items.retain(|item| !is_generic_template(item));

            // Recursively process modules
            for item in &mut file.items {
                remove_generics_from_item(item)?;
            }
            Ok(())
        }
        NodeKind::Item(item) => {
            remove_generics_from_item(item)?;
            Ok(())
        }
        NodeKind::Expr(_) => Ok(()),
    }
}

fn remove_generics_from_item(item: &mut Item) -> Result<()> {
    match item.kind_mut() {
        ItemKind::Module(module) => {
            module.items.retain(|item| !is_generic_template(item));
            for child in &mut module.items {
                remove_generics_from_item(child)?;
            }
        }
        ItemKind::Impl(impl_block) => {
            impl_block.items.retain(|item| !is_generic_template(item));
            for child in &mut impl_block.items {
                remove_generics_from_item(child)?;
            }
        }
        _ => {}
    }
    Ok(())
}

fn is_generic_template(item: &Item) -> bool {
    match item.kind() {
        ItemKind::DefFunction(func) => !func.sig.generics_params.is_empty(),
        _ => false,
    }
}
