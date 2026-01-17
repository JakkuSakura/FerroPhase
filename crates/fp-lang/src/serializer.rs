use fp_core::ast::{
    AstSerializer, BlockStmt, Expr, ExprBlock, File, Item, ItemDefFunction, Node, Ty, Value,
    ValueFunction,
};
use fp_core::pretty::{PrettyOptions, pretty};

#[derive(Debug, Clone)]
pub struct PrettyAstSerializer {
    options: PrettyOptions,
}

impl PrettyAstSerializer {
    pub fn new() -> Self {
        Self {
            options: PrettyOptions::default(),
        }
    }

    pub fn with_options(options: PrettyOptions) -> Self {
        Self { options }
    }
}

impl Default for PrettyAstSerializer {
    fn default() -> Self {
        Self::new()
    }
}

impl AstSerializer for PrettyAstSerializer {
    fn serialize_node(&self, node: &Node) -> Result<String, fp_core::Error> {
        Ok(format!("{}", pretty(node, self.options.clone())))
    }

    fn serialize_expr(&self, node: &Expr) -> Result<String, fp_core::Error> {
        Ok(format!("{}", pretty(node, self.options.clone())))
    }

    fn serialize_item(&self, node: &Item) -> Result<String, fp_core::Error> {
        Ok(format!("{}", pretty(node, self.options.clone())))
    }

    fn serialize_file(&self, node: &File) -> Result<String, fp_core::Error> {
        Ok(format!("{}", pretty(node, self.options.clone())))
    }

    fn serialize_value(&self, node: &Value) -> Result<String, fp_core::Error> {
        Ok(format!("{node:?}"))
    }

    fn serialize_type(&self, node: &Ty) -> Result<String, fp_core::Error> {
        Ok(format!("{node:?}"))
    }

    fn serialize_block(&self, node: &ExprBlock) -> Result<String, fp_core::Error> {
        let mut out = String::new();
        for (idx, stmt) in node.stmts.iter().enumerate() {
            if idx > 0 {
                out.push('\n');
            }
            out.push_str(&self.serialize_stmt(stmt)?);
        }
        Ok(out)
    }

    fn serialize_stmt(&self, node: &BlockStmt) -> Result<String, fp_core::Error> {
        Ok(format!("{node:?}"))
    }

    fn serialize_value_function(&self, node: &ValueFunction) -> Result<String, fp_core::Error> {
        Ok(format!("{node:?}"))
    }

    fn serialize_def_function(
        &self,
        node: &ItemDefFunction,
    ) -> Result<String, fp_core::Error> {
        Ok(format!("{node:?}"))
    }
}
