use fp_core::ast::{
    AstSerializer, BlockStmt, Expr, ExprBlock, File, Item, ItemDefFunction, Ty, Value,
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
    fn serialize_expr(&self, node: &Expr) -> Result<String, fp_core::Error> {
        Ok(format!("{}", pretty(node, self.options.clone())))
    }

    fn serialize_item(&self, node: &Item) -> Result<String, fp_core::Error> {
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

    fn serialize_def_function(&self, node: &ItemDefFunction) -> Result<String, fp_core::Error> {
        Ok(format!("{node:?}"))
    }
}

impl PrettyAstSerializer {
    pub fn serialize_file(&self, file: &File) -> Result<String, fp_core::Error> {
        Ok(format!("{}", pretty(file, self.options.clone())))
    }

    /// Serializes a package into one pretty-printed Rust-ish source file
    /// per module. Returns `Vec<(relative_path, code)>`.
    pub fn serialize_package(
        &self,
        source: &fp_core::package::PackageSource,
    ) -> Result<Vec<(String, String)>, fp_core::Error> {
        fp_core::package::split_package_into_modules(source)
            .into_iter()
            .map(|module| {
                let rel_path = module.relative_path();
                let file = File {
                    path: std::path::PathBuf::from(&rel_path),
                    attrs: Vec::new(),
                    collected_items: Vec::new(),
                    items: module.items,
                };
                let code = self.serialize_file(&file)?;
                Ok((rel_path, code))
            })
            .collect()
    }
}

pub struct RustBackend {
    serializer: PrettyAstSerializer,
    config: fp_core::backend::BackendConfig,
}

impl RustBackend {
    pub fn new(config: fp_core::backend::BackendConfig) -> Self {
        Self {
            serializer: PrettyAstSerializer::new(),
            config,
        }
    }
}

impl fp_core::backend::TargetBackend for RustBackend {
    fn compile_package(
        &self,
        workspace: &fp_core::workspace::WorkspaceContext,
        package_id: &fp_core::package::PackageId,
    ) -> Result<(), fp_core::Error> {
        let package = workspace.package_source(package_id)?;
        let package = &package;
        let files = self.serializer.serialize_package(package)?;
        let writer = fp_core::backend::PackageWriter::new(self.config.workspace_root.join(&package.name));
        for (rel_path, code) in files {
            let rel = if rel_path.contains('.') {
                rel_path
            } else {
                format!("{rel_path}.rs")
            };
            writer.write_file(&rel, code)?;
        }
        Ok(())
    }
}
