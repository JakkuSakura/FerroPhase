use eyre::Result;
use fp_core::ast::{File, FunctionParam, Item, ItemKind, Module, StructuralField, Visibility};

use super::ZigEmitter;

impl ZigEmitter {
    pub(super) fn emit_file(&mut self, file: &File) -> Result<()> {
        self.ensure_header();
        for (index, item) in file.items.iter().enumerate() {
            if index > 0 {
                self.push_blank_line();
            }
            self.emit_item(item)?;
        }
        Ok(())
    }

    pub(super) fn emit_item(&mut self, item: &Item) -> Result<()> {
        match item.kind() {
            ItemKind::DefStruct(def) => self.emit_struct(def)?,
            ItemKind::DefEnum(def) => self.emit_enum(def)?,
            ItemKind::DefConst(def) => self.emit_const(def)?,
            ItemKind::DefFunction(def) => self.emit_function(def)?,
            ItemKind::Module(module) => self.emit_module(module)?,
            ItemKind::Expr(expr) => {
                self.push_placeholder_comment(
                    "module level expressions are not yet supported in Zig output",
                );
                if let Some(rendered) = self.render_expr(expr) {
                    self.push_comment(&format!("Original expression: {}", rendered));
                }
            }
            other => {
                self.push_placeholder_comment(&format!(
                    "skipping unsupported item kind `{other:?}` in Zig backend",
                ));
            }
        }
        Ok(())
    }

    fn emit_struct(&mut self, def: &fp_core::ast::ItemDefStruct) -> Result<()> {
        self.ensure_header();
        let visibility = self.render_visibility(def.visibility);
        self.push_line(&format!(
            "{}const {} = struct {{",
            visibility, def.name.name
        ));
        self.indent += 1;
        if def.value.fields.is_empty() {
            self.push_line("// NOTE: struct has no fields");
        } else {
            for field in &def.value.fields {
                self.emit_struct_field(field);
            }
        }
        self.indent -= 1;
        self.push_line("};");
        Ok(())
    }

    fn emit_enum(&mut self, def: &fp_core::ast::ItemDefEnum) -> Result<()> {
        self.ensure_header();
        let visibility = self.render_visibility(def.visibility);
        self.push_line(&format!("{}const {} = enum {{", visibility, def.name.name));
        self.indent += 1;
        if def.value.variants.is_empty() {
            self.push_line("_ = void;");
        } else {
            for variant in &def.value.variants {
                self.push_line(&format!("{},", variant.name.name));
            }
        }
        self.indent -= 1;
        self.push_line("};");
        Ok(())
    }

    fn emit_const(&mut self, def: &fp_core::ast::ItemDefConst) -> Result<()> {
        self.ensure_header();
        let visibility = self.render_visibility(def.visibility);
        let ty = def
            .ty_annotation()
            .or(def.ty.as_ref())
            .map(|ty| self.render_type(ty))
            .unwrap_or_else(|| "anytype".to_string());

        let value_expr = self.render_expr(def.value.as_ref());
        match value_expr {
            Some(value) => {
                self.push_line(&format!(
                    "{}const {}: {} = {};",
                    visibility, def.name.name, ty, value
                ));
            }
            None => {
                self.push_line(&format!(
                    "{}const {}: {} = undefined;",
                    visibility, def.name.name, ty
                ));
                self.push_comment("TODO: translate constant initializer");
            }
        }
        Ok(())
    }

    fn emit_function(&mut self, def: &fp_core::ast::ItemDefFunction) -> Result<()> {
        self.ensure_header();
        let visibility = self.render_visibility(def.visibility);
        let params = def
            .sig
            .params
            .iter()
            .map(|param| self.render_param(param))
            .collect::<Vec<_>>()
            .join(", ");

        let ret_ty = def
            .sig
            .ret_ty
            .as_ref()
            .map(|ty| self.render_type(ty))
            .unwrap_or_else(|| "void".to_string());

        self.push_line(&format!(
            "{}fn {}({}) {} {{",
            visibility, def.name.name, params, ret_ty
        ));
        self.indent += 1;
        let body_emitted = self.emit_function_body(def)?;
        if !body_emitted {
            for param in &def.sig.params {
                self.push_line(&format!("_ = {};", param.name.as_str()));
            }
            self.push_comment("TODO: translate function body");
            self.push_line("@panic(\"FerroPhase Zig backend: body not generated yet\");");
        }
        self.indent -= 1;
        self.push_line("}");
        Ok(())
    }

    fn emit_module(&mut self, module: &Module) -> Result<()> {
        self.ensure_header();
        let visibility = self.render_visibility(module.visibility);
        self.push_line(&format!(
            "{}const {} = struct {{",
            visibility, module.name.name
        ));
        self.indent += 1;
        if module.items.is_empty() {
            self.push_comment("empty module");
        } else {
            for (index, item) in module.items.iter().enumerate() {
                if index > 0 {
                    self.push_blank_line();
                }
                self.emit_item(item)?;
            }
        }
        self.indent -= 1;
        self.push_line("};");
        Ok(())
    }

    fn emit_struct_field(&mut self, field: &StructuralField) {
        let ty = self.render_type(&field.value);
        self.push_line(&format!("{}: {},", field.name.name, ty));
    }

    fn render_param(&self, param: &FunctionParam) -> String {
        let ty = param
            .ty_annotation()
            .map(|ty| self.render_type(ty))
            .unwrap_or_else(|| self.render_type(&param.ty));
        format!("{}: {}", param.name.as_str(), ty)
    }

    fn render_visibility(&self, visibility: Visibility) -> &'static str {
        match visibility {
            Visibility::Public => "pub ",
            Visibility::Inherited | Visibility::Private => "",
        }
    }
}
