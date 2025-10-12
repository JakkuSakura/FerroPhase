use std::fmt::Write;
use std::mem;

use fp_core::ast::{
    self, AstSerializer, FunctionSignature, Ident, Item, ItemKind, Node, NodeKind, Ty, TypeInt,
    TypePrimitive, Visibility,
};
use fp_core::error::{Error as CoreError, Result};

/// Serializes FerroPhase AST nodes into WIT source text.
#[derive(Debug, Default, Clone, Copy)]
pub struct WitSerializer;

impl WitSerializer {
    pub fn new() -> Self {
        Self
    }
}

impl AstSerializer for WitSerializer {
    fn serialize_node(&self, node: &Node) -> Result<String> {
        let mut emitter = WitEmitter::new();
        emitter.emit_node(node)?;
        Ok(emitter.finish())
    }
}

struct WitEmitter {
    root: InterfaceBuilder,
    interfaces: Vec<String>,
    exports: Vec<String>,
}

impl WitEmitter {
    fn new() -> Self {
        Self {
            root: InterfaceBuilder::new("ferrophase".to_string()),
            interfaces: Vec::new(),
            exports: Vec::new(),
        }
    }

    fn emit_node(&mut self, node: &Node) -> Result<()> {
        match node.kind() {
            NodeKind::File(file) => {
                let mut root = mem::take(&mut self.root);
                for item in &file.items {
                    self.emit_into_builder(item, None, &mut root)?;
                }
                self.root = root;
                Ok(())
            }
            NodeKind::Item(item) => {
                let mut root = mem::take(&mut self.root);
                let result = self.emit_into_builder(item, None, &mut root);
                self.root = root;
                result
            }
            NodeKind::Expr(_) => Err(CoreError::from(
                "WIT serialization expects a file or module-level item".to_string(),
            )),
        }
    }

    fn emit_into_builder(
        &mut self,
        item: &Item,
        scope: Option<String>,
        builder: &mut InterfaceBuilder,
    ) -> Result<()> {
        match item.kind() {
            ItemKind::Module(module) => {
                let interface_name = interface_name(scope.as_deref(), &module.name);
                let mut child = InterfaceBuilder::new(interface_name.clone());
                for inner in &module.items {
                    self.emit_into_builder(inner, Some(interface_name.clone()), &mut child)?;
                }
                if child.has_entries() {
                    if !self.exports.contains(&interface_name) {
                        self.exports.push(interface_name.clone());
                    }
                    self.interfaces.push(child.finish());
                }
                Ok(())
            }
            ItemKind::DefStruct(def) => {
                builder.add_struct(def);
                Ok(())
            }
            ItemKind::DefEnum(def) => {
                builder.add_enum(def);
                Ok(())
            }
            ItemKind::DefType(def) => {
                builder.add_alias(def);
                Ok(())
            }
            ItemKind::DefFunction(def) => {
                builder.add_function(&def.sig, &def.name, def.visibility);
                Ok(())
            }
            ItemKind::DeclFunction(decl) => {
                builder.add_function(&decl.sig, &decl.name, Visibility::Public);
                Ok(())
            }
            ItemKind::Expr(_)
            | ItemKind::Any(_)
            | ItemKind::Import(_)
            | ItemKind::DeclConst(_)
            | ItemKind::DeclStatic(_)
            | ItemKind::DeclType(_)
            | ItemKind::Impl(_)
            | ItemKind::DefConst(_)
            | ItemKind::DefStatic(_)
            | ItemKind::DefStructural(_)
            | ItemKind::DefTrait(_) => Ok(()),
        }
    }

    fn finish(mut self) -> String {
        if self.root.has_entries() {
            let root_name = self.root.name().to_string();
            if !self.exports.contains(&root_name) {
                self.exports.push(root_name.clone());
            }
            self.interfaces.insert(0, self.root.finish());
        }

        self.interfaces.sort();
        self.interfaces.dedup();

        self.exports.sort();
        self.exports.dedup();

        let mut output = String::new();
        let _ = writeln!(output, "package ferrophase:generated;\n");

        for interface in self.interfaces {
            output.push_str(&interface);
            if !interface.ends_with('\n') {
                output.push('\n');
            }
        }

        if !self.exports.is_empty() {
            let _ = writeln!(output, "world ferrophase {{");
            for export in self.exports {
                let _ = writeln!(output, "    export {export};");
            }
            let _ = writeln!(output, "}}");
        }

        output
    }
}

#[derive(Debug)]
struct InterfaceBuilder {
    name: String,
    entries: Vec<InterfaceEntry>,
}

impl InterfaceBuilder {
    fn new(name: String) -> Self {
        Self {
            name,
            entries: Vec::new(),
        }
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn has_entries(&self) -> bool {
        !self.entries.is_empty()
    }

    fn add_struct(&mut self, def: &ast::ItemDefStruct) {
        if matches!(def.visibility, Visibility::Private) {
            return;
        }

        let fields = def
            .value
            .fields
            .iter()
            .map(|field| (ident_to_wit(&field.name), ty_to_wit(&field.value)))
            .collect();
        self.entries.push(InterfaceEntry::Record {
            name: ident_to_wit(&def.name),
            fields,
        });
    }

    fn add_enum(&mut self, def: &ast::ItemDefEnum) {
        if matches!(def.visibility, Visibility::Private) {
            return;
        }

        let cases = def
            .value
            .variants
            .iter()
            .map(|variant| {
                let assoc = match &variant.value {
                    ast::Ty::Unit(_) => None,
                    other => Some(ty_to_wit(other)),
                };
                (ident_to_wit(&variant.name), assoc)
            })
            .collect();
        self.entries.push(InterfaceEntry::Variant {
            name: ident_to_wit(&def.name),
            cases,
        });
    }

    fn add_alias(&mut self, def: &ast::ItemDefType) {
        if matches!(def.visibility, Visibility::Private) {
            return;
        }
        self.entries.push(InterfaceEntry::Alias {
            name: ident_to_wit(&def.name),
            target: ty_to_wit(&def.value),
        });
    }

    fn add_function(&mut self, sig: &FunctionSignature, name: &Ident, visibility: Visibility) {
        if matches!(visibility, Visibility::Private) {
            return;
        }

        let params = sig
            .params
            .iter()
            .enumerate()
            .map(|(idx, param)| {
                let pname = if param.name.as_str().is_empty() {
                    format!("arg{idx}")
                } else {
                    ident_to_wit(&param.name)
                };
                (pname, ty_to_wit(&param.ty))
            })
            .collect();

        let result = sig.ret_ty.as_ref().map(ty_to_wit);

        self.entries.push(InterfaceEntry::Function {
            name: ident_to_wit(name),
            params,
            result,
        });
    }

    fn finish(self) -> String {
        let mut out = String::new();
        let _ = writeln!(out, "interface {} {{", self.name);
        for entry in self.entries {
            match entry {
                InterfaceEntry::Record { name, fields } => {
                    if fields.is_empty() {
                        let _ = writeln!(out, "    record {name} {{}}\n");
                    } else {
                        let _ = writeln!(out, "    record {name} {{");
                        for (field, ty) in fields {
                            let _ = writeln!(out, "        {field}: {ty},");
                        }
                        let _ = writeln!(out, "    }}\n");
                    }
                }
                InterfaceEntry::Variant { name, cases } => {
                    if cases.is_empty() {
                        let _ = writeln!(out, "    variant {name} {{}}\n");
                    } else {
                        let _ = writeln!(out, "    variant {name} {{");
                        for (case, assoc) in cases {
                            if let Some(ty) = assoc {
                                let _ = writeln!(out, "        {case}({ty}),");
                            } else {
                                let _ = writeln!(out, "        {case},");
                            }
                        }
                        let _ = writeln!(out, "    }}\n");
                    }
                }
                InterfaceEntry::Alias { name, target } => {
                    let _ = writeln!(out, "    type {name} = {target};\n");
                }
                InterfaceEntry::Function { name, params, result } => {
                    let params_str = params
                        .iter()
                        .map(|(param, ty)| format!("{param}: {ty}"))
                        .collect::<Vec<_>>()
                        .join(", ");
                    let result_str = result.map(|ty| format!(" -> {ty}")).unwrap_or_default();
                    let _ = writeln!(out, "    {name}: func({params_str}){result_str};\n");
                }
            }
        }
        let _ = writeln!(out, "}}\n");
        out
    }
}

impl Default for InterfaceBuilder {
    fn default() -> Self {
        Self::new(String::new())
    }
}

#[derive(Debug)]
enum InterfaceEntry {
    Record {
        name: String,
        fields: Vec<(String, String)>,
    },
    Variant {
        name: String,
        cases: Vec<(String, Option<String>)>,
    },
    Alias {
        name: String,
        target: String,
    },
    Function {
        name: String,
        params: Vec<(String, String)>,
        result: Option<String>,
    },
}

fn interface_name(parent: Option<&str>, ident: &Ident) -> String {
    let mut base = ident_to_wit(ident);
    if let Some(parent) = parent {
        base = sanitize_identifier(&format!("{parent}-{base}"));
    }
    base
}

fn ident_to_wit(ident: &Ident) -> String {
    sanitize_identifier(ident.as_str())
}

fn sanitize_identifier(raw: &str) -> String {
    let mut result = String::new();
    for ch in raw.chars() {
        let mapped = match ch {
            'a'..='z' | '0'..='9' => ch,
            'A'..='Z' => ch.to_ascii_lowercase(),
            '_' | '-' => '-',
            _ => '-',
        };
        result.push(mapped);
    }
    while result.contains("--") {
        result = result.replace("--", "-");
    }
    result = result.trim_matches('-').to_string();
    if result.is_empty() {
        result.push_str("item");
    }
    if result.chars().next().unwrap().is_ascii_digit() {
        result.insert(0, '_');
    }
    result
}

fn ty_to_wit(ty: &Ty) -> String {
    match ty {
        Ty::Primitive(p) => match p {
            TypePrimitive::Bool => "bool".to_string(),
            TypePrimitive::Char => "char".to_string(),
            TypePrimitive::String => "string".to_string(),
            TypePrimitive::Int(int_ty) => match int_ty {
                TypeInt::I8 => "s8".to_string(),
                TypeInt::I16 => "s16".to_string(),
                TypeInt::I32 => "s32".to_string(),
                TypeInt::I64 => "s64".to_string(),
                TypeInt::U8 => "u8".to_string(),
                TypeInt::U16 => "u16".to_string(),
                TypeInt::U32 => "u32".to_string(),
                TypeInt::U64 => "u64".to_string(),
                TypeInt::BigInt => "s64".to_string(),
            },
            TypePrimitive::Decimal(dec) => match dec {
                ast::DecimalType::F32 => "f32".to_string(),
                ast::DecimalType::F64 => "f64".to_string(),
                _ => "f64".to_string(),
            },
            TypePrimitive::List => "list<string>".to_string(),
        },
        Ty::Reference(reference) => ty_to_wit(&reference.ty),
        Ty::Tuple(tuple) => {
            let inner = tuple
                .types
                .iter()
                .map(ty_to_wit)
                .collect::<Vec<_>>()
                .join(", ");
            format!("tuple<{inner}>")
        }
        Ty::Vec(vec_ty) => {
            let inner = ty_to_wit(vec_ty.ty.as_ref());
            format!("list<{inner}>")
        }
        Ty::Array(array) => {
            let inner = ty_to_wit(array.elem.as_ref());
            format!("list<{inner}>")
        }
        Ty::Slice(slice) => {
            let inner = ty_to_wit(slice.elem.as_ref());
            format!("list<{inner}>")
        }
        Ty::Struct(s) => ident_to_wit(&s.name),
        Ty::Enum(e) => ident_to_wit(&e.name),
        Ty::Unit(_) => "unit".to_string(),
        Ty::Any(_) | Ty::AnyBox(_) => "string".to_string(),
        Ty::Expr(_) | Ty::Value(_) => "string".to_string(),
        Ty::Unknown(_) | Ty::Nothing(_) => "string".to_string(),
        Ty::Type(_) | Ty::TypeBounds(_) | Ty::ImplTraits(_) => "string".to_string(),
        Ty::Structural(_) => "string".to_string(),
        Ty::Function(_) => "func".to_string(),
    }
}
