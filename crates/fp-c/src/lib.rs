//! C frontend for FerroPhase.
//!
//! The Clang frontend owns the AST lowering; this crate provides the C-only
//! entry point and a small libc parsing helper.

use std::path::{Path, PathBuf};
use std::sync::Arc;

use fp_core::ast::{
    Abi, ExprKind, FunctionParam, FunctionSignature, Ident, Name, Ty, TypeInt, TypePrimitive,
};
use fp_core::ast::{
    AstSerializer, File, Item, ItemDeclFunction, ItemDefType, ItemKind, Visibility,
};
use fp_core::diagnostics::DiagnosticManager;
use fp_core::frontend::{FrontendResult, FrontendSnapshot, LanguageFrontend};
use fp_core::module::path::PathPrefix;

pub use fp_clang::ast;
pub use fp_clang::{ClangError, CompileOptions};

pub type Result<T> = std::result::Result<T, ClangError>;
pub type TranslationUnit = ast::TranslationUnit;

/// C frontend backed by Clang and the normal Ferro frontend pipeline.
pub struct CFrontend {
    c: CParser,
}

impl CFrontend {
    pub fn new() -> Result<Self> {
        Ok(Self { c: CParser::new()? })
    }

    fn parse_c_file(&self, source: &str, path: &Path) -> fp_core::Result<FrontendResult> {
        let mut options = CompileOptions::default();
        options.flags.push("-D_POSIX_C_SOURCE=200809L".to_string());
        let unit = self
            .c
            .parse_libc_bindings(source, options)
            .map_err(|err| fp_core::Error::from(err.to_string()))?;
        let ast = shared_ast_from_translation_unit(&unit, path);
        let serializer: Arc<dyn AstSerializer> = Arc::new(CSerializer);
        let diagnostics = Arc::new(DiagnosticManager::new());
        Ok(FrontendResult {
            last: ast.clone(),
            ast,
            serializer,
            intrinsic_normalizer: None,
            macro_parser: None,
            snapshot: Some(FrontendSnapshot {
                language: "c".to_string(),
                description: "Clang C declarations lowered to the shared AST".to_string(),
                serialized: None,
            }),
            diagnostics,
        })
    }
}

impl LanguageFrontend for CFrontend {
    fn language(&self) -> &'static str {
        "c"
    }

    fn extensions(&self) -> &'static [&'static str] {
        &["c", "h"]
    }

    fn parse_expr(&self, _source: &str) -> fp_core::Result<FrontendResult> {
        Err(fp_core::Error::from(
            "C frontend does not support standalone expressions",
        ))
    }

    fn parse_file(&self, source: &str, path: &Path) -> fp_core::Result<FrontendResult> {
        self.parse_c_file(source, path)
    }
}

pub struct CParser {
    inner: fp_clang::ClangParser,
}

impl CParser {
    pub fn new() -> Result<Self> {
        Ok(Self {
            inner: fp_clang::ClangParser::new()?,
        })
    }

    pub fn with_path(path: PathBuf) -> Result<Self> {
        Ok(Self {
            inner: fp_clang::ClangParser::with_path(path)?,
        })
    }

    pub fn parse_file(&self, source: &Path, options: &CompileOptions) -> Result<TranslationUnit> {
        self.inner.parse_translation_unit(source, options)
    }

    pub fn parse_source(&self, source: &str, options: &CompileOptions) -> Result<TranslationUnit> {
        self.parse_temp_source(source, "c", options)
    }

    /// Parse a libc-facing C declaration with the platform headers enabled.
    pub fn parse_libc_source(
        &self,
        source: &str,
        mut options: CompileOptions,
    ) -> Result<TranslationUnit> {
        options.flags.push("-D_POSIX_C_SOURCE=200809L".to_string());
        self.parse_temp_source(source, "c", &options)
    }

    fn parse_libc_bindings(
        &self,
        source: &str,
        mut options: CompileOptions,
    ) -> Result<TranslationUnit> {
        options.flags.push("-D_POSIX_C_SOURCE=200809L".to_string());
        let file = tempfile::Builder::new()
            .prefix("fp-c-libc-")
            .suffix(".c")
            .tempfile()
            .map_err(ClangError::IoError)?;
        std::fs::write(file.path(), source).map_err(ClangError::IoError)?;
        self.inner
            .parse_translation_unit_with_includes(file.path(), &options)
    }

    fn parse_temp_source(
        &self,
        source: &str,
        extension: &str,
        options: &CompileOptions,
    ) -> Result<TranslationUnit> {
        let suffix = format!(".{extension}");
        let file = tempfile::Builder::new()
            .prefix("fp-c-")
            .suffix(&suffix)
            .tempfile()
            .map_err(ClangError::IoError)?;
        std::fs::write(file.path(), source).map_err(ClangError::IoError)?;
        self.parse_file(file.path(), options)
    }
}

fn shared_ast_from_translation_unit(unit: &TranslationUnit, path: &Path) -> File {
    let mut items = Vec::new();
    let mut aliases = std::collections::HashSet::new();
    for declaration in &unit.declarations {
        let ast::Declaration::Typedef(typedef) = declaration else {
            continue;
        };
        let Some(ty) = shared_type(&typedef.aliased_type, false) else {
            continue;
        };
        if aliases.insert(typedef.name.clone()) {
            items.push(Item::new(ItemKind::DefType(ItemDefType {
                attrs: Vec::new(),
                visibility: Visibility::Public,
                name: Ident::new(typedef.name.clone()),
                generics_params: Vec::new(),
                value: ty,
            })));
        }
    }
    for declaration in &unit.declarations {
        let ast::Declaration::Function(function) = declaration else {
            continue;
        };
        if function.is_variadic || function.name.is_empty() {
            continue;
        }
        let Some(ret) = shared_type(&function.return_type, false) else {
            continue;
        };
        if function
            .parameters
            .iter()
            .any(|parameter| shared_type(&parameter.param_type, true).is_none())
        {
            continue;
        }
        let params = function
            .parameters
            .iter()
            .enumerate()
            .filter_map(|(index, parameter)| {
                let name = parameter
                    .name
                    .as_deref()
                    .filter(|name| !name.is_empty())
                    .map(str::to_string)
                    .unwrap_or_else(|| format!("arg{index}"));
                Some(FunctionParam::new(
                    Ident::new(name),
                    shared_type(&parameter.param_type, true)?,
                ))
            })
            .collect::<Vec<_>>();
        let mut signature = FunctionSignature::unit();
        signature.name = Some(Ident::new(function.name.clone()));
        signature.abi = Abi::Named("C".to_string());
        signature.params = params;
        signature.ret_ty = (ret != Ty::unit()).then_some(ret);
        items.push(Item::new(ItemKind::DeclFunction(ItemDeclFunction {
            attrs: Vec::new(),
            ty_annotation: None,
            name: Ident::new(function.name.clone()),
            sig: signature,
        })));
    }
    File {
        path: path.to_path_buf(),
        attrs: Vec::new(),
        collected_items: Vec::new(),
        items,
    }
}

fn shared_type(ty: &ast::Type, parameter: bool) -> Option<Ty> {
    match ty {
        ast::Type::Void => Some(Ty::unit()),
        ast::Type::Bool => Some(Ty::bool()),
        ast::Type::Char => Some(Ty::Primitive(TypePrimitive::Int(TypeInt::I8))),
        ast::Type::UChar => Some(Ty::Primitive(TypePrimitive::Int(TypeInt::U8))),
        ast::Type::Short => Some(Ty::Primitive(TypePrimitive::Int(TypeInt::I16))),
        ast::Type::UShort => Some(Ty::Primitive(TypePrimitive::Int(TypeInt::U16))),
        ast::Type::Int => Some(Ty::Primitive(TypePrimitive::Int(TypeInt::I32))),
        ast::Type::UInt => Some(Ty::Primitive(TypePrimitive::Int(TypeInt::U32))),
        ast::Type::Long | ast::Type::LongLong => {
            Some(Ty::Primitive(TypePrimitive::Int(TypeInt::I64)))
        }
        ast::Type::ULong | ast::Type::ULongLong => {
            Some(Ty::Primitive(TypePrimitive::Int(TypeInt::U64)))
        }
        ast::Type::Float | ast::Type::Double | ast::Type::LongDouble => None,
        ast::Type::Pointer(inner) => {
            if parameter && is_const_char(inner) {
                return Some(Ty::reference(Ty::path(fp_core::ast::Path::new(
                    PathPrefix::Plain,
                    vec![Ident::new("std"), Ident::new("ffi"), Ident::new("CStr")],
                ))));
            }
            Some(Ty::raw_ptr(shared_type(inner, false)?, Some(true)))
        }
        ast::Type::Qualified { base, .. } => shared_type(base, parameter),
        ast::Type::Typedef(name) | ast::Type::Struct(name) | ast::Type::Union(name) => Some(
            Ty::path(fp_core::ast::Path::from_ident(Ident::new(name.clone()))),
        ),
        ast::Type::Enum(_) => Some(Ty::Primitive(TypePrimitive::Int(TypeInt::I32))),
        ast::Type::Array(_, _)
        | ast::Type::Function { .. }
        | ast::Type::Reference { .. }
        | ast::Type::Custom(_) => None,
    }
}

fn is_const_char(ty: &ast::Type) -> bool {
    match ty {
        ast::Type::Qualified { base, is_const, .. } => *is_const && is_char(base),
        _ => is_char(ty),
    }
}

fn is_char(ty: &ast::Type) -> bool {
    matches!(ty, ast::Type::Char | ast::Type::UChar)
}

#[derive(Debug, Clone, Copy)]
pub struct CSerializer;

impl AstSerializer for CSerializer {
    fn serialize_file(&self, file: &File) -> fp_core::Result<String> {
        file.items
            .iter()
            .map(|item| self.serialize_item(item))
            .collect::<fp_core::Result<Vec<_>>>()
            .map(|items| items.join("\n"))
    }

    fn serialize_item(&self, item: &Item) -> fp_core::Result<String> {
        match &item.kind {
            ItemKind::DefType(def) => Ok(format!(
                "pub type {} = {};",
                def.name,
                self.serialize_type(&def.value)?
            )),
            ItemKind::DeclFunction(decl) => {
                let params = decl
                    .sig
                    .params
                    .iter()
                    .map(|param| {
                        Ok(format!(
                            "{}: {}",
                            param.name,
                            self.serialize_type(&param.ty)?
                        ))
                    })
                    .collect::<fp_core::Result<Vec<_>>>()?;
                let ret = decl
                    .sig
                    .ret_ty
                    .as_ref()
                    .map(|ty| self.serialize_type(ty))
                    .transpose()?
                    .map(|ty| format!(" -> {ty}"))
                    .unwrap_or_default();
                Ok(format!(
                    "pub extern \"C\" fn {}({}){};",
                    decl.name,
                    params.join(", "),
                    ret
                ))
            }
            _ => Err(fp_core::Error::from(
                "C serializer received unsupported item",
            )),
        }
    }

    fn serialize_type(&self, ty: &Ty) -> fp_core::Result<String> {
        match ty {
            Ty::Unit(_) => Ok("()".to_string()),
            Ty::Primitive(TypePrimitive::Bool) => Ok("bool".to_string()),
            Ty::Primitive(TypePrimitive::Int(int)) => Ok(int.to_string()),
            Ty::Reference(reference) => Ok(format!("&{}", self.serialize_type(&reference.ty)?)),
            Ty::RawPtr(pointer) => Ok(format!("*mut {}", self.serialize_type(&pointer.ty)?)),
            Ty::Expr(expr) => match &expr.kind {
                ExprKind::Name(Name::Ident(ident)) => Ok(ident.to_string()),
                ExprKind::Name(Name::Path(path)) => Ok(path.join("::")),
                _ => Err(fp_core::Error::from("unsupported C type expression")),
            },
            _ => Err(fp_core::Error::from("unsupported C type")),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{CFrontend, CParser, CompileOptions, ast::Declaration};
    use fp_core::frontend::LanguageFrontend;
    use std::path::Path;

    #[test]
    fn parses_c_source() {
        let parser = CParser::new().expect("clang is required for the C parser test");
        let unit = parser
            .parse_source(
                "int add(int a, int b) { return a + b; }",
                &CompileOptions::default(),
            )
            .expect("C source should parse");
        assert!(unit.declarations.iter().any(|decl| matches!(
            decl,
            Declaration::Function(function) if function.name == "add"
        )));
    }

    #[test]
    fn parses_libc_declarations() {
        let parser = CParser::new().expect("clang is required for the libc parser test");
        let unit = parser
            .parse_libc_source(
                "#include <unistd.h>\npid_t current_pid(void) { return getpid(); }",
                CompileOptions::default(),
            )
            .expect("libc-backed C source should parse");
        assert!(unit.declarations.iter().any(|decl| matches!(
            decl,
            Declaration::Function(function) if function.name == "current_pid"
        )));
    }

    #[test]
    fn c_frontend_uses_ferro_pipeline() {
        let frontend = CFrontend::new().expect("clang is required for the C frontend test");
        let result = frontend
            .parse_file(
                "#include <unistd.h>\nint answer(void) { return getpid(); }",
                Path::new("answer.c"),
            )
            .expect("C declarations should enter the normal frontend pipeline");
        assert!(!result.ast.items.is_empty());
        assert_eq!(frontend.language(), "c");
    }
}
