//! Utilities for lowering Clang's JSON AST into fp-clang's internal AST representation.

use crate::ast::{
    Declaration, EnumDecl as FpEnumDecl, Enumerator, FieldDecl as FpFieldDecl,
    FunctionDecl as FpFunction, Parameter, StructDecl as FpStructDecl, TranslationUnit,
    Type as FpType, TypedefDecl as FpTypedef, VarDecl as FpVarDecl,
};
use crate::error::{ClangError, Result};
use clang_ast::{Node, SourceLocation};
use serde::Deserialize;
use std::fs;
use std::path::{Path, PathBuf};

pub(crate) fn lower_translation_unit_from_json(
    json: &str,
    source_file: &Path,
) -> Result<TranslationUnit> {
    let normalized_source = normalize_path(source_file);

    let root: Node<Clang> = serde_json::from_str(json)
        .map_err(|err| ClangError::ParseError(format!("failed to deserialize clang AST: {err}")))?;

    let Node { kind, inner, .. } = root;
    match kind {
        Clang::TranslationUnitDecl => {
            let mut decls = Vec::new();
            let ctx = LoweringContext::new(normalized_source);
            for child in inner {
                ctx.collect_top_level(&child, &mut decls);
            }
            Ok(TranslationUnit {
                file_path: source_file.to_string_lossy().to_string(),
                declarations: decls,
            })
        }
        _ => Err(ClangError::ParseError(
            "expected TranslationUnitDecl as AST root".to_string(),
        )),
    }
}

#[derive(Debug)]
struct LoweringContext {
    main_file: PathBuf,
    main_file_name: Option<String>,
}

impl LoweringContext {
    fn new(main_file: PathBuf) -> Self {
        let main_file_name = main_file
            .file_name()
            .and_then(|name| name.to_str())
            .map(|s| s.to_string());
        Self {
            main_file,
            main_file_name,
        }
    }

    fn collect_top_level(&self, node: &Node<Clang>, out: &mut Vec<Declaration>) {
        match &node.kind {
            Clang::FunctionDecl(func) => {
                if let Some(decl) = self.lower_function(node, func) {
                    out.push(Declaration::Function(decl));
                }
            }
            Clang::VarDecl(var) => {
                if let Some(decl) = self.lower_variable(var) {
                    out.push(Declaration::Variable(decl));
                }
            }
            Clang::EnumDecl(enum_decl) => {
                if let Some(decl) = self.lower_enum(node, enum_decl) {
                    out.push(Declaration::Enum(decl));
                }
            }
            Clang::RecordDecl(record) | Clang::CXXRecordDecl(record) => {
                if let Some((decl, is_union)) = self.lower_record(node, record) {
                    if is_union {
                        out.push(Declaration::Union(decl));
                    } else {
                        out.push(Declaration::Struct(decl));
                    }
                }
            }
            Clang::TypedefDecl(typedef) => {
                if let Some(decl) = self.lower_typedef(typedef) {
                    out.push(Declaration::Typedef(decl));
                }
            }
            Clang::LinkageSpecDecl | Clang::NamespaceDecl => {
                for child in &node.inner {
                    self.collect_top_level(child, out);
                }
            }
            _ => {}
        }
    }

    fn lower_function(&self, node: &Node<Clang>, func: &FunctionDecl) -> Option<FpFunction> {
        if func.is_implicit.unwrap_or(false) {
            return None;
        }
        let loc = func.loc.as_ref();
        if !self.is_from_main_file(loc) {
            return None;
        }

        let name = func.name.as_deref()?.trim();
        if name.is_empty() {
            return None;
        }

        let type_info = func.type_info.as_ref()?;
        let return_type_str = extract_return_type(&type_info.qual_type)?;
        let return_type = parse_type(return_type_str);

        let parameters = node
            .inner
            .iter()
            .filter_map(|child| match &child.kind {
                Clang::ParmVarDecl(param) => self.lower_parameter(param),
                _ => None,
            })
            .collect::<Vec<_>>();

        let is_variadic = func
            .is_variadic
            .or(func.variadic)
            .unwrap_or_else(|| type_info.qual_type.contains("..."));

        let is_definition = node
            .inner
            .iter()
            .any(|child| matches!(child.kind, Clang::CompoundStmt));

        Some(FpFunction {
            name: name.to_string(),
            return_type,
            parameters,
            is_variadic,
            is_definition,
            storage_class: parse_storage_class(func.storage_class.as_deref()),
        })
    }

    fn lower_parameter(&self, param: &ParmVarDecl) -> Option<Parameter> {
        let type_info = param.type_info.as_ref()?;
        Some(Parameter {
            name: param.name.clone(),
            param_type: parse_type(&type_info.qual_type),
        })
    }

    fn lower_variable(&self, var: &VarDecl) -> Option<FpVarDecl> {
        if var.is_implicit.unwrap_or(false) {
            return None;
        }
        if !self.is_from_main_file(var.loc.as_ref()) {
            return None;
        }
        let name = var.name.as_ref()?.trim();
        if name.is_empty() {
            return None;
        }
        let type_info = var.type_info.as_ref()?;
        let ty = parse_type(&type_info.qual_type);
        let storage_class = parse_storage_class(var.storage_class.as_deref());
        let qualifiers = collect_qualifiers(&type_info.qual_type);
        Some(FpVarDecl {
            name: name.to_string(),
            var_type: ty,
            storage_class,
            is_extern: matches!(storage_class, crate::ast::StorageClass::Extern),
            is_static: matches!(storage_class, crate::ast::StorageClass::Static),
            is_const: qualifiers.is_const,
        })
    }

    fn lower_enum(&self, node: &Node<Clang>, enum_decl: &EnumDecl) -> Option<FpEnumDecl> {
        if !self.is_from_main_file(enum_decl.loc.as_ref()) {
            return None;
        }

        let enumerators = node
            .inner
            .iter()
            .filter_map(|child| match &child.kind {
                Clang::EnumConstantDecl(constant) => {
                    let value = constant
                        .value
                        .as_deref()
                        .and_then(|v| v.parse::<i64>().ok());
                    Some(Enumerator {
                        name: constant.name.clone()?,
                        value,
                    })
                }
                _ => None,
            })
            .collect::<Vec<_>>();

        if enumerators.is_empty() {
            return None;
        }

        Some(FpEnumDecl {
            name: enum_decl.name.clone(),
            enumerators,
        })
    }

    fn lower_record(
        &self,
        node: &Node<Clang>,
        record: &RecordDecl,
    ) -> Option<(FpStructDecl, bool)> {
        if !record.complete_definition.unwrap_or(false) {
            return None;
        }
        if !self.is_from_main_file(record.loc.as_ref()) {
            return None;
        }

        let name = record.name.clone();
        let is_union = record
            .tag_used
            .as_deref()
            .map(|tag| tag == "union")
            .unwrap_or(false);

        let fields = node
            .inner
            .iter()
            .filter_map(|child| match &child.kind {
                Clang::FieldDecl(field) => {
                    let field_type = field.type_info.as_ref()?;
                    Some(FpFieldDecl {
                        name: field.name.clone(),
                        field_type: parse_type(&field_type.qual_type),
                        bit_width: field.bit_width_value,
                    })
                }
                _ => None,
            })
            .collect::<Vec<_>>();

        Some((
            FpStructDecl {
                name,
                fields,
                is_union,
            },
            is_union,
        ))
    }

    fn lower_typedef(&self, typedef: &TypedefDecl) -> Option<FpTypedef> {
        if typedef.is_implicit.unwrap_or(false) {
            return None;
        }
        if !self.is_from_main_file(typedef.loc.as_ref()) {
            return None;
        }
        let name = typedef.name.as_ref()?.trim();
        if name.is_empty() {
            return None;
        }
        let aliased = typedef.type_info.as_ref()?;
        Some(FpTypedef {
            name: name.to_string(),
            aliased_type: parse_type(&aliased.qual_type),
        })
    }

    fn is_from_main_file(&self, loc: Option<&SourceLocation>) -> bool {
        let Some(loc) = loc else {
            return false;
        };
        let Some(file) = location_file_path(loc) else {
            return false;
        };

        let file_path = Path::new(&file);
        if file_path == self.main_file {
            return true;
        }

        if let Ok(canon) = file_path.canonicalize() {
            if canon == self.main_file {
                return true;
            }
        }

        if let Some(name) = &self.main_file_name {
            if file_path.file_name().and_then(|f| f.to_str()) == Some(name) {
                return true;
            }
        }

        false
    }
}

#[derive(Debug, Deserialize)]
pub enum Clang {
    TranslationUnitDecl,
    FunctionDecl(FunctionDecl),
    VarDecl(VarDecl),
    ParmVarDecl(ParmVarDecl),
    EnumDecl(EnumDecl),
    EnumConstantDecl(EnumConstantDecl),
    RecordDecl(RecordDecl),
    CXXRecordDecl(RecordDecl),
    FieldDecl(FieldDecl),
    TypedefDecl(TypedefDecl),
    LinkageSpecDecl,
    NamespaceDecl,
    CompoundStmt,
    Other,
}

#[derive(Debug, Deserialize)]
pub struct FunctionDecl {
    #[serde(default)]
    pub name: Option<String>,
    #[serde(rename = "loc", default)]
    pub loc: Option<SourceLocation>,
    #[serde(rename = "type", default)]
    pub type_info: Option<TypeInfo>,
    #[serde(rename = "storageClass", default)]
    pub storage_class: Option<String>,
    #[serde(rename = "variadic", default)]
    pub variadic: Option<bool>,
    #[serde(rename = "isVariadic", default)]
    pub is_variadic: Option<bool>,
    #[serde(rename = "isImplicit", default)]
    pub is_implicit: Option<bool>,
}

#[derive(Debug, Deserialize)]
pub struct VarDecl {
    #[serde(default)]
    pub name: Option<String>,
    #[serde(rename = "loc", default)]
    pub loc: Option<SourceLocation>,
    #[serde(rename = "type", default)]
    pub type_info: Option<TypeInfo>,
    #[serde(rename = "storageClass", default)]
    pub storage_class: Option<String>,
    #[serde(rename = "isImplicit", default)]
    pub is_implicit: Option<bool>,
}

#[derive(Debug, Deserialize)]
pub struct ParmVarDecl {
    #[serde(default)]
    pub name: Option<String>,
    #[serde(rename = "type", default)]
    pub type_info: Option<TypeInfo>,
}

#[derive(Debug, Deserialize)]
pub struct EnumDecl {
    #[serde(default)]
    pub name: Option<String>,
    #[serde(rename = "loc", default)]
    pub loc: Option<SourceLocation>,
}

#[derive(Debug, Deserialize)]
pub struct EnumConstantDecl {
    pub name: Option<String>,
    #[serde(default)]
    pub value: Option<String>,
}

#[derive(Debug, Deserialize)]
pub struct RecordDecl {
    #[serde(default)]
    pub name: Option<String>,
    #[serde(rename = "tagUsed", default)]
    pub tag_used: Option<String>,
    #[serde(rename = "completeDefinition", default)]
    pub complete_definition: Option<bool>,
    #[serde(rename = "loc", default)]
    pub loc: Option<SourceLocation>,
}

#[derive(Debug, Deserialize)]
pub struct FieldDecl {
    #[serde(default)]
    pub name: Option<String>,
    #[serde(rename = "type", default)]
    pub type_info: Option<TypeInfo>,
    #[serde(rename = "bitWidthValue", default)]
    pub bit_width_value: Option<u32>,
}

#[derive(Debug, Deserialize)]
pub struct TypedefDecl {
    #[serde(default)]
    pub name: Option<String>,
    #[serde(rename = "type", default)]
    pub type_info: Option<TypeInfo>,
    #[serde(rename = "loc", default)]
    pub loc: Option<SourceLocation>,
    #[serde(rename = "isImplicit", default)]
    pub is_implicit: Option<bool>,
}

#[derive(Debug, Deserialize)]
pub struct TypeInfo {
    #[serde(rename = "qualType")]
    pub qual_type: String,
}

#[derive(Default)]
struct QualifierFlags {
    is_const: bool,
    is_volatile: bool,
    is_restrict: bool,
}

#[derive(Clone, Copy)]
enum ReferenceKind {
    LValue,
    RValue,
}

fn parse_storage_class(value: Option<&str>) -> crate::ast::StorageClass {
    use crate::ast::StorageClass;
    match value {
        Some("auto") => StorageClass::Auto,
        Some("register") => StorageClass::Register,
        Some("static") => StorageClass::Static,
        Some("extern") => StorageClass::Extern,
        Some("typedef") => StorageClass::Typedef,
        _ => StorageClass::None,
    }
}

fn parse_type(qual_type: &str) -> FpType {
    let trimmed = qual_type.trim();
    if trimmed.is_empty() {
        return FpType::Custom(String::new());
    }

    if let Some(function_pointer) = try_parse_function_pointer(trimmed) {
        return function_pointer;
    }
    if let Some(function_type) = try_parse_function_type(trimmed) {
        return function_type;
    }

    let (without_arrays, arrays) = strip_array_suffixes(trimmed);
    let (without_refs, references) = strip_reference_suffixes(&without_arrays);
    let (base_part, pointer_depth) = strip_pointer_suffixes(&without_refs);

    let (base, qualifiers) = parse_base_type(&base_part, trimmed);

    let mut ty = if qualifiers.is_const || qualifiers.is_volatile || qualifiers.is_restrict {
        FpType::Qualified {
            base: Box::new(base),
            is_const: qualifiers.is_const,
            is_volatile: qualifiers.is_volatile,
            is_restrict: qualifiers.is_restrict,
        }
    } else {
        base
    };

    for _ in 0..pointer_depth {
        ty = FpType::Pointer(Box::new(ty));
    }

    for reference in references {
        let is_rvalue = matches!(reference, ReferenceKind::RValue);
        ty = FpType::Reference {
            base: Box::new(ty),
            is_rvalue,
        };
    }

    for size in arrays {
        ty = FpType::Array(Box::new(ty), size);
    }

    ty
}

fn parse_base_type(base_part: &str, original: &str) -> (FpType, QualifierFlags) {
    let mut qualifiers = QualifierFlags::default();
    let mut words = Vec::new();
    for word in base_part.split_whitespace() {
        match word {
            "const" => qualifiers.is_const = true,
            "volatile" => qualifiers.is_volatile = true,
            "restrict" => qualifiers.is_restrict = true,
            "" => {}
            _ => words.push(word),
        }
    }

    let base = match words.as_slice() {
        [] => FpType::Custom(original.to_string()),
        ["void"] => FpType::Void,
        ["bool"] => FpType::Bool,
        ["char"] => FpType::Char,
        ["signed", "char"] | ["char", "signed"] => FpType::Char,
        ["unsigned", "char"] => FpType::UChar,
        ["short"] | ["short", "int"] | ["signed", "short"] | ["signed", "short", "int"] => {
            FpType::Short
        }
        ["unsigned", "short"] | ["unsigned", "short", "int"] => FpType::UShort,
        ["int"] | ["signed"] | ["signed", "int"] => FpType::Int,
        ["unsigned"] | ["unsigned", "int"] => FpType::UInt,
        ["long"] | ["long", "int"] | ["signed", "long"] | ["signed", "long", "int"] => FpType::Long,
        ["unsigned", "long"] | ["unsigned", "long", "int"] => FpType::ULong,
        ["long", "long"] | ["long", "long", "int"] | ["signed", "long", "long"] => FpType::LongLong,
        ["unsigned", "long", "long"] | ["unsigned", "long", "long", "int"] => FpType::ULongLong,
        ["float"] => FpType::Float,
        ["double"] => FpType::Double,
        ["long", "double"] => FpType::LongDouble,
        ["struct", name] => FpType::Struct(name.to_string()),
        ["union", name] => FpType::Union(name.to_string()),
        ["enum", name] => FpType::Enum(name.to_string()),
        ["class", name] => FpType::Struct(name.to_string()),
        [name] => FpType::Typedef((*name).to_string()),
        _ => FpType::Custom(original.to_string()),
    };

    (base, qualifiers)
}

fn strip_pointer_suffixes(input: &str) -> (String, usize) {
    let mut pointer_depth = 0;
    let mut remaining = input.trim_end().to_string();

    loop {
        let trimmed = remaining.trim_end();
        if trimmed.is_empty() {
            remaining.clear();
            break;
        }

        if let Some(stripped) = trimmed
            .strip_suffix("const")
            .map(|s| s.trim_end().to_string())
        {
            remaining = stripped;
            continue;
        }
        if let Some(stripped) = trimmed
            .strip_suffix("volatile")
            .map(|s| s.trim_end().to_string())
        {
            remaining = stripped;
            continue;
        }
        if let Some(stripped) = trimmed
            .strip_suffix("restrict")
            .map(|s| s.trim_end().to_string())
        {
            remaining = stripped;
            continue;
        }

        if let Some(stripped) = trimmed.strip_suffix('*') {
            pointer_depth += 1;
            remaining = stripped.to_string();
            continue;
        }

        remaining = trimmed.to_string();
        break;
    }

    (remaining.trim_end().to_string(), pointer_depth)
}

fn strip_reference_suffixes(input: &str) -> (String, Vec<ReferenceKind>) {
    let mut remaining = input.trim_end().to_string();
    let mut references = Vec::new();

    loop {
        let trimmed = remaining.trim_end();
        if let Some(stripped) = trimmed.strip_suffix("&&") {
            references.push(ReferenceKind::RValue);
            remaining = stripped.to_string();
            continue;
        }
        if let Some(stripped) = trimmed.strip_suffix('&') {
            references.push(ReferenceKind::LValue);
            remaining = stripped.to_string();
            continue;
        }
        break;
    }

    (remaining.trim_end().to_string(), references)
}

fn try_parse_function_type(qual_type: &str) -> Option<FpType> {
    let trimmed = qual_type.trim();
    let open_paren = trimmed.find('(')?;
    let close_paren = trimmed.rfind(')')?;
    if close_paren != trimmed.len() - 1 {
        return None;
    }

    let params_section = &trimmed[open_paren + 1..close_paren];
    if params_section.contains('(') || params_section.contains(')') {
        return None;
    }
    let return_part = trimmed[..open_paren].trim();
    if return_part.is_empty() {
        return None;
    }

    let return_type = parse_type(return_part);
    let (params, is_variadic) = parse_parameter_list(params_section);

    Some(FpType::Function {
        return_type: Box::new(return_type),
        params,
        is_variadic,
    })
}

fn try_parse_function_pointer(qual_type: &str) -> Option<FpType> {
    let trimmed = qual_type.trim();
    let last_close = trimmed.rfind(')')?;
    let open_for_params = find_matching_paren(trimmed, last_close)?;
    let params_section = &trimmed[open_for_params + 1..last_close];
    let prefix = trimmed[..open_for_params].trim_end();

    if !prefix.ends_with(')') {
        return None;
    }
    let inner_open = find_matching_paren(prefix, prefix.len() - 1)?;
    let outer_return = prefix[..inner_open].trim_end();
    if outer_return.is_empty() {
        return None;
    }
    let declarator = prefix[inner_open + 1..prefix.len() - 1].trim();
    if declarator.is_empty() {
        return None;
    }

    let (params, is_variadic) = parse_parameter_list(params_section);
    let function = FpType::Function {
        return_type: Box::new(parse_type(outer_return)),
        params,
        is_variadic,
    };

    let mut ty = function;
    let mut declarator_str = declarator.to_string();

    let is_rvalue_reference = declarator_str.contains("&&");
    if is_rvalue_reference {
        declarator_str = declarator_str.replace("&&", "");
    }
    let is_reference = is_rvalue_reference || declarator_str.contains('&');
    declarator_str = declarator_str.replace('&', "");

    let pointer_count = declarator_str.chars().filter(|c| *c == '*').count();
    for _ in 0..pointer_count {
        ty = FpType::Pointer(Box::new(ty));
    }

    if is_reference {
        ty = FpType::Reference {
            base: Box::new(ty),
            is_rvalue: is_rvalue_reference,
        };
    }

    Some(ty)
}

fn find_matching_paren(text: &str, close_index: usize) -> Option<usize> {
    if text.as_bytes().get(close_index)? != &b')' {
        return None;
    }

    let mut depth = 0i32;
    for (idx, ch) in text[..=close_index].char_indices().rev() {
        if ch == ')' {
            depth += 1;
        } else if ch == '(' {
            depth -= 1;
            if depth == 0 {
                return Some(idx);
            }
        }
    }
    None
}

fn strip_array_suffixes(input: &str) -> (String, Vec<Option<usize>>) {
    let mut remaining = input.trim_end();
    let mut arrays = Vec::new();

    loop {
        let trimmed = remaining.trim_end();
        if let Some(close_idx) = trimmed.rfind(']') {
            let prefix = &trimmed[..close_idx];
            if let Some(open_idx) = prefix.rfind('[') {
                let size_str = prefix[open_idx + 1..].trim();
                let size = if size_str.is_empty() {
                    None
                } else {
                    size_str.parse::<usize>().ok()
                };
                arrays.push(size);
                remaining = prefix[..open_idx].trim_end();
                continue;
            }
        }
        break;
    }

    (remaining.to_string(), arrays)
}

fn collect_qualifiers(qual_type: &str) -> QualifierFlags {
    let mut flags = QualifierFlags::default();
    for word in qual_type.split_whitespace() {
        match word {
            "const" => flags.is_const = true,
            "volatile" => flags.is_volatile = true,
            "restrict" => flags.is_restrict = true,
            _ => {}
        }
    }
    flags
}

fn extract_return_type(qual_type: &str) -> Option<&str> {
    let trimmed = qual_type.trim();
    let idx = trimmed.find('(')?;
    Some(trimmed[..idx].trim())
}

fn location_file_path(loc: &SourceLocation) -> Option<String> {
    loc.spelling_loc
        .as_ref()
        .or_else(|| loc.expansion_loc.as_ref())
        .map(|bare| bare.file.as_ref().to_string())
}

fn normalize_path(path: &Path) -> PathBuf {
    match fs::canonicalize(path) {
        Ok(canonical) => canonical,
        Err(_) => path.to_path_buf(),
    }
}

fn parse_parameter_list(params_section: &str) -> (Vec<FpType>, bool) {
    let trimmed = params_section.trim();
    if trimmed.is_empty() {
        return (Vec::new(), false);
    }

    if trimmed == "void" {
        return (Vec::new(), false);
    }

    let mut params = Vec::new();
    let mut is_variadic = false;

    for part in split_top_level_params(params_section) {
        let arg = part.trim();
        if arg.is_empty() {
            continue;
        }

        if arg == "..." {
            is_variadic = true;
            continue;
        }

        if let Some(stripped) = arg.strip_suffix("...") {
            let before = stripped.trim_end();
            if !before.is_empty() {
                params.push(parse_type(before));
            }
            is_variadic = true;
            continue;
        }

        if arg == "void" && params.is_empty() && !is_variadic {
            continue;
        }

        params.push(parse_type(arg));
    }

    (params, is_variadic)
}

fn split_top_level_params(s: &str) -> Vec<&str> {
    let mut parts = Vec::new();
    let mut depth = 0i32;
    let mut start = 0usize;
    let bytes = s.as_bytes();
    let mut idx = 0usize;
    while idx < bytes.len() {
        let ch = bytes[idx] as char;
        match ch {
            '(' => depth += 1,
            ')' => {
                if depth > 0 {
                    depth -= 1;
                }
            }
            ',' if depth == 0 => {
                parts.push(&s[start..idx]);
                start = idx + 1;
            }
            _ => {}
        }
        idx += ch.len_utf8();
    }

    if start <= s.len() {
        parts.push(&s[start..]);
    }

    parts
}
