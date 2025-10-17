//! AST representation for C/C++ constructs
//!
//! This module provides types for representing C/C++ AST elements
//! that are extracted from clang output.

use serde::{Deserialize, Serialize};

/// Represents a C/C++ translation unit (source file)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TranslationUnit {
    pub file_path: String,
    pub declarations: Vec<Declaration>,
}

/// Top-level declarations in C/C++
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Declaration {
    Function(FunctionDecl),
    Variable(VarDecl),
    Typedef(TypedefDecl),
    Struct(StructDecl),
    Enum(EnumDecl),
    Union(UnionDecl),
}

/// Function declaration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FunctionDecl {
    pub name: String,
    pub return_type: Type,
    pub parameters: Vec<Parameter>,
    pub is_variadic: bool,
    pub is_definition: bool,
    pub storage_class: StorageClass,
}

/// Variable declaration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VarDecl {
    pub name: String,
    pub var_type: Type,
    pub storage_class: StorageClass,
    pub is_extern: bool,
    pub is_static: bool,
    pub is_const: bool,
}

/// Typedef declaration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypedefDecl {
    pub name: String,
    pub aliased_type: Type,
}

/// Struct declaration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StructDecl {
    pub name: Option<String>,
    pub fields: Vec<FieldDecl>,
    pub is_union: bool,
}

/// Union declaration (similar to struct)
pub type UnionDecl = StructDecl;

/// Enum declaration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EnumDecl {
    pub name: Option<String>,
    pub enumerators: Vec<Enumerator>,
}

/// Enumerator in an enum
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Enumerator {
    pub name: String,
    pub value: Option<i64>,
}

/// Struct/union field
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FieldDecl {
    pub name: Option<String>,
    pub field_type: Type,
    pub bit_width: Option<u32>,
}

/// Function parameter
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Parameter {
    pub name: Option<String>,
    pub param_type: Type,
}

/// C/C++ type representation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Type {
    Void,
    Bool,
    Char,
    UChar,
    Short,
    UShort,
    Int,
    UInt,
    Long,
    ULong,
    LongLong,
    ULongLong,
    Float,
    Double,
    LongDouble,
    Pointer(Box<Type>),
    Array(Box<Type>, Option<usize>),
    Function {
        return_type: Box<Type>,
        params: Vec<Type>,
        is_variadic: bool,
    },
    Reference {
        base: Box<Type>,
        is_rvalue: bool,
    },
    Struct(String),
    Union(String),
    Enum(String),
    Typedef(String),
    Qualified {
        base: Box<Type>,
        is_const: bool,
        is_volatile: bool,
        is_restrict: bool,
    },
    Custom(String),
}

/// Storage class specifiers
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum StorageClass {
    None,
    Auto,
    Register,
    Static,
    Extern,
    Typedef,
}

impl Type {
    /// Check if this is a pointer type
    pub fn is_pointer(&self) -> bool {
        matches!(self, Type::Pointer(_))
    }

    /// Check if this is an array type
    pub fn is_array(&self) -> bool {
        matches!(self, Type::Array(_, _))
    }

    /// Check if this is a function type
    pub fn is_function(&self) -> bool {
        matches!(self, Type::Function { .. })
    }

    /// Get the base type if this is a qualified type
    pub fn unqualified(&self) -> &Type {
        match self {
            Type::Qualified { base, .. } => base.unqualified(),
            _ => self,
        }
    }
}
