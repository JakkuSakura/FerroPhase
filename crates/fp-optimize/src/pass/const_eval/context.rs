// Context and data structures for const evaluation

use fp_core::ast::*;
use fp_core::ctx::ty::TypeId;
use std::collections::{HashMap, HashSet};

/// Represents a const block or expression that needs evaluation
#[derive(Debug, Clone)]
pub struct ConstBlock {
    pub id: u64,
    pub name: Option<String>,
    pub expr: AstExpr,
    pub dependencies: HashSet<u64>,
    pub state: ConstEvalState,
    pub result: Option<AstValue>,
    pub generic_context: Option<GenericEvaluationContext>,
}

/// State of const evaluation for a block
#[derive(Debug, Clone, PartialEq)]
pub enum ConstEvalState {
    NotEvaluated,
    Evaluating, // Prevents infinite recursion
    Evaluated,
    Error(String),
}

/// Context for evaluating generic const expressions
#[derive(Debug, Clone)]
pub struct GenericEvaluationContext {
    pub generic_params: Vec<GenericParam>,
    pub type_variable_mappings: HashMap<String, AstType>,
    pub specialization_key: String, // Unique key for this generic instantiation
}

/// Information about a generic function or type that needs specialization
#[derive(Debug, Clone)]
pub struct GenericCandidate {
    pub id: u64,
    pub name: String,
    pub generic_params: Vec<GenericParam>,
    pub item_type: GenericItemType,
    pub ast_node: AstNode, // The actual AST node to specialize
}

#[derive(Debug, Clone)]
pub enum GenericItemType {
    Function(ValueFunction),
    Struct(TypeStruct),
    Type(AstType),
}

/// Side effects that const evaluation can produce
#[derive(Debug, Clone)]
pub enum SideEffect {
    GenerateField {
        target_type: String,
        field_name: String,
        field_type: AstType,
    },
    GenerateMethod {
        target_type: String,
        method_name: String,
        method_body: AstExpr,
    },
    GenerateImpl {
        target_type: String,
        trait_name: String,
        methods: Vec<(String, AstExpr)>,
    },
    GenerateType {
        type_name: String,
        type_definition: AstType,
    },
}

impl ConstBlock {
    pub fn new(id: u64, name: Option<String>, expr: AstExpr) -> Self {
        Self {
            id,
            name,
            expr,
            dependencies: HashSet::new(),
            state: ConstEvalState::NotEvaluated,
            result: None,
            generic_context: None,
        }
    }

    pub fn is_evaluated(&self) -> bool {
        matches!(self.state, ConstEvalState::Evaluated)
    }

    pub fn is_evaluating(&self) -> bool {
        matches!(self.state, ConstEvalState::Evaluating)
    }

    pub fn has_error(&self) -> bool {
        matches!(self.state, ConstEvalState::Error(_))
    }
}

impl GenericCandidate {
    pub fn has_generics(&self) -> bool {
        !self.generic_params.is_empty()
    }
}