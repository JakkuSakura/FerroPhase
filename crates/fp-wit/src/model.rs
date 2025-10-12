use fp_core::ast::{Ident, Ty};

/// Parsed representation of one or more WIT packages.
#[derive(Clone, Debug, PartialEq)]
pub struct WitDocument {
    pub packages: Vec<WitPackage>,
}

/// A parsed WIT package containing interfaces and their type definitions.
#[derive(Clone, Debug, PartialEq)]
pub struct WitPackage {
    pub name: String,
    pub interfaces: Vec<WitInterface>,
}

/// A parsed WIT interface.
#[derive(Clone, Debug, PartialEq)]
pub struct WitInterface {
    pub name: Ident,
    pub functions: Vec<WitFunction>,
    pub types: Vec<WitType>,
}

/// A named type alias declared in a WIT interface.
#[derive(Clone, Debug, PartialEq)]
pub struct WitType {
    pub name: Ident,
    pub ty: Ty,
}

/// A parsed WIT function.
#[derive(Clone, Debug, PartialEq)]
pub struct WitFunction {
    pub name: Ident,
    pub params: Vec<WitParameter>,
    pub results: Vec<WitParameter>,
}

/// WIT function parameter or result mapped into FerroPhase types.
#[derive(Clone, Debug, PartialEq)]
pub struct WitParameter {
    pub name: Option<Ident>,
    pub ty: Ty,
}
