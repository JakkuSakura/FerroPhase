use fp_core::ast::{Ident, Ty};

/// Parsed representation of one or more WIT packages.
#[derive(Clone, Debug, PartialEq)]
pub struct WitDocument {
    pub packages: Vec<WitPackage>,
}

/// A parsed WIT package combining *service IDL* surfaces (interfaces/worlds)
/// with their accompanying *type IDL* blocks.
#[derive(Clone, Debug, PartialEq)]
pub struct WitPackage {
    pub name: String,
    pub interfaces: Vec<WitInterface>,
}

/// A WIT interface treated as a *service IDL* section describing callable APIs.
#[derive(Clone, Debug, PartialEq)]
pub struct WitInterface {
    pub name: Ident,
    pub functions: Vec<WitFunction>,
    pub types: Vec<WitType>,
}

/// A named type alias from the WIT *type IDL* surface.
#[derive(Clone, Debug, PartialEq)]
pub struct WitType {
    pub name: Ident,
    pub ty: Ty,
}

/// A callable operation lifted from service IDL into FerroPhase runtime types.
#[derive(Clone, Debug, PartialEq)]
pub struct WitFunction {
    pub name: Ident,
    pub params: Vec<WitParameter>,
    pub results: Vec<WitParameter>,
}

/// Parameter/result slots mapped from IDL shapes into runtime type references.
#[derive(Clone, Debug, PartialEq)]
pub struct WitParameter {
    pub name: Option<Ident>,
    pub ty: Ty,
}
