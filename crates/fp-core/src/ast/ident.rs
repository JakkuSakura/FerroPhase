//! AST-specific identifier types
//!
//! Each compilation stage has its own identifier representation:
//! - AST: Ident, Path, Name (this module)
//! - HIR: Symbol (String), hir::Path
//! - MIR: Symbol (String), Vec<Symbol>
//! - LIR: String

use serde::{Deserialize, Serialize};

use crate::ast::path::PathPrefix;
use crate::ast::{Expr, ExprKind, Value};
use crate::span::Span;

/// A simple identifier - a single name like `foo` or `MyStruct`
#[derive(Debug, Clone, Serialize, Deserialize, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Ident {
    pub name: String,
}

impl Ident {
    pub fn new(name: impl Into<String>) -> Self {
        Self { name: name.into() }
    }

    pub fn as_str(&self) -> &str {
        self.name.as_str()
    }

    pub fn is_root(&self) -> bool {
        self.name == "__root__"
    }

    pub fn root() -> Self {
        Self::new("__root__")
    }

    pub fn span(&self) -> Span {
        Span::null()
    }
}

impl std::fmt::Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl From<Ident> for String {
    fn from(ident: Ident) -> Self {
        ident.name
    }
}

impl From<&Ident> for String {
    fn from(ident: &Ident) -> Self {
        ident.name.clone()
    }
}

impl From<String> for Ident {
    fn from(name: String) -> Self {
        Ident::new(name)
    }
}

impl From<&str> for Ident {
    fn from(name: &str) -> Self {
        Ident::new(name)
    }
}

/// A path is a sequence of identifiers separated by `::`, like `std::io::File`.
/// The prefix captures leading qualifiers like `::`, `crate`, `self`, or `super`.
#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq)]
pub struct Path {
    /// Source span covering the complete path.
    pub span: Span,
    pub prefix: PathPrefix,
    pub segments: Vec<PathSegment>,
}

impl Path {
    pub fn new(prefix: PathPrefix, segments: Vec<PathSegment>) -> Self {
        debug_assert!(
            !segments.is_empty() || !matches!(prefix, PathPrefix::Plain),
            "Plain path must have at least one segment"
        );
        Self {
            span: Span::null(),
            prefix,
            segments,
        }
    }

    pub fn with_span(span: Span, prefix: PathPrefix, segments: Vec<PathSegment>) -> Self {
        debug_assert!(
            !segments.is_empty() || !matches!(prefix, PathPrefix::Plain),
            "Plain path must have at least one segment"
        );
        Self {
            span,
            prefix,
            segments,
        }
    }

    pub fn plain(mut segments: Vec<Ident>) -> Self {
        // Keep programmatically-built paths consistent with parsed Rust
        // paths: these keywords are prefixes, never ordinary identifiers.
        let prefix = match segments.first().map(Ident::as_str) {
            _ if segments.len() < 2 => PathPrefix::Plain,
            Some("crate") => {
                segments.remove(0);
                PathPrefix::Crate
            }
            Some("self") => {
                segments.remove(0);
                PathPrefix::SelfMod
            }
            Some("super") => {
                let depth = segments
                    .iter()
                    .take_while(|segment| segment.as_str() == "super")
                    .count();
                segments.drain(..depth);
                PathPrefix::Super(depth)
            }
            _ => PathPrefix::Plain,
        };
        Self::new(
            prefix,
            segments.into_iter().map(PathSegment::from_ident).collect(),
        )
    }

    pub fn from_ident(ident: Ident) -> Self {
        Self::new(PathPrefix::Plain, vec![PathSegment::from_ident(ident)])
    }

    pub fn is_empty(&self) -> bool {
        self.segments.is_empty()
    }

    pub fn len(&self) -> usize {
        self.segments.len()
    }

    pub fn first(&self) -> Option<&PathSegment> {
        self.segments.first()
    }

    pub fn last(&self) -> &PathSegment {
        self.segments.last().unwrap()
    }

    pub fn push(&mut self, segment: impl Into<PathSegment>) {
        self.segments.push(segment.into());
    }

    pub fn join(&self, separator: &str) -> String {
        self.segments
            .iter()
            .map(|s| s.as_str())
            .collect::<Vec<_>>()
            .join(separator)
    }

    pub fn try_into_ident(self) -> Option<Ident> {
        if self.prefix != PathPrefix::Plain || self.segments.len() != 1 {
            return None;
        }
        self.segments
            .into_iter()
            .next()
            .map(|segment| segment.ident)
    }

    pub fn is_root(&self) -> bool {
        self.prefix == PathPrefix::Root && self.segments.is_empty()
    }

    /// Whether this path uses the global `::` prefix.
    pub fn is_global(&self) -> bool {
        self.prefix == PathPrefix::Root
    }

    /// Whether this path is one plain identifier without generic arguments.
    pub fn is_single_argless_ident(&self) -> bool {
        self.prefix == PathPrefix::Plain
            && self.segments.len() == 1
            && self.segments[0].args.is_none()
    }

    /// Return the identifier for a plain, argument-free one-segment path.
    pub fn as_single_argless_ident(&self) -> Option<Ident> {
        self.is_single_argless_ident()
            .then(|| self.segments[0].ident.clone())
    }

    pub fn root() -> Self {
        Self::new(PathPrefix::Root, Vec::new())
    }

    pub fn with_ident(&self, ident: Ident) -> Self {
        let mut segments = self.segments.clone();
        segments.push(ident.into());
        Self::with_span(self.span, self.prefix, segments)
    }

    pub fn span(&self) -> Span {
        self.span
            .or(Span::union(self.segments.iter().map(PathSegment::span)))
    }

    pub fn segments(&self) -> &[PathSegment] {
        &self.segments
    }
}

/// Type qualification on a path, matching rustc's AST representation.
#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq)]
pub struct QSelf {
    pub ty: Box<Ty>,
    /// Span of the trait portion in an explicit `as Trait` qualification.
    /// For trait-less `<T>::Assoc` paths this is left dummy, as in the
    /// parser's representation.
    pub path_span: Span,
    /// Insertion index of the qualified self in the complete path. For
    /// `<T as Trait>::Assoc`, whose path is `Trait::Assoc`, this is `1`;
    /// for trait-less `<T>::Assoc` it is `0`.
    pub position: usize,
}

impl std::fmt::Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.prefix {
            PathPrefix::Root => {
                if self.segments.is_empty() {
                    write!(f, "::")
                } else {
                    write!(
                        f,
                        "::{}",
                        self.segments
                            .iter()
                            .map(ToString::to_string)
                            .collect::<Vec<_>>()
                            .join("::")
                    )
                }
            }
            PathPrefix::Crate => {
                if self.segments.is_empty() {
                    write!(f, "crate")
                } else {
                    write!(
                        f,
                        "crate::{}",
                        self.segments
                            .iter()
                            .map(ToString::to_string)
                            .collect::<Vec<_>>()
                            .join("::")
                    )
                }
            }
            PathPrefix::SelfMod => {
                if self.segments.is_empty() {
                    write!(f, "self")
                } else {
                    write!(
                        f,
                        "self::{}",
                        self.segments
                            .iter()
                            .map(ToString::to_string)
                            .collect::<Vec<_>>()
                            .join("::")
                    )
                }
            }
            PathPrefix::Super(depth) => {
                let prefix = std::iter::repeat("super")
                    .take(depth)
                    .collect::<Vec<_>>()
                    .join("::");
                if self.segments.is_empty() {
                    write!(f, "{}", prefix)
                } else {
                    write!(
                        f,
                        "{}::{}",
                        prefix,
                        self.segments
                            .iter()
                            .map(ToString::to_string)
                            .collect::<Vec<_>>()
                            .join("::")
                    )
                }
            }
            PathPrefix::Plain => write!(
                f,
                "{}",
                self.segments
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join("::")
            ),
        }
    }
}

impl From<Ident> for Path {
    fn from(ident: Ident) -> Self {
        Self::from_ident(ident)
    }
}

impl From<&Ident> for Path {
    fn from(ident: &Ident) -> Self {
        Self::from_ident(ident.clone())
    }
}

impl From<&Path> for Path {
    fn from(path: &Path) -> Self {
        path.clone()
    }
}

/// A path segment with optional generic arguments, matching rustc's AST path
/// representation. Generic arguments belong to the segment they parameterize.
#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq)]
pub struct PathSegment {
    pub ident: Ident,
    #[serde(default)]
    /// Type/lifetime arguments attached to this segment. `None` means the
    /// source omitted an argument list; `Some` may still hold an explicitly
    /// empty list, matching rustc's AST representation.
    pub args: Option<Box<GenericArgs>>,
}

impl PathSegment {
    pub fn new(ident: Ident, args: Option<GenericArgs>) -> Self {
        Self {
            ident,
            args: args.map(Box::new),
        }
    }

    pub fn with_args(ident: Ident, args: Option<GenericArgs>) -> Self {
        Self {
            ident,
            args: args.map(Box::new),
        }
    }

    pub fn from_ident(ident: Ident) -> Self {
        Self {
            ident,
            args: None,
        }
    }

    pub fn as_str(&self) -> &str {
        self.ident.as_str()
    }

    /// Source span for this segment, including its generic arguments when
    /// they are present. The identifier span is currently unavailable in the
    /// compact AST identifier representation, so generated argument spans are
    /// used as the fallback.
    pub fn span(&self) -> Span {
        Span::union([
            self.ident.span(),
            self.args
                .as_deref()
                .map(GenericArgs::span)
                .unwrap_or_else(Span::null),
        ])
    }
}

/// Generic arguments attached to one AST path segment.
///
/// The structured representation preserves the distinction between types,
/// constants, lifetimes, and associated-item constraints instead of forcing
/// every argument through a type-only list.
#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq)]
pub enum GenericArgs {
    AngleBracketed(AngleBracketedArgs),
    Parenthesized(ParenthesizedArgs),
    /// Return-type notation, `Trait(..)`, as distinct from the concrete
    /// parenthesized `Trait(Args) -> Output` form.
    ParenthesizedElided(Span),
}

#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq)]
pub struct AngleBracketedArgs {
    pub span: Span,
    pub args: Vec<AngleBracketedArg>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq)]
pub struct ParenthesizedArgs {
    pub span: Span,
    pub inputs: Vec<Ty>,
    /// Span covering the parenthesized input list, including `(` and `)`.
    /// This is separate from `span`, which also includes a return type when
    /// one is present.
    pub inputs_span: Span,
    /// Return type notation for the parenthesized trait arguments.  Keeping
    /// the default case distinct from an explicit type mirrors rustc's
    /// `FnRetTy` and preserves the source distinction for later lowering.
    pub output: FnRetTy,
}

#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq)]
pub enum FnRetTy {
    /// No return type was written. The span marks where one could be added.
    Default(Span),
    /// An explicit `-> Ty` return type.
    Ty(Box<Ty>),
}

impl FnRetTy {
    pub fn span(&self) -> Span {
        match self {
            Self::Default(span) => *span,
            Self::Ty(ty) => ty.span(),
        }
    }
}

/// A lifetime argument in the AST.
///
/// Rustc represents lifetimes as nodes with their own source span instead of
/// embedding their spelling directly in `GenericArg`. Keeping that boundary
/// here lets generic-argument spans survive parsing and HIR lowering.
#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq, Eq)]
pub struct Lifetime {
    pub ident: Ident,
    pub span: Span,
}

impl Lifetime {
    pub fn new(ident: impl Into<Ident>, span: Span) -> Self {
        Self {
            ident: ident.into(),
            span,
        }
    }

    pub fn from_name(name: impl Into<String>, span: Span) -> Self {
        Self::new(Ident::new(name), span)
    }

    pub fn as_str(&self) -> &str {
        self.ident.as_str()
    }
}

impl From<&str> for Lifetime {
    fn from(name: &str) -> Self {
        Self::from_name(name, Span::null())
    }
}

impl From<String> for Lifetime {
    fn from(name: String) -> Self {
        Self::from_name(name, Span::null())
    }
}

impl PartialEq<&str> for Lifetime {
    fn eq(&self, other: &&str) -> bool {
        self.as_str() == *other
    }
}

impl PartialEq<str> for Lifetime {
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}

impl PartialEq<Lifetime> for &str {
    fn eq(&self, other: &Lifetime) -> bool {
        *self == other.as_str()
    }
}

impl std::fmt::Display for Lifetime {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.ident.fmt(f)
    }
}

impl GenericArgs {
    pub fn from_types(types: &[Ty]) -> Self {
        Self::AngleBracketed(AngleBracketedArgs {
            span: Span::null(),
            args: types
                .iter()
                .cloned()
                .map(ast_ty_to_generic_arg)
                .map(AngleBracketedArg::Arg)
                .collect(),
        })
    }

    pub fn is_angle_bracketed(&self) -> bool {
        matches!(self, Self::AngleBracketed(_))
    }

    pub fn legacy_types(&self) -> Vec<Ty> {
        match self {
            Self::AngleBracketed(args) => args
                .args
                .iter()
                .filter_map(|arg| match arg {
                    AngleBracketedArg::Arg(GenericArg::Type(ty)) => Some((**ty).clone()),
                    _ => None,
                })
                .collect(),
            Self::Parenthesized(args) => args.inputs.clone(),
            Self::ParenthesizedElided(_) => Vec::new(),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Self::AngleBracketed(args) => args.span,
            Self::Parenthesized(args) => args.span,
            Self::ParenthesizedElided(span) => *span,
        }
    }
}

impl GenericArg {
    pub fn from_ty(ty: Ty) -> Self {
        ast_ty_to_generic_arg(ty)
    }

    /// Source span of this generic argument, matching rustc's AST helper.
    pub fn span(&self) -> Span {
        match self {
            Self::Lifetime(lifetime) => lifetime.span,
            Self::Type(ty) => ty.span(),
            Self::Const(expr) => expr.span(),
        }
    }
}

fn ast_ty_to_generic_arg(ty: Ty) -> GenericArg {
    match ty {
        Ty::Expr(expr) => match expr.kind() {
            ExprKind::Name(name)
                if name.path.segments.len() == 1
                    && name.path.segments[0].as_str().starts_with('\'') =>
            {
                GenericArg::Lifetime(Lifetime::from_name(
                    name.path.segments[0].as_str(),
                    name.path.span(),
                ))
            }
            ExprKind::Value(value)
                if matches!(
                    value.as_ref(),
                    Value::Int(_)
                        | Value::UInt(_)
                        | Value::BigInt(_)
                        | Value::Bool(_)
                        | Value::Decimal(_)
                        | Value::BigDecimal(_)
                        | Value::Char(_)
                        | Value::String(_)
                        | Value::Bytes(_)
                        | Value::Unit(_)
                        | Value::Null(_)
                        | Value::None(_)
                ) =>
            {
                GenericArg::Const(expr.clone())
            }
            _ => GenericArg::Type(Box::new(Ty::Expr(expr))),
        },
        other => GenericArg::Type(Box::new(other)),
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq)]
pub enum GenericArg {
    Lifetime(Lifetime),
    Type(Box<Ty>),
    Const(Box<Expr>),
}

/// An angle-bracketed argument in rustc's AST is either a positional generic
/// argument or an associated-item constraint. Keeping constraints outside
/// `GenericArg` preserves the distinction used by HIR lowering.
#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq)]
pub enum AngleBracketedArg {
    Arg(GenericArg),
    Constraint(AssocItemConstraint),
}

impl AngleBracketedArg {
    pub fn span(&self) -> Span {
        match self {
            Self::Arg(arg) => arg.span(),
            Self::Constraint(constraint) => constraint.span(),
        }
    }
}

/// A constraint on an associated item in an angle-bracketed argument list.
///
/// Rustc keeps generic arguments attached to the constrained item itself, so
/// `Trait<Item<'a> = T>` is represented differently from `Trait<Item = T>`.
/// Keep that distinction here instead of throwing away the `<...>` portion.
#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq)]
pub struct AssocItemConstraint {
    /// Source span covering the complete associated-item constraint.
    #[serde(default)]
    pub span: Span,
    /// The constrained associated item identifier, matching rustc AST.
    pub ident: Ident,
    pub gen_args: Option<GenericArgs>,
    pub kind: AssocItemConstraintKind,
}

impl AssocItemConstraint {
    /// Source span of this constraint, falling back to its generic arguments
    /// and payload when a generated node has no explicit span.
    pub fn span(&self) -> Span {
        let payload = match &self.kind {
            AssocItemConstraintKind::Equality { term } => term.span(),
            AssocItemConstraintKind::Bound { bounds } => {
                Span::union(bounds.iter().map(Ty::span))
            }
        };
        let gen_args = self
            .gen_args
            .as_ref()
            .map(GenericArgs::span)
            .unwrap_or_else(Span::null);
        self.span.or(Span::union([gen_args, payload]))
    }
}

/// The right-hand side of an associated-item equality constraint.
///
/// Rustc keeps associated type and associated const bindings distinct in the
/// AST. Keeping the distinction here prevents a const expression such as
/// `Trait<VALUE = 1>` from being reinterpreted as a malformed type path during
/// HIR lowering.
#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq)]
pub enum Term {
    Ty(Box<Ty>),
    Const(Box<Expr>),
}

impl Term {
    pub fn span(&self) -> Span {
        match self {
            Self::Ty(ty) => ty.span(),
            Self::Const(expr) => expr.span(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq)]
pub enum AssocItemConstraintKind {
    Equality { term: Term },
    Bound { bounds: Vec<Ty> },
}

impl From<Ident> for PathSegment {
    fn from(ident: Ident) -> Self {
        Self::from_ident(ident)
    }
}

impl From<&str> for PathSegment {
    fn from(name: &str) -> Self {
        Self::from_ident(Ident::new(name))
    }
}

impl From<String> for PathSegment {
    fn from(name: String) -> Self {
        Self::from_ident(Ident::new(name))
    }
}

impl Eq for PathSegment {}
impl Eq for Path {}

impl std::fmt::Display for PathSegment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ident)?;
        if let Some(arguments) = &self.args {
            write!(f, "{arguments}")?;
        }
        Ok(())
    }
}

impl std::fmt::Display for GenericArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Lifetime(name) => write!(f, "{name}"),
            Self::Type(ty) => write!(f, "{ty}"),
            Self::Const(expr) => write!(f, "{{ {expr} }}"),
        }
    }
}

impl std::fmt::Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ty(ty) => ty.fmt(f),
            Self::Const(expr) => expr.fmt(f),
        }
    }
}

impl std::fmt::Display for AngleBracketedArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Arg(arg) => arg.fmt(f),
            Self::Constraint(AssocItemConstraint {
                ident,
                gen_args,
                kind: AssocItemConstraintKind::Equality { term },
                ..
            }) => {
                write!(f, "{ident}")?;
                if let Some(args) = gen_args {
                    write!(f, "{args}")?;
                }
                write!(f, " = {term}")
            }
            Self::Constraint(AssocItemConstraint {
                ident,
                gen_args,
                kind: AssocItemConstraintKind::Bound { bounds },
                ..
            }) => {
                write!(f, "{ident}")?;
                if let Some(args) = gen_args {
                    write!(f, "{args}")?;
                }
                write!(
                    f,
                    ": {}",
                    bounds
                        .iter()
                        .map(ToString::to_string)
                        .collect::<Vec<_>>()
                        .join(" + ")
                )
            }
        }
    }
}

impl std::fmt::Display for GenericArgs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::AngleBracketed(args) => write!(
                f,
                "<{}>",
                args.args
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::Parenthesized(ParenthesizedArgs { inputs, output, .. }) => {
                write!(
                    f,
                    "({})",
                    inputs
                        .iter()
                        .map(ToString::to_string)
                        .collect::<Vec<_>>()
                        .join(", ")
                )?;
                if let FnRetTy::Ty(output) = output {
                    write!(f, " -> {output}")?;
                }
                Ok(())
            }
            Self::ParenthesizedElided(_) => f.write_str("(..)"),
        }
    }
}

/// A name use carries an optional qualified self type and its path.
#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq)]
pub struct Name {
    pub qself: Option<QSelf>,
    pub path: Path,
}

impl Name {
    pub fn ident(name: impl Into<String>) -> Self {
        Self {
            qself: None,
            path: Path::from_ident(Ident::new(name)),
        }
    }

    pub fn path(path: Path) -> Self {
        Self { qself: None, path }
    }

    pub fn from_ident(ident: Ident) -> Self {
        Self {
            qself: None,
            path: Path::from_ident(ident),
        }
    }

    pub fn to_path(&self) -> Path {
        self.path.clone()
    }

    pub fn as_ident(&self) -> Option<&Ident> {
        (self.path.prefix == PathPrefix::Plain
            && self.path.segments.len() == 1
            && self.path.segments[0].args.is_none())
        .then_some(&self.path.segments[0].ident)
    }

    pub fn span(&self) -> Span {
        self.path.span()
    }
}

impl std::fmt::Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let Some(qself) = &self.qself else {
            return write!(f, "{}", self.path);
        };
        let render_segments = |segments: &[PathSegment]| {
            segments
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join("::")
        };
        // `position` is the insertion point of the qself in the complete
        // path.  A non-zero position therefore denotes an explicit
        // `<T as Trait>` qualification; zero is the trait-less `<T>::Assoc`
        // form, even when the associated tail has multiple segments.
        let explicit_trait = qself.position > 0;
        if !explicit_trait {
            write!(f, "<{}>", qself.ty)?;
            let associated = render_segments(&self.path.segments);
            if !associated.is_empty() {
                write!(f, "::{associated}")?;
            }
            return Ok(());
        }
        if self.path.segments.is_empty() {
            return write!(f, "<{}>", qself.ty);
        }
        let position = qself.position.min(self.path.segments.len());
        let trait_path = render_segments(&self.path.segments[..position]);
        write!(f, "<{} as {}>", qself.ty, trait_path)?;
        let associated = render_segments(&self.path.segments[position..]);
        if !associated.is_empty() {
            write!(f, "::{associated}")?;
        }
        Ok(())
    }
}

// Import Ty from parent module for PathSegment
use super::Ty;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parameterized_path_retains_per_segment_arguments() {
        let path = Path::new(
            PathPrefix::Plain,
            vec![PathSegment::new(Ident::new("Vec"), None)],
        );
        assert_eq!(path.segments.len(), 1);
        assert_eq!(path.segments[0].ident.as_str(), "Vec");
        assert!(path.segments[0].args.is_none());
    }

    #[test]
    fn qualified_path_keeps_qself_outside_path_segments() {
        let ty = Ty::ident(Ident::new("Vec"));
        let path = Path::new(
            PathPrefix::Plain,
            vec![PathSegment::from("Trait"), PathSegment::from("Item")],
        );
        let qualified = Name {
            qself: Some(QSelf {
                ty: Box::new(ty),
                path_span: Span::null(),
                position: 1,
            }),
            path,
        };
        assert_eq!(qualified.path.join("::"), "Trait::Item");
        assert_eq!(qualified.qself.as_ref().unwrap().position, 1);
    }

    #[test]
    fn path_segment_retains_structured_generic_arguments() {
        let segment = PathSegment::new(
            Ident::new("Array"),
            Some(GenericArgs::AngleBracketed(AngleBracketedArgs {
                span: Span::null(),
                args: vec![
                    AngleBracketedArg::Arg(GenericArg::Type(Box::new(Ty::ident(Ident::new("T"))))),
                    AngleBracketedArg::Arg(GenericArg::Const(Box::new(Expr::ident(Ident::new(
                        "N",
                    ))))),
                ],
            })),
        );
        let Some(arguments) = segment.args else {
            panic!("expected generic arguments");
        };
        let GenericArgs::AngleBracketed(args) = *arguments else {
            panic!("expected angle-bracketed arguments");
        };
        assert!(matches!(
            args.args[0],
            AngleBracketedArg::Arg(GenericArg::Type(_))
        ));
        assert!(matches!(
            args.args[1],
            AngleBracketedArg::Arg(GenericArg::Const(_))
        ));
    }

    #[test]
    fn plain_constructor_normalizes_rust_path_prefixes() {
        let bare_self = Path::plain(vec![Ident::new("self")]);
        assert_eq!(bare_self.prefix, PathPrefix::Plain);

        let crate_path = Path::plain(vec![Ident::new("crate"), Ident::new("module")]);
        assert_eq!(crate_path.prefix, PathPrefix::Crate);
        assert_eq!(crate_path.join("::"), "module");

        let self_path = Path::plain(vec![Ident::new("self"), Ident::new("Item")]);
        assert_eq!(self_path.prefix, PathPrefix::SelfMod);
        assert_eq!(self_path.join("::"), "Item");

        let super_path = Path::plain(vec![
            Ident::new("super"),
            Ident::new("super"),
            Ident::new("Item"),
        ]);
        assert_eq!(super_path.prefix, PathPrefix::Super(2));
        assert_eq!(super_path.join("::"), "Item");
    }
}
