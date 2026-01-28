use crate::ast::{Expr, Ident, Locator, QuoteFragmentKind, QuoteItemKind, Ty, TySlot};
use crate::span::Span;
use crate::common_struct;
pub type BPattern = Box<Pattern>;
#[derive(Debug, Clone, PartialEq, Hash)]
pub enum PatternKind {
    Ident(PatternIdent),
    Bind(PatternBind),
    Tuple(PatternTuple),
    TupleStruct(PatternTupleStruct),
    Struct(PatternStruct),
    Structural(PatternStructural),
    Box(PatternBox),
    Variant(PatternVariant),
    Quote(PatternQuote),
    QuotePlural(PatternQuotePlural),
    Type(PatternType),
    Wildcard(PatternWildcard),
}

impl From<PatternIdent> for PatternKind {
    fn from(value: PatternIdent) -> Self {
        PatternKind::Ident(value)
    }
}

impl From<PatternBind> for PatternKind {
    fn from(value: PatternBind) -> Self {
        PatternKind::Bind(value)
    }
}

impl From<PatternTuple> for PatternKind {
    fn from(value: PatternTuple) -> Self {
        PatternKind::Tuple(value)
    }
}

impl From<PatternTupleStruct> for PatternKind {
    fn from(value: PatternTupleStruct) -> Self {
        PatternKind::TupleStruct(value)
    }
}

impl From<PatternStruct> for PatternKind {
    fn from(value: PatternStruct) -> Self {
        PatternKind::Struct(value)
    }
}

impl From<PatternStructural> for PatternKind {
    fn from(value: PatternStructural) -> Self {
        PatternKind::Structural(value)
    }
}

impl From<PatternBox> for PatternKind {
    fn from(value: PatternBox) -> Self {
        PatternKind::Box(value)
    }
}

impl From<PatternVariant> for PatternKind {
    fn from(value: PatternVariant) -> Self {
        PatternKind::Variant(value)
    }
}

impl From<PatternQuote> for PatternKind {
    fn from(value: PatternQuote) -> Self {
        PatternKind::Quote(value)
    }
}

impl From<PatternQuotePlural> for PatternKind {
    fn from(value: PatternQuotePlural) -> Self {
        PatternKind::QuotePlural(value)
    }
}

impl From<PatternType> for PatternKind {
    fn from(value: PatternType) -> Self {
        PatternKind::Type(value)
    }
}

impl From<PatternWildcard> for PatternKind {
    fn from(value: PatternWildcard) -> Self {
        PatternKind::Wildcard(value)
    }
}

common_struct! {
    pub struct Pattern {
        pub ty: TySlot,
        pub kind: PatternKind,
    }
}

impl Pattern {
    pub fn new(kind: PatternKind) -> Self {
        Self { ty: None, kind }
    }

    pub fn with_ty(kind: PatternKind, ty: TySlot) -> Self {
        Self { ty, kind }
    }

    pub fn ty(&self) -> Option<&Ty> {
        self.ty.as_ref()
    }

    pub fn ty_mut(&mut self) -> &mut TySlot {
        &mut self.ty
    }

    pub fn set_ty(&mut self, ty: Ty) {
        self.ty = Some(ty);
    }

    pub fn kind(&self) -> &PatternKind {
        &self.kind
    }

    pub fn kind_mut(&mut self) -> &mut PatternKind {
        &mut self.kind
    }

    pub fn into_parts(self) -> (TySlot, PatternKind) {
        (self.ty, self.kind)
    }

    pub fn from_parts(ty: TySlot, kind: PatternKind) -> Self {
        Self { ty, kind }
    }

    pub fn as_ident(&self) -> Option<&Ident> {
        match &self.kind {
            PatternKind::Ident(ident) => Some(&ident.ident),
            PatternKind::Bind(bind) => Some(&bind.ident.ident),
            PatternKind::Type(pattern_type) => pattern_type.pat.as_ident(),
            _ => None,
        }
    }
    pub fn make_mut(&mut self) {
        match &mut self.kind {
            PatternKind::Ident(ident) => {
                ident.mutability = Some(true);
            }
            PatternKind::Bind(bind) => {
                bind.ident.mutability = Some(true);
            }
            PatternKind::Type(PatternType { pat, .. }) => {
                pat.make_mut();
            }
            _ => {}
        }
    }

    pub fn span(&self) -> Span {
        self.kind.span()
    }
}
// TODO: add patterns for let, if, match, etc.
common_struct! {
    pub struct PatternTuple {
        pub patterns: Vec<Pattern>,
    }
}
impl PatternTuple {
    pub fn span(&self) -> Span {
        Span::union(self.patterns.iter().map(Pattern::span))
    }
}
common_struct! {
    pub struct PatternBind {
        pub ident: PatternIdent,
        pub pattern: Box<Pattern>,
    }
}
impl PatternBind {
    pub fn span(&self) -> Span {
        Span::union([self.ident.span(), self.pattern.span()])
    }
}
common_struct! {
    pub struct PatternTupleStruct {
        pub name: Locator,
        pub patterns: Vec<Pattern>,
    }
}
impl PatternTupleStruct {
    pub fn span(&self) -> Span {
        Span::union(
            Some(self.name.span())
                .into_iter()
                .chain(self.patterns.iter().map(Pattern::span)),
        )
    }
}
common_struct! {
    pub struct PatternStruct {
        pub name: Ident,
        pub fields: Vec<PatternStructField>,
        pub has_rest: bool,
    }
}
impl PatternStruct {
    pub fn span(&self) -> Span {
        Span::union(self.fields.iter().map(PatternStructField::span))
    }
}
common_struct! {
    pub struct PatternStructural {
        pub fields: Vec<PatternStructField>,
        pub has_rest: bool,
    }
}
impl PatternStructural {
    pub fn span(&self) -> Span {
        Span::union(self.fields.iter().map(PatternStructField::span))
    }
}
common_struct! {
    pub struct PatternStructField {
        pub name: Ident,
        pub rename: Option<Box<Pattern>>,
    }
}
impl PatternStructField {
    pub fn span(&self) -> Span {
        self.rename
            .as_ref()
            .map(|pat| pat.span())
            .unwrap_or_else(Span::null)
    }
}
common_struct! {
    pub struct PatternBox {
        pub pattern: Box<Pattern>,
    }
}
impl PatternBox {
    pub fn span(&self) -> Span {
        self.pattern.span()
    }
}
common_struct! {
    pub struct PatternVariant {
        pub name: Expr, // TypeExpr
        pub pattern: Option<Box<Pattern>>,
    }

}
impl PatternVariant {
    pub fn span(&self) -> Span {
        Span::union(
            [
                Some(self.name.span()),
                self.pattern.as_ref().map(|pat| pat.span()),
            ]
            .into_iter()
            .flatten(),
        )
    }
}
common_struct! {
    /// let x: T = expr;
    /// where x: T is PatternType
    pub struct PatternType {
        pub pat: BPattern,
        pub ty: Ty,
    }
}
impl PatternType {
    pub fn new(pat: Pattern, ty: Ty) -> Self {
        Self {
            pat: pat.into(),
            ty,
        }
    }

    pub fn span(&self) -> Span {
        Span::union([self.pat.span(), self.ty.span()])
    }
}

fn bool_is_false(value: &bool) -> bool {
    !*value
}

impl From<PatternKind> for Pattern {
    fn from(kind: PatternKind) -> Self {
        Pattern::new(kind)
    }
}

common_struct! {
    /// pattern like `mut x`
    pub struct PatternIdent {
        pub ident: Ident,
        pub mutability: Option<bool>,
    }
}
impl PatternIdent {
    pub fn new(ident: Ident) -> Self {
        Self {
            ident,
            mutability: None,
        }
    }

    pub fn span(&self) -> Span {
        self.ident.span()
    }
}
common_struct! {
    pub struct PatternWildcard {}
}
impl PatternWildcard {
    pub fn span(&self) -> Span {
        Span::null()
    }
}

common_struct! {
    pub struct PatternQuote {
        pub fragment: QuoteFragmentKind,
        pub item: Option<QuoteItemKind>,
        pub fields: Vec<PatternStructField>,
        pub has_rest: bool,
    }
}
impl PatternQuote {
    pub fn span(&self) -> Span {
        Span::union(self.fields.iter().map(PatternStructField::span))
    }
}

common_struct! {
    pub struct PatternQuotePlural {
        pub fragment: QuoteFragmentKind,
        pub patterns: Vec<Pattern>,
    }
}
impl PatternQuotePlural {
    pub fn span(&self) -> Span {
        Span::union(self.patterns.iter().map(Pattern::span))
    }
}

impl PatternKind {
    pub fn span(&self) -> Span {
        match self {
            PatternKind::Ident(ident) => ident.span(),
            PatternKind::Bind(bind) => bind.span(),
            PatternKind::Tuple(tuple) => tuple.span(),
            PatternKind::TupleStruct(tuple) => tuple.span(),
            PatternKind::Struct(pattern) => pattern.span(),
            PatternKind::Structural(pattern) => pattern.span(),
            PatternKind::Box(pattern) => pattern.span(),
            PatternKind::Variant(pattern) => pattern.span(),
            PatternKind::Quote(pattern) => pattern.span(),
            PatternKind::QuotePlural(pattern) => pattern.span(),
            PatternKind::Type(pattern) => pattern.span(),
            PatternKind::Wildcard(pattern) => pattern.span(),
        }
    }
}
