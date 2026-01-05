use crate::ast::{Expr, Ident, Locator, QuoteFragmentKind, QuoteItemKind, Ty, TySlot};
use crate::{common_enum, common_struct};
pub type BPattern = Box<Pattern>;
common_enum! {
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
        Type(PatternType),
        Wildcard(PatternWildcard),
    }
}

common_struct! {
    pub struct Pattern {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub ty: TySlot,
        #[serde(flatten)]
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
}
// TODO: add patterns for let, if, match, etc.
common_struct! {
    pub struct PatternTuple {
        pub patterns: Vec<Pattern>,
    }
}
common_struct! {
    pub struct PatternBind {
        pub ident: PatternIdent,
        pub pattern: Box<Pattern>,
    }
}
common_struct! {
    pub struct PatternTupleStruct {
        pub name: Locator,
        pub patterns: Vec<Pattern>,
    }
}
common_struct! {
    pub struct PatternStruct {
        pub name: Ident,
        pub fields: Vec<PatternStructField>,
        #[serde(default, skip_serializing_if = "bool_is_false")]
        pub has_rest: bool,
    }
}
common_struct! {
    pub struct PatternStructural {
        pub fields: Vec<PatternStructField>,
        #[serde(default, skip_serializing_if = "bool_is_false")]
        pub has_rest: bool,
    }
}
common_struct! {
    pub struct PatternStructField {
        pub name: Ident,
        pub rename: Option<Box<Pattern>>,
    }
}
common_struct! {
    pub struct PatternBox {
        pub pattern: Box<Pattern>,
    }
}
common_struct! {
    pub struct PatternVariant {
        pub name: Expr, // TypeExpr
        pub pattern: Option<Box<Pattern>>,
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
}
common_struct! {
    pub struct PatternWildcard {}
}

common_struct! {
    pub struct PatternQuote {
        pub fragment: QuoteFragmentKind,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub item: Option<QuoteItemKind>,
    }
}
