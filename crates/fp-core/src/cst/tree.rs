use crate::span::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CstCategory {
    Root,
    Expr,
    Type,
    Pattern,
    Stmt,
    Item,
    Error,
    Other,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CstKind {
    Root,

    ItemList,
    ItemFn,
    ItemStruct,
    ItemEnum,
    ItemTrait,
    ItemImpl,
    ItemMod,
    ItemUse,
    ItemExternCrate,
    ItemConst,
    ItemStatic,
    ItemTypeAlias,
    ItemMacro,
    ItemExpr,

    AttrOuter,
    AttrInner,
    AttrMetaPath,

    FnSig,
    FnParam,
    FnReceiver,
    FnRet,

    VisibilityPublic,
    VisibilityCrate,
    VisibilityRestricted,
    VisibilityPrivate,
    VisibilityInherited,

    GenericParams,
    GenericParam,

    UseTreePath,
    UseTreeGroup,
    UseTreeGlob,
    UseTreeRename,
    UseTreeSelf,
    UseTreeSuper,
    UseTreeRoot,

    StructFieldDecl,
    EnumVariantDecl,
    TraitMember,
    ImplMember,
    ExprBinary,
    ExprRange,
    ExprCast,
    ExprUnary,
    ExprTry,
    ExprAwait,
    ExprSelect,
    ExprIndex,
    ExprCall,
    ExprMacroCall,
    ExprBlock,
    ExprIf,
    ExprLoop,
    ExprWhile,
    ExprFor,
    ExprMatch,
    ExprClosure,
    ExprQuote,
    ExprQuoteToken,
    ExprSplice,
    ExprAsync,
    ExprConstBlock,
    ExprReturn,
    ExprBreak,
    ExprContinue,
    ExprUnit,
    ExprTuple,
    ExprArray,
    ExprArrayRepeat,
    ExprStruct,
    ExprStructural,
    ExprParen,
    ExprName,
    ExprPath,
    ExprNumber,
    ExprString,

    TyUnit,
    TyUnknown,
    TyPath,
    TyRef,
    TySlice,
    TyArray,
    TyFn,
    TyTuple,
    TyStructural,
    TyField,
    TyBinary,
    TyOptional,
    TyValue,
    TyExpr,
    TyImplTraits,
    TyMacroCall,
    TyNot,

    PatternIdent,
    PatternWildcard,
    PatternTuple,
    PatternType,

    MatchArm,
    StructField,
    BlockStmtItem,
    BlockStmtLet,
    BlockStmtExpr,

    Error,
}

impl CstKind {
    pub fn category(self) -> CstCategory {
        match self {
            CstKind::Root => CstCategory::Root,

            CstKind::ItemList
            | CstKind::ItemFn
            | CstKind::ItemStruct
            | CstKind::ItemEnum
            | CstKind::ItemTrait
            | CstKind::ItemImpl
            | CstKind::ItemMod
            | CstKind::ItemUse
            | CstKind::ItemExternCrate
            | CstKind::ItemConst
            | CstKind::ItemStatic
            | CstKind::ItemTypeAlias
            | CstKind::ItemMacro
            | CstKind::ItemExpr => CstCategory::Item,

            CstKind::ExprBinary
            | CstKind::ExprRange
            | CstKind::ExprCast
            | CstKind::ExprUnary
            | CstKind::ExprTry
            | CstKind::ExprAwait
            | CstKind::ExprSelect
            | CstKind::ExprIndex
            | CstKind::ExprCall
            | CstKind::ExprMacroCall
            | CstKind::ExprBlock
            | CstKind::ExprIf
            | CstKind::ExprLoop
            | CstKind::ExprWhile
            | CstKind::ExprFor
            | CstKind::ExprMatch
            | CstKind::ExprClosure
            | CstKind::ExprQuote
            | CstKind::ExprQuoteToken
            | CstKind::ExprSplice
            | CstKind::ExprAsync
            | CstKind::ExprConstBlock
            | CstKind::ExprReturn
            | CstKind::ExprBreak
            | CstKind::ExprContinue
            | CstKind::ExprUnit
            | CstKind::ExprTuple
            | CstKind::ExprArray
            | CstKind::ExprArrayRepeat
            | CstKind::ExprStruct
            | CstKind::ExprStructural
            | CstKind::ExprParen
            | CstKind::ExprName
            | CstKind::ExprPath
            | CstKind::ExprNumber
            | CstKind::ExprString => CstCategory::Expr,

            CstKind::TyUnit
            | CstKind::TyUnknown
            | CstKind::TyPath
            | CstKind::TyRef
            | CstKind::TySlice
            | CstKind::TyArray
            | CstKind::TyFn
            | CstKind::TyTuple
            | CstKind::TyStructural
            | CstKind::TyField
            | CstKind::TyBinary
            | CstKind::TyOptional
            | CstKind::TyValue
            | CstKind::TyExpr
            | CstKind::TyImplTraits
            | CstKind::TyMacroCall
            | CstKind::TyNot => CstCategory::Type,

            CstKind::PatternIdent
            | CstKind::PatternWildcard
            | CstKind::PatternTuple
            | CstKind::PatternType => CstCategory::Pattern,

            CstKind::BlockStmtItem | CstKind::BlockStmtLet | CstKind::BlockStmtExpr => {
                CstCategory::Stmt
            }

            // Non-expression nodes that still participate in CST.
            CstKind::AttrOuter
            | CstKind::AttrInner
            | CstKind::AttrMetaPath
            | CstKind::FnSig
            | CstKind::FnParam
            | CstKind::FnReceiver
            | CstKind::FnRet
            | CstKind::VisibilityPublic
            | CstKind::VisibilityCrate
            | CstKind::VisibilityRestricted
            | CstKind::VisibilityPrivate
            | CstKind::VisibilityInherited
            | CstKind::GenericParams
            | CstKind::GenericParam
            | CstKind::UseTreePath
            | CstKind::UseTreeGroup
            | CstKind::UseTreeGlob
            | CstKind::UseTreeRename
            | CstKind::UseTreeSelf
            | CstKind::UseTreeSuper
            | CstKind::UseTreeRoot
            | CstKind::StructFieldDecl
            | CstKind::EnumVariantDecl
            | CstKind::TraitMember
            | CstKind::ImplMember
            | CstKind::MatchArm
            | CstKind::StructField => CstCategory::Other,

            CstKind::Error => CstCategory::Error,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CstTokenKind {
    Token,
    TriviaWhitespace,
    TriviaLineComment,
    TriviaBlockComment,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CstToken {
    pub kind: CstTokenKind,
    pub text: String,
    pub span: Span,
}

impl CstToken {
    pub fn is_trivia(&self) -> bool {
        matches!(
            self.kind,
            CstTokenKind::TriviaWhitespace
                | CstTokenKind::TriviaLineComment
                | CstTokenKind::TriviaBlockComment
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CstElement {
    Node(Box<CstNode>),
    Token(CstToken),
}

impl CstElement {
    pub fn span(&self) -> Span {
        match self {
            CstElement::Node(node) => node.span,
            CstElement::Token(token) => token.span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CstNode {
    pub kind: CstKind,
    pub children: Vec<CstElement>,
    pub span: Span,
}

impl CstNode {
    pub fn new(kind: CstKind, children: Vec<CstElement>, span: Span) -> Self {
        Self {
            kind,
            children,
            span,
        }
    }
}

pub fn span_for_children(children: &[CstElement]) -> Option<Span> {
    let mut iter = children.iter();
    let first = iter.next()?;
    let mut span = first.span();
    for child in iter {
        let s = child.span();
        span = Span::new(span.file, span.lo.min(s.lo), span.hi.max(s.hi));
    }
    Some(span)
}

pub fn collect_tokens<'a>(node: &'a CstNode, out: &mut Vec<&'a CstToken>) {
    for child in &node.children {
        match child {
            CstElement::Node(inner) => collect_tokens(inner, out),
            CstElement::Token(tok) => out.push(tok),
        }
    }
}
