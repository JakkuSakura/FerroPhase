//! Recursive-descent parser for the basic Lean 4 subset, building
//! `fp_core::ast` nodes directly (no intermediate IR — the grammar is
//! small enough that a two-stage lowering would only add indirection).

use fp_core::ast::{
    self, Expr, ExprBinOp, ExprBlock, ExprIf, ExprInvoke, ExprInvokeTarget, ExprKind, File,
    FunctionParam, FunctionSignature, Ident, Item, ItemDefFunction, ItemKind, Name, StmtLet, Ty,
    TypeInt, TypePrimitive, TypeRefinement, Value,
};
use fp_core::ops::{BinOpKind, UnOpKind};

use crate::error::LeanParseError;
use crate::lexer::{Token, TokenKind};

pub struct Parser<'a> {
    tokens: &'a [Token],
    pos: usize,
}

type PResult<T> = Result<T, LeanParseError>;

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self { tokens, pos: 0 }
    }

    fn peek(&self) -> &TokenKind {
        &self.tokens[self.pos].kind
    }

    fn advance(&mut self) -> TokenKind {
        let kind = self.tokens[self.pos].kind.clone();
        if self.pos + 1 < self.tokens.len() {
            self.pos += 1;
        }
        kind
    }

    fn expect(&mut self, expected: &TokenKind) -> PResult<()> {
        if self.peek() == expected {
            self.advance();
            Ok(())
        } else {
            Err(LeanParseError::Expected {
                expected: format!("{expected:?}"),
                found: format!("{:?}", self.peek()),
            })
        }
    }

    fn eat_ident(&mut self) -> PResult<String> {
        match self.advance() {
            TokenKind::Ident(name) => Ok(name),
            other => Err(LeanParseError::Expected {
                expected: "identifier".into(),
                found: format!("{other:?}"),
            }),
        }
    }

    pub fn parse_file(&mut self) -> PResult<File> {
        let mut items = Vec::new();
        while *self.peek() != TokenKind::Eof {
            items.push(self.parse_item()?);
        }
        Ok(File {
            path: std::path::PathBuf::from("<lean>"),
            attrs: Vec::new(),
            items,
        })
    }

    fn parse_item(&mut self) -> PResult<Item> {
        self.expect(&TokenKind::Def)?;
        let name = Ident::new(self.eat_ident()?);

        let mut params = Vec::new();
        while *self.peek() == TokenKind::LParen {
            self.advance();
            loop {
                let pname = Ident::new(self.eat_ident()?);
                self.expect(&TokenKind::Colon)?;
                let ty = self.parse_type()?;
                params.push(FunctionParam::new(pname, ty));
                if *self.peek() == TokenKind::Comma {
                    self.advance();
                    continue;
                }
                break;
            }
            self.expect(&TokenKind::RParen)?;
        }

        let ret_ty = if *self.peek() == TokenKind::Colon {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };

        self.expect(&TokenKind::ColonEq)?;
        let body = self.parse_block()?;

        let mut sig = FunctionSignature::unit();
        sig.name = Some(name.clone());
        sig.params = params;
        sig.ret_ty = ret_ty;

        let item = ItemDefFunction {
            ty_annotation: None,
            attrs: Vec::new(),
            name,
            collected_items: Vec::new(),
            ty: None,
            sig,
            body,
            is_async: false,
            visibility: ast::Visibility::Public,
        };
        Ok(Item::new(ItemKind::DefFunction(item)))
    }

    /// `Type := "Nat" | "Int" | "Bool" | "String" | Refinement`.
    fn parse_type(&mut self) -> PResult<Ty> {
        if *self.peek() == TokenKind::LBrace {
            return self.parse_refinement_type();
        }
        let name = self.eat_ident()?;
        match name.as_str() {
            "Nat" => Ok(Ty::Primitive(TypePrimitive::Int(TypeInt::U64))),
            "Int" => Ok(Ty::Primitive(TypePrimitive::Int(TypeInt::I64))),
            "Bool" => Ok(Ty::Primitive(TypePrimitive::Bool)),
            "String" => Ok(Ty::Primitive(TypePrimitive::String)),
            other => Err(LeanParseError::Expected {
                expected: "Nat, Int, Bool, String, or a refinement type".into(),
                found: other.to_string(),
            }),
        }
    }

    /// `{binder : Type // predicate}`.
    fn parse_refinement_type(&mut self) -> PResult<Ty> {
        self.expect(&TokenKind::LBrace)?;
        let binder = Ident::new(self.eat_ident()?);
        self.expect(&TokenKind::Colon)?;
        let base = self.parse_type()?;
        self.expect(&TokenKind::SlashSlash)?;
        let predicate = self.parse_expr()?;
        self.expect(&TokenKind::RBrace)?;
        Ok(Ty::Refinement(Box::new(TypeRefinement::new(
            base, binder, predicate,
        ))))
    }

    /// `(let ...)* tail_expr`, folded into one `ExprBlock`. The `;` after
    /// each `let`'s value is mandatory (not optional, as it is in real
    /// Lean) — this lexer has no newline/indentation sensitivity, so `;`
    /// is the only marker available for where a `let`'s value expression
    /// ends and the next statement begins; without it, juxtaposition
    /// application (`AppExpr := Atom Atom*`) can't tell "the next atom is
    /// another argument" from "the next atom starts a new statement".
    fn parse_block(&mut self) -> PResult<ExprBlock> {
        let mut stmts = Vec::new();
        loop {
            if *self.peek() != TokenKind::Let {
                break;
            }
            self.advance();
            let name = Ident::new(self.eat_ident()?);
            let ty = if *self.peek() == TokenKind::Colon {
                self.advance();
                Some(self.parse_type()?)
            } else {
                None
            };
            self.expect(&TokenKind::ColonEq)?;
            let value = self.parse_expr()?;
            self.expect(&TokenKind::Semi)?;
            let let_stmt = match ty {
                Some(ty) => StmtLet::new_typed(name, ty, value),
                None => StmtLet::new_simple(name, value),
            };
            stmts.push(ast::BlockStmt::Let(let_stmt));
        }
        let tail = self.parse_expr()?;
        let mut block = ExprBlock::new_stmts(stmts);
        block.push_expr(tail);
        Ok(block)
    }

    /// `Expr := IfExpr | CmpExpr`.
    fn parse_expr(&mut self) -> PResult<Expr> {
        if *self.peek() == TokenKind::If {
            return self.parse_if();
        }
        self.parse_cmp()
    }

    fn parse_if(&mut self) -> PResult<Expr> {
        self.expect(&TokenKind::If)?;
        let cond = self.parse_cmp()?;
        self.expect(&TokenKind::Then)?;
        let then = self.parse_cmp()?;
        self.expect(&TokenKind::Else)?;
        let elze = self.parse_cmp()?;
        Ok(Expr::new(ExprKind::If(ExprIf {
            span: Default::default(),
            cond: Box::new(cond),
            then: Box::new(then),
            elze: Some(Box::new(elze)),
        })))
    }

    /// `CmpExpr := AddExpr (CmpOp AddExpr)?` — non-chaining, `&&`/`||` bind
    /// looser than comparisons so `x >= 0 && x <= 100` parses as expected.
    fn parse_cmp(&mut self) -> PResult<Expr> {
        let lhs = self.parse_and_or()?;
        Ok(lhs)
    }

    fn parse_and_or(&mut self) -> PResult<Expr> {
        let mut lhs = self.parse_comparison()?;
        loop {
            let kind = match self.peek() {
                TokenKind::AndAnd => BinOpKind::And,
                TokenKind::OrOr => BinOpKind::Or,
                _ => break,
            };
            self.advance();
            let rhs = self.parse_comparison()?;
            lhs = binop(kind, lhs, rhs);
        }
        Ok(lhs)
    }

    fn parse_comparison(&mut self) -> PResult<Expr> {
        let lhs = self.parse_add()?;
        let kind = match self.peek() {
            TokenKind::Lt => BinOpKind::Lt,
            TokenKind::Le => BinOpKind::Le,
            TokenKind::Gt => BinOpKind::Gt,
            TokenKind::Ge => BinOpKind::Ge,
            TokenKind::EqEq => BinOpKind::Eq,
            TokenKind::NotEq => BinOpKind::Ne,
            _ => return Ok(lhs),
        };
        self.advance();
        let rhs = self.parse_add()?;
        Ok(binop(kind, lhs, rhs))
    }

    fn parse_add(&mut self) -> PResult<Expr> {
        let mut lhs = self.parse_mul()?;
        loop {
            let kind = match self.peek() {
                TokenKind::Plus => BinOpKind::Add,
                TokenKind::Minus => BinOpKind::Sub,
                _ => break,
            };
            self.advance();
            let rhs = self.parse_mul()?;
            lhs = binop(kind, lhs, rhs);
        }
        Ok(lhs)
    }

    fn parse_mul(&mut self) -> PResult<Expr> {
        let mut lhs = self.parse_unary()?;
        loop {
            let kind = match self.peek() {
                TokenKind::Star => BinOpKind::Mul,
                TokenKind::Slash => BinOpKind::Div,
                _ => break,
            };
            self.advance();
            let rhs = self.parse_unary()?;
            lhs = binop(kind, lhs, rhs);
        }
        Ok(lhs)
    }

    fn parse_unary(&mut self) -> PResult<Expr> {
        if *self.peek() == TokenKind::Minus {
            self.advance();
            let inner = self.parse_unary()?;
            return Ok(binop(BinOpKind::Sub, Expr::value(Value::int(0)), inner));
        }
        if *self.peek() == TokenKind::Bang {
            self.advance();
            let inner = self.parse_unary()?;
            return Ok(Expr::new(ExprKind::UnOp(ast::ExprUnOp {
                span: Default::default(),
                op: UnOpKind::Not,
                val: Box::new(inner),
            })));
        }
        self.parse_application()
    }

    /// `AppExpr := Atom Atom*` (Lean-style juxtaposition call), with a
    /// direct `f(a, b)` comma-call form also collapsing to the same
    /// `ExprInvoke` shape (see the module-level design notes in the plan).
    fn parse_application(&mut self) -> PResult<Expr> {
        let head = self.parse_atom()?;
        let mut args = Vec::new();
        loop {
            match self.peek() {
                TokenKind::Int(_)
                | TokenKind::Str(_)
                | TokenKind::True
                | TokenKind::False
                | TokenKind::Ident(_)
                | TokenKind::LParen => {
                    args.push(self.parse_atom()?);
                }
                _ => break,
            }
        }
        if args.is_empty() {
            return Ok(head);
        }
        let target = invoke_target(head)?;
        Ok(Expr::new(ExprKind::Invoke(ExprInvoke {
            span: Default::default(),
            target,
            args,
            kwargs: Vec::new(),
        })))
    }

    fn parse_atom(&mut self) -> PResult<Expr> {
        match self.advance() {
            TokenKind::Int(v) => Ok(Expr::value(Value::int(v))),
            TokenKind::Str(s) => Ok(Expr::value(Value::string(s))),
            TokenKind::True => Ok(Expr::value(Value::bool(true))),
            TokenKind::False => Ok(Expr::value(Value::bool(false))),
            TokenKind::Ident(name) => {
                // `f(a, b)` comma-call form: only treated as a call when a
                // top-level comma is present; a bare `(a)` falls through to
                // ordinary juxtaposition (`f (a)` == `f a`), matching real
                // Lean's application syntax.
                if *self.peek() == TokenKind::LParen {
                    let save = self.pos;
                    self.advance();
                    let mut args = Vec::new();
                    let mut saw_comma = false;
                    if *self.peek() != TokenKind::RParen {
                        args.push(self.parse_expr()?);
                        while *self.peek() == TokenKind::Comma {
                            saw_comma = true;
                            self.advance();
                            args.push(self.parse_expr()?);
                        }
                    }
                    if saw_comma {
                        self.expect(&TokenKind::RParen)?;
                        return Ok(Expr::new(ExprKind::Invoke(ExprInvoke {
                            span: Default::default(),
                            target: ExprInvokeTarget::Function(Name::ident(name)),
                            args,
                            kwargs: Vec::new(),
                        })));
                    }
                    // No comma seen: rewind and let application-juxtaposition
                    // parse the parenthesized atom normally.
                    self.pos = save;
                }
                Ok(Expr::ident(Ident::new(name)))
            }
            TokenKind::LParen => {
                let inner = self.parse_expr()?;
                self.expect(&TokenKind::RParen)?;
                Ok(inner)
            }
            other => Err(LeanParseError::Expected {
                expected: "an expression".into(),
                found: format!("{other:?}"),
            }),
        }
    }
}

fn binop(kind: BinOpKind, lhs: Expr, rhs: Expr) -> Expr {
    Expr::new(ExprKind::BinOp(ExprBinOp {
        span: Default::default(),
        kind,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }))
}

fn invoke_target(head: Expr) -> PResult<ExprInvokeTarget> {
    match head.kind() {
        ExprKind::Name(_) => Ok(ExprInvokeTarget::expr(head)),
        _ => Ok(ExprInvokeTarget::Expr(Box::new(head))),
    }
}

pub fn parse_file(source: &str) -> PResult<File> {
    let tokens = crate::lexer::lex(source)?;
    Parser::new(&tokens).parse_file()
}

#[cfg(test)]
mod tests {
    use super::*;
    use fp_core::ast::{BlockStmt, ExprKind};

    fn parse_def(src: &str) -> ItemDefFunction {
        let file = parse_file(src).expect("parse");
        assert_eq!(file.items.len(), 1);
        match file.items[0].kind().clone() {
            ItemKind::DefFunction(f) => f,
            other => panic!("expected DefFunction, got {other:?}"),
        }
    }

    #[test]
    fn parses_simple_def() {
        let f = parse_def("def id (x : Nat) : Nat := x");
        assert_eq!(f.sig.params.len(), 1);
        assert_eq!(f.body.stmts.len(), 1);
    }

    #[test]
    fn parses_let_chain() {
        let f = parse_def("def f (x : Nat) : Nat := let y := x + 1; let z := y * 2; z");
        assert_eq!(f.body.stmts.len(), 3);
        assert!(matches!(f.body.stmts[0], BlockStmt::Let(_)));
        assert!(matches!(f.body.stmts[1], BlockStmt::Let(_)));
    }

    #[test]
    fn parses_if_then_else() {
        let f = parse_def("def max (a : Nat) (b : Nat) : Nat := if a > b then a else b");
        let BlockStmt::Expr(tail) = &f.body.stmts[0] else {
            panic!("expected tail expr");
        };
        assert!(matches!(tail.expr.kind(), ExprKind::If(_)));
    }

    #[test]
    fn precedence_mul_before_add() {
        let f = parse_def("def f : Int := 1 + 2 * 3");
        let BlockStmt::Expr(tail) = &f.body.stmts[0] else {
            panic!()
        };
        let ExprKind::BinOp(op) = tail.expr.kind() else {
            panic!("expected binop")
        };
        assert_eq!(op.kind, BinOpKind::Add);
        assert!(matches!(op.rhs.kind(), ExprKind::BinOp(_)));
    }

    #[test]
    fn juxtaposition_and_comma_call_agree() {
        let f1 = parse_def("def f (x : Nat) (y : Nat) : Nat := add x y");
        let f2 = parse_def("def f (x : Nat) (y : Nat) : Nat := add(x, y)");
        for f in [f1, f2] {
            let BlockStmt::Expr(tail) = &f.body.stmts[0] else {
                panic!()
            };
            let ExprKind::Invoke(invoke) = tail.expr.kind() else {
                panic!("expected invoke")
            };
            assert_eq!(invoke.args.len(), 2);
        }
    }

    #[test]
    fn parses_refinement_type() {
        let f = parse_def("def f (x : {n : Nat // n >= 0}) : Nat := x");
        let ty = &f.sig.params[0].ty;
        assert!(matches!(ty, Ty::Refinement(_)));
    }

    #[test]
    fn parses_literals() {
        let f = parse_def(r#"def f : String := "hi""#);
        let BlockStmt::Expr(tail) = &f.body.stmts[0] else {
            panic!()
        };
        assert!(matches!(tail.expr.kind(), ExprKind::Value(_)));
    }
}
