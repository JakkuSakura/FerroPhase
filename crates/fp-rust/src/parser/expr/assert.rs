use super::*;

impl<'a> ExprParser<'a> {
    pub(super) fn parse_assert_macro(&self, mac: &syn::Macro, kind: AssertKind) -> Result<Expr> {
        let args = parse_expr_arguments(mac.tokens.clone())?;
        let base = match kind {
            AssertKind::Assert => self.lower_assert_condition(args, "assertion failed")?,
            AssertKind::DebugAssert => {
                self.lower_assert_condition(args, "debug assertion failed")?
            }
            AssertKind::AssertEq => {
                self.lower_assert_eq(args, true, "assertion failed: left != right")?
            }
            AssertKind::DebugAssertEq => {
                self.lower_assert_eq(args, true, "debug assertion failed: left != right")?
            }
            AssertKind::AssertNe => {
                self.lower_assert_eq(args, false, "assertion failed: left == right")?
            }
            AssertKind::DebugAssertNe => {
                self.lower_assert_eq(args, false, "debug assertion failed: left == right")?
            }
        };

        if kind.is_debug() {
            let cond = self.debug_assertions_intrinsic();
            let if_expr = ExprIf {
                cond: cond.into(),
                then: base.into(),
                elze: None,
            };
            Ok(Expr::from(ExprKind::If(if_expr)))
        } else {
            Ok(base)
        }
    }

    fn lower_assert_condition(&self, args: Vec<syn::Expr>, default_message: &str) -> Result<Expr> {
        if args.is_empty() {
            return self.err("assert! requires a condition", Expr::unit());
        }

        let mut iter = args.into_iter();
        let condition_expr = match iter.next() {
            Some(e) => e,
            None => return self.err("assert! requires a condition", Expr::unit()),
        };
        let message_args: Vec<_> = iter.collect();

        let condition = self.parse_expr(condition_expr)?;
        let fail_stmts = self.make_failure_stmts(message_args, Vec::new(), default_message)?;

        let fail_cond = Expr::from(ExprKind::UnOp(ExprUnOp {
            op: UnOpKind::Not,
            val: condition.into(),
        }));

        let if_expr = ExprIf {
            cond: fail_cond.into(),
            then: Expr::block(ExprBlock::new_stmts(fail_stmts)).into(),
            elze: None,
        };

        let stmts = vec![BlockStmt::Expr(
            BlockStmtExpr::new(Expr::from(ExprKind::If(if_expr))).with_semicolon(true),
        )];

        Ok(Expr::block(ExprBlock::new_stmts(stmts)))
    }

    fn lower_assert_eq(
        &self,
        args: Vec<syn::Expr>,
        expect_equal: bool,
        default_message: &str,
    ) -> Result<Expr> {
        if args.len() < 2 {
            return self.err("assert_eq!/assert_ne! require two operands", Expr::unit());
        }

        let idx = ASSERT_COUNTER.fetch_add(1, Ordering::Relaxed);
        let left_ident = Ident::new(format!("__fp_assert_left_{idx}"));
        let right_ident = Ident::new(format!("__fp_assert_right_{idx}"));
        let left_ident_name = left_ident.as_str().to_string();
        let right_ident_name = right_ident.as_str().to_string();

        let left_expr = self.parse_expr(args[0].clone())?;
        let right_expr = self.parse_expr(args[1].clone())?;

        let mut stmts = Vec::new();
        stmts.push(BlockStmt::Let(StmtLet::new_simple(
            left_ident.clone(),
            left_expr,
        )));
        stmts.push(BlockStmt::Let(StmtLet::new_simple(
            right_ident.clone(),
            right_expr,
        )));

        let lhs_value = Expr::ident(left_ident.clone());
        let rhs_value = Expr::ident(right_ident.clone());
        let cmp_kind = if expect_equal {
            BinOpKind::Ne
        } else {
            BinOpKind::Eq
        };

        let message_args = args.into_iter().skip(2).collect::<Vec<_>>();

        let format_lit = if expect_equal {
            LitStr::new(
                "assertion failed: left != right (left: {:#?}, right: {:#?})",
                Span::call_site(),
            )
        } else {
            LitStr::new(
                "assertion failed: left == right (left: {:#?}, right: {:#?})",
                Span::call_site(),
            )
        };
        let left_syn_ident = syn::Ident::new(&left_ident_name, Span::call_site());
        let right_syn_ident = syn::Ident::new(&right_ident_name, Span::call_site());
        let default_print_args = vec![
            syn::parse_quote!(#format_lit),
            syn::parse_quote!(#left_syn_ident),
            syn::parse_quote!(#right_syn_ident),
        ];
        let fail_block_stmts =
            self.make_failure_stmts(message_args, default_print_args, default_message)?;

        let comparison = Expr::from(ExprKind::BinOp(ExprBinOp {
            kind: cmp_kind,
            lhs: lhs_value.into(),
            rhs: rhs_value.into(),
        }));

        let if_expr = ExprIf {
            cond: comparison.into(),
            then: Expr::block(ExprBlock::new_stmts(fail_block_stmts)).into(),
            elze: None,
        };

        stmts.push(BlockStmt::Expr(
            BlockStmtExpr::new(Expr::from(ExprKind::If(if_expr))).with_semicolon(true),
        ));

        Ok(Expr::block(ExprBlock::new_stmts(stmts)))
    }

    #[allow(dead_code)]
    fn make_failure_stmts_impl(
        &self,
        message_args: Vec<syn::Expr>,
        mut default_print_args: Vec<syn::Expr>,
        default_message: &str,
    ) -> Result<Vec<BlockStmt>> {
        if default_print_args.is_empty() {
            let lit = LitStr::new(default_message, Span::call_site());
            default_print_args.push(syn::parse_quote!(#lit));
        }

        let mut stmts = Vec::new();
        let use_user_format = matches!(
            message_args.first(),
            Some(syn::Expr::Lit(syn::ExprLit {
                lit: syn::Lit::Str(_),
                ..
            }))
        );

        let print_args = if message_args.is_empty() {
            default_print_args
        } else if use_user_format {
            message_args
        } else {
            for expr in message_args {
                let parsed = self.parse_expr(expr)?;
                stmts.push(BlockStmt::Expr(
                    BlockStmtExpr::new(parsed).with_semicolon(true),
                ));
            }
            default_print_args
        };

        let println_syn: syn::Expr = syn::parse_quote!(println!(#(#print_args),*));
        let println_expr = self.parse_expr(println_syn)?;
        stmts.push(BlockStmt::Expr(
            BlockStmtExpr::new(println_expr).with_semicolon(true),
        ));
        stmts.push(BlockStmt::Expr(
            BlockStmtExpr::new(super::make_abort_expr()).with_semicolon(true),
        ));
        Ok(stmts)
    }
}
