use super::*;

impl InterpretationOrchestrator {
    pub fn interpret_items(&self, node: &ItemChunk, ctx: &SharedScopedContext) -> Result<Value> {
        let result: Vec<_> = node
            .iter()
            .map(|x| self.interpret_item(x, ctx))
            .try_collect()?;
        Ok(result.into_iter().next().unwrap_or(Value::unit()))
    }

    pub fn interpret_import(&self, _node: &ItemImport, _ctx: &SharedScopedContext) -> Result<()> {
        Ok(())
    }

    pub fn interpret_impl(&self, node: &ItemImpl, ctx: &SharedScopedContext) -> Result<()> {
        // Get the type name that this impl block is for
        let type_name = match &node.self_ty {
            Expr::Locator(Locator::Ident(ident)) => ident.clone(),
            _ => {
                #[allow(unreachable_code)]
                return opt_bail!("Only simple type names are supported in impl blocks for now");
            }
        };

        // For each function in the impl block, register it as a method for the type
        for item in &node.items {
            if let Item::DefFunction(func_def) = item {
                // Store the method in context with a special naming scheme: TypeName::method_name
                let method_key = Ident::new(&format!(
                    "{}::{}",
                    type_name.as_str(),
                    func_def.name.as_str()
                ));
                let func_value = func_def._to_value();
                ctx.insert_value(method_key, Value::Function(func_value));
            }
        }

        Ok(())
    }

    pub fn interpret_block(&self, node: &ExprBlock, ctx: &SharedScopedContext) -> Result<Value> {
        let ctx = ctx.child(Ident::new("__block__"), Visibility::Private, true);

        // FIRST PASS: Process all items (const declarations, structs, functions)
        // Items can reference each other and need to be processed before statements
        for stmt in node.first_stmts().iter() {
            if let BlockStmt::Item(item) = stmt {
                self.interpret_item(item, &ctx)?;
            }
        }

        // SECOND PASS: Process all non-item statements (expressions, let statements, etc.)
        for stmt in node.first_stmts().iter() {
            if !matches!(stmt, BlockStmt::Item(_)) {
                self.interpret_stmt(&stmt, &ctx)?;
            }
        }

        // Process final expression if any
        if let Some(expr) = node.last_expr() {
            self.interpret_expr(&expr, &ctx)
        } else {
            Ok(Value::unit())
        }
    }

    pub fn interpret_def_function(
        &self,
        def: &ItemDefFunction,
        ctx: &SharedScopedContext,
    ) -> Result<()> {
        let name = &def.name;
        ctx.insert_value_with_ctx(name.clone(), Value::Function(def._to_value()));
        Ok(())
    }

    pub fn interpret_def_struct(
        &self,
        def: &ItemDefStruct,
        ctx: &SharedScopedContext,
    ) -> Result<()> {
        ctx.insert_value_with_ctx(def.name.clone(), Ty::Struct(def.value.clone()).into());
        Ok(())
    }

    pub fn interpret_def_enum(&self, def: &ItemDefEnum, ctx: &SharedScopedContext) -> Result<()> {
        ctx.insert_value_with_ctx(def.name.clone(), Ty::Enum(def.value.clone()).into());
        Ok(())
    }

    pub fn interpret_def_type(&self, def: &ItemDefType, ctx: &SharedScopedContext) -> Result<()> {
        ctx.insert_value_with_ctx(def.name.clone(), Value::Type(def.value.clone()));
        Ok(())
    }

    pub fn interpret_def_const(&self, def: &ItemDefConst, ctx: &SharedScopedContext) -> Result<()> {
        let value = self.interpret_expr(&def.value, ctx)?;
        tracing::debug!("Storing const {}: {:?}", def.name.name, value);
        ctx.insert_value_with_ctx(def.name.clone(), value);
        Ok(())
    }

    pub fn interpret_item(&self, node: &Item, ctx: &SharedScopedContext) -> Result<Value> {
        debug!("Interpreting {}", self.serializer.serialize_item(&node)?);
        match node {
            Item::Module(n) => self.interpret_items(&n.items, ctx),
            Item::DefFunction(n) => self.interpret_def_function(n, ctx).map(|_| Value::unit()),
            Item::DefStruct(n) => self.interpret_def_struct(n, ctx).map(|_| Value::unit()),
            Item::DefEnum(n) => self.interpret_def_enum(n, ctx).map(|_| Value::unit()),
            Item::DefType(n) => self.interpret_def_type(n, ctx).map(|_| Value::unit()),
            Item::DefConst(n) => self.interpret_def_const(n, ctx).map(|_| Value::unit()),
            Item::Import(n) => self.interpret_import(n, ctx).map(|_| Value::unit()),
            Item::Impl(n) => self.interpret_impl(n, ctx).map(|_| Value::unit()),

            Item::Any(n) => Ok(Value::Any(n.clone())),
            _ => opt_bail!(format!("Failed to interpret {:?}", node)),
        }
    }

    pub fn interpret_let(&self, node: &StmtLet, ctx: &SharedScopedContext) -> Result<Value> {
        if let Some(init) = &node.init {
            let value = self.interpret_expr(&init, ctx)?;
            ctx.insert_value(
                node.pat
                    .as_ident()
                    .ok_or_else(|| optimization_error("Only supports ident"))?
                    .as_str(),
                value.clone(),
            );
            Ok(value)
        } else {
            ctx.insert_value(
                node.pat
                    .as_ident()
                    .ok_or_else(|| optimization_error("Only supports ident"))?
                    .as_str(),
                Value::undefined(),
            );
            Ok(Value::unit())
        }
    }

    pub fn interpret_stmt(
        &self,
        node: &BlockStmt,
        ctx: &SharedScopedContext,
    ) -> Result<Option<Value>> {
        debug!("Interpreting {}", self.serializer.serialize_stmt(&node)?);
        // eprintln!("DEBUG: interpret_stmt called with: {:?}", node);
        match node {
            BlockStmt::Let(n) => self.interpret_let(n, ctx).map(|_| None),
            BlockStmt::Expr(n) => {
                self.interpret_expr(&n.expr, ctx).map(
                    |x| {
                        if n.has_value() {
                            Some(x)
                        } else {
                            None
                        }
                    },
                )
            }
            BlockStmt::Item(item) => self.interpret_item(item, ctx).map(|_| None),
            BlockStmt::Any(any_box) => {
                // Handle macro statements
                if any_box.downcast_ref::<fp_rust::RawStmtMacro>().is_some() {
                    // Macros like println! are now converted to function calls at parse time
                    // For any remaining macros, just return unit
                    Ok(None)
                } else {
                    opt_bail!(format!("Unsupported Any statement type: {:?}", any_box))
                }
            }
            BlockStmt::Noop => Ok(None),
        }
    }

    pub fn interpret_tree(&self, node: &Node, ctx: &SharedScopedContext) -> Result<Value> {
        match node {
            Node::Item(item) => self.interpret_item(item, ctx),
            Node::Expr(expr) => self.interpret_expr(expr, ctx),
            Node::File(file) => self.interpret_items(&file.items, ctx),
        }
    }
}
