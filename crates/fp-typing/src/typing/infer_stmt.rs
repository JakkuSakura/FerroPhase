use crate::typing::unify::TypeTerm;
use crate::{typing_error, AstTypeInferencer, LoopContext, TypeVarId};
use fp_core::ast::*;
use fp_core::error::Result;
use fp_core::module::path::PathPrefix;
use std::collections::{HashMap, HashSet};

impl<'ctx> AstTypeInferencer<'ctx> {
    pub(crate) fn is_stmt_or_item_quote(&self, ty: &Ty) -> bool {
        match ty {
            Ty::Quote(quote) => matches!(
                quote.kind,
                QuoteFragmentKind::Stmt | QuoteFragmentKind::Item | QuoteFragmentKind::Expr
            ),
            Ty::Vec(vec) => self.is_stmt_or_item_quote(vec.ty.as_ref()),
            Ty::Array(array) => self.is_stmt_or_item_quote(array.elem.as_ref()),
            Ty::Slice(slice) => self.is_stmt_or_item_quote(slice.elem.as_ref()),
            _ => false,
        }
    }

    pub(crate) fn is_item_quote(&self, ty: &Ty) -> bool {
        match ty {
            Ty::Quote(quote) => matches!(quote.kind, QuoteFragmentKind::Item),
            Ty::Vec(vec) => self.is_item_quote(vec.ty.as_ref()),
            Ty::Array(array) => self.is_item_quote(array.elem.as_ref()),
            Ty::Slice(slice) => self.is_item_quote(slice.elem.as_ref()),
            _ => false,
        }
    }

    pub(crate) fn infer_block(&mut self, block: &mut ExprBlock) -> Result<TypeVarId> {
        self.enter_scope();
        let mut last = self.fresh_type_var();
        self.bind(last, TypeTerm::Unit);
        for stmt in &mut block.stmts {
            match stmt {
                BlockStmt::Item(item) => {
                    self.predeclare_item(item);
                    self.infer_item(item)?;
                    last = self.fresh_type_var();
                    self.bind(last, TypeTerm::Unit);
                }
                BlockStmt::Let(stmt_let) => {
                    let init_var = if let Some(init) = stmt_let.init.as_mut() {
                        if let PatternKind::Type(typed) = &stmt_let.pat.kind {
                            init.set_ty(typed.ty.clone());
                        }
                        self.infer_expr(init)?
                    } else {
                        let unit = self.fresh_type_var();
                        self.bind(unit, TypeTerm::Unit);
                        unit
                    };
                    let pattern_info = self.infer_pattern(&mut stmt_let.pat)?;
                    self.unify(pattern_info.var, init_var)?;
                    self.apply_pattern_generalization(&pattern_info)?;
                    last = self.fresh_type_var();
                    self.bind(last, TypeTerm::Unit);
                }
                BlockStmt::Defer(stmt_defer) => {
                    self.infer_expr(stmt_defer.expr.as_mut())?;
                    last = self.fresh_type_var();
                    self.bind(last, TypeTerm::Unit);
                }
                BlockStmt::Expr(expr_stmt) => {
                    // If this is a splice in statement position, enforce stmt token
                    if let ExprKind::Splice(splice) = expr_stmt.expr.kind_mut() {
                        let token_var = self.infer_expr(splice.token.as_mut())?;
                        let token_ty = self.resolve_to_ty(token_var)?;
                        if !self.is_stmt_or_item_quote(&token_ty) {
                            match token_ty {
                                Ty::Quote(quote) => {
                                    self.emit_error(format!(
                                        "splice in statement position requires stmt/item/expr token, found {:?}",
                                        quote.kind
                                    ));
                                }
                                _ => self.emit_error("splice expects a quote token expression"),
                            }
                        }
                        // Statements do not contribute a value
                        last = self.fresh_type_var();
                        self.bind(last, TypeTerm::Unit);
                        continue;
                    }
                    let expr_var = self.infer_expr(expr_stmt.expr.as_mut())?;
                    if expr_stmt.has_value() {
                        last = expr_var;
                    } else {
                        last = self.fresh_type_var();
                        self.bind(last, TypeTerm::Unit);
                    }
                }
                BlockStmt::Noop => {
                    last = self.fresh_type_var();
                    self.bind(last, TypeTerm::Unit);
                }
                BlockStmt::Any(_) => {
                    let unit = self.unit_type_var();
                    last = unit;
                }
            }
        }
        self.exit_scope();
        Ok(last)
    }

    pub(crate) fn infer_if(&mut self, if_expr: &mut ExprIf) -> Result<TypeVarId> {
        let cond = self.infer_expr(if_expr.cond.as_mut())?;
        self.ensure_bool(cond, "if condition")?;
        let then_ty = self.infer_expr(if_expr.then.as_mut())?;
        if let Some(elze) = if_expr.elze.as_mut() {
            let else_ty = self.infer_expr(elze)?;
            self.unify(then_ty, else_ty)?;
        }
        Ok(then_ty)
    }

    pub(crate) fn infer_loop(&mut self, expr_loop: &mut ExprLoop) -> Result<TypeVarId> {
        let loop_result_var = self.fresh_type_var();
        self.loop_stack.push(LoopContext::new(loop_result_var));

        let body_var = match self.infer_expr(expr_loop.body.as_mut()) {
            Ok(var) => var,
            Err(err) => {
                self.loop_stack.pop();
                return Err(err);
            }
        };

        let unit_var = self.unit_type_var();
        if let Err(err) = self.unify(body_var, unit_var) {
            self.loop_stack.pop();
            return Err(err);
        }

        let Some(context) = self.loop_stack.pop() else {
            let message = "loop stack underflow when finishing loop inference".to_string();
            self.emit_error(message.clone());
            return Err(typing_error(message));
        };

        if !context.saw_break {
            self.bind(loop_result_var, TypeTerm::Nothing);
        }

        Ok(loop_result_var)
    }

    pub(crate) fn infer_while(&mut self, expr_while: &mut ExprWhile) -> Result<TypeVarId> {
        let cond_var = self.infer_expr(expr_while.cond.as_mut())?;
        self.ensure_bool(cond_var, "while condition")?;
        let loop_unit_var = self.unit_type_var();
        self.loop_stack.push(LoopContext::new(loop_unit_var));

        let body_var = match self.infer_expr(expr_while.body.as_mut()) {
            Ok(var) => var,
            Err(err) => {
                self.loop_stack.pop();
                return Err(err);
            }
        };

        if let Err(err) = self.unify(body_var, loop_unit_var) {
            self.loop_stack.pop();
            return Err(err);
        }

        let Some(_context) = self.loop_stack.pop() else {
            let message = "loop stack underflow when finishing while inference".to_string();
            self.emit_error(message.clone());
            return Err(typing_error(message));
        };

        Ok(loop_unit_var)
    }

    pub(crate) fn infer_match(&mut self, match_expr: &mut ExprMatch) -> Result<TypeVarId> {
        let mut result_var: Option<TypeVarId> = None;

        if let Some(scrutinee) = match_expr.scrutinee.as_mut() {
            let scrutinee_var = self.infer_expr(scrutinee.as_mut())?;
            let scrutinee_ty_initial = self.resolve_to_ty(scrutinee_var).ok();
            let scrutinee_enum_hint = self
                .scrutinee_enum_from_explicit_generic_invoke(scrutinee.as_ref());
            for case in &mut match_expr.cases {
                self.enter_scope();

                if let Some(pat) = case.pat.as_mut() {
                    let mut enum_ty = scrutinee_enum_hint.clone().or_else(|| {
                        match scrutinee_ty_initial.as_ref() {
                            Some(Ty::Enum(enum_ty)) => Some(enum_ty.clone()),
                            _ => None,
                        }
                    });
                    if enum_ty.is_none() {
                        enum_ty = self.enum_ty_from_pattern_variant(pat.as_ref());
                    }
                    let enum_ty_hint = enum_ty.clone();
                    if let Some(enum_ty) = enum_ty.as_ref() {
                        if scrutinee_enum_hint.is_some()
                            || !matches!(scrutinee_ty_initial, Some(Ty::Enum(_)))
                        {
                            let enum_var = self.fresh_type_var();
                            self.bind(enum_var, TypeTerm::Enum(enum_ty.clone()));
                            self.unify(enum_var, scrutinee_var)?;
                        }
                        qualify_enum_variant_pattern(pat, enum_ty);
                    }
                    let pat_info = self.infer_pattern(pat.as_mut())?;
                    self.unify(pat_info.var, scrutinee_var)?;
                    let resolved_enum = match self.resolve_to_ty(scrutinee_var) {
                        Ok(Ty::Enum(enum_ty)) => Some(enum_ty),
                        _ => enum_ty_hint,
                    };
                    if let Some(enum_ty) = resolved_enum.as_ref() {
                        self.bind_enum_variant_pattern(pat.as_ref(), enum_ty)?;
                    }
                    self.apply_pattern_generalization(&pat_info)?;
                }

                if let Some(guard) = case.guard.as_mut() {
                    let guard_var = self.infer_expr(guard.as_mut())?;
                    self.ensure_bool(guard_var, "match guard")?;
                }

                let body_var = self.infer_expr(case.body.as_mut())?;
                if let Some(existing) = result_var {
                    self.unify(existing, body_var)?;
                } else {
                    result_var = Some(body_var);
                }
                self.exit_scope();
            }
        } else {
            // Legacy lowering: cases are boolean conditions.
            for case in &mut match_expr.cases {
                let cond_var = self.infer_expr(case.cond.as_mut())?;
                self.ensure_bool(cond_var, "match case condition")?;

                let body_var = self.infer_expr(case.body.as_mut())?;
                if let Some(existing) = result_var {
                    self.unify(existing, body_var)?;
                } else {
                    result_var = Some(body_var);
                }
            }
        }

        match result_var {
            Some(var) => Ok(var),
            None => {
                self.emit_error("match expression requires at least one case");
                Ok(self.error_type_var())
            }
        }
    }

    fn scrutinee_enum_from_explicit_generic_invoke(
        &mut self,
        scrutinee: &Expr,
    ) -> Option<TypeEnum> {
        let ExprKind::Invoke(invoke) = scrutinee.kind() else {
            return None;
        };
        let ExprInvokeTarget::Function(locator) = &invoke.target else {
            return None;
        };
        let generic_args = Self::match_locator_generic_args(locator)?;
        if generic_args.is_empty() {
            return None;
        }
        let sig = self.lookup_function_signature(locator)?;
        if sig.generics_params.len() != generic_args.len() {
            return None;
        }
        let mut generic_mapping: HashMap<String, Ty> = HashMap::new();
        for (param, arg_ty) in sig.generics_params.iter().zip(generic_args.iter()) {
            if Self::is_inferred_generic_argument(arg_ty) {
                continue;
            }
            generic_mapping.insert(param.name.as_str().to_string(), arg_ty.clone());
        }
        let ret_ty = sig.ret_ty.as_ref()?;
        let substituted_ret_ty = if generic_mapping.is_empty() {
            ret_ty.clone()
        } else {
            self.substitute_generic_ty(ret_ty, &generic_mapping)
        };
        let ret_var = self.type_from_ast_ty(&substituted_ret_ty).ok()?;
        match self.resolve_to_ty(ret_var).ok()? {
            Ty::Enum(enum_ty) => Some(enum_ty),
            _ => None,
        }
    }

    fn is_inferred_generic_argument(arg_ty: &Ty) -> bool {
        // `_` generic placeholders are parsed as `Ty::Unknown`.
        // Keep `()` as a real explicit generic argument.
        matches!(arg_ty, Ty::Unknown(_))
    }

    fn match_locator_generic_args(locator: &Name) -> Option<&[Ty]> {
        let Name::ParameterPath(path) = locator else {
            return None;
        };
        let segment = path.segments.iter().rev().find(|seg| !seg.args.is_empty())?;
        Some(segment.args.as_slice())
    }

    fn bind_enum_variant_pattern(
        &mut self,
        pat: &Pattern,
        enum_ty: &TypeEnum,
    ) -> Result<()> {
        let needs_generics = !enum_ty.generics_params.is_empty();
        if needs_generics {
            self.enter_scope();
            for param in &enum_ty.generics_params {
                let var = self.register_generic_param(param.name.as_str());
                let bounds = Self::extract_trait_bounds(&param.bounds);
                if !bounds.is_empty() {
                    self.generic_trait_bounds.insert(var, bounds);
                }
            }
        }
        let result = (|| {
            match pat.kind() {
                PatternKind::TupleStruct(tuple_struct) => {
                    let variant_name = match &tuple_struct.name {
                        Name::Ident(ident) => ident.as_str(),
                        Name::Path(path) => path
                            .segments
                            .last()
                            .map(|seg| seg.as_str())
                            .unwrap_or(""),
                        _ => "",
                    };
                    if let Some(def_variant) = enum_ty
                        .variants
                        .iter()
                        .find(|variant| variant.name.as_str() == variant_name)
                    {
                        let expected_value = self
                            .resolve_enum_variant_expected_value(enum_ty, variant_name)?
                            .unwrap_or_else(|| def_variant.value.clone());
                        match &expected_value {
                            Ty::Tuple(tuple_ty) => {
                                for (pat, expected_ty) in tuple_struct
                                    .patterns
                                    .iter()
                                    .zip(tuple_ty.types.iter())
                                {
                                    self.bind_pattern_expected_type(pat, expected_ty)?;
                                }
                            }
                            _ if tuple_struct.patterns.len() == 1 => {
                                let pat = &tuple_struct.patterns[0];
                                self.bind_pattern_expected_type(pat, &expected_value)?;
                            }
                            _ => {}
                        }
                    }
                }
                PatternKind::Variant(variant) => {
                    let variant_name = match variant.name.kind() {
                        ExprKind::Name(Name::Ident(ident)) => ident.as_str(),
                        ExprKind::Name(Name::Path(path)) => path
                            .segments
                            .last()
                            .map(|seg| seg.as_str())
                            .unwrap_or(""),
                        _ => "",
                    };
                    if let Some(def_variant) = enum_ty
                        .variants
                        .iter()
                        .find(|variant| variant.name.as_str() == variant_name)
                    {
                        let expected_value = self
                            .resolve_enum_variant_expected_value(enum_ty, variant_name)?
                            .unwrap_or_else(|| def_variant.value.clone());
                        let Some(inner) = variant.pattern.as_ref() else {
                            return Ok(());
                        };
                        if let PatternKind::Structural(pat) = inner.kind() {
                            let fields = match &expected_value {
                                Ty::Structural(structural) => Some(&structural.fields),
                                Ty::Struct(struct_ty) => Some(&struct_ty.fields),
                                _ => None,
                            };
                            if let Some(fields) = fields {
                                for field in &pat.fields {
                                    let Some(expected_field) =
                                        fields.iter().find(|f| f.name == field.name)
                                    else {
                                        continue;
                                    };
                                    if let Some(rename) = field.rename.as_ref() {
                                        self.bind_pattern_expected_type(
                                            rename,
                                            &expected_field.value,
                                        )?;
                                    } else if let Some(var) =
                                        self.lookup_env_var(field.name.as_str())
                                    {
                                        let expected_var =
                                            self.type_from_ast_ty(&expected_field.value)?;
                                        self.unify(var, expected_var)?;
                                    }
                                }
                            }
                        }
                    }
                }
                _ => {}
            }
            Ok(())
        })();
        if needs_generics {
            self.exit_scope();
        }
        result
    }

    fn enum_ty_from_pattern_variant(&self, pat: &Pattern) -> Option<TypeEnum> {
        let variant_name = match pat.kind() {
            PatternKind::TupleStruct(tuple_struct) => match &tuple_struct.name {
                Name::Ident(ident) => Some(ident.as_str()),
                Name::Path(path)
                    if path.prefix == PathPrefix::Plain && path.segments.len() == 1 =>
                {
                    Some(path.segments[0].as_str())
                }
                _ => None,
            },
            PatternKind::Variant(variant) => match variant.name.kind() {
                ExprKind::Name(Name::Ident(ident)) => Some(ident.as_str()),
                ExprKind::Name(Name::Path(path))
                    if path.prefix == PathPrefix::Plain && path.segments.len() == 1 =>
                {
                    Some(path.segments[0].as_str())
                }
                _ => None,
            },
            _ => None,
        }?;
        let mut candidate: Option<TypeEnum> = None;
        for enum_def in self.enum_defs.values() {
            if enum_def
                .variants
                .iter()
                .any(|variant| variant.name.as_str() == variant_name)
            {
                if candidate.is_some() {
                    return None;
                }
                candidate = Some(enum_def.clone());
            }
        }
        candidate
    }

    pub(crate) fn resolve_enum_variant_expected_value(
        &mut self,
        enum_ty: &TypeEnum,
        variant_name: &str,
    ) -> Result<Option<Ty>> {
        let Some((_, enum_def)) =
            self.lookup_enum_def_by_name(enum_ty.name.as_str())
        else {
            let expected = enum_ty
                .variants
                .iter()
                .find(|variant| variant.name.as_str() == variant_name)
                .map(|variant| variant.value.clone());
            return Ok(expected);
        };
        let Some(def_variant) = enum_def
            .variants
            .iter()
            .find(|variant| variant.name.as_str() == variant_name)
        else {
            let expected = enum_ty
                .variants
                .iter()
                .find(|variant| variant.name.as_str() == variant_name)
                .map(|variant| variant.value.clone());
            return Ok(expected);
        };
        let Some(concrete_variant) = enum_ty
            .variants
            .iter()
            .find(|variant| variant.name.as_str() == variant_name)
        else {
            return Ok(Some(def_variant.value.clone()));
        };
        if enum_def.generics_params.is_empty() {
            return Ok(Some(concrete_variant.value.clone()));
        }
        let generic_names: HashSet<String> = enum_def
            .generics_params
            .iter()
            .map(|param| param.name.as_str().to_string())
            .collect();
        let mut mapping: HashMap<String, Ty> = HashMap::new();
        self.collect_enum_generic_mapping(
            &def_variant.value,
            &concrete_variant.value,
            &generic_names,
            &mut mapping,
        );
        if mapping.is_empty() {
            return Ok(Some(concrete_variant.value.clone()));
        }
        Ok(Some(self.substitute_generic_ty(&def_variant.value, &mapping)))
    }

    fn collect_enum_generic_mapping(
        &self,
        def_value: &Ty,
        concrete_value: &Ty,
        generic_names: &HashSet<String>,
        mapping: &mut HashMap<String, Ty>,
    ) {
        match def_value {
            Ty::Expr(expr) => {
                if let ExprKind::Name(locator) = expr.kind() {
                    if let Some(name) = self.generic_name_from_locator(locator) {
                        if generic_names.iter().any(|n| n == name) && !mapping.contains_key(name) {
                            mapping.insert(name.to_string(), concrete_value.clone());
                        }
                    }
                }
            }
            Ty::Reference(reference) => {
                if let Ty::Reference(concrete) = concrete_value {
                    self.collect_enum_generic_mapping(
                        reference.ty.as_ref(),
                        concrete.ty.as_ref(),
                        generic_names,
                        mapping,
                    );
                }
            }
            Ty::RawPtr(ptr) => {
                if let Ty::RawPtr(concrete) = concrete_value {
                    self.collect_enum_generic_mapping(
                        ptr.ty.as_ref(),
                        concrete.ty.as_ref(),
                        generic_names,
                        mapping,
                    );
                }
            }
            Ty::Slice(slice) => {
                if let Ty::Slice(concrete) = concrete_value {
                    self.collect_enum_generic_mapping(
                        slice.elem.as_ref(),
                        concrete.elem.as_ref(),
                        generic_names,
                        mapping,
                    );
                }
            }
            Ty::Vec(vec) => {
                if let Ty::Vec(concrete) = concrete_value {
                    self.collect_enum_generic_mapping(
                        vec.ty.as_ref(),
                        concrete.ty.as_ref(),
                        generic_names,
                        mapping,
                    );
                }
            }
            Ty::Array(array) => {
                if let Ty::Array(concrete) = concrete_value {
                    self.collect_enum_generic_mapping(
                        array.elem.as_ref(),
                        concrete.elem.as_ref(),
                        generic_names,
                        mapping,
                    );
                }
            }
            Ty::Tuple(tuple) => {
                if let Ty::Tuple(concrete) = concrete_value {
                    for (def_elem, conc_elem) in
                        tuple.types.iter().zip(concrete.types.iter())
                    {
                        self.collect_enum_generic_mapping(
                            def_elem,
                            conc_elem,
                            generic_names,
                            mapping,
                        );
                    }
                }
            }
            Ty::Struct(def_struct) => {
                if let Ty::Struct(concrete) = concrete_value {
                    for def_field in &def_struct.fields {
                        if let Some(concrete_field) = concrete
                            .fields
                            .iter()
                            .find(|field| field.name == def_field.name)
                        {
                            self.collect_enum_generic_mapping(
                                &def_field.value,
                                &concrete_field.value,
                                generic_names,
                                mapping,
                            );
                        }
                    }
                }
            }
            Ty::Structural(def_struct) => {
                if let Ty::Structural(concrete) = concrete_value {
                    for def_field in &def_struct.fields {
                        if let Some(concrete_field) = concrete
                            .fields
                            .iter()
                            .find(|field| field.name == def_field.name)
                        {
                            self.collect_enum_generic_mapping(
                                &def_field.value,
                                &concrete_field.value,
                                generic_names,
                                mapping,
                            );
                        }
                    }
                }
            }
            Ty::Enum(def_enum) => {
                if let Ty::Enum(concrete) = concrete_value {
                    for def_variant in &def_enum.variants {
                        if let Some(concrete_variant) = concrete
                            .variants
                            .iter()
                            .find(|variant| variant.name == def_variant.name)
                        {
                            self.collect_enum_generic_mapping(
                                &def_variant.value,
                                &concrete_variant.value,
                                generic_names,
                                mapping,
                            );
                        }
                    }
                }
            }
            _ => {}
        }
    }

    fn bind_pattern_expected_type(&mut self, pat: &Pattern, expected: &Ty) -> Result<()> {
        match pat.kind() {
            PatternKind::Ident(ident) => {
                if let Some(var) = self.lookup_env_var(ident.ident.as_str()) {
                    let expected_var = self.type_from_ast_ty(expected)?;
                    self.unify(var, expected_var)?;
                }
            }
            PatternKind::Bind(bind) => {
                if let Some(var) = self.lookup_env_var(bind.ident.ident.as_str()) {
                    let expected_var = self.type_from_ast_ty(expected)?;
                    self.unify(var, expected_var)?;
                }
                self.bind_pattern_expected_type(bind.pattern.as_ref(), expected)?;
            }
            PatternKind::Type(pattern_type) => {
                self.bind_pattern_expected_type(pattern_type.pat.as_ref(), &pattern_type.ty)?;
            }
            PatternKind::Tuple(tuple) => {
                if let Ty::Tuple(tuple_ty) = expected {
                    for (pat, expected_ty) in tuple
                        .patterns
                        .iter()
                        .zip(tuple_ty.types.iter())
                    {
                        self.bind_pattern_expected_type(pat, expected_ty)?;
                    }
                }
            }
            PatternKind::TupleStruct(tuple_struct) => {
                if let Ty::Enum(enum_ty) = expected {
                    let nested = Pattern::new(PatternKind::TupleStruct(tuple_struct.clone()));
                    self.bind_enum_variant_pattern(&nested, enum_ty)?;
                }
            }
            PatternKind::Struct(struct_pat) => {
                if let Ty::Struct(struct_ty) = expected {
                    for field in &struct_pat.fields {
                        let Some(def_field) =
                            struct_ty.fields.iter().find(|f| f.name == field.name)
                        else {
                            continue;
                        };
                        if let Some(rename) = field.rename.as_ref() {
                            self.bind_pattern_expected_type(rename, &def_field.value)?;
                        } else if let Some(var) =
                            self.lookup_env_var(field.name.as_str())
                        {
                            let expected_var = self.type_from_ast_ty(&def_field.value)?;
                            self.unify(var, expected_var)?;
                        }
                    }
                } else if let Ty::Structural(structural) = expected {
                    for field in &struct_pat.fields {
                        let Some(def_field) =
                            structural.fields.iter().find(|f| f.name == field.name)
                        else {
                            continue;
                        };
                        if let Some(rename) = field.rename.as_ref() {
                            self.bind_pattern_expected_type(rename, &def_field.value)?;
                        } else if let Some(var) =
                            self.lookup_env_var(field.name.as_str())
                        {
                            let expected_var = self.type_from_ast_ty(&def_field.value)?;
                            self.unify(var, expected_var)?;
                        }
                    }
                }
            }
            PatternKind::Structural(structural) => {
                if let Ty::Structural(struct_ty) = expected {
                    for field in &structural.fields {
                        let Some(def_field) =
                            struct_ty.fields.iter().find(|f| f.name == field.name)
                        else {
                            continue;
                        };
                        if let Some(rename) = field.rename.as_ref() {
                            self.bind_pattern_expected_type(rename, &def_field.value)?;
                        } else if let Some(var) =
                            self.lookup_env_var(field.name.as_str())
                        {
                            let expected_var = self.type_from_ast_ty(&def_field.value)?;
                            self.unify(var, expected_var)?;
                        }
                    }
                }
            }
            PatternKind::Variant(variant) => {
                if let Ty::Enum(enum_ty) = expected {
                    let nested = Pattern::new(PatternKind::Variant(variant.clone()));
                    self.bind_enum_variant_pattern(&nested, enum_ty)?;
                }
            }
            PatternKind::Box(box_pat) => {
                let inner_expected = match expected {
                    Ty::Expr(expr) => {
                        if let ExprKind::Name(Name::ParameterPath(path)) = expr.kind() {
                            if let Some(segment) = path.segments.last() {
                                if segment.ident.as_str() == "Box" && segment.args.len() == 1 {
                                    &segment.args[0]
                                } else {
                                    expected
                                }
                            } else {
                                expected
                            }
                        } else {
                            expected
                        }
                    }
                    _ => expected,
                };
                self.bind_pattern_expected_type(box_pat.pattern.as_ref(), inner_expected)?;
            }
            PatternKind::Ref(ref_pat) => {
                let inner_expected = match expected {
                    Ty::Reference(reference) => reference.ty.as_ref(),
                    _ => expected,
                };
                self.bind_pattern_expected_type(ref_pat.pattern.as_ref(), inner_expected)?;
            }
            PatternKind::Quote(_)
            | PatternKind::QuotePlural(_)
            | PatternKind::Wildcard(_) => {}
        }
        Ok(())
    }
}

fn qualify_enum_variant_pattern(pat: &mut Pattern, enum_ty: &TypeEnum) {
    if let PatternKind::Ident(ident) = pat.kind() {
        let variant_name = ident.ident.clone();
        if let Some(variant) = enum_ty
            .variants
            .iter()
            .find(|variant| variant.name.as_str() == variant_name.as_str())
        {
            if matches!(variant.value, Ty::Unit(_)) {
                let enum_ident = enum_ty.name.clone();
                let path = Path::plain(vec![enum_ident, variant_name]);
                pat.kind = PatternKind::Variant(PatternVariant {
                    name: Expr::path(path),
                    pattern: None,
                });
                return;
            }
        }
    }

    match pat.kind_mut() {
        PatternKind::Variant(variant) => {
            let ExprKind::Name(locator) = variant.name.kind() else {
                return;
            };
            let variant_name = match locator {
                Name::Ident(ident) => ident.clone(),
                Name::Path(path)
                    if path.prefix == PathPrefix::Plain && path.segments.len() == 1 =>
                {
                    path.segments[0].clone()
                }
                _ => return,
            };
            if !enum_ty
                .variants
                .iter()
                .any(|variant| variant.name.as_str() == variant_name.as_str())
            {
                return;
            }

            let enum_ident = enum_ty.name.clone();
            let path = Path::plain(vec![enum_ident, variant_name]);
            variant.name = Expr::path(path);
        }
        PatternKind::TupleStruct(tuple_struct) => {
            let variant_name = match &tuple_struct.name {
                Name::Ident(ident) => ident.clone(),
                Name::Path(path)
                    if path.prefix == PathPrefix::Plain && path.segments.len() == 1 =>
                {
                    path.segments[0].clone()
                }
                _ => return,
            };
            if !enum_ty
                .variants
                .iter()
                .any(|variant| variant.name.as_str() == variant_name.as_str())
            {
                return;
            }
            let enum_ident = enum_ty.name.clone();
            let path = Path::plain(vec![enum_ident, variant_name]);
            tuple_struct.name = Name::Path(path);
        }
        _ => {}
    }
}
