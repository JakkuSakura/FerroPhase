use crate::ast::*;
use crate::context::ArcScopedContext;
use crate::interpreter::Interpreter;
use crate::passes::OptimizePass;
use crate::typing::TypeSystem;
use crate::value::*;
use crate::*;
use common::*;
use std::sync::atomic::{AtomicUsize, Ordering};

pub struct SpecializePass {
    spec_id: AtomicUsize,
    serializer: Rc<dyn Serializer>,
    interpreter: Interpreter,
    type_system: TypeSystem,
}
impl SpecializePass {
    pub fn new(serializer: Rc<dyn Serializer>) -> Self {
        Self {
            spec_id: AtomicUsize::default(),
            interpreter: Interpreter::new(serializer.clone()),
            type_system: TypeSystem::new(serializer.clone()),
            serializer,
        }
    }

    pub fn specialize_import(&self, import: Import, _ctx: &ArcScopedContext) -> Result<Import> {
        Ok(import)
    }

    pub fn specialize_invoke_details(
        &self,
        invoke: Invoke,
        func: &FunctionValue,
        ctx: &ArcScopedContext,
    ) -> Result<Invoke> {
        let args: Vec<_> = invoke
            .args
            .iter()
            .map(|x| match x {
                Expr::Locator(v) => ctx
                    .get_expr(v)
                    .with_context(|| format!("Couldn't find {:?} in context", v)),

                _ => Ok(x.clone()),
            })
            .try_collect()?;
        debug!(
            "Specializing Invoke {} with {} [{}]",
            self.serializer.serialize_function(&func)?,
            self.serializer.serialize_args(&args)?,
            ctx.list_values()
                .into_iter()
                .map(|x| x.to_string())
                .join(", ")
        );
        let name = func.name.as_ref().map(|x| x.name.as_str()).unwrap_or("fun");
        let mut new_params: Vec<FunctionParam> = vec![];
        let mut new_args: Vec<Expr> = vec![];
        for (param, arg) in zip_eq(func.params.iter(), args.iter()) {
            match self.interpreter.interpret_expr(arg.clone(), ctx) {
                Err(err) => {
                    warn!("Cannot evaluate arg {} {:?}: {:?}", param.name, arg, err);
                    new_args.push(arg.clone());
                    new_params.push(param.clone());
                }
                Ok(_) => {}
            }
        }
        if !new_params.is_empty() && new_params.len() == func.params.len() {
            warn!(
                "Couldn't specialize Invoke {} with {}",
                self.serializer.serialize_function(&func)?,
                self.serializer.serialize_args(&args)?,
            );
            ctx.print_values(&*self.serializer)?;
            return Ok(invoke);
        }
        let mut bindings = vec![];
        for name in ctx.list_values() {
            let value = ctx.get_value(&name).unwrap();

            if matches!(value, Value::Function(_)) {
                continue;
            }
            let Some(name) = name.try_into_ident() else {
                continue;
            };

            let binding = Statement::Let(Let {
                name: name.into(),
                mutability: Some(false),
                ty: None,
                value: Expr::value(value),
            });
            bindings.push(binding);
        }

        let new_body = Expr::block(Block::prepend(bindings, *func.body.clone()));
        let new_name = Ident::new(format!(
            "{}_{}",
            name,
            self.spec_id.fetch_add(1, Ordering::Relaxed)
        ));

        let mut ret = func.ret.clone();
        match &ret {
            TypeValue::Expr(expr) => match &**expr {
                TypeExpr::Locator(Locator::Ident(ident))
                    if func
                        .generics_params
                        .iter()
                        .find(|x| &x.name == ident)
                        .is_some() =>
                {
                    ret = self.type_system.infer_expr(&new_body, &ctx)?.into();
                }
                _ => {}
            },
            _ => {}
        }
        let sig = FunctionSignature {
            name: Some(new_name.clone()),
            params: new_params.clone(),
            generics_params: vec![],
            ret: ret.clone(),
        };
        let new_func = FunctionValue {
            sig,
            body: new_body.into(),
        };
        trace!(
            "Specialized function {} with {} => {}",
            name,
            self.serializer.serialize_args(&args)?,
            self.serializer.serialize_function(&new_func)?
        );

        ctx.root()
            .insert_specialized(new_name.clone().into(), new_func);
        return Ok(Invoke {
            func: Expr::ident(new_name).into(),
            args: Default::default(),
        });
    }

    pub fn specialize_invoking(
        &self,
        invoke: Invoke,
        func: &FunctionValue,
        ctx: &ArcScopedContext,
    ) -> Result<Invoke> {
        match &*invoke.func {
            Expr::Locator(Locator::Ident(ident)) if ident.as_str() == "print" => {
                return Ok(invoke);
            }
            _ => {}
        }

        self.specialize_invoke_details(invoke, func, ctx)
    }
    pub fn specialize_module(&self, mut module: Module, ctx: &ArcScopedContext) -> Result<Module> {
        debug!(
            "Specializing module {}",
            self.serializer.serialize_module(&module)?
        );
        module.items = module
            .items
            .into_iter()
            .filter(|x| match x {
                Item::Define(d) if d.name.as_str() == "main" || d.name.as_str() == "print" => true,
                Item::Define(d) => {
                    let func = match &d.value {
                        DefineValue::Function(f) => f,
                        _ => return true,
                    };
                    func.params.is_empty() && func.generics_params.is_empty()
                }
                _ => true,
            })
            .collect();
        for specialized_name in ctx.list_specialized().into_iter().sorted() {
            let func = ctx.get_function(specialized_name).unwrap();
            let define = Define {
                name: func.name.clone().expect("No specialized name"),
                kind: DefineKind::Function,
                ty: Some(TypeValue::Function(
                    self.type_system.infer_function(&func, ctx)?,
                )),
                value: DefineValue::Function(func),
                visibility: Visibility::Public,
            };
            module.items.push(Item::Define(define));
        }

        Ok(module)
    }
    pub fn specialize_item(&self, item: Item, ctx: &ArcScopedContext) -> Result<Item> {
        match item {
            Item::Define(_) => Ok(item),
            Item::Module(x) => self.specialize_module(x, ctx).map(Item::Module),
            _ => Ok(item),
        }
    }
}
impl OptimizePass for SpecializePass {
    fn name(&self) -> &str {
        "specialize"
    }

    fn optimize_item_post(&self, item: Item, ctx: &ArcScopedContext) -> Result<Item> {
        self.specialize_item(item, ctx)
    }
    fn optimize_invoke_post(
        &self,
        invoke: Invoke,
        func: &FunctionValue,
        ctx: &ArcScopedContext,
    ) -> Result<Invoke> {
        self.specialize_invoking(invoke, &func, ctx)
    }
    fn optimize_module_post(&self, module: Module, ctx: &ArcScopedContext) -> Result<Module> {
        self.specialize_module(module, ctx)
    }

    fn evaluate_condition(
        &self,
        expr: Expr,
        ctx: &ArcScopedContext,
    ) -> Result<Option<ControlFlow>> {
        match self.interpreter.opt.pass.evaluate_condition(expr, ctx) {
            Ok(ok) => Ok(ok),
            Err(err) => {
                warn!("Cannot evaluate condition: {}", err);
                Ok(None)
            }
        }
    }

    fn try_evaluate_expr(&self, pat: &Expr, ctx: &ArcScopedContext) -> Result<Expr> {
        match ctx.try_get_value_from_expr(pat) {
            Some(value) => Ok(Expr::value(value)),
            None => Ok(pat.clone()),
        }
    }
    fn evaluate_invoke(
        &self,
        _invoke: Invoke,
        _ctx: &ArcScopedContext,
    ) -> Result<Option<ControlFlow>> {
        Ok(Some(ControlFlow::Into))
    }
}
