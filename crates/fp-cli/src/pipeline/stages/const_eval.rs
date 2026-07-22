use super::super::*;
use fp_core::ast::{File, ItemKind, Node, NodeKind};
use fp_core::context::SharedScopedContext;
use fp_core::lang::{collect_lang_items, register_threadlocal_lang_items};
use fp_interpret::const_eval::{
    ConstEvalOptions, ConstEvalOutcome, ConstEvaluationOrchestrator,
};
use fp_lang::embedded_std;
use std::path::Path;

impl Pipeline {
    pub(crate) fn stage_const_eval(
        &mut self,
        ast: &mut Node,
        options: &PipelineOptions,
    ) -> Result<ConstEvalOutcome, CliError> {
        if matches!(
            ast.kind(),
            NodeKind::Query(_) | NodeKind::Schema(_) | NodeKind::Workspace(_)
        ) {
            return Ok(ConstEvalOutcome::default());
        }

        let mut std_modules = Vec::new();
        let include_std = if matches!(options.target, BackendKind::Interpret) {
            !ast_has_std(ast)
        } else {
            true
        };
        if include_std {
            for std_path in runtime_std_paths(&options.target) {
                let source = match embedded_std::read(&std_path) {
                    Some(source) => source,
                    None => {
                        return Err(Pipeline::emit_stage_error(
                            STAGE_CONST_EVAL,
                            options,
                            format!("failed to read std module {}", std_path.display()),
                        ));
                    }
                };
                let language = self.resolve_language(options, Some(&std_path));
                let frontend = match self.frontends.get(&language).cloned() {
                    Some(frontend) => frontend,
                    None => {
                        return Err(Pipeline::emit_stage_error(
                            STAGE_CONST_EVAL,
                            options,
                            format!("unsupported source language: {}", language),
                        ));
                    }
                };
                let mut std_node =
                    match self.parse_with_frontend(&frontend, &source, Some(&std_path), options) {
                        Ok(node) => node,
                        Err(err) => {
                            return Err(Pipeline::emit_stage_error(
                                STAGE_CONST_EVAL,
                                options,
                                format!(
                                    "failed to parse std module {}: {}",
                                    std_path.display(),
                                    err
                                ),
                            ));
                        }
                    };
                if let NodeKind::File(file) = std_node.kind().clone() {
                    let base_dir = std_path.parent().unwrap_or_else(|| Path::new("."));
                    let mut loader = FileModuleLoader::new(self, options, &frontend);
                    let items = match loader.resolve_items(&file.items, base_dir) {
                        Ok(items) => items,
                        Err(err) => {
                            return Err(Pipeline::emit_stage_error(
                                STAGE_CONST_EVAL,
                                options,
                                format!(
                                    "failed to resolve std module {}: {}",
                                    std_path.display(),
                                    err
                                ),
                            ));
                        }
                    };
                    std_node = Node::file(File {
                        path: file.path,
                        attrs: file.attrs,
                        items,
                    });
                }
                std_modules.push(std_node);
            }
        }

        let mut diagnostics = PipelineDiagnostics::default();
        diagnostics.set_display_options(diag::display_options(options));
        let mut working_ast = ast.clone();
        for std_node in std_modules {
            working_ast = merge_std_module(
                working_ast,
                std_node,
                &options.target,
                &mut diagnostics,
                STAGE_CONST_EVAL,
            )
            .map_err(|_| {
                diagnostics.emit_stage(STAGE_CONST_EVAL);
                Self::stage_failure(STAGE_CONST_EVAL)
            })?;
        }

        let lang_items = collect_lang_items(&working_ast);
        register_threadlocal_lang_items(lang_items);
        let normalization = if let Some(normalizer) = self.intrinsic_normalizer.as_ref() {
            fp_core::intrinsics::normalize_intrinsics_with(&mut working_ast, normalizer.as_ref())
        } else {
            fp_core::intrinsics::normalize_intrinsics(&mut working_ast)
        };
        if let Err(err) = normalization {
            diagnostics.push(
                Diagnostic::error(format!("Intrinsic normalization failed: {}", err))
                    .with_source_context(STAGE_CONST_EVAL),
            );
            diagnostics.emit_stage(STAGE_CONST_EVAL);
            return Err(Self::stage_failure(STAGE_CONST_EVAL));
        }

        let serializer = self.serializer.clone().ok_or_else(|| {
            Pipeline::emit_stage_error(
                STAGE_CONST_EVAL,
                options,
                "No serializer registered for const-eval",
            )
        })?;
        let shared_context = SharedScopedContext::new();
        let mut orchestrator = ConstEvaluationOrchestrator::new(serializer);
        let eval_options = ConstEvalOptions {
            release: options.release,
            execute_main: options.execute_main,
        };
        orchestrator.set_debug_assertions(!eval_options.release);
        orchestrator.set_execute_main(eval_options.execute_main);
        let outcome = orchestrator
            .evaluate(
                &mut working_ast,
                &shared_context,
                self.macro_parser.clone(),
                self.intrinsic_normalizer.clone(),
            )
            .map_err(|err| {
                diagnostics.push(
                    Diagnostic::error(format!("Const evaluation failed: {}", err))
                        .with_source_context(STAGE_CONST_EVAL),
                );
                diagnostics.emit_stage(STAGE_CONST_EVAL);
                Self::stage_failure(STAGE_CONST_EVAL)
            })?;
        diagnostics.extend(outcome.diagnostics.clone());
        *ast = working_ast;
        if !diagnostics.items.is_empty() {
            diagnostics.emit_stage(STAGE_CONST_EVAL);
        }
        if outcome.has_errors {
            return Err(Self::stage_failure(STAGE_CONST_EVAL));
        }
        Ok(outcome)
    }
}

fn ast_has_std(ast: &Node) -> bool {
    let NodeKind::File(file) = ast.kind() else {
        return false;
    };
    file.items.iter().any(|item| {
        matches!(
            item.kind(),
            ItemKind::Module(module) if module.name.as_str() == "std"
        )
    })
}
