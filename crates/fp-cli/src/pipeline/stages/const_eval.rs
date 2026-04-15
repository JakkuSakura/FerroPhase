use super::super::*;
use fp_core::ast::{File, ItemKind, Node, NodeKind};
use fp_interpret::const_eval::{
    ConstEvalContext, ConstEvalOptions, ConstEvalOutcome, ConstEvalResult, ConstEvalStage,
};
use fp_lang::embedded_std;
use std::path::Path;

impl Pipeline {
    pub(crate) fn stage_const_eval(
        &mut self,
        ast: &mut Node,
        options: &PipelineOptions,
    ) -> Result<ConstEvalOutcome, CliError> {
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

        let stage = ConstEvalStage;
        let context = ConstEvalContext {
            ast: ast.clone(),
            options: ConstEvalOptions {
                release: options.release,
                execute_main: options.execute_main,
            },
            serializer: self.serializer.clone(),
            macro_parser: self.macro_parser.clone(),
            intrinsic_normalizer: self.intrinsic_normalizer.clone(),
            std_modules,
        };
        let mut diagnostics = PipelineDiagnostics::default();
        diagnostics.set_display_options(diag::display_options(options));
        let pipeline = PipelineBuilder::<ConstEvalContext, ConstEvalContext>::new()
            .add_stage(stage)
            .build();
        match pipeline.run(context, &mut diagnostics) {
            Ok(ConstEvalResult {
                ast: next_ast,
                outcome,
            }) => {
                *ast = next_ast;
                if !diagnostics.items.is_empty() {
                    diagnostics.emit_stage(STAGE_CONST_EVAL);
                }
                if outcome.has_errors {
                    return Err(Self::stage_failure(STAGE_CONST_EVAL));
                }
                Ok(outcome)
            }
            Err(_) => {
                diagnostics.emit_stage(STAGE_CONST_EVAL);
                Err(Self::stage_failure(STAGE_CONST_EVAL))
            }
        }
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
