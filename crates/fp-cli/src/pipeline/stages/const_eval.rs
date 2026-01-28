use super::super::*;
use fp_core::ast::{ItemKind, Node, NodeKind};
use fp_interpret::const_eval::{
    ConstEvalContext, ConstEvalOptions, ConstEvalOutcome, ConstEvalResult, ConstEvalStage,
};
use std::fs;

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
            for std_path in runtime_std_paths() {
                let source = match fs::read_to_string(&std_path) {
                    Ok(source) => source,
                    Err(err) => {
                        return Err(Pipeline::emit_stage_error(
                            STAGE_CONST_EVAL,
                            options,
                            format!("failed to read std module {}: {}", std_path.display(), err),
                        ));
                    }
                };
                let std_node = match self.parse_input_source(options, &source, Some(&std_path)) {
                    Ok(node) => node,
                    Err(err) => {
                        return Err(Pipeline::emit_stage_error(
                            STAGE_CONST_EVAL,
                            options,
                            format!("failed to parse std module {}: {}", std_path.display(), err),
                        ));
                    }
                };
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
