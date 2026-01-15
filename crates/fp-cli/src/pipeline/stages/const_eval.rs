use super::super::*;
use fp_core::ast::Node;
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
        if !matches!(options.target, BackendKind::Interpret) {
            for std_path in runtime_std_paths() {
                let source = match fs::read_to_string(&std_path) {
                    Ok(source) => source,
                    Err(err) => {
                        return Err(Pipeline::emit_stage_error(
                            STAGE_CONST_EVAL,
                            options,
                            format!(
                                "failed to read std module {}: {}",
                                std_path.display(),
                                err
                            ),
                        ));
                    }
                };
                let std_node = match self.parse_input_source(options, &source, Some(&std_path)) {
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
            std_modules,
        };
        let ConstEvalResult {
            ast: next_ast,
            outcome,
        } = self.run_pipeline_stage(STAGE_CONST_EVAL, stage, context, options)?;
        *ast = next_ast;
        Ok(outcome)
    }
}
