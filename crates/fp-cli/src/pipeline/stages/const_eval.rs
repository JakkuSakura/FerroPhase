use super::super::*;
use fp_core::ast::Node;
use fp_interpret::const_eval::{
    ConstEvalContext, ConstEvalOutcome, ConstEvalResult, ConstEvalStage,
};
use std::fs;

impl Pipeline {
    pub(crate) fn stage_const_eval(
        &mut self,
        ast: &mut Node,
        options: &PipelineOptions,
    ) -> Result<ConstEvalOutcome, CliError> {
        let mut std_modules = Vec::new();
        if !matches!(options.target, fp_pipeline::PipelineTarget::Interpret) {
            for std_path in runtime_std_paths() {
                let source = match fs::read_to_string(&std_path) {
                    Ok(source) => source,
                    Err(err) => {
                        return Err(CliError::Compilation(format!(
                            "failed to read std module {}: {}",
                            std_path.display(),
                            err
                        )));
                    }
                };
                let std_node = self.parse_input_source(options, &source, Some(&std_path))?;
                std_modules.push(std_node);
            }
        }

        let stage = ConstEvalStage;
        let context = ConstEvalContext {
            ast: ast.clone(),
            options: options.clone(),
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
