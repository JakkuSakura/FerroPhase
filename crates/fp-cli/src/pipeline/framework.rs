use fp_core::diagnostics::{Diagnostic, DiagnosticDisplayOptions, DiagnosticManager};
use std::error::Error;
use std::fmt;
use std::marker::PhantomData;

#[derive(Clone)]
pub struct PipelineDiagnostics {
    pub items: Vec<Diagnostic>,
    display_options: DiagnosticDisplayOptions,
}

impl PipelineDiagnostics {
    pub fn new(display_options: DiagnosticDisplayOptions) -> Self {
        Self {
            items: Vec::new(),
            display_options,
        }
    }

    pub fn set_display_options(&mut self, display_options: DiagnosticDisplayOptions) {
        self.display_options = display_options;
    }

    pub fn push(&mut self, diagnostic: Diagnostic) {
        self.items.push(diagnostic);
    }

    pub fn emit_stage(&mut self, stage: &'static str) {
        if self.items.is_empty() {
            return;
        }
        DiagnosticManager::emit(&self.items, Some(stage), &self.display_options);
        self.items.clear();
    }

    pub fn extend(&mut self, diagnostics: Vec<Diagnostic>) {
        if diagnostics.is_empty() {
            return;
        }
        self.items.extend(diagnostics);
    }
}

impl Default for PipelineDiagnostics {
    fn default() -> Self {
        Self::new(DiagnosticDisplayOptions::default())
    }
}

#[derive(Debug)]
pub struct PipelineError {
    pub stage: &'static str,
    pub message: String,
}

impl PipelineError {
    pub fn new(stage: &'static str, message: impl Into<String>) -> Self {
        Self {
            stage,
            message: message.into(),
        }
    }
}

impl fmt::Display for PipelineError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}] {}", self.stage, self.message)
    }
}

impl Error for PipelineError {}

pub trait PipelineStage: Send + Sync {
    type SrcCtx;
    type DstCtx;

    fn name(&self) -> &'static str;
    fn run(
        &self,
        context: Self::SrcCtx,
        diagnostics: &mut PipelineDiagnostics,
    ) -> Result<Self::DstCtx, PipelineError>;
}

pub struct Pipeline<Src, Dst> {
    run: Box<dyn Fn(Src, &mut PipelineDiagnostics) -> Result<Dst, PipelineError> + Send + Sync>,
}

impl<Src, Dst> Pipeline<Src, Dst> {
    pub fn run(
        &self,
        context: Src,
        diagnostics: &mut PipelineDiagnostics,
    ) -> Result<Dst, PipelineError> {
        (self.run)(context, diagnostics)
    }
}

pub struct PipelineBuilder<Src, Dst> {
    pipeline: Pipeline<Src, Dst>,
    _marker: PhantomData<(Src, Dst)>,
}

impl<Src> PipelineBuilder<Src, Src> {
    pub fn new() -> Self {
        let run = |context: Src, _diagnostics: &mut PipelineDiagnostics| Ok(context);
        Self {
            pipeline: Pipeline { run: Box::new(run) },
            _marker: PhantomData,
        }
    }
}

impl<Src, Mid> PipelineBuilder<Src, Mid> {
    pub fn add_stage<Next, S>(self, stage: S) -> PipelineBuilder<Src, Next>
    where
        S: PipelineStage<SrcCtx = Mid, DstCtx = Next> + 'static,
        Src: 'static,
        Mid: 'static,
        Next: 'static,
    {
        let name = stage.name();
        let previous = self.pipeline.run;
        let run = move |context: Src, diagnostics: &mut PipelineDiagnostics| {
            let mid = previous(context, diagnostics)?;
            match stage.run(mid, diagnostics) {
                Ok(next) => {
                    diagnostics.emit_stage(name);
                    Ok(next)
                }
                Err(err) => {
                    diagnostics.emit_stage(name);
                    if err.stage == name {
                        Err(err)
                    } else {
                        Err(PipelineError::new(name, err.message))
                    }
                }
            }
        };

        PipelineBuilder {
            pipeline: Pipeline { run: Box::new(run) },
            _marker: PhantomData,
        }
    }

    pub fn build(self) -> Pipeline<Src, Mid> {
        self.pipeline
    }
}
