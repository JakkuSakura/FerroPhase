use crate::config::PipelineOptions;
use crate::error::{PipelineDiagnostics, PipelineError};
use std::marker::PhantomData;

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
    run: Box<
        dyn Fn(Src, &mut PipelineDiagnostics, &PipelineOptions) -> Result<Dst, PipelineError>
            + Send
            + Sync,
    >,
}

impl<Src, Dst> Pipeline<Src, Dst> {
    pub fn run(
        &self,
        context: Src,
        diagnostics: &mut PipelineDiagnostics,
        options: &PipelineOptions,
    ) -> Result<Dst, PipelineError> {
        (self.run)(context, diagnostics, options)
    }
}

pub struct PipelineBuilder<Src, Dst> {
    pipeline: Pipeline<Src, Dst>,
    _marker: PhantomData<(Src, Dst)>,
}

impl<Src> PipelineBuilder<Src, Src> {
    pub fn new() -> Self {
        let run = |context: Src,
                   _diagnostics: &mut PipelineDiagnostics,
                   _options: &PipelineOptions| Ok(context);
        Self {
            pipeline: Pipeline {
                run: Box::new(run),
            },
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
        let run = move |context: Src,
                        diagnostics: &mut PipelineDiagnostics,
                        options: &PipelineOptions| {
            let mid = previous(context, diagnostics, options)?;
            match stage.run(mid, diagnostics) {
                Ok(next) => {
                    diagnostics.emit_stage(name, options);
                    Ok(next)
                }
                Err(err) if err.stage == name => Err(err),
                Err(err) => Err(PipelineError::new(name, err.message)),
            }
        };

        PipelineBuilder {
            pipeline: Pipeline {
                run: Box::new(run),
            },
            _marker: PhantomData,
        }
    }

    pub fn build(self) -> Pipeline<Src, Mid> {
        self.pipeline
    }
}
