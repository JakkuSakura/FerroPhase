use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::{InitializationConfig, Target, TargetTriple};
use inkwell::values::FunctionValue;
use std::path::Path;

use crate::target::{CodeModel, OptimizationLevel, RelocMode, TargetConfig};

/// LLVM compilation context that manages the LLVM module.
pub struct LlvmContext {
    pub context: &'static Context,
    pub module: Module<'static>,
    pub builder: Builder<'static>,
    pub(crate) current_function: Option<FunctionValue<'static>>,
    next_unnamed_counter: u32,
}

impl LlvmContext {
    /// Create a new LLVM context with the given name.
    pub fn new(module_name: &str) -> Self {
        let context = Box::leak(Box::new(Context::create()));
        let module = context.create_module(module_name);
        let builder = context.create_builder();

        Self {
            context,
            module,
            builder,
            current_function: None,
            next_unnamed_counter: 0,
        }
    }

    /// Initialize target machine for the current platform.
    pub fn init_target_machine(&mut self) -> Result<(), String> {
        let config = TargetConfig::host();
        self.init_target_machine_with_config(&config)
    }

    /// Initialize target machine for a specific configuration.
    pub fn init_target_machine_with_config(&mut self, config: &TargetConfig) -> Result<(), String> {
        Target::initialize_all(&InitializationConfig::default());

        let triple = TargetTriple::create(&config.triple);
        self.module.set_triple(&triple);

        let optimization_level = match config.optimization_level {
            OptimizationLevel::None => inkwell::OptimizationLevel::None,
            OptimizationLevel::Less => inkwell::OptimizationLevel::Less,
            OptimizationLevel::Default => inkwell::OptimizationLevel::Default,
            OptimizationLevel::Aggressive => inkwell::OptimizationLevel::Aggressive,
        };
        let reloc_mode = match config.reloc_mode {
            RelocMode::Default => inkwell::targets::RelocMode::Default,
            RelocMode::Static => inkwell::targets::RelocMode::Static,
            RelocMode::PIC => inkwell::targets::RelocMode::PIC,
            RelocMode::DynamicNoPic => inkwell::targets::RelocMode::DynamicNoPic,
        };
        let code_model = match config.code_model {
            CodeModel::Default => inkwell::targets::CodeModel::Default,
            CodeModel::JITDefault => inkwell::targets::CodeModel::JITDefault,
            CodeModel::Small => inkwell::targets::CodeModel::Small,
            CodeModel::Kernel => inkwell::targets::CodeModel::Kernel,
            CodeModel::Medium => inkwell::targets::CodeModel::Medium,
            CodeModel::Large => inkwell::targets::CodeModel::Large,
        };

        let target = Target::from_triple(&triple).map_err(|e| e.to_string())?;
        let target_machine = target
            .create_target_machine(
                &triple,
                &config.cpu,
                &config.features,
                optimization_level,
                reloc_mode,
                code_model,
            )
            .ok_or_else(|| "Failed to create target machine".to_string())?;

        self.module
            .set_data_layout(&target_machine.get_target_data().get_data_layout());
        Ok(())
    }

    /// Generate a unique unnamed value name.
    pub fn gen_unnamed(&mut self) -> String {
        let name = format!("unnamed_{}", self.next_unnamed_counter);
        self.next_unnamed_counter += 1;
        name
    }

    /// Verify the module.
    pub fn verify_module(&self) -> Result<(), String> {
        self.module.verify().map_err(|e| e.to_string()).map(|_| ())
    }

    /// Print the module IR to string.
    pub fn print_to_string(&self) -> String {
        self.module.print_to_string().to_string()
    }

    /// Write module to file.
    pub fn write_to_file(&self, output_path: &Path) -> Result<(), String> {
        self.module
            .print_to_file(output_path)
            .map_err(|e| e.to_string())
    }
}
