// use fp_core::ast::BExpr;
// use fp_core::error::Result;
// use fp_core::{hir, mir, lir};
//
// /// Unified transformation pipeline that replaces the 5 separate Generator structs
// /// This consolidates AST→HIR→THIR→MIR→LIR transformations into a single, simpler interface
// pub struct TransformationPipeline {
//     /// Skip intermediate IRs and go directly to useful targets
//     direct_mode: bool,
// }
//
// impl TransformationPipeline {
//     pub fn new() -> Self {
//         Self { direct_mode: true }
//     }
//
//     /// Transform AST directly to the target you actually need
//     pub fn transform_to_target(&self, ast: &BExpr, target: TransformTarget) -> Result<TransformResult> {
//         match target {
//             TransformTarget::Interpret => {
//                 // Direct interpretation without intermediate IRs
//                 self.interpret_ast(ast)
//             }
//             TransformTarget::Rust => {
//                 // Direct Rust generation
//                 self.generate_rust(ast)
//             }
//             TransformTarget::Llvm => {
//                 // Only use MIR→LIR for LLVM (the only path that actually needs it)
//                 self.compile_to_llvm(ast)
//             }
//         }
//     }
//
//     fn interpret_ast(&self, ast: &BExpr) -> Result<TransformResult> {
//         // Direct interpretation without building intermediate IRs
//         // This eliminates the massive InterpretationOrchestrator complexity
//         todo!("Direct AST interpretation")
//     }
//
//     fn generate_rust(&self, ast: &BExpr) -> Result<TransformResult> {
//         // Direct code generation from AST
//         // No need for HIR/THIR/MIR for simple transpilation
//         todo!("Direct Rust generation")
//     }
//
//     fn compile_to_llvm(&self, ast: &BExpr) -> Result<TransformResult> {
//         // Simplified pipeline: AST → HIR → MIR → LIR (skip THIR)
//         let hir = self.ast_to_hir(ast)?;
//         let mir = self.hir_to_mir(&hir)?;
//         let lir = self.mir_to_lir(&mir)?;
//         Ok(TransformResult::Lir(lir))
//     }
//
//     // Simplified transformation methods (much smaller than the original Generators)
//     fn ast_to_hir(&self, _ast: &BExpr) -> Result<hir::HirModule> {
//         todo!("Simplified AST→HIR")
//     }
//
//     fn hir_to_mir(&self, _hir: &hir::HirModule) -> Result<mir::MirModule> {
//         todo!("Simplified HIR→MIR (skip THIR)")
//     }
//
//     fn mir_to_lir(&self, _mir: &mir::MirModule) -> Result<lir::LirModule> {
//         todo!("Simplified MIR→LIR")
//     }
// }
//
// pub enum TransformTarget {
//     Interpret,  // Direct execution
//     Rust,       // Transpilation to Rust
//     Llvm,       // Compilation to LLVM IR
// }
//
// pub enum TransformResult {
//     Value(fp_core::ast::Value),
//     Code(String),
//     Lir(lir::LirModule),
// }
//
// impl Default for TransformationPipeline {
//     fn default() -> Self {
//         Self::new()
//     }
// }
