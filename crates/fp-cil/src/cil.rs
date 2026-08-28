use std::fmt::Write;
use std::path::{Path, PathBuf};
use std::process::Command;

use eyre::{Result as EyreResult, bail};
use fp_core::error::Result;
use fp_core::mir;
use tempfile::TempDir;

/// Lowers `mir` into CIL assembly text — mirrors `fp_jvm::lower_program`+
/// `emit_class_files`'s MIR→bytecode shape (CIL is stack-based bytecode
/// like JVM's, not a source-level transpile target like Kotlin's; walking
/// the typed AST directly, as this used to, was the wrong level).
pub fn emit_cil(program: &mir::MirCodeUnit) -> Result<String> {
    let mut out = String::from(
        "// FerroPhase .NET backend\n.assembly FerroPhaseProgram {}\n\n.class public auto ansi beforefieldinit FerroPhaseProgram extends [mscorlib]System.Object {\n",
    );
    for item in &program.items {
        let mir::ItemKind::Function(function) = &item.kind else {
            continue;
        };
        if function.is_extern {
            continue;
        }
        let body = program.bodies.get(&function.body_id).ok_or_else(|| {
            fp_core::error::Error::from(format!("missing MIR body for `{}`", function.name))
        })?;
        let ret = cil_type(&function.sig.output)?;
        let params = function
            .sig
            .inputs
            .iter()
            .map(cil_type)
            .collect::<Result<Vec<_>>>()?
            .join(", ");
        out.push_str(&format!("  .method public hidebysig static {ret} '{}'({params}) cil managed {{\n    .maxstack 8\n", function.name));
        if function.name.as_str() == "main" {
            out.push_str("    .entrypoint\n");
        }
        if !body.locals.is_empty() {
            let locals = body
                .locals
                .iter()
                .enumerate()
                .map(|(index, local)| cil_type(&local.ty).map(|ty| format!("{ty} V_{index}")))
                .collect::<Result<Vec<_>>>()?
                .join(", ");
            out.push_str(&format!("    .locals init ({locals})\n"));
        }
        emit_body(&mut out, body, program)?;
        out.push_str("  }\n\n");
    }
    out.push_str("}\n");
    Ok(out)
}

fn cil_type(ty: &mir::ty::Ty) -> Result<String> {
    match &ty.kind {
        mir::ty::TyKind::Int(mir::ty::IntTy::I32) => Ok("int32".into()),
        mir::ty::TyKind::Int(mir::ty::IntTy::I64) => Ok("int64".into()),
        mir::ty::TyKind::Uint(mir::ty::UintTy::U32) => Ok("uint32".into()),
        mir::ty::TyKind::Uint(mir::ty::UintTy::U64) => Ok("uint64".into()),
        mir::ty::TyKind::Bool => Ok("bool".into()),
        mir::ty::TyKind::Tuple(items) if items.is_empty() => Ok("void".into()),
        other => Err(fp_core::error::Error::from(format!(
            "unsupported CIL type: {other:?}"
        ))),
    }
}

fn emit_body(out: &mut String, body: &mir::Body, program: &mir::MirCodeUnit) -> Result<()> {
    for block in &body.basic_blocks {
        if block.statements.is_empty() && block.terminator.is_none() {
            continue;
        }
        for statement in &block.statements {
            let mir::StatementKind::Assign(place, rvalue) = &statement.kind else {
                continue;
            };
            emit_rvalue(out, rvalue, program)?;
            writeln!(out, "    stloc {}", place.local).ok();
        }
        if let Some(terminator) = &block.terminator {
            match &terminator.kind {
                mir::TerminatorKind::Return => {
                    writeln!(out, "    ldloc 0").ok();
                    writeln!(out, "    ret").ok();
                }
                mir::TerminatorKind::Goto { target } => {
                    writeln!(out, "    br IL_{target}").ok();
                }
                mir::TerminatorKind::Call {
                    func,
                    args,
                    destination,
                    ..
                } => {
                    for arg in args {
                        emit_operand(out, arg, program)?;
                    }
                    let name = match func {
                        mir::Operand::Constant(constant) => match &constant.literal {
                            mir::ConstantKind::FnDef(def_id, _) => {
                                program.items.iter().find_map(|item| match &item.kind {
                                    mir::ItemKind::Function(function)
                                        if function.def_id.as_ref() == Some(def_id) =>
                                    {
                                        Some(function.name.to_string())
                                    }
                                    _ => None,
                                })
                            }
                            _ => None,
                        },
                        _ => None,
                    }
                    .ok_or_else(|| fp_core::error::Error::from("unsupported CIL call target"))?;
                    let signature = program
                        .items
                        .iter()
                        .find_map(|item| match &item.kind {
                            mir::ItemKind::Function(function)
                                if function.name.to_string() == name =>
                            {
                                Some(function.sig.clone())
                            }
                            _ => None,
                        })
                        .ok_or_else(|| fp_core::error::Error::from("missing CIL call signature"))?;
                    let call_ret = cil_type(&signature.output)?;
                    let call_params = signature
                        .inputs
                        .iter()
                        .map(cil_type)
                        .collect::<Result<Vec<_>>>()?
                        .join(", ");
                    writeln!(
                        out,
                        "    call {call_ret} FerroPhaseProgram::'{name}'({call_params})"
                    )
                    .ok();
                    if let Some((place, target)) = destination {
                        writeln!(out, "    stloc {}", place.local).ok();
                        writeln!(out, "    br IL_{target}").ok();
                    }
                }
                other => {
                    return Err(fp_core::error::Error::from(format!(
                        "unsupported CIL terminator: {other:?}"
                    )));
                }
            }
        }
    }
    Ok(())
}

fn emit_rvalue(out: &mut String, rvalue: &mir::Rvalue, program: &mir::MirCodeUnit) -> Result<()> {
    match rvalue {
        mir::Rvalue::Use(mir::Operand::Constant(constant)) => match constant.literal {
            mir::ConstantKind::Int(value) => {
                writeln!(out, "    ldc.i8 {value}").ok();
                Ok(())
            }
            mir::ConstantKind::Bool(value) => {
                writeln!(out, "    ldc.i4 {}", i32::from(value)).ok();
                Ok(())
            }
            _ => Err(fp_core::error::Error::from("unsupported CIL constant")),
        },
        mir::Rvalue::Use(mir::Operand::Copy(place) | mir::Operand::Move(place)) => {
            writeln!(out, "    ldloc {}", place.local).ok();
            Ok(())
        }
        mir::Rvalue::BinaryOp(op, lhs, rhs) => {
            emit_operand(out, lhs, program)?;
            emit_operand(out, rhs, program)?;
            let name = match op {
                mir::BinOp::Add => "add",
                mir::BinOp::Sub => "sub",
                mir::BinOp::Mul => "mul",
                mir::BinOp::Div => "div",
                _ => {
                    return Err(fp_core::error::Error::from(
                        "unsupported CIL binary operator",
                    ));
                }
            };
            writeln!(out, "    {name}").ok();
            Ok(())
        }
        _ => Err(fp_core::error::Error::from("unsupported CIL rvalue")),
    }
}

fn emit_operand(
    out: &mut String,
    operand: &mir::Operand,
    program: &mir::MirCodeUnit,
) -> Result<()> {
    match operand {
        mir::Operand::Constant(constant) => emit_rvalue(
            out,
            &mir::Rvalue::Use(mir::Operand::Constant(constant.clone())),
            program,
        ),
        mir::Operand::Copy(place) | mir::Operand::Move(place) => {
            writeln!(out, "    ldloc {}", place.local).ok();
            Ok(())
        }
    }
}

pub fn emit_assembly(
    mir: &fp_core::mir::MirCodeUnit,
    output_path: &Path,
    keep_cil: bool,
) -> Result<PathBuf> {
    let cil = emit_cil(mir)?;
    assemble_cil(&cil, output_path, keep_cil).map_err(fp_core::error::Error::from)
}

pub fn assemble_cil_text(cil: &str, output_path: &Path) -> Result<PathBuf> {
    assemble_cil(cil, output_path, false).map_err(fp_core::error::Error::from)
}

fn assemble_cil(cil: &str, output_path: &Path, keep_cil: bool) -> EyreResult<PathBuf> {
    let assembly_kind = assembly_kind_for(output_path);
    let cil_path = if keep_cil {
        let cil_path = output_path.with_extension("il");
        std::fs::write(&cil_path, cil)?;
        cil_path
    } else {
        let temp_dir = TempDir::new()?;
        let cil_path = temp_dir.path().join("ferrophase.il");
        std::fs::write(&cil_path, cil)?;
        run_ilasm(&cil_path, output_path, assembly_kind)?;
        return Ok(output_path.to_path_buf());
    };

    run_ilasm(&cil_path, output_path, assembly_kind)?;
    Ok(output_path.to_path_buf())
}

fn run_ilasm(
    cil_path: &Path,
    output_path: &Path,
    assembly_kind: DotnetAssemblyKind,
) -> EyreResult<()> {
    let ilasm = find_ilasm().ok_or_else(|| {
        eyre::eyre!(
            "unable to locate `ilasm`; install Mono ilasm or provide it on PATH to emit .NET assemblies"
        )
    })?;

    if let Some(parent) = output_path.parent() {
        std::fs::create_dir_all(parent)?;
    }

    let mut command = Command::new(ilasm);
    command.arg(cil_path);
    command.arg(format!("/output:{}", output_path.display()));
    command.arg(match assembly_kind {
        DotnetAssemblyKind::Exe => "/exe",
        DotnetAssemblyKind::Dll => "/dll",
    });

    let output = command.output()?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout = String::from_utf8_lossy(&output.stdout);
        bail!(
            "ilasm failed for `{}`: {}{}{}",
            cil_path.display(),
            stdout,
            if !stdout.is_empty() && !stderr.is_empty() {
                "\n"
            } else {
                ""
            },
            stderr
        );
    }

    Ok(())
}

fn find_ilasm() -> Option<PathBuf> {
    let path = std::env::var_os("PATH")?;
    std::env::split_paths(&path)
        .map(|entry| entry.join("ilasm"))
        .find(|candidate| candidate.is_file())
}

#[derive(Clone, Copy)]
enum DotnetAssemblyKind {
    Exe,
    Dll,
}

fn assembly_kind_for(output_path: &Path) -> DotnetAssemblyKind {
    match output_path.extension().and_then(|ext| ext.to_str()) {
        Some(ext) if ext.eq_ignore_ascii_case("dll") => DotnetAssemblyKind::Dll,
        _ => DotnetAssemblyKind::Exe,
    }
}
