use crate::error::JvmError;
use crate::jir::{JvmClass, JvmCode, JvmInstr, JvmMethod, JvmProgram};
use fp_core::mir;
use std::collections::HashMap;

const ACC_PUBLIC: u16 = 0x0001;
const ACC_STATIC: u16 = 0x0008;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct JvmBackendOptions {
    pub class_name: String,
    pub emit_java_entrypoint: bool,
}

impl Default for JvmBackendOptions {
    fn default() -> Self {
        Self {
            class_name: "Main".to_string(),
            emit_java_entrypoint: true,
        }
    }
}

pub fn lower_program(
    program: &mir::Program,
    options: &JvmBackendOptions,
) -> Result<JvmProgram, JvmError> {
    let method_descriptors = collect_method_descriptors(program)?;
    let mut methods = Vec::new();
    let mut has_fp_main = false;

    for item in &program.items {
        match &item.kind {
            mir::ItemKind::Function(function) => {
                if function.is_extern {
                    continue;
                }
                let body = program
                    .bodies
                    .get(&function.body_id)
                    .ok_or_else(|| JvmError::MissingBody(function.name.to_string()))?;
                match lower_function(function, body, &method_descriptors, &options.class_name) {
                    Ok(method) => {
                        if function.name.to_string() == "main" {
                            has_fp_main = true;
                        }
                        methods.push(method);
                    }
                    Err(err) if function.name.to_string() != "main" => continue,
                    Err(err) => return Err(err),
                }
            }
            mir::ItemKind::Static(_) => return Err(JvmError::UnsupportedItem("static")),
            mir::ItemKind::Query(_) => continue,
        }
    }

    if options.emit_java_entrypoint && has_fp_main {
        methods.push(build_java_entrypoint(
            &options.class_name,
            &method_descriptors,
        )?);
    }

    Ok(JvmProgram {
        class: JvmClass {
            name: options.class_name.clone(),
            super_name: "java/lang/Object".to_string(),
            methods,
        },
    })
}

fn collect_method_descriptors(program: &mir::Program) -> Result<HashMap<String, String>, JvmError> {
    let mut descriptors = HashMap::new();
    for item in &program.items {
        if let mir::ItemKind::Function(function) = &item.kind {
            if function.is_extern {
                continue;
            }
            descriptors.insert(function.name.to_string(), method_descriptor(&function.sig)?);
        }
    }
    Ok(descriptors)
}

fn lower_function(
    function: &mir::Function,
    body: &mir::Body,
    method_descriptors: &HashMap<String, String>,
    class_name: &str,
) -> Result<JvmMethod, JvmError> {
    let descriptor = method_descriptor(&function.sig)?;
    let mut instructions = Vec::new();

    for (block_index, block) in body.basic_blocks.iter().enumerate() {
        instructions.push(JvmInstr::Label(format!("bb{block_index}")));
        for statement in &block.statements {
            lower_statement(statement, &mut instructions, method_descriptors)?;
        }
        let terminator = block
            .terminator
            .as_ref()
            .ok_or(JvmError::MissingTerminator(block_index))?;
        lower_terminator(
            terminator,
            &mut instructions,
            method_descriptors,
            class_name,
        )?;
    }

    Ok(JvmMethod {
        name: function.name.to_string(),
        descriptor,
        access_flags: ACC_PUBLIC | ACC_STATIC,
        code: JvmCode {
            max_stack: 8,
            max_locals: local_slot_count(&body.locals),
            instructions,
        },
    })
}

fn lower_statement(
    statement: &mir::Statement,
    out: &mut Vec<JvmInstr>,
    method_descriptors: &HashMap<String, String>,
) -> Result<(), JvmError> {
    match &statement.kind {
        mir::StatementKind::Assign(place, rvalue) => {
            lower_assign(place, rvalue, out, method_descriptors)
        }
        mir::StatementKind::Nop
        | mir::StatementKind::StorageLive(_)
        | mir::StatementKind::StorageDead(_) => Ok(()),
        _ => Err(JvmError::UnsupportedItem("statement kind")),
    }
}

fn lower_assign(
    place: &mir::Place,
    rvalue: &mir::Rvalue,
    out: &mut Vec<JvmInstr>,
    _method_descriptors: &HashMap<String, String>,
) -> Result<(), JvmError> {
    if !place.projection.is_empty() {
        return Err(JvmError::UnsupportedItem("place projection"));
    }

    match rvalue {
        mir::Rvalue::Use(operand) => {
            lower_operand(operand, out)?;
            out.push(JvmInstr::IStore(place.local as u16));
            Ok(())
        }
        mir::Rvalue::Query(_) => Err(JvmError::UnsupportedItem("query rvalue")),
        mir::Rvalue::BinaryOp(op, lhs, rhs) => {
            lower_operand(lhs, out)?;
            lower_operand(rhs, out)?;
            match op {
                mir::BinOp::Add => out.push(JvmInstr::IAdd),
                mir::BinOp::Sub => out.push(JvmInstr::ISub),
                mir::BinOp::Mul => out.push(JvmInstr::IMul),
                mir::BinOp::Div => out.push(JvmInstr::IDiv),
                _ => return Err(JvmError::UnsupportedRvalue),
            }
            out.push(JvmInstr::IStore(place.local as u16));
            Ok(())
        }
        _ => Err(JvmError::UnsupportedRvalue),
    }
}

fn lower_terminator(
    terminator: &mir::Terminator,
    out: &mut Vec<JvmInstr>,
    method_descriptors: &HashMap<String, String>,
    class_name: &str,
) -> Result<(), JvmError> {
    match &terminator.kind {
        mir::TerminatorKind::Goto { target } => {
            out.push(JvmInstr::Goto(format!("bb{target}")));
            Ok(())
        }
        mir::TerminatorKind::Return => {
            out.push(JvmInstr::IReturn);
            Ok(())
        }
        mir::TerminatorKind::SwitchInt { discr, targets, .. } => {
            if targets.values.len() != 1 {
                return Err(JvmError::UnsupportedTerminator);
            }
            lower_operand(discr, out)?;
            match targets.values[0] {
                0 => out.push(JvmInstr::IfEq(format!("bb{}", targets.targets[0]))),
                1 => out.push(JvmInstr::IfNe(format!("bb{}", targets.targets[0]))),
                _ => return Err(JvmError::UnsupportedTerminator),
            }
            out.push(JvmInstr::Goto(format!("bb{}", targets.otherwise)));
            Ok(())
        }
        mir::TerminatorKind::Call {
            func,
            args,
            destination,
            ..
        } => {
            let (class, method, descriptor) = lower_callee(func, method_descriptors, class_name)?;
            for arg in args {
                lower_operand(arg, out)?;
            }
            out.push(JvmInstr::Invokestatic {
                class,
                method,
                descriptor,
            });
            if let Some((place, target)) = destination {
                if !place.projection.is_empty() {
                    return Err(JvmError::UnsupportedItem("call destination projection"));
                }
                out.push(JvmInstr::IStore(place.local as u16));
                out.push(JvmInstr::Goto(format!("bb{target}")));
            }
            Ok(())
        }
        _ => Err(JvmError::UnsupportedTerminator),
    }
}

fn lower_callee(
    operand: &mir::Operand,
    method_descriptors: &HashMap<String, String>,
    class_name: &str,
) -> Result<(String, String, String), JvmError> {
    match operand {
        mir::Operand::Constant(constant) => match &constant.literal {
            mir::ConstantKind::Fn(symbol, _) => Ok((
                class_name.to_string(),
                symbol.to_string(),
                method_descriptors
                    .get(symbol.as_str())
                    .cloned()
                    .ok_or_else(|| {
                        JvmError::Lowering(format!("missing descriptor for {symbol}"))
                    })?,
            )),
            _ => Err(JvmError::UnsupportedItem("callee literal")),
        },
        _ => Err(JvmError::UnsupportedItem("callee operand")),
    }
}

fn lower_operand(operand: &mir::Operand, out: &mut Vec<JvmInstr>) -> Result<(), JvmError> {
    match operand {
        mir::Operand::Copy(place) | mir::Operand::Move(place) => {
            if !place.projection.is_empty() {
                return Err(JvmError::UnsupportedItem("place projection"));
            }
            out.push(JvmInstr::ILoad(place.local as u16));
            Ok(())
        }
        mir::Operand::Constant(constant) => match &constant.literal {
            mir::ConstantKind::Int(value) => {
                let value = i32::try_from(*value).map_err(|_| JvmError::IntOutOfRange(*value))?;
                out.push(JvmInstr::IConst(value));
                Ok(())
            }
            mir::ConstantKind::Bool(value) => {
                out.push(JvmInstr::IConst(if *value { 1 } else { 0 }));
                Ok(())
            }
            _ => Err(JvmError::UnsupportedItem("constant literal")),
        },
    }
}

fn method_descriptor(sig: &mir::FunctionSig) -> Result<String, JvmError> {
    let mut descriptor = String::from("(");
    for input in &sig.inputs {
        descriptor.push_str(&field_descriptor(input)?);
    }
    descriptor.push(')');
    descriptor.push_str(&field_descriptor(&sig.output)?);
    Ok(descriptor)
}

fn field_descriptor(ty: &mir::ty::Ty) -> Result<String, JvmError> {
    match &ty.kind {
        mir::ty::TyKind::Int(mir::ty::IntTy::I32) => Ok("I".to_string()),
        mir::ty::TyKind::Int(mir::ty::IntTy::I64) => Ok("J".to_string()),
        mir::ty::TyKind::Bool => Ok("Z".to_string()),
        mir::ty::TyKind::Tuple(items) if items.is_empty() => Ok("V".to_string()),
        mir::ty::TyKind::Slice(_)
        | mir::ty::TyKind::Tuple(_)
        | mir::ty::TyKind::Ref(_, _, _)
        | mir::ty::TyKind::RawPtr(_)
        | mir::ty::TyKind::Adt(_, _)
        | mir::ty::TyKind::Array(_, _)
        | mir::ty::TyKind::FnDef(_, _)
        | mir::ty::TyKind::FnPtr(_)
        | mir::ty::TyKind::Dynamic(_, _)
        | mir::ty::TyKind::Closure(_, _)
        | mir::ty::TyKind::Generator(_, _, _)
        | mir::ty::TyKind::GeneratorWitness(_)
        | mir::ty::TyKind::Projection(_)
        | mir::ty::TyKind::Opaque(_, _)
        | mir::ty::TyKind::Param(_)
        | mir::ty::TyKind::Bound(_, _)
        | mir::ty::TyKind::Placeholder(_)
        | mir::ty::TyKind::Infer(_)
        | mir::ty::TyKind::Char => Ok("Ljava/lang/Object;".to_string()),
        _ => Err(JvmError::UnsupportedType(ty.clone())),
    }
}

fn build_java_entrypoint(
    class_name: &str,
    method_descriptors: &HashMap<String, String>,
) -> Result<JvmMethod, JvmError> {
    let fp_main_descriptor = method_descriptors
        .get("main")
        .ok_or_else(|| JvmError::Lowering("missing source main descriptor".to_string()))?;

    let mut instructions = default_argument_setup(fp_main_descriptor)?;
    instructions.push(JvmInstr::Invokestatic {
        class: class_name.to_string(),
        method: "main".to_string(),
        descriptor: fp_main_descriptor.clone(),
    });

    if fp_main_descriptor.ends_with('V') {
        instructions.push(JvmInstr::Return);
    } else if fp_main_descriptor.ends_with('I') {
        instructions.push(JvmInstr::Invokestatic {
            class: "java/lang/System".to_string(),
            method: "exit".to_string(),
            descriptor: "(I)V".to_string(),
        });
        instructions.push(JvmInstr::Return);
    } else {
        return Err(JvmError::Lowering(
            "Java launcher only supports `main` returning `i32` or unit in the MVP".to_string(),
        ));
    }

    Ok(JvmMethod {
        name: "main".to_string(),
        descriptor: "([Ljava/lang/String;)V".to_string(),
        access_flags: ACC_PUBLIC | ACC_STATIC,
        code: JvmCode {
            max_stack: 4,
            max_locals: 1,
            instructions,
        },
    })
}

fn default_argument_setup(descriptor: &str) -> Result<Vec<JvmInstr>, JvmError> {
    let mut instructions = Vec::new();
    let mut chars = descriptor.chars().peekable();
    if chars.next() != Some('(') {
        return Err(JvmError::Lowering(format!(
            "invalid method descriptor: {descriptor}"
        )));
    }

    while let Some(ch) = chars.peek().copied() {
        if ch == ')' {
            chars.next();
            break;
        }
        match ch {
            'I' | 'Z' => {
                instructions.push(JvmInstr::IConst(0));
                chars.next();
            }
            'J' => {
                instructions.push(JvmInstr::LConst0);
                chars.next();
            }
            'L' => {
                instructions.push(JvmInstr::AConstNull);
                chars.next();
                for next in chars.by_ref() {
                    if next == ';' {
                        break;
                    }
                }
            }
            '[' => {
                instructions.push(JvmInstr::AConstNull);
                chars.next();
                while let Some('[') = chars.peek().copied() {
                    chars.next();
                }
                if chars.peek() == Some(&'L') {
                    chars.next();
                    for next in chars.by_ref() {
                        if next == ';' {
                            break;
                        }
                    }
                } else {
                    chars.next();
                }
            }
            other => {
                return Err(JvmError::Lowering(format!(
                    "unsupported launcher parameter descriptor: {other} in {descriptor}"
                )));
            }
        }
    }

    Ok(instructions)
}

fn local_slot_count(locals: &[mir::LocalDecl]) -> u16 {
    locals
        .iter()
        .map(|local| match &local.ty.kind {
            mir::ty::TyKind::Int(mir::ty::IntTy::I64)
            | mir::ty::TyKind::Float(mir::ty::FloatTy::F64) => 2,
            _ => 1,
        })
        .sum()
}

pub fn derive_class_name(input_name: &str) -> String {
    let mut result = String::new();
    for (index, ch) in input_name.chars().enumerate() {
        let is_valid = ch.is_ascii_alphanumeric() || ch == '_' || ch == '$';
        let normalized = if is_valid { ch } else { '_' };

        if index == 0 {
            if normalized.is_ascii_digit() {
                result.push('_');
            }
            if normalized == '_' && input_name.is_empty() {
                result.push('_');
            }
        }

        result.push(normalized);
    }

    if result.is_empty() {
        "Main".to_string()
    } else {
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fp_core::mir;
    use std::collections::HashMap;

    #[test]
    fn lowers_simple_constant_return() {
        let body_id = mir::BodyId(0);
        let int_ty = mir::ty::Ty {
            kind: mir::ty::TyKind::Int(mir::ty::IntTy::I32),
        };
        let program = mir::Program {
            items: vec![mir::Item {
                mir_id: 0,
                kind: mir::ItemKind::Function(mir::Function {
                    name: "main".into(),
                    path: vec!["main".into()],
                    def_id: None,
                    sig: mir::FunctionSig {
                        inputs: vec![],
                        output: int_ty.clone(),
                    },
                    body_id,
                    abi: mir::ty::Abi::Rust,
                    is_extern: false,
                    attrs: Vec::new(),
                }),
            }],
            bodies: HashMap::from([(
                body_id,
                mir::Body {
                    basic_blocks: vec![mir::BasicBlockData {
                        statements: vec![mir::Statement {
                            source_info: fp_core::span::Span::default(),
                            kind: mir::StatementKind::Assign(
                                mir::Place {
                                    local: 0,
                                    projection: vec![],
                                },
                                mir::Rvalue::Use(mir::Operand::Constant(mir::Constant {
                                    span: fp_core::span::Span::default(),
                                    user_ty: None,
                                    literal: mir::ConstantKind::Int(7),
                                })),
                            ),
                        }],
                        terminator: Some(mir::Terminator {
                            source_info: fp_core::span::Span::default(),
                            kind: mir::TerminatorKind::Return,
                        }),
                        is_cleanup: false,
                    }],
                    locals: vec![mir::LocalDecl {
                        mutability: mir::Mutability::Not,
                        local_info: mir::LocalInfo::Other,
                        internal: false,
                        is_block_tail: None,
                        ty: int_ty,
                        user_ty: None,
                        source_info: fp_core::span::Span::default(),
                    }],
                    arg_count: 0,
                    return_local: 0,
                    var_debug_info: vec![],
                    span: fp_core::span::Span::default(),
                },
            )]),
        };

        let lowered = lower_program(
            &program,
            &JvmBackendOptions {
                class_name: "sample".to_string(),
                emit_java_entrypoint: true,
            },
        )
        .expect("lowering should succeed");
        assert_eq!(lowered.class.name, "sample");
        assert_eq!(lowered.class.methods.len(), 2);
        assert_eq!(lowered.class.methods[0].descriptor, "()I");
    }

    #[test]
    fn derives_sanitized_class_names() {
        assert_eq!(derive_class_name("hello-world"), "hello_world");
        assert_eq!(derive_class_name("123demo"), "_123demo");
        assert_eq!(derive_class_name(""), "Main");
    }
}
