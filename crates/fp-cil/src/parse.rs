use fp_core::error::{Error, Result};
use fp_core::lir::{
    BasicBlockId, LirBasicBlock, LirBlob, LirConstant, LirDataLayout, LirFunction,
    LirFunctionSignature, LirInstruction, LirInstructionKind, LirInteger, LirRegister,
    LirTerminator, LirType, LirValue, Name,
};
use std::collections::HashMap;

/// Parse a narrow subset of textual CIL into `LirBlob`.
///
/// Scope:
/// - Intended for CIL emitted by FerroPhase itself.
/// - Supports `ldc.i4`, `ldloc`, `stloc`, `add/sub/mul/div`, `ret`, `br`, `brtrue`, labels.
pub fn parse_cil_program(text: &str) -> Result<LirBlob> {
    let mut program = LirBlob::new(
        LirDataLayout::new(
            64,
            8,
            vec![(1, 1), (8, 1), (16, 2), (32, 4), (64, 8), (128, 16)],
        )
        .expect("valid .NET LIR data layout"),
    );
    for method in parse_methods(text)? {
        program.add_function(lower_method(method)?);
    }
    Ok(program)
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ParsedMethod {
    name: Name,
    locals: u32,
    instructions: Vec<ParsedLine>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ParsedLine {
    Label(String),
    Instr(String),
}

fn parse_methods(text: &str) -> Result<Vec<ParsedMethod>> {
    let mut methods = Vec::new();
    let mut lines = text.lines().peekable();

    while let Some(raw) = lines.next() {
        let line = raw.trim();
        if !line.starts_with(".method") {
            continue;
        }

        let name = parse_method_name(line)?;
        let mut locals = 0u32;
        let mut body = Vec::new();

        while let Some(raw) = lines.next() {
            let line = raw.trim();
            if line == "}" {
                break;
            }
            if line.starts_with(".maxstack") || line.starts_with(".entrypoint") {
                continue;
            }
            if let Some(rest) = line.strip_prefix(".locals") {
                locals = parse_locals_count(rest)?;
                continue;
            }
            if let Some(label) = line.strip_suffix(':') {
                body.push(ParsedLine::Label(label.to_string()));
                continue;
            }
            if line.starts_with("//") || line.is_empty() {
                continue;
            }
            body.push(ParsedLine::Instr(line.to_string()));
        }

        methods.push(ParsedMethod {
            name,
            locals,
            instructions: body,
        });
    }

    Ok(methods)
}

fn parse_method_name(line: &str) -> Result<Name> {
    // We only need a best-effort parse for FerroPhase-emitted methods:
    // ".method public hidebysig static void Main() cil managed"
    // ".method public hidebysig static int32 main() cil managed"
    let open = line
        .find('(')
        .ok_or_else(|| Error::from("cil parse: malformed .method signature"))?;
    let before = line[..open].trim();
    let token = before
        .split_whitespace()
        .last()
        .ok_or_else(|| Error::from("cil parse: missing method name"))?;
    Ok(Name::new(token.trim_matches('\'')))
}

fn parse_locals_count(rest: &str) -> Result<u32> {
    // Accept either "init (...)" or a single local.
    let rest = rest.trim();
    if let Some(open) = rest.find('(') {
        let close = rest
            .rfind(')')
            .ok_or_else(|| Error::from("cil parse: malformed .locals"))?;
        let inside = &rest[open + 1..close];
        let count = inside
            .split(',')
            .map(|tok| tok.trim())
            .filter(|tok| !tok.is_empty())
            .count();
        return Ok(count as u32);
    }
    Ok(0)
}

fn lower_method(method: ParsedMethod) -> Result<LirFunction> {
    let label_to_block = collect_block_labels(&method.instructions);
    let mut blocks: Vec<LirBasicBlock> = Vec::new();

    // Always produce at least one block.
    let mut current_block_id: BasicBlockId = 0;
    let mut current_instructions = Vec::new();
    let mut current_label: Option<Name> = None;

    let mut stack: Vec<LirValue> = Vec::new();
    let mut next_vreg: u32 = 0;

    let mut iter = method.instructions.into_iter().peekable();
    while let Some(line) = iter.next() {
        match line {
            ParsedLine::Label(label) => {
                // Flush current block.
                if !current_instructions.is_empty() || current_label.is_some() {
                    blocks.push(LirBasicBlock {
                        id: current_block_id,
                        label: current_label.take(),
                        instructions: std::mem::take(&mut current_instructions),
                        terminator: LirTerminator::Br(
                            label_to_block
                                .get(&label)
                                .copied()
                                .unwrap_or(current_block_id),
                        ),
                        predecessors: Vec::new(),
                        successors: Vec::new(),
                    });
                    current_block_id += 1;
                    stack.clear();
                }
                current_label = Some(Name::new(label));
            }
            ParsedLine::Instr(text) => {
                if let Some(term) =
                    try_parse_terminator(&text, &mut stack, &label_to_block, current_block_id + 1)?
                {
                    blocks.push(LirBasicBlock {
                        id: current_block_id,
                        label: current_label.take(),
                        instructions: std::mem::take(&mut current_instructions),
                        terminator: term,
                        predecessors: Vec::new(),
                        successors: Vec::new(),
                    });
                    current_block_id += 1;
                    stack.clear();
                    continue;
                }
                if let Some(inst) = parse_stack_instruction(&text, &mut stack, &mut next_vreg)? {
                    current_instructions.push(inst);
                }
            }
        }
    }

    if blocks.is_empty() {
        blocks.push(LirBasicBlock {
            id: current_block_id,
            label: current_label.take(),
            instructions: current_instructions,
            terminator: LirTerminator::Return(stack.pop()),
            predecessors: Vec::new(),
            successors: Vec::new(),
        });
    } else if !current_instructions.is_empty() || current_label.is_some() {
        blocks.push(LirBasicBlock {
            id: current_block_id,
            label: current_label.take(),
            instructions: current_instructions,
            terminator: LirTerminator::Return(stack.pop()),
            predecessors: Vec::new(),
            successors: Vec::new(),
        });
    }

    Ok(LirFunction {
        def_id: None,
        name: method.name,
        signature: LirFunctionSignature {
            params: Vec::new(),
            return_type: LirType::I64,
            is_variadic: false,
        },
        basic_blocks: blocks,
        locals: (0..method.locals)
            .map(|id| fp_core::lir::LirLocal {
                id,
                ty: LirType::I64,
                name: Some(format!("loc{id}")),
                is_argument: false,
            })
            .collect(),
        stack_slots: Vec::new(),
        calling_convention: fp_core::lir::CallingConvention::C,
        linkage: fp_core::lir::Linkage::External,
        is_declaration: false,
    })
}

fn collect_block_labels(lines: &[ParsedLine]) -> HashMap<String, BasicBlockId> {
    let mut map = HashMap::new();
    let mut next = 0u32;
    let mut has_content = false;
    let mut has_label = false;
    for line in lines {
        match line {
            ParsedLine::Label(label) => {
                if has_content || has_label {
                    next += 1;
                    has_content = false;
                }
                map.entry(label.clone()).or_insert(next);
                has_label = true;
            }
            ParsedLine::Instr(text) => {
                if matches!(text.as_str(), "ret")
                    || text.starts_with("br ")
                    || text.starts_with("brtrue ")
                {
                    next += 1;
                    has_content = false;
                    has_label = false;
                } else {
                    has_content = true;
                }
            }
        }
    }
    if map.is_empty() {
        map.insert("entry".to_string(), 0);
    }
    map
}

fn try_parse_terminator(
    line: &str,
    stack: &mut Vec<LirValue>,
    labels: &HashMap<String, BasicBlockId>,
    fallthrough: BasicBlockId,
) -> Result<Option<LirTerminator>> {
    let line = line.trim();
    if line == "ret" {
        return Ok(Some(LirTerminator::Return(stack.pop())));
    }
    if let Some(rest) = line.strip_prefix("br ") {
        let target = rest.trim();
        let bb = labels
            .get(target)
            .copied()
            .ok_or_else(|| Error::from(format!("cil parse: unknown label `{target}`")))?;
        return Ok(Some(LirTerminator::Br(bb)));
    }
    if let Some(rest) = line.strip_prefix("brtrue ") {
        let target = rest.trim();
        let bb = labels
            .get(target)
            .copied()
            .ok_or_else(|| Error::from(format!("cil parse: unknown label `{target}`")))?;
        let cond = stack
            .pop()
            .ok_or_else(|| Error::from("cil parse: brtrue missing condition"))?;
        return Ok(Some(LirTerminator::CondBr {
            condition: cond,
            if_true: bb,
            // `brtrue` carries only the taken target; false falls through.
            if_false: fallthrough,
        }));
    }
    Ok(None)
}

fn parse_stack_instruction(
    line: &str,
    stack: &mut Vec<LirValue>,
    next_vreg: &mut u32,
) -> Result<Option<LirInstruction>> {
    let line = line.trim();

    if let Some(rest) = line.strip_prefix("ldc.i4 ") {
        let value = rest
            .trim()
            .parse::<i64>()
            .map_err(|_| Error::from("cil parse: invalid ldc.i4"))?;
        stack.push(LirValue::constant(
            LirConstant::integer(LirType::I64, LirInteger::I64(value as u64))
                .expect("valid .NET integer"),
        ));
        return Ok(None);
    }
    if let Some(rest) = line.strip_prefix("ldc.i8 ") {
        let value = rest
            .trim()
            .parse::<i64>()
            .map_err(|_| Error::from("cil parse: invalid ldc.i8"))?;
        stack.push(LirValue::constant(
            LirConstant::integer(LirType::I64, LirInteger::I64(value as u64))
                .expect("valid .NET integer"),
        ));
        return Ok(None);
    }
    if let Some(rest) = line
        .strip_prefix("ldloc.")
        .or_else(|| line.strip_prefix("ldloc "))
    {
        let id = rest
            .trim()
            .parse::<u32>()
            .map_err(|_| Error::from("cil parse: invalid ldloc"))?;
        stack.push(LirValue::local(id, LirType::I64));
        return Ok(None);
    }
    if let Some(rest) = line
        .strip_prefix("stloc.")
        .or_else(|| line.strip_prefix("stloc "))
    {
        let id = rest
            .trim()
            .parse::<u32>()
            .map_err(|_| Error::from("cil parse: invalid stloc"))?;
        let value = stack
            .pop()
            .ok_or_else(|| Error::from("cil parse: stloc missing value"))?;
        let id_inst = *next_vreg;
        *next_vreg += 1;
        return Ok(Some(LirInstruction {
            id: id_inst,
            kind: LirInstructionKind::Store {
                value,
                address: LirValue::local(id, LirType::I64),
                alignment: None,
                volatile: false,
            },
            result: None,
            debug_info: None,
        }));
    }

    let binop = match line {
        "add" => Some(LirInstructionKind::Add as fn(_, _) -> _),
        "sub" => Some(LirInstructionKind::Sub as fn(_, _) -> _),
        "mul" => Some(LirInstructionKind::Mul as fn(_, _) -> _),
        "div" => Some(LirInstructionKind::Div as fn(_, _) -> _),
        _ => None,
    };
    if let Some(constructor) = binop {
        let rhs = stack
            .pop()
            .ok_or_else(|| Error::from("cil parse: missing rhs"))?;
        let lhs = stack
            .pop()
            .ok_or_else(|| Error::from("cil parse: missing lhs"))?;
        let id = *next_vreg;
        *next_vreg += 1;
        let kind = constructor(lhs.clone(), rhs.clone());
        stack.push(LirValue::register(id, LirType::I64));
        return Ok(Some(LirInstruction {
            id,
            kind,
            result: Some(LirRegister {
                id,
                ty: LirType::I64,
            }),
            debug_info: None,
        }));
    }

    Err(Error::from(format!(
        "cil parse: unsupported instruction `{line}`"
    )))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn stloc_preserves_destination_local() {
        let program = parse_cil_program(
            ".method public static int32 'main'() cil managed {\n.locals init (int32 V_0)\nldc.i4 7\nstloc 0\nldloc 0\nret\n}\n",
        )
        .expect("valid CIL");
        let instructions = &program.functions[0].basic_blocks[0].instructions;
        let LirInstructionKind::Store { address, .. } = &instructions[0].kind else {
            panic!("expected stloc to lower to Store");
        };
        assert!(matches!(address.kind, fp_core::lir::LirValueKind::Local(0)));
    }

    #[test]
    fn unknown_instruction_is_rejected() {
        let result =
            parse_cil_program(".method public static int32 'main'() cil managed {\nfoo\nret\n}\n");
        assert!(result.is_err());
    }

    #[test]
    fn emitted_cil_round_trips_and_executes() {
        use fp_core::mir::ty::{Abi, IntTy, Ty};
        use fp_core::mir::{
            self, BasicBlockData, Body, BodyId, Constant, ConstantKind, Function, FunctionSig,
            Item, ItemKind, LocalDecl, LocalInfo, MirCodeUnit, Mutability, Operand, Rvalue,
            Statement, StatementKind, Symbol, Terminator, TerminatorKind,
        };
        use fp_core::span::Span;

        let span = Span::new(0, 0, 0);
        let int_ty = Ty::int(IntTy::I32);
        let body = Body::new(
            vec![BasicBlockData {
                statements: vec![Statement {
                    source_info: span,
                    kind: StatementKind::Assign(
                        mir::Place::from_local(0),
                        Rvalue::Use(Operand::Constant(Constant {
                            span,
                            ty: int_ty.clone(),
                            user_ty: None,
                            literal: ConstantKind::Int(7),
                        })),
                    ),
                }],
                terminator: Some(Terminator {
                    source_info: span,
                    kind: TerminatorKind::Return,
                }),
                is_cleanup: false,
            }],
            vec![LocalDecl {
                mutability: Mutability::Mut,
                local_info: LocalInfo::Other,
                internal: false,
                is_block_tail: None,
                ty: int_ty.clone(),
                user_ty: None,
                source_info: span,
            }],
            0,
            span,
        );
        let mut mir = MirCodeUnit::new();
        mir.bodies.insert(BodyId(0), body);
        mir.items.push(Item {
            mir_id: 0,
            kind: ItemKind::Function(Function {
                name: Symbol::new("main"),
                def_id: None,
                substs: Vec::new(),
                sig: FunctionSig {
                    inputs: Vec::new(),
                    output: int_ty,
                },
                body_id: BodyId(0),
                abi: Abi::Rust,
                is_extern: false,
                attrs: Vec::new(),
            }),
        });

        let cil = super::super::cil::emit_cil(&mir).expect("emit CIL");
        let lir = parse_cil_program(&cil).expect("parse emitted CIL");
        let value = fp_interpret::LirInterpreter::new()
            .run_main(&lir)
            .expect("execute LIR");
        assert_eq!(value, fp_core::ast::Value::int(7));
    }
}
