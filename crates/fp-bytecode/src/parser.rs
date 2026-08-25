use super::*;

pub(super) fn parse_program_winnow(input: &mut &str) -> ModalResult<BytecodeProgram> {
    ws0.parse_next(input)?;
    literal("fp-bytecode").parse_next(input)?;
    ws0.parse_next(input)?;
    literal("{").parse_next(input)?;
    ws0.parse_next(input)?;
    literal("const_pool:").parse_next(input)?;
    consume_line_end(input);

    let mut const_pool = Vec::new();
    let mut functions = Vec::new();
    let mut entry = None;

    loop {
        ws0.parse_next(input)?;
        if input.is_empty() {
            return Err(ErrMode::Cut(ContextError::new()));
        }
        if input.trim_start().starts_with("functions:") {
            literal("functions:").parse_next(input)?;
            consume_line_end(input);
            break;
        }
        let line = next_non_empty_line(input)?.ok_or(ErrMode::Cut(ContextError::new()))?;
        let (index, value) =
            parse_const_pool_entry_line(line).map_err(|_| ErrMode::Cut(ContextError::new()))?;
        if index != const_pool.len() as u32 {
            return Err(ErrMode::Cut(ContextError::new()));
        }
        const_pool.push(value);
    }

    loop {
        ws0.parse_next(input)?;
        let Some(line) = next_non_empty_line(input)? else {
            return Err(ErrMode::Cut(ContextError::new()));
        };
        if line == "}" {
            break;
        }
        if let Some(rest) = line.strip_prefix("entry:") {
            let name = rest.trim();
            if name.is_empty() {
                return Err(ErrMode::Cut(ContextError::new()));
            }
            entry = Some(name.to_string());
            continue;
        }
        if line.starts_with("fn ") {
            let (name, param_types, return_type, local_types) =
                parse_function_header_line(line).map_err(|_| ErrMode::Cut(ContextError::new()))?;
            let mut blocks = Vec::new();
            loop {
                ws0.parse_next(input)?;
                let Some(peek) = peek_next_non_empty_line(input) else {
                    return Err(ErrMode::Cut(ContextError::new()));
                };
                if peek.starts_with("fn ") || peek.starts_with("entry:") || peek == "}" {
                    break;
                }
                let block_line =
                    next_non_empty_line(input)?.ok_or(ErrMode::Cut(ContextError::new()))?;
                let block_id = parse_block_header_line(block_line)
                    .map_err(|_| ErrMode::Cut(ContextError::new()))?;
                let block = parse_block_winnow(input, block_id)
                    .map_err(|_| ErrMode::Cut(ContextError::new()))?;
                blocks.push(block);
            }

            functions.push(BytecodeFunction {
                name,
                param_types,
                return_type,
                local_types,
                blocks,
            });
            continue;
        }
        return Err(ErrMode::Cut(ContextError::new()));
    }

    Ok(BytecodeProgram {
        const_pool,
        functions,
        entry,
    })
}

fn parse_const_pool_entry_line(line: &str) -> Result<(u32, BytecodeConst), BytecodeError> {
    let trimmed = line.trim();
    let Some(rest) = trimmed.strip_prefix('[') else {
        return Err(BytecodeError::Format {
            message: format!("invalid const pool entry: {}", line),
        });
    };
    let (index_part, value_part) = rest.split_once(']').ok_or_else(|| BytecodeError::Format {
        message: format!("invalid const pool entry: {}", line),
    })?;
    let index = index_part
        .trim()
        .parse::<u32>()
        .map_err(|_| BytecodeError::Format {
            message: format!("invalid const pool index: {}", line),
        })?;
    let value = parse_const_value(value_part.trim())?;
    Ok((index, value))
}

fn parse_function_header_line(
    line: &str,
) -> Result<
    (
        String,
        Vec<fp_core::lir::LirType>,
        fp_core::lir::LirType,
        Vec<fp_core::lir::LirType>,
    ),
    BytecodeError,
> {
    let trimmed = line.trim();
    let Some(rest) = trimmed.strip_prefix("fn ") else {
        return Err(BytecodeError::Format {
            message: format!("invalid function header: {}", line),
        });
    };
    let (name_part, tail) = rest.split_once('(').ok_or_else(|| BytecodeError::Format {
        message: format!("invalid function header: {}", line),
    })?;
    let name = name_part.trim();
    if name.is_empty() {
        return Err(BytecodeError::Format {
            message: format!("invalid function name: {}", line),
        });
    }
    let tail = tail.trim();
    let (tail, after) = tail.rsplit_once(')').ok_or_else(|| BytecodeError::Format {
        message: format!("invalid function header: {}", line),
    })?;
    if !after.trim().is_empty() {
        return Err(BytecodeError::Format {
            message: format!("invalid function header: {}", line),
        });
    }
    let tail = tail.trim();
    let (params_part, tail) =
        tail.split_once("], return:")
            .ok_or_else(|| BytecodeError::Format {
                message: format!("invalid typed function header: {}", line),
            })?;
    let params = params_part
        .strip_prefix("params: [")
        .ok_or_else(|| BytecodeError::Format {
            message: format!("invalid typed function parameters: {}", line),
        })?;
    let (return_part, locals_part) =
        tail.split_once(", locals: [")
            .ok_or_else(|| BytecodeError::Format {
                message: format!("invalid typed function locals: {}", line),
            })?;
    let locals = locals_part
        .strip_suffix(']')
        .ok_or_else(|| BytecodeError::Format {
            message: format!("invalid typed function locals: {}", line),
        })?;
    Ok((
        name.to_string(),
        parse_lir_type_list(params)?,
        parse_lir_type(return_part.trim())?,
        parse_lir_type_list(locals)?,
    ))
}

pub(super) fn format_lir_type(ty: &fp_core::lir::LirType) -> String {
    use fp_core::lir::LirType;
    match ty {
        LirType::Integer(bits) => format!("i{bits}"),
        LirType::I1 => "i1".to_string(),
        LirType::I8 => "i8".to_string(),
        LirType::I16 => "i16".to_string(),
        LirType::I32 => "i32".to_string(),
        LirType::I64 => "i64".to_string(),
        LirType::I128 => "i128".to_string(),
        LirType::F32 => "f32".to_string(),
        LirType::F64 => "f64".to_string(),
        LirType::Ptr(pointee) => format!("ptr<{}>", format_lir_type(pointee)),
        LirType::Array(element, count) => format!("array<{},{}>", format_lir_type(element), count),
        LirType::Void => "void".to_string(),
        unsupported => format!("unsupported<{unsupported:?}>"),
    }
}

fn parse_lir_type_list(input: &str) -> Result<Vec<fp_core::lir::LirType>, BytecodeError> {
    if input.trim().is_empty() {
        return Ok(Vec::new());
    }
    input
        .split(',')
        .map(|part| parse_lir_type(part.trim()))
        .collect()
}

fn parse_lir_type(input: &str) -> Result<fp_core::lir::LirType, BytecodeError> {
    use fp_core::lir::LirType;
    let primitive = match input {
        "i1" => Some(LirType::I1),
        "i8" => Some(LirType::I8),
        "i16" => Some(LirType::I16),
        "i32" => Some(LirType::I32),
        "i64" => Some(LirType::I64),
        "i128" => Some(LirType::I128),
        "f32" => Some(LirType::F32),
        "f64" => Some(LirType::F64),
        "void" => Some(LirType::Void),
        _ => None,
    };
    if let Some(ty) = primitive {
        return Ok(ty);
    }
    if let Some(bits) = input
        .strip_prefix('i')
        .and_then(|bits| bits.parse::<u32>().ok())
    {
        return Ok(LirType::Integer(bits));
    }
    if let Some(inner) = input
        .strip_prefix("ptr<")
        .and_then(|value| value.strip_suffix('>'))
    {
        return Ok(LirType::Ptr(Box::new(parse_lir_type(inner)?)));
    }
    Err(BytecodeError::Format {
        message: format!("unsupported bytecode type: {input}"),
    })
}

fn parse_block_header_line(line: &str) -> Result<u32, BytecodeError> {
    let trimmed = line.trim();
    let Some(rest) = trimmed.strip_prefix("bb") else {
        return Err(BytecodeError::Format {
            message: format!("invalid block header: {}", line),
        });
    };
    let rest = rest.trim_end_matches(':');
    let id = rest
        .trim()
        .parse::<u32>()
        .map_err(|_| BytecodeError::Format {
            message: format!("invalid block header: {}", line),
        })?;
    Ok(id)
}

fn parse_block_winnow(input: &mut &str, block_id: u32) -> Result<BytecodeBlock, BytecodeError> {
    let mut code = Vec::new();
    let terminator = loop {
        let line = next_line(input).map_err(|_| BytecodeError::Format {
            message: "unexpected end while parsing block".to_string(),
        })?;
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }
        if let Some(rest) = trimmed.strip_prefix("terminator ") {
            break parse_terminator(rest.trim())?;
        }
        if trimmed.starts_with("bb")
            || trimmed.starts_with("fn ")
            || trimmed.starts_with("entry:")
            || trimmed == "}"
        {
            return Err(BytecodeError::Format {
                message: "block terminator missing".to_string(),
            });
        }
        code.push(parse_instr(trimmed)?);
    };

    Ok(BytecodeBlock {
        id: block_id,
        code,
        terminator,
    })
}

pub(super) fn ws0(input: &mut &str) -> ModalResult<()> {
    take_while(0.., char::is_whitespace)
        .map(|_| ())
        .parse_next(input)
}

fn next_line<'a>(input: &mut &'a str) -> ModalResult<&'a str> {
    let line = take_till(0.., |ch: char| ch == '\n' || ch == '\r').parse_next(input)?;
    consume_line_end(input);
    Ok(line)
}

fn next_non_empty_line<'a>(input: &mut &'a str) -> ModalResult<Option<&'a str>> {
    loop {
        if input.is_empty() {
            return Ok(None);
        }
        let line = next_line(input)?;
        let trimmed = line.trim();
        if !trimmed.is_empty() {
            return Ok(Some(trimmed));
        }
    }
}

fn peek_next_non_empty_line(input: &str) -> Option<&str> {
    input
        .lines()
        .map(|line| line.trim())
        .find(|line| !line.is_empty())
}

fn consume_line_end(input: &mut &str) {
    if input.starts_with("\r\n") {
        *input = &input[2..];
    } else if input.starts_with('\n') || input.starts_with('\r') {
        *input = &input[1..];
    }
}

fn parse_instr(line: &str) -> Result<BytecodeInstr, BytecodeError> {
    if let Some(rest) = line.strip_prefix("load.const ") {
        return Ok(BytecodeInstr::LoadConst(parse_u32(rest)?));
    }
    if let Some(rest) = line.strip_prefix("load.local ") {
        return Ok(BytecodeInstr::LoadLocal(parse_u32(rest)?));
    }
    if let Some(rest) = line.strip_prefix("store.local ") {
        return Ok(BytecodeInstr::StoreLocal(parse_u32(rest)?));
    }
    if let Some(rest) = line.strip_prefix("load.place ") {
        return Ok(BytecodeInstr::LoadPlace(parse_place(rest)?));
    }
    if let Some(rest) = line.strip_prefix("store.place ") {
        return Ok(BytecodeInstr::StorePlace(parse_place(rest)?));
    }
    if let Some(rest) = line.strip_prefix("binop ") {
        return Ok(BytecodeInstr::BinaryOp(parse_binop(rest)?));
    }
    if let Some(rest) = line.strip_prefix("unop ") {
        return Ok(BytecodeInstr::UnaryOp(parse_unop(rest)?));
    }
    if let Some(rest) = line.strip_prefix("intrinsic ") {
        return parse_intrinsic(rest);
    }
    if let Some(rest) = line.strip_prefix("make.tuple ") {
        return Ok(BytecodeInstr::MakeTuple(parse_u32(rest)?));
    }
    if let Some(rest) = line.strip_prefix("make.array ") {
        return Ok(BytecodeInstr::MakeArray(parse_u32(rest)?));
    }
    if let Some(rest) = line.strip_prefix("make.list ") {
        return Ok(BytecodeInstr::MakeList(parse_u32(rest)?));
    }
    if let Some(rest) = line.strip_prefix("make.map ") {
        return Ok(BytecodeInstr::MakeMap(parse_u32(rest)?));
    }
    if line == "container.get" {
        return Ok(BytecodeInstr::ContainerGet);
    }
    if line == "container.len" {
        return Ok(BytecodeInstr::ContainerLen);
    }
    if line == "pop" {
        return Ok(BytecodeInstr::Pop);
    }

    Err(BytecodeError::Format {
        message: format!("unknown instruction: {}", line),
    })
}

fn parse_terminator(line: &str) -> Result<BytecodeTerminator, BytecodeError> {
    if line == "return" {
        return Ok(BytecodeTerminator::Return);
    }
    if let Some(rest) = line.strip_prefix("jump bb") {
        return Ok(BytecodeTerminator::Jump {
            target: parse_u32(rest)?,
        });
    }
    if let Some(rest) = line.strip_prefix("jump_if_true bb") {
        let (target, otherwise) = parse_jump_pair(rest)?;
        return Ok(BytecodeTerminator::JumpIfTrue { target, otherwise });
    }
    if let Some(rest) = line.strip_prefix("jump_if_false bb") {
        let (target, otherwise) = parse_jump_pair(rest)?;
        return Ok(BytecodeTerminator::JumpIfFalse { target, otherwise });
    }
    if let Some(rest) = line.strip_prefix("switch ") {
        return parse_switch(rest);
    }
    if let Some(rest) = line.strip_prefix("call ") {
        return parse_call(rest);
    }
    if line == "abort" {
        return Ok(BytecodeTerminator::Abort);
    }
    if line == "unreachable" {
        return Ok(BytecodeTerminator::Unreachable);
    }

    Err(BytecodeError::Format {
        message: format!("unknown terminator: {}", line),
    })
}

fn parse_jump_pair(rest: &str) -> Result<(u32, u32), BytecodeError> {
    let (target_part, otherwise_part) =
        rest.split_once(" else bb")
            .ok_or_else(|| BytecodeError::Format {
                message: format!("invalid jump format: {}", rest),
            })?;
    let target = parse_u32(target_part)?;
    let otherwise = parse_u32(otherwise_part)?;
    Ok((target, otherwise))
}

fn parse_switch(rest: &str) -> Result<BytecodeTerminator, BytecodeError> {
    let (list_part, otherwise_part) = rest
        .strip_prefix('[')
        .and_then(|s| s.split_once("] otherwise bb"))
        .ok_or_else(|| BytecodeError::Format {
            message: format!("invalid switch format: {}", rest),
        })?;
    let mut values = Vec::new();
    let mut targets = Vec::new();
    for entry in split_top_level(list_part) {
        let entry = entry.trim();
        if entry.is_empty() {
            continue;
        }
        let (value, target) = entry
            .split_once(":bb")
            .ok_or_else(|| BytecodeError::Format {
                message: format!("invalid switch entry: {}", entry),
            })?;
        let value = value
            .trim()
            .parse::<u128>()
            .map_err(|_| BytecodeError::Format {
                message: format!("invalid switch value: {}", value),
            })?;
        let target = parse_u32(target)?;
        values.push(value);
        targets.push(target);
    }
    let otherwise = parse_u32(otherwise_part)?;
    Ok(BytecodeTerminator::SwitchInt {
        values,
        targets,
        otherwise,
    })
}

fn parse_call(rest: &str) -> Result<BytecodeTerminator, BytecodeError> {
    let (before_arrow, after_arrow) =
        rest.split_once(" -> ")
            .ok_or_else(|| BytecodeError::Format {
                message: format!("invalid call format: {}", rest),
            })?;
    let (dest_part, target_part) =
        after_arrow
            .split_once(" then bb")
            .ok_or_else(|| BytecodeError::Format {
                message: format!("invalid call format: {}", rest),
            })?;
    let (before_type, type_part) =
        before_arrow
            .rsplit_once(" : ")
            .ok_or_else(|| BytecodeError::Format {
                message: format!("call is missing result type: {}", rest),
            })?;
    let result_type = parse_lir_type(type_part.trim())?;
    let (callee_part, arg_count_part) =
        before_type
            .rsplit_once(' ')
            .ok_or_else(|| BytecodeError::Format {
                message: format!("invalid call format: {}", rest),
            })?;
    let callee = parse_callee(callee_part.trim())?;
    let arg_count = parse_u32(arg_count_part.trim())?;
    let destination = if dest_part.trim() == "_" {
        None
    } else {
        Some(parse_place(dest_part.trim())?)
    };
    let target = parse_u32(target_part.trim())?;
    Ok(BytecodeTerminator::Call {
        callee,
        arg_count,
        destination,
        result_type,
        target,
    })
}

fn parse_intrinsic(rest: &str) -> Result<BytecodeInstr, BytecodeError> {
    let (signature, result_type, format_part) = match rest.split_once(" : ") {
        Some((signature, result)) => {
            let (type_part, format_part) = result.split_once(' ').unwrap_or((result, ""));
            (signature, parse_lir_type(type_part)?, format_part.trim())
        }
        None => {
            return Err(BytecodeError::Format {
                message: format!("intrinsic is missing result type: {}", rest),
            });
        }
    };
    let mut parts = signature.splitn(3, ' ');
    let kind_part = parts.next().ok_or_else(|| BytecodeError::Format {
        message: format!("invalid intrinsic: {}", rest),
    })?;
    let count_part = parts.next().ok_or_else(|| BytecodeError::Format {
        message: format!("invalid intrinsic: {}", rest),
    })?;
    let kind = parse_intrinsic_kind(kind_part)?;
    let arg_count = parse_u32(count_part)?;
    let format = match format_part {
        raw if !raw.is_empty() => {
            let (value, rest) = parse_debug_string(raw)?;
            if !rest.trim().is_empty() {
                return Err(BytecodeError::Format {
                    message: format!("invalid intrinsic format: {}", rest),
                });
            }
            Some(value)
        }
        _ => None,
    };

    Ok(BytecodeInstr::IntrinsicCall {
        kind,
        arg_count,
        format,
        result_type,
    })
}

fn parse_intrinsic_kind(raw: &str) -> Result<IntrinsicKind, BytecodeError> {
    match raw {
        "Println" => Ok(IntrinsicKind::Println),
        "Print" => Ok(IntrinsicKind::Print),
        "Format" => Ok(IntrinsicKind::Format),
        "Len" => Ok(IntrinsicKind::Len),
        "DebugAssertions" => Ok(IntrinsicKind::DebugAssertions),
        "Input" => Ok(IntrinsicKind::Input),
        "Panic" => Ok(IntrinsicKind::Panic),
        "CatchUnwind" => Ok(IntrinsicKind::CatchUnwind),
        "SizeOf" => Ok(IntrinsicKind::SizeOf),
        "ReflectFields" => Ok(IntrinsicKind::ReflectFields),
        "HasMethod" => Ok(IntrinsicKind::HasMethod),
        "TypeName" => Ok(IntrinsicKind::TypeName),
        "TypeOf" => Ok(IntrinsicKind::TypeOf),
        "HasField" => Ok(IntrinsicKind::HasField),
        "FieldCount" => Ok(IntrinsicKind::FieldCount),
        "MethodCount" => Ok(IntrinsicKind::MethodCount),
        "FieldType" => Ok(IntrinsicKind::FieldType),
        "StructSize" => Ok(IntrinsicKind::StructSize),
        "GenerateMethod" => Ok(IntrinsicKind::GenerateMethod),
        "CompileError" => Ok(IntrinsicKind::CompileError),
        "CompileWarning" => Ok(IntrinsicKind::CompileWarning),
        _ => Err(BytecodeError::Format {
            message: format!("unknown intrinsic kind: {}", raw),
        }),
    }
}

fn parse_binop(raw: &str) -> Result<BytecodeBinOp, BytecodeError> {
    match raw {
        "Add" => Ok(BytecodeBinOp::Add),
        "Sub" => Ok(BytecodeBinOp::Sub),
        "Mul" => Ok(BytecodeBinOp::Mul),
        "Div" => Ok(BytecodeBinOp::Div),
        "Rem" => Ok(BytecodeBinOp::Rem),
        "And" => Ok(BytecodeBinOp::And),
        "Or" => Ok(BytecodeBinOp::Or),
        "BitXor" => Ok(BytecodeBinOp::BitXor),
        "BitAnd" => Ok(BytecodeBinOp::BitAnd),
        "BitOr" => Ok(BytecodeBinOp::BitOr),
        "Shl" => Ok(BytecodeBinOp::Shl),
        "Shr" => Ok(BytecodeBinOp::Shr),
        "Eq" => Ok(BytecodeBinOp::Eq),
        "Lt" => Ok(BytecodeBinOp::Lt),
        "Le" => Ok(BytecodeBinOp::Le),
        "Ne" => Ok(BytecodeBinOp::Ne),
        "Ge" => Ok(BytecodeBinOp::Ge),
        "Gt" => Ok(BytecodeBinOp::Gt),
        _ => Err(BytecodeError::Format {
            message: format!("unknown binop: {}", raw),
        }),
    }
}

fn parse_unop(raw: &str) -> Result<BytecodeUnOp, BytecodeError> {
    match raw {
        "Not" => Ok(BytecodeUnOp::Not),
        "Neg" => Ok(BytecodeUnOp::Neg),
        _ => Err(BytecodeError::Format {
            message: format!("unknown unop: {}", raw),
        }),
    }
}

fn parse_place(raw: &str) -> Result<BytecodePlace, BytecodeError> {
    let mut chars = raw.trim().chars().peekable();
    if chars.next() != Some('_') {
        return Err(BytecodeError::Format {
            message: format!("invalid place: {}", raw),
        });
    }
    let local = parse_number_token(&mut chars)?;
    let mut projection = Vec::new();
    while let Some(ch) = chars.peek().copied() {
        match ch {
            '.' => {
                chars.next();
                let field = parse_number_token(&mut chars)?;
                projection.push(BytecodePlaceElem::Field(field));
            }
            '[' => {
                chars.next();
                if chars.next() != Some('_') {
                    return Err(BytecodeError::Format {
                        message: format!("invalid index projection: {}", raw),
                    });
                }
                let index = parse_number_token(&mut chars)?;
                if chars.next() != Some(']') {
                    return Err(BytecodeError::Format {
                        message: format!("unterminated index projection: {}", raw),
                    });
                }
                projection.push(BytecodePlaceElem::Index(index));
            }
            _ => {
                return Err(BytecodeError::Format {
                    message: format!("invalid place projection: {}", raw),
                });
            }
        }
    }
    Ok(BytecodePlace { local, projection })
}

fn parse_callee(raw: &str) -> Result<BytecodeCallee, BytecodeError> {
    let raw = raw.trim();
    if let Some(rest) = raw.strip_prefix("fn ") {
        let name = rest.trim();
        if name.is_empty() {
            return Err(BytecodeError::Format {
                message: format!("invalid function callee: {}", raw),
            });
        }
        return Ok(BytecodeCallee::Function(name.to_string()));
    }
    if let Some(rest) = raw.strip_prefix("local ") {
        let place = parse_place(rest.trim())?;
        return Ok(BytecodeCallee::Local(place));
    }
    parse_callee_debug(raw)
}

fn parse_callee_debug(raw: &str) -> Result<BytecodeCallee, BytecodeError> {
    if let Some(inner) = raw
        .strip_prefix("Function(")
        .and_then(|s| s.strip_suffix(')'))
    {
        let (value, rest) = parse_debug_string(inner.trim())?;
        if !rest.trim().is_empty() {
            return Err(BytecodeError::Format {
                message: format!("invalid function callee: {}", raw),
            });
        }
        return Ok(BytecodeCallee::Function(value));
    }
    if let Some(inner) = raw.strip_prefix("Local(").and_then(|s| s.strip_suffix(')')) {
        let inner = inner.trim();
        let local_prefix = "BytecodePlace { local: ";
        let projection_prefix = ", projection: ";
        let local_start =
            inner
                .strip_prefix(local_prefix)
                .ok_or_else(|| BytecodeError::Format {
                    message: format!("invalid local callee: {}", raw),
                })?;
        let (local_part, rest) =
            local_start
                .split_once(projection_prefix)
                .ok_or_else(|| BytecodeError::Format {
                    message: format!("invalid local callee: {}", raw),
                })?;
        let local = local_part
            .trim()
            .parse::<u32>()
            .map_err(|_| BytecodeError::Format {
                message: format!("invalid local index: {}", local_part),
            })?;
        let rest = rest.trim();
        let projections = rest
            .strip_prefix('[')
            .and_then(|s| s.strip_suffix("] }"))
            .ok_or_else(|| BytecodeError::Format {
                message: format!("invalid local projection: {}", raw),
            })?;
        let mut projection = Vec::new();
        for part in split_top_level(projections) {
            let part = part.trim();
            if part.is_empty() {
                continue;
            }
            if let Some(inner) = part
                .strip_prefix("Field(")
                .and_then(|s| s.strip_suffix(')'))
            {
                let index = inner
                    .trim()
                    .parse::<u32>()
                    .map_err(|_| BytecodeError::Format {
                        message: format!("invalid field index: {}", part),
                    })?;
                projection.push(BytecodePlaceElem::Field(index));
            } else if let Some(inner) = part
                .strip_prefix("Index(")
                .and_then(|s| s.strip_suffix(')'))
            {
                let index = inner
                    .trim()
                    .parse::<u32>()
                    .map_err(|_| BytecodeError::Format {
                        message: format!("invalid index projection: {}", part),
                    })?;
                projection.push(BytecodePlaceElem::Index(index));
            } else {
                return Err(BytecodeError::Format {
                    message: format!("invalid projection element: {}", part),
                });
            }
        }
        return Ok(BytecodeCallee::Local(BytecodePlace { local, projection }));
    }

    Err(BytecodeError::Format {
        message: format!("unknown callee: {}", raw),
    })
}

fn parse_const_value(raw: &str) -> Result<BytecodeConst, BytecodeError> {
    let raw = raw.trim();
    if let Some(rest) = raw.strip_prefix("u64 ") {
        let value = rest
            .trim()
            .parse::<u64>()
            .map_err(|_| BytecodeError::Format {
                message: format!("invalid u64 constant: {}", raw),
            })?;
        return Ok(BytecodeConst::UInt(value));
    }
    if let Some(rest) = raw.strip_prefix("i64 ") {
        let value = rest
            .trim()
            .parse::<i64>()
            .map_err(|_| BytecodeError::Format {
                message: format!("invalid i64 constant: {}", raw),
            })?;
        return Ok(BytecodeConst::Int(value));
    }
    if let Some(rest) = raw.strip_prefix("f64 ") {
        let value = rest
            .trim()
            .parse::<f64>()
            .map_err(|_| BytecodeError::Format {
                message: format!("invalid f64 constant: {}", raw),
            })?;
        return Ok(BytecodeConst::Float(value));
    }
    if raw == "()" {
        return Ok(BytecodeConst::Unit);
    }
    if raw == "true" {
        return Ok(BytecodeConst::Bool(true));
    }
    if raw == "false" {
        return Ok(BytecodeConst::Bool(false));
    }
    if raw == "null" {
        return Ok(BytecodeConst::Null);
    }
    if let Some(rest) = raw.strip_prefix("fn ") {
        return Ok(BytecodeConst::Function(rest.trim().to_string()));
    }
    if raw.starts_with('"') {
        let (value, rest) = parse_debug_string(raw)?;
        if !rest.trim().is_empty() {
            return Err(BytecodeError::Format {
                message: format!("invalid string const: {}", raw),
            });
        }
        return Ok(BytecodeConst::Str(value));
    }
    if let Some(rest) = raw.strip_prefix("tuple") {
        let rest = rest.trim_start();
        let items = parse_const_list(rest)?;
        return Ok(BytecodeConst::Tuple(items));
    }
    if let Some(rest) = raw.strip_prefix("array") {
        let rest = rest.trim_start();
        let items = parse_const_list(rest)?;
        return Ok(BytecodeConst::Array(items));
    }
    if let Some(rest) = raw.strip_prefix("list") {
        let rest = rest.trim_start();
        let items = parse_const_list(rest)?;
        return Ok(BytecodeConst::List(items));
    }
    if let Some(rest) = raw.strip_prefix("map") {
        let rest = rest.trim_start();
        let entries = parse_map_entries(rest)?;
        return Ok(BytecodeConst::Map(entries));
    }
    if let Ok(value) = raw.parse::<i64>() {
        return Ok(BytecodeConst::Int(value));
    }
    if let Ok(value) = raw.parse::<u64>() {
        if value > i64::MAX as u64 {
            return Ok(BytecodeConst::UInt(value));
        }
        return Ok(BytecodeConst::Int(value as i64));
    }
    if let Ok(value) = raw.parse::<f64>() {
        return Ok(BytecodeConst::Float(value));
    }
    Err(BytecodeError::Format {
        message: format!("invalid constant: {}", raw),
    })
}

fn parse_const_list(raw: &str) -> Result<Vec<BytecodeConst>, BytecodeError> {
    let content = raw
        .trim()
        .strip_prefix('[')
        .and_then(|s| s.strip_suffix(']'))
        .ok_or_else(|| BytecodeError::Format {
            message: format!("invalid list constant: {}", raw),
        })?;
    let mut items = Vec::new();
    for entry in split_top_level(content) {
        let entry = entry.trim();
        if entry.is_empty() {
            continue;
        }
        items.push(parse_const_value(entry)?);
    }
    Ok(items)
}

fn parse_map_entries(raw: &str) -> Result<Vec<(BytecodeConst, BytecodeConst)>, BytecodeError> {
    let content = raw
        .trim()
        .strip_prefix('[')
        .and_then(|s| s.strip_suffix(']'))
        .ok_or_else(|| BytecodeError::Format {
            message: format!("invalid map constant: {}", raw),
        })?;
    let mut entries = Vec::new();
    for entry in split_top_level(content) {
        let entry = entry.trim();
        if entry.is_empty() {
            continue;
        }
        let (key, value) =
            split_once_top_level(entry, "=>").ok_or_else(|| BytecodeError::Format {
                message: format!("invalid map entry: {}", entry),
            })?;
        entries.push((
            parse_const_value(key.trim())?,
            parse_const_value(value.trim())?,
        ));
    }
    Ok(entries)
}

fn split_top_level(input: &str) -> Vec<&str> {
    let mut parts = Vec::new();
    let mut start = 0;
    let mut depth = 0usize;
    let mut in_string = false;
    let mut escape = false;
    for (idx, ch) in input.char_indices() {
        if in_string {
            if escape {
                escape = false;
            } else if ch == '\\' {
                escape = true;
            } else if ch == '"' {
                in_string = false;
            }
            continue;
        }
        match ch {
            '"' => in_string = true,
            '[' => depth += 1,
            ']' => depth = depth.saturating_sub(1),
            ',' if depth == 0 => {
                parts.push(input[start..idx].trim());
                start = idx + 1;
            }
            _ => {}
        }
    }
    if start <= input.len() {
        parts.push(input[start..].trim());
    }
    parts
}

fn split_once_top_level<'a>(input: &'a str, needle: &str) -> Option<(&'a str, &'a str)> {
    let mut depth = 0usize;
    let mut in_string = false;
    let mut escape = false;
    let bytes = input.as_bytes();
    let needle_bytes = needle.as_bytes();
    let mut i = 0;
    while i + needle_bytes.len() <= bytes.len() {
        let ch = bytes[i] as char;
        if in_string {
            if escape {
                escape = false;
            } else if ch == '\\' {
                escape = true;
            } else if ch == '"' {
                in_string = false;
            }
            i += 1;
            continue;
        }
        match ch {
            '"' => in_string = true,
            '[' => depth += 1,
            ']' => depth = depth.saturating_sub(1),
            _ => {}
        }
        if depth == 0 && bytes[i..].starts_with(needle_bytes) {
            return Some((&input[..i], &input[i + needle_bytes.len()..]));
        }
        i += 1;
    }
    None
}

fn parse_u32(raw: &str) -> Result<u32, BytecodeError> {
    raw.trim()
        .parse::<u32>()
        .map_err(|_| BytecodeError::Format {
            message: format!("invalid number: {}", raw),
        })
}

fn parse_number_token(
    chars: &mut std::iter::Peekable<std::str::Chars<'_>>,
) -> Result<u32, BytecodeError> {
    let mut digits = String::new();
    while let Some(ch) = chars.peek().copied() {
        if ch.is_ascii_digit() {
            digits.push(ch);
            chars.next();
        } else {
            break;
        }
    }
    if digits.is_empty() {
        return Err(BytecodeError::Format {
            message: "missing number".to_string(),
        });
    }
    digits.parse::<u32>().map_err(|_| BytecodeError::Format {
        message: format!("invalid number: {}", digits),
    })
}

fn parse_debug_string(raw: &str) -> Result<(String, &str), BytecodeError> {
    let mut chars = raw.char_indices().peekable();
    match chars.next() {
        Some((_, '"')) => {}
        _ => {
            return Err(BytecodeError::Format {
                message: format!("expected string literal: {}", raw),
            });
        }
    }
    let mut output = String::new();
    while let Some((idx, ch)) = chars.next() {
        match ch {
            '"' => {
                let rest = &raw[idx + 1..];
                return Ok((output, rest));
            }
            '\\' => {
                let Some((_, escaped)) = chars.next() else {
                    return Err(BytecodeError::Format {
                        message: "unterminated escape sequence".to_string(),
                    });
                };
                match escaped {
                    '\\' => output.push('\\'),
                    '"' => output.push('"'),
                    'n' => output.push('\n'),
                    'r' => output.push('\r'),
                    't' => output.push('\t'),
                    '0' => output.push('\0'),
                    'u' => {
                        let Some((_, '{')) = chars.next() else {
                            return Err(BytecodeError::Format {
                                message: "invalid unicode escape".to_string(),
                            });
                        };
                        let mut hex = String::new();
                        while let Some((_, ch)) = chars.next() {
                            if ch == '}' {
                                break;
                            }
                            hex.push(ch);
                        }
                        let value =
                            u32::from_str_radix(&hex, 16).map_err(|_| BytecodeError::Format {
                                message: format!("invalid unicode escape: {}", hex),
                            })?;
                        if let Some(ch) = char::from_u32(value) {
                            output.push(ch);
                        } else {
                            return Err(BytecodeError::Format {
                                message: format!("invalid unicode scalar: {}", hex),
                            });
                        }
                    }
                    other => {
                        return Err(BytecodeError::Format {
                            message: format!("unsupported escape: \\{}", other),
                        });
                    }
                }
            }
            other => output.push(other),
        }
    }
    Err(BytecodeError::Format {
        message: "unterminated string literal".to_string(),
    })
}
