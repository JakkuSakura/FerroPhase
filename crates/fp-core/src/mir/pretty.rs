use std::fmt::{self, Formatter};

use crate::pretty::{PrettyCtx, PrettyPrintable};

use super::{
    AggregateKind, BasicBlockData, Body, BodyId, Constant, Function, Item, ItemKind, Operand,
    Place, Program, Rvalue, Statement, StatementKind, Static, Terminator, TerminatorKind,
};

impl PrettyPrintable for Program {
    fn fmt_pretty(&self, f: &mut Formatter<'_>, ctx: &mut PrettyCtx<'_>) -> fmt::Result {
        ctx.writeln(f, "mir::Program {")?;
        ctx.with_indent(|ctx| {
            if !self.items.is_empty() {
                ctx.writeln(f, "items:")?;
                ctx.with_indent(|ctx| {
                    for (idx, item) in self.items.iter().enumerate() {
                        write_item(item, f, ctx)?;
                        if idx + 1 < self.items.len() {
                            writeln!(f)?;
                        }
                    }
                    Ok(())
                })?;
            }

            if !self.bodies.is_empty() {
                ctx.writeln(f, "bodies:")?;
                let mut ids: Vec<_> = self.bodies.keys().copied().collect();
                ids.sort_by_key(|id| id.0);
                ctx.with_indent(|ctx| {
                    for id in &ids {
                        if let Some(body) = self.bodies.get(id) {
                            write_body(*id, body, f, ctx)?;
                        }
                    }
                    Ok(())
                })?;
            }
            Ok(())
        })?;
        ctx.writeln(f, "}")
    }
}

fn write_item(item: &Item, f: &mut Formatter<'_>, ctx: &mut PrettyCtx<'_>) -> fmt::Result {
    match &item.kind {
        ItemKind::Function(func) => write_function(func, f, ctx),
        ItemKind::Static(stat) => write_static(stat, f, ctx),
    }
}

fn write_function(func: &Function, f: &mut Formatter<'_>, ctx: &mut PrettyCtx<'_>) -> fmt::Result {
    let path = if func.path.is_empty() {
        String::from(func.name.clone())
    } else {
        format!(
            "{}::{}",
            func.path
                .iter()
                .map(|s| s.as_str())
                .collect::<Vec<_>>()
                .join("::"),
            func.name
        )
    };
    let params = func
        .sig
        .inputs
        .iter()
        .enumerate()
        .map(|(idx, ty)| format!("arg{}: {}", idx, ty))
        .collect::<Vec<_>>()
        .join(", ");
    let ret = format!(" -> {}", func.sig.output);
    let body_ref = format_body_id(func.body_id);

    ctx.writeln(
        f,
        format!("fn {}({}){} [body: {}]", path, params, ret, body_ref),
    )
}

fn write_static(stat: &Static, f: &mut Formatter<'_>, ctx: &mut PrettyCtx<'_>) -> fmt::Result {
    let mutability = match stat.mutability {
        super::Mutability::Mut => "mut ",
        super::Mutability::Not => "",
    };
    let name = "<static>";
    ctx.writeln(
        f,
        format!(
            "static {}{}: {} = {}",
            mutability,
            name,
            stat.ty,
            summarize_operand(&stat.init)
        ),
    )
}

fn write_body(
    body_id: BodyId,
    body: &Body,
    f: &mut Formatter<'_>,
    ctx: &mut PrettyCtx<'_>,
) -> fmt::Result {
    let header = format!(
        "{} {{ arg_count: {}, locals: {} }}",
        format_body_id(body_id),
        body.arg_count,
        body.locals.len()
    );
    let span_suffix = if ctx.options.show_spans {
        format!(" // span: {:?}", body.span)
    } else {
        String::new()
    };

    ctx.writeln(f, header + &span_suffix)?;
    ctx.with_indent(|ctx| {
        ctx.writeln(f, format!("return_local: _{}", body.return_local))?;

        if !body.locals.is_empty() {
            ctx.writeln(f, "locals:")?;
            ctx.with_indent(|ctx| {
                for (idx, local) in body.locals.iter().enumerate() {
                    let mut line = format!("_{}: {}", idx, local.ty);
                    if local.internal {
                        line.push_str(" (internal)");
                    }
                    if ctx.options.show_spans {
                        line.push_str(&format!(" // span: {:?}", local.source_info));
                    }
                    ctx.writeln(f, line)?;
                }
                Ok(())
            })?;
        }

        if !body.basic_blocks.is_empty() {
            ctx.writeln(f, "blocks:")?;
            ctx.with_indent(|ctx| {
                for (idx, block) in body.basic_blocks.iter().enumerate() {
                    write_block(idx, block, f, ctx)?;
                }
                Ok(())
            })?;
        }

        Ok(())
    })
}

fn write_block(
    index: usize,
    block: &BasicBlockData,
    f: &mut Formatter<'_>,
    ctx: &mut PrettyCtx<'_>,
) -> fmt::Result {
    let cleanup = if block.is_cleanup { " (cleanup)" } else { "" };
    ctx.writeln(f, format!("bb{}{}:", index, cleanup))?;
    ctx.with_indent(|ctx| {
        for (stmt_idx, stmt) in block.statements.iter().enumerate() {
            let mut line = format!("s{}: {}", stmt_idx, summarize_statement(stmt, ctx));
            if ctx.options.show_spans {
                line.push_str(&format!(" // span: {:?}", stmt.source_info));
            }
            ctx.writeln(f, line)?;
        }
        if let Some(term) = &block.terminator {
            let mut line = format!("terminator: {}", summarize_terminator(term));
            if ctx.options.show_spans {
                line.push_str(&format!(" // span: {:?}", term.source_info));
            }
            ctx.writeln(f, line)?;
        } else {
            ctx.writeln(f, "terminator: <none>")?;
        }
        Ok(())
    })
}

fn summarize_statement(stmt: &Statement, _ctx: &PrettyCtx<'_>) -> String {
    match &stmt.kind {
        StatementKind::Assign(place, rvalue) => {
            format!("{} = {}", format_place(place), summarize_rvalue(rvalue))
        }
        StatementKind::IntrinsicCall { kind, format, args } => {
            let args = args
                .iter()
                .map(summarize_operand)
                .collect::<Vec<_>>()
                .join(", ");
            format!("intrinsic {:?} \"{}\" [{}]", kind, format, args)
        }
        StatementKind::SetDiscriminant {
            place,
            variant_index,
        } => {
            format!(
                "set_discriminant({}, {})",
                format_place(place),
                variant_index
            )
        }
        StatementKind::StorageLive(local) => format!("storage_live(_{})", local),
        StatementKind::StorageDead(local) => format!("storage_dead(_{})", local),
        StatementKind::Retag(kind, place) => format!("retag({:?}, {})", kind, format_place(place)),
        StatementKind::AscribeUserType(place, _, _) => {
            format!("ascribe_user_type({})", format_place(place))
        }
        StatementKind::Nop => "nop".into(),
    }
}

fn summarize_rvalue(rvalue: &Rvalue) -> String {
    match rvalue {
        Rvalue::Use(op) => summarize_operand(op),
        Rvalue::Repeat(op, len) => format!("repeat({}, {})", summarize_operand(op), len),
        Rvalue::Ref(_, kind, place) => format!("ref({:?}, {})", kind, format_place(place)),
        Rvalue::AddressOf(mutability, place) => {
            format!("address_of({:?}, {})", mutability, format_place(place))
        }
        Rvalue::Len(place) => format!("len({})", format_place(place)),
        Rvalue::Cast(kind, op, ty) => {
            format!("cast({:?}, {}, {})", kind, summarize_operand(op), ty)
        }
        Rvalue::BinaryOp(op, left, right) => format!(
            "{:?}({}, {})",
            op,
            summarize_operand(left),
            summarize_operand(right)
        ),
        Rvalue::CheckedBinaryOp(op, left, right) => format!(
            "checked_{:?}({}, {})",
            op,
            summarize_operand(left),
            summarize_operand(right)
        ),
        Rvalue::UnaryOp(op, operand) => format!("{:?}({})", op, summarize_operand(operand)),
        Rvalue::NullaryOp(op, ty) => format!("{:?} {}", op, ty),
        Rvalue::Aggregate(kind, operands) => {
            let kind_desc = summarize_aggregate_kind(kind);
            let ops = operands
                .iter()
                .map(summarize_operand)
                .collect::<Vec<_>>()
                .join(", ");
            format!("aggregate({}; [{}])", kind_desc, ops)
        }
        Rvalue::ContainerLiteral { kind, elements } => {
            let elements = elements
                .iter()
                .map(summarize_operand)
                .collect::<Vec<_>>()
                .join(", ");
            format!("container_literal({:?}; [{}])", kind, elements)
        }
        Rvalue::ContainerMapLiteral { kind, entries } => {
            let entries = entries
                .iter()
                .map(|(key, value)| format!("({}, {})", summarize_operand(key), summarize_operand(value)))
                .collect::<Vec<_>>()
                .join(", ");
            format!("container_map_literal({:?}; [{}])", kind, entries)
        }
        Rvalue::ContainerLen { kind, container } => {
            format!("container_len({:?}; {})", kind, summarize_operand(container))
        }
        Rvalue::ContainerGet {
            kind,
            container,
            key,
        } => format!(
            "container_get({:?}; {}, {})",
            kind,
            summarize_operand(container),
            summarize_operand(key)
        ),
        Rvalue::ThreadLocalRef(def_id) => format!("thread_local_ref({:?})", def_id),
        Rvalue::Discriminant(place) => format!("discriminant({})", format_place(place)),
        Rvalue::ShallowInitBox(op, ty) => format!("box({}, {})", summarize_operand(op), ty),
    }
}

fn summarize_aggregate_kind(kind: &AggregateKind) -> String {
    match kind {
        AggregateKind::Array(ty) => format!("array [{}]", ty),
        AggregateKind::Tuple => "tuple".into(),
        AggregateKind::Adt(def, variant, _, _) => {
            format!("adt({:?}::{:?})", def, variant)
        }
        AggregateKind::Closure(def, _) => format!("closure({:?})", def),
        AggregateKind::Generator(def, _, _) => format!("generator({:?})", def),
    }
}

fn summarize_operand(op: &Operand) -> String {
    match op {
        Operand::Copy(place) => format!("copy {}", format_place(place)),
        Operand::Move(place) => format!("move {}", format_place(place)),
        Operand::Constant(constant) => summarize_constant(constant),
    }
}

fn summarize_constant(constant: &Constant) -> String {
    use super::ConstantKind;

    match &constant.literal {
        ConstantKind::Int(value) => value.to_string(),
        ConstantKind::UInt(value) => format!("{}u", value),
        ConstantKind::Float(value) => format!("{}", value),
        ConstantKind::Bool(value) => value.to_string(),
        ConstantKind::Str(value) => format!("\"{}\"", escape_str(value)),
        ConstantKind::Fn(name, _) => format!("fn {}", name),
        ConstantKind::Global(name, _) => format!("global {}", name),
        ConstantKind::Ty(_) => "<type>".into(),
        ConstantKind::Val(_, ty) => format!("const <{}>", ty),
        ConstantKind::Null => "null".into(),
    }
}

fn escape_str(input: &str) -> String {
    let mut out = String::with_capacity(input.len());
    for ch in input.chars() {
        match ch {
            '"' => {
                out.push('\\');
                out.push('"');
            }
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            ch if ch.is_control() => {
                use std::fmt::Write as _;
                let _ = write!(out, "\\u{{{:x}}}", ch as u32);
            }
            _ => out.push(ch),
        }
    }
    out
}

fn summarize_terminator(term: &Terminator) -> String {
    match &term.kind {
        TerminatorKind::Goto { target } => format!("goto -> bb{}", target),
        TerminatorKind::SwitchInt { discr, targets, .. } => {
            let values = targets
                .values
                .iter()
                .zip(&targets.targets)
                .map(|(value, block)| format!("{} -> bb{}", value, block))
                .collect::<Vec<_>>()
                .join(", ");
            format!(
                "switch {} [{}] otherwise -> bb{}",
                summarize_operand(discr),
                values,
                targets.otherwise
            )
        }
        TerminatorKind::Return => "return".into(),
        TerminatorKind::Resume => "resume".into(),
        TerminatorKind::Abort => "abort".into(),
        TerminatorKind::Unreachable => "unreachable".into(),
        TerminatorKind::Drop {
            place,
            target,
            unwind,
        } => {
            let unwind = unwind
                .map(|u| format!(", unwind -> bb{}", u))
                .unwrap_or_default();
            format!("drop {} -> bb{}{}", format_place(place), target, unwind)
        }
        TerminatorKind::DropAndReplace {
            place,
            value,
            target,
            unwind,
        } => {
            let unwind = unwind
                .map(|u| format!(", unwind -> bb{}", u))
                .unwrap_or_default();
            format!(
                "drop_replace {} = {}; goto bb{}{}",
                format_place(place),
                summarize_operand(value),
                target,
                unwind
            )
        }
        TerminatorKind::Call {
            func,
            args,
            destination,
            cleanup,
            ..
        } => {
            let args = args
                .iter()
                .map(summarize_operand)
                .collect::<Vec<_>>()
                .join(", ");
            let dest = destination
                .as_ref()
                .map(|(place, target)| format!(" -> {} then bb{}", format_place(place), target))
                .unwrap_or_else(|| "".into());
            let cleanup = cleanup
                .map(|c| format!(", cleanup -> bb{}", c))
                .unwrap_or_default();
            format!(
                "call {}({}){}{}",
                summarize_operand(func),
                args,
                dest,
                cleanup
            )
        }
        TerminatorKind::Assert {
            cond,
            expected,
            target,
            cleanup,
            ..
        } => {
            let cleanup = cleanup
                .map(|c| format!(", cleanup -> bb{}", c))
                .unwrap_or_default();
            format!(
                "assert {} == {} -> bb{}{}",
                summarize_operand(cond),
                expected,
                target,
                cleanup
            )
        }
        TerminatorKind::FalseEdge {
            real_target,
            imaginary_target,
        } => {
            format!(
                "false_edge real -> bb{}, imaginary -> bb{}",
                real_target, imaginary_target
            )
        }
        TerminatorKind::FalseUnwind {
            real_target,
            unwind,
        } => {
            let unwind = unwind
                .map(|u| format!(", unwind -> bb{}", u))
                .unwrap_or_default();
            format!("false_unwind -> bb{}{}", real_target, unwind)
        }
        TerminatorKind::Yield {
            value,
            resume,
            resume_arg,
            drop,
        } => {
            let drop = drop
                .map(|d| format!(", drop -> bb{}", d))
                .unwrap_or_default();
            format!(
                "yield {} resume -> bb{} ({}){}",
                summarize_operand(value),
                resume,
                format_place(resume_arg),
                drop
            )
        }
        TerminatorKind::GeneratorDrop => "generator_drop".into(),
        TerminatorKind::InlineAsm {
            template,
            destination,
            ..
        } => {
            let dest = destination
                .map(|d| format!(" -> bb{}", d))
                .unwrap_or_default();
            format!("inline_asm({} pieces){}", template.len(), dest)
        }
    }
}

fn format_place(place: &Place) -> String {
    if place.projection.is_empty() {
        format!("_{}", place.local)
    } else {
        format!("_{}{:?}", place.local, place.projection)
    }
}

fn format_body_id(id: BodyId) -> String {
    format!("body{}", id.0)
}
