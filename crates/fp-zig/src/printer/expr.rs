use fp_core::ast::{
    Expr, ExprAssign, ExprBinOp, ExprField, ExprIntrinsicCall, ExprInvoke, ExprInvokeTarget,
    ExprKind, ExprStruct, ExprUnOp, FormatTemplatePart, Locator, Value,
};
use fp_core::intrinsics::IntrinsicCallPayload;

use super::{utils::escape_zig_string, ZigEmitter};

impl ZigEmitter {
    pub(crate) fn render_expr(&self, expr: &Expr) -> Option<String> {
        match expr.kind() {
            ExprKind::Value(value) => self.render_value(value),
            ExprKind::Locator(locator) => Some(self.render_locator(locator)),
            ExprKind::Select(select) => {
                let target = self.render_expr(select.obj.as_ref())?;
                Some(format!("{}.{}", target, select.field.name))
            }
            ExprKind::BinOp(bin) => self.render_bin_op(bin),
            ExprKind::UnOp(un) => self.render_un_op(un),
            ExprKind::Assign(assign) => self.render_assign(assign),
            ExprKind::Struct(struct_expr) => self.render_struct(struct_expr),
            ExprKind::Invoke(invoke) => self.render_invoke(invoke),
            ExprKind::IntrinsicCall(call) => self.render_intrinsic_expr(call),
            _ => None,
        }
    }

    fn render_locator(&self, locator: &Locator) -> String {
        match locator {
            Locator::Ident(ident) => ident.name.clone(),
            _ => format!("{}", locator).replace("::", "."),
        }
    }

    fn render_invoke(&self, invoke: &ExprInvoke) -> Option<String> {
        let target = match &invoke.target {
            ExprInvokeTarget::Function(locator) => self.render_locator(locator),
            ExprInvokeTarget::Expr(expr) => self.render_expr(expr.as_ref())?,
            ExprInvokeTarget::Method(select) => {
                let receiver = self.render_expr(select.obj.as_ref())?;
                format!("{}.{}", receiver, select.field.name)
            }
            _ => return None,
        };

        let mut args = Vec::new();
        for arg in &invoke.args {
            args.push(self.render_expr(arg)?);
        }

        Some(format!("{}({})", target, args.join(", ")))
    }

    fn render_intrinsic_expr(&self, _call: &ExprIntrinsicCall) -> Option<String> {
        None
    }

    fn render_bin_op(&self, bin: &ExprBinOp) -> Option<String> {
        let lhs = self.render_expr(bin.lhs.as_ref())?;
        let rhs = self.render_expr(bin.rhs.as_ref())?;
        Some(format!("({} {} {})", lhs, bin.kind, rhs))
    }

    fn render_un_op(&self, un: &ExprUnOp) -> Option<String> {
        let value = self.render_expr(un.val.as_ref())?;
        Some(format!("({}{})", un.op, value))
    }

    fn render_assign(&self, assign: &ExprAssign) -> Option<String> {
        let target = self.render_expr(assign.target.as_ref())?;
        let value = self.render_expr(assign.value.as_ref())?;
        Some(format!("{} = {}", target, value))
    }

    fn render_struct(&self, struct_expr: &ExprStruct) -> Option<String> {
        let type_name = self.render_expr(struct_expr.name.as_ref())?;
        let mut fields = Vec::new();
        for field in &struct_expr.fields {
            fields.push(self.render_struct_field(field)?);
        }
        if fields.is_empty() {
            Some(format!("{}{{}}", type_name))
        } else {
            Some(format!("{}{{ {} }}", type_name, fields.join(", ")))
        }
    }

    pub(super) fn render_print_call(
        &self,
        call: &ExprIntrinsicCall,
        newline: bool,
    ) -> Option<String> {
        let (mut format_string, args) = self.render_print_payload(call)?;
        if newline && !format_string.ends_with('\n') {
            format_string.push('\n');
        }
        let literal = format!("\"{}\"", escape_zig_string(&format_string));
        let arg_tuple = if args.is_empty() {
            ".{}".to_string()
        } else {
            format!(".{{ {} }}", args.join(", "))
        };
        Some(format!("std.debug.print({}, {});", literal, arg_tuple))
    }

    fn render_print_payload(&self, call: &ExprIntrinsicCall) -> Option<(String, Vec<String>)> {
        match &call.payload {
            IntrinsicCallPayload::Format { template } => {
                if !template.kwargs.is_empty() {
                    return None;
                }
                let mut fmt = String::new();
                for part in &template.parts {
                    match part {
                        FormatTemplatePart::Literal(literal) => fmt.push_str(literal),
                        FormatTemplatePart::Placeholder(_) => fmt.push_str("{any}"),
                    }
                }
                let mut args = Vec::new();
                for expr in &template.args {
                    args.push(self.render_expr(expr)?);
                }
                Some((fmt, args))
            }
            IntrinsicCallPayload::Args { args } => {
                let mut rendered_args = Vec::new();
                for expr in args {
                    rendered_args.push(self.render_expr(expr)?);
                }

                if args.len() == 1 {
                    if let ExprKind::Value(value) = args[0].kind() {
                        if let Value::String(str_val) = value.as_ref() {
                            return Some((str_val.value.clone(), Vec::new()));
                        }
                    }
                }

                let fmt = if rendered_args.is_empty() {
                    String::new()
                } else {
                    std::iter::repeat("{any}")
                        .take(rendered_args.len())
                        .collect::<Vec<_>>()
                        .join(" ")
                };

                Some((fmt, rendered_args))
            }
        }
    }

    fn render_struct_field(&self, field: &ExprField) -> Option<String> {
        match field.value.as_ref() {
            Some(expr) => {
                let rendered = self.render_expr(expr)?;
                Some(format!(".{} = {}", field.name.name, rendered))
            }
            None => Some(format!(".{}", field.name.name)),
        }
    }
}
