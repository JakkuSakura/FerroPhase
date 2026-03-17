use fp_shell_core::{
    ArithmeticOp, Block, BoolExpr, ComparisonOp, ConditionExpr, FunctionDef, HostExpr,
    OperationCopy, OperationGuards, OperationRsync, OperationRun, OperationTemplate,
    RsyncOptions, ScriptItem, ScriptProgram, ScriptRenderer, ShellInventory, Statement,
    StringExpr, StringPart, TransportKind, ValueExpr,
};

pub struct BashTarget;

impl BashTarget {
    pub fn new() -> Self {
        Self
    }
}

impl Default for BashTarget {
    fn default() -> Self {
        Self::new()
    }
}

impl ScriptRenderer for BashTarget {
    type Error = String;

    fn render(
        &self,
        program: &ScriptProgram,
        inventory: &ShellInventory,
    ) -> Result<String, Self::Error> {
        let mut renderer = BashRenderer::new(inventory);
        renderer.render_program(program)?;
        Ok(renderer.finish())
    }
}

struct BashRenderer<'a> {
    inventory: &'a ShellInventory,
    lines: Vec<String>,
}

impl<'a> BashRenderer<'a> {
    fn new(inventory: &'a ShellInventory) -> Self {
        Self {
            inventory,
            lines: Vec::new(),
        }
    }

    fn finish(self) -> String {
        let mut script = String::new();
        script.push_str("#!/usr/bin/env bash\n");
        script.push_str("set -euo pipefail\n\n");
        script.push_str("__fp_last_changed=0\n\n");
        script.push_str("declare -A FP_HOST_TRANSPORT=()\n");
        script.push_str("declare -A FP_SSH_ADDRESS=()\n");
        script.push_str("declare -A FP_SSH_USER=()\n");
        script.push_str("declare -A FP_SSH_PORT=()\n");
        script.push_str("declare -A FP_DOCKER_CONTAINER=()\n");
        script.push_str("declare -A FP_DOCKER_USER=()\n");
        script.push_str("declare -A FP_K8S_POD=()\n");
        script.push_str("declare -A FP_K8S_NAMESPACE=()\n");
        script.push_str("declare -A FP_K8S_CONTAINER=()\n");
        script.push_str("declare -A FP_K8S_CONTEXT=()\n");
        script.push_str("declare -A FP_WINRM_ADDRESS=()\n");
        script.push_str("declare -A FP_WINRM_USER=()\n");
        script.push_str("declare -A FP_WINRM_PASSWORD=()\n");
        script.push_str("declare -A FP_WINRM_PORT=()\n");
        script.push_str("declare -A FP_WINRM_SCHEME=()\n\n");
        for (name, host) in &self.inventory.hosts {
            script.push_str(&format!("FP_HOST_TRANSPORT[{}]={}\n", shell_arg_quote(name), shell_arg_quote(match host.transport {
                TransportKind::Local => "local",
                TransportKind::Ssh => "ssh",
                TransportKind::Docker => "docker",
                TransportKind::Kubectl => "kubectl",
                TransportKind::Winrm => "winrm",
            })));
            if let Some(ssh) = &host.ssh {
                if let Some(address) = &ssh.address {
                    script.push_str(&format!("FP_SSH_ADDRESS[{}]={}\n", shell_arg_quote(name), shell_arg_quote(address)));
                }
                if let Some(user) = &ssh.user {
                    script.push_str(&format!("FP_SSH_USER[{}]={}\n", shell_arg_quote(name), shell_arg_quote(user)));
                }
                if let Some(port) = ssh.port {
                    script.push_str(&format!("FP_SSH_PORT[{}]={}\n", shell_arg_quote(name), shell_arg_quote(&port.to_string())));
                }
            }
            if let Some(docker) = &host.docker {
                script.push_str(&format!("FP_DOCKER_CONTAINER[{}]={}\n", shell_arg_quote(name), shell_arg_quote(&docker.container)));
                if let Some(user) = &docker.user {
                    script.push_str(&format!("FP_DOCKER_USER[{}]={}\n", shell_arg_quote(name), shell_arg_quote(user)));
                }
            }
            if let Some(kubectl) = &host.kubectl {
                script.push_str(&format!("FP_K8S_POD[{}]={}\n", shell_arg_quote(name), shell_arg_quote(&kubectl.pod)));
                if let Some(namespace) = &kubectl.namespace {
                    script.push_str(&format!("FP_K8S_NAMESPACE[{}]={}\n", shell_arg_quote(name), shell_arg_quote(namespace)));
                }
                if let Some(container) = &kubectl.container {
                    script.push_str(&format!("FP_K8S_CONTAINER[{}]={}\n", shell_arg_quote(name), shell_arg_quote(container)));
                }
                if let Some(context) = &kubectl.context {
                    script.push_str(&format!("FP_K8S_CONTEXT[{}]={}\n", shell_arg_quote(name), shell_arg_quote(context)));
                }
            }
            if let Some(winrm) = &host.winrm {
                script.push_str(&format!("FP_WINRM_ADDRESS[{}]={}\n", shell_arg_quote(name), shell_arg_quote(&winrm.address)));
                script.push_str(&format!("FP_WINRM_USER[{}]={}\n", shell_arg_quote(name), shell_arg_quote(&winrm.user)));
                if let Some(password) = &winrm.password {
                    script.push_str(&format!("FP_WINRM_PASSWORD[{}]={}\n", shell_arg_quote(name), shell_arg_quote(password)));
                }
                if let Some(port) = winrm.port {
                    script.push_str(&format!("FP_WINRM_PORT[{}]={}\n", shell_arg_quote(name), shell_arg_quote(&port.to_string())));
                }
                if let Some(scheme) = &winrm.scheme {
                    script.push_str(&format!("FP_WINRM_SCHEME[{}]={}\n", shell_arg_quote(name), shell_arg_quote(scheme)));
                }
            }
        }
        script.push_str("\nSSH_CONTROL_PATH=\"${TMPDIR:-/tmp}/fp-shell-%r@%h:%p\"\n\n");
        script.push_str(BASH_HELPERS);
        for line in self.lines {
            script.push_str(&line);
            script.push('\n');
        }
        script
    }

    fn render_program(&mut self, program: &ScriptProgram) -> Result<(), String> {
        for item in &program.items {
            match item {
                ScriptItem::Function(def) => self.render_function(def)?,
                ScriptItem::Statement(statement) => self.render_statement(statement, 0)?,
            }
        }
        Ok(())
    }

    fn render_function(&mut self, def: &FunctionDef) -> Result<(), String> {
        self.push_line(0, &format!("{}() {{", def.name));
        for (index, param) in def.params.iter().enumerate() {
            self.push_line(1, &format!("local {}=\"${}\"", param, index + 1));
        }
        self.render_block(&def.body, 1)?;
        self.push_line(0, "}");
        self.push_line(0, "");
        Ok(())
    }

    fn render_block(&mut self, block: &Block, indent: usize) -> Result<(), String> {
        for statement in &block.statements {
            self.render_statement(statement, indent)?;
        }
        Ok(())
    }

    fn render_statement(&mut self, statement: &Statement, indent: usize) -> Result<(), String> {
        match statement {
            Statement::Run(op) => self.render_run(op, indent),
            Statement::Copy(op) => self.render_copy(op, indent),
            Statement::Template(op) => self.render_template(op, indent),
            Statement::Rsync(op) => self.render_rsync(op, indent),
            Statement::If(stmt) => {
                self.push_line(indent, &format!("if {}; then", self.render_condition(&stmt.condition)?));
                self.render_block(&stmt.then_block, indent + 1)?;
                if let Some(else_block) = &stmt.else_block {
                    self.push_line(indent, "else");
                    self.render_block(else_block, indent + 1)?;
                }
                self.push_line(indent, "fi");
                Ok(())
            }
            Statement::While(stmt) => {
                self.push_line(indent, &format!("while {}; do", self.render_condition(&stmt.condition)?));
                self.render_block(&stmt.body, indent + 1)?;
                self.push_line(indent, "done");
                Ok(())
            }
            Statement::ForEach(stmt) => {
                let values = stmt.values.iter().map(|value| self.render_word(value)).collect::<Vec<_>>().join(" ");
                self.push_line(indent, &format!("for {} in {}; do", stmt.binding, values));
                self.render_block(&stmt.body, indent + 1)?;
                self.push_line(indent, "done");
                Ok(())
            }
            Statement::Let(stmt) => {
                let value = self.render_value(&stmt.value)?;
                self.push_line(indent, &format!("local {}={}", stmt.name, value));
                Ok(())
            }
            Statement::Invoke(stmt) => {
                let args = stmt.args.iter().map(|arg| self.render_value(arg)).collect::<Result<Vec<_>, _>>()?.join(" ");
                self.push_line(indent, &format!("{} {}", stmt.name, args));
                Ok(())
            }
        }
    }

    fn render_run(&mut self, op: &OperationRun, indent: usize) -> Result<(), String> {
        for host in &op.hosts {
            self.render_guarded(indent, &op.guards, |renderer, guard_indent| {
                match host {
                    HostExpr::Localhost => renderer.push_line(guard_indent, &renderer.render_command_expr(&op.command)),
                    HostExpr::Selector(selector) => renderer.push_line(
                        guard_indent,
                        &format!("run_host {} {}", renderer.render_word(selector), shell_arg_quote(&renderer.render_command_expr(&op.command))),
                    ),
                }
                renderer.push_line(guard_indent, "__fp_changed=1");
                Ok(())
            })?;
        }
        Ok(())
    }

    fn render_copy(&mut self, op: &OperationCopy, indent: usize) -> Result<(), String> {
        for host in &op.hosts {
            self.render_guarded(indent, &op.guards, |renderer, guard_indent| {
                match host {
                    HostExpr::Localhost => renderer.push_line(
                        guard_indent,
                        &format!("cp -- {} {}", renderer.render_word(&op.source), renderer.render_word(&op.destination)),
                    ),
                    HostExpr::Selector(selector) => renderer.push_line(
                        guard_indent,
                        &format!("copy_host {} {} {}", renderer.render_word(selector), renderer.render_word(&op.source), renderer.render_word(&op.destination)),
                    ),
                }
                renderer.push_line(guard_indent, "__fp_changed=1");
                Ok(())
            })?;
        }
        Ok(())
    }

    fn render_template(&mut self, op: &OperationTemplate, indent: usize) -> Result<(), String> {
        for host in &op.hosts {
            self.render_guarded(indent, &op.guards, |renderer, guard_indent| {
                let mut vars = String::new();
                for (key, value) in &op.vars {
                    vars.push_str(&format!("{}={} ", key, renderer.render_word(value)));
                }
                match host {
                    HostExpr::Localhost => renderer.push_line(
                        guard_indent,
                        &format!("{}envsubst < {} > {}", vars, renderer.render_word(&op.source), renderer.render_word(&op.destination)),
                    ),
                    HostExpr::Selector(selector) => renderer.push_line(
                        guard_indent,
                        &format!("template_host {} {} {} {}", renderer.render_word(selector), renderer.render_word(&op.source), renderer.render_word(&op.destination), shell_arg_quote(vars.trim())),
                    ),
                }
                renderer.push_line(guard_indent, "__fp_changed=1");
                Ok(())
            })?;
        }
        Ok(())
    }

    fn render_rsync(&mut self, op: &OperationRsync, indent: usize) -> Result<(), String> {
        let flags = rsync_flags(op.options);
        for host in &op.hosts {
            self.render_guarded(indent, &op.guards, |renderer, guard_indent| {
                match host {
                    HostExpr::Localhost => renderer.push_line(
                        guard_indent,
                        &format!("rsync {} -- {} {}", flags, renderer.render_word(&op.source), renderer.render_word(&op.destination)),
                    ),
                    HostExpr::Selector(selector) => renderer.push_line(
                        guard_indent,
                        &format!("rsync_host {} {} {} {}", renderer.render_word(selector), shell_arg_quote(&flags), renderer.render_word(&op.source), renderer.render_word(&op.destination)),
                    ),
                }
                renderer.push_line(guard_indent, "__fp_changed=1");
                Ok(())
            })?;
        }
        Ok(())
    }

    fn render_guarded<F>(&mut self, indent: usize, guards: &OperationGuards, emit: F) -> Result<(), String>
    where
        F: FnOnce(&mut Self, usize) -> Result<(), String>,
    {
        self.push_line(indent, "__fp_changed=0");
        let mut condition_parts = Vec::new();
        if let Some(command) = &guards.only_if {
            condition_parts.push(format!("({})", self.render_command_expr(command)));
        }
        if let Some(command) = &guards.unless {
            condition_parts.push(format!("! ({})", self.render_command_expr(command)));
        }
        if let Some(path) = &guards.creates {
            condition_parts.push(format!("[ ! -e {} ]", self.render_word(path)));
        }
        if let Some(path) = &guards.removes {
            condition_parts.push(format!("[ -e {} ]", self.render_word(path)));
        }
        if condition_parts.is_empty() {
            emit(self, indent)?;
        } else {
            self.push_line(indent, &format!("if {}; then", condition_parts.join(" && ")));
            emit(self, indent + 1)?;
            self.push_line(indent, "fi");
        }
        self.push_line(indent, "__fp_last_changed=\"$__fp_changed\"");
        Ok(())
    }

    fn render_condition(&self, condition: &ConditionExpr) -> Result<String, String> {
        Ok(match condition {
            ConditionExpr::Bool(BoolExpr::Literal(true)) => "true".to_string(),
            ConditionExpr::Bool(BoolExpr::Literal(false)) => "false".to_string(),
            ConditionExpr::Bool(BoolExpr::Variable(name)) => format!("[[ \"${{{}}}\" == 'true' ]]", name),
            ConditionExpr::Bool(BoolExpr::IntComparison { lhs, op, rhs }) => {
                format!("[[ {} {} {} ]]", self.render_int(lhs), render_comparison(*op), self.render_int(rhs))
            }
            ConditionExpr::Command(command) => self.render_command_expr(command),
            ConditionExpr::StringTruthy(expr) => format!("[[ {} == 'true' ]]", self.render_word(expr)),
        })
    }

    fn render_value(&self, value: &ValueExpr) -> Result<String, String> {
        Ok(match value {
            ValueExpr::String(expr) => self.render_word(expr),
            ValueExpr::Int(expr) => self.render_int(expr),
            ValueExpr::Bool(BoolExpr::Literal(value)) => shell_arg_quote(&value.to_string()),
            ValueExpr::Bool(BoolExpr::Variable(name)) => format!("\"${{{}}}\"", name),
            ValueExpr::Bool(BoolExpr::IntComparison { lhs, op, rhs }) => {
                shell_arg_quote(&format!("{} {} {}", self.render_int(lhs), render_comparison(*op), self.render_int(rhs)))
            }
            ValueExpr::StringList(values) => values.iter().map(|value| self.render_word(value)).collect::<Vec<_>>().join(" "),
        })
    }

    fn render_int(&self, expr: &fp_shell_core::IntExpr) -> String {
        match expr {
            fp_shell_core::IntExpr::Literal(value) => value.to_string(),
            fp_shell_core::IntExpr::Variable(name) => format!("${{{}}}", name),
            fp_shell_core::IntExpr::UnaryNeg(value) => format!("-{}", self.render_int(value)),
            fp_shell_core::IntExpr::Binary { lhs, op, rhs } => {
                format!("$(({} {} {}))", self.render_int(lhs), render_arithmetic(*op), self.render_int(rhs))
            }
        }
    }

    fn render_word(&self, expr: &StringExpr) -> String {
        match expr {
            StringExpr::Literal(text) => shell_arg_quote(text),
            StringExpr::Variable(name) => format!("\"${{{}}}\"", name),
            StringExpr::Interpolated(parts) => {
                let mut out = String::from("\"");
                for part in parts {
                    match part {
                        StringPart::Literal(text) => out.push_str(&escape_double_quotes(text)),
                        StringPart::Variable(name) => out.push_str(&format!("${{{}}}", name)),
                    }
                }
                out.push('"');
                out
            }
        }
    }

    fn render_command_expr(&self, expr: &StringExpr) -> String {
        match expr {
            StringExpr::Literal(text) => text.clone(),
            StringExpr::Variable(name) => format!("${{{}}}", name),
            StringExpr::Interpolated(parts) => {
                let mut out = String::new();
                for part in parts {
                    match part {
                        StringPart::Literal(text) => out.push_str(text),
                        StringPart::Variable(name) => out.push_str(&format!("${{{}}}", name)),
                    }
                }
                out
            }
        }
    }

    fn push_line(&mut self, indent: usize, text: &str) {
        self.lines.push(format!("{}{}", "    ".repeat(indent), text));
    }
}

fn render_arithmetic(op: ArithmeticOp) -> &'static str {
    match op {
        ArithmeticOp::Add => "+",
        ArithmeticOp::Sub => "-",
        ArithmeticOp::Mul => "*",
        ArithmeticOp::Div => "/",
        ArithmeticOp::Mod => "%",
    }
}

fn render_comparison(op: ComparisonOp) -> &'static str {
    match op {
        ComparisonOp::Gt => "-gt",
        ComparisonOp::Lt => "-lt",
        ComparisonOp::Ge => "-ge",
        ComparisonOp::Le => "-le",
        ComparisonOp::Eq => "-eq",
        ComparisonOp::Ne => "-ne",
    }
}

fn shell_arg_quote(value: &str) -> String {
    format!("'{}'", value.replace('\'', "'\"'\"'"))
}

fn escape_double_quotes(value: &str) -> String {
    value.replace('\\', "\\\\").replace('"', "\\\"")
}

fn rsync_flags(options: RsyncOptions) -> String {
    let mut short = String::new();
    if options.archive {
        short.push('a');
    }
    if options.compress {
        short.push('z');
    }
    let mut flags = if short.is_empty() { String::new() } else { format!("-{}", short) };
    if options.delete {
        if !flags.is_empty() { flags.push(' '); }
        flags.push_str("--delete");
    }
    if options.checksum {
        if !flags.is_empty() { flags.push(' '); }
        flags.push_str("--checksum");
    }
    flags
}

const BASH_HELPERS: &str = r#"
ssh_cmd() {
  ssh -o ControlMaster=auto -o ControlPersist=60 -o ControlPath="$SSH_CONTROL_PATH" -- "$@"
}

scp_cmd() {
  scp -o ControlMaster=auto -o ControlPersist=60 -o ControlPath="$SSH_CONTROL_PATH" -- "$@"
}

rsync_cmd() {
  rsync -e "ssh -o ControlMaster=auto -o ControlPersist=60 -o ControlPath=$SSH_CONTROL_PATH" "$@"
}

run_host() {
  local host="$1"
  local cmd="$2"
  local transport="${FP_HOST_TRANSPORT[$host]:-ssh}"
  case "$transport" in
    local)
      bash -lc "$cmd"
      ;;
    ssh)
      local address="${FP_SSH_ADDRESS[$host]:-$host}"
      local user="${FP_SSH_USER[$host]:-}"
      local port="${FP_SSH_PORT[$host]:-}"
      local target="$address"
      if [[ -n "$user" ]]; then
        target="$user@$target"
      fi
      if [[ -n "$port" ]]; then
        ssh_cmd -p "$port" "$target" "$cmd"
      else
        ssh_cmd "$target" "$cmd"
      fi
      ;;
    docker)
      local container="${FP_DOCKER_CONTAINER[$host]}"
      local user="${FP_DOCKER_USER[$host]:-}"
      if [[ -n "$user" ]]; then
        docker exec --user "$user" "$container" sh -lc "$cmd"
      else
        docker exec "$container" sh -lc "$cmd"
      fi
      ;;
    kubectl)
      local pod="${FP_K8S_POD[$host]}"
      local namespace="${FP_K8S_NAMESPACE[$host]:-}"
      local container="${FP_K8S_CONTAINER[$host]:-}"
      local context="${FP_K8S_CONTEXT[$host]:-}"
      local args=()
      [[ -n "$context" ]] && args+=(--context "$context")
      [[ -n "$namespace" ]] && args+=(-n "$namespace")
      args+=(exec)
      [[ -n "$container" ]] && args+=(-c "$container")
      args+=("$pod" -- sh -lc "$cmd")
      kubectl "${args[@]}"
      ;;
    winrm)
      local address="${FP_WINRM_ADDRESS[$host]}"
      local user="${FP_WINRM_USER[$host]}"
      local password="${FP_WINRM_PASSWORD[$host]:-}"
      local scheme="${FP_WINRM_SCHEME[$host]:-http}"
      local port="${FP_WINRM_PORT[$host]:-}"
      local args=(-i "$address" -u "$user" -s "$scheme")
      [[ -n "$password" ]] && args+=(-p "$password")
      [[ -n "$port" ]] && args+=(-P "$port")
      evil-winrm "${args[@]}" -c "$cmd"
      ;;
    *)
      echo "unsupported transport: $transport" >&2
      return 1
      ;;
  esac
}

copy_host() {
  local host="$1"
  local src="$2"
  local dest="$3"
  local transport="${FP_HOST_TRANSPORT[$host]:-ssh}"
  case "$transport" in
    ssh)
      local address="${FP_SSH_ADDRESS[$host]:-$host}"
      local user="${FP_SSH_USER[$host]:-}"
      local port="${FP_SSH_PORT[$host]:-}"
      local target="$address"
      if [[ -n "$user" ]]; then
        target="$user@$target"
      fi
      if [[ -n "$port" ]]; then
        scp_cmd -P "$port" "$src" "$target:$dest"
      else
        scp_cmd "$src" "$target:$dest"
      fi
      ;;
    docker)
      docker cp "$src" "${FP_DOCKER_CONTAINER[$host]}:$dest"
      ;;
    kubectl)
      local pod="${FP_K8S_POD[$host]}"
      local namespace="${FP_K8S_NAMESPACE[$host]:-}"
      local context="${FP_K8S_CONTEXT[$host]:-}"
      local args=()
      [[ -n "$context" ]] && args+=(--context "$context")
      [[ -n "$namespace" ]] && args+=(-n "$namespace")
      kubectl cp "${args[@]}" "$src" "$pod:$dest"
      ;;
    winrm)
      echo "copy is not supported for winrm in bash target" >&2
      return 1
      ;;
    local)
      cp -- "$src" "$dest"
      ;;
  esac
}

template_host() {
  local host="$1"
  local src="$2"
  local dest="$3"
  local vars="$4"
  local tmp
  tmp=$(mktemp)
  eval "$vars envsubst < \"$src\" > \"$tmp\""
  copy_host "$host" "$tmp" "$dest"
  rm -f "$tmp"
}

rsync_host() {
  local host="$1"
  local flags="$2"
  local src="$3"
  local dest="$4"
  local transport="${FP_HOST_TRANSPORT[$host]:-ssh}"
  if [[ "$transport" != "ssh" ]]; then
    echo "rsync is only supported for ssh in bash target" >&2
    return 1
  fi
  local address="${FP_SSH_ADDRESS[$host]:-$host}"
  local user="${FP_SSH_USER[$host]:-}"
  local target="$address"
  if [[ -n "$user" ]]; then
    target="$user@$target"
  fi
  rsync_cmd $flags -- "$src" "$target:$dest"
}

"#;

#[cfg(test)]
mod tests {
    use super::*;
    use fp_shell_core::{
        InventoryHost, ScriptProgram, SshInventory, Statement, StringExpr, TransportKind,
    };
    use std::collections::HashMap;

    #[test]
    fn renders_ssh_dispatch() {
        let program = ScriptProgram {
            items: vec![ScriptItem::Statement(Statement::Run(OperationRun {
                hosts: vec![HostExpr::Selector(StringExpr::Literal("web-1".to_string()))],
                command: StringExpr::Literal("uptime".to_string()),
                cwd: None,
                sudo: false,
                guards: OperationGuards::default(),
            }))],
        };
        let mut hosts = HashMap::new();
        hosts.insert(
            "web-1".to_string(),
            InventoryHost {
                transport: TransportKind::Ssh,
                ssh: Some(SshInventory {
                    address: Some("10.0.0.11".to_string()),
                    user: Some("deploy".to_string()),
                    port: None,
                }),
                docker: None,
                kubectl: None,
                winrm: None,
            },
        );
        let inventory = ShellInventory {
            groups: HashMap::new(),
            hosts,
        };
        let script = BashTarget::new().render(&program, &inventory).expect("render should succeed");
        assert!(script.contains("FP_HOST_TRANSPORT['web-1']='ssh'"));
        assert!(script.contains("run_host 'web-1' 'uptime'"));
    }
}
