use std::collections::HashMap;

#[derive(Debug, Clone, Default)]
pub struct ShellInventory {
    pub groups: HashMap<String, Vec<String>>,
    pub hosts: HashMap<String, InventoryHost>,
}

#[derive(Debug, Clone)]
pub struct InventoryHost {
    pub transport: TransportKind,
    pub ssh: Option<SshInventory>,
    pub docker: Option<DockerInventory>,
    pub kubectl: Option<KubectlInventory>,
    pub winrm: Option<WinRmInventory>,
}

impl Default for InventoryHost {
    fn default() -> Self {
        Self {
            transport: TransportKind::Ssh,
            ssh: Some(SshInventory::default()),
            docker: None,
            kubectl: None,
            winrm: None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum TransportKind {
    Local,
    Ssh,
    Docker,
    Kubectl,
    Winrm,
}

#[derive(Debug, Clone, Default)]
pub struct SshInventory {
    pub address: Option<String>,
    pub user: Option<String>,
    pub port: Option<u16>,
}

#[derive(Debug, Clone, Default)]
pub struct DockerInventory {
    pub container: String,
    pub user: Option<String>,
}

#[derive(Debug, Clone, Default)]
pub struct KubectlInventory {
    pub pod: String,
    pub namespace: Option<String>,
    pub container: Option<String>,
    pub context: Option<String>,
}

#[derive(Debug, Clone, Default)]
pub struct WinRmInventory {
    pub address: String,
    pub user: String,
    pub password: Option<String>,
    pub port: Option<u16>,
    pub scheme: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScriptTarget {
    Bash,
    PowerShell,
}

impl ScriptTarget {
    pub fn extension(self) -> &'static str {
        match self {
            Self::Bash => "sh",
            Self::PowerShell => "ps1",
        }
    }
}

#[derive(Debug, Clone)]
pub struct ScriptProgram {
    pub items: Vec<ScriptItem>,
}

impl Default for ScriptProgram {
    fn default() -> Self {
        Self { items: Vec::new() }
    }
}

#[derive(Debug, Clone)]
pub enum ScriptItem {
    Function(FunctionDef),
    Statement(Statement),
}

#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub name: String,
    pub params: Vec<String>,
    pub body: Block,
}

#[derive(Debug, Clone, Default)]
pub struct Block {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Run(OperationRun),
    Copy(OperationCopy),
    Template(OperationTemplate),
    Rsync(OperationRsync),
    Primitive(PrimitiveStmt),
    If(IfStmt),
    While(WhileStmt),
    ForEach(ForEachStmt),
    Let(LetStmt),
    Invoke(InvokeStmt),
}

#[derive(Debug, Clone)]
pub struct OperationRun {
    pub hosts: Vec<HostExpr>,
    pub command: StringExpr,
    pub cwd: Option<StringExpr>,
    pub sudo: bool,
    pub guards: OperationGuards,
}

#[derive(Debug, Clone)]
pub struct OperationCopy {
    pub hosts: Vec<HostExpr>,
    pub source: StringExpr,
    pub destination: StringExpr,
    pub guards: OperationGuards,
}

#[derive(Debug, Clone)]
pub struct OperationTemplate {
    pub hosts: Vec<HostExpr>,
    pub source: StringExpr,
    pub destination: StringExpr,
    pub vars: HashMap<String, StringExpr>,
    pub guards: OperationGuards,
}

#[derive(Debug, Clone)]
pub struct OperationRsync {
    pub hosts: Vec<HostExpr>,
    pub source: StringExpr,
    pub destination: StringExpr,
    pub options: RsyncOptions,
    pub guards: OperationGuards,
}

#[derive(Debug, Clone, Default)]
pub struct OperationGuards {
    pub only_if: Option<StringExpr>,
    pub unless: Option<StringExpr>,
    pub creates: Option<StringExpr>,
    pub removes: Option<StringExpr>,
}

#[derive(Debug, Clone, Copy)]
pub struct RsyncOptions {
    pub archive: bool,
    pub compress: bool,
    pub delete: bool,
    pub checksum: bool,
}

impl Default for RsyncOptions {
    fn default() -> Self {
        Self {
            archive: true,
            compress: true,
            delete: false,
            checksum: false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct IfStmt {
    pub condition: ConditionExpr,
    pub then_block: Block,
    pub else_block: Option<Block>,
}

#[derive(Debug, Clone)]
pub struct WhileStmt {
    pub condition: ConditionExpr,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct ForEachStmt {
    pub binding: String,
    pub values: Vec<StringExpr>,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct LetStmt {
    pub name: String,
    pub value: ValueExpr,
}

#[derive(Debug, Clone)]
pub struct InvokeStmt {
    pub name: String,
    pub args: Vec<ValueExpr>,
}

#[derive(Debug, Clone)]
pub enum PrimitiveStmt {
    RunLocal {
        command: StringExpr,
    },
    RunSsh {
        host: StringExpr,
        command: StringExpr,
    },
    RunDocker {
        host: StringExpr,
        command: StringExpr,
    },
    RunKubectl {
        host: StringExpr,
        command: StringExpr,
    },
    RunWinrm {
        host: StringExpr,
        command: StringExpr,
    },
    CopyLocal {
        source: StringExpr,
        destination: StringExpr,
    },
    CopySsh {
        host: StringExpr,
        source: StringExpr,
        destination: StringExpr,
    },
    CopyDocker {
        host: StringExpr,
        source: StringExpr,
        destination: StringExpr,
    },
    CopyKubectl {
        host: StringExpr,
        source: StringExpr,
        destination: StringExpr,
    },
    CopyWinrm {
        host: StringExpr,
        source: StringExpr,
        destination: StringExpr,
    },
    RenderTemplate {
        source: StringExpr,
        destination: StringExpr,
        vars: StringExpr,
    },
    RemoveFile {
        path: StringExpr,
    },
    RsyncSsh {
        host: StringExpr,
        flags: StringExpr,
        source: StringExpr,
        destination: StringExpr,
    },
    UnsupportedTransport {
        operation: UnsupportedTransportOperation,
        transport: StringExpr,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum UnsupportedTransportOperation {
    Run,
    Copy,
    Rsync,
}

#[derive(Debug, Clone)]
pub enum ValueExpr {
    String(StringExpr),
    Int(IntExpr),
    Bool(BoolExpr),
    StringList(Vec<StringExpr>),
}

#[derive(Debug, Clone)]
pub enum HostExpr {
    Localhost,
    Selector(StringExpr),
}

#[derive(Debug, Clone)]
pub enum ConditionExpr {
    Bool(BoolExpr),
    Command(StringExpr),
    StringTruthy(StringExpr),
}

#[derive(Debug, Clone)]
pub enum BoolExpr {
    Literal(bool),
    Variable(String),
    IntComparison {
        lhs: IntExpr,
        op: ComparisonOp,
        rhs: IntExpr,
    },
    StringComparison {
        lhs: StringExpr,
        op: StringComparisonOp,
        rhs: StringExpr,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum ComparisonOp {
    Gt,
    Lt,
    Ge,
    Le,
    Eq,
    Ne,
}

#[derive(Debug, Clone, Copy)]
pub enum StringComparisonOp {
    Eq,
    Ne,
}

#[derive(Debug, Clone)]
pub enum IntExpr {
    Literal(i64),
    Variable(String),
    UnaryNeg(Box<IntExpr>),
    Binary {
        lhs: Box<IntExpr>,
        op: ArithmeticOp,
        rhs: Box<IntExpr>,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum ArithmeticOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Clone)]
pub enum StringExpr {
    Literal(String),
    Variable(String),
    HostTransport(Box<StringExpr>),
    TempPath,
    Interpolated(Vec<StringPart>),
}

#[derive(Debug, Clone)]
pub enum StringPart {
    Literal(String),
    Variable(String),
}

pub trait ScriptRenderer {
    type Error;

    fn render(
        &self,
        program: &ScriptProgram,
        inventory: &ShellInventory,
    ) -> Result<String, Self::Error>;
}
