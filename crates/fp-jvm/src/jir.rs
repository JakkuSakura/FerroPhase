#[derive(Debug, Clone, PartialEq, Eq)]
pub struct JvmProgram {
    pub class: JvmClass,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct JvmClass {
    pub name: String,
    pub super_name: String,
    pub methods: Vec<JvmMethod>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct JvmMethod {
    pub name: String,
    pub descriptor: String,
    pub access_flags: u16,
    pub code: JvmCode,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct JvmCode {
    pub max_stack: u16,
    pub max_locals: u16,
    pub instructions: Vec<JvmInstr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum JvmInstr {
    AConstNull,
    IConst(i32),
    LConst0,
    ILoad(u16),
    IStore(u16),
    IAdd,
    ISub,
    IMul,
    IDiv,
    Label(String),
    Goto(String),
    IfEq(String),
    IfNe(String),
    Invokestatic {
        class: String,
        method: String,
        descriptor: String,
    },
    Return,
    IReturn,
}
