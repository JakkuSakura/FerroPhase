use super::AsmOperand;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PosixFlagStyle {
    Linux,
    Darwin,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PosixDirentStyle {
    Linux,
    Darwin,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AsmSysOp {
    Exit {
        code: AsmOperand,
    },
    GetPid,
    GetTid,
    Dlopen {
        path: AsmOperand,
        flags: AsmOperand,
    },
    Dlsym {
        handle: AsmOperand,
        symbol: AsmOperand,
    },
    Dlclose {
        handle: AsmOperand,
    },
    Unlink {
        path: AsmOperand,
    },
    Mkdir {
        path: AsmOperand,
        mode: AsmOperand,
    },
    Rmdir {
        path: AsmOperand,
    },
    Rename {
        from: AsmOperand,
        to: AsmOperand,
    },
    Access {
        path: AsmOperand,
        mode: AsmOperand,
    },
    Write {
        fd: AsmOperand,
        buffer: AsmOperand,
        len: AsmOperand,
    },
    Read {
        fd: AsmOperand,
        buffer: AsmOperand,
        len: AsmOperand,
    },
    Close {
        fd: AsmOperand,
    },
    Open {
        path: AsmOperand,
        flags: AsmOperand,
        mode: AsmOperand,
        flag_style: PosixFlagStyle,
    },
    Seek {
        fd: AsmOperand,
        offset: AsmOperand,
        whence: AsmOperand,
    },
    Mmap {
        addr: AsmOperand,
        len: AsmOperand,
        prot: AsmOperand,
        flags: AsmOperand,
        fd: AsmOperand,
        offset: AsmOperand,
    },
    Munmap {
        addr: AsmOperand,
        len: AsmOperand,
    },

    Opendir {
        path: AsmOperand,
    },
    Readdir {
        dir: AsmOperand,
        dirent_style: PosixDirentStyle,
    },
    Closedir {
        dir: AsmOperand,
    },
}
