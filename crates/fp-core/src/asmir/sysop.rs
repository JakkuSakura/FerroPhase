use super::AsmValue;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PosixFlagStyle {
    Linux,
    Darwin,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AsmSysOp {
    Exit {
        code: AsmValue,
    },
    GetPid,
    GetTid,
    Dlopen {
        path: AsmValue,
        flags: AsmValue,
    },
    Dlsym {
        handle: AsmValue,
        symbol: AsmValue,
    },
    Dlclose {
        handle: AsmValue,
    },
    Unlink {
        path: AsmValue,
    },
    Mkdir {
        path: AsmValue,
        mode: AsmValue,
    },
    Rmdir {
        path: AsmValue,
    },
    Rename {
        from: AsmValue,
        to: AsmValue,
    },
    Access {
        path: AsmValue,
        mode: AsmValue,
    },
    Write {
        fd: AsmValue,
        buffer: AsmValue,
        len: AsmValue,
    },
    Read {
        fd: AsmValue,
        buffer: AsmValue,
        len: AsmValue,
    },
    Close {
        fd: AsmValue,
    },
    Open {
        path: AsmValue,
        flags: AsmValue,
        mode: AsmValue,
        flag_style: PosixFlagStyle,
    },
    Seek {
        fd: AsmValue,
        offset: AsmValue,
        whence: AsmValue,
    },
    Mmap {
        addr: AsmValue,
        len: AsmValue,
        prot: AsmValue,
        flags: AsmValue,
        fd: AsmValue,
        offset: AsmValue,
    },
    Munmap {
        addr: AsmValue,
        len: AsmValue,
    },
}

