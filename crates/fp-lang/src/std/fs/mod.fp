use super::path::Path;

pub enum ErrorKind {
    NotFound,
    PermissionDenied,
    AlreadyExists,
    InvalidInput,
    Interrupted,
    UnexpectedEof,
    WriteZero,
    Other,
}

pub struct IoError {
    kind: ErrorKind,
    raw_os_error: i32,
    message: str,
}

impl IoError {
    pub fn kind(&self) -> ErrorKind {
        self.kind
    }

    pub fn raw_os_error(&self) -> i32 {
        self.raw_os_error
    }

    pub fn message(&self) -> str {
        self.message
    }
}

pub struct Metadata {
    len: i64,
    is_dir: bool,
    is_file: bool,
}

impl Metadata {
    pub fn len(&self) -> i64 {
        self.len
    }

    pub fn is_dir(&self) -> bool {
        self.is_dir
    }

    pub fn is_file(&self) -> bool {
        self.is_file
    }
}

pub enum SeekFrom {
    Start(i64),
    End(i64),
    Current(i64),
}

pub struct OpenOptions {
    read: bool,
    write: bool,
    append: bool,
    truncate: bool,
    create: bool,
    create_new: bool,
    mode: i32,
}

impl OpenOptions {
    pub fn new() -> OpenOptions {
        OpenOptions {
            read: false,
            write: false,
            append: false,
            truncate: false,
            create: false,
            create_new: false,
            mode: 0o666,
        }
    }

    pub fn read(self, read: bool) -> OpenOptions {
        OpenOptions {
            read,
            write: self.write,
            append: self.append,
            truncate: self.truncate,
            create: self.create,
            create_new: self.create_new,
            mode: self.mode,
        }
    }

    pub fn write(self, write: bool) -> OpenOptions {
        OpenOptions {
            read: self.read,
            write,
            append: self.append,
            truncate: self.truncate,
            create: self.create,
            create_new: self.create_new,
            mode: self.mode,
        }
    }

    pub fn append(self, append: bool) -> OpenOptions {
        OpenOptions {
            read: self.read,
            write: self.write,
            append,
            truncate: self.truncate,
            create: self.create,
            create_new: self.create_new,
            mode: self.mode,
        }
    }

    pub fn truncate(self, truncate: bool) -> OpenOptions {
        OpenOptions {
            read: self.read,
            write: self.write,
            append: self.append,
            truncate,
            create: self.create,
            create_new: self.create_new,
            mode: self.mode,
        }
    }

    pub fn create(self, create: bool) -> OpenOptions {
        OpenOptions {
            read: self.read,
            write: self.write,
            append: self.append,
            truncate: self.truncate,
            create,
            create_new: self.create_new,
            mode: self.mode,
        }
    }

    pub fn create_new(self, create_new: bool) -> OpenOptions {
        OpenOptions {
            read: self.read,
            write: self.write,
            append: self.append,
            truncate: self.truncate,
            create: self.create,
            create_new,
            mode: self.mode,
        }
    }

    pub fn mode(self, mode: i32) -> OpenOptions {
        OpenOptions {
            read: self.read,
            write: self.write,
            append: self.append,
            truncate: self.truncate,
            create: self.create,
            create_new: self.create_new,
            mode,
        }
    }

    pub fn open(self, path: &Path) -> std::result::Result<File, IoError> {
        let _ = self;
        let _ = path;
        compile_error!("std::fs::OpenOptions::open is not implemented for the current std surface")
    }
}

pub struct File {
    fd: i32,
}

impl File {
    pub fn open(path: &Path) -> std::result::Result<File, IoError> {
        OpenOptions::new().read(true).open(path)
    }

    pub fn create(path: &Path) -> std::result::Result<File, IoError> {
        OpenOptions::new()
            .write(true)
            .truncate(true)
            .create(true)
            .open(path)
    }

    pub fn options() -> OpenOptions {
        OpenOptions::new()
    }

    pub fn metadata(&self) -> std::result::Result<Metadata, IoError> {
        let _ = self.fd;
        compile_error!("std::fs::File::metadata is not implemented for the current std surface")
    }

    pub fn read_to_string(&mut self) -> std::result::Result<str, IoError> {
        let _ = self.fd;
        compile_error!("std::fs::File::read_to_string is not implemented for the current std surface")
    }

    pub fn write_all(&mut self, content: &str) -> std::result::Result<(), IoError> {
        let _ = self;
        let _ = content;
        compile_error!("std::fs::File::write_all is not implemented for the current std surface")
    }

    pub fn flush(&mut self) -> std::result::Result<(), IoError> {
        std::result::Result::Ok(())
    }

    pub fn sync_all(&mut self) -> std::result::Result<(), IoError> {
        let _ = self;
        compile_error!("std::fs::File::sync_all is not implemented for the current std surface")
    }

    pub fn seek(&mut self, pos: SeekFrom) -> std::result::Result<i64, IoError> {
        let _ = self;
        let _ = pos;
        compile_error!("std::fs::File::seek is not implemented for the current std surface")
    }

    pub fn close(self) -> std::result::Result<(), IoError> {
        let _ = self;
        compile_error!("std::fs::File::close is not implemented for the current std surface")
    }

    pub fn as_raw_fd(&self) -> i32 {
        self.fd
    }
}

fn io_error_other(message: &str) -> IoError {
    IoError {
        kind: ErrorKind::Other,
        raw_os_error: 0,
        message,
    }
}

#[lang = "fs_read_dir"]
pub fn read_dir(path: &Path) -> Vec<&str> { compile_error!("compiler intrinsic") }

#[lang = "fs_walk_dir"]
pub fn walk_dir(path: &Path) -> Vec<&str> { compile_error!("compiler intrinsic") }

#[lang = "fs_read_to_string"]
pub fn read_to_string(path: &Path) -> str { compile_error!("compiler intrinsic") }

#[lang = "fs_write_string"]
pub fn write_string(path: &Path, content: &str) { compile_error!("compiler intrinsic") }

#[lang = "fs_append_string"]
pub fn append_string(path: &Path, content: &str) { compile_error!("compiler intrinsic") }

#[lang = "fs_exists"]
pub fn exists(path: &Path) -> bool { compile_error!("compiler intrinsic") }

#[lang = "fs_is_dir"]
pub fn is_dir(path: &Path) -> bool { compile_error!("compiler intrinsic") }

#[lang = "fs_is_file"]
pub fn is_file(path: &Path) -> bool { compile_error!("compiler intrinsic") }

#[lang = "fs_create_dir_all"]
pub fn create_dir_all(path: &Path) { compile_error!("compiler intrinsic") }

#[lang = "fs_remove_file"]
pub fn remove_file(path: &Path) { compile_error!("compiler intrinsic") }

#[lang = "fs_remove_dir_all"]
pub fn remove_dir_all(path: &Path) { compile_error!("compiler intrinsic") }

#[lang = "fs_glob"]
pub fn glob(pattern: &str) -> Vec<&str> { compile_error!("compiler intrinsic") }
