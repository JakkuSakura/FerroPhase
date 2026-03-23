pub enum Result<T, E> {
    Ok(T),
    Err(E),
}

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

    pub fn open(self, path: &str) -> Result<File, IoError> {
        libc_file_open_options(path, self)
    }
}

pub struct File {
    fd: i32,
}

impl File {
    pub fn open(path: &str) -> Result<File, IoError> {
        OpenOptions::new().read(true).open(path)
    }

    pub fn create(path: &str) -> Result<File, IoError> {
        OpenOptions::new()
            .write(true)
            .truncate(true)
            .create(true)
            .open(path)
    }

    pub fn options() -> OpenOptions {
        OpenOptions::new()
    }

    pub fn metadata(&self) -> Result<Metadata, IoError> {
        libc_file_metadata(self.fd)
    }

    pub fn read_to_string(&mut self) -> Result<str, IoError> {
        libc_file_read_to_string(self.fd)
    }

    pub fn write_all(&mut self, content: &str) -> Result<(), IoError> {
        libc_file_write_all(self.fd, content)
    }

    pub fn flush(&mut self) -> Result<(), IoError> {
        libc_file_flush(self.fd)
    }

    pub fn sync_all(&mut self) -> Result<(), IoError> {
        libc_file_sync_all(self.fd)
    }

    pub fn seek(&mut self, pos: SeekFrom) -> Result<i64, IoError> {
        match pos {
            SeekFrom::Start(offset) => libc_file_seek(self.fd, offset, 0),
            SeekFrom::Current(offset) => libc_file_seek(self.fd, offset, 1),
            SeekFrom::End(offset) => libc_file_seek(self.fd, offset, 2),
        }
    }

    pub fn close(self) -> Result<(), IoError> {
        libc_file_close(self.fd)
    }

    pub fn as_raw_fd(&self) -> i32 {
        self.fd
    }
}

#[lang = "libc_file_open_options"]
fn libc_file_open_options(path: &str, options: OpenOptions) -> Result<File, IoError> {
    compile_error!("std::fs::File open hooks are not implemented yet")
}

#[lang = "libc_file_metadata"]
fn libc_file_metadata(fd: i32) -> Result<Metadata, IoError> {
    compile_error!("std::fs::File metadata hooks are not implemented yet")
}

#[lang = "libc_file_read_to_string"]
fn libc_file_read_to_string(fd: i32) -> Result<str, IoError> {
    compile_error!("std::fs::File read hooks are not implemented yet")
}

#[lang = "libc_file_write_all"]
fn libc_file_write_all(fd: i32, content: &str) -> Result<(), IoError> {
    compile_error!("std::fs::File write hooks are not implemented yet")
}

#[lang = "libc_file_flush"]
fn libc_file_flush(fd: i32) -> Result<(), IoError> {
    compile_error!("std::fs::File flush hooks are not implemented yet")
}

#[lang = "libc_file_sync_all"]
fn libc_file_sync_all(fd: i32) -> Result<(), IoError> {
    compile_error!("std::fs::File sync hooks are not implemented yet")
}

#[lang = "libc_file_seek"]
fn libc_file_seek(fd: i32, offset: i64, whence: i32) -> Result<i64, IoError> {
    compile_error!("std::fs::File seek hooks are not implemented yet")
}

#[lang = "libc_file_close"]
fn libc_file_close(fd: i32) -> Result<(), IoError> {
    compile_error!("std::fs::File close hooks are not implemented yet")
}

#[lang = "fs_read_dir"]
pub fn read_dir(path: &str) -> Vec<&str> { compile_error!("compiler intrinsic") }

#[lang = "fs_walk_dir"]
pub fn walk_dir(path: &str) -> Vec<&str> { compile_error!("compiler intrinsic") }

#[lang = "fs_read_to_string"]
pub fn read_to_string(path: &str) -> str { compile_error!("compiler intrinsic") }

#[lang = "fs_write_string"]
pub fn write_string(path: &str, content: &str) { compile_error!("compiler intrinsic") }

#[lang = "fs_append_string"]
pub fn append_string(path: &str, content: &str) { compile_error!("compiler intrinsic") }

#[lang = "fs_exists"]
pub fn exists(path: &str) -> bool { compile_error!("compiler intrinsic") }

#[lang = "fs_is_dir"]
pub fn is_dir(path: &str) -> bool { compile_error!("compiler intrinsic") }

#[lang = "fs_is_file"]
pub fn is_file(path: &str) -> bool { compile_error!("compiler intrinsic") }

#[lang = "fs_create_dir_all"]
pub fn create_dir_all(path: &str) { compile_error!("compiler intrinsic") }

#[lang = "fs_remove_file"]
pub fn remove_file(path: &str) { compile_error!("compiler intrinsic") }

#[lang = "fs_remove_dir_all"]
pub fn remove_dir_all(path: &str) { compile_error!("compiler intrinsic") }

#[lang = "fs_glob"]
pub fn glob(pattern: &str) -> Vec<&str> { compile_error!("compiler intrinsic") }
