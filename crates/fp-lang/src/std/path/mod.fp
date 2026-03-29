use std::option::Option;

pub struct Path {
    inner: str,
}

impl Path {
    pub fn new(path: &str) -> Path {
        Path {
            inner: path,
        }
    }

    pub fn as_str(&self) -> &str {
        &self.inner
    }

    pub fn to_path_buf(&self) -> PathBuf {
        PathBuf::from(self.inner)
    }

    pub fn join(&self, child: &Path) -> PathBuf {
        PathBuf::from(intrinsic_join(self.inner, child.as_str()))
    }

    pub fn parent(&self) -> Option<PathBuf> {
        option_path_buf(intrinsic_parent(self.inner))
    }

    pub fn file_name(&self) -> Option<str> {
        option_str(intrinsic_file_name(self.inner))
    }

    pub fn extension(&self) -> Option<str> {
        option_str(intrinsic_extension(self.inner))
    }

    pub fn stem(&self) -> Option<str> {
        option_str(intrinsic_stem(self.inner))
    }

    pub fn is_absolute(&self) -> bool {
        intrinsic_is_absolute(self.inner)
    }

    pub fn normalize(&self) -> PathBuf {
        PathBuf::from(intrinsic_normalize(self.inner))
    }

    pub fn has_extension(&self, extension: &str) -> bool {
        match self.extension() {
            Option::Some(current) => current == extension,
            Option::None => false,
        }
    }
}

pub struct PathBuf {
    inner: str,
}

impl PathBuf {
    pub fn new() -> PathBuf {
        PathBuf {
            inner: "",
        }
    }

    pub fn from(path: &str) -> PathBuf {
        PathBuf {
            inner: path,
        }
    }

    pub fn as_path(&self) -> Path {
        Path::new(self.inner)
    }

    pub fn as_str(&self) -> &str {
        &self.inner
    }

    pub fn into_string(self) -> str {
        self.inner
    }

    pub fn join(&self, child: &Path) -> PathBuf {
        self.as_path().join(child)
    }

    pub fn push(&mut self, child: &Path) {
        self.inner = intrinsic_join(self.inner, child.as_str());
    }

    pub fn parent(&self) -> Option<PathBuf> {
        self.as_path().parent()
    }

    pub fn file_name(&self) -> Option<str> {
        self.as_path().file_name()
    }

    pub fn extension(&self) -> Option<str> {
        self.as_path().extension()
    }

    pub fn stem(&self) -> Option<str> {
        self.as_path().stem()
    }

    pub fn is_absolute(&self) -> bool {
        self.as_path().is_absolute()
    }

    pub fn normalize(&self) -> PathBuf {
        self.as_path().normalize()
    }

    pub fn has_extension(&self, extension: &str) -> bool {
        self.as_path().has_extension(extension)
    }
}

fn option_str(value: str) -> Option<str> {
    if value == "" {
        std::option::none()
    } else {
        std::option::some(value)
    }
}

fn option_path_buf(value: str) -> Option<PathBuf> {
    if value == "" {
        std::option::none()
    } else {
        std::option::some(PathBuf::from(value))
    }
}

#[lang = "path_join"]
fn intrinsic_join(lhs: &str, rhs: &str) -> str { compile_error!("compiler intrinsic") }

#[lang = "path_parent"]
fn intrinsic_parent(path: &str) -> str { compile_error!("compiler intrinsic") }

#[lang = "path_file_name"]
fn intrinsic_file_name(path: &str) -> str { compile_error!("compiler intrinsic") }

#[lang = "path_extension"]
fn intrinsic_extension(path: &str) -> str { compile_error!("compiler intrinsic") }

#[lang = "path_stem"]
fn intrinsic_stem(path: &str) -> str { compile_error!("compiler intrinsic") }

#[lang = "path_is_absolute"]
fn intrinsic_is_absolute(path: &str) -> bool { compile_error!("compiler intrinsic") }

#[lang = "path_normalize"]
fn intrinsic_normalize(path: &str) -> str { compile_error!("compiler intrinsic") }
