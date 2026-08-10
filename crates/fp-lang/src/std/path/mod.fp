
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
        PathBuf::from(::std::intrinsics::path::join(self.inner, child.as_str()))
    }

    pub fn parent(&self) -> ::std::option::Option<PathBuf> {
        option_path_buf(::std::intrinsics::path::parent(self.inner))
    }

    pub fn file_name(&self) -> ::std::option::Option<str> {
        option_str(::std::intrinsics::path::file_name(self.inner))
    }

    pub fn extension(&self) -> ::std::option::Option<str> {
        option_str(::std::intrinsics::path::extension(self.inner))
    }

    pub fn stem(&self) -> ::std::option::Option<str> {
        option_str(::std::intrinsics::path::stem(self.inner))
    }

    pub fn is_absolute(&self) -> bool {
        ::std::intrinsics::path::is_absolute(self.inner)
    }

    pub fn normalize(&self) -> PathBuf {
        PathBuf::from(::std::intrinsics::path::normalize(self.inner))
    }

    pub fn has_extension(&self, extension: &str) -> bool {
        match self.extension() {
            ::std::option::Option::Some(current) => current == extension,
            ::std::option::Option::None => false,
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
        self.inner = ::std::intrinsics::path::join(self.inner, child.as_str());
    }

    pub fn parent(&self) -> ::std::option::Option<PathBuf> {
        self.as_path().parent()
    }

    pub fn file_name(&self) -> ::std::option::Option<str> {
        self.as_path().file_name()
    }

    pub fn extension(&self) -> ::std::option::Option<str> {
        self.as_path().extension()
    }

    pub fn stem(&self) -> ::std::option::Option<str> {
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

fn option_str(value: str) -> ::std::option::Option<str> {
    if value == "" {
        ::std::option::none()
    } else {
        ::std::option::some(value)
    }
}

fn option_path_buf(value: str) -> ::std::option::Option<PathBuf> {
    if value == "" {
        ::std::option::none()
    } else {
        ::std::option::some(PathBuf::from(value))
    }
}
