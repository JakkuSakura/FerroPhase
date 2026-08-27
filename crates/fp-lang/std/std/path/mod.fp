
pub struct Path {
    inner: &str,
}

impl Path {
    pub fn new(path: &str) -> Path {
        Path {
            inner: path,
        }
    }

    pub fn as_str(&self) -> &str {
        self.inner
    }

    pub fn to_path_buf(&self) -> PathBuf {
        PathBuf::from(self.inner)
    }

    pub fn join(&self, child: &Path) -> PathBuf {
        PathBuf::from(self.joined(child.as_str()).as_str())
    }

    pub fn parent(&self) -> ::core::option::Option<PathBuf> {
        let value = self.parent_string();
        if value.len() == 0 {
            ::core::option::Option::None
        } else {
            ::core::option::Option::Some(PathBuf::from(value.as_str()))
        }
    }

    pub fn file_name(&self) -> ::core::option::Option<::alloc::string::String> {
        let value = self.file_name_string();
        if value.len() == 0 {
            ::core::option::Option::None
        } else {
            ::core::option::Option::Some(value)
        }
    }

    pub fn extension(&self) -> ::core::option::Option<::alloc::string::String> {
        let value = self.extension_string();
        if value.len() == 0 {
            ::core::option::Option::None
        } else {
            ::core::option::Option::Some(value)
        }
    }

    pub fn stem(&self) -> ::core::option::Option<::alloc::string::String> {
        let value = self.stem_string();
        if value.len() == 0 {
            ::core::option::Option::None
        } else {
            ::core::option::Option::Some(value)
        }
    }

    pub fn is_absolute(&self) -> bool {
        self.is_absolute_path()
    }

    pub fn normalize(&self) -> PathBuf {
        PathBuf::from(self.normalized().as_str())
    }

    pub fn has_extension(&self, extension: &str) -> bool {
        match self.extension() {
            ::core::option::Option::Some(current) => current.as_str() == extension,
            ::core::option::Option::None => false,
        }
    }

    fn joined(&self, rhs: &str) -> ::alloc::string::String {
        if rhs.len() > 0 && rhs[0] == 47 as i8 {
            let mut absolute = ::alloc::string::String::new();
            absolute.extend(rhs);
            return absolute;
        }
        let mut result = ::alloc::string::String::new();
        result.extend(self.inner);
        if result.len() > 0 && result.as_str()[result.len() - 1] != 47 as i8 {
            result.push_byte(47 as u8);
        }
        result.extend(rhs);
        result
    }

    fn parent_string(&self) -> ::alloc::string::String {
        let mut end = self.inner.len();
        while end > 0 && self.inner[end - 1] == 47 as i8 {
            end = end - 1;
        }
        while end > 0 && self.inner[end - 1] != 47 as i8 {
            end = end - 1;
        }
        let mut result = ::alloc::string::String::new();
        if end > 0 {
            result.extend(&self.inner[0..end - 1]);
        }
        result
    }

    fn file_name_string(&self) -> ::alloc::string::String {
        let mut start: usize = 0;
        let mut idx: usize = 0;
        while idx < self.inner.len() {
            if self.inner[idx] == 47 as i8 {
                start = idx + 1;
            }
            idx = idx + 1;
        }
        let mut result = ::alloc::string::String::new();
        result.extend(&self.inner[start..self.inner.len()]);
        result
    }

    fn extension_string(&self) -> ::alloc::string::String {
        let mut start: usize = 0;
        let mut idx: usize = 0;
        while idx < self.inner.len() {
            if self.inner[idx] == 47 as i8 {
                start = idx + 1;
            }
            idx = idx + 1;
        }
        let mut dot = self.inner.len();
        idx = start;
        while idx < self.inner.len() {
            if self.inner[idx] == 46 as i8 {
                dot = idx;
            }
            idx = idx + 1;
        }
        let mut result = ::alloc::string::String::new();
        if dot < self.inner.len() - 1 {
            result.extend(&self.inner[dot + 1..self.inner.len()]);
        }
        result
    }

    fn stem_string(&self) -> ::alloc::string::String {
        let mut start: usize = 0;
        let mut idx: usize = 0;
        while idx < self.inner.len() {
            if self.inner[idx] == 47 as i8 {
                start = idx + 1;
            }
            idx = idx + 1;
        }
        let mut dot = self.inner.len();
        idx = start;
        while idx < self.inner.len() {
            if self.inner[idx] == 46 as i8 {
                dot = idx;
            }
            idx = idx + 1;
        }
        let mut result = ::alloc::string::String::new();
        result.extend(&self.inner[start..dot]);
        result
    }

    fn is_absolute_path(&self) -> bool {
        self.inner.len() > 0 && self.inner[0] == 47 as i8
    }

    fn normalized(&self) -> ::alloc::string::String {
        let mut result: ::alloc::string::String = ::alloc::string::String::new();
        let mut idx: usize = 0;
        while idx < self.inner.len() {
            while idx < self.inner.len() && self.inner[idx] == 47 as i8 {
                idx = idx + 1;
            }
            let start: usize = idx;
            while idx < self.inner.len() && self.inner[idx] != 47 as i8 {
                idx = idx + 1;
            }
            if idx > start {
                let part: &str = &self.inner[start..idx];
                if part != "." {
                    if result.len() > 0 && result.as_str()[result.len() - 1] != 47 as i8 {
                        result.push_byte(47 as u8);
                    }
                    result.extend(part);
                }
            }
        }
        if self.is_absolute_path() && (result.len() == 0 || result.as_str()[0] != 47 as i8) {
            let mut absolute: ::alloc::string::String = ::alloc::string::String::new();
            absolute.push_byte(47 as u8);
            absolute.extend(result.as_str());
            return absolute;
        }
        result
    }
}

pub struct PathBuf {
    inner: ::alloc::string::String,
}

impl PathBuf {
    pub fn new() -> PathBuf {
        PathBuf {
            inner: ::alloc::string::String::new(),
        }
    }

    pub fn from(path: &str) -> PathBuf {
        let mut inner = ::alloc::string::String::new();
        inner.extend(path);
        PathBuf {
            inner,
        }
    }

    pub fn as_path(&self) -> Path {
        Path::new(self.inner.as_str())
    }

    pub fn as_str(&self) -> &str {
        self.inner.as_str()
    }

    pub fn into_string(self) -> ::alloc::string::String {
        self.inner
    }

    pub fn join(&self, child: &Path) -> PathBuf {
        self.as_path().join(child)
    }

    pub fn push(&mut self, child: &Path) {
        self.inner = self.as_path().joined(child.as_str());
    }

    pub fn parent(&self) -> ::core::option::Option<PathBuf> {
        self.as_path().parent()
    }

    pub fn file_name(&self) -> ::core::option::Option<::alloc::string::String> {
        self.as_path().file_name()
    }

    pub fn extension(&self) -> ::core::option::Option<::alloc::string::String> {
        self.as_path().extension()
    }

    pub fn stem(&self) -> ::core::option::Option<::alloc::string::String> {
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
