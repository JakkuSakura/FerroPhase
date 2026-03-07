use std::path::PathBuf;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EmitKind {
    Object,
    Executable,
    AssemblyText,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NativeTarget {
    X86_64,
    Aarch64,
}

impl NativeTarget {
    pub fn parse(value: &str) -> Option<Self> {
        let value = value.trim().to_ascii_lowercase();
        match value.as_str() {
            "x86_64" | "amd64" => Some(Self::X86_64),
            "aarch64" | "arm64" => Some(Self::Aarch64),
            _ => None,
        }
    }

    pub fn resolve(value: &str, target_triple: Option<&str>) -> Option<Self> {
        let _ = target_triple;
        Self::parse(value)
    }
}

#[derive(Debug, Clone)]
pub struct NativeConfig {
    pub emit: EmitKind,
    pub output_path: PathBuf,
    pub linker_args: Vec<String>,
    pub keep_object: bool,
    pub asm_dump: Option<PathBuf>,
    pub native_target: Option<NativeTarget>,

    /// Target triple for codegen/linking (e.g. x86_64-apple-darwin).
    pub target_triple: Option<String>,
    /// Target CPU (e.g. "native"), forwarded into LLVM target config.
    pub target_cpu: Option<String>,
    /// Target feature string (e.g. "+avx2"), forwarded into LLVM target config.
    pub target_features: Option<String>,
    /// Optional sysroot for cross compilation.
    pub sysroot: Option<PathBuf>,
    /// Linker driver to invoke. Currently unused by the in-process linker.
    pub linker_driver: Option<String>,
    /// Explicit link-editor override. Currently unused by the in-process linker.
    pub fuse_ld: Option<PathBuf>,
    /// Whether this is a release build (enables -O2 at minimum).
    pub release: bool,
}

impl NativeConfig {
    pub fn executable(output_path: impl Into<PathBuf>) -> Self {
        Self {
            emit: EmitKind::Executable,
            output_path: output_path.into(),
            linker_args: Vec::new(),
            keep_object: false,
            asm_dump: None,
            native_target: None,

            target_triple: None,
            target_cpu: None,
            target_features: None,
            sysroot: None,
            linker_driver: None,
            fuse_ld: None,
            release: false,
        }
    }

    pub fn object(output_path: impl Into<PathBuf>) -> Self {
        Self {
            emit: EmitKind::Object,
            output_path: output_path.into(),
            linker_args: Vec::new(),
            keep_object: true,
            asm_dump: None,
            native_target: None,

            target_triple: None,
            target_cpu: None,
            target_features: None,
            sysroot: None,
            linker_driver: None,
            fuse_ld: None,
            release: false,
        }
    }

    pub fn assembly(output_path: impl Into<PathBuf>) -> Self {
        Self {
            emit: EmitKind::AssemblyText,
            output_path: output_path.into(),
            linker_args: Vec::new(),
            keep_object: false,
            asm_dump: None,
            native_target: None,

            target_triple: None,
            target_cpu: None,
            target_features: None,
            sysroot: None,
            linker_driver: None,
            fuse_ld: None,
            release: false,
        }
    }

    pub fn with_linker_args(mut self, args: Vec<String>) -> Self {
        self.linker_args.extend(args);
        self
    }

    pub fn with_target_triple(mut self, triple: Option<String>) -> Self {
        self.target_triple = triple;
        self
    }

    pub fn with_target_cpu(mut self, cpu: Option<String>) -> Self {
        self.target_cpu = cpu;
        self
    }

    pub fn with_target_features(mut self, features: Option<String>) -> Self {
        self.target_features = features;
        self
    }

    pub fn with_sysroot(mut self, sysroot: Option<PathBuf>) -> Self {
        self.sysroot = sysroot;
        self
    }

    pub fn with_linker_driver(mut self, driver: Option<String>) -> Self {
        self.linker_driver = driver;
        self
    }

    pub fn with_fuse_ld(mut self, fuse_ld: Option<PathBuf>) -> Self {
        self.fuse_ld = fuse_ld;
        self
    }

    pub fn with_release(mut self, release: bool) -> Self {
        self.release = release;
        self
    }

    pub fn with_keep_object(mut self, keep: bool) -> Self {
        self.keep_object = keep;
        self
    }

    pub fn with_asm_dump(mut self, dump: Option<PathBuf>) -> Self {
        self.asm_dump = dump;
        self
    }

    pub fn with_native_target(mut self, target: Option<NativeTarget>) -> Self {
        self.native_target = target;
        self
    }
}
