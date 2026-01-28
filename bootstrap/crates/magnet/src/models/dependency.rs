#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DependencyKind {
    Normal,
    Development,
    Build,
}

#[derive(Debug, Clone)]
pub struct DependencyModel {
    pub name: String,
    pub version: Option<String>,
    pub path: Option<String>,
    pub optional: bool,
    pub features: Vec<String>,
    pub kind: DependencyKind,
}
