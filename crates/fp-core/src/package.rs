use std::fmt::{self, Display};

use semver::Version;

/// The canonical identity of a package throughout every compiler phase.
///
/// The name alone is not sufficient after dependency resolution: distinct
/// versions or sources of the same named package must remain distinct keys.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct PackageId {
    name: String,
    version: Option<Version>,
    source: Option<String>,
}

impl PackageId {
    pub fn new<S: Into<String>>(name: S) -> Self {
        Self {
            name: name.into(),
            version: None,
            source: None,
        }
    }

    pub fn resolved(name: impl Into<String>, version: Version, source: impl Into<String>) -> Self {
        Self::with_source(name, Some(version), source)
    }

    pub fn with_source(
        name: impl Into<String>,
        version: Option<Version>,
        source: impl Into<String>,
    ) -> Self {
        Self {
            name: name.into(),
            version,
            source: Some(source.into()),
        }
    }

    pub fn as_str(&self) -> &str {
        &self.name
    }

    pub fn version(&self) -> Option<&Version> {
        self.version.as_ref()
    }

    pub fn source(&self) -> Option<&str> {
        self.source.as_deref()
    }
}

impl Display for PackageId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.name.fmt(f)?;
        if let Some(version) = &self.version {
            write!(f, "@{version}")?;
        }
        if let Some(source) = &self.source {
            write!(f, " [{source}]")?;
        }
        Ok(())
    }
}

impl serde::Serialize for PackageId {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        (
            &self.name,
            self.version.as_ref().map(ToString::to_string),
            &self.source,
        )
            .serialize(serializer)
    }
}

impl<'de> serde::Deserialize<'de> for PackageId {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let (name, version, source): (String, Option<String>, Option<String>) =
            serde::Deserialize::deserialize(deserializer)?;
        let version = version
            .map(|version| Version::parse(&version).map_err(serde::de::Error::custom))
            .transpose()?;
        Ok(Self {
            name,
            version,
            source,
        })
    }
}
