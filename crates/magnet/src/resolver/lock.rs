use crate::models::{LockIndex, LockedRegistryVersion};
use semver::VersionReq;

pub fn match_locked_registry(
    lock_index: Option<&LockIndex>,
    name: &str,
    req: &VersionReq,
) -> Option<LockedRegistryVersion> {
    lock_index.and_then(|index| index.match_registry(name, req))
}
