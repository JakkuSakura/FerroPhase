use serde::de::DeserializeOwned;
use serde_json::Value;
use crate::bail;

pub trait ToJson {
    fn to_json(&self) -> crate::error::Result<Value>;
    fn to_value<T: DeserializeOwned>(&self) -> crate::error::Result<T>
    where
        Self: Sized,
    {
        let json = self.to_json()?;
        let str = serde_json::to_string(&json)?;
        serde_json::from_value(json).map_err(|e| crate::Error::from(e))
    }
}
