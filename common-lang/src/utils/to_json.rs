use common::EyreContext;
use serde::de::DeserializeOwned;
use serde_json::Value;

pub trait ToJson {
    fn to_json(&self) -> common::Result<Value>;
    fn to_value<T: DeserializeOwned>(&self) -> common::Result<T>
    where
        Self: Sized,
    {
        let json = self.to_json()?;
        let str = serde_json::to_string(&json)?;
        Ok(serde_json::from_value(json).with_context(|| format!("value: {}", str))?)
    }
}
