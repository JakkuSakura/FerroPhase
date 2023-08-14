use serde::*;
use std::fmt::Debug;
pub mod builtins;

pub use builtins::*;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AddOp<T> {
    pub lhs: Box<T>,
    pub rhs: Box<T>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SubOp<T> {
    pub lhs: Box<T>,
    pub rhs: Box<T>,
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MulOp<T> {
    pub lhs: Box<T>,
    pub rhs: Box<T>,
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DivOp<T> {
    pub lhs: Box<T>,
    pub rhs: Box<T>,
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModOp<T> {
    pub lhs: Box<T>,
    pub rhs: Box<T>,
}
