//! Runtime value system with ownership semantics
//! 
//! This module provides RuntimeValue which wraps AstValue with ownership tracking
//! to support different language runtime semantics (Rust, JavaScript, Python, etc.)

use crate::ast::AstValue;
use crate::{Result, bail};
use std::sync::{Arc, RwLock};
use std::cell::RefCell;
use std::rc::Rc;
use std::fmt::{Debug, Display, Formatter};

/// Runtime value with ownership semantics tracking
#[derive(Debug, Clone)]
pub enum RuntimeValue {
    /// Pure literal value (no ownership semantics)
    Literal(AstValue),
    
    /// Owned value (Rust-style ownership)
    Owned(AstValue),
    
    /// Borrowed immutable reference
    Borrowed { 
        value: AstValue, 
        source: String 
    },
    
    /// Borrowed mutable reference
    BorrowedMut { 
        value: AstValue, 
        source: String 
    },
    
    /// Shared ownership (single-threaded)
    Shared(Rc<RefCell<AstValue>>),
    
    /// Shared ownership (multi-threaded)
    SharedAtomic(Arc<RwLock<AstValue>>),
    
    /// Language-specific extensions
    Extension(Box<dyn RuntimeExtension>),
}

/// Trait for language-specific runtime value extensions
pub trait RuntimeExtension: Debug + Send + Sync {
    /// Name of the extension (e.g., "javascript_object", "python_object")
    fn name(&self) -> &str;
    
    /// Get the underlying value
    fn get_value(&self) -> AstValue;
    
    /// Try to mutate the value
    fn try_mutate(&mut self, f: Box<dyn FnOnce(&mut AstValue) -> Result<()>>) -> Result<()>;
    
    /// Clone this extension
    fn clone_box(&self) -> Box<dyn RuntimeExtension>;
}

impl Clone for Box<dyn RuntimeExtension> {
    fn clone(&self) -> Self {
        self.clone_box()
    }
}

/// Runtime-specific errors
#[derive(Debug, Clone)]
pub enum RuntimeError {
    CannotMutateLiteral,
    CannotMutate,
    BorrowCheckFailed,
    UseAfterMove,
    ExtensionError(String),
    InvalidOperation(String),
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::CannotMutateLiteral => write!(f, "Cannot mutate literal value"),
            RuntimeError::CannotMutate => write!(f, "Cannot mutate this value"),
            RuntimeError::BorrowCheckFailed => write!(f, "Borrow check failed"),
            RuntimeError::UseAfterMove => write!(f, "Use of moved value"),
            RuntimeError::ExtensionError(msg) => write!(f, "Extension error: {}", msg),
            RuntimeError::InvalidOperation(msg) => write!(f, "Invalid operation: {}", msg),
        }
    }
}

impl std::error::Error for RuntimeError {}

impl RuntimeValue {
    // Constructors
    
    /// Create a literal runtime value (no ownership semantics)
    pub fn literal(value: AstValue) -> Self {
        RuntimeValue::Literal(value)
    }
    
    /// Create an owned runtime value
    pub fn owned(value: AstValue) -> Self {
        RuntimeValue::Owned(value)
    }
    
    /// Create a borrowed runtime value
    pub fn borrowed(value: AstValue, source: String) -> Self {
        RuntimeValue::Borrowed { value, source }
    }
    
    /// Create a mutable borrowed runtime value
    pub fn borrowed_mut(value: AstValue, source: String) -> Self {
        RuntimeValue::BorrowedMut { value, source }
    }
    
    /// Create a shared runtime value (single-threaded)
    pub fn shared(value: AstValue) -> Self {
        RuntimeValue::Shared(Rc::new(RefCell::new(value)))
    }
    
    /// Create a shared runtime value (multi-threaded)
    pub fn shared_atomic(value: AstValue) -> Self {
        RuntimeValue::SharedAtomic(Arc::new(RwLock::new(value)))
    }
    
    /// Create an extension runtime value
    pub fn extension(ext: Box<dyn RuntimeExtension>) -> Self {
        RuntimeValue::Extension(ext)
    }
    
    // Access methods
    
    /// Get the underlying value (always works, may clone)
    pub fn get_value(&self) -> AstValue {
        match self {
            RuntimeValue::Literal(v) => v.clone(),
            RuntimeValue::Owned(v) => v.clone(),
            RuntimeValue::Borrowed { value, .. } => value.clone(),
            RuntimeValue::BorrowedMut { value, .. } => value.clone(),
            RuntimeValue::Shared(rc) => rc.borrow().clone(),
            RuntimeValue::SharedAtomic(arc) => arc.read().unwrap().clone(),
            RuntimeValue::Extension(ext) => ext.get_value(),
        }
    }
    
    /// Try to get a reference to the value (zero-copy when possible)
    pub fn try_borrow(&self) -> Result<&AstValue> {
        match self {
            RuntimeValue::Literal(v) => Ok(v),
            RuntimeValue::Owned(v) => Ok(v),
            RuntimeValue::Borrowed { value, .. } => Ok(value),
            RuntimeValue::BorrowedMut { value, .. } => Ok(value),
            _ => bail!("Cannot borrow reference to shared/extension value"),
        }
    }
    
    /// Check if this value can be mutated
    pub fn can_mutate(&self) -> bool {
        match self {
            RuntimeValue::Literal(_) => false,
            RuntimeValue::Borrowed { .. } => false,
            RuntimeValue::Owned(_) | 
            RuntimeValue::BorrowedMut { .. } | 
            RuntimeValue::Shared(_) | 
            RuntimeValue::SharedAtomic(_) |
            RuntimeValue::Extension(_) => true,
        }
    }
    
    /// Try to mutate the value
    pub fn try_mutate<F>(&mut self, f: F) -> Result<()>
    where F: FnOnce(&mut AstValue) -> Result<()>
    {
        match self {
            RuntimeValue::Literal(_) => {
                bail!("Cannot mutate literal value")
            },
            RuntimeValue::Borrowed { .. } => {
                bail!("Cannot mutate this value")
            },
            RuntimeValue::Owned(ref mut v) => {
                f(v)
            },
            RuntimeValue::BorrowedMut { ref mut value, .. } => {
                f(value)
            },
            RuntimeValue::Shared(rc) => {
                let mut borrowed = rc.try_borrow_mut()
                    .map_err(|_| RuntimeError::BorrowCheckFailed)?;
                f(&mut borrowed)
            },
            RuntimeValue::SharedAtomic(arc) => {
                let mut borrowed = arc.try_write()
                    .map_err(|_| "Borrow check failed")?;
                f(&mut borrowed)
            },
            RuntimeValue::Extension(ext) => {
                // For now, we'll use a simplified approach
                let mut value = ext.get_value();
                f(&mut value)?;
                Ok(())
            },
        }
    }
    
    // Ownership operations
    
    /// Move this value (consumes self)
    pub fn take_ownership(self) -> Result<AstValue> {
        match self {
            RuntimeValue::Literal(v) => Ok(v),
            RuntimeValue::Owned(v) => Ok(v),
            RuntimeValue::Borrowed { value, .. } => Ok(value),
            RuntimeValue::BorrowedMut { value, .. } => Ok(value),
            RuntimeValue::Shared(rc) => {
                Ok(Rc::try_unwrap(rc)
                    .map_err(|_| "Borrow check failed")?
                    .into_inner())
            },
            RuntimeValue::SharedAtomic(arc) => {
                Ok(Arc::try_unwrap(arc)
                    .map_err(|_| "Borrow check failed")?
                    .into_inner()
                    .map_err(|_| "Borrow check failed")?)
            },
            RuntimeValue::Extension(ext) => Ok(ext.get_value()),
        }
    }
    
    /// Convert to shared ownership
    pub fn to_shared(self) -> RuntimeValue {
        match self {
            RuntimeValue::Shared(_) => self,
            _ => RuntimeValue::shared(self.get_value()),
        }
    }
    
    /// Convert to atomic shared ownership
    pub fn to_shared_atomic(self) -> RuntimeValue {
        match self {
            RuntimeValue::SharedAtomic(_) => self,
            _ => RuntimeValue::shared_atomic(self.get_value()),
        }
    }
    
    // Type queries
    
    /// Check if this is a literal value
    pub fn is_literal(&self) -> bool {
        matches!(self, RuntimeValue::Literal(_))
    }
    
    /// Check if this is an owned value
    pub fn is_owned(&self) -> bool {
        matches!(self, RuntimeValue::Owned(_))
    }
    
    /// Check if this is a borrowed value
    pub fn is_borrowed(&self) -> bool {
        matches!(self, RuntimeValue::Borrowed { .. } | RuntimeValue::BorrowedMut { .. })
    }
    
    /// Check if this is a shared value
    pub fn is_shared(&self) -> bool {
        matches!(self, RuntimeValue::Shared(_) | RuntimeValue::SharedAtomic(_))
    }
    
    /// Check if this is an extension value
    pub fn is_extension(&self) -> bool {
        matches!(self, RuntimeValue::Extension(_))
    }
}

impl Display for RuntimeValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeValue::Literal(v) => write!(f, "literal({})", v),
            RuntimeValue::Owned(v) => write!(f, "owned({})", v),
            RuntimeValue::Borrowed { value, source } => write!(f, "&{}({})", source, value),
            RuntimeValue::BorrowedMut { value, source } => write!(f, "&mut {}({})", source, value),
            RuntimeValue::Shared(_) => write!(f, "shared({})", self.get_value()),
            RuntimeValue::SharedAtomic(_) => write!(f, "shared_atomic({})", self.get_value()),
            RuntimeValue::Extension(ext) => write!(f, "{}({})", ext.name(), ext.get_value()),
        }
    }
}

impl From<AstValue> for RuntimeValue {
    fn from(value: AstValue) -> Self {
        RuntimeValue::literal(value)
    }
}

impl From<RuntimeValue> for Result<AstValue> {
    fn from(runtime_value: RuntimeValue) -> Self {
        Ok(runtime_value.get_value())
    }
}

// Convenience constructors
impl AstValue {
    /// Convert this AstValue to a literal RuntimeValue
    pub fn to_runtime_literal(self) -> RuntimeValue {
        RuntimeValue::literal(self)
    }
    
    /// Convert this AstValue to an owned RuntimeValue
    pub fn to_runtime_owned(self) -> RuntimeValue {
        RuntimeValue::owned(self)
    }
    
    /// Convert this AstValue to a shared RuntimeValue
    pub fn to_runtime_shared(self) -> RuntimeValue {
        RuntimeValue::shared(self)
    }
}