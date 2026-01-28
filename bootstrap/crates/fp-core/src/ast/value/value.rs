use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::{Add, Deref, DerefMut, Mul, Sub};


use crate::ast::{
    get_threadlocal_serializer, BExpr, BlockStmt, Expr, Ident, Item, MacroTokenTree,
    QuoteFragmentKind, Ty, TySlot, TypeBounds, TypeStruct, Value,
};
use crate::common_struct;
use crate::span::Span;

/// wrap struct declare with derive Debug, Clone, ,
/// PartialEq, Eq,
/// Hash, PartialOrd, Ord
macro_rules! plain_value {
    ($(#[$attr:meta])* $name:ident) => {
        $(#[$attr])*
        #[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
        pub struct $name;
        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", stringify!($name))
            }
        }
    };
    (no_ord $(#[$attr:meta])* $name:ident: $ty:ty) => {
        $(#[$attr])*
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub struct $name {
            pub value: $ty,
        }
        impl $name {
            pub fn new(v: $ty) -> Self {
                Self { value: v }
            }
        }
        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.value)
            }
        }
    };
    ($(#[$attr:meta])* $name:ident: $ty:ty) => {
        plain_value!(no_ord $(#[$attr])* #[derive(PartialOrd, Ord)] $name: $ty);
    };
}

plain_value! {
    ValueInt: i64
}
// TODO(literal semantics): numeric suffix/bit-width metadata is not carried into `ValueInt`
// (it is still fixed to i64), and there is no overflow checking.
// If we want full support for suffix semantics like `10i32`/`10u8`, we should preserve raw
// literal/suffix metadata in the token/AST and validate/diagnose it in a post-AST stage
// (typing/semantic) based on contextual types.
plain_value! {
    ValueBool: bool
}

#[derive(Debug, Clone)]
pub struct ValueDecimal {
    pub value: f64,
}
// TODO(literal semantics): float suffixes (e.g. `1.0f32`) are not reflected in storage/validation.
// For full semantics, preserve suffix metadata in the token/AST and handle it post-AST.
impl PartialEq for ValueDecimal {
    fn eq(&self, other: &Self) -> bool {
        self.value.total_cmp(&other.value) == std::cmp::Ordering::Equal
    }
}

impl Eq for ValueDecimal {}
impl PartialOrd for ValueDecimal {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.value.total_cmp(&other.value))
    }
}
impl Ord for ValueDecimal {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.value.total_cmp(&other.value)
    }
}
impl Hash for ValueDecimal {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.value.to_bits().hash(state);
    }
}
impl ValueDecimal {
    pub fn new(v: f64) -> Self {
        Self { value: v }
    }
}
impl Display for ValueDecimal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

plain_value! {
    ValueChar: char
}

#[derive(Debug, Clone)]
pub struct ValueString {
    pub value: String,
    pub owned: bool,
}

impl PartialEq for ValueString {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl Eq for ValueString {}

impl Hash for ValueString {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.value.hash(state);
    }
}

impl ValueString {
    pub fn new_owned(s: impl Into<String>) -> Self {
        Self {
            value: s.into(),
            owned: true,
        }
    }
    pub fn new_ref(s: impl Into<String>) -> Self {
        Self {
            value: s.into(),
            owned: false,
        }
    }
}


impl Display for ValueString {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}
common_struct! {
    pub struct ValueList {
        pub values: Vec<Value>,
    }
}
impl ValueList {
    pub fn new(values: Vec<Value>) -> Self {
        Self { values }
    }
}
impl Display for ValueList {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        let mut first = true;
        for value in &self.values {
            if !first {
                write!(f, ", ")?;
            }
            first = false;
            write!(f, "{}", value)?;
        }
        write!(f, "]")
    }
}

common_struct! {
    pub struct ValueMapEntry {
        pub key: Value,
        pub value: Value,
    }
}
impl ValueMapEntry {
    pub fn new(key: Value, value: Value) -> Self {
        Self { key, value }
    }
}

common_struct! {
    pub struct ValueMap {
        pub entries: Vec<ValueMapEntry>,
    }
}
impl ValueMap {
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
        }
    }

    pub fn from_pairs(pairs: impl IntoIterator<Item = (Value, Value)>) -> Self {
        let mut map = Self::new();
        for (key, value) in pairs.into_iter() {
            map.insert(key, value);
        }
        map
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = (&Value, &Value)> {
        self.entries.iter().map(|entry| (&entry.key, &entry.value))
    }

    pub fn get(&self, key: &Value) -> Option<&Value> {
        self.entries
            .iter()
            .find(|entry| &entry.key == key)
            .map(|entry| &entry.value)
    }

    pub fn insert(&mut self, key: Value, value: Value) {
        if let Some(existing) = self.entries.iter_mut().find(|entry| entry.key == key) {
            existing.value = value;
        } else {
            self.entries.push(ValueMapEntry::new(key, value));
        }
    }

    fn key_to_string(value: &Value) -> Option<String> {
        match value {
            Value::String(s) => Some(s.value.clone()),
            Value::Int(i) => Some(i.value.to_string()),
            Value::Bool(b) => Some(b.value.to_string()),
            Value::Char(c) => Some(c.value.to_string()),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum QuoteTokenValue {
    Expr(Expr),
    Stmts(Vec<BlockStmt>),
    Items(Vec<Item>),
    Type(Ty),
}

impl From<Expr> for QuoteTokenValue {
    fn from(value: Expr) -> Self {
        QuoteTokenValue::Expr(value)
    }
}

impl From<Vec<BlockStmt>> for QuoteTokenValue {
    fn from(value: Vec<BlockStmt>) -> Self {
        QuoteTokenValue::Stmts(value)
    }
}

impl From<Vec<Item>> for QuoteTokenValue {
    fn from(value: Vec<Item>) -> Self {
        QuoteTokenValue::Items(value)
    }
}

impl From<Ty> for QuoteTokenValue {
    fn from(value: Ty) -> Self {
        QuoteTokenValue::Type(value)
    }
}

common_struct! {
    pub struct ValueQuoteToken {
        pub kind: QuoteFragmentKind,
        pub value: QuoteTokenValue,
    }
}

common_struct! {
    pub struct ValueTokenStream {
        pub tokens: Vec<MacroTokenTree>,
    }
}

impl ValueTokenStream {
    pub fn span(&self) -> Span {
        fn token_span(tree: &MacroTokenTree) -> Span {
            match tree {
                MacroTokenTree::Token(tok) => tok.span,
                MacroTokenTree::Group(group) => group.span,
            }
        }
        Span::union(self.tokens.iter().map(token_span))
    }
}


impl Display for ValueMap {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        let mut first = true;
        for entry in &self.entries {
            if !first {
                write!(f, ", ")?;
            }
            first = false;
            let key_display =
                Self::key_to_string(&entry.key).unwrap_or_else(|| entry.key.to_string());
            write!(f, "{}: {}", key_display, entry.value)?;
        }
        write!(f, "}}")
    }
}

common_struct! {
    pub struct ValueBytes {
        pub value: Vec<u8>,
    }
}
impl ValueBytes {
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            value: Vec::with_capacity(capacity),
        }
    }
    pub fn zeroed(len: usize) -> Self {
        Self {
            value: vec![0; len],
        }
    }
    pub fn new(value: Vec<u8>) -> Self {
        Self { value }
    }
}
impl<T: Into<Vec<u8>>> From<T> for ValueBytes {
    fn from(values: T) -> Self {
        Self::new(values.into())
    }
}
impl Deref for ValueBytes {
    type Target = Vec<u8>;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}
impl DerefMut for ValueBytes {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ValuePointerKind {
    Unspecified,
    Managed,
    Escaped,
}
common_struct! {
    #[derive(Copy, PartialOrd, Ord, Eq)]
    pub struct ValuePointer {
        pub value: i64,
        pub kind: ValuePointerKind,
    }
}
impl ValuePointer {
    pub fn new(value: i64) -> Self {
        Self {
            value,
            kind: ValuePointerKind::Unspecified,
        }
    }
    pub fn managed(value: i64) -> Self {
        Self {
            value,
            kind: ValuePointerKind::Managed,
        }
    }
    pub fn escaped(value: *const u8) -> Self {
        Self {
            value: value as _,
            kind: ValuePointerKind::Escaped,
        }
    }
    pub fn as_ptr(&self) -> *const u8 {
        self.value as _
    }
    pub fn as_mut_ptr(&mut self) -> *mut u8 {
        self.value as _
    }
}
impl Display for ValuePointer {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "ptr({})", self.value)
    }
}
impl Add<ValueOffset> for ValuePointer {
    type Output = Self;

    fn add(self, rhs: ValueOffset) -> Self::Output {
        Self {
            value: self.value + rhs.value,
            kind: self.kind,
        }
    }
}
impl Sub<ValuePointer> for ValuePointer {
    type Output = ValueOffset;

    fn sub(self, rhs: Self) -> Self::Output {
        ValueOffset {
            value: self.value - rhs.value,
        }
    }
}
impl Sub<ValueOffset> for ValuePointer {
    type Output = Self;

    fn sub(self, rhs: ValueOffset) -> Self::Output {
        Self {
            value: self.value - rhs.value,
            kind: self.kind,
        }
    }
}
plain_value!(ValueOffset: i64);

impl Add<ValueOffset> for ValueOffset {
    type Output = Self;

    fn add(self, rhs: ValueOffset) -> Self::Output {
        Self {
            value: self.value + rhs.value,
        }
    }
}
impl Sub<ValueOffset> for ValueOffset {
    type Output = Self;

    fn sub(self, rhs: ValueOffset) -> Self::Output {
        Self {
            value: self.value - rhs.value,
        }
    }
}
impl Add<ValuePointer> for ValueOffset {
    type Output = ValuePointer;

    fn add(self, rhs: ValuePointer) -> Self::Output {
        ValuePointer {
            value: self.value + rhs.value,
            kind: rhs.kind,
        }
    }
}
impl Mul<ValueInt> for ValueOffset {
    type Output = Self;

    fn mul(self, rhs: ValueInt) -> Self::Output {
        Self {
            value: self.value * rhs.value,
        }
    }
}
plain_value!(ValueUnit);

plain_value!(ValueNull);
plain_value!(ValueUndefined);

common_struct! {
    pub struct ValueEscaped {
        pub ptr: ValuePointer,
        pub size: i64,
        pub align: i64,
        _priv: ()
    }
}
impl ValueEscaped {
    /// Safety and invariants for ValueEscaped
    /// - The memory pointed to by `ptr` is allocated using the layout derived from `size` and `align`.
    /// - The allocation is zero-initialized at creation and freed exactly once in Drop.
    /// - Callers must uphold type safety when using `as_slice[_mut]` and `drop_in_place<T>`.
    /// - `size` and `align` must describe a valid layout (alignment is non-zero and a power of two).
    pub fn new(size: i64, align: i64) -> Self {
        let layout = std::alloc::Layout::from_size_align(size as _, align as _).unwrap();
        let ptr = unsafe { std::alloc::alloc_zeroed(layout) };
        Self {
            ptr: ValuePointer::escaped(ptr),
            size,
            align,
            _priv: (),
        }
    }
    fn as_layout(&self) -> std::alloc::Layout {
        std::alloc::Layout::from_size_align(self.size as _, self.align as _).unwrap()
    }
    pub unsafe fn as_slice(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.ptr.as_ptr(), self.size as _) }
    }
    pub unsafe fn as_slice_mut(&mut self) -> &mut [u8] {
        unsafe { std::slice::from_raw_parts_mut(self.ptr.as_mut_ptr(), self.size as _) }
    }
    pub fn as_ptr(&self) -> *const u8 {
        self.ptr.as_ptr()
    }
    pub fn as_mut_ptr(&mut self) -> *mut u8 {
        self.ptr.as_mut_ptr()
    }
    pub unsafe fn drop_in_place<T>(&mut self) {
        std::ptr::drop_in_place(self.as_mut_ptr() as *mut T);
    }
}
impl Drop for ValueEscaped {
    fn drop(&mut self) {
        let layout = self.as_layout();
        unsafe { std::alloc::dealloc(self.ptr.as_mut_ptr(), layout) }
    }
}

plain_value!(ValueNone);

common_struct! {
    pub struct ValueSome {
        pub value: Box<Value>,
    }
}
impl ValueSome {
    pub fn new(value: Value) -> Self {
        Self {
            value: value.into(),
        }
    }
}
common_struct! {
    pub struct ValueOption {
        pub value: Option<Box<Value >>,
    }
}

impl ValueOption {
    pub fn new(value: Option<Value>) -> Self {
        Self {
            value: value.map(|x| x.into()),
        }
    }
}
#[derive(Debug, Clone, PartialEq, Hash)]
pub struct ValueField {
    pub name: Ident,
    pub value: Value,
}
impl ValueField {
    pub fn new(name: Ident, value: Value) -> Self {
        Self { name, value }
    }
}

common_struct! {
    pub struct ValueStruct {
        pub ty: TypeStruct,
        pub structural: ValueStructural
    }
}
impl ValueStruct {
    pub fn new(ty: TypeStruct, fields: Vec<ValueField>) -> Self {
        Self {
            ty,
            structural: ValueStructural { fields },
        }
    }
}
impl Display for ValueStruct {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ty.name)?;
        write!(f, "{{")?;
        let mut first = true;
        for field in &self.structural.fields {
            if !first {
                write!(f, ", ")?;
            }
            first = false;
            write!(f, "{}: {}", field.name, field.value)?;
        }
        write!(f, "}}")
    }
}
common_struct! {
    pub struct ValueStructural {
        pub fields: Vec<ValueField>,
    }
}
impl ValueStructural {
    pub fn new(fields: Vec<ValueField>) -> Self {
        Self { fields }
    }
    pub fn get_field(&self, name: &Ident) -> Option<&ValueField> {
        self.fields.iter().find(|x| &x.name == name)
    }
}
impl Display for ValueStructural {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        let mut first = true;
        for field in &self.fields {
            if !first {
                write!(f, ", ")?;
            }
            first = false;
            write!(f, "{}: {}", field.name, field.value)?;
        }
        write!(f, "}}")
    }
}
// receiver worth its special treatment
// in C++ and Java, they are emitted
#[derive(Debug, Clone, PartialEq, Hash)]
pub enum FunctionParamReceiver {
    // case in C++
    Implicit,
    Value,
    MutValue,
    Ref,
    RefStatic,
    RefMut,
    RefMutStatic,
}

common_struct! {
    pub struct FunctionParam {
        pub ty_annotation: TySlot,
        pub name: Ident,
        pub ty: Ty,
        pub is_const: bool,
        pub default: Option<Value>,
        /// in Python, *args
        pub as_tuple: bool,
        /// in Python, **kwargs
        pub as_dict: bool,
        /// in Python, parameters before `/`
        pub positional_only: bool,
        /// in Python, parameters after `*`
        pub keyword_only: bool,
    }
}
impl FunctionParam {
    pub fn new(name: Ident, ty: Ty) -> Self {
        Self {
            ty_annotation: None,
            name,
            ty,
            is_const: false,
            default: None,
            as_tuple: false,
            as_dict: false,
            positional_only: false,
            keyword_only: false,
        }
    }

    pub fn ty_annotation(&self) -> Option<&Ty> {
        self.ty_annotation.as_ref()
    }

    pub fn ty_annotation_mut(&mut self) -> &mut TySlot {
        &mut self.ty_annotation
    }

    pub fn set_ty_annotation(&mut self, ty: Ty) {
        self.ty_annotation = Some(ty);
    }

    pub fn span(&self) -> Span {
        Span::union(
            [
                self.ty_annotation.as_ref().map(Ty::span),
                Some(self.ty.span()),
                self.default.as_ref().map(Value::span),
            ]
            .into_iter()
            .flatten(),
        )
    }
}

// TODO: make it enum to support lifetimes, type bounds and const
common_struct! {
    pub struct GenericParam {
        pub name: Ident,
        pub bounds: TypeBounds,
    }
}
impl GenericParam {
    pub fn span(&self) -> Span {
        self.bounds.span()
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum Abi {
    Rust,
    C,
}

impl Default for Abi {
    fn default() -> Self {
        Abi::Rust
    }
}

common_struct! {
    pub struct FunctionSignature {
        pub name: Option<Ident>,
        pub receiver: Option<FunctionParamReceiver>,
        pub params: Vec<FunctionParam>,
        pub generics_params: Vec<GenericParam>,
        pub is_const: bool,
        pub abi: Abi,
        pub quote_kind: Option<QuoteFragmentKind>,
        pub ret_ty: Option<Ty>,
    }
}
impl FunctionSignature {
    pub fn unit() -> Self {
        Self {
            name: None,
            receiver: None,
            params: vec![],
            generics_params: vec![],
            is_const: false,
            abi: Abi::Rust,
            quote_kind: None,
            ret_ty: None,
        }
    }

    pub fn span(&self) -> Span {
        Span::union(
            self.params
                .iter()
                .map(FunctionParam::span)
                .chain(self.ret_ty.as_ref().map(Ty::span))
                .chain(self.generics_params.iter().map(GenericParam::span)),
        )
    }
}

common_struct! {
    pub struct ValueFunction {
        pub sig: FunctionSignature,
        pub body: BExpr,
    }
}
impl ValueFunction {
    pub fn is_runtime_only(&self) -> bool {
        self.generics_params.is_empty()
    }

    pub fn span(&self) -> Span {
        Span::union([self.sig.span(), self.body.span()])
    }
}
impl Deref for ValueFunction {
    type Target = FunctionSignature;

    fn deref(&self) -> &Self::Target {
        &self.sig
    }
}
impl DerefMut for ValueFunction {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.sig
    }
}
impl Display for ValueFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = get_threadlocal_serializer()
            .serialize_value_function(self)
            .unwrap();
        f.write_str(&s)
    }
}
common_struct! {
    pub struct ValueTuple {
        pub values: Vec<Value>,
    }
}
impl ValueTuple {
    pub fn new(values: Vec<Value>) -> Self {
        Self { values }
    }
}

impl Display for ValueTuple {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        let mut first = true;
        for value in &self.values {
            if !first {
                write!(f, ", ")?;
            }
            first = false;
            write!(f, "{}", value)?;
        }
        write!(f, ")")
    }
}
