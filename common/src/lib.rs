#![allow(incomplete_features)]
#![feature(associated_type_defaults)]
#![feature(type_alias_impl_trait)]
#![feature(specialization)]
#![feature(decl_macro)]
#![feature(trait_alias)]
#![feature(once_cell)]

// these are re-exports from other crates
pub extern crate alloc;
extern crate core;

pub use ::tap::*;
pub use async_compat::{Compat, CompatExt};
pub use async_trait::async_trait;
pub use bevy_reflect::{
    self, FromReflect, GetTypeRegistration, Reflect, TypeRegistration, TypeRegistry, Typed,
};
pub use bstr::*;
pub use bytes::{self, Buf, BufMut, Bytes, BytesMut};
pub use cfg_if::cfg_if;
pub use chrono::{
    DateTime, Datelike, NaiveDate, NaiveDateTime, NaiveTime, TimeZone, Timelike, Utc,
};
pub use convert_case::{Case, Casing};
pub use dashmap::*;
pub use derivative::{self, Derivative};
pub use downcast_rs::{self, impl_downcast, Downcast};
pub use eyre::{bail, ensure, eyre, Context as EyreContext, ContextCompat, Error, Result};
pub use futures::{
    future::{
        join_all, lazy, poll_fn as run_fn, poll_immediate as poll_fut, try_join_all, BoxFuture,
        LocalBoxFuture,
    },
    join, pending, pin_mut, poll as poll_once, ready,
    stream::{BoxStream, FuturesOrdered, FuturesUnordered, LocalBoxStream},
    task::{noop_waker, noop_waker_ref, Context as FutureContext, Poll},
    try_join, AsyncBufRead, AsyncBufReadExt, AsyncRead, AsyncReadExt, AsyncWrite, AsyncWriteExt,
    Future, FutureExt, Sink, SinkExt, Stream, StreamExt,
};
pub use http;
pub use itertools::*;
pub use kanal;
pub use lazy_static::*;
pub use minimal_executor::{block_fn, block_on, poll_fn, poll_on};
pub use serde::{self, de::DeserializeOwned, Deserialize, Serialize};
pub use serde_json::{self, Value};
pub use serde_yaml;
pub use static_assertions::*;
pub use std::{
    any::{type_name, Any},
    cell::OnceCell,
    collections::{BTreeMap, BTreeSet, HashMap, HashSet, LinkedList, VecDeque},
    default::Default,
    future::{pending, ready},
    io::{Error as StdIoError, ErrorKind as StdIoErrorKind},
    marker::{PhantomData, Unpin},
    ops::{Deref, DerefMut, Fn, FnMut, FnOnce},
    pin::Pin,
    time::{Duration, Instant, SystemTime, UNIX_EPOCH},
};
pub use strum::{self, *};
pub use strum_macros::{self, *};
pub use thiserror::Error;
pub use tokio::{
    self,
    io::{
        AsyncBufReadExt as TokioAsyncBufReadExt, AsyncReadExt as TokioAsyncReadExt,
        AsyncWriteExt as TokioAsyncWriteExt,
    },
    task::yield_now,
};
pub use tracing::{debug, error, info, instrument, span, trace, warn, Level};
// these are modules of the crate

mod log;

// these are re-exports within the crate
pub use log::*;
