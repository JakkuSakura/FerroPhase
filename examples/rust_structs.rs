use serde::{Deserialize, Serialize};
use parse_display::{Display, FromStr};

// Basic event type enum with comprehensive derives
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Hash,
    Serialize,
    Deserialize,
    Display,
    FromStr,
)]
#[repr(u16)]
pub enum EventType {
    Unknown = 0,
    Data = 1,
    Notification = 2,
    Command = 3,
    Response = 4,
    Error = 5,
}

// Simple entity with basic fields
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, Display, FromStr)]
#[display("{value}/{amount} {category}")]
pub struct DataPoint {
    pub value: f64,
    pub amount: f64,
    pub category: String,
}

// Complex event structure with timestamps and metadata
#[derive(Clone, PartialEq, Debug, Deserialize, Serialize, Display, FromStr)]
#[display("{id} created={created_at} updated={updated_at} {status} {value}/{amount} {metadata}")]
pub struct Event {
    pub id: String,
    pub value: f64,
    pub amount: f64,
    pub metadata: String,
    pub status: String,
    pub created_at: u64,
    pub updated_at: u64,
    pub received_at: u64,
}

// Enum with different event variants
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum SystemEvent {
    Data(Event),
    Notification(NotificationEvent),
    Command(CommandEvent),
    Response(ResponseEvent),
}

// Notification event structure
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Display)]
#[display("{id} ts={timestamp} msg={message} lvl={level}")]
pub struct NotificationEvent {
    pub id: String,
    pub timestamp: u64,
    pub message: String,
    pub level: NotificationLevel,
}

// Command event structure
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct CommandEvent {
    pub id: String,
    pub timestamp: u64,
    pub command: String,
    pub parameters: Vec<String>,
}

// Response event structure
#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct ResponseEvent {
    pub id: String,
    pub timestamp: u64,
    pub status_code: u16,
    pub data: Vec<u8>,
}

// Notification level enum
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    Display,
    FromStr,
)]
pub enum NotificationLevel {
    Debug,
    Info,
    Warning,
    Error,
    Critical,
}

// Configuration struct with various field types
#[derive(Clone, Debug, PartialEq)]
pub struct Configuration {
    pub min_value: f64,
    pub max_value: f64,
    pub default_amount: f64,
    pub created_at: u64,
    pub updated_at: u64,
    pub received_at: u64,
}

// Aggregated event collection
#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct EventCollection {
    pub created_at: u64,
    pub updated_at: u64,
    pub received_at: u64,
    pub source: String,
    pub events: Vec<Event>,
}

// Batch container for processing multiple events
pub struct EventBatch {
    processed: u8,
    events: Vec<SystemEvent>,
}

// Trait for event processing
pub trait EventProcessor: Send {
    fn process(&mut self, event: SystemEvent) -> Result<Option<SystemEvent>, String>;
}