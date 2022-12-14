use std::str::FromStr;

use eyre::*;
use serde::*;
use tracing::{info, level_filters::LevelFilter};
use tracing_error::ErrorLayer;
use tracing_subscriber::{fmt, prelude::*, EnvFilter};

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum LogLevel {
	Off,
	Trace,
	Debug,
	Info,
	Warn,
	Error,
}
impl LogLevel {
	pub fn as_level_filter(&self) -> LevelFilter {
		match self {
			LogLevel::Error => LevelFilter::ERROR,
			LogLevel::Warn => LevelFilter::WARN,
			LogLevel::Info => LevelFilter::INFO,
			LogLevel::Debug => LevelFilter::DEBUG,
			LogLevel::Trace => LevelFilter::TRACE,
			LogLevel::Off => LevelFilter::OFF,
		}
	}
}
impl FromStr for LogLevel {
	type Err = Error;

	fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
		match s.to_ascii_lowercase().as_ref() {
			"error" => Ok(LogLevel::Error),
			"warn" => Ok(LogLevel::Warn),
			"info" => Ok(LogLevel::Info),
			"debug" => Ok(LogLevel::Debug),
			"trace" => Ok(LogLevel::Trace),
			"off" => Ok(LogLevel::Off),
			_ => Err(eyre!("Invalid log level: {}", s)),
		}
	}
}
impl Default for LogLevel {
	fn default() -> Self {
		LogLevel::Off
	}
}
pub fn setup_logs(log_level: LogLevel) -> Result<()> {
	color_eyre::config::HookBuilder::new()
		.panic_section("ERROR|FATAL program panicked")
		.install()?;
	let filter = EnvFilter::from_default_env()
		.add_directive(log_level.as_level_filter().into());

	let fmt = fmt::layer().with_thread_names(true).with_line_number(true);

	tracing_subscriber::registry()
		.with(fmt)
		.with(filter)
		.with(ErrorLayer::default())
		.try_init()?;

	let compile_mode = match cfg!(debug_assertions) {
		true => "debug",
		false => "release",
	};
	info!("Log level: {:?} compile mode: {} ", log_level, compile_mode);
	Ok(())
}
