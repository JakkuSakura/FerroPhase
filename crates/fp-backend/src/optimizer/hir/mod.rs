mod dce;

pub use dce::eliminate_dead_code;
pub(crate) use dce::collect_item_refs;
