mod dce;

pub(crate) use dce::collect_item_refs;
pub use dce::eliminate_dead_code;
