use std::cell::RefCell;
use std::collections::{HashMap, VecDeque};
use std::future::Future;
use std::pin::Pin;
use std::rc::Rc;
use std::task::{Context, Poll, RawWaker, RawWakerVTable, Waker};

/// Minimal single-poll driver for callers (tests, and any synchronous entry
/// point) that just want a driver-level result right now and know up front
/// that nothing in this call will genuinely suspend (no unloaded package, no
/// pending comptime value). Real suspend/resume across an actual
/// `Poll::Pending` belongs to `Executor`, which owns the ready-queue/waker
/// machinery that makes resuming meaningful; this just polls once with a
/// waker that panics if the future isn't ready on that first poll.
pub fn block_on<F: Future>(fut: F) -> F::Output {
    fn no_wake(_: *const ()) {}
    fn clone_noop_waker(_: *const ()) -> RawWaker {
        RawWaker::new(std::ptr::null(), &VTABLE)
    }
    static VTABLE: RawWakerVTable = RawWakerVTable::new(clone_noop_waker, no_wake, no_wake, |_| {});

    let waker = unsafe { Waker::from_raw(RawWaker::new(std::ptr::null(), &VTABLE)) };
    let mut cx = Context::from_waker(&waker);
    let mut fut = std::pin::pin!(fut);
    match fut.as_mut().poll(&mut cx) {
        Poll::Ready(value) => value,
        Poll::Pending => panic!(
            "fp_core::executor::block_on: future returned Poll::Pending -- this helper only \
             supports futures that resolve on the very first poll (tests / synchronous callers \
             with no real package or comptime suspension); drive genuinely suspending futures \
             through Executor instead"
        ),
    }
}

/// A minimal, hand-rolled, single-threaded `Future` executor. No I/O, no
/// timers, no thread pool — every future driven here is CPU-bound compiler
/// work that either makes progress immediately or genuinely suspends on
/// another in-flight task (a package finishing its on-demand load, a
/// comptime value being resolved) waking it later. Tasks are keyed by a
/// caller-chosen `String` (e.g. a compile unit's `FullyQualifiedPath::to_key()`,
/// or a const/type-alias item's name) so a caller can spawn a fresh attempt
/// under the same key without having to track a separately allocated id.
///
/// Safety note: the `Waker`s this hands out wrap an `Rc`, not an `Arc` — they
/// must never be sent to another thread or otherwise woken from outside the
/// thread that owns this `Executor`.
pub struct Executor<O> {
    tasks: RefCell<HashMap<String, Pin<Box<dyn Future<Output = O>>>>>,
    ready: Rc<RefCell<VecDeque<String>>>,
}

impl<O> Executor<O> {
    pub fn new() -> Self {
        Self {
            tasks: RefCell::new(HashMap::new()),
            ready: Rc::new(RefCell::new(VecDeque::new())),
        }
    }

    /// Insert (or replace) the task registered under `key`, marking it ready
    /// to poll. Only call this when `key` isn't already tracked (see
    /// `contains`) -- the whole point of keying tasks is that a compile
    /// unit's typing attempt is spawned *once* and then polled repeatedly
    /// (via `poll_task`) until it resolves, resuming exactly where it
    /// suspended each time rather than restarting. Replacing an in-flight
    /// task under the same key drops the old future (and whatever it had
    /// suspended on) -- only do that deliberately (e.g. a genuine restart).
    ///
    /// Takes `&self` (not `&mut self`): tasks routinely spawn *other* tasks
    /// into this same `Executor` while they themselves are being polled
    /// (e.g. one const/type-alias item's resolution task discovering it
    /// needs another) -- see `poll_one`'s doc comment for why that's sound.
    pub fn spawn(&self, key: impl Into<String>, future: impl Future<Output = O> + 'static) {
        let key = key.into();
        self.tasks
            .borrow_mut()
            .insert(key.clone(), Box::pin(future));
        self.ready.borrow_mut().push_back(key);
    }

    pub fn contains(&self, key: &str) -> bool {
        self.tasks.borrow().contains_key(key)
    }

    /// Poll the task registered under `key`. Takes the task *out* of the
    /// shared map before polling it, and puts it back afterward only if
    /// still `Pending` — so `self.tasks`'s `RefCell` is never borrowed while
    /// the inner future is actually running. This is what makes it sound
    /// for a task's own body to reentrantly call back into this same
    /// `Executor` (`contains`/`spawn`) from within its own poll — holding
    /// the borrow across the inner `.poll()` call (the natural-looking
    /// `get_mut`-based implementation) would double-borrow and panic the
    /// moment a task did that.
    fn poll_one(&self, key: &str) -> Option<Poll<O>> {
        let mut task = self.tasks.borrow_mut().remove(key)?;
        let waker = {
            let wake_key = key.to_string();
            let ready = self.ready.clone();
            make_waker(move || ready.borrow_mut().push_back(wake_key.clone()))
        };
        let mut cx = Context::from_waker(&waker);
        match task.as_mut().poll(&mut cx) {
            Poll::Ready(output) => Some(Poll::Ready(output)),
            Poll::Pending => {
                self.tasks.borrow_mut().insert(key.to_string(), task);
                Some(Poll::Pending)
            }
        }
    }

    /// Poll the task registered under `key` directly, regardless of whether
    /// its waker has fired -- the caller (the driver, synchronously handling
    /// one compile unit) already knows exactly which task it wants an answer
    /// for right now, so there's no need to go through the ready-queue
    /// indirection `tick()` uses for "poll whatever's next". Returns `None`
    /// if `key` isn't tracked at all (caller should `spawn` first) or the
    /// task is still pending.
    pub fn poll_task(&self, key: &str) -> Option<O> {
        match self.poll_one(key)? {
            Poll::Ready(output) => Some(output),
            Poll::Pending => None,
        }
    }

    /// Poll ready tasks until one resolves or the ready queue drains. Returns
    /// `None` if nothing made progress this round — callers should then
    /// check `has_parked_tasks()` to distinguish "truly idle" from
    /// "everything left is waiting on something that hasn't happened yet".
    pub fn tick(&self) -> Option<(String, O)> {
        loop {
            let key = self.ready.borrow_mut().pop_front()?;
            if !self.tasks.borrow().contains_key(&key) {
                // Woken after the task already resolved or was replaced by a
                // fresh `spawn()` under the same key — stale, skip it.
                continue;
            }
            match self.poll_one(&key) {
                Some(Poll::Ready(output)) => return Some((key, output)),
                Some(Poll::Pending) => continue,
                None => continue,
            }
        }
    }

    pub fn is_idle(&self) -> bool {
        self.tasks.borrow().is_empty()
    }

    /// True when at least one task is suspended and the ready queue is empty
    /// — this round of `tick()` calls made no progress. In this executor
    /// there's no external event source (no I/O, no timers), so if nothing
    /// woke a parked task by the time its waker was registered, nothing ever
    /// will unless driver-level code (e.g. finishing a package load) does so
    /// explicitly — callers use this to detect that condition.
    pub fn has_parked_tasks(&self) -> bool {
        !self.tasks.borrow().is_empty() && self.ready.borrow().is_empty()
    }
}

impl<O> Default for Executor<O> {
    fn default() -> Self {
        Self::new()
    }
}

struct WakeData {
    wake: Box<dyn Fn()>,
}

static VTABLE: RawWakerVTable = RawWakerVTable::new(clone_raw, wake_raw, wake_by_ref_raw, drop_raw);

unsafe fn clone_raw(data: *const ()) -> RawWaker {
    unsafe {
        Rc::increment_strong_count(data as *const WakeData);
    }
    RawWaker::new(data, &VTABLE)
}

unsafe fn wake_raw(data: *const ()) {
    unsafe {
        wake_by_ref_raw(data);
        drop(Rc::from_raw(data as *const WakeData));
    }
}

unsafe fn wake_by_ref_raw(data: *const ()) {
    let wake_data = unsafe { &*(data as *const WakeData) };
    (wake_data.wake)();
}

unsafe fn drop_raw(data: *const ()) {
    unsafe {
        drop(Rc::from_raw(data as *const WakeData));
    }
}

/// Build a `Waker` around a plain closure. Type-erasing via `Box<dyn Fn()>`
/// (rather than a generic `RawWakerVTable` per key/output type) keeps the
/// vtable a single ordinary `static` — no per-instantiation statics or
/// generic `unsafe fn`s to reason about.
fn make_waker(wake: impl Fn() + 'static) -> Waker {
    let data = Rc::into_raw(Rc::new(WakeData {
        wake: Box::new(wake),
    })) as *const ();
    unsafe { Waker::from_raw(RawWaker::new(data, &VTABLE)) }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::cell::Cell;

    /// A future that stays `Pending` until an external flag is flipped, then
    /// resolves — proves the executor's suspend/wake/ready-queue plumbing in
    /// isolation, without depending on any real compiler behavior.
    struct FlagGate {
        ready: Rc<Cell<bool>>,
        wakers: Rc<RefCell<Vec<Waker>>>,
    }

    impl Future for FlagGate {
        type Output = &'static str;
        fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
            if self.ready.get() {
                Poll::Ready("done")
            } else {
                self.wakers.borrow_mut().push(cx.waker().clone());
                Poll::Pending
            }
        }
    }

    #[test]
    fn resolves_immediately_when_never_pending() {
        let mut exec: Executor<i32> = Executor::new();
        exec.spawn("unit", async { 42 });
        let (key, out) = exec.tick().expect("should resolve on first poll");
        assert_eq!(key, "unit");
        assert_eq!(out, 42);
        assert!(exec.is_idle());
    }

    #[test]
    fn parks_until_woken_then_resolves() {
        let mut exec: Executor<&'static str> = Executor::new();
        let flag = Rc::new(Cell::new(false));
        let wakers = Rc::new(RefCell::new(Vec::new()));
        exec.spawn(
            "gated",
            FlagGate {
                ready: flag.clone(),
                wakers: wakers.clone(),
            },
        );

        assert!(
            exec.tick().is_none(),
            "should not resolve while the gate is closed"
        );
        assert!(exec.has_parked_tasks());

        flag.set(true);
        for waker in wakers.borrow_mut().drain(..) {
            waker.wake();
        }

        let (key, out) = exec.tick().expect("should resolve once woken");
        assert_eq!(key, "gated");
        assert_eq!(out, "done");
        assert!(exec.is_idle());
    }

    #[test]
    fn respawning_under_same_key_replaces_the_stale_attempt() {
        let mut exec: Executor<i32> = Executor::new();
        exec.spawn("unit", async { 1 });
        let (_, first) = exec.tick().unwrap();
        assert_eq!(first, 1);

        // A second, never-resolving attempt gets parked...
        let flag = Rc::new(Cell::new(false));
        let wakers = Rc::new(RefCell::new(Vec::new()));
        exec.spawn(
            "unit",
            FlagGateInt {
                ready: flag.clone(),
                wakers: wakers.clone(),
            },
        );
        assert!(exec.tick().is_none());
        assert!(exec.has_parked_tasks());

        // ...and respawning under the same key drops it, without needing to
        // wake it first.
        exec.spawn("unit", async { 3 });
        let (key, out) = exec.tick().expect("fresh attempt should resolve");
        assert_eq!(key, "unit");
        assert_eq!(out, 3);
        assert!(
            exec.is_idle(),
            "stale parked attempt must be gone, not just superseded"
        );
    }

    struct FlagGateInt {
        ready: Rc<Cell<bool>>,
        wakers: Rc<RefCell<Vec<Waker>>>,
    }

    impl Future for FlagGateInt {
        type Output = i32;
        fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
            if self.ready.get() {
                Poll::Ready(2)
            } else {
                self.wakers.borrow_mut().push(cx.waker().clone());
                Poll::Pending
            }
        }
    }
}
