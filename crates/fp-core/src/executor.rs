#![allow(dead_code)]

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
/// `Poll::Pending` belongs to `CompilerExecutor`, which owns the ready-queue/waker
/// machinery that makes resuming meaningful; this just polls once with a
/// waker that panics if the future isn't ready on that first poll.
#[cfg(test)]
pub(crate) fn block_on<F: Future>(fut: F) -> F::Output {
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
            "test block_on: future returned Poll::Pending -- this helper only \
             supports futures that resolve on the very first poll (tests / synchronous callers \
             with no real package or comptime suspension); drive genuinely suspending futures \
             through CompilerExecutor instead"
        ),
    }
}

/// A minimal, hand-rolled, single-threaded `Future` executor. No I/O, no
/// timers, no thread pool — every future driven here is CPU-bound compiler
/// work that either makes progress immediately or genuinely suspends on
/// another in-flight task (a package finishing its on-demand load, another
/// item's typing/lowering, a comptime value being resolved) waking it later.
/// Tasks are keyed by a caller-chosen `String`, namespaced by phase (e.g.
/// `"typecheck:{def_id}"`, `"mir:{def_id}"`, `"lir:{def_id}"`,
/// `"comptime:{hir_id}"`) so independent phases and callers never collide,
/// and a caller can share (`get_or_spawn`) or replace (`spawn`) an
/// in-flight attempt without tracking a separately allocated id.
///
/// This is shared, uniformly, by `fp-typing` (per-item type-checking),
/// `fp-backend` (per-item HIR->MIR and MIR->LIR lowering), and
/// `fp-compiler` (per-request comptime resolution) — living in `fp-core` so
/// all three can hold the same executor instance without a circular crate
/// dependency.
///
/// Safety note: the `Waker`s this hands out wrap an `Rc`, not an `Arc` — they
/// must never be sent to another thread or otherwise woken from outside the
/// thread that owns this `CompilerExecutor`.
pub struct TaskHandle<T> {
    state: Rc<RefCell<TaskState<T>>>,
}

struct TaskState<T> {
    result: Option<T>,
    wakers: Vec<Waker>,
}

/// Polling *clones* the result rather than consuming it (unlike a plain
/// oneshot channel) so several independent `TaskHandle`s created from the
/// same underlying task (see `get_or_spawn`) can each observe completion —
/// required for two dependents that both need the same not-yet-resolved
/// value to share one in-flight task instead of duplicating the work.
impl<T: Clone> Future for TaskHandle<T> {
    type Output = T;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let mut state = self.state.borrow_mut();
        if let Some(result) = state.result.clone() {
            Poll::Ready(result)
        } else {
            state.wakers.push(cx.waker().clone());
            Poll::Pending
        }
    }
}

pub struct CompilerExecutor {
    inner: Rc<ExecutorState>,
}

#[derive(Clone)]
pub struct ExecutorHandle {
    inner: Rc<ExecutorState>,
}

struct ExecutorState {
    tasks: RefCell<HashMap<String, Pin<Box<dyn Future<Output = ()>>>>>,
    ready: Rc<RefCell<VecDeque<String>>>,
    /// Side table from key to a spawned task's own `TaskState`, type-erased
    /// via `Rc<dyn Any>` (downcast back to `Rc<RefCell<TaskState<T>>>` at
    /// lookup) — lets `get_or_spawn` hand a *second* caller for the same
    /// key a `TaskHandle` backed by the exact same state as the first,
    /// instead of `spawn`'s always-fresh behavior (which would silently
    /// drop the first attempt, per its own doc comment).
    task_states: RefCell<HashMap<String, Rc<dyn std::any::Any>>>,
}

impl CompilerExecutor {
    pub fn new() -> Self {
        Self {
            inner: Rc::new(ExecutorState {
                tasks: RefCell::new(HashMap::new()),
                ready: Rc::new(RefCell::new(VecDeque::new())),
                task_states: RefCell::new(HashMap::new()),
            }),
        }
    }

    pub fn handle(&self) -> ExecutorHandle {
        ExecutorHandle {
            inner: self.inner.clone(),
        }
    }

    pub fn run<F: Future>(&self, future: F) -> F::Output {
        self.inner.run(future)
    }

    pub fn spawn<T: 'static>(
        &self,
        key: impl Into<String>,
        future: impl Future<Output = T> + 'static,
    ) -> TaskHandle<T> {
        self.inner.spawn(key, future)
    }

    /// Like `spawn`, but for callers that may not be the first to need
    /// `key`'s result — if a task is already tracked under `key` (whether
    /// still running or already resolved), returns a `TaskHandle` sharing
    /// its state instead of spawning (and thereby dropping) a duplicate.
    /// `make_future` is only called on a genuine first request.
    pub fn get_or_spawn<T: 'static + Clone>(
        &self,
        key: impl Into<String>,
        make_future: impl FnOnce() -> Pin<Box<dyn Future<Output = T>>>,
    ) -> TaskHandle<T> {
        self.inner.get_or_spawn(key, make_future)
    }

    pub fn contains(&self, key: &str) -> bool {
        self.inner.contains(key)
    }

    pub fn tick(&self) -> Option<String> {
        self.inner.tick()
    }

    pub fn is_idle(&self) -> bool {
        self.inner.is_idle()
    }

    pub fn has_parked_tasks(&self) -> bool {
        self.inner.has_parked_tasks()
    }
}

impl ExecutorHandle {
    pub fn spawn<T: 'static>(
        &self,
        key: impl Into<String>,
        future: impl Future<Output = T> + 'static,
    ) -> TaskHandle<T> {
        self.inner.spawn(key, future)
    }

    /// See `CompilerExecutor::get_or_spawn`.
    pub fn get_or_spawn<T: 'static + Clone>(
        &self,
        key: impl Into<String>,
        make_future: impl FnOnce() -> Pin<Box<dyn Future<Output = T>>>,
    ) -> TaskHandle<T> {
        self.inner.get_or_spawn(key, make_future)
    }

    pub fn contains(&self, key: &str) -> bool {
        self.inner.contains(key)
    }

    pub fn tick(&self) -> Option<String> {
        self.inner.tick()
    }

    pub fn is_idle(&self) -> bool {
        self.inner.is_idle()
    }

    pub fn has_parked_tasks(&self) -> bool {
        self.inner.has_parked_tasks()
    }

    pub fn parked_task_keys(&self) -> Vec<String> {
        self.inner.parked_task_keys()
    }

    pub fn run<F: Future>(&self, future: F) -> F::Output {
        self.inner.run(future)
    }
}

impl ExecutorState {
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
    /// into this same `CompilerExecutor` while they themselves are being polled
    /// (e.g. one const/type-alias item's resolution task discovering it
    /// needs another) -- see `poll_one`'s doc comment for why that's sound.
    pub(crate) fn spawn<T: 'static>(
        &self,
        key: impl Into<String>,
        future: impl Future<Output = T> + 'static,
    ) -> TaskHandle<T> {
        let key = key.into();
        let state = Rc::new(RefCell::new(TaskState {
            result: None,
            wakers: Vec::new(),
        }));
        let task_state = state.clone();
        let task = async move {
            let result = future.await;
            let wakers = {
                let mut state = task_state.borrow_mut();
                state.result = Some(result);
                std::mem::take(&mut state.wakers)
            };
            for waker in wakers {
                waker.wake();
            }
        };
        self.tasks.borrow_mut().insert(key.clone(), Box::pin(task));
        self.task_states.borrow_mut().insert(key.clone(), state.clone());
        self.ready.borrow_mut().push_back(key);
        TaskHandle { state }
    }

    /// See `CompilerExecutor::get_or_spawn`'s doc comment. Looks `key` up in
    /// `task_states` first (covers both a task still running and one
    /// already resolved, since `spawn` registers there too and a resolved
    /// task's `TaskState` is left in place rather than removed); only calls
    /// `make_future`/`spawn` on a genuine miss.
    pub(crate) fn get_or_spawn<T: 'static + Clone>(
        &self,
        key: impl Into<String>,
        make_future: impl FnOnce() -> Pin<Box<dyn Future<Output = T>>>,
    ) -> TaskHandle<T> {
        let key = key.into();
        if let Some(erased) = self.task_states.borrow().get(&key) {
            if let Ok(state) = erased.clone().downcast::<RefCell<TaskState<T>>>() {
                return TaskHandle { state };
            }
            // A different `T` was previously spawned under this key — a
            // caller bug (keys should be namespaced per use site), but
            // fall through to a fresh spawn rather than panicking.
        }
        self.spawn(key, make_future())
    }

    pub(crate) fn contains(&self, key: &str) -> bool {
        self.tasks.borrow().contains_key(key)
    }

    /// Poll the task registered under `key`. Takes the task *out* of the
    /// shared map before polling it, and puts it back afterward only if
    /// still `Pending` — so `self.tasks`'s `RefCell` is never borrowed while
    /// the inner future is actually running. This is what makes it sound
    /// for a task's own body to reentrantly call back into this same
    /// `CompilerExecutor` (`contains`/`spawn`) from within its own poll — holding
    /// the borrow across the inner `.poll()` call (the natural-looking
    /// `get_mut`-based implementation) would double-borrow and panic the
    /// moment a task did that.
    fn poll_one(&self, key: &str) -> Option<Poll<()>> {
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
    pub(crate) fn poll_task(&self, key: &str) -> bool {
        let Some(poll) = self.poll_one(key) else {
            return false;
        };
        match poll {
            Poll::Ready(()) => true,
            Poll::Pending => false,
        }
    }

    /// Poll ready tasks until one resolves or the ready queue drains. Returns
    /// `None` if nothing made progress this round — callers should then
    /// check `has_parked_tasks()` to distinguish "truly idle" from
    /// "everything left is waiting on something that hasn't happened yet".
    pub(crate) fn tick(&self) -> Option<String> {
        loop {
            let key = self.ready.borrow_mut().pop_front()?;
            if !self.tasks.borrow().contains_key(&key) {
                // Woken after the task already resolved or was replaced by a
                // fresh `spawn()` under the same key — stale, skip it.
                continue;
            }
            match self.poll_one(&key) {
                Some(Poll::Ready(())) => return Some(key),
                Some(Poll::Pending) => continue,
                None => continue,
            }
        }
    }

    pub(crate) fn is_idle(&self) -> bool {
        self.tasks.borrow().is_empty()
    }

    /// True when at least one task is suspended and the ready queue is empty
    /// — this round of `tick()` calls made no progress. In this executor
    /// there's no external event source (no I/O, no timers), so if nothing
    /// woke a parked task by the time its waker was registered, nothing ever
    /// will unless driver-level code (e.g. finishing a package load) does so
    /// explicitly — callers use this to detect that condition.
    pub(crate) fn has_parked_tasks(&self) -> bool {
        !self.tasks.borrow().is_empty() && self.ready.borrow().is_empty()
    }

    /// Keys of every task still tracked (i.e. not yet `Ready`) at the
    /// moment of a stall — for diagnostics only (see `has_parked_tasks`'s
    /// doc comment on what "parked" means); not meant to be polled in a
    /// hot loop.
    pub(crate) fn parked_task_keys(&self) -> Vec<String> {
        self.tasks.borrow().keys().cloned().collect()
    }

    fn run<F: Future>(&self, future: F) -> F::Output {
        let mut future = std::pin::pin!(future);
        let waker = Waker::noop();
        let mut context = Context::from_waker(waker);

        loop {
            match future.as_mut().poll(&mut context) {
                Poll::Ready(output) => return output,
                Poll::Pending => {
                    if self.tick().is_none() {
                        panic!("CompilerExecutor stalled while driving a future: no ready task");
                    }
                }
            }
        }
    }
}

impl Default for CompilerExecutor {
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
        let exec = CompilerExecutor::new();
        let handle = exec.spawn("unit", async { 42 });
        let key = exec.tick().expect("should resolve on first poll");
        let out = block_on(handle);
        assert_eq!(key, "unit");
        assert_eq!(out, 42);
        assert!(exec.is_idle());
    }

    #[test]
    fn parks_until_woken_then_resolves() {
        let exec = CompilerExecutor::new();
        let flag = Rc::new(Cell::new(false));
        let wakers = Rc::new(RefCell::new(Vec::new()));
        let handle = exec.spawn(
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

        let key = exec.tick().expect("should resolve once woken");
        let out = block_on(handle);
        assert_eq!(key, "gated");
        assert_eq!(out, "done");
        assert!(exec.is_idle());
    }

    #[test]
    fn respawning_under_same_key_replaces_the_stale_attempt() {
        let exec = CompilerExecutor::new();
        let first_handle = exec.spawn("unit", async { 1 });
        exec.tick().unwrap();
        let first = block_on(first_handle);
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
        let handle = exec.spawn("unit", async { 3 });
        let key = exec.tick().expect("fresh attempt should resolve");
        let out = block_on(handle);
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

    #[test]
    fn get_or_spawn_shares_one_in_flight_task_across_two_callers() {
        let exec = CompilerExecutor::new();
        let flag = Rc::new(Cell::new(false));
        let wakers = Rc::new(RefCell::new(Vec::new()));
        let spawn_count = Rc::new(Cell::new(0u32));

        let make = {
            let flag = flag.clone();
            let wakers = wakers.clone();
            let spawn_count = spawn_count.clone();
            move || {
                spawn_count.set(spawn_count.get() + 1);
                Box::pin(FlagGate {
                    ready: flag.clone(),
                    wakers: wakers.clone(),
                }) as Pin<Box<dyn Future<Output = &'static str>>>
            }
        };
        let first = exec.get_or_spawn("shared", make.clone());
        // A second dependent asking for the same key before the first
        // resolves must NOT spawn a second attempt.
        let second = exec.get_or_spawn("shared", make);
        assert_eq!(spawn_count.get(), 1, "make_future must run only once");

        assert!(exec.tick().is_none(), "should not resolve while gated");
        assert!(exec.has_parked_tasks());

        flag.set(true);
        for waker in wakers.borrow_mut().drain(..) {
            waker.wake();
        }
        exec.tick().expect("should resolve once woken");

        assert_eq!(block_on(first), "done");
        assert_eq!(block_on(second), "done");
        assert_eq!(spawn_count.get(), 1, "still only one real spawn");
    }

    /// A future that awaits another key's `TaskHandle` via `get_or_spawn`,
    /// spawning it lazily as a `FlagGate` the first time it's asked for —
    /// used to build two tasks that mutually depend on each other.
    async fn wait_for(exec: Rc<CompilerExecutor>, key: &'static str) -> &'static str {
        let handle = exec.get_or_spawn(key, || {
            Box::pin(std::future::pending::<&'static str>())
                as Pin<Box<dyn Future<Output = &'static str>>>
        });
        handle.await
    }

    #[test]
    fn mutual_dependency_between_two_tasks_stalls_rather_than_resolving() {
        // Task "a" awaits key "b"; task "b" awaits key "a". Neither key is
        // ever spawned with real work (both `get_or_spawn` calls below
        // register the *other* task's own eventual state, not a fresh
        // pending future), so once both are parked there is no way for
        // either to make progress — this is what a genuine compile-time
        // dependency cycle looks like from the executor's point of view.
        // Driver-level code (the real fixpoint loop) is what turns this
        // signal into a clean diagnostic instead of hanging forever.
        let exec = Rc::new(CompilerExecutor::new());
        let a = exec.spawn("a", wait_for(exec.clone(), "b"));
        let b = exec.spawn("b", wait_for(exec.clone(), "a"));

        // Drain whatever can run; nothing should ever resolve.
        while exec.tick().is_some() {}

        assert!(
            exec.has_parked_tasks(),
            "both tasks should be permanently parked on each other"
        );
        assert!(!exec.is_idle());
        // Confirm this isn't just "hasn't run yet" — ticking repeatedly
        // never makes progress.
        for _ in 0..8 {
            assert!(exec.tick().is_none());
        }
        assert!(exec.has_parked_tasks());
        drop((a, b));
    }
}
