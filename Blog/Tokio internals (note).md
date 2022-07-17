#Rust
#concurrency 
#finite-state-machine


## Tasks

![[tokio-task.excalidraw|800]]

### Task State

Task state is stored in `Header.state.val: AtomicUsize`
![[tokio-task-state.excalidraw|800]]

These handlers together handles a task(`Header`)'s state, in which:
* RUNNING - Tracks whether the task is currently being polled or cancelled.
* COMPLETE - Is one once the future has fully completed and has been dropped
* NOTIFIED - Tracks whether a Notified object currently exists, i.e., if the task has been pushed in to a _run queue_.
* CANCELLED - Is set to one for tasks that should be cancelled as soon as possible. May take any value for completed tasks.
* JOIN_INTEREST - Is set to one if there exists a `JoinHandle`.
* JOIN_WAKER - Is set to one if the `JoinHandle` has set a waker.


### Task Handlers

>[!NOTE]
>The key design point here is to separate different functionalities of a Task in to different types of handlers

```rust
// <https://docs.rs/tokio/1.19.2/src/tokio/runtime/task/mod.rs.html>

#[repr(transparent)]
pub(crate) struct Task<S: 'static> {
    raw: RawTask,
    _p: PhantomData<S>,
}

#[repr(transparent)]
pub(crate) struct Notified<S: 'static>(Task<S>);

#[repr(transparent)]
pub(crate) struct LocalNotified<S: 'static> {
    task: Task<S>,
    _not_send: PhantomData<*const ()>,
}

pub(crate) struct UnownedTask<S: 'static> {
    raw: RawTask,
    _p: PhantomData<S>,
}
```

![[tokio-task.excalidraw|800]]

* The state field is accessed with atomic instructions.
* The `OwnedTask` reference has exclusive access to the `owned` field.
* The `Notified` reference has exclusive access to the `queue_next` field.
* If COMPLETE is one, then the `JoinHandle` has exclusive access to the


### Create a Task

```rust
// runtime/task/mod.rs

/// This is the constructor for a new task. Three references to the task are
/// created. The first task reference is usually put into an OwnedTasks
/// immediately. The Notified is sent to the scheduler as an ordinary
/// notification.
fn new_task<T, S>(
    task: T,
    scheduler: S,
    id: Id,
) -> (Task<S>, Notified<S>, JoinHandle<T::Output>)
where
    S: Schedule,
    T: Future + 'static,
    T::Output: 'static,
{
    let raw = RawTask::new::<T, S>(task, scheduler, id.clone());
    /* RawTask impl Copy */
    let task = Task {
        raw,
        _p: PhantomData,
    };
    let notified = Notified(Task {
        raw,
        _p: PhantomData,
    });
    let join = JoinHandle::new(raw, id);

    (task, notified, join)
}


// runtime/task/raw.rs
impl RawTask {
    pub(super) fn new<T, S>(task: T, scheduler: S, id: Id) -> RawTask
    where
        T: Future,
        S: Schedule,
    {
        let ptr = Box::into_raw(Cell::<_, S>::new(task, scheduler, State::new(), id));
        let ptr = unsafe { NonNull::new_unchecked(ptr as *mut Header) };

        RawTask { ptr }
    }
}



// runtime/task/core.rs
impl<T: Future, S: Schedule> Cell<T, S> {
    /// Allocates a new task cell, containing the header, trailer, and core
    /// structures.
    pub(super) fn new(future: T, scheduler: S, state: State, task_id: Id) -> Box<Cell<T, S>> {
        #[cfg(all(tokio_unstable, feature = "tracing"))]
        let id = future.id();
        Box::new(Cell {
            header: Header {
                state,
                owned: UnsafeCell::new(linked_list::Pointers::new()),
                queue_next: UnsafeCell::new(None),
                vtable: raw::vtable::<T, S>(),
                owner_id: UnsafeCell::new(0),
                #[cfg(all(tokio_unstable, feature = "tracing"))]
                id,
            },
            core: Core {
                scheduler,
                stage: CoreStage {
                    stage: UnsafeCell::new(Stage::Running(future)),
                },
                task_id,
            },
            trailer: Trailer {
                waker: UnsafeCell::new(None),
            },
        })
    }
}

```

All task data is contained/owned in a Cell struct
```rust
// runtime/task/core.rs

/// The task cell. Contains the components of the task.
///
/// It is critical for `Header` to be the first field as the task structure will
/// be referenced by both *mut Cell and *mut Header.
#[repr(C)]
pub(super) struct Cell<T: Future, S> {
    /// Hot task state data
    pub(super) header: Header,

    /// Either the future or output, depending on the execution stage.
    pub(super) core: Core<T, S>,

    /// Cold data
    pub(super) trailer: Trailer,
}

pub(super) struct Core<T: Future, S> {
    /// Scheduler used to drive this future.
    pub(super) scheduler: S,
    /// Either the future or the output.
    pub(super) stage: CoreStage<T>,
    /// The task's ID, used for populating `JoinError`s.
    pub(super) task_id: Id,
}

pub(super) struct CoreStage<T: Future> {
    stage: UnsafeCell<Stage<T>>,
}

/// Either the future or the output.
pub(super) enum Stage<T: Future> {
    Running(T),
    Finished(super::Result<T::Output>),
    Consumed,
}

```




## Spawning a task

We tracing the runtime by first examine how tasks are spawned, the function `task::spawn(fut)` is just a wrapper of the following function:
```rust
// https://docs.rs/tokio/1.19.2/src/tokio/task/spawn.rs.html#125-137

pub(super) fn spawn_inner<T>(future: T, name: Option<&str>) -> JoinHandle<T::Output>
where
    T: Future + Send + 'static,
    T::Output: Send + 'static,
{
    use crate::runtime::{task, context};
    let id = task::Id::next();
    let spawn_handle = context::spawn_handle().expect(CONTEXT_MISSING_ERROR);
    /* the following func for tracing 
       with the future untouched */
    let task = crate::util::trace::task(future, "task", name, id.as_u64());
    spawn_handle.spawn(task, id)
}
```
we see that a `spawn_handle` extracted from context does the spawn work
```rust
// src: https://docs.rs/tokio/1.19.2/src/tokio/runtime/context.rs.html

thread_local! {
    static CONTEXT: RefCell<Option<Handle>> = RefCell::new(None)
}

pub(crate) fn spawn_handle() -> Option<crate::runtime::Spawner> {
    match CONTEXT.try_with(
        |ctx| (*ctx.borrow())
              .as_ref()
              .map(|ctx| ctx.spawner.clone())
    ) {
        Ok(spawner) => spawner,
        Err(_) => panic!("{}", crate::util::error::THREAD_LOCAL_DESTROYED_ERROR),
    }
}
```
we see the `spawn_handle` in the `spawn_inner()` function is a `runtime::Spawner` and bind to a thread using a thread local variable `context::CONTEXT`.

We got 2 types of spawners
```rust
// src: https://docs.rs/tokio/1.19.2/src/tokio/runtime/spawner.rs.html
pub(crate) enum Spawner {
    Basic(basic_scheduler::Spawner),
    ThreadPool(thread_pool::Spawner),
}
```


### `basic_scheduler::Spawner`

```rust
// <https://docs.rs/tokio/1.19.2/src/tokio/runtime/basic_scheduler.rs.html>
#[derive(Clone)]
pub(crate) struct Spawner {
    shared: Arc<Shared>,
}

impl Spawner {
    /// Spawns a future onto the basic scheduler
    pub(crate) fn spawn<F>(&self, future: F, id: runtime::task::Id) -> JoinHandle<F::Output>
    where
        F: crate::future::Future + Send + 'static,
        F::Output: Send + 'static,
    {
        /* 
          self.shared: Arc<Shared> 
          the call creates a task of the future and pushes it to the
            front of the owned list
        */
        let (handle, notified) = self.shared.owned.bind(future, self.shared.clone(), id);

        if let Some(notified) = notified {
            self.shared.schedule(notified);
        }

        handle
    }
}
```


A `BasicScheduler` ultimately uses a `rutime::basic_scheduler::Shared` to scheduling tasks.

```rust
// <https://docs.rs/tokio/1.19.2/src/tokio/runtime/basic_scheduler.rs.html>

struct Shared {
    /// Remote run queue. None if the `Runtime` has been dropped.
    queue: Mutex<Option<VecDeque<task::Notified<Arc<Shared>>>>>,
    /// Collection of all active tasks spawned onto this executor.
    /* 
      the owned list donot contain Arc<Shared>, its just PhantomData  
    */
    owned: OwnedTasks<Arc<Shared>>,  
    /// Unpark the blocked thread.
    unpark: <Driver as Park>::Unpark,
    /// Indicates whether the blocked on thread was woken.
    woken: AtomicBool,
    /// Handle to I/O driver, timer, blocking pool, ...
    handle_inner: HandleInner
    /// Scheduler configuration options
    config: Config,
    /// Keeps track of various runtime metrics.
    scheduler_metrics: SchedulerMetrics,
    /// This scheduler only has one worker.
    worker_metrics: WorkerMetrics,
}
```

then the call `Shared.owned.bind()` eventually goes to:

```rust
// <https://docs.rs/tokio/1.19.2/src/tokio/runtime/task/list.rs.html>
pub(crate) struct OwnedTasks<S: 'static> {
    inner: Mutex<OwnedTasksInner<S>>,
    id: u64,
}

struct OwnedTasksInner<S: 'static> {
    /* LinkedList<Task<S>, task::Header> */
    list: LinkedList<Task<S>, <Task<S> as Link>::Target>,  
    closed: bool,
}

// <https://docs.rs/tokio/1.19.2/src/tokio/runtime/task/list.rs.html#81-112>
impl<S: 'static> OwnedTasks<S> {
    /// Binds the provided task to this OwnedTasks instance.
    pub(crate) fn bind<T>(
        &self,
        task: T,
        scheduler: S,
        id: super::Id,
    ) -> (JoinHandle<T::Output>, Option<Notified<S>>)
    where
        S: Schedule,
        T: Future + Send + 'static,
        T::Output: Send + 'static,
    {
        /* calls task::new_task to create a new task */
        let (task, notified, join) = super::new_task(task, scheduler, id);

        unsafe {
            // safety: We just created the task, so we have exclusive access
            // to the field.
            task.header().set_owner_id(self.id);
        }

        let mut lock = self.inner.lock();
        if lock.closed {
            drop(lock);
            drop(notified);
            task.shutdown();
            (join, None)
        } else {
            lock.list.push_front(task);
            (join, Some(notified))
        }
    }
}
```




# Driving the runtime

A runtime in Tokio is defined as 
```rust
pub struct Runtime {
    /// Task executor
    kind: Kind,
    /// Handle to runtime, also contains driver handles
    handle: Handle,
    /// Blocking pool handle, used to signal shutdown
    blocking_pool: BlockingPool,
}
enum Kind {
    /// Execute all tasks on the current-thread.
    CurrentThread(BasicScheduler),
    /// Execute tasks across multiple threads.
    #[cfg(feature = "rt-multi-thread")]
    ThreadPool(ThreadPool),
}
#[derive(Clone)]
pub struct Handle {
    pub(super) spawner: Spawner,
}
#[derive(Clone)]
pub(crate) enum Spawner {
    Basic(basic_scheduler::Spawner),
    #[cfg(feature = "rt-multi-thread")]
    ThreadPool(thread_pool::Spawner),
}
```

the runtime's entry point is the function `Runtime::block_on(&rt, future)`
```rust
pub fn block_on<F: Future>(&self, future: F) -> F::Output {
    #[cfg(all(tokio_unstable, feature = "tracing"))]
    let future = crate::util::trace::task(
      future, "block_on", None, task::Id::next().as_u64());
    /* 
      1. enters the runtime context
         this set context::CONTEXT to runtime's handle field
    */
    let _enter = self.enter();  

    match &self.kind {
        Kind::CurrentThread(exec) => exec.block_on(future),
        #[cfg(feature = "rt-multi-thread")]
        Kind::ThreadPool(exec) => exec.block_on(future),
    }
}
```
The function 
1. makes the current thread's thread local spawner be the runtime's spawner
2. call the `block_on()` method on runtime's scheduler.


## Thread local context

```rust
// context.rs
thread_local! {
    static CONTEXT: RefCell<Option<Handle>> = RefCell::new(None)
}


// enter.rs
thread_local! {
    static ENTERED: Cell<EnterContext> = Cell::new(EnterContext::NotEntered)
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum EnterContext {
    #[cfg_attr(not(feature = "rt"), allow(dead_code))]
    Entered { allow_blocking: bool, },
    NotEntered,
}

/// Represents an executor context.
pub(crate) struct Enter {
    _p: PhantomData<RefCell<()>>,
}

impl Enter {
    /// Blocks the thread on the specified future, returning the value with
    /// which that future completes.
    pub(crate) fn block_on<F>(&mut self, f: F) -> Result<F::Output, ParkError>
    where
        F: std::future::Future,
    {
        use crate::park::thread::CachedParkThread;

        let mut park = CachedParkThread::new();
        park.block_on(f)
    }
}

/// Tries to enter a runtime context, returns `None` if already in a runtime
/// context.
pub(crate) fn try_enter(allow_blocking: bool) -> Option<Enter> {
    ENTERED.with(|c| {
        if c.get().is_entered() {
            None
        } else {
            c.set(EnterContext::Entered { allow_blocking });
            Some(Enter { _p: PhantomData })
        }
    })
}
```

## Block On
### future run block on a thread

the module `tokio::park` implement parking and unparking of an OS thread.
Each OS thread contains a thread local variable 
```rust
// defined in src/unpark/thread.rs
thread_local! {
    static CURRENT_PARKER: ParkThread = ParkThread::new();
}
```
and the type `tokio::unpark::thread::CachedParkThread` is a handle of the above thread local `ParkThread` object.
a `block_on` method runs a future on the current thread defined on the handle `CachedParkThread`
```rust
// impl CachedParkThread
pub(crate) fn block_on<F: Future>(&mut self, f: F) -> Result<F::Output, ParkError> {
    use std::task::Context;
    use std::task::Poll::Ready;

    // `get_unpark()` should not return a Result
    let waker = self.get_unpark()?.into_waker();
    let mut cx = Context::from_waker(&waker);

    pin!(f);

    loop {
        /*
            the context `cx` head is an unparker of the current thread
            
            by contract the future's `poll` method should wakeup the context
            before returning `Poll::Pending` 
            and thus the later call of `self.park()` will return immediately 
        */
        if let Ready(v) = crate::coop::budget(|| f.as_mut().poll(&mut cx)) {
            return Ok(v);
        }
        /* this shall not block if the future is 
           well implemented, i.e., call wake() before 
           return Poll::Pending */
        self.park()?;  
    }
}
```



### future run block on a `BasicScheduler`

```rust
// impl BasicScheduler
pub(crate) fn block_on<F: Future>(&self, future: F) -> F::Output {
    pin!(future);

    // Attempt to steal the scheduler core and block_on the future if we can
    // there, otherwise, lets select on a notification that the core is
    // available or the future is complete.
    loop {
        /*
         * the BasicScheduler might be refered 
         *   from multiple threads
         * and the block_on function may be called 
         *   from multiple threads
         */
        if let Some(core) = self.take_core() {
            return core.block_on(future);  /* core: CoreGuard */
        } else {
            /* 
               the enter call here 
               marks the current thread as a 
               worker of runtime
             */
            let mut enter = crate::runtime::enter(false);

            let notified = self.notify.notified();
            pin!(notified);

            if let Some(out) = enter  
                /* 
                  `enter.block_on()` eventurally calls
                  `unpark::thread::CachedParkThread.block_on()`
                   which runs the future on the current thread.
                */
                .block_on(poll_fn(|cx| {
                    if notified.as_mut().poll(cx).is_ready() {
                        return Ready(None);
                    }

                    if let Ready(out) = future.as_mut().poll(cx) {
                        return Ready(Some(out));
                    }

                    Pending
                }))
                .expect("Failed to `Enter::block_on`")
            {
                return out;
            }
        }
    }
}
```


we first inspect what `BasicScheduler.take_core` does
```rust
fn take_core(&self) -> Option<CoreGuard<'_>> {
    /* 
      self.core: AtomicCell<Core> 
      the Core structure holds 
       1. IO driver 
       2. task queue
     */ 
    let core = self.core.take()?;  

    /*
        2 methods defined on a CoreGuard
        - `enter(func)`  sets the core and other scheduler states 
                         in a thread local variable
                         for a temporary duration of excution of 
                         the closure `func` 
        - `Drop::drop()` swaps old thread local scheduler state back
     */
    Some(CoreGuard {
        context: Context {
            spawner: self.spawner.clone(),
            core: RefCell::new(Some(core)),
        },
        basic_scheduler: self,
    })
}
```
and finally the `CoreGuard::block_on()`
```rust
// impl CoreGuard
fn block_on<F: Future>(self, future: F) -> F::Output {
    /*
        `enter(func)`  sets the core and other scheduler states 
                       in a thread local variable
                       for a temporary duration of excution of 
                       the closure `func` 
     */
    let ret = self.enter(|mut core, context| {
        /* 
           the enter call here 
           marks the current thread as a 
           worker of runtime
         */
        let _enter = crate::runtime::enter(false);
        let waker = context.spawner.waker_ref();
        let mut cx = std::task::Context::from_waker(&waker);

        pin!(future);

        /* THE EVENT LOOP */
        'outer: loop {
            if core.spawner.reset_woken() {
                let (c, res) = context.enter(core, || {
                    crate::coop::budget(|| future.as_mut().poll(&mut cx))
                });

                core = c;

                if let Ready(v) = res {
                    return (core, Some(v));
                }
            }

            for _ in 0..core.spawner.shared.config.event_interval {
                // Make sure we didn't hit an unhandled_panic
                if core.unhandled_panic {
                    return (core, None);
                }

                // Get and increment the current tick
                let tick = core.tick;
                core.tick = core.tick.wrapping_add(1);

                let entry = if tick % core.spawner.shared.config.global_queue_interval == 0 {
                    core.spawner.pop().or_else(|| core.tasks.pop_front())
                } else {
                    core.tasks.pop_front().or_else(|| core.spawner.pop())
                };

                let task = match entry {
                    Some(entry) => entry,
                    None => {
                        core = context.park(core);

                        // Try polling the `block_on` future next
                        continue 'outer;
                    }
                };

                let task = context.spawner.shared.owned.assert_owner(task);

                let (c, _) = context.run_task(core, || {
                    task.run();
                });

                core = c;
            }

            // Yield to the driver, this drives the timer and pulls any
            // pending I/O events.
            core = context.park_yield(core);
        }
    });

    match ret {
        Some(ret) => ret,
        None => {
            // `block_on` panicked.
            panic!("a spawned task panicked and the runtime is configured to shut down on unhandled panic");
        }
    }
}

```

### `ThreadPool`


