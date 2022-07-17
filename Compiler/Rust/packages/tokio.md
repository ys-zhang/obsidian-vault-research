#Rust
#concurrency 
#multithreading
#coroutine


# Common Types

## Channels

Defined in module: `tokio::sync`.

| Channel                                                                   | Producer | Consumer | Volume        | Num of msg |
| ------------------------------------------------------------------------- | -------- | -------- | ------------- | ---------- |
| [mpsc](https://docs.rs/tokio/latest/tokio/sync/mpsc/index.html)           | $n$      | $1$      | bound/unbound | $n$        |
| [broadcast](https://docs.rs/tokio/latest/tokio/sync/broadcast/index.html) [^bc] | $1$      | $n$      | bound/unbound | $n$        |
| [watch](https://docs.rs/tokio/latest/tokio/sync/watch/index.html)[^watch]         | $1$      | $n$      | $0$             | $n$           |
| [oneshot](https://docs.rs/tokio/latest/tokio/sync/oneshot/index.html)     | $1$      | $1$      | $0$             | $1$        |

[^watch]: `watch` only retains the _last_ sent value. This channel is useful for _watching for changes_ to a value from multiple points in the code base, for example, changes to configuration values.
[^bc]: `broadcast` guarantees _all receivers observe all values_ sent by sender

 > [!NOTE]
 > Channels will be closed once the/all senders are dropped, then receivers will get `None` as the last value.


## Notify

[Notify in tokio::sync - Rust (docs.rs)](https://docs.rs/tokio/latest/tokio/sync/struct.Notify.html)

If `notify_one()` is called **before** `notified().await`, then the next call to `notified().await` will complete immediately, consuming the permit. Any subsequent calls to `notified().await` will wait for a new permit.

If `notify_one()` is called **multiple** times before `notified().await`, only a **single** permit is stored. The next call to `notified().await` will complete immediately, but the one after will wait for a new permit.


## IO

Defined in module `tokio::io`

| Traits/Functions              | Meaning                        |
| ----------------------------- | ------------------------------ |
| `AsyncRead`, `AsyncReadExt`   | async ver. of `std::io::Read`  |
| `AsyncWrite`, `AsyncWriteExt` | async ver. of `std::io::Write` |
| `copy(reader, writer)`      | async ver. of `std::io::copy`  |
| `split(rw)`                 | split `rw` into a reader and a writer                               |




# Runtime

following the runtime documentation [`tokio::runtime`](https://docs.rs/tokio/latest/tokio/runtime/index.html)

A runtime must implement the following services:
1. **I/O event loop**: drives I/O resources and dispatches I/O events to tasks that depend on them;
2. **Scheduler**: execute tasks that use these I/O resources;
    1. The **multi-thread scheduler** executes futures on a _thread pool_, using a _work-stealing_ strategy.
    2. The **current-thread scheduler** provides a _single-threaded_ future executor.
3. **Timer**: scheduling work to run after a set period of time. 


## Concepts

1. Each **OS Thread** has a `CURRENT_PARKER` thread local variable which is an instance of `tokio::park::thread::ParkThread`. `CURRENT_PARKER` can be handled using `CachedParkThread` instances which provide function of 
    1. `CachedParkThread::park()` parks the current thread.
    2. `CachedParkThread::block_on(future)` polls the input future on current thread.
2. **Executor** is a thread that works for some runtime instance executing tasks.
    1.  For each OS thread there is a thread local variable `tokio::runtime::enter::ENTERED` works as _a marker of whether the thread is an Executor_.
    2. the function `runtime::enter::enter: fn(bool) -> Enter` _marks the caller thread as an executor_ and returns a guard `Enter` instance which unmarks the caller thread.
    3. the `Enter` instance returned by `runtime::enter::enter` can be used as a handle of the Executor thread to run tasks using `Enter::block_on(future)`, which polls the future using `CachedParkThread::block_on(future)` on current thread
3. **Scheduler**
    1. A `BasicScheduler` can be referenced from multiple OS threads. Each thread has a thread local variable `tokio::runtime::basic_scheduler::CURRENT` which is a `basic_scheduler::Context` instance holding data for executing tasks:
        1. spawner
        2. IO drivers
        3. scheduler run queue
4. **Blocking** in Tokio means blocking the OS thread with the current task, i.e., the thread cannot execute any other tasks until the current task is done.


## Event loop of `BasicScheduler`
if the `BasicScheduler` is used then `Runtime::block_on(future)` will delegate to the method `BasicScheduler::block_on(future)` in which the scheduler tries to steal the scheduler core (i.e. IO drivers and scheduler run queue).

The method `basic_scheduler::CoreGuard::block_on()`  implements the event loop of the scheduler, the actual code is here
```rust
// impl runtime::basic_scheduler::CoreGuard
fn block_on<F: Future>(self, future: F) -> F::Output {
    /* 
     * self.enter set the core to a thread local context variable 
     * then reset the context variable after 
     * complete running the input closure
     */
    let ret = self.enter(|mut core, context| {
        /* marking current thread an executor */
        let _enter = crate::runtime::enter(false); 
        /* the waker wakes up from IO or unpart current thread */
        let waker = context.spawner.waker_ref();   
        let mut cx = std::task::Context::from_waker(&waker);

        pin!(future);

        /* start eventloop */
        'outer: loop {
            /* ----------------------------------------------------------- *\
             * Firstly, poll the entry point for one step further.         *
             * There is a woken flag stored in Spawner.shared object,      *
             * this object is shared across multiple threads indicating    *
             * whether the blocking thread (the thread runnning this func) *
             * was woken.                                                  *
            \* ----------------------------------------------------------- */
            if core.spawner.reset_woken() {
                let (c, res) = context.enter(core, || {
                    crate::coop::budget(|| future.as_mut().poll(&mut cx))
                });

                core = c;

                if let Ready(v) = res {
                    return (core, Some(v));
                }
            }
            /* -----------------------------------------------------
             * The left of the event interval is used for polling 
             * tasks spawned through task::spawn.
             * The scheduler keeps track on 2 task queues:
             * 1. (Local) the local owned running queue owned 
             *    by scheduler,`Core.tasks` accessible only
             *    from current thread
             * 2. (Global) a shared running queue owned 
             *    by spawner`Shared.queue` accessible from
             *    multiple threads
             * ----------------------------------------------------- */
            for _ in 0..core.spawner.shared.config.event_interval {
                // Make sure we didn't hit an unhandled_panic
                if core.unhandled_panic {
                    return (core, None);
                }

                // Get and increment the current tick
                let tick = core.tick;
                core.tick = core.tick.wrapping_add(1);

                let entry = if tick % core.spawner.shared.config.global_queue_interval == 0 {
                    /* After certern amont of time try steel from Global
                       try steel from global queue. */
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

                /* Asserts that the given task is owned by this 
                   OwnedTasks and convert it to a LocalNotified,
                   giving the thread permission to poll this task. */
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


## IO

Tokio used [[Mio - Wrapping epoll]] for IO events.
The most important function of IO driver is `Driver::turn : fn(Driver, Option<Duration>) -> Result<()>`

```rust
fn turn(&mut self, max_wait: Option<Duration>) -> io::Result<()> {
    // How often to call `compact()` on the resource slab
    const COMPACT_INTERVAL: u8 = 255;

    self.tick = self.tick.wrapping_add(1);

    if self.tick == COMPACT_INTERVAL {
        self.resources.compact()
    }

    let mut events = self.events.take().expect("i/o driver event store missing");

    // Block waiting for an event to happen, peeling out how many events
    // happened.
    match self.poll.poll(&mut events, max_wait) 
        /* events are cleared before polling */ 
    { 
        Ok(_) => {}
        Err(ref e) if e.kind() == io::ErrorKind::Interrupted => {}
        Err(e) => return Err(e),
    }

    // Process all the events that came in, dispatching appropriately
    let mut ready_count = 0;
    for event in events.iter() {
        let token = event.token();

        if token != TOKEN_WAKEUP {
            self.dispatch(token, Ready::from_mio(event));
            ready_count += 1;
        }
    }

    self.inner.metrics.incr_ready_count_by(ready_count);

    self.events = Some(events);

    Ok(())
}
```


## Blocking Poll
When dealing with an operation that prevents the task from reaching an `.await` for an extended period of time, e.g.
- CPU bound computation
- Synchronous IO
- Forever loop
in order to prevent starvation possible solutions are:

| Methods/Cases    | CPU-bound computation | Synchronous IO | Running forever |
| ---------------- | --------------------- | -------------- | --------------- |
| `spawn_blocking` | Suboptimal            | Ok             | No              |
| `ryon`           | Ok                    | No             | No              |
| dedicate thread  | Ok                    | Ok             | Ok              |

Tokio uses `runtime::blocking::pool::BlockingPool` instance as a OS thread pool dedicated for blocking tasks.
The thread pool's working threads has a upper limit.


## Thread Pool



# Further reading

1. [[Tokio internals (note)]]
2. [[Tokio internals (Staled)]]
3. [Async: What is blocking? – Alice Ryhl](https://ryhl.io/blog/async-what-is-blocking/)
4. [Reducing tail latencies with automatic cooperative task yielding | Tokio - An asynchronous Rust runtime](https://tokio.rs/blog/2020-04-preemption)