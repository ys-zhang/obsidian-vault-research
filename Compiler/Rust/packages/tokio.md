#Rust
#concurrency 


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


A `tokio::runtime::Runtime` also contains


## Code reading


###### runtime struct
```rust
pub struct Runtime {
  /// Task scheduler: `BasicScheduler` or `ThreadPool`
  kind: Kind,    
  /// Handle to runtime, also contains driver handles
  handle: Handle,
  /// Blocking pool handle, used to signal shutdown
  blocking_pool: BlockingPool,
}
```





###### current-thread scheduler runtime

Notice `Runtime.handle` contains a spawner which is created from the scheduler `Runtime.kind`.

```rust
// code for build a runtime with a current-thread scheduler 
fn build_basic_runtime(&mut self) -> io::Result<Runtime> {
  use crate::runtime::basic_scheduler::Config;
  use crate::runtime::{BasicScheduler, HandleInner, Kind};
    
  let (driver, resources) = driver::Driver::new(self.get_cfg())?;
      
  // Blocking pool
  let blocking_pool    = blocking::create_blocking_pool(self, self.max_blocking_threads);
  let blocking_spawner = blocking_pool.spawner().clone();
  
  let handle_inner = HandleInner {
    io_handle: resources.io_handle,
    time_handle: resources.time_handle,
    signal_handle: resources.signal_handle,
    clock: resources.clock,
    blocking_spawner,
  };

  // And now put a single-threaded scheduler on top of the timer. When
  // there are no futures ready to do something, it'll let the timer or
  // the reactor to generate some new stimuli for the futures to continue
  // in their life.
  let scheduler = BasicScheduler::new(
    driver,
    handle_inner,
    Config {
      before_park: self.before_park.clone(),
      after_unpark: self.after_unpark.clone(),
      global_queue_interval: self.global_queue_interval,
      event_interval: self.event_interval,
      #[cfg(tokio_unstable)]
      unhandled_panic: self.unhandled_panic.clone(),
    },
  );
  let spawner = Spawner::Basic(scheduler.spawner().clone());

  Ok(Runtime {
    kind: Kind::CurrentThread(scheduler),
    handle: Handle { spawner },
    blocking_pool,
  })
}
```


Let's see how a _single-threaded scheduler_ works:
```rust
/// Executes tasks on the current thread
pub(crate) struct BasicScheduler {
  /// Core scheduler data is acquired by a thread entering `block_on`.
  core: AtomicCell<Core>,
  /// Notifier for waking up other threads to steal the driver.
  notify: Notify,
  /// Sendable task spawner
  spawner: Spawner,
  /// This is usually None, but right before dropping the BasicScheduler, it
  /// is changed to `Some` with the context being the runtime's own context.
  /// This ensures that any tasks dropped in the `BasicScheduler`s destructor
  /// run in that runtime's context.
  context_guard: Option<EnterGuard>,
}



impl BasicScheduler {
  pub(crate) fn block_on<F: Future>(&self, future: F) -> F::Output {
    pin!(future);
  
    /* Attempt to steal the scheduler core and block_on the
       future if we can there, otherwise, lets select on a 
       notification that the core is available or the future
       is complete. */
  
    loop {
      if let Some(core) = self.take_core() {
        // core is a guard contains the real executor core
        // the method block_on consumes the guard and return 
        // bach the real core when the guard is dropped 
        return core.block_on(future);
      } else {
        let mut enter = crate::runtime::enter(false);
        let notified = self.notify.notified();
        pin!(notified);
        if let Some(out) = enter.block_on(
          poll_fn(|cx| {
            if notified.as_mut().poll(cx).is_ready() { 
              return Ready(None); 
            }
            if let Ready(out) = future.as_mut().poll(cx) {
              return Ready(Some(out));
            }

            Pending
        })).expect("Failed to `Enter::block_on`") {
          return out;
        }
      }
    }
  
  }
}

```


