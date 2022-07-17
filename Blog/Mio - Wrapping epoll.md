#blog 
#concurrency 
#code-reading 
#Rust 


>[!TLDR]
> All polling mechanisms are implemented using syscalls, such as `epoll` and `eventfd`, i.e., `mio` is just a wrapper over syscalls


# Misc.
The sys call macro 
```rust
macro_rules! syscall {
    ($fn: ident ( $($arg: expr),* $(,)* ) ) => {{
        let res = unsafe { libc::$fn($($arg, )*) };
        if res == -1 {
            Err(std::io::Error::last_os_error())
        } else {
            Ok(res)
        }
    }};
}
```


# Wrapping `epoll`
For prerequisite knowledge see [[epoll]]

```rust
// Interest is rust counterpart of `libc::epoll_event.events`
// src: https://github.com/tokio-rs/mio/blob/v0.8.4/src/interest.rs
pub struct Interest(std::num::NonZeroU8);


// Convert interest to unix `epoll_event` struct
// src: https://github.com/tokio-rs/mio/blob/v0.8.4/src/sys/unix/selector/epoll.rs
fn interests_to_epoll(interests: Interest) -> u32 {
    let mut kind = EPOLLET;   /* edge-triggered is used */

    if interests.is_readable() {
        kind = kind | EPOLLIN | EPOLLRDHUP;
    }

    if interests.is_writable() {
        kind |= EPOLLOUT;
    }

    kind as u32
}


/* A selector is just a wrapper of unix epoll instance */
impl Selector {

    pub fn register(&self, fd: RawFd, token: Token, interests: Interest) -> io::Result<()> {
        let mut event = libc::epoll_event {
            events: interests_to_epoll(interests),
            /* A token is binded to the event as an identifier */
            u64: usize::from(token) as u64,
        };  
        syscall!(epoll_ctl(self.ep, libc::EPOLL_CTL_ADD, fd, &mut event)).map(|_| ())
    }


    pub fn select(&self, events: &mut Events, timeout: Option<Duration>) -> io::Result<()> {
        /* type Events = Vec<libc::epoll_event>; */
        events.clear();
        syscall!(epoll_wait(
            /* this is the fd for the wrapped epoll instance*/
            self.ep,  
            events.as_mut_ptr(),
            events.capacity() as i32,
            timeout.unwrap_or(-1);,
        ))
        .map(|n_events| {
            // This is safe because `epoll_wait` ensures that `n_events` are
            // assigned.
            unsafe { events.set_len(n_events as usize) };
        })
    }

}
```


# The `Event` struct 

The `Event` struct is just a wrapper of `libc::epoll_event` in Linux
``` rust 
// src: https://github.com/tokio-rs/mio/blob/v0.8.4/src/sys/unix/selector/epoll.rs
pub mod sys {
  pub type Event = libc::epoll_event;
}

// src: https://github.com/tokio-rs/mio/blob/master/src/event/event.rs
pub struct Event {
    inner: sys::Event,
}
```

A `Events` is  a collection of __readiness__ events, which is just a wrapper of a vector of `libc::epoll_event` in Linux
```rust 
// src: https://github.com/tokio-rs/mio/blob/v0.8.4/src/sys/unix/selector/epoll.rs
pub mod sys {
  pub type Events = Vec<libc::epoll_event>;
}

// src: https://github.com/tokio-rs/mio/blob/master/src/event/events.rs
pub struct Events {
    inner: sys::Events,
}
```


# Event Sources

A `Registry` can register `event::Source` which is a trait can be registered an deregistered
```rust
pub trait Source {
    /*
     * binds a source and interest to a token
     */
    fn register(
        &mut self,
        registry: &Registry,
        token: Token,
        interests: Interest,
    ) -> io::Result<()>;

    fn reregister(
        &mut self,
        registry: &Registry,
        token: Token,
        interests: Interest,
    ) -> io::Result<()>;

    fn deregister(&mut self, registry: &Registry) -> io::Result<()>;
}
```

`Source` is implemented for `IoSource` which wraps a ref to a file descriptor
```rust
pub struct IoSource<T> {
    state: IoSourceState,
    inner: T,  /* this is the wrapped fd*/
    #[cfg(debug_assertions)]
    selector_id: SelectorId,
}
```
The `mio::UdpSocket`, `mio::TcpSocket` etc. wraps a `IoSource<net::UdpSocket>` and `IoSource<net::TcpSocket>` respectively:
```rust
pub struct UdpSocket {
    inner: IoSource<net::UdpSocket>,
}
```


# `Waker` 
waker allows wake up a poll from another thread.
In Linux `Waker` is implemented through [eventfd(2)](https://man7.org/linux/man-pages/man2/eventfd.2.html)

> The kernel overhead of an `eventfd` file descriptor is
> much lower than that of a pipe, and only one file descriptor is
> required (versus the two required for a pipe).



# The `Poll` struct

A `Poll` is just a wrapper of epoll instance in Linux
```rust
pub struct Poll {
    registry: Registry,
}

pub struct Registry {
    selector: sys::Selector,
}

mod sys {
  struct Selector {
    ep: std::os::unix::io::RawFd
  }
}

impl Poll {
    pub fn poll(&mut self, events: &mut Events, timeout: Option<Duration>) -> io::Result<()> {
        self.registry.selector.select(events.inner, timeout)
    }
}

```

`Poll.poll()` eventually calls `epoll_wait(2)` in Linux.

