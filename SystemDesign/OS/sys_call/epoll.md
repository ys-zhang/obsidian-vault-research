# The `epoll` API

> All modern applications should use [poll(2)](https://man7.org/linux/man-pages/man2/poll.2.html) or [epoll(7)](https://man7.org/linux/man-pages/man7/epoll.7.html) instead of [select(2)](https://man7.org/linux/man-pages/man2/pselect.2.html).

> [!QUOTE] epoll(7)
> The central concept of the **epoll** API is the **epoll** _instance_, an
> _in-kernel_ data structure which, from a user-space perspective,
> can be considered as _a container for two lists_:
> - The _interest_ list (sometimes also called the **epoll** set): the
>     set of file descriptors that the process has registered an
>     interest in monitoring.
> - The _ready_ list: the set of file descriptors that are "ready"
>     for I/O.  The ready list is a subset of (or, more precisely, a
>     set of references to) the file descriptors in the interest
>     list.  The ready list is dynamically populated by the kernel as
>     a result of I/O activity on those file descriptors.

## Create an epoll instance
```c
#include <sys/epoll.h>

/** 
 * Creates an epoll instance,
 * The file descriptor returned must be closed after usage.
 * The instance will be recycled if all associated descriptors
 *   are closed.
 * 
 * @param size: a hint of interest list size
 *              but is ignored by modern linux versons
 * @return:     epoll instance's file descriptor
 */
int epoll_create(int size);
/**
 * By setting flags = EPOLL_CLOEXEC
 * The file descriptor returned will be closed 
 * after successfull `execve` 
 */
int epoll_create1(int flags);

```


## Wait for ready descriptor

```c
#include <sys/epoll.h>

int epoll_wait(int epfd, /* out */ struct epoll_event *events,
              int maxevents, int timeout);
int epoll_pwait(int epfd, /* out */ struct epoll_event *events,
              int maxevents, int timeout,
              const sigset_t *sigmask);
int epoll_pwait2(int epfd, /* out */ struct epoll_event *events,
              int maxevents, const struct timespec *timeout,
              const sigset_t *sigmask);
```

Arguments:
- The buffer pointed to by `events` is used to return information from the _ready list_ about file descriptors in the _interest list_ that have some events available.
- Up to `maxevents` are returned by `epoll_wait()`.
- The `timeout` argument specifies the number of milliseconds that `epoll_wait()` will block.
    - `timeout=-1` will block the thread indefinitely

Returns the number of file descriptors ready for the requested I/O,


## Modify interest list

```c
#include <sys/epoll.h>

// Applies the operation `op` on the target file descriptor, `fd`.
int epoll_ctl(int epfd, int op, int fd, struct epoll_event *event);
```

Operation options:
1. **EPOLL_CTL_ADD**: Add an entry with settings `event`;
2. **EPOLL_CTL_MOD**: Change the settings;
3. **EPOLL_CTL_DEL**: Remove (deregister) the target file descriptor, `event` is ignored


## Setting Interesting Event 
This specifies what kinds of changes of the file descriptor we are waiting for.

```c
typedef union epoll_data {
    void     *ptr;
    int       fd;
    uint32_t  u32;
    uint64_t  u64;
} epoll_data_t;

struct epoll_event {
    uint32_t     events;    /* Epoll events */
    epoll_data_t data;      /* User data variable */
};
```

The `data` member of the `epoll_event` struct specifies data that the kernel should save and then return (see [[#Wait for ready descriptor]]) when this file descriptor becomes ready.

The `events` member of the `epoll_event` struct is a bit mask composed by:
- **EPOLLIN**: available for read
- **EPOLLOUT**: available for write
- **EPOLLERR**: error happened
- **EPOLLRDHUP**: Stream socket peer closed connection, or shut down writing half of connection
- [[#Polling Mechanisms|Trigger mechanisms]] 
    - [[#level-triggered LT]] is default
    - **EPOLLET**: use [[#edge-triggered ET]]



# Polling Mechanisms

[c - Level vs Edge Trigger Network Event Mechanisms - Stack Overflow](https://stackoverflow.com/questions/1966863/level-vs-edge-trigger-network-event-mechanisms)

> [!Quote]
> The short answer is, edge-triggered means that you get notified only when the event is detected (which takes place, conceptually, in an instant), while level-triggered means you get notified whenever the event is present (which will be true over a period of time). 
> 
> For example, in an edge-triggered system, if you want a notification to signal you when data is available to read, you'll only get that notification when data was not available to read before, but now it is. If you read some of the available data (so that remaining data is still available to read) you would not get another notification, and if you read all of the available data, then you would get another notification when data became available to read again. In a level-triggered system, you'd get that notification _whenever_ data is available to read.

## In Electronics
> Clipped From https://qr.ae/pv4TTI.

Triggering basically means switching. The way in which a switch or an event is triggered can be different, based on the user's requirements.

Edge and Level Triggering, are both common terms when using sequential circuits(flip flops) triggered by an external or internal clock signal.

Essentially, this is what a clock signal looks like:

![[Pasted image 20220707000553.png|600]]

**LEVEL TRIGGERING**:
You can notice two voltage levels, $V_H$ and $V_L$. Those are the two levels at which the event can be triggered. Consider you want to turn on an LED at the positive clock level. What it means is that the LED can turn on at any moment when the voltage is at $V_H$. _This is known as level triggering, where the event is triggered whenever a clock level is encountered. The event may start at any moment during the time for which the clock signal is at the given level._

**EDGE TRIGGERING**:

![[Pasted image 20220707000759.png|600]]

In the image you notice two edges, Rising Edge and Falling Edge, respectively.

_When an event is triggered at a rising/falling edge, it is said to be edge triggered._

Consider the LED you wanted to turn on, but now by (rising) edge triggering. It means that the LED turns on every time the clock makes a transition from $V_L$ to $V_H$ level, and not when it is at the respective levels. The voltage level of the clock doesn't matter much and the LED turns on the moment there is a transition from low voltage level to high voltage level.
 

## edge-triggered (ET)

See this [c - What is the purpose of epoll's edge triggered option? - Stack Overflow](https://stackoverflow.com/questions/9162712/what-is-the-purpose-of-epolls-edge-triggered-option)

An application that employs the **EPOLLET** flag should use
       nonblocking file descriptors (see [open(2) **O_NONBLOCK**](https://man7.org/linux/man-pages/man2/open.2.html)) to avoid having a blocking read or
       write starve a task that is handling multiple file descriptors.
       The suggested way to use **epoll** as an edge-triggered (**EPOLLET**)
       interface is as follows:
1. with nonblocking file descriptors; and
2. by waiting for an event only after [read(2)](https://man7.org/linux/man-pages/man2/read.2.html) or [write(2)](https://man7.org/linux/man-pages/man2/write.2.html) return **EAGAIN**.

> [!NOTE]
> when a file descriptor is opened with `O_NONBLOCK`, IO on this file descriptor will not blocking, which means that 
> 1. IO is performed successfully and returned immediately 
> 2. a `EAGAIN` error is set and IO function returned immediately


## level-triggered (LT)
The default setting for epoll


# See Also
[[Mio - Wrapping epoll]]

