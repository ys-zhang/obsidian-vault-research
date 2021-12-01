# Design pattern

> `fork` then `exec`/`execvp`.
> Almost every `fork` followed by an `exec`.

Actually Windows decide to use `CreateProcessW`, which has a semantic with combining `fork` and `exec`, to replace `fork`.

Unix/Linux, uses `fork`-`exec` to simplify syscall interfaces.

## Process state changes

Three syscall intends to wait for process _state changes_, if state changes already happened, they will return immediately:
- `pid_t wait(int* wstatus);` waits for termination;
- `pid_t waitpid(pid_t pid, int* wstatus, int options);`
- `int waitid(...)`

_State change_ includes:
1.  process terminate;
2.  process stopped by a signal;
3.  process resumed by a signal.

# Pitfalls
[CS 110L: Pitfalls in multiprocessing](https://reberhardt.com/cs110l/spring-2020/lecture-notes/lecture-07/)

[CommandExt in `std::os::unix::process` - Rust (rust-lang.org)](https://doc.rust-lang.org/std/os/unix/process/trait.CommandExt.html#notes-and-safety)

## `fork`

```ad-warning
title: Get concurrent execution
It's is great danger if run another piece of your own program at the same time. 

You should run a seperate executable in the child process.
```

```ad-warning 
title: allocate memory in child process
see [[fork#Warnings]].
```

1. Accidentally nesting forks when spawning multiple child process.
2. Runaway children: forget to `exit`....
3. Using data structures when threads are involved.
4. Failure to clean up: forget to `wait`.


> The general problem with making `fork()` work in a multi-threaded world is what to do with all of the threads. There are two alternatives. _One is to copy all of the threads into the new process_. This causes the programmer or implementation to deal with threads that are suspended on system calls or that might be about to execute system calls that should not be executed in the new process. _The other alternative is to copy only the thread that calls `fork()`_. This creates the difficulty that the state of process-local resources is usually held in process memory. If a thread that is not calling fork() holds a resource, that resource is never released in the child process because the thread whose job it is to release the resource does not exist in the child process. 
> 
> When a programmer is writing a multi-threaded program, the first described use of `fork()`, creating new threads in the same program, is provided by the `pthread_create()` function. ***The `fork()` function is thus used only to run new programs***, and the effects of calling functions that require certain resources between the call to `fork()` and the call to an exec function are undefined.
> 
> From [fork (opengroup.org)](https://pubs.opengroup.org/onlinepubs/9699919799/functions/fork.html)



## `pipe`

1. Leak file descriptors: forget to `close(fd)`;
2. Calling `close` on bad values.
    - `if (close(fd == -1)) {...}`
3. Use before `pipe`.
4. Use after `close`.

## signal safety

[signal-safety(7) - Linux manual page (man7.org)](https://man7.org/linux/man-pages/man7/signal-safety.7.html)

An ___async-signal-safe___ function is one that can be safely called from within a _signal handler_.

All functions rely on global data is not _async-signal-safe_, as when signal is delivered, the original call may not finished, then the handler jumps in using an undefined/uncleared state of global vars.

> 当捕捉到信号时，不论进程的主控制流程当前执行到哪儿，都会先跳到信号处理函数中执行, 从信号处理函数返回后再继续执行主控制流程。信号处理函数是一个单独的控制流程，因为 _它和主控制流程是异步的，二者不存在调用和被调用的关系，并且使用不同的堆栈空间_。引入了信号处理函数使得一个进程具有多个控制流程，如果这些控制流程访问相同的全局资源（全局变量、硬件资源等），就有可能出现冲突.

如果一个函数符合以下条件之一则是 __不可重入__ 的：
1. 调用了`malloc`或`free`，因为`malloc`也是用全局链表来管理堆的。
2. 调用了标准I/O库函数。标准I/O库的很多实现都以不可重入的方式使用全局数据结构。

# Abstractions in other language

- [[Rust]]: The `Command` struct;

