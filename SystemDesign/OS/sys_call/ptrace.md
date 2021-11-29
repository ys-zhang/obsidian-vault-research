`ptrace` is a syscall which is used to _observe_ and _control_ processes.

By control it means changing:
1. file descriptor
2. memory (incl. code segment)
3. register
4. observe and intercept syscall/signals

> `ptrace` can only attach to processes that the owner can send signal to .

> While  being  traced,  the tracee will stop each time a signal is delivered, even if the signal is being ignored. (An exception is SIGKILL, which has its usual effect.)  The tracer will be notified at its  next  call  to  `waitpid(2)`  (or  one of the related "wait" system calls); that call will return a status value containing information that indicates the cause of the stop in the tracee.  While the tracee is stopped,  the  tracer  can  use  various `ptrace`  requests to inspect and modify the tracee.  The tracer then causes the tracee to continue, optionally ignoring the delivered signal (or even delivering a different signal instead).

| option                                 | description                                 |
| -------------------------------------- | ------------------------------------------- |
| PTRACE_ME                              | child proc calls to allow tracer to jump in |
| PTRACE_PEEKUSER, PTRACE_GETREGS        | peek memory/register                        |
| PTRACE_GETSIGNINFO, PTRACE_SETSIGNINFO | signal cause tracee to stall                |

| options        | description                                                 |
| -------------- | ----------------------------------------------------------- |
| PTRACE_SYSCALL | called in tracer makes tracee stall when there is a syscall |
|                |                                                             |

# New powerful replacement

see [[procfs]], which allows the owner process have direct access to the target process, which saves cost on context switch compared with `ptrace`.

# How it works

## syscall

When a process wants to invoke a system call, it puts the arguments to system calls in registers and calls soft interrupt `0x80`.

```ad-note
title: syscall on i386
The system call number is put in the register `%eax`.

The arguments to this system call are put into registers `%ebx`, `%ecx`, `%edx`, `%esi` and `%edi`, in that order.

see [[register]]
```

> Before executing the system call, the kernel checks whether the process is being traced. If it is, the kernel stops the process and gives control to the tracking process so it can examine and modify the traced process' registers.

# References

- [`ptrace` - Wikipedia](https://en.wikipedia.org/wiki/Ptrace)
- [Playing with `ptrace`, Part I | Linux Journal](https://www.linuxjournal.com/article/6100)
- [Playing with `ptrace`, Part II | Linux Journal](https://www.linuxjournal.com/article/6210)