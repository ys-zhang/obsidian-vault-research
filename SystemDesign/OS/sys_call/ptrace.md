`ptrace` is a syscall which is used to _observe_ and _control_ processes.

By control it means changing:
1. file descriptor
2. memory (incl. code segment)
3. register
4. observe and intercept syscall/signals

>[!warning] 
>`ptrace` can only attach to processes that the owner can send signal to.

> While  being  traced,  the tracee will stop each time a signal is delivered, even if the signal is being ignored. (An exception is SIGKILL, which has its usual effect.)  The tracer will be notified at its  next  call  to  `waitpid(2)`  (or  one of the related "wait" system calls); that call will return a status value containing information that indicates the cause of the stop in the tracee.  While the tracee is stopped,  the  tracer  can  use  various `ptrace`  requests to inspect and modify the tracee.  The tracer then causes the tracee to continue, optionally ignoring the delivered signal (or even delivering a different signal instead).

# Trace options

| Cat              | Opt                | Description                   |
| ---------------- | ------------------ | ----------------------------- |
| Entry trace mode | PTRACE_ME          | Force self into trace mode    |
| Entry trace mode | PTRACE_ATTACH      | Force `pid` into trace mode   |
| Tracing control  | PTRACE_SYSCALL     | Stop before and after syscall |
| Tracing control  | PTRACE_SINGLESTEP  | Stop at every CPU instruction |
| Read proc state  | PTRACE_PEEKUSER    | peek memory/register          |
| Read proc state  | PTRACE_GETSIGNINFO | signal cause tracee to stall  |
| Read proc state  | PTRACE_GETREGS     | register                      |
|                  | PTRACE_SETSIGNINFO |                               |


###### Enter trace mode
![[Pasted image 20211130221206.png]]

###### Single step tracing

![[PTRACE_SINGLESTEP.png]]

# New powerful replacement

see [[procfs]], which allows the owner process have direct access to the target process, which saves cost on context switch compared with `ptrace`.

# How it works

## syscall

When a process wants to invoke a system call, it puts the arguments to system calls in registers and calls soft interrupt `0x80`.

> [!NOTE] syscall on i386
> 
> The system call number is put in the register `%eax`.
> 
> The arguments to this system call are put into registers `%ebx`, `%ecx`, `%edx`, `%esi` and `%edi`, in that order.
>
> see [[register]]


> [!INFO]
> Before executing the system call, the kernel checks whether the process is being traced. If it is, the kernel stops the process and gives control to the tracking process so it can examine and modify the traced process' registers.

# Example


```cpp
#include <sys/ptrace.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <sys/user.h>
#include <stdio.h>
int main()
{   
  pid_t child;
  struct user_regs_struct regs;

  child = fork();  // 创建一个子进程
  if(child == 0) { // 子进程
    // 表示当前进程进入被追踪状态
    ptrace(PTRACE_TRACEME, 0, NULL, NULL); 
    // 执行 `/bin/ls` 程序
    execl("/bin/ls", "ls", NULL);          
  } else { // 父进程
    wait(NULL); // 等待子进程发送一个 SIGCHLD 信号
    // 获取子进程的各个寄存器的值
    ptrace(PTRACE_GETREGS, child, NULL, &regs); 
    printf(   // 打印寄存器的值
      "Register: rdi[%ld], rsi[%ld], "
        "rdx[%ld], rax[%ld], orig_rax[%ld]\n",
      regs.rdi, regs.rsi, regs.rdx,
      regs.rax, regs.orig_rax
    ); 
    ptrace(PTRACE_CONT, child, NULL, NULL); // 继续运行子进程
    sleep(1);
  }
  return 0;
}
```

# References

- [`ptrace` - Wikipedia](https://en.wikipedia.org/wiki/Ptrace)
- [Playing with `ptrace`, Part I | Linux Journal](https://www.linuxjournal.com/article/6100)
- [Playing with `ptrace`, Part II | Linux Journal](https://www.linuxjournal.com/article/6210)
- [GDB调试程序的核心技术-`ptrace`系统调用与使用示例 - 知乎](https://zhuanlan.zhihu.com/p/436433331?utm_source=wechat_session&utm_medium=social&utm_oi=1264633478584340480)