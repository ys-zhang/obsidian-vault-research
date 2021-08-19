
# Blogs

[linux 内核task_struct 源码分析与解析(整合配图）_HTmonster的博客-CSDN博客_linux pgd task_struct](https://blog.csdn.net/weixin_38371073/article/details/114376410)

# Why Process: Virtualization

> OS needs to provide to each of its residences a virtual world as if he is the king of the world.


# Process State
![[os-proc-state.png]]

| State                  | Meaning                                                                                                                                           |
| ---------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------- |
| `TASK_RUNNING`         | either executing on the CPU or waiting to be executed                                                                                             |
| `TASK_INTERRUPTIBLE`   | The process is suspended (sleeping) until some condition becomes true.                                                                            |
| `TASK_UNINTERRUPTIBLE` | Like the previous state, except that delivering a signal to the sleeping process leaves its state unchanged.                                      |
| `TASK_STOPPED`         | Totally ended                                                                                                                                     |
| `TASK_ZOMBIE`          | Process execution is terminated, but the parent process has not yet issued a `wait`-like system call to return information about the dead process |

> The [[top]] command can list process states 

```c
//  目录:  include/linux/sched.h

/*
 * Task state bitmask. NOTE! These bits are also
 * encoded in fs/proc/array.c: get_task_state().
 *
 * We have two separate sets of flags: task->state
 * is about runnability, while task->exit_state are
 * about the task exiting. Confusing, but this way
 * modifying one set can't modify the other one by
 * mistake.
 */

/* Used in tsk->state: */
#define TASK_RUNNING			0x0000
#define TASK_INTERRUPTIBLE		0x0001
#define TASK_UNINTERRUPTIBLE		0x0002
#define __TASK_STOPPED			0x0004
#define __TASK_TRACED			0x0008
/* Used in tsk->exit_state: */
#define EXIT_DEAD			0x0010
#define EXIT_ZOMBIE			0x0020
#define EXIT_TRACE			(EXIT_ZOMBIE | EXIT_DEAD)
/* Used in tsk->state again: */
#define TASK_PARKED			0x0040
#define TASK_DEAD			0x0080
#define TASK_WAKEKILL			0x0100
#define TASK_WAKING			0x0200
#define TASK_NOLOAD			0x0400
#define TASK_NEW			0x0800
#define TASK_STATE_MAX			0x1000
```



# Process API

- [[fork]]: 新建子进程
- [[wait]]: parent wait for child to enter `TASK_ZOMBIE` state, wait terns child's state to `TASK_STOPPED`
- [[exec]]: exec()会从可执行程序中加载代码和静态数据，并用它覆写自己的代码段（以及静态数据），堆、栈及其他内存空间也会被重新初始化。然后操作系统就执行该程序，将参数通过argv传递给该进程。因此，它并没有创建新进程，而是直接将当前运行的程序（以前的p3）替换为不同的运行程序（wc）。



