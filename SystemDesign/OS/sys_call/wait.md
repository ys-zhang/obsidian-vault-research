[wait(2) - Linux manual page (man7.org)](https://www.man7.org/linux/man-pages/man2/wait.2.html)

`wait`, `waitpid`, `waitid` - wait for process to change state.

一个进程在终止时会关闭所有文件描述符，释放在用户空间分配的内存，但它的**PCB还保留着**，内核在其中保存了一些信息：
    
如果是正常终止则保存着退出状态，如果是异常终止则保存着导致该进程终止的信号是哪个。

> In the case of a terminated child, performing a wait allows the system to release the resources associated with the child; if a wait is not performed, then the terminated child remains in a "zombie" state.

> If a child has already changed state, then these calls return immediately.  Otherwise, they block until either a child changes state or a signal handler interrupts the call.

```c
// block the caller/parent process
pid_t wait(int *status);
pid_t waitpid(pid_t pid, int *status, int options);
```

# Options

| option       | name          | description                                                             |
| ------------ | ------------- | ----------------------------------------------------------------------- |
| `WNOHANG`    | wait no hang  | do not block parent proc                                                |
| `WUNTRACED`  | wait untraced | also  return  if a child has stopped (but not traced via [[ptrace]])    |
| `WCONTINUED` |               | also return if a stopped child has been resumed by delivery of SIGCONT. |


# Status

| macro       | usage                                               |
| ----------- | --------------------------------------------------- |
| WIFEXIT     | wait if exit: true if the child terminates normally |
| WEXITSTATUS | exit status (`$?`) of the child                                                    |