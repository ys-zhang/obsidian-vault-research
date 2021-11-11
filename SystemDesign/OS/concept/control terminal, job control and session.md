# Control Terminal
在UNIX系统中，用户通过终端登录系统后得到一个Shell进程，这个终端成为Shell进程的控制终端(_Controlling Terminal_).

每个进程都可以通过一个特殊的设备文件`/dev/tty`访问它的控制终端。事实上每个终端设备都对应一个不同的设备文件， `/dev/tty`提供了一个通用的接口，一个进程要访问它的控制终端既可以通过`/dev/tty`也可以通过该终端设备所对应的设备文件来访问。

> `tty` stands for teletypewriter.

默认情况下（没有重定向），每个进程的标准输入、标准输出和标准错误输出都指向控制终端。

一台PC通常只有一套键盘和显示器，也就是只有一套终端设备，但是可以通过`Ctrl-Alt-F1~CtrlAlt-F6`切换到6个字符终端，相当于有6套虚拟的终端设备，它们共用同一套物理终端设备，对应的设备文件分别是`/dev/tty1~/dev/tty6`，所以称为虚拟终端(_Virtual Terminal_)。

做嵌入式开发时经常会用到串口终端(_serial terminal_)，目标板的每个串口对应一个终端设备，比如`/dev/ttyS0`、 `/dev/ttyS1`等，将主机和目标板用串口线连起来，就可以在主机上通过Linux的`minicom`或Windows的超级终端工具登录到目标板的系统。
![[terminal.png]]

1. terminal device drive: read/write hardware
2. terminal line discipline: filter special actions, like translate `Ctrl+Z` to `SIGTSTP`.
3. read/write functions: implements IO buffer.

![[Pasted image 20211110160730.png]]



#### `ttyname` find tty file name from file descriptor

```c
#include <unistd.h>  
#include <stdio.h>  
int main()  
{  
    printf("fd 0: %s\n", ttyname(0));  
    printf("fd 1: %s\n", ttyname(1));  
    printf("fd 2: %s\n", ttyname(2));  
    return 0;  
}
```
在 telnet/ssh 下运行有不同结果

# Job Control

Shell 前后台控制的是job/process group, one job can have multiple processes.
```bash
# all follow belongs to the same session
proc1 | proc2 &        # a background process group
proc3 | proc4 | proc5  # front process group, only havr 1 proc at front
```

![[Pasted image 20211110165659.png]]

> One session can only have one process group on front.

- `jobs`命令可以查看当前有哪些作业;
- `fg`命令可以将某个作业提至前台运行,  如果该作业处于停止状态，则给进程组的每个进程发`SIGCONT`信号使它继续运行;
- `bg`命令可以让某个停止的作业在后台继续运行;
- `Ctrl-Z`则向所有 _前台_ 进程发`SIGTSTP`信号，该信号的默认动作是使进程停止;
- `kill`命令给一个停止的进程发`SIGTERM`信号，这个信号并不会立刻处理，而要等进程准备继续运行之前处理，默认动作是终止进程。

login process:

1. `getty` or `telnetd` starts
2. call `setsid` to start a session, the current process is the _session leader_, and its `id` is the _session id_, the current terminal is session's _controlling terminal_.
3. 在登录过程中， `getty`或`telnetd`进程变成login，然后变成Shell，但仍然是同一个进程, 仍然是session leader。
4. 由Shell进程fork出的子进程本来具有和Shell相同的Session、进程组和控制终端，但是Shell调用`setpgid`函数将作业中的某个子进程指定为一个新进程组的Leader，然后调`setpgid`将该作业中的 _其它子进程也转移到这个进程组_ 中。如果这个进程组需要在前台运行，就调用`tcsetpgrp`函数将它设置为前台进程组，由于一个Session只能有一个前台进程组，所以Shell所在的进程组就自动变成后台进程组。


# Daemon 
> 其它进程都是在用户登录或运行程序时创建，在运行结束或用户注销时终止，但系统服务进程不受用户登录注销的影响，它们一直在运行着。这种进程有一个名称叫守护进程（Daemon）。

> Daemon do not have a controlling terminal.    守护进程通常采用以`d`结尾的名字，表示Daemon

### Create Daemon
>  Linux也提供了一个库函数`daemon(3)`实现我们的`daemonize`函数的功能，它带两个参数指示要不要切换工作目录到根目录，以及要不要把文件描述符0、 1、 2重定向到`/dev/null`。

```c
#include <stdlib.h>  
#include <stdio.h>  
#include <fcntl.h>

void daemonize()
{
  pid_t pid;
  /*
   * Become a session leader to lose controlling TTY.
   * 需要保证当前进程不是进程组的Leader，
   * fork创建的子进程和父进程在同一个进程组中，
   * 进程组的Leader必然是该组的第一个进程
   */
  if ((pid = fork()) < 0) {
    perror("fork");
    exit(1);
  } else if (pid != 0) /* parent */
    exit(0);
  
  // child process
  setsid();       // create a session and become its leader
  
  /* Change the current working directory to the root. */
  if (chdir("/") < 0) {
    perror("chdir");
    exit(1);
  }
  /*Attach file descriptors 0, 1, and 2 to /dev/null.*/
  close(0);
  open("/dev/null", O_RDWR);
  dup2(0, 1);
  dup2(0, 2);
}
```
