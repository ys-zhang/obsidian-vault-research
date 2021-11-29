当进程调用一种`exec`函数时，该进程的 _用户空间代码_ 和 _数据_ 完全被新程序替换，从新程序的启动例程开始执行。调用`exec`并不创建新进程，所以调用`exec`前后该进程的`id`并未改变。

> `exec`函数只有出错的返回值而没有成功的返回值

![[Pasted image 20211129163328.png]]

```c
#include <unistd.h>
int execl(const char *path, const char *arg, ...);
int execlp(const char *file, const char *arg, ...);
int execle(const char *path, const char *arg, ..., char *const envp[]);
int execv(const char *path, char *const argv[]);
int execvp(const char *file, char *const argv[]);
int execve(const char *path, char *const argv[], char *const envp[]);
```

只有`execve`是真正的系统调用.
![[exec-syscall-family-pic.png]]

1. 字母$p$(表示`PATH` ENV VAR): 
    - 不带$p$的`exec`函数第一个参数必须是程序的相对路径或绝对路径; 
    - 带字母$p$的函数：
        - 如果参数中包含`/`，则将其视为路径名;
        - 否则在`PATH`环境变量的目录列表中搜索这个程序。
2. 字母$l$(表示list):
    - 带字母$l$要求将新程序的每个命令行参数都当作一个参数传给它, 最后一个可变参数应该是`NULL`, 起sentinel的作用.
3. 以$e$(表示environment)结尾的exec函数，可以把一份新的环境变量表传给它.
4. 对于带有字母$v$(表示vector)的函数，则应该先构造一个指向各参数的 _指针数组_，然后将该数组的首地址当作参数传给它，数组中的最后一个指针也应该是`NULL`.