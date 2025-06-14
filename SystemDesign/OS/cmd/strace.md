#cmd 

system call trace.
`strace`命令执行该程序，跟踪该程序执行过程中用到的所有系统调用的参数及返回值
in mac this is `dtruss`

> [!tldr] what is strace
> `strace` is a program on Linux that lets you _spy on what a program is doing_ without
> - a debugger, or
> - the source, or
> - even knowing the programming language at all.

`strace` does its spy work by _tracing system calls_ using [[ptrace]] in its implementation.

# Tips & Options

- `{bash}ls -l /proc/<proc-id>/fd` see all the file descriptors for some process;^[or use the command `lsof`] 
- `{bash}strace -e <syscall-name> <prog>` only tracing the syscall specified
- `{bash}strace -f <prog>` _follow_ subprocesses forked from the prog
- `{bash}strace -p <pid>` trace an already running process
- `strace -s <len:int>`, some syscall involve large string, such as reading a huge chunk of data to a buffer, `-s` suppress string to the length provided.
- `{bash}strace -o <output.txt>` write result to some file