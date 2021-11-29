#Rust 

[rCore-Tutorial-Book 第三版 — rCore-Tutorial-Book-v3 0.1 文档 (rcore-os.github.io)](https://rcore-os.github.io/rCore-Tutorial-Book-v3/index.html)

[Writing an OS in Rust](https://os.phil-opp.com/zh-CN/)

# Chap 0 操作系统概述

操作系统这个系统软件干的事主要有两件：
1. 向下管理计算机硬件和各种外设；
2. 向上给应用软件提供各种服务帮助。

![[Pasted image 20211122124448.png]]

操作系统的历史
1. 人类操作员 (Operator)来管理和操作机器；
2. 监控程序 (Monitor) 辅助完成输入、输出、加载、运行程序等工作；
3. 支持“批处理”和“多道程序”(multi-job, see[[control terminal, job control and session]])；
4. 分时系统
5. GUI

###### 系统调用(system call):
-   _进程_（即程序运行过程）管理：复制创建进程 `fork` 、退出进程 `exit` 、执行进程 `exec` 等。
-   同步互斥的 _并发控制_：信号量 `semaphore` 、管程 `monitor` 、条件变量 `condition variable` 等。
-   _进程间通信_：管道 `pipe` 、信号 `signal` 、事件 `event` 等。
-   _虚存管理_：内存空间映射 `mmap` 、改变数据段地址空间大小 `sbrk` 、共享内存 `shm` 等。
-   _文件I/O操作_：读 `read` 、写 `write` 、打开 `open` 、关闭 `close` 等。
-   _外设I/O操作_：外设包括键盘、显示器、串口、磁盘、时钟 …，但接口均采用了文件 I/O 操作的通用系统调用接口。

![[Pasted image 20211122125637.png]]
-   _文件 (File) 是外设的一种抽象和虚拟化_。特别对于存储外设而言，文件是持久存储的抽象。
-   _地址空间 (Address Space) 是对内存的抽象和虚拟化_。
-   _进程 (Process) 是对计算机资源的抽象和虚拟化_。而其中最核心的部分是对CPU的抽象与虚拟化。

### 执行环境

> 执行环境是应用程序正确运行所需的 _服务与管理环境_，用来完成应用程序在 _运行时的数据与资源管理_、应用程序的 _生存期_ 等方面的处理，它定义了应用程序有 _权_ 访问的其他数据或资源，并决定了应用程序的 _行为限制范围_。

1. _计算机硬件_；
2. _函数库_ -> _计算机硬件_；
3. _函数库_ -> _操作系统内核_ -> _计算机硬件_；
4. _函数库_ -> _Java 虚拟机_ -> _操作系统内核_ -> _计算机硬件_；
5. _函数库_ -> _Java 虚拟机_ -> _操作系统内核_ -> _Hypervisor/VMM_ -> _计算机硬件_;

![[Pasted image 20211122130122.png]]

### Common Control Flow (CCF) & Exceptional Control Flow (ECF)

CCF 于 ECF 的区别是否产生 _执行环境的切换_。

######  控制流上下文（执行环境的状态）

会影响控制流正确执行的有限的物理/虚拟资源内容。
-   _物理资源_：即计算机硬件资源，如CPU的寄存器、可访问的物理内存等。
-   _虚拟资源_：即操作系统提供的资源，如文件，网络端口号，网络地址，信号等。

###### ECF: Interrupt

> 外设 **中断** (Interrupt) 由外部设备引起的外部 I/O 事件如时钟中断、控制台中断等。外设中断是 _异步_ 产生的，与处理器的执行无关。

![[Pasted image 20211122131101.png]]

###### ECF: Exception

> **异常** (Exception) 是在处理器执行指令期间检测到不正常的或非法的内部事件（如除零错、地址访问越界）。对于不可恢复的异常，操作系统有权直接 _终止_ 该应用程序的执行。


![[Pasted image 20211122131408.png]]


###### ECF: Trap

> **陷入** (Trap) 是在程序中使用请求操作系统服务的系统调用而引发的 _有意事件_。

![[Pasted image 20211122131710.png]]

 >`RISC-V`中 _软件中断_ 是指软件可以通过写特定寄存器（`mip/sip`）的特定位（`MSIP/SSIP/USIP`）来产生的中断。

### 进程

虚拟化

> 一个进程是一个具有一定独立功能的程序在一个数据集合上的一次动态执行过程。 操作系统中的进程管理需要采用某种调度策略将处理器资源分配给程序并在适当的时候回收，并且要尽可能充分利用处理器的硬件资源。

![[Pasted image 20211122132054.png]]

![[Pasted image 20211122132149.png]]
[[进程切换]]
![[Pasted image 20211122132237.png]]


###### 地址空间 Address space

> **地址空间** (Address Space) 是对物理内存的虚拟化和抽象，也称虚存 (Virtual Memory)。它就是操作系统通过处理器中的内存管理单元 (MMU, Memory Management Unit) 硬件的支持而给应用程序和用户提供一个大的（可能超过计算机中的物理内存容量）、连续的（连续的地址空间编址）、私有的（其他应用程序无法破坏）的存储空间

操作系统中的虚存管理与处理器的 `MMU` 密切相关,在启动虚存机制后，软件通过 `CPU` 访问的每个虚拟地址都需要通过 `CPU` 中的 `MMU` 转换为一个物理地址来进行访问。

![[Pasted image 20211122132617.png]]

###### 文件 File

> **文件** (File) 主要用于对持久存储的抽象，并进一步扩展到为外设的抽象。

磁盘数据访问单位是一个扇区或一个块，而在内存中的数据访问单位是一个字节或一个字。

![[Pasted image 20211122140648.png]]

### 操作系统的 Characteristics

- Virtualization
    - Memory virtualization
    - CPU virtualization
- Concurrency
- Asynchronous
- Shared
- Persistent 


# Chapter 1 应用程序和基本执行环境


## Tools

- [[strace]]  运行一个程序并输出程序运行过程当中向内核请求的所有的系统调用及其返回值

## Remove runtime dependency

[A Freestanding Rust Binary | Writing an OS in Rust](https://os.phil-opp.com/freestanding-rust-binary/)

- `#![no_std]`, get rid of `std` library, we still have `core` library
- `lang_item`, **Language items are special functions and types that are required internally by the compiler**.
- `start` language item and `#![no_main]`: in typical rust binary, execution starts at `crt0`(C runtime zero), then calls the `start` language item, then calls `main`