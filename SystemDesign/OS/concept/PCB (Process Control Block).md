每个进程在内核中都有一个进程控制块（PCB）来维护进程相关的信息， Linux内核的进程控制块是`task_struct`结构体。
包含：

1. `pid_t`  process unique id.
2.  进程的状态，有运行、挂起、停止、僵尸等状态。
3.  进程切换时需要保存和恢复的一些CPU寄存器
4.  描述虚拟地址空间的信息。  
5. 描述控制终端的信息。  
6. 当前工作目录（Current Working Directory） 。  
7. umask掩码。  
8. 文件描述符表，包含很多指向file结构体的指针。  
9. 和信号相关的信息。  
10. 用户id和组id。  
11. control terminal、 Session和progress group。  
12. 进程可以使用的资源上限（Resource Limit）