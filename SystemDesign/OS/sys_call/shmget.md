共享内存是[[IPC (inter process call) | IPC]]中效率最高的一个，它是原理是Linux内核在内存中开辟一个空间，给进程进行读写。  
每个进程都会通过API函数，把这块Linux内核中的内存映射到自己的进程空间里面来，是映射的，虚拟的，不是实际在进程内存中。  
通过这种方法来达到进程间共享数据目的。

## `shmget`：创建共享内存
```c
int shmget(key_t key, size_t size, int shmflg);
```

• `key`: 这个共享内存段的名字，我们通常自定义数字并用key_t类型强转  
• `size`: 需要共享的内存量  
• `shmflg`:由九个权限标志构成，它们的用法和创建文件时使用的mode模式标志是一样的  
• 如果共享内存创建成功，`shmget`将返回一个非负整数，即该段共享内存的标识码；如果失败，则返回“－1”


## `shmat`：shared memory attach 

> 将创建好的共享内存连接到某个进程，并指定内存空间

```c
void *shmat(int shmid, const void *shmaddr, int shmflg);
```

• `shm_id`: `shmget`返回的共享内存标识  
• `shm_addr`:把共享内存连接到当前进程去的时候准备放置它的那个地址  
• `shmflg`是一组按位OR(或)在一起的标志。它的两个可能取值是`SHM_RND`和`SHM_RDONLY`  
• 调用成功，返回一个指针，指针指向共享内存的第一个字节，如果失败，则返回“－1”  

```c
void *shm_addr = NULL;  
//shmat的第二个参数shm_addr:把共享内存连接到当前进程去的时候准备放置它的那个地址，为NULL为让系统自动选择  
//第三个参数是一组按位OR(或)在一起的标志，SHM_RDONLY表示只读  
shm_addr = shmat(shm_id, NULL, SHM_RDONLY);  
```
• `shmaddr`为0(NULL)，核心自动选择一个地址  
• `shmaddr`不为０且`shmflg`无`SHM_RND`标记，则以`shmaddr`为连接地址。  
• `shmaddr`不为０且`shmflg`设置了`SHM_RND`标记，则连接的地址会自动向下调整为SHMLBA的整数倍。公式：$shmaddr - (shmaddr \% SHMLBA)$
• `shmflg=SHM_RDONLY`，表示连接操作用来只读共享内存

**注意点：**  
• 在fork() 后，子进程继承已连接的共享内存  
• 在exec后，已连接的共享内存会自动脱离(detach)  
• 在结束进程后，已连接的共享内存会自动脱离(detach)

## `shmdt`：shared memory detach

脱钩函数，把共享内存与当前进程脱离开
```c
int shmdt(const void *shmaddr);
```
• `shm_addr`: 由`shmat`返回的地址指针  
• 操作成功，返回“0”，失败则返回“－1”  
• 脱离共享内存并不等于删除它，只是当前进程不能再继续访问它而已  
注意：共享内存实际是独立于内存存在的，意味着进程结束后，共享内存以及里面保存的数据实际还存在。


