 mmap, munmap - map or unmap files or devices into memory
 
 
 mmap()  creates a new mapping in the virtual address space of the calling process.
 
 `mmap()` 是一个系统调用函数，本质是一种进程虚拟内存的映射方法，可以将一个文件、一段物理内存或者其它对象映射到进程的虚拟内存地址空间。实现这样的映射关系后，进程就可以采用指针的方式来读写操作这一段内存，进而完成对文件的操作，而不必再调用 read/write 等系统调用函数了。

 	void *mmap(void *adrr, size_t length, int prot, int flags, int fd, off_t offset);
	
- addr：建立映射区的首地址，由 Linux 内核指定。用户程序调用时直接传递 NULL。
- length：创建映射区的大小。
- prot：映射区的权限，有 PROT_READ、PROT_WRITE、PROT_READ|PROT_WRITE 类型。
- flags：标志位参数，常用于设定更新物理区域、设置共享、创建匿名映射区。
- MAP_SHARED：映射区所做的修改会反映到物理设备（磁盘）上。
- MAP_PRIVATE：映射区所做的修改不会反映到物理设备上。
- fd：用来建立映射区的文件描述符。
- offset：映射文件的偏移量（4k 的整数倍），可以映射整个文件，也可以只映射一部分内容。
