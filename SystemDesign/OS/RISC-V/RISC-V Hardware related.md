# CPU 加电

1. 初始化 register
2. 初始化 memory (扫描内存信息, 内存大小 etc)
3. 初始化基本外设
4. 执行ROM 中 firmware  (first instruction)


#### CPU 加电.
[[qemu]] 中CPU加电后首先进入M模式,屏蔽中断并 从reset_vector, 它被设为ROM的首条地址,读入第一条指令. ROM 中放入主板初始化代码.
#### 初始化 memory 及 外设
初始化并获取各物理地址段的映射信息, 即memory地址段, 各硬件控制地址段, 端口 等等.

由于硬件通过主板相连, 故而此地址段映射由主板规定, 可查询主板手册获取.


# Interrupt, Exception and Syscall



 