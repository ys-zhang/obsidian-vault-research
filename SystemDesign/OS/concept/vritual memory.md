**Virtual memory** means breaking the **virtual address space** into chunks of (potentially) _discontiguous_ physical addresses.


>[!NOTE] _virtual address_ is a general concept, while _virtual memory_ is a specific technique for mapping virtual addresses.

for detail see [[Memory Management.pdf]]


# Paging
map [[page (memory)]] to [[frame (memory)]]

![[Pasted image 20220630203702.png]]

**Translation Look aside Buffer (TLB)** caches page to frame translation, which located on chip, not in memory.

![[Pasted image 20220630204206.png]]


