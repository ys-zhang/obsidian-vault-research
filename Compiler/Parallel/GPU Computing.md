
# Terms

| NVIDEA                     | AMD         |      Concept                |
| -------------------------- | ----------- | -------------------- |
| Stream Multiprocessor (SM) | SIMD engine | Compute Unit (CU)    |
| Stream Processor           | ALU         | Process Element (PE) |

| OpenCL        | CUDA           | Hardware (AMD/NVIDEA) |
| ------------- | -------------- | --------------------- |
| Working Item  | Thread         | PE (ALU/SP)           |
| Working Group | (Thread) Block | CU (SIMD engine/SM)   |


######  Process Element

![[GPU-Process-Element.excalidraw]]

###### Compute Unit

![[GPU-Compute-Unit.excalidraw]]

## Warp 

[Using CUDA Warp-Level Primitives | NVIDIA Developer Blog](https://developer.nvidia.com/blog/using-cuda-warp-level-primitives/)

> NVIDIA GPUs execute groups of threads known as **_warps_** in **SIMT** (Single Instruction, Multiple Thread) fashion.

The **compute unit** schedules and executes** work-items** from the same *work-group* in groups of 32 parallel work-items called **warps**.

A **warp** executes one common instruction at a time. It means that *work-items *in a warp execute in a so-called** lock-step basis**, running the same instruction but on different data.

> Parallel is only true when the warp execute the same instruction/branch, if threads in the warp are in different branch they will execute serially. 
> *(PE share the same Fetcher)*

##### Warp Scheduling
At every instruction issue time, a warp scheduler selects a warp that has work-items ready to execute its next instruction (Fig. 5.7), and issues the instruction to those  work-items.

Work-items that are ready to execute are called **active work-items**. 

The number of clock cycles it takes for a warp to be ready to execute its next instruction is called **latency**.

![[GPU-Warp-Scheduling.png]]
> Each warp always contains work-items of consecutive work-items IDs, but* warps are executed out of order.*

Grouping of work-items into warps is not only relevant to computation, but also to global memory accesses. The GPU device [[Coalesced memory access | coalesces ]] global memory loads and stores issued by work-items of a warp into as few transactions as possible to minimize DRAM bandwidth.

## Memory Hierarchy

![[Pasted image 20210725125421.png]]

| Storage type    | Access time (cycles) |
| --------------- | -------------------- |
| Registers       | 1                    |
| Local memory    | 1-32                 |
| Texture memory  | ~500                 |
| Constant memory | ~500                 |
| Global memory   | ~500                 |

##### Register

*The GPU has thousands of registers **per compute unit (CU)**. *

GPU dedicates real registers to each and every work-item. 

**The number of registers per work-item is calculated at compile time**.


##### Local/Shared Memory

***local/shared memory is mainly  used for data interchange within a work-group running on CU.***

Local memory acts as a user-controlled L1 cache.  
	Actually, on modern GPUs, this on-chip memory can be used as a user-controlled local memory or standard hardware-controlled L1 cache. For example, on Kepler(GTX 780) CUs this memory can be split of 48 KB local memory/16 KB L1 cache.
	
##### Global Memory
GPU global memory is global because it’s accessible from both the GPU and the CPU (or any device that can access to PCI-E hub).

Reads and writes to global memory are always initiated from CU and are always 128 bytes wide starting at the address aligned at 128-bytes boundary. The blocks of memory that are accessed in one memory transactions are called ***segments***.

If two work-items of the same warp access two data that fall into the same 128-bytes segment, data is delivered in a single transaction.


#####    constant memory

The constant memory (readonly) space resides in device memory (global memory) and is cached. This is where constants and program arguments are stored.
-    it is cached