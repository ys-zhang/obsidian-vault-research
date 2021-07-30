# OpenCL  (Open Computing Language)

#  Heterogeneous System

![[Pasted image 20210725154123.png]]

| Host | Device                  | Platform       |
| ---- | ----------------------- | -------------- |
| CPU  | [[GPU]], FPGA, DSP, CPU | cluster of GPU |

![[OpenCL and CUDA Execution Model.excalidraw]]

# OpenCL

## NDRange (id of work-item)
- **NDRange** or **global work size**: total number of work-items run in parallel
-  **local work size**: number of work-items within a work-group

![[opencl-ndrange.png]]

##    Memory Model

OpenCL generalizes the different types of memory available on a device into **private memory, local memory, global memory, and constant memory**

| Memory Type     | Description            | [[GPU]]                              | [[address space qualifier]] |
| --------------- | ---------------------- | ------------------------------------ | ---------------------------------- |
| Private memory  | private per work-item  | CU register / local memory           | default                            |
| Local memory    | shared amid work-group | CU local memory                      | `__local`                          |
| Global memory   | all work-groups        | in GDDR5, `alloc` by host on runtime | `__global`                         |
| Constant memory | static                 | constant/texture                     | `__constant`                       |

## Program architecture

	program = kernel function + host code

### Kernel function
The OpenCL kernel is a code sequence that will be executed by every single thread running on a GPU.

_**Kernel function** is very similar in structure to a C function*, but it has the qualifier `_ _kernel`_. This qualifier alerts the compiler that a function is to be compiled to **run on an OpenCL device** instead of the host. 

The arguments are passed to a kernel as they are passed to any C function. 
- The arguments in the global memory are described with `_ _global` qualifier 
- the arguments in the shared memory are described with `_ _local` qualifier. 
- These arguments should be always **passed as pointers**.

>    the **kernel functions are compiled in runtime** and the compilation process is initiated from the host code.

### Host code

The host application runs on a user’s computer (the host) and dispatches kernels to connected devices.

1.  Discover the OpenCL devices that constitute the heterogeneous system.
2.  Probe the *characteristics of these devices* so that the **kernel function**s can adapt to the *specific features*.
3.  Read and compile the kernel(s)'source file.
4. Set up memory objects on the selected device(s).
5. Compile and run the kernel(s).
6. Compile and run the kernel(s).

##### From the programer's perspective

1. Prepare and initialize data on host.  
2. Discover and initialize the devices.  
3. Create a context.  
4. Create a command queue.  
5. Create the program object for a context.  
6. Build the OpenCL program.  
7. Create device buffers.  
8. Write host data to device buffers.  
9. Create and compile the kernel.  
10. Set the kernel arguments.  
11. Set the execution model and enqueue the kernel for execution.  
12. Read the output buffer back to the host.

## Library

```c
size_t get_global_id (uint dimindx);
```

# CUDA

![[CUDA.excalidraw]]
## sync

```c
void __syncthreads(); // shared with in thread block
	

// implicit barrier
kernel_foo<<<M,N>>>();
// implicit barrier here that is global
kernel_bar<<<M,N>>>();
```


# Creed of GPGUP

1.  maximize math-memory intensity
$$
	\max {\textrm{Math}\over \textrm{Memory Access}}
$$
  