**Trampolines** (sometimes referred to as _indirect jump_ vectors) are memory locations holding addresses pointing to _interrupt_ service routines, I/O routines, etc. Execution jumps into the trampoline and then immediately jumps out, or bounces, hence the term _trampoline_. They have many uses:

-   Trampoline can be used to overcome the limitations imposed by a CPU architecture that expects to always find vectors in fixed locations.
-   When an operating system is booted on a symmetric multiprocessing (SMP) machine, only one processor, the bootstrap processor, will be active. After the operating system has configured itself, it will instruct the other processors to jump to a piece of trampoline code that will initialize the processors and wait for the operating system to start scheduling threads on them.



# GCC Trampoline

[Nested Functions (GCC)](https://gcc.gnu.org/onlinedocs/gcc/Nested-Functions.html)

It is possible to call the nested function from outside the scope of its name by storing its address or passing the address to another function:

```c
void hack (int *array, int size)
{
  void store (int index, int value)
    { array[index] = value; }

  intermediate (store, size);
}

```
Here, the function intermediate receives the address of store as an argument. If intermediate calls store, the arguments given to store are used to store into array. But this technique works only so long as the containing function (hack, in this example) does not exit.

If you try to call the nested function through its address after the containing function exits, all hell breaks loose. If you try to call it after a containing scope level exits, and if it refers to some of the variables that are no longer in scope, you may be lucky, but it’s not wise to take the risk. If, however, the nested function does not refer to anything that has gone out of scope, you should be safe.

GCC implements taking the address of a nested function using a technique called _trampolines_. This technique was described in _Lexical Closures for C++ (Thomas M. Breuel, USENIX C++ Conference Proceedings, October 17-21, 1988)._

# LLVM Trampoline Intrinsics

see [[LLVM ESSENTIALS#LLVM intrinsics]]

[(24) What are the trampoline intrinsics in LLVM used for? - Quora](https://www.quora.com/What-are-the-trampoline-intrinsics-in-LLVM-used-for)

[LLVM Trampoline Intrinsics](https://llvm.org/docs/LangRef.html#trampoline-intrinsics)



> These intrinsics make it possible to excise one parameter, marked with the [nest](https://llvm.org/docs/LangRef.html#nest) attribute, from a function. The result is a callable function pointer lacking the nest parameter - the caller does not need to provide a value for it. Instead, the value to use is stored in advance in a “trampoline”, a block of memory usually allocated on the stack, which also contains code to splice the nest value into the argument list. This is used to implement the GCC nested function address extension.





```
declare void @llvm.init.trampoline(i8* <tramp>, i8* <func>, i8* <nval>)


```
