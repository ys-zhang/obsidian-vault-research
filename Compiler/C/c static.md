In the C programming language, `static` is used with **global variables and functions** to **set their scope to the containing file**.

In **local variables**, `static` is used to **store the variable in the statically allocated memory instead of the automatically allocated memory**. While the language does not dictate the implementation of either type of memory, statically allocated memory is typically reserved in data segment of the program at compile time, while the automatically allocated memory is normally implemented as a transient call stack.


> Static variables (like global variables) are initialized as 0 if not initialized explicitly.


# linkage

> static has internal linkage

```c
static void foo(void);
```
foo 这个函数只具有Internal Linkage，只有在foo.c中多次声明才表示同一个函数，而在main.c中声明就不表示它了。如果把foo.c编译成目标文件，函数名foo在其中是一个LOCAL的符号，不参与链接过程，所以在链接时， main.c中用到一个External Linkage的foo函数，链接器却找不到它的定义在哪儿，无法确定它的地址，也就无法做符号解析.

# storage duration

-   _**static**_ storage duration. The storage duration is the entire execution of the program, and the value stored in the object is initialized only once, **prior to main function**. All objects declared `static` and all objects with either internal or external linkage that aren't declared `_Thread_local` (since C11) have this storage duration.