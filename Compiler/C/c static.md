In the C programming language, `static` is used with **global variables and functions** to **set their scope to the containing file**.

In **local variables**, `static` is used to **store the variable in the statically allocated memory instead of the automatically allocated memory**. While the language does not dictate the implementation of either type of memory, statically allocated memory is typically reserved in data segment of the program at compile time, while the automatically allocated memory is normally implemented as a transient call stack.


> Static variables (like global variables) are initialized as 0 if not initialized explicitly.