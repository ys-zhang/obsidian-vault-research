`__thread`, `thread_local`

[Storage-class specifiers](https://en.cppreference.com/w/c/language/storage_duration#Storage_duration)

[Storage-class specifiers](https://en.cppreference.com/w/c/language/storage_duration)

> Every object has a property called storage duration, which limits the object **lifetime**.

There are four kinds of storage duration in C

- _**automatic**_ storage duration.
- _**[[c static| static]]**_ storage duration. The storage duration is the entire execution of the program, and the value stored in the object is initialized only once, **prior to main function**. 
- _**thread**_ storage duration. The storage duration is the entire execution of the thread in which it was created, and the value stored in the object is initialized when the thread is started. **Each thread has its own, distinct, object**.
- -   _**allocated**_ storage duration. The storage is allocated and deallocated on request, using _dynamic memory allocation_ functions.

# POSIX
线程局部存储在不同的平台有不同的实现，可移植性不太好。幸好要实现线程局部存储并不难，最简单的办法就是 _建立一个全局表，通过当前线程ID去查询相应的数据_，因为各个线程的ID不同，查到的数据自然也不同了。

```c
int pthread_key_create(pthread_key_t *key, void (*destructor)(void*));
int pthread_key_delete(pthread_key_t key);
void *pthread_getspecific(pthread_key_t key);
int pthread_setspecific(pthread_key_t key, const void *value);

```


# LLVM thread local storage models

[LLVM Language Reference Manual -- Thread local storage](https://llvm.org/docs/LangRef.html#thread-local-storage-models)

> A variable may be defined as `thread_local`, which means that it will not be shared by threads (each thread will have a _separated copy_ of the variable). Not all targets support thread-local variables.

Optionally, a TLS model may be specified:
- `localdynamic`: For variables that are only used within the current shared library.
- `initialexec`: For variables in modules that will not be loaded dynamically.
- `localexec`: For variables defined in the executable and only used within it.
- 
If no explicit model is given, the “general dynamic” model is used.


[All about thread-local storage](https://maskray.me/blog/2021-02-14-all-about-thread-local-storage)