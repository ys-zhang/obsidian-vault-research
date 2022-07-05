NAME
       `dlsym`, `dlvsym` - obtain address of a symbol in a shared object or executable

SYNOPSIS
```c
 #include <dlfcn.h>
 void *dlsym(void *handle, const char *symbol);
 
 #define _GNU_SOURCE
 #include <dlfcn.h>
 void *dlvsym(void *handle, char *symbol, char *version);
```

Link with `-ldl`.


see also [[dlopen]]
