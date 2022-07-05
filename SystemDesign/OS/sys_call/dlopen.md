NAME
       dlclose, dlopen, dlmopen - open and close a shared object

SYNOPSIS
```c

#include <dlfcn.h>

void *dlopen(const char *filename, int flags);

int dlclose(void *handle);

#define _GNU_SOURCE
#include <dlfcn.h>

void *dlmopen (Lmid_t lmid, const char *filename, int flags);
```

Link with `-ldl`.

