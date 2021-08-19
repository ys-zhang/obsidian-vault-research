# Misc

## `popen`

```c
#include <stdio.h>

/*
 * if `type` == 'r' or 'w' then the returned FILE is connected to 
 * stdout/stdin of the `command`
 */
FILE *popen(const char *command, const char *type);

// wait for the sub-proc to finish; closes the `FILE`
int pclose(FILE *stream);
```

> The unfortunate effect of using the shell is that for every call to `popen`, a shell is invoked along with the requested program.

