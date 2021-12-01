The `dup()` system call family creates a copy of the _file descriptor_ `oldfd`.

1. `dup` using the _lowest-numbered unused_ file descriptor for the new descriptor.
2. uses the file descriptor number specified in `newfd`.

```c
int dup (int oldfd);
int dup2 (int oldfd, int newfd);
int dup3(int oldfd, int newfd, int flags);
```