[fork(2) - Linux manual page (man7.org)](https://www.man7.org/linux/man-pages/man2/fork.2.html)

**fork**() creates a new process by duplicating the calling process. **Always works with [[exec]]**.

```c
#include <unistd.h>

// returns child's PID in the parent proc
// returns 0  in the child proc
pid_t fork(void);
```

The child process and the parent process **run in separate memory spaces**.

- returns child's PID in the parent process
- returns 0  in the child process
- returns negative if fails

# Warnings
```ad-warning 
title: Not thread safe
(In child process) only the caller thread survive after fork, all other threads disappear and may never have a chance to clearup (thinking about some other thread calling `malloc`, which is not thread safe, at the same time of forking, heap of child process get corrupted).
```

# Example
```c
#include<unistd.h>
#include<stdio.h>

int main(int argc, char *argv[]) {
	pid_t pid = get_pid();
	printf("hello world from pid: %d \n", (int) pid);
	pid_t rc = fork();
	if (rc < 0) exit(1);   // fork failed
	if (rc == 0) {
		// child proc
		printf("I am the child %d, my parent is %d", (int) get_pid(), (int) pid);
	} else {
		// parent proc
		printf("I am the parent %d, my child is %d", (int) pid, (int) rc);
	}
	return 0;
}
```
