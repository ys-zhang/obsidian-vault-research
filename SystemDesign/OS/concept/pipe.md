2 types of pipe: `pipe` and `fifo/named-pipe`

> Pipe  and  FIFO  (also  known as named pipes) provide a **unidirectional interprocess** communication channel.

> The only difference between pipes and FIFO is the manner in which they are created and opened.  Once these tasks have  been  accomplished,  I/O  on pipes and FIFO has exactly the same semantics. FIFO 出现在文件体统中

# Pipe

```c
 #include <unistd.h>

   int pipe(int pipefd[2]);

   #define _GNU_SOURCE             /* See feature_test_macros(7) */
   #include <fcntl.h>                         /* Obtain O_* constant definitions */
   #include <unistd.h>

  int pipe2(int pipefd[2], int flags);
```

可以`fork`子进程然后在子进程里使用`pipe`与父进程通信

see `man pipe.2` `man pipe.7`

# FIFO

> A FIFO (short for First In First Out) has a name within the file system (created using `mkfifo(3)`), and is opened using `open(2)`.  Any process may open a FIFO, assuming the file permissions allow it.  The read end is opened using the `O_RDONLY` flag; the write end is opened using  the  `O_WRONLY`  flag.

# Others

## `PIPE_BUF`

> POSIX.1 says that `write(2)`s of less than `PIPE_BUF` bytes must be atomic: the output data is written to the pipe as a contiguous sequence. Writes of more than `PIPE_BUF` bytes may be nonatomic: the kernel may interleave the data with data written by other processes.

|          | POSIX   | Linux   |
| -------- | ------- | ------- |
| PIPE_BUF | $>512B$ | $4096B$ | 

