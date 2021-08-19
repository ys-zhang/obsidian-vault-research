[wait(2) - Linux manual page (man7.org)](https://www.man7.org/linux/man-pages/man2/wait.2.html)

`wait`, `waitpid`, `waitid` - wait for process to change state.

> In the case of a terminated child, performing a wait allows the system to release the resources associated with the child; if a wait is not performed, then the terminated child remains in a "zombie" state.

> If a child has already changed state, then these calls return
       immediately.  Otherwise, they block until either a child changes
       state or a signal handler interrupts the call