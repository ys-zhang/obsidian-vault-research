The **top** program provides a dynamic real-time view of a running system.  It can display **system** summary information as well as a list of **processes** or **threads** currently being managed by the Linux kernel.


# Result

![[linux-cmd-top.png]]

## Process State - COLUMN `S`

The status of the task which can be one of:
- **D** = uninterruptible sleep
- **I** = idle
- **R** = running
- **S** = sleeping
- **T** = stopped by job control signal
- **t** = stopped by debugger during trace
- **Z** = zombie