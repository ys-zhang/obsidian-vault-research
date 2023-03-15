#concurrency 

The ABA problem occurs when multiple [threads](https://en.wikipedia.org/wiki/Thread_(computer_science) "Thread (computer science)") (or [processes](https://en.wikipedia.org/wiki/Process_(computing) "Process (computing)")) accessing shared data interleave. Below is a sequence of events that illustrates the ABA problem:

1.  Process $P$ reads value $A$ from some shared memory location,
2.  $P$ is [preempted](https://en.wikipedia.org/wiki/Preemption_(computing) "Preemption (computing)"), allowing process $Q$ to run,
3.  $Q$ writes value $B$ to the shared memory location
4.  $Q$ writes value $A$ to the shared memory location
5.  $Q$ is preempted, allowing process $P$ to run,
6.  $P$ reads value $A$ from the shared memory location,
7.  $P$ determines that _the shared memory value has not changed_ and continues.

**Although $P$ can continue executing, it is possible that the behaviour will not be correct due to the "hidden" modification in shared memory.**



