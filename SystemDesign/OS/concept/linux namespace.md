> **Namespaces** are a feature of the [Linux kernel](https://en.wikipedia.org/wiki/Linux_kernel "Linux kernel") that partitions kernel resources such that one set of [processes](https://en.wikipedia.org/wiki/Process_(computing) "Process (computing)") sees one set of resources while another set of processes sees a different set of resources.

> Examples of such resources are process IDs, hostnames, user IDs, file names, and some names associated with network access, and [interprocess communication](https://en.wikipedia.org/wiki/Interprocess_communication "Interprocess communication").


A Linux system starts out with a single namespace of each type, used by all processes. 
Processes can create additional namespaces and join different namespaces.

# type of namespaces

| Type     | Description                                                                                                                                      |
| -------- | ------------------------------------------------------------------------------------------------------------------------------------------------ |
| `mnt`    | Mount namespaces control mount points.                                                                                                           |
| `pid`    |                                                                                                                                                  |
| `net`    | Network namespaces virtualize the network stack, Each *network interface* is present in exactly 1 namespace and can be moved between namespaces. |
| `ipc`    | prevents process in different groups communicate with shared memory.                                                                             |
| `cgroup` | see [[cgroup]]                                                                                                                                                 |
