#concurrency #MacOS


Grand Central Dispatch is a technology that is designed to make the execution of tasks on multicore hardware performant and straightforward. 

It can do this because it operates at the _system level_. Your application operates in a sandbox, which means that it is unaware of other processes running on the system at the same time. Because Grand Central Dispatch operates at the system level, it has an accurate view of the processes that are running and the resources that are available.

# References 

- https://cocoacasts.com/mastering-grand-central-dispatch-what-is-grand-central-dispatch