[ipc-bench](https://github.com/goldsborough/ipc-bench)

| Method                  |         100 Byte Messages |       1 Kilo Byte Messages |
| ----------------------- | -------------------------:| --------------------------:|
| Unix Signals            |                --broken-- |                 --broken-- |
| ZeroMQ (TCP)            |              24,901 msg/s |               22,679 msg/s |
| Internet sockets (TCP)  |              70,221 msg/s |               67,901 msg/s |
| Domain sockets          |             130,372 msg/s |              127,582 msg/s |
| Pipes                   |             162,441 msg/s |              155,404 msg/s |
| Message Queues          |             232,253 msg/s |              213,796 msg/s |
| FIFOs (named pipes)     |             265,823 msg/s |              254,880 msg/s |
| Shared Memory           |           4,702,557 msg/s |            1,659,291 msg/s |
| Memory-Mapped Files     |           5,338,860 msg/s |            1,701,759 msg/s |