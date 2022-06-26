#Rust
#concurrency 

# Common Types


## Channels

Defined in module: `tokio::sync`.

| Channel                                                                   | Producer | Consumer | Volume        | Num of msg |
| ------------------------------------------------------------------------- | -------- | -------- | ------------- | ---------- |
| [mpsc](https://docs.rs/tokio/latest/tokio/sync/mpsc/index.html)           | $n$      | $1$      | bound/unbound | $n$        |
| [broadcast](https://docs.rs/tokio/latest/tokio/sync/broadcast/index.html) [^bc] | $1$      | $n$      | bound/unbound | $n$        |
| [watch](https://docs.rs/tokio/latest/tokio/sync/watch/index.html)[^watch]         | $1$      | $n$      | $0$             | $n$           |
| [oneshot](https://docs.rs/tokio/latest/tokio/sync/oneshot/index.html)     | $1$      | $1$      | $0$             | $1$        |

[^watch]: `watch` only retains the _last_ sent value. This channel is useful for _watching for changes_ to a value from multiple points in the code base, for example, changes to configuration values.
[^bc]: `broadcast` guarantees _all receivers observe all values_ sent by sender

 > [!NOTE]
 > Channels will be closed once the/all senders are dropped, then receivers will get `None` as the last value.


## IO

Defined in module `tokio::io`

| Traits/Functions              | Meaning                        |
| ----------------------------- | ------------------------------ |
| `AsyncRead`, `AsyncReadExt`   | async ver. of `std::io::Read`  |
| `AsyncWrite`, `AsyncWriteExt` | async ver. of `std::io::Write` |
| `copy(reader, writer)`      | async ver. of `std::io::copy`  |
| `split(rw)`                 | split `rw` into a reader and a writer                               |




# Runtime




