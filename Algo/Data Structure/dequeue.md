#data-structure 
#multi-threading 


# A Multi-thread Fixed-size Dequeue support spmc

This is a good example of work-stealing dequeue implementation, which also a buffered _single producer multiple consumer_ channel.

[tokio/queue.rs at tokio-1.20.x · tokio-rs/tokio (github.com)](https://github.com/tokio-rs/tokio/blob/tokio-1.20.x/tokio/src/runtime/thread_pool/queue.rs)

<iframe src="https://github.com/tokio-rs/tokio/blob/tokio-1.20.x/tokio/src/runtime/thread_pool/queue.rs" style="width:100%; height:600px;" ></iframe>

the struct `Inner` holds the shared data buffer, `Local` serves the single producer and `Steal` instances are the consumers.


## Wrapping trick

the `head` and the `tail` are of type `u16`, but the capacity of the buffer is only `256`.
when pushing elements to the buffer tail will keep increasing
```rust
tail: u16 = tail.wrap_add(1);  // pushing 
head: u16 = head.wrap_sub(1);  // popping 

const cap:  u16 = 256;
const mask: u16 = cap - 1;

# compute index at the buffer
let tail_idx = tail & mask;
let head_idx = head & mask;
```


# A Multi-thread Growable Dequeue support mpmc

Compared with mpsc and spmc, poping and pushing of mpmc requires acquiring a mutex.

<iframe src="https://github.com/tokio-rs/tokio/blob/tokio-1.20.x/tokio/src/runtime/task/inject.rs" style="width:100%; height:600px;" ></iframe>



